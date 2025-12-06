#!/usr/bin/env python3
"""
3_train_lda.py
Train final LDA model on full corpus using optimal k from elbow method.
Requires: 1_build_dictionary.py and 2_select_num_topics.py to have been run first.
"""

import sys
import sqlite3
import re
import json
import logging
from pathlib import Path
from collections import defaultdict

from gensim.corpora import Dictionary
from gensim.models import LdaMulticore

OUTPUT_DIR = Path("artifacts/3_lda")
DB_PATH = Path("data/counts.db")
LOG_FILE = Path(__file__).parent / "3_train_lda.log"

# Set up logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s : %(levelname)s : %(message)s',
    handlers=[
        logging.FileHandler(LOG_FILE, mode='a'),
        logging.StreamHandler(sys.stdout)
    ]
)
logger = logging.getLogger(__name__)


class WeiboCorpus:
    """Streaming corpus from SQLite database."""

    def __init__(self, db_path, dictionary, batch_size=10000):
        self.db_path = db_path
        self.dictionary = dictionary
        self.batch_size = batch_size
        self._length = None

    def __iter__(self):
        """Yield bag-of-words for each document."""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()

        # Optimize SQLite for reading
        cursor.execute("PRAGMA cache_size = -67108864")  # 64GB cache
        cursor.execute("PRAGMA mmap_size = 68719476736")  # 64GB mmap
        cursor.execute("PRAGMA temp_store = MEMORY")

        cursor.execute("SELECT text FROM all_data WHERE text IS NOT NULL")

        count = 0
        while True:
            rows = cursor.fetchmany(self.batch_size)
            if not rows:
                break

            for (text,) in rows:
                if text:
                    tokens = self.tokenize(text)
                    bow = self.dictionary.doc2bow(tokens)
                    if bow:
                        yield bow
                        count += 1
                        if count % 1000000 == 0:
                            logger.info(f"Processed {count:,} documents...")

        conn.close()
        logger.info(f"Total documents processed: {count:,}")

    def __len__(self):
        if self._length is None:
            self._length = self._count_valid_docs()
        return self._length

    def _count_valid_docs(self):
        """Count documents with non-empty bag-of-words."""
        logger.info("Counting valid documents...")
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()

        cursor.execute("PRAGMA cache_size = -67108864")
        cursor.execute("PRAGMA mmap_size = 68719476736")
        cursor.execute("PRAGMA temp_store = MEMORY")

        cursor.execute("SELECT text FROM all_data WHERE text IS NOT NULL")

        count = 0
        batch_num = 0
        while True:
            rows = cursor.fetchmany(self.batch_size)
            if not rows:
                break
            for (text,) in rows:
                if text:
                    tokens = self.tokenize(text)
                    bow = self.dictionary.doc2bow(tokens)
                    if bow:
                        count += 1
            batch_num += 1
            if batch_num % 100 == 0:
                logger.info(f"  Counted {count:,} valid docs so far...")

        conn.close()
        logger.info(f"Total valid documents: {count:,}")
        return count

    @staticmethod
    def tokenize(text):
        """Tokenize Chinese text (already space-separated from jieba)."""
        if not text:
            return []
        tokens = text.split()
        tokens = [t for t in tokens if len(t) > 1 and not re.match(r'^[^\w]+$', t)]
        return tokens


def get_optimal_k(elbow_path):
    """Read optimal k from elbow results."""
    with open(elbow_path) as f:
        results = json.load(f)
    best = max(results, key=lambda x: x['coherence'])
    return best['k']


def train_lda(corpus, dictionary, num_topics, passes=1, chunksize=100000):
    """Train LDA model using Gensim."""
    import multiprocessing as mp

    logger.info(f"Training LDA with {num_topics} topics...")
    logger.info(f"  passes={passes}, chunksize={chunksize}")

    n_workers = max(1, mp.cpu_count() - 2)
    logger.info(f"  workers={n_workers}")

    lda = LdaMulticore(
        corpus=corpus,
        id2word=dictionary,
        num_topics=num_topics,
        passes=passes,
        chunksize=chunksize,
        workers=n_workers,
        random_state=42,
        eval_every=None,
        minimum_probability=0.0
    )

    return lda


def analyze_topic_censorship(lda, dictionary, db_path, sample_size=200000):
    """Analyze which topics correlate with censorship using a sample."""
    logger.info(f"Analyzing topic-censorship correlation on {sample_size:,} samples...")

    conn = sqlite3.connect(db_path)
    cursor = conn.cursor()

    cursor.execute(f"""
        SELECT text, permission_denied FROM all_data
        WHERE text IS NOT NULL AND permission_denied IS NOT NULL
        ORDER BY RANDOM()
        LIMIT {sample_size}
    """)

    topic_stats = defaultdict(lambda: {'total': 0, 'censored': 0})
    censored_weights = defaultdict(float)
    uncensored_weights = defaultdict(float)
    total_censored = 0
    total_uncensored = 0

    for text, censored in cursor.fetchall():
        if not text:
            continue

        tokens = WeiboCorpus.tokenize(text)
        if len(tokens) <= 3:
            continue

        bow = dictionary.doc2bow(tokens)
        if not bow:
            continue

        topic_dist = lda.get_document_topics(bow, minimum_probability=0.0)

        # Get dominant topic
        dominant_topic = max(topic_dist, key=lambda x: x[1])[0]

        topic_stats[dominant_topic]['total'] += 1
        if censored:
            topic_stats[dominant_topic]['censored'] += 1
            total_censored += 1
            for topic_id, weight in topic_dist:
                censored_weights[topic_id] += weight
        else:
            total_uncensored += 1
            for topic_id, weight in topic_dist:
                uncensored_weights[topic_id] += weight

    conn.close()

    # Calculate statistics
    results = []
    for topic_id in range(lda.num_topics):
        stats = topic_stats.get(topic_id, {'total': 0, 'censored': 0})
        n = stats['total']
        if n == 0:
            continue

        cens_rate = stats['censored'] / n

        # Weight ratio
        avg_cens = censored_weights[topic_id] / total_censored if total_censored > 0 else 0
        avg_uncens = uncensored_weights[topic_id] / total_uncensored if total_uncensored > 0 else 0
        weight_ratio = avg_cens / avg_uncens if avg_uncens > 0 else 0

        results.append({
            'topic': topic_id,
            'n_dominant': n,
            'n_censored': stats['censored'],
            'censorship_rate': cens_rate,
            'weight_ratio': weight_ratio
        })

    results.sort(key=lambda x: x['weight_ratio'], reverse=True)
    return results


def get_topic_words(lda, num_words=20):
    """Get top words for each topic."""
    topics = {}
    for topic_id in range(lda.num_topics):
        words = lda.show_topic(topic_id, num_words)
        topics[topic_id] = [w for w, _ in words]
    return topics


def main():
    logger.info("=" * 70)
    logger.info("TRAINING FINAL LDA MODEL")
    logger.info("=" * 70)

    # Check prerequisites
    dict_path = OUTPUT_DIR / "dictionary.pkl"
    elbow_path = OUTPUT_DIR / "elbow_results.json"
    model_path = OUTPUT_DIR / "lda_model"

    if not dict_path.exists():
        logger.error(f"Dictionary not found at {dict_path}")
        logger.error("Please run 1_build_dictionary.py first")
        sys.exit(1)

    if not elbow_path.exists():
        logger.error(f"Elbow results not found at {elbow_path}")
        logger.error("Please run 2_select_num_topics.py first")
        sys.exit(1)

    # Load dictionary
    dictionary = Dictionary.load(str(dict_path))
    logger.info(f"Loaded dictionary: {len(dictionary)} tokens")

    # Get optimal k
    num_topics = get_optimal_k(elbow_path)
    logger.info(f"Optimal k from elbow method: {num_topics}")

    # Check if model already exists
    if model_path.exists():
        logger.info(f"Model already exists at {model_path}")
        lda = LdaMulticore.load(str(model_path))
        logger.info(f"Loaded model with {lda.num_topics} topics")
    else:
        # Create streaming corpus
        corpus = WeiboCorpus(DB_PATH, dictionary, batch_size=50000)
        logger.info(f"Corpus size: ~{len(corpus):,} documents")

        # Train LDA
        lda = train_lda(
            corpus,
            dictionary,
            num_topics=num_topics,
            passes=1,
            chunksize=100000
        )

        # Save model
        lda.save(str(model_path))
        logger.info(f"Saved model to {model_path}")

    # Get topic words
    topics = get_topic_words(lda, num_words=20)

    logger.info("\n" + "=" * 70)
    logger.info("ALL TOPICS")
    logger.info("=" * 70)
    for topic_id, words in topics.items():
        logger.info(f"Topic {topic_id}: {' | '.join(words[:10])}")

    # Analyze censorship correlation
    results = analyze_topic_censorship(lda, dictionary, DB_PATH)

    # Print results
    logger.info("\n" + "=" * 70)
    logger.info("TOPICS SORTED BY CENSORSHIP ASSOCIATION")
    logger.info("=" * 70)
    logger.info(f"{'Topic':<8} {'N':<10} {'Cens%':<10} {'Ratio':<10} Top Words")
    logger.info("-" * 80)

    for r in results:
        topic_id = r['topic']
        words = ' '.join(topics[topic_id][:6])
        logger.info(f"{topic_id:<8} {r['n_dominant']:<10} {r['censorship_rate']:<10.3f} "
                    f"{r['weight_ratio']:<10.2f} {words}")

    # Top/bottom topics
    logger.info("\n" + "=" * 70)
    logger.info("TOP 5 MOST CENSORSHIP-ASSOCIATED TOPICS")
    logger.info("=" * 70)
    for r in results[:5]:
        topic_id = r['topic']
        logger.info(f"\nTopic {topic_id} (ratio={r['weight_ratio']:.2f}, cens_rate={r['censorship_rate']:.1%}):")
        logger.info(f"  Words: {' | '.join(topics[topic_id])}")

    logger.info("\n" + "=" * 70)
    logger.info("TOP 5 LEAST CENSORSHIP-ASSOCIATED TOPICS")
    logger.info("=" * 70)
    for r in results[-5:]:
        topic_id = r['topic']
        logger.info(f"\nTopic {topic_id} (ratio={r['weight_ratio']:.2f}, cens_rate={r['censorship_rate']:.1%}):")
        logger.info(f"  Words: {' | '.join(topics[topic_id])}")

    # Save results
    output = {
        'n_topics': lda.num_topics,
        'vocab_size': len(dictionary),
        'topics': {str(k): v for k, v in topics.items()},
        'topic_stats': results
    }

    output_file = OUTPUT_DIR / "lda_results.json"
    with open(output_file, 'w', encoding='utf-8') as f:
        json.dump(output, f, ensure_ascii=False, indent=2, default=lambda x: float(x) if hasattr(x, 'item') else x)
    logger.info(f"\nSaved results to: {output_file}")


if __name__ == "__main__":
    main()
