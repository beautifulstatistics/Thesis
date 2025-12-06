#!/usr/bin/env python3
"""
2_select_num_topics.py
Find optimal number of topics using coherence scores (elbow method).
Requires: 1_build_dictionary.py to have been run first.
"""

import sys
import sqlite3
import json
import re
import logging
from pathlib import Path
from datetime import datetime

import numpy as np
import matplotlib.pyplot as plt
from gensim.corpora import Dictionary
from gensim.models import LdaMulticore, CoherenceModel

OUTPUT_DIR = Path("artifacts/3_lda")
DB_PATH = Path("data/counts.db")
LOG_FILE = Path(__file__).parent / "2_select_num_topics.log"

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s : %(levelname)s : %(message)s',
    handlers=[
        logging.FileHandler(LOG_FILE, mode='w'),
        logging.StreamHandler(sys.stdout)
    ]
)
logger = logging.getLogger(__name__)

# Topic counts to evaluate
K_VALUES = [5, 10, 15, 20, 25, 30, 40, 50, 60]

# Sample size for coherence calculation (full corpus too slow)
COHERENCE_SAMPLE = 50000
TRAIN_SAMPLE = 5000000  # 5M docs for faster training during search


def tokenize(text):
    """Tokenize Chinese text."""
    if not text:
        return []
    tokens = text.split()
    tokens = [t for t in tokens if len(t) > 1 and not re.match(r'^[^\w]+$', t)]
    return tokens


def get_sample_texts(db_path, n_samples, dictionary):
    """Get sample of tokenized texts for coherence calculation."""
    logger.info(f"Loading {n_samples:,} sample texts for coherence...")
    conn = sqlite3.connect(db_path)
    cursor = conn.cursor()

    cursor.execute(f"""
        SELECT text FROM all_data
        WHERE text IS NOT NULL
        ORDER BY RANDOM()
        LIMIT {n_samples}
    """)

    texts = []
    for (text,) in cursor.fetchall():
        if text:
            tokens = tokenize(text)
            if len(tokens) > 3:
                # Filter to only words in dictionary
                tokens = [t for t in tokens if t in dictionary.token2id]
                if tokens:
                    texts.append(tokens)

    conn.close()
    logger.info(f"Loaded {len(texts):,} valid texts")
    return texts


def get_training_corpus(db_path, dictionary, max_docs):
    """Generator for training corpus (subset for speed)."""
    conn = sqlite3.connect(db_path)
    cursor = conn.cursor()

    cursor.execute("SELECT text FROM all_data WHERE text IS NOT NULL")

    count = 0
    while count < max_docs:
        rows = cursor.fetchmany(10000)
        if not rows:
            break

        for (text,) in rows:
            if text:
                tokens = tokenize(text)
                if len(tokens) > 3:
                    bow = dictionary.doc2bow(tokens)
                    if bow:
                        yield bow
                        count += 1
                        if count >= max_docs:
                            break

        if count % 500000 == 0:
            logger.info(f"  Yielded {count:,} docs...")

    conn.close()


class SampleCorpus:
    """Reusable corpus from sample."""
    def __init__(self, db_path, dictionary, max_docs):
        self.db_path = db_path
        self.dictionary = dictionary
        self.max_docs = max_docs
        self._docs = None

    def __iter__(self):
        if self._docs is None:
            self._docs = list(get_training_corpus(self.db_path, self.dictionary, self.max_docs))
            logger.info(f"Cached {len(self._docs):,} docs for training")
        return iter(self._docs)

    def __len__(self):
        if self._docs is None:
            list(self)  # Force load
        return len(self._docs)


def compute_coherence(lda, texts, dictionary):
    """Compute c_v coherence score."""
    coherence_model = CoherenceModel(
        model=lda,
        texts=texts,
        dictionary=dictionary,
        coherence='c_v'
    )
    return coherence_model.get_coherence()


def main():
    logger.info("=" * 70)
    logger.info("LDA ELBOW METHOD - Finding Optimal Number of Topics")
    logger.info("=" * 70)
    logger.info(f"K values to test: {K_VALUES}")
    logger.info(f"Training sample: {TRAIN_SAMPLE:,} docs")
    logger.info(f"Coherence sample: {COHERENCE_SAMPLE:,} docs")

    # Load dictionary
    dict_path = OUTPUT_DIR / "dictionary.pkl"
    if not dict_path.exists():
        logger.error(f"Dictionary not found at {dict_path}")
        logger.error("Please run 1_build_dictionary.py first")
        sys.exit(1)

    dictionary = Dictionary.load(str(dict_path))
    logger.info(f"Loaded dictionary: {len(dictionary)} tokens")

    # Get sample texts for coherence
    coherence_texts = get_sample_texts(DB_PATH, COHERENCE_SAMPLE, dictionary)

    # Create training corpus (cached in memory for reuse)
    logger.info(f"Loading training corpus ({TRAIN_SAMPLE:,} docs)...")
    corpus = SampleCorpus(DB_PATH, dictionary, TRAIN_SAMPLE)

    # Train and evaluate for each k
    results = []

    for k in K_VALUES:
        logger.info(f"{'='*70}")
        logger.info(f"Training LDA with k={k} topics...")
        logger.info(f"{'='*70}")

        start = datetime.now()

        lda = LdaMulticore(
            corpus=corpus,
            id2word=dictionary,
            num_topics=k,
            passes=2,
            chunksize=50000,
            workers=15,
            random_state=42,
            eval_every=None,
            minimum_probability=0.0
        )

        train_time = (datetime.now() - start).total_seconds()
        logger.info(f"Training took {train_time:.1f}s")

        # Compute coherence
        logger.info("Computing coherence...")
        coherence = compute_coherence(lda, coherence_texts, dictionary)
        logger.info(f"k={k}: coherence={coherence:.4f}")

        results.append({
            'k': k,
            'coherence': coherence,
            'train_time': train_time
        })

        # Save intermediate results
        with open(OUTPUT_DIR / "elbow_results.json", 'w') as f:
            json.dump(results, f, indent=2)

    # Print summary
    logger.info("=" * 70)
    logger.info("RESULTS SUMMARY")
    logger.info("=" * 70)
    logger.info(f"{'k':<8} {'Coherence':<12} {'Time (s)':<10}")
    logger.info("-" * 30)
    for r in results:
        logger.info(f"{r['k']:<8} {r['coherence']:<12.4f} {r['train_time']:<10.1f}")

    # Find best k
    best = max(results, key=lambda x: x['coherence'])
    logger.info(f"\nBest k={best['k']} with coherence={best['coherence']:.4f}")

    # Plot
    ks = [r['k'] for r in results]
    coherences = [r['coherence'] for r in results]

    plt.figure(figsize=(10, 6))
    plt.plot(ks, coherences, 'bo-', linewidth=2, markersize=8)
    plt.xlabel('Number of Topics (k)', fontsize=12)
    plt.ylabel('Coherence Score (c_v)', fontsize=12)
    plt.title('LDA Topic Coherence - Elbow Method', fontsize=14)
    plt.grid(True, alpha=0.3)
    plt.xticks(ks)

    # Mark best
    plt.axvline(x=best['k'], color='r', linestyle='--', alpha=0.5, label=f"Best k={best['k']}")
    plt.legend()

    plt.tight_layout()
    plt.savefig(OUTPUT_DIR / "elbow_plot.png", dpi=150)
    logger.info(f"Saved plot to {OUTPUT_DIR / 'elbow_plot.png'}")


if __name__ == "__main__":
    main()
