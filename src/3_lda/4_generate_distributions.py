#!/usr/bin/env python3
"""
4_generate_distributions.py
Generate LDA topic distributions for all posts.
Stores distributions in the SQLite database as a new table.
Requires: 1_build_dictionary.py, 2_select_num_topics.py, and 3_train_lda.py to have been run first.
"""

import sqlite3
import re
import numpy as np
from pathlib import Path
from gensim.corpora import Dictionary
from gensim.models import LdaMulticore
import sys
import logging
from datetime import datetime

DB_PATH = Path("data/counts.db")
LDA_DIR = Path("artifacts/3_lda")
LOG_FILE = Path(__file__).parent / "4_generate_distributions.log"

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


def tokenize(text):
    """Tokenize Chinese text (already space-separated from jieba)."""
    if not text:
        return []
    tokens = text.split()
    tokens = [t for t in tokens if len(t) > 1 and not re.match(r'^[^\w]+$', t)]
    return tokens


def get_topic_distribution(lda, dictionary, text):
    """Get topic distribution for a single document."""
    if not text or not isinstance(text, str):
        return np.zeros(lda.num_topics)

    tokens = tokenize(text)
    if len(tokens) == 0:
        return np.zeros(lda.num_topics)

    bow = dictionary.doc2bow(tokens)
    if not bow:
        return np.zeros(lda.num_topics)

    topic_dist = lda.get_document_topics(bow, minimum_probability=0.0)
    dist = np.zeros(lda.num_topics)
    for topic_id, prob in topic_dist:
        dist[topic_id] = prob

    return dist


def create_lda_table(conn, num_topics):
    """Create the LDA distributions table."""
    cursor = conn.cursor()

    # Drop if exists
    cursor.execute("DROP TABLE IF EXISTS lda_distributions")

    # Create table with ROWID as primary key and topic columns
    topic_cols = ", ".join([f"topic_{i} REAL" for i in range(num_topics)])
    create_sql = f"""
        CREATE TABLE lda_distributions (
            rowid INTEGER PRIMARY KEY,
            {topic_cols}
        )
    """
    cursor.execute(create_sql)
    conn.commit()
    logger.info(f"Created lda_distributions table with {num_topics} topic columns")


def get_checkpoint(conn):
    """Get the last processed ROWID from checkpoint."""
    cursor = conn.cursor()
    try:
        cursor.execute("SELECT MAX(rowid) FROM lda_distributions")
        result = cursor.fetchone()[0]
        return result if result else 0
    except:
        return 0


def main():
    logger.info("=" * 70)
    logger.info("GENERATING LDA DISTRIBUTIONS FOR ALL POSTS")
    logger.info("=" * 70)

    # Check prerequisites
    dict_path = LDA_DIR / "dictionary.pkl"
    model_path = LDA_DIR / "lda_model"

    if not dict_path.exists():
        logger.error(f"Dictionary not found at {dict_path}")
        logger.error("Please run 1_build_dictionary.py first")
        sys.exit(1)

    if not model_path.exists():
        logger.error(f"LDA model not found at {model_path}")
        logger.error("Please run 3_train_lda.py first")
        sys.exit(1)

    # Load LDA model and dictionary
    logger.info("Loading LDA model and dictionary...")
    dictionary = Dictionary.load(str(dict_path))
    lda = LdaMulticore.load(str(model_path))
    num_topics = lda.num_topics

    logger.info(f"Loaded model with {num_topics} topics, vocab size {len(dictionary)}")

    # Connect to database
    conn = sqlite3.connect(DB_PATH)
    cursor = conn.cursor()

    # Optimize SQLite
    cursor.execute("PRAGMA cache_size = -67108864")  # 64GB cache
    cursor.execute("PRAGMA mmap_size = 68719476736")  # 64GB mmap
    cursor.execute("PRAGMA temp_store = MEMORY")
    cursor.execute("PRAGMA synchronous = OFF")
    cursor.execute("PRAGMA journal_mode = WAL")

    # Check if table exists and get checkpoint
    cursor.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='lda_distributions'")
    table_exists = cursor.fetchone() is not None

    if table_exists:
        last_rowid = get_checkpoint(conn)
        logger.info(f"Resuming from ROWID {last_rowid}")
    else:
        create_lda_table(conn, num_topics)
        last_rowid = 0

    # Get total count
    cursor.execute("SELECT COUNT(*) FROM all_data WHERE text IS NOT NULL")
    total_docs = cursor.fetchone()[0]
    logger.info(f"Total documents to process: {total_docs:,}")

    # Process in batches
    batch_size = 10000
    insert_batch_size = 5000

    # Query for documents after checkpoint
    cursor.execute(f"""
        SELECT ROWID, text FROM all_data
        WHERE text IS NOT NULL AND ROWID > ?
        ORDER BY ROWID
    """, (last_rowid,))

    processed = last_rowid
    insert_buffer = []
    start_time = datetime.now()
    last_report_time = start_time

    # Prepare insert statement
    topic_placeholders = ", ".join(["?"] * (num_topics + 1))  # +1 for rowid
    insert_sql = f"INSERT INTO lda_distributions VALUES ({topic_placeholders})"

    logger.info("Starting processing...")

    while True:
        rows = cursor.fetchmany(batch_size)
        if not rows:
            break

        for rowid, text in rows:
            dist = get_topic_distribution(lda, dictionary, text)
            insert_buffer.append((rowid, *dist.tolist()))
            processed += 1

            # Bulk insert when buffer is full
            if len(insert_buffer) >= insert_batch_size:
                cursor.executemany(insert_sql, insert_buffer)
                conn.commit()
                insert_buffer = []

        # Progress report every 30 seconds
        now = datetime.now()
        if (now - last_report_time).seconds >= 30:
            elapsed = (now - start_time).total_seconds()
            rate = (processed - last_rowid) / elapsed if elapsed > 0 else 0
            remaining = (total_docs - processed) / rate if rate > 0 else 0

            logger.info(
                f"Processed {processed:,}/{total_docs:,} ({100*processed/total_docs:.1f}%) | "
                f"Rate: {rate:.0f} docs/sec | "
                f"ETA: {remaining/3600:.1f} hours"
            )
            last_report_time = now

    # Insert remaining buffer
    if insert_buffer:
        cursor.executemany(insert_sql, insert_buffer)
        conn.commit()

    # Create index for faster joins
    logger.info("Creating index on rowid...")
    cursor.execute("CREATE INDEX IF NOT EXISTS idx_lda_rowid ON lda_distributions(rowid)")
    conn.commit()

    # Final stats
    cursor.execute("SELECT COUNT(*) FROM lda_distributions")
    final_count = cursor.fetchone()[0]

    elapsed = (datetime.now() - start_time).total_seconds()
    logger.info("=" * 70)
    logger.info(f"COMPLETE: {final_count:,} distributions generated")
    logger.info(f"Total time: {elapsed/3600:.2f} hours")
    logger.info("=" * 70)

    conn.close()


if __name__ == "__main__":
    main()
