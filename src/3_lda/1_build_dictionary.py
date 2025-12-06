#!/usr/bin/env python3
"""
1_build_dictionary.py
Build Gensim dictionary from full Weibo corpus (streaming, memory-efficient).
"""

import sys
import sqlite3
import re
import logging
from pathlib import Path

from gensim.corpora import Dictionary

OUTPUT_DIR = Path("artifacts/3_lda")
DB_PATH = Path("data/counts.db")
LOG_FILE = Path(__file__).parent / "1_build_dictionary.log"

# Set up logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s : %(levelname)s : %(message)s',
    handlers=[
        logging.FileHandler(LOG_FILE, mode='w'),
        logging.StreamHandler(sys.stdout)
    ]
)
logger = logging.getLogger(__name__)


def tokenize(text):
    """Tokenize Chinese text (already space-separated from jieba)."""
    if not text:
        return []
    tokens = text.split()
    # Filter short tokens and punctuation
    tokens = [t for t in tokens if len(t) > 1 and not re.match(r'^[^\w]+$', t)]
    return tokens


def build_dictionary(db_path, min_df=500, max_df_ratio=0.3, keep_n=50000, batch_size=50000):
    """Build dictionary from the FULL corpus by streaming."""
    logger.info("Building dictionary from FULL corpus (streaming)...")

    conn = sqlite3.connect(db_path)
    cursor = conn.cursor()

    # Optimize SQLite for reading
    cursor.execute("PRAGMA cache_size = -67108864")  # 64GB cache in KB
    cursor.execute("PRAGMA mmap_size = 68719476736")  # 64GB memory-mapped I/O
    cursor.execute("PRAGMA temp_store = MEMORY")

    cursor.execute("SELECT COUNT(*) FROM all_data WHERE text IS NOT NULL")
    total_docs = cursor.fetchone()[0]
    logger.info(f"Total documents: {total_docs:,}")

    # Stream through all documents
    cursor.execute("SELECT text FROM all_data WHERE text IS NOT NULL")

    dictionary = Dictionary()
    count = 0
    batch = []

    while True:
        rows = cursor.fetchmany(batch_size)
        if not rows:
            break

        for (text,) in rows:
            if text:
                tokens = tokenize(text)
                if tokens:
                    batch.append(tokens)
                    count += 1

        # Add batch to dictionary
        if batch:
            dictionary.add_documents(batch)
            batch = []

        if count % 1000000 == 0:
            logger.info(f"Processed {count:,} / {total_docs:,} ({100*count/total_docs:.1f}%), vocab: {len(dictionary):,}")

    conn.close()

    logger.info(f"Processed {count:,} valid documents")
    logger.info(f"Initial vocabulary: {len(dictionary):,} tokens")

    # Filter extremes
    dictionary.filter_extremes(
        no_below=min_df,
        no_above=max_df_ratio,
        keep_n=keep_n
    )

    logger.info(f"Filtered vocabulary: {len(dictionary):,} tokens")

    return dictionary


def main():
    logger.info("=" * 70)
    logger.info("BUILDING GENSIM DICTIONARY")
    logger.info("=" * 70)

    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    dict_path = OUTPUT_DIR / "dictionary.pkl"

    if dict_path.exists():
        logger.info(f"Dictionary already exists at {dict_path}")
        dictionary = Dictionary.load(str(dict_path))
        logger.info(f"Loaded dictionary with {len(dictionary)} tokens from {dictionary.num_docs:,} docs")
        return

    dictionary = build_dictionary(
        DB_PATH,
        min_df=500,
        max_df_ratio=0.3,
        keep_n=50000
    )

    dictionary.save(str(dict_path))
    logger.info(f"Saved dictionary to {dict_path}")


if __name__ == "__main__":
    main()
