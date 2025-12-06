#!/usr/bin/env python3
"""
3_reduce_embeddings.py
Apply PCA to reduce embedding dimensionality and store in SQLite.
1024-dim embeddings are too large for SQLite (226M * 1024 * 4 bytes = 925GB).
Reduces to 50 dimensions and stores in embedding_distributions table.
"""

import sys
import sqlite3
import numpy as np
from pathlib import Path
import logging
from datetime import datetime
from sklearn.decomposition import IncrementalPCA
import joblib

DB_PATH = Path("data/counts.db")
EMBEDDING_DIR = Path("artifacts/4_embedding/embeddings")
OUTPUT_DIR = Path("artifacts/4_embedding")
LOG_FILE = Path(__file__).parent / "2_reduce_embeddings.log"

N_COMPONENTS = 50  # Reduce to 50 dimensions
PCA_SAMPLE_SIZE = 500000  # Samples for fitting PCA

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


def load_sample_embeddings(sample_size=PCA_SAMPLE_SIZE):
    """Load a sample of embeddings for fitting PCA."""
    logger.info(f"Loading sample of {sample_size:,} embeddings for PCA fitting...")

    embeddings_list = []

    # Load from chunk files
    chunk_files = sorted(EMBEDDING_DIR.glob("embeddings_chunk_*.npz"))

    if not chunk_files:
        raise ValueError(f"No embeddings found in {EMBEDDING_DIR}! Run 1_generate_embeddings.py first.")

    for chunk_file in chunk_files:
        if sum(len(e) for e in embeddings_list) >= sample_size:
            break
        data = np.load(chunk_file)
        embeddings_list.append(data['embeddings'])
        logger.info(f"  Loaded {len(data['row_ids']):,} from {chunk_file.name}")

    all_embeddings = np.vstack(embeddings_list)

    # Sample if we have more than needed
    if len(all_embeddings) > sample_size:
        indices = np.random.choice(len(all_embeddings), sample_size, replace=False)
        all_embeddings = all_embeddings[indices]

    logger.info(f"Sample shape: {all_embeddings.shape}")
    return all_embeddings


def fit_pca(embeddings, n_components=N_COMPONENTS):
    """Fit PCA on embeddings sample."""
    logger.info(f"Fitting PCA with {n_components} components...")

    pca = IncrementalPCA(n_components=n_components, batch_size=10000)
    pca.fit(embeddings)

    explained_var = sum(pca.explained_variance_ratio_) * 100
    logger.info(f"Explained variance: {explained_var:.1f}%")

    return pca


def create_embedding_table(conn, n_components):
    """Create the embedding distributions table."""
    cursor = conn.cursor()

    # Drop if exists
    cursor.execute("DROP TABLE IF EXISTS embedding_distributions")

    # Create table with component columns
    comp_cols = ", ".join([f"emb_{i} REAL" for i in range(n_components)])
    create_sql = f"""
        CREATE TABLE embedding_distributions (
            rowid INTEGER PRIMARY KEY,
            {comp_cols}
        )
    """
    cursor.execute(create_sql)
    conn.commit()
    logger.info(f"Created embedding_distributions table with {n_components} component columns")


def get_checkpoint(conn):
    """Get the last processed ROWID from checkpoint."""
    cursor = conn.cursor()
    try:
        cursor.execute("SELECT MAX(rowid) FROM embedding_distributions")
        result = cursor.fetchone()[0]
        return result if result else 0
    except:
        return 0


def process_and_store_embeddings(pca, conn):
    """Process all embeddings through PCA and store in database."""
    cursor = conn.cursor()
    n_components = pca.n_components_

    # Check for existing table and get checkpoint
    cursor.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='embedding_distributions'")
    table_exists = cursor.fetchone() is not None

    processed_row_ids = set()
    if table_exists:
        cursor.execute("SELECT rowid FROM embedding_distributions")
        processed_row_ids = set(r[0] for r in cursor.fetchall())
        logger.info(f"Found {len(processed_row_ids):,} already processed")
    else:
        create_embedding_table(conn, n_components)

    # Prepare insert statement
    placeholders = ", ".join(["?"] * (n_components + 1))
    insert_sql = f"INSERT INTO embedding_distributions VALUES ({placeholders})"

    # Process chunk files
    chunk_files = sorted(EMBEDDING_DIR.glob("embeddings_chunk_*.npz"))
    total_chunks = len(chunk_files)

    if total_chunks == 0:
        logger.warning(f"No embedding chunks found in {EMBEDDING_DIR}")
        return 0

    for chunk_idx, chunk_file in enumerate(chunk_files):
        logger.info(f"Processing {chunk_file.name} ({chunk_idx+1}/{total_chunks})...")

        data = np.load(chunk_file)
        embeddings = data['embeddings']
        row_ids = data['row_ids']

        # Filter already processed
        mask = ~np.isin(row_ids, list(processed_row_ids))
        if mask.sum() == 0:
            logger.info(f"  Skipping - all already processed")
            continue

        embeddings = embeddings[mask]
        row_ids = row_ids[mask]

        # Transform in batches (memory efficient)
        batch_size = 50000
        for i in range(0, len(row_ids), batch_size):
            batch_ids = row_ids[i:i+batch_size]
            batch_emb = embeddings[i:i+batch_size]

            # Transform
            reduced = pca.transform(batch_emb)

            # Insert
            rows = [(int(rid), *emb.tolist()) for rid, emb in zip(batch_ids, reduced)]
            cursor.executemany(insert_sql, rows)

            if (i + batch_size) % 100000 == 0:
                conn.commit()
                logger.info(f"    Processed {i+batch_size:,}/{len(row_ids):,}")

        conn.commit()
        processed_row_ids.update(row_ids.tolist())
        logger.info(f"  Completed {chunk_file.name}: {len(row_ids):,} embeddings")

    return len(processed_row_ids)


def main():
    logger.info("=" * 70)
    logger.info("REDUCING EMBEDDINGS WITH PCA")
    logger.info("=" * 70)
    logger.info(f"Target dimensions: {N_COMPONENTS}")

    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    pca_path = OUTPUT_DIR / "pca_model.pkl"

    # Check if PCA model exists
    if pca_path.exists():
        logger.info(f"Loading existing PCA model from {pca_path}")
        pca = joblib.load(pca_path)
        logger.info(f"Loaded PCA with {pca.n_components_} components")
        logger.info(f"Explained variance: {sum(pca.explained_variance_ratio_)*100:.1f}%")
    else:
        # Load sample and fit PCA
        sample = load_sample_embeddings(PCA_SAMPLE_SIZE)
        pca = fit_pca(sample, N_COMPONENTS)

        # Save PCA model
        joblib.dump(pca, pca_path)
        logger.info(f"Saved PCA model to {pca_path}")

        del sample  # Free memory

    # Connect to database
    logger.info("\nConnecting to database...")
    conn = sqlite3.connect(DB_PATH)
    cursor = conn.cursor()

    # Optimize SQLite
    cursor.execute("PRAGMA cache_size = -67108864")
    cursor.execute("PRAGMA mmap_size = 68719476736")
    cursor.execute("PRAGMA temp_store = MEMORY")
    cursor.execute("PRAGMA synchronous = OFF")
    cursor.execute("PRAGMA journal_mode = WAL")

    # Process and store
    start_time = datetime.now()
    total_processed = process_and_store_embeddings(pca, conn)

    # Create index
    logger.info("Creating index...")
    cursor.execute("CREATE INDEX IF NOT EXISTS idx_emb_rowid ON embedding_distributions(rowid)")
    conn.commit()

    # Final stats
    cursor.execute("SELECT COUNT(*) FROM embedding_distributions")
    final_count = cursor.fetchone()[0]

    elapsed = (datetime.now() - start_time).total_seconds()

    logger.info("=" * 70)
    logger.info(f"COMPLETE: {final_count:,} embeddings reduced and stored")
    logger.info(f"Dimensions: 1024 -> {N_COMPONENTS}")
    logger.info(f"Time: {elapsed/60:.1f} minutes")
    logger.info("=" * 70)

    conn.close()


if __name__ == "__main__":
    main()
