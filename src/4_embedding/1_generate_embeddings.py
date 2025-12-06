#!/usr/bin/env python3
"""
1_generate_embeddings.py
Generate embeddings for ALL posts using sentence-transformers.
Streams from database in chunks, saves in multiple output files.
Supports checkpointing and resume.
"""

import sys
import sqlite3
import numpy as np
from pathlib import Path
import time
import argparse
import torch

sys.stdout.reconfigure(line_buffering=True)
sys.stderr.reconfigure(line_buffering=True)

# Configuration
DB_FILE = Path("data/counts.db")
OUTPUT_DIR = Path("artifacts/4_embedding/embeddings")
MAX_TEXT_LENGTH = 512
BATCH_SIZE = 128
CHUNK_SIZE = 1_000_000  # Save every 1M embeddings to a new file
PROGRESS_INTERVAL = 50_000  # Log progress every 50K


def get_processed_row_ids(output_dir):
    """Load all row IDs that have already been embedded."""
    processed_ids = set()

    for npz_file in output_dir.glob("embeddings_chunk_*.npz"):
        try:
            data = np.load(npz_file)
            if 'row_ids' in data:
                processed_ids.update(data['row_ids'].tolist())
                print(f"  {npz_file.name}: {len(data['row_ids']):,} IDs")
        except Exception as e:
            print(f"  Warning: Could not load {npz_file.name}: {e}")

    return processed_ids


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--model', type=str, default='BAAI/bge-m3', help='Model name')
    parser.add_argument('--batch-size', type=int, default=BATCH_SIZE, help='Batch size for GPU')
    parser.add_argument('--device', type=str, default='cuda', help='Device (cuda or cpu)')
    parser.add_argument('--chunk-size', type=int, default=CHUNK_SIZE, help='Embeddings per output file')
    args = parser.parse_args()

    print("=" * 70)
    print("GENERATE EMBEDDINGS FOR ALL POSTS")
    print("=" * 70)
    print(f"Model: {args.model}")
    print(f"Batch size: {args.batch_size}")
    print(f"Chunk size: {args.chunk_size:,} (embeddings per file)")
    print(f"Device: {args.device}")

    # Check GPU
    if args.device == 'cuda':
        if torch.cuda.is_available():
            print(f"GPU: {torch.cuda.get_device_name(0)}")
            print(f"VRAM: {torch.cuda.get_device_properties(0).total_memory / 1e9:.1f} GB")
        else:
            print("WARNING: CUDA not available, falling back to CPU")
            args.device = 'cpu'

    # Create output directory
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    # Load model
    print(f"\nLoading model {args.model}...")
    from sentence_transformers import SentenceTransformer
    model = SentenceTransformer(args.model, device=args.device)
    emb_dim = model.get_sentence_embedding_dimension()
    print(f"Model loaded! Embedding dimension: {emb_dim}")

    # Get already processed IDs
    print("\nChecking for existing embeddings...")
    processed_ids = get_processed_row_ids(OUTPUT_DIR)
    print(f"Already processed: {len(processed_ids):,} row IDs")

    # Determine starting chunk number
    existing_chunks = list(OUTPUT_DIR.glob("embeddings_chunk_*.npz"))
    if existing_chunks:
        current_chunk = max(int(f.stem.split('_')[-1]) for f in existing_chunks) + 1
    else:
        current_chunk = 0

    # Connect to database
    print("\nConnecting to database...")
    conn = sqlite3.connect(DB_FILE)
    cursor = conn.cursor()

    # Optimize SQLite for reading (conservative memory usage)
    cursor.execute("PRAGMA cache_size = -2097152")  # 2GB cache
    cursor.execute("PRAGMA mmap_size = 2147483648")  # 2GB mmap
    cursor.execute("PRAGMA temp_store = MEMORY")

    # Get total count
    cursor.execute("SELECT COUNT(*) FROM all_data WHERE text IS NOT NULL AND length(text) > 0")
    total_in_db = cursor.fetchone()[0]
    remaining = total_in_db - len(processed_ids)

    print(f"Total in database: {total_in_db:,}")
    print(f"Already processed: {len(processed_ids):,}")
    print(f"Remaining to embed: {remaining:,}")

    if remaining == 0:
        print("\nAll posts already embedded!")
        conn.close()
        return

    # Estimate time
    rate_estimate = 75  # embeddings per second
    hours = remaining / rate_estimate / 3600
    print(f"\nEstimated time at {rate_estimate}/sec: {hours:.1f} hours")

    # Stream through database
    print(f"\nStarting embedding generation...")
    cursor.execute("""
        SELECT ROWID, text
        FROM all_data
        WHERE text IS NOT NULL AND length(text) > 0
        ORDER BY ROWID
    """)

    start_time = time.time()
    total_processed = 0
    chunk_row_ids = []
    chunk_embeddings = []
    batch_texts = []
    batch_row_ids = []

    while True:
        rows = cursor.fetchmany(10000)  # Fetch 10K rows at a time
        if not rows:
            break

        for row_id, text in rows:
            # Skip if already processed
            if row_id in processed_ids:
                continue

            # Add to batch
            text_truncated = str(text)[:MAX_TEXT_LENGTH] if text else ""
            batch_texts.append(text_truncated)
            batch_row_ids.append(row_id)

            # Process batch when full
            if len(batch_texts) >= args.batch_size:
                # Generate embeddings
                embeddings = model.encode(
                    batch_texts,
                    batch_size=args.batch_size,
                    show_progress_bar=False,
                    convert_to_numpy=True,
                    normalize_embeddings=False
                )

                # Add to chunk
                chunk_embeddings.extend(embeddings)
                chunk_row_ids.extend(batch_row_ids)
                total_processed += len(batch_texts)

                batch_texts = []
                batch_row_ids = []

                # Progress update
                if total_processed % PROGRESS_INTERVAL == 0:
                    elapsed = time.time() - start_time
                    rate = total_processed / elapsed if elapsed > 0 else 0
                    remaining_time = (remaining - total_processed) / rate / 3600 if rate > 0 else 0
                    print(f"Progress: {total_processed:,}/{remaining:,} ({100*total_processed/remaining:.1f}%) "
                          f"- {rate:.0f}/sec - ETA: {remaining_time:.1f}h")

                # Save chunk when full
                if len(chunk_row_ids) >= args.chunk_size:
                    chunk_file = OUTPUT_DIR / f"embeddings_chunk_{current_chunk:04d}.npz"
                    print(f"\nSaving chunk {current_chunk} to {chunk_file.name}...")
                    np.savez_compressed(
                        chunk_file,
                        embeddings=np.array(chunk_embeddings, dtype=np.float32),
                        row_ids=np.array(chunk_row_ids)
                    )
                    print(f"Chunk {current_chunk} saved: {len(chunk_row_ids):,} embeddings")

                    # Reset for next chunk
                    current_chunk += 1
                    chunk_row_ids = []
                    chunk_embeddings = []

    # Process remaining batch
    if batch_texts:
        embeddings = model.encode(
            batch_texts,
            batch_size=args.batch_size,
            show_progress_bar=False,
            convert_to_numpy=True,
            normalize_embeddings=False
        )
        chunk_embeddings.extend(embeddings)
        chunk_row_ids.extend(batch_row_ids)
        total_processed += len(batch_texts)

    # Save final chunk
    if chunk_row_ids:
        chunk_file = OUTPUT_DIR / f"embeddings_chunk_{current_chunk:04d}.npz"
        print(f"\nSaving final chunk {current_chunk}...")
        np.savez_compressed(
            chunk_file,
            embeddings=np.array(chunk_embeddings, dtype=np.float32),
            row_ids=np.array(chunk_row_ids)
        )
        print(f"Final chunk saved: {len(chunk_row_ids):,} embeddings")

    conn.close()

    # Final stats
    elapsed = time.time() - start_time
    rate = total_processed / elapsed if elapsed > 0 else 0

    print(f"\n{'=' * 70}")
    print("EMBEDDING GENERATION COMPLETE")
    print(f"{'=' * 70}")
    print(f"Total embeddings generated: {total_processed:,}")
    print(f"Chunks saved: {current_chunk + 1}")
    print(f"Time: {elapsed/3600:.2f} hours ({rate:.0f}/sec)")
    print(f"Output directory: {OUTPUT_DIR}")


if __name__ == "__main__":
    main()
