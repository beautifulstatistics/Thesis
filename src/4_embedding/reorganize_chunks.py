#!/usr/bin/env python3
"""
reorganize_chunks.py
Reorganize embedding chunks from 10M to 1M embeddings per file.
Only processes chunks that have more than 1M embeddings.
"""

import sys
import numpy as np
from pathlib import Path
import shutil

sys.stdout.reconfigure(line_buffering=True)
sys.stderr.reconfigure(line_buffering=True)

EMBEDDINGS_DIR = Path("artifacts/4_embedding/embeddings")
NEW_CHUNK_SIZE = 1_000_000
BACKUP_DIR = EMBEDDINGS_DIR / "backup_10m"


def get_next_chunk_number(embeddings_dir):
    """Get the next available chunk number."""
    existing = list(embeddings_dir.glob("embeddings_chunk_*.npz"))
    if not existing:
        return 0
    return max(int(f.stem.split('_')[-1]) for f in existing) + 1


def main():
    print("=" * 60)
    print("REORGANIZE EMBEDDING CHUNKS (10M -> 1M)")
    print("=" * 60)

    if not EMBEDDINGS_DIR.exists():
        print(f"Embeddings directory not found: {EMBEDDINGS_DIR}")
        return

    # Find all chunk files
    chunk_files = sorted(EMBEDDINGS_DIR.glob("embeddings_chunk_*.npz"))
    print(f"Found {len(chunk_files)} chunk files")

    if not chunk_files:
        print("No chunks to reorganize.")
        return

    # Identify chunks that need splitting (>1M embeddings)
    chunks_to_split = []
    chunks_ok = []

    for chunk_file in chunk_files:
        try:
            data = np.load(chunk_file)
            n_embeddings = len(data['row_ids'])
            print(f"  {chunk_file.name}: {n_embeddings:,} embeddings")

            if n_embeddings > NEW_CHUNK_SIZE:
                chunks_to_split.append((chunk_file, n_embeddings))
            else:
                chunks_ok.append((chunk_file, n_embeddings))
        except Exception as e:
            print(f"  Warning: Could not read {chunk_file.name}: {e}")

    if not chunks_to_split:
        print("\nNo chunks need splitting. All chunks are <= 1M.")
        return

    print(f"\nChunks to split: {len(chunks_to_split)}")
    print(f"Chunks already ok: {len(chunks_ok)}")

    # Create backup directory
    BACKUP_DIR.mkdir(parents=True, exist_ok=True)
    print(f"\nBackup directory: {BACKUP_DIR}")

    # Process each large chunk
    for chunk_file, n_embeddings in chunks_to_split:
        print(f"\n{'=' * 40}")
        print(f"Processing {chunk_file.name} ({n_embeddings:,} embeddings)")
        print(f"{'=' * 40}")

        # Load the data
        data = np.load(chunk_file)
        embeddings = data['embeddings']
        row_ids = data['row_ids']

        print(f"Loaded embeddings shape: {embeddings.shape}")
        print(f"Loaded row_ids shape: {row_ids.shape}")

        # Calculate how many new chunks we need
        n_new_chunks = (n_embeddings + NEW_CHUNK_SIZE - 1) // NEW_CHUNK_SIZE
        print(f"Will create {n_new_chunks} new chunks")

        # Backup the original file
        backup_path = BACKUP_DIR / chunk_file.name
        print(f"Backing up to {backup_path}")
        shutil.move(str(chunk_file), str(backup_path))

        # Get next available chunk number
        next_chunk_num = get_next_chunk_number(EMBEDDINGS_DIR)
        print(f"Starting from chunk number: {next_chunk_num}")

        # Split into new chunks
        for i in range(n_new_chunks):
            start_idx = i * NEW_CHUNK_SIZE
            end_idx = min((i + 1) * NEW_CHUNK_SIZE, n_embeddings)

            chunk_embeddings = embeddings[start_idx:end_idx]
            chunk_row_ids = row_ids[start_idx:end_idx]

            new_chunk_file = EMBEDDINGS_DIR / f"embeddings_chunk_{next_chunk_num:04d}.npz"
            print(f"  Creating {new_chunk_file.name}: {len(chunk_row_ids):,} embeddings "
                  f"(row_ids {chunk_row_ids[0]} to {chunk_row_ids[-1]})")

            np.savez_compressed(
                new_chunk_file,
                embeddings=chunk_embeddings.astype(np.float32),
                row_ids=chunk_row_ids
            )

            next_chunk_num += 1

        print(f"Split complete for {chunk_file.name}")

    # Renumber all chunks sequentially
    print("\n" + "=" * 60)
    print("RENUMBERING ALL CHUNKS SEQUENTIALLY")
    print("=" * 60)

    all_chunks = sorted(EMBEDDINGS_DIR.glob("embeddings_chunk_*.npz"))
    print(f"Total chunks to renumber: {len(all_chunks)}")

    # First, rename to temporary names to avoid conflicts
    temp_names = []
    for chunk_file in all_chunks:
        temp_name = chunk_file.parent / f"temp_{chunk_file.name}"
        shutil.move(str(chunk_file), str(temp_name))
        temp_names.append(temp_name)

    # Then rename to sequential numbers
    temp_names.sort(key=lambda f: int(f.stem.split('_')[-1]))
    for i, temp_file in enumerate(temp_names):
        final_name = EMBEDDINGS_DIR / f"embeddings_chunk_{i:04d}.npz"
        shutil.move(str(temp_file), str(final_name))
        print(f"  {temp_file.name} -> {final_name.name}")

    print("\n" + "=" * 60)
    print("REORGANIZATION COMPLETE")
    print("=" * 60)

    # Final summary
    final_chunks = sorted(EMBEDDINGS_DIR.glob("embeddings_chunk_*.npz"))
    total_embeddings = 0
    for chunk_file in final_chunks:
        try:
            data = np.load(chunk_file)
            total_embeddings += len(data['row_ids'])
        except:
            pass

    print(f"Total chunks: {len(final_chunks)}")
    print(f"Total embeddings: {total_embeddings:,}")
    print(f"Backups saved in: {BACKUP_DIR}")


if __name__ == "__main__":
    main()
