#!/usr/bin/env python3
"""
parse_liwc.py
Parse the LIWC dictionary file to extract words for each category.

Format of .dic file:
- Lines before second % are category definitions: ID\tname(DisplayName)
- Lines after second % are word mappings: word\tcategory_id1\tcategory_id2...
"""

from pathlib import Path
from collections import defaultdict

DICT_FILE = Path("dictionaries/LIWC2015 Dictionary - Chinese (Simplified)(adjusted).dic")

def parse_liwc_dictionary(dict_file: Path = DICT_FILE) -> dict:
    """
    Parse LIWC dictionary and return {category_name: [words...]}.
    """
    categories = {}  # id -> name
    category_words = defaultdict(list)  # name -> [words]

    with open(dict_file, 'r', encoding='utf-8') as f:
        lines = f.readlines()

    # Find the two % markers
    percent_indices = [i for i, line in enumerate(lines) if line.strip() == '%']

    if len(percent_indices) < 2:
        raise ValueError("Could not find two % markers in dictionary file")

    # Parse category definitions (between first and second %)
    for i in range(percent_indices[0] + 1, percent_indices[1]):
        line = lines[i].strip()
        if not line:
            continue

        parts = line.split('\t')
        if len(parts) >= 2:
            cat_id = parts[0]
            cat_name = parts[1].split('(')[0]  # Extract name before parentheses
            categories[cat_id] = cat_name
        elif len(parts) == 1 and '(' in parts[0]:
            # Format like "othergram(OtherGrammar)"
            cat_name = parts[0].split('(')[0]
            categories[cat_name] = cat_name

    # Parse word mappings (after second %)
    for i in range(percent_indices[1] + 1, len(lines)):
        line = lines[i].strip()
        if not line:
            continue

        parts = line.split('\t')
        if len(parts) < 2:
            continue

        word = parts[0]
        cat_ids = parts[1:]

        for cat_id in cat_ids:
            cat_id = cat_id.strip()
            if cat_id in categories:
                cat_name = categories[cat_id]
                category_words[cat_name].append(word)

    return dict(category_words)


def main():
    """Test the parser."""
    print("Parsing LIWC dictionary...")
    liwc = parse_liwc_dictionary()

    print(f"\nFound {len(liwc)} categories:")
    for cat, words in sorted(liwc.items(), key=lambda x: -len(x[1])):
        print(f"  {cat}: {len(words)} words")
        if len(words) <= 10:
            print(f"    Words: {words}")
        else:
            print(f"    Sample: {words[:5]}...")

    return liwc


if __name__ == "__main__":
    main()
