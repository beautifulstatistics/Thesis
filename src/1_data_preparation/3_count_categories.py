import os
import time
import csv
from collections import Counter
import multiprocessing as mp

import liwc

def parse_nesting_structure(file_path="dictionaries/Factors_nesting_adjusted_clean.txt"):
    """Parse the nesting structure from the factors dictionary file."""
    nesting_map = {}
    with open(file_path, 'r') as f:
        lines = f.readlines()
        
    current_parents = {}
    for line in lines:
        if not line.strip():
            continue
            
        indent = len(line) - len(line.lstrip())
        level = indent // 4
        
        parts = line.strip().split(" ", 1)
        if len(parts) > 1:
            category_parts = parts[1].split("(", 1)
            category = category_parts[0].strip()
        else:
            continue
            
        current_parents[level] = category
        
        for l in list(current_parents.keys()):
            if l > level:
                del current_parents[l]
                
        if level > 0:
            nesting_map[category] = [current_parents[l] for l in range(level)]
            
    return nesting_map

def core(text, parse):
    text = str(text)
    counts = Counter()
    for token in text.split():
        counts.update(['tokencount'])
        for category in parse(token):
            counts.update([category])    
    return counts

def apply_nesting_sums(counts, nesting_map):
    """Sum counts of lower level categories into their parent categories."""
    new_counts = counts.copy()
    
    # For each category that has parents
    for category, parents in nesting_map.items():
        # If this category has a count
        if category in counts:
            # Add its count to each parent
            for parent in parents:
                new_counts[parent] += counts[category]
                
    return new_counts

def count(file):
    dictionary_path = os.path.join('dictionaries', 'LIWC2015 Dictionary - Chinese (Simplified)(adjusted).dic')
    parse, category_names = liwc.load_token_parser(dictionary_path)
    category_names.append('tokencount')

    csv_path = file.replace('clean','counts')

    with open(file, 'r') as read_file:
        reader = csv.reader(read_file)
        headers = next(reader)
        with open(csv_path, 'w', newline='') as write_file:
            writer = csv.writer(write_file)
            category_names_clean = [x.split('(')[0] for x in category_names] + ['image', 'text_raw', 'text', 'permission_denied']
            writer.writerow(category_names_clean)

            for row in reader:
                permission_denied = row[headers.index('permission_denied')]
                image = row[headers.index('image')]
                text = row[headers.index('text')]
                text_raw = row[headers.index('text_raw')]
                
                counts = core(text, parse)
                
                ordered_counts = [counts[name] for name in category_names] + [image, text_raw, text, permission_denied]
                writer.writerow(ordered_counts)

if __name__ == '__main__':
    path = os.path.join('data', 'counts')
    os.makedirs(path, exist_ok=True)
    
    path = os.path.join('data', 'clean')
    files = [os.path.join(path, file) for file in os.listdir(path)]
    filel = len(files)

    t1 = time.time()
    # Use most available cores (leave a few for system)
    n_processes = max(1, mp.cpu_count() - 2)  # 30 on this system
    with mp.Pool(processes=n_processes) as pool:
        for index, _ in enumerate(pool.imap_unordered(count, files)):
            elapsed_time = (time.time() - t1) / (index + 1) / 60 / 60 * (filel - index - 1)
            print(f'{(index+1)/filel*100:.2f}% Complete: {elapsed_time:.2f} hours left', flush=True)

    print((time.time()-t1)/60/60,"hours")
