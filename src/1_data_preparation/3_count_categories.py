import os
import time
import csv
from collections import Counter
import multiprocessing as mp

import liwc

os.chdir("/mnt/workingfast/Thesis")

def core(text, parse):
    text = str(text)
    counts = Counter()
    for token in text.split():
        counts.update(['tokencount'])
        for category in parse(token):
            counts.update([category])    
    return counts

def count(file):
    dictionary_path = os.path.join('data', 'dictionaries', 'LIWC2015 Dictionary - Chinese (Simplified)(adjusted).dic')
    parse, category_names = liwc.load_token_parser(dictionary_path)
    category_names.extend('tokencount')

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
    with mp.Pool(processes=7) as pool:
        for index, _ in enumerate(pool.imap_unordered(count, files)):
            elapsed_time = (time.time() - t1) / (index + 1) / 60 / 60 * (filel - index - 1)
            print(f'{(index+1)/filel*100:.2f}% Complete: {elapsed_time:.2f} hours left', flush=True)

    print((time.time()-t1)/60/60,"hours")
