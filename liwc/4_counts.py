import os
import time
import multiprocessing as mp
from collections import Counter, defaultdict, OrderedDict

import numpy as np
import pandas as pd

import liwc


def return_time():
    return time.strftime('%H:%M', time.localtime())


def load_data(dictionary_path, linear_factor_path):
    parse, category_names = liwc.load_token_parser(dictionary_path)

    with open(linear_factor_path, 'r') as f:
        lf = defaultdict(list)
        for line in f:
            line = [x.strip() + str(i+1) for i, x in enumerate(line.split("/"))]
            for i, lineh in enumerate(line[:-1]):
                lf[lineh].append(line[i+1])

    lfo = OrderedDict(sorted(((k, sorted(set(v), key=lambda x: int(x[-1]), reverse=True))
                              for k, v in lf.items()), key=lambda x: int(x[0][-1]), reverse=True))

    category_names.extend(['persconc', 'totallen', 'tokencount', 'notdict'])
    category_names.extend([f'o{x[:-1]}' for x in lfo.keys()])

    return parse, category_names, lfo


def enforce_nesting(counts, lfo):
    for key in lfo:
        category = key[:-1]
        sub_count = sum(counts[sub_category[:-1]] for sub_category in lfo[key]
                        if sub_category[:-1] in counts)
        diff = counts[category] - sub_count
        if diff > 0:
            counts[f'o{category}'] = diff
        elif diff < 0:
            counts[category] = sub_count
    return counts


def fill_missing_category(category):
    if category in ['work', 'leisure', 'home', 'money', 'relig', 'death']:
        return 'persconc'


def count(weibo, parse, lfo):
    weibo = str(weibo)
    counts = Counter()
    weibo_clean = weibo.replace(' ', '').replace('\t', '').replace('\n', '')
    counts['totallen'] = len(weibo_clean)
    for token in weibo.split():
        counts.update(['tokencount'])
        for category in parse(token):
            counts.update([category])
            if missing := fill_missing_category(category):
                counts.update([missing])
        else:
            counts.update(['notdict'])

    counts = enforce_nesting(counts, lfo)
    return counts


def count_df(file):
    df = pd.read_csv(file, nrows=10)
    
    dictionary_path = os.path.join('dictionaries',
                                    'LIWC2015 Dictionary - Chinese (Simplified)(adjusted).dic')
    linear_factor_path = os.path.join('dictionaries', 'restructured_factors.txt')

    parse, category_names, lfo = load_data(dictionary_path, linear_factor_path)

    counts = df.text.apply(lambda x: count(x, parse, lfo))
    counts = pd.DataFrame.from_records(counts.values, columns=category_names)
    counts = counts.fillna(0).astype(np.int16)

    df = df.drop('text', axis=1)
    df = pd.concat([df, counts], axis=1)

    df = df.astype(np.uint8)
    file = os.path.basename(file)
    csv_path = os.path.join('data', 'counts', file)
    df.columns = [x.split('(')[0] for x in df.columns]
    df.to_csv(csv_path, index=False)

if __name__ == '__main__':
    path = os.path.join('data', 'counts')
    os.makedirs(path, exist_ok=True)
    
    path = os.path.join('data', 'clean2')
    files = [os.path.join(path, file) for file in os.listdir(path)]
    filel = len(files)

    print(f"Processing Started. {return_time()}")
    t1 = time.time()
    with mp.Pool(processes=1) as pool:
        for index, _ in enumerate(pool.imap_unordered(count_df, files)):
            elapsed_time = (time.time() - t1) / (index + 1) / 60 / 60 * (filel - index - 1)
            print(f'{(index+1)/filel*100:.2f}% Complete: {elapsed_time:.2f} hours left', flush=True)

    print(f"Finished. {return_time()}")
