import os
import time

import numpy as np
import pandas as pd

import dask.array as da
from dask.array.slicing import shuffle_slice
from sklearn.preprocessing import Normalizer

import spacy

import logging
logging.basicConfig(filename='make_vectors.log', encoding='utf-8', level=logging.INFO)


def make_shuffle(strings):
    vocab_list = da.from_array(np.array(strings))
    vocab_len = vocab_list.shape[0]
    index = np.random.choice(vocab_len, vocab_len, replace=False)
    shuffled_vocab = shuffle_slice(vocab_list, index).compute().astype(object)
    return shuffled_vocab

def vocab_gen(nlp_name):
    nlp = spacy.load(nlp_name)
    for token in make_shuffle(nlp.vocab.strings):
        nlpt = nlp(token)
        if nlpt.has_vector:
            yield nlpt.text, nlpt.vector

def main(nlp_name):
    os.makedirs(f'./{nlp_name}_vectors/data',exist_ok=True)

    index, vals = zip(*vocab_gen(nlp_name))
    pdf = pd.DataFrame(vals)
    pdf.columns = pdf.columns.astype(str)
    pdf.index = index

    pdf = Normalizer().fit_transform(pdf)
    pdf.to_parquet(f'./{nlp_name}_vectors/data/vectors.parquet',index=True)

if __name__ == '__main__':
    nlp_name = 'en_core_web_lg'
    t1 = time.time()
    main(nlp_name)
    logging.info(f'Took {(time.time()-t1)/60/60} hours')
