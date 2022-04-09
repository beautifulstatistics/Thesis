import os
import time
from itertools import product, islice, groupby, chain

import numpy as np
import pandas as pd

import dask.array as da
from dask.array.slicing import shuffle_slice

import spacy

import logging
logging.basicConfig(filename='make_simmat.log', encoding='utf-8', level=logging.INFO)

def peek(gen):
    try:
        first = next(gen)
    except StopIteration:
        return None
    return chain([first],gen)

def get_vocabi(shuffled_vocab):
    for i in shuffled_vocab:
        yield i

def has_vector(nlpt):
    if nlpt.has_vector and nlpt.vector_norm != 0:
        return nlpt
    return None

def sims(v):
    if v[0]:
        return(v[1].text,v[1].similarity(v[2]))
    return (v[1].text,None)

def make_shuffle(nlp_name,strings):
    if os.path.exists(f'shuffled_{nlp_name}.csv'):
        shuffled_vocab= pd.read_csv(f'shuffled{nlp_name}.csv').values.reshape(-1)
    else:
        vocab_list = da.from_array(np.array(strings))
        vocab_len = vocab_list.shape[0]
        index = np.random.choice(vocab_len, vocab_len, replace=False)
        shuffled_vocab = shuffle_slice(vocab_list, index).compute().astype(object)
    
    return shuffled_vocab, len(shuffled_vocab)

def main(nlp_name,size):
    if not os.path.exists(f'./{nlp_name}/data'):
        os.makedirs(f'./{nlp_name}/data')

    nlp = spacy.load(nlp_name)
    shuffled_vocab, nshuffled_vocab = make_shuffle(nlp_name,nlp.vocab.strings)
    total = nshuffled_vocab/size
    get_vocab = get_vocabi(shuffled_vocab)
    computel = [j>i for i,j in product(range(size),range(size))]
    i = 0
    while get_vocab:
        t2 = time.time()
        vocab_tokens = []
        size_to_get = size
        while size_to_get > 0:
            for token in islice(get_vocab,size_to_get):
                nlpt = nlp(token)
                if good_token := has_vector(nlpt):
                    vocab_tokens.append(good_token)
            if not (get_vocab := peek(get_vocab)):
                size = len(vocab_tokens)
                computel = [j>i for i,j in product(range(size),range(size))]
                total = i-1
            size_to_get = size - len(vocab_tokens)

        to_bd = ((comp,vs[0],vs[1]) for comp,vs in zip(computel,product(vocab_tokens,vocab_tokens)))
        vl = map(sims,to_bd)

        bigl = ([x[1] for x in v] for _,v in groupby(vl,key=lambda x: x[0]))

        pdf = pd.DataFrame(bigl)
        pdf.index = [token.text for token in vocab_tokens]
        pdf.columns = pdf.index
        pdf.to_parquet(f'./{nlp_name}/data/sim{i}.parquet',index=True)

        logging.info('Sleeping')
        time.sleep(2*60)
        
        logging.info(f"Finshed Batch: {i}; took {(time.time()-t2)/60} mins")
        logging.info(f"Estimated time remaining: {(time.time()-t1)/60/60/(i+1)*(total-i+1)} hours")

        i += 1


if __name__ == '__main__':
    nlp_name = 'en_core_web_lg'
    size = 30000
    t1 = time.time()
    main(nlp_name,size)
    logging.info('Took',(time.time()-t1)/60/60,'hours')
