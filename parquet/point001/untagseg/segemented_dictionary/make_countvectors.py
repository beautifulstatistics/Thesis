import time
import pickle
from dask_ml.feature_extraction.text import CountVectorizer
import dask.array as da
import dask.dataframe as dd
import dask.bag as db

with open('./data_dictionary/dict_index/data/dict_index.pkl','rb') as f:
    cdict = pickle.load(f)

print('Dictionary loaded')

df = dd.read_parquet('./data_segmented').sample(frac=.01)
corpus = db.from_sequence(df['text'])

t1 = time.time()
counter = CountVectorizer(vocabulary=cdict)
tc = counter.transform(corpus)
print(tc.compute())
