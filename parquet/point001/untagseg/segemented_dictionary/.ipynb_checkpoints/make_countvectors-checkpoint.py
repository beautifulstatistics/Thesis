from dask_ml.feature_extraction.text import CountVectorizer
import dask.bag as db
import dask.dataframe as dd


df = dd.read_parquet('./data').sample(frac=.01)
corpus = db.from_sequence(df['text'])

counter = CountVectorizer()
vocab = counter.fit(corpus)
tc = vocab.transform(corpus)