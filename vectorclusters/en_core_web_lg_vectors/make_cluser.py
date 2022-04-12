import dask.dataframe as dd
from dask_ml.cluster import SpectralClustering
from sklearn.metrics.pairwise import pairwise_kernels
import time
import os
import logging
import numpy as np
import pandas as pd
from itertools import product

logging.basicConfig(filename='make_cluster.log',level=logging.INFO)

if not os.path.exists('./cluster/data'):
    os.makedirs('./cluster/data')

df = dd.read_parquet('./data/vectors.parquet')

def cosine_kernel(X,y=None,gamma=None,degree=None,coef0=None):
    kerns = pairwise_kernels(X,y,metric='cosine')
    return np.exp(np.absolute(kerns))

times = []
for n_components, frac in product([50,100,200],[.002,.003,.004]):
    t1 = time.time()
    sc = SpectralClustering(persist_embedding=True,n_jobs=-1,affinity=cosine_kernel,n_components=n_components)
    labs = sc.fit_predict(df.sample(frac=frac).to_dask_array(lengths=True))
    k = (n_components,frac,(time.time()-t1)/60,len(np.unique(labs.compute())))
    times.append(k)
    print(k)

pd.DataFrame(times).to_csv('times.csv')

