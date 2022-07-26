import dask.array as da
from sklearn.cluster import OPTICS
import time
import pandas as pd
import logging
import os

logging.basicConfig(filename='make_optics.log',level=logging.INFO)
logging.info(f"PID: {os.getpid()}")

X = da.from_zarr('./data/cosine.zarr')
ops =  OPTICS(metric='precomputed',min_samples=100,n_jobs=-1)
logging.info(f"Fitting Started {time.strftime('%m-%d-%H', time.localtime())}")
t1 = time.time()
clusters = ops.fit_predict(X)
logging.info(f"Real Hours: {(time.time()-t1)/60/60}")
logging.info(f'Number of clusters: {len(set(clusters))}')
pd.DataFrame(clusters,columns=['clusters']).to_parquet('./optics/data/clusters.parquet')
logging.info("Complete.")