from sklearn.cluster import Birch, OPTICS
from sklearn.preprocessing import Normalizer
import pandas as pd

import time
import logging
import os

logging.basicConfig(filename='make_birch.log',level=logging.INFO,filemode='w')
logging.info(f"PID: {os.getpid()}")
logging.info(f"Fitting Started {time.strftime('%m-%d-%H', time.localtime())}")

X = pd.read_parquet('./data/vectors.parquet').sample(n=10000)
X = Normalizer().fit_transform(X=X)
logging.info("Vectors Normalized")

ap = OPTICS(min_samples=1000,metric='euclidean')
bir = Birch(n_clusters=ap)

t1 = time.time()
bir.fit(X)
t2 = time.time()
logging.info(f"Real Hours to fit: {(t2 - t1)/60/60}")

clust_pred = bir.predict(X)
t3 = time.time()
logging.info(f"Real Hours to predict: {(t2-t3)/60/60}")
logging.info(f"Cluster count: {len(set(clust_pred))}")

os.makedirs('./birch/data/clusters.parquet',exist_ok=True)

pd.DataFrame(clust_pred,columns=['cluster']).to_parquet('./birch/data/clusters.parquet')
logging.info("Complete")