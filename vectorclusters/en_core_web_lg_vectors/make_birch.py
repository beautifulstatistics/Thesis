from sklearn.cluster import Birch, OPTICS
import pandas as pd

import time
import logging
import os

logging.basicConfig(filename='make_birch.log',level=logging.INFO,filemode='w')
logging.info(f"PID: {os.getpid()}")
logging.info(f"Fitting Started {time.strftime('%m-%d-%H', time.localtime())}")
os.makedirs('./birch/data',exist_ok=True)

X = pd.read_parquet('./data/vectors.parquet')

ap = OPTICS(min_samples=1000,metric='euclidean')
bir = Birch(n_clusters=ap)

t1 = time.time()
bir.fit(X)
t2 = time.time()
logging.info(f"Real Hours to fit: {(t2 - t1)/60/60}")

clust_pred = bir.predict(X)
t3 = time.time()
logging.info(f"Real Hours to predict: {(t3-t2)/60/60}")
logging.info(f"Cluster count: {len(set(clust_pred))}")

pd.DataFrame(clust_pred,columns=['cluster']).to_parquet('./birch/data/clusters.parquet')
logging.info("Complete")