from dask_ml.cluster import KMeans
import dask.dataframe as dd
import numpy as np
import zarr

import logging
import time

logging.basicConfig(filename='make_kmeans.log',filemode='a',level=logging.INFO,
                    format="%(process)s-%(asctime)s-%(message)s")

X = dd.read_parquet('./data/vectors.parquet')

n_samples = 200
k_samples = []

while len(k_samples) < n_samples:
    candidate = np.random.randint(low=2,high=2000,size=1)[0]
    if candidate not in k_samples:
        k_samples.append(candidate)

k_inertias = zarr.open_array('./kmeans/data/k_inertias.zarr', mode='w',
                    shape=(n_samples,2),chunks=None, fill_value=0,
                    dtype=float)

print('Start',flush=True)
t_start = time.time()
for index, k in enumerate(k_samples):
    t1 = time.time()
    km = KMeans(n_clusters=k,n_jobs=-1)
    km.fit(X)
    k_inertias[index,:] = (k,km.inertia_)

    time.sleep(60*30)
    t2 = time.time()
    print(f"index: {index} finished. Hours: {(t2-t1)/60/60}, Time Left: {(t2-t_start)/(index+1)*(n_samples-index+1)}",flush=True)

print(f"Complete")