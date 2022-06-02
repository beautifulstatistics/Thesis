from dask_ml.cluster import KMeans
import dask.dataframe as dd
import random
import zarr
import numpy as np

import logging
import time

logging.basicConfig(filename='./logs/make_kmeans5.log',filemode='a',level=logging.INFO,
                    format="%(process)s-%(asctime)s-%(message)s")

k_max = 500

X = dd.read_parquet('./data/vectors.parquet')

clusters = list(range(2,k_max+1))
random.shuffle(clusters)

k_inertiaso = zarr.open_array('./kmeans/data/k_inertias4.zarr', mode='r')
k_inertias = zarr.open_array('./kmeans/data/k_inertias5.zarr', mode='w',
                    shape=(len(clusters),2),chunks=None, fill_value=0,
                    dtype=float)

print('Start',flush=True)
t_start = time.time()
for index, k in enumerate(clusters):
    ktf = k_inertiaso[:,0] == k
    if any(ktf):
        k_inertias[index,:] = k_inertiaso[np.where(ktf)[0][0]]
        print(f"{k} found.",flush=True)
        continue

    t1 = time.time()
    km = KMeans(n_clusters=k,n_jobs=-1)
    km.fit(X)
    k_inertias[index,:] = (k,km.inertia_)

    t2 = time.time()
    print(f"Index: {index} finished, Clusters: {k}, Hours: {round((t2-t1)/60/60,2)}, " \
          f"Hours Left: {round((t2-t_start)/(index+1)*(len(clusters)-index-1)/60/60,2)}",flush=True)

print(f"Complete")