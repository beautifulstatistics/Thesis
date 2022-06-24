import os
from dask_ml.cluster import KMeans
import pandas as pd
import zarr
import time

X = pd.read_parquet('./data/vectors.parquet')

clusters = list(range(45000,52500,500))

path = './kmeans/data/k_inertias_45000_52500.zarr'
if not os.path.exists(path):
    k_inertias_sample = zarr.open_array(path, mode='w',
                    shape=(len(clusters),2),chunks=None, fill_value=0,
                    dtype=float)
else:
    k_inertias_sample = zarr.open_array(path, mode='a')

print('Start',flush=True)
t_start = time.time()
for index, k in enumerate(clusters):
    if k_inertias_sample[index][1] != 0:
        continue

    t1 = time.time()
    
    if k <= 1:
        km = KMeans(n_clusters=1)
    else:
        km = KMeans(n_clusters=k,n_jobs=-1)
    
    km.fit(X)
    k_inertias_sample[index,:] = (k,km.inertia_)

    t2 = time.time()
    print(f"Index: {index} finished, Clusters: {k}, Hours: {round((t2-t1)/60/60,2)}, " \
          f"Hours Left: {round((t2-t_start)/(index+1)*(len(clusters)-index-1)/60/60,2)}",flush=True)

print(f"Complete")