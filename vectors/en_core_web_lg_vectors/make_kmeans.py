from dask_ml.cluster import KMeans
import dask.dataframe as dd
import random
import zarr

import logging
import time

logging.basicConfig(filename='make_kmeans.log',filemode='a',level=logging.INFO,
                    format="%(process)s-%(asctime)s-%(message)s")

k_max = 200

X = dd.read_parquet('./data/vectors.parquet')

clusters = list(range(2,k_max+1))
random.shuffle(clusters)
print(len(clusters))

k_inertias = zarr.open_array('./kmeans/data/k_inertias.zarr', mode='w',
                    shape=(len(clusters),2),chunks=None, fill_value=0,
                    dtype=float)

print('Start',flush=True)
t_start = time.time()
for index, k in enumerate(clusters):
    t1 = time.time()
    km = KMeans(n_clusters=k,n_jobs=-1)
    km.fit(X)
    k_inertias[index,:] = (k,km.inertia_)

    t2 = time.time()
    print(f"Index: {index} finished, Clusters: {k}, Hours: {round((t2-t1)/60/60,2)}, " \
          f"Days Left: {round((t2-t_start)/(index+1)*(len(clusters)-index-1)/60/60/24,2)}",flush=True)

print(f"Complete")