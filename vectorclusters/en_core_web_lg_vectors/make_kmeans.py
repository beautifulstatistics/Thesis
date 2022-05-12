from dask_ml.cluster import KMeans
import dask.dataframe as dd
import numpy as np
import zarr

import logging
import time

logging.basicConfig(filename='make_kmeans.log',filemode='w',level=logging.INFO,
                    format="%(process)s-%(asctime)s-%(message)s")

X = dd.read_parquet('./data/vectors.parquet')

n_samples = 200
k_samples = np.random.randint(low=2,high=5000,size=n_samples)
k_ineritas = zarr.open_array('./kmeans/data/k_ineritas.zarr', mode='w',
                    shape=(n_samples,2),chunks=None, fill_value=0,
                    dtype=float)

print('Start')
for index, k in enumerate(k_samples):
    t1 = time.time()
    km = KMeans(n_clusters=k,n_jobs=-1)
    km.fit(X)
    k_ineritas[index,:] = (k,km.inertia_)

    t2 = time.time()
    print(f"index: {index} finished. Hours: {(t2-t1)/60/60}")

print(f"Complete")