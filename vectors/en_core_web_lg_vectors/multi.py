from multiprocessing import Pool
from dask_ml.cluster import KMeans
import pandas as pd
import time
import zarr

clusters = list(range(1,10))

X = pd.read_parquet('./data/vectors.parquet')

path = './kmeans/data/multi_inertias.zarr'
k_inertias = zarr.open_array(path, mode='w',
                    shape=(len(clusters),3),chunks=None, fill_value=0,
                    dtype=float)

def kmeans_k(k):
    t1 = time.time()
    km = KMeans(n_clusters=k,n_jobs=1)
    km.fit(X)
    return (k,km.inertia_,(time.time()-t1)/60)

if __name__ == "__main__":
    t1 = time.time()
    with Pool(15) as pool:
        a = pool.imap(kmeans_k, clusters)
        for index, value in enumerate(a):
            k_inertias[index] = value

    print((time.time()-t1)/60)