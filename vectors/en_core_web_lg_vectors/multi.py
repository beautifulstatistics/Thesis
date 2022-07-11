from multiprocessing import Pool
from sklearn.cluster import KMeans
import pandas as pd
import time
import zarr

clusters = [1]

X = pd.read_parquet('./data/vectors.parquet')

path = './kmeans/data/multi_inertias.zarr'
k_inertias = zarr.open_array(path, mode='w',
                    shape=(len(clusters),3),chunks=None, fill_value=0,
                    dtype=float)

def kmeans_k(k):
    t1 = time.time()
    km = KMeans(n_clusters=k)
    km.fit(X)
    t2 = (time.time()-t1)/60
    return (k,km.inertia_,t2)


if __name__ == "__main__":
    print(time.strftime('%H:%M', time.localtime()))
    t1 = time.time()
    with Pool(16) as pool:
        a = pool.imap(kmeans_k, clusters)
        for index, value in enumerate(a):
            k_inertias[index] = value
            print(value)

    print("Total Days:",(time.time()-t1)/60/60/24)