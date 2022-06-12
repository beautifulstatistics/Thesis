from dask_ml.cluster import KMeans
import pandas as pd
import zarr
import time

X = pd.read_parquet('./data/vectors.parquet')

clusters = list(range(2000,3000,50))

k_inertias_sample = zarr.open_array('./kmeans/data/k_inertias_2000_3000.zarr', mode='w',
                    shape=(len(clusters),2),chunks=None, fill_value=0,
                    dtype=float)

print('Start',flush=True)
t_start = time.time()
for index, k in enumerate(clusters):
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