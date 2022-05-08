from dask_ml.cluster import KMeans
import dask.dataframe as dd
import numpy as np
import zarr

X = dd.read_parquet('./data/vectors.parquet')

k_samples = np.random.randint(low=2,high=10000,size=1000)

k_ineritas = zarr.open_array('./kmeans/data/k_ineritas.zarr', mode='w', 
                    shape=(1000,2),chunks=None, fill_value=0,
                    dtype=float)

for index, k in enumerate(k_samples):
    km = KMeans(n_clusters=k,n_jobs=-1)
    X = km.fit_transform(X)
    k_ineritas[index,:] = (k,X.inertia_)