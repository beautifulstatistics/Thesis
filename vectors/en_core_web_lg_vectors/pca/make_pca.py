from dask_ml.decomposition import PCA
import dask.dataframe as dd
import time
X = dd.read_parquet('./data/vectors.parquet').to_dask_array(lengths=True)

t1 = time.time()
pca = PCA(n_componenets=.90)
pca.fit(X)