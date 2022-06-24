from dask_ml.decomposition import PCA
import dask.dataframe as dd
import time
import joblib

X = dd.read_parquet('./data/vectors.parquet').to_dask_array(lengths=True)

t1 = time.time()
pca = PCA()
pca.fit(X)
joblib.dump(pca, "./pca/data/model.pkl")