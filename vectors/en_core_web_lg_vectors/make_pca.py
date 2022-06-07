from dask_ml.decomposition import PCA
import dask.dataframe as dd
import logging
import time
import joblib

logging.basicConfig(filename='./logs/make_pca.log',filemode='a',level=logging.INFO,
                    format="%(process)s-%(asctime)s-%(message)s")


X = dd.read_parquet('./data/vectors.parquet').to_dask_array(lengths=True)

t1 = time.time()
pca = PCA()
pca.fit(X)
t2 = time.time()
logging.info(f"{(t1-t2)/60}")
joblib.dump(pca, "model.pkl")
logging.info('done.')
