from sklearn.cluster import KMeans
from sklearn.linear_model import LinearRegression
import pandas as pd
import time
import zarr
import logging

clusters = list(range(1,5))


logging.basicConfig(filename="./logs/multi.log",level=logging.INFO,filemode='w')

X = pd.read_parquet('./data/vectors.parquet')

path = './kmeans/data/multi_inertias.zarr'
k_inertias = zarr.open_array(path, mode='w',
                    shape=(len(clusters),3),chunks=False, fill_value=0,
                    dtype=float)

def kmeans_k(k):
    t1 = time.time()
    km = KMeans(n_clusters=k)
    km.fit(X)
    t2 = (time.time()-t1)/60/60
    return (k,km.inertia_,t2)


if __name__ == "__main__":
    logging.INFO("Start Time: ", time.strftime('%H:%M', time.localtime()))
    t1 = time.time()
    for index, value in enumerate(clusters):
        k_inertias[index] = value
        logging.INFO("K: ", value[0])
        logging.INFO("    Hours: ", value[2])
        
        ks, _, ktimes = zip(*k_inertias[k_inertias[0,:] != 0.0])
        
        lr = LinearRegression()
        lr.fit(X=ks,y=ktimes)
        logging.INFO("    Coefficients: ", lr.coef_)

        ks_left = [i for i in clusters if i not in ks]
        logging.INFO("    Time in Hours Left: ", sum(lr.predict(ks_left)))


    logging.INFO("Total Hours:", (time.time()-t1)/60/60)