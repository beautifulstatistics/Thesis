import pandas as pd
import time
import zarr

from multiprocessing import Pool

import hdbscan

ks =  [25, 50, 100, 500, 1000, 2000, 5000, 10000]

X = pd.read_parquet('./data/vectors.parquet')
print(f"Start {time.strftime('%H:%M', time.localtime())}",flush=True)

times = zarr.open_array("./hdbscan/data/all.zarr",mode='w',shape=(len(ks),6),chunks=False, fill_value=0,
                    dtype=float)

def hdbclust(k):
    clusterer = hdbscan.HDBSCAN(min_cluster_size=k, gen_min_span_tree=True, allow_single_cluster=True)
    t1 = time.time()
    clusterer.fit(X)
    t2 = (time.time()-t1)/60/60
    n_labels = len(set(clusterer.labels_))
    rv = clusterer.relative_validity_
    pr05 = sum(clusterer.probabilities_ < .05)/len(clusterer.probabilities_)
    clustpersist = min(clusterer.cluster_persistence_)
    return (k,t2,n_labels,rv,pr05,clustpersist)

if __name__ == "__main__":
    t1 = time.time()
    with Pool(8) as pool:
        a = pool.imap_unordered(hdbclust, ks)
        for index, value in enumerate(a):
            print(value)
            times[index,:] = value
    print((time.time()-t1)/60/60)