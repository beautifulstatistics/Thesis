from sklearn.cluster import MiniBatchKMeans
from sklearn.metrics import silhouette_score

import pandas as pd
import random
import zarr

import logging
import time

logging.basicConfig(filename='./logs/make_minikmeans_sample.log',filemode='a',level=logging.INFO,
                    format="%(process)s-%(asctime)s-%(message)s")

k_max = 1000
k_sam = 150

X = pd.read_parquet('./data/vectors.parquet')

clusters = [1]
while len(clusters) < k_sam:
    randi = random.randint(a=2,b=k_max)
    if randi not in clusters:
        clusters.append(randi)

k_inertias_sample = zarr.open_array('./kmeans/data/k_inertias_sample.zarr', mode='w',
                    shape=(len(clusters),3),chunks=None, fill_value=0,
                    dtype=float)

print('Start',flush=True)
t_start = time.time()
for index, k in enumerate(clusters):
    t1 = time.time()
    
    if k <= 1:
        km = MiniBatchKMeans(n_clusters=1)
    else:
        km = MiniBatchKMeans(n_clusters=k,batch_size=256*16)
    
    km.fit(X)

    if k <= 1:
        sil_score = None
    else:
        sil_score = silhouette_score(X=X,labels=km.labels_)
        
    k_inertias_sample[index,:] = (k,km.inertia_,sil_score)

    t2 = time.time()
    print(f"Index: {index} finished, Clusters: {k}, Hours: {round((t2-t1)/60/60,2)}, " \
          f"Hours Left: {round((t2-t_start)/(index+1)*(len(clusters)-index-1)/60/60,2)}",flush=True)

print(f"Complete")