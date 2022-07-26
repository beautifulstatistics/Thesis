from sklearn.decomposition import PCA
import time

import pandas as pd

import hdbscan

from joblib import dump

X = pd.read_parquet('./data/vectors.parquet')

pca = PCA(n_components=.9)
X = pd.DataFrame(pca.fit_transform(X))

def hdbclust(k):
    clusterer = hdbscan.HDBSCAN(min_cluster_size=k, core_dist_n_jobs=-1, 
    algorithm = "boruvka_balltree", gen_min_span_tree=True, allow_single_cluster=True)
    clusterer.fit(X)
    return clusterer

print(time.strftime('%D %H:%M', time.localtime()),flush=True)
t1 = time.time()
clusterer = hdbclust(1000)
t2 = time.time()
print((t2-t1)/60)
dump(clusterer, './pca_hdbscan/data/clusterer_1000.joblib')
print(time.strftime('%D %H:%M', time.localtime()))

# clusterer = load('./pca_hdbscan/data/clusterer_1000.joblib')
# print(sum(clusterer.probabilities_ < .05)/len(clusterer.probabilities_))
