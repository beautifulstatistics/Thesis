import time
import pandas as pd
from sklearn.decomposition import PCA

from sklearn.manifold import TSNE
from sklearn.manifold import Isomap
from sklearn.manifold import MDS

from umap import UMAP

def pca_fit(X,m,**kwargs):
    return PCA(n_components=m).fit_transform(X)

def umap_fit(X,m,**kwargs):
    return UMAP(n_components=m,**kwargs).fit_transform(X)

def tsne_fit(X,m,**kwargs):
    return TSNE(n_components=3,**kwargs).fit_transform(X)

def isomap_fit(X,m,**kwargs):
    return Isomap(n_components=m,**kwargs).fit_transform(X)

def mds_fit(X,m,**kwargs):
    return MDS(n_components=m,**kwargs).fit_transform(X)

def dimension(X,m):
    funcs = [pca_fit,umap_fit,tsne_fit,isomap_fit,mds_fit]

    for i,func in enumerate(funcs):
        print(i,flush=True, end = ":")
        t1 = time.time()
        X = func(X,m,n_jobs=-1)
        t2 = time.time()
        print((t2-t1)/60)

def scale(file,n,m):
    X = pd.read_parquet(file)
    X = X.sample(n=n)
    return dimension(X,m)

scale("data/vectors.parquet",2000,30)