from sklearn.metrics import pairwise_distances_chunked
import os
import logging
import numpy as np
import pandas as pd
import zarr
from tqdm import tqdm

logging.basicConfig(filename='make_cluster.log',level=logging.INFO)

os.makedirs('./cosine_distance/data',exist_ok=True)

df = pd.read_parquet('./data/vectors.parquet')
za_words = zarr.open_array('./cosine_distance/data/words.zarr', mode='w', 
                    shape=df.shape[0],chunks=None, fill_value=0,
                    dtype=str)
za_words = df.index.to_list()

za_arr = zarr.open_array('./cosine_distance/data/cosine.zarr', mode='w', 
                    shape=(0,df.shape[0]),chunks=None, fill_value=0,
                    dtype=np.uint8)

with tqdm(total=df.shape[0]) as pbar:
    for chunk in pairwise_distances_chunked(df,metric='cosine',n_jobs=-1,working_memory=32*1024):
        chunk = np.round(chunk/2*255).astype(np.uint8)
        za_arr.append(chunk,axis=0)
        pbar.update(chunk.shape[0])