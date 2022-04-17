from sklearn.metrics import pairwise_distances_chunked
import time
import os
import logging
import numpy as np
import pandas as pd
import zarr

logging.basicConfig(filename='make_cluster.log',level=logging.INFO)

if not os.path.exists('./cosine_distance/data'):
    os.makedirs('./cosine_distance/data')

df = pd.read_parquet('./data/vectors.parquet')
st = zarr.open_array('./cosine_distance/data/cosine.zarr', mode='a', 
                    shape=(0,df.shape[0]),chunks=None, fill_value=0,
                    dtype=np.uint16)

t1 = time.time()
for i, chunk in enumerate(pairwise_distances_chunked(df,metric='cosine',n_jobs=-1,working_memory=16*1024)):
    chunk = np.round(chunk/2*65535).astype(np.uint16)
    st.append(chunk,axis=0)
    if i > 2: break
t2 = (time.time()-t1)/60

# rr = st.shape[1]/st.shape[0]
# print(rr*(t2/(i+1))/60)
# print(rr*8)
