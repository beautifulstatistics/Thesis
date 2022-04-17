import pandas as pd
import numpy as np
from sklearn.cluster import OPTICS
import os
import re
import time
import logging

logging.basicConfig(filename='make_cluster.log',level=logging.INFO)

def make_whole(sm):
    sm = sm.fillna(0)
    sm = sm + sm.T
    np.fill_diagonal(sm.values,1)
    return sm.round(6)
    
def make_distance(sm):
    return 1-sm.abs()

if not os.path.exists('./cluster/data'):
    os.makedirs('./cluster/data')

siml = list(filter(lambda x: re.search('(?<=sim)[0-9]+(?=.parquet)',x),os.listdir('./data')))
t1 = time.time()
for i, path_name in enumerate(siml):
    t2 = time.time()
    smv = pd.read_parquet(os.path.join('./data',path_name))
    print('here')
    smv = make_whole(smv)
    smv = make_distance(smv)

    cl = OPTICS(metric='precomputed',n_jobs=-1)
    cl.fit(smv)

    num = re.search('(?<=sim)[0-9]+(?=.parquet)',path_name).group()
    pd.DataFrame(zip(cl.feature_names_in_,cl.labels_),columns=['word','label']).to_parquet(f'./cluster/data/labels{num}.parquet',index=False)
    logging.info(path_name + ' finished')
    mins = ((time.time()-t2)/60)
    logging.info(f'Took:{mins} mins')
    logging.info(f'Expected time left:{mins/(i+1)*(len(siml)-i+1)}')