import pandas as pd
import time

import umap

reducer = umap.UMAP()

t1 = time.time()
embedding = reducer.fit_transform(X)
print((time.time()-t1)/60)
print(embedding.shape)