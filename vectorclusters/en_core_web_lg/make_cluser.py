import dask.dataframe as dd

sm = dd.read_parquet('./data/sim0.parquet')

def make_whole(df):
    df = df.fillna(0) + df.fillna(0).T
    for i in range(df.shape[0]):
        df.iloc[i,i] = 1
    return(df)