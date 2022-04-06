import dask.dataframe as dd

def main():
    df = dd.read_parquet('./data')
    df = df.sample(frac=.001)
    df = df.repartition(partition_size='100MB',force=True)
    df.to_parquet('./point001/data')
    print('Done')

if __name__ == '__main__':
    main()