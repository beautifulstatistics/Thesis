import dask.dataframe as dd

from tag_regex import extract_tags

def main():
    df = dd.read_parquet('./data/')
    df['tags'] = df.text.apply(extract_tags,meta=('text',str))
    df = df.drop(['text'],axis=1)
    df = df[['image','tags','permission_denied']]
    df.to_parquet('./tags/data')

if __name__ == '__main__':
    main()