import zipfile
import os

import dask.dataframe as dd
import dask.array as da
import dask

dask.config.set(scheduler='processes')

from dask.diagnostics import ProgressBar

def extract_files():
    with zipfile.ZipFile('./raw_data.zip', 'r') as archive:
        for file in archive.namelist():
            if file.startswith('week'):
                archive.extract(file,'.')
                print(file)

    for i in range(1,53):
        file = f'./week{i}.zip'
        with zipfile.ZipFile(file, 'r') as archive:
            archive.extractall('.')
            os.unlink(file)
            print(f'Deleted {file}')

    print('CSV Week Files Extracted')

def make_parquet():
    df = dd.read_csv('./week*.csv',encoding='utf-8',dtype=str,engine='c',on_bad_lines='skip',**{'encoding_errors':'replace'})
    df['permission_denied'] = ~df.permission_denied.isna()
    df['image'] = (df.image == 1).astype(bool)
    df = df.drop(['mid','retweeted_status_mid','uid','geo','created_at','retweeted_uid','source','deleted_last_seen'],axis=1)
    df = df.loc[~df.text.isna(),:]

    with ProgressBar():
        df.to_parquet('./parquet/data')

    print('Parquet files made.')

def clean_up():
    for root, dirs, files in os.walk(".", topdown=False):
        for name in files:
            if name.startswith('week') or name.startswith('user') or name.startswith('README') or name.startswith('random'):
                os.unlink(os.path.join(root, name))
                print(f'Deleted {name}')

    print("All Finished")

def main():
    extract_files()
    make_parquet()
    clean_up()


if __name__ == '__main__':
    main()