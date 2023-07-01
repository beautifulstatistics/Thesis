import os
import requests
import zipfile
from tqdm import tqdm

def download_raw():
    chunk_size=1024

    os.makedirs(os.path.join('data','raw'),exist_ok=True)
    raw_data_path = os.path.join('data','raw','raw_data.zip')
    url = "https://datahub.hku.hk/ndownloader/articles/16674565/versions/1"

    resp = requests.get(url, stream=True)
    total = int(resp.headers.get('content-length', 0))
    with open(raw_data_path, 'wb') as file, tqdm(
        desc=raw_data_path,
        total=total,
        unit='iB',
        unit_scale=True,
        unit_divisor=1024,
    ) as bar:
        for data in resp.iter_content(chunk_size=chunk_size):
            size = file.write(data)
            bar.update(size)
    print("Download Complete")

def extract_files():
    print(os.getcwd())
    raw_data_path = os.path.join('data','raw','raw_data.zip')
    zip_path = os.path.join('data','zip')
    weeks_path = os.path.join('data','weeks')

    os.makedirs(weeks_path,exist_ok=True)
    os.makedirs(zip_path,exist_ok=True)
    
    with zipfile.ZipFile(raw_data_path, 'r') as archive:
        for file in archive.namelist():
            if file.startswith('week'):
                archive.extract(file,zip_path)
                print(file)

    for i in range(1,53):
        file = os.path.join('data','zip',f'week{i}.zip')
        with zipfile.ZipFile(file, 'r') as archive:
            archive.extractall(weeks_path)
            print(file)

    print('CSV Week Files Extracted')

def clean_up():
    raw_data_path = os.path.join('data','weeks')
    for root, _, files in os.walk(raw_data_path, topdown=False):
        for name in files:
            if name.startswith('user') or name.startswith('README') or name.startswith('random'):
                os.unlink(os.path.join(root, name))
                print(f'Deleted {name}')

def main():
    download_raw()
    extract_files()
    clean_up()

if __name__ == '__main__':
    main()