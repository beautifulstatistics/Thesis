import os
import time
import csv
import jieba
import re
import multiprocessing as mp

import pandas as pd
import emoji

def core(path):
    with open(path, newline='', errors='replace') as f:
        reader = csv.reader(f)
        next(reader)

        text_pd = []
        while True:
            try:
                line = next(reader)
            except StopIteration:
                break
            else:                
                if len(line) == 11: 
                    image = line[5]
                    text = line[6]
                    perm = line[-2]
                    
                    text = re.sub(r'http[s]?://\S+', '', text)
                    text = re.sub(r'/?/?@\w+：', '', text)
                    text = re.sub(r'\[.*?\]', '', text)
                    text = re.sub(r'@\w+', '', text)
                    text = emoji.demojize(text)
                    
                    
                    seg_list = jieba.cut(text)
                    text = ' '.join(seg_list)
                    
                    text_pd.append((image, text, perm))
        
        df = pd.DataFrame(text_pd,columns=['image','text','permission_denied'])
        df['image'] = (df.image == '1') + 0
        df['permission_denied'] = df.permission_denied.isna() + 0
        
        path = path.replace('weeks','clean2')
        df.to_csv(path, index = False)
        return path
        
def main():
    print("Processing Started.", time.strftime('%H:%M', time.localtime()))
    
    os.makedirs(os.path.join('data','clean'), exist_ok=True)
    
    weeks_dir = os.path.join('data','weeks')
    part_paths = [os.path.join(weeks_dir,file) for file in os.listdir(weeks_dir)]
    filel = len(part_paths)
    
    t1 = time.time()
    with mp.Pool(processes=1) as pool:
        for index, _ in enumerate(pool.imap_unordered(core, part_paths)):
            print(f'{(index+1)/filel*100:.2f}% Complete',end=": ")
            print(f'{(time.time()-t1)/(index+1)/60/60*(filel - index - 1):.2f} hours left',flush=True)

    print((time.time()-t1)/60/60,"hours")
    
main()
