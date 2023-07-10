import os
import time
import csv
import re
import multiprocessing as mp

import emoji
import jieba

def core(path):
    clean_path = path.replace('unzipped', 'clean')
    with open(clean_path, 'w', newline='') as file:
        writer = csv.writer(file)
        writer.writerow(['image', 'text_raw', 'text', 'permission_denied'])
        with open(path, newline='', errors='replace') as f:
            reader = csv.reader(f)
            next(reader)

            malformed = 0
            while True:
                try:
                    line = next(reader)
                except StopIteration:
                    break
                else:
                    if len(line) == 11: 
                        image = line[5]
                        text_raw = line[6]
                        perm = line[-1]
                                                                                    
                        text = re.sub(r'https?://\S+|www\.\S+', '', text_raw)
                        text = re.sub(r'fs2you://\S+|magnet:\?\S+', '', text)
                        text = re.sub(r'/?/?@\w+： ?', '', text)
                        text = re.sub(r'\[.*?\]', '', text)
                        text = re.sub(r'@\w+', '', text)
                        text = emoji.replace_emoji(text, replace='')

                        seg_list = jieba.cut(text)
                        text = ' '.join(seg_list)
                                                
                        image = 1 if image == '1' else 0                           
                        perm = 1 if perm == 'True' else 0

                        data = (image, text_raw, text, perm)
                        writer.writerow(data)
                    else:
                        malformed += 1
    
    return malformed
        
def main():    
    os.makedirs(os.path.join('data','clean'), exist_ok=True)
    
    weeks_dir = os.path.join('data','unzipped')
    part_paths = [os.path.join(weeks_dir,file) for file in os.listdir(weeks_dir)]
    filel = len(part_paths)
    
    t1 = time.time()
    with mp.Pool(processes=4) as pool:
        for index, malformed in enumerate(pool.imap_unordered(core, part_paths)):
            print(f'{(index+1)/filel*100:.2f}% Complete.', end=": ")
            print(f'{(time.time()-t1)/(index+1)/60/60*(filel - index - 1):.2f} hours left. Rows malformed:', malformed, flush=True)

    print((time.time()-t1)/60/60,"hours")
    
main()
