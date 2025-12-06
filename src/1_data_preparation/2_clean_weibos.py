import os
import time
import csv
import re
import multiprocessing as mp

import emoji
import jieba

# Patterns to exclude (non-content posts)
# These are reposts, shares, empty, or system-generated - not original content
EXCLUDE_PATTERNS = [
    # Reposts (not original content, just forwarding)
    r'^\s*转发\s*微博[\s\.\。\!\！\~\～]*$',   # "转发 微博" with optional punctuation
    r'^\s*轉發\s*微博[\s\.\。\!\！\~\～]*$',   # Traditional Chinese
    r'^\s*转发\s*微薄[\s\.\。\!\！\~\～]*$',   # Common typo for 微博
    r'^\s*Repost\s*$',                          # English

    # Share markers (content is in image/link, not text)
    r'^\s*分享\s*图片[\s\.\。\!\！]*$',        # Share image
    r'^\s*分享\s*圖片[\s\.\。\!\！]*$',        # Traditional
    r'^\s*分享\s*链接[\s\.\。\!\！]*$',        # Share link
    r'^\s*分享\s*網址[\s\.\。\!\！]*$',        # Share URL
    r'^\s*分享\s*网址[\s\.\。\!\！]*$',

    # System/bot generated
    r'^\s*系统\s*自动\s*转发.*$',              # System auto-forward

    # Empty content
    r'^\s*$',                                   # Empty or whitespace
    r'^[\s\.\,\!\?\。\，\！\？\~\～\…\-\/\:]+$', # Punctuation only
    r'^[\s\d\.\,\-\/\:\：]+$',                 # Numbers/dates only

    # Forward-only (no commentary added)
    r'^\s*转发?[\s\.\。\!\！\~\～]*$',         # Just "转" or "转发"
    r'^\s*轉發?[\s\.\。\!\！\~\～]*$',         # Traditional
]
EXCLUDE_RE = re.compile('|'.join(EXCLUDE_PATTERNS), re.IGNORECASE)

def core(path):
    clean_path = path.replace('unzipped', 'clean')
    with open(clean_path, 'w', newline='') as file:
        writer = csv.writer(file)
        writer.writerow(['image', 'text_raw', 'text', 'permission_denied'])
        with open(path, newline='', errors='replace') as f:
            reader = csv.reader(f)

            malformed = 0
            filtered_pattern = 0  # Matched exclude patterns
            filtered_empty = 0   # Empty after cleaning
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

                        # Clean text: remove URLs, mentions, emojis, etc.
                        text = re.sub(r'https?://\S+|www\.\S+', '', text_raw)
                        text = re.sub(r'fs2you://\S+|magnet:\?\S+', '', text)
                        text = re.sub(r'/?/?@\w+： ?', '', text)
                        text = re.sub(r'\[.*?\]', '', text)
                        text = re.sub(r'@\w+', '', text)
                        text = emoji.replace_emoji(text, replace='')

                        # Tokenize with jieba
                        seg_list = jieba.cut(text)
                        text = ' '.join(seg_list)

                        # Filter out non-content posts (reposts, shares, etc.)
                        if EXCLUDE_RE.match(text):
                            filtered_pattern += 1
                            continue

                        # Check if empty after all cleaning (may have only had URLs/mentions)
                        text_stripped = text.strip()
                        if not text_stripped or len(text_stripped.split()) == 0:
                            filtered_empty += 1
                            continue

                        image = 1 if image == '1' else 0
                        perm = 1 if perm == 'True' else 0

                        data = (image, text_raw, text, perm)
                        writer.writerow(data)
                    else:
                        malformed += 1

    return malformed, filtered_pattern, filtered_empty
        
def main():
    os.makedirs(os.path.join('data','clean'), exist_ok=True)

    weeks_dir = os.path.join('data','unzipped')
    part_paths = [os.path.join(weeks_dir,file) for file in os.listdir(weeks_dir)]
    filel = len(part_paths)

    total_malformed = 0
    total_filtered_pattern = 0
    total_filtered_empty = 0

    t1 = time.time()
    # Use most available cores (leave a few for system)
    n_processes = max(1, mp.cpu_count() - 2)  # 30 on this system
    with mp.Pool(processes=n_processes) as pool:
        for index, (malformed, filtered_pattern, filtered_empty) in enumerate(pool.imap_unordered(core, part_paths)):
            total_malformed += malformed
            total_filtered_pattern += filtered_pattern
            total_filtered_empty += filtered_empty
            print(f'{(index+1)/filel*100:.2f}% Complete.', end=": ")
            print(f'{(time.time()-t1)/(index+1)/60/60*(filel - index - 1):.2f} hours left. '
                  f'Malformed: {malformed}, Pattern: {filtered_pattern}, Empty: {filtered_empty}', flush=True)

    print(f"\nTotal time: {(time.time()-t1)/60/60:.2f} hours")
    print(f"Total malformed rows: {total_malformed:,}")
    print(f"Total filtered (pattern match): {total_filtered_pattern:,}")
    print(f"Total filtered (empty after cleaning): {total_filtered_empty:,}")
    print(f"Total filtered: {total_filtered_pattern + total_filtered_empty:,}")

if __name__ == '__main__':
    main()
