import os
import time

import sqlite3
import csv
import glob

os.chdir("/mnt/working2/Thesis")

path = os.path.join('data','counts','week36.csv')

with open(path, 'r') as f:
    reader = csv.reader(f)
    columns = next(reader)

path = os.path.join('data','counts','week*.csv')
csv_files = glob.glob(path)

path = os.path.join('data','counts.db')
conn = sqlite3.connect(path)
cursor = conn.cursor()

columns_str = 'week INTEGER, ' + ", ".join([f"{column} {'TEXT' if column in ['text', 'text_raw'] else 'INTEGER'}" for column in columns])
cursor.execute(f"CREATE TABLE IF NOT EXISTS all_data ({columns_str})")

conn.commit()

columns_str = ', '.join(['week'] + columns)
placeholders = ', '.join('?' * (len(columns) + 1))

t1 = time.time()
for index, filename in enumerate(csv_files):
    with open(filename, 'r') as f:
        dr = csv.DictReader(f)
        fname = os.path.basename(filename)
        week = str(fname[4:].split('.')[0])
        to_db = [[week] + [i[column] for column in columns] for i in dr]

    cursor.executemany(f"INSERT INTO all_data ({columns_str}) VALUES ({placeholders})", to_db)
    
    print(f'{(index+1)/52*100:.2f}% Complete',end=": ")
    print(f'{(time.time()-t1)/(index+1)/60/60*(52 - index - 1):.2f} hours left', flush=True)

conn.commit()

print('Changing name of function to functions.')
cursor.execute("ALTER TABLE all_data RENAME COLUMN function TO functions")

print('Adding persconc.')
cursor.execute("ALTER TABLE all_data ADD COLUMN persconc INTEGER")
cursor.execute("UPDATE all_data SET persconc = work + leisure + home + money + relig + death")

print('Adding othergram.')
cursor.execute("ALTER TABLE all_data ADD COLUMN othergram INTEGER")
cursor.execute("UPDATE all_data SET othergram = compare + interrog + number + quant")

conn.commit()

conn.close()
print((time.time()-t1)/60/60, 'hours')
