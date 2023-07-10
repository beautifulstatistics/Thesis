import os
import time

import sqlite3
import csv
import glob

path = os.path.join('data','counts','week36.csv')

with open(path, 'r') as f:
    reader = csv.reader(f)
    columns = next(reader)

columns.remove('function')
columns = ['function_'] + columns
    
path = os.path.join('data','counts','week*.csv')
csv_files = glob.glob(path)

path = os.path.join('data','counts.db')
conn = sqlite3.connect(path)
cursor = conn.cursor()

columns

columns_str = ", ".join([f"{column} {'TEXT' if column in ['text', 'text_raw'] else 'INTEGER'}" for column in columns]) 
cursor.execute(f"CREATE TABLE IF NOT EXISTS all_data ({columns_str})")

conn.commit()

columns_str = ", ".join(columns)
placeholders = ", ".join("?" * len(columns))

t1 = time.time()
for index, filename in enumerate(csv_files):
    with open(filename, 'r') as f:
        dr = csv.DictReader(f)
        to_db = [[i[column] for column in columns] for i in dr]

    cursor.executemany(f"INSERT INTO all ({columns_str}) VALUES ({placeholders});", to_db)
    
    print(f'{(index+1)/52*100:.2f}% Complete',end=": ")
    print(f'{(time.time()-t1)/(index+1)/60/60*(52 - index - 1):.2f} hours left',flush=True)

conn.commit()

cursor.execute("ALTER TABLE all ADD COLUMN persconc INTEGER")
cursor.execute("UPDATE all SET persconc = work + leisure + home + money + relig + death")

cursor.execute("ALTER TABLE all ADD COLUMN othergram INTEGER")
cursor.execute("UPDATE all SET othergram = compare + interrog + number + quant")

conn.commit()

conn.close()
print((time.time()-t1)/60/60, 'hours')