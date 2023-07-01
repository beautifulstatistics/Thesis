import os
import time

import sqlite3
import csv
import glob
import psutil

path = os.path.join('data','counts','week1.csv')

with open(path, 'r') as f:
    reader = csv.reader(f)
    columns = next(reader)
    
path = os.path.join('data','counts','week*.csv')
csv_files = glob.glob(path)

path = os.path.join('data','count.db')
conn = sqlite3.connect(path)

cursor = conn.cursor()

memory_info = psutil.virtual_memory()
available_memory_gb = memory_info.available / (1024.0 ** 3)
cache_size_kb = (available_memory_gb - 8) * (1024 ** 2)
cursor.execute(f'PRAGMA cache_size = -{int(cache_size_kb)}')

columns_str = ", ".join([f"{column} INTEGER" for column in columns])
cursor.execute(f"""
    CREATE TABLE IF NOT EXISTS count_table (
        {columns_str}
    )
""")

conn.commit()

columns_str = ", ".join(columns)
placeholders = ", ".join("?" * len(columns))

print("Create Started.")
t1 = time.time()
for index, filename in enumerate(csv_files):
    with open(filename, 'r') as f:
        dr = csv.DictReader(f)
        to_db = [[i[column] for column in columns] for i in dr]

    cursor.executemany(f"INSERT INTO count_table ({columns_str}) VALUES ({placeholders});", to_db)
    
    print(f'{(index+1)/52*100:.2f}% Complete',end=": ")
    print(f'{(time.time()-t1)/(index+1)/60/60*(52 - index - 1):.2f} hours left',flush=True)

conn.commit()
print("Finished count table.")

t1 = time.time()

cases = [f"CASE WHEN {name} = 0 THEN 0 ELSE 1 END as {name}" for name in columns]
presence = ','.join(cases)
\
cursor.execute(f"""
    CREATE TABLE presence_table AS
    SELECT {presence}
    FROM count_table
""")

conn.commit()
conn.close()

print("Finished presence table.")
print("Took", ((time.time() - t1)/60/60), ' hours.')