import os
import csv
import sqlite3

import psutil

path = os.path.join('data','consolidated.db')
conn = sqlite3.connect(path)
cursor = conn.cursor()

memory_info = psutil.virtual_memory()
available_memory_gb = memory_info.available / (1024.0 ** 3)
cache_size_kb = (available_memory_gb - 2) * (1024 ** 2)
cursor.execute(f'PRAGMA cache_size = -{int(cache_size_kb)}')

cursor.execute("PRAGMA table_info(my_table)")
results = cursor.fetchall()
column_names = [result[1] for result in results]

to_remove = ['function', #778232531
                'persconc', # 0, all remaning are 0 also
                'oppron',
                'opronoun',
                'otensem', # 52259902
                'oparticle',
                'onegemo',
                'ofunction',
                'oaffect',
                'osocial',
                'ocogproc',
                'opercept',
                'obio',
                'odrives',
                'orelativ',
                'opersconc',
                'oinformal',
                'notdict', # 3538495146
                'totallen', # 5554108820
                'permission_denied']

for name in to_remove:
    column_names.remove(name)

cases = [f"CASE WHEN {name} = 0 THEN 0 ELSE 1 END" for name in column_names]

presence = ','.join([case + " as " + name for case, name in zip(cases,column_names)])

query = f"""
    SELECT
        {presence},
        SUM(permission_denied) as censored,
        COUNT(permission_denied) - SUM(permission_denied) as not_censored
    FROM 
        my_table
    GROUP BY 
        {','.join(cases)}
"""

cursor.execute(query)
results = cursor.fetchall()
column_names = [description[0] for description in cursor.description]

path = os.path.join('data','consolidated.csv')
with open(path, 'w', newline='') as f:
    writer = csv.writer(f)
    writer.writerow(column_names)
    writer.writerows(results)

print("Finished.")