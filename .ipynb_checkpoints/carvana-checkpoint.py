import urllib.request
import json
import pandas as pd
from bs4 import BeautifulSoup
import time

header= {'User-Agent': 'Mozilla/5.0 (X11; Linux x86_64) ' 
      'AppleWebKit/537.11 (KHTML, like Gecko) '
      'Chrome/23.0.1271.64 Safari/537.11',
      'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
      'Accept-Charset': 'ISO-8859-1,utf-8;q=0.7,*;q=0.3',
      'Accept-Encoding': 'none',
      'Accept-Language': 'en-US,en;q=0.8',
      'Connection': 'keep-alive'}

columns = ['name','modelDate','manufacturer','model','color','brand','description','mileageFromOdometer','sku','vehicleIdentificationNumber','offers']

data = pd.DataFrame()
for i in range(1,2052):
    time.sleep(2)
    urls = f"https://www.carvana.com/cars/in-philadelphia-pa?email-capture=&page=2{i}"

    req = urllib.request.Request(url=url, headers=header)
    page = urllib.request.urlopen(req).read()
    page = page.decode('UTF-8')

    soup = BeautifulSoup(page)
    cars_json = soup.find_all('script',attrs={'data-react-helmet':'true'})
    
    for car in cars_json:
        car = car.contents[0]
        data = data.append(pd.DataFrame(json.loads(car)).loc['price',columns])

data = data.reset_index(drop=True)
data.to_csv('cars.csv',index=False)
data