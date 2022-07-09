import os
import psutil
import time

while psutil.cpu_percent(5) < 90:
    time.sleep(60*5)

os.system("kill -STOP 170101")