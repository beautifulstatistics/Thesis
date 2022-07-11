import os
import psutil
import time

print(f"Start {time.strftime('%d %H:%M',time.localtime())}")
while psutil.cpu_percent(5) < 90:
    time.sleep(60*15)

os.system("kill -STOP 298450")
print(f"Stopped at {time.strftime('%d %H:%M',time.localtime())}")
