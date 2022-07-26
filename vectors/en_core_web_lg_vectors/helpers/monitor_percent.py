import time
import psutil

pid = 24626

print("Day", "Hour", "CPU"," RAM", " SWAP")
while psutil.pid_exists(pid):
    t1 = f"{time.strftime('%d %H:%M', time.localtime())}"
    cp = psutil.cpu_percent(10)
    mp = psutil.virtual_memory().percent
    sp = psutil.swap_memory().percent
    print(t1,cp,mp,sp, flush=True)
    time.sleep(60*30)