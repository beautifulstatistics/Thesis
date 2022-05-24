import time
import psutil

pid = 94445
while psutil.pid_exists(pid):
    psutil.Process(pid=pid).resume()
    time.sleep(60*30)
    psutil.Process(pid=pid).suspend()
    time.sleep(60*30)