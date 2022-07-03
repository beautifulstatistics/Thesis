import subprocess
import re
import os
import time
import sys
import psutil

def main(script):
    log = os.path.join('logs',script.rsplit(".")[:-1] + ['.log'])
    pause = False
    with open(log,"w+",buffering=1) as loghanndle:
        proc = subprocess.Popen(["python",script],stderr=subprocess.STDOUT,stdout=loghanndle).communicate()
        pid = proc.pid
        p = psutil.Process(pid)

        sys.stdout = loghanndle
        sys.stderr = loghanndle

        print(f"PID {pid}")

        while True:
            if psutil.cpu_percent(5) < 95:
                time.sleep(60)
                continue

            temp_second = []
            for _ in range(10):
                sens = subprocess.run("sensors",capture_output=True)
                sens = sens.stdout.decode()

                packageid = re.findall("(?<=Package id 0:  \+)[0-9]{2}",sens)
                cores = re.findall("(?<=Core [0-9]:        \+)[0-9]{2}",sens)
                packageid = float(packageid[0])
                cores = [float(i) for i in cores]

                temp_second.append(packageid/3 + sum(cores)/8*2/3)
                time.sleep(3)

            temp_average = round(sum(temp_second)/10)

            if psutil.pid_exists(pid):
                if not pause:
                    if temp_average > 82:
                        print(f"Suspending {pid}")
                        p.suspend()
                        time.sleep(60)
                        pause = True
                else:
                    if temp_average <= 82:
                        print(f"Resuming {pid}")
                        p.resume()
                        pause = False
            else:
                return

if __name__ == "__main__":
    main(script=sys.argv[1])
