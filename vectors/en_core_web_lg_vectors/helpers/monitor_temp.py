import subprocess
import re
import os
import time
import sys
import psutil

def timeof():
    t1 = time.localtime()
    return f"Start {time.strftime('%d %H:%M', t1)}"

def main(script):
    log = os.path.join('logs',"".join(script.rsplit(".")[:-1] + ['.log']))
    pause = False
    with open(log,"w+",buffering=1) as loghanndle:
        sys.stdout = loghanndle
        sys.stderr = loghanndle

        proc = subprocess.Popen(["python",script],stderr=subprocess.STDOUT,stdout=loghanndle)
        pid = proc.pid

        print(f"PID {pid}")
        print(timeof())

        while True:
            if psutil.cpu_percent(5) < 95:
                print(timeof())
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
                        print(timeof())
                        os.system(f"kill -STOP {pid}")
                        time.sleep(60)
                        pause = True
                else:
                    if temp_average <= 82:
                        print(f"Resuming {pid}")
                        print(timeof())
                        os.system(f"kill -CONT {pid}")
                        pause = False
            else:
                return

if __name__ == "__main__":
    main(script=sys.argv[1])
