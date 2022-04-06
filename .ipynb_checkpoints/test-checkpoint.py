import os
import pandas as pd
from random import randint
import requests
import time

def test():    
    class vpn():
        def connect(self, location):
            os.system(f"expressvpn connect {location}")

        def disconnect(self):
            os.system("expressvpn disconnect")

    def get_time(URL):
        start_time = time.time()
        requests.get(URL)
        return(time.time() - start_time)

    df = pd.read_csv('~/Desktop/Censorship/Uncensorship/words.csv')
    
    vpn = vpn()

    cen_servers = ['hk1','hk2','hk4','tw3','mo']
    ncen_servers = ['usny','uswd','ussf','usla','usda']
    cens_order = ['cen','ncen']
    test_order = ['control','test']

    df['cen_server'] = [cen_servers[randint(0,4)] for i in range(df.shape[0])]
    df['ncen_server'] = [ncen_servers[randint(0,4)] for i in range(df.shape[0])]
    df['cens_order'] = [cens_order[randint(0,1)] for i in range(df.shape[0])]
    df['test_order'] = [test_order[randint(0,1)] for i in range(df.shape[0])]

    core = "https://s.weibo.com/weibo/"

    server0_test0l = []
    server0_test1l = []
    server1_test0l = []
    server1_test1l = []

    for index, vals in df.iterrows():
        if vals.cens_order == 'cens':
            server = [vals.cen_server,vals.ncen_server]
        else:
            server = [vals.ncen_server,vals.cen_server]

        if vals.test_order == 'control':
            test = [core + 'BTS', core + vals.zh]
        else:
            test = [core + vals.zh, core + 'BTS']

        for i in range(3):
            try:
                vpn.connect(server[0])

                server0_test0 = get_time(test[0])
                server0_test1 = get_time(test[1])

                vpn.disconnect()
                break
            except:
                vpn.disconnect()
                time.sleep(60*2)
        else:
            return("Error")

        for i in range(3):
            try:
                vpn.connect(server[1])

                server1_test0 = get_time(test[0])
                server1_test1 = get_time(test[1])

                vpn.disconnect()
                break
            except:
                vpn.disconnect()
                time.sleep(60*2)
        else:
            return('Error')
        
        time.sleep(randint(30,90))


        server0_test0l.append(server0_test0)
        server0_test1l.append(server0_test1)
        server1_test0l.append(server1_test0)
        server1_test1l.append(server1_test1)


        if index % 10 == 0:
            print('THIS IS THE INDEX" +' + ':'*500 + str(index))

    df['server0_test0'] = server0_test0l
    df['server0_test1'] = server0_test1l

    df['server1_test0'] = server1_test0l
    df['server1_test1'] = server1_test1l

    df.to_csv('test.csv', index=False)
    
    return("Finished Successfully")
    

if __name__ == "__main__":
    print(test())
    