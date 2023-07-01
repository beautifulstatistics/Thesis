import os
import re

factors_path = os.path.join('dictionaries','Factors_nesting_adjusted_.txt')
with open(factors_path,'r') as f:
    hier = f.read()

k = re.sub(" |[0-9]",'',hier)
k = re.sub("\(.+","",k)

previous_count = 0
previous_line = ''
masterh = []
master = []
for line in k.split("\n"):
    count = 0
    for i in line:
        if i == '\t':
            count += 1
    line = re.sub('\t','',line)
    if count > previous_count:
        if previous_line:
            masterh += [previous_line]
        previous_count = count
    else:
        master += ['/'.join(masterh) + '/' + previous_line]
        if count < previous_count:
            masterh = masterh[:(count-previous_count)]
            previous_count = count
    
    previous_line = line

result_path = os.path.join('dictionaries','restructured_factors.txt')
with open(result_path,'w') as f:
    f.write("\n".join(master))