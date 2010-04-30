'''
Created on Apr 30, 2010

@author: j35
'''

import re

if __name__ == '__main__':
    
#    file = '/Users/j35/results/REF_L_Batch_Run20927_2009y_06m_18d_02h_13mn_08s.txt'
    file = '/Users/j35/results/REF_M_Batch_Run6499_2010y_04m_15d_12h_29mn_59s.txt'
    f = open(file,"r")
    text = f.read() 
    lines = text.split('\n')
    output_files=[]
    for line in lines:
        print('+++++++++++++++++++')
        print(line)
        m = re.findall('--output=([^ ]*)',line)
        if m is not None:
#            output_files.append(m.group(1))
            print('----------------')
#            print(m.groups())
            print(len(m))