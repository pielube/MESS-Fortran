
import os
import pandas as pd
import matplotlib.pyplot as plt

os.chdir(r'C:\Users\pietro\Desktop\MESS2021\testfolder')
NPV = pd.read_csv('NPVcompletedata.csv',sep =',',header=None)


plt.plot(NPV[0], label='case0')
plt.plot(NPV[1], label='case1')
plt.plot(NPV[2], label='delta')

plt.legend()