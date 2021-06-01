
import os
import pandas as pd
import seaborn as sns
import matplotlib.dates as mdates

os.chdir(r'C:\Users\pietro\Desktop\MESS2021\testfolder')
buildings = pd.read_csv('buildingsenergybalances.csv',sep =';',index_col='timestamp',parse_dates=True)

sns.set(rc={'figure.figsize':(11,6)})

#myfmt = mdates.DateFormatter('%d')
myfmt = mdates.DateFormatter('%d')


columns = []
for ii in range(1):
    columns.append(str(ii+1)+'_EnBalanceElectr')

start = '2020-01-01 00:00:00'
stop  = '2020-12-31 23:00:00'
axes = buildings[columns].plot(xlim=None,ylim=(-5,5),subplots=True)
for ax in axes:
    ax.xaxis.set_major_formatter(myfmt)
    ax.set_ylabel('[kWh]')


    
