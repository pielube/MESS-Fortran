
import os
import pandas as pd
import seaborn as sns

os.chdir(r'C:\Users\pietro\Desktop\MESS2021\testfolder')
techs = pd.read_csv('techinfo.csv',sep =';',index_col='timestamp',parse_dates=True)

sns.set(rc={'figure.figsize':(11,4)})

#techs['161'].plot(linewidth=0.5)

#start = '2020-06-15 00:00:00'
#stop  = '2020-06-16 00:00:00'
#cols_plot = ['131','161']
#axes = techs[cols_plot].plot(xlim=(start,stop),marker='.',alpha=0.5,linestyle='None',figsize=(11, 9), subplots=True)
#axes[0].set_ylabel('PV energy [kWh]')
#axes[1].set_ylabel('SoC')
##for ax in axes:
##    ax.set_ylabel('State of Charge')

ax = techs.loc['2020', '171'].plot()
ax.xaxis.label.set_visible(False)
ax.set_ylabel('State of Charge')

#ax = techs.loc['2020-08-22', '131'].plot()
#ax.set_ylabel('PV energy [kWh]')

