
import os
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import graphics_dict as gd

os.chdir(r'C:\Users\pietro\Desktop\MESS2021\testfolder')
techs = pd.read_csv('techinfo.csv',sep =';',index_col='timestamp',parse_dates=True)

sns.set_theme(rc=gd.slideshow.dict)

fig,ax = plt.subplots(figsize=(15,8),dpi=50)


ax1 = ax.twinx() 


color1 = 'tab:blue'
color2 = 'tab:green'
color3 = 'tab:red'

techs.loc['2019', '1121'].plot(ax=ax1, color=color1,label = 'SoC hydrogen tank',linewidth=4,zorder=1)
#techs.loc['2019-06', '161'].plot(ax=ax1, c=color2,label = 'SoC battery',linewidth=3,zorder=1)
#techs.loc['2019-06', '131'].plot(ax=ax,color=color3,kind='area',label='PV production',stacked=False,zorder=0)


ax.grid(which='minor',alpha=0.2)
ax1.grid(which='both',alpha=0.)
ax.xaxis.set_ticks_position('none')
ax.xaxis.label.set_visible(False)
ax.set_xlim('2019-06-03','2019-06-10')
ax1.set_ylim(0,1)
ax.set_ylim(0,8)

ax1.set_ylabel('SoC [-]')
ax.set_ylabel('Energy [kWh]')

fig.legend(loc=(0.18,0.02),fontsize=22,ncol=3)

fig.savefig('figura1.png',dpi=200, bbox_inches='tight')


