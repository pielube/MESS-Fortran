

import os
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import graphics_dict as gd

os.chdir(r'C:\Users\pietro\Desktop\MESS2021\testfolder')
techs = pd.read_csv('techinfo.csv',sep =';',index_col='timestamp',parse_dates=True)

sns.set_theme(rc=gd.slideshow.dict)





color1 = 'tab:blue'
color2 = 'tab:green'
color3 = 'tab:red'

ax=techs.loc['2019', '1122'].plot(figsize=(20,8),color=color1,
            alpha=0.5,label = 'SoC hydrogen tank',linewidth=3)



ax.grid(which='minor',alpha=0.2)
ax.xaxis.set_ticks_position('none')
ax.xaxis.label.set_visible(False)
#ax.set_xlim('2019-05-25','2019-05-31')
#ax.set_ylim(0,1)

ax.set_ylabel('SoC [-]')

ax.legend(loc=(0.05,0.8),fontsize=22,ncol=3)

plt.savefig('figura2.png',dpi=200, bbox_inches='tight')

