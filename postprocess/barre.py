
import os
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.dates import DateFormatter
from mpl_toolkits.axisartist.axislines import SubplotZero


# Reading results files

os.chdir(r'C:\Users\pietro\Desktop\MESS2021\testfolder')
techs = pd.read_csv('techinfo.csv',sep =';',index_col='timestamp',parse_dates=True)
build = pd.read_csv('buildingsenergybalances.csv',sep =';',index_col='timestamp',parse_dates=True)

# Day to be simulated

day = '2019-11-15'

## Working on techinfo.csv

techs['1112p'] = 0
techs['1112n'] = 0
techs['1112p'] = techs.apply(lambda x: x['1112p'] if x['1112']<0 else x['1112'], axis=1)
techs['1112n'] = techs.apply(lambda x: x['1112n'] if x['1112']>0 else x['1112'], axis=1)

techs['1123p'] = 0
techs['1123n'] = 0
techs['1123p'] = techs.apply(lambda x: x['1123p'] if x['1123']<0 else x['1123'], axis=1)
techs['1123n'] = techs.apply(lambda x: x['1123n'] if x['1123']>0 else x['1123'], axis=1)

techs['demtot'] = 0
def f(x):
    return x['1011']+x['1062']+x['1072']
techs['demtot'] = techs.apply(f, axis=1)


## Working on buildingsenergybalances.csv

build['1_EnBalanceElectr'] =  - build['1_EnBalanceElectr']
build['imp'] = 0
build['exp'] = 0
build['imp'] = build.apply(lambda x: x['imp'] if x['1_EnBalanceElectr']<0 else x['1_EnBalanceElectr'], axis=1)
build['exp'] = build.apply(lambda x: x['exp'] if x['1_EnBalanceElectr']>0 else x['1_EnBalanceElectr'], axis=1)

## Building balance df to graph

techsbal  = techs.loc[day][['1031','1112p','1112n','1123p','1123n']]
buildbal  = build.loc[day][['imp','exp']]

balance = techsbal
imp = buildbal[['imp']]
exp = buildbal[['exp']]

balance.insert(5,'imp',imp,True)
balance.insert(6,'exp',exp,True)

## Building demand df to graph

demandbal = techs.loc[day][['1011','1062','1072']]
demandtot = techs.loc[day][['demtot']]

## Balance graph

fig1, ax1 = plt.subplots(figsize=(10,5))

#fig1 = plt.figure(figsize=(10,5))
## a subplot with two additional axis, "xzero" and "yzero". "xzero" is
## y=0 line, and "yzero" is x=0 line.
#ax1 = SubplotZero(fig1, 1, 1, 1)
#fig1.add_subplot(ax1)
## make xzero axis (horizontal axis line through y=0) visible.
#ax1.axis["xzero"].set_visible(True)
#ax1.axis["xzero"].label.set_text("Axis Zero")
## make other axis (bottom, top, right) invisible.
#for n in ["bottom", "top", "right"]:
#    ax1.axis[n].set_visible(False)

ax1.grid(axis='y',ls='dashed',lw=0.5,zorder=1)

ax1.plot(demandtot, c='black',zorder=3)

ax1.bar(x=balance.index,height=balance['1031'], width=40./24/60,zorder=2)
xpos=balance['1031'].copy()
ax1.bar(x=balance.index,height=balance['1112p'], width=40./24/60,bottom=xpos,zorder=2)
xpos=xpos+balance['1112p'].copy()
ax1.bar(x=balance.index,height=balance['1112n'], width=40./24/60,zorder=2)
xneg=balance['1112n'].copy()
ax1.bar(x=balance.index,height=balance['1123p'], width=40./24/60,bottom=xpos,zorder=2)
xpos=xpos+balance['1123p'].copy()
ax1.bar(x=balance.index,height=balance['1123n'], width=40./24/60,bottom=xneg,zorder=2)
xneg = xneg+balance['1123n']
ax1.bar(x=balance.index,height=balance['imp'], width=40./24/60,bottom=xpos,zorder=2)
xpos=xpos+balance['imp'].copy()
ax1.bar(x=balance.index,height=balance['exp'], width=40./24/60,bottom=xneg,zorder=2)
xneg = xneg+balance['exp']

plt.gca().xaxis.set_major_formatter(DateFormatter("%H"))


plt.ylim(-6,8)
plt.xlabel('time [h]')
plt.ylabel('Energy [kWh]')
plt.title(str(day))

line_labels = ['Total demand','PV', 'Batt discharge', 'Batt charge','H2 discharge','H2 charge', 'import','export']
fig1.legend(labels= line_labels, loc=(0.775,0.54))

## Demand graph

#fig2, ax2 = plt.subplots(figsize=(15,7))
#demandbal.plot(kind='bar',ax=ax2,stacked=True,colormap='viridis')
#
#ticks2 = [tick.get_text() for tick in ax2.get_xticklabels()]
#ticks2 = pd.to_datetime(ticks2).strftime('%H')
#ax2.set_xticklabels(ticks2)


