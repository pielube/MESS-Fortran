import numpy as np
import matplotlib.pyplot as plt

plt.rc('font', size=16)
plt.rc('axes', titlesize=19)     # fontsize of the axes title
plt.rc('axes', labelsize=19)    # fontsize of the x and y labels


file=np.loadtxt('output_case1.out',usecols=range(9))
el_load=np.loadtxt('demand_Wel.dat',skiprows=1,usecols=range(2))
el_load=el_load[:,1]

giorno=175
file=file[24*giorno+1:24*giorno+24+1]
el_load=el_load[24*giorno+1:24*giorno+24+1]
PV=file[:,0]
charge=np.zeros(24)
discharge=np.zeros(24)
for i in range (24):
    if file[i,2]<0:
        charge[i]=file[i,2]
    else:
        discharge[i]=file[i,2]
import_elec=file[:,3]
export_elec=file[:,4]
aircond    =file[:,5]
pump       =file[:,6]


x=['\n','','  ','   ','    ','6','     ','      ','       ',' \n','  \n','12','    \n','     \n','      \n','       \n','\n\n','18','  \n\n','   \n\n','    \n\n','     \n\n','      \n\n','24']
labels=x
width = 0.4       # the width of the bars: can also be len(x) sequence

#fig, ax = plt.subplots(1, 1, figsize=(10,3))
fig = plt.figure(figsize=(12, 10))

from mpl_toolkits.axisartist.axislines import SubplotZero
ax = SubplotZero(fig, 1, 1, 1)
fig.add_subplot(ax)
ax.axis["xzero"].set_visible(True)
ax.axis( zorder=3)
#ax.axis["xzero"].label.set_text("hour")
for n in ["bottom", "top", "right"]:
    ax.axis[n].set_visible(False)

#ax.grid()
#ax.grid(axis='y')
ax.grid(axis='y',ls='dashed',lw=0.5,zorder=0)
#ax.bar(range(len(x)), y, width=0.3, align='center', color='skyblue', zorder=3)

l1=ax.bar(labels, PV, width, zorder=3)
l2=ax.bar(labels,import_elec, width,bottom=PV, zorder=3)
l3=ax.bar(labels,-export_elec, width,   zorder=1)
l4=ax.bar(labels, discharge, width,  bottom=PV+import_elec, zorder=3)
l5=ax.bar(labels, charge, width,bottom=export_elec,   zorder=1)



#l6=plt.plot(x,el_load,color='0.5', zorder=3)
l7=plt.plot(x,el_load+pump+aircond,'k', zorder=3)          
#plt.plot([-1,24],[0,0],'k')
# =============================================================================
# plt.plot(0,7,'w')
# plt.plot(0,-1,'w')
# 
# =============================================================================
#plt.legend(loc='upper left')#'best')
#plt.xlabel("hour")
plt.xlim(-0.5, 23.5)
plt.ylim(-4,6)
plt.text(22, -0.8,'time [h]')
plt.ylabel("Energy [kWh] ")
plt.title('Day '+str(giorno))

line_labels = ['Total el demand',"PV", "import. elec.", "export. elec.",'ESS discharge','ESS charge']
fig.legend(              # List of the line objects
           labels= line_labels,       # The labels for each line
           loc=(0.72,0.72)        # Position of the legend
           )      # Title for the legend



# Adjust the scaling factor to fit your legend text completely outside the plot
# (smaller value results in more space being made for the legend)
#plt.subplots_adjust(right=0.79)

name1 = 'grafico'+str(giorno)+'.png'
name2 = 'grafico'+str(giorno)+'.svg'

plt.savefig(name1,dpi=1000,bbox_inches='tight')
plt.savefig(name2,format='svg',bbox_inches='tight')

#plt.show()