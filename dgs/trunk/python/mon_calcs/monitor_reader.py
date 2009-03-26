LM=[]
fid=nxs.open('/SNS/SEQ/shared/SEQ_118.nxs','r')
# get distance of sample to moderator and change into distance from moderator to sample 
fid.openpath('/entry/instrument/moderator/distance')
Lsam=fid.getdata()*-1.0
# get moderator distances
fid.openpath('/entry/monitor1/distance')
LM.append(fid.getdata())
fid.openpath('/entry/monitor2/distance')
LM.append(fid.getdata())
#change monitor distances from relative to sample to relative to moderator 
LM=LM+Lsam
fid.openpath('/entry/monitor2/time_of_flight')
t2=fid.getdata()
fid.openpath('/entry/monitor1/time_of_flight')
t1=fid.getdata()
fid.openpath('/entry/monitor2/data')
d2=fid.getdata()
fid.openpath('/entry/monitor1/data')
d1=fid.getdata()
