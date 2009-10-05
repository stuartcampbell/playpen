# routines to read an process runinfo files
#GEG 6.2009
#SNS
from xml.dom.minidom import *
def filename_build(instrument,proposal,runnum,filetype='cvinfo'):
     """
       given an instrument (short name), proposal and run number, return, and filetype
       return a path string to the file
     """
     # possible instrument names
     posinst=set(['ARCS','SEQ','CNCS','BASIS'])
     if not set([instrument]).issubset(posinst):
          raise ValueError, "must be a short instrument name"
     instrun='%s_%i' %(instrument,runnum)
     filestr=r'/'+instrument+r'-DAS-FS/'+proposal+r'/'+instrun+r'/'+instrun+'_'+filetype+'.xml'
     return filestr
def runinfo(instrument,proposal,runnum):
     """
       runinfo(instrument,proposal,runnum)
       instrument is the short insturment name, proposal is the poroposal ID, 
       runnum is the runnumber
       returns a dictionary containing info from the run info file
     """
     # possible instrument names
     posinst=set(['ARCS','SEQ','CNCS','BASIS'])
     if not set([instrument]).issubset(posinst):
          raise ValueError, "must be a short instrument name"
     instrun='%s_%i' %(instrument,runnum)
     filestr=r'/'+instrument+r'-DAS-FS/'+proposal+r'/'+instrun+r'/'+instrun+'_runinfo.xml'
     return read_info(filestr)
def runinfomult(instrument,proposal,runnums):
     """
       runinfomult(instrument,proposal,runnums)
       retruns a list of dicitionaries corresponding to several runs
     """
     infolst=[]
     for idx in range(len(runnums)):
        infolst.append(runinfo(instrument,proposal,runnums[idx]))
     return infolst
     
def read_info(filename):
      vainfo={}
      dat=parse(filename)
      akeys=dat.documentElement.attributes.keys()
      for aitem in akeys:
          vainfo[aitem]=dat.documentElement.attributes[aitem].value
       
      # get time and date
      di=dat.childNodes[0].childNodes[1]
      for idx in range(len(di.childNodes)):
           locName=di.childNodes[idx].localName
           if locName=='StartTime':
	        evalstr="vainfo['"+locName+"']= '%s'" %(di.childNodes[idx].childNodes[0].nodeValue)
		exec(evalstr)
	   if locName=='StopTime':
	        evalstr="vainfo['"+locName+"']= '%s'" %(di.childNodes[idx].childNodes[0].nodeValue)
		exec(evalstr)
      #operational info
      oi=dat.childNodes[0].childNodes[3]
      for idx in range(len(oi.childNodes)):
          locName=oi.childNodes[idx].localName
      	  if locName=='PCurrent':
	  	evalstr="vainfo['"+locName+"']= %s" %(oi.childNodes[idx].childNodes[0].nodeValue)
		exec(evalstr)
      #Sample info
      #dat.childNodes[0].childNodes[5]
      #General info
      gi=dat.childNodes[0].childNodes[7]
      for idx in range(len(gi.childNodes)):
          locName=gi.childNodes[idx].localName
          if locName=='Title':
	       evalstr="vainfo['"+locName+"']= '%s'" %(gi.childNodes[idx].childNodes[0].nodeValue)
	       exec(evalstr)
	  if locName=='Notes':
	       evalstr="vainfo['"+locName+"']= '%s'" %(gi.childNodes[idx].childNodes[0].nodeValue)
	       exec(evalstr)
	  
	       
      # Detector info
      dei=dat.childNodes[0].getElementsByTagName(u'DetectorInfo')[0].getElementsByTagName('Scattering')[0]
      timechannel={}
      akeys=dei.getElementsByTagName('NumTimeChannels')[0].attributes.keys()
      for aitem in akeys:
          timechannel[aitem]=dei.getElementsByTagName('NumTimeChannels')[0].attributes[aitem].value
      vainfo['timing']=timechannel	       
      return vainfo


def strg2lst(instr):
    """
    """
    from time import mktime, strptime
    lstr=instr.splitlines()
    dt=[]
    temp=[]
    for idx in range(len(lstr)):
       if len(lstr[idx])>0:
	  ltmp=lstr[idx].split()
	  #print ltmp
	  tmpdate=ltmp[0]
	  tmptime=ltmp[1].split('.')
	  temp.append(float(ltmp[2]))
	  tuptime=strptime(tmpdate+' '+tmptime[0],"%Y-%m-%d %H:%M:%S")
	  dt.append(mktime(tuptime)+float('.'+tmptime[1]))
    outidx=range(len(temp))
    out=[outidx,dt,temp]
    return out
    
def read_ids(filename):
    """
    """
    idlst=[]
    dat=parse(filename)
    for idx in range(len(dat.childNodes[0].childNodes)):
       dei=dat.childNodes[0].childNodes[idx]
       if (len(dei.childNodes)>0):
          if (dei.attributes.keys().count('id')>0):
	    idlst.append(dei.attributes['id'].value)
    return idlst
def read_names(filename, id_name):
     """
     """
     namelst=[] 
     dat=parse(filename)
     id_idx=find_id(dat,id_name)
     dei=dat.childNodes[0].childNodes[id_idx]
     for idx in range(len(dei.childNodes)):
        if dei.childNodes[idx].hasAttributes():
          if (dei.childNodes[idx].attributes.keys().count('name')>0):
	    namelst.append(dei.childNodes[idx].attributes['name'].value)
     return namelst
def read_parm(filename,id_name,parm_name):
    """
    """
    dat=parse(filename)
    id_idx=find_id(dat,id_name)
    parm_idx=find_parm(dat,id_idx,parm_name)
    print parm_idx
    dei=dat.childNodes[0].childNodes[id_idx].childNodes[parm_idx].childNodes[1]
    return dei.data
     
def find_id(dat,id_name):
    """
    """
    for idx in range(len(dat.childNodes[0].childNodes)):
       dei=dat.childNodes[0].childNodes[idx]
       if (len(dei.childNodes)>0):
          if (dei.attributes.keys().count('id')>0):
	    if dei.attributes['id'].value==id_name:
	      out=idx
    return out	    
def find_parm(dat,id_idx,parm_name):
    """
    """
    out=-1
    for idx in range(len(dat.childNodes[0].childNodes[id_idx].childNodes)):
       dei=dat.childNodes[0].childNodes[id_idx].childNodes[idx]
       if dei.hasAttributes():
          if dei.attributes.keys().count('name')>0:
	    #print dei.attributes['name'].value
	    if dei.attributes['name'].value==parm_name:
	      out=idx
    return out	             
	  
def savelst(lstin,filename):
    """
    function to save list in csv format
    """
    b=map(lambda lst: "%d,%g,%g,%g,%g\n" %(lst[0],lst[1],lst[2],lst[3],lst[4]),lstin)
    fid=open(filename,'w')
    fid.writelines(b)
    fid.close()
def read_var(instrument,proposal,runnum,id_name,parm_name):
     """
     outputs the mean standard deviation min and maximum of a parameter
     """
     from numpy import array
     filename=filename_build(instrument,proposal,runnum,filetype='cvinfo')
     istrg=read_parm(filename,id_name,parm_name)
     out=strg2lst(istrg)
     aout=array(out[2])
     return [runnum,aout.mean(),aout.std(),aout.min(),aout.max()]
def plot_var(instrument,proposal,runnum,id_name,parm_name):
    """
    plots a variable
    """
    from numpy import array,arange
    from pylab import plot
    filename=filename_build(instrument,proposal,runnum,filetype='cvinfo')
    istrg=read_parm(filename,id_name,parm_name)
    out=strg2lst(istrg)
    y=array(out[2])
    x=arange(len(y))
    figure()
    title("%s %s" %(filename,id_name))
    plot(x,y)
    ylabel(parm_name)
    xlabel('index')
    show()
def list_var(instrument,proposal,runnum,id_name,parm_name):
    filename=filename_build(instrument,proposal,runnum,filetype='cvinfo')
    istrg=read_parm(filename,id_name,parm_name)
    return istrg
        
def read_var_mult(instrument,proposal,runnums,parm_name):
     infolst=[]
     for idx in range(len(runnums)):
        infolst.append(read_var(instrument,proposal,runnums[idx],parm_name))
     return infolst
