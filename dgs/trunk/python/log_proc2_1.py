from subprocess import PIPE, Popen
from xml.dom.minidom import *
class runinfo(object):
    def __init__(self,instr_name,runnum,filetype='cvinfo'):
         self.instr_name=instr_name
	 self.runnum=runnum
	 self.filetype=filetype
	 #find filename
         findnx_str='findnexus -i %s %d --prenexus' %(instr_name,runnum)
         filestr=Popen([findnx_str],shell=True,stdout=PIPE).communicate()[0]
         filestrlst=filestr.split('\n')
         self.filename=filename_build(instr_name,filestrlst[0],runnum)
	 self.dat=parse(self.filename)
	 self.ids=dict()
	 idlst=self.read_ids()
	 for idx in range(len(idlst)):
	   self.ids[idlst[idx]]=self.read_names(idlst[idx])

    def read_ids(self):
      """
        read the ids from the file
      """
      idlst=[]
      for idx in range(len(self.dat.childNodes[0].childNodes)):
         dei=self.dat.childNodes[0].childNodes[idx]
         if (len(dei.childNodes)>0):
            if (dei.attributes.keys().count('id')>0):
	      idlst.append(dei.attributes['id'].value)
      return idlst
    
       
    def read_names(self, id_name):
       """
       reads names from file given an id and returns a list of the parameter names
       """
       namelst=[] 
       id_idx=self.find_id(id_name)
       dei=self.dat.childNodes[0].childNodes[id_idx]
       for idx in range(len(dei.childNodes)):
          if dei.childNodes[idx].hasAttributes():
            if (dei.childNodes[idx].attributes.keys().count('name')>0):
	      namelst.append(dei.childNodes[idx].attributes['name'].value)
       return namelst
    def find_id(self,id_name):
       """
       """
       for idx in range(len(self.dat.childNodes[0].childNodes)):
          dei=self.dat.childNodes[0].childNodes[idx]
          if (len(dei.childNodes)>0):
             if (dei.attributes.keys().count('id')>0):
	       if dei.attributes['id'].value==id_name:
	         out=idx
       return out
    def find_parm(self,id_idx,parm_name):
       """
       """
       out=-1
       for idx in range(len(self.dat.childNodes[0].childNodes[id_idx].childNodes)):
          dei=self.dat.childNodes[0].childNodes[id_idx].childNodes[idx]
          if dei.hasAttributes():
             if dei.attributes.keys().count('name')>0:
	       #print dei.attributes['name'].value
	       if dei.attributes['name'].value==parm_name:
	         out=idx
       return out  
    def read_parm(self,id_name,parm_name):
      """
      """
      id_idx=self.find_id(id_name)
      parm_idx=self.find_parm(id_idx,parm_name)
      #print parm_idx
      dei=self.dat.childNodes[0].childNodes[id_idx].childNodes[parm_idx].childNodes[1]
      return dei.data
    
    def plot_var(self,id_name,parm_name,idxflag=True):
       """
          plots a variable
       """
       from numpy import array,arange
       from pylab import plot
       istrg=self.read_parm(id_name,parm_name)
       out=strg2lst(istrg)
       y=array(out[2])
       if idxflag:
          x=arange(len(y))
       else:
          x=out[1]
       figure()
       title("%s %s" %(self.filename,id_name))
       plot(x,y)
       ylabel(parm_name)
       xlabel('index')
       show()
    def stat_var(self,id_name,parm_name):
        """
           outputs the mean standard deviation min and maximum of a parameter
        """
        from numpy import array
        istrg=self.read_parm(id_name,parm_name)
        out=strg2lst(istrg)
        aout=array(out[2])
        return [self.runnum,aout.mean(),aout.std(),aout.min(),aout.max()]   

#----------------------------------
#other functions
#------------------------------------
def filename_build(instrument,fldir,runnum,filetype='cvinfo'):
     """
       given an instrument (short name), proposal and run number, return, and filetype
       return a path string to the file
     """
     # possible instrument names
     posinst=set(['ARCS','SEQ','CNCS','BASIS'])
     if not set([instrument]).issubset(posinst):
          raise ValueError, "must be a short instrument name"
     instrun='%s_%i' %(instrument,runnum)
     filestr=fldir+r'/'+instrun+'_'+filetype+'.xml'
     return filestr
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
    dt=array(dt)-dt[0]
    out=[outidx,dt,temp]
    return out    
def savelst(lstin,filename):
    """
    function to save list in csv format
    """
    b=map(lambda lst: "%d,%g,%g,%g,%g\n" %(lst[0],lst[1],lst[2],lst[3],lst[4]),lstin)
    fid=open(filename,'w')
    fid.writelines(b)
    fid.close()
def mult_run_report(instr_name,runnums,id_name,parm_name):
     """
     """
     outlst=[]
     for idx in range(len(runnums)):
	classnm=instr_name+"_%d" %runnums[idx]
	classstr=classnm+"=runinfo(\'%s\',%d)"%(instr_name,runnums[idx])
	exec(classstr)
	exec("outlst.append(%s.stat_var(\'%s\',\'%s\'))" %(classnm,id_name,parm_name))
     return(outlst)
	
	
     
