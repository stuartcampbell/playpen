#python module to read SNS generated sqe files and put them in a Caltech Histogram
from numpy import array, zeros, nan, isnan
from histogram import histogram
#class to read SNS generated sqe files into histograms for use with Dr chops
class SNS_dgs():
    def __init__(self):
        self.name=''
        self.Q=array([])
        self.E=array([])
        self.I=array([])
        self.err=array([])
    def read_sqe(self,filename):
    	#method to read sqe file
	#filename is the full path to the sqe file
        fid=open(filename,'r')
        lines=fid.readlines()
        fid.close()
        nEvals=int(lines[1])
        nQvals=int(lines[3])
        Evalend=5+nEvals
        Qvalst=Evalend+1
        Qvalend=Qvalst+nQvals
        Evals=array(map(float,lines[5:Evalend]))
        Qvals=array(map(float,lines[Qvalst:Qvalend]))
        I=zeros((nEvals,nQvals))
        err=zeros((nEvals,nQvals))
        idxstart=Qvalend
        for idx in range(len(Qvals)):
            idxstart=idxstart+1
            for idx2 in range(len(Evals)):
                tmp=lines[idxstart].split()
                I[idx2,idx]=eval(tmp[0])
                err[idx2,idx]=eval(tmp[1])
                idxstart=idxstart+1
        self.Q=Qvals
        self.E=Evals
        self.I=I
        self.err=err
        self.name=filename
    def sqe_hist(self,histname):
    	#function to create the histogram
        h=histogram(histname,[('Q',self.Q),('E',self.E)],self.I.transpose(),(self.err*self.err).transpose())
        return h
def powfile2hist(instr,runnum,userid,remove_nans=1):
	#function that takes an instruemnt a runnum and a user name and returns a histogram 
	filename='/SNS/users/%s/results/%s/%d/%s_%d.sqe' %(userid,instr,runnum,instr,runnum)
	print filename
	datname='dat_%d'%(runnum)
	histname='h_%d_qe'%(runnum)
	exec(datname+r'=SNS_dgs()')
	exec(datname+r'.read_sqe(filename)')
	exec(histname+'='+datname+r".sqe_hist('"+histname+"')")
	if remove_nans:
	   exec(histname+'=removenans('+histname+')')
	return(eval(histname))
def removenans(hist):
     hist.I[isnan(hist.I)]=0.0
     hist.E2[isnan(hist.I)]=0.0
     return hist
     

      
    
        
