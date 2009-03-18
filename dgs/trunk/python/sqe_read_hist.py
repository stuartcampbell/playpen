from numpy import array, zeros, nan
from histogram import histogram
class SNS_dgs():
    def __init__(self):
        self.name=''
        self.Q=array([])
        self.E=array([])
        self.I=array([])
        self.err=array([])
    def read_sqe(self,filename):
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
        h=histogram(histname,[('Q',self.Q),('E',self.E)],self.I.transpose(),(self.err*self.err).transpose())
        return h

      
    
        
