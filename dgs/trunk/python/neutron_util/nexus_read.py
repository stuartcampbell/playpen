import nxs
from scipy.optimize import leastsq 
from numpy import sqrt
from unit_convert import *

class nxs_file(object):
    def __init__(self,filename):
       self.filename=filename
       self.fid=nxs.open(filename,'r')
       self.fid.openpath('/entry/instrument/')
       self.entry_dict=self.fid.getentries()
       
class det_bank(object):
    def __init__(self,filename,banknum,Ei,T0):
       self.filename=filename
       self.banknum=banknum
       self.Ei=Ei
       self.T0=T0
       self.bank_read()
       
    
    def bank_read(self):
       fid=nxs.open(self.filename,'r')
       bankstr='/entry/instrument/bank%d/' %self.banknum
       fid.openpath(bankstr+'data')
       self.datain=fid.getdata()
       fid.openpath(bankstr+'origin/translation/distance')
       self.Lvec=fid.getdata()
       self.L0=sqrt((self.Lvec*self.Lvec).sum())
       self.theta0=pi-arccos(abs(self.Lvec[1])/self.L0)
       fid.openpath('/entry/instrument/moderator/distance')
       self.samp_pos=fid.getdata()*-1.0
       fid.openpath(bankstr+'origin/shape/size')
       self.tube_size=fid.getdata()
       fid.openpath(bankstr+'time_of_flight')
       self.tof=fid.getdata()
       return self
       
       
    def fit_bank(self,tlims):
       """
       """
       datatmp=self.datain.sum(0)
       ytmp=linspace(-self.tube_size[1]/2.0,self.tube_size[1]/2.0,datatmp.shape[0])
       tflt=(self.tof>tlims[0])&(self.tof<tlims[1])
       datatmp=datatmp[:,tflt]
       tin=self.tof[tflt]
       ymat=transpose(kron(ones((datatmp.shape[1],1)),ytmp))
       tmat=kron(ones((datatmp.shape[0],1)),tin)
       pts2use=find(datatmp>0)
       tlst=tmat.flatten()[pts2use]
       ylst=ymat.flatten()[pts2use] 
       datlst=datatmp.flatten()[pts2use]
       fout=leastsq(errorfunc,[self.L0,self.theta0],args=(tlst*1e-6,-ylst,self.Ei,self.T0,datlst,self.samp_pos),full_output=1)
       self.ylst=ylst
       self.tlst=tlst
       self.datlst=datlst
       return fout
    def plot_bank(self):
       """
       """
       imshow(self.datain.sum(0),extent=[min(self.tof),max(self.tof),-self.tube_size[1]/2.0,self.tube_size[1]/2.0],aspect='auto')
       title('bank %g'%self.banknum)
    def plot_ft(self,L0,theta0):
        x=vt_f([L0,theta0],self.ylst)/E2V(self.Ei)+self.samp_pos/E2V(self.Ei)+self.T0
	plot(x*1e6,self.ylst,'y')
       


def vt_f(p,y):
   """
   """
   return sqrt(p[0]*p[0]+y*y-2*p[0]*y*cos(p[1]))    
def errorfunc(p,tin,y,E,t0,w,L12): 
   """
   """
   return (vt_f(p,y)/E2V(E)+L12/E2V(E)+t0-tin)/w
def mult_bank_fit(banks):
    out=[]
    for idx in banks:
       bank=det_bank('SEQ_1044.nxs',idx,21.45,22.36e-6)
       fout=bank.fit_bank([12500,12800])
       out.append([fout[0],[idx,bank.L0,bank.theta0]])
    return out
def plot_banks(filename,bnks):
    for idx in bnks:
      bank=det_bank(filename,idx,0,0)
      figure()
      bank.plot_bank()
