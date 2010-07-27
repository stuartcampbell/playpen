import nxs
from scipy.optimize import leastsq 
from numpy import sqrt, pi
from unit_convert import *
from enthought.tvtk.tools import mlab
import pylab
from copy import deepcopy

class nxl_file(object):
    def __init__(self,filename):
       self.filename=filename
       self.fid=nxs.open(filename,'r')
       self.fid.openpath('/')
       self.entry_dict=self.fid.getentries()

class nxs_file(object):
    def __init__(self,filename):
       self.filename=filename
       self.fid=nxs.open(filename,'r')
       self.fid.openpath('/entry/instrument/')
       self.entry_dict=self.fid.getentries()
       
class mag_bank(object):
   """
      class to manipulate a single bank for pulse field experiments
   """
   def __init__(self,filename,geom_file,banknum,T0=0.0):
       """ initialize class 
           needs filename string
	         nxs file for geometry string
	         banknum int
       """
       self.filename=filename
       self.geom_file=geom_file
       self.banknum=banknum
       self.T0=T0
       self.data_dict={}
       self.data_dict['entry-off']=self.bank_read('entry-off')
       self.data_dict['entry-on']=self.bank_read('entry-on')
       self.data_dict['entry-bad']=self.bank_read('entry-bad')
       self.data_dict['entry-decay']=self.bank_read('entry-decay')
       self.data_dict['entry-invalid']=self.bank_read('entry-invalid')
       self.data_dict['entry-bad_decay']=self.bank_read('entry-bad_decay')
       self.data_dict['entry-invalid_decay']=self.bank_read('entry-invalid_decay')
       self.geom_read()
       self.generate_d()


   def bank_read(self,hist_type):
       """
       function to read in data
       """
       fid=nxs.open(self.filename,'r')
       bankstr='/'+hist_type+'/instrument/bank%d/' %self.banknum
       # read data
       fid.openpath(bankstr+'data')
       datain=fid.getdata()
       #read_tof
       fid.openpath(bankstr+'time_of_flight')
       tof=fid.getdata()
       # done reading file
       fid.close()
       return([datain,tof])
       
   def geom_read(self):
       """
       read the geometry from a nexus file
       """
       fid=nxs.open(self.geom_file,'r')
       bankstr='/entry/instrument/bank%d/' %self.banknum
       fid.openpath(bankstr+'distance')
       self.dist=fid.getdata()
       fid.openpath(bankstr+'polar_angle')
       self.two_theta=fid.getdata()
       fid.openpath(bankstr+'azimuthal_angle')
       self.az=fid.getdata() 
       fid.openpath(bankstr+'origin/translation/distance')
       self.Lvec=fid.getdata()
       self.L0=sqrt((self.Lvec*self.Lvec).sum())
       self.theta0=pi-arccos(abs(self.Lvec[1])/self.L0)
       fid.openpath('/entry/instrument/moderator/distance')
       self.samp_pos=fid.getdata()*-1.0
       fid.openpath(bankstr+'origin/shape/size')
       self.tube_size=fid.getdata()
       # done reading file
       fid.close()
       return self
   def generate_d(self):
       self.L_md=self.samp_pos+self.dist
       d_notof=3956.0/2.0/self.L_md/sin(self.two_theta/2.0)
       for x in self.data_dict.keys():  
          jd=tile(d_notof,(self.data_dict[x][1].shape[0],1,1))
	  jv=tile(self.L_md,(self.data_dict[x][1].shape[0],1,1))
          jv=swapaxes(jv,0,2)
          jv=swapaxes(jv,0,1)
	  jd=swapaxes(jd,0,2)
          jd=swapaxes(jd,0,1)
          jt=tile(self.data_dict[x][1]*1e-6,(d_notof.shape[0],d_notof.shape[1],1))
	  self.data_dict[x].append(jt*jd)
	  self.data_dict[x].append(jv/jt)
	  self.data_dict[x].append(3956.0*jt/jv)
       return self
   def clip(self,newlims):
       newcls=deepcopy(self)
       newcls.az=eval('self.az['+newlims+']')
       newcls.two_theata=eval('self.two_theta['+newlims+']')
       newcls.L_md=eval('self.L_md['+newlims+']')
       idxlst=[0,2,3]
       for x in self.data_dict.keys():            
           for idx in idxlst:
               evalstr='self.data_dict[\'%s\'][%g][%s,:]'%(x,idx,newlims)
	       newcls.data_dict[x][idx]=eval(evalstr)
       return newcls
   
   
   def plot_tsurf(self,tchan,subdict=[]):
        """
	plot all the histograms or some of them according to a dictionary
	
	"""
	idx=1
	pylab.figure()
	subdictlen=len(subdict)
	if subdictlen==0:
	  keylst=self.data_dict.keys()
	else:
	  keylst=subdict
	if mod(len(keylst),2)==0:
	      clnum=len(keylst)/2
	else:
	      clnum=len(keylst)/2+1
        for x in keylst:   
	   pylab.subplot(2,clnum,idx)
	   pylab.pcolor(self.two_theta,self.az,self.data_dict[x][0][:,:,tchan])
	   pylab.colorbar()
	   pylab.xlabel('two_theta')
	   pylab.ylabel('azimuthal')
	   pylab.title(x+'time=%g'%self.data_dict[x][1][tchan])
	   idx=idx+1
	show()
	
   def plot_dsurf(self,tchan,subdict=[]):
        """
	plot all the histograms or some of them according to a dictionary
	
	"""
	idx=1
	pylab.figure()
	subdictlen=len(subdict)
	if subdictlen==0:
	  keylst=self.data_dict.keys()
	else:
	  keylst=subdict
	if mod(len(keylst),2)==0:
	      clnum=len(keylst)/2
	else:
	      clnum=len(keylst)/2+1
	tubenum=arange(self.az.shape[0]-1,-1,-1)
	pixnum=arange(self.az.shape[1])
	pixmat=tile(pixnum,(len(tubenum),1))
	tubemat=tile(tubenum,(len(pixnum),1))
	tubemat=swapaxes(tubemat,0,1)
        for x in keylst:   
	   pylab.subplot(2,clnum,idx)
	   pylab.pcolor(tubemat,pixmat,self.data_dict[x][0][:,:,tchan])
	   pylab.colorbar()
	   pylab.xlabel('two_theta')
	   pylab.ylabel('azimuthal')
	   pylab.title(x+'time=%g'%self.data_dict[x][1][tchan])
	   idx=idx+1
	show()
	
   def plot_tsurf_d(self,tchan,indict):
        for x in indict:
	   pylab.figure()
	   pylab.pcolor(self.two_theta,self.az,self.data_dict[x][0][:,:,tchan])
	   pylab.colorbar()
	   pylab.xlabel('two_theta')
	   pylab.ylabel('azimuthal')
	   pylab.title(x)
	   show() 	   

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
       fid.openpath(bankstr+'distance')
       self.dist=fid.getdata()
       fid.openpath(bankstr+'polar_angle')
       self.two_theta=fid.getdata()
       fid.openpath(bankstr+'azimuthal_angle')
       self.az=fid.getdata() 
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
       # done reading file
       fid.close()
       self.L_md=self.samp_pos+self.dist
       d_notof=3956.0/2.0/self.L_md/sin(self.two_theta/2.0)
       jd=tile(d_notof,(self.tof.shape[0],1,1))
       jd=swapaxes(jd,0,2)
       jd=swapaxes(jd,0,1)
       jt=tile(self.tof*1e-6,(d_notof.shape[0],d_notof.shape[1],1))
       self.d=jt*jd
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
    def plot_dsurf(self,tchan):
        fig=mlab.figure()
        s2=mlab.Surf(self.two_theta,self.az,self.d[:,:,tchan],self.datain[:,:,tchan])
	s2.lut_type='blue-red'
	s2.show_scalar_bar=True
        fig.add(s2)
	fig.objects[0].axis.x_label='two_theta'
	fig.objects[0].axis.y_label='azimuthal'
	fig.objects[0].axis.z_label='dspacing'
	
      
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
