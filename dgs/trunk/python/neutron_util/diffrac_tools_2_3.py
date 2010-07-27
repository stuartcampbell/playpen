import nxs
import os
from scipy.optimize import leastsq 
from numpy import sqrt
from unit_convert import *
from enthought.tvtk.tools import mlab
import pylab
from copy import deepcopy
from subprocess import PIPE, Popen

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
   def __init__(self,filename,geom_file,banknum,T0=0.0,PC=True):
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
       self.Iunit='Counts'
       fid=nxs.open(self.filename,'r')
       #cycle through the available histograms
       entrys=fid.getentries()
       for x in entrys.keys():
           self.data_dict[x]=self.bank_read(x,fid,PC)
       #read in the geometry information
       self.geom_read()
       self.generate_d()
       fid.close()
   def __add__(self,y):
       """
       function that adds the data and the proton charge
       checks to see that all keys are identical
       """
       #check to see if data_dict keys are identical without caring about order
       #use sets
       s1=set(self.data_dict.keys())
       s2=set(y.data_dict.keys())
       s1ms2=s1.symmetric_difference(s2)
       if len(s1ms2)>0:
          raise RuntimeError,"data dictionary mismatch"
       iout=deepcopy(self)
       iout.filename='%s+%s'%(self.filename,y.filename)
       keys2add=['data','PC']
       for idx in self.data_dict.keys():
	    for idx2 in keys2add:
	       iout.data_dict[idx][idx2]=self.data_dict[idx][idx2]+y.data_dict[idx][idx2]
       return iout   
	  
          

   def bank_read(self,hist_type,fid,PC):
       """
       function to read in data
       """
       bankstr='/'+hist_type+'/instrument/bank%d/' %self.banknum
       # read data
       fid.openpath(bankstr+'data')
       datain=fid.getdata()
       #read_tof
       fid.openpath(bankstr+'time_of_flight')
       tof=fid.getdata()
       if PC:
          pc_str='/'+hist_type+'/proton_charge'
	  fid.openpath(pc_str)
	  proton_charge=fid.getdata()
       # done reading file
       return({'data':datain,'tof':tof,'PC':proton_charge})
       
   def geom_read(self):
       """
       read the geometry from a nexus file
       """
       fid=nxs.open(self.geom_file,'r')
       bankstr='/entry/instrument/bank%d/' %self.banknum
       fid.openpath(bankstr+'distance')
       self.dist=fid.getdata()
       ntubes=self.dist.shape[0]
       npix=self.dist.shape[1]
       fid.openpath(bankstr+'origin/orientation/value')
       dircostmp=fid.getdata()
       self.dircos=gen_dir_cos_matrix(dircostmp)
       fid.openpath(bankstr+'origin/shape/size')
       self.tube_size=fid.getdata()
       fid.openpath(bankstr+'origin/translation/distance')
       self.Lvec=fid.getdata()
       self.L0=sqrt((self.Lvec*self.Lvec).sum())
       fid.openpath(bankstr+'x_pixel_offset')
       tctemp=fid.getdata()
       tbmins=tctemp-.0127
       tbmaxs=tctemp+.0127
       self.pixbounds=arange((npix+1))*self.tube_size[1]/(npix+1)-self.tube_size[1]/2.0+self.Lvec[1]
       self.tubebounds=arange((ntubes+1))*0.0254-8.0*0.0254/2.0
       #predefine arry
       unrottbnds=zeros((len(self.tubebounds),3))
       #populate x of array
       unrottbnds[:,0]=self.tubebounds
       #predefine matrix for rotated tube bounds to go in
       rottbnds=zeros((len(self.tubebounds),3))
       for idx in range(len(self.tubebounds)):
           rottbnds[idx,:]=dot(self.dircos,unrottbnds[idx,:])
       self.tubebndsmat=rottbnds+self.Lvec
       ttbends=tile(self.tubebndsmat,(len(self.pixbounds),1,1))
       tpixbnds=tile(self.pixbounds,(len(self.tubebndsmat),1))
       tpixbnds=tpixbnds.swapaxes(0,1)
       ttbends[:,:,1]=ttbends[:,:,1]+tpixbnds
       xylen=sqrt(ttbends[:,:,0]*ttbends[:,:,0]+ttbends[:,:,1]*ttbends[:,:,1])
       self.two_theta=arctan(xylen/ttbends[:,:,2])
       self.az=arccos_quad(ttbends[:,:,0],xylen,ttbends[:,:,1])
       self.dist=sqrt(xylen*xylen+ttbends[:,:,2]*ttbends[:,:,2])
      #fid.openpath(bankstr+'polar_angle')
      # self.two_theta=fid.getdata()
      # fid.openpath(bankstr+'azimuthal_angle')
      # self.az=fid.getdata() 
       
       self.theta0=pi-arccos(abs(self.Lvec[1])/self.L0)
       fid.openpath('/entry/instrument/moderator/distance')
       self.samp_pos=fid.getdata()*-1.0
      
       # done reading file
       fid.close()
       return self
   def generate_d(self):
       self.L_md=self.samp_pos+self.dist
       d_notof=3956.0/2.0/self.L_md/sin(self.two_theta/2.0)
       for x in self.data_dict.keys():  
          jd=tile(d_notof,(self.data_dict[x]['tof'].shape[0],1,1))
	  jv=tile(self.L_md,(self.data_dict[x]['tof'].shape[0],1,1))
          jv=swapaxes(jv,0,2)
          jv=swapaxes(jv,0,1)
	  jd=swapaxes(jd,0,2)
          jd=swapaxes(jd,0,1)
          jt=tile(self.data_dict[x]['tof']*1e-6,(d_notof.shape[0],d_notof.shape[1],1))
	  self.data_dict[x]['dspacing']=jt*jd
	  self.data_dict[x]['velocity']=jv/jt
	  self.data_dict[x]['wavelength']=(3956.0*jt/jv)
       return self
   def normalize(self,denom={'PC':1e-12}):
        """
	 function to nomralize data 
	 Only proton charge is currently implemented
	"""
        if self.Iunit!='Counts':  
	  raise RuntimeError, "to Normalize the units must be in counts"
	try: 
	  denom.keys().index('Monitor')
	  raise RuntimeError, "Monitor normalizaiton not supported yet"
	except:
	  pass
	try: 
	  denom.keys().index('PC')    
	except:
	   raise RuntimeError, "Must use either Montor or PC"
	iout=deepcopy(self)
	for idx in self.data_dict.keys():   
	   iout.data_dict[idx]['data']=self.data_dict[idx]['data']/self.data_dict[idx]['PC']/denom['PC']
	iout.Iunit='Counts/%g pC'%denom['PC']
	return iout
	    
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
	   pylab.pcolor(self.two_theta,self.az,self.data_dict[x]['data'][:,:,tchan])
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
	   pylab.pcolor(tubemat,pixmat,self.data_dict[x]['data'][:,:,tchan])
	   pylab.colorbar()
	   pylab.xlabel('tube')
	   pylab.ylabel('pixel')
	   pylab.title(x+'time=%g'%self.data_dict[x][1][tchan])
	   idx=idx+1
	show()
   def plot_lambdavstube(self,pixlims=[0,127],subdict=[],rwn=2,climlst=[],PCnorm=False):
       """
       plot the wavelength vs. tube over certain pixel limits
       """
       idx=1
       fig=pylab.figure()
       subdictlen=len(subdict)
       if subdictlen==0:
	  keylst=self.data_dict.keys()
       else:
	  keylst=subdict
       if mod(len(keylst),rwn)==0:
	      clnum=len(keylst)/rwn
       else:
	      clnum=len(keylst)/rwn+1
       for x in keylst:
	   Itmp=self.data_dict[x]['data'][:,pixlims[0]:pixlims[1],:].sum(axis=1)
	   xtmp=self.data_dict[x]['wavelength'][pixlims[0]:pixlims[1],:,:].mean(axis=0)
	   xtmp=(xtmp[:,1:]+xtmp[:,:-1])/2.0
	   print xtmp#
	   tube=tile(arange(9),(self.data_dict[x]['data'].shape[2],1))
	   tube=swapaxes(tube,0,1) 
	   print tube.shape  
	   pylab.subplot(rwn,clnum,idx)
	   if PCnorm:
	       Itmp=Itmp/self.data_dict[x]['PC']
	   #print xtmp.shape
	   #print tube.shape
	   #print Itmp.shape
	   pylab.pcolor(xtmp,tube,Itmp)
	   if len(climlst)>0:
	     if len(climlst)==len(subdict):
	       pylab.clim(climlst[keylst.index(x)])
	   pylab.colorbar()
	   pylab.xlabel('$\lambda(\AA)$')
	   pylab.ylabel('tube number')
	   pylab.title(x)
	   idx=idx+1
       fig.text(0.5,0.95,self.filename,horizontalalignment='center')
       show()	   
   def plot_dvstube(self,pixlims=[0,127],subdict=[],rwn=2):
       """
       plot the dspacing vs. tube over certain pixel limits
       """
       idx=1
       pylab.figure()
       subdictlen=len(subdict)
       if subdictlen==0:
	  keylst=self.data_dict.keys()
       else:
	  keylst=subdict
       if mod(len(keylst),rwn)==0:
	      clnum=len(keylst)/rwn
       else:
	      clnum=len(keylst)/rwn+1
       for x in keylst:
	   Itmp=self.data_dict[x]['data'][:,pixlims[0]:pixlims[1],:].sum(axis=1)
	   xtmp=self.data_dict[x]['dspacing'][:,pixlims[0]:pixlims[1],:].mean(axis=1)
	   xtmp=(xtmp[:,1:]+xtmp[:,:-1])/2.0
	   tube=tile(arange(8),(140,1))
	   tube=swapaxes(tube,0,1)   
	   pylab.subplot(rwn,clnum,idx)
	   pylab.pcolor(xtmp,tube,Itmp)
	   pylab.colorbar()
	   pylab.xlabel('$\d(\AA)$')
	   pylab.ylabel('tube number')
	   pylab.title(x)
	   idx=idx+1     
	
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
    def __init__(self,instrument,runnum,banknum,Ei,T0,geomfile='',*args):
       if len(args)>0:
           findnx_str='findnexus --prefix=%s -i %s %d' %(args[0],instrument,runnum)
       else:
           findnx_str='findnexus -i %s %d' %(instrument,runnum)
       filestr=Popen([findnx_str],shell=True,stdout=PIPE).communicate()[0]
       filestr=filestr.strip('\n')
       self.instrument=instrument
       self.runnum=runnum
       self.filename=filestr
       if len(geomfile)>0:
          self.geomfile=geomfile
       else:
          self.geomfile=filestr
       self.banknum=banknum
       self.Ei=Ei
       self.T0=T0
       self.bank_read()
       
     
     
       
    
    def bank_read(self):
       fid=nxs.open(self.filename,'r')
       bankstr='/entry/instrument/bank%d/' %self.banknum
       strstr='/entry/instrument/'
       fid.openpath(bankstr+'data')
       self.datain=fid.getdata()
       fid.openpath(bankstr+'time_of_flight')
       self.tof=fid.getdata()
       fid.close()
       #done reading data
       #start reading geometry 
       self.geom_read()       
       # done reading geometry
       self.L_md=self.samp_pos+self.dist
       d_notof=3956.0/2.0/self.L_md/sin(self.two_theta/2.0)
       jd=tile(d_notof,(self.tof.shape[0],1,1))
       jd=swapaxes(jd,0,2)
       jd=swapaxes(jd,0,1)
       jt=tile((self.tof-self.T0)*1e-6,(d_notof.shape[0],d_notof.shape[1],1))
       self.d=jt*jd
       return self
       
    def geom_read(self):
       #start reading geometry 
       fid=nxs.open(self.geomfile,'r')
       if self.geomfile!=self.filename:
            bankstr='/instrument/bank%d/' %self.banknum
	    strstr='/instrument/'
       fid.openpath(bankstr+'distance')
       self.dist=fid.getdata()
       fid.openpath(bankstr+'polar_angle')
       self.two_theta=fid.getdata()
       fid.openpath(bankstr+'azimuthal_angle')
       self.az=fid.getdata()
       fid.openpath(bankstr+'x_pixel_offset')
       self.xpixel=fid.getdata()
       fid.openpath(bankstr+'y_pixel_offset')
       self.ypixel=fid.getdata()
       fid.openpath(bankstr+'origin/orientation/value')
       dircostmp=fid.getdata()
       self.dircos=gen_dir_cos_matrix(dircostmp)
       fid.openpath(bankstr+'origin/translation/distance')
       self.Lvec=fid.getdata()
       self.L0=sqrt((self.Lvec*self.Lvec).sum())
       self.theta0=pi-arccos(abs(self.Lvec[1])/self.L0)
       fid.openpath(strstr+'moderator/distance')
       self.samp_pos=fid.getdata()*-1.0
       fid.openpath(bankstr+'origin/shape/size')
       self.tube_size=fid.getdata()
       ntubes=self.dist.shape[0] 
       npix=self.dist.shape[1]
       fid.close()
       # done reading geometry
       return self
    def cutt(self, tlims):
        """  
	  cut data along t
	"""
	outinst=deepcopy(self)
	tidx_min=max(find(self.tof<tlims[0]))
	tidx_max=min(find(self.tof>tlims[1]))
	outinst.tof=outinst.tof[tidx_min:tidx_max]
	outinst.datain=outinst.datain[:,:,tidx_min:(tidx_max-1)]
	outinst.d=outinst.d[:,:,tidx_min:tidx_max]
	return outinst
    
    def bint(self,bincomb):
        """
	   
	"""
	outinst=deepcopy(self)
	outinst.tof=outinst.tof[::bincomb]
	outinst.datain=outinst.datain[:,:,::bincomb]
	outinst.d=outinst.d[:,:,::bincomb]
	return outinst
    def biny(self,bincomb):
        """
	  group bincomb detector detector pixels
	""" 
	outinst=deepcopy(self)
	inshape=self.datain.shape
	ypixels=inshape[1]/bincomb
	zshape=(bincomb,inshape[0],ypixels,inshape[2])
	tmp=zeros(zshape)
	tmpy=zeros((bincomb,ypixels))
	for idx in range(bincomb):
	   tmp[idx,:,:,:]=self.datain[:,idx::bincomb,:]
	   tmpy[idx,:]=self.ypixel[idx::bincomb]
	outinst.datain=tmp.sum(0)
	outinst.ypixel=tmpy.mean(0)
	
	return outinst  
	  
	     
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
    #def plot_bank_new(self,sumaxis=0):
        
    
    def plot_tube(self,tube,tlims=[],bincomb=1):
        """  
	   tube= (0-7)
	   tlims in mus
	   bincomb number of time bins to combine 	
	"""
	if len(tlims)==0:
	    tmin=0
	    tmax=len(self.tof)-1
	    pdat=self
	else:
	    tmin=tlims[0]
	    tmax=tlims[1]
	    pdat=self.cutt([tmin,tmax])
	taxis=(pdat.tof[1:]+pdat.tof[:-1])/2.0
	#tmat=tile(taxis,(1,pdat.az.shape[1]))
	tmat=tile(taxis,(128,1))
	dmat=(pdat.d[tube,:,1:]+pdat.d[tube,:,:-1])/2.0
	print dmat.shape
	print tmat.shape
	pcolor(tmat,dmat,pdat.datain[tube,:,:])
	title('tube %d ' %(tube),fontsize=12.0)
	ylabel('d($\AA$)')
	xlabel('t($\mu s$)')

    def plot_tubes(self,tubes=range(8),ylims=[]):
       figure()
       for idx in tubes:
          subplot(2,4,idx+1)
	  self.plot_tube(idx)
	  if len(ylims)==2:
	      ylim(ylims)
       fn=self.filename.split(os.sep)[-1]
       suptitle('%s bank: %d' %(fn,self.banknum)) 
	    
	    
	  
	
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

def gen_dir_cos_matrix(dircos):
   """
   function to generate direction cosines from information read in nexus file"
   """
   outmat=zeros((3,3))
   outmat[0,:]=dircos[0:3]
   outmat[1,:]=dircos[3:6]
   outmat[2,0]=-outmat[0,1]*outmat[1,2]+outmat[0,2]*outmat[1,1]
   outmat[2,1]=-outmat[0,2]*outmat[1,0]+outmat[0,0]*outmat[1,2]
   outmat[2,2]=-outmat[0,0]*outmat[1,1]+outmat[0,1]*outmat[1,0]
   return outmat
def arccos_quad(x,r,y):
      out=arccos(x/r)
      out[y<0]=2.0*pi-out[y<0]
      return out
def make_lists(self,x):
# predeclare variables
  nel=array(self.data_dict[x]['data'].shape).prod()
  wl=zeros((nel,4))
  dsp=zeros((nel,4))
  Itmp=self.data_dict[x]['data'].swapaxes(0,1)
  I=zeros(nel)
  dI=zeros(nel)
  wltmp=(self.data_dict[x]['wavelength'][:,1:,:]+self.data_dict[x]['wavelength'][:,:-1,:])/2.0
  dtmp=(self.data_dict[x]['dspacing'][:,1:,:]+self.data_dict[x]['dspacing'][:,:-1,:])/2.0
  pixlen=len(Itmp[0,:,0])
  n2=nel/pixlen
  for idx in range(pixlen):
    wlt=wltmp[1:,idx,:]
    wltl=wlt[:,:-1]
    wltr=wlt[:,1:]
    dt=dtmp[1:,idx,:]
    dtl=dt[:,:-1]
    dtr=dt[:,1:]
    wlb=wltmp[:-1,idx,:]
    wlbl=wlb[:,:-1]
    wlbr=wlb[:,1:]
    db=dtmp[:-1,idx,:]
    dbl=db[:,:-1]
    dbr=db[:,1:]
    print wltl.shape, pixlen,n2
    wl[idx*n2:(idx+1)*n2,0]=wltl.reshape(n2)
    wl[idx*n2:(idx+1)*n2,1]=wltr.reshape(n2)
    wl[idx*n2:(idx+1)*n2,2]=wlbr.reshape(n2)
    wl[idx*n2:(idx+1)*n2,3]=wlbl.reshape(n2)
    dsp[idx*n2:(idx+1)*n2,0]=dtl.reshape(n2)
    dsp[idx*n2:(idx+1)*n2,1]=dtr.reshape(n2)
    dsp[idx*n2:(idx+1)*n2,2]=dbr.reshape(n2)
    dsp[idx*n2:(idx+1)*n2,3]=dbl.reshape(n2)
    I[idx*n2:(idx+1)*n2]=Itmp[:,idx,:].reshape(n2)
    dI[idx*n2:(idx+1)*n2]=sqrt(I[idx*n2:(idx+1)*n2])
  return([wl,dsp,I,dI])
def rebin (x,y,I,dI,xlims,ylims,xbin,ybin):
  import nessi_list as nl
  import hlr_utils
  import axis_manip
  import array_manip
  x1=nl.NessiList(len(I))
  x2=nl.NessiList(len(I))
  x3=nl.NessiList(len(I))
  x4=nl.NessiList(len(I))
  y1=nl.NessiList(len(I))
  y2=nl.NessiList(len(I))
  y3=nl.NessiList(len(I))
  y4=nl.NessiList(len(I))
  Inl=nl.NessiList(len(I))
  dInl=nl.NessiList(len(I))
  x1[:]=x[:,0]
  x2[:]=x[:,1]
  x3[:]=x[:,2]
  x4[:]=x[:,3]
  y1[:]=y[:,0]
  y2[:]=y[:,1]
  y3[:]=y[:,2]
  y4[:]=y[:,3]
  Inl[:]=I[:]
  dInl[:]=dI[:]
  xout=hlr_utils.Axis(xlims[0],xlims[1],xbin).toNessiList()
  yout=hlr_utils.Axis(ylims[0],ylims[1],ybin).toNessiList()
  (outc,oute,frac,bc)=axis_manip.rebin_2D_quad_to_rectlin(x1,y1,x2,y2,x3,y3,x4,y4,Inl,dInl,xout,yout)
  dmb=nl.NessiList(len(outc))
  #(fdata,ferr)=array_manip.div_ncerr(outc,oute,frac,dmb)
  return([xout.toNumPy(True),yout.toNumPy(True),outc.toNumPy(),oute.toNumPy()])
  #return([xout.toNumPy(True),yout.toNumPy(True),fdata.toNumPy(),ferr.toNumPy()])
