#code to calculate timing diagrams for ARCS and SEQUOIA
#this code is designed to look for spurions coming from multiple chopper openinga

#Syntax: timing_diagram(Ei,nu_fermi,nu_T0,spectrometer,slit_package)
# Ei is the incident energy in meV
#nu_fermi is the frequency of the Fermi chopper in Hz
#nu_T0 is the frequency of the T0 chopper in Hz
#spectrometer is the name of the Spectrometer  ('ARCS' or 'SEQUOIA')
#slit_package is an instance of the slit package class
#standard slit packages are defined at the end of the file
#see the bottom of the file for an example
#GEG 3_13_2009
#ORNL-NSSD

from math import sqrt
from numpy import array, logical_and, zeros, concatenate, ones
from pylab import figure, plot, show, hold, xlabel, ylabel, title, text, pi
from matplotlib import rc

def E2V(E):
    """
      Takes a neutron energy in meV and converts it to velocity in m/s 
    """
# for energy in mev returns velocity in m/s
    return sqrt(E/5.227e-6)

def V2E(V):
    """
      Takes a neutron velocity in m/s and converts it to energy in meV
    """
# for  v in  m/s returns energy in meV
    return 5.227e-6*V*V
    
class Slit_pack(object):
    #class that holds slit package parameters and functions
    def __init__(self,d,r,name,R=0.05):
        self.d=d #distance between slits in m
        self.R=R #radius of slit package in m
        self.r=r #radius of curvature of slits
        self.name=name # name of slit package
    def __repr__(self):
        msg="%s\n distance between slits: %g \n radius of slit package: %g \n slit radius of curvature: %g \n" % (self.name,self.d,self.R,self.r)
        return msg
    def Beta(self,nu,v0,v):
        return (2.0*pi*nu/self.d*(self.R*self.R))*abs(1/v-1/v0)
        
    def Fermi_T(self,nu,v0,v):
          #transmission for neutrons of velovity v through a fermi chopper rotating at nu with  where optimal transmission is at v0  
          beta=self.Beta(nu,v0,v)
          if beta>1:
            return 0
          elif beta<=0.25:
            return 1-8.0/3.0*beta*beta
          else:
            return 16.0/3.0*sqrt(beta)-8*beta+8.0/3.0*beta*beta
    def v0(self,nu):
        #optimal velocity for a chopper rotating at nu
        return self.r*4.0*pi*nu
    def Fermi_dt(self,nu,alpha=0.0):
    	"""
	nu is the frequency of the Fermi chopper in Hz
	alpha is the angular divergence of the beam perpendicular to the slit package
	"""
        return 1/(4.0*pi*nu)*(self.d/self.R+2.0*alpha)  
    
    
def timing_diagram(E,Ferminu,T0nu,spec,slit_pack,show_all=0,e02det=0,frame_bound=0):
    """
      routine to plot timing diagram to check for fram overlap
      timing_diagram(E,Ferminu,T0nu,spec,slit_pack,show_all=0,frame_bound=0)
      E is the incident energy in meV
      Ferminu is the frequency of the fermi chopper in Hz
      T0nu is the frequency of the T0 chopper in Hz
      spec is either 'ARCS' or'SEQUOIA'
      slit_pack is one of the values defined at the end of this file
      show_all(default=0) toggless showing all Fermi chopper transmission settings(1) versus just those that
      should make it through the T0 chopper(0).
      frame_bound toggles showing the frame boundaries to check fo possible leakage through the T0 chopper.
      The Ei and Ef are printed on the plot in meV
      GEG
      ORNL 7.1.2009
    """
    # define possible spectrometers
    pos_spec=set(['ARCS','SEQUOIA'])
    tmpset=set([spec])
    if not tmpset.issubset(pos_spec):
        #raise RuntimeError, 'spectrometer must be'+pos_spec
        raise RuntimeError,'spectrometer not defined'
    #define lengths for spectrometers
    if spec=='ARCS':
        L=array([8.77,11.6,13.6,3.4])
    else:
        L=array([9.78,18.0,20.0,6.3])
    dtT0=0.10/(0.2*T0nu*2.0*pi)/2.0
    vcen=E2V(E)
    tcen=L[0]/vcen
    tT0min=tcen-dtT0/2.0
    tT0max=tcen+dtT0/2.0
    tcenf=L[1]/vcen
    #number of T0 repetitions  per source pulse
    npulse=int(T0nu/30.0);
    # determine the maximum and minimum velocity for eacy pulse through the T0 chopper
    vmax=zeros(npulse);
    vmin=zeros(npulse);
    tT0mins=zeros(npulse);
    tT0maxs=zeros(npulse);
    for idx in range(npulse):
        tT0mins[idx]=(tT0min+idx*1.0/(2.0*T0nu))
        tT0maxs[idx]=(tT0max+idx*1.0/(2.0*T0nu))
    vmax=L[0]/tT0mins
    vmin=L[0]/tT0maxs 
    tmax=L[1]/vmin
    tmin=L[1]/vmax
    t0T0=zeros(tmax.shape)
    tT0mins=concatenate((tT0mins,tT0mins+1.0/60.0))
    tT0maxs=concatenate((tT0maxs,tT0maxs+1.0/60.0))
    tmax=concatenate((tmax,tmax+1.0/60.0))
    tmin=concatenate((tmin,tmin+1.0/60.0))
    t0T0=concatenate((t0T0,t0T0+1.0/60.0))
    # number of Fermi chopper repetitions per source pulse
    npulse_f=int(Ferminu/60.0);
    vfermi=zeros(npulse_f)    
    for idx in range(npulse_f):
        vfermi[idx]=L[1]/(tcenf+idx*1.0/Ferminu)
    #generate all pulses for fermi
    tframe_bound=ones(npulse_f)*1.0/60.0
    tfermi=L[1]/vfermi
    tsample=L[2]/vfermi
    tdete0=(L[2]+L[3])/vfermi
    tfermiT0=L[0]/vfermi
    if frame_bound:
         tslow=tframe_bound
	 vslow=L[3]/(tframe_bound - tsample)
    else:
         vslow=sqrt(0.011)*vfermi
         tslow=(L[2]+L[3])/vslow 
    tfermi=concatenate((tfermi,tfermi+tframe_bound[0]))
    tfermiT0=concatenate((tfermiT0,tfermiT0+tframe_bound[0]))
    tsample=concatenate((tsample,tsample+tframe_bound[0]))
    tdete0=concatenate((tdete0,tdete0+tframe_bound[0]))
    tslow=concatenate((tslow,tslow+tframe_bound[0]))
    tframe_bound=concatenate((tframe_bound,tframe_bound*2.0))
    Efermi=V2E(vfermi)
    Eslow=V2E(vslow)
    Efermi=concatenate((Efermi,Efermi))
    vfermi=concatenate((vfermi,vfermi))
    Eslow=concatenate((Eslow,Eslow))
    #determine if all pulses shown or only those that can be transmitted
    if not show_all:
        # remove Fermi chopper pulses that are removed by the T0 chopper
        kidx=array([],dtype=int)
        for idx in range(len(tfermi)):
            if logical_and(tT0mins<tfermiT0[idx],tT0maxs>tfermiT0[idx]).any():
                fermi_v0=slit_pack.v0(Ferminu)
                #remove Fermi pulses that are removed by  the energy dependent transmission of the chopper
                if slit_pack.Fermi_T(Ferminu,fermi_v0,vfermi[idx])>1e-7:
                    kidx=concatenate((kidx,array([idx])),1)
        tfermi=tfermi[kidx]
        tfermiT0=tfermiT0[kidx]
        tsample=tsample[kidx]
	tdete0=tdete0[kidx]
        tslow=tslow[kidx]
        Efermi=Efermi[kidx]
	Eslow=Eslow[kidx]
    # generate minimum and maximum lines for detector  
   # rc('text', usetex=True)
    figure()
    hold(True)
    for idx in range(len(tmax)):
        plot([t0T0[idx],tmin[idx]],[0.0,L[1]],'k')    
        plot([t0T0[idx],tmax[idx]],[0.0,L[1]],'k')
    for idx in range(len(tfermi)):
        if e02det>0:
	     plot([tfermiT0[idx],tdete0[idx]],[L[0],L[2]+L[3]],'g') 
	else :
             plot([tfermiT0[idx],tsample[idx]],[L[0],L[2]],'g') 
        plot([tsample[idx],tslow[idx]],[L[2],L[2]+L[3]],'r')
        plot([tsample[idx],tsample[idx]],[L[2],L[2]+L[3]],'b')
        teststr='%1.2f'%(Efermi[idx])
        text(tfermi[idx],L[1]+0.5,teststr)
	teststr2='%1.2f'%(Eslow[idx])
	text(tslow[idx],L[2]+L[3]-0.5,teststr2)
    if frame_bound:
        plot([tframe_bound[0],tframe_bound[0]],[0,L[2]+L[3]],'m')
	plot([2.0*tframe_bound[0],2.0*tframe_bound[0]],[0,L[2]+L[3]],'m')
    xlabel('t(s)')
    ylabel('L(m)')
    title(r'E=%g meV $\nu_{fermi}$=%g Hz $\nu_{T_0}$=%g Hz slit pack:%s'%(E,Ferminu,T0nu,slit_pack.name))
    show()
    
def T0_only_timing(E,T0nu,spec):
    """
    plot timing diagram to check for frame overlap when no Fermi chopper is in the beam
    """ 
    # define possible spectrometers
    pos_spec=set(['ARCS','SEQUOIA'])
    tmpset=set([spec])
    if not tmpset.issubset(pos_spec):
        #raise RuntimeError, 'spectrometer must be'+pos_spec
        raise RuntimeError,'spectrometer not defined'
    #define lengths for spectrometers
    if spec=='ARCS':
        L=array([8.77,11.6,13.6,3.4])
    else:
        L=array([9.78,18.0,20.0,6.3])
    dtT0=0.06/(0.2*T0nu*2.0*pi)/2.0
    vcen=E2V(E)
    tcen=L[0]/vcen
    tT0min=tcen-dtT0/2.0
    tT0max=tcen+dtT0/2.0
    tcenf=L[1]/vcen
    #number of T0 repetitions  per source pulse
    npulse=int(T0nu/30.0);
    # determine the maximum and minimum velocity for eacy pulse through the T0 chopper
    vmax=zeros(npulse);
    vmin=zeros(npulse);
    tT0mins=zeros(npulse);
    tT0maxs=zeros(npulse);
    for idx in range(npulse):
        tT0mins[idx]=(tT0min+idx*1.0/(2.0*T0nu))
        tT0maxs[idx]=(tT0max+idx*1.0/(2.0*T0nu))
    vmax=L[0]/tT0mins
    vmin=L[0]/tT0maxs 
    tmax=(L[2]+L[3])/vmin
    tmin=(L[2]+L[3])/vmax
    t0T0=zeros(tmax.shape)
    tT0mins=concatenate((tT0mins,tT0mins+1.0/60.0))
    tT0maxs=concatenate((tT0maxs,tT0maxs+1.0/60.0))
    tmax=concatenate((tmax,tmax+1.0/60.0))
    tmin=concatenate((tmin,tmin+1.0/60.0))
    t0T0=concatenate((t0T0,t0T0+1.0/60.0))
    figure()
    hold(True)
    for idx in range(len(tmax)):
        plot([t0T0[idx],tmin[idx]],[0.0,L[2]+L[3]],'k')    
        plot([t0T0[idx],tmax[idx]],[0.0,L[2]+L[3]],'k')
    xlabel('t(s)')
    ylabel('L(m)')
    show()
       
#define defualt slit packages
SEQ_100=Slit_pack(0.00203,0.58,'SEQ 100')
SEQ_700=Slit_pack(0.00356,1.53,'SEQ 700')
ARCS_100=Slit_pack(0.00152,0.58,'ARCS 100')
ARCS_300=Slit_pack(0.00305,1.00,'ARCS 300')
ARCS_700_2=Slit_pack(0.00152,1.53,'ARCS 700 2')
ARCS_700_3=Slit_pack(0.00356,1.53,'ARCS 700 3')
    
    
if __name__ == "__main__":
   
    timing_diagram(100.0,600.0,60.0,'SEQUOIA',SEQ_100)
        
    
    
        
    
