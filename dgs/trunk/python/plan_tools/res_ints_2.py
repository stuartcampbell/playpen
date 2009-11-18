sys.path.append('/SNS/users/19g/SEQUOIA/commissioning/python')
from unit_convert import E2V,E2K
from numpy import pi, log,exp,sqrt,tanh,linspace
from timing_diagram import Slit_pack
from scipy.interpolate import interp1d
from pylab import figure, plot, subplot, show, xlabel, ylabel, title
from UB import Bmat_gen,gen_rec_latt,Bmat

class Chopper_spec(object):
     def __init__(self,instr_name,L,slit_pack,w=0.0254,sw=0.05,sh=0.05): 
	 
	 self.instr_name=instr_name
	 self.L=L
	 self.w=w
	 self.sw=sw
	 self.sh=sh
	 self.slit_pack=slit_pack
     def domega_in(self,Ei,Ef,nu):
         """
	 """
	 dtm=H2Omod_dt(Ei)
	 dtc=self.slit_pack.Fermi_dt(nu)
	 dtd=det_dt(self.w,Ef)
	 return domega(Ei,Ef,self.L,dtm,dtc,dtd)
     def dE(self,Ei,nu):
         """
	 """
	 vi=E2V(Ei)
	 dtc=self.slit_pack.Fermi_dt(nu)
	 return 2*5.227e-6*vi**3.0*dtc/self.L[0]
     def flux(self,filename,Ei,nu):
         v=E2V(Ei)
	 print v
	 v0=self.slit_pack.v0(nu)
	 print v0
	 T=self.slit_pack.Fermi_T(nu,v0,v)
         I_func=read_mod_file(filename)
	 return I_func(Ei/1000.0)*self.dE(Ei,nu)*self.sw*self.sh/((self.L[0]+self.L[1])**2.0)*T


def domega(Ei,Ef,L,dtm,dtc,dtd):
     """
     provides the energy resolution of a direct chopper spectrometer given
     Ei: incident energy in meV
     Ef: fineal energy in meV
     A three element tuple
     L[0]= moderator to chopper distance
     L[1]= chopper to sample distance
     L[3]= sample to detector distance
     dtm = moderator pulse width (s)
     dtc= chopper pulse width (s)
     dtd= detector time uncertainty (s)
     """
     mn=1.674e-5/1.602 #mass of the neutron in meV
     vi=E2V(Ei)
     vf=E2V(Ef)
     return mn*sqrt(((vi**3.0)/L[0]+(vf**3.0)*L[1]/L[0]/L[2])**2.0*(dtm**2.0)+((vi**3.0)/L[0]+(vf**3.0)*(L[1]+L[0])/L[0]/L[2])**2.0*(dtc**2.0)+((vf**3.0)/L[2])**2.0*(dtd)**2.0)
   
def H2Omod_dt(E):
    """
    returns the time width of the neutron distribution as a function of energy
    E(meV)	
    """
    E=E
    x=log(E)
    p=[-0.4494,-0.046,4.3672,0.8530,3.7389,0.1271]
    y=exp(m1tanhm2(x,p))
    return y*1e-6
    
def det_dt(w,Ef):
    """
    w is the detector width in (m)
    Ef is the final energy in meV
    """
    return w/E2V(Ef)
def m1tanhm2(x,p):
    """
    """
    m=[p[0],p[1]]
    #m[0]=p[0]
    #m[1]=p[1]
    x0=p[2]
    w=p[3]
    y0=p[4]
    A=p[5]
    return (1+tanh((x-x0)/w))/2.0*m[0]*x+(1-tanh((x-x0)/w))/2.0*m[1]*x+y0+A*tanh((x-x0)/w)
def read_mod_file(filename):
    """
    """
    fid=open(filename)
    dattmp=fid.readlines()
    fid.close()
    idx=0
    E=[]
    flux=[]
    while '#' in dattmp[idx]:
       idx=idx+1
    while  not ('#' in dattmp[idx]):
       #print dattmp[idx]
       tmp1=dattmp[idx].split()
       if len(tmp1)>0:
          E.append(eval(tmp1[0]))
	  flux.append(eval(tmp1[2]))
       idx=idx+1 
    flux_func=interp1d(E,flux,kind='linear')               
    return flux_func
def plot_flux(nu,Ei,Ef,Spec):
     """
      plot_flux(nu,Ei,Ef,Spec)
      give a range of chopper frequencies (nu) (Hz)
      an incident energy Ei (meV) and a final energy Ef (meV)
      and an instance of a spectrometer class (examples are given in the bottome of this file)
      plot a number proportional to flux and the resolution as a function of chopper frequency  
     """
     dw=Spec.domega_in(Ei,Ef,nu)
     I=[]
     for idx in range(len(nu)):
       I.append(Spec.flux('source_sct521_bu_17_1.dat',Ei,nu[idx]))
     figure()
     subplot(2,1,1)
     plot(nu,I,'bo')
     ylabel('I (arb. units)')
     title('$E_i$ = %g (meV),$E_f$ = %g (meV), $\omega$ = %g (meV)'%(Ei,Ef,Ei-Ef))
     subplot(2,1,2)
     plot(nu,dw,'bo')
     ylabel('$d\omega$ (meV)')
     xlabel('$\\nu$ (Hz)')
     show()
def plot_qrange(Ei, wmin,UB=[[1,0,0],[0,1,0],[0,0,1]]):
   """
   """
   ki=E2K(Ei)
   omega=linspace(wmin,Ei*0.9,100)
   Ef=Ei-omega
   kf=E2K(Ef)
   Qxmax=-kf*sin(radians(60.0))
   Qxmin=-kf*sin(radians(2.1))
   Qxmin2=-kf*sin(radians(-5.3))
   Qxmax2=-kf*sin(radians(-30.0))
   Qymax=-kf*sin(radians(30.0))
   Qymin=-kf*sin(radians(6.7))
   Qymin2=-kf*sin(radians(-7.5))
   Qymax2=-kf*sin(radians(-30.0))
   Qzmax=ki-kf*cos(radians(60.0))
   Qzmin=ki-kf*cos(radians(2.1))
   Qzmax2=ki-kf*cos(radians(-30.0))
   Qzmin2=ki-kf*cos(radians(-5.3))
   Qmins=array([Qxmin,Qymin,Qzmin])
   Qmins2=array([Qxmin2,Qymin2,Qzmin2])
   Qmaxs=array([Qxmax,Qymax,Qzmax])
   Qmaxs2=array([Qxmax2,Qymax2,Qzmax2])
   hklmins=dot(UB,Qmins)
   hklmaxs=dot(UB,Qmaxs)
   hklmins2=dot(UB,Qmins2)
   hklmaxs2=dot(UB,Qmaxs2)
   figure()
   hold('on')
   xlbs=['$Q_x$','$Q_y$','$Q_z$']
   for idx in range(3):
     subplot(2,2,idx+1)
     plot_qlims(hklmins,hklmaxs,hklmins2,hklmaxs2,omega,idx)
     xlabel(xlbs[idx])
   show()
        
def plot_qlims(mins,maxs,mins2,maxs2,omega,idx):
   """
   """
   plot(mins[idx,:],omega,'b')
   plot(maxs[idx,:],omega,'b')
   plot(mins2[idx,:],omega,'r')
   plot(maxs2[idx,:],omega,'r')
   ylabel('$\omega$')
   
    
       
       

#define default slit packages
SEQ_100=Slit_pack(0.00203,0.58,'SEQ 100')
SEQ_700=Slit_pack(0.00356,1.53,'SEQ 700')
ARCS_100=Slit_pack(0.00152,0.58,'ARCS 100')
ARCS_300=Slit_pack(0.00305,1.00,'ARCS 300')
ARCS_700_2=Slit_pack(0.00152,1.53,'ARCS 700 2')
ARCS_700_3=Slit_pack(0.00356,1.53,'ARCS 700 3')
SEQUOIA=Chopper_spec('SEQUOIA',[18.0,2.0,5.5],SEQ_100)
SEQUOIA_sloppy=Chopper_spec('SEQUOIA',[18.0,2.0,5.5],SEQ_700)
ARCS=Chopper_spec('ARCS',[11.6,2.0,3.0],ARCS_100)
