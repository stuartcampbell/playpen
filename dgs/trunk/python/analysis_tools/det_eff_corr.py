import sys
sys.path.append('/SNS/users/19g/SEQUOIA/commissioning/python/neutron_util')
import time
import nxs
import shutil
from numpy import expm1
from unit_convert import E2V
def He3_cross(v,P,T,L):
   """ He3 cross section as a function of 
	v = neutron velocity in m/s
	P = Pressure in Atm
	T = Temperature in K
	L=detector thickness in mm
   """ 
   PdT=P/T
   return PdT*L*8606.3/v
   
def det_eff_corr(filename,write_spe=False,debug=False):
   """ function to correct for detector efficiency
       takes as an input an nxspefile
   """		
   # create output file
   if debug:
     print "start time "
     print time.localtime()
   filebeg= filename.split('.')[0]
   filenew=filebeg+'_corr'+'.nxspe'
   shutil.copy2(filename,filenew)
   if debug:
     print "after file copy"
     print time.localtime()
   # open file
   rn=nxs.open(filenew,mode='rw')
   #retrieve data
   jk=rn.getnextentry()
   rn.openpath(r'/'+jk[0])
   rn.openpath('data')
   rn.opendata('data')
   dat=rn.getdata()
   rn.closedata()
   if debug:
     print "after get data"
     print time.localtime()
   rn.opendata('energy')
   en=rn.getdata()
   rn.closedata()
   if debug:
     print "after get data"
     print time.localtime()
   rn.opendata('error')
   err=rn.getdata()
   rn.closedata()
   if debug:
     print "after get error"
     print time.localtime()
   #process data
   rn.openpath(r'/'+jk[0])
   rn.openpath('NXSPE_info')
   rn.opendata('fixed_energy')
   Ei=rn.getdata()
   rn.closedata()
   vf=E2V(Ei-en)
   vf=(vf[:-1]+vf[1:])/2.0 #find center velocities
   fac=-1.0/(expm1(-He3_cross(vf,10.0,290.0,2.54)))
   fac_m=tile(fac,(dat.shape[0],1))
   dat=dat*fac_m
   err=err*fac_m
   if debug:
     print "after process data"
     print time.localtime()
   # write data
   rn.openpath(r'/'+jk[0])
   rn.openpath('data')
   rn.opendata('data')
   rn.putdata(dat)
   rn.closedata()
   if debug:
     print "after write data"
     print time.localtime()
   rn.opendata('error')
   rn.putdata(err)
   rn.closedata()
   if debug:
     print "after write error"
     print time.localtime()
   rn.close()   
   if debug:
     print "after close"
     print time.localtime()
   
