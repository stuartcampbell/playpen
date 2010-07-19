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
   
def det_eff_corr(filename,scl=1.0,write_spe=False, write_nxspe=True, debug=False):
   """ function to correct for detector efficiency
       takes as an input an nxspefile
       Flags:
       		write_spe=False change to True to write an spec file
		write_nxspe=True change to False to not write the nxspe file
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
   rn.opendata('polar')
   polar=rn.getdata()
   rn.closedata()
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
   fac=-scl/(expm1(-He3_cross(vf,10.0,290.0,2.54)))
   fac_m=tile(fac,(dat.shape[0],1))
   dat=dat*fac_m
   err=err*fac_m
   if debug:
     print "after process data"
     print time.localtime()
   # write data  
   # write nxspe
   if write_nxspe:
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
   #write spe
   if write_spe:
        spefilename = filenew.replace('.nxspe', '.spe')
	if debug:
            print "spe file open for writing"
            print time.localtime()
        spefile = open(spefilename, "w")
        
        len_det = len(polar)
        len_energy = len(en)
        
        print >> spefile, "%5i%5i" % (len_det, len_energy-1)
        
	
        print >> spefile, "### Phi Grid"
        for i in range(len_det+1):
            spefile.write("%10.3E" % 1.0)
            if ((i+1) % 8) == 0:
                print >> spefile 

    # Make sure that there is a newline at the end of this section
        if ((len_det+1) % 8) != 0:
            print >> spefile
        if debug:
            print "phi grid written"
            print time.localtime()
        print >> spefile, "### Energy Grid"
        for i in range(len_energy):
            spefile.write("%10.3E" % (en[i]))  
            if ((i+1) % 8) == 0:
                print >> spefile
                
    # Make sure that there is a newline at the end of this section
        if ((len_energy) % 8) != 0:
            print >> spefile
        if debug:
            print "energy grid written"
            print time.localtime()
    # Now write the data....

        for i in range(len_det):

            print >> spefile, "### S(Phi,w)"
            counter_y = 1
            # Extract out the counts for this angle
            yvalues = dat[i,:]
            for y in yvalues:    
                spefile.write("%10.3E" % (y))
                if (counter_y % 8) == 0:
                    print >> spefile
                counter_y = counter_y + 1

    # Make sure that there is a newline at the end of this section
    # (we subtract the 1 because we've just added it at the end of 
    # the above loop!
            if ((counter_y-1) % 8) != 0:
                print >> spefile
    
            
            print >> spefile, "### Errors"
            counter_err_y = 1
            # Extract out the errors for this angle
            err_values = err[i,:]
            for err_y in err_values:
                spefile.write("%10.3E" % (math.sqrt(math.fabs(err_y))))
                if (counter_err_y % 8) == 0:
                    print >> spefile
                counter_err_y = counter_err_y + 1
        
        # Make sure that there is a newline at the end of this section
        # (we subtract the 1 because we've just added it at the end of 
        # the above loop!
            if ((counter_err_y-1) % 8) != 0:
                print >> spefile
        if debug:
               print "spE file complete"
               print time.localtime()
