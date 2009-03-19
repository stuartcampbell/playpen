function [ei,ei_extras]=getei(runfile_in,ei_guess,mon_array)
% ei=getei(IXTrunfile,ei_guess)
% will determine energy of fermi chopper given a test ei value and appropriately 
% populated runfile, ie with 2 monitors filled (generally M2 and M3)
[ei,ei_extras]=libisisexc('IXTrunfile','getei',runfile_in,ei_guess,int32(mon_array));