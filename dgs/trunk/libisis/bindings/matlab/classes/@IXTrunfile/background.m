function runfile_out = background(runfile_in,bmin,bmax)
runfile_in=create_background_command(runfile_in,inputname(1),bmin,bmax,mfilename('fullpath'),mfilename);
runfile_out= libisisexc('IXTrunfile','background',IXTrunfile,runfile_in,bmin,bmax);

