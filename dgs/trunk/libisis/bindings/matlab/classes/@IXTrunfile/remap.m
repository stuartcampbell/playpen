function runfile_out=remap(runfile_in,dso)
% a=remap(b,dso)
% will remap runfile b according to detector map file defined in dso 
runfile_in=create_remap_command(runfile_in,inputname(1),inputname(2),mfilename('fullpath'),mfilename);
runfile_out=libisisexc('IXTrunfile','remap',IXTrunfile,runfile_in,dso);