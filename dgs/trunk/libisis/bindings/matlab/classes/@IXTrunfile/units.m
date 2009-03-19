function runfile_out = units(runfile_in,axis,varargin)

if (nargin >= 3)
    runfile_in=create_unitsrebin_command(runfile_in,inputname(1),inputname(2),varargin{1},mfilename('fullpath'),mfilename);
    runfile_out = libisisexc('IXTrunfile','unitsrebin',IXTrunfile,runfile_in,axis,varargin{1});
else
    runfile_in=create_units_command(runfile_in,inputname(1),inputname(2),mfilename('fullpath'),mfilename);
    runfile_out = libisisexc('IXTrunfile','units',IXTrunfile,runfile_in,axis);
end
