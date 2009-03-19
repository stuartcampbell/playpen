function libisishelp
%---libisishelp - help file for libisis
%
% syntax: libisishelp
%
% displays all current matlab commands in libisis

IXG_MATLAB_ROOT = matlabroot;
if (isempty(IXG_MATLAB_ROOT))
    IXG_MATLAB_ROOT = [];
    error('Cannot find matlab root, Exiting');
end

help_file = 'libisishelp';
find_help = feval('which', help_file);
if (isempty(find_help))
    IXG_MATLAB_ROOT=[];
    error('libisis help file not found, Exiting');    
else
    ind = strfind(find_help,help_file);
    path_string = find_help(1:ind-1);
end

%global structure

name = 'libisishelp.txt';
filename = [path_string '\' name];
fid=fopen(filename);
while ~feof(fid)
    tline = fgetl(fid);
    if ~ischar(tline), break, end
    disp(tline)
end
fclose(fid);
