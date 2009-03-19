function data=get_spe(file)
% Load ASCII .spe file
%   >> data = get_spe(file)
%
% data has following fields:
%   data.filename   Name of file excluding path
%   data.filepath   Path to file including terminating file separator
%   data.S          [ne x ndet] array of signal values
%   data.ERR        [ne x ndet] array of error values (st. dev.)
%   data.en         Column vector of energy bin boundaries

% T.G.Perring   13/6/07
% Note: does NOT convert data <= -10^30 to NaN, following mslice convention

% Remove blanks from beginning and end of filename
file_tmp=strtrim(file);

% Get file name and path (incl. final separator)
[path,name,ext,ver]=fileparts(file_tmp);
data.filename=[name,ext,ver];
data.filepath=[path,filesep];

% Read spe file using fortran routine
disp(['Fortran loading of .spe file : ' file_tmp]);
[data.S,data.ERR,data.en]=libisisexc('IXTutility','getspe',file_tmp);
%[data.S,data.ERR,data.en]=get_spe_fortran(file_tmp);

[ne,ndet]=size(data.S);
disp([num2str(ndet) ' detector(s) and ' num2str(ne) ' energy bin(s)']);
