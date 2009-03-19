function [filename,filepath]=put_spe(data,file)
% Writes ASCII .spe file
%   >> [filename,filepath]=put_spe(data,file)
%
% data has following fields:
%   data.S          [ne x ndet] array of signal values
%   data.ERR        [ne x ndet] array of error values (st. dev.)
%   data.en         Column vector of energy bin boundaries
%
%   filename        Name of file excluding path; empty if problem
%   filepath        Path to file including terminating file separator; empty if problem
%

% T.G.Perring   13/6/07

null_data = -1.0e30;    % conventional NaN in spe files

% If no input parameter given, return
if ~exist('file','var')||~exist('data','var')
    error('Check arguments to put_spe')
end

% Check input arguments
if ~isstruct(data) ||...
        ~isfield(data,'S') || isempty(data.S) ||...
        ~isfield(data,'ERR') || isempty(data.ERR) ||...
        ~isfield(data,'en') || isempty(data.en)
    error('Check arguments to put_spe')
end

size_S=size(data.S); size_ERR=size(data.ERR); size_en=size(data.en); 
if length(size_S)~=2 || length(size_ERR)~=2|| length(size_en)~=2 ...
        || ~all(size_S==size_ERR) || min(size_en)~=1 || max(size_en)~=size_S(1)+1
    error('Check arguments to put_spe')
end

% Remove blanks from beginning and end of filename
file_tmp=strtrim(file);

% Get file name and path (incl. final separator)
[path,name,ext,ver]=fileparts(file_tmp);
filename=[name,ext,ver];
filepath=[path,filesep];

% Prepare data for Fortran routine
index=~isfinite(data.S)|~isfinite(data.ERR);
if sum(index(:)>0)
    data.S(index)=null_data;
    data.ERR(index)=0;
end

% Write spe file using fortran routine
disp(['Fortran writing of .spe file : ' file_tmp]);
ierr=libisisexc('IXTutility','putspe',file_tmp,data.S,data.ERR,data.en');
%ierr=put_spe_fortran(file_tmp,data.S,data.ERR,data.en);

if round(ierr)~=0
    error(['Error writing spe data to ',file_tmp])
    filename='';
    filepath='';
end
