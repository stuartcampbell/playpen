function w = read_mask (file)
% READ_MASK reads the list of spectra in a mask file into an IXTdataset_1d.
%
% Syntax
%   >> w = read_mask (file)    % read from named file
%   >> w = read_mask           % prompts for file
%

% Duplicates a lot of code with READ_MSK. Should really create subroutines to read data.

% Get file name - prompt if file does not exist (using file to set default seach location and extension

% original: T.G. Perring, Modified by: Dean Whittaker 30/10/2006
% -----------------------------------------------------------------------------------------------------
if (nargin==1)
    if (exist(file,'file')==2)
        file_internal = file;
    else
        file_internal = getfile(file);
    end
else
    file_internal = getfile('*.msk');
end
if (isempty(file_internal))
    error ('No file given')
end

% Read data from file
% ---------------------
fid = fopen(file_internal);
% skip over lines that begin with '!'
ind = [];
while 1
    istring = fgetl(fid);
    if (~ischar(istring)), break, end
    if (~isempty(istring) && ~strcmp(istring(1:1),'!'))
        ind = [ind, str_to_iarray(istring)];
    end
end
fclose (fid);

% Sort list of masked spectra
% ---------------------------
if (isempty(ind))
    error (['No data read from file ' file_internal])
end

ind = sort(ind);
ind = ind(find([1,ind(2:end)-ind(1:end-1)])); % remove double counts
x = linspace(1,ind(end),ind(end));
y = zeros(1,ind(end));
y(ind) = 1;
e = zeros(1,ind(end));
    
% Put data in a spectrum (point mode)
% ----------------------

w = IXTdataset_1d(x,y,e);
w.title =avoidtex(file_internal);
disp (['Data read from ' file_internal])