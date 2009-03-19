function w = read_hist (file)
% READ_HIST reads x,y or x,y,e arrays of x bin boundaries, corresponding y values, errors on y
% Format of file is nth line: y(n) +/- e(n) = counts per unit x for x(n) to x(n+1)
% The last line read must have two entries (in the case of x-y data) or three entries
% (in the case of x-y-e data), but the y and e values are ignored.
%
% Lines at the beginning and the end of the file that are not of the form x-y or x-y-e are ignored
%
% Syntax
%   >> w = read_hist (file)    % read from named file
%   >> w = read_hist           % prompts for file
%

% original: T.G. Perring, Modified by: Dean Whittaker 30/10/2006

% Get file name - prompt if file does not exist (using file to set default seach location and extension
% -----------------------------------------------------------------------------------------------------
if (nargin==1)
    if (exist(file,'file')==2)
        file_internal = file;
    else
        file_internal = getfile(file);
    end
else
    file_internal = getfile;
end
if (isempty(file_internal))
    error ('No file given')
end

% Read data from file
% ---------------------
fid = fopen(file_internal);
% skip over lines that do not consist solely of two or three numbers
data_found = 0;
while ~data_found
    istart=ftell(fid);
    if (istart<0)
        fclose(fid);
        error (['No x-y-e data encountered in ' file_internal])
    end
    tline = fgets(fid);
    temp = str2num(tline);
    if (length(temp)==3)        % x-y-e data
        data_found = 1;
        xye = 1;
    elseif (length(temp)==2)    % x-y data only (no error bars)
        data_found = 1;
        xye = 0;
    end
end
fstatus=fseek(fid,istart,'bof'); % step back one line
if (fstatus~=0)
    fclose(fid);
    error (['Error reading from file ' file_internal])
end
% read array to the end, or until unable to read from file with specified format
if (xye)
    a = fscanf(fid,'%g %g %g',[3,inf]);
else
    a = fscanf(fid,'%g %g',[2,inf]);
end
if (isempty(a))
    fclose(fid);
    error (['No x-y-e data encountered in ' file_internal])
end
fclose(fid);

% Put data in a spectrum (histogram mode)
% ----------------------
n=size(a,2);
if (xye)
    w = IXTdataset_1d(a(1,1:n),a(2,1:n-1),a(3,1:n-1));
else
    w = IXTdataset_1d(a(1,1:n),a(2,1:n-1),zeros(1,n-1));
end

w.title = char(avoidtex(file_internal));

disp (['Data read from ' file_internal])%