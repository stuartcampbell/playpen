function w = read_ascii (varargin)
% READ_ASCII Reads x,y or x,y,e arrays of x coordinate, corresponding y values
%           and errors on y, into a dataset_1d. Automatically detects if data is
%           point or histogram data.
%            Lines at the beginning and the end of the file that are not of the
%           form x-y or x-y-e are ignored.
%            Columns can be separated by spaces, commas or tabs. Commas and tabs
%           can be used to indicate columns to skip e.g. the line
%                   13.2, ,15.8
%           puts 13.2 in column 1 and 15.8 in column 3.
%
%            Can be used to read an array of data rather than array of spectra.
%            
% By default, if just two columns of numeric data are found, then these are used as the x and y values; if
% three or more columns are found then these are used as the x,y,e values:
%
%   >> w = read_ascii (file)    % read from named file
%   >> w = read_ascii           % prompts for file
%
% Alternatively, the column numbers corresponding to x,y and (optionally) e can be specified. Vector
% arguments will return an array of spectra:
%
%   >> w = read_ascii (file, 3,5,2)    % columns 3,5, and 2 are x,y,e respectively
%   >> w = read_ascii (4,6)            % prompts for file, and columns 4,6 are x,y
%   >> w = read_ascii (4,[6,8,10],[7,9,11])   % three returned spectra
%
% To return the data as an array:
%
%   >> w = read_ascii (file,0)  % read from named file
%   >> w = read_ascii (0)       % prompts for file
%


% Get file name - prompt if file does not exist (using file to set default seach location and extension
% -----------------------------------------------------------------------------------------------------

ncol_read = nargin; % number of columns to read from file

% Get file name
if nargin>0
    if ischar(varargin{1})
        file = varargin{1};
        ncol_read = ncol_read - 1;
        if (exist(file,'file')==2)
            file_internal = file;
        else
            file_internal = getfile(file);
        end
    else
        file_internal = getfile;
    end
else
    file_internal = getfile;
end
if (isempty(file_internal))
    error ('No file given')
end

return_dataset = 1;    % return result as a dataset
hist=0;

% Get column numbers for x,y,e data:
% ---------------------------------------
if ncol_read==0         % auto-detect x-y or x-y-e data
    col_x=1;
    col_y=2;
    ncol_min=2;
    xye=0;
elseif ncol_read==1     % return an array, not a dataset
    return_dataset = 0;
    ncol_min=1;
elseif ncol_read==2 || ncol_read==3
    offset = nargin - ncol_read;
    if isnumeric(varargin{offset+1}) && min(varargin{offset+1})>0
        col_x = varargin{offset+1};
    else
        error ('ERROR: Must give positive integer(s) for x-axis column number(s)')
    end
    if isnumeric(varargin{offset+2}) && min(varargin{offset+2})>0 && ...
            (length(col_x)==1 ||length(varargin{offset+2})==length(col_x))
        col_y = varargin{offset+2};
        if length(col_x)==1
            col_x = repmat(col_x,1,length(col_y));
        end
    else
        error ('ERROR: Check y-axis column number(s)')
    end
    if ncol_read==3
        if isnumeric(varargin{offset+3}) && min(varargin{offset+3})>0 && length(varargin{offset+3})==length(col_y)
            col_e = varargin{offset+3};
        else
            error ('ERROR: Check error column number(s)')
        end
        ncol_min = max([max(col_x),max(col_y),max(col_e)]);
        xye=1;
    else
        ncol_min = max([max(col_x),max(col_y)]);
        xye=0;
    end
elseif ncol_read>3      % can only ask for two or three columns
    error ('ERROR: Can only read x-y or x-y-e data')
end

% Read data from file
% ---------------------
fid = fopen(file_internal);
% skip over lines that do not consist solely of numbers
data_found = 0;
while ~data_found
    istart=ftell(fid);
    if (istart<0)
        fclose(fid);
        error (['No x-y-e data encountered in ' file_internal])
    end
    tline = fgets(fid);
    temp = str2num(tline);
    if length(temp)>=ncol_min
        ncol = length(temp);
        data_found = 1;
        if return_dataset && ncol_read==0 && ncol>=3        % autodetect x-y or x-y-e
            col_e=3;
            xye=1;
        end
    end
end
% Error if no data found:
if ~data_found
    fclose(fid);
    error('ERROR: No block of numeric data found with the required number of columns')
end
% Step back one line now that number of columns found and any format ambiguities resolved:
fstatus=fseek(fid,istart,'bof'); % step back one line
if (fstatus~=0)
    fclose(fid);
    error (['Error reading from file ' file_internal])
end
% read array to the end, or until unable to read from file with specified format
fmt=repmat('%f',1,ncol);
tab = char(9);
a = textscan(fid, fmt, 'delimiter', [tab,',']);
if (isempty(a))
    fclose(fid);
    error (['No data encountered in ' file_internal])
end
fclose(fid);

% perform some checks on the data that has been read:
% (recall that if a row contains more than ncol numeric fields, more than one row in the output cell array will be filled)
n = length(a{1});
if ncol>1
    n_nan = zeros(1,n);
    for i=1:ncol
        n_nan(i) = length(find(isnan(a{i}(1:end-1))));
    end
    if max(n_nan(1:end))>0
        error('ERROR: Check format of column data - some fields are empty')
    end
end
% Determine if histogram data or not
last_row = zeros(1,ncol);
for i=1:ncol
    last_row(i)=a{i}(end);
end
if return_dataset
    nx=length(a{col_x(1)});
    nw=length(col_x);
    lx=length(find(isnan(last_row(col_x))));
    ly=length(find(isnan(last_row(col_y))));
    if ~xye
        if lx==0 && ly==nw && nx>1  % one more x value than y values
            ny=nx-1;
        elseif lx==0 && ly==0
            ny=nx;
        elseif lx==nw && ly==nw && nx>1
            nx=nx-1;
            ny=nx;
        else
            error('ERROR: Check format of column data - must be all histogram or all point x-y-e data')
        end
    else               
        le=length(find(isnan(last_row(col_e))));
        if lx==0 && ly==nw && le==nw && nx>1
            ny=nx-1;
        elseif lx==0 && ly==0 && le==0
            ny=nx;
        elseif lx==nw && ly==nw && le==nw && nx>1
            nx=nx-1;
            ny=nx;
        else
            error('ERROR: Check format of column data - must be all histogram or all point x-y data')
        end
    end
else
    if length(find(isnan(last_row)))>0
        error('ERROR: Check format of column data - some empty fields in last row')
    end
end

% Return data
% ----------------------
if return_dataset
    if (xye)
        w = IXTdataset_1d(a{col_x(1)}(1:nx)',a{col_y(1)}(1:ny)',a{col_e(1)}(1:ny)');
        w.title = char(avoidtex(file_internal));
        if nw>1
            for i=2:nw
                w(i) = IXTdataset_1d(a{col_x(i)}(1:nx)',a{col_y(i)}(1:ny)',a{col_e(i)}(1:ny)');
            end
        end
    else
        w = IXTdataset_1d(a{col_x(1)}(1:nx)',a{col_y(1)}(1:ny)',zeros(ny,1)');
        w.title = char(avoidtex(file_internal));
        if nw>1
            for i=2:nw
                w(i) = IXTdataset_1d(a{col_x(i)}(1:nx)',a{col_y(i)}(1:ny)',zeros(ny,1)');
            end
        end
    end
else
    w = cell2mat(a);
end

if return_dataset
    if nx~=ny; type = 'histogram'; else type = 'point'; end;
    if xye; cols='x-y-e'; else cols='x-y'; end;
    if nw>1
        disp(['Read ', num2str(nw),' dataset of ', cols, ' ', type,' data [', num2str(ny), ' signal-values]'])
    else
        disp(['Read one dataset of ', cols, ' ', type, ' data [', num2str(ny), ' signal-values]'])
    end
else
    disp (['Output is an array of data: ',num2str(size(w,2)),' columns, ',num2str(size(w,1)),' rows'])
end
disp (['Data read from ' file_internal])
