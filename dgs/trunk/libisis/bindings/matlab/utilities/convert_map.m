function map = convert_map (file_in, file_out)
% Lconvert_map converts an old-style (i.e. VMS) Homer map file into a new-style
% (i.e. LIBISIS) map file.
%
% Optionally also returns a cell array containing the spectra in each workspace.
%
% Assumes that the workspaces are numbered 1,2,3... (the old format map file does
% not allow an alternative workspace numbering)
%
% Syntax:
% -------
%   >> convert_map              % prompts for input and output file names
%   >> convert_map (file_in)    % read from named file, prompt for output file name
%   >> convert_map (file_in, file_out)
%
%   >> map = convert_map (...)  % fill a cell array with spectrum numbers for each workspace:
%
%                               map{1}  array of spectra in workspace 1
%                               map{2}  array of spectra in workspace 2
%                                 :                    :
%


% Get file names - prompt if file does not exist (using file to set default seach location and extension
% ------------------------------------------------------------------------------------------------------
% Input file:
if (nargin>=1)
    if (exist(file_in,'file')==2)
        file_internal_in = file_in;
    else
        file_internal_in = getfile(file);
    end
else
    file_internal_in = getfile('*.map');
end
if (isempty(file_internal_in))
    error ('No input file given')
end

% Output file:
if (nargin<2)
    file_internal_out = putfile('*.map');
    if (isempty(file_internal_out))
        error ('No output file given')
    end
elseif (nargin==2)
    file_internal_out = file_out;
end


% Read input file:
% -----------------
map_out = get_map (file_internal_in);
if nargout==1
    map=map_out;
end


% Write output file:
% -------------------
fid = fopen (file_internal_out, 'wt');
if (fid < 0)
    error (['ERROR: cannot open file ' file_internal])
end
fprintf (fid, '%g \n', length(map_out));

for i=1:length(map_out)
    fprintf (fid, '%g \n', i);
    fprintf (fid, '%g \n', length(map_out{i}));
    vals=sort(map_out{i});
    str=iarray_to_str(vals);
    for j=1:length(str)
        fprintf(fid,'%s \n', str{j});
    end
end
fclose(fid);


%-------------------------------------------------------------------------------------
% The following is an alternative write, that does NOT use the notation m-n to
% mean m m+1 m+2 ...n-1 n
%-------------------------------------------------------------------------------------
% Write output file:
% -------------------
% fid = fopen (file_internal_out, 'wt');
% if (fid < 0)
%     error (['ERROR: cannot open file ' file_internal])
% end
% fprintf (fid, '%g \n', length(map_out));
% 
% len_row=10;  % number of spectra per row in output file
% for i=1:len_row
%     fmt{i}=[repmat('%g ',[1,i]),'\n'];
% end
% for i=1:length(map_out)
%     fprintf (fid, '%g \n', i);
%     fprintf (fid, '%g \n', length(map_out{i}));
%     nrow=floor(length(map_out{i})/len_row);
%     nrem=length(map_out{i})-len_row*nrow;
%     for j=1:nrow
%         fprintf (fid, fmt{len_row}, map_out{i}(1+len_row*(j-1):len_row*j));
%     end
%     if nrem>0
%         fprintf (fid, fmt{nrem}, map_out{i}(1+len_row*nrow:end));
%     end
% end
% fclose(fid);
%-------------------------------------------------------------------------------------

%================================================================================================
function map_out = get_map (file_internal)
% FOR INTERNAL USE
% 
% GET_MAP reads the list of spectra in a map file into a cell array of 1D arrays.
% Each array contains the spectra for that workspace. For example, if the examples below
% are used to load a map file into the cell array, map:
%
%   map{1}  array of spectra in workspace 1
%   map{2}  array of spectra in workspace 2
%    :                    :
%
% Syntax
%   >> map = get_map (file)    % read from named file
%

% Read data from file
% ---------------------
fid = fopen(file_internal);
% skip over lines that begin with '!'
first_line = 1;
nw = 0;
iw = 1;
ns = 0;
while 1
    istring = fgetl(fid);
    if ~ischar(istring)
        if nw < 1 | iw<= nw
            error ('Check format of .map file')
        else
            break
        end
    end
    if (~strcmp(istring(1:1),'!'))
        if first_line==1 % first line to be read
            first_line = 0;
            nw = str_to_iarray(istring);
            if length(nw)~=1
                error ('Check format of .map file')
            elseif nw < 1
                error ('Check number of workspaces declared in first non-comment line')
            else
                map = cell(1,nw);
            end
        else
            if ns==0   % array not declared yet
                if iw > nw
                    error ('Check format of .map file: excess uncommented information at bottom of file')
                end
                wdata = sscanf(istring,'%d');
                if ~isempty(wdata)   % header line for workspace
                    if wdata(1) > 0
                        ns = wdata(1);
                    else
                        error (['Check number of spectra declared for workspace ',num2str(iw)])
                    end
                else
                    error (['Check format of .map file at workspace ',num2str(iw)])
                end
            else
                map{iw} = [map{iw}, str_to_iarray(istring)];
                if length(map{iw})==ns
                    map{iw}=sort(map{iw});
                    if round(min(diff(map{iw}))) == 0
                        error (['One or more spectra are repeated in mapping for workspace ',num2str(iw)])
                    else
                        map{iw}=round(map{iw});
                    end
                    iw = iw + 1;
                    ns = 0;
                end
            end
        end
    end
end
fclose (fid);
map_out = map;
%================================================================================================