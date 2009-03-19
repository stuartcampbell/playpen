function w = read_cut (file)
% TEMPORARILY ALTERED SO THAT TITLE INFORMATION IS NOT SET! 
% READ_CUT Reads the x,y,e arrays of x coordinate, corresponding y values, errors on y from
%          a file with the format of and Mslice .cut file.
%
% Syntax:
%   >> w = read_cut (file)    % read from named file
%   >> w = read_cut           % prompts for file
%

% Use Mslice version of load_cut, copied to private area, to make Libisis independent of an Mslice installation.
% Update if Radu Coldea updates the Mslice load_cut.

% Get file name - prompt if file does not exist (using file to set default seach location and extension
% -----------------------------------------------------------------------------------------------------
if (nargin==1)
    if (exist(file,'file')==2)
        file_internal = file;
    else
        file_internal = getfile(file);
    end
else
    file_internal = getfile('*.cut');
end
if (isempty(file_internal))
    error ('No file given')
end

% Use Mslice load_cut
% --------------------
c = load_cut(file_internal);

% Put data in a spectrum (point mode)
% ----------------------
if (isfield(c,'emode'))   % use the presence of this field to determine if trailing info. appears in cut file WILL DO FOR NOW ONLY
    w = IXTdataset_1d(c.x, c.y, c.e); 
    w.title = char(c.title);
else
    w = IXTdataset_1d(c.x, c.y, c.e);
    w.title = char(avoidtex(file_internal));
end

disp (['Data read from ' file_internal])