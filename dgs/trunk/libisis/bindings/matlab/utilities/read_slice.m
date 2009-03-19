function w = read_slice (file)
% TEMPORARILY ALTERED SO THAT TITLE INFORMATION IS NOT SET! 
% READ_SLICE Reads the x,y,e arrays of x coordinate, corresponding y values, errors on y from
%          a file with the format of and Mslice .cut file.
%
% Syntax:
%   >> w = read_slice (file)    % read from named file
%   >> w = read_slice           % prompts for file
%

% Use Mslice version of load_cut, copied to private area, to make MGENIE independent of an Mslice installation.
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
    file_internal = getfile('*.slc');
end
if (isempty(file_internal))
    error ('No file given')
end

% load slice
% --------------------
slice = load_slice(file_internal);

% put data in an array of spectra
% -------------------------------
nx = size(slice.x,2) - 1;
ny = size(slice.y,2) - 1;
slice.c = reshape(slice.c,nx,ny);
slice.e = reshape(slice.e,nx,ny);

% set titles:
if isfield(slice.f,'title')
    temp = cellstr(slice.f.title);   % add file name to top of the title
    ltemp = length(temp);
    ltitle = ltemp + 1;
    title(2:ltitle) = temp(1:ltemp);
    title{1} = avoidtex(file_internal);
    if isfield(slice.f,'y_label')    % store control information in title
        ltitle = ltitle + 1;
        title{ltitle} = ['libisis_control_y_label = ',slice.f.y_label];
    end
    if isfield(slice.f,'x_unitlength') && isfield(slice.f,'y_unitlength')
        ltitle = ltitle + 1;
        title{ltitle} = ['libisis_control_x_unitlength = ',slice.f.x_unitlength];
        ltitle = ltitle + 1;
        title{ltitle} = ['libisis_control_y_unitlength = ',slice.f.y_unitlength];
    end
    ltitle = ltitle + 1;
    title{ltitle} = ['libisis_control_y_start = ',num2str(slice.header_info(4))];
    ltitle = ltitle + 1;
    title{ltitle} = ['libisis_control_y_step = ',num2str(slice.header_info(6))];
else
    title = avoidtex(file_internal);
end


if isfield(slice.f,'x_label')
    xlabel = slice.f.x_label;
else
    xlabel = 'x axis';
end

if isfield(slice.f,'y_label')
    ylabel = slice.f.y_label;
else
    ylabel = 'y axis';
end

if isfield(slice.f,'z_label')
    zlabel = slice.f.z_label;
else
    zlabel = 'Intensity';
end

% fill spectra:
ylo  = slice.header_info(4)-0.5*slice.header_info(6);
yhi  = slice.header_info(4)+0.5*slice.header_info(6);
step = slice.header_info(6);
if (ny==1)    % just one y-strip in the slice file
    temp = [zlabel,' [',num2str(ylo),' to ',num2str(yhi),']'];
    w = IXTdataset_2d(slice.x,slice.y,slice.c,slice.e);
    
    w.title = char(title);
    w.x_units.caption = char(xlabel);
    w.y_units.caption = char(temp);
    w.s_units.caption = char(zlabel);
    
else
    % create dummy spectrum and replicate NDET times to preallocate memory (and therefore save CPU time)
    w = IXTdataset_2d(slice.x,slice.y,slice.c,slice.e);

    w.title = char(title);
    w.x_units.caption = char(xlabel);
    w.y_units.caption = char(ylabel);
    w.s_units.caption = char(zlabel);

end

%disp (['Data read from ' file_internal])

