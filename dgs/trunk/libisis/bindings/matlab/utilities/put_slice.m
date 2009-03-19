function [filename,filepath]=put_slice(data,file)
% Writes ASCII slice file
%   >> [filename,filepath]=put_slice(data,file)
%
% Data has following fields:
%         xbounds: [1x22 double]
%         ybounds: [1x30 double]
%               x: [1x609 double]
%               y: [1x609 double]
%               c: [1x609 double]
%               e: [1x609 double]
%         npixels: [1x609 double]
%          pixels: [53747x7 double]
%         x_label: '[ Q_h, 0, 3 ]  in 2.894 Å^{-1}'
%         y_label: '[ +0.5   Q_vert, -Q_vert, 3 ]  in 2.506 Å^{-1}'
%         z_label: 'Intensity (abs. units)'
%           title: {'map02114.spe, , Ei=447 meV'  [1x56 char]  [1x54 char]}
%    x_unitlength: '2.894'
%    y_unitlength: '2.5063'
%       SliceFile: 'slice1.slc'
%        SliceDir: 'c:\temp\'
%
% Additionally, other fields will be added as appended lines
%      e.g.  data.as = 2.507    results in the appended line:     as = '2.507'
% Cell arrays of strings will be appended as muliple lines
%
%   filename        Name of file excluding path; empty if problem
%   filepath        Path to file including terminating file separator; empty if problem
%

% T.G.Perring   March 2008

null_data = -1e30;    % conventional NaN in spe files

% If no input parameter given, return
if ~exist('file','var')||~exist('data','var')
    error('Check arguments to put_slice')
end

% Check input arguments
if ~isstruct(data) ||...
        ~isfield(data,'xbounds') || isempty(data.xbounds) ||...
        ~isfield(data,'ybounds') || isempty(data.ybounds) ||...
        ~isfield(data,'x') || isempty(data.x) ||...
        ~isfield(data,'y') || isempty(data.y) ||...
        ~isfield(data,'c') || isempty(data.c) ||...
        ~isfield(data,'e') || isempty(data.e) ||...
        ~isfield(data,'npixels') || isempty(data.npixels) ||...
        ~isfield(data,'pixels') || isempty(data.pixels)
    error('Check arguments to put_slice')
end

size_xb=size(data.xbounds); size_yb=size(data.ybounds);
size_x=size(data.x); size_y=size(data.y); size_c=size(data.c); size_e=size(data.e);
size_npixels=size(data.npixels); size_pixels=size(data.pixels);
if length(size_xb)~=2 || length(size_yb)~=2 || size_xb(1)~=1 || size_yb(1)~=1 || size_xb(2)<2 || size_yb(2)<2
    error('Check arguments to put_slice')
end
np=(size_xb(2)-1)*(size_yb(2)-1);
if length(size_x)~=2 || length(size_y)~=2 || length(size_c)~=2 || length(size_e)~=2 || length(size_npixels)~=2 || length(size_pixels)~=2 ...
        || ~all(size_x==size_y) || ~all(size_x==size_c) || ~all(size_x==size_e) || ~all(size_x==size_npixels) ...
        || size_x(1)~=1 || size_x(2)~=np || ~all(data.npixels>=0)...
        || size_pixels(1)~=sum(data.npixels(:)) || size_pixels(2)~=7
    error('Check arguments to put_slice')
end

% Remove blanks from beginning and end of filename
file_tmp=strtrim(file);

% Get file name and path (incl. final separator)
[path,name,ext,ver]=fileparts(file_tmp);
filename=[name,ext,ver];
filepath=[path,filesep];

% Get header fields
nx=size_xb(2)-1;
ny=size_yb(2)-1;
xorig=0.5*(data.xbounds(1)+data.xbounds(2));
yorig=0.5*(data.ybounds(1)+data.ybounds(2));
dx=data.xbounds(2)-data.xbounds(1);
dy=data.ybounds(2)-data.ybounds(1);
header=[nx;ny;xorig;yorig;dx;dy];

% Make bins with zero data go to NaN
data.c(data.npixels==0)=null_data;
data.e(data.npixels==0)=0;

% Make labels to go as footer in the file
labels=write_labels(data,'except',{'xbounds','ybounds','x','y','c','e','npixels','pixels'});
footer=char(labels);
%line_len=size(footer,1);    % maximum string length
%footer=footer(:)';          % make a string

% Write slice file using fortran routine
%ierr = put_slice_fortran (file_tmp,header,data.x',data.y',data.c',data.e',data.npixels',data.pixels',footer,line_len);
ierr = libisisexc('IXTutility','putslice',file_tmp,header,data.x,data.y,data.c,data.e,data.npixels,data.pixels',footer);
if round(ierr)~=0
    error(['Error writing slice data to ',file_tmp])
    filename='';
    filepath='';
end
