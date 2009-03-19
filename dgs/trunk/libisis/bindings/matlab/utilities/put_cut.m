function [filename,filepath]=put_cut(data,file)
% Writes ASCII .cut file
%   >> [filename,filepath]=put_cut(data,file)
%
% Data has following fields:
%         x: [1xn double]
%         y: [1xn double]
%         e: [1xn double]
%   npixels: [1xn double]
%    pixels: [mx6 double], m=sum(npixels(:))
%     title: {'map02114.spe, , Ei=447 meV'  [1x56 char]  [1x54 char]}
%   x_label: '[ Q_h, 0, 3 ]  in 2.894 Å^{-1}, <Q_vert>=0.00084693 <Q_l>=3.0052'
%   y_label: 'Intensity (abs. units)'
%   CutFile: 'bog.cut'
%    CutDir: 'c:\temp\'
%
% Additionally, other fields will be added as appended lines
%      e.g.  data.as = 2.507    results in the appended line:     as = '2.507'
% Cell arrays of strings will be appended as muliple lines
%
%   filename        Name of file excluding path; empty if problem
%   filepath        Path to file including terminating file separator; empty if problem
%

% T.G.Perring   March 2008

% If no input parameter given, return
if ~exist('file','var')||~exist('data','var')
    error('Check arguments to put_cut')
end

% Check input arguments
if ~isstruct(data) ||...
        ~isfield(data,'x') || isempty(data.x) ||...
        ~isfield(data,'y') || isempty(data.y) ||...
        ~isfield(data,'e') || isempty(data.e) ||...
        ~isfield(data,'npixels') || isempty(data.npixels) ||...
        ~isfield(data,'pixels') || isempty(data.pixels)
    error('Check arguments to put_cut')
end

size_x=size(data.x); size_y=size(data.y); size_e=size(data.e);
size_npixels=size(data.npixels); size_pixels=size(data.pixels);
if length(size_x)~=2 || length(size_y)~=2 || length(size_e)~=2 || length(size_npixels)~=2 || length(size_pixels)~=2 ...
        || ~all(size_x==size_y) || ~all(size_x==size_e) || ~all(size_x==size_npixels) ...
        || size_x(1)~=1 || ~all(data.npixels>0)...
        || size_pixels(1)~=sum(data.npixels(:)) || size_pixels(2)~=6
    error('Check arguments to put_cut')
end

% Remove blanks from beginning and end of filename
file_tmp=strtrim(file);

% Get file name and path (incl. final separator)
[path,name,ext,ver]=fileparts(file_tmp);
filename=[name,ext,ver];
filepath=[path,filesep];

% Make labels to go as footer in the file
labels=write_labels(data,'except',{'x','y','e','npixels','pixels'});
footer=char(labels);
%line_len=size(footer,1);    % maximum string length
%footer=footer(:)';          % make a string

% Write cut file using fortran routine
%ierr = put_cut_fortran (file_tmp,data.x',data.y',data.e',data.npixels',data.pixels',footer,line_len);
ierr = libisisexc('IXTutility','putcut',file_tmp,data.x,data.y,data.e,data.npixels,data.pixels',footer);
if round(ierr)~=0
    error(['Error writing cut data to ',file_tmp])
    filename='';
    filepath='';
end
