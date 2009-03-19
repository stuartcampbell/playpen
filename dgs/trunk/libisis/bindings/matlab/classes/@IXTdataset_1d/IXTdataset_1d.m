function dataset_1d = IXTdataset_1d( varargin )
% Create IXTdataset_1d object
%
%   >> w = IXTdataset_1d (x)
%   >> w = IXTdataset_1d (x,signal)
%   >> w = IXTdataset_1d (x,signal,error)
%   >> w = IXTdataset_1d (title, signal, error, s_axis, x, x_axis, x_distribution)
%
%  or with IXTbase object:
%   >> w = IXTdataset_1d(base, title, signal, error, s_axis, x, x_axis, x_distribution)
%
%  Creates an IXTdataset_1d object with the following elements:
%
%   base                IXTbase 
% 	title				char			Title of dataset for plotting purposes
% 	signal              real    		Signal
% 	error				real    		Standard error
% 	s_axis				IXTaxis			Signal axis object containing caption and units codes
%                                     (Can also just give caption; multiline input in the form of a
%                                      cell array or a character array)
% 	x					real        	Values of bin boundaries (if histogram data)
% 						real            Values of data point positions (if point data)
% 	x_axis				IXTaxis			x-axis object containing caption and units codes
%                                     (Can also just give caption; multiline input in the form of a
%                                      cell array or a character array)
% 	x_distribution      logical         Distribution data flag

dataset_1d.base = IXTbase;
dataset_1d.title=' ';
dataset_1d.signal=0;
dataset_1d.error=0;
dataset_1d.s_axis = IXTaxis;
dataset_1d.x=0;
dataset_1d.x_axis = IXTaxis;
dataset_1d.x_distribution=false;
dataset_1d = class(dataset_1d,'IXTdataset_1d');


% Check for empty character fields and replace if necessary
for i = 1:length(varargin) 
    if ischar(varargin{i}) && isempty(varargin{i})
        varargin{i} = ' ';
    end
end


% Check for a special xye constructor
if(nargin == 1) && iscellnum(varargin)
    if ~isvector(varargin{1}), error('Check input x-axis is a vector'), end
    dataset_1d = libisisexc('IXTdataset_1d','createxye',dataset_1d,varargin{1}(:)',zeros(1,numel(varargin{1})),zeros(1,numel(varargin{1})));

elseif(nargin == 2) && iscellnum(varargin)
    if ~isvector(varargin{1}), error('Check input x-axis is a vector'), end
    if ~isvector(varargin{2}), error('Check input y-axis is a vector'), end
    if ~( (numel(varargin{1})==numel(varargin{2})) || (numel(varargin{1})==numel(varargin{2})+1) ), error('Number of x and y coordinates incompatible'), end
    dataset_1d = libisisexc('IXTdataset_1d','createxye',dataset_1d,varargin{1}(:)',varargin{2}(:)',zeros(1,numel(varargin{2})));

elseif (nargin == 3) && iscellnum(varargin)
    if ~isvector(varargin{1}), error('Check input x-axis is a vector'), end
    if ~isvector(varargin{2}), error('Check input y-axis is a vector'), end
    if ~isvector(varargin{3}), error('Check input error axis is a vector'), end
    if ~( ((numel(varargin{1})==numel(varargin{2})) || (numel(varargin{1})==numel(varargin{2})+1)) && (numel(varargin{2})==numel(varargin{3})) )
        error('Number of x, y and e coordinates incompatible')
    end
    dataset_1d = libisisexc('IXTdataset_1d','createxye',dataset_1d,varargin{1}(:)',varargin{2}(:)',varargin{3}(:)');

elseif (nargin > 0)
    % Check if first argument class is IXTbase, and prepend if not
    if ~isa(varargin{1},'IXTbase'), varargin={IXTbase,varargin{:}}; end
    if numel(varargin)~=8, error('Check number of arguments'), end
    % Perform some basic checks on input data
    ix=6; iy=3; ie=4; i_s_axis=5; i_x_axis=7;
    if ~isvector(varargin{ix}), error('Check input x-axis is a vector'), end
    if ~isvector(varargin{iy}), error('Check input y-axis is a vector'), end
    if ~isvector(varargin{ie}), error('Check input error axis is a vector'), end
    varargin{ix}=varargin{ix}(:)'; varargin{iy}=varargin{iy}(:)'; varargin{ie}=varargin{ie}(:)';
    if ~( ((numel(varargin{ix})==numel(varargin{iy})) || (numel(varargin{ix})==numel(varargin{iy})+1)) && (numel(varargin{iy})==numel(varargin{ie})) )
        error('Number of x, y and e coordinates incompatible')
    end
     % Try to make axis objects if not provided
    if ~isa(varargin{i_s_axis},'IXTaxis')
        if ischar(varargin{i_s_axis}) || iscellstr(varargin{i_s_axis})
            varargin{i_s_axis}=IXTaxis(varargin{i_s_axis});
        else
            error('Check type of signal axis information')
        end
    end
    if ~isa(varargin{i_x_axis},'IXTaxis')
        if ischar(varargin{i_x_axis}) || iscellstr(varargin{i_x_axis})
            varargin{i_x_axis}=IXTaxis(varargin{i_x_axis});
        else
            error('Check type of x-axis information')
        end
    end
    dataset_1d = libisisexc('IXTdataset_1d','create',dataset_1d,varargin);
    
end
