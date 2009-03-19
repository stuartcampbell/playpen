function dataset_2d = IXTdataset_2d( varargin )
% Create IXTdataset_2d object
%
%   >> w = IXTdataset_2d (x)
%   >> w = IXTdataset_2d (x,y,signal) 
%   >> w = IXTdataset_2d (x,y,signal,error)
%   >> w = IXTdataset_2d (title, signal, error, s_axis,...
%                           x, x_axis, x_distribution, y, y_axis, y_distribution)
%
%  or with IXTbase object:
%   >> w = IXTdataset_2d (base, title, signal, error, s_axis,...
%                           x, x_axis, x_distribution, y, y_axis, y_distribution)
%
%  Creates an IXTdataset_12d object with the following elements:
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
%
%   y                   real            -|
%   y_axis              IXTaxis          |- Similarly for y axis
%   y_distribution      logical         -|

%  IXTdataset_2d(IXTbase, 'title', [signal], [error], IXTaxis,
%  [x], IXTaxis, [x_distribution],
%  [y], IXTaxis, [y_distribution])


% core object
dataset_2d.base = IXTbase;
dataset_2d.title=' ';
dataset_2d.signal=0;
dataset_2d.error=0;
dataset_2d.s_axis=IXTaxis;
dataset_2d.x=0;
dataset_2d.x_axis=IXTaxis;
dataset_2d.x_distribution=false;
dataset_2d.y=0;
dataset_2d.y_axis=IXTaxis;
dataset_2d.y_distribution=false;
dataset_2d = class(dataset_2d,'IXTdataset_2d');

% Check for empty character fields and replace if necessary
for i = 1:length(varargin) 
    if ischar(varargin{i}) && isempty(varargin{i})
        varargin{i} = ' ';
    end
end

% Check for a special xyze constructor
if(nargin == 2) && iscellnum(varargin)
    if ~isvector(varargin{1}), error('Check input x-axis is a vector'), end
    if ~isvector(varargin{2}), error('Check input y-axis is a vector'), end
    dataset_2d = libisisexc('IXTdataset_2d','createxyze',dataset_2d,varargin{1}(:)',varargin{2}(:)',...
        zeros(length(varargin{1}),length(varargin{2})),zeros(length(varargin{1}),length(varargin{2})));
    
elseif(nargin == 3) && iscellnum(varargin)
    if ~isvector(varargin{1}), error('Check input x-axis is a vector'), end
    if ~isvector(varargin{2}), error('Check input y-axis is a vector'), end
    nx=numel(varargin{1}); ny=numel(varargin{2});
    if ~( isequal(size(varargin{3}),[nx,ny]) || isequal(size(varargin{3}),[nx-1,ny]) ||...
          isequal(size(varargin{3}),[nx,ny-1]) || isequal(size(varargin{3}),[nx-1,ny-1])) || isempty(varargin{3})
        error('Signal array inconsistent with x-axis and y-axis arrays')
    end
    dataset_2d = libisisexc('IXTdataset_2d','createxyze',dataset_2d,varargin{1}(:)',varargin{2}(:)',varargin{3},zeros(size(varargin{3})));
    
elseif (nargin == 4) && iscellnum(varargin)
    if ~isvector(varargin{1}), error('Check input x-axis is a vector'), end
    if ~isvector(varargin{2}), error('Check input y-axis is a vector'), end
    nx=numel(varargin{1}); ny=numel(varargin{2});
    if ~( isequal(size(varargin{3}),[nx,ny]) || isequal(size(varargin{3}),[nx-1,ny]) ||...
          isequal(size(varargin{3}),[nx,ny-1]) || isequal(size(varargin{3}),[nx-1,ny-1])) || isempty(varargin{3})
        error('Signal array inconsistent with x-axis and y-axis arrays')
    end
    if ~isequal(size(varargin{3}),size(varargin{4}))
        error('Signal and error arrays have different sizes')
    end
    dataset_2d = libisisexc('IXTdataset_2d','createxyze',dataset_2d,varargin{1}(:)',varargin{2}(:)',varargin{3}, varargin{4});
    
elseif (nargin > 0)           
    % Check if first argument class is IXTbase, and prepend if not
    if ~isa(varargin{1},'IXTbase'), varargin={IXTbase,varargin{:}}; end
    if numel(varargin)~=11, error('Check number of arguments'), end
    % Perform some basic checks on input data
    ix=6; iy=9; isig=3; ie=4; i_x_axis=7; i_y_axis=10; i_s_axis=5;
    if ~isvector(varargin{ix}), error('Check input x-axis is a vector'), end
    if ~isvector(varargin{iy}), error('Check input y-axis is a vector'), end
    nx=numel(varargin{ix}); ny=numel(varargin{iy});
    if ~( isequal(size(varargin{isig}),[nx,ny]) || isequal(size(varargin{isig}),[nx-1,ny]) ||...
          isequal(size(varargin{isig}),[nx,ny-1]) || isequal(size(varargin{isig}),[nx-1,ny-1])) || isempty(varargin{isig})
        error('Signal array inconsistent with x-axis and y-axis arrays')
    end
    if ~isequal(size(varargin{isig}),size(varargin{ie}))
        error('Signal and error arrays have different sizes')
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
    if ~isa(varargin{i_y_axis},'IXTaxis')
        if ischar(varargin{i_y_axis}) || iscellstr(varargin{i_y_axis})
            varargin{i_y_axis}=IXTaxis(varargin{i_y_axis});
        else
            error('Check type of y-axis information')
        end
    end
    dataset_2d = libisisexc('IXTdataset_2d','create',dataset_2d,varargin);
    
end
