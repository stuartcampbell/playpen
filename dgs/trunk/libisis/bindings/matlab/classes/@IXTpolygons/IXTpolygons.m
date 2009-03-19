function wout = IXTpolygons(varargin)
% constructor for IXTpolygons object
%
% >> wout = IXTpolygons('title', signal, error, IXTunits, vertices, faces, ...
%                       x_units, x_dist, y_units, y_dist)
%
% >> wout = IXTpolygons(win)
%
% Inputs:
%       title:      String          String containing title
%       signal:     n x 1 matrix    Signal data
%       error:      n x 1 matrix    Error data
%       s_axis:    IXTaxis        Signal axis
%       vertices:   m x 2 matrix    List of verticies in x,y plane
%       faces:      n x p matrix    List of faces (as in patch)
%       x_axis:    IXTaxis        x axis 
%       x_dist:     Logical         x distribution data flag
%       y_axis:    IXTaxis        y axis 
%       y_dist:     Logical         y distribution data flag
%
%       win:        Structure       Structure containing the above fields.
%           
% where n is the number of polygons, p is the number of vertices of the largest
% polygon, m is the number of points required to define all the
% polygons. 
%
% Output:
%
%       IXTpolygons with fields:
%                       title
%                       signal
%                       error
%                       s_axis
%                       vertices
%                       faces
%                       x_axis
%                       x_distribution
%                       y_axis
%                       y_distribution

% IXTunits replaced by IXTaxis to prevent any complaint, this is not a fortran object yet and may
% have to be changed in the future

wout_struct = struct('title',' ', 'signal',0,'error',0, 's_axis', IXTaxis, 'vertices', [0,0], 'faces', [1 1 1 1],  ...
    'x_axis',IXTaxis, 'x_distribution', false, 'y_axis', IXTaxis, 'y_distribution', false);
wout_default = wout_struct;
    % set our default structure, can alter structure contents at any time.
    
field_names = fieldnames(wout_struct); % get a list of field names

   
if nargin == 1  
    if isa(varargin{1},'IXTpolygons')
    wout = varargin{1};
    return
    elseif isstruct(varargin{1})
        field_names = fieldnames(varargin{1});
        for i = 1:length(field_names)
            wout_struct.(field_names{i}) = varargin{1}.(field_names{i});
        end
    else 
        error('Incorrect input - if one input is given it must be a structure')
    end

elseif nargin == numel(field_names)

        % simple constructor
    for i = 1:length(varargin)
        
        if ~ isempty(varargin{i})
            
            if  ~isa(varargin{i}, class(wout_struct.(field_names{i})))
                error(['Incorrect format for input field  ''' field_names{i} '''. This should be ' class(wout_struct.(field_names{i})) ]);
            end

            wout_struct.(field_names{i}) = varargin{i};
            
        end
        
    end
    
elseif nargin > 0
    error('Incorrect number of inputs')
end

% ---------- error checking

% get sizes to compare 

wout = class(wout_struct,'IXTpolygons');

res = polygon_check(wout);

if res
    warning('Polygons object not properly constructed - a blank object has been returned.')
    wout = wout_default;
    wout = class(wout,'IXTpolygons');
end

