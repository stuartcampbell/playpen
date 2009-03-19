function ui_set_aspect(varargin)
%--- help for libisis gtk ui_set_aspect set aspect ratio user function-----
%
% >> ui_set_aspect(axesHandle, x_ulen, y_ulen, z_ulen)
% >> ui_set_aspect(axesHandle, x_ulen, y_ulen)
% >> ui_set_aspect(axesHandle, mode)
% >> ui_set_aspect(axesHandle)
%
%
% or one can use name tags to identify which graphs to change the aspect
% ratio of
%
% >> ui_set_aspect(name, tag, xulen, yulen, zulen)
%
% where xulen, yulen and zulen are optional 
%
% axesHandle / name tag can be omitted, in this case, the current axes will be used.
%
% inputs: 
%
%       axesHandle: handle of the axes which is to be scaled. Can be a
%                   vector array of axes handles - in which case the
%                   operation is performed on each handle in turn.
%       x_ulen:     defines the unit length along the x axis
%       y_ulen:     defines the unit length along the y axis
%       z_ulen:     defines the unit length along the z axis]
%       mode:       either 'auto' or 'manual'
%
% This sets the scale of axes so that one unit along the x axis
% is the same as one unit along the y axis and z axis.
%
% If just given values for x_ulen and y_ulen, the x_ulen retains the same
% proportion to x and y as it did before the aspect was set
%
% With just the axesHandle input, the function is the same as
%
% >> ui_set_aspect(axesHandle, 1, 1)
%
% when the mode is set to 'auto', matlab chooses the aspect ratio which
% fills the figure the best.
%
%--------------------------------------------------------------------------
argin = {};

if nargin >= 2 && ischar(varargin{1}) && ischar(varargin{2})
    if ixf_checkforfigure(varargin{1}, varargin{2})
        [figureHandle_, axesHandle_] = ixf_get_related_handles([varargin{1}, varargin{2}]);
        for i = 3:nargin
            argin{i-2} = varargin{i};
        end
    else
        error(['''' varargin{1} ''' and ''' varargin{2} ''' are not recognised as name tags with currently open plots'])
    end
elseif nargin >=1 && all(ishandle(varargin{1}))
    if strcmpi(get(varargin{1},'type'), 'axes')
        axesHandle_ = varargin{1};
        for i = 2:nargin
            argin{i-1} = varargin{i};
        end
    else
        axesHandle_ = gca;
        argin = varargin;
    end
else
    axesHandle_ = gca;
    argin = varargin;
end

for i = 1:length(axesHandle_)
    ixf_gen_interface('iname','setprop_interface','fname','setaspect', axesHandle_(i), argin{:});
end
