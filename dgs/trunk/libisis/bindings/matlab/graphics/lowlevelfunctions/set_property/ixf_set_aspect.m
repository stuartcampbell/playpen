function ixf_set_aspect(axesHandle_,varargin)
%---- help for libisis gtk ixf_set_aspect sets the aspect ratio of a plot--
%
% >> ixf_set_aspect(axesHandle, x_ulen, y_ulen, z_ulen)
% >> ixf_set_aspect(axesHandle, x_ulen, y_ulen)
% >> ixf_set_aspect(axesHandle, mode)
% >> ixf_set_aspect(axesHandle, [x_ulen, y_ulen, z_ulen])
% >> ixf_set_aspect(axesHandle)
% 
% where mode is either 'auto' or 'manual'. axesHandle is optional. 
%
% inputs: 
%
%       axesHandle: handle of the axes which is to be scaled.
%       x_ulen:     defines the unit length along the x axis
%       y_ulen:     defines the unit length along the y axis
%       z_ulen:     defines the unit length along the z axis]
%       mode:       either 'auto' or 'manual'
%
% This sets the scale of axes so that one unit along the x axis
% is the same as one unit along the y axis and z axis.
%
% If just given values for x_ulen and y_ulen, the z_ulen retains the same
% proportion to x and y as it did before the aspect was set. If given all 3
% values and one is NaN, then this axes will retain the same proportion.
%
% With just the axesHandle input, the function is the same as
%
% >> ixf_set_aspect(axesHandle, 1, 1)
%
% when the mode is set to 'auto', matlab chooses the aspect ratio which
% fills the figure the best.
%
%--------------------------------------------------------------------------

   
a=get(axesHandle_,'DataAspectRatio');

if nargin==1
    x_aspect=1;
    y_aspect=1;
    z_aspect= 2/(a(1)+a(2))*a(3);
elseif nargin == 2
    if ischar(varargin{1})
        switch varargin{1}
            case 'auto'
                set(axesHandle_,'DataAspectRatioMode','auto');
            case 'manual'
                set(axesHandle_,'DataAspectRatioMode','manual');
            otherwise
                error('Input arguments in ixf_aspect must contain either ''auto'', ''manual'' or at least 2 numbers defining unit lengths')
        end
    elseif isnumeric(varargin{1})
        if numel(varargin{1}) ==  2
            x_aspect = 1/varargin{1}(1);
            y_aspect = 1/varargin{1}(2);
            z_aspect = (1/varargin{1}(1) + 1/varargin{1}(2))/(a(1)+a(1))*a(3);
        elseif numel(varargin{1}) == 3
            if  ~isnan(varargin{1}(1))                   % if any are NaN, then assume keep current aspect 
                x_aspect = 1/varargin{1}(1);
            else
                x_aspect=(1/varargin{1}(2)+1/varargin{1}(3))/(a(2)+a(3))*a(1);
            end
            if ~isnan(varargin{1}(2))
                y_aspect = 1/varargin{1}(2);
            else
                y_aspect=(1/varargin{1}(1)+1/varargin{1}(3))/(a(1)+a(3))*a(2);
            end
            if ~isnan(varargin{1}(3))
                z_aspect = 1/varargin{1}(3);
            else
                z_aspect = (1/varargin{1}(1) + 1/varargin{1}(2))/(a(1)+a(1))*a(3);
            end
        end
    end
    
elseif nargin ==3
    x_aspect = 1/varargin{1};
    y_aspect = 1/varargin{2};
    z_aspect=(1/varargin{1}+1/varargin{2})/(a(1)+a(2))*a(3);
elseif nargin == 4
    if ~isnan(varargin{1})
        x_aspect = 1/varargin{1};
    else
        x_aspect=(1/varargin{2}+1/varargin{3})/(a(2)+a(3))*a(1);
    end

    if ~isnan(varargin{2})
        y_aspect = 1/varargin{2};
    else
        y_aspect=(1/varargin{1}+1/varargin{3})/(a(1)+a(3))*a(2);
    end
    
    if ~isnan(varargin{3})
        z_aspect = 1/varargin{3};
    else
        z_aspect = (1/varargin{1} + 1/varargin{2})/(a(1)+a(1))*a(3);
    end
end

set(axesHandle_,'DataAspectRatio',[x_aspect y_aspect z_aspect]);
