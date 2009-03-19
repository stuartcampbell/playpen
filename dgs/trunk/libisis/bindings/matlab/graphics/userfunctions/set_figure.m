function set_figure(varargin)
%--------------help for gtk set figure properties set_figure function------
%
% syntax: SET_FIGURE (ident,'property','value', 'property','value',...)
%       >>` set_figure(name,tag,'property','value', 'property','value',...)
%
% inputs: 
%    ident can be either of the following:
%           the figure number
%           axes handle
%           plot handle
%
%   name - the name of a plot} optional instead of ident.
%   tag - the tag of a plot. }
%
%    property, value pairs - properties to be set
%
% output: none
%
% purpose: to set properties of the figure (within libisis)
%
% example:  SET_FIGURE(1, 'color', 'red')
%           SET_FIGURE('myplot tag', 'color', 'blue')
%           
%--------------------------------------------------------------------------

% Dean Whittaker 2008

if nargin < 3
    ui_set(gcf,'figure',varargin{:})
elseif ~rem(nargin,2) && ischar(varargin{1}) && ischar(varargin{2})
    figure_handles = ixf_checkforfigure(varargin{1}, varargin{2});
    if figure_handles
        ui_set(figure_handles,'figure',varargin{3:end});
    else
        error('No currently open figure has that name and tag')
    end
elseif isnumeric(varargin{1})
    ui_set(varargin{1},'figure',varargin{2:end});
else
    error('incorrect input identifier')
end