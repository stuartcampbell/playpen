function set_plot(varargin)
%--------------help for gtk set plot properties set_plot commdand----------
%
% syntax: SET_PLOT(ident,'property','value', 'property','value',...)
%       >>` set_plot(name,tag,'property','value', 'property','value',...)
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
%
% output: none
%
% purpose: to set properties of the plots (within gtk)
%
% example: SET_PLOT(1,'color','green')
% sets all plots in figure 1 to the colour green
%
% >> SET_PLOT(myplot,'linestyle','--')
% sets the linestyle of all plots with the nametag myplot to dashed
%
%--------------------------------------------------------------------------

% Dean Whittaker 2008

if nargin < 3
    ui_set(gcf,'plot',varargin{:})
elseif ~rem(nargin,2) && ischar(varargin{1}) && ischar(varargin{2})
    figure_handles = ixf_checkforfigure(varargin{1}, varargin{2});
    if figure_handles
        ui_set(figure_handles,'plot',varargin{3:end});
    else
        error('No currently open figure has that name and tag')
    end
elseif isnumeric(varargin{1})
    ui_set(varargin{1},'plot',varargin{2:end});
else
    error('incorrect input identifier')
end