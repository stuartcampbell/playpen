function [t]=ixf_place_text(varargin)

%---------help for libisis ixf_place_text----------------------------------
%Function Syntax: [text] = ixf_place_text(x,y,text)
%Purpose: function to place text on a graph at x and y points
%Output: text placed
%Input: Text
%Example:
%[text] = ixf_place_text('text')
%--------------------------------------------------------------------------

% initial checks
[IXG_ST_ERROR, IXG_ST_STDVALUES]=ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES');
IXG_ST_DEFAULT = ixf_name_tag_properties('get','IXGDEFAULT','IXGDEFAULT');

flag = ixf_checkinit('Currentfigure');
if (flag == IXG_ST_STDVALUES.false)
    ixf_display_error(IXG_ST_ERROR.no_figure);
end

st_local_default = ixf_get_standard_default(gcf, IXG_ST_DEFAULT, varargin{:});
% main function
if ischar(varargin{1})
    varargin{1} = [149 ' ' varargin{1}];
end
tHandle_ = text(varargin{:});

set(tHandle_,'color',st_local_default.text.tcolor,'units',st_local_default.text.tunits,'fontname',st_local_default.text.tfontname,...
    'fontsize',st_local_default.text.tfontsize,'fontangle',st_local_default.text.tfontangle,'fontweight',st_local_default.text.tfontweight,...
    'fontunits',st_local_default.text.tfontunits,'horizontalalignment', st_local_default.text.horizontalalignment,...
    'verticalalignment', st_local_default.text.verticalalignment, 'tag', st_local_default.text.ttag);

t=varargin{end};