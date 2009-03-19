function [] = lz (varargin)
%-----------help for gtk change z limits function lz-
% Function Syntax: LZ(zlo,zhi)
% Purpose: Change Z axis limits 
% Output: None
% Input: zlo = low, zhi = high limits
% Example: 
% LZ(1,2) or lz 1 2 --> for z axes zlo = 1 zhi = 2
% LZ --> stretch z limit to fullest
%-----------------------------------------------------
% Dean Whittaker 2007
% I.Bustinduy 20/11/07


IXG_ST_STDVALUES = ixf_global_var('libisis_graphics','get','IXG_ST_STDVALUES');

[figureHandle_, axesHandle_, plotHandles_, otherHandle_] = ixf_get_related_handles(gca);
area_flag=[];
isXYZ='z';

% check for area plot, use colour instead of z limits

for i = 1:length(plotHandles_)
    
    plot_type = ixf_plotdata('get', plotHandles_(i), 'plot_type');

   if (plot_type == IXG_ST_STDVALUES.area_type) || plot_type == (IXG_ST_STDVALUES.patch_type)
        area_flag(i) = 1;
   end

   if plot_type == IXG_ST_STDVALUES.oned_type
        error('one d data only - lz is not applicable')
   end
   
end

if all(area_flag) && ~isempty(area_flag)       % check all plots are area (otherwise z might be set)
    isXYZ='c';
end

if nargin == 0
    [varargin{1}, varargin{2}] = ixf_get_limits('hdl', gca, isXYZ);
end

axescode = IXG_ST_STDVALUES.(isXYZ);
for i=1:nargin,
    if(~isnumeric(varargin{i})), varargin{i}=str2double(varargin{i}); end
end
ui_setaxes(axescode,varargin{:});

if strcmp(isXYZ,'z')            % set colour as well, if not set here
    ui_setaxes(3, varargin{:});
end

cbarH_ = findobj(figureHandle_, 'tag', 'Colorbar');

if cbarH_
    colorbar;
end
