function [fig_out, axes_out, plot_out] = ph(w,varargin)
%----help for gtk histogram overplot command ph----------------------------
%
% function syntax: PH(1ddataset_object,[property_name,property_value])
%
% purpose:overplot with histogram data
%
% input: 1d dataset object,property name and value
% output: none
%
% example: PH(w)
% PH(w,'color','red')
%-----------------------------------

% Dean Whittaker 2008

%global structures
[IXG_ST_ERROR, IXG_ST_STDVALUES] = ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES');

%check args
if ( nargin < 1 )
    ixf_display_error(IXG_ST_ERROR.wrong_arg);
end

%check my figure
currflag = ixf_checkinit(IXG_ST_STDVALUES.currentfigure);
if (currflag == IXG_ST_STDVALUES.false)
    ixf_display_error(IXG_ST_ERROR.no_figure);
end

for i = 1:length(axes_handles)
    % set the state of the plot to be held (same as "hold on" but without
    % messages)
    state = get(axes_handles(i), 'NextPlot');
    set(axes_handles(i),'NextPlot','add');
    
    % keep track of plots which were held before this operation
    if strcmp(state, 'add')
        isheld(i) = true;
    end
    
end   

%call already prepared dh utility
%[figureHandle_, axesHandle_, plotHandle_] = dh(w,'counter',IXG_ST_STDVALUES.counter_increment,varargin{:});
[figureHandle_, axesHandle_, plotHandle_] = dh(w,varargin{:},'counter',IXG_ST_STDVALUES.counter_increment);
for i = 1:length(axes_handles)
    
    % only reset the state of the axes if it wasn't already held before
    if ~isheld(i)
        set(axes_handles(i),'NextPlot','replace');
    end

end   

if nargout > 0
    fig_out = figureHandle_;
    axes_out = axesHandle_;
    plot_out = plotHandle_;
end