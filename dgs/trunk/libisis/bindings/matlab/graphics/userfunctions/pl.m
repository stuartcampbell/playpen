function [fig_out, axes_out, plot_out] = pl(w,varargin)
%-----help for gtk line overplot command pl--------------------------------
%function syntax: PL(1ddataset_object,[property_name,property_value])
% purpose:overplot
% input: 1d dataset object, property name and value
% output: none
% example: PL(w)
% PL(w,'color','red')
%--------------------------------------------------------------------------


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

% get a list of all open figures
[figure_handles, axes_handles] = get_all_handles(0);
isheld = false(size(axes_handles));

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


%call already prepared dl utility
%[figureHandle_, axesHandle_, plotHandle_] = dl(w,'counter',IXG_ST_STDVALUES.counter_increment,varargin{:});
[figureHandle_, axesHandle_, plotHandle_] = dl(w,varargin{:},'counter',IXG_ST_STDVALUES.counter_increment);



        
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