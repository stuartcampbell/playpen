function [fig_out, axes_out, plot_out] = peoc(w,varargin)
%--------------------------------------------------------------------------
%function syntax: PEOC(1ddataset_object,[property_name,property_value])
% purpose: plot a line over the current plot, regardless of it's current
% type
% input: 1d dataset object, property name and value
% output: none
% example: PEOC(w)
% PEOC(w,'color','red')
%--------------------------------------------------------------------------

% Dean Whittaker 2008

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

[name tag] = ixf_get_nametag('hdl',gcf);

[figureHandle_, axesHandle_, plotHandle_] = pe(w, 'name', name, 'tag', tag, varargin{:});

if nargout > 0
    fig_out = figureHandle_;
    axes_out = axesHandle_;
    plot_out = plotHandle_;
end