function [figureHandle_ axesHandle_ plotHandle_]=uib_plot_threed(w,format,varargin)
%-----------------Help for GTK uib_plot_threed-------------------------------
%
% purpose: plot interface for a single dataset3d object
%
% Syntax: [figurehandle, axeshandle,
% plothandle]=uib_plot_twod(w,format,varargin)
%
% inputs: w - dataset 2d object, format - type of graph to plot, varargin -
% optional settings for graph
% outputs: figure handle, axes handle, plot handle
%
%-----------updated: 15/08/2006, Dean Whittaker----------------------------
%global structures
[IXG_ST_INTERFACEVALIDATION, IXG_ST_STDVALUES]=ixf_global_var('libisis_graphics','get','IXG_ST_INTERFACEVALIDATION','IXG_ST_STDVALUES');
tot = numel(varargin);
% insure arguments match allowed values
ixf_validate_arg(IXG_ST_INTERFACEVALIDATION.uib_plotfigure,nargin,'greater');
ixf_validate_arg(IXG_ST_INTERFACEVALIDATION.ixf_null_value,rem(tot,2));


% check this is a single dataset 2d object
len = length(w);
% REBUNCH required here, currently not a function

if len==1
    [xlab, ylab,zlab] = make_label(w);
    title = w(1).title;
% the label for the third data axis does not seem to be plotted/determined here
else
    error('Can only plot one IXTdataset_3d at a time')
end



switch format

    case 'sliceomatic'    
        ixf_validate_plot('name',IXG_ST_STDVALUES.sliceomatic_name,'tag','sliceomatic',varargin{:});
           
        counter_value = IXG_ST_STDVALUES.counter_reset;
           [figureHandle_ axesHandle_ plotHandle_] = ixf_gen_interface('iname','plot_interface','fname','sliceomatic',w,'format',format,'name',IXG_ST_STDVALUES.sliceomatic_name,'tag',IXG_ST_STDVALUES.twod,'title',title,...
            'xlabel',xlab,'ylabel',ylab,'zlabel',zlab,'marker','none', 'counter', counter_value,varargin{:});
  
    otherwise
        error('incorrect format type')
end


