

function [figureHandle_ axesHandle_ plotHandle_] = uib_multiplot(w,varargin)
%--------------------------------------------------------------------------
%Function Syntax: figure_handle = uib_multiplot(w,['name',value,'tag',value])
%Output: figure handle
%Input: 1d dataset object, control parameter like name and tag can be
%given 
%Purpose: multiplot the data
%Example: 
%uib_multiplot(w) 
%the above example multiplot spectrum(w) data
%uib_multiplot(w,'name','tobie') 
%the above example multiplot spectrum(w) data with name as tobie
%uib_multiplot(w,'name','tobie','tag','1d') 
%the above example multiplot spectrum(w) data with name as tobie and tag as
%1d
%--------------------------------------------------------------------------

%global structures
if isnumeric(w)
    w = getspectra(w);
end

[IXG_ST_INTERFACEVALIDATION IXG_ST_STDVALUES]=ixf_global_var('libisis_graphics','get','IXG_ST_INTERFACEVALIDATION', 'IXG_ST_STDVALUES');

ixf_validate_arg(IXG_ST_INTERFACEVALIDATION.uib_plotfigure,nargin,'greater');
tot = numel(varargin);
ixf_validate_arg(IXG_ST_INTERFACEVALIDATION.ixf_null_value,rem(tot,2));

if length(w) == 1
title = w.title;
else
    if ~ ixf_check_labels(w) % check that labels are same, if not then display a warning
        display('WARNING: Axes labels or distribution in the elements of the array are different, information from the first dataset has been used for plotting')
    end
    title = w(1).title;
end

%rebunch
for i=1:length(w)
w(i) = rebunch(w(i),IXG_ST_STDVALUES.binning);
end
ixf_validate_plot('name', IXG_ST_STDVALUES.multiplot_name, 'tag',IXG_ST_STDVALUES.multioned,varargin{:});
[figureHandle_ axesHandle_ plotHandle_] = ixf_gen_interface('iname','plot_interface','fname','multiplot',w,'format','multiplot','name',IXG_ST_STDVALUES.multiplot_name,'tag',IXG_ST_STDVALUES.multioned,'title',title,...
    'marker','none','facealpha',0,varargin{:});





