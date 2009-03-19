function [fig_out, axes_out, plot_out] = mp(w,varargin)
%------help for gtk multiplot command mp-----------------------------------
% multiplot an array of dataset_1d or single dataset_2d
%
% Function Syntax: figure_handle = MP(w,['name',value,'tag',value,options])
% MP(runfile, 'det', ...)
% MP(runfile, 'mon', ...)
% 
% Output: figure,axes and plot handle
%
% Input:  IXTdataset_1d array and other control parameters (name value pairs)
%  you can also give axis limit for x and y.
%
%       'det'       If input is IXTrunfile - plot detector spectrum
%       'mon'       If input is IXTrunfile - plot monitor spectrum 
%
% NOTE: 'det' and 'mon' can not be used unless the input is an IXTrunfile. 
% 
% Dataset may be IXTdataset_2d, IXTrunfile object or array of IXTdataset_1d
% objects.
%
% Examples: 
%
% MP(w) 
% the above example multiplot spectrum(w) data
%
% MP(w,'name','tobie') 
% the above example multiplot spectrum(w) data with name as tobie
%
% MP(w,'name','tobie','tag','1d') 
% the above example multiplot spectrum(w) data with name as tobie and tag as
% 1d
%
%--------------------------------------------------------------------------

% Dean Whittaker 2008

[w, varargin] = ixf_parse_plotdata('IXTdataset_1d', w, varargin);

if isa(w,'IXTdataset_2d')
    [xlab,ylab,zlab]=make_label(w);
elseif isa(w,'IXTdataset_1d');
% ylabel cannot be determined from a dataset_1d, so is forced to be
% Spectrum Number, use the first dataset_1d in the array
[xlab,zlab]=make_label(w(1));
    ylab = 'Spectrum Number';
else
    error('ERROR: Invalid input to mp function, must be an IXTdataset_1d or IXTdataset_2d')
end


tot = numel(varargin);

if (tot==6) && (isnumeric(varargin{1}) && isnumeric(varargin{2}) && isnumeric(varargin{3}) && isnumeric(varargin{4})&& isnumeric(varargin{5}) && isnumeric(varargin{6}))
        [figureHandle_,axesHandle_,plotHandle_] = uib_multiplot(w, 'xlim',[varargin{1},varargin{2}],'ylim',[varargin{3},varargin{4}],'zlim',[varargin{5},varargin{6}],'xlabel',xlab, 'ylabel',ylab,'zlabel',zlab,varargin{:});
else
        [figureHandle_,axesHandle_,plotHandle_] =uib_multiplot(w,'xlabel',xlab, 'ylabel',ylab,'zlabel',zlab,varargin{:});    
end

if nargout > 0
    fig_out = figureHandle_;
    axes_out = axesHandle_;
    plot_out = plotHandle_;
end
