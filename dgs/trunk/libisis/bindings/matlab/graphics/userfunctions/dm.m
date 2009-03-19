function [fig_out, axes_out, plot_out] = dm(w,varargin)
%-------help for gtk marker plot command dm--------------------------------
% Function Syntax: 
% [figureHandle_,axesHandle_,plotHandle_] = 
% DM(w,[property_name,property_value]) or
% DM(w,xlo,xhi) or
% DM(w,xlo,xhi,ylo,yhi) 
% DM(runfile, 'det', ...)
% DM(runfile, 'mon', ...)
% 
% Output: figure,axes and plot handle
%
% Input: 1d dataset object and other control parameters (name value pairs)
%  you can also give axis limit for x and y.
%
%       'det'       If input is IXTrunfile - plot detector spectrum
%       'mon'       If input is IXTrunfile - plot monitor spectrum 
%
% NOTE: 'det' and 'mon' can not be used unless the input is an IXTrunfile. 
% 
% Dataset may be IXTdataset_2d or IXTrunfile object.%
% you can also give axis limit for x and y 
% Purpose: plot the data according to values and control properties (for
% figure, axes and plot)
%
% Example: 
% DM(w) --> default structure plot
% DM(w,'Color','red') --> override default structure values 
% DM(w,'default','my_struct','Color','red') --> override values 
% DM(w,'default','my_struct') --> from structure
% DM(w,10,20)
% DM(w,10,20,0,200)
%--------------------------------------------------------------------------
% Dean Whittaker 2007

[w, varargin] = ixf_parse_plotdata('IXTdataset_1d', w, varargin);

[figureHandle_,axesHandle_,plotHandle_] = uib_plotgeneral(w,'m',varargin{:});


if nargout > 0
    fig_out = figureHandle_;
    axes_out = axesHandle_;
    plot_out = plotHandle_;
end

