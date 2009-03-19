function [fig_out, axes_out, plot_out] = dl(w,varargin)
%--------help for gtk line plot command dl --------------------------------
% Function Syntax: 
% [figureHandle_,axesHandle_,plotHandle_] = 
% DL(w,[property_name,property_value]) or
% DL(w,xlo,xhi) or
% DL(w,xlo,xhi,ylo,yhi)
% DL(runfile, 'det', ...)
% DL(runfile, 'mon', ...)
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
% Dataset may be IXTdataset_2d or IXTrunfile object.
% you can also give axis limit for x and y 
% Purpose: plot the data according to values and control properties (for
% figure, axes and plot)
%
% Example: 
% DL(w) --> default structure plot
% DL(w,'Color','red') --> override default structure values 
% DL(w,'default','my_struct','Color','red') --> override values 
% DL(w,'default','my_struct') --> from structure
% DL(w,10,20)
% DL(w,10,20,0,200)
%--------------------------------------------------------------------------
% Dean Whittaker 2007


[w, varargin] = ixf_parse_plotdata('IXTdataset_1d', w, varargin);

[figureHandle_,axesHandle_,plotHandle_] = uib_plotgeneral(w,'l',varargin{:});


if nargout > 0
    fig_out = figureHandle_;
    axes_out = axesHandle_;
    plot_out = plotHandle_;
end

