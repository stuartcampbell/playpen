function [fig_out, axes_out, plot_out] = dd(w,varargin)
%----------------help for gtk dp errorbar and marker plot------------------
%
% purpose: plot errorbars, markers and line through the data
% 
% Function Syntax: 
% [ figureHandle_,axesHandle_,plotHandle_] = 
% DD(w,[property_name,property_value]) or
% DD(w,xlo,xhi) or
% DD(w,xlo,xhi,ylo,yhi)
% DD(runfile, 'det', ...)
% DD(runfile, 'mon', ...)
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
%
% Purpose: plot the data according to values and control properties (for
% figure, axes and plot)
%
% Example: 
% DD(w) --> default structure plot
% DD(w,'Color','red') --> override default structure values 
% DD(w,'default','my_struct','Color','red') --> override values 
% DD(w,'default','my_struct') --> from structure
% DD(w,10,20)
% DD(w,10,20,0,200)
%--------------------------------------------------------------------------
% Dean Whittaker 2007

[w, varargin] = ixf_parse_plotdata('IXTdataset_1d', w, varargin);

[figureHandle_,axesHandle_,plotHandle_] = uib_plotgeneral(w,'d',varargin{:});


if nargout > 0
    fig_out = figureHandle_;
    axes_out = axesHandle_;
    plot_out = plotHandle_;
end

