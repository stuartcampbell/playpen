function [fig_out, axes_out, plot_out] = de(w,varargin)
% ----help for gtk errorbar plot function de-------------------------------
% Function Syntax: 
% [figureHandle_,axesHandle_,plotHandle_] = 
% DE(w,[property_name,property_value]) or
% DE(w,xlo,xhi) or
% DE(w,xlo,xhi,ylo,yhi)
% DE(runfile, 'det', ...)
% DE(runfile, 'mon', ...)
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
% DE(w) --> default structure plot
% DE(w,'Color','red') --> override default structure values 
% DE(w,'default','my_struct','Color','red') --> override values 
% DE(w,'default','my_struct') --> from structure
% DE(w,10,20)
% DE(w,10,20,0,200)
%--------------------------------------------------------------------------
% Dean Whittaker 2007

%total
[w, varargin] = ixf_parse_plotdata('IXTdataset_1d', w, varargin);

[figureHandle_,axesHandle_,plotHandle_] = uib_plotgeneral(w,'e',varargin{:});


if nargout > 0
    fig_out = figureHandle_;
    axes_out = axesHandle_;
    plot_out = plotHandle_;
end

