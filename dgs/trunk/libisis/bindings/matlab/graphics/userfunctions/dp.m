function [fig_out, axes_out, plot_out] = dp(w,varargin)
%----------------help for gtk dp errorbar and marker plot command----------
%
% purpose: plot markers and errorbars.
% 
% Function Syntax: 
% [ figureHandle_,axesHandle_,plotHandle_] = 
% DP(w,[property_name,property_value]) or
% DP(w,xlo,xhi) or
% DP(w,xlo,xhi,ylo,yhi)
% DP(runfile, 'det', ...)
% DP(runfile, 'mon', ...)
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
% list of control propertie names
% >>IXG_ST_DEFAULT.figure
% >>IXG_ST_DEFAULT.plot
% >>IXG_ST_DEFAULT.axes
% you can also give axis limit for x and y 
% Purpose: plot the data according to values and control properties (for
% figure, axes and plot)
%
% Examples: 
% DP(w) --> default structure plot
% DP(w,'Color','red') --> override default structure values 
% DP(w,'default','my_struct','Color','red') --> override values 
% DP(w,'default','my_struct') --> from structure
% DP(w,10,20)
% DP(w,10,20,0,200)
%--------------------------------------------------------------------------
% Dean Whittaker 2007

[w, varargin] = ixf_parse_plotdata('IXTdataset_1d', w, varargin);

[figureHandle_,axesHandle_,plotHandle_] = uib_plotgeneral(w,'p',varargin{:});


if nargout > 0
    fig_out = figureHandle_;
    axes_out = axesHandle_;
    plot_out = plotHandle_;
end
