function [fig_out, axes_out, plot_out] = dh(w,varargin)
%--------help for gtk histogram plot command dh----------------------------
% Function Syntax: 
% [figureHandle_,axesHandle_,plotHandle_] = 
% DH(w,[property_name,property_value]) or DH(w,xlo,xhi) or
% DH(w,xlo,xhi) or
% DH(w,xlo,xhi,ylo,yhi)
% DH(runfile, 'det', ...)
% DH(runfile, 'mon', ...)
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
% DH(w) --> default structure plot
% DH(w,'Color','red') --> override default structure values 
% DH(w,'default','my_struct','Color','red') --> override values 
% DH(w,'default','my_struct') --> from structure
% DH(w,10,20)
% DH(w,10,20,0,200)
% DH(
%--------------------------------------------------------------------------
% Dean Whittaker 2007

[w, varargin] = ixf_parse_plotdata('IXTdataset_1d', w, varargin);

[figureHandle_,axesHandle_,plotHandle_] = uib_plotgeneral(w,'h',varargin{:});


if nargout > 0
    fig_out = figureHandle_;
    axes_out = axesHandle_;
    plot_out = plotHandle_;
end