function [fig_out, axes_out, plot_out] = da(w,varargin)
%--------help for gtk area plot da command---------------------------------
%
% Area plot of dataset_2d (or array of dataset_1d)
%
% Function Syntax: 
% [figureHandle_,axesHandle_,plotHandle_] = DA(w,[property_name,property_value]) or
% DA(w,xlo,xhi) or
% DA(w,xlo,xhi,ylo,yhi)
% DA(runfile, 'det', ...)
% DA(runfile, 'mon', ...)
% 
% Output: figure,axes and plot handle
%
% Input:  IXTdataset_2d object and other control parameters (name value pairs)
%  you can also give axis limit for x and y.
%
%       'det'       If input is IXTrunfile - plot detector spectrum
%       'mon'       If input is IXTrunfile - plot monitor spectrum 
%
% NOTE: 'det' and 'mon' can not be used unless the input is an IXTrunfile. 
% 
% Dataset may be IXTdataset_2d, IXTrunfile object or array of IXTdataset_1d objects.
%
% >>IXG_ST_DEFAULT.figure
% >>IXG_ST_DEFAULT.plot
% >>IXG_ST_DEFAULT.axes
% you can also give axis limit for x, y and z
%
% Purpose: plot the data according to values and control properties (for
% figure, axes and plot)
%
% Example: 
% DA(w) --> default structure plot
% DA(w,'Color','red') --> override default structure values 
% DA(w,'default','my_struct','Color','red') --> override values 
% DA(w,'default','my_struct') --> from structure
% DA(w,10,20)
% DA(w,10,20,0,200)
%-------------------updated 15/08/2006, Dean Whittaker---------------------

%total

[w, varargin] = ixf_parse_plotdata('IXTdataset_2d', w, varargin);

tot = numel(varargin);

if (tot==6) && (isnumeric(varargin{1}) && isnumeric(varargin{2}) && isnumeric(varargin{3}) && isnumeric(varargin{4})&& isnumeric(varargin{5}) && isnumeric(varargin{6}))
        [figureHandle_,axesHandle_,plotHandle_] = uib_plot_twod(w,'area','xlim',[varargin{1},varargin{2}],'ylim',[varargin{3},varargin{4}],'clim',[varargin{5},varargin{6}]);
else
    [figureHandle_,axesHandle_,plotHandle_] = uib_plot_twod(w,'area',varargin{:});    
end

if nargout > 0
    fig_out = figureHandle_;
    axes_out = axesHandle_;
    plot_out = plotHandle_;
end
