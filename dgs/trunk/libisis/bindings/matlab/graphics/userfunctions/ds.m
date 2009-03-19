function [fig_out, axes_out, plot_out] = ds(w,varargin)
%---------------help for gtk surface plot, ds command----------------------
%
% surface plot of a dataset_2d object (or array of dataset_1d objects) 
%
% Function Syntax: 
% [figureHandle_,axesHandle_,plotHandle_] = 
% DS(w,[property_name,property_value]) or
% DS(w,xlo,xhi) or
% DS(w,xlo,xhi,ylo,yhi)
% DS(runfile, 'det', ...)
% DS(runfile, 'mon', ...)
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
% Dataset may be IXTdataset_2d, IXTrunfile object or array of IXTdataset_1d
% objects.
% if plotting an ARRAY of dataset2d objects, set the separation property to
% 'seperate' by adding 'SEPARATIOIN','ON' to the optional arguments,
% otherwise the data will be combined and all points considered on the same
% graph  i.e.
%
% DS(w,'separation','on','color','red'); 
%
% if separation is off, the data will be interpolated and plotted at
% equally distributed points along the axes between the maxima and minima
% of the data, the number of points is the same as the number of datapoints
% in ALL the data, unless it reaches the maximum or is specified by
%
% DS(w,'noxvalues',xx,'noyvalues',yy);
%
% where xx and yy are the number of x points and y points respectively to
% plot. 
%
% Output: figure,axes and plot handle
% Input: 2d dataset object and other control parameters (name value pairs)
% list of control propertie names:
%
% >>IXG_ST_DEFAULT.figure
% >>IXG_ST_DEFAULT.plot
% >>IXG_ST_DEFAULT.axes
% you can also give axis limit for x, y and z
%
% Purpose: plot the data on a surface according to values and control properties (for
% figure, axes and plot)
%
% Example: 
% DS(w) --> default structure plot
% DS(w,'Color','red') --> override default structure values 
% DS(w,'default','my_struct','Color','red') --> override values 
% DS(w,'default','my_struct') --> from structure
% DS(w,10,20)
% DS(w,10,20,0,200)
% DS(ww,10,20,0,400)
%-------------------updated 24/08/2006, Dean Whittaker---------------------

%total
[w, varargin] = ixf_parse_plotdata('IXTdataset_2d', w, varargin);

tot = numel(varargin);

if (tot==6) && (isnumeric(varargin{1}) && isnumeric(varargin{2}) && isnumeric(varargin{3}) && isnumeric(varargin{4})&& isnumeric(varargin{5}) && isnumeric(varargin{6}))
        [figureHandle_,axesHandle_,plotHandle_] = uib_plot_twod(w,'surface','xlim',[varargin{1},varargin{2}],'ylim',[varargin{3},varargin{4}],'zlim',[varargin{5},varargin{6}]);
else
    [figureHandle_,axesHandle_,plotHandle_] = uib_plot_twod(w,'surface',varargin{:});    
end

if nargout > 0
    fig_out = figureHandle_;
    axes_out = axesHandle_;
    plot_out = plotHandle_;
end
