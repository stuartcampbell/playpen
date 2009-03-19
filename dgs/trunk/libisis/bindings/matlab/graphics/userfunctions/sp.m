function [fig_out, axes_out, plot_out] = sp(w, varargin)
%---------------help for GTK stem plot command sp--------------------------
%
% Stem plot of a dataset_2d object (or array of dataset_1d objects) 
%
% Function Syntax: 
% [figureHandle_,axesHandle_,plotHandle_] = 
% SP(w,[property_name,property_value]) or
% SP(w,xlo,xhi) or
% SP(w,xlo,xhi,ylo,yhi)
%
% Output: figure,axes and plot handle
% Input: 2d dataset object or array of 1d dataset objects and other control parameters (name value pairs)
% list of control propertie names:
%
% >>IXG_ST_DEFAULT.figure
% >>IXG_ST_DEFAULT.plot
% >>IXG_ST_DEFAULT.axes
% you can also give axis limit for x, y and z
%
% Purpose: plot the data as stems from the x-y plane
%
% Example: 
% SP(w) --> default structure plot
% SP(w,'Color','red') --> override default structure values 
% SP(w,'default','my_struct','Color','red') --> override values 
% SP(w,'default','my_struct') --> from structure
% SP(w,10,20)
% SP(w,10,20,0,200)
% SP(ww,10,20,0,400)
%-------------------updated 26/09/2006, Dean Whittaker---------------------

tot = numel(varargin);

if strcmp(class(w),'IXTdataset_1d')
    w=ixf_oned_to_twod(w);
end

if ( tot == 2)
    if (isnumeric(varargin{1}) && isnumeric(varargin{2}))
        [figureHandle_,axesHandle_,plotHandle_] = uib_plot_twod(w,'stem','xlim',[varargin{1},varargin{2}]);
    else
        [figureHandle_,axesHandle_,plotHandle_] = uib_plot_twod(w,'stem',varargin{:});
    end
elseif (tot == 4)
    if (isnumeric(varargin{1}) && isnumeric(varargin{2}) && isnumeric(varargin{3}) && isnumeric(varargin{4}))
        [figureHandle_,axesHandle_,plotHandle_] = uib_plot_twod(w,'stem','xlim',[varargin{1},varargin{2}],'ylim',[varargin{3},varargin{4}]);
    else 
        [figureHandle_,axesHandle_,plotHandle_] = uib_plot_twod(w,'stem',varargin{:});
    end    
elseif (tot==6)
    if (isnumeric(varargin{1}) && isnumeric(varargin{2}) && isnumeric(varargin{3}) && isnumeric(varargin{4})&& isnumeric(varargin{5}) && isnumeric(varargin{6}))
        [figureHandle_,axesHandle_,plotHandle_] = uib_plot_twod(w,'stem','xlim',[varargin{1},varargin{2}],'ylim',[varargin{3},varargin{4}],'zlim',[varargin{5},varargin{6}]);
    else
        [figureHandle_,axesHandle_,plotHandle_] = uib_plot_twod(w,'stem',varargin{:});
    end
else
    [figureHandle_,axesHandle_,plotHandle_] = uib_plot_twod(w,'stem',varargin{:});    
end

if nargout > 0
    fig_out = figureHandle_;
    axes_out = axesHandle_;
    plot_out = plotHandle_;
end
