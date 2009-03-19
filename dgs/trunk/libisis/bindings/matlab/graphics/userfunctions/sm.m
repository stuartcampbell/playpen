function [fig_out, axes_out, plot_out] = sm(w, varargin)

% sliceomatic    Plots 3D dataset using sliceomatic
%
% Syntax:
%   >> [figure, axes, plot] sliceomatic (w)
%   >> sliceomatic (w,['property','value','property','value',...])
%
% Inputs: 
% 
%       w           IXTdataset_3d object ONLY
%       property-value pairs
%
%       Outputs:    OPTIONAL figure, axes and plot handles
%
% Accepts many of the property value pairs that libisis accepts. See
% libisis documentation for further information
%
%--------------
% ISONORMALS
%--------------
% 
% By default isonormals are not calculated to save time. Calculating Isonormals allow
% one to plot isosurfaces in the sliceomatic plot. To activate this
% feature, use the following syntax:
%
% >> plot(w,'isonormals',true, ['property','value','property','value',...])
%
% 
% NOTES:
%
% - Ensure that the slice color plotting is in 'texture' mode -
%      On the 'AllSlices' menu click 'Color Texture'. No indication will
%      be made on this menu to show that it has been selected, but you can
%      see the result if you right-click on an arrow indicating a slice on
%      the graphics window.
%
% - To set the default for future Sliceomatic sessions - 
%      On the 'Object_Defaults' menu select 'Slice Color Texture'

% Dean Whittaker 2008

[w, varargin] = ixf_parse_plotdata('IXTdataset_3d', w, varargin);

tot = numel(varargin);

if (tot==6) && (isnumeric(varargin{1}) && isnumeric(varargin{2}) && isnumeric(varargin{3}) && isnumeric(varargin{4})&& isnumeric(varargin{5}) && isnumeric(varargin{6}))
        [figureHandle_,axesHandle_,plotHandle_] = uib_plot_threed(w,'sliceomatic','xlim',[varargin{1},varargin{2}],'ylim',[varargin{3},varargin{4}],'zlim',[varargin{5},varargin{6}]);
else
    [figureHandle_,axesHandle_,plotHandle_] = uib_plot_threed(w,'sliceomatic',varargin{:});    
end

if nargout > 0
    fig_out = figureHandle_;
    axes_out = axesHandle_;
    plot_out = plotHandle_;
end

