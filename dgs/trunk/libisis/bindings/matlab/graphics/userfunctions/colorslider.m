function colorslider(figureHandle_)
%------ libisis colorslider function help ------------------------------
% Synonym for color_slider, to avoid conflict with mslice
%
% >> colorslider(figureHandle_)
%
% adds sliders to the figure "figureHandle_" to control the colour scale of
% the plot. uses ixf_colour_slider function as callback.
%
% if not given a figureHandle, the current figure will be used.
%
% Dean Whittaker 2007, adapted from mslice.
%

if nargin < 1
    figureHandle_ = gcf;
end

ui_color_slider(figureHandle_)