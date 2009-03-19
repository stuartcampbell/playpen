function logc(axesHandle_)
%----libisis gtk logc help-----------
%
% >> logc
% >> logc(axesHandle_)
%
% changes the colour axes to a logorithmic scale
%
% if axes handle is given, the axes given will be changed, if no arguments
% are give, the current axes will be used.
% Dean Whittaker 2007
if nargin == 0
    axesHandle_ = gca;
end

ixf_plotdata('set', axesHandle_, 'clog_flag', true);

ixf_redraw_graph(axesHandle_)
