function ui_set_cscale(axesHandle_, varargin)
%----- lbisis gtk ui_set_cscale, log color axes standard user function-----------
%
% >> ui_set_cscale(axesHandle_, scale)
%
% changes the colour axes to a logorithmic or linear scale
%
% inputs:   axesHandle_     - handle of axes to be changed
%           scale           - 'log' or 'linear'

ixf_gen_interface('iname','setprop_interface','fname','cscale','axesHandle_',axesHandle_,'scale',varargin{1});

