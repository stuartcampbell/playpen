function ui_set(ident, type, varargin)
%----help for gtk standard user set function-------------------------------
%
% call syntax: ui_set(identifier, type, 'property','value',....)
%
% inputs: identifier - plot, axes or figure handle, or name tag of figure
% window, type = 'figure', 'axes', 'plot' depending on what needs to be
% changed, property and value pairs are what needs to be changed
%
% output: none
%
% purpose: to simplify the set commands, so that any identifier can be used
% to change any aspect of a gtk plot
%
% note: will accept arrays of handles and alter all graphs within that
% array, arrays must be of the form [1; 2; 3]
%
%--------------------------------------------------------------------------


IXG_ST_INTERFACEVALIDATION = ixf_global_var('libisis_graphics','get','IXG_ST_INTERFACEVALIDATION');

tot = numel(varargin);
ixf_validate_arg(IXG_ST_INTERFACEVALIDATION.ixf_null_value,rem(tot,2));

[figureHandle_, axesHandle_, plotHandle_, otherHandle_] = ixf_get_related_handles(ident);

ixf_gen_interface('iname','setprop_interface','fname','set',figureHandle_, axesHandle_, plotHandle_, otherHandle_,...
    type, varargin{:});
