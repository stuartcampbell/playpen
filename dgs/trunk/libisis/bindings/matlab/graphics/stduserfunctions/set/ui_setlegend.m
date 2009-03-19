function ret = ui_setlegend(varargin)
%----------------------------------------------------
%Function Syntax: ret = ui_setlegend(string_1,string_2,...)
%Purpose: Strings to identify different plots
%Output: flag to denote property set (true or false)
%Input: strings 
%Example: 
% ret = ui_setlegend('y=sinx','y=cosx')
%Note: give the strings according to plot in descending order
%(recent plot first and then onwards...)
%Hence if you have plot first error bar in red, second histogram 
%in green, then execute the legend command as
%ret = ui_setlegend('y=histogram','y=errorbar')
%-----------------------------------------------------

%global structures
[IXG_ST_STDVALUES, IXG_ST_MESSAGE, IXG_ST_INTERFACEVALIDATION] = ixf_global_var('libisis_graphics','get','IXG_ST_STDVALUES','IXG_ST_MESSAGE','IXG_ST_INTERFACEVALIDATION');

iArg = 1;
tot = numel(varargin);
ixf_validate_arg(IXG_ST_INTERFACEVALIDATION.ui_setlegend,tot,'greater');
ret = ixf_gen_interface('iname','setprop_interface','fname','setlegend',varargin{:});
if ( ret == IXG_ST_STDVALUES.true )
    ixf_display_message(IXG_ST_MESSAGE.set);
end

