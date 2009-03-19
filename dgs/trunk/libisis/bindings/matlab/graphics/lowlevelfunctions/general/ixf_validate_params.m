function flag = ixf_validate_params(varargin)
%---------------------------------------------------------------
%purpose: to validate parameters
%input: arguments to be validated
%first will be style (which is for marker, line or color,etc)
%second will be the value
%
%output : flag true or false. if the validation is false return
%false else true
%
%example: ixf_validate_params('m','o') 
%the above example validates for marker passing 'o' as the symbol
%---------------------------------------------------------------


%global structures

[IXG_LINE_VALIDVALUES , IXG_MARKER_VALIDVALUES , IXG_COLOR_VALIDVALUES , IXG_ST_STDVALUES] = ixf_global_var('libisis_graphics','get','IXG_LINE_VALIDVALUES','IXG_MARKER_VALIDVALUES','IXG_COLOR_VALIDVALUES','IXG_ST_STDVALUES');


%assign values
style = varargin{1};
value = varargin{2};
flag = IXG_ST_STDVALUES.true;

%validate style
%check in the globals
switch(style)
    case 'm'        
        ind = strmatch(value,IXG_MARKER_VALIDVALUES);
    case 'l'
        ind = strmatch(value,IXG_LINE_VALIDVALUES);
    case 'c'
        ind = strmatch(value,IXG_COLOR_VALIDVALUES);
end

%check if empty
if (isempty(ind))
    flag = IXG_ST_STDVALUES.false;
end


       