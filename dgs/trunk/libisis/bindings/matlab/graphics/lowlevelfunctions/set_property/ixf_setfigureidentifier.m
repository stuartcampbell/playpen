function res = ixf_setfigureidentifier(oid,hdl)
%--------------------------------------------------------------------------
%Function Syntax: ixf_setfigureidentifier(figure_id,figure_handle)
%Purpose: for the given figure handle set identifier
%Input: figure id and hdl
%Output: identifier value
%Example: ixf_setfigureidentifier('hdl',1)
%the above examples sets identifier value for the passed figure handle
%--------------------------------------------------------------------------

%global structure
IXG_ST_STDVALUES= ixf_global_var('libisis_graphics','get','IXG_ST_STDVALUES');
res = false;
setappdata(hdl,'id',IXG_ST_STDVALUES.identifier);
res = true;

