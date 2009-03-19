function idvalue = ixf_getfigureidentifier(oid,hdl)
%--------------------------------------------------------------------------
%Function Syntax: ixf_getfigureidentifier(figure_id,figure_handle)
%Purpose: for the given figure handle get and identifier
%Input: figure id and hdl
%Output: identifier value
%Example: ixf_getfigureidentifier('hdl',1)
%the above examples identifier value for the passed figure handle
%--------------------------------------------------------------------------

idvalue = getappdata(hdl,'id');
