function res = ixf_check_graphicfigure(oid,hdl)
%--------------------------------------------------------------------------
%Function Syntax: ixf_check_graphicfigure(figure_oid,figure_hdl)
%Purpose: check whether figure created by graphic or by other tools
%Input: figure oid and handle 
%Output: true or false
%Example: ixf_check_graphicfigure('hdl',1)
%the above examples figure with handle 1 is a graphics created or not
%--------------------------------------------------------------------------

%global structure
IXG_ST_STDVALUES=ixf_global_var('libisis_graphics','get','IXG_ST_STDVALUES');


%check identifier
id = ixf_getfigureidentifier('hdl',hdl);
if (isempty(id))
    res = IXG_ST_STDVALUES.false;
elseif (strcmp(id,IXG_ST_STDVALUES.identifier))
    res = IXG_ST_STDVALUES.true;
end

