
function ixf_keepcurr_cmd(oid,hdl,type)
%--------------------------------------------------------------------------
%Function Syntax: ixf_keepcurr_cmd(figure_oid,figure_hdl)
%Purpose: to store the current figure. it will not be used for new plots
%Input: figure oid and handle
%Output: none
%Example: ixf_keepcurr_cmd('hdl',1)
%--------------------------------------------------------------------------

%global structure
[IXG_ST_ERROR, IXG_ST_STDVALUES]=ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES');

%check my figure
res = ixf_check_graphicfigure('hdl',hdl);
if (res == IXG_ST_STDVALUES.false)
    ixf_display_error(IXG_ST_ERROR.no_figure); 
end

switch type
    case 'keep'
%set values
    ixf_setapplicationdata('hdl',hdl,'keep',IXG_ST_STDVALUES.true);
    ixf_setlabel_hold('hdl',hdl,'flag','true');
    case 'release'
    ixf_setapplicationdata('hdl',hdl,'keep',IXG_ST_STDVALUES.false);
    ixf_setlabel_hold('hdl',hdl,'flag','false');
end