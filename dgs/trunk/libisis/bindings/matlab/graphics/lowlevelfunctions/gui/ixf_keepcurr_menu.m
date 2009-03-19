
function ixf_keepcurr_menu(oid,hdl)
%-------------------------------------------------------------------- 
%Function Syntax: ixf_keepcurr_menu(figure_oid,figure_handle)
%Purpose: create menu items (keep and current figure)
%Input: figure oid and figure handle
%Output: None
%Example: ixf_keepcurr_menu('hdl',1)
%This figure creates menu items on figure handle 1
%--------------------------------------------------------------------

%global structures
IXG_ST_STDVALUES= ixf_global_var('libisis_graphics','get','IXG_ST_STDVALUES');

%check for current figure
%Add the menu on 6th position
position = IXG_ST_STDVALUES.menu_position+1;
mn_figure_= uimenu(gcf,'Label','ChngFigure','Tag','ChngFigureMenu','Position',position);
%default set
ixf_setapplicationdata('hdl',hdl,'keep',IXG_ST_STDVALUES.false);
%when button is pressed
mn_keep_ = uimenu(mn_figure_,'Label',IXG_ST_STDVALUES.keep_label,'Tag',IXG_ST_STDVALUES.keep_tag,'Callback','ixf_gui_endisable(''hdl'',gcbf,''field'',''mn_keep'')');    
mn_curr_ = uimenu(mn_figure_,'Label',IXG_ST_STDVALUES.curr_label,'Tag',IXG_ST_STDVALUES.curr_tag,'enable','off','Callback','ixf_gui_endisable(''hdl'',gcbf,''field'',''mn_curr'')');