function ixf_gui_endisable(oid,hdl,field,value)
%--------------------------------------------------------------------------
%Function Syntax: ixf_display_warning(error_no)
%Purpose: to display error
%Input: error no 
%Output: none
%Example: ixf_display_error(IXG_ST_WARNING.no_figure)
%the above examples displays an error message of no figure
%--------------------------------------------------------------------------

%global structure
[IXG_ST_ERROR, IXG_ST_STDVALUES]= ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES');

%parse value 
switch(lower(value))
    
    case 'mn_keep'
        %for keep
        ixf_setapplicationdata('hdl',hdl,'keep',IXG_ST_STDVALUES.true);
        ixf_setlabel_hold('hdl',hdl,'flag','true');
        set(gcbo,'enable','off');
        nextHandle_ = findobj(hdl,'Label',IXG_ST_STDVALUES.curr_label,'Tag',IXG_ST_STDVALUES.curr_tag);
        set(nextHandle_,'enable','on');
    
    case 'mn_curr'
        %for curr
        ixf_setapplicationdata('hdl',hdl,'keep',IXG_ST_STDVALUES.false);
        ixf_setlabel_hold('hdl',hdl,'flag','false');
        set(gcbo,'enable','off');
        nextHandle_ = findobj(hdl,'Label',IXG_ST_STDVALUES.keep_label,'Tag',IXG_ST_STDVALUES.keep_tag);
        set(nextHandle_,'enable','on');

    case 'tb_crosshair'
        %for crosshair
        set(hdl,'state','off');

    case 'tb_rectxy'
        %for rect xy
        set(hdl,'state','off');
        
    otherwise
        ixf_display_error(IXG_ST_ERROR.wrong_field);
end