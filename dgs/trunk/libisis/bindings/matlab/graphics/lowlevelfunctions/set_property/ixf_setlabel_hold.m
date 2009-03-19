function ixf_setlabel_hold(oid,hdl,flag,value)
%--------------------------------------------------------------------------
%Function Syntax: ixf_setlabel_hold(figure_oid,figure_hdl,label_flag,value)
%Purpose: to set label hold on the keep figure
%Input: figure oid, figure handle, label flag, value
%Output: none
%Example: ixf_setlabel_hold('hdl',1,'label','true')
%the above examples displays an hold tag on the figure handle passed
%ixf_setlabel_hold('hdl',2,'label','false')
%the above examples removes hold tag on the figure handle passed
%--------------------------------------------------------------------------

%global structure
IXG_ST_STDVALUES= ixf_global_var('libisis_graphics','get','IXG_ST_STDVALUES');

switch(lower(value))
    case 'true'
        %name
        name = get(hdl,'name');
        ind = findstr(name,IXG_ST_STDVALUES.hold);
        length_str=length(IXG_ST_STDVALUES.hold);
        
        if (~isempty(ind))
            try
                set(hdl,'name',[name(1:ind-1) name((ind+length_str):end)]);        
            catch                         % case string isn't long enough.
                set(hdl,'name',name(1:ind-1));
            end
        end
        %name
        name = get(hdl,'name');
        nametag = [name IXG_ST_STDVALUES.hold];
        set(hdl,'name',nametag);        
    case 'false'
        name = get(hdl,'name');
        ind = findstr(name,IXG_ST_STDVALUES.hold);
        length_str=length(IXG_ST_STDVALUES.hold);
        if (~isempty(ind))
            try
                set(hdl,'name',[name(1:ind-1) name((ind+length_str):end)]);        
            catch                         % case string isn't long enough.
                set(hdl,'name',name(1:ind-1));
            end       
        end
end
