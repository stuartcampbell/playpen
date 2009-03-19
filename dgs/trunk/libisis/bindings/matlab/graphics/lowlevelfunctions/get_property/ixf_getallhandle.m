function ret = ixf_getallhandle(varargin)
%--------------------------------------------------------------------------
%Function Syntax: hdl_structure = ixf_getallhandle('hdl',[figure_handle])
%or ixf_getallhandle
%Purpose: provide figure,axes and line handles in a hierarchy
%Input: either figure handle or none
%Output: structure (containing handle,name and application name)
%Example: hdl_struct = ixf_getallhandle(1)
%The above example gives handle structure for figure no 1
%hdl_struct = ixf_getallhandle
%The above example gives handle structure for all figures
%--------------------------------------------------------------------------

%global structures
[IXG_ST_HDL , IXG_ST_ERROR , IXG_ST_STDVALUES] = ixf_global_var('libisis_graphics','get','IXG_ST_HDL','IXG_ST_ERROR','IXG_ST_STDVALUES');

%structure
onef = IXG_ST_HDL;
%total arguments
totArg = numel(varargin);

allfig = 0; %multi figure (no varargin)
%check
if ( totArg == allfig ) %figure are many
       
    %find figure type only
    fh = findobj(0,'type','figure');
    totfh = numel(fh);
    %create a multi structure
    multif = repmat(onef,totfh,1);
    %pass it
    for n = 1:totfh
        multif(n) = ixf_findall(fh(n));
    end

    %return
    ret= multif;
    
elseif  ( totArg > allfig ) %figure is only one
    
    hdl = varargin{2};
    %find figure type only
    fh = findobj(0,'type','figure');
    ind = find(fh == hdl);
    
    if (isempty(ind))
        ixf_display_error(IXG_ST_STDVALUES.no_figure);
        return;
    end    
    
    %pass it
    onef = ixf_findall(hdl);
    
    %return
    ret = onef;
    
else
  
    ret = IXG_ST_STDVALUES.false;
    ixf_display_error(IXG_ST_ERROR.wrong_arg);
        
end


%--------------------------------------------------------------------------
%subfunction
%--------------------------------------------------------------------------
function lstruct = ixf_findall(fh)
%--------------------------------------------------------------------------
%Function syntax: ret_struct = findall(figure_handle)
%Input: figure handle
%Output:structure
%Purpose:to fill up the structure with figure handle,name,axes and
%plothandle
%--------------------------------------------------------------------------
%tot figures
[IXG_ST_HDL IXG_ST_ERROR IXG_ST_STDVALUES] = ixf_global_var('libisis_graphics','get','IXG_ST_HDL','IXG_ST_ERROR','IXG_ST_STDVALUES');

lstruct = IXG_ST_HDL;
%find axes    from figures
lstruct.fh = fh;
lstruct.name = get(fh,'name');
idvalue = ixf_getfigureidentifier('hdl',fh);
if (strcmp(idvalue,IXG_ST_STDVALUES.identifier))
    lstruct.app = IXG_ST_STDVALUES.identifier;
end
ah = get(lstruct.fh,'children');  
tot = length(ah);
n = 1;
for i = 1:tot
    ahtype = get(ah(i),'type');
    if ( strcmp(ahtype,'axes'))
        lstruct.ah(n) = ah(i);
        n = n + 1;
    end
end
%tot axes
totah = length(lstruct.ah);
%get plots from axes
for i = 1:totah
    lstruct.ah(i);
    ahtype = get(lstruct.ah(i),'type');
    %check for axes only
    if ( strcmp(ahtype,'axes'))
        lstruct.ph = get(lstruct.ah(i),'children');
        %tot plots
        totph = length(lstruct.ph);
        %plots
        for i = 1:totph
            lstruct.ph(i);
        end
    end
end