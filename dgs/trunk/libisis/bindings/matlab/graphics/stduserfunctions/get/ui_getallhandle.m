

function hdl = ui_getallhandle(varargin)
%--------------------------------------------------------------------------
%Function Syntax: hdl_structure = ui_getallhandle([figure_handle])
%Purpose: provide figure,axes and line handles in a hierarchy
%Input: either figure handle or none
%Output: structure (containing handle,name and application name)
%return structure is in this format
%struct('fh',[],'ah',[],'ph',[])
%Example: hdl_struct = ui_getallhandle(1)
%The above example gives handle structure for figure no 1
%hdl_struct = ui_getallhandle
%The above example gives handle structure for all figures
%--------------------------------------------------------------------------

%global structures

[IXG_ST_ERROR, IXG_ST_STDVALUES]=ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES');

tot = numel(varargin);
if (tot == 0)
    [hdl] = ixf_gen_interface('iname','getprop_interface','fname','getallhandle');
else
    [hdl] = ixf_gen_interface('iname','getprop_interface','fname','getallhandle','hdl',varargin{:});
end
if ( ~isstruct(hdl) )
    ixf_display_error(IXG_ST_ERROR.no_figfound_at);
end
tot = numel(hdl);
n = tot;
display(['total number of figures : ' int2str(tot)]); 
for i = 1:tot
    %     ['------' int2str(n) '----------']
    %     display(['handle: ' int2str(hdl(i).fh) ' | name: ' hdl(i).name ' | axes handle: '  ' |  ...
    %             ' | application: ' hdl(i).app]);
    display('--------------');
    f = sprintf('figure handle: %s | name:  %s  | app: %s ',int2str(hdl(i).fh),hdl(i).name,hdl(i).app);
    a = hdl(i).ah;
    display(['axis handle (a): ',num2str(a)])
    p = hdl(i).ph; 
    display(['plot handle (p): ',num2str(p)])
end