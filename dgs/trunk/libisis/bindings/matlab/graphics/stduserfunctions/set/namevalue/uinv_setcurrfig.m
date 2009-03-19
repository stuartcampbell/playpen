

function res = uinv_setcurrfig(varargin)
%--------------------------------------------------------------------------
%Function syntax: uinv_setcurrfig('hdl',value,'appname',value,'tag',value)
%Purpose: set current figure
%Input: [figure handle] or [figure handle,appname,tag] or [appname,tag]
%Output: handle is returned if the figure is set else error
%Example: uinv_setcurrfig('hdl',2,'appname',appname,'tag',tag) 
%The above example sets figure handle 2 with name tobie and tag 1d as current
%uinv_setcurrfig('hdl',2) 
%The above example sets figure handle 2 as current
%--------------------------------------------------------------------------

%global structures
[IXG_ST_INTERFACEVALIDATION IXG_ST_ERROR IXG_ST_STDVALUES IXG_ST_MESSAGE]  = ixf_global_var('libisis_graphics','get','IXG_ST_INTERFACEVALIDATION','IXG_ST_ERROR','IXG_ST_STDVALUES','IXG_ST_MESSAGE');

totArg = numel(varargin);
if ( totArg == 0)
    ixf_validate_arg(IXG_ST_INTERFACEVALIDATION.uinv_setcurrfig,totArg);
else
    ixf_validate_arg(IXG_ST_INTERFACEVALIDATION.ixf_null_value,rem(totArg,IXG_ST_INTERFACEVALIDATION.uinv_setcurrfig));
end
for iArgLoop = IXG_ST_STDVALUES.start:IXG_ST_STDVALUES.incr:totArg
    switch(lower(varargin{iArgLoop}))
    case 'hdl'
        hdl = varargin{iArgLoop + 1};
        if (~isnumeric(hdl))
            ixf_display_error(IXG_ST_ERROR.invalid_number,'hdl');
        end
    case 'tag'
        tag = varargin{iArgLoop + 1};
        if (~ischar(tag))
            ixf_display_error(IXG_ST_ERROR.invalid_character,'tag');
        end
    case 'appname'
        appname = varargin{iArgLoop + 1};
        if (~ischar(appname))
            ixf_display_error(IXG_ST_ERROR.invalid_character,'appname');
        end
    otherwise
        ixf_display_error(IXG_ST_ERROR.wrong_field);
    end
end
[res] = ixf_gen_interface('iname','setprop_interface','fname','setcurrfig',varargin{:});
if ( res == IXG_ST_STDVALUES.false )
    ixf_display_error(IXG_ST_ERROR.no_figfound_hta);
else
    ixf_display_message(IXG_ST_MESSAGE.set);
end