

function hdl = uinv_gethandle(varargin)
%--------------------------------------------------------------------------
%Function Syntax: figureHandle_ = uinv_gethandle('appname',value,'tag',
%value)
%Purpose: To return handle if any figure matches passed property values
%Output: return figure handle. if 0 is returned consider it as empty or NO
%figure exists [ 0 stands for NONE].
%Input: application name, application tag
%Example:  fh = uinv_gethandle('appname','tobie','tag','1d')
%the above example returns figure handle if any figure matches name as
%tobie and tag as 1d
%--------------------------------------------------------------------------

%global structure

[IXG_ST_INTERFACEVALIDATION IXG_ST_ERROR IXG_ST_STDVALUES] = ixf_global_var('libisis_graphics','get','IXG_ST_INTERFACEVALIDATION','IXG_ST_ERROR','IXG_ST_STDVALUES');

totArg = numel(varargin);
ixf_validate_arg(IXG_ST_INTERFACEVALIDATION.uinv_gethandle,totArg);
%check values
for iArgLoop = IXG_ST_STDVALUES.start:IXG_ST_STDVALUES.incr:totArg
    switch(lower(varargin{iArgLoop}))
    case 'appname'
        appname = varargin{iArgLoop + 1};
        if (~ischar(appname))
            ixf_display_error(IXG_ST_ERROR.invalid_character,'appname');
        end
    case 'tag'
        tag = varargin{iArgLoop + 1};
        if (~ischar(tag))
            ixf_display_error(IXG_ST_ERROR.invalid_character,'tag');
        end
    otherwise
        ixf_display_error(IXG_ST_ERROR.wrong_field);
    end
end
[hdl] = ixf_gen_interface('iname','getprop_interface','fname','gethandle','appname',appname,'tag',tag);
if ( hdl == IXG_ST_STDVALUES.false )
    ixf_display_error(IXG_ST_ERROR.no_figfound_at);
end
