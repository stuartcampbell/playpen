

function hdl = uinv_setappname(varargin)
%--------------------------------------------------------------------------
%Function syntax: ret = uinv_setappname('hdl',value,'appname',value,'tag',value)
%Purpose: set application name and tag for the figure handle passed
%Input: figure handle,app name,app tag
%Output: handle is returned if the figure is set else error
%Example: uinv_setappname('hdl',2,'appname','tobie','tag','1d') 
%The above example sets 2 is figure handle, tobie is name and 1d is tag
%--------------------------------------------------------------------------

%global structure

[IXG_ST_INTERFACEVALIDATION IXG_ST_ERROR IXG_ST_STDVALUES IXG_ST_MESSAGE] = ixf_global_var('libisis_graphics','get','IXG_ST_INTERFACEVALIDATION','IXG_ST_ERROR','IXG_ST_STDVALUES','IXG_ST_MESSAGE');

totArg = numel(varargin);
ixf_validate_arg(IXG_ST_INTERFACEVALIDATION.uinv_setappname,totArg);
%check values
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
[hdl] = ixf_gen_interface('iname','setprop_interface','fname','setappname','hdl',hdl,'appname',appname,'tag',tag);
if ( res == IXG_ST_STDVALUES.false )
    ixf_display_error(IXG_ST_ERROR.no_figure);
else
    ixf_display_message(IXG_ST_MESSAGE.set);
end
