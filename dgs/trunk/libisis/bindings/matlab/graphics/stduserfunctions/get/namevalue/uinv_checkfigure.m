function hdl = uinv_checkfigure(varargin)
%--------------------------------------------------------------------------
%Function Syntax: figureHandle_ = uinv_checkfigure('hdl',value,
%'appname',value,'tag',value)
%Purpose: To check any figure exists which matches passed property values
%Input: figure handle,name,tag
%Output: return figure handle. if 0 is returned consider it as empty or NO
%figure exists [ 0 stands for NONE ].
%Example: 
% fh = uinv_checkfigure('hdl',1,'appname','tobie','tag','1d')
%the abov examples check whether figure with handle 1, name tobie and tag
%as 1d exists or not
%--------------------------------------------------------------------------

%global structures

[IXG_ST_INTERFACEVALIDATION IXG_ST_ERROR IXG_ST_STDVALUES] = ixf_global_var('libisis_graphics','get','IXG_ST_INTERFACEVALIDATION','IXG_ST_ERROR','IXG_ST_STDVALUES');

%total arguments
totArg = numel(varargin);
ixf_validate_arg(IXG_ST_INTERFACEVALIDATION.uinv_checkfigure,totArg);
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
[hdl] = ixf_gen_interface('iname','getprop_interface','fname','checkfigure','hdl',hdl,'appname',appname,'tag',tag);
if ( hdl == IXG_ST_STDVALUES.false )
    ixf_display_error(IXG_ST_ERROR.no_figfound_at);
end