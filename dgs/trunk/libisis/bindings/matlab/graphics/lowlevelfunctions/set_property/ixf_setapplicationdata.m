function ixf_setapplicationdata(oid,hdl,varargin)
%--------------------------------------------------------------------------
% Function Syntax: ixf_setapplicationdata(oid,hdl,property,value)
% Purpose: set the application property data for the figure handle passed
% Input: figure oid and handle, property and value
% Output: none
% Example: ixf_setapplicationdata('hdl',1,'keep','true')
% the above examples sets application data property for the figure handle
% passed
%--------------------------------------------------------------------------

%global structure
[IXG_ST_ERROR, IXG_ST_STDVALUES]=ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES');

%tot args
totArg = numel(varargin);

%parse
for iArgLoop = IXG_ST_STDVALUES.start:IXG_ST_STDVALUES.incr:totArg
    switch(lower(varargin{iArgLoop}))
        case 'keep'
            setappdata(hdl,'keep',varargin{iArgLoop+1});
        otherwise
            ixf_display_error(IXG_ST_ERROR.wrong_field);
    end
end