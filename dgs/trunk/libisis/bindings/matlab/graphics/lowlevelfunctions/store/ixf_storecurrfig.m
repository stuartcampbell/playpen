function res = ixf_storecurrfigure(varargin)
%--------------------------------------------------------------------------
%Function Syntax: ixf_storecurrfigure(figure_oid,figure_hdl)
%Purpose: to store the current figure. it will not be used for new plots
%Input: figure oid and handle
%Output: return true if it is set else false
%Example: res = ixf_storecurrfigure('hdl',1)
%the above examples returns true or false
%--------------------------------------------------------------------------

%global structure
[IXG_ST_ERROR, IXG_ST_STDVALUES]=ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES');

%tot
totArg = numel(varargin);
res = true;
%parse
for iArgLoop = IXG_ST_STDVALUES.start:IXG_ST_STDVALUES.incr:totArg
    switch(lower(varargin{iArgLoop}))
        case 'hdl'
            hdl = varargin{iArgLoop};
            if (~isnumeric(hdl))
                ixf_display_error(IXG_ST_ERROR.invalid_number,'hdl');
            end
        otherwise
            ixf_validate_arg(IXG_ST_ERROR.wrong_arg);
    end
end

lfigureHandle_ = findobj(0,'Type','Figure');
%check range of available figure handles, then set it
index = find(lfigureHandle_ == hdl);

%if yes then make the handle current else return false
if (isempty(index))
    res = IXG_ST_STDVALUES.false;
else
    %modify user data
    setappdata(hdl,'keep',IXG_ST_STDVALUES.true);
    %return true
    res = IXG_ST_STDVALUES.true;
end    


