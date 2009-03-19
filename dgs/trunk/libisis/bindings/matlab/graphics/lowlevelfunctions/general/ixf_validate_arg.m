function ixf_validate_arg(varargin)
%--------------------------------------------------------------------------
%Function syntax: ixf_validate_arg(no_of_args_passed,no_of_args_required)
%Purpose: to validate function arguments
%Input: arguments 
%Output: none
%Example: ixf_validate_arg(2,2) 
%The above example checks if passed and required no of arguments 
%are same or not
%ixf_validate_arg(2,3,'greater')
%The above example checks if passed is greater than required no of arguments 
%--------------------------------------------------------------------------

%global structure
[IXG_ST_ERROR, IXG_ST_STDVALUES]=ixf_global_var('libisis_graphics','get','IXG_ST_ERROR', 'IXG_ST_STDVALUES');

iArg = 1;
tot = numel(varargin);
flag = IXG_ST_STDVALUES.true;
%default check for equality
if (tot == 2)
    %passed and required must be equal
    if  ( (nargin < 2) || ( varargin{iArg}  ~= varargin{iArg+1} ) )
        flag = IXG_ST_STDVALUES.false;
    end
%check for other cases than equality    
elseif (tot == 3)
    switch(varargin{iArg + 2})
        case 'greater'
            %passed cannot be less than equal to required
            if  ( varargin{iArg}  > varargin{iArg+1} ) 
                flag = IXG_ST_STDVALUES.false;
            end
        case 'range'
            %passed must be in range of required
            ind = find(varargin{iArg} == varargin{iArg+1});
            if (isempty(ind))
                flag = IXG_ST_STDVALUES.false;                
            end
        case 'less'
            %passed must not be greater than equal to required
            if  ( varargin{iArg}  < varargin{iArg+1} ) 
                flag = IXG_ST_STDVALUES.false;
            end
        otherwise
            flag = IXG_ST_STDVALUES.false;                
    end
end
%check flag
if (flag == IXG_ST_STDVALUES.false)
    ixf_display_error(IXG_ST_ERROR.wrong_arg);            
end