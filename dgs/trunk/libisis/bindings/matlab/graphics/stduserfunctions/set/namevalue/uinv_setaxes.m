

function uinv_setaxes(varargin)
%----------------------------------------------------
%Function Syntax: uinv_setaxes('isXY',value,'logap',value,'higap',value)
%Purpose: Shift X or Y axes by gap specified
%Output: None
%Input: isXY for denoting for (0)X or (1)ediY axes, gap to denote
%how much to shift
%Example: 
% uinv_setaxes('isXY',1,'logap',1,'higap',2) --> for y axes ylo = 1 yhi = 2
% uinv_setaxes('isXY',0,'logap',1,'higap',2) --> for x axes xlo = 1 xhi = 2
%-----------------------------------------------------

%global structure

[IXG_ST_INTERFACEVALIDATION IXG_ST_ERROR IXG_ST_STDVALUES] = ixf_global_var('libisis_graphics','get','IXG_ST_INTERFACEVALIDATION','IXG_ST_ERROR','IXG_ST_STDVALUES');

totArg = numel(varargin);
ixf_validate_arg(IXG_ST_INTERFACEVALIDATION.uinv_chngaxis,totArg);
%check values
for iArgLoop = IXG_ST_STDVALUES.start:IXG_ST_STDVALUES.incr:totArg
    switch(lower(varargin{iArgLoop}))
    case 'isxy'
        isXY = varargin{iArgLoop + 1};
        if (~isnumeric(isXY))
            ixf_display_error(IXG_ST_ERROR.invalid_number,'isXY');
        end
        if (isXY ~= IXG_ST_STDVALUES.x && isXY ~= IXG_ST_STDVALUES.y)
            ixf_display_error(IXG_ST_ERROR.wrong_value,'isXY');
        end  
    case 'logap'
        logap = varargin{iArgLoop + 1};
        if (~isnumeric(logap))
            ixf_display_error(IXG_ST_ERROR.invalid_number,'logap');
        end
    case 'higap'
        higap = varargin{iArgLoop + 1};
        if (~isnumeric(higap))
            ixf_display_error(IXG_ST_ERROR.invalid_number,'higap');
        end        
    otherwise
        ixf_display_error(IXG_ST_ERROR.wrong_field);
    end
end

if (higap < logap)
    ixf_display_error(IXG_ST_ERROR.greater_value,'higap','logap');
end

ixf_gen_interface('iname','setprop_interface','fname','setaxes',...
    'isXY',isXY,'logap',logap,'higap',higap);
