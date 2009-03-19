function ixf_set(figureHandle, axesHandle, plotHandle, otherHandle, type, varargin)
%----- help for gtk ixf_set------------------------------------------------
%
% syntax: ixf_set(figureHandle, axesHandle, plotHandle, type, 'property', ' value'...)
%
% purpose: sets properties of the plot, axes or graph according to property
% value pairs.
%
% inputs: figure, axes and plot Handles, type = type of object to change
% (either plot, axes or figure), property value pairs
%
% output: none
%
%--------------------------------------------------------------------------
[IXG_ST_STDVALUES, IXG_ST_INTERFACEVALIDATION] = ixf_global_var('libisis_graphics','get','IXG_ST_STDVALUES','IXG_ST_INTERFACEVALIDATION');

totArg = numel(varargin);
ixf_validate_arg(IXG_ST_INTERFACEVALIDATION.ixf_null_value,rem(totArg,2));

for iArgLoop = IXG_ST_STDVALUES.start:IXG_ST_STDVALUES.incr:totArg
    
    switch type
        case 'plot'
            for j = 1:length(plotHandle)
                set(plotHandle(j),varargin{iArgLoop},varargin{iArgLoop + 1});
            end
        case 'axes'
            for j = 1:length(axesHandle)
                set(axesHandle(j),varargin{iArgLoop},varargin{iArgLoop + 1});
            end
        case 'figure'
            for j = 1:length(figureHandle)
                set(figureHandle(j),varargin{iArgLoop},varargin{iArgLoop + 1});
            end
        otherwise
            error('unrecognised type of object to alter')
    end
    
end