function varargout=ixf_global_var(varargin)
% ixf_gloabal_var - store and retrieve variables which are hidden from the
% matlab workspace. 
%
% syntax: 
% 
% >> output=ixf_global_var('variable_set', 'set / get / clear','variable',variable_value) 
% >> ixf_global_var('variable_set','copy','new_set1', 'new_set2', ...)
%
% usage:
%
% >> output = ixf_gloabl_var('variable_set','get','var1','var2','var3',...)
% >> ixf_global_var('variable_set','set','variable',value)
% >> ixf_global_var('variable_set','remove')
% >> ixf_global_var('variable_set','clear')
% >> ixf_global_var('variable_set','copy','new_set', ....)
%
%---------
% inputs:   
%---------
%           'variable_set'      The set of properties to take values from      
%
%           'set/get/clear/remove / copy'     
%                            can be either 'set' or 'get' depending if you wish to
%                               set a variable or retrieve it's value.
%           'variable'          the name of the variable you wish to set
%                               (or get).
%           variable_value      the value of the variable you're setting (if set).
%
%           'new_set'           New variable set to copy the data from the
%                               variable set to
%
%---------
% outputs: 
%---------
%
%           either:
%              variable  values     If given 'get' command and a variable name, the requested
%                                   property values will be output
%
%           or
%               variable names      If given 'get' command without a
%                                   variable name a cell array of strings
%                                   will be output containing the stored
%                                   property names
%
%           or
%               varoable set names  If given no arguments, a list of
%                                   available property sets will be output
%
%
% if given the command "clear" all variables contained in the variable set
% given will be removed, but the variable set itself will remain e.g.
%
% >> ixf_global_var('libisis_graphics','clear')
%
% If given 'set' without a value then the variable will be cleared within
% the function, OR if given 'clear' so that
%
% >> ixf_global_var('libisis_graphics','clear','IXG_ST_VARS')
%
% AND
%
% >> ixf_global_var('libisis_graphics','set','IXG_ST_VARS')
%
% Are the same.
%
% 'copy' will copy the variable set into a new variable set named 'new_set'
%
% If the variable set doesn't exist, then the output will be blank
% (when performing remove, get or copy)
%
% when using get or copy, multiple variable names can be used, values will be
% output in the order they are input.
%
% ixf_global_var('variable_set', 'get') displays a list of current stored variables
%       for the given property set
% ixf_global_var() displays a list of property_sets
% ixf_global_var'variable_set') displays a list of stored values also
%
%----------
% examples:
%----------
%
% >> [IXG_ST_DEFAULT, IXG_ST_VALUES] =
% ixf_global_var('graphics','get', 'IXG_ST_DEFAULT',
% 'IXG_ST_VALUES')
%
% Will read the structures IXG_ST_DEFAULT and IXG_ST_VALUES from the
% graphics property set
%
% >> ixf_global_var('graphics','set','IXG_ST_DEFAULT',
% IXG_ST_DEFAULT)
%
% Will write the structure IXG_ST_DEFAULT into the graphics property set.
%
%--------------14/09/2007, Dean Whittaker----------------------------------

mlock; % for stability

persistent main_struct  % initiate a strcuture to store everything
 
% split if statements for readability
if ((nargin > 0)&& ~isfield(main_struct, varargin{1})) % the property set doesn't exist
    if (nargin > 1 && strcmp(varargin{2},'get')) || nargin ==1
 for i = 1:(nargin-2)
        varargout{i} = [];      % this stops the program from trying to access later on.
        end
        return
        % error(['property set ' varargin{1} ' does not exist']) 
    end
end          

if nargin==0    % if no args, list the first level
    varargout{1} = fields(main_struct);
elseif nargin == 1 % if 1 arg, list the second level
    varargout{1} = fields(main_struct.(varargin{1}));
elseif nargin == 2 % only get can be used
    switch varargin{2}
        case 'set'
            error('require a variable to set')
        case 'get'
            varargout{1} = fields(main_struct.(varargin{1}));
        case 'clear'
            if isfield(main_struct,varargin{1})
                field_names = fieldnames(main_struct.(varargin{1}));
                main_struct.(varargin{1}) = rmfield(main_struct.(varargin{1}), field_names);
            else
                disp(['property set ' varargin{1} ' does not exist, no action taken'])
            end
        case 'remove'
            if isfield(main_struct,varargin{1})
                main_struct=rmfield(main_struct,varargin{1});
            else
                disp(['property set ' varargin{1} ' does not exist, no remove performed'])
            end
            
        otherwise
            error('incorrect set/get type')
    end
elseif nargin==3    % 3 args, can get a property or set means remove
    switch varargin{2}
        case {'set' 'clear'}
            try   % Some situations occur when the field doesn't exist - but ways of testing for it are too varied. i.e. different tests throw errors in different situations - isstruct doesn't work with variables, exist doesn't work with structures.
            main_struct.(varargin{1}) = rmfield(main_struct.(varargin{1}),varargin{3});
            end
        case 'get'
            if isfield(main_struct.(varargin{1}),varargin{3})
                varargout{1} = main_struct.(varargin{1}).(varargin{3});
            else
                varargout{1} = [];
            end
        case 'copy'
            main_struct.(varargin{3}) = main_struct.(varargin{1});
        otherwise
            error('incorrect set/get type');
    end
elseif nargin>=4        % more than 3 args means we're getting lots of properties or setting 1
    switch varargin{2}
        case 'set'
            if nargin == 4
                main_struct.(varargin{1}).(varargin{3}) = varargin{4};
            else
                error('only one property can be set at a time')
            end
        case 'get'
            for i = 3:length(varargin)
                if isfield(main_struct.(varargin{1}), varargin{i})
                    varargout{i-2} = main_struct.(varargin{1}).(varargin{i});
                else
                    varargout{i-2} = [];
                end
            end
            if nargout ~= length(varargout)
                warning('more properties requested than output arguments')
            end
        case 'copy'
            for i = 3:length(varargin)
                main_struct.(varargin{i}) = main_struct.(varargin{1});
            end
        otherwise
            error('incorrect set/get type');
    end
elseif main_struct_empty
    error('given property set does not exist, or was not given')
end