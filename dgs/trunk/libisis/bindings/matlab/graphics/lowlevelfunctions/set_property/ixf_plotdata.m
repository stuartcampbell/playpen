function varargout = ixf_plotdata(getset, ident, varargin)
%-- libisis graphics ixf_plotdata store data with plot / figure / handle
% function
%
% Full Syntax:
% >> output = ixf_plotdata ('get / set', ident, 'variable_name', variable_value)
%
% Useful Syntaxes:
%
% >> ixf_plotdata('set',ident,'variable_name', variable_value);
%       sets the variable 'variable_name' in the handle 'ident' to
%       variable_value
%
% >> variable_value = ixf_plotdata('get',ident,'variable_name');
%       retrieves the value of 'variable_name' stored in handle ident.
%
% >> [v1, v2, v3, ...] = ixf_plotdata('get',ident,'v1','v2','v3',...);
%       multiple values can be retrieved at the same time.
%
%
% Purpose:
% Used to store data into objects with the handle "ident". This could
% include storing original data into plot handles or count data into figure
% handles.
%
% inputs:
%       'set / get':        May be either 'set' to put data into the handle
%                           or 'get' to retrieve data from the handle
%
%       ident:              May be the handle to any object (typically
%                           plot, figure or axes objects)
%
%       'variable_name'     May be any valid Matlab variable name (for
%                           example 'xdata', 'mycount'. 
%
%       variable_value      Only given if set / get has the value 'set'.
%                           This is the value to be stored under the
%                           variable name. May be any valid matlab value
%
% outputs:
%       ouput:              Only given if set / get has the value 'get',
%                           will be the value which was set into the
%                           variable 'variable_name'
%
% example:
%
% >> ixf_plotdata ('set', 2, 'my_var', [1, 2, 3])
%      This sets 'my_var' stored with figure 2 to [1, 2, 3] and can be
%      retrieved later with the call
%
% >> my_var = ixf_plotdata('get', 2, 'my_var')
%
% if ident is an array, then set will aply to all handles in ident, get
% will return a cell array where output{i} corresponds to ident(i)

% get handles and standard values


% check the figure containing the plot is part of gtk. 

if nargin==0
    for i = 1:length(ident)
        varargout{1}{i} = getappdata(ident(i));
    end
elseif nargin==2
    switch getset
        case 'set'
            error('require a variable to set')
        case 'get'
            if length(ident) > 1
                for i = 1:length(ident)
                    varargout{1}{i} = getappdata(ident(i));
                end
            else
                    varargout{1} = getappdata(ident);
            end
        otherwise
            error('incorrect set/get type')
    end
elseif nargin==3
    switch getset
        case 'set'
                for i = 1:length(ident)
                    rmappdata(ident(i), varargin{1});
                end
        
        case 'get'
            if length(ident) > 1
            for i = 1:length(ident)
                varargout{1}{i} = getappdata(ident(i),varargin{1});
            end
            else
                varargout{1} = getappdata(ident,varargin{1});

            end
            
        otherwise
            error('incorrect set/get type');
    end
elseif nargin>=4
    switch getset
        case 'set'
            if nargin == 4
                for i = 1:length(ident)
                    setappdata(ident(i), varargin{1}, varargin{2});
                end
            else
                error('only one property can be set at a time')
            end
        case 'get'
            if length(ident) > 1
                for j = 1:length(ident)
                    for i = 2:(length(varargin)+1)
                        varargout{i-1}{j} = getappdata(ident(j), varargin{i-1});
                    end
                end
            else
                for i = 2:(length(varargin)+1)
                    varargout{i-1} = getappdata(ident, varargin{i-1});
                end
            end
            
            if nargout ~= length(varargout)
                warning('more variables requested than output arguments')
            end
        otherwise
            error('incorrect set/get type');
    end
end