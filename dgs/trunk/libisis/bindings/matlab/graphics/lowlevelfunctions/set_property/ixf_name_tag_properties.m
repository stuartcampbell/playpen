function varargout = ixf_name_tag_properties(varargin)
% function to get the properties or set the properties for name tags. This
% is a layer on top of the ixf_global_var mfile
%
% >> ixf_name_tag_properties('set', name, tag, data)
% >> ixf_name_tag_properties('set',data)
% >> props = ixf_name_tag_properties('get',name,tag)
% >> props = ixf_name_tag_properties('get')
% >> props = ixf_name_tag_properties('clear',name,tag)
% >> props = ixf_name_tag_properties('clear')
% 
% inputs:
%       get/set/clear           if get, then the properties corresponding
%                               to the passed name and tag will be output.
%                               If set, then the data will be set for that
%                               name-tag, if clear then the data will be
%                               cleared for the given name-tag.
%
%       name                    Name of plot to store properties for
%
%       tag                     Tag of plot to store properties for
%
%       data                    data to store with name-tags given
%
% The data stored in name tags must be structures with the fields
%
% data.figure.name  -       Figure name as a string
% data.figure.tag   -       Figure tag as a string
%
% If no name and tag are given, then actions are performed on all name tags
% used by the graphics package.
%
% outputs:
%       props                   If using get, then the properties for the
%                               given name-tag are returned. This is always 
%                               as a cell array.
%
 


if nargin >=3
    if ~ ischar(varargin{2}) || ~ischar(varargin{3})
        error('Argument 2 and 3 must be the name and tag repsectively')
    end
    i = 1;                          % i gives us an index
    j = true;                       % j tells us when to stop
    
    valid_name = ixf_remove_spaces(varargin{2});
    valid_tag = ixf_remove_spaces(varargin{3});
    varstart = ['IXG_ST_' valid_name valid_tag];
   
    while j
        varname = [varstart '_' num2str(i)];
        props = ixf_global_var('IXT_GRAPHICS_NAMETAGS','get',varname);
        if ~isempty(props) % the nametag has been set already
                    if ~ isstruct(props) || ~isfield(props,'figure') || ...
                            ~isfield(props.figure,'name') || ~isfield(props.figure,'tag') 
                        error('data stored for this name tag is not in the valid structure format')
                    end
            if strcmp(props.figure.name, varargin{2}) && strcmp(props.figure.tag, varargin{3})
                j = false;      % if they're correct, we've got our match
            else
                i = i + 1;      % otherwise look at the next posibility 
            end
        else
            j = false;          % if the props don't exist then we've scanned 
                                % through all posibilities. It isn't there.
        end
    end 
else
    varname = ixf_global_var('IXT_GRAPHICS_NAMETAGS','get');
end
% we now have a list of variable names coresponding to input name tags
if ~ isempty(varname)
    varname = cellstr(varname);     % needs to be a cell for future.
else
    varargout = [];
    return
end

 switch varargin{1}
            case 'set'
                if nargin == 1 || nargin == 3
                    error('No data given to set')
                else
                    if ~ isstruct(varargin{nargin}) || ~isfield(varargin{nargin},'figure') || ...
                            ~isfield(varargin{nargin}.figure,'name') || ~isfield(varargin{nargin}.figure,'tag') 
                        error('properties that have been passed are not in a valid structure to be set for name tags')
                    end

                    for i = 1:length(varname)
                        ixf_global_var('IXT_GRAPHICS_NAMETAGS','set', varname{i}, varargin{nargin});
                    end
                end
            case 'get'
                if nargin == 2 || nargin == 4
                    error('incorrect number of arguments for ''get'', insure you pass both the name and tag')
                end
                for i = 1:length(varname)
                    varargout{1}{i} = ixf_global_var('IXT_GRAPHICS_NAMETAGS','get',varname{i}); 
                end
                if numel(varargout{1}) == 1 && nargin > 2
                    varargout{1} = varargout{1}{1};
                end
            case 'clear'
                if nargin == 2 || nargin == 4
                    error('incorrect number of arguments for ''clear'', insure you pass both the name and tag')
                end
                
                for i = 1:length(varname)
                    ixf_global_var('IXT_GRAPHICS_NAMETAGS','clear',varname{i});
                end
        otherwise
            error('incorrect set/get/clear type')
end