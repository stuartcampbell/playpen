function aline(varargin)
%-----------help for gtk aline command - aline ----------------------------
% changes the line properties of the next plot(s)
% syntax:
%
% >> aline('linestyle') OR
% >> aline(width) OR
% >> aline('linestyle',width) OR
% >> aline('name','tag','linestyle',size) OR
% >> aline('style1', 'style2', width1, width2, 'style3',...) OR
% >> aline('name','tag','linestyle1', 'linestyle2', width1, width2, 'linestyle3', ...)
%
% aline will accept a list of styles and widths for line properties. The
% styles and widths can be given in any order and any number can be set. If
% an array is set, then successive overplots will cycle around the
% properties in the order they are given. 
%
% If given a name and tag as the first two arguments, then the properties will 
% be set into the default properties for that name and tag, otherwise the 
% colours will apply to all name tags (i.e. all graphs and all future graphs)
%
% Inputs: 
%       linestyle(s):  strings of different valid matlab line styles 
%       AND/OR
%       linewidth:     a number defining the line width (in points) to set
%       
%       (optional) name: a name to identify a particular plot for the
%                        defaults to be changed
%       (optional) tag:  if given a name, the tag must also be given which
%                        identifies a particular graph.
% output: none
%
% the name and tag may be a cell array of names and tags where
%
% name{i} tag{i} identify each plot for which the colours are to be set.
%
% example:
% 
%       >> aline({'toby', 'dickon'},{'plot',''}, '--',4,3,4,'-.',':')
%
% will set the next line properties for all figures identified by the name tag
% combinations
%
% 'toby', 'plot'        and
% 'dickon', ''
%
% to a size 4 dashed line, then size 3 dot/dash line, then size 4 dotted
% line.
%
%-------------------------------------------------------------------------
% Dean Whittaker 2007

%global

if nargin > 1
    
    nametag_flag = false;
    properties_flag = false;

    % set 2 flags - when nametag_flag is true then the first two arguments are
    % valid name and tag, when properties_flag is true, then the first two
    % arguments are valid properties

    [linetype, linesize, ltypei, lsizei] = ixf_parse_properties(varargin{:});

    %----------set the flags---------------
    if ltypei(1) || ltypei(2) || lsizei(1) || lsizei(2)
        properties_flag = true;
    end
    
    if ixf_checkfornametag(varargin{1}, varargin{2}) 
        nametag_flag = true;
    end
   %--------    catch cases where flags are the same - i.e. no differentation
   %            between name tags and properties 
   
    if nametag_flag && properties_flag % both 1
         choice = menu(['Your first two inputs:  ' varargin{1}, '  ',varargin{2} '  could be interpreted as either a name and tag or as properties - which are they?'],'this is a name tag', 'these are properties', 'these are both');
        switch choice
            case 1
                ltypei(1:2) = false;
                lsizei(1:2) = false;
                % i.e. nametag flag == true, erase the indexes from
                % properties 
                
            case 2
                nametag_flag = false;
                % i.e. properties_flag == true - so keep the indexes 
            case 3
            otherwise
        end
        
    elseif ~nametag_flag && ~properties_flag  % both 0
        choice = menu(['Your first two inputs:  ' varargin{1}, '  ',varargin{2} '  are not known name tags, would you like to create this name tag for future plotting?'],'yes', 'no');
        switch choice
            case 1              % use it as a nametag (it'll be created if given)
                nametag_flag = true;
            case 2             

        end
    
    end
    
%-----------------do the actual calculations ------------------------------
% note that the indexes have been set up above so the properties go in
% correctly. 
    if nametag_flag 
        if any(ltypei)
            ret = uinv_setdefaultprop(varargin{1}, varargin{2},'array', 1, 'linestyle',varargin{logical(ltypei)});
        end
        if any(lsizei)
            ret = uinv_setdefaultprop(varargin{1}, varargin{2},'array', 1, 'linewidth',varargin{logical(lsizei)});
        end
    else
        [name, tag] = ixf_gallnt;
        name = {name{:}, 'IXGDEFAULT'};
        tag = {tag{:}, 'IXGDEFAULT'};
        if any(ltypei)
            ret = uinv_setdefaultprop(name, tag,'array', 1, 'linestyle',varargin{logical(ltypei)});
        end
        if any(lsizei)
            ret = uinv_setdefaultprop(name,tag,'array', 1, 'linewidth',varargin{logical(lsizei)});
        end
    end
    
    %------------------warning check - give warning if not all values are
    % valid ------------------%
    
    if ~all(ltypei | lsizei) && ~nametag_flag
        display(['WARNING in aline: some properties were not valid and have not been used:  ', varargin{~logical(ltypei | lsizei)}])
    elseif ~all(ltypei(3:end) | lsizei(3:end)) && nametag_flag
        display(['WARNING in aline: some properties were not valid and have not been used:  ', varargin{~logical(ltypei | lsizei)}])
    end
else
    
[name, tag] = ixf_gallnt;
    name = {name{:}, 'IXGDEFAULT'};
    tag = {tag{:}, 'IXGDEFAULT'};
    if ischar(varargin{1})
        ret = uinv_setdefaultprop(name, tag,'array', false, 'linestyle',varargin{1});
    elseif isnumeric(varargin{1})
        ret = uinv_setdefaultprop(name, tag,'array', false, 'linewidth',varargin{1});
    end
end