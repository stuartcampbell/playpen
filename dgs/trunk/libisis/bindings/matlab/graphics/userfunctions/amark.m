function amark(varargin)
%-----------help for gtk aline command - aline ----------------------------
% changes the line properties of the next plot(s)
% syntax:
%
% >> amark('marker') OR
% >> amark(markersize) OR
% >> amark('marker',markersize) OR
% >> amark('name','tag','marker',size) OR
% >> amark('style1', 'style2', markersize1, markersize2, 'style3',...) OR
% >> amark('name','tag','marker1', 'marker2', markersize1, markersize2, 'marker3', ...)
%
% amark will accept a list of styles and markersizes for line properties. The
% styles and markersizes can be given in any order and any number can be set. If
% an array is set, then successive overplots will cycle around the
% properties in the order they are given. 
%
% If given a name and tag as the first two arguments, then the properties will 
% be set into the default properties for that name and tag, otherwise the 
% colours will apply to all name tags (i.e. all graphs and all future graphs)
%
% Inputs: 
%       marker(s):      strings of different valid matlab marker styles 
%       AND/OR
%       markersize:     a number defining the markersize (in points) to set
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
%       >> amark({'toby', 'dickon'},{'plot',''}, 'x',4,3,4,'.','o')
%
% will set the next marker properties for all figures identified by the name tag
% combinations
%
% 'toby', 'plot'        and
% 'dickon', ''
%
% to a size 4 cross marker, then size 3 solid circle marker, then size 4 open 
% circle marker.
%
%-------------------------------------------------------------------------
% Pranav Amin 2005, Dean Whittaker 2008

if nargin > 1    
    nametag_flag = false;
    properties_flag = false;

    % set 2 flags - when nametag_flag is true then the first two arguments are
    % valid name and tag, when properties_flag is true, then the first two
    % arguments are valid properties

    [markertype, markersize, mtypei, msizei] = ixf_parse_properties(varargin{:});

    %----------set the flags---------------
    if mtypei(1) || mtypei(2) || msizei(1) || msizei(2)
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
                mtypei(1:2) = false;
                msizei(1:2) = false;
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
        if any(mtypei)
            ret = uinv_setdefaultprop(varargin{1}, varargin{2},'array', 1, 'marker',varargin{logical(mtypei)});
        end
        if any(msizei)
            ret = uinv_setdefaultprop(varargin{1}, varargin{2},'array', 1, 'markersize',varargin{logical(msizei)});
        end
    else
        [name, tag] = ixf_gallnt;
        name = {name{:}, 'IXGDEFAULT'};
        tag = {tag{:}, 'IXGDEFAULT'};
        if any(mtypei)
            ret = uinv_setdefaultprop(name, tag,'array', 1, 'marker',varargin{logical(mtypei)});
        end
        if any(msizei)
            ret = uinv_setdefaultprop(name,tag,'array', 1, 'markersize',varargin{logical(msizei)});
        end
    end
    
    %------------------warning check - give warning if not all values are
    % valid ------------------%
    
    if ~all(mtypei | msizei) && ~nametag_flag
        display(['WARNING in amark: some properties were not valid and have not been used:  ', varargin{~logical(mtypei | msizei)}])
    elseif ~all(mtypei(3:end) | msizei(3:end)) && nametag_flag
        display(['WARNING in amark: some properties were not valid and have not been used:  ', varargin{~logical(mtypei | msizei)}])
    end

else
    [name, tag] = ixf_gallnt;
    name = {name{:}, 'IXGDEFAULT'};
    tag = {tag{:}, 'IXGDEFAULT'};
    if ischar(varargin{1})
        ret = uinv_setdefaultprop(name, tag,'array', false, 'marker',varargin{1});
    elseif isnumeric(varargin{1})
        ret = uinv_setdefaultprop(name, tag,'array', false, 'markersize',varargin{1});
    end
end