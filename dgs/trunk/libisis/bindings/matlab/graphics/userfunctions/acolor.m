function acolor(varargin)
%----------help for gtk acolor command acolor------------------------------
% changes the colour of the next plot(s)
% syntax:
%
% >> acolor('color') OR
% >> acolor('name','tag','color') OR
% >> acolor('color1', 'color2', 'color3', ...) OR
% >> acolor('name','tag','color1','color2','color3',...)
%
% If a single colour is chosen, then all future plots will be in that
% colour. If more than one colour is entered then plots in a single figure
% will cycle around the colours in the order they are given.
%
% If given a name and tag, then the colour(s) will be set into the default
% properties for that name and tag, otherwise the colours will apply to all
% name tags (i.e. all graphs and all future graphs)
%
% Inputs: 
%       color:  list of colors to set into the default properties
%       (optional) name: a name to identify a particular plot 
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
% >> acolor({'toby', 'dickon'},{'plot',''}, 'blue','green','red')
%
% will set the next colours for all figures identified by the name tag
% combinations
%
% 'toby', 'plot'        and
% 'dickon', ''
%
% to blue, then green, then red. 
%-------------------------------------------------------------------------
% Dean Whittaker 2007

%global
if nargin == 0
    error('Not enough input arguments to acolor')
end

if nargin > 1
    
    nametag_flag = false;
    properties_flag = false;

    % set 2 flags - when nametag_flag is true then the first two arguments are
    % valid name and tag, when properties_flag is true, then the first two
    % arguments are valid properties

    [colortype, colorsize, ctypei, csizei] = ixf_parse_properties(varargin{:});

    %----------set the flags---------------
    if ctypei(1) || ctypei(2) || csizei(1) || csizei(2)
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
                ctypei(1:2) = false;
                csizei(1:2) = false;
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
        if any(ctypei)
            ret = uinv_setdefaultprop(varargin{1}, varargin{2},'array', 1, 'color',varargin{logical(ctypei)});
        end
        if any(csizei)
            ret = uinv_setdefaultprop(varargin{1}, varargin{2},'array', 1, 'color',varargin{logical(csizei)});
        end
    else
        [name, tag] = ixf_gallnt;
        name = {name{:}, 'IXGDEFAULT'};
        tag = {tag{:}, 'IXGDEFAULT'};
        if any(ctypei)
            ret = uinv_setdefaultprop(name, tag,'array', 1, 'color',varargin{logical(ctypei)});
        end
        if any(csizei)
            ret = uinv_setdefaultprop(name,tag,'array', 1, 'color',varargin{logical(csizei)});
        end
    end
    
    %------------------warning check - give warning if not all values are
    % valid ------------------%
    
    if ~all(ctypei | csizei) && ~nametag_flag
        display(['WARNING in acolor: some properties were not valid and have not been used:  ', varargin{~logical(ctypei | csizei)}])
    elseif ~all(ctypei(3:end) | csizei(3:end)) && nametag_flag
        display(['WARNING in acolor: some properties were not valid and have not been used:  ', varargin{~logical(ctypei | csizei)}])
    end
else
    [name, tag] = ixf_gallnt;
    name = {name{:}, 'IXGDEFAULT'};
    tag = {tag{:}, 'IXGDEFAULT'};
     ret = uinv_setdefaultprop(name, tag,'array', false, 'color',varargin{1});
end