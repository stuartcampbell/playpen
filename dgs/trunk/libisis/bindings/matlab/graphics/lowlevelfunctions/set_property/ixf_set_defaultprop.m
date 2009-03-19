function ret = ixf_set_defaultprop(nid, namei, tid, tagi, aid, array, varargin)
%--------------------------------------------------------------------------
%Function Syntax: 
% 
% >> ret = ixf_set_defaultprop (nid, namei, tid, tagi, array_id, 0, property1, 
%        value1, property2, value2,...)
% OR
% >> ret = ixf_set_defaultprop (nid, namei, tid, tagi, array_id, 1, property, 
%        value1, value2,value3,...)
% 
% in the first instance, the properties given will be set with the values
% given. In the second instance, a property is given and the values are
% stored as an array into that property.
%
%
%Purpose: to set the default properties of plots,figure or axes
% 
%Inputs:
%       nid:    Will always be 'name'
%       namei   The name of the graph to alter the property for - this
%               can be a single name or cell array of names
%       tid:    Will always be 'tag'
%       tagi    The tag of the graph to alter the property for - this
%               can be a single tag or cell array of tags  
%       aid     Will always be 'array'
%       array   Can be either TRUE or FALSE
%                   TRUE: a property and list of values has been given next
%                   FALSE: property / value pairs have been given next
% 
%       property / value pairs                  OR
%       property and values:   Note that not all properties support
%                              multiple values.
% 
%Output: true or false (success or fail)
%
%Example: ixf_set_defaultprop('name', 'toby', 'tag', 'plot', 'array', false, 
%           'fcolor','black')
%
% Will change the defaults for the name/tag "toby plot" to the figure color
% 'black'
%
% >> ixf_set_defaultprop('name', 'DEFAULT',
% 'tag','','array',true,'color','red', 'green', 'blue')
%
% Will set the default properties to the plot color 'red', 'green' and 'blue' 
%
%--------------------------------------------------------------------------

%global structure
[IXG_ST_STDVALUES]=ixf_global_var('libisis_graphics','get','IXG_ST_STDVALUES');
IXG_ST_DEFAULT = ixf_name_tag_properties('get','IXGDEFAULT','IXGDEFAULT');

%tot args
totArg = numel(varargin);
ret = IXG_ST_STDVALUES.true;

%check the gcf
% flag = ixf_checkinit('Currentfigure');
% switch flag
%    case true

if ~iscell(namei)
    namei = cellstr(namei);
end
if ~ iscell(tagi)
    tagi = cellstr(tagi);
end

if length(namei) ~= length(tagi)
    error('name and tag lengths are different. The defaults can not be set')
end

for i = 1:length(namei)
    
    name = namei{i};        tag = tagi{i};
    %chk if nametag already exists 
    
    chk =  ixf_name_tag_properties('get',name,tag);
    
    if(isstruct(chk))
             %cannot create again
         st_local_default = chk;
    else
             %create it
        st_local_default=IXG_ST_DEFAULT;
        st_local_default.figure.name = name;
        st_local_default.figure.tag = tag;
             %assign some value 
    end
        
    if array
        st_local_default = ixf_set_arrayprop(st_local_default, totArg, varargin{:});
      %---------------reset all counters to 0------------------------------------
        figures = ixf_checkforfigure(name,tag);
        count.(varargin{1}) = [];
        
     
        for j = 1:length(figures)
            if figures(j)
                ixf_plotdata('set',figures(j),'count',count);
            end
        end

    else
       st_local_default = ixf_set_prop(st_local_default,totArg, varargin{:});
    end

    ixf_name_tag_properties('set',name,tag,st_local_default);
end
        %at the end change the default global strucutre
