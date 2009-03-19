function res = ixf_reset_default(varargin)
%-----------------------------------------------------------
%function syntax: res = ixf_reset_default
%purpose: to reset the default structure
%input: none
%output: true or false
%example: res = ixf_reset_default
%the above example will reset the default structure
%(this will be corresponding to current figure)
%-----------------------------------------------------------
[IXG_ST_ERROR, IXG_ST_STDVALUES] =  ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES');

IXG_ST_DEFAULT = ixf_name_tag_properties('get','IXGDEFAULT','IXGDEFAULT');

j = 1;
name = {};
tag =  {};
res = IXG_ST_STDVALUES.false;

switch nargin
    case 0
         flag = ixf_checkinit('Currentfigure');
        if (flag == false)
            res = IXG_ST_STDVALUES.false;
            ixf_display_error(IXG_ST_ERROR.no_figure);
        end
        %get old name and tag
        hdl = gcf;
        [name tag] = ixf_get_nametag('handle',hdl);
    case 1
        if isnumeric(varargin{1}) && ~ any(varargin{1} == 0)
            for i = 1:length(varargin{1})
                flag=ishandle(varargin{1}(i));
                if flag==false
                    ixf_display_error(IXG_ST_ERROR.no_figure);
                end
                graphic_flag = ixf_check_graphicfigure('hdl',varargin{1}(i));
                if graphic_flag
                    [name{j} tag{j}] = ixf_get_nametag('handle',varargin{1}(i));
                    j = j+1;
                end
            end
        elseif any(varargin{1} == 0)
            [name tag] = ixf_gallnt;
            
        else
            ixf_display_error(IXG_ST_ERROR.no_figure)
        end
    case 2
            name = varargin{1};
            tag = varargin{2};
    otherwise
        error('incorrect input arguments');
end

if isempty(name) && isempty(tag)
    return
elseif numel(name) ~= numel(tag)
    error('name and tag lengths are different')
end

    if (~iscell(name) || ~iscell(tag)) && (ischar(name) || ischar(tag))
        name = cellstr(name);
        tag = cellstr(tag);
    elseif ~iscell(name) || ~iscell(tag)
        error('incorrect name tag format')
    end
    
    for i = 1:length(name)
        props = ixf_name_tag_properties('get',name{i}, tag{i});
        IXG_ST_DEFAULT.figure.name = name{i};
        IXG_ST_DEFAULT.figure.tag = tag{i};
        if isstruct(props)
            ixf_name_tag_properties('set',name{i},tag{i},IXG_ST_DEFAULT);
            res = IXG_ST_STDVALUES.true;
        end
    end
    