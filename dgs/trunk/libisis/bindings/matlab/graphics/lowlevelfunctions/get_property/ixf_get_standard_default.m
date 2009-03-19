function st_local_default = ixf_get_standard_default(handle_,IXG_ST_DEFAULT, varargin)
%------------------help for gtk ixf_get_standard_default-------------------
%
% purpose: gets the defaults for handle 'handle_' (or if given in the
% optional inputs, the name and tag in the inputs), if there are no
% defaults, creates them for that name tag using IXG_ST_DEFAULT.
%
% syntax: st_local_default = ixf_get_standard_default(handle_, IXG_ST_DEFAULT,
% 'property', 'value')
%
% inputs: 
%       
%       handle_                     handle of the graph
% 
%       IXG_ST_DEFAULT              default to use if no defaults are found
%
%       property value pairs        recognises 'default',
%                                   'name' and 'tag' properties, ignores others. 
%
% example: 
% >> st_local_default = ixf_get_standard_default(1, IXG_ST_DEFAULT,
% 'name','myplot','tag','example') 
% 
% will set the st_local_default to IXG_ST_DEFAULT (if no myplot, example exists)
% with the name 'myplot', and tag 'example'
%
%--------------------------------------------------------------------------
[IXG_ST_ERROR, IXG_ST_STDVALUES] = ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES');

totArg = numel(varargin);

if ixf_check_graphicfigure('handle',handle_)  

    [name tag] = ixf_get_nametag('hdl',handle_);

    %chk default structure
    for iArgLoop = IXG_ST_STDVALUES.start:IXG_ST_STDVALUES.incr:totArg
            switch(lower(varargin{iArgLoop}))
                case 'default'
                    st_local_default = varargin{iArgLoop + 1};  
                case 'name'
                    name = varargin{iArgLoop + 1};  
                case 'tag'
                    tag = varargin{iArgLoop + 1};
            end
    end

st_local_default = ixf_name_tag_properties('get',name, tag);

if  isempty(st_local_default)
    st_local_default = IXG_ST_DEFAULT;
    st_local_default.figure.name = name;    % make sure that the correct name and tag are set into the local default. 
    st_local_default.figure.tag = tag;
    ixf_name_tag_properties('set',name, tag, st_local_default)
end


else

    ixf_display_error(IXG_ST_ERROR.no_figure);

end