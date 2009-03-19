
function [res] = ixf_setappname(figoid,hdl,nameoid,appname, tagoid , tag)
%--------------------------------------------------------------------
%Function Syntax: figureHandle_ = ixf_setappname(figure oid,figure hdl,
%name oid,value, tag oid, value)
%Purpose: set name and tag for the figure handle passed
%Output: return figure handle. if 0 is returned consider it as empty or NO
%figure exists [ 0 stands for NONE].
%Input: figure oid, value,app oid, value,tag oid, value
%Example: 
%ret = ixf_setappname('hdl',1,'appname','tobie','tag','1d')
%ret will be either figure handle (if success) else false (failure)
%--------------------------------------------------------------------

%global structures
[IXG_ST_ERROR, IXG_ST_STDVALUES]= ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES');

% ixf_validate_arg(IXG_ST_INTERFACEVALIDATION.ixf_setappname,nargin);

flag = ixf_checkinit('Currentfigure');
if (flag == IXG_ST_STDVALUES.false)
    ixf_display_error(IXG_ST_ERROR.no_figure);
end

one = 1;


%check values
% for iArgLoop = IXG_ST_STDVALUES.start:IXG_ST_STDVALUES.incr:totArg
%     switch(lower(varargin{iArgLoop}))
%     case 'hdl'
%         hdl = varargin{iArgLoop + 1};
%         if (~isnumeric(hdl))
%             ixf_display_error(IXG_ST_ERROR.invalid_number,'hdl');
%         end
%     case 'tag'
%         tag = varargin{iArgLoop + 1};
%         if (~ischar(tag))
%             ixf_display_error(IXG_ST_ERROR.invalid_character,'tag');
%         end
%     case 'appname'
%         appname = varargin{iArgLoop + 1};
%         if (~ischar(appname))
%             ixf_display_error(IXG_ST_ERROR.invalid_character,'appname');
%         end
%     otherwise
%         ixf_display_error(IXG_ST_ERROR.wrong_arg);
%     end
% end

%for one
lfigureHandle_ = findobj(0,'Type','Figure');
if (isempty(lfigureHandle_))
    res = IXG_ST_STDVALUES.false;
    return;
end
index = find(lfigureHandle_ == hdl);
%if yes then make the handle current else return false
if (isempty(index))
    res = IXG_ST_STDVALUES.false;
else
    %recreation of structure
    %this is done to use less memory 
    %it deletes the old structure and creates new one by the name 
    %specified.
    
    %get old name and tag
[oldname oldtag] = ixf_get_nametag('handle',hdl);
    %chk structure is empty or not
    chk = ixf_name_tag_properties('get',oldname, oldtag);
    if(isstruct(chk))
        %cannot create again
        set(hdl,'Name',appname);
        set(hdl,'tag',tag);
        %new nametag
         %create it
        %prepare string
        ixf_name_tag_properties('set',appname,tag,chk);
        ixf_name_tag_properties('clear',oldname, oldtag);
        
    else
        %create new one
        %set new name and tag
        set(hdl,'Name',appname);
        set(hdl,'tag',tag);
        %new nametag
        IXG_ST_DEFAULT = ixf_name_tag_properties('get','IXGDEFAULT','IXGDEFAULT');
        IXG_ST_DEFAULT.figure.name = appname;
        IXG_ST_DEFAULT.figure.tag = tag;
        
        ixf_name_tag_properties('set',appname,tag,IXG_ST_DEFAULT);
        
    end
    res = IXG_ST_STDVALUES.true;
end
