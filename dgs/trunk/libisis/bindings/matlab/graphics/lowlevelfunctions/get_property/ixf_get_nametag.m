function [name, tag] = ixf_get_nametag(oid,hdl)
% ixf_get_nametag - gets the name and tag of a given handle
% use this instead of directly getting the name and tag, as it removes any
% formatting (like (HOLD)) that have been added to the name or tag
%
% [name, tag] = ixf_get_nametag(oid, hdl)
%
% inputs:
% oid = the identifier for handle - usually use 'hdl'. This input does not
% effect the output of the function, it makes it easier to impliment into
% GTK
% hdl = handle of the plot for which the name tag is wanted.
%
% example:
%
% >> [name, tag] = ixf_get_nametag('handle',2)
%
% this will get the name and tag of figure 2
%
% written by Dean Whittaker 21/12/2006

IXG_ST_STDVALUES =  ixf_global_var('libisis_graphics','get','IXG_ST_STDVALUES');

name = get(hdl,'name');
tag = get(hdl,'tag');

if ~ (ixf_check_graphicfigure('figure_handle',hdl)==IXG_ST_STDVALUES.true);
    display('WARNING: names and tags of plots which are not part of the libisis graphics package have been given')
end

    keep_flag = getappdata(hdl,'keep');

    if keep_flag
        index_ = findstr(name, IXG_ST_STDVALUES.hold);
        length_str = length(IXG_ST_STDVALUES.hold);
        name(index_:(index_ + length_str-1)) = [];
    end