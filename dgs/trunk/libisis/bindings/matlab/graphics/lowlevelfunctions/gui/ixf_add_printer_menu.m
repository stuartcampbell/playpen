function [outHandle_] = ixf_add_printer_menu(inHandle_)
%- help for libisis graphics ixf_add_printer_menu add printer menu function
%
% adds a ui menu to the current plot with printer names. These names are
% contained in the IXG_ST_PRINTERS property set in ixf_global_default
%
% >> outHandle = ixf_add_printer_menu(inHandle)
%
% inputs:
%   inHandle -      any handle type, the associated figure will have the menu
%                   added
%
% outputs:
%   outHandle -     Handle of the menu on the figure
%

if ~exist('inHandle_','var')
    inHandle_ = gcf;
end

[figureHandle_, axesHandle_, plotHandle_, otherHandle_] = ixf_get_related_handles(inHandle_);
[IXG_ST_PRINTERS, IXG_ST_ERROR] =ixf_global_var('libisis_graphics','get','IXG_ST_PRINTERS','IXG_ST_ERROR');
avail_fields = fieldnames(IXG_ST_PRINTERS);

for i = 1:length(figureHandle_)
    if ~ ixf_check_graphicfigure('handle',figureHandle_(i))
        ixf_display_error(IXG_ST_ERROR.no_figure);
    end

    h = findobj(figureHandle_(i),'tag','Color_Print');
    
    if isempty(h)
        Handle_=uimenu(figureHandle_(i),'Label','ColourPrint','Tag','Color_Print','Enable','on');

        for j = 1:length(avail_fields)
            uimenu(Handle_,'Label',avail_fields{j},'Callback',['ixf_printc(''' IXG_ST_PRINTERS.(avail_fields{j}) ''')']);
        end
    else
        warning('printer menu already exists, no action taken');
    end
end

if nargout > 0
    outHandle_ = Handle_;
end
