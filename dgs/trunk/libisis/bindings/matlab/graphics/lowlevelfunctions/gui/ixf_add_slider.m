function ixf_add_slider(fid, figureHandle_,cid,option)
%------ libisis gtk add slider function help ------------------------------
%
% >> ixf_add_slider(fid, figureHandle_)
%
% adds sliders to the figure "figureHandle_" to control the colour scale of
% the plot. uses ixf_colour_slider function as callback.
%
% fid can be any value, usually 'figureHandle_'

% adapted from script by Radu Coldea 02-Oct-1999, by Dean Whittaker
% 2-2-2007
IXG_ST_ERROR =  ixf_global_var('libisis_graphics','get','IXG_ST_ERROR');

flag = ixf_checkinit('Currentfigure');

if ~flag
   ixf_display_error(IXG_ST_ERROR.no_figure);
end
% check for current sliders and boxes 
curr_slider_min = findobj(figureHandle_,'tag','color_slider_min');
curr_slider_max = findobj(figureHandle_,'tag','color_slider_max');
curr_slider_min_val = findobj(figureHandle_,'tag','color_slider_min_value');
curr_slider_max_val = findobj(figureHandle_,'tag','color_slider_max_value');

% delete currrent objects
delete(curr_slider_min, curr_slider_max, curr_slider_min_val, curr_slider_max_val);

switch option
    case 'create'
        i_min = [];
        i_max = [];

        cbarHandle_ = colorbar;

        pos_h = get(cbarHandle_,'position');    % get position of the colorbar

        [figureHandle_, axesHandle_, plotHandle_, otherHandle_] = ixf_get_related_handles(figureHandle_);
                                                % get all plot handles
        for i = 1:length(axesHandle_)
            irange = get(axesHandle_(i),'clim');
            i_min = min([irange(1), i_min]);
            i_max = max([irange(2), i_max]);
        end

        range = i_max-i_min;

        % bottom slider
        hh=uicontrol(figureHandle_,'Style','slider',...
             'Units',get(cbarHandle_,'Units'),'Position',[pos_h(1) (pos_h(2)-pos_h(4)/15) pos_h(3)*1.5 pos_h(4)/20],...
             'Min',i_min-range/2,'Max',i_max-range*0.1,...
             'SliderStep',[0.01/1.4*2 0.10/1.4],'Value',i_min,'Tag','color_slider_min','Callback','ixf_color_slider(''figure'',gcf,''option'',''slider_min'')');  
             % the SliderStep is adjusted such that in real terms it is [0.02 0.10] of the displayed intensity range 

             val = get(hh,'Value');
             val = truncdig(val,3);

        hh_value=uicontrol(figureHandle_,'Style','edit',...
             'Units',get(cbarHandle_,'Units'),'Position',get(hh,'Position')+pos_h(3)*1.5*[1 0 0 0],...
             'String',num2str(val),'Tag','color_slider_min_value','Callback','ixf_color_slider(''figure'',gcf,''option'',''min'')');


        % top slider 
        hh=uicontrol(figureHandle_,'Style','slider',...
             'Units',get(cbarHandle_,'Units'),'Position',[pos_h(1) pos_h(2)+pos_h(4)+pos_h(4)/30 pos_h(3)*1.5 pos_h(4)/20],...
             'Min',i_min+range*0.1,'Max',i_max+range/2,...
             'SliderStep',[0.01/1.4*2 0.10/1.4],'Value',i_max,'Tag','color_slider_max','Callback','ixf_color_slider(''figure'',gcf,''option'',''slider_max'')');

             val = get(hh,'Value');
             val = truncdig(val,3);

         hh_value=uicontrol(figureHandle_,'Style','edit',...
             'Units',get(cbarHandle_,'Units'),'Position',get(hh,'Position')+pos_h(3)*1.5*[1 0 0 0],...
             'String',num2str(val),'Tag','color_slider_max_value','Callback','ixf_color_slider(''figure'',gcf,''option'',''max'')');

    case 'delete'
    otherwise
        error('incorrect option given')
end