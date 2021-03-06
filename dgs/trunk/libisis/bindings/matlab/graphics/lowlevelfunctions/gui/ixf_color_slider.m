function ixf_color_slider(fid,figureHandle_,option,cmd)
% function ixf_color_slider(fid,fig,obj,cmd)
% fig = current figure
% cmd = 'slider' , 'min' , 'max'
% adjust colour table and slider limits

slider_min=findobj(figureHandle_,'Tag','color_slider_min');
slider_min_value=findobj(figureHandle_,'Tag','color_slider_min_value');
slider_max=findobj(figureHandle_,'Tag','color_slider_max');
slider_max_value=findobj(figureHandle_,'Tag','color_slider_max_value');

i_min=get(slider_min,'Value');
i_max=get(slider_max,'value');
switch cmd
    
    case 'create'
        
    case 'slider_max'
  % === slider move, top
        if i_max==i_min % do not change i_max if range becomes 0
             i_min = i_max - 0.01;
        end

    case 'slider_min'
        if i_max == i_min
            i_max = i_min + 0.01;
        end

    case 'min'
   % only change i_min if numeric value entered and would not make range=0
          temp=get(slider_min_value,'String');
          if str2double(temp)==i_max % do not change i_min if range becomes 0
             i_min=get(slider_min,'value');
          else
             i_min=str2double(temp);
          end

    case 'max'
   % only change i_max if numeric value entered and would not make range=0
          temp = get(slider_max_value,'String');      
          if str2double(temp)==i_max % do not change i_min if range becomes 0
             i_max=get(slider_min,'value');
             
          else
             i_max=str2double(temp);
          end   
          
    case 'update'
        
        [figureHandle_, axesHandle_, plotHandle_, otherHandle_] = ixf_get_related_handles(figureHandle_);
        other_tags = get(otherHandle_,'tag');

        if any(strcmp(other_tags,'color_slider_min')) && any(strcmp(other_tags,'color_slider_max')) && ...
            any(strcmp(other_tags,'color_slider_min_value')) && any(strcmp(other_tags,'color_slider_max_value'))
            ixf_add_slider('figure', figureHandle_,'option','create')
            return
        else
            return
        end
    case 'delete'
        ixf_add_slider('figure',figureHandle_,'option','delete');
        return
    otherwise 
   disp('Unknown slider command. Return.');
   return;
end
temp=min(i_min,i_max);
i_max=max(i_min,i_max);
i_min=temp;

if strcmp(get(findobj(figureHandle_,'Tag','Colorbar'),'YScale'),'linear'),

   caxis([i_min i_max]);
   range=abs(i_max-i_min);
   set(slider_min,'Min',i_min-range/2,'Max',i_max-range*0.1,'Value',i_min);
   set(slider_max,'Min',i_min+range*0.1,'Max',i_max+range/2,'Value',i_max);
   h=findobj(figureHandle_,'Tag','Colorbar');
   set(h,'YLim',[i_min i_max]);
   set(get(h,'Children'),'YData',[i_min i_max]);
   i_min_round = truncdig(i_min,3);
   i_max_round = truncdig(i_max,3);
   set(slider_min_value,'String',num2str(i_min_round));
   set(slider_max_value,'String',num2str(i_max_round));
end