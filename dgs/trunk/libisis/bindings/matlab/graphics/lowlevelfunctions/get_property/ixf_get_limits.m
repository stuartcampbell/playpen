
function [temp1, temp2] = ixf_get_limits(arg,hdl,type)
%------------------------------------------------
% Function syntax: [low, high] = ixf_get_limits('hdl',gca_handle,type)
% Purpose to get min,max limits for x, y and z
% Input: arg ('hdl'), gca hdl and type (x or y or z)
% Output: min, max values
% Example: [temp1,temp2] = ixf_get_limits('hdl',gca,'x')
%------------------------------------------------

[figureHandle_, axesHandle_, plotHandle_, otherHandle_] = ixf_get_related_handles(hdl);

temp1 = [];
temp2 = [];
for i = 1:numel(plotHandle_)
    child_type = get(plotHandle_(i),'type');
    if (strcmp(child_type,'line'))
        switch(type)
            case 'x'
                xyzdata=ixf_plotdata('get',plotHandle_(i),'xyzdata');
                temp_min = min(xyzdata.x);
                temp_max =max(xyzdata.x);
                temp1 = min([temp1, temp_min]);
                temp2 = max([temp2, temp_max]);
            case 'y'
                y = get(plotHandle_(i),'YData');
                temp_min =min(y);
                temp_max =max(y);
                temp1 = min([temp1, temp_min]);
                temp2 = max([temp2, temp_max]);
            case 'z'
                z = get(plotHandle_(i),'ZData');
                temp1 = min(z);
                temp2 = max(z);
        end   
    elseif strcmp(child_type,'surface')
        switch(type)
            case 'x'
                xyzdata=ixf_plotdata('get',plotHandle_(i),'xyzdata');
                temp_min=min(xyzdata.x);
                temp_min=min(temp_min);
                temp_max=max(xyzdata.x);
                temp_max=max(temp_max);
                temp1 = min([temp1, temp_min]);
                temp2 = max([temp2, temp_max]);
            case 'y'
                xyzdata=ixf_plotdata('get',plotHandle_(i),'xyzdata');
                temp_min = min(xyzdata.y);
                temp_max = max(xyzdata.y);
                temp_min = min(temp_min);
                temp_max = max(temp_max);
                temp1 = min([temp1, temp_min]);
                temp2 = max([temp2, temp_max]);
            case 'z'
                z=get(plotHandle_(i),'ZData');
                temp_min=min(z);
                temp_min=min(temp_min);
                temp_max=max(z);
                temp_max=max(temp_max);
                temp1 = min([temp1, temp_min]);
                temp2 = max([temp2, temp_max]);
            case 'c'
                c=get(plotHandle_(i),'Cdata');
                temp_min=min(c);
                temp_min=min(temp_min);
                temp_max=max(c);
                temp_max=max(temp_max);
                temp1 = min([temp1, temp_min]);
                temp2 = max([temp2, temp_max]);
        end

    elseif strcmp(child_type,'patch')
            
        switch (type)
        case 'x'
                
            xyzdata=ixf_plotdata('get',plotHandle_(i),'xyzdata');
                temp_min=min(xyzdata.vertices(:,1));
                temp_min=min(temp_min);
                temp_max=max(xyzdata.vertices(:,1));
                temp_max=max(temp_max);
                temp1 = min([temp1, temp_min]);
                temp2 = max([temp2, temp_max]);
                
            case 'y'
                
                xdata = get(plotHandle_(i),'xdata');
                ydata = get(plotHandle_(i),'ydata');
                xlim = get(axesHandle_,'xlim');

                index = (xdata < xlim(2) & xdata > xlim(1));
                
                temp_min = min(min(ydata(index)));
                temp_max = max(max(ydata(index)));
                
                temp1 = min([temp1, temp_min]);
                temp2 = max([temp2, temp_max]);
                
            case 'z'
                xdata = get(plotHandle_(i),'xdata');
                ydata = get(plotHandle_(i),'ydata');
                xlim = get(axesHandle_,'xlim');
                ylim = get(axesHandle_,'ylim');
                zdata = get(plotHandle_(i),'zdata');
            
                [ind1, ind2] = find((xdata < xlim(2) & xdata > xlim(1) & ydata < ylim(2) & ydata > ylim(1)));
                
                temp_min = min(zdata(ind2));
                temp_max = max(zdata(ind2));
                
                temp1 = min([temp1, temp_min]);
                temp2 = max([temp2, temp_max]);
            
            case 'c'
                xdata = get(plotHandle_(i),'xdata');
                ydata = get(plotHandle_(i),'ydata');
                xlim = get(axesHandle_,'xlim');
                ylim = get(axesHandle_,'ylim');
                cdata = get(plotHandle_(i),'cdata');
                [ind1, ind2] = find((xdata < xlim(2) & xdata > xlim(1) & ydata < ylim(2) & ydata > ylim(1)));
                
                temp_min = min(cdata(ind2));
                temp_max = max(cdata(ind2));
                
                temp1 = min([temp1, temp_min]);
                temp2 = max([temp2, temp_max]);
        end
   
    end
end

