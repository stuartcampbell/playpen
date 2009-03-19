function s=dv_read_int

% function s=dv_read_int
% Enable sensitive map reading of spectra numbers and intensities

% DO NOT READ ANYTHING IF CURRENT FIGURE IS NOT plot_sum or plot_msk 
fig=gcf;
if any(fig==findobj('Tag','plot_msk'))|any(fig==findobj('Tag','old_plot_msk'))|...
      any(fig==findobj('Tag','plot_sum'))|any(fig==findobj('Tag','old_plot_sum')),
	text_h=findobj(fig,'Tag','Text');
	d=get(fig,'UserData');
	spectramap=d.spectramap;
   data=d.data; % intensities or mask
else
   return;
end

% READ POINTER POSITION AND CONVERT INTO SPECTRUM NUMBER given the spectrum map
scrn_pt=get(0,'PointerLocation');
set(fig,'units','pixels');
loc=get(fig,'Position');
pt=[scrn_pt(1)-loc(1), scrn_pt(2)-loc(2)];
set(fig,'CurrentPoint',pt);
pt=get(gca,'CurrentPoint');
x=pt(1,1);
y=pt(1,2);

spec=pos2spec([x y],spectramap);
if spec>=1,
   if ~isempty(data)&size(data,2)==3,
      % intensities also stored 
      i=(find(data(:,1)==spec(1)));
      if ~isempty(i),
         str=str2mat(sprintf('Spectrum %d',data(i(1),1)),...
            sprintf('Intensity %g',data(i(1),2)),...
            sprintf('± %g',data(i(1),3)));         
      else
         str=sprintf('Spectrum %d',spec);
      end
   else
   	str=sprintf('Spectrum %d',spec);      
   end   
else
	str='Spectrum none';
end 
set(text_h,'String',str);
