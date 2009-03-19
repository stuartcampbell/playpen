function plot_cut(cut)

% function plot_cut(cut_d)
% called by : cut_spe to plot data obtained from 1d cuts from mslice  
% cut_d 
% required fields
%          x: [   1x41 double]	required
%          y: [   1x41 double]	required
%          e: [   1x41 double]	required
% optional fields
%    x_label: '[ -0.3, 0, Q_l ]  in 1.160 Å^{-1}' or cell of strings , default 'x' 
%    y_label: 'Intensity(abs.units)' or cell of strings, default 'y' 
%      title: {   1x2  cell  } , default 'cut_d', could be 'title', {'title'} or {'title_1',...'title_n'} 
%     symbol: 'rs'	optional, default 'wo', could be 'o,s,d ..' or 'b.'... or 'wo-'... 
%       axis: [-0.6150 0.6300 0 0.6000]	optional, default axis auto
%      y_min: 0	optional, default ax(3) only present if y_min is forced by the user
%      y_max: 0.6000	optional, default ax(4) only present if y_max is forced by the user
% Radu Coldea 02-Oct-1999
% to add more postscript printers to the list under 'ColorPrint' menu go to line 42 

cut_d=cut;

fig=findobj('Tag','plot_cut');
if isempty(fig), 
   %=== if 'plot_cut' figure does not exist, create a new one
	colordef none;	% black background
   % === create a new figure with 'Tag' property 'plot_cut'
   fig=figure('Tag','plot_cut','PaperPositionMode','auto','Name','MSlice : Plot Cut');	
   % === find any old figures and set current figure size to match the dimensions of the 
   % === last old figure of the same type
   oldfig=findobj('Tag','old_plot_cut'); 
   if ~isempty(oldfig),
      oldfig=sort(oldfig);
     	set(fig,'Position',get(oldfig(end),'Position'));
   end
   % === define 'UserData' field of current figure     
   set(fig,'UserData',[]);
   % === create menu option to be able to do colour hardcopy of 1d plots  
   h=uimenu(fig,'Label','ColourPrint','Tag','Color_Print','Enable','on');
   uimenu(h,'Label','Phaser560','Callback','ms_printc('' \\NDAIRETON\Phaser560:'');');
   uimenu(h,'Label','Phaser0','Callback','ms_printc('' \\NDAIRETON\Phaser0:'');');
   uimenu(h,'Label','DACColour','Callback','ms_printc('' \\NDAIRETON\DACColour:'');');
   uimenu(h,'Label','CRISPColour','Callback','ms_printc('' \\NDAIRETON\CRISPColour:'');');      
   uimenu(h,'Label','MAPSColour','Callback','ms_printc('' \\NDAIRETON\MAPSColour:'');');      
   uimenu(h,'Label','LASER00','Callback','ms_printc('' \\ndablagrave\LASER00:'');'); 
   uimenu(h,'Label','LASER01','Callback','ms_printc('' \\ndablagrave\LASER01:'');');      
   uimenu(h,'Label','default printer','Callback','ms_printc;');   
   %=== TO ADD ANOTHER PRINTER IN THIS LIST INSERT ANOTHER LINE AS ABOVE AND 
   %=== EDIT NAME (LABEL) 'PS0PS' AND PATH '\\ndablagrave\LASER00:'
   % === create menu option to be able to keep figure
   h=uimenu(fig,'Label','Keep','Tag','keep','Enable','on');
   uimenu(h,'Label','Keep figure','Callback','keep(gcf);');
   % === create menu option to be able to make old plot cut figures current
   h=uimenu(fig,'Label','Make Current','Tag','make_cur','Enable','off');
   uimenu(h,'Label','Make Figure Current','Callback','make_cur(gcf);');
   % === create axis menu option 
   h=uimenu(fig,'Label','Axis','Tag','Axis','Enable','on');
   uimenu(h,'Label','Log y axis','Checked','off',...
	 'Tag','Log_y_axis','Callback','ms_ax_linear_log(findobj(gcf,''Tag'',''Log_y_axis''),''YScale'');');
   uimenu(h,'Label','Log x axis','Checked','off',...
    'Tag','Log_x_axis','Callback','ms_ax_linear_log(findobj(gcf,''Tag'',''Log_x_axis''),''XScale'');');
   % === create read point menu option 
   h=uimenu(fig,'Label','Read Point','Enable','on');
   uimenu(h,'Label','Mouse left click on point','Callback','[x,y]=ginput(1);disp(sprintf(''x = %g, y=%g'',x,y));');   
else % === figure 'plot_cut' already exists
  	figure(fig);
  	% === in case plot is over the existing plots read axes limits and previous symbols
  	if strcmp(deblank(get(gca,'Nextplot')),'add'),
     	oldtitle=get(get(gca,'Title'),'String');
     	delete(get(gca,'Title'));
     	set(gca,'DataAspectRatioMode','auto');
     	ax=axis;	% current plot axes        
        
      % === read symbols of previous plots
     	h=findobj(gcf,'Type','line');
     	prevsymb=[];
     	for i=1:length(h),
        	marker=get(h(i),'Marker');
        	if ~strcmp(marker,'none'),
        		prevsymb=[prevsymb marker(1)];
        	end
     	end
     	if ~isfield(cut_d,'symbol'),
        	cut_d.symbol='wo';
      end 
        
     	% === compare cut_d.symbol with previous symbols and if overlap occurs choose a new symbol for plotting
      symbols=str2mat('o','s','d','p','h','*','x','+','^','v','>','<','.');	% available plotting symbols
      % === extract plot symbol from field cut_d.symbol if this exists
      if isfield(cut_d,'symbol')|~isempty(cut_d.symbol)|ischar(cut_d.symbol),
         symbol=symbols(findstr(symbols(:)',cut_d.symbol(1)));
  		   if isempty(symbol)&(length(cut_d.symbol)>=2),
     		   symbol=symbols(findstr(symbols(:)',cut_d.symbol(2)));
        	end
      end
      % === plot symbol extracted from cut_d.symbol could not be identified with a valid symbol use default value  
     	if ~exist('symbol','var')|isempty(symbol),
         symbol='o'; % default plot symbol = circle  
     	end
     
     	n=0;	% === compare symbol with symbols of previous plots, if overlap found try a different symbol
     	while (n<=length(symbols))&(findstr(prevsymb,symbol)>=1),
        	n=n+1;
        	if n<=length(symbols),
           	symbol=symbols(n);
        	end
     	end
            
		% === modify cut_d.symbol in the ControlWindow panel, if this exists (if cut_spe is called from the MSlice control window)     
  	   h=findobj('Tag','ms_cut_symbol_type');
     	if ~isempty(h),
        	set(h,'Value',findstr(symbols(:)',symbol));
     	end
        
      if n>length(symbols),
   	   disp(['Maximum number of different types of symbols exceeded.']);
         disp(['Plot cut using symbol ' cut_d.symbol ' anyway.']);
      else % === now modify cut_d.symbol field to include the new plotting symbol  
      	if ~isempty(cut_d.symbol)&length(cut_d.symbol>=2),
         		cut_d.symbol(2)=symbol;
      	else
         		cut_d.symbol=symbol;
      	end
		end
	else    
      % disp('Plot in an existing plot_cut figure, but delete previous graphs')
    	figure(fig);   
      yscale=get(gca,'Yscale'); % keep axes scaling the same as in the previous plot
      xscale=get(gca,'Xscale');
  		delete(gca);   
	end   
end

if ~isfield(cut_d,'symbol')|isempty(cut_d.symbol)|~ischar(cut_d.symbol),
   cut_d.symbol='wo';
end
ms_errorbar(cut_d.x,cut_d.y,cut_d.e,cut_d.symbol);

% == if plotting in an existing plot_cut window keep the scaling linear/log of the previous plot
if exist('yscale','var')&~isempty(yscale)&ischar(yscale),
   set(gca,'YScale',yscale);
end
if exist('xscale','var')&~isempty(xscale)&ischar(xscale),
   set(gca,'XScale',xscale);
end

% === establish axes limits
if ~isfield(cut_d,'axis')|isempty(cut_d.axis)|~isnumeric(cut_d.axis),
   xmin=min(cut_d.x);
   xmax=max(cut_d.x);
   ymin=min(cut_d.y-cut_d.e);
	ymax=max(cut_d.y+cut_d.e);
  	cut_d.axis=[xmin-0.05*(xmax-xmin) xmax+0.05*(xmax-xmin) ymin-0.2*(ymax-ymin) ymax+0.2*(ymax-ymin)]; %  default range
   if isfield(cut_d,'y_min')&~isempty(cut_d.y_min)&isnumeric(cut_d.y_min),
      cut_d.axis(3)=cut_d.y_min;
   end
   if isfield(cut_d,'y_max')&~isempty(cut_d.y_max)&isnumeric(cut_d.y_max),
      cut_d.axis(4)=cut_d.y_max;
   end        
end

% === treat special case if min and max y limits are the same
if (abs(cut_d.axis(4)-cut_d.axis(3))<eps),
   disp([ 'All y values in the plot seem to be equal to  ' num2str(cut_d.axis(3))]);
   if abs(cut_d.axis(3))<eps,
      cut_d.axis(4)=cut_d.axis(3)+1;
   else
      cut_d.axis(4)=2*cut_d.axis(3);
   end
end

% === treat special case min and max x limits are the same
if (abs(cut_d.axis(2)-cut_d.axis(1))<eps),
   disp([ 'All x values in the plot seem to be equal to  ' num2str(cut_d.axis(1))]);
   if abs(cut_d.axis(1))<eps,
      cut_d.axis(2)=cut_d.axis(1)+1;
   else
      if cut_d.axis(1)>0,
         cut_d.axis(2)=2*cut_d.axis(1);
      else
         cut_d.axis(2)=cut_d.axis(1);
         cut_d.axis(1)=2*cut_d.axis(2); 
      end
   end
end

if exist('ax','var'),	% ===== if overplotting on a given graph
   cut_d.axis(1:2)=[min(ax(1),cut_d.axis(1)) max(ax(2),cut_d.axis(2))];
   % === set y_min if forced by the user 
   if isfield(cut_d,'y_min')&~isempty(cut_d.y_min)&isnumeric(cut_d.y_min),
      cut_d.axis(3)=cut_d.y_min;
   else
      cut_d.axis(3)=min(ax(3),cut_d.axis(3));
   end
   if isfield(cut_d,'y_max')&~isempty(cut_d.y_max)&isnumeric(cut_d.y_max),
      cut_d.axis(4)=cut_d.y_max;
   else
      cut_d.axis(4)=max(ax(4),cut_d.axis(4));
   end   
end


% === establish name of plotting symbol for the title
colours=str2mat('w','r','b','m','g','c','y','k');
fullcolours=str2mat('white','red','blue','magenta','green','cyan','yellow','black');
symbols=str2mat('o','s','d','p','h','*','x','+','^','v','>','<','.');
fullsymbols=str2mat('circle o','square','diamond','pentagram','hexagram','star *','cross x','plus +',...
'triangle u','triangle d','triangle r','triangle l','point .');
symbol=deblank(fullcolours(findstr(colours(:)',cut_d.symbol(1)),:));
symbol=[symbol ' ' deblank(fullsymbols(findstr(symbols(:)',cut_d.symbol(2)),:))];

% === establish title with symbol legend cut_d.title, can be absent, empty, 'string', {'cell(1,1)'} or {'string1', 'string2'}
if ~isfield(cut_d,'title')|isempty(cut_d.title),
   cut_d.title='cut';
end
if iscell(cut_d.title),
   titlestr={[symbol ' = ' cut_d.title{1}]};
   if length(cut_d.title)>=2,
      [titlestr{2:length(cut_d.title)}]=deal(cut_d.title{2:end});
   end
elseif ischar(cut_d.title),
   if size(cut_d.title,1)>1,
      titlestr={[symbol ' = ' cut_d.title(1,:)]};
   else
      titlestr={[symbol ' = ' cut_d.title]};
	end      
end   

% === establish axes labels
if ~isfield(cut_d,'x_label')|isempty(cut_d.x_label)|~(ischar(cut_d.x_label)|iscell(cut_d.x_label)),
   cut_d.x_label='x';
end
if ~isfield(cut_d,'y_label')|isempty(cut_d.y_label)|~(ischar(cut_d.y_label)|iscell(cut_d.y_label)),
   cut_d.y_label='y';
end

figure(fig); 
xlabel(cut_d.x_label);
ylabel(cut_d.y_label);
axis(cut_d.axis); 
% add current plot title at the bottom of the list with the other titles
if exist('oldtitle','var'),	
  	n=length(oldtitle);
  	oldtitle{n+1}=titlestr{1};
   if length(titlestr)>=2,
     [oldtitle{(n+2):(n+length(titlestr))}]=deal(titlestr{2:end});
   end
  	titlestr=oldtitle;
end
h=title(titlestr);
% === change graph dimensions such that there is enough room for the whole title above the graph
set(gca,'DataAspectRatioMode','manual');
a=get(gca,'DataAspectRatio');
extent=get(h,'Extent');
YLim=get(gca,'YLim');
extent=(length(titlestr)+2.5)*(extent(4)/length(titlestr))/a(2);
ap=(YLim(2)-YLim(1))/((YLim(2)-YLim(1))/a(2)-extent);
set(gca,'DataAspectRatio',[a(1) ap a(3)]); 
pos=get(h,'Position');
set(h,'Position',[pos(1) YLim(2) pos(3)]);
pos=get(gca,'Position');
if length(titlestr)>2,
  	set(gca,'Position',[pos(1) pos(2)*a(2)/ap pos(3) pos(4)]);
end