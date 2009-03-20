function ms_disp;

% function ms_disp;
% callback function for the 'Display' button on the MSlice Control Window

% == return if no Control Window present, no data read or projections of data or bin boundaries not calculated yet
fig=findobj('Tag','ms_ControlWindow');
if isempty(fig),
   disp('Control Window not active. Return.');
   return;
end
data=get(fig,'UserData');
if isempty(data)|~isfield(data,'S'),
   disp('Load data first, then do display.');
   return;
elseif ~isfield(data,'v'),
   disp('Calculate projections first, then do slice.');
   return;
end

% === read disp parameters from ControlWindow
vars=str2mat('vx_min','vx_max','vy_min','vy_max');
vars=str2mat(vars,'i_min','i_max');
vars=str2mat(vars,'colmap','nsmooth','shad');
for i=1:size(vars,1),
   name=vars(i,:);
   name=name(~isspace(name));
   h=findobj('Tag',['ms_disp_' name]);
   if strcmp(get(h,'Style'),'popupmenu')|strcmp(get(h,'Style'),'checkbox'),
      value=num2str(get(h,'Value'));
   else
      value=get(h,'String');
      value=value(~isspace(value));
   end
   if ~isempty(value),	    
      eval([ name '=' value ';']); 
   else
      eval([name '=[];']);
   end
end

% === establish shading option
shad_opt=get(findobj('Tag','ms_disp_shad'),'String');
if ~isempty(shad_opt),
	shad=shad_opt{shad};	% change type from number to 'flat','faceted' or 'interp'
else
   shad='flat';
   disp('Default "flat" shading option chosen');
end
   
% === read colour table
if colmap==3,
   map=jet;	% MATLAB blue -> red 'jet.m' RGB colour table
   linearlog='linear';
else
   % read PHOENIX-type colour map black->red
   map=get(findobj('Tag','ms_disp_colmap'),'UserData');
   if colmap==1,
      linearlog='linear';	% linear scale 
   else
      linearlog='log';		% logarithmmic scale
   end
end   
colordef none;

% === call disp_spe function
if isempty(nsmooth)|~isnumeric(nsmooth)|(nsmooth<1),
	disp_spe(data,vx_min,vx_max,vy_min,vy_max,i_min,i_max,shad,map,linearlog);
else
   disp_spe(smooth_spe(data,nsmooth),vx_min,vx_max,vy_min,vy_max,i_min,i_max,shad,map,linearlog);
end
