function ms_fitcut_load_data

% function ms_fitcut_load_data
% callback function for the 'Load Data' button in the Mslice :Fit Cut window
% === return if Ms_fitcut window not opened 
h_cw=findobj('Tag','ms_fitcut');
if isempty(h_cw),
   disp(['No Mslice FitCut widow opened, no .cut data to be loaded.']);
   return;
end

%=== read parameters of .cut data file
h_dir=findobj(h_cw,'Tag','ms_fitcut_CutDir');
h_file=findobj(h_cw,'Tag','ms_fitcut_CutFile');
if isempty(h_dir)|isempty(h_file),
   disp(['Could not associate objects to .cut directory and filename. Do cut file could be read.']);
   return;
end
cut_file=get(h_file,'String');
cut_file=cut_file(~isspace(cut_file));	% remove white spaces
if ~isempty(cut_file),
   cut_filename=[get(h_dir,'String') cut_file];
else	% whole filename is empty if no file given
   cut_filename=[];
end

% === read parameters of .par detector layout file
h_dir=findobj('Tag','ms_fitcut_ParDir');
h_file=findobj('Tag','ms_fitcut_ParFile');
if isempty(h_dir)|isempty(h_file),
   disp(['Could not associate objects to .par directory and filename. No data file could be read.']);
   return;
end
par_file=get(h_file,'String');
par_file=par_file(~isspace(par_file));
if ~isempty(par_file),
	par_filename=[get(h_dir,'String') par_file];
else
   par_filename=[];
end

% === construct .cut data structure 
cut=load_cut(cut_filename);,
if isempty(cut),
   disp(['Error loading cut from file ' cut_filename]);
   return;
end

if isfield(cut,'title')&~isempty(cut.title),
   if iscell(cut.title),	
      cut.title{length(cut.title)+1}=avoidtex(stripath(cut_filename));
   elseif ischar(cut.title),
      cut.title=str2mat(cut.title,avoidtex(stripath(cut_filename)));
   end
else
   cut.title=avoidtex(stripath(cut_filename));
end

% === retain only last two lines in the title
if iscell(cut.title)&(length(cut.title)>2)
   temp=cut.title;
   cut.title=cell(1,2);
   for i=1:2,
      cut.title{i}=temp{length(temp)-2+i};
   end
end   
   
fields=fieldnames(cut);
for i=1:length(fields),
   h=findobj(h_cw,'Tag',['ms_fitcut_'fields{i}]);
   if ~isempty(h),
      if strcmp(get(h,'Style'),'popupmenu')|strcmp(get(h,'Style'),'checkbox'),
         set(h,'Value',str2num(getfield(cut,fields{i})));         
       %  disp([fields{i} '=' getfield(cut,fields{i}) ' as Value']);  
      elseif (~strcmp(fields{i},'CutFile')&~strcmp(fields{i},'CutDir')),  
         set(h,'String',getfield(cut,fields{i}));
       %  disp([fields{i} '=' getfield(cut,fields{i}) ' as a String']);           
      end
   end   
end

% === add detector information to the pixels 
if ~isempty(par_filename), 	  
   par=load_par(par_filename);
else
   par=[];
end
if isempty(par),
   disp('Detector file not loaded.');
   return;
end
par=par(cut.pixels(:,1),:);	% [l2 theta psi width height] for all pixels

% === remove points with zero error bars
deselected=find(cut.e==0);	% index of points with zero errorbars
if ~isempty(deselected),
   disp('Removed points with zero error bar.');
	cut.x(deselected)=[];
	cut.y(deselected)=[];
	cut.e(deselected)=[];
	cumindex=[0 cumsum(cut.npixels)];	% absolute indexes into the pixel sequence
	cut.npixels(deselected)=[];
	index=[];	% will index pixels to be removed
	for i=1:length(deselected),
		j=deselected(i);
   	index=[index (cumindex(j)+1):cumindex(j+1)];
	end
   cut.pixels(index(:),:)=[];	% remove pixels of removed points
   par(index(:),:)=[];
end
cut.pixels=[cut.pixels(:,2)';cut.pixels(:,3)';par(:,2)'*pi/180;par(:,3)'*pi/180;par(:,1)';par(:,4)';par(:,5)';...
   cut.pixels(:,1)'; cut.pixels(:,4)'; cut.pixels(:,5)'; cut.pixels(:,6)']';
% [en(meV) enbin(meV) theta(radians) psi(radians) l2(m) width(m) height(m) det_number x y e]

if ~isempty(cut),
   set(h_cw,'UserData',cut);
end

ms_fitcut_update;	% update parameters in the ms_fitcut window

% === set number of fit data points to number of points in the cut 
h=findobj('Tag','mf_FitPoints');
if ~isempty(h),
	set(h,'String','');
end

colordef none;
cut2mfit(cut);


