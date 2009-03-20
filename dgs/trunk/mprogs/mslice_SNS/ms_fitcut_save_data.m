function ms_fitcut_save_data

% function ms_fitcut_save_data
% callback function for the 'Save Cut' button in the Mslice :Fit Cut window

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
if isempty(cut_filename),
   disp(['Empty filename. Cut data not saved.']);
end

% === construct .cut data structure 
cut=get(h_cw,'Userdata');
if isempty(cut),
   disp(['Error extracting cut data.']);
   return;
end
npix=size(cut.pixels,1);	% total number of pixels
cut.pixels=[cut.pixels(:,8)'; cut.pixels(:,1)'; cut.pixels(:,2)'; cut.pixels(:,9)'; ...
      cut.pixels(:,10)'; cut.pixels(:,11)']';	% (npix,6) [det_number en(meV) enbin(meV) x y err] for each pixel
h_sample=findobj(h_cw,'Tag','ms_fitcut_sample');
if isempty(h_sample),
   disp(['Could not identify sample type. Return.']);
   return;
end
sample=get(h_sample,'Value');
if sample==1,	% single crystal sample
   cut.sample=1;
   vars=str2mat('as','bs','cs','aa','bb','cc','ux','uy','uz');
   vars=str2mat(vars,'vx','vy','vz','psi_samp','emode','efixed');
   for i=1:size(vars,1),
      name=vars(i,:);
      name=name(~isspace(name));
      h=findobj(h_cw,'Tag',['ms_fitcut_' name]);
      if ~isempty(h),
         if strcmp(get(h,'Style'),'edit'),
            %disp([name '=' get(h,'String') ';']);            
            eval([name '=' get(h,'String') ';']);
         elseif strcmp(get(h,'Style'),'popupmenu')|strcmp(get(h,'Style'),'checkbox'),
            %disp([name '=' num2str(get(h,'Value')) ';']);
            eval([name '=' num2str(get(h,'Value')) ';']);
         end
      else
         disp(['Could not read object for parameter ' name '.']);
      end
   end
   cut.abc=[as bs cs; aa bb cc];
   cut.uv=[ux uy uz; vx vy vz];
   cut.psi_samp=psi_samp*pi/180;	% angle  (degrees -> radians)
   cut.emode=emode;	% energy (meV)
   cut.efixed=efixed;
   if ~isfield(cut,'MspDir'),
   	cut.MspDir=[];
   end
   if ~isfield(cut,'MspFile'),
      cut.MspFile=[];
   end        
   % === remove last line of title which has old filename  
   if isfield(cut,'title')&iscell(cut.title),
      temp=cut.title{1};
      for i=2:(length(cut.title)-1),
         temp={temp cut.title{i}};
      end
      cut.title=temp;
   elseif isfield(cut,'title')&ischar(cut.title),
      temp=deblank(cut.title(1,:));
      for i=2:(size(cut.title,1)-1),
         temp={temp deblank(cut.title(i,:))};
      end
      cut.title=temp;
	end  
   
   % === save only selected data points and their pixels
   [hmf_ctrl, hmf_data, hmf_pars]=mf_figs;
   data=get(hmf_data,'UserData');
   deselected=(find(data(:,4)==0))';	% index of selected points =1 if selected data point, =0 otherwise
   if sum(deselected)>=1,
      disp('Saving only selected data points.');
   end
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
   cut.pixels(index(:),:)=[];	% remove pixels of deselected points
else
   disp(sprintf('Only single crystal parameters are currently appended \n when saving cuts in Mfit .cut format '));
end
% === save cut in Mfit .cut format with labels at the end
save_cut(cut,cut_filename,'Mfit .cut');



