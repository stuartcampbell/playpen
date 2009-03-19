function ms_cut(cmd);

% function ms_cut(cmd);
% callback function for the 'Plot Cut' button on the MSlice Control Window
% cmd='tomfit' if cut is to be sent directly to Mfit as an xye file
% cmd='over' for over plotting, new plot otherwise
% cmd='store_bkg_E' to generate an energy-dependent 'background' estimate from the cut

% == return if no Control Window present, no data read or projections not calculated yet
fig=findobj('Tag','ms_ControlWindow');
if isempty(fig),
   disp('Control Window not active. Return.');
   return;
end
data=get(fig,'UserData');
if isempty(data)|~isfield(data,'S'),
   disp('Load data first, calculate projections, then do cut.');
   return;
elseif ~isfield(data,'v'),
   disp('Calculate projections first, then do cut.');
   return;
end

% ===== read cut parameters from ControlWindow
vars=str2mat('x','vx_min','vx_max','bin_vx','vy_min','vy_max');
sample=get(findobj(fig,'Tag','ms_sample'),'Value');
if sample==1,	% single crystal sample
   analmode=get(findobj(fig,'Tag','ms_analysis_mode'),'Value');
   if analmode==1,	% analysed as single crystal
	   psd=get(findobj(fig,'Tag','ms_det_type'),'Value');	
		% =1 if PSD detectors = 2 if conventional detectors
		if (psd==1),  % extra cut dimension for single crystal psd data 
   		vars=str2mat(vars,'vz_min','vz_max');
      end
   end
end
vars=str2mat(vars,'intensity','i_min','i_max','symbol_colour','symbol_type','symbol_line');
vars=str2mat(vars,'OutputType','OutputDir','OutputFile','xaxis');
for i=1:size(vars,1),
   name=vars(i,:);	% take one variable name at a time
   name=name(~isspace(name));	% strip off white spaces
   h=findobj(fig,'Tag',['ms_cut_' name]);	% find associated object
   if isempty(h),	% display warning message if object not found
      disp(['Warning: object with Tag ms_cut_' name ' not found.']);
   end
   if strcmp(get(h,'Style'),'popupmenu')|strcmp(get(h,'Style'),'checkbox'),
      if strcmp(name,'OutputType'),	% extract value as a string label
         strings=get(h,'String');
         value=strings{get(h,'Value')};	% {'none','.cut','.xye','.smh'}
         eval([ name '=''' value ''';']);
      else	% extract value as an index in the list
         value=num2str(get(h,'Value'));
		   eval([ name '=' value ';']);  
      end
   else
      value=get(h,'String');
      if strcmp(name,'OutputDir')|strcmp(name,'OutputFile'),
         eval([ name '=''' value ''';']);  % assign as a string 
      elseif isempty(value);
         eval([name '=[];']);
      else
         eval([name '=' value ';']);	% assign as a number or an expression to be evaluated
      end         
   end
end
colours=str2mat('w','r','b','m','g','c','y','k');
symbols=str2mat('o','s','d','p','h','*','x','+','^','v','>','<','.');
lines=str2mat('','-','--',':','-.');
symbol=[deblank(colours(symbol_colour,:)) deblank(symbols(symbol_type,:)) deblank(lines(symbol_line,:))];

% === extract data from ControlWindow and set symbol type according to cut menu 
data.symbol=symbol; 
colordef none;

% === set full path to OutputFile if cut data is to be saved and calculate hkl projections if required
if ~isempty(OutputFile)&(~strcmp('none',OutputType(~isspace(OutputType)))),
   output_filename=[OutputDir OutputFile];		      
   % === if cut to be saved in .hkl format calculate reciprocal space projections 
   if (sample==1)&~isempty(findstr(lower(OutputType),'hkl')),
      % === calculate h,k,l, projections 
      try
         [aa,bb,cc]=basis_hkl(data.ar,data.br,data.cr);
         data.hkl=zeros(length(data.det_group),length(data.en),3);
         [data.hkl(:,:,1),data.hkl(:,:,2),data.hkl(:,:,3)]=...
            spe2proj_df(data.emode,data.efixed,data.en,...
      		data.det_theta,data.det_psi,data.psi_samp,...
      		[aa 0],[bb 0],[cc 0]);
      	%disp('Using spe2proj_df for hkl');         
      catch   
         data.hkl=q2rlu(sqe2samp(spe2sqe(data),data.psi_samp),data.ar,data.br,data.cr);	% (h,k,l)
      end   
   elseif ~isempty(findstr(lower(OutputType),'smh')),
      % === calculate h,k,l, projections if cut to be saved in smh format
      if (sample==1),	% single crystal sample either SingleCrystal or Powder AnalysisMode
      	try
        		[aa,bb,cc]=basis_hkl(data.ar,data.br,data.cr);
         	data.hkl=zeros(length(data.det_group),length(data.en),3);
         	[data.hkl(:,:,1),data.hkl(:,:,2),data.hkl(:,:,3)]=...
            	spe2proj_df(data.emode,data.efixed,data.en,...
      			data.det_theta,data.det_psi,data.psi_samp,...
               [aa 0],[bb 0],[cc 0]);
            %disp('Using spe2proj_df for smh');
      	catch            
         	data.hkl=q2rlu(sqe2samp(spe2sqe(data),data.psi_samp),data.ar,data.br,data.cr);	% (h,k,l)
      	end   
      else	% sample is 'powder'
         data.hkl=cat(3,spe2modQ(data),data.det_theta*180/pi*ones(size(data.en)),...
            data.det_psi*180/pi*ones(size(data.en)));	% (|Q| (Angs^{-1}),2Theta (deg), Azimuth(deg)) 
		end         
   elseif ~isempty(findstr(lower(OutputType),'cut'))&(~isempty(findstr(lower(OutputType),'mfit'))),
      data.MspDir=ms_getstring(fig,'ms_MspDir');
      data.MspFile=ms_getstring(fig,'ms_MspFile');
      data.sample=sample;	% numerinc
      if sample==1,	% if sample is single crystal put also lattice parmeters
         data.abc=[str2num(get(findobj(fig,'Tag','ms_as'),'String')) ...
               str2num(get(findobj(fig,'Tag','ms_bs'),'String')) ...
               str2num(get(findobj(fig,'Tag','ms_cs'),'String'));...
               str2num(get(findobj(fig,'Tag','ms_aa'),'String')) ...
               str2num(get(findobj(fig,'Tag','ms_bb'),'String')) ...
               str2num(get(findobj(fig,'Tag','ms_cc'),'String'))];
         data.uv=[str2num(get(findobj(fig,'Tag','ms_ux'),'String')) ...
               str2num(get(findobj(fig,'Tag','ms_uy'),'String')) ...
               str2num(get(findobj(fig,'Tag','ms_uz'),'String'));...
               str2num(get(findobj(fig,'Tag','ms_vx'),'String')) ...
               str2num(get(findobj(fig,'Tag','ms_vy'),'String')) ...
               str2num(get(findobj(fig,'Tag','ms_vz'),'String'))];        
      end
   end   
else
   output_filename='';
end

% === highlight red busy button
h_status=findobj(fig,'Tag','ms_status');
if ~isempty(h_status)&ishandle(h_status),
   red=[1 0 0];
   set(h_status,'BackgroundColor',red);
   drawnow;
end

% === choose between plotting a new graph, plotting over an existing graph, 
% === sending data to MFit or storing cut as bkg(E)
if exist('cmd','var')&(~isempty(cmd))&(ischar(cmd))&(strcmp(cmd(~isspace(cmd)),'over'))&...
      (~isempty(findobj('Tag','plot_cut'))),
   figure(findobj('Tag','plot_cut'));
   hold on;
   tomfit=(1<0);	% false
elseif exist('cmd','var')&(~isempty(cmd))&(ischar(cmd))&(strcmp(cmd(~isspace(cmd)),'tomfit')),
   tomfit=(1>0);	% true   
elseif exist('cmd','var')&(~isempty(cmd))&(ischar(cmd))&(strcmp(cmd(~isspace(cmd)),'store_bkg_E')),
   tomfit=(1<0); % false
   hh=findobj(fig,'Tag','ms_cut_x'); 
   if isempty(hh),
      disp('Cut direction could not be identified. Energy-dependent ''background'' not generated.');
      return;
   else
      temp=get(hh,'String');
      xlabel=temp{get(hh,'Value')};
      if strcmp(xlabel(~isspace(xlabel)),'Energy'),	% if cut direction is energy, then perform cut and store in window
         disp(['Storing cut as an estimate for an energy-dependent ''background''.']);
         disp(['Adjusting range and binning along the energy axis to match those of the spe data.']);
         if (sample==1)&(analmode==1)&(psd==1), % if sample is single crystal, analysed as single crystal and with psd detectors
			   cut_d=cut_spe(data,x,data.en(1),data.en(end),data.en(2)-data.en(1),vy_min,vy_max,vz_min,vz_max,i_min,i_max,OutputType,output_filename,tomfit);
			else	% powder mode
   			cut_d=cut_spe(data,x,data.en(1),data.en(end),data.en(2)-data.en(1),vy_min,vy_max,i_min,i_max,OutputType,output_filename,tomfit);
         end         
         set(findobj(fig,'Tag','ms_bkg_E'),'UserData',cut_d);
         return;
      else
         disp(['Currently can only extract ''background'' data from cuts along the energy axis.']);
         return;
      end
   end   
else   
   tomfit=(1<0);	% false
end    
if intensity~=1,	% choose to plot for y-axis not intensity, but say |Q| or K
   [data.S,axislabel_y,unitname_y]=pickvar(data,intensity-1,[]);
   data.ERR=[];
   if (sample==1)&(analmode==1)&(psd==1),
      data.axis_unitlabel=str2mat(deblank(data.axis_unitlabel(1,:)),deblank(data.axis_unitlabel(2,:)),...
         data.axis_unitlabel(3,:),[axislabel_y unitname_y]);
   else
		data.axis_unitlabel=str2mat(deblank(data.axis_unitlabel(1,:)),deblank(data.axis_unitlabel(2,:)),...
         [axislabel_y unitname_y]);
	end      
end

if xaxis~=1,	% choose x-axis for plot not the cut axis but say |Q| or K
   [data.altx,data.altx_axislabel,data.altx_unitname]=pickvar(data,xaxis-1,[]);
end

% === call cut_spe routine with different parameters for cuts through 3d data (single crystal+psd) and 2d data (single crystal+non-PSD or powder)
if (sample==1)&(analmode==1)&(psd==1),
   cut_d=cut_spe(data,x,vx_min,vx_max,bin_vx,vy_min,vy_max,vz_min,vz_max,i_min,i_max,OutputType,output_filename,tomfit);
else
   cut_d=cut_spe(data,x,vx_min,vx_max,bin_vx,vy_min,vy_max,i_min,i_max,OutputType,output_filename,tomfit);
end

% === remove plot_over attribute for plot_cut figure if plot was over an existing graph and cut was not sent directly to mfit 
if ~tomfit,
	fig=findobj('Tag','plot_cut');
	if ~isempty(fig),
   	figure(fig);
		if ishold,
			hold off;	
   	end
   end
end

% === highlight green button indicating 'not busy' 
if ~isempty(h_status)&ishandle(h_status),
   green=[0 1 0];
	set(h_status,'BackgroundColor',green);
   drawnow;
end