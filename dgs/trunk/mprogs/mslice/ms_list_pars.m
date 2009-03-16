function ms_list_pars(file)

% function ms_list_pars
% produce an ascii image of the Mslice Control Window 
% and display in command line or save to a file for printing

% === locate ControlWindow
h_cw=findobj('Tag','ms_ControlWindow');
if isempty(h_cw),
   disp('Control Window not active. Return.');
   return;
elseif length(h_cw)>1,
   disp([ nums2str (length(h_cw)) ' Control Windows detected. Command not executed.']);
   return;
end   

% === open ascii file to output parameter list
if ~exist('file','var')|isempty(file)|~ischar(file)|isempty(file(~isspace(file))),
   % === list parameters only (save to a temporary file which will be deleted at the end)
   file=tempname;
   list_only=(1>0); % TRUE
else 
   % === select file to output parameter list
   
   % === default name and directory are as for the msp file
   h_mspdir=findobj(h_cw,'Tag','ms_MspDir');
   if isempty(h_mspdir),
      disp(['Could not locate handle to object MspDir']);
      defaultdir=pwd;
   else
      defaultdir=get(h_mspdir,'String');
      if isempty(defaultdir)|isempty(defaultdir(~isspace(defaultdir))),
         defaultdir=pwd;
      end
   end
   
   h_mspfile=findobj(h_cw,'Tag','ms_MspFile');
   if isempty(h_mspfile),
      disp(['Could not locate handle to object MspFile']);
      defaultfile='mslice.txt';
   else
      defaultfile=get(h_mspfile,'String');
      if isempty(defaultfile)|isempty(defaultfile(~isspace(defaultfile))),
         defaultfile='mslice.txt';
      end
   end
   
   fpos=findstr(defaultfile,'.');
   if ~isempty(fpos),	% eliminate extension of msp file
      filename=[defaultdir defaultfile(1:(fpos(1)-1))];
   else
      filename=[defaultdir defaultfile];
   end
   
   [filename,pathname]=uiputfile( [filename '.txt'],'Select file to output parameter list');
	if ischar(filename),
      file=[pathname filename];
   	list_only=(1<0);	% FALSE   
   else % === no file selected 
   	return;
   end
end

fid=fopen(file,'wt');
if fid==-1, 
   if list_only, 
      disp(['Error writing parameter list to temporary file ' file ' . Return.']);
   else
      disp(['Error writing parameter list to file ' file ' . Return.']);
	end      
  	return;
end   

% === write SPECTROMETER section (spectrometer and data file parameters)
fprintf(fid,'%-15s\n','SPECTROMETER');
fprintf(fid,'%-15s = %-15s    %s-%s\n','efixed(meV)',ms_getstring(h_cw,'ms_efixed'),...
   ms_getstring(h_cw,'ms_emode'),'geometry');
fprintf(fid,'%-15s = %s\n','DataFile(.spe)',ms_getstring(h_cw,'ms_DataFile'));
fprintf(fid,'%-15s = %s\n','DataDir',ms_getstring(h_cw,'ms_DataDir'));
fprintf(fid,'%-15s = %s\n','DetFile(.phx)',ms_getstring(h_cw,'ms_PhxFile'));
fprintf(fid,'%-15s = %s\n','DetectorDir',ms_getstring(h_cw,'ms_PhxDir'));
fprintf(fid,'%-15s = %s\n','ParamFile(.msp)',ms_getstring(h_cw,'ms_MspFile'));
fprintf(fid,'%-15s = %s\n','ParameterDir',ms_getstring(h_cw,'ms_MspDir'));
fprintf(fid,'%-15s = %s\n','IntensityLabel',ms_getstring(h_cw,'ms_IntensityLabel'));
fprintf(fid,'%-15s = %s\n \n','TitleLabel',ms_getstring(h_cw,'ms_TitleLabel'));

% === write sample parameters
fprintf(fid,'%-15s = %s\n','SAMPLE',ms_getstring(h_cw,'ms_sample'));

% === determine sample type 
h_samp=findobj(h_cw,'Tag','ms_sample');
if isempty(h_samp),
   disp(['Sample type could not be identified. Return.']);
   fclose(fid);
   return;
end
sample=get(h_samp,'Value');

if sample==1, 	% single crystal sample 
   % === write UNIT CELL LATTICE PARAMETERS AND CRYSTAL ORIENTATION ===
   fprintf(fid,'%-s\n','Unit cell lattice parameters');   
   fprintf(fid,'%-10s%-10s%-10s%-10s%-10s%-10s\n','a(Å)    =',ms_getstring(h_cw,'ms_as'),...
      'b(Å)    =',ms_getstring(h_cw,'ms_bs'),'c(Å)    =',ms_getstring(h_cw,'ms_cs'));
   fprintf(fid,'%-10s%-10s%-10s%-10s%-10s%-10s\n','aa(deg) =',ms_getstring(h_cw,'ms_aa'),...
      'bb(deg) =',ms_getstring(h_cw,'ms_bb'),'cc(deg) =',ms_getstring(h_cw,'ms_cc'));
   % === write crystal orientation
   fprintf(fid,'%-s\n','Crystal orientation'); 
   fprintf(fid,'%-10s%-10s%-10s%-10s%-10s%-10s\n','ux      =',ms_getstring(h_cw,'ms_ux'),...
      'uy      =',ms_getstring(h_cw,'ms_uy'),'uz      =',ms_getstring(h_cw,'ms_uz'));
   fprintf(fid,'%-10s%-10s%-10s%-10s%-10s%-10s\n','vx      =',ms_getstring(h_cw,'ms_vx'),...
      'vy      =',ms_getstring(h_cw,'ms_vy'),'vz      =',ms_getstring(h_cw,'ms_vz'));
   fprintf(fid,'%-10s%-10s%s\n \n','Psi(deg)=',ms_getstring(h_cw,'ms_psi_samp'),'angle between ki and u');
   
   % === determine analysis mode : single crystal / powder
	h_anal=findobj(h_cw,'Tag','ms_analysis_mode');
	if isempty(h_anal),
   	disp(['Analysis mode for single crystal sample could not be identified. Return.']);
   	fclose(fid);
   	return;
	end
   anal_mode=get(h_anal,'Value');
   
   if anal_mode==1,	% analyse as single crystal
	   % === determine detector type 
		h_det=findobj(h_cw,'Tag','ms_det_type');
		if isempty(h_det),
   		disp(['Detector type for single crystal analysis could not be identified. Return.']);
   		fclose(fid);
   		return;
		end
		det_type=get(h_det,'Value');
      fprintf(fid,'%-s= %-18s%-s =%-s\n \n','ANALYSIS MODE',ms_getstring(h_cw,'ms_analysis_mode'),...
         'DETECTORS',ms_getstring(h_cw,'ms_det_type'));
   else
      fprintf(fid,'%-s= %-18s\n \n','ANALYSIS MODE',ms_getstring(h_cw,'ms_analysis_mode'));
   end
end
if (sample==1)&(anal_mode==1),	% single crystal sample analysed as single crystal
	% === write VIEWING AXES section
   fprintf(fid,'%-s\n','ORTHOGONAL');   
   fprintf(fid,'%-15s%-10s%-10s%-10s%-10s%-s\n','VIEWING AXES','a*','b*','c*','Energy','Label');
   fprintf(fid,'%-15s%-10s%-10s%-10s%-10s%-s\n','u1      =',ms_getstring(h_cw,'ms_u11'),...
     	ms_getstring(h_cw,'ms_u12'),ms_getstring(h_cw,'ms_u13'),ms_getstring(h_cw,'ms_u14'),...
     	ms_getstring(h_cw,'ms_u1label'));
   fprintf(fid,'%-15s%-10s%-10s%-10s%-10s%-s\n','u2      =',ms_getstring(h_cw,'ms_u21'),...
     	ms_getstring(h_cw,'ms_u22'),ms_getstring(h_cw,'ms_u23'),ms_getstring(h_cw,'ms_u24'),...
     	ms_getstring(h_cw,'ms_u2label'));
	if det_type==1,	% if PSD detectors then 3 viewing axes
      fprintf(fid,'%-15s%-10s%-10s%-10s%-10s%-s\n \n','u3      =',ms_getstring(h_cw,'ms_u31'),...
     		ms_getstring(h_cw,'ms_u32'),ms_getstring(h_cw,'ms_u33'),ms_getstring(h_cw,'ms_u34'),...
     		ms_getstring(h_cw,'ms_u3label'));
   end
else
   % === sample is POWDER or single crystal analysed as POWDER ===
   fprintf(fid,'%-15s%-10s%-10s\n','VIEWING AXES','','Label');
   fprintf(fid,'%-15s%-10s%-s\n','u1      =',ms_getstring(h_cw,'ms_u1'),...
      ms_getstring(h_cw,'ms_u1label'));
   fprintf(fid,'%-15s%-10s%-s\n','u2      =',ms_getstring(h_cw,'ms_u2'),...
      ms_getstring(h_cw,'ms_u2label'));   
end   

if (sample==1)&(anal_mode==1)&(det_type==1),	% single crystal and PSD detectors SLICE MENU
   % === write SLICE section      
 	fprintf(fid,'%-15s%-15s%-s\n','SLICE PLANE','perp to axis',ms_getstring(h_cw,'ms_slice_z'));
   fprintf(fid,'%-25s = %-10s %-4s %-10s\n',...
      'thickness',ms_getstring(h_cw,'ms_slice_vz_min'),...
      ' to ',ms_getstring(h_cw,'ms_slice_vz_max'));
   fprintf(fid,'%-25s = %-10s %-4s %-10s %-10s %-s\n',...
      ms_getstring(h_cw,'ms_slice_x_axis'),ms_getstring(h_cw,'ms_slice_vx_min'),...
      ' to ',ms_getstring(h_cw,'ms_slice_vx_max'),'step',ms_getstring(h_cw,'ms_slice_bin_vx'));       
   fprintf(fid,'%-25s = %-10s %-4s %-10s %-10s %-s\n',...
      ms_getstring(h_cw,'ms_slice_y_axis'),ms_getstring(h_cw,'ms_slice_vy_min'),...
      ' to ',ms_getstring(h_cw,'ms_slice_vy_max'),'step',ms_getstring(h_cw,'ms_slice_bin_vy'));              
   fprintf(fid,'%-25s = %-10s %-4s %-10s %-10s %-s\n',...
      'Intensity range',ms_getstring(h_cw,'ms_slice_i_min'),' to ',ms_getstring(h_cw,'ms_slice_i_max'),...
      'ColorMap',ms_getstring(h_cw,'ms_slice_colmap'));              
   fprintf(fid,'%-25s = %-10s%-10s  =  %-10s\n \n',...
      'Smoothing level',ms_getstring(h_cw,'ms_slice_nsmooth'),' Shading',ms_getstring(h_cw,'ms_slice_shad'));   
else % single crystal and conventional detectors or powder mode, only 2 viewing axes
      % === write DISPLAY section    
   fprintf(fid,' \n%-10s\n','DISPLAY');
   fprintf(fid,'%-25s = %-10s %-4s %-10s\n',...
      ms_getstring(h_cw,'ms_disp_x_axis'),ms_getstring(h_cw,'ms_disp_vx_min'),...
      ' to ',ms_getstring(h_cw,'ms_disp_vx_max'));
   fprintf(fid,'%-25s = %-10s %-4s %-10s\n',...
      ms_getstring(h_cw,'ms_disp_y_axis'),ms_getstring(h_cw,'ms_disp_vy_min'),...
      ' to ',ms_getstring(h_cw,'ms_disp_vy_max'));  
   fprintf(fid,'%-25s = %-10s %-4s %-10s %-10s %-s\n',...
      'Intensity range',ms_getstring(h_cw,'ms_disp_i_min'),' to ',ms_getstring(h_cw,'ms_disp_i_max'),...
      'ColorMap',ms_getstring(h_cw,'ms_disp_colmap'));              
   fprintf(fid,'%-25s = %-10s%-10s  =  %-10s\n \n',...
      'Smoothing level',ms_getstring(h_cw,'ms_disp_nsmooth'),' Shading',ms_getstring(h_cw,'ms_disp_shad'));   
end
   
% === write CUT section      
fprintf(fid,'%-5s%-15s%-s\n','CUT','along axis',ms_getstring(h_cw,'ms_cut_x'));
fprintf(fid,'%-25s = %-10s %-4s %-10s %-10s %-s\n',...
   'from',ms_getstring(h_cw,'ms_cut_vx_min'),...
   ' to ',ms_getstring(h_cw,'ms_cut_vx_max'),...
   'step',ms_getstring(h_cw,'ms_cut_bin_vx'));
fprintf(fid,'%-25s = %-10s %-4s %-s\n',...
   ms_getstring(h_cw,'ms_cut_y_axis'),ms_getstring(h_cw,'ms_cut_vy_min'),...
   ' to ',ms_getstring(h_cw,'ms_cut_vy_max'));
if (sample==1)&(anal_mode==1)&(det_type==1),	% if spingle crystal and PSD detectors, then 3 axes      
  fprintf(fid,'%-25s = %-10s %-4s %-s\n',...
      ms_getstring(h_cw,'ms_cut_z_axis'),ms_getstring(h_cw,'ms_cut_vz_min'),...
      ' to ',ms_getstring(h_cw,'ms_cut_vz_max'));
end
fprintf(fid,'%-5s%-15s\n','x-axis = ',ms_getstring(h_cw,'ms_cut_xaxis'));
fprintf(fid,'%-15s %-9s = %-10s %-4s %-s\n',...
   ms_getstring(h_cw,'ms_cut_intensity'),'range',ms_getstring(h_cw,'ms_cut_i_min'),...
   ' to ',ms_getstring(h_cw,'ms_cut_i_max'));
fprintf(fid,'%-15s = %-s %-s, %-s\n',...
  'Symbol',ms_getstring(h_cw,'ms_cut_symbol_colour'),ms_getstring(h_cw,'ms_cut_symbol_type'),...
  ms_getstring(h_cw,'ms_cut_symbol_line'));   
fprintf(fid,'%-15s = %-10s\n','OutputType',ms_getstring(h_cw,'ms_cut_OutputType'));
fprintf(fid,'%-15s = %s\n','OutputFile',ms_getstring(h_cw,'ms_cut_OutputFile'));
fprintf(fid,'%-15s = %s\n \n','OutputDir',ms_getstring(h_cw,'ms_cut_OutputDir'));   

% === write DETECTOR TRAJECTORIES section
fprintf(fid,'%-s\n','DETECTOR TRAJECTORIES');
fprintf(fid,'%-10s = %-15s %-4s %-10s %-4s %-10s\n',...
   'x',ms_getstring(h_cw,'ms_plot_traj_x'),'from',...
   ms_getstring(h_cw,'ms_plot_traj_vx_min'),' to ',ms_getstring(h_cw,'ms_plot_traj_vx_max'));
fprintf(fid,'%-10s = %-15s %-4s %-10s %-4s %-10s\n',...
   'y',ms_getstring(h_cw,'ms_plot_traj_y'),'from',...
   ms_getstring(h_cw,'ms_plot_traj_vy_min'),' to ',ms_getstring(h_cw,'ms_plot_traj_vy_max'));
fprintf(fid,'%-10s = %-15s %-4s %-10s %-4s%-10s\n',...
   'z',ms_getstring(h_cw,'ms_plot_traj_z'),'from',...
   ms_getstring(h_cw,'ms_plot_traj_vz_min'),' to ',ms_getstring(h_cw,'ms_plot_traj_vz_max'));
fprintf(fid,'%-10s = %-15s\n',...
   'Contours',ms_getstring(h_cw,'ms_plot_traj_cont_lines'));
fprintf(fid,'%-10s%-10s%-4s%-10s%-10s%-10s%-10s%-10s\n','from',...
   ms_getstring(h_cw,'ms_plot_traj_cont_min'),' to ',ms_getstring(h_cw,'ms_plot_traj_cont_max'),...
   'step1 = ',ms_getstring(h_cw,'ms_plot_traj_cont_step1'),'step2 = ',ms_getstring(h_cw,'ms_plot_traj_cont_step2'));
if sample==1,
	fprintf(fid,'%-10s= %-5s %-10s= %-5s\n',...
   	'(hkl)points',ms_getstring(h_cw,'ms_plot_traj_hkl_points'),...
      '(hkl)labels',ms_getstring(h_cw,'ms_plot_traj_hkl_labels'));
end
fprintf(fid,'%-10s = %-s',...
   'Command',ms_getstring(h_cw,'ms_plot_traj_command'));
fclose(fid);

% === list saved file if required, or return otherwise 
if list_only,
	% === open file to read parameter list and display in matlab command window
	fid=fopen(file,'rt');
	if fid==-1, 
   	disp(['Error reading parameter list from file ' file ' . Return.']);
   	return;
	end

	disp('----------------------------------------------------------------------------');
	temp=fgetl(fid);
	while ischar(temp)|~isempty(temp),
   	disp(temp);
   	temp=fgetl(fid);
   	if temp==-1,
      	temp=[];
   	end
	end
	disp('----------------------------------------------------------------------------');
	fclose(fid); 
   delete(file);
else
   disp(['Saved parameter list to file ' file]);
end


