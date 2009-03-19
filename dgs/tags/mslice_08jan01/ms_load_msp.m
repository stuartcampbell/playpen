function ms_load_msp(file);

% function ms_load_msp(file);

% ==== return if ControlWindow not openend or if MspDir and MspFile objects could not be located
h_cw=findobj('Tag','ms_ControlWindow');
if isempty(h_cw),
   disp(['No MSlice Control widow opened, no parameter file loaded.']);
   return;
end
h_dir=findobj(h_cw,'Tag','ms_MspDir');
h_file=findobj(h_cw,'Tag','ms_MspFile');
if isempty(h_dir)|isempty(h_file),
  	disp(['Could not associate objects to .msp directory and filename. Parameter file not read.']);
  	return;
end

%=== SELECT .MSP PARAMETER FILE 
if ~exist('file','var'), 
   % ==== select .msp file by browsing =====
   cancel=ms_getfile(h_dir,h_file,'*.msp','Load parameter file for MSlice ControlWindow (.msp)');
   % === if cancel button pressed then return, do not load in .msp file
   if cancel,
      return
   end
   fullname=[get(h_dir,'String') get(h_file,'String')];  
else  % ==== select given or default .msp file =========
   h_mslicedir=findobj('Tag','ms_MSliceDir');
   if isempty(h_mslicedir),
      disp(['Could not determine MSlice path. Return.']);
      return;
   end
   MSliceDir=get(h_mslicedir,'String');
   if isempty(MSliceDir),
      disp(['MSlice path appears to be empty. Return.']);
      return;
   end
   if exist(file,'file')==2,
      fullname=file;
      %fullname=which(file);
   elseif exist([MSliceDir filesep file])==2,
      fullname=[MSliceDir filesep file];
   elseif exist([pwd filesep file])==2,
      fullname=[pwd filesep file];
   else % file could not be found in the default search path ,
          % default parameter file chosen instead 
      disp(['Given parameter file ' file ' not found in search path.']);
      fullname=[MSliceDir 'crystal_psd.msp']; 
      disp(['Select default parameter file ' fullname]);  
   end
end

% === open .msp parameter file for reading as an ASCII text file
fid=fopen(fullname,'rt');
if fid==-1,
   disp(['Error opening parameter file ' fullname '. Return.']);
   return;
end

% === highlight red button indicating 'busy'
h_status=findobj(h_cw,'Tag','ms_status');
if ~isempty(h_status)&ishandle(h_status),
   red=[1 0 0];
   set(h_status,'BackgroundColor',red);
   drawnow;
end

%=== READ .MSP FILE LINE BY LINE
disp(['Proceed reading parameter file ' fullname]);
t=fgetl(fid);
while (ischar(t))&(~isempty(t(~isspace(t)))),
   pos=findstr(t,'=');
   field=t(1:pos-1);
   field=field(~isspace(field));
   
   % === if having reached det_type, the sample is single crystal and analysis mode is undefined
   % === put by default analysis mode = single crystal
   if strcmp(lower(field),'det_type')&(get(findobj(h_cw,'Tag','ms_sample'),'Value')==1),
      analobj=findobj(h_cw,'Tag','ms_analysis_mode');
      if ~isempty(analobj)&(get(analobj,'Value')~=1),	
	      set(analobj,'Value',1);
         ms_analysis_mode;
         drawnow;
      end
   end   
   h=findobj(h_cw,'Tag',['ms_' field]);
   if ~isempty(h),
      value=t(pos+1:length(t));
   	value=deblank(value);	% remove trailing blanks from both the beginning and end of string
   	value=fliplr(deblank(fliplr(value)));
   	if strcmp(get(h,'Style'),'popupmenu')|strcmp(get(h,'Style'),'checkbox'),
      	set(h,'Value',str2num(value));
   	%	disp(['ms_' field ' gets ''Value'' property ' value]);   
   	else
      	set(h,'String',value);
   	%   disp(['ms_' field ' gets ''String'' property ' value]); 
   	end 
      eval(get(h,'Callback'));
      drawnow;
   else   
      disp(['Field ms_' field ' not defined. Check .msp file.']);
      %fclose(fid);
      %return;
   end 
   t=fgetl(fid);
end
fclose(fid);
disp(['Successfully read parameter file ' fullname]);

% === update .msp file details
[filename,pathname]=stripath(fullname);
set(h_dir,'String',pathname);
set(h_file,'String',filename);   

% === highlight green button indicating 'not busy' 
if ~isempty(h_status)&ishandle(h_status),
   green=[0 1 0];
	set(h_status,'BackgroundColor',green);
   drawnow;
end