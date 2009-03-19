function ms_save_msp(file);

% function ms_save_msp;
   
% ==== select .msp file to save parameters to by browsing through the directory structure =====
h_cw=findobj('Tag','ms_ControlWindow');
if isempty(h_cw),
   disp(['No Control widow opened, no data to be saved']);
   return;
end

h_dir=findobj(h_cw,'Tag','ms_MspDir');
h_file=findobj(h_cw,'Tag','ms_MspFile');
if isempty(h_dir)|isempty(h_file),
   disp(['Could not associate objects to .msp directory and filename. Parameter file not saved.']);
	return;
end

% === if no <filename> given select file by browsing
if ~exist('file','var')|isempty(file)|~ischar(file),
	cancel=ms_putfile(h_dir,h_file,'*.msp','Select file (.msp) to save Control Window parameters');
	%==== if cancel button pressed then return, do not save any .msp file
	if cancel,
		return
	end
	newfile=[get(h_dir,'String') get(h_file,'String')];
else
   newfile=file;
end

ext='msp';
% === put extension .msp by default ===
fpos=findstr(newfile,'.');
if isempty(fpos),	% no file extension, i.e. crystal_psd
   newfile=[deblank(newfile) '.' ext];
elseif isempty(deblank(newfile((fpos+1):end))),	%	i.e. crystal_psd. 
   newfile=[newfile ext];
elseif ~strcmp(lower(deblank(newfile((fpos+1):end))),ext),	% i.e. crystal.ms or other extensions different from .msp
   newfile=[deblank(newfile(1:(fpos-1))) '.' ext];
end

% === update name of Msp file in the variables list
set(h_file,'String',stripath(newfile));

% ==== get default .msp parameter file 
h_samp=findobj(h_cw,'Tag','ms_sample');
if isempty(h_samp),
   disp(['Sample type could not be determined. Parameter file not saved.']);
end
samp=get(h_samp,'Value');
if samp==1,
   % === single crystal sample
   h_analysis_mode=findobj(h_cw,'Tag','ms_analysis_mode');
   if isempty(h_analysis_mode),
      disp(['Could not determine analysis mode for single crystal data. Parameter file not saved.']);
      return;
   end
   analmode=get(h_analysis_mode,'Value');
   if analmode==1,
	   h_det_type=findobj(h_cw,'Tag','ms_det_type');
   	if isempty(h_det_type),
      	disp(['Could not determine whether PSD or non-PSD detectors. Parameter file not saved.']);
      	return;
   	end
   	det_type=get(h_det_type,'Value');
	   if det_type==1,
   	   % ==== PSD type detectors
      	DefaultMspFile='crystal_psd.msp';
   	elseif det_type==2,
      	% === non-PSD (conventional detectors)
      	DefaultMspFile='crystal_no_psd.msp';
   	else
      	disp(['Only PSD (det_type=1) and non-PSD (det_type=2) detector types allowed.']);
      	disp('Parameter file not saved');
      	return;
      end
   elseif analmode==2,
      % ==== single crystal sample analysed in powder mode
      DefaultMspFile='crystal_as_powder.msp';
   else
    	disp(['Only single-crystal- and powder-mode (analysis_mode=1,2) allowed.']);
     	disp('Parameter file not saved');
    	return;
	end      
elseif samp==2,
   % === powder sample 
	DefaultMspFile='powder.msp';   
else
   disp(['Wrong sample type. Only single crystal(1) and powder(2) allowed.']);
	return;   
end

default=[get(findobj('Tag','ms_MsliceDir'),'String') DefaultMspFile];

% ==== open both the default .msp file and new file as ASCII text files 
f1=fopen(default,'rt');
if (f1==-1),
   disp(['Error opening default parameter file ' default]);
   disp(['Parameter file not saved.']);
   return;
end

f2=fopen(newfile,'wt');
if (f2==-1),
   disp(['Error opening selected parameter file ' newfile]);
   disp(['Parameter file not saved.']);
   return;
end

% === write parameters line by line to the newfile mirroring structure of default file
t=fgetl(f1);	% read one line of the default file
while (ischar(t)&(~isempty(t(~isspace(t))))),	% until reaching the end of the defauilt file do ...
   pos=findstr(t,'=');
   field=t(1:pos-1);
   fieldname=field(~isspace(field));	% obtain true fieldname by removing white spaces from field
   h=findobj('Tag',['ms_' fieldname]);
   if isempty(h),
      disp(['Field ms_' fieldname ' not defined. Check ' default ' parameter file.']);
      fclose(f1);
      fclose(f2);
      return;
   end
   % if object is popupmenu or checkbox its value is stored in the 'Value' property, otherwise in 'String' 
   if strcmp(get(h,'Style'),'popupmenu')|strcmp(get(h,'Style'),'checkbox'),
      value=num2str(get(h,'Value'));
%   	disp(['ms_' field ' has ''Value'' property ' value]);   
   else
      value=get(h,'String');
      value=deblank(value);	% remove trailing blanks from both beginning and end
      value=fliplr(deblank(fliplr(value))); 
%      disp(['ms_' field ' has ''String'' property ' value]); 
   end 
   fprintf(f2,'%s%2s%s\n',field,'= ',value);
   t=fgetl(f1);
end
fclose(f1);
fclose(f2);
disp(['Saved parameters to file ' newfile]);