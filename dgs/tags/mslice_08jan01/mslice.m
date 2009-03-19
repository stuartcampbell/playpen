function mslice(MspFile);

% function mslice;
% draws the Control Window for the MSlice programme
% Radu Coldea 02-October-1999

% === find MSlice directory and place at top of MATLAB search path if not in path already
[temp,MSliceDir]=stripath(which('mslice'));
path(MSliceDir,path);

% === start by default in Crystal PSD mode if no MspFile given
if ~exist('MspFile','var')|isempty(MspFile)|~ischar(MspFile),
   MspFile=[MSliceDir 'crystal_psd.msp'];
else
   if isempty(findstr(MspFile,'.msp')),
      MspFile=[MspFile '.msp'];
   end
end

% === define colours in RGB format
colordef white;
white=[1 1 1];
black=[0 0 0];
grey=[0.752941 0.752941 0.752941];

% === close previous MSlice Control Windows
h=findobj('Tag','ms_ControlWindow');
if ~isempty(h),
   disp(['Closing previous MSlice Control Window'])
   delete(h);
end

% === display MSlice version ===
helpfile=[MSliceDir 'help.txt'];
fpos=ffind(helpfile,'%%% version');
if fpos<1,
   disp('Cannot determine MSlice version date.');
	lastupdate='(date of last update not available)';   
else
	fid=fopen(helpfile,'rt');
   fseek(fid,fpos+length('%%% version'),'bof');
   lastupdate=fgetl(fid);
   fclose(fid);
end
disp(['Starting MSlice version' lastupdate ]);
disp(['More information in the About MSlice... box.']);   

% === draw Control Window and establish positions and dimensions of menu items
fig=figure('MenuBar','none','Resize','off',...
'Position',[5    33   536   693],...
	'Name','MSlice : Control Window',...
   'NumberTitle','off',...
   'Tag','ms_ControlWindow');
h=uicontrol('Parent',fig,'Style','text','String','Spectrometer',...
   'BackgroundColor',white);
pos=get(h,'Extent');
lineheight=pos(4);
interlines=ceil(lineheight/5);
nlines=33.2;
width=8*pos(3);
bottomx=5;
bottomy=33;

set(fig,'Position',[bottomx bottomy width lineheight*nlines+interlines*(nlines-1)]);
pos=[0 lineheight*(nlines-1)+interlines*(nlines-1) pos(3) pos(4)];
set(h,'Position',pos); 
oneline=[0 lineheight+0*interlines 0 0];
pos=pos-[0 interlines 0 0];

% ============== MSliceDir, MspDir and MspFile 
h=uicontrol('Parent',fig,'Style','text','String',MSliceDir,...
   'Position',pos,'Tag','ms_MSliceDir','Visible','off');
h=uicontrol('Parent',fig,'Style','text','String','',...
   'Position',pos,'Tag','ms_MspDir','Visible','off');
h=uicontrol('Parent',fig,'Style','text','String','',...
   'Position',pos,'Tag','ms_MspFile','Visible','off');
green=[0 1 0];
red  =[1 0 0];
h=uicontrol('Parent',fig,'Style','frame',...
   'Position',pos+[7.5*pos(3) 0 -pos(3)/2 interlines],'Tag','ms_status',...
   'Visible','on','BackgroundColor',green,'UserData',[green; red]);
%========= efixed and emode =========
pos=pos-oneline;
h=uicontrol('Parent',fig,'Style','text','String','efixed (meV)',...
   'Position',pos);
h=uicontrol('Parent',fig,'Style','edit','Enable','on','Tag','ms_efixed',...
   'Position',pos+[pos(3) 0 0 interlines],...
   'HorizontalAlignment','left',...
	'BackgroundColor',white);
h=uicontrol('Parent',fig,'Style','popupmenu','String',{'direct','indirect'},...
   'Tag','ms_emode','BackgroundColor',white,...
	'Position',pos+[3*pos(3) interlines/2 0 interlines],'Value',2);
h=uicontrol('Parent',fig,'Style','text','String','geometry',...
   'Position',pos+[4*pos(3) 0 0 0]);   

%========= DataFile ==================
pos=pos-oneline;
h=uicontrol('Parent',fig,'Style','text','String',[pwd '\'],...
   'Position',pos,'Visible','off','Tag','ms_DataDir');
h=uicontrol('Parent',fig,'Style','text','String','DataFile(.spe)',...
   'Position',pos);
h=uicontrol('Parent',fig,'Style','edit','Enable','on','Tag','ms_DataFile',...
   'Position',pos+[pos(3) 0 2*pos(3) interlines],...
   'HorizontalAlignment','left',...
	'BackgroundColor',white);
h=uicontrol('Parent',fig,'Style','pushbutton','String','Browse',...
   'Callback',...
   'ms_getfile(findobj(''Tag'',''ms_DataDir''),findobj(''Tag'',''ms_DataFile''),''*.spe'',''Choose Data File'');',...
   'Position',pos+[4*pos(3) interlines/2 0 0]);   

%========= DetFile, Save Data As... and Load Data pushbuttons ==================
pos=pos-oneline;
h=uicontrol('Parent',fig,'Style','text','String','DetFile(.phx)',...
   'Position',pos);
h=uicontrol('Parent',fig,'Style','text','String',[pwd '\'],...
   'Position',pos,'Visible','off','Tag','ms_PhxDir');
h=uicontrol('Parent',fig,'Style','edit','Enable','on','Tag','ms_PhxFile',...
   'Position',pos+[pos(3) 0 2*pos(3) interlines],...
   'HorizontalAlignment','left',...
	'BackgroundColor',white);
h=uicontrol('Parent',fig,'Style','pushbutton','String','Browse',...
   'Callback',...
   'ms_getfile(findobj(''Tag'',''ms_PhxDir''),findobj(''Tag'',''ms_PhxFile''),''*.phx'',''Choose Detector File'');',...
   'Position',pos+[4*pos(3) interlines/2 0 0]);   
h=uicontrol('Parent',fig,'Style','pushbutton','String','Save Data As ...',...
   'Callback','ms_save_data;',...
   'Position',pos+[5.5*pos(3) interlines 0.5*pos(3) 0]);   
h=uicontrol('Parent',fig,'Style','pushbutton','String','Load Data',...
   'Callback','ms_load_data;',...
   'Position',pos+[7*pos(3) interlines 0 0]);   

%========= Intensity and title label =================
pos=pos-oneline;
h=uicontrol('Parent',fig,'Style','text','String','Intensity label',...
   'Position',pos);
h=uicontrol('Parent',fig,'Style','edit','Enable','on','Tag','ms_IntensityLabel',...
   'Position',pos+[pos(3) 0 2*pos(3) interlines],...
   'HorizontalAlignment','left',...
   'BackgroundColor',white);
h=uicontrol('Parent',fig,'Style','text','String','Title label*',...
   'Position',pos+[4*pos(3) 0 0 0]);
h=uicontrol('Parent',fig,'Style','edit','Enable','on','Tag','ms_TitleLabel',...
   'Position',pos+[5*pos(3) 0 2*pos(3) interlines],...
   'HorizontalAlignment','left',...
   'BackgroundColor',white);

%========= Sample :Single Crystal/Powder ==================

pos=pos-1.5*oneline;
h=uicontrol('Parent',fig,'Style','text','String','Sample',...
   'Position',pos,'BackgroundColor',white);
h=uicontrol('Parent',fig,'Style','popupmenu','String',{'single crystal','powder'},...
   'Tag','ms_sample','BackgroundColor',white,...
   'Position',pos+[pos(3) 0 0.5*pos(3) interlines],...
   'Callback','ms_sample','Value',1);

%========== Top Figure Menu ===========================
h=uimenu(fig,'Label','Exit');
h=uimenu(h,'Label','Exit','Callback','close(gcf);');

% === construct Parameters menu
h=uimenu(fig,'Label','Parameters');
h1=uimenu(h,'Label','Load Parameters','Callback','ms_load_msp;');
h1=uimenu(h,'Label','Save Parameters','Callback','ms_save_msp;');
h1=uimenu(h,'Label','List Parameters','Callback','ms_list_pars;');
h1=uimenu(h,'Label','Save List to File','Callback','ms_list_pars(''file'');');

% === construct Background menu
h=uimenu(fig,'Label','Background');
h1=uimenu(h,'Label','Subtract stored bkg(E) from data','Callback','ms_bkg(''subtract'');');
h2=uimenu(h,'Label','Add stored bkg(E) back to data','Callback','ms_bkg(''add'');');
h3=uimenu(h,'Label','Display history of stored bkg(E)','Callback','ms_bkg(''display'');');
h5=uimenu(h,'Label','Delete currently stored bkg(E)','Callback','ms_bkg(''delete'');');

%==== construct 'Help' menu
h=uimenu(fig,'Label','Help','Tag','ms_help');
% === for the moment do not enable help option on the VMS, problems reading the help file
a=version;
if (a(1)<=5)&isvms,
   set(h,'Visible','off');
end
% === the ControlWindow top menu
h1=uimenu(h,'Label','ControlWindow Top menu');
h2=uimenu(h1,'Label','Exit',...
   'Callback','ms_help(''Exit'');');
h2=uimenu(h1,'Label','Parameters',...
   'Callback','ms_help(''Parameters'');');
h2=uimenu(h1,'Label','Background',...
   'Callback','ms_help(''Background'');');
h2=uimenu(h1,'Label','Help',...
   'Callback','ms_help(''Help'');');
% === the Spectrometer menu
h1=uimenu(h,'Label','Spectrometer');
h2=uimenu(h1,'Label','efixed',...
   'Callback','ms_help(''efixed'');');
h2=uimenu(h1,'Label','geometry',...
   'Callback','ms_help(''geometry'');');
h2=uimenu(h1,'Label','DataFile(.spe)',...
   'Callback','ms_help(''DataFile'');');
h2=uimenu(h1,'Label','DataFile(.spe from IRIS)',...
   'Callback','ms_help(''DataFile(.spe from IRIS)'');');
h2=uimenu(h1,'Label','SaveDataAs...',...
   'Callback','ms_help(''SaveDataAs...'');');
h2=uimenu(h1,'Label','LoadData',...
   'Callback','ms_help(''LoadData'');');
h2=uimenu(h1,'Label','DetFile(.phx)',...
   'Callback','ms_help(''DetFile'');');
h2=uimenu(h1,'Label','IntensityLabel',...
   'Callback','ms_help(''IntensityLabel'');');
h2=uimenu(h1,'Label','TitleLabel',...
   'Callback','ms_help(''TitleLabel'');');

% === the Sample menu
h1=uimenu(h,'Label','Sample');
h2=uimenu(h1,'Label','general',...
   'Callback','ms_help(''Sample'');');
h2=uimenu(h1,'Label','Single Crystal');

% === the Analysis Mode/Detectors/Viewing axes for single crystal mode 
h3=uimenu(h2,'Label','Unit cell and orientation',...
   'Callback','ms_help(''Single crystal: unit cell and orientation'');');
h3=uimenu(h2,'Label','Analysis Mode',...
   'Callback','ms_help(''Analysis Mode'');');
h3=uimenu(h2,'Label','Detectors',...
   'Callback','ms_help(''Detectors'');');
h3=uimenu(h2,'Label','Viewing Axes',...
   'Callback','ms_help(''Viewing Axes: Single Crystal'');');

% === the Viewing axes for powder mode
h2=uimenu(h1,'Label','Powder');
h3=uimenu(h2,'Label','general',...
   'Callback','ms_help(''Powder'');');
h3=uimenu(h2,'Label','Viewing Axes',...
   'Callback','ms_help(''Viewing Axes: Powder'');');

% === the Calculate Projections button 
h1=uimenu(h,'Label','Calculate Projections', ...
   'Callback','ms_help(''Calculate Projections'');');

% === the Display menu
h1=uimenu(h,'Label','Display');
h2=uimenu(h1,'Label','general',...
	'Callback','ms_help(''Display'');');
h2=uimenu(h1,'Label','Intensity range',...
   'Callback','ms_help(''Display: Intensity range:'');');
h2=uimenu(h1,'Label','Colour map',...
   'Callback','ms_help(''Display: Colour map'');');
h2=uimenu(h1,'Label','Smoothing',...
   'Callback','ms_help(''Display: Smoothing level'');');
h2=uimenu(h1,'Label','Shading',...
   'Callback','ms_help(''Display: Shading'');');
h2=uimenu(h1,'Label','Display',...
   'Callback','ms_help(''Display: Display'');');	

% === the Slice menu
h1=uimenu(h,'Label','Slice');
h2=uimenu(h1,'Label','general',...
	'Callback','ms_help(''Slice plane'');');
h2=uimenu(h1,'Label','Intensity range',...
   'Callback','ms_help(''Slice plane: Intensity range:'');');
h2=uimenu(h1,'Label','Colour map',...
   'Callback','ms_help(''Slice plane: Colour map'');');
h2=uimenu(h1,'Label','Smoothing',...
   'Callback','ms_help(''Slice plane: Smoothing level'');');
h2=uimenu(h1,'Label','Shading',...
   'Callback','ms_help(''Slice plane: Shading'');');
h2=uimenu(h1,'Label','Plot Slice',...
   'Callback','ms_help(''Slice plane: Plot Slice'');');	
h2=uimenu(h1,'Label','Surf Slice', ...
   'Callback','ms_help(''Slice plane: Surf Slice'');');

% === the Cut menu
h1=uimenu(h,'Label','Cut');
h2=uimenu(h1,'Label','general',...
   'Callback','ms_help(''Cut'');');
h2=uimenu(h1,'Label','Intensity range',...
   'Callback','ms_help(''Cut: Intensity range'');');
h2=uimenu(h1,'Label','Symbol', ...
   'Callback','ms_help(''Cut: Symbol'');');
h2=uimenu(h1,'Label','OutputFile', ...
   'Callback','ms_help(''Cut: OutputFile'');');
h2=uimenu(h1,'Label','To Mfit', ...
   'Callback','ms_help(''Cut: To Mfit'');');
h2=uimenu(h1,'Label','Store bkg(E)', ...
   'Callback','ms_help(''Cut: Store bkg(E)'');');
h2=uimenu(h1,'Label','x-axis', ...
   'Callback','ms_help(''Cut: x-axis:'');');
h2=uimenu(h1,'Label','Plot Cut', ...
   'Callback','ms_help(''Cut: Plot Cut:'');');
h2=uimenu(h1,'Label','Plot Cut Over', ...
   'Callback','ms_help(''Cut: Plot Cut Over'');');

% === the Detector Trajectories menu
h1=uimenu(h,'Label','Detector Trajectories',...
   'Callback','ms_help(''Detector Trajectories'');');

% === the Command Line Operations menu
h1=uimenu(h,'Label','Command line operations');
h2=uimenu(h1,'Label','Adding files',...
   'Callback','ms_help(''Adding .spe files'');');
h2=uimenu(h1,'Label','More files in memory simultaneously',...
   'Callback','ms_help(''More files in memory simultaneously'');');
h2=uimenu(h1,'Label','Saving data in binary .mat format','Callback',...
   'ms_help(''Saving data in binary .mat format'');');
h2=uimenu(h1,'Label','Masking detectors','Callback',...
   'ms_help(''Masking detectors'');');
h2=uimenu(h1,'Label','Simulating scattering','Callback',...
   'ms_help(''Simulating scattering'');');
h2=uimenu(h1,'Label','Subtracting backgrounds',...
   'Callback','ms_help(''Subtracting backgrounds'');');
h2=uimenu(h1,'Label','Displaying 3d/4d data from other sources','Callback',...
   'ms_help(''Displaying 3d/4d data from other sources'');');

% === the Useful Software menu
h1=uimenu(h,'Label','Useful Software');
h2=uimenu(h1,'Label','Editor for PC',...
   'Callback','ms_help(''Editor for PC'');');
h2=uimenu(h1,'Label','FTP for PC',...
   'Callback','ms_help(''FTP for PC'');');
h2=uimenu(h1,'Label','Mfit',...
   'Callback','ms_help(''Mfit fitting programme for MATLAB'');');


%========== Top Menu : About ==========================
hmenu=uimenu(fig,'Label','About MSlice ...');
hmenu1=uimenu(hmenu,'Label',['MSlice version' lastupdate]);
hmenu1=uimenu(hmenu,'Label','Matlab Visualisation Software');
hmenu1=uimenu(hmenu,'Label','for Single Crystal and Powder Time-of-Flight Neutron Data');
hmenu1=uimenu(hmenu,'Label','written by Radu Coldea @1998-2001');
hmenu1=uimenu(hmenu,'Label','           Oak Ridge National Labortatory, USA');
hmenu1=uimenu(hmenu,'Label','           and ISIS Facility, Rutherford Appleton Laboratory, UK');
hmenu1=uimenu(hmenu,'Label','           email : r.coldea@rl.ac.uk');

% =========== Draw sample and viewing parameters menu options ===========
% =========== different for powder and single crystal samples ===
ms_sample;

% ======= Load parameter (.msp) file ==============
ms_load_msp(MspFile);











  