function varargout = Homer_ver0(varargin)
% HOMER_VER0 M-file for Homer_ver0.fig
%      HOMER_VER0, by itself, creates a new HOMER_VER0 or raises the existing
%      singleton*.
%
%      H = HOMER_VER0 returns the handle to a new HOMER_VER0 or the handle to
%      the existing singleton*.
%
%      HOMER_VER0('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in HOMER_VER0.M with the given input arguments.
%
%      HOMER_VER0('Property','Value',...) creates a new HOMER_VER0 or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before Homer_ver0_OpeningFunction gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to Homer_ver0_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Edit the above text to modify the response to help Homer_ver0

% Last Modified by GUIDE v2.5 26-Nov-2008 14:08:47

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
    'gui_Singleton',  gui_Singleton, ...
    'gui_OpeningFcn', @Homer_ver0_OpeningFcn, ...
    'gui_OutputFcn',  @Homer_ver0_OutputFcn, ...
    'gui_LayoutFcn',  [] , ...
    'gui_Callback',   []);
if nargin && ischar(varargin{1})
    gui_State.gui_Callback = str2func(varargin{1});
end

if nargout
    [varargout{1:nargout}] = gui_mainfcn(gui_State, varargin{:});
else
    gui_mainfcn(gui_State, varargin{:});
end
% End initialization code - DO NOT EDIT


% --- Executes just before Homer_ver0 is made visible.
function Homer_ver0_OpeningFcn(hObject, eventdata, handles, varargin)
% This function has no output args, see OutputFcn.
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% varargin   command line arguments to Homer_ver0 (see VARARGIN)

% Choose default command line output for Homer_ver0

%addpath('DLL')
handles.output = hObject;

% Update handles structure
guidata(hObject, handles);

% UIWAIT makes Homer_ver0 wait for user response (see UIRESUME)
% uiwait(handles.figure1);


% --- Outputs from this function are returned to the command line.
function varargout = Homer_ver0_OutputFcn(hObject, eventdata, handles)
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Get default command line output from handles structure
varargout{1} = handles.output;


% --- Executes on selection change in popupmenu1.
function popupmenu1_Callback(hObject, eventdata, handles)
% hObject    handle to popupmenu1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = get(hObject,'String') returns popupmenu1 contents as cell array
%        contents{get(hObject,'Value')} returns selected item from popupmenu1

get(hObject,'Value')
% --- Executes during object creation, after setting all properties.
function popupmenu1_CreateFcn(hObject, eventdata, handles)
% hObject    handle to popupmenu1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end




% --------------------------------------------------------------------
function MARI_Callback(hObject, eventdata, handles)
% hObject    handle to MARI (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

%set mari defaults
disp('MARI defaults selected')
indat=inst_defs('mar');
set(handles.edit30,'string',indat.hardmask);
set(handles.edit15,'string',indat.mv_mapfile);
set(handles.edit8,'string',indat.map_file);
set(handles.radiobutton6,'value',indat.sum_files);
set(handles.edit65,'string',indat.mon_mapfile)
set(handles.popupmenu3,'value',1);
set(handles.popupmenu5,'value',indat.mv_mask);

set(handles.edit23,'string',num2str(indat.VLOW));
set(handles.edit24,'string',num2str(indat.VHIGH));
set(handles.edit25,'string',num2str(indat.FACTOR));
set(handles.edit26,'string',num2str(00000));
set(handles.edit27,'string',num2str(indat.background_default(1)));
set(handles.edit28,'string',num2str(indat.background_default(2)));
set(handles.radiobutton10,'value',1);
set(handles.radiobutton21,'value',1);%setting display for noback
set(handles.edit12,'string',num2str(indat.mono_van_int_lim(1)));
set(handles.edit14,'string',num2str(indat.mono_van_int_lim(2)));
set(handles.edit20,'string',num2str(indat.samp_mass));
set(handles.edit22,'string',num2str(indat.samp_rmm));
set(handles.edit66,'string',num2str(indat.vmass));
set(handles.text2,'string','MARI');
set(handles.text2,'BackgroundColor','b');
handles.indat=indat;

%%setup paths
setup_mari_inst(indat.datadir,indat.mapdir,indat.paramdir,indat.maskdir);

disp('Paths set');
guidata(gcbo,handles);
% --------------------------------------------------------------------
function MAPS_Callback(hObject, eventdata, handles)
% hObject    handle to MAPS (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

%set maps defaults
disp('MAPS defaults selected')

%get defaults
indat=inst_defs('maps');

set(handles.edit30,'string',indat.hardmask)
set(handles.edit15,'string',indat.mv_mapfile)
set(handles.edit8,'string',indat.map_file)
set(handles.edit65,'string',indat.mon_mapfile)
set(handles.radiobutton6,'value',indat.sum_files);

set(handles.popupmenu3,'value',1);
set(handles.popupmenu5,'value',indat.mv_mask);

set(handles.edit23,'string',num2str(indat.VLOW));
set(handles.edit24,'string',num2str(indat.VHIGH));
set(handles.edit25,'string',num2str(indat.FACTOR));
set(handles.edit26,'string',num2str(indat.STAB));
set(handles.edit27,'string',num2str(indat.background_default(1)));
set(handles.edit28,'string',num2str(indat.background_default(2)));
set(handles.radiobutton10,'value',1);
set(handles.radiobutton21,'value',1);%setting display for noback
set(handles.edit12,'string',num2str(indat.mono_van_int_lim(1)));
set(handles.edit14,'string',num2str(indat.mono_van_int_lim(2)));
set(handles.edit20,'string',num2str(indat.samp_mass));
set(handles.edit22,'string',num2str(indat.samp_rmm));
set(handles.edit66,'string',num2str(indat.vmass));
set(handles.text2,'string','MAPS');
set(handles.text2,'BackgroundColor','g');
handles.indat=indat;

%%set up the paths
setup_maps_inst(indat.datadir,indat.mapdir,indat.paramdir,indat.maskdir);

disp('Paths set')
guidata(gcbo,handles);
% --------------------------------------------------------------------
function MERLIN_Callback(hObject, eventdata, handles)
% hObject    handle to MERLIN (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

%set maps defaults
disp('MERLIN defaults selected')

%get defaults
indat=inst_defs('merlin');

set(handles.edit30,'string',indat.hardmask)
set(handles.edit15,'string',indat.mv_mapfile)
set(handles.edit8,'string',indat.map_file)

set(handles.radiobutton6,'value',indat.sum_files);
set(handles.edit65,'string',indat.mon_mapfile)
set(handles.popupmenu3,'value',1);
set(handles.popupmenu5,'value',indat.mv_mask);

set(handles.edit23,'string',num2str(indat.VLOW));
set(handles.edit24,'string',num2str(indat.VHIGH));
set(handles.edit25,'string',num2str(indat.FACTOR));
set(handles.edit26,'string',num2str(indat.STAB));
set(handles.edit27,'string',num2str(indat.background_default(1)));
set(handles.edit28,'string',num2str(indat.background_default(2)));
set(handles.radiobutton10,'value',1);
set(handles.radiobutton21,'value',1);%setting display for noback
set(handles.edit12,'string',num2str(indat.mono_van_int_lim(1)));
set(handles.edit14,'string',num2str(indat.mono_van_int_lim(2)));
set(handles.edit20,'string',num2str(indat.samp_mass));
set(handles.edit22,'string',num2str(indat.samp_rmm));
set(handles.edit66,'string',num2str(indat.vmass));
set(handles.text2,'string','MERLIN');
set(handles.text2,'BackgroundColor','r');
handles.indat=indat;

%%set up the paths
setup_merlin_inst(indat.datadir,indat.mapdir,indat.paramdir,indat.maskdir);

disp('Paths set')
guidata(gcbo,handles);
% --------------------------------------------------------------------
function HET_Callback(hObject, eventdata, handles)
% hObject    handle to HET (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
%set maps defaults
disp('HET defaults selected')

%get defaults
indat=inst_defs('het');

set(handles.edit30,'string',indat.hardmask)
set(handles.edit15,'string',indat.mv_mapfile)
set(handles.edit8,'string',indat.map_file)

set(handles.radiobutton6,'value',indat.sum_files);
set(handles.edit65,'string',indat.mon_mapfile)
set(handles.popupmenu3,'value',1);
set(handles.popupmenu5,'value',indat.mv_mask);

set(handles.edit23,'string',num2str(indat.VLOW));
set(handles.edit24,'string',num2str(indat.VHIGH));
set(handles.edit25,'string',num2str(indat.FACTOR));
set(handles.edit26,'string',num2str(indat.STAB));
set(handles.edit27,'string',num2str(indat.background_default(1)));
set(handles.edit28,'string',num2str(indat.background_default(2)));
set(handles.radiobutton10,'value',1);
set(handles.radiobutton21,'value',1);%setting display for noback
set(handles.edit12,'string',num2str(indat.mono_van_int_lim(1)));
set(handles.edit14,'string',num2str(indat.mono_van_int_lim(2)));
set(handles.edit20,'string',num2str(indat.samp_mass));
set(handles.edit22,'string',num2str(indat.samp_rmm));
set(handles.text2,'string','HET');
set(handles.text2,'BackgroundColor','r');
handles.indat=indat;

%%set up the paths
setup_het_inst(indat.datadir,indat.mapdir,indat.paramdir,indat.maskdir);

disp('Paths set')
guidata(gcbo,handles);

% --------------------------------------------------------------------
function Untitled_1_Callback(hObject, eventdata, handles)
% hObject    handle to Untitled_1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --- Executes on button press in pushbutton1.
function pushbutton1_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
if ~isfield(handles,'indat') || isempty(handles.indat)
    warning('Please select instrument first')
else
set(handles.uipanel3, 'Visible', 'on');
set(handles.uipanel5, 'Visible', 'off');
set(handles.uipanel7, 'Visible', 'off');
set(handles.uipanel51, 'Visible', 'off');
end
% --- Executes on button press in pushbutton2.
function pushbutton2_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
if ~isfield(handles,'indat') || isempty(handles.indat)
    warning('Please select instrument first')
else
set(handles.uipanel3, 'Visible', 'off');
set(handles.uipanel5, 'Visible', 'on');
set(handles.uipanel7, 'Visible', 'off');
set(handles.uipanel51, 'Visible', 'off');
end

% --- Executes on button press in pushbutton3.
function pushbutton3_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
if ~isfield(handles,'indat') || isempty(handles.indat)
    warning('Please select instrument first')
else
set(handles.uipanel3, 'Visible', 'off');
set(handles.uipanel5, 'Visible', 'off');
set(handles.uipanel7, 'Visible', 'on');
set(handles.uipanel51, 'Visible', 'off');
end
%%
function edit1_Callback(hObject, eventdata, handles)
% hObject    handle to edit1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit1 as text
%        str2double(get(hObject,'String')) returns contents of edit1 as a double
string=get(handles.edit1,'string');
%send the string to the parser
out=parse_homer(string);
handles.indat.white_file=out;
guidata(gcbo,handles);
% --- Executes during object creation, after setting all properties.
function edit1_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end

%%
function edit2_Callback(hObject, eventdata, handles)
% hObject    handle to edit2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit2 as text
%        str2double(get(hObject,'String')) returns contents of edit2 as a double
string=get(handles.edit2,'string');
out=parse_homer(string);

handles.indat.run_num=out;

guidata(gcbo,handles);
% --- Executes during object creation, after setting all properties.
function edit2_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in pushbutton5.
function pushbutton5_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton5 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)



function edit3_Callback(hObject, eventdata, handles)
% hObject    handle to edit3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit3 as text
%        str2double(get(hObject,'String')) returns contents of edit3 as a double


% --- Executes during object creation, after setting all properties.
function edit3_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function edit4_Callback(hObject, eventdata, handles)
% hObject    handle to edit4 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit4 as text
%        str2double(get(hObject,'String')) returns contents of edit4 as a double


% --- Executes during object creation, after setting all properties.
function edit4_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit4 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function edit5_Callback(hObject, eventdata, handles)
% hObject    handle to edit5 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit5 as text
%        str2double(get(hObject,'String')) returns contents of edit5 as a double


% --- Executes during object creation, after setting all properties.
function edit5_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit5 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function edit6_Callback(hObject, eventdata, handles)
% hObject    handle to edit6 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit6 as text
%        str2double(get(hObject,'String')) returns contents of edit6 as a double


% --- Executes during object creation, after setting all properties.
function edit6_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit6 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in pushbutton6.
function pushbutton6_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton6 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)



function edit7_Callback(hObject, eventdata, handles)
% hObject    handle to edit7 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit7 as text
%        str2double(get(hObject,'String')) returns contents of edit7 as a double


% --- Executes during object creation, after setting all properties.
function edit7_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit7 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in pushbutton7.
function pushbutton7_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton7 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)



function edit8_Callback(hObject, eventdata, handles)
% hObject    handle to edit8 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit8 as text
%        str2double(get(hObject,'String')) returns contents of edit8 as a double

%string=get(handles.edit8,'string');
%handles.indat.map_file=string;
%handles.indat.diag_detmap=string;
%guidata(gcbo,handles)

% --- Executes during object creation, after setting all properties.
function edit8_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit8 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in pushbutton8.
function pushbutton8_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton8 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
currentdir=pwd;
cd (handles.indat.mapdir);
[file, path, filter] = uigetfile('*.map', 'Select mapping file');
cd(currentdir);
if ~isnumeric(file)
    set(handles.edit8,'string',file)
    % if mapdirectory is different from that set, then add it to list
    if ~strcmp(handles.indat.mapdir,path)
        addtogpath('inst_maps',path);
    end
    handles.indat.mapdir=path;
    handles.indat.map_file=file;
end
guidata(gcbo,handles)
% --- Executes on button press in radiobutton6.
function radiobutton6_Callback(hObject, eventdata, handles)
% hObject    handle to radiobutton6 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of radiobutton6

handles.indat.sum_files=get(handles.radiobutton6,'value');
guidata(gcbo,handles)

function edit10_Callback(hObject, eventdata, handles)
% hObject    handle to edit10 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit10 as text
%        str2double(get(hObject,'String')) returns contents of edit10 as a double
string=get(handles.edit10,'string');
out=parse_homer(string);

handles.indat.run_num_mono=out;
guidata(gcbo,handles);

% --- Executes during object creation, after setting all properties.
function edit10_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit10 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function edit11_Callback(hObject, eventdata, handles)
% hObject    handle to edit11 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit11 as text
%        str2double(get(hObject,'String')) returns contents of edit11 as a double


% --- Executes during object creation, after setting all properties.
function edit11_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit11 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function edit12_Callback(hObject, eventdata, handles)
% hObject    handle to edit12 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit12 as text
%        str2double(get(hObject,'String')) returns contents of edit12 as a double
handles.indat.mono_van_int_lim(1)=str2num(get(handles.edit12,'string'));
guidata(gcbo,handles);

% --- Executes during object creation, after setting all properties.
function edit12_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit12 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function edit13_Callback(hObject, eventdata, handles)
% hObject    handle to edit13 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit13 as text
%        str2double(get(hObject,'String')) returns contents of edit13 as a double


% --- Executes during object creation, after setting all properties.
function edit13_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit13 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function edit14_Callback(hObject, eventdata, handles)
% hObject    handle to edit14 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit14 as text
%        str2double(get(hObject,'String')) returns contents of edit14 as a double
handles.indat.mono_van_int_lim(2)=str2num(get(handles.edit14,'string'));
guidata(gcbo,handles);

% --- Executes during object creation, after setting all properties.
function edit14_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit14 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function edit15_Callback(hObject, eventdata, handles)
% hObject    handle to edit15 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit15 as text
%        str2double(get(hObject,'String')) returns contents of edit15 as a double
currentdir=pwd;
cd (handles.indat.mapdir);
[file, path, filter] = uigetfile('*.map', 'Select mapping file');
cd(currentdir);
if ~isnumeric(file)
    set(handles.edit15,'string',file)
    % if mapdirectory is different from that set, then add it to list
    if ~strcmp(handles.indat.mapdir,path)
        addtogpath('inst_maps',path);
    end
    handles.indat.mapdir=path;
    handles.indat.mv_mapfile=file;
end
guidata(gcbo,handles)

% --- Executes during object creation, after setting all properties.
function edit15_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit15 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in pushbutton9.
function pushbutton9_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton9 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
currentdir=pwd;
cd (handles.indat.mapdir);
[file, path, filter] = uigetfile('*.map', 'Select mapping file');
cd(currentdir);
if ~isnumeric(file)
    set(handles.edit15,'string',file)
    set(handles.edit8,'string',file)
    % if mapdirectory is different from that set, then add it to list
    if ~strcmp(handles.indat.mapdir,path)
        addtogpath('inst_maps',path);
    end
    handles.indat.mapdir=path;
    handles.indat.map_file=file;
end

guidata(gcbo,handles)

% --- Executes on button press in radiobutton8.
function radiobutton8_Callback(hObject, eventdata, handles)
% hObject    handle to radiobutton8 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of radiobutton8
handles.indat.do_absolute=(get(handles.radiobutton8,'value'));
guidata(gcbo,handles);
% --------------------------------------------------------------------
function Untitled_3_Callback(hObject, eventdata, handles)
% hObject    handle to Untitled_3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
[file, path] = uiputfile('*.mat', 'Save parameters as');
savefile=strcat(path,file);
if ~isnumeric(file)
    out=handles.indat;
    save(savefile, 'out')
end

% --------------------------------------------------------------------
function Untitled_4_Callback(hObject, eventdata, handles)
% hObject    handle to Untitled_4 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
[file, path] = uigetfile('*.mat', 'load parameters from');
if ~isnumeric(file)
    infile=strcat(path,file);
    indat=load(infile);
    indat=indat.out;
    set(handles.edit23,'string',num2str(indat.VLOW));
    set(handles.edit24,'string',num2str(indat.VHIGH));
    set(handles.edit25,'string',num2str(indat.FACTOR));
    set(handles.edit26,'string',num2str(00000));
    set(handles.edit27,'string',num2str(indat.background_default(1)));
    set(handles.edit28,'string',num2str(indat.background_default(2)));
    if length(indat.white_file(:,1))>1
        files='';
        for i=1:length(indat.white_file(:,1))
            files=strcat(files,indat.white_file(i,:),',');
        end
        set(handles.edit1,'string',(files));
    else
        set(handles.edit1,'string',(indat.white_file));
    end
    if length(indat.run_num(:,1))>1
        files='';
        for i=1:length(indat.run_num(:,1));
            files=strcat(files,indat.run_num(i,:),',');
        end
        set(handles.edit2,'string',(files));
    else
        set(handles.edit2,'string',(indat.run_num));
    end
    set(handles.edit8,'string',(indat.map_file));
    set(handles.edit16,'string',num2str(indat.ei_init));
    set(handles.edit17,'string',num2str(indat.spe_rebin_lims(1)));
    set(handles.edit18,'string',num2str(indat.spe_rebin_lims(2)));
    set(handles.edit19,'string',num2str(indat.spe_rebin_lims(3)));
    %absloute pane
    set(handles.edit10,'string',(indat.run_num_mono));
    set(handles.edit29,'string',(indat.white_file_mono));
    set(handles.edit15,'string',(indat.mv_mapfile));
    set(handles.edit11,'string',num2str(indat.ei_init));
    set(handles.edit12,'string',num2str(indat.mono_van_int_lim(1)));
    set(handles.edit14,'string',num2str(indat.mono_van_int_lim(2)));
    set(handles.edit20,'string',num2str(indat.samp_mass));
    set(handles.edit22,'string',num2str(indat.samp_rmm));
    set(handles.radiobutton8,'value',indat.do_absolute);
    handles.indat=indat;
end
guidata(gcbo,handles)
% --------------------------------------------------------------------
function Untitled_5_Callback(hObject, eventdata, handles)
% hObject    handle to Untitled_5 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

path=uigetdir(handles.indat.datadir, 'Select data directory');
if ~isnumeric(path)
    path=strcat(path,'\');
    if ~strcmp(handles.indat.datadir,path)
        addtogpath('inst_data',path);
    end
    handles.indat.datadir=path;
else
    disp('keeping current path for data')
end

path=uigetdir(handles.indat.spedir, 'Select directory for SPE files');
if ~isnumeric(path)
    handles.indat.spedir=strcat(path,'\');
else
    disp('keeping current path for SPE data')
end

path=uigetdir(handles.indat.mapdir,'Select map file directory');

if ~isnumeric(path)
    path=strcat(path,'\');
    if ~strcmp(handles.indat.mapdir,path)
        addtogpath('inst_maps',path);
    end
    handles.indat.mapdir=path;
else
    disp('keeping current path for map files')
end

path=uigetdir(handles.indat.paramdir, 'Select parameter file directory');
if ~isnumeric(path)
    path=strcat(path,'\');
    if ~strcmp(handles.indat.paramdir,path)
        addtogpath('inst_nxs',path);
    end
    handles.indat.paramdir=path;
else
    disp('keeping current path for parameter files')
end
guidata(gcbo,handles)
% --------------------------------------------------------------------
function Untitled_2_Callback(hObject, eventdata, handles)
% hObject    handle to Untitled_2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)



function edit16_Callback(hObject, eventdata, handles)
% hObject    handle to edit16 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit16 as text
%        str2double(get(hObject,'String')) returns contents of edit16 as a double
handles.indat.ei_init=str2num(get(handles.edit16,'string'));
set(handles.edit11,'string',num2str(handles.indat.ei_init));
guidata(gcbo,handles);

% --- Executes during object creation, after setting all properties.
function edit16_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit16 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function edit17_Callback(hObject, eventdata, handles)
% hObject    handle to edit17 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit17 as text
%        str2double(get(hObject,'String')) returns contents of edit17 as a double
handles.indat.spe_rebin_lims(1)=str2num(get(handles.edit17,'string'));
guidata(gcbo,handles);
% --- Executes during object creation, after setting all properties.
function edit17_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit17 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function edit18_Callback(hObject, eventdata, handles)
% hObject    handle to edit18 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit18 as text
%        str2double(get(hObject,'String')) returns contents of edit18 as a double
handles.indat.spe_rebin_lims(2)=str2num(get(handles.edit18,'string'));
guidata(gcbo,handles);
% --- Executes during object creation, after setting all properties.
function edit18_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit18 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function edit19_Callback(hObject, eventdata, handles)
% hObject    handle to edit19 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit19 as text
%        str2double(get(hObject,'String')) returns contents of edit19 as a double
handles.indat.spe_rebin_lims(3)=str2num(get(handles.edit19,'string'));
guidata(gcbo,handles);

% --- Executes during object creation, after setting all properties.
function edit19_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit19 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on key press with focus on pushbutton3 and no controls selected.
function pushbutton3_KeyPressFcn(hObject, eventdata, handles)
% hObject    handle to pushbutton3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)



function edit20_Callback(hObject, eventdata, handles)
% hObject    handle to edit20 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit20 as text
%        str2double(get(hObject,'String')) returns contents of edit20 as a double
handles.indat.samp_mass=str2num(get(handles.edit20,'string'));
guidata(gcbo,handles);


% --- Executes during object creation, after setting all properties.
function edit20_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit20 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function edit22_Callback(hObject, eventdata, handles)
% hObject    handle to edit22 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit22 as text
%        str2double(get(hObject,'String')) returns contents of edit22 as a double
handles.indat.samp_rmm=str2num(get(handles.edit22,'string'));
guidata(gcbo,handles);


% --- Executes during object creation, after setting all properties.
function edit22_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit22 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns \\data.isis.rl.ac.uk\mar11060

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



% Hint: get(hObject,'Value') returns toggle state of radiobutton9
% --- Executes on button press in radiobutton7.
function radiobutton9_Callback(hObject, eventdata, handles)
% hObject    handle to radiobutton7 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of radiobutton7

handles.indat.mv_sum_files=get(handles.radiobutton9,'value');
guidata(gcbo,handles)

function edit23_Callback(hObject, eventdata, handles)
% hObject    handle to edit23 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit23 as text
%        str2double(get(hObject,'String')) returns contents of edit23 as a double
handles.indat.VLOW=str2num(get(handles.edit23,'string'));
guidata(gcbo,handles);

% --- Executes during object creation, after setting all properties.
function edit23_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit23 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function edit24_Callback(hObject, eventdata, handles)
% hObject    handle to edit24 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit24 as text
%        str2double(get(hObject,'String')) returns contents of edit24 as a double
handles.indat.VHIGH=str2num(get(handles.edit24,'string'));
guidata(gcbo,handles);

% --- Executes during object creation, after setting all properties.
function edit24_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit24 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function edit25_Callback(hObject, eventdata, handles)
% hObject    handle to edit25 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit25 as text
%        str2double(get(hObject,'String')) returns contents of edit25 as a double
handles.indat.FACTOR=str2num(get(handles.edit25,'string'));
guidata(gcbo,handles);

% --- Executes during object creation, after setting all properties.
function edit25_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit25 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function edit26_Callback(hObject, eventdata, handles)
% hObject    handle to edit26 (see GCBO)handles.indat.fixei
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit26 as text
%        str2double(get(hObject,'String')) returns contents of edit26 as a double
handles.indat.STAB=str2num(get(handles.edit26,'string'));
guidata(gcbo,handles);

% --- Executes during object creation, after setting all properties.
function edit26_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit26 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in radiobutton10.
function radiobutton10_Callback(hObject, eventdata, handles)
% hObject    handle to radiobutton10 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of radiobutton10

value=(get(handles.radiobutton10,'value'));
if value == 0
    handles.indat.ZERO_BKGD=false;
else
    handles.indat.ZERO_BKGD=true;
end
guidata(gcbo,handles);

function edit27_Callback(hObject, eventdata, handles)
% hObject    handle to edit27 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit27 as text
%        str2double(get(hObject,'String')) returns contents of edit27 as a double
handles.indat.background_default(1)=str2num(get(handles.edit27,'string'));
guidata(gcbo,handles);

% --- Executes during object creation, after setting all properties.
function edit27_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit27 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function edit28_Callback(hObject, eventdata, handles)
% hObject    handle to edit28 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit28 as text
%        str2double(get(hObject,'String')) returns contents of edit28 as a double
handles.indat.background_default(2)=str2num(get(handles.edit28,'string'));
guidata(gcbo,handles);

% --- Executes during object creation, after setting all properties.
function edit28_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit28 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in radiobutton11.
function radiobutton11_Callback(hObject, eventdata, handles)
% hObject    handle to radiobutton11 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of radiobutton11

value=(get(handles.radiobutton11,'value'));
if value == 0
    handles.indat.energy_method='ei';
else
    handles.indat.energy_method='fixei';
end
guidata(gcbo,handles);
% --------------------------------------------------------------------
function File_menu_Callback(hObject, eventdata, handles)
% hObject    handle to File_menu (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)





function edit29_Callback(hObject, eventdata, handles)
% hObject    handle to edit29 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit29 as text
%        str2double(get(hObject,'String')) returns contents of edit29 as a double
string=get(handles.edit29,'string');
out=parse_homer(string);

handles.indat.white_file_mono=out;
guidata(gcbo,handles);

% --- Executes during object creation, after setting all properties.
function edit29_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit29 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end




% --- Executes during object creation, after setting all properties.
function text2_CreateFcn(hObject, eventdata, handles)
% hObject    handle to text2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called





function edit30_Callback(hObject, eventdata, handles)
% hObject    handle to edit30 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit30 as text
%        str2double(get(hObject,'String')) returns contents of edit30 as a double

handles.indat.hardmask=(get(handles.edit30,'string'));
guidata(gcbo,handles);

% --- Executes during object creation, after setting all properties.
function edit30_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit30 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in pushbutton10.
function pushbutton10_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton10 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
currentdir=pwd;
cd(handles.indat.maskdir)
[file, path, filter] = uigetfile('*.msk', 'Select hard mask file');
cd(currentdir);

if ~isnumeric(file)
    handles.indat.hardmask=strcat(path,file);
    set(handles.edit30,'string',file)
    % if mapdirectory is different from that set, then add it to list
    if ~strcmp(handles.indat.maskdir,path)
        addtogpath('inst_masks',path);
    end
    handles.indat.maskdir=path;
    handles.indat.hardmask=file;
end
guidata(gcbo,handles)

% --- Executes on selection change in popupmenu3.
function popupmenu3_Callback(hObject, eventdata, handles)
% hObject    handle to popupmenu3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = get(hObject,'String') returns popupmenu3 contents as cell array
%        contents{get(hObject,'Value')} returns selected item from popupmenu3
value= get(handles.popupmenu3, 'Value');

if value == 1;
    handles.indat.norm_method=1;
    handles.indat.range=[1000,2000];
end
if value == 2;
    handles.indat.norm_method='uahr';
    handles.indat.range=[];
end
if value == 3;
    handles.indat.norm_method=2;
    handles.indat.range='peak';
end

if value == 4;
    handles.indat.norm_method=0;
    handles.indat.range='nonorm';
end
handles.indat;
guidata(gcbo,handles);
% --- Executes during object creation, after setting all properties.
function popupmenu3_CreateFcn(hObject, eventdata, handles)
% hObject    handle to popupmenu3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end




% --- Executes on button press in radiobutton14.
function radiobutton14_Callback(hObject, eventdata, handles)
% hObject    handle to radiobutton14 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of radiobutton14





function edit39_Callback(hObject, eventdata, handles)
% hObject    handle to edit39 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit39 as text
%        str2double(get(hObject,'String')) returns contents of edit39 as a double


% --- Executes during object creation, after setting all properties.
function edit39_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit39 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function edit44_Callback(hObject, eventdata, handles)
% hObject    handle to edit44 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit44 as text
%        str2double(get(hObject,'String')) returns contents of edit44 as a double


% --- Executes during object creation, after setting all properties.
function edit44_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit44 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function edit45_Callback(hObject, eventdata, handles)
% hObject    handle to edit45 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit45 as text
%        str2double(get(hObject,'String')) returns contents of edit45 as a double


% --- Executes during object creation, after setting all properties.
function edit45_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit45 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in pushbutton13.
function pushbutton13_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton13 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
set(handles.uipanel3, 'Visible', 'off');
set(handles.uipanel5, 'Visible', 'off');
set(handles.uipanel7, 'Visible', 'off');
set(handles.uipanel51, 'Visible', 'on');



% --- Executes on button press in pushbutton14.
function pushbutton14_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton14 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
string=get(handles.edit39,'string');
handles.indat.utils.fname=parse_homer(string);
handles.indat.utils.intlow=str2num(get(handles.edit44,'string'));
handles.indat.utils.inthigh=str2num(get(handles.edit45,'string'));
if isempty(handles.indat.utils.inthigh)
    handles.indat.utils.inthigh=19000;
end
if isempty(handles.indat.utils.intlow)
    handles.indat.utils.intlow=10;
end

indat=handles.indat;
ei_wb='white';
DSOw=setup_DSO_bank(indat.inst);
DSOw =add_diffinst(DSOw);
DSOw=add_rawfile(DSOw,strcat('inst_data:::',strcat(indat.inst,handles.indat.utils.fname),'.RAW'));

cellout = pre_parse(2,'ei',ei_wb,'det_units','$t','d_int',[handles.indat.utils.intlow,handles.indat.utils.inthigh],'normalisation',indat.norm_method,'range',...
    indat.range,'scale',1);
wbrf= homer(DSOw,cellout{:});


hplot=findobj('Tag', 'axes1');
if isempty(hplot)
    hplot=gca;
end
ydata=wbrf.det_data.datasets.signal;
xdata=wbrf.det_data.datasets.y;
plot(hplot,xdata,ydata)
guidata(gcbo,handles);




% --- Executes on button press in radiobutton18.
function radiobutton18_Callback(hObject, eventdata, handles)
% hObject    handle to radiobutton18 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of radiobutton18
val=get(handles.radiobutton18, 'Value');
if val == 1;
    hplot=findobj('Tag', 'axes1');
    if isempty(hplot);
        hplot=gca;
    end
    hold(hplot, 'all');
else
    hplot=findobj('Tag', 'axes1');
    if isempty(hplot);
        hplot=gca;
    end
    hold(hplot, 'off');
end


% --- Executes on button press in pushbutton16.
function pushbutton16_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton16 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

string=get(handles.edit39,'string');
handles.indat.utils.fname=parse_homer(string);
if isempty(handles.indat.utils.fname)
    disp('Specify run number')
    return
end
handles.indat.utils.intlow=str2num(get(handles.edit44,'string'));
handles.indat.utils.inthigh=str2num(get(handles.edit45,'string'));
handles.indat.utils.specnum=str2num(get(handles.edit47,'string'));
if isempty(handles.indat.utils.specnum)
    disp('No spectrum selected to plot')
    return
end

if isempty(handles.indat.utils.inthigh)
    handles.indat.utils.inthigh=19000;
end
if isempty(handles.indat.utils.intlow)
    handles.indat.utils.intlow=1;
end
indat=handles.indat;
rawfile = IXTraw_file(strcat(indat.datadir,'/',strcat(indat.inst,handles.indat.utils.fname),'.RAW'));
w = getspectrum(rawfile,handles.indat.utils.specnum);
hplot=findobj('Tag', 'axes1');
if isempty(hplot)
    hplot=gca;
end
pointx=(w.x(2:length(w.x))+w.x(1:length(w.x)-1))./2;
plot(hplot,pointx,w.signal)

guidata(gcbo,handles)


function edit47_Callback(hObject, eventdata, handles)
% hObject    handle to edit47 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit47 as text
%        str2double(get(hObject,'String')) returns contents of edit47 as a double


% --- Executes during object creation, after setting all properties.
function edit47_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit47 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end




% --- Executes on button press in radiobutton19.
function radiobutton19_Callback(hObject, eventdata, handles)
% hObject    handle to radiobutton19 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of radiobutton19
val=get(handles.radiobutton19, 'Value');
if val == 1;
    zoom on
else
    zoom off
end



% --- Executes on button press in radiobutton21.
function radiobutton21_Callback(hObject, eventdata, handles)
% hObject    handle to radiobutton21 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of radiobutton21

value=(get(handles.radiobutton21,'value'));
if value == 0
    handles.indat.background=handles.indat.background_default;
else
    handles.indat.background=[];
end
guidata(gcbo,handles);




% --- Executes on selection change in popupmenu5.
function popupmenu5_Callback(hObject, eventdata, handles)
% hObject    handle to popupmenu5 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = get(hObject,'String') returns popupmenu5 contents as cell array
%        contents{get(hObject,'Value')} returns selected item from popupmenu5
value= get(handles.popupmenu5, 'Value');

if value == 1;
    handles.indat.mv_mask=1;
end
if value == 2;
    handles.indat.mv_mask=2;
end
guidata(gcbo,handles);




% --- Executes during object creation, after setting all properties.
function popupmenu5_CreateFcn(hObject, eventdata, handles)
% hObject    handle to popupmenu5 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end




% --- Executes on button press in pushbutton18.
function pushbutton18_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton18 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% picks up values from the GUI in case 'return' was not pressed when values
% entered
[indat,handles]=get_GUI_input(handles);


%setup progressbar structure
pbar.color=[1 1 0];
pbar.title='Progress: ';
pbar.msg='starting Homer';
pbar.size=2;
pbar.pos='centreleft';
pb=progbar(pbar);


% The case for multple and single files with no summation

%here some checking is needed to see everything is populated
%send the indat structure to the routine
%preserve the origonal input structure

instname=get(handles.text2,'string');
instcolour=get(handles.text2,'BackgroundColor');
set(handles.text2,'BackgroundColor','y')
set(handles.text2,'string','running')
drawnow
msliceflag=get(handles.radiobutton14,'value');
if msliceflag == 1
    disp('Data will be sent to mslice window')
end

[white,absolute,abs_indat,use_mask]=check_handles(indat,handles);

% here check to see what corrections to run

solid_angle=get(handles.checkbox1,'value');
diag=get(handles.checkbox2,'value');

if indat.sum_files ~= 1
    a=size(indat.run_num,1);
    for i = 1:a %loop over the input numors by puting individual numors into a dummy indat
        %indat1 is local to loop for making singular indat.run_num entries
        indat1=indat;
        indat1.run_num=indat.run_num(i,:);
        pbar.title=strcat(['Progress: Run '  indat1.run_num ':' num2str(i) ' of ' num2str(a)]);
        if white &solid_angle==1
            pbar.msg='White beam';
            pbar.progress=0;
            progbar(pb,pbar);
            [handles.wbrf1,handles.wbrf2]=do_white(indat1);
        end
        if solid_angle==0
            %when no solid angle make white beam empty 
            disp('------------------------------------------------')
            disp('No solid angle correction will be preformed')
            disp('------------------------------------------------')
            handle.white_file='';
            handles.wbrf1=[];
        end
        if diag ==1
        pbar.msg='Diag';
        pbar.progress=20;
        progbar(pb,pbar);
        mask=do_diag2(indat1,handles.wbrf1,handles.wbrf2,true);
        else
            %when no diag make mask empty
            disp('------------------------------------------------')
            disp('Diag will not be run')
            disp('------------------------------------------------')
            mask=[];
        end
        
        if indat1.do_absolute==1
            % if ~absolute then will use handles.val_abs
            if absolute
                pbar.msg='White beam (absolute)';
                pbar.progress=25;
                progbar(pb,pbar);
                [handles.wbrf1_mono,handles.wbrf2_mono]=do_white(abs_indat);
                if(use_mask)
                    mask_mono=mask;
                else
                    pbar.msg='Diag (absolute)';
                    pbar.progress=30;
                    progbar(pb,pbar);
                    [mask_mono]=do_diag2(abs_indat,handles.wbrf1_mono,handles.wbrf2_mono,false);
                end
                pbar.msg='absolute normalisation';
                pbar.progress=40;
                progbar(pb,pbar);
                handles.val_abs=do_absolute(abs_indat,handles.wbrf1_mono,mask_mono);
            end
        else
            handles.val_abs=[];
        end
        indat1.spefilename=strcat(indat1.inst,indat1.run_num);
        pbar.msg='homer';
        pbar.progress=60;
        progbar(pb,pbar);
        data_out=do_homer(indat1,mask,handles.wbrf1,handles.val_abs);
        pbar.msg='writing spe file';
        pbar.progress=80;
        progbar(pb,pbar);
        runfile2spe(data_out,strcat(indat1.spedir,indat1.spefilename,'.spe'),msliceflag);
        pbar.msg='finished run';
        pbar.progress=100;
        progbar(pb,pbar);
        pause(0.5)
    end
else
    pbar.title=strcat(['Progress: Summing Multiple Runs']);
    if white
        pbar.msg='White beam';
        pbar.progress=0;
        progbar(pb,pbar);
        [handles.wbrf1,handles.wbrf2]=do_white(indat);
    end

    pbar.msg='Diag';
    pbar.progress=20;
    progbar(pb,pbar);

    mask=do_diag2(indat,handles.wbrf1,handles.wbrf2,true);
    if indat.do_absolute==1
        % if ~absolute then will use handles.val_abs
        if absolute
            pbar.msg='White beam (absolute)';
            pbar.progress=25;
            progbar(pb,pbar);
            [handles.wbrf1_mono,handles.wbrf2_mono]=do_white(abs_indat);
            if(use_mask)
                mask_mono=mask;
            else
                pbar.msg='Diag (absolute)';
                pbar.progress=30;
                progbar(pb,pbar);
                [mask_mono]=do_diag2(abs_indat,handles.wbrf1_mono,handles.wbrf2_mono,false);
            end
            pbar.msg='absolute normalisation';
            pbar.progress=40;
            progbar(pb,pbar);
            handles.val_abs=do_absolute(abs_indat,handles.wbrf1_mono,mask_mono);
        end
    else
        handles.val_abs=[];
    end
    pbar.msg='homer';
    pbar.progress=60;
    progbar(pb,pbar);
    data_out=do_homer(indat,mask,handles.wbrf1,handles.val_abs);
    indat.spefilename=strcat(indat.inst,indat.run_num(1,:));
    pbar.msg='writing spe file';
    pbar.progress=80;
    progbar(pb,pbar);
    runfile2spe(data_out,strcat(indat.spedir,indat.spefilename,'.spe'),msliceflag);
    pbar.msg='finished';
    pbar.progress=100;
    progbar(pb,pbar);
    pause(0.5)
end


set(handles.text2,'BackgroundColor',instcolour)
set(handles.text2,'string',instname)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% store run number variables in handles
handles.white_file_mono = indat.white_file_mono;
handles.white_file = indat.white_file;

if indat.do_absolute==1
    handles.run_num_mono = indat.run_num_mono;
    handles.mv_mapfile = indat.mv_mapfile;
    handles.mv_mask = indat.mv_mask;
end
close(pb);
guidata(gcbo,handles);




% --------------------------------------------------------------------
function exit_button_Callback(hObject, eventdata, handles)
% hObject    handle to exit_button (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

exit_button=questdlg('Exit Now?','Exit Program','Yes','No','No');
switch exit_button
    case 'Yes'
        delete(handles.figure1)
    case 'No'
        return
end





function edit65_Callback(hObject, eventdata, handles)
% hObject    handle to edit65 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit65 as text
%        str2double(get(hObject,'String')) returns contents of edit65 as a double


% --- Executes during object creation, after setting all properties.
function edit65_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit65 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in pushbutton20.
function pushbutton20_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton20 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
currentdir=pwd;
cd (handles.indat.mapdir);
[file, path, filter] = uigetfile('*.map', 'Select monitor mapping file');
cd(currentdir);
if ~isnumeric(file)
    set(handles.edit65,'string',file)
    % if mapdirectory is different from that set, then add it to list
    if ~strcmp(handles.indat.mapdir,path)
        addtogpath('inst_maps',path);
    end
    handles.indat.mapdir=path;
    handles.indat.mon_mapfile=file;
end
guidata(gcbo,handles)




function edit66_Callback(hObject, eventdata, handles)
% hObject    handle to edit66 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit66 as text
%        str2double(get(hObject,'String')) returns contents of edit66 as a double
handles.indat.vmass=str2double(get(hObject,'String'));
guidata(gcbo,handles);

% --- Executes during object creation, after setting all properties.
function edit66_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit66 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end




% --- Executes on button press in checkbox1.
function checkbox1_Callback(hObject, eventdata, handles)
% hObject    handle to checkbox1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of checkbox1
 set(handles.edit1,'string','')
 handles.white_file=1;
 indat.white_file=11;
 guidata(gcbo,handles);

% --- Executes on button press in checkbox2.
function checkbox2_Callback(hObject, eventdata, handles)
% hObject    handle to checkbox2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of checkbox2


