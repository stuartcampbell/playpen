function [indat,handles]=get_GUI_input(handles)


%%%
handles.indat.white_file=parse_homer(get(handles.edit1,'string'));
%%%
handles.indat.run_num=parse_homer(get(handles.edit2,'string'));
%%%
handles.indat.run_num_mono=parse_homer(get(handles.edit10,'string'));
%%%
handles.indat.white_file_mono=parse_homer(get(handles.edit29,'string'));
%%%
handles.indat.hardmask=get(handles.edit30,'string');
handles.indat.mv_mapfile=get(handles.edit15,'string');
handles.indat.map_file=get(handles.edit8,'string');
% loading of mono_van_int_lim moved to check_handles
%handles.indat.mono_van_int_lim(1)=str2num(get(handles.edit12,'string'));
%handles.indat.mono_van_int_lim(2)=str2num(get(handles.edit14,'string'));
handles.indat.ei_init=str2num(get(handles.edit16,'string'));
if isempty(handles.indat.ei_init)
    error('ERROR: ei value must be filled in')
end
a=str2num(get(handles.edit17,'string'));
b=str2num(get(handles.edit18,'string'));
c=str2num(get(handles.edit19,'string'));
if isempty(a) || isempty(b) ||isempty(c)
    error('ERROR: Emin, Estep and E max must be filled in ')
end
handles.indat.spe_rebin_lims(1)=a;
handles.indat.spe_rebin_lims(2)=b;
handles.indat.spe_rebin_lims(3)=c;
handles.indat.samp_mass=str2num(get(handles.edit20,'string'));
handles.indat.samp_rmm=str2num(get(handles.edit22,'string'));

VLOW=str2num(get(handles.edit23,'string'));
VHIGH=str2num(get(handles.edit24,'string'));
FACTOR=str2num(get(handles.edit25,'string'));
STAB=str2num(get(handles.edit26,'string'));
background1=str2num(get(handles.edit27,'string'));
background2=str2num(get(handles.edit28,'string'));

if isempty(VLOW) || isempty(VHIGH) || isempty (FACTOR) || isempty(STAB) || isempty(background1) ||isempty (background2)
    error('ERROR:  all diag parameters must be filled in, VLOW, VHIGH, BKGD etc.')
end
handles.indat.VLOW=VLOW;
handles.indat.VHIGH=VHIGH;
handles.indat.FACTOR=FACTOR;
handles.indat.STAB=STAB;
handles.indat.background_default(1)=background1;
handles.indat.background_default(2)=background2;

indat=handles.indat;