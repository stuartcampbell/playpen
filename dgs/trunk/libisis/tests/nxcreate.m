%addpath ../bindings/matlab/classes/
file_name='4to1_detinfo.nxs';
disp(strcat('creating ',file_name));
fio=open(IXTfileio,file_name,4);
% write instance of moderator
write(rfnorm1.inst.spectra,fio,'spectra');
write(rfnorm1.inst.detector,fio,'fulldet');
fio=close(fio);
%%%%%%%%%%%%%%%%%%%%%%
file_name='source.nxs';
disp(strcat('creating ',file_name));
fio=open(IXTfileio,file_name,4);
source=IXTsource;
source.base=IXTbase('source',logical(1),logical(1));
source.facility_name='isis';
source.frequency=50.0;
write(source,fio,'source');
fio=close(fio);
%%%%%%%%%%%%%%%%%%%%%%
file_name='fermi_chopper.nxs';
disp(strcat('creating ',file_name));
fio=open(IXTfileio,file_name,4);
chopper=IXTfermi_chopper;
chopper.base = IXTbase('IXTfermi_chopper',logical(1),logical(1));
% no energy/frequency, not fully populated/initialised
chopper.name='sloppy';
chopper.distance=10.0;
chopper.frequency = 0;        % Frequency of rotation (hz)
chopper.period = 0;           % Period of chopper rotation (s) = 1/frequency
chopper.radius = 0.049;           % Radius of chopper body (m)
chopper.curvature = 1.3 ;        % Radius of curvature of slits (m)
chopper.slit_width = 0.00228;       % Slit width (m)  (Fermi)
chopper.slit_spacing = 0.00283;     % Spacing between slit centres (m)
chopper.blade_width = 0.00055;      % Thickness of neutron absorbing slat
chopper.width = 0.045;            % Width of aperture (m)
chopper.height = 0.064;           % Height of aperture (m)
chopper.energy= 800.0;   
write(chopper,fio,'fermi_chopper');
fio=close(fio);
%%%%%%%%%%%%
att=IXTdataset_1d;
att.base=IXTbase('IXTdataset_1d',logical(1),logical(1));
att.title='Attenuation factor as a function of energy';
att.s_units=IXTunits('$cts','$Counts');
att.x_units=IXTunits('e');
att.signal=[1.0];
att.error=[1.0];
att.x=[1.0];
%%%%%%%%%%%%
file_name='attenuator_1.nxs';
disp(strcat('creating ',file_name));
fio=open(IXTfileio,file_name,4);
attenuator1=IXTattenuator;
attenuator1.base=IXTbase('IXTattenuator_1',logical(1),logical(1));
attenuator1.name='annual report 1999';
attenuator1.distance=5.0;
attenuator1.material='paper';
attenuator1.thickness=0.07;
attenuator1.attenuation=att;
write(attenuator1,fio,'attenuator_1');
fio=close(fio);
%%%%%%%%%%%%%%%%%%%
file_name='attenuator_1.nxs';
disp(strcat('creating ',file_name));
fio=open(IXTfileio,file_name,4);
attenuator2=IXTattenuator;
attenuator2.base=IXTbase('IXTattenuator_2',logical(1),logical(1));
attenuator2.name='annual report 2000';
attenuator2.distance=3.45;
attenuator2.material='cardboard';
attenuator2.thickness=0.1;
attenuator2.attenuation=att;
write(attenuator2,fio,'attenuator_2');
fio=close(fio);

%%%%%%%%%%%%%%%%%%%
file_name='aperture_1.nxs';
disp(strcat('creating ',file_name));
fio=open(IXTfileio,file_name,4);
aperture1=IXTaperture;
aperture1.base = IXTbase('IXTaperture',logical(1),logical(1));
aperture1.name = 'aperture 1';
aperture1.distance = 8.5;
aperture1.shape = 'square';
aperture1.horiz_posn = 0.0;
aperture1.vert_posn = 0.0;
aperture1.width = 0.10;
aperture1.height = 0.10;
aperture1.radius = 0.0;
write(aperture1,fio,'aperture');
fio=close(fio);
%%%%%%%%%%%%%%%%%%%
file_name='aperture_2.nxs';
disp(strcat('creating ',file_name));
fio=open(IXTfileio,file_name,4);
aperture2=IXTaperture;
aperture2.base = IXTbase('IXTaperture',logical(1),logical(1));
aperture2.name = 'aperture 2';
aperture2.distance = 0.3;
aperture2.shape = 'circular';
aperture2.horiz_posn = 0.0;
aperture2.vert_posn = 0.0;
aperture2.width = 0.0;
aperture2.height = 0.0;
aperture2.radius = 0.2;
write(aperture2,fio,'aperture');
fio=close(fio);

file_name='apertures.nxs';
disp(strcat('creating ',file_name));
fio=open(IXTfileio,file_name,4);
write(aperture1,fio,'aperture_1');
write(aperture2,fio,'aperture_2');
fio=close(fio);
%
