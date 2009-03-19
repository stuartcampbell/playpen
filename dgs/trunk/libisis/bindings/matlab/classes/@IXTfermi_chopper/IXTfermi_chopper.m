function fermi_chopper = IXTfermi_chopper(varargin)
% MATLAB constructor for IXTfermi_chopper
% >> fermi_chopper = IXTfermi_chopper( IXTbase, [name], [distance], 
%                                     [frequency], [radius], [curvature], [slit_width], 
%                                     [blade_width], [width], [energy]);
% Author: I.Bustinduy, T.G.Perring

% Create a blank IXTfermi_chopper - this is needed as fortran cannot create real classes

fermi_chopper.base = IXTbase;       % Information for use in NeXus or other files
fermi_chopper.name = ' ';            % Name of the slit package (e.g. 'sloppy')
fermi_chopper.distance = 0;         % Distance from sample (m) (-ve if upstream of sample)
fermi_chopper.frequency = 0;        % Frequency of rotation (hz)
fermi_chopper.period = 0;           % Period of chopper rotation (s) = 1/frequency
fermi_chopper.radius = 0;           % Radius of chopper body (m)
fermi_chopper.curvature = 0;        % Radius of curvature of slits (m)
fermi_chopper.slit_width = 0;       % Slit width (m)  (Fermi)
fermi_chopper.slit_spacing = 0;     % Spacing between slit centres (m)
fermi_chopper.blade_width = 0;      % Thickness of neutron absorbing slat
fermi_chopper.width = 0;            % Width of aperture (m)
fermi_chopper.height = 0;           % Height of aperture (m)
fermi_chopper.energy= 0;            % Energy of neutrons transmitted by chopper (mev)

fermi_chopper = class(fermi_chopper,'IXTfermi_chopper');  

% Now call the fortran constructor which will fill the object
% and also check its arguments
if (nargin > 0)
    fermi_chopper = libisisexc('IXTfermi_chopper','create',fermi_chopper,varargin);
end

