function sample = IXTsample(  varargin )
%IXTsample Create an IXTsample object 
% ! Create an IXTsample object
% ! REQUIRED INPUT PARAMETERS
% ! sample = ISISEXCsample( [entry_name], [name], [chemical_formula],
%                           [temperature], [electric_field], [electric_coord], 
%                           [magnetic_field], [magnetic_coord], [pressure], 
%                           [unit_cell], [symmetry_cell_setting], [uvec], 
%                           [vvec], [psi], [omega], [gonio], [shape], 
%                           [x_geom], [y_geom], [position], [dimension], 
%                           [radius], [inner_radius], [height], [mass], 
%                           [molecular_weight], [xcoh], [xinc], [xabs]);
%
% !! @author Ibon Bustinduy, ISIS
% !! @V 1.0 Date: 2004/05/13
% 
% !-----------------------------------------------------------------------------------------------------------------------------------
% ! ISISEXCsample
% ! =============
% ! NeXus class: NXsample
% !
% !	entry_name					char	entry name in NeXus file
% !	name						char	name of sample
% !	chemical_formula			char
% !	temperature					real	sample temperature (K)
% !	electric_field(3)			real	applied electric field vector (V/m)
% ! *electric_coord				char	coordinate frame in which electric field is expressed ('instrument'|'rlu')
% !	magnetic_field(3)			real	applied magnetic field vector (Tesla)
% ! *magnetic_coord				char	coordinate frame in which magnetic field is expressed ('instrument'|'rlu')
% !	pressure					real	pressure (bar)
% !	unit_cell(6)				real	lattice parameters (angstrom and degree)
% !	symmetry_cell_setting		char	symmetry e.g. 'orthorhombic', 'cubic', 'hexagonal' (14 - one for each Bravais lattice)
% ! *uvec(3)					real	first vector defining scattering plane (r.l.u.)
% ! *vvec(3)					real	second vector defining scattering plane (r.l.u.)
% !	psi							real	Angle of uvec w.r.t. spectrometer coordinate frame
% !	omega						real	Angle of goniometer x-axis w.r.t. first vector defining scattering plane (uvec)
% ! *gonio(3)   				real	Angles (gs, gl, dpsi) that correct for position of 
% !	shape						char	shape of sample ('plate', 'sphere', 'cylinder', 'hollow cylinder')
% ! *	x_geom(3)				real	sample geometry x-axis (r.l.u.)
% ! *	y_geom(3)				real	sample geometry y-axis (r.l.u.)
% !	position(3)					real	Position of centre of sample w.r.t. nominal sample position
% !									   (coord. frame is spectrometer coordinate frame)
% !	dimension(3)				real	plate edge lengths (m)
% !	radius						real	radius of cylinder or sphere (m)
% !	inner_radius				real	inner radius of hollow cylinder (m)
% !	height						real	height of cylinder (m)
% !	mass						real	mass (kg)
% !	molecular_weight			real	per formula unit
% !	xcoh						real	per formula unit (barns)
% !	xinc						real	per formula unit (barns)
% !	xabs						real	per formula unit (barns) at 25 mev
% !
% !
    sample.base = IXTbase;
    sample.name = 'none';
    sample.chemical_formula = 'formula';
    sample.temperature = 0.0;
    sample.electric_field = [0.0, 0.0, 0.0];
    sample.electric_coord = 'none';
    sample.magnetic_field = [0.0, 0.0, 0.0];
    sample.magnetic_coord = 'none';
    sample.pressure = 0.0;
    sample.lattice = IXTlattice;
    sample.uvec = [0.0, 0.0, 0.0];
    sample.vvec = [0.0, 0.0, 0.0];   
    sample.psi = 0.0;
    sample.omega = 0.0;    
    sample.gonio = [0.0, 0.0, 0.0];
    sample.shape = 'none';    
    sample.x_geom = [0.0, 0.0, 0.0];
    sample.y_geom = [0.0, 0.0, 0.0];
    sample.position = [0.0, 0.0, 0.0];
    sample.dimensions = [0.0, 0.0, 0.0];
    sample.radius = 0.0;
    sample.inner_radius = 0.0;
    sample.height = 0.0;
    sample.mass = 0.0;
    sample.molecular_weight = 0.0;
    sample.xcoh = 0.0;
    sample.xinc = 0.0;
    sample.xabs = 0.0;
    
    sample = class(sample,'IXTsample');
    
if (nargin > 0)
    lattice = libisisexc('IXTsample','create',sample,varargin);
end 