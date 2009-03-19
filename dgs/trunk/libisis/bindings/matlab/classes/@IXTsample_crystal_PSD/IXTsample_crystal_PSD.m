function samplecrystalPSD = ISISEXCsample_crystal_PSD( entry_name, varargin )
%ISISEXCsamplecrystalPSD Create an ISISEXCsample_crystal_PSD object 


% !-----------------------------------------------------------------------------------------------------------------------------------
% ! ISISEXCsample_crystal_PSD
% ! =============
% ! NeXus class: NXsample_crystal_PSD
% !
% !	entry_name					char	entry name in NeXus file
% !	name						char	name of sample_crystal_PSD
% !	chemical_formula			char
% !	temperature					real	sample_crystal_PSD temperature (K)
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
% !	shape						char	shape of sample_crystal_PSD ('plate', 'sphere', 'cylinder', 'hollow cylinder')
% ! *	x_geom(3)				real	sample_crystal_PSD geometry x-axis (r.l.u.)
% ! *	y_geom(3)				real	sample_crystal_PSD geometry y-axis (r.l.u.)
% !	position(3)					real	Position of centre of sample_crystal_PSD w.r.t. nominal sample_crystal_PSD position
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



switch nargin
case 0
    % if no input arguments, create a default object
    samplecrystalPSD.entry_name ='';
    
    samplecrystalPSD = class(samplecrystalPSD,'ISISEXCsample_crystal_PSD',ISISEXCsample ); % inherit from ISISEXCsample
    
case 1
    % if single argument of class ISISEXCsample_crystal_PSD, return it
    if isa(entry_name,'ISISEXCsample_crystal_PSD')
        samplecrystalPSD = entry_name;
    else
        
        disp([entry_name, '  is not an ISISEXCsample_crystal_PSD object.']);
        return
    end
    

    % create object using specified arguments
    
case 2    
    
    check=1;
    
    samplecrystalPSD.entry_name = entry_name;
    if(isa(varargin{1},'ISISEXCsample')), sample = varargin{1};  else disp('1'), check =0; end


    if (check==1), 
        samplecrystalPSD = class(samplecrystalPSD,'ISISEXCsample_crystal_PSD', sample); 
    else 
        disp('!! It is not a valid ISISEXCsample_crystal_PSD constructor 1');
        samplecrystalPSD =[];
    end

    
    
    
    
otherwise
    disp('!! It is not a valid ISISEXCsample_crystal_PSD constructor 2');
end

