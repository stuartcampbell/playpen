!-----------------------------------------------------------------------------------------------------------------------------------
!MODULE: isisexc_classes
!-----------------------------------------------------------------------------------------------------------------------------------
!! Various derived data types for analysis of ISIS data
!! @author Toby Perring, ISIS
!! @version $Revision: 191 $ ($Date: 2004-07-16 09:20:10 -0400 (Fri, 16 Jul 2004) $)
!!
!!   Defines derived data types to hold raw data or analysed data. Analysed data includes: SPE files;
!!	the output of several SPE files combined into one file; projected SPE files (i.e. after projection onto
!!	e.g. (h,k,l,e) ), both single and combined; and cuts in 1D, 2D, 3D ...
!!
!!	On the whole, the derived data types follow the present NeXus format where there are equivalent NeXus classes.
!! They diverge in places, however, sometimes containing additional items, sometimes omitting others. The comments
!! section for each group indicates the major divergences. In the description of the variables, '*' indicates item
!! with no equivalent NeXus definition. Some items in the classes are renamed from the corresponding NeXus values in
!! order to keep variable names to a manageable length.
!!
!!	The definitions do not follow NeXus religiously for several reasons:
!! - (most significantly) the standard is still very fluid, and is still being defined,
!! - there are essential pieces of information that are not contained in the present NeXus definitions,
!! - much information is irrelevant for our purposes, and would clutter up Fortran90 derived data types
!!     which (unlike e.g. MATLAB) cannot be dynamically enlarged.
!!
!!
!!  The correspondence between type declarations in the definitions and the NeXus standard are:
!!>            Here           NeXus
!!          ----------------------------
!!          integer(i4b)    NX_int32
!!          real(dp)        NX_float32
!!          character*n     NX_char
!!<
!!	Notes:
!!
!!	- NeXus places no restrictions on the contents of classes, except that only fully NeXus-compliant classes
!!	  can have names beginning with NX. In the following, the derived types have names beginning ISISEXC (standing
!!	  for ISIS Excitations) e.g. ISISEXCsource.
!!
!!	- There is presently (January 2004) a push within ISIS and the neutron community in general to agree a 
!!	  standard for the NeXus classes. When that occurs, we can enforce these classes to conform and rename the
!!	  type definitions NXsource etc.
!! 
!!	- Most data types include both ENTRY_NAME, to hold the name of the instance of the corresponding NeXus class,
!!	  as well as NAME to hold a specific name. For example, we probably wish to have ISISEXCsample%entry_name = 'sample'
!!	  so that when browsing the NeXus file it is clear which group belongs to the sample, and (for example)
!!	  ISISEXCsample%name = 'Chromium'.
!!
!!	- To avoid having to define every variable as a derived type to allow the inclusion of attributes, we enforce the
!!	  units of quantities as defined in the classes below. Excepting some special cases e.g. lattice angles (deg),
!!	  collimation FWHH (deg), neutron properties (meV, Angstrom, Angstrom^-1), and time-of-flight (microseconds)
!!	  the rule generally followed is kg, m, s, rad.
!!
	
module isisexc_classes
	use IXMtype_definitions
	use IXMbase
	use IXMdataset_1d
	use IXMdataset_2d
    use IXMdataset_nd
	use IXMdatum
	use IXMmoments

!-----------------------------------------------------------------------------------------------------------------------------------
! ISISEXCsource
!-----------------------------------------------------------------------------------------------------------------------------------
!! Information describing the neutron source.
!!
!! NeXus class: NXsource

	type ISISEXCsource
	    type(IXTbase) cdata
		character(len=name_len) entry_name		!! Name of entry in NeXus file
		character(len=long_len) facility_name	!! Source name e.g. ISIS
		real(dp) frequency						!! Source frequency (Hz)
	end type

!-----------------------------------------------------------------------------------------------------------------------------------
! ISISEXCmoderator
!-----------------------------------------------------------------------------------------------------------------------------------
!! Information describing the neutron moderator.
!! @author Toby Perring, ISIS
!!
!! NeXus class: Not defined in NeXus, but should be. The ISIS proposed NeXus standard contains NXmoderator.

	type ISISEXCmoderator
	    type(IXTbase) cdata
		character(len=name_len) entry_name		!! Name of entry in NeXus file
		character(len=long_len) name			!! Name of the moderator
		real(dp) distance						!! Distance of moderator face from sample (m)
		real(dp) width							!! Moderator width (m)
		real(dp) height							!! Moderator height (m)
		real(dp) thickness						!! Moderator thickness (m)
		real(dp) angle							!! Angle of normal to incident beam (rad): +ve if normal is anticlockwise from incident beam
		real(dp) temperature					!! Temperature of moderator (k)
		character(len=short_len) pulse_model	!! Moderator pulse shape model
		real(dp), pointer :: pulse_pars(:)		!! Moderator pulse-shape parameters (interpretation depends on pulse_model)
	end type

!-----------------------------------------------------------------------------------------------------------------------------------
! ISISEXCfermi_chopper
!-----------------------------------------------------------------------------------------------------------------------------------
!! Information describing the Fermi chopper.
!! @author Toby Perring, ISIS
!!
!! NeXus class: NXfermi_chopper

	type ISISEXCfermi_chopper
		character(len=name_len) entry_name		!! Name of entry in NeXus file
		character(len=long_len) name			!! Name of the slit package (e.g. 'sloppy')
		real(dp) distance						!! distance from sample (m) (-ve if upstream of sample)
		real(dp) frequency						!! Frequency of rotation (hz)
		real(dp) period							!! Period of chopper rotation (s) = 1/frequency
		real(dp) radius							!! Radius of chopper body (m)
		real(dp) curvature						!! Radius of curvature of slits (m)
		real(dp) slit_width						!! Slit width (m)  (fermi)
		real(dp) slit_spacing					!! Spacing between slit centres (m)
		real(dp) blade_width					!! Thickness of neutron absorbing slat
		real(dp) width							!! Width of aperture (m)
		real(dp) height							!! Height of aperture (m)
		real(dp) energy							!! Energy of neutrons transmitted by chopper (mev)
	end type

!-----------------------------------------------------------------------------------------------------------------------------------
! ISISEXCcrystal
!-----------------------------------------------------------------------------------------------------------------------------------
!! Information describing a crystal analyser.
!! @author Toby Perring, ISIS
!!
!! NeXus class: NXcrystal
!!
!! Notes:
!!	- Some of the names have been changed from the NeXus standard simply to keep them to a reasonable length.
!!	- Take the energy as the ACTUAL energy, not simply the nominal energy.

	type ISISEXCcrystal
		character(len=name_len) entry_name	!! Name of entry in NeXus file
		character(len=long_len) name		!! Name of the crystal
		real(dp) distance					!! Distance from sample (m) (-ve if upstream of sample)
		real(dp) energy						!! Energy of reflected neutrons (mev)
		real(dp) dspace						!! Lattice parameter of nominal reflection (ang)
		integer(i4b) reflection(3)			!! Reflection [h,k,l]
		real(dp) rho_h						!! Radius of curvature of crystal in the horizontal plane (m)
		real(dp) rho_v						!! Radius of curvature of crystal in the horizontal plane (m)
		real(dp) horiz_ap					!! Width of crystal if rectangular (m)
		real(dp) vert_ap					!! Height of crystal if rectangular (m)
	end type


!-----------------------------------------------------------------------------------------------------------------------------------
! ISISEXCaperture
!-----------------------------------------------------------------------------------------------------------------------------------
!! Information describing an aperture.
!! @author Toby Perring, ISIS
!!
!! NeXus class: NXaperture
!!
!! Notes:
!!  - Some of the names have been changed from the NeXus standard simply to keep them to a reasonable length

	type ISISEXCaperture
		character(len=name_len) entry_name	!! Name of entry in NeXus file
		character(len=long_len) name		!! Name of the aperture
		real(dp) distance					!! Distance of centre from sample (m) (-ve if upstream of sample)
		character(len=short_len) shape		!! 'rectangular', 'circular'
		real(dp) horiz_posn, vert_posn		!! Horizontal, vertical position of aperture centre (m)
		real(dp) horiz_ap, vert_ap			!! Aperture width, height if rectangular (m)
		real(dp) radius						!! Aperture radius if circular (m)
	end type


!-----------------------------------------------------------------------------------------------------------------------------------
! ISISEXCattenuator
!-----------------------------------------------------------------------------------------------------------------------------------
!! Information describing an attenuator.
!! @author Toby Perring, ISIS
!!
!! NeXus class: NXattenuator
!!
!! Notes:
!!  - NeXus standard inadequate at moment - attenuation will be energy dependent. Lets store the
!!    attenuation factor as a function of energy

	type ISISEXCattenuator
		character(len=name_len) entry_name	!! Name of entry in NeXus file
		character(len=long_len) name		!! Name of the attenuator
		real(dp) distance					!! Distance of centre from sample (m) (-ve if upstream of sample)
		character(len=long_len) material	!! Type (e.g. polythene)
		real(dp) thickness					!! Thickness (m)
		type(IXTdataset_1d) attenuation	!! Attenuation factor as a function of energy (meV)
	end type

!-----------------------------------------------------------------------------------------------------------------------------------
! ISISEXCdetarray
!-----------------------------------------------------------------------------------------------------------------------------------
!! Information about the elements of a detector array.
!! @author Toby Perring, ISIS
!!
!! NeXus class: NXdetector
!!
!! Notes:
!!  - The order of indices is important: the first index in e.g. alpha is the one that gives the components for
!!    a single detector.
!!  - Rather different to NeXus standard, as the present definition stores much more information about a detector.
!!  - Not used:
!!     - TYPE is replaced by CODE
!!     - GAS_PRESSUE, EFFICIENCY, HEIGHT, RADIUS are implicit in DET
!!     - TIME_OF_FLIGHT omitted

	type ISISEXCdetarray
		integer(i4b) ndet_tot						!! * Total number of detectors
		integer(i4b), pointer :: det_no(:)			!! * Detector index number (use in preference to a name, as e.g. on maps can have O(10^5) detectors
		real(dp), pointer ::	 delta(:)			!! * Electronic delay time (microseconds)
		real(dp), pointer ::	 distance(:)		!! Distance from sample (m)
		integer(i4b), pointer :: code(:)			!! * Detector type code			(equivalent to TYPE, but integer, not character)
		real(dp), pointer ::	 theta(:)			!! Scattering angle (rad)		(normally called phi or two_theta)
		real(dp), pointer ::	 phi(:)				!! * Azimuthal angle (rad)
		real(dp), pointer ::	 w(:,:)				!! * True dimensions in natural frame (m) (first dimension must be 3)
		real(dp), pointer ::	 f(:,:)				!! * False dimensions in natural frame (m) (first dimension must be 3)
!! * Vector describing orientation of detector (degrees) (see notes elsewhere)
!! (first dimension must be 3)
		real(dp), pointer ::	 alpha(:,:)
!! * Detector parameters; interpretation depends on code (see notes elsewhere)
!! (first dimension must be 4)
		real(dp), pointer ::	 det(:,:)
	end type


!-----------------------------------------------------------------------------------------------------------------------------------
! ISISEXCworkarray
!-----------------------------------------------------------------------------------------------------------------------------------
!! Holds the workspace-to-spectrum-to-detector mapping, and all the detector information. Includes
!! information about which spectra are masked.
!! @author Toby Perring, ISIS
!!
!! NeXus class: None
!!
!! Information is held in three groups of data:
!!   - mapping of each workspace to spectra
!!   - mapping of each spectrum to detectors
!!   - information for each of the detectors, and the effective detector information for each
!!     workspace when averaged over all the detectors that contribute to the workspace.
!!
!! The detector information will be held only if the instance of ISISEXCworkarray is in a single SPE file.
!! When several SPE files are combined, or when cuts are made from either a single or multiple SPE file,
!! the instances of ISISEXCdetarray that contain the detector parameter and the effective detector
!! parameters for the workspaces will be held separately as top-level objects.
!! This is to avoid multiple repetition of the same detector information if a multiple SPE file , or a cut
!! from a multiple SPE file, has been created from many runs all taken on the same spectrometer.
!! See ISISEXCmultiple_tofndgs for more information. In this case, we insist that ISISEXCworkarray%det_eff%ndet_tot=0
!! and ISISEXCworkarray%det%ndet_tot=0, and that the allocatable arrays in ISISEXCworkarray%det_eff and
!! ISISEXCworkarray%det are not allocated.
!!
!!
!! Notes:
!!
!!  - It is required that the workspace numbers for ISISEXCtofndgs%DATA(1), ISISEXCtofndgs%DATA(2) ...are in the same
!!    order as stored in the array WORK_NO. This limits ISISEXCtofndgs to the case where each detctor appears in at
!!    most one spectrum, and each spectrum appears in at most one workspace.
!!  - The total number of workspaces and spectra, NWORK_TOT and NSPEC_TOT in can be found using F95
!!    intrinsic functions to enquire about the length of e.g. the arrays WORK_NO and SPEC_NO. It is required that
!!    the values of NWORK_TOT and NSPEC_TOT are correctly given, however. It is convenient to do so,
!!    as (i) we need always to check that the arrays are allocated before using SIZE, and (ii)
!!    we require NWORK_TOT = 0 to indicate that the arrays are not allocated in the case of multiple SPE files
!!    and cuts.
!!  - The memory saving in multiple SPE files can been tremendous  - e.g. if there are 160000 detector
!!    elements in the instrument, and a multple spe file is created from 100 runs, then the memory need
!!    is reduced from 2GB to 20MB (assuming 160 bytes per detector element). However, it is at odds with
!!    the ideas of data encapsulation, for the detector information is no longer held in ISISEXCworkarray
!!    itself.
!!
!! For future consideration:
!!  - Require that with multiple SPE files that detector information is held in ISISEXCwork_array, despite the
!!    memoryt penalty, in order to follow object-oriented practices more closely.
!!  - Split up the workspace and spectrum information into two classes.

	type ISISEXCworkarray
		character(len=name_len) entry_name	!! Name of entry in NeXus file
		character(len=long_len) name		!! Name of the workspace array

! Workspace information
		!! Number of workspaces
		integer(i4b) nwork_tot

		!! Workspace numbers are contained in work_no(1:nwork_tot).
		integer(i4b), pointer :: work_no(:)

		!! Number of contributing spectra to each workspace (before any masking)
		!! are contained in nspec(1:nwork_tot).
		integer(i4b), pointer :: nspec(:)

		!! spec_ind(i) contains the index to the first element in the array spec_no to contain
		!! the spectrum number of of a spectrum in the workspace i. See documentation for spec_ind below.
		!! The array runs over spec_ind(1:nwork_tot).
		integer(i4b), pointer :: spec_ind(:)

		!! Index of effective detector parameters for the workspace in the derived type
		!! ISISEXCdetarray with name DET_EFF. ('eff' stands for 'effective').
		integer(i4b), pointer :: det_eff_ind(:)

! Spectrum information
		!! Number of spectra in all the workspaces [must equal sum(nspec)]
		integer(i4b) nspec_tot

		!! Spectrum numbers for the workspaces are copntained in spec_no(1:nspec_tot).
		!! The spectrum numbers corresponding to workspace work_no(i) are
		!! spec_no(spec_ind(i)) ... spec_no(spec_ind(i) + nspec(i) - 1).
		integer(i4b), pointer :: spec_no(:)

		!! Mask array for spectra. If masked, then =.TRUE., if unmasked then = .FALSE.
		!! Array runs over masked(1:nspec_tot).
		logical, pointer :: masked(:)

		!! Number of detectors contributing to each spectrum
		!! Array runs over ndet(1:nspec_tot).
		integer(i4b), pointer :: ndet(:)

		!! det_ind(i) contains the index to the first element in the derived data type of type
		!! ISISEXCdetarray with name DET to contain
		!! the detector information of a detector in the spectrum number spec_no(i).
		!! The array runs over det_ind(1:nspec_tot).
		integer(i4b), pointer :: det_ind(:)

! Detector information
		!! det_eff(i) contains the effective detector parameters for a single detector corresponding to 
		!! the ith workspace. Elements run over det_eff(1:nwork_tot)
		type (ISISEXCdetarray) det_eff

		!! det(i) contains the detector parameters for the ith detector. The number of detectors, and hence
		!! the number of elements of det is given by sum(ndet) - ndet is defined above.
		type (ISISEXCdetarray) det

	end type


!-----------------------------------------------------------------------------------------------------------------------------------
! ISISEXCchopper_instrument
!-----------------------------------------------------------------------------------------------------------------------------------
!! Description of a chopper instrument.
!! @author Toby Perring, ISIS
!!
!! NeXus class: NXinstrument
!!
!! Notes:
!!
!!  - Mainly follows the NeXus standard, except that it uses the derived type ISISEXCworkarray to store information
!!    about the workspaces and constituent spectra and detectors.
!!
!! For future consideration:
!!  - Can be straighforwardly extended to account for other instrument types. For example, to accommodate
!!    a crystal analyser instrument, one of two approaches could be followed:
!!        - ISISEXCcrystal could be added (and the name changed to e.g. ISISEXCtof_instrument).
!!          If the pointer to type(ISISIEXCcrystal) is allocated, then the instrument is a crystal spectrometer;
!!          if instead type(ISISEXCfermi_chopper) is allocated, then is a chopper instrument.
!!        - In the object-oriented paradigm, we define ISISEXCcrystal_instrument. Methods that are common to the
!!          two instrument types are overloaded, e.g. to calculate energy resolution.

	type ISISEXCchopper_instrument
		character(len=name_len) entry_name	!! Name of entry in NeXus file
		character(len=long_len) name		!! Name on instrument (e.g. HET, MARI, MAPS)
		type(ISISEXCsource)	source			!! Source information
		type(ISISEXCmoderator)	moderator	!! Moderator information
		type(ISISEXCfermi_chopper)	monochromator			!! Fermi chopper information

		!! Aperture information. There can be more than one aperture, hence a pointer array.
		type(ISISEXCaperture),   pointer :: aperture(:)

		!! Attenuator information. There can be more than one attenuator, hence a pointer array.
		type(ISISEXCattenuator), pointer :: attenuator(:)

		!! Description of the workspaces. There is only one entry, as ISISEXCworkarray itself contains
		!! a set of allocatable arrays that hold workspace, spectrum and detector information for all
		!! the workspaces.
		type(ISISEXCworkarray) workspaces

	end type

!-----------------------------------------------------------------------------------------------------------------------------------
! ISISEXCuser
!-----------------------------------------------------------------------------------------------------------------------------------
!! Information about a user.
!! @author Toby Perring, ISIS
!!
!! NeXus class: NXuser

	type ISISEXCuser
		character(len=name_len) entry_name			!! Name of entry in NeXus file
		character(len=long_len) name				!! Name of user
		character(len=long_len) affiliation			!! name of home institution
		character(len=long_len) address
		character(len=long_len) telephone_number
		character(len=long_len) fax_number
		character(len=long_len) email
	end type

!-----------------------------------------------------------------------------------------------------------------------------------
! ISISEXCsample
!-----------------------------------------------------------------------------------------------------------------------------------
!! Description of the sample.
!! @author Toby Perring, ISIS
!!
!! NeXus class: NXsample

	type ISISEXCsample 
		character(len=name_len)	entry_name			!! Name of entry in NeXus file
		character(len=long_len)	name				!! Name of sample e.g. 'bilayer manganite, Kimura no. 3'
		character(len=long_len)	chemical_formula	!! Sample chemical formula e.g. 'La1.4Sr1.6Mn2O7'
		real(dp)	temperature						!! Temperature in Kelvin
		real(dp)	electric_field(3)				!! Electric field in V/m, in one of the conventional coordinate frames below.
		character(len=short_len)	electric_coord	!! Coordinate frame in which electric field is expressed ('instrument'|'rlu')
		real(dp)	magnetic_field(3)				!! Applied magnetic field in Tesla in coordinate frame below.
		character(len=short_len)	magnetic_coord	!! Coordinate frame in which magnetic field is expressed ('instrument'|'rlu')
		real(dp)	pressure						!! Applied hydrostatic pressure
		real(dp)	alatt(3)						!! Lattice parameters (Angstrom)
		real(dp)	ang(3)							!! Lattice angles (degree)

		!! Bravais lattice symmetry e.g. 'orthorhombic', 'cubic', 'hexagonal' (14 - one for each Bravais lattice)
		character(len=short_len) bravais

		real(dp)	uvec(3)				!! First reciprocal lattice vector defining scattering plane
		real(dp)	vvec(3)				!! Second reciprocal lattice vector defining scattering plane
		real(dp)	psi					!! Angle of uvec with respect to incident beam on sample (radians)

		!! Angle of minor goniometer arc with respect to nominal (i.e. uncorrected)
		!! direction of uvec
		real(dp)	omega

		!! Corrections to crystal orientation (gl, gs, dpsi), where
		!!     - gl   major goniometer arc (radian)
		!!     - gs   minor goniometer arc (radian)
		!!     - dpsi correction to psi (radian)

		real(dp)	gonio(3)		

		!! Shape of sample ('plate', 'sphere', 'cylinder', 'hollow cylinder')
		character(len=short_len)	shape

		real(dp)	x_geom(3)			!! Sample geometry x-axis (r.l.u.)
		real(dp)	y_geom(3)			!! Sample geometry y-axis (r.l.u.)

		!! Position of sample with respect to nominal sample position. The coordinatre frame is the
		!! spectrometer frame i.e. z axis along the incident beam direction, the y axis vertically upwards
		real(dp)	position(3)

		real(dp)	dimension(3)		!! Plate sample size: full widths along sample geometry x, y, z axes (m).
		real(dp)	radius				!! (Outer) adius of sample if sphere or (hollow) cylinder (m).
		real(dp)	inner_radius		!! Inner radius of sample if hollow cylinder (m).
		real(dp)	height				!! Height of sample if cylinder (m).
		real(dp)	mass				!! Mass of sample (kg).
		real(dp)	molecular_weight	!! Molecular weight per formula unit.
		real(dp)	xcoh				!! Coherent cross-section (barn per formula unit).
		real(dp)	xinc				!! Incoherent cross-section (barn per formula unit).
		real(dp)	xabs				!! Absorption cross-section at 25 meV (barn per formula unit).

	end type

!-----------------------------------------------------------------------------------------------------------------------------------
! ISISEXCmonitor
!-----------------------------------------------------------------------------------------------------------------------------------
!! Information about a monitor.
!! @author Toby Perring, ISIS
!!
!! NeXus class: NXmonitor
!!
!! Notes:
!!
!!   - We omit raw data, ie time_of_flight and data. Also, omits energy-dependent efficiency. Other information,
!!     such as distance, height etc. that are currently in the NeXus standard can in fact be contained as a detector
!!     desciption. Also stores derived information about peaks.

	type ISISEXCmonitor
		character(len=name_len) entry_name	!! Name of entry in NeXus file
		character(len=long_len) name		!! Name of the monitor: whitebeam_1, whitebeam_2,... monochromatic_1, monochromatic_2 ...
		type(IXTdatum) integral				!! Monitor integral and error
		real(dp) range(2)					!! Integration range
		character(len=short_len) integral_units	!! Units in which the integration was performed
		type(IXTmoments) moments					!! Various derived quantities for peaks (see separate derived data type)
		character(len=short_len) moments_units	!! Units in which the moments are expressed
		type(ISISEXCdetarray) det				!! detector information for the monitor
	end type

!-----------------------------------------------------------------------------------------------------------------------------------
! ISISEXCtofndgs
!-----------------------------------------------------------------------------------------------------------------------------------
!! Data file. Can hold raw data or processed data, both elastic and inelastic.
!! @author Toby Perring, ISIS
!!
!! NeXus class: NXentry
!!
!!      The name 'tofndgs' originates from 'time-of-flight neutron direct geometry spectrometer'. The name
!!      is misleading, however, as there is nothing explicit to constrain the data to that type of instrument.
!!      The data could be the raw time-of-flight information. White beam diffraction data will be held
!!      if DEFINITION = 'tofnsxd'
!!
!!      Data from the detectors is stored in the array type(ISISEXCdataset_2d) DATA(:). For each of the
!!      ISISEXCdataset_2d objects in the array DATA(:), the
!!         - x-axis is the time-of-flight, or the energy transfer, or d-spacing ...
!!         - y-axis is a list of workspace numbers
!!      For speed of access, we insist that the workspace numbers in DATA(1), DATA(2) ... are in the same order
!!      as stored in the array INSTRUMENT%WORKSPACES%WORK_NO. All workspaces will have data appearing in DATA(:).
!!
!!      The reason why DATA(:) is an array is that the workspaces may have different energy bins, for example.
!!      There are two common limiting cases:
!!         - All workspaces have the same energy bins.
!!           This is the case with .SPE files produced by the current VMS based HOMER program, where the data
!!           is rebinned onto the energy transfer axis with equal sized bins. In this case, size(data) = 1, that is,
!!           there only needs to be one element in the array DATA.
!!         - Each workspace has different energy bins.
!!           This will be the case if the detectors have different sample-to-detector distances, and the 
!!           time-of-flight bin boundaries are converted to energy transfer but without
!!           rebinning. In this case, the energy bins will be different for each workspace, and
!!           size(data) equals the number of workspaces.
!!      Intermediate cases may occur, for example:
!!         - If the x-axis is time-of-flight, and there is more than one time channel bin boundaries regime
!!         - On HET, the non-PSD detectors are all at either 2.5m or 4m.
!!      In these cases, the number of elements in DATA will depend on the order of the workspaces. If all 2.5m
!!      detectors appear first, followed by the 4m detectors, then size(DATA)=2
!!
!! Notes:
!!
!!   - When an pointer to an array of type ISISEXCtofndgs appears in ISISEXCmultiple_tofndgs
!!     (which holds multiple data files, the values of INSTRUMENT%WORKSPACES%DET_EFF%NDET_TOT
!!     and INSTRUMENT%WORKSPACES%DET%NDET_TOT are zero, and the
!!     detector information arrays are not allocated. Instead, the detector information is held in two other arrays
!!     at the top level of ISISEXCmultiple_tofndgs. See
!!     the definition of ISISEXCmultiple_tofndgs for more details. The same applies for projected datasets
!!     (ISISproj_tofndgs) and for 1D, 2D ... cuts (ISISEXCcut_tofndgs, ISISESCcutgen_tofndgs).
!!
!!     When an pointer to an array of type ISISEXCtofndgs appears in s appears in 
!!     ISISproj_tofndgs (which holds projected datasets), or ISISEXCcut_tofndgs or ISISESCcutgen_tofndgs
!!     (which hold 1D, 2D, ... cuts), then same applies. In addition, 
!!     the data arrays DATA(:) are not allocated either. This is because the data is held on a
!!     pixel-by-pixel basis in an instance of the derived type ISISproj_data, which includes indexing back to 
!!     the particular ISISEXCtofndgs file and workspace from which that pixel came.
!!
!!   - The detector information will generally only be kept for those workspaces that appear in DATA(:). This means
!!     that unnecessary detector information is not retained when only a few workspaces are kept. The limiting case
!!     is that of a single spectrum. We certainly do not want to keep the detector information for the other 40000
!!     on MAPS !
!!
!! For future consideration:
!!
!!   - See the discussion in the definition of ISISEXCworkarray about storage of detector information
!!      in ISISEXCmultiple_tofndgs in future.
!!
!!   - The data should be kept as one item, for example as:
!!>       type(ISISEXCdata)
!!            type(ISISEXCdataset2d), pointer :: data(:)
!!<       end type
!!      The details of the structure will be hidden.

	type ISISEXCtofndgs
		character(len=name_len) entry_name		!! Name of entry in NeXus file
		character(len=long_len) title			!! Title of run
		character*24 start_time	!! Start time in iso8601 form [yyyy-mm-dd hh:mm:ss+zzzz  zzzz=times zone e.g. 0600 for chicago]
		character*24 end_time	!! End time in iso8601 form [yyyy-mm-dd hh:mm:ss+zzzz  zzzz=times zone e.g. 0600 for chicago]
		integer(i4b) run_number	!! Run number
		real(dp) total_charge	!! Total number of microamp.hours (i.e. before vetoing)
		real(dp) good_charge	!! Number of microamp.hours
		integer(i4b) total_raw_frames			!! Total number of frames (i.e. before vetoing)
		integer(i4b) total_good_frames			!! Number of frames
		character(len=long_len) program_name	!! Program name that last performed analysis e.g. homer, diag
		character(len=long_len), pointer :: command_line(:)	!! Associated command line that produced data - can extend over many lines
		!! Definition of instrument. This dictates the analysis type:
		!!    - 'tofndgs' direct geometry spectrometer
		!!    - 'tofnigs' inverse geometry spectrometer
		!!    - 'tofnsxd' single crystal diffractometer
		character(len=short_len) definition

		type(ISISEXCuser), pointer ::	user(:)		!! User details (may have more than one user)
		type(ISISEXCsample)				sample		!! Sample information
		type(ISISEXCchopper_instrument)	instrument	!! Instrument description

		!! Monitors. They should have names whitebeam_1, whitebeam_2, ... monochromatic_1, monochromatic_2, ...
		!! In the case of the current chopper instruments at ISIS, our M1, M2, M3 are whitebeam_1, monochromatic_1, monochromatic_2
		type(ISISEXCmonitor), pointer ::monitor(:)

		!! The data. This item is a vector to allow for multiple time-channel or energy transfer bin boundary regimes. It is
		!! NOT for holding data of multiple periods.
		type(IXTdataset_2d), pointer ::	data(:)
	end type


!-----------------------------------------------------------------------------------------------------------------------------------
! ISISEXCmultiple_tofndgs
!-----------------------------------------------------------------------------------------------------------------------------------
!! For holding multiple data files as a single entity. The instruments need not be the same, nor have the same workspace-to-spectra
!! mapping.
!! @author Toby Perring, ISIS
!!
!! NeXus class: NXentry
!!
!! Notes:
!!
!! In any instance of ISISEXCmultiple_tofndgs, the detector information will be absent from all
!! DATASET(I)%INSTRUMENT%WORKSPACES 
!!   - INSTRUMENT%WORKSPACES%DET_EFF%NDET_TOT = 0 and INSTRUMENT%WORKSPACES%DET%NDET_TOT = 0, and
!!   - detector information arrays are not allocated.
!! Instead, the detector information will have been collected into DET_EFF and DET. The index arrays in all of
!! DATASET(I)%INSTRUMENT%WORKSPACES%DET_EFF_IND and DATASET(I)%INSTRUMENT%WORKSPACES%DET_EFF_IND will need
!! to be altered accordingly. The reason for this is so that
!! when combining many tens of runs from, for example, MAPS, it is not necessary to hold multiple copies of the same
!! detector information.
!!
!! We insist that the detector information is collected as follows:
!!    - if DATASET(I)%INSTRUMENT%WORKSPACES%DET_EFF_IND == DATASET(J=1,I-1)%INSTRUMENT%WORKSPACES%DET_EFF_IND, then
!!      append DATASET(I)%INSTRUMENT%WORKSPACES%DET_EFF to DET_EFF 
!!    - if DATASET(I)%INSTRUMENT%WORKSPACES%DET_IND == DATASET(J=1,I-1)%INSTRUMENT%WORKSPACES%DET_IND, then
!!      append DATASET(I)%INSTRUMENT%WORKSPACES%DET to DET.
!! Note that the entire detector arrays are appended, even if there are only minor differences compared to previous detector
!! information.The cases of the effective detector information for the workspaces and the physical detectors
!! are treated separately.
!! This is most efficient in the cases where there are data files being combined which have the same mapping and spectra.dat
!! files, or different mapping files but the same spectra.dat.
!!
!! It is always possible to recover exactly an instance of ISISEXCtofndgs from ISISEXCmultiple_tofndgs without loss of information.
 
	type ISISEXCmultiple_tofndgs
		character(len=name_len) entry_name				!! Name of entry in NeXus file
		character(len=long_len) title					!! Main title for the whole entry
		type(ISISEXCtofndgs), pointer :: dataset(:)		!! Data (less detector information)
		type(ISISEXCdetarray)   det_eff					!! Effective detector information for the workspaces in all the datasets
		type(ISISEXCdetarray)   det						!! Detector information for all the datasets
	end type



!-----------------------------------------------------------------------------------------------------------------------------------
! ISISEXCproj_info
! ================
!! @author Toby Perring, ISIS
! NeXus class: None
!	Information about the projection axes (called viewing axes in present MSLICE) in which the data are expressed.
!
!
!	entry_name			char		Name of entry in NeXus file
!	name				char		Name
!	n_axes				int			Number of projection axes
!	caption(1:n_axes)	char		Captions for viewing axes
!	uindex(3)			int			Indices of any projection axes that were given in terms of reciprocal lattice units (rlu).
!								   Up to three such 'rlu projection axes' can occur e.g. if the axes were (1,1,0), (1,-1,0) and
!								  (1,1,1). Entries must be set to zero if not used e.g. if only two projection axes were in rlu,
!								  then uindex(3)=0
!	u(3,3)				real		Rlu projection axes expressed in term of rlu: e.g.in the above (u(:,1)=(1,1,0), u(:,2)=(1,-1,0),
!								   and u(:,3) = (1,1,1)
!	ulength(3)			real		Length of rlu projection axes in inverse Angstroms (unused elements set = 0.0)
!	u2ortho(3,3)		real		Matrix to convert those coordinates of a point expressed in rlu projection axes to orthonormal
!								   coordinates (needed to actually plot the data). (Elements in unused rows set to zero). The
!								   orthonormal is defined as follows:
!										if		u1(i), i=1->3 : first rlu projection axis, u2(i), i=1->3 second rlu projection axis ...
!										then	x1(i), i=1->3 : first axis in orthonormal frame; is parallel to u1
!												x2(i), i=1->3 : 2nd axis (if exists); in plane of u1 and u2
!												x3(i), i=1->3 : 3rd axis (if exists); parallel to vector product u1 x u2
!
!									 The matrix u2ortho is defined as follows: if a vector r = a(i)*xi  = b(i)*ui
!												a(i) = u2ortho(i,j)*b(j)
! Notes:
! ------
!	The reason why ulength and u2ortho are needed is that they will represent some average over all the SPE files if the lattice
!	parameters are not all equal (for example, if they changed slightly with changing temperature).
!
!	In fact, ulength is implicit in u2ortho, but it is convenient to have it held explicitly.
!
!	The number of projection axes does not need to be stored, but is convenient.
!
	type ISISEXCproj_info
		character(len=name_len) entry_name
		character(len=long_len) name
		integer(i4b) n_axes
		character(len=long_len), pointer :: caption(:)
		real(dp) uindex(3)
		real(dp) u(3,3)
		real(dp) ulength(3)
		real(dp) u2ortho(3,3)
	end type



!-----------------------------------------------------------------------------------------------------------------------------------
! ISISEXCproj_data
! ================
!! @author Toby Perring, ISIS
! NeXus class: None
!	For holding the coordinates of the centres of the pixels along the projection axes, with an index to the
!	unprojected dataset and the workspace from which it came.
!
!
!	entry_name		char		Name of entry in NeXus file
!	name			char		Name
!	s(:)			real		Intensity
!	e(:)			real		Standard error
!	u(:,:)			real		Coordinates of centre of pixel along projection axes [u(n_axes, n_pixels)]
!	id(:)			int			Index of dataset from which pixel came
!	iw(:)			int			Index of workspace for that dataset
!	xlo(:)			real		Lower bin boundary
!	xhi(:)			real		Upper bin boundary
!
!	IW is the workspace index, NOT the workspace number. For example, in ISISEXCproj_tofndgs, for the nth pixel
!	dataset(id(n))%instrument%workspaces%work_no(iw(n)) gives the actual workspace number.
!
! Notes:
! ------
!	The more rapidly varying dimension (first dimension) of U gives the coordinates in the projection axes for a single pixel.
!
!
! *** No constraint is imposed on the order of the pixels. However, we can imagine that order may be helpful
!	e.g. in order of idataset, then in order of iw for each idataset, then in order of xlo for that workspace
!
	type ISISEXCproj_data
		character(len=name_len) entry_name
		character(len=long_len) title
		real(dp), pointer :: s(:), e(:), u(:,:)
		integer(i4b), pointer :: id(:), iw(:)
		real(dp), pointer :: xlo(:), xhi(:)
	end type

!-----------------------------------------------------------------------------------------------------------------------------------
! ISISEXCproj_tofndgs
! ===================
!! @author Toby Perring, ISIS
! NeXus class: None
!	For holding one or multiple SPE file data in terms of an arbitrary number of projection axes.
!	For comments on the allocation of detector information and data arrays in TYPE(ISISEXCtofndgs) DATASET(:), see ISISEXCtofndgs.
!	For comments on how detector information is stored in DET_EFF and DET, see ISISEXCmultiple_tofndgs.
!
!	entry_name		char				Name of entry in NeXus file
!	title			char				Main title for the whole entry
!	dataset(:)		ISISEXCtofndgs		Data (less detector information and data arrays))
!	proj_info		ISISEXCproj_info	Information about the projection axes
!	proj_data		ISISEXCproj_data	Intensity and coordinates in terms of projection axes for each pixel
!	det_eff			ISISEXCdetarray		Effective detector information for the workspaces in all the datasets
!	det				ISISEXCdetarray		Detector information for all the datasets
!
!
!	
	type ISISEXCproj_tofndgs
		character(len=name_len) entry_name
		character(len=long_len) title
		type(ISISEXCtofndgs), pointer :: dataset(:)
		type(ISISEXCproj_info)	proj_info
		type(ISISEXCproj_data)	proj_data
		type(ISISEXCdetarray)   det_eff
		type(ISISEXCdetarray)   det
	end type


!-----------------------------------------------------------------------------------------------------------------------------------
! ISISEXCcut_tofndgs, ISISESCcutgen_tofndgs
! ============================================
!! @author Toby Perring, ISIS
! NeXus class: None
!
! Notes
! -----
!	Can hold any dimension of cut. The dimension is equal to size(plot%nx)
!

	type ISISEXCcut_tofndgs
		character(len=name_len) entry_name
		character(len=long_len) title
		type(IXTdataset_nd)	::	plot	! must add possibility for non-orthogonal axes to ISISEXCdataset_nd
		real(dp), pointer :: view_ind(:), view_range(:,:), view_step(:)	! only first really needed, others in ISISEXCdataset_nd
		real(dp), pointer :: proj_ind(:), proj_range(:,:)				! proj_range and view_range have forst dimension = 2
		integer(i4b), pointer :: npix(:), pix_ind(:)
		type(ISISEXCproj_tofndgs)	pixels
	end type


	type ISISEXCcutgen_tofndgs
		character(len=name_len) entry_name
		character(len=long_len) title
		! bin data: needs to have all the titles, distribution etc. of ISISEXCdataset_nd, as well as:
		real(dp), pointer :: s(:), e(:), bin_bound(:,:), bin_centre(:)

		real(dp), pointer :: view_ind(:), view_range(:,:), view_step(:)	! only first really needed, others in ISISEXCdataset_nd
		real(dp), pointer :: proj_ind(:), proj_range(:,:)				! proj_range and view_range have first dimension = 2
		integer(i4b), pointer :: npix(:), pix_ind(:)
		type(ISISEXCproj_tofndgs)	pixels
	end type



end module isisexc_classes

