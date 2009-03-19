


! Header File 
! Created By: Pranav
! Date: 08-02-2005



! translation module
      module IXMtranslation
      
	use IXMtype_definitions
	use IXMbase
	use IXMmaths_basis
	
	implicit none
! default is all variables and functions are hidden
!	private
! public types
	public :: IXTtranslation
! public interfaces
!    public :: 
    
	type IXTtranslation
		private		! The type is public, but the contents not
		real(dp) :: vector(3)	! cartesian 3-vector
	end type IXTtranslation
      end module IXMtranslation


! orientation module
      module IXMorientation
      
	use IXMtype_definitions
	use IXMbase
	use IXMmaths_basis
	use IXMmaths_utils
	use IXMtranslation
	
	implicit none
! default is all variables and functions are hidden
!	private
! public types
	public :: IXTorientation
! public interfaces
!    public :: 
    
	type IXTorientation
	
		private		! The type is public, but the contents not
		real(dp) :: rotmat(3,3)	! rotation matrix
		
	end type IXTorientation
	
      end module IXMorientation

! shape module
      module IXMshape
      
	use IXMtype_definitions
	use IXMbase
	use IXMmaths_geometry
	use IXMmaths_projection
	
	implicit none
! default is all variables and functions are hidden
!	private
! public parameters
	integer(i4b), parameter, public :: IXCpoint=0, IXCbox=1, IXCcylinder=2, IXCsphere=3, IXCholcyl=4, &
	                                   IXCpolygon=5
! public types
	  public :: IXTshape
! public interfaces
        public :: IXFvolume
    
	type IXTshape
	
		private		! The type is public, but the contents not
		integer(i4b) type						! type of shape (IXCbox, IXCcylinder ...)
		real(dp), pointer :: dimensions(:) => null()	! length and interpretation of contents of dimensions depends on value of TYPE
		
	end type IXTshape
	
	!volume???
      end module IXMshape


! geometry module
      module IXMgeometry
      
	use IXMtype_definitions
	use IXMbase
	use IXMtranslation
	use IXMorientation
	use IXMshape
	
	implicit none
! default is all variables and functions are hidden
!	private
! public parameters

! public types
	public :: IXTgeometry
! public interfaces
!   public :: 
    
	type IXTgeometry
	
		private		! The type is public, but the contents not
		type(IXTtranslation) :: translation
		type(IXTorientation) :: orientation
		type(IXTshape) :: shape
	end type IXTgeometry
	
      end module IXMgeometry

! group module
      module group
! default is all variables and functions are hidden
!	private
! public parameters

! public types
	public :: IXTgroup
! public interfaces
!   public :: 

      type IXTgroup
      
      private
	char(len=256):: inst_name ! instrument name
	integer :: group_index ! group index
	integer :: subgroup_no(:)  !subgroup included
	char(len=256) :: group_name ! group name
	
      end type IXTgroup

      end module group


! run file module
      module IXMrunfile
      
	use IXMmonitor
	use IXMsample
	use IXMchopperinstrument
	use IXMuser
      
      private :: IXTeff_det_index, IXTworkspace, IXTworktospectra, IXTspectratowork
      
      type IXTeff_det_index
      
      integer(i4b) :: good_ind(:)
	!! Index of Total effective detector parameters for the workspace in the derived type
	!! IXTdetarray with name DET_EFF. ('eff' stands for 'effective').
	integer(i4b) :: total_ind(:)		
            
      end type
      
      type IXTworkspace

		character(len=long_len) name		!! Name of the workspace array
        ! Workspace information
		!! Workspace numbers are contained in work_no(1:nwork_tot).
		integer(i4b), pointer :: work_no(:)
		!! Index of Good effective detector parameters for the workspace in the derived type
		!! IXTdetarray with name DET_EFF. ('eff' stands for 'effective').
		type (IXTeff_det_index) :: eff_det_index
        ! Detector information
		!! det_eff(i) contains the effective detector parameters for a single detector corresponding to 
		!! the ith workspace. Elements run over eff_det(1:nwork_tot)    
		type (IXTdetector) :: eff_det(:)
		
	end type


	
      
      public :: IXTrunfile
      

	type IXTrunfile

		character(len=long_len) title			!! Title of run
		character*24 start_time	!! Start time in iso8601 form [yyyy-mm-dd hh:mm:ss+zzzz  zzzz=times zone e.g. 0600 for chicago]
		character*24 end_time	!! End time in iso8601 form [yyyy-mm-dd hh:mm:ss+zzzz  zzzz=times zone e.g. 0600 for chicago]
		integer(i4b) :: ref_no(:)                  !! reference count from multiple runfile
		real(dp) total_charge	!! Total number of microamp.hours (i.e. before vetoing)
		real(dp) good_charge	!! Number of microamp.hours
		integer(i4b) total_raw_frames			!! Total number of frames (i.e. before vetoing)
		integer(i4b) total_good_frames			!! Number of frames
		character(len=long_len) program_name	!! Program name that last performed analysis e.g. homer, diag
		character(len=long_len) :: command_line(:)	!! Associated command line that produced data - can extend over many lines
		!! Definition of instrument. This dictates the analysis type:
		!!    - 'tofndgs' direct geometry spectrometer
		!!    - 'tofnigs' inverse geometry spectrometer
		!!    - 'tofnsxd' single crystal diffractometer
		character(len=short_len) definition
		type(IXTuser) ::	user(:)		!! User details (may have more than one user)
		type(IXTsample)	::			sample		!! Sample information
		type(IXTchopper_instrument)	:: instrument	!! Instrument description
		!! Monitors. They should have names whitebeam_1, whitebeam_2, ... monochromatic_1, monochromatic_2, ...
		!! In the case of the current chopper instruments at ISIS, our M1, M2, M3 are whitebeam_1, monochromatic_1, monochromatic_2
		type(IXTmonitor) ::monitor(:)
		!! The data. This item is a vector to allow for multiple time-channel or energy transfer bin boundary regimes. It is
		!! NOT for holding data of multiple periods.
		type(IXTdataset_2d), pointer ::	data(:)
		type(IXTdataset_2d), pointer ::	monitor_data(:)	
      	type(IXTworkspace) :: detector_workspaces
		type(IXTworktospectra) :: workspectrabridge 
		type(IXTspectratowork) :: spectraworkbridge 
		type(IXTworkspace) :: monitor_workspaces
		
	end type

      end module IXMrunfile

! multipe runfile module
      module IXMmultiplerunfile
      
      private :: IXTmerge_detector
      
      type IXTmerge_detector
      
      type (IXTdetector), pointer :: multiple_runfile_detector
      type (IXTdetector), pointer :: multiple_runfile_effective_detector
      
      end type

      public :: IXTmultiplerunfile
      
	type IXTmultiplerunfile

		character(len=long_len) title					!! Main title for the whole entry
		type(IXTrunfile) :: runfile(:)		!! run file Data.		
		!! Index of Good effective detector parameters for the workspace in the derived type
		!! IXTdetarray with name DET_EFF. ('eff' stands for 'effective').
		type (IXTmerge_detector) :: detector_purpose
		type (IXTmerge_detector) :: monitor_purpose
		
	end type
	
      end module IXMmultiplerunfile

