!----------------------------------------------------------------------------------------------------------------------
! Symmetry operations for azimuthal symmetry of geometric objects. We use in relation to detectors, for example.
!
! A symetry operation consists of a single character that describes the type, and a real array of length upto
! IXCmax_azisympar that gives parameters associated with that type. 
!
!	no symmetry: symtype = ' '   params = 0
!
!	powder:                'p'          = 0
!
!	rotation               'r'          = [phi_0, nfold]  (all objects moved to phi_0 +/- (pi/nfold))
!	
!	mirror                 'm'          = [phi_0]   (all objects reflected to phi_0 +/- (pi/2))
!
! The choice of IXCmax_azisympar=10 is soley to allo for other tyep to be added easily, I cannot imagine ever needing
! more that 10, but for the sake of generality, make it a constant at the top of the module.
!----------------------------------------------------------------------------------------------------------------------
module IXMazisym
	use IXMbase
	use IXMmaths_geometry
	use IXMmaths_projection
	implicit none
	! default is all variables and functions are hidden
	! public parameters
	integer(i4b), parameter, public :: IXCmax_azisympar=10
	! public types
	public :: IXTazisym
	! public interfaces
	type IXTazisym
		private		! The type is public, but the contents not
		type(IXTbase):: base
		character(len=1):: symtype = ' ' ! Type of symmetry: ' ' none (default), 'p'=powder, 'r'=rotation, 'm' mirror
		real(dp):: params(IXCmax_azisympar) = 0.0_dp
	end type IXTazisym
 !-----------------------------------------------------------------------
  ! The following creates interfaces for generic functions defined by LIBISISEXC intrastructure
#define IXD_TYPE azisym
#include "class_header.f90"

contains

#define IXD_DESCRIPTION	"IXTazisym class"
#define IXD_TYPE azisym
#define IXD_SQTYPE 'azisym'
#include "class_base.f90"

!----------------------------------------------------------------------------------------------------------------------
! All the usual create, destroy etc should go here...

! make sure that in the constructor that the symmetry type character is made lower case.
! default is a space, with azisym%params=0.0_dp

   subroutine IXFdestroy_azisym(arg, status)
    implicit none
    type(IXTazisym) :: arg
    type(IXTstatus) :: status
    arg%symtype=' '
    arg%params=0.0_dp 
    call IXFclear_valid(arg)
  end subroutine IXFdestroy_azisym

  recursive subroutine IXFoperation_run_azisym(op, field, arg, status)
    implicit none
    type(IXTazisym) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTazisym', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    ! this order must match the declarion order in matlab as it is
    ! used when parsing arguments passed in class creation with varargin
    call IXFoperation_run(op, 'base', arg%base, status)
    call IXFoperation_run(op,'symtype', arg%symtype, status)
    call IXFoperation_run(op,'params', arg%params, status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_azisym

  !-----------------------------------------------------------------------------------------------------------------------
  !! The IXFget subroutine will return elements of an object to an optional supplied arrays/variables. The supplied variables
  !! should be referrred to by keyword to avoid errors. The 'wout' variable is special and can be used to copy the 
  !! contents of a whole object to a new one.

  subroutine IXFget_azisym(azisym,status,symtype,params,wout)
    type(IXTazisym),intent(in)::azisym
    type(IXTazisym),optional,intent(out)::wout
    character(len=1),optional,intent(out)::symtype
    real(dp),optional,intent(out)::params(IXCmax_azisympar)   
    type(IXTstatus),intent(inout)::status

    if(present(wout))call IXFcopy(azisym,wout,status)

    if(present(symtype))symtype=azisym%symtype
    if(present(params))params=azisym%params

  end subroutine IXFget_azisym
  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFcreate subroutine STRICTLY takes all the elements required to define a class and creates the resulting
  !! object. The IXTbase type is an optional argument, if it is not supplied the base type from the default IXTtestclass
  !! declaration will be used, it is therefore the last argument. If an element of the object is another class then
  !! it MUST be initialised. 

  subroutine IXFcreate_azisym(azisym,symtype,params,status)
    type(IXTazisym),intent(out)::azisym
    character(len=1),optional,intent(in)::symtype
    real(dp),optional,intent(in)::params(IXCmax_azisympar)
    type(IXTstatus),intent(inout)::status

    call IXFmark_valid(azisym)
    call IXFset_azisym(azisym,status,symtype,params)
  end subroutine IXFcreate_azisym
  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFset operation can only be performed on a properly filled or initialised object. It takes an optional number of
  !! arguments to modify the object contents. A check is made at the end to determine that the edited object is correctly
  !! formed. Error flags are raised if there is any inconsistency. The optional arguments must always be specified by keywords.
  !! The order of arguments should match the order of declaration, except for the IXTbase type which will be the penultimate argument.
  !! The 'ref' argument is used to copy the values of a reference object to another. In this case the object being modified 
  !!does not have to be initialised, but the reference object must be initialised instead.

  recursive subroutine IXFset_azisym(azisym,status,symtype,params,ref)
    type(IXTazisym),intent(inout)::azisym
    type(IXTazisym),intent(in),optional::ref
    character(len=1),intent(in),optional::symtype
    real(dp),optional,intent(in)::params(IXCmax_azisympar)
    type(IXTstatus),intent(inout)::status

    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_azisym)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(azisym)
    else    
       if(IXFvalid(azisym) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_azisym)')
       endif
       if(status == IXCseverity_error)return
    endif

    if(present(ref))call IXFset_azisym(azisym,status,ref%symtype,ref%params)
    if(present(symtype))azisym%symtype=symtype
    if(present(params))azisym%params=params
        
    call IXFcheck_azisym(azisym,status)

  end subroutine IXFset_azisym

  !-----------------------------------------------------------------------------------------------------------------------  
  !! IXFcheck will make internal consistency checks in the object, such as array length checking to make
  !! sure the object is properly filled.

  subroutine IXFcheck_azisym(sym, status)
    implicit none
    type(IXTazisym) :: sym
    type(IXTstatus) :: status
    if (sym%symtype=='p') then
	endif
	if (sym%symtype=='r') then
	endif	
	if (sym%symtype=='m') then
    endif
  end subroutine IXFcheck_azisym

!----------------------------------------------------------------------------------------------------------------------
! Get the effective azimuthal angle after applying symmetry operations
	function IXTphieff_azisym (sym, phi) result(phieff)
	implicit none
	type (IXTazisym), intent(in) :: sym(:)	! could be several operations
	real(dp), intent(in):: phi(:)
	real(dp) :: phieff(size(phi))
	! internal variables
	real(dp) :: phi_0, dphi
	integer(i4b)::i

	phieff=phi	! initialise output as if no symmetry

	do i=1,size(sym)
		if (sym(i)%symtype=='p') then
			phieff=0	! all shapes given zero azimuthal angle
		elseif (sym(i)%symtype=='r') then
			phi_0 = sym(i)%params(1)
			dphi = twopi_dp/sym(i)%params(2)
			phieff = phieff - dphi*anint((phieff-phi_0)/dphi)
		elseif (sym(i)%symtype=='m') then
			phi_0 = sym(i)%params(1)
			phieff = phieff-phi_0
			phieff = -pi_dp/2 + phi_0 + (1-(2.0*modulo(anint(phieff/pi_dp),2.0_dp)))*(phieff+pio2_dp)
		endif
	end do

	end function IXTphieff_azisym

end module
