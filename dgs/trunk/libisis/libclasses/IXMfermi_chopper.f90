!-----------------------------------------------------------------------------------------------------------------------------------
! MODULE: IXMfermi_chopper
!-----------------------------------------------------------------------------------------------------------------------------------
!! @author Toby Perring, ISIS
!! @version $Revision: 1191 $ ($Date: 2007-08-09 11:09:49 -0400 (Thu, 09 Aug 2007) $)
!!
!! Fortran definition of IXMfermi_chopper object.
!!
!! Nearest equivalent NeXus class: NXfermi_chopper

module IXMfermi_chopper
  use IXMtype_definitions
  use IXMbase
  use IXMneutron_constants	! various constants for performing units conversions
  implicit none
  public :: IXTfermi_chopper
  type IXTfermi_chopper
     private
     type(IXTbase):: base						!! Information for use in NeXus or other files
     character(len=short_len):: name='name'			!! Name of the slit package (e.g. 'sloppy')
     real(dp):: distance=0.0_dp						!! distance from sample (m) (-ve if upstream of sample)
     real(dp):: frequency=0.0_dp						!! Frequency of rotation (hz)
     real(dp):: period=0.0_dp							!! Period of chopper rotation (s) = 1/frequency
     real(dp):: radius=0.0_dp							!! Radius of chopper body (m)
     real(dp):: curvature=0.0_dp						!! Radius of curvature of slits (m)
     real(dp):: slit_width=0.0_dp						!! Slit width (m)  (fermi)
     real(dp):: slit_spacing=0.0_dp					!! Spacing between slit centres (m)
     real(dp):: blade_width=0.0_dp					!! Thickness of neutron absorbing slat
     real(dp):: width=0.0_dp							!! Width of aperture (m)
     real(dp):: height=0.0_dp							!! Height of aperture (m)
     real(dp):: energy=0.0_dp							!! Energy of neutrons transmitted by chopper (mev)
  end type IXTfermi_chopper

  !-----------------------------------------------------------------------
  ! The following creates interfaces for generic functions defined by LIBISISEXC intrastructure
#define IXD_TYPE fermi_chopper
#include "class_header.f90"

  !-----------------------------------------------------------------------
  ! Interfaces to generic functions defined by class authors
  interface IXFtransmission_fermi_chopper
     module procedure IXFtransmission_internal_ei_fermi_chopper, &
          IXFtransmission_scalar_ei_fermi_chopper, IXFtransmission_vector_ei_fermi_chopper
  end interface

  interface IXFtransmission_odd_fermi_chopper
     module procedure IXFtransmission_internal_ei_odd_fermi_chopper, &
          IXFtransmission_scalar_ei_odd_fermi_chopper, IXFtransmission_vector_ei_odd_fermi_chopper
  end interface

  interface IXFvariance_fermi_chopper
     module procedure IXFvariance_internal_ei_fermi_chopper, &
          IXFvariance_scalar_ei_fermi_chopper, IXFvariance_vector_ei_fermi_chopper
  end interface

  interface IXFvariance_odd_fermi_chopper
     module procedure IXFvariance_internal_ei_odd_fermi_chopper, &
          IXFvariance_scalar_ei_odd_fermi_chopper, IXFvariance_vector_ei_odd_fermi_chopper
  end interface

contains

  !----------------------------------------------------------------------------------------------------------------------
  ! Generate standard display subroutine: SUBROUTINE IXFfermi_chopperDisplay(fermi_chopper,status).
  ! This can be used in any Fortran application that uses this module. The subroutine
  ! will also be used by the binding language (Matlab, Python or whatever) to display
  ! the contents of an instance of the class.
#define IXD_DESCRIPTION	"IXTfermi_chopper class"
#define IXD_TYPE fermi_chopper
#define IXD_SQTYPE 'fermi_chopper'
#include "class_base.f90"

  !-----------------------------------------------------------------------------------------------------------------------
  !!IXFdestroy routine deallocates any pointer arrays in the type, and calls the destroy function on any nested
  !!objects, could also be used to set variables back to their default values.
  !!If one of the objects is an array of structures, then each structure will be recursively destroyed by the
  !!IXFdealloc function.

  subroutine IXFdestroy_fermi_chopper(arg, status)
    implicit none
    type(IXTfermi_chopper) :: arg
    type(IXTstatus) :: status
    call IXFdestroy(arg%base,status)
    call IXFclear_valid(arg)
  end subroutine IXFdestroy_fermi_chopper


  !----------------------------------------------------------------------------------------------------------------------
  ! All classes must provide the operation IXFxxxxxOperationRun; it loops through
  ! all members of the class doing the supplied operation. Needed by 
  ! Fortran - e.g. used in IXFxxxxxDisplay (ther dafault display function) as well as
  ! Matlab & any other binding language (e.g. Python)

  recursive subroutine IXFoperation_run_fermi_chopper(op, field, arg, status)
    implicit none
    type(IXTfermi_chopper) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTfermi_chopper', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    ! The order of the fields below must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run(op,'base', arg%base, status)
    call IXFoperation_run(op,'name', arg%name, status)
    call IXFoperation_run(op,'distance', arg%distance, status)
    call IXFoperation_run(op,'frequency', arg%frequency, status)
    call IXFoperation_run(op,'period', arg%period, status)
    call IXFoperation_run(op,'radius', arg%radius, status)
    call IXFoperation_run(op,'curvature', arg%curvature, status)
    call IXFoperation_run(op,'slit_width', arg%slit_width, status)
    call IXFoperation_run(op,'slit_spacing', arg%slit_spacing, status)
    call IXFoperation_run(op,'blade_width', arg%blade_width, status)
    call IXFoperation_run(op,'width', arg%width, status)
    call IXFoperation_run(op,'height', arg%height, status)
    call IXFoperation_run(op,'energy', arg%energy, status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_fermi_chopper

  !----------------------------------------------------------------------------------------------------------------------
  ! Constructor routine to assemble the object from the individual components.
  ! this will call the set routine with all keywords except for ref
  subroutine IXFcreate_Fermi_chopper(fc, name, distance, frequency, period, radius, curvature, slit_width, &
       & slit_spacing, blade_width, width, height, energy, status)
    implicit none
    type(IXTfermi_chopper),intent(out) :: fc
    character(len=*),intent(in):: name
    real(dp),intent(in):: distance, frequency, period, radius, curvature, slit_width
    real(dp),intent(in)::slit_spacing, blade_width, width, height, energy
    type(IXTstatus),intent(inout) :: status

    ! the set routine can ONLY be called on an initialised object
    ! so in this *special* case it is initialised before it is filled
    call IXFmark_valid(fc)

    call IXFset_Fermi_chopper(fc,status, name, distance, frequency, period, radius, curvature, slit_width, &
            slit_spacing, blade_width, width, height, energy)
    !check is called in the set routine    
  end subroutine IXFcreate_Fermi_chopper

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFset operation can only be performed on a properly filled or initialised object. It takes an optional number of
  !! arguments to modify the object contents. A check is made at the end to determine that the edited object is correctly
  !! formed. Error flags are raised if there is any inconsistency. The optional arguments must always be specified by keywords.
  !! The order of arguments should match the order of declaration, except for the IXTbase type which will be the penultimate argument.
  !! The 'ref' argument is used to copy the values of a reference object to another. In this case the object being modified 
  !!does not have to be initialised, but the reference object must be initialised instead.

  recursive subroutine IXFset_Fermi_chopper(fc,status, name, distance, frequency, period, radius, curvature, slit_width, &
       slit_spacing, blade_width, width, height, energy, ref)
    implicit none
    type(IXTFermi_chopper),intent(inout)::fc
    type(IXTFermi_chopper),optional,intent(in)::ref
    character(len=*),intent(in),optional:: name			!! Name of the slit package (e.g. 'sloppy')
    real(dp),intent(in),optional:: distance						!! distance from sample (m) (-ve if upstream of sample)
    real(dp),intent(in),optional:: frequency						!! Frequency of rotation (hz)
    real(dp),intent(in),optional:: period							!! Period of chopper rotation (s) = 1/frequency
    real(dp),intent(in),optional:: radius							!! Radius of chopper body (m)
    real(dp),intent(in),optional:: curvature						!! Radius of curvature of slits (m)
    real(dp),intent(in),optional:: slit_width						!! Slit width (m)  (fermi)
    real(dp),intent(in),optional:: slit_spacing					!! Spacing between slit centres (m)
    real(dp),intent(in),optional:: blade_width					!! Thickness of neutron absorbing slat
    real(dp),intent(in),optional:: width							!! Width of aperture (m)
    real(dp),intent(in),optional:: height							!! Height of aperture (m)
    real(dp),intent(in),optional:: energy							!! Energy of neutrons transmitted by chopper (mev)
    type(IXTstatus),intent(inout)::status !! error status object

    ! check that either the reference object is initialised
    ! or that object to be modified is initialised
    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_fermi_chopper)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(fc)
    else    
       if(IXFvalid(fc) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_fermi_chopper)')
       endif
       if(status == IXCseverity_error)return
    endif

    if (present(ref))call IXFset_Fermi_chopper(fc,status, ref%name, ref%distance, ref%frequency, ref%period, ref%radius,&
         ref%curvature,ref%slit_width, ref%slit_spacing, ref%blade_width, ref%width, ref%height, ref%energy)

    if (present(name))fc%name = name
    if (present(distance))fc%distance = distance
    if (present(frequency))then 
       fc%frequency = frequency
       ! by definition if you change the frequency then the following must be true
       fc%period=1.0d0/frequency
    endif
    if (present(period))fc%period = period
    if (present(radius))fc%radius = radius
    if (present(curvature))fc%curvature = curvature
    if (present(slit_width))fc%slit_width = slit_width
    if (present(slit_spacing))fc%slit_spacing = slit_spacing
    if (present(blade_width))fc%blade_width = blade_width
    if (present(width))fc%width = width
    if (present(height))fc%height = height
    if (present(energy))fc%energy = energy

    call IXFcheck_fermi_chopper(fc,status)

  end subroutine IXFset_Fermi_chopper

  !-----------------------------------------------------------------------------------------------------------------------
  !! The IXFget subroutine will return elements of an object to an optional supplied arrays/variables. The supplied variables
  !! should be referrred to by keyword to avoid errors. The 'wout' variable is special and can be used to copy the 
  !! contents of a whole object to a new one.

  subroutine IXFget_Fermi_chopper(fc,status,name, distance, frequency, period, radius, curvature, slit_width, &
       & slit_spacing, blade_width, width, height, energy, wout)
    implicit none
    type(IXTFermi_chopper),intent(in)::fc
    type(IXTFermi_chopper),optional,intent(out)::wout
    character(len=*),intent(out),optional:: name			!! Name of the slit package (e.g. 'sloppy')
    real(dp),intent(out),optional:: distance						!! distance from sample (m) (-ve if upstream of sample)
    real(dp),intent(out),optional:: frequency						!! Frequency of rotation (hz)
    real(dp),intent(out),optional:: period							!! Period of chopper rotation (s) = 1/frequency
    real(dp),intent(out),optional:: radius							!! Radius of chopper body (m)
    real(dp),intent(out),optional:: curvature						!! Radius of curvature of slits (m)
    real(dp),intent(out),optional:: slit_width						!! Slit width (m)  (fermi)
    real(dp),intent(out),optional:: slit_spacing					!! Spacing between slit centres (m)
    real(dp),intent(out),optional:: blade_width					!! Thickness of neutron absorbing slat
    real(dp),intent(out),optional:: width							!! Width of aperture (m)
    real(dp),intent(out),optional:: height							!! Height of aperture (m)
    real(dp),intent(out),optional:: energy							!! Energy of neutrons transmitted by chopper (mev)
    type(IXTstatus),intent(inout)::status !! error status object

    if (present(wout))then ! need to create a copy, use set with ref=fc
       call IXFcopy_Fermi_chopper(fc,wout,status)
    endif
    if (present(name))name=fc%name
    if (present(distance))distance=fc%distance
    if (present(frequency))frequency=fc%frequency
    if (present(period))period=fc%period
    if (present(radius))radius=fc%radius
    if (present(curvature))curvature=fc%curvature 
    if (present(slit_width))slit_width=fc%slit_width
    if (present(slit_spacing))slit_spacing=fc%slit_spacing
    if (present(blade_width))blade_width=fc%blade_width
    if (present(width))width=fc%width
    if (present(height))height=fc%height
    if (present(energy))energy=fc%energy

    call IXFcheck_fermi_chopper(fc,status)

  end subroutine IXFget_Fermi_chopper




  !----------------------------------------------------------------------------------------------------------------------
  ! Subroutine to check consistency of arguments for the Fermi chopper class
  subroutine IXFcheck_Fermi_chopper(fc, status)
    type(IXTfermi_chopper) :: fc
    type(IXTstatus) :: status
    ! Do not do any checks at present - just a dummy routine
    call IXFcheck_base(fc%base,status)	
  end subroutine IXFcheck_Fermi_chopper

  !======================================================================================================================
  ! Methods for calculating transmission, pulse width etc.
  !======================================================================================================================
  ! Want functions that will take an optional scalar energy, or a vector of energy values at which to calculate
  ! the transmission and time variance. If no energy parameter is provided, use the internal energy parameter of the
  ! chopper class.
  ! Note we go through a hoop to call the vector version from the scalar versions because scalars are NOT the same as
  ! unit length vectors in F95.

  !======================================================================================================================
  ! Transmission functions. Function names are overloaded above
  !-----------------------------------------------------------------------
  function IXFtransmission_internal_ei_fermi_chopper (c, status)
    implicit none
    ! i/o arguments
    type(IXTfermi_chopper) :: c
    type(IXTstatus) :: status
    real(dp) IXFtransmission_internal_ei_fermi_chopper
    ! internal arguments
    real(dp) ei(1), ans(1)

    ei(1) = c%energy
    ans = IXFtransmission_gen_fermi_chopper (c, .TRUE., ei, status)
    IXFtransmission_internal_ei_fermi_chopper = ans(1)

  end function IXFtransmission_internal_ei_fermi_chopper

  !-----------------------------------------------------------------------
  function IXFtransmission_scalar_ei_fermi_chopper (c, energy, status)
    implicit none
    ! i/o arguments
    type(IXTfermi_chopper) :: c
    real(dp) :: energy
    type(IXTstatus) :: status
    real(dp) IXFtransmission_scalar_ei_fermi_chopper
    ! internal arguments
    real(dp) ei(1), ans(1)

    ei(1) = energy
    ans = IXFtransmission_gen_fermi_chopper (c, .TRUE., ei, status)
    IXFtransmission_scalar_ei_fermi_chopper = ans(1)

  end function IXFtransmission_scalar_ei_fermi_chopper

  !-----------------------------------------------------------------------
  function IXFtransmission_vector_ei_fermi_chopper (c, energy, status)
    implicit none
    ! i/o arguments
    type(IXTfermi_chopper) :: c
    real(dp) :: energy(:)
    type(IXTstatus) :: status
    real(dp) IXFtransmission_vector_ei_fermi_chopper(size(energy))

    IXFtransmission_vector_ei_fermi_chopper = IXFtransmission_gen_fermi_chopper (c, .TRUE., energy, status)

  end function IXFtransmission_vector_ei_fermi_chopper

  !-----------------------------------------------------------------------
  function IXFtransmission_internal_ei_odd_fermi_chopper (c, status)
    implicit none
    ! i/o arguments
    type(IXTfermi_chopper) :: c
    type(IXTstatus) :: status
    real(dp) IXFtransmission_internal_ei_odd_fermi_chopper
    ! internal arguments
    real(dp) ei(1), ans(1)

    ei(1) = c%energy
    ans = IXFtransmission_gen_fermi_chopper (c, .FALSE., ei, status)
    IXFtransmission_internal_ei_odd_fermi_chopper = ans(1)

  end function IXFtransmission_internal_ei_odd_fermi_chopper

  !-----------------------------------------------------------------------
  function IXFtransmission_scalar_ei_odd_fermi_chopper (c, energy, status)
    implicit none
    ! i/o arguments
    type(IXTfermi_chopper) :: c
    real(dp) :: energy
    type(IXTstatus) :: status
    real(dp) IXFtransmission_scalar_ei_odd_fermi_chopper
    ! internal arguments
    real(dp) ei(1), ans(1)

    ei(1) = energy
    ans = IXFtransmission_gen_fermi_chopper (c, .FALSE., ei, status)
    IXFtransmission_scalar_ei_odd_fermi_chopper = ans(1)

  end function IXFtransmission_scalar_ei_odd_fermi_chopper

  !-----------------------------------------------------------------------
  function IXFtransmission_vector_ei_odd_fermi_chopper (c, energy, status)
    implicit none
    ! i/o arguments
    type(IXTfermi_chopper) :: c
    real(dp) :: energy(:)
    type(IXTstatus) :: status
    real(dp) IXFtransmission_vector_ei_odd_fermi_chopper(size(energy))

    IXFtransmission_vector_ei_odd_fermi_chopper = IXFtransmission_gen_fermi_chopper (c, .FALSE., energy, status)

  end function IXFtransmission_vector_ei_odd_fermi_chopper

  !-----------------------------------------------------------------------
  !! Return the transmission of a Fermi chopper expressed as an effective open time. Account is taken of the
  !! slit width to slit spacing ratio, that is, the effective open time is multiplied by the ratio
  !! slit_width/(slit_width+blade_width).
  !! 
  function IXFtransmission_gen_fermi_chopper (c, sigma, energy, status)
    implicit none

    ! i/o arguments: [nb: (TGP is pretty certain) array functions must has explicit bounds]
    type(IXTfermi_chopper) :: c			!! Fermi chopper
    logical, intent(in) :: sigma		!! sigma = .TRUE. if chopper in phase, =.FALSE. if 180 degrees out of phase
    real(dp), intent(in) :: energy(:)	!! Array of energy values (meV) at which transmission is to be calculated
    type(IXTstatus) :: status			!! Status flag
    real(dp) :: IXFtransmission_gen_fermi_chopper(size(energy))	!! Transmission of chopper as an effective open time (seconds)

    ! arguments for internal use
    real(dp) :: sign, gam(size(energy)), max_trans

    ! Check that the required contraints on chopper parameters are satisfied
    ! Do NOT check if energy==0, as assume IEEE arithmetic and that the final result is NaN (as involves 1/sqrt(energy))
    if ((c%frequency == 0_dp) .OR. &
         (c%radius < 0.0_dp) .OR. &
         (c%curvature < 0.0_dp) .OR. &
         (c%slit_width==0.0_dp) .OR. &
         (c%blade_width < 0.0_dp)) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'check failed on fermi_chopper')
       IXFtransmission_gen_fermi_chopper = 0.0_dp
       return
    endif

    ! Get transmission:
    if (sigma) then
       sign = 1.0_dp
    else
       sign = -1.0_dp
    endif
    gam = (2.0_dp*(c%radius**2)/c%slit_width)* &
         & abs( 1.0_dp/c%curvature - (sign*fourpi_dp*abs(c%frequency))/sqrt(energy/c_v_to_emev) )
    max_trans = (c%slit_width/(fourpi_dp*c%radius)) * (c%slit_width/(c%slit_width+c%blade_width))
    where (gam < 1.0_dp)
       IXFtransmission_gen_fermi_chopper = max_trans * (1.0_dp - gam**2/6.0_dp)
    elsewhere (gam < 4.0_dp)
       gam = sqrt(gam)
       IXFtransmission_gen_fermi_chopper = max_trans * (gam*((gam-2.0_dp)**2)*(gam+4.0_dp)) / 6.0_dp
    elsewhere
       IXFtransmission_gen_fermi_chopper = 0.0_dp
    end where

  end function IXFtransmission_gen_fermi_chopper

  !======================================================================================================================
  ! Time variance functions. Function names are overloaded above
  !-----------------------------------------------------------------------
  function IXFvariance_internal_ei_fermi_chopper (c, status)
    implicit none
    ! i/o arguments
    type(IXTfermi_chopper) :: c
    type(IXTstatus) :: status
    real(dp) IXFvariance_internal_ei_fermi_chopper
    ! internal arguments
    real(dp) ei(1), ans(1)

    ei(1) = c%energy
    ans = IXFvariance_gen_fermi_chopper (c, .TRUE., ei, status)
    IXFvariance_internal_ei_fermi_chopper = ans(1)

  end function IXFvariance_internal_ei_fermi_chopper

  !-----------------------------------------------------------------------
  function IXFvariance_scalar_ei_fermi_chopper (c, energy, status)
    implicit none
    ! i/o arguments
    type(IXTfermi_chopper) :: c
    real(dp) :: energy
    type(IXTstatus) :: status
    real(dp) IXFvariance_scalar_ei_fermi_chopper
    ! internal arguments
    real(dp) ei(1), ans(1)

    ei(1) = energy
    ans = IXFvariance_gen_fermi_chopper (c, .TRUE., ei, status)
    IXFvariance_scalar_ei_fermi_chopper = ans(1)

  end function IXFvariance_scalar_ei_fermi_chopper

  !-----------------------------------------------------------------------
  function IXFvariance_vector_ei_fermi_chopper (c, energy, status)
    implicit none
    ! i/o arguments
    type(IXTstatus) :: status
    type(IXTfermi_chopper) :: c
    real(dp) :: energy(:)
    real(dp) IXFvariance_vector_ei_fermi_chopper(size(energy))

    IXFvariance_vector_ei_fermi_chopper = IXFvariance_gen_fermi_chopper (c, .TRUE., energy, status)

  end function IXFvariance_vector_ei_fermi_chopper
  !-----------------------------------------------------------------------
  function IXFvariance_internal_ei_odd_fermi_chopper (c, status)
    implicit none
    ! i/o arguments
    type(IXTfermi_chopper) :: c
    type(IXTstatus) :: status
    real(dp) IXFvariance_internal_ei_odd_fermi_chopper
    ! internal arguments
    real(dp) ei(1), ans(1)

    ei(1) = c%energy
    ans = IXFvariance_gen_fermi_chopper (c, .FALSE., ei, status)
    IXFvariance_internal_ei_odd_fermi_chopper = ans(1)

  end function IXFvariance_internal_ei_odd_fermi_chopper

  !-----------------------------------------------------------------------
  function IXFvariance_scalar_ei_odd_fermi_chopper (c, energy, status)
    implicit none
    ! i/o arguments
    type(IXTfermi_chopper) :: c
    real(dp) :: energy
    type(IXTstatus) :: status
    real(dp) IXFvariance_scalar_ei_odd_fermi_chopper
    ! internal arguments
    real(dp) ei(1), ans(1)

    ei(1) = energy
    ans = IXFvariance_gen_fermi_chopper (c, .FALSE., ei, status)
    IXFvariance_scalar_ei_odd_fermi_chopper = ans(1)

  end function IXFvariance_scalar_ei_odd_fermi_chopper

  !-----------------------------------------------------------------------
  function IXFvariance_vector_ei_odd_fermi_chopper (c, energy, status)
    implicit none
    ! i/o arguments
    type(IXTstatus) :: status
    type(IXTfermi_chopper) :: c
    real(dp) :: energy(:)
    real(dp) IXFvariance_vector_ei_odd_fermi_chopper(size(energy))

    IXFvariance_vector_ei_odd_fermi_chopper = IXFvariance_gen_fermi_chopper (c, .FALSE., energy, status)

  end function IXFvariance_vector_ei_odd_fermi_chopper

  !-----------------------------------------------------------------------
  !! Return the variance of the pulse width for a Fermi chopper.
  !!
  function IXFvariance_gen_fermi_chopper(c, sigma, energy, status)
    implicit none

    ! i/o arguments: [nb: (TGP is pretty certain) array functions must has explicit bounds]
    type(IXTfermi_chopper) :: c			!! Fermi chopper
    logical, intent(in) :: sigma		!! sigma = .TRUE. if chopper in phase, =.FALSE. if 180 degrees out of phase
    real(dp), intent(in) :: energy(:)	!! Array of energy values (meV) at which variance is to be calculated
    type(IXTstatus) :: status			!! Status flag
    real(dp) IXFvariance_gen_fermi_chopper(size(energy))	!! Variance of chopper pulse width (seconds^2)

    ! arguments for internal use
    real(dp) :: sign, gam(size(energy)), max_var

    ! Check that the required contraints on chopper parameters are satisfied
    ! Do NOT check if energy==0, as assume IEEE arithmetic and that the final result is NaN (as involves 1/sqrt(energy))
    if ((c%frequency == 0_dp) .OR. &
         (c%radius < 0.0_dp) .OR. &
         (c%curvature < 0.0_dp) .OR. &
         (c%slit_width==0.0_dp) .OR. &
         (c%blade_width < 0.0_dp)) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'check failed on fermi_chopper')
       IXFvariance_gen_fermi_chopper = 0.0_dp
       return
    endif

    ! Get variance:
    if (sigma) then
       sign = 1.0_dp
    else
       sign = -1.0_dp
    endif
    gam = (2.0_dp*(c%radius**2)/c%slit_width)* &
         & abs( 1.0_dp/c%curvature - (sign*fourpi_dp*abs(c%frequency))/sqrt(energy/c_v_to_emev) )
    max_var = (c%slit_width/(fourpi_dp*c%radius))**2 / 6.0_dp
    where (gam < 1.0_dp)
       IXFvariance_gen_fermi_chopper = max_var * ((1.0_dp-gam**4/10.0_dp)/(1.0_dp-gam**2/6.0_dp))
    elsewhere (gam < 4.0_dp)
       gam = sqrt(gam)
       IXFvariance_gen_fermi_chopper = max_var * 0.6_dp * (gam*((gam-2.0_dp)**2)*(gam+8.0_dp)/(gam+4.0_dp))
    elsewhere
       IXFvariance_gen_fermi_chopper = 0.0_dp
    end where

  end function IXFvariance_gen_fermi_chopper

  !======================================================================================================================

end module IXMfermi_chopper
