!------------------------------
! MODULE: IXMinstrument
!------------------------------
!! @author  Dickon Champion, ISIS
!! @version $Revision: 1414 $ ($Date: 2008-07-03 12:27:21 -0400 (Thu, 03 Jul 2008) $)
!!
!! FORTRAN definition of IXMinstrument class
! chopper instrument module
module IXMinstrument

  use IXMsource
  use IXMmoderator
  use IXMattenuator
  use IXMaperture
  use IXMspectra
  use IXMdetector
  use IXMchopper_instrument
  use IXMdiffraction_instrument
  use IXMdata_source
  implicit none
  public :: IXTinstrument
  type IXTinstrument
     private
     type(IXTbase):: base
     !! inst_type is an integer variable which describes the type of instrument which is present
     !! its values follow the values of emode which are returned by the instruemnt
     !! IXCdiffraction=0, IXCdirect=1, IXCindirect=2
     !! its default value is -1 
     integer(i4b)::inst_type=-1 !! Integer code describing type of instrument
     character(len=long_len):: name='name'		!! Name on instrument (e.g. HET, MARI, MAPS)
     type(IXTsource)::	source			!! Source information
     type(IXTmoderator)::	moderator	    !! Moderator information
     !! these are pointers to instrument specific objects, the diffraction instrument is empty and just contains 
     !! a base class object and a subroutine to return units information. the chopper instrument contains the 
     !! fermi_chopper and a subroutine to return units_information
     !! both objects need to be defined in the instrument object since matlab cannot cope with NULL pointers
     !! the object will therefore either be "initialised" or "not initialised". both instruments cannot have the initialised
     !! flags set
     type(IXTchopper_instrument)::ci !!chopper instrument object
     type(IXTdiffraction_instrument)::di !! diffraction instruemnt object

     !! Aperture information. There can be more than one aperture, hence an allocatable array.
     !! use IXFalloc/IXFdealloc to allocate/deallocate
     type(IXTaperture),allocatable   :: apertures(:)
     !! Attenuator information. There can be more than one attenuator, hence an allocatable array.
     !! use IXFalloc/IXFdealloc to allocate/deallocate
     type(IXTattenuator),allocatable  :: attenuators(:)
     !! Description of the workspaces. There is only one entry, as IXTworkarray itself contains
     !! a set of allocatable arrays that hold workspace, spectrum and detector information for all
     !! the workspaces.
     type(IXTspectra) :: spectra
     type(IXTdetector):: detector !put a ref count in det when spectra maps to it.

  end type IXTinstrument

  private:: finduseddetectors,findusedspectra,pop_alloc_objects
#define IXD_TYPE instrument
#include "class_header.f90"

contains

#define IXD_DESCRIPTION	"IXTinstrument class"
#define IXD_TYPE instrument
#define IXD_SQTYPE 'instrument'
#include "class_base.f90"

  !-----------------------------------------------------------------------------------------------------------------------
  ! All classes must provide this operation; it loops through
  ! all members of the class doing the supplied operation

  recursive subroutine IXFoperation_run_instrument(op, field, arg, status)
    implicit none
    type(IXTinstrument) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTinstrument', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    ! this order must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run(op,'base', arg%base, status)
    call IXFoperation_run(op,'inst_type', arg%inst_type, status)
    call IXFoperation_run(op,'name', arg%name, status)
    call IXFoperation_run(op,'source',arg%source,status)
    call IXFoperation_run(op,'moderator', arg%moderator, status)
    call IXFoperation_run(op,'ci', arg%ci, status)
    call IXFoperation_run(op,'di', arg%di, status)
    call IXFoperation_run_alloc(op,'apertures',arg%apertures,status)
    call IXFoperation_run_alloc(op,'attenuators',arg%attenuators,status)
    call IXFoperation_run(op,'spectra',arg%spectra,status)
    call IXFoperation_run(op,'detector',arg%detector,status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_instrument

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFcheck will make internal consistency checks in the object, such as array length checking to make
  !! sure the object is properly filled.
  subroutine IXFcheck_instrument(inst,status)
    implicit none
    type(IXTinstrument)::inst
    type(IXTstatus)::status
    integer(i4b)::idiff,ichop !! check variable for associated instruemnt pointers

    ichop=0
    idiff=0
    call IXFcheck_base(inst%base,status)
    call IXFcheck(inst%source,status)
    call IXFcheck(inst%moderator,status)
    if(IXFvalid(inst%ci))then
       ichop=1
       call IXFcheck(inst%ci,status)
    endif
    if(IXFvalid(inst%di))then
       idiff=1
       call IXFcheck(inst%di,status)
    endif

    if(allocated(inst%apertures) .and. IXFvalid(inst%apertures))call IXFcheck(inst%apertures,status) !this calls a check on the array of structures
    if(allocated(inst%attenuators).and. IXFvalid(inst%attenuators))call IXFcheck(inst%attenuators,status)!this calls a check on the array of structures
    call IXFcheck(inst%spectra,status)
    call IXFcheck(inst%detector,status)

    ! *one* instrument only must be initialised
    if(idiff+ichop /= 1) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
            IXCerr_invparam,'IXTinstrument failure only one type of instrument must be associated (IXFcheck_instrument)')
    endif

    if(ichop + inst%inst_type /= 2) then
       if(idiff + inst%inst_type /= 1) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
               IXCerr_invparam,'IXTinstrument%inst_type does not agree with initialised instruemnt object (IXFcheck_instrument)')    
       endif
    endif

  end subroutine IXFcheck_instrument

  !-----------------------------------------------------------------------------------------------------------------------
  !!IXFdestroy routine deallocates any pointer arrays in the type, and calls the destroy function on any nested
  !!objects, could also be used to set variables back to their default values.
  !!If one of the objects is an array of structures, then each structure will be recursively destroyed by the
  !!IXFdealloc function.

  subroutine IXFdestroy_instrument(inst,status)
    implicit none
    type(IXTinstrument)::inst
    type(IXTstatus)::status
    integer(i4b)::ref !! variable which is checked by instrument class to see if it can destroy detector instance fully
    call IXFdestroy(inst%base,status)
    if (IXFvalid(inst%source)) call IXFdestroy(inst%source,status)
    if (IXFvalid(inst%moderator)) call IXFdestroy(inst%moderator,status)
    if (IXFvalid(inst%ci))call IXFdestroy(inst%ci,status)
    if (IXFvalid(inst%di))call IXFdestroy(inst%di,status)

    ! dealloc is called since these objects are allocatable arrays of structures
    if(allocated(inst%apertures)) call IXFdealloc(inst%apertures,status)
    if(allocated(inst%attenuators)) call IXFdealloc(inst%attenuators,status)

    if (IXFvalid(inst%spectra))call IXFdestroy(inst%spectra,status)
    ! this will decrement the counter in inst%detector and destroy the arrays if necessary
    call IXFdestroy(inst%detector,status)

    ! check value of ref_count of detector
    !call IXFget_detector(inst%detector,status,ref_count=ref)
    ! if ref_count is zero destroy it totally
    !if (ref .eq. 0)deallocate(inst%detector)
    !always remove pointer from THIS instrument 

    !inst%detector=>NULL()

    ! the initialised status is now revoked for the object
    ! this statement MUST exist in all destroy routines
    call IXFclear_valid(inst)

  end subroutine IXFdestroy_instrument

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFcreate subroutine STRICTLY takes all the elements required to define a class and creates the resulting
  !! object. The IXTbase type is an optional argument, if it is not supplied the base type from the default IXTtestclass
  !! declaration will be used, it is therefore the last argument. If an element of the object is another class then
  !! it MUST be initialised.

  subroutine IXFcreate_instrument(inst,name,source,moderator,ci,di,apertures,attenuators,&
       spectra,detector,status)
    implicit none
    type(IXTinstrument),intent(out)::inst
    character(len=long_len),intent(in):: name    
    type(IXTsource),intent(in)::source
    type(IXTmoderator),intent(in)::moderator
    type(IXTaperture),intent(in)::apertures(:)
    type(IXTattenuator),intent(in)::attenuators(:)
    type(IXTspectra),intent(in)::spectra
    type(IXTchopper_instrument),intent(in),optional::ci !!chopper instrument object
    type(IXTdiffraction_instrument),intent(in),optional::di !! diffraction instruemnt object
    type(IXTdetector),intent(in)::detector !(detector structure to be pointed at by inst%detector)
    type(IXTstatus),intent(inout)::status

    ! nested objects should be tested for initialisation, this shows they have been created properly   
    if( IXFvalid(source) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTsource failure, all nested objects MUST be initialised (IXFcreate_instrument)')
    endif

    if( IXFvalid(moderator) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTmoderator failure, all nested objects MUST be initialised (IXFcreate_instrument)')
    endif


    if( IXFvalid(apertures) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTaperture failure, all nested objects MUST be initialised (IXFcreate_instrument)')
    endif

    if( IXFvalid(attenuators) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTattenuator failure, all nested objects MUST be initialised (IXFcreate_instrument)')
    endif

    if( IXFvalid(spectra) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTspectra failure, all nested objects MUST be initialised (IXFcreate_instrument)')
    endif

    if( IXFvalid(detector) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTdetector failure, all nested objects MUST be initialised (IXFcreate_instrument)')
    endif


    ! only one instrument must be supplied
    if((.not. present(ci)) .and. (.not. present(di)))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'either IXTdiffraction_instrument or IXTchopper_instrument objects MUST be present (IXFcreate_instrument)')
    endif
    if (status == IXCseverity_error) return
    if( present(ci) .and. present(di))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'both IXTdiffraction_instrument and IXTchopper_instrument cannot be present (IXFcreate_instrument)')
    endif
    if (status == IXCseverity_error) return


    call IXFmark_valid(inst)
    ! becuase of the ci/di optional arguments we make a special call to the IXFset command
    if (present(ci))then
       ! nested objects should be tested for initialisation, this shows they have been created properly   
       if( IXFvalid(ci) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'IXTchopper_instrument failure, all nested objects MUST be initialised (IXFcreate_instrument)')
          return
       endif
       inst%inst_type=1
       call IXFset_instrument(inst,status,name,source,moderator,ci=ci,apertures=apertures,attenuators=attenuators,spectra=spectra,detector=detector)       
    endif
    if (present(di))then
       ! nested objects should be tested for initialisation, this shows they have been created properly   
       if( IXFvalid(di) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'IXTdiffraction_instrument failure, all nested objects MUST be initialised (IXFcreate_instrument)')
          return
       endif
       inst%inst_type=0
       call IXFset_instrument(inst,status,name,source,moderator,di=di,apertures=apertures,attenuators=attenuators,spectra=spectra,detector=detector)
    endif

    if(status == IXCseverity_error)return


    ! makes a special call to the set routine without the detector pointer
    ! since we have set it with the call above and do not want to over increment the reference counter

    !call IXFget_ptr(i_ptr=inst%detector)
    !   call IXFincref_detector(inst%detector)

    ! the set routine can ONLY be called on an initialised object
    ! so in this *special* case it is initialised before it is filled
  end subroutine IXFcreate_instrument

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFset operation can only be performed on a properly filled or initialised object. It takes an optional number of
  !! arguments to modify the object contents. A check is made at the end to determine that the edited object is correctly
  !! formed. Error flags are raised if there is any inconsistency. The optional arguments must always be specified by keywords.
  !! The order of arguments should match the order of declaration, except for the IXTbase type which will be the penultimate argument.
  !! The 'ref' argument is used to copy the values of a reference object to another. In this case the object being modified 
  !!does not have to be initialised, but the reference object must be initialised instead.

  !inst_type cannot be set manually, so is not an argument to this subroutine
  recursive subroutine IXFset_instrument(inst,status,name,source,moderator,ci,di, &
       apertures,attenuators,spectra,detector,ref)
    implicit none
    type(IXTinstrument)::inst
    type(IXTinstrument),optional,intent(in)::ref
    character(len=long_len),optional,intent(in):: name
    type(IXTsource),optional,intent(in)::source
    type(IXTmoderator),optional,intent(in)::moderator
    type(IXTaperture),optional,intent(in)::apertures(:)
    type(IXTattenuator),optional,intent(in)::attenuators(:)
    type(IXTspectra),optional,intent(in)::spectra
    type(IXTchopper_instrument),optional,intent(in)::ci !!chopper instrument object
    type(IXTdiffraction_instrument),optional,intent(in)::di !! diffraction instruemnt object
    type(IXTdetector),optional::detector !!treated as normal sub-object   
    type(IXTstatus)::status

    ! check that either the reference object is initialised
    ! or that object to be modified is initialised
    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_workspace)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(inst)
    else    
       if(IXFvalid(inst) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_workspace)')
       endif
       if(status == IXCseverity_error)return
    endif


    if (present(ref))then 
       call IXFset_instrument(inst,status,ref%name,ref%source,ref%moderator,ref%ci,ref%di,ref%apertures,&
            ref%attenuators,ref%spectra,ref%detector)
    endif

    if (present(name))inst%name=name

    if (present(source))then
       call IXFcopy(source,inst%source,status)
    endif

    if (present(moderator))then
       call IXFcopy(moderator,inst%moderator,status)
    endif

    if(present(di))then
       if (IXFvalid(di))then
          call IXFcopy(di,inst%di,status)
          inst%inst_type=0
       endif
    endif

    if(present(ci))then
       if(IXFvalid(ci))then
          call IXFcopy(ci,inst%ci,status)
          inst%inst_type=1
       endif
    endif

    if (present(apertures))then   
       !       if(allocated(inst%apertures))call IXFdealloc(inst%apertures,status)
       call IXFrealloc(inst%apertures,size(apertures),.false.,status) ! this calls the IXFalloc_aperture to allocate an array of structures
       call IXFcopy(apertures,inst%apertures,status)  ! this calls IXFcopyarray_apertures to copy the array of structures, interfaced to IXFcopy
    endif

    if (present(attenuators))then   
       !       if(allocated(inst%attenuators))call IXFdealloc(inst%attenuators,status)
       call IXFrealloc(inst%attenuators,size(attenuators),.false.,status) ! this calls the IXFalloc_attenuator to allocate an array of structures
       call IXFcopy(attenuators,inst%attenuators,status)!! this should call IXFcopyarray_attenuator, which is interfaced to IXFcopy
    endif

    if (present(spectra))then
       call IXFcopy(spectra,inst%spectra,status)
    endif

    if (present(detector))then
       call IXFcopy(detector,inst%detector,status)
    endif
!!$    ! this should be called if setting detector to be new or from a copy
!!$    ! is not called by create routine
!!$    if (present(detector))then
!!$       ! if the set command is called to change a detector table tha tthe instrument points to
!!$
!!$       ! a) check if pointer of instrument is associated
!!$       if (associated(inst%detector))then
!!$          ! b) decrement the detector being changed
!!$          call IXFdecref_detector(inst%detector)
!!$          ! initialize the pointer of the instrument
!!$          inst%detector=>NULL()
!!$          ! c) increment will be made below
!!$       endif
!!$
!!$       ! and then create new pointer (either copy or a change)
!!$       inst%detector=>detector
!!$       ! and increment the reference (possibly to the new detector)
!!$       call IXFincref_detector(inst%detector)
!!$    endif

    call IXFcheck_instrument(inst,status)

  end subroutine IXFset_instrument

  !-----------------------------------------------------------------------------------------------------------------------
  !! The IXFget subroutine will return elements of an object to an optional supplied arrays/variables. The supplied variables
  !! should be referrred to by keyword to avoid errors. The 'wout' variable is special and can be used to copy the 
  !! contents of a whole object to a new one.

  subroutine IXFget_instrument(inst,status,inst_type,name,source,moderator,&
       ci,di,apertures,attenuators,spectra,detector,wout)
    implicit none
    type(IXTinstrument),intent(in)::inst
    type(IXTinstrument),optional,intent(out)::wout
    integer(i4b),optional,intent(out)::inst_type
    character(len=long_len),optional,intent(out):: name
    type(IXTsource),optional,intent(out)::source
    type(IXTmoderator),optional,intent(out)::moderator
    type(IXTchopper_instrument),optional,intent(out)::ci !!chopper instrument object
    type(IXTdiffraction_instrument),optional,intent(out)::di !! diffraction instruemnt object    
    type(IXTaperture),allocatable,optional,intent(out)::apertures(:)
    type(IXTattenuator),allocatable,optional,intent(out)::attenuators(:)
    type(IXTspectra),optional,intent(out)::spectra
    type(IXTdetector),optional,intent(out)::detector
    type(IXTstatus)::status

    ! this will create a copy of the chopper instrument and increment the reference counter on the detctor being pointed to
    if(present(wout))call IXFcopy(inst,wout,status)
    if(present(inst_type))inst_type=inst%inst_type
    if(present(name))name=inst%name
    if(present(source))call IXFcopy(inst%source,source,status)
    if(present(moderator))call IXFcopy(inst%moderator,moderator,status)
    if(present(ci))call IXFcopy(inst%ci,ci,status)
    if(present(di))call IXFcopy(inst%di,di,status)
    if(present(apertures))then
       call IXFrealloc(apertures,size(inst%apertures),.false.,status)
       call IXFcopy(inst%apertures,apertures,status)
       !this should call IXFcopyarray to make a copy of an allocatable aray of structures
    endif

    if(present(attenuators))then
       call IXFrealloc(attenuators,size(inst%attenuators),.false.,status)
       call IXFcopy(inst%attenuators,attenuators,status)
    endif

    if(present(spectra))call IXFcopy(inst%spectra,spectra,status)
    if(present(detector))then
       call IXFcopy(inst%detector,detector,status)
    endif

  end subroutine IXFget_instrument

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFget_ptr will return a pointer to a structure or an array, from an optional argument.
  !! The pointer arguments are generally the same name as the object elements they are pointing to.
  !! Care must be taken since if the pointers are edited, then the data in the structure will also be edited.

  subroutine IXFget_ptr_instrument(inst,ci,spectra,detector)
    implicit none
    type(IXTinstrument),intent(in),target :: inst !! it is a target since a pointer will point to a part of it
    type(IXTdetector),optional,pointer:: detector
    type(IXTspectra),optional,pointer::spectra
    type(IXTchopper_instrument),optional,pointer::ci

    if(present(ci)) ci=>inst%ci
    if (present(spectra)) spectra=>inst%spectra    
    if (present(detector)) detector=>inst%detector
    !may need to put a check in if it doesn't return a valid pointer

  end subroutine IXFget_ptr_instrument

  !-----------------------------------------------------------------------------------------------------------------------
  subroutine IXFunitsinfo_instrument(inst,status,emode,L1,efixed)
    implicit none
    type(IXTinstrument):: inst
    type(IXTstatus)::status
    integer(i4b),intent(out)::emode
    real(dp),intent(out),optional::L1,efixed

    if (present(L1))call IXFget_moderator(inst%moderator,status,distance=L1)
    ! maybe do an IXFcheck on instrument first
    ! find which type with inst_type

    if (IXFvalid(inst%ci))call IXFget_emode(inst%ci,status,emode,efixed)
    if (IXFvalid(inst%di))call IXFget_emode(inst%di,status,emode,efixed)

  end subroutine IXFunitsinfo_instrument
  !-----------------------------------------------------------------------------------------------------------------------
  subroutine IXFei_info_instrument(inst,L1,status)
    implicit none
    type(IXTinstrument)::inst
    real(dp)::L1
    type(IXTstatus)::status
    call IXFget_moderator(inst%moderator,status,distance=L1)
  end subroutine IXFei_info_instrument
  !-----------------------------------------------------------------------------------------------------------------------
  pure logical function IXFcompare_instrument(inst1,inst2)result(ident)
    implicit none
    type(IXTinstrument),intent(in):: inst1,inst2

    ident=IXFcompare_spectra(inst1%spectra,inst2%spectra)

  end function IXFcompare_instrument

  logical function IXFwhitecompare_instrument(sample,whitebeam)result(ident)
    implicit none
    type(IXTinstrument),intent(in):: sample,whitebeam
    integer,pointer::sampdet(:),whitedet(:)

    call IXFget_ptr_detector(sample%detector,det_no=sampdet)
    call IXFget_ptr_detector(whitebeam%detector,det_no=whitedet)

    ident=IXFwhitecompare_spectra(sample%spectra,sampdet,whitebeam%spectra,whitedet)

    sampdet=> NULL()
    whitedet=>NULL()

  end function IXFwhitecompare_instrument

  !-----------------------------------------------------------------------------------------------------------------------
  subroutine IXFpopulate_instrument(inst,inputsource,dso,status,det_map,mon_map)
    use IXMraw_file
    use IXMmap
    implicit none
    type(IXTinstrument):: inst
    type(IXTraw_file)::inputsource
    type(IXTdata_source)::dso
    type(IXTmap),optional::det_map,mon_map    
    logical::cfound,dfound,specfound,detfound
    integer(i4b)::ichop,idiff
    character(len=long_len)::nullpath,null2path,spec_path,det_path,spec_obj_name,det_obj_name
    type(IXTstatus)::status
    integer(i4b),allocatable::spe_used(:),list_out(:) !total detectors used by mapfiles

    call IXFpopulate_file_dso_source(inst%source,dso,status)
    call IXFpopulate_file_dso_moderator(inst%moderator,dso,status)    
    if(status == IXCseverity_error)return

    call pop_alloc_objects(inst,dso,status)
    call IXFfindpath_data_source(dso,IXCdiff_inst,nullpath,null2path,dfound,status)
    call IXFfindpath_data_source(dso,IXCchop_inst,nullpath,null2path,cfound,status)

    idiff=0
    ichop=0
    ! extra line must be added in here when further types of instruemnt are added    
    if(dfound)idiff=1
    if(cfound)ichop=1
    ! *one* instrument only must be initialised
    ! if =0,2 (only other possibilities) this will be flagged
    if(idiff+ichop /= 1) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
            IXCerr_invparam,'at least/only *one* type of instrument *must* be defined in IXTdata_source (IXFcheck_instrument)')
       return
    endif

    ! now we know only *one* of these will happen 
    if(dfound)then
       call IXFwrite_line('Diffraction instrument will be defined',status)
       inst%inst_type=IXCdiffraction
       call IXFpopulate_diffraction_instrument(inst%di,dso,status)
    endif
    if(cfound)then
       call IXFwrite_line('Direct Geometry chopper instrument will be defined',status)
       inst%inst_type=IXCdirect
       call IXFpopulate_chopper_instrument(inst%ci,dso,status)
    endif
    if(status == IXCseverity_error)return


    ! The population of IXTspectra and IXTdetector are intimately linked, therefore if populating from nexus file, 
    ! both objects must defined in the data_source object

    call IXFfindpath_data_source(dso,'spectra',spec_path,spec_obj_name,specfound,status)
    call IXFfindpath_data_source(dso,'detector',det_path,det_obj_name,detfound,status)


    if(specfound .and. detfound) then
       if(trim(adjustl(spec_obj_name)) /= IXCundef_char)then
          call IXFpopulate_file_spectra(inst%spectra,spec_path,status,spec_obj_name)
       else
          call IXFpopulate_file_spectra(inst%spectra,spec_path,status)
       endif
       call IXFwrite_line('instrument spectra object populated from '//spec_path,status)

       if(trim(adjustl(det_obj_name)) /= IXCundef_char)then
!          if(trim(adjustl(det_obj_name)) /= IXCdet_ref)then
!             ! load detector selectively from a detector.nxs file using detector numbers from IXTspectra
!             call IXFpopulate_reference_detector(inst%detector,det_path,inst%spectra,status)
!             call IXFwrite_line('instrument detector object populated selectively from '//det_path,status)
!          else
             ! load IXTdetector unselectively from a fully populated nexus file with a particular entry_name
             call IXFpopulate_file_detector(inst%detector,det_path,status,det_obj_name)
             call IXFwrite_line('instrument detector object populated from '//det_path,status)
!          endif
       else
          ! load IXTdetector unselectively from a fully populated nexus file, using default load
          call IXFpopulate_file_detector(inst%detector,det_path,status)
          call IXFwrite_line('instrument detector object populated from '//det_path,status)                
       endif
       if(status == IXCseverity_error)then
          return
       else
          call IXFmark_valid(inst)
          return
       endif
    endif

    if(specfound .and.(.not. detfound)) then
       ! warning messages, does not need to die horribly
       call IXFwrite_line(trim(adjustl(spec_path))//' ignored for population as no accompanying detector file present',status)
       call IXFwrite_line('spectra will be populated from the map file',status)
    endif


    call findusedspectra(inst,spe_used,status,det_map,mon_map)    
    !it is important that the IXTspectra  is populated before the detector
    ! this calls the interfaced/private subroutine populate_list_dso_isis in IXTspectra
    call IXFpopulate_spectra(inst%spectra,inputsource,spe_used,list_out,status)    
    deallocate(spe_used)

    if(detfound)then    
       if(trim(adjustl(det_obj_name)) /= IXCundef_char)then
          if(trim(adjustl(det_obj_name)) == IXCdet_ref)then
             ! load detector selectively from a detector.nxs file using detector numbers from IXTspectra
             call IXFpopulate_reference_detector(inst%detector,det_path,list_out,inputsource,status)
             if(status == IXCseverity_error)return
             call IXFwrite_line('instrument detector object populated selectively from '//det_path,status)
          else
             ! load IXTdetector unselectively from a fully populated nexus file with a particular entry_name
             call IXFwrite_line(trim(adjustl(det_path))//' not used to populate detector as no accompanying spectra file present',status)
             call IXFwrite_line('detector will be populated from the map file',status)
          endif
       else
          ! load IXTdetector unselectively from a fully populated nexus file, using default load
          call IXFwrite_line(trim(adjustl(det_path))//' not used to populate detector as no accompanying spectra file present',status)
          call IXFwrite_line('detector will be populated from the map file',status)
       endif
    else      
       ! populate the detector object without file input
       call IXFpopulate_detector(inst%detector,inputsource,list_out,status)
    endif
    deallocate(list_out)    
    if(status == IXCseverity_error)return
    call IXFmark_valid(inst)

  end subroutine IXFpopulate_instrument
  !-----------------------------------------------------------------------------------------------------------------------  
  subroutine pop_alloc_objects(inst,dso,status)
    implicit none
    type(IXTinstrument),intent(inout)::inst
    type(IXTdata_source),intent(in)::dso
    type(IXTstatus)::status    
    character(len=long_len),allocatable :: paths(:),obj_name(:)
    logical :: found    
    integer(i4b):: arr_len,i

    ! look for multiple instances of aperture object
    call IXFfindpaths_data_source(dso,'aperture',paths,obj_name,found,status)
    if(found)then
       arr_len=size(paths)    
       call IXFrealloc(inst%apertures,arr_len,.false.,status)   
       do i=1,arr_len
          if(trim(adjustl(obj_name(i))) /= IXCundef_char)then
             call IXFpopulate_file_aperture(inst%apertures(i),paths(i),status,obj_name(i))
          else
             call IXFpopulate_file_aperture(inst%apertures(i),paths(i),status)
          endif
       enddo
    endif

    ! look for multiple instances of attenuator object    
    call IXFfindpaths_data_source(dso,'attenuator',paths,obj_name,found,status)
    if(found)then
       arr_len=size(paths)    
       call IXFrealloc(inst%attenuators,arr_len,.false.,status)    
       do i=1,arr_len
          if(trim(adjustl(obj_name(i))) /= IXCundef_char)then
             call IXFpopulate_file_attenuator(inst%attenuators(i),paths(i),status,obj_name(i))
          else
             call IXFpopulate_file_attenuator(inst%attenuators(i),paths(i),status)
          endif
       enddo
    endif
  end subroutine pop_alloc_objects

  !-----------------------------------------------------------------------------------------------------------------------  
  ! this private subroutine will determin the independent used spectra given optional detector and/or  
  ! monitor map objects
  subroutine findusedspectra(inst,tot_used,status,det_map,mon_map)
    use IXMmap
    use IXMsort
    implicit none
    type(IXTinstrument),intent(in):: inst
    type(IXTmap),optional,intent(in)::det_map,mon_map
    type(IXTstatus)::status
    integer(i4b),allocatable::det_used(:),mon_used(:),tot_used(:),used(:),ranks(:)
    integer(i4b),pointer::dspecno(:),mspecno(:)   
    integer(i4b)::a,b,c,det_total,mon_total,total,counter,i 

    a=0
    b=0
    if(present(det_map))then
       a=1
       call IXFget_ptr_map(det_map,spec_no=dspecno)
       det_total=size(dspecno)
    else
       det_total=0
    endif
    if(present(mon_map))then
       b=2
       call IXFget_ptr_map(mon_map,spec_no=mspecno)
       mon_total=size(mspecno)
    else
       mon_total=0
    endif
    total=det_total+mon_total

    allocate(tot_used(total),ranks(total))

    c=a+b
    if(c==0) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
            IXCerr_invparam,'at least *one* map object *must* be present see DC (find_detectors)')
       return
    endif
    if(c==1)then
       tot_used(1:det_total)=dspecno
    endif
    if(c==2)then
       tot_used(1:mon_total)=mspecno
    endif
    if(c==3)then
       tot_used(1:det_total)=dspecno
       tot_used(det_total+1:total)=mspecno
    endif

    mspecno=>NULL()
    dspecno=>NULL()

    call IXFrank(tot_used,ranks)
    allocate(used(total))
    used=tot_used(ranks)    
    deallocate (ranks)    

    ! now rationalise used(:) back into tot_used(:) removing duplications
    tot_used=0
    tot_used(1)=used(1)
    counter=2
    do i=2,total
       if(used(i)/=used(i-1))then
          tot_used(counter)=used(i)
          counter=counter+1
       endif
    enddo
    call IXFreallocfortran(tot_used,counter-1,.true.,status)
    deallocate(used)

  end subroutine findusedspectra
  !******************************************  

  !-----------------------------------------------------------------------------------------------------------------------  
  ! this private subroutine will determin the independently used detectors given optional detector and/or  
  ! monitor map objects
  subroutine finduseddetectors(inst,tot_used,status,det_map,mon_map)
    use IXMmap
    use IXMsort
    implicit none
    type(IXTinstrument),intent(in):: inst
    type(IXTmap),optional,intent(in)::det_map,mon_map
    type(IXTstatus)::status
    integer(i4b),allocatable::det_used(:),mon_used(:),tot_used(:),used(:),ranks(:)
    integer(i4b),pointer::dspecno(:),mspecno(:)   
    integer(i4b)::a,b,c,det_total,mon_total,total,counter,i 

    a=0
    b=0
    if(present(det_map))then
       a=1
       call IXFget_ptr_map(det_map,spec_no=dspecno)
       call IXFgetdets_spectra(inst%spectra,dspecno,det_used,status)
       det_total=size(det_used)
    else
       det_total=0
    endif
    if(present(mon_map))then
       b=2
       call IXFget_ptr_map(mon_map,spec_no=mspecno)
       call IXFgetdets_spectra(inst%spectra,mspecno,mon_used,status)      
       mon_total=size(mon_used)
    else
       mon_total=0
    endif
    total=det_total+mon_total

    allocate(tot_used(total),ranks(total))

    c=a+b
    if(c==0) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
            IXCerr_invparam,'at least *one* map object *must* be present see DC (find_detectors)')
       return
    endif
    if(c==1)then
       tot_used(1:det_total)=det_used
    endif
    if(c==2)then
       tot_used(1:mon_total)=mon_used
    endif
    if(c==3)then
       tot_used(1:det_total)=det_used
       tot_used(det_total+1:total)=mon_used
    endif

    call IXFrank(tot_used,ranks)
    allocate(used(total))
    used=tot_used(ranks)
    if(a==1)deallocate (det_used)
    if(b==2)deallocate (mon_used)
    deallocate (ranks)

    ! now rationalise used(:) back into tot_used(:) removing duplications
    tot_used=0
    tot_used(1)=used(1)
    counter=2
    do i=2,total
       if(used(i)/=used(i-1))then
          tot_used(counter)=used(i)
          counter=counter+1
       endif
    enddo
    call IXFreallocfortran(tot_used,counter-1,.true.,status)
    deallocate(used)

  end subroutine finduseddetectors
  !******************************************
end module IXMinstrument
