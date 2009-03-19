!------------------------------
! MODULE: IXMmask
!------------------------------
!! @author Dickon Champion, ISIS
!! @version $Revision: 1193 $ ($Date: 2007-08-17 04:06:25 -0400 (Fri, 17 Aug 2007) $)
!!
!! FORTRAN definition of  IXMmask object 



module IXMmask
  use IXMtype_definitions
  use IXMbase
  use IXMtools
  use IXMsort
  use IXMdata_source
  implicit none
  public :: IXTmask
  type IXTmask
     private
     type(IXTbase)::base
     integer(i4b) ,pointer::mask_array(:)=>NULL()

  end type IXTmask

#define IXD_TYPE mask
#include "class_header.f90"

contains

#define IXD_DESCRIPTION	"IXTmask class"
#define IXD_TYPE mask
#define IXD_SQTYPE 'mask'
#include "class_base.f90"

  !-----------------------------------------------------------------------------------------------------------------------
  !!IXFdestroy routine deallocates any pointer arrays in the type, and calls the destroy function on any nested
  !!objects, could also be used to set variables back to their default values.
  !!If one of the objects is an array of structures, then each structure will be recursively destroyed by the
  !!IXFdealloc function.
  subroutine IXFdestroy_mask(arg, status)
    implicit none
    type(IXTmask) :: arg
    type(IXTstatus) :: status
    call IXFdestroy(arg%base,status)
    call IXFdealloc(arg%mask_array,status)    
    call IXFclear_valid(arg)
  end subroutine IXFdestroy_mask

  recursive subroutine IXFoperation_run_mask(op, field, arg, status)
    implicit none
    type(IXTmask) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTmask', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    ! this order must match the declarion order in matlab as it is
    ! used when parsing arguments passed in class creation with varargin
    call IXFoperation_run(op, 'base', arg%base, status)
    call IXFoperation_run_ptr(op,'mask_array', arg%mask_array, status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_mask

  !-----------------------------------------------------------------------------------------------------------------------
  !! The IXFget subroutine will return elements of an object to an optional supplied arrays/variables. The supplied variables
  !! should be referrred to by keyword to avoid errors. The 'wout' variable is special and can be used to copy the 
  !! contents of a whole object to a new one.

  subroutine IXFget_mask(mask,status,mask_array,wout)
    type(IXTmask),intent(in)::mask
    type(IXTmask),optional,intent(out)::wout
    integer(i4b),optional,intent(out)::mask_array(:)
    type(IXTstatus),intent(inout)::status

    if(present(wout))call IXFcopy(mask,wout,status)

    if(present(mask_array))then 
       if(size(mask_array) == size(mask%mask_array))then
          mask_array=mask%mask_array
       else
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'supplied array must be correct size to receive mask_array array (IXFget_spectra)')
       endif
    endif

  end subroutine IXFget_mask
  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFcreate subroutine STRICTLY takes all the elements required to define a class and creates the resulting
  !! object. The IXTbase type is an optional argument, if it is not supplied the base type from the default IXTtestclass
  !! declaration will be used, it is therefore the last argument. If an element of the object is another class then
  !! it MUST be initialised. 

  subroutine IXFcreate_mask(mask,mask_array,status)
    type(IXTmask),intent(out)::mask
    integer(i4b),optional,intent(in)::mask_array(:)
    type(IXTstatus),intent(inout)::status

    call IXFmark_valid(mask)
    call IXFset_mask(mask,status,mask_array)
  end subroutine IXFcreate_mask
  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFset operation can only be performed on a properly filled or initialised object. It takes an optional number of
  !! arguments to modify the object contents. A check is made at the end to determine that the edited object is correctly
  !! formed. Error flags are raised if there is any inconsistency. The optional arguments must always be specified by keywords.
  !! The order of arguments should match the order of declaration, except for the IXTbase type which will be the penultimate argument.
  !! The 'ref' argument is used to copy the values of a reference object to another. In this case the object being modified 
  !!does not have to be initialised, but the reference object must be initialised instead.

  recursive subroutine IXFset_mask(mask,status,mask_array,ref)
    type(IXTmask),intent(inout)::mask
    type(IXTmask),intent(in),optional::ref
    integer(i4b),optional,intent(in)::mask_array(:)
    type(IXTstatus),intent(inout)::status

    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_mask)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(mask)
    else    
       if(IXFvalid(mask) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_mask)')
       endif
       if(status == IXCseverity_error)return
    endif

    if(present(ref))call IXFset_mask(mask,status,ref%mask_array)

    call IXFset_integer_array(mask%mask_array,status,mask_array)
    
    call IXFcheck_mask(mask,status)

  end subroutine IXFset_mask

  !-----------------------------------------------------------------------------------------------------------------------  
  !! IXFcheck will make internal consistency checks in the object, such as array length checking to make
  !! sure the object is properly filled.

  subroutine IXFcheck_mask(w1, status)
    implicit none
    type(IXTmask) :: w1
    type(IXTStatus) :: status
    ! there is nothing to check since it is a single pointer array of indeterminate length
  end subroutine IXFcheck_mask

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFget_ptr will return a pointer to a structure or an array, from an optional argument.
  !! The pointer arguments are generally the same name as the object elements they are pointing to.
  !! Care must be taken since if the pointers are edited, then the data in the structure will also be edited.

  subroutine IXFget_ptr_mask(mask, mask_array)
    implicit none
    type(IXTmask) :: mask
    integer(i4b),optional,pointer::mask_array(:)

    if (present(mask_array))mask_array=>mask%mask_array

  end subroutine IXFget_ptr_mask

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFget_alloc will fill optionally supplied allocatable arrays with the data contained in the 
  !! object array elements. The supplied arrays can be either allocated or not. If they are the wrong
  !! length then they are adjusted accordingly. This is a routine only for internal Fortran use.

  subroutine IXFget_alloc_mask(mask,status, mask_array,wout)
    implicit none
    type(IXTmask),intent(in) :: mask
    type(IXtmask),intent(out)::wout
    integer(i4b),optional,allocatable::mask_array(:)
    type(IXTstatus)::status

    if (present(mask_array))then
       call IXFreallocdimsFortran(mask_array,shape(mask%mask_array),.false.,status)
    endif
    call IXFget_mask(mask,status,mask_array,wout)
  end subroutine IXFget_alloc_mask

  !-----------------------------------------------------------------------------------------------------------------------
  !!IXFread_dso_mask will inspect an IXTdata_source object and if it finds an appropriate entry will populate the 
  !! given IXTmask object
  ! HAS POTENTIAL to be templated

  subroutine IXFread_dso_mask(mask,dso,status)
    implicit none
    type(IXTmask)::mask
    type(IXTdata_source)::dso
    type(IXTstatus)::status
    character(len=long_len)::filepath,nullpath
    logical::found
    
    call IXFfindpath_data_source(dso,IXCmaskfile,filepath,nullpath,found,status)
    
!    if (found)then
!      call IXFfileread_mask(mask,filepath,status)
!      call IXFwrite_line('Mask file loaded from '//filepath,status)
!    else
!    ! load default file settings
!      call IXFfindpath_data_source(dso,filepath,IXCdefmask,found,status)
!      if (found)then
!        call IXFfileread_mask(mask,filepath,status)
!      ! say that this is what you are doing
!        call IXFwrite_line('Default mask file loaded from '//filepath,status)
!      else
!      ! status call to say error no file found
!      call IXFwrite_line('no file found',status)
!      endif
!    endif
    
  end subroutine IXFread_dso_mask
 
   !-----------------------------------------------------------------------------------------------------------------------
  !!IXFread_dso_mask will inspect an IXTdata_source object and if it finds an appropriate entry will populate the 
  !! given IXTmask object
! HAS POTENTIAL to be templated
  subroutine IXFreadgen_dso_mask(mask,dso,gentype,status)
    implicit none
    type(IXTmask)::mask
    type(IXTdata_source)::dso
    type(IXTstatus)::status
    character(len=long_len)::filepath
    character(len=*)::gentype !! can be a name to describe different maskfiles ie pink/blue/yellow
    logical::found
    
!    call IXFfindpath_data_source(dso,filepath,gentype,found,status)
    
!    if (found)then
!      call IXFfileread_mask(mask,filepath,status)
!      call IXFwrite_line('Mask file loaded from '//filepath,status)
!    else
!    ! load default file settings
!      call IXFfindpath_data_source(dso,filepath,IXCdefmask,found,status)
!      call IXFfileread_mask(mask,filepath,status)
!      ! say that this is what you are doing
!      call IXFwrite_line('Default mask file loaded from '//filepath,status)
!    endif
!    
  end subroutine IXFreadgen_dso_mask
 
 
  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFfileread_mask will populate an IXTmask object given a properly formatted mask file
  
  subroutine IXFfileread_mask(mask,flname,status)
    implicit none
    type(IXTmask)::mask
    integer(i4b),allocatable::mask_array(:)
    character(len=*)::flname !! full path to the name of the file used to populate the mask object    
    type(IXTstatus)::status
    
    !arrays are allocated in this subroutine
    call IXFread_mask(flname,mask_array,status)
    !now fill the object
    if (status == IXCseverity_error)return    
    call IXFcreate_mask(mask,mask_array,status)
    deallocate(mask_array)

  end subroutine IXFfileread_mask
!-----------------------------------------------------------------------------------------------------------------------
!! IXFread_mask will populate allocatable arrays which fill an IXTmask object given a properly formatted mask file
  
  subroutine IXFread_mask(flname,mask_array,status)
    implicit none
    character(len=*)::flname !! full path to the name of the file used to populate the mask object
    type(IXTstatus)::status
    integer(i4b),allocatable::mask_array(:),temp(:),ranks(:)
    character (len=132):: line
    integer(i4b)::iunit,istatus,len_line,nval,nbspec,i,fileend,is
    istatus=0
    nbspec=0
    fileend=0
    !dummy is an array to read dummy variables into
    ! it is assumed that no more than 10000 spectra will be declared
    ! on *EACH LINE* as being bad
    allocate(temp(10000))
!    call unitno (iunit)
!    open (unit=iunit, file=flname, iostat=istatus, status='OLD', action='READ')
!
!    if (istatus /= 0) then
!       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
!            IXCerr_invparam, 'ERROR: Unable to open MASK file (IXFread_mask)')	
!       !		call homer_message ('ERROR','Unable to open MAP file :',flname)
!       return
!    endif
call IXFfile_open(flname,'OLD',iunit,status)

if(status == IXCseverity_error)return
    do while(fileend==0)
       ! read spectra until the end of the file
       call read_line (iunit, line, len_line)
       i=1
       nval = getis (line, i, temp, 10000)
       nbspec=nbspec+nval	 
       if(len_line == EOF)fileend=1	 
    end do

    deallocate(temp)          
    rewind (iunit)
    allocate(temp(nbspec))
    ! now we know the number of bad spectra, read into temp array
    is=1
    do while(nbspec > 0)
       call read_line (iunit, line, len_line)
       i=1
       nval = getis (line, i, temp(is:), nbspec)
       is=is+nval
       nbspec=nbspec-nval
    end do
    call IXFfile_close(iunit)   
    ! sort order of bad spectra, to optimise populate of spectra module
    allocate(mask_array(is-1),ranks(is-1))
    call IXFrank(temp,ranks)
    mask_array=temp(ranks)
    deallocate(ranks,temp)      
  end subroutine IXFread_mask
  
!  !-----------------------------------------------------------------------------------------------------------------------  
  subroutine IXFpopulate_mask(mask,fpath,status)
    use IXMraw_file
    implicit none
    type(IXTmask),intent(out)::mask
    type(IXTstatus)::status
    integer(i4b)::file_type
    character(len=long_len),intent(in)::fpath
    
    file_type=IXFfile_type(IXFtranslate(fpath)) 
    if (iand(file_type,IXCfile_type_ascii).ne. 0)then
        ! run ascii read process
          call IXFfileread_mask(mask,fpath,status)
       if(status == IXCseverity_error)return
    else 
       if(iand(file_type,IXCfile_type_binary).ne. 0)then
          ! nexus/xml read process
          call IXFpopulate_file_mask(mask,fpath,status)                     
          if (status == IXCseverity_error)return
       endif
    endif
      
  end subroutine IXFpopulate_mask  
  
end module IXMmask

