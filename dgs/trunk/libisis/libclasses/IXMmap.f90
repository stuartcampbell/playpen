!------------------------------
! MODULE: IXMmap
!------------------------------
!! @author Dickon Champion, ISIS
!! @version $Revision: 1386 $ ($Date: 2008-05-20 04:26:17 -0400 (Tue, 20 May 2008) $)
!!
!! FORTRAN definition of IXMmap class
! This is the workspace to spectra bridge
module IXMmap
  use IXMbase
  use IXMsort
  use IXMdata_source
  implicit none	
  public :: IXTmap	
  type IXTmap
     private
     !! workspace->spectra elements
     type(IXTbase):: base
     integer(i4b),pointer::work_no(:)=>NULL() !! unique identifier
     integer(i4b),pointer::total_spec(:)=>NULL()
     integer(i4b),pointer::spec_ind(:)=>NULL()
     integer(i4b),pointer::spec_no(:)=>NULL() 
  end type IXTmap

#define IXD_TYPE map
#include "class_header.f90"

contains

#define IXD_DESCRIPTION	"IXTmap class"
#define IXD_TYPE map
#define IXD_SQTYPE 'map'
#include "class_base.f90"

  !-----------------------------------------------------------------------------------------------------------------------
  ! All classes must provide this operation; it loops through
  ! all members of the class doing the supplied operation
  recursive subroutine IXFoperation_run_map(op, field, arg, status)
    implicit none
    type(IXTmap) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTmap', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    ! this order must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run(op, 'base', arg%base, status)
    call IXFoperation_run_ptr(op,'work_no', arg%work_no, status)
    call IXFoperation_run_ptr(op,'total_spec',arg%total_spec,status)
    call IXFoperation_run_ptr(op,'spec_ind', arg%spec_ind, status)
    call IXFoperation_run_ptr(op,'spec_no',arg%spec_no,status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_map


  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFcheck will make internal consistency checks in the object, such as array length checking to make
  !! sure the object is properly filled.

  subroutine IXFcheck_map(arg, status)
    implicit none
    type(IXTmap) :: arg
    type(IXTstatus) :: status
    call IXFcheck_base(arg%base,status)
  end subroutine IXFcheck_map

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFcreate subroutine STRICTLY takes all the elements required to define a class and creates the resulting
  !! object. The IXTbase type is an optional argument, if it is not supplied the base type from the default IXTtestclass
  !! declaration will be used, it is therefore the last argument. If an element of the object is another class then
  !! it MUST be initialised. 

  subroutine IXFcreate_map(map,work_no,total_spec,spec_ind,spec_no,status)
    type(IXTmap) :: map
    type(IXTstatus) :: status
    integer(i4b),intent(in)::work_no(:),spec_ind(:),total_spec(:),spec_no(:)

    ! the set routine can ONLY be called on an initialised object
    ! so in this *special* case it is initialised before it is filled
    call IXFmark_valid(map)

    call IXFset_map(map,status,work_no,total_spec,spec_ind,spec_no)

  end subroutine IXFcreate_map

  !-----------------------------------------------------------------------------------------------------------------------
  !!IXFdestroy routine deallocates any pointer arrays in the type, and calls the destroy function on any nested
  !!objects, could also be used to set variables back to their default values.
  !!If one of the objects is an array of structures, then each structure will be recursively destroyed by the
  !!IXFdealloc function.

  subroutine IXFdestroy_map(map,status)
    implicit none
    type(IXTmap) :: map
    type(IXTstatus) :: status

    call IXFdestroy(map%base,status)
    call IXFdealloc(map%work_no,status)
    call IXFdealloc(map%total_spec,status)
    call IXFdealloc(map%spec_ind,status)
    call IXFdealloc(map%spec_no,status)

    call IXFclear_valid(map)

  end subroutine IXFdestroy_map

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFset operation can only be performed on a properly filled or initialised object. It takes an optional number of
  !! arguments to modify the object contents. A check is made at the end to determine that the edited object is correctly
  !! formed. Error flags are raised if there is any inconsistency. The optional arguments must always be specified by keywords.
  !! The order of arguments should match the order of declaration, except for the IXTbase type which will be the penultimate argument.
  !! The 'ref' argument is used to copy the values of a reference object to another. In this case the object being modified 
  !!does not have to be initialised, but the reference object must be initialised instead.

  recursive subroutine IXFset_map(map,status,work_no,total_spec,spec_ind,spec_no,ref)
    implicit none
    type(IXTmap),intent(inout) :: map
    type(IXTmap),intent(in),optional:: ref
    integer(i4b),optional,intent(in)::work_no(:)
    integer(i4b),optional,intent(in)::total_spec(:)
    integer(i4b),optional,intent(in)::spec_ind(:)

    integer(i4b),optional,intent(in)::spec_no(:)
    type(IXTstatus) :: status

    ! check that either the reference object is initialised
    ! or that object to be modified is initialised
    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_map)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(map)
    else    
       if(IXFvalid(map) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_map)')
       endif
       if(status == IXCseverity_error)return
    endif

    if(present(ref))call IXFset_map(map,status,ref%work_no,ref%total_spec,ref%spec_ind,&
         ref%spec_no)
         
    call IXFset_integer_array(map%work_no,status,work_no)    
    call IXFset_integer_array(map%total_spec,status,total_spec)    
    call IXFset_integer_array(map%spec_ind,status,spec_ind)    
    call IXFset_integer_array(map%spec_no,status,spec_no)    

    call IXFcheck_map(map,status)

  end subroutine IXFset_map


  !-----------------------------------------------------------------------------------------------------------------------
  !! The IXFget subroutine will return elements of an object to an optional supplied arrays/variables. The supplied variables
  !! should be referrred to by keyword to avoid errors. The 'wout' variable is special and can be used to copy the 
  !! contents of a whole object to a new one.

  subroutine IXFget_map(map, status,work_no,total_spec,spec_ind, spec_no,wout)
    implicit none
    type(IXTmap),intent(in) :: map
    type(IXTmap),optional,intent(out)::wout
    type(IXTstatus) :: status
    integer(i4b),optional,intent(out)::work_no(:)
    integer(i4b),optional,intent(out)::total_spec(:)
    integer(i4b),optional,intent(out)::spec_ind(:)
    integer(i4b),optional,intent(out)::spec_no(:)

    if (present(wout))call IXFcopy(map,wout,status)

    call IXFget_integer_array(map%work_no,status,work_no)    
    call IXFget_integer_array(map%total_spec,status,total_spec)    
    call IXFget_integer_array(map%spec_ind,status,spec_ind)    
    call IXFget_integer_array(map%spec_no,status,spec_no)    
 
  end subroutine IXFget_map

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFget_ptr will return a pointer to a structure or an array, from an optional argument.
  !! The pointer arguments are generally the same name as the object elements they are pointing to.
  !! Care must be taken since if the pointers are edited, then the data in the structure will also be edited.

  subroutine IXFget_ptr_map(map, work_no,total_spec,spec_ind,spec_no)
    implicit none
    type(IXTmap) :: map
    integer(i4b),optional,pointer::work_no(:),total_spec(:),spec_ind(:),spec_no(:)

    if (present(work_no))work_no=>map%work_no
    if (present(total_spec))total_spec=>map%total_spec
    if (present(spec_ind))spec_ind=>map%spec_ind
    if (present(spec_no))spec_no=>map%spec_no

  end subroutine IXFget_ptr_map

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFget_alloc will fill optionally supplied allocatable arrays with the data contained in the 
  !! object array elements. The supplied arrays can be either allocated or not. If they are the wrong
  !! length then they are adjusted accordingly. This is a routine only for internal Fortran use.

  subroutine IXFget_alloc_map(map,status,work_no,total_spec,spec_ind,spec_no,wout)
    implicit none            
    type(IXTmap),intent(in) :: map
    type(IXTmap),intent(out),optional :: wout
    integer(i4b),optional,allocatable::work_no(:)
    integer(i4b),optional,allocatable::total_spec(:)
    integer(i4b),optional,allocatable::spec_ind(:)
    integer(i4b),optional,allocatable::spec_no(:)
    type(IXTstatus) :: status

    if (present(work_no))then
       call IXFreallocdimsFortran(work_no,shape(map%work_no),.false.,status)
    endif
    if (present(total_spec))then
      call IXFreallocdimsFortran(total_spec,shape(map%total_spec),.false.,status)
    endif
    if (present(spec_ind))then
      call IXFreallocdimsFortran(spec_ind,shape(map%spec_ind),.false.,status)
    endif
    if (present(spec_no))then
      call IXFreallocdimsFortran(spec_no,shape(map%spec_no),.false.,status)
    endif

    call IXFget_map(map, status,work_no,total_spec,spec_ind, spec_no,wout)

  end subroutine IXFget_alloc_map
  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFfileread_mask will inspect an IXTdata_source object and if it finds an appropriate entry will populate the 
  !! given IXTmask object

  subroutine IXFread_dso_map(map,dso,status)
    implicit none
    type(IXTmap)::map
    type(IXTdata_source)::dso
    type(IXTstatus)::status
    character(len=long_len)::filepath,nullpath
    logical::found
    
    call IXFfindpath_data_source(dso,IXCmapfile,filepath,nullpath,found,status)
    
    if (found)then
      call IXFfileread_map(map,filepath,status)
    else
    ! perhaps some warning or informational message
    endif
    
  end subroutine IXFread_dso_map  
  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFfileread_map will fill an IXTmap object from a properly formatted map file
  subroutine IXFfileread_map(map,flname,status)
    implicit none
    type(IXTmap)::map
    type(IXTstatus)::status
    character(len=*)::flname
    integer(i4b),allocatable::total_spec(:),spec_ind(:),spec_no(:),work_no(:)

    !arrays are allocated in this subroutine
    call IXFread_map (flname,total_spec,spec_ind,spec_no,work_no,status)
     !now fill the object
    if (status == IXCseverity_error)return
    call IXFcreate_map(map,work_no,total_spec,spec_ind,spec_no,status)
    deallocate(total_spec,spec_ind,spec_no,work_no)

  end subroutine IXFfileread_map
!-----------------------------------------------------------------------------------------------------------------------
!! IXFread_map will populate allocatable arrays which fill an IXTmap object given a properly formatted mask file
  subroutine IXFread_map ( flname, nspec, spec_ind, specs, wn, status, wk_spec)

    ! Reads new format MAP file.
    !
    ! Input:
    !	flname		     ch		    Full name of MAP file
    !
    ! Output:  (nw = no. workspaces, tspec=total no. spectra)
    !	nspec(1:nw)		 int		No. spectra for each workspace
    !	spec_ind(1:nw)	 int		Index of start of spectra list in array SPECS
    !	specs(1:tspec)	 int		List of spectra for each workspace, in locations:
    !							        specs(spec_ind(iw)) -> specs(spec_ind(iw)+nspec(iw)-1)
    !                               and sorted into increasing order for each workspace
    !   wn(1:nw)         int        Workspace number of each workspace
    !   status           IXTstatus  Staus object
    ! * wk_spec(1:tspec) int        Workspace number for each spectrum 
    !
    character(len=*):: flname
    integer(i4b):: iunit, len_line
    integer(i4b),allocatable::spec_temp(:),wn(:),spec_ind(:),nspec(:)
    integer(i4b),allocatable::specs(:)
    integer(i4b),allocatable,optional::wk_spec(:)

    integer(i4b)::tspec,nw,is,mleft,iw,i,nval
    character (len=132):: line
    type(IXTstatus)::status

    call IXFfile_open(flname,'OLD',iunit,status)

    ! Read in nw, the number of workspaces
    call read_line (iunit, line, len_line)
    if (len_line <= 0) goto 999	! error reading from file
    i = 1
    nval = geti (line, i, nw) 
    if (nw < 1) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'ERROR: No. of workspaces in MAP file must be > 0 (IXFread_mapfile)')	
    call IXFfile_close(iunit)   
       return
    endif

    ! Read worksspace number and number of spectra spectra for each workspace:
    allocate(nspec(nw),wn(nw))
    is = 1
    do iw = 1, nw
       call read_line (iunit, line, len_line)
       if (len_line <= 0) goto 999
       i=1
       nval = geti (line, i, wn(iw))    !reads workspace number into array wn(:)
       if (nval /= 1) goto 999

       call read_line (iunit, line, len_line)
       if (len_line <= 0) goto 999
       i = 1
       nval = geti (line, i, nspec(iw)) !reads no of spectra into ns(:)
       if (nval /= 1) goto 999

       ! allocate an array to hold specs temporarily
       if (nspec(iw) /= 0) then
          allocate (spec_temp(nspec(iw)))
          mleft = nspec(iw)		! no. spectra left to read for this workspace
          do while (mleft > 0 )
             call read_line (iunit, line, len_line)
             if (len_line <= 0) goto 999
             i = 1
             nval = getis (line, i, spec_temp, mleft)
             mleft = mleft - nval
          enddo
          deallocate(spec_temp)
       endif
    end do

    ! Rewind file; now we know how many spectra in total, can allocate space and read in
    ! No need to error check, as succesfully read the whole file above.

    tspec=sum(nspec)
    allocate(spec_ind(nw),specs(tspec))
    if (present(wk_spec))allocate(wk_spec(tspec))

    rewind(iunit)
    call read_line (iunit, line, len_line) ! read line with nw in
    is = 1
    do iw = 1, nw
       call read_line (iunit, line, len_line) !read in line with # of spectra
       call read_line (iunit, line, len_line) !read in line with workspace #

       spec_ind(iw) = is !increments each time section of specnos read in until finished for each workspace

       !  read spectra (if not an empty workspace):
       if (nspec(iw) /= 0) then
          mleft = nspec(iw)		! no. spectra left to read for this workspace
          do while (mleft > 0)
             call read_line (iunit, line, len_line)
             i = 1
             nval = getis (line, i, specs(is:), mleft)
             ! fill section filled in specs with workspace number
             if (present(wk_spec))wk_spec(is:(is+nval-1))=wn(iw) ! this is not the index it is the actual workspace number			
             is = is + nval
             mleft = mleft - nval
          end do
          call IXFsort(specs(spec_ind(iw):spec_ind(iw)+nspec(iw)-1))    ! sort the block of spectra for (iw)th workspace
       endif
    end do
    call IXFfile_close(iunit)   
    return

    !-----------------------------------------------
    ! Error reading file:
999 continue
    call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
         IXCerr_invparam, 'Error reading ASCII MAP file (IXFread_mapfile)')	
    call IXFfile_close(iunit)   
    return
  end subroutine IXFread_map
 !  !-----------------------------------------------------------------------------------------------------------------------
 
  subroutine IXFwkspec_map(map,wk_spec)
    implicit none
    type(IXTmap)::map
    integer(i4b),allocatable::wk_spec(:)
    integer(i4b)::i
    allocate (wk_spec(sum(map%total_spec)))
    do i=1,size(map%work_no)
      wk_spec(map%spec_ind(i):map%spec_ind(i)+map%total_spec(i)-1)=map%work_no(i)
    enddo   
  end subroutine IXFwkspec_map
!  !-----------------------------------------------------------------------------------------------------------------------  
  subroutine IXFpopulate_map(map,fpath,status)
    use IXMraw_file
    implicit none
    type(IXTmap),intent(out)::map
    type(IXTstatus)::status
    integer(i4b)::file_type
    character(len=long_len),intent(in)::fpath
    
    file_type=IXFfile_type(IXFtranslate(fpath)) 
    if (iand(file_type,IXCfile_type_ascii).ne. 0)then
        ! run ascii read process
       call IXFfileread_map(map,fpath,status)
       if(status == IXCseverity_error)return
    else 
       if(iand(file_type,IXCfile_type_binary).ne. 0)then
          ! nexus/xml read process
          call IXFpopulate_file_map(map,fpath,status)
          if (status == IXCseverity_error)return
       endif
    endif
      
  end subroutine IXFpopulate_map
!  !-----------------------------------------------------------------------------------------------------------------------
  subroutine IXFrawfile_popmon_map(map,inputsource,status)
    use IXMraw_file  
    implicit none
    type(IXTmap),intent(out)::map
    type(IXTraw_file),intent(in)::inputsource
    type(IXTstatus)::status
    integer(i4b)::nmon,i,ndet
    integer(i4b),allocatable::mdet(:),spec(:),marray(:)
    call IXFget_raw(inputsource, 'NDET', ndet, status)
    call IXFget_raw(inputsource, 'NMON', nmon, status)
    allocate(mdet(nmon),spec(ndet))    
    call IXFget_raw(inputsource, 'SPEC', spec, status)
    call IXFget_raw(inputsource, 'MDET', mdet, status)
  
    allocate(marray(nmon))
    !this fills the marray with the spectrum numbers of the monitors
    do i=1,nmon
       marray(i)=spec(mdet(i))
    enddo
    deallocate(spec,mdet)

!this is a 1:1 mapping
    call IXFalloc(map%work_no,nmon,status)
    call IXFalloc(map%total_spec,nmon,status)
    call IXFalloc(map%spec_ind,nmon,status)
    call IXFalloc(map%spec_no,nmon,status)
    
    map%work_no=marray
    map%spec_no=marray
    map%spec_ind=(/(i,i=1,nmon)/)
    map%total_spec=1
    deallocate(marray)
    call IXFmark_valid(map)
    
  end subroutine IXFrawfile_popmon_map

  subroutine IXFrawfile_popdet_map(map,inputsource,status)
    use IXMraw_file
    implicit none
    type(IXTmap),intent(out)::map
    type(IXTraw_file),intent(in)::inputsource
    type(IXTstatus)::status
    
    integer(i4b)::nspec,nmon,ndets,i,detectorzeros,narray
    integer(i4b),allocatable::mdet(:),spec(:),marray(:),darray(:),temp(:),mask(:),ndet(:)

    call IXFget_raw(inputsource, 'NSP1', nspec, status)
    call IXFget_raw(inputsource, 'NDET', ndets, status)
    call IXFget_raw(inputsource, 'NMON', nmon, status)
    allocate(mdet(nmon),spec(ndets))    
    call IXFget_raw(inputsource, 'SPEC', spec, status)
    call IXFget_raw(inputsource, 'MDET', mdet, status)
  
    allocate(marray(nmon))
    !this fills the marray with the spectrum numbers of the monitors
    do i=1,nmon
       marray(i)=spec(mdet(i))
    enddo

    ! now we know the spectrum numbers of the monitors we create a mask to remove 
    ! them from contributing spectra to the detectors, even if they have no detectors
    ! wired to them
    allocate (temp(nspec),mask(nspec),ndet(nspec))
    temp=(/ (i,i=1,nspec) /)
    mask=1
    ndet=0
    !removes spectrum numbers of monitors from list of used spectra
    mask(marray)=0      
    !we also need to remove all entries where spectra%ndet=0
    !determine what would be spectra%ndet on the fly
    do i=1,ndets
       if(spec(i) > 0) ndet(spec(i))=ndet(spec(i))+1
    enddo


      detectorzeros=0
      do i=1,nspec
        if(ndet(i)==0)then
 !it's assumed all monitor spectra have a detector pointing at them
              mask(i)=0
              detectorzeros=detectorzeros+1
        endif
      enddo
      deallocate(ndet)
      !**
      narray=nspec-nmon-detectorzeros      
      allocate(darray(narray))     
      darray=pack(temp,mask /=0)
      deallocate(temp,mask,mdet,spec,marray)

! 1:1 mapping
    call IXFalloc(map%work_no,narray,status)
    call IXFalloc(map%total_spec,narray,status)
    call IXFalloc(map%spec_ind,narray,status)
    call IXFalloc(map%spec_no,narray,status)
    
    map%work_no=darray
    map%spec_no=darray
    map%spec_ind=(/(i,i=1,narray)/)
    map%total_spec=1
    deallocate(darray)
    call IXFmark_valid(map)
    
  end subroutine IXFrawfile_popdet_map
     
  subroutine IXFverify_period_map(Vmap,period,rawfile,status)
    use IXMraw_file
    implicit none
    integer(i4b),intent(in)::period
    integer(i4b)::nper,nspec
    type(IXTraw_file),intent(in)::rawfile
    type(IXTmap),intent(in)::Vmap
    type(IXTstatus)::status

    call IXFget_raw_file(rawfile,status,nper=nper,nsp1=nspec)
    
    
    if(period > nper) call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'invalid period for rawfile(IXFverify_period_map)') 
    
    if(period == 0)then
    !spectra spanning periods can be specified in the map file
      if( maxval(Vmap%spec_no) > (nper*(nspec+1))) call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'invalid period and mapfile(IXFverify_period_map)') 
      !add status
    else
    ! all spectra specified must be contained in one period
    ! don't need to worry about spectrum zero 
      if( maxval(Vmap%spec_no) > nspec) call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'invalid period and mapfile(IXFverify_period_map)') 
      !add status
    endif    
          
  end subroutine IXFverify_period_map

!! IXFverify_spectra_map
!! if necessary this function will determine whether a spectrum has been defined in more than once in the
!! same workspace or another workspace. this is a time consuming way of doing it but there is no other way
  pure logical function IXFverify_spectra_map(Vmap)result(verified)
    implicit none    
    type(IXTmap),intent(in)::Vmap    
    integer(i4b),allocatable::times(:)
    integer(i4b)::i
    verified=.true.
    allocate(times(maxval(Vmap%spec_no)))
    times=0
    do i=1,size(Vmap%spec_no)
       times(Vmap%spec_no(i))=times(Vmap%spec_no(i))+1
       if(times(Vmap%spec_no(i)) > 1)then
         verified=.false.
         return
       endif
    enddo
    deallocate(times)
  end function IXFverify_spectra_map  
end module IXMmap
