!------------------------------
! MODULE: IXMpath
!------------------------------
!! @author Dickon Champion, ISIS
!! @version $Revision: 1091 $ ($Date: 2007-04-13 15:48:23 +0100 (Fri, 13 Apr 2007) $)
!!
!! FORTRAN definition of IXMpath class
module IXMpath
  use IXMtools
  use IXMmemory
  implicit none	
  public:: IXTpath
  type IXTpath
     character(len=long_len)::name
     character(len=long_len),allocatable::directory(:)
     integer(i4b)::counter = 0
  end type IXTpath
  !these options are duplicated from IXMfileio
  integer, parameter :: IXC_READ=1, IXC_WRITE=2, IXC_RDWR=IXC_READ+IXC_WRITE, IXC_CREATE=4, IXC_CREATEXML=8  
!  integer(i4b),parameter,private:: IXCdir_initlength=5 !!initial length of array list elements
  integer(i4b),parameter,private:: IXCgpath_initlength=5 !!initial length of array list elements
  type(IXTpath),private,allocatable::g_paths(:)
  integer(i4b),private::counter=0
  
  interface IXFdestroy_path
    module procedure IXFdestroy_array_path, IXFdestroy_path
  end interface
  interface IXFcheck
    module procedure IXFcheck_path, IXFcheck_array_path
  end interface
  interface IXFtranslate
    module procedure IXFtranslate_read, IXFtranslate_write
  end interface
  interface IXFcheck_and_valid
     module procedure IXFcheck_and_valid_path,IXFcheckarray_and_valid_path
  end interface
   
contains

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFcheck will make internal consistency checks in the object, such as array length checking to make
  !! sure the object is properly filled.

   subroutine IXFcheck_path(arg, status)
    implicit none
    type(IXTpath),intent(in) :: arg
    type(IXTstatus) :: status
    ! nested objects should be tested for initialisation, this shows they have been created properly   
    if( .not. allocated(arg%directory))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'directory list unallocated (IXFcheck_path)')
       return
    endif
    if(arg%counter /= size(arg%directory))then
        call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'length of directory incommensurate with its counter (IXFcheck_path)')
    endif
          
  end subroutine IXFcheck_path

  subroutine IXFcheck_array_path(w1,s)
    implicit none
    type(IXTpath),intent(in)::w1(:)
    type(IXTstatus)::s
    integer :: i
    do i = 1, size(w1)
       call IXFcheck_path(w1(i), s)
    end do
  end subroutine IXFcheck_array_path

  subroutine IXFcheck_and_valid_path  (arg,s)
    implicit none
    type(IXTpath),intent(in)::arg
    type(IXTstatus)::s
    call IXFcheck(arg,s)
  end subroutine IXFcheck_and_valid_path

  subroutine IXFcheckarray_and_valid_path  (arg,s)
    implicit none
    type(IXTpath),intent(in)::arg(:)
    type(IXTstatus)::s
    call IXFcheck(arg,s)      
  end subroutine IXFcheckarray_and_valid_path

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFcreate subroutine STRICTLY takes all the elements required to define a class and creates the resulting
  !! object. The IXTbase type is an optional argument, if it is not supplied the base type from the default IXTtestclass
  !! declaration will be used, it is therefore the last argument. If an element of the object is another class then
  !! it MUST be initialised. 

  subroutine IXFcreate_path(path,name,directory, status)
    implicit none
    type(IXTpath) :: path
    type(IXTstatus) :: status
    character(len=*),intent(in)::name
    character(len=*),allocatable,intent(in)::directory(:)

    ! nested objects should be tested for initialisation, this shows they have been created properly   
    if( .not. allocated(directory))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'directory list unallocated (IXFcreate_path)')
    endif
    if(status == IXCseverity_error)return

    call IXFset_path(path,status,name,directory)

  end subroutine IXFcreate_path

!! the IXFcheck routine for IXTpath is kept in IXM operation as it is a special module which does 
!! NOT contain an IXTbase object


  !-----------------------------------------------------------------------------------------------------------------------
  !!IXFdestroy routine deallocates any pointer arrays in the type, and calls the destroy function on any nested
  !!objects, could also be used to set variables back to their default values.
  !!If one of the objects is an array of structures, then each structure will be recursively destroyed by the
  !!IXFdealloc function.

  subroutine IXFdestroy_path(path,status)
    implicit none
    type(IXTpath) :: path
    type(IXTstatus) :: status
    
    path%name=''
    if(allocated(path%directory))deallocate (path%directory)
    path%counter=0

    ! the initialised status is now revoked for the object
    ! this statement MUST exist in all destroy routines

  end subroutine IXFdestroy_path
  
  subroutine IXFdestroy_array_path(path,status)
    implicit none
    type(IXTpath) :: path(:)
    type(IXTstatus) :: status
    integer(i4b)::i
    
    do i=1,size(path)
      call IXFdestroy_path(path(i),status)
    enddo
    
    
    ! the initialised status is now revoked for the object
    ! this statement MUST exist in all destroy routines

  end subroutine IXFdestroy_array_path

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFset operation can only be performed on a properly filled or initialised object. It takes an optional number of
  !! arguments to modify the object contents. A check is made at the end to determine that the edited object is correctly
  !! formed. Error flags are raised if there is any inconsistency. The optional arguments must always be specified by keywords.
  !! The order of arguments should match the order of declaration, except for the IXTbase type which will be the penultimate argument.
  !! The 'ref' argument is used to copy the values of a reference object to another. In this case the object being modified 
  !!does not have to be initialised, but the reference object must be initialised instead.

  recursive subroutine IXFset_path(path,status,name,directory,ref)
    implicit none
    type(IXTpath),intent(inout) :: path
    type(IXTpath),intent(in),optional:: ref
    character(len=*),optional,intent(in)::name
    character(len=*),optional,intent(in)::directory(:)
    type(IXTstatus) :: status
    character(len=long_len)::name_low

    ! check that either the reference object is initialised
    ! or that object to be modified is initialised

    if(present(ref))call IXFset_path(path,status,ref%name,ref%directory)
    
    if(present(name))then
      name_low=name
      call locase(name_low)
      path%name=trim(adjustl(name_low))
    endif
    
    if(present(directory))then
       call IXFreallocFortran(path%directory, size(directory),.false.,status)
       path%directory=directory
       path%counter=size(directory)
    endif
    
    call IXFcheck_path(path,status)

  end subroutine IXFset_path

  !-----------------------------------------------------------------------------------------------------------------------
  !! The IXFget subroutine will return elements of an object to an optional supplied arrays/variables. The supplied variables
  !! should be referrred to by keyword to avoid errors. The 'wout' variable is special and can be used to copy the 
  !! contents of a whole object to a new one.

  subroutine IXFget_path(path, status,name,directory,wout)
    implicit none
    type(IXTpath),intent(in) :: path
    type(IXTpath),optional,intent(out)::wout
    type(IXTstatus) :: status
    character(len=*),optional,intent(out)::name
    character(len=*),optional,allocatable,intent(out)::directory(:)

    if (present(wout))call IXFset_path(wout,status,ref=path)
    if(present(name))name=path%name
    if(present(directory))then
      call IXFreallocdimsfortran(directory,(/ size(path%directory) /),.false.,status)
      directory=path%directory
    endif
    
! some extra things wrt allocatable arrays of strings
! the equivalent of reallocfortran for strings 

  end subroutine IXFget_path
!! IXFtranslate_read_path will determine the existence or not of a file by translating a 'name:::file.txt' filepath string to 
!! all possible real paths eg 'C:\banana\file.txt' using the global g_paths object
!-----------------------------------------------------------------------------------------------------------------------  
  function IXFtranslate_read(fpath) result(elab_path)
    implicit none
    character(len=*),intent(in)::fpath
    character(len=long_len)::elab_path,test_path
    logical::existence
    integer(i4b)::colons,index,i
    ! try it without any change first
    !g_paths is empty
    if(counter==0)then
      elab_path=fpath
      return
    endif    
    colons=0
    index=0    
    inquire(file=trim(adjustl(fpath)),exist=existence)
    if(existence)then
      elab_path=fpath
      return    
    else
      !try and translate the file path
      colons=loc_ch(fpath,1,':::')
      if(colons == 0)then !there is no hope for you
        elab_path=fpath
        return
      endif
      call IXFfindgname_path(fpath(1:colons-1),index)
      if (index == 0)then
         elab_path=fpath
         return
      else    
         do i=1,g_paths(index)%counter
           test_path=trim(adjustl(g_paths(index)%directory(i)))//'/'//trim(adjustl(fpath(colons+3:len(fpath))))
           inquire(file=test_path,exist=existence)
           if(existence)then
             elab_path=trim(adjustl(test_path))
             return
           endif
         enddo
      endif      
    endif
    if(.not. existence) then
      elab_path=fpath
    endif 
    
  end function
!! IXFtranslate_write_path will determine the existence of a valid directory for writing a file  by translating a 'name:::file.txt' filepath string to 
!! the first  real path eg 'C:\banana\file.txt' using the global g_paths object
!-----------------------------------------------------------------------------------------------------------------------  
  function IXFtranslate_write(fpath,mode) result(elab_path)
    implicit none
    character(len=*),intent(in)::fpath
    integer(i4b),intent(in)::mode
    character(len=long_len)::elab_path,test_path,filetest_path
    logical::existence
    integer(i4b)::colons,index,i,ifail,iunit
    !g_paths is empty    
    if(counter==0)then
      elab_path=fpath
      return
    endif
    index=0
    colons=0
    if(mode==IXC_CREATE .or. mode==IXC_WRITE)then
      colons=loc_ch(fpath,1,':::')
      if(colons==0)then !there is no path to elaborate
        elab_path=fpath
        return
      endif
      call IXFfindgname_path(fpath(1:colons-1),index)
      if (index == 0)then !path doesn't exist
         elab_path=fpath
         return
      else    
         do i=1,g_paths(index)%counter
         ! this is a dirty hack to check the existence of a directory because 
         ! inquire(directory=test_path,exist=existence)
         ! will not work in g95 as it is non-standard
         ! so we seee if we can create a file, then if succesful delete it and return
         ! the elaborated path         
           test_path=trim(adjustl(g_paths(index)%directory(i)))
           filetest_path=trim(adjustl(test_path))//'/junk.txt'
           !call IXFunitno(iunit)
           open(IOSTAT=ifail,unit=99,file=filetest_path,STATUS='NEW')
           if (ifail /= 0)then
              existence=.false.
              close(99,status='delete')
           else
!           inquire(directory=test_path,exist=existence)
!           if(existence)then             
             elab_path=trim(adjustl(test_path))//'/'//trim(adjustl(fpath(colons+3:len(fpath))))
             close(99,status='delete')
             return
           endif
         enddo
      endif
      if(.not. existence) then
      elab_path=fpath
      endif 
    else
      elab_path=IXFtranslate(fpath)
    endif
    
  end function
  
    !-----------------------------------------------------------------------------------------------------------------------
  !! IXFfindgname_path will determine the index of the array which contains the directories for a particular name in the g_paths object
  
  subroutine IXFfindgname_path(name,index)
    implicit none    
    character(len=*), intent(in) :: name !!datatype which is checked against entries in datatype(:) array
    integer(i4b),intent(out)::index
    character(len=long_len) :: name_lower
    integer(i4b)::i
    index=0
    name_lower = name
    call locase(name_lower)
    do i=1,counter !module variable: counter 
      if(trim(adjustl(name_lower)) == trim(adjustl(g_paths(i)%name))) then
        index=i
      endif
    enddo
  end subroutine IXFfindgname_path
  
!! IXFaddpath will create a searchpath entry in the global g_paths object with 'name' with 'directory(:)' entries if it does not exist already
  !-----------------------------------------------------------------------------------------------------------------------
  subroutine IXFaddpath(name,directory,status)
    implicit none
    character(len=*),intent(in)::name
    character(len=*),intent(in)::directory(:)
    integer(i4b)::index
    type(IXTstatus) :: status    

    if(counter /=0)then
      call IXFfindgname_path(name,index)
      if(index /= 0)then
        call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
            IXCerr_invparam, 'path name '''//trim(adjustl(name))//''' already exists in global path, use IXFaddtoend/IXFaddtobeg instead(IXFaddpath)')      
        return
      endif
    endif
    
    call IXFrealloc_path(g_paths,counter+1,.true.,status)
    counter=counter+1    
    call IXFset_path(g_paths(counter),status,name,directory)
    
  end subroutine IXFaddpath
  
!! IXFaddtoend will add 'directory(:)' to the end of the directory list for the searchpath entry 'name' in the global g_paths object 
  !-----------------------------------------------------------------------------------------------------------------------
  subroutine IXFaddtoend(name,directory,status)
    implicit none
    character(len=*),intent(in)::name
    character(len=*),intent(in)::directory(:)
    integer(i4b)::index
    type(IXTstatus) :: status    
    if(counter==0)return    
    call IXFfindgname_path(name,index)
    if (index == 0)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
            IXCerr_invparam, 'path name '''//trim(adjustl(name))//''' not found in global path(IXFaddtoend)')      
       return
    else    
       call IXFaddtoend_path(g_paths(index),directory,status)  
    endif
  
  end subroutine IXFaddtoend
  !-----------------------------------------------------------------------------------------------------------------------
  
  !! IXFaddtoend_path will add 'directory(:)' to the end of the directory list for a given IXTpath object 
  subroutine IXFaddtoend_path(path,directory,status)
    implicit none
    type(IXTpath)::path
    character(len=*),intent(in)::directory(:)
    character(len=long_len)::trim_direc(size(directory))
    type(IXTstatus)::status
    integer(i4b)::i,dir_length
    if(counter==0)return
    dir_length=size(directory)
    do i=1,dir_length
      trim_direc(i)=trim(adjustl(directory(i)))
    enddo       
    ! the first time it is created the directory array is set to the length of the incoming array
    ! now we will allocate it in sections to avoid over use of reallocation subroutine
    if ( path%counter == size(path%directory) )then
    ! the array is full so add some more space to the array list
      call IXFreallocdimsFortran(path%directory,(/ size(path%directory)+dir_length /),.true.,status)
    endif    
    !   add new entries to the list and increment counter
    path%directory(path%counter+1:path%counter+dir_length)=trim_direc
    path%counter=path%counter+dir_length

    call IXFcheck_path(path,status)
  
  end subroutine IXFaddtoend_path
  !-----------------------------------------------------------------------------------------------------------------------
 !! IXFaddtobeg will add 'directory(:)' to the beginning of the directory list for the searchpath entry 'name' in the global g_paths object 
  subroutine IXFaddtobeg(name,directory,status)
    implicit none
    character(len=*),intent(in)::name
    character(len=*),intent(in)::directory(:)
    integer(i4b)::index
    type(IXTstatus) :: status    
    if(counter==0)return    
    call IXFfindgname_path(name,index)
    if (index == 0)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
            IXCerr_invparam, 'path name '''//trim(adjustl(name))//''' not found in global path(IXFaddtobeg)')      
       return
    else    
       call IXFaddtobeg_path(g_paths(index),directory,status)  
    endif
  
  end subroutine IXFaddtobeg  
!-----------------------------------------------------------------------------------------------------------------------
!! IXFaddtoend_path will add 'directory(:)' to the beginning of the directory list for a given IXTpath object   
  subroutine IXFaddtobeg_path(path,directory,status)
    implicit none
    type(IXTpath)::path
    character(len=*),intent(in)::directory(:)
    character(len=long_len)::trim_direc(size(directory))
    type(IXTstatus)::status
    integer(i4b)::i,dir_length
    if(counter==0)return
    dir_length=size(directory)    
    do i=1,dir_length
      trim_direc(i)=trim(adjustl(directory(i)))
    enddo       
    ! the first time it is created the directory array is set to the length of the incoming array
    ! now we will allocate it in sections to avoid over use of reallocation subroutine
    if ( path%counter == size(path%directory) )then
    ! the array is full so add some more space to the array list
      call IXFreallocdimsFortran(path%directory,(/ path%counter+dir_length /),.true.,status)
    endif    
    !   add new entries to the list and increment counter
    
    path%directory(dir_length+1:path%counter+dir_length)=path%directory(1:path%counter)
    
    path%directory(1:dir_length)=trim_direc
    path%counter=path%counter+dir_length

    call IXFcheck_path(path,status)
  
  end subroutine IXFaddtobeg_path  
!! IXFdelitem_path will delete 'directory(:)' from the directory list in a given IXTpath object if the directory(s) exists
  !-----------------------------------------------------------------------------------------------------------------------
  subroutine IXFdelitem_path(path,directory,status)
    implicit none
    type(IXTpath)::path
    character(len=*),intent(in)::directory(:)
    type(IXTstatus)::status
    integer(i4b)::len,i,j,hits
    logical::mask(size(path%directory))
    if(counter==0)return
    mask = .true.
    do j=1,size(directory)
      hits=0      
      do i=1,size(mask)
        if(trim(adjustl(directory(j))) == trim(adjustl(path%directory(i))))then
          mask(i)=.false.
          hits=hits+1
        endif
      enddo      
      if(hits==0)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
            IXCerr_invparam, ''''//trim(adjustl(directory(j)))//''' path not found in '''//trim(adjustl(path%name))//''' searchpath entry in global path(IXFdelitem_path)')          
      endif      
    enddo
    len=count(mask) 
    if(len == path%counter)then !messages were written in loop above
       return
    endif
    path%counter=len    
    if(path%counter ==0)then
      return  
    else
      call IXFset_path(path,status,directory=pack(path%directory,mask))
    endif
  end subroutine IXFdelitem_path 
  !-----------------------------------------------------------------------------------------------------------------------  
  !! IXFdelpath will delete a given searchpath entry 'name' from the global g_paths object
  subroutine IXFdelpath(name,status)
    implicit none
    character(len=*),intent(in)::name
    type(IXTstatus)::status
    integer(i4b)::index,i,j
    type(IXTpath)::tempcopy(counter-1)
    if(counter==0)return    
    call IXFfindgname_path(name,index)    
    if(index ==0)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
            IXCerr_invparam, 'path name '''//trim(adjustl(name))//''' not found in global path(IXFdelpath)')      
       return
    endif
    
    if (counter-1 ==0)then
      call IXFdestroy_path(g_paths,status)
      deallocate(g_paths)
      counter=0
      return
    else
      j=1    
      do i=1,counter
        if(i /= index)then
          call IXFset_path(tempcopy(j),status,ref=g_paths(i))
          j=j+1
        endif
      enddo
      counter=counter-1
      call IXFrealloc_path(g_paths,counter,.false.,status)      
      do i=1,counter
        call IXFset_path(g_paths(i),status,ref=tempcopy(i))
      enddo
      call IXFdestroy_path(tempcopy,status)
    endif
  end subroutine IXFdelpath  
  
  !! IXFdeldir will delete a given 'directory(:)' from a given searchpath entry 'name' from the global g_paths object,
  !! if all directories are subsequently removed then the searchpath entry is deleted
  !-----------------------------------------------------------------------------------------------------------------------  
  subroutine IXFdeldir(name,directory,status)
    implicit none
    character(len=*),intent(in)::name,directory(:)
    type(IXTstatus)::status
    integer(i4b)::index,i
    type(IXTpath)::tempcopy(counter-1)
    if(counter==0)return    
    call IXFfindgname_path(name,index)    
    if(index ==0)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
            IXCerr_invparam, 'path name '''//trim(adjustl(name))//''' not found in global path(IXFdeldir)')      
       return
    endif
    call IXFdelitem_path(g_paths(index),directory,status)
    if(g_paths(index)%counter==0)then
      call IXFwrite_line('path name '''//trim(adjustl(name))//''' is now empty and will be removed',status)
      call IXFdelpath(g_paths(index)%name,status)
    endif
  end subroutine IXFdeldir
  !-----------------------------------------------------------------------------------------------------------------------  
!! IXFcopygpath will copy the g_paths global path object to a like object which can then be manipulated separately or 
!! returned back through the matlab binding etc.
  subroutine IXFcopygpath(outpath,status)
    implicit none
    type(IXTpath),allocatable::outpath(:)
    type(IXTstatus)::status
    integer(i4b)::i
    character(len=long_len)::blank(1)
    blank(1)=''
    if(counter==0)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
            IXCerr_invparam, 'global path object empty, returning empty path(IXFcopygpath)')      
       allocate(outpath(1))
       call IXFset_path(outpath(1),status,name='',directory=blank)
       return
    endif    
    allocate(outpath(counter))
    do i=1,counter
      call IXFset_path(outpath(i),status,ref=g_paths(i))
      outpath(i)%counter=g_paths(i)%counter
    enddo  
  end subroutine IXFcopygpath
    !-----------------------------------------------------------------------------------------------------------------------
!! IXFshowpath will display the searchpath entry 'name' of the global path object exhaustively to the screen, if this argument is blank then the 
!! whole g_paths object will be displayed
  subroutine IXFshowpath(name,status)
    implicit none
    character(len=*),intent(in)::name
    type(IXTstatus)::status
    integer(i4b)::index,i
    if(counter==0)return
    if(name /= '')then
      call IXFfindgname_path(name,index)    
      if(index ==0)then
         call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
            IXCerr_invparam, 'path name '''//trim(adjustl(name))//''' not found in global path(IXFshowpath)')      
         return
      endif
      call IXFshowpath_path(g_paths(index),status)
    else
      call IXFwrite_line(' ',status)          
      do i=1,counter
        call IXFshowpath_path(g_paths(i),status)
      enddo
    endif  
    
  end subroutine IXFshowpath
  !-----------------------------------------------------------------------------------------------------------------------
!! IXFshowpath_path will exhaustively display the whole directory list for a given path object
  subroutine IXFshowpath_path(path,status)
    implicit none
    type(IXTpath),intent(in)::path
    type(IXTstatus)::status
    integer(i4b)::i
    if(counter==0)return
    call IXFwrite_line(trim(adjustl(path%name))//':::',status)
    do i=1,path%counter
      call IXFwrite_line_indent(trim(adjustl(path%directory(i))),4,' ',status)
    enddo
    call IXFwrite_line(' ',status)      
  end subroutine IXFshowpath_path
  !-----------------------------------------------------------------------------------------------------------------------
!! IXFrealloc_path this function will reallocate allocatable arrays of the IXTpath object, and preserve their contents if applicable.
!! This function is not automatic since the classes_base.f90 file cannot be used
  subroutine IXFrealloc_path (dt, n, preserve, status)
    implicit none
    integer :: n, istat,i
    logical :: preserve
    type(IXTpath), allocatable :: dt(:)
    type(IXTpath)::existing(size(dt))
    type(IXTstatus) :: status
    
    if (allocated(dt))then
      if(size(dt) .eq. n ) then
        if (.not. preserve)call IXFdestroy_path(dt,status)
        return
      endif      
      if (preserve) then
          do i=1,size(dt)
            call IXFset_path(existing(i),status,ref=dt(i))
          enddo
        deallocate(dt)
        allocate(dt(n))

        do i=1,min(n,size(existing))
          call IXFset_path(dt(i),status,ref=existing(i))
        enddo
          
        call IXFdestroy_path(existing,status)
      else
        call IXFdestroy_path(dt,status)
        deallocate(dt)
        allocate(dt(n))
      endif
    else
      allocate(dt(n))
    endif      
  end subroutine  
 !-----------------------------------------------------------------------------------------------------------------------


end module IXMpath
