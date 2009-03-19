module IXMfileio
  use IXMio
  use IXMmemory
  use NXUmodule
  use IXMpath
  implicit none
  type IXTfileio
     character(len=256) :: file_name = ' '
     integer :: mode
     integer :: file_id(15000) ! really type(NXhandle)
  end type IXTfileio
!these options are now defined in IXMpath since the IXFtranslate function may be passed a mode argument
!integer, parameter :: IXC_READ=1, IXC_WRITE=2, IXC_RDWR=IXC_READ+IXC_WRITE, IXC_CREATE=4, IXC_CREATEXML=8
  interface IXBfileWrite
     module procedure IXBfileWriteChar,IXBfileWriteReal,IXBfileWriteInteger,IXBfileWriteLogical, &
          IXBfileWritedp1,IXBfileWritedp2,IXBfileWritedp3,IXBfileWritedp4, &
          IXBfileWritei4b1,IXBfileWritei4b2,IXBfileWritei4b3,IXBfileWritei4b4, &
          IXBfileWritec1
  end interface
  interface IXBfileRead
     module procedure IXBfileReadChar,IXBfileReadReal,IXBfileReadInteger,IXBfileReadLogical, &
          IXBfileReaddp1,IXBfileReaddp2,IXBfileReaddp3,IXBfileReaddp4, &
          IXBfileReadi4b1,IXBfileReadi4b2,IXBfileReadi4b3,IXBfileReadi4b4, &
          IXBfileReadc1
  end interface
  interface IXBfileReadAlloc
     module procedure IXBfileReadAllocdp1,IXBfileReadAllocdp2,IXBfileReadAllocdp3, &
          IXBfileReadAllocdp4,IXBfileReadAlloci4b1,IXBfileReadAlloci4b2, &
          IXBfileReadAlloci4b3,IXBfileReadAlloci4b4, &
          IXBfileReadAllocc1
  end interface
  interface IXBfileReadPtr
     module procedure IXBfileReadPtrdp1,IXBfileReadPtrdp2,IXBfileReadPtrdp3, &
          IXBfileReadPtrdp4,IXBfileReadPtri4b1,IXBfileReadPtri4b2, &
          IXBfileReadPtri4b3,IXBfileReadPtri4b4
  end interface
  interface IXFfile_read
     module procedure IXFfile_read_fileio
  end interface
  interface IXFfile_write
     module procedure IXFfile_write_fileio
  end interface
  interface IXFfile_type
     module procedure IXFfile_get_type, IXFfile_check_type
  end interface IXFfile_type
  interface IXFfile_open
     module procedure IXFfile_open_nxs,IXFfile_open_asc
  end interface     
  interface IXFfile_close
     module procedure IXFfile_close_nxs,IXFfile_close_asc
  end interface      

  integer, parameter :: IXCfile_type_ascii = 1
  integer, parameter :: IXCfile_type_binary = 2
  
  integer :: nxigetdata, nxiputdata
  external :: nxigetdata, nxiputdata
contains
  subroutine IXFfile_open_asc( file_name,file_status,unit, status)
    implicit none
    character(len=*),intent(in) :: file_name,file_status
    type(IXTstatus) :: status
    integer(i4b),intent(out) :: unit
    integer(i4b)::open_status
    character(len=long_len)::elab_name
    elab_name=IXFtranslate(file_name)
    call IXFunitno(unit)
    open (unit=unit, file=trim(adjustl(elab_name)), iostat=open_status, status=file_status, action='READ')
    
    if (open_status /= 0) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'ERROR: Unable to open file '//elab_name//'(IXFfile_open)')	
       return
    endif    
    
  end subroutine IXFfile_open_asc

  subroutine IXFfile_close_asc(unit)
    implicit none
    integer(i4b),intent(in):: unit
    integer(i4b)::close_status

    close_status=shutfl(unit)

  end subroutine IXFfile_close_asc
  subroutine nexus_error(nx_stat, message, status)
    implicit none
    integer nx_stat
    character(len=*) :: message
    type(IXTstatus) :: status
    if (nx_stat == NX_ERROR) then
	call IXFadd_status(status, IXCfacility_file, IXCseverity_error, IXCerr_outofmem, message)
    endif
  end subroutine

  function IXFfile_check_type(file_name, ftype) result(ans)
    character(len=*), intent(in) :: file_name
    integer(i4b), intent(in) :: ftype
    logical :: ans
    ans = (iand(IXFfile_type(IXFtranslate(file_name)), ftype) .ne. 0)
  end function IXFfile_check_type
  
  function IXFfile_get_type(file_name) result(ftype)
    implicit none
    character(len=*), intent(in) :: file_name
    integer(i4b) :: ftype
    type(NXhandle) :: handle
    integer :: stat
    stat = NXopen(IXFtranslate(file_name,0),NXACC_READ,handle)
    if (stat /= NX_OK) then
        ftype = IXCfile_type_ascii
    else
        stat = NXclose(handle)
        ftype = IXCfile_type_binary
    endif
  end function IXFfile_get_type

  subroutine IXFfile_open_nxs(fio, file_name, mode, status)
    implicit none
    character(len=*) :: file_name
    type(IXTfileio), intent(out) :: fio
    type(IXTstatus) :: status
    type(NXhandle) :: handle
    integer :: mode, nxmode, stat
    !write(6,*) ' Opening file ',file_name
    !    call IXFwrite_line('open', status)
    fio%file_name = IXFtranslate(file_name,mode)   
    fio%mode = mode
    if (mode == IXC_READ) then
       nxmode = NXACC_READ
    else if (mode == IXC_WRITE) then
       nxmode = NXACC_RDWR
    else if (mode == IXC_RDWR) then
       nxmode = NXACC_RDWR
    else if (mode == IXC_CREATEXML) then
       nxmode = NXACC_CREATEXML
    else if (mode == IXC_CREATE) then
       nxmode = NXACC_CREATE
    endif
    stat = NXopen(IXFtranslate(file_name,mode),nxmode,handle)
    call nexus_error(stat, 'Error in IXFfile_open of '//trim(adjustl(file_name)), status)
    fio%file_id = transfer(handle, fio%file_id, size(fio%file_id))
  end subroutine IXFfile_open_nxs

  subroutine IXFfile_close_nxs(fio, status)
    implicit none
    integer stat
    type(IXTfileio) :: fio
    type(IXTstatus) :: status
    type(NXhandle) :: handle
    !write(6,*) ' Closing file'
    handle = transfer(fio%file_id, handle)
    stat = NXclose(handle)
    call nexus_error(stat, 'Error in IXFfile_close', status)
    fio%file_id = 0
  end subroutine IXFfile_close_nxs


   !----------------------------------------------------------------------------------------------------------------------
   !  Opening, closing files
   !----------------------------------------------------------------------------------------------------------------------
   !! Program to find an unused FORTRAN unit number. If unable to provide
   !! one, then iunit is returned with the value -1.
   subroutine IXFunitno (iunit)
      integer(i4b), parameter :: istart=60, iend=99
      integer(i4b), intent(OUT) :: iunit
      integer(i4b):: i
      logical:: used

      do i = istart, iend
         inquire (unit=i, opened=used)
         if (.not. used) then
            iunit = i
            return
         endif
      end do
      iunit = -1  ! All unit numbers in range are used
      return
   end subroutine IXFunitno
    !----------------------------------------------------------------------------------------------------------------------
  
 	integer(i4b) function SYS_DELETE_FILE (file_name)
	character(len=*):: file_name
!C
!C Deletes a file after checking it exists and closing it if open.
!C Return value =0 all OK; = 1 if unable to delete or an error occurs.
!C
!C Comments
!C --------
!C  Pure FORTRAN 77
!C
!C  Strictly, CLOSE (...STATUS=DELETE) only deletes the file from the view
!C  of the executing program. Also, in Digital Fortran, it will not delete files
!C  opened with READONLY access. That is why the subroutine closes the file and
!C  re-opens, even though it looks clumsy.
!C
	integer iunit, len_name, ioerr
	logical lex, lop

	len_name = len(file_name)
	if (len_name .eq. 0) goto 1000

	inquire (file=file_name, opened=lop, number=iunit, err=1000)
	if (lop) close (unit=iunit, err=1000)

	inquire (file=file_name, exist=lex, err=1000)
	if (lex) then
		call IXFunitno (iunit)
		if (iunit .lt. 0) goto 1000
		open (unit=iunit, file=file_name, status='OLD', err=1000)
		close (unit=iunit, status='DELETE', err=1000)
		sys_delete_file = 0
		return
	endif

 1000	sys_delete_file = 1

      return
      end function SYS_DELETE_FILE  
   !----------------------------------------------------------------------------------------------------------------------
   !! Graceful verstion of Fortran close - it does not produce an error if the unit does not exist
   !!v   istat = shutfl(iunit)
   !!>   input:   iunit    unit number
   !!    output:  istat    = OK if the unit exists and is open
   !!                      = ERR otherwise
   !!<
   integer(i4b) function shutfl (iunit)
      integer(i4b), intent(IN) :: iunit
      logical :: uexist, uopen

      inquire (unit=iunit, exist=uexist, opened=uopen)
      if (uexist .and. uopen) then
         close (iunit)
         shutfl = OK
      else
         shutfl = ERR
      endif
      return

   end function shutfl   


  subroutine IXBfileMakeGroup(fio, name, class)
    implicit none
    type(IXTfileio) :: fio
    type(NXhandle) :: handle
    type(IXTstatus) :: status
    character(len=*) :: name, class
    integer stat
    handle = transfer(fio%file_id, handle)
    if (class /= ' ') then
!       call IXFwrite_line(' Creating group '//name//' class '//class, status)
       stat = NXmakegroup(handle,name,'NX'//class)
       call nexus_error(stat, 'Error in IXBfileMakeGroup of '//trim(name)// ' type '//trim(class), status)
    else
!       call IXFwrite_line(' Creating group '//name//' class NXentry', status)
       stat = NXmakegroup(handle,name,'NXentry')
       call nexus_error(stat, 'Error in IXBfileMakeGroup of '//trim(name)// ' type NXentry', status)
    endif
    fio%file_id = transfer(handle, fio%file_id, size(fio%file_id))
  end subroutine IXBfileMakeGroup

  subroutine IXBfileOpenGroup(fio, name, class)
    implicit none
    type(IXTfileio) :: fio
    type(IXTstatus) :: status
    character(len=*) :: name, class
    integer stat
    type(NXhandle) :: handle
    handle = transfer(fio%file_id, handle)
    if (class /= ' ') then
!       call IXFwrite_line(' Opening group '//name//' class '//class, status)
       stat = NXopengroup(handle,name,'NX'//class)
       call nexus_error(stat, 'Error in IXBfileOpenGroup of '//trim(name)// ' class '//trim(class), status)
    else
!       call IXFwrite_line(' Opening group '//name//' class NXentry', status)
       stat = NXopengroup(handle,name,'NXentry')
       call nexus_error(stat, 'Error in IXBfileOpenGroup of '//trim(name)// ' class NXentry', status)
    endif
    fio%file_id = transfer(handle, fio%file_id, size(fio%file_id))
  end subroutine IXBfileOpenGroup

  subroutine IXBfileCloseGroup(fio, name)
    implicit none
    type(IXTfileio) :: fio
    type(IXTstatus) :: status
    character(len=*) :: name
    integer stat
    type(NXhandle) :: handle
    handle = transfer(fio%file_id, handle)
!    call IXFwrite_line(' Closing group '//name, status)
    stat = NXclosegroup(handle)
    call nexus_error(stat, 'Error in IXBfileCloseGroup of '//trim(name), status)
    fio%file_id = transfer(handle, fio%file_id, size(fio%file_id))
  end subroutine IXBfileCloseGroup

  subroutine IXBfileWriteChar(fio, name, value, status)
    implicit none
    type(IXTfileio) :: fio
    character(len=*) :: name
    type(IXTstatus) :: status
    character(len=*) :: value
    integer stat
    type(NXhandle) :: handle
    handle = transfer(fio%file_id, handle)
!    call IXFwrite_line(' Writing char field '//name, status)
    stat = NXUwritedata(handle, name, value)
    call nexus_error(stat, 'Error in IXBfileWriteChar of '//trim(name), status)
    fio%file_id = transfer(handle, fio%file_id, size(fio%file_id))
  end subroutine IXBfileWriteChar

  subroutine IXBfileReadChar(fio, name, value, status)
    implicit none
    type(IXTfileio) :: fio
    character(len=*) :: name
    type(IXTstatus) :: status
    character(len=*) :: value
    integer stat
    type(NXhandle) :: handle
    handle = transfer(fio%file_id, handle)
!    call IXFwrite_line(' Reading char field '//name, status)
    stat = NXUreaddata(handle, name, value)
    call nexus_error(stat, 'Error in IXBfileReadChar of '//trim(name), status)
    fio%file_id = transfer(handle, fio%file_id, size(fio%file_id))
  end subroutine IXBfileReadChar

  subroutine IXBfileWriteInteger(fio, name, value, status)
    implicit none
    type(IXTfileio) :: fio
    character(len=*) :: name
    type(IXTstatus) :: status
    integer(i4b) :: value
    integer stat
    type(NXhandle) :: handle
    handle = transfer(fio%file_id, handle)
!    call IXFwrite_line(' Writing integer field '//name, status)
    stat = NXUwritedata(handle, name, value)
    call nexus_error(stat, 'Error in IXBfileWriteInteger of '//trim(name), status)
    fio%file_id = transfer(handle, fio%file_id, size(fio%file_id))
  end subroutine IXBfileWriteInteger

  subroutine IXBfileReadInteger(fio, name, value, status)
    implicit none
    type(IXTfileio) :: fio
    character(len=*) :: name
    type(IXTstatus) :: status
    integer(i4b) :: value
    integer stat
    type(NXhandle) :: handle
    handle = transfer(fio%file_id, handle)
!    call IXFwrite_line(' Reading integer field '//name, status)
    stat = NXUreaddata(handle, name, value)
    call nexus_error(stat, 'Error in IXBfileReadInteger of '//trim(name), status)
    fio%file_id = transfer(handle, fio%file_id, size(fio%file_id))
  end subroutine IXBfileReadInteger

  subroutine IXBfileWriteReal(fio, name, value, status)
    implicit none
    type(IXTfileio) :: fio
    character(len=*) :: name
    type(IXTstatus) :: status
    real(dp) :: value
    integer stat
    type(NXhandle) :: handle
    handle = transfer(fio%file_id, handle)
!    call IXFwrite_line(' Writing real field '//name, status)
    stat = NXUwritedata(handle, name, value)
    call nexus_error(stat, 'Error in IXBfileWriteReal of '//trim(name), status)
    fio%file_id = transfer(handle, fio%file_id, size(fio%file_id))
  end subroutine IXBfileWriteReal

  subroutine IXBfileReadReal(fio, name, value, status)
    implicit none
    type(IXTfileio) :: fio
    character(len=*) :: name
    type(IXTstatus) :: status
    real(dp) :: value
    integer stat
    type(NXhandle) :: handle
    handle = transfer(fio%file_id, handle)
!    call IXFwrite_line(' Reading real field '//name, status)
    stat = NXUreaddata(handle, name, value)
    call nexus_error(stat, 'Error in IXBfileReadReal of '//trim(name), status)
    fio%file_id = transfer(handle, fio%file_id, size(fio%file_id))
  end subroutine IXBfileReadReal

  subroutine IXBfileWriteLogical(fio, name, value, status)
    implicit none
    type(IXTfileio) :: fio
    character(len=*) :: name
    type(IXTstatus) :: status
    logical :: value
    integer stat
    type(NXhandle) :: handle
    handle = transfer(fio%file_id, handle)
!    call IXFwrite_line(' Writing logical field '//name, status)
    if (value) then
       stat = NXUwritedata(handle, name, 1)
    else
       stat = NXUwritedata(handle, name, 0)
    endif
    call nexus_error(stat, 'Error in IXBfileWriteLogical of '//trim(name), status)
    fio%file_id = transfer(handle, fio%file_id, size(fio%file_id))
  end subroutine IXBfileWriteLogical

  subroutine IXBfileReadLogical(fio, name, value, status)
    implicit none
    type(IXTfileio) :: fio
    character(len=*) :: name
    type(IXTstatus) :: status
    logical :: value
    integer stat, val
    type(NXhandle) :: handle
    handle = transfer(fio%file_id, handle)
!    call IXFwrite_line(' Reading logical field '//name, status)
    stat = NXUreaddata(handle, name, val)
    call nexus_error(stat, 'Error in IXBfileReadLogical of '//trim(name), status)
    if (val == 0) then
       value = .false.
    else
       value = .true.
    endif
    fio%file_id = transfer(handle, fio%file_id, size(fio%file_id))
  end subroutine IXBfileReadLogical

  subroutine IXFfile_read_fileio(fio, value, name, status)
    implicit none
    type(IXTfileio) :: fio
    character(len=*) :: name
    type(IXTstatus) :: status
    type(IXTfileio) :: value
    type(NXhandle) :: handle
    handle = transfer(fio%file_id, handle)
!    call IXFwrite_line(' Reading fileio field '//name, status)
    fio%file_id = transfer(handle, fio%file_id, size(fio%file_id))
  end subroutine IXFfile_read_fileio

  subroutine IXFfile_write_fileio(fio, value, name, status)
    implicit none
    type(IXTfileio) :: fio
    character(len=*) :: name
    type(IXTstatus) :: status
    type(IXTfileio) :: value
    type(NXhandle) :: handle
    handle = transfer(fio%file_id, handle)
!    call IXFwrite_line(' Writing fileio field '//name, status)
    fio%file_id = transfer(handle, fio%file_id, size(fio%file_id))
  end subroutine IXFfile_write_fileio

  subroutine IXBfindGroup(fio, group_name, group_class, found, status)
    type(IXTfileio) :: fio
    type(NXhandle) :: handle
    type(IXTstatus) :: status
    character(len=*) :: group_name, group_class
    logical :: found
    integer :: stat
    handle = transfer(fio%file_id, handle)
!    call IXFwrite_line(' IXBfindGroup', status)
    stat = NXUfindgroup(handle,group_name,group_class)
    if (stat == NX_OK) then
	found = .true.
    else 
	found = .false.
    endif
    call nexus_error(stat, 'IXBfindGroup of '//trim(group_name)// ' class '//trim(group_class), status)
    fio%file_id = transfer(handle, fio%file_id, size(fio%file_id))
  end subroutine IXBfindGroup

#define IXD_NAME		i4b1
#define IXD_TYPE		integer(i4b)
#define IXD_TYPE_TEMP	integer(i4b)
#define IXD_NXTYPE      NX_INT32
#define IXD_DIMS		:
#include "fileio_routines.f90"

#define IXD_NAME		i4b2
#define IXD_TYPE		integer(i4b)
#define IXD_TYPE_TEMP	integer(i4b)
#define IXD_NXTYPE      NX_INT32
#define IXD_DIMS		:,:
#include "fileio_routines.f90"

#define IXD_NAME		i4b3
#define IXD_TYPE		integer(i4b)
#define IXD_TYPE_TEMP	integer(i4b)
#define IXD_NXTYPE      NX_INT32
#define IXD_DIMS		:,:,:
#include "fileio_routines.f90"

#define IXD_NAME		i4b4
#define IXD_TYPE		integer(i4b)
#define IXD_TYPE_TEMP	integer(i4b)
#define IXD_NXTYPE      NX_INT32
#define IXD_DIMS		:,:,:,:
#include "fileio_routines.f90"

#define IXD_NAME		dp1
#define IXD_TYPE		real(dp)
#define IXD_TYPE_TEMP	real(dp)
#define IXD_NXTYPE      NX_FLOAT64
#define IXD_DIMS		:
#include "fileio_routines.f90"

#define IXD_NAME		dp2
#define IXD_TYPE		real(dp)
#define IXD_TYPE_TEMP	real(dp)
#define IXD_NXTYPE      NX_FLOAT64
#define IXD_DIMS		:,:
#include "fileio_routines.f90"

#define IXD_NAME		dp3
#define IXD_TYPE		real(dp)
#define IXD_TYPE_TEMP	real(dp)
#define IXD_NXTYPE      NX_FLOAT64
#define IXD_DIMS		:,:,:
#include "fileio_routines.f90"

#define IXD_NAME		dp4
#define IXD_TYPE		real(dp)
#define IXD_TYPE_TEMP	real(dp)
#define IXD_NXTYPE      NX_FLOAT64
#define IXD_DIMS		:,:,:,:
#include "fileio_routines.f90"

! char done differently - fileio_routine2

#define IXD_NAME		c1
#define IXD_TYPE		character(len=*)
#define IXD_TYPE_TEMP	character(len=len(value))
#define IXD_NXTYPE      NX_CHAR
#define IXD_DIMS		:
#include "fileio_routines2.f90"

end module IXMfileio

