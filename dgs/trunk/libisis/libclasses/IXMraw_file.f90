module IXMraw_file
  use IXMtype_definitions
  use IXMstatus
  use IXMbase
  use IDFroutines
  implicit none
  !  private
  public :: IXTraw_file
  public :: IXFopen_raw, IXFsize_raw, IXFget_raw
  public :: global_raw_status
  private ::close_raw_file,close_raw_files,special_tag
  !! type(IXTraw_file) :: handle
  !! character(len=*) :: file_name
  !! type(IXTstatus) :: status
  !!
  !! call IXFopen_raw(handle, file_name, status)
  !! call IXFget_raw(handle, item_name, value, status)
  !! call IXFsize_raw(handle, item_name, item_size, status)
  !!
  interface IXFget_raw
     module procedure IXFget_real, IXFget_real1, IXFget_real2, IXFget_int, &
          IXFget_int1, IXFget_int2, IXFget_dp, IXFget_dp1, IXFget_dp2, &
          IXFget_char, IXFget_char_c1, IXFget_spectrum_d1, IXFget_spectrum_array_d1, IXFget_spectrum_d2, IXFgetA_spectrum_d2, &
!           IXFget_char_c1, IXFget_spectrum_d1, IXFget_spectrum_array_d1, IXFget_spectrum_d2, IXFgetA_spectrum_d2, &
          IXFget_spectrum_array_d2,IXFget_data_i1, IXFget_data_i2 ,IXFgetsum_spectrum_d1
  end interface

  interface IXFsize_raw
     module procedure IXFsize_raw_i, IXFsize_raw_i_array
  end interface

  interface IXFclose_raw_file
     module procedure close_raw_file,close_raw_files
  end interface
  
  type IXTraw_file
     private
     character(len=256) :: runid=''
     logical :: found = .false.
     integer :: ntc1 = 0
     integer :: nsp1 = 0
     integer :: ndet = 0
     integer :: nper = 0
     integer :: nmon = 0
     integer :: nuse = 0
     integer :: daeh(2) = 0
  end type IXTraw_file

  type(IXTstatus), pointer :: global_raw_status => NULL()

#define IXD_TYPE raw_file
#define IXD_NO_BASE	1
#include "class_header.f90"

contains

#define IXD_DESCRIPTION	"IXTraw_file class"
#define IXD_TYPE raw_file
#define IXD_SQTYPE 'raw_file'
#include "class_base.f90"

  !!generic destroy routine which does nothing but helps compilation
  subroutine IXFdestroy_raw_file(arg, status)
    implicit none
    type(IXTraw_file) :: arg
    type(IXTstatus) :: status
    arg%runid=''
    arg%found=.false.
    arg%ntc1 = 0
    arg%nsp1 = 0
    arg%ndet = 0
    arg%nper = 0
    arg%nmon = 0
    arg%nuse = 0
    arg%daeh = 0
  end subroutine IXFdestroy_raw_file
  !!generic check routine which does nothing but helps compilation
  subroutine IXFcheck_raw_file(arg, status)
    implicit none
    type(IXTraw_file) :: arg
    type(IXTstatus) :: status
  end subroutine IXFcheck_raw_file
  !!generic create routine which does nothing but helps compilation
  subroutine IXFcreate_raw_file(arg, status)
    implicit none
    type(IXTraw_file) :: arg
    type(IXTstatus) :: status
  end subroutine IXFcreate_raw_file
  !***********************************************
  !!generic set  routine which does nothing but helps compilation
  subroutine IXFset_raw_file(arg, status,ref)
    implicit none
    type(IXTraw_file) :: arg,ref
    type(IXTstatus) :: status
  end subroutine IXFset_raw_file
  !***********************************************
  !-----------------------------------------------------------------------------------------------------------------------
  !! The IXFget subroutine will return elements of an object to an optional supplied arrays/variables. The supplied variables
  !! should be referrred to by keyword to avoid errors. The 'wout' variable is NOT used in this subroutine
  subroutine IXFget_raw_file(arg, status,runid,found,ntc1,nsp1,ndet,nper,nmon,nuse)
    implicit none
    type(IXTraw_file),intent(in)::arg
!    type(IXTraw_file),intent(out),optional::wout
    character(len=*),intent(out),optional :: runid
    logical,intent(out),optional :: found
    integer(i4b),intent(out),optional :: ntc1,nsp1,ndet,nper,nmon,nuse
    type(IXTstatus) :: status

    if (present(runid))runid=arg%runid
    if (present(found))found=arg%found
    if (present(ntc1))ntc1=arg%ntc1
    if (present(nsp1))nsp1=arg%nsp1
    if (present(ndet))ndet=arg%ndet
    if (present(nper))nper=arg%nper
    if (present(nmon))nmon=arg%nmon
    if (present(nuse))nuse=arg%nuse    
    
  end subroutine IXFget_raw_file
  !***********************************************



  recursive subroutine IXFoperation_run_raw_file(op, field, arg, status)
    implicit none
    type(IXToperation) :: op
    type(IXTstatus) :: status
    type(IXTraw_file) :: arg
    character(len=*) :: field
    call IXFoperationStart(op, 'IXTraw_file', field, status)
    ! this order must match the declaration order in matlab as it is
    ! used when parsing argemnts passed in class creation with vargin
    call IXFoperation_run(op, 'runid', arg%runid, status)
    call IXFoperation_run(op, 'found', arg%found, status)
    call IXFoperation_run(op, 'ntc1', arg%ntc1, status)
    call IXFoperation_run(op, 'nsp1', arg%nsp1, status)
    call IXFoperation_run(op, 'ndet', arg%ndet, status)
    call IXFoperation_run(op, 'nper', arg%nper, status)
    call IXFoperation_run(op, 'nmon', arg%nmon, status)
    call IXFoperation_run(op, 'nuse', arg%nuse, status)
    call IXFoperation_run(op, 'daeh', arg%daeh, status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_raw_file

  subroutine IXFopen_raw(file_name, handle, status)
    implicit none
    type(IXTraw_file), intent(out) :: handle
    type(IXTstatus), intent(inout) :: status
    character(len=*), intent(in) :: file_name
    ! elaborate file_name here.......
    handle%runid = trim(adjustl(IXFtranslate(file_name)))
    call IXFopen_raw_handle(handle, status)
  end subroutine IXFopen_raw

  logical function dae_access(handle)
    implicit none
    type(IXTraw_file), intent(inout) :: handle
    dae_access = .false.
    if (len(handle%runid) .gt. 4) then
      if (handle%runid(1:4) == 'DAE:') then
         dae_access = .true.
      endif
    endif
  end function
	
  character*80 function dae_name(handle)
    implicit none
    type(IXTraw_file), intent(inout) :: handle
    if (dae_access(handle)) then
       dae_name = handle%runid(5:)
    else
       dae_name = ' '
    endif
  end function

  subroutine IXFcheck_raw_handle(handle, status)
    implicit none
    type(IXTraw_file), intent(inout) :: handle
    type(IXTstatus), target, intent(inout) :: status
    if (dae_access(handle)) then
	return
    else
  	call IXFopen_raw_handle(handle, status)
    endif
  end subroutine IXFcheck_raw_handle

  subroutine IXFopen_raw_handle(handle, status)
    implicit none
    type(IXTraw_file), intent(inout) :: handle
    type(IXTstatus), target, intent(inout) :: status
    integer len_out, errcode, dims_array(1)
    logical found
    handle%found = .false.
    global_raw_status => status
    if (dae_access(handle)) then
        call IDFopen(trim(dae_name(handle)), 0, 0, handle%daeh, errcode)
        if (errcode == 0) then
           handle%found = .true.
           dims_array(1) = 1
	   call IDFgetpari(handle%daeh, 'NTC1', handle%ntc1, errcode)
	   call IDFgetpari(handle%daeh, 'NSP1', handle%nsp1, errcode)
	   call IDFgetpari(handle%daeh, 'NDET', handle%ndet, errcode)
	   call IDFgetpari(handle%daeh, 'NPER', handle%nper, errcode)
	   call IDFgetpari(handle%daeh, 'NMON', handle%nmon, errcode)
	   call IDFgetpari(handle%daeh, 'NUSE', handle%nuse, errcode)
        endif
    else
        call open_file(IXFtranslate(handle%runid), found)
        if (found) then
           handle%found = .true.
           call getpari(handle%runid, 'NTC1', handle%ntc1, 1, len_out, errcode)
           call getpari(handle%runid, 'NSP1', handle%nsp1, 1, len_out, errcode)
           call getpari(handle%runid, 'NDET', handle%ndet, 1, len_out, errcode)
           call getpari(handle%runid, 'NPER', handle%nper, 1, len_out, errcode)
           call getpari(handle%runid, 'NMON', handle%nmon, 1, len_out, errcode)
           call getpari(handle%runid, 'NUSE', handle%nuse, 1, len_out, errcode)
        endif
    endif
    if (.not. handle%found) then
        handle%ntc1 = 0
        handle%nsp1 = 0
        handle%ndet = 0
        handle%nper = 0
        handle%nmon = 0
        handle%nuse = 0
        call IXFadd_status(status, IXCfacility_file, IXCseverity_error, &
                IXCerr_filenotfound, 'unable to open raw file '//handle%runid) 
    endif
    global_raw_status => NULL()
  end subroutine IXFopen_raw_handle

  subroutine IXFsize_raw_i_array(handle, item_name, item_size, status)
    implicit none
    type(IXTraw_file) :: handle
    type(IXTstatus), target :: status
    integer :: lt, item_size(:)
    character(len=*) :: item_name
    item_size = 0
    call IXFcheck_raw_handle(handle, status)
    if (status == IXCseverity_error) return
    item_size = 1
    lt = len_trim(item_name)
    if (item_name(:lt) .eq. 'CNT1') then
       item_size(1) = handle%ntc1 + 1
       item_size(2) = handle%nsp1 + 1
       return
    endif
    call IXFsize_raw_i(handle, item_name, item_size(1), status)
  end subroutine IXFsize_raw_i_array

  subroutine IXFsize_raw_i(handle, item_name, item_size, status)
    implicit none
    type(IXTraw_file) :: handle
    type(IXTstatus), target :: status
    integer(i4b),intent(out):: item_size
    character(len=*),intent(in) :: item_name
    character(len=4) :: ndet_names(11) = & 
         (/ 'SPEC', 'DELT', 'LEN2', 'CODE', 'TTHE', &
         'PHI ', 'UDET', 'CRAT', 'MODN', 'MPOS', 'TIMR' /)
!          'UT01', 'UT02', 'UT03', 'UT04',  'UT05' /) 
    integer(i4b):: i, lt ,n_ut
    character(len=2)::nuse
    call IXFcheck_raw_handle(handle, status)
    if (status == IXCseverity_error) return
    item_size = 1
    lt = len_trim(item_name)
    if(lt<3)return ! well it's wrong anyway
    if (item_name(:lt) .eq. 'USER') item_size = 8
    ! includes CRPB,IRPB,RRPB
    if (item_name(lt-2:lt) .eq. 'RPB') item_size = 32
    ! includes CSPB, RSPB
    if (item_name(lt-2:lt) .eq. 'SPB') item_size = 64
    if (item_name(:lt) .eq. 'SE01') item_size = 24
    ! includes IVPB, RVPB
    if (item_name(lt-2:lt) .eq. 'VPB') item_size = 64
    if (item_name(:lt) .eq. 'DAEP') item_size = 64
    if (item_name(:lt) .eq. 'TCM1') item_size = 5
    if (item_name(:lt) .eq. 'TCP1') item_size = 20  
    if (item_name(:lt) .eq. 'TCB1') item_size = handle%ntc1 + 1  
    if (item_name(:lt) .eq. 'TIM1') item_size = handle%ntc1 + 1  
    if (item_name(:lt) .eq. 'MDET') item_size = handle%nmon
    if (item_name(:lt) .eq. 'MONP') item_size = handle%nmon
    if (item_name(:lt) .eq. 'CNT1') item_size = (handle%ntc1 + 1) * (handle%nsp1 + 1)
    do i = 1, size(ndet_names)
       if ( ndet_names(i) .eq. item_name(:lt) ) item_size = handle%ndet
    end do
    if( (item_name (1:2)=='UT'))then
      i=1
      if(lt==4)then
        n_ut=ctoxi(item_name(3:4),i)
      endif
      if(lt==3)then
        n_ut=ctoxi(item_name(3:3),i)
      endif 
      if (n_ut == 0)then
        call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error,IXCerr_invparam,&
           'UT array invalid: '//item_name//' (IXFsize_raw_i)' )        
      else  
        if(n_ut>handle%nuse)then
          write(nuse,'(i2)')handle%nuse
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error,IXCerr_invparam,&
             'UT array number: '//item_name(3:lt)//', must be less than or equal to '//nuse//' (IXFsize_raw_i)' )        
        else  
          item_size=handle%ndet
        endif
      endif
    endif
  end subroutine IXFsize_raw_i

  subroutine IXFget_spectrum_d2(handle,ispec,d2,period,status)
    use IXMdataset_2d
    implicit none
    type(IXTraw_file),intent(inout) :: handle
    type(IXTstatus), target :: status
    integer,intent(in) :: ispec(:),period !!ispec is array of required spectra  
    type(IXTdataset_2d),intent(out) :: d2
    integer(i4b) :: i, j,perlocal,spno(size(ispec)),nspec
    integer, allocatable :: idat(:,:)
    real(dp), pointer :: d2signal(:,:), d2error(:,:), d2x(:), d2y(:)  !d2dx(:)
    type(IXTaxis)::s_axis,x_axis,y_axis
    character(len=long_len)::title(1)
    real(sp),allocatable::tcb(:)
    
    call IXFcheck_raw_handle(handle, status)
    if (status == IXCseverity_error) return
    perlocal=period
    if (perlocal == 0)then
    ! spectra spanning periods
      if (maxval(ispec) > (handle%nper*(handle%nsp1+1)))then     
         call IXFadd_status(status, IXCfacility_file, IXCseverity_error, &
              IXCerr_invparam, 'spectrum numbers not valid for raw file(IXFget_spectrum_d2)') 
         return
      endif
      perlocal=1      
    else
    ! spectra contained in one period only, not worried about spectrum zero
      if (maxval(ispec) > (handle%nsp1))then     
         call IXFadd_status(status, IXCfacility_file, IXCseverity_error, &
              IXCerr_invparam, 'spectrum numbers not valid with period for raw file(IXFget_spectrum_d2)') 
         return
      endif      
    endif     
    ! initialize the dataset_2d object first
!    call IXFinitialise(d2,status)
    nspec=size(ispec)
    call IXFmake_dataset_2d(d2,handle%ntc1+1,nspec,.true.,.true.,.false.,.false.,status)
    call IXFget_ptr_dataset_2d(d2,x=d2x,y=d2y,signal=d2signal,error=d2error)
    allocate(tcb(handle%ntc1+1))   
    call IXFget_real1(handle, 'TIM1', tcb, status)
    call IXFget_char_c1(handle, 'TITL', title, status) 
    d2x=dble(tcb)
    deallocate(tcb)
    allocate(idat(handle%ntc1+1,nspec)) ! +1 for time channel 0
    spno=ispec+((perlocal-1)*(handle%nsp1+1))
    call IXFget_data_i2(handle, spno, idat, status)    
    do i = 1,nspec
       d2y(i) = dble(ispec(i))
       d2signal(:,i) =  dble(idat(2:handle%ntc1+1,i))     / (d2x(2:handle%ntc1+1) - d2x(1:handle%ntc1))   
       d2error(:,i) =  sqrt(dble(idat(2:handle%ntc1+1,i)))    / (d2x(2:handle%ntc1+1) - d2x(1:handle%ntc1))  
    end do
    deallocate(idat)
    ! add details about labels for dataset_2d raw file etc....    
    call IXFcreate_code_axis(x_axis,IXCcode_t,status)
    call IXFcreate_code_axis(s_axis,IXCcountsC,status)
    call IXFcreate_code_axis(y_axis,IXCspecnoC,status)
    ! whole title array is called
    call IXFset_dataset_2d(d2,status,title=title,s_axis=s_axis,x_axis=x_axis,y_axis=y_axis, &
    x_distribution=.true.,y_distribution=.false.)
  end subroutine IXFget_spectrum_d2
  
! this will take an array of rawfile objects and populate a dataset_2d with summed up data
  subroutine IXFgetA_spectrum_d2(handle,ispec,d2,period,status)
    use IXMdataset_1d
    use IXMdataset_2d
    implicit none
    type(IXTraw_file),intent(inout) :: handle(:)
    type(IXTstatus), target :: status
    integer,intent(in) :: ispec(:),period !!ispec is array of required spectra  
    type(IXTdataset_2d),intent(out) :: d2
    type(IXTdataset_1d) :: d1
    integer(i4b) :: i, perlocal,spno(size(ispec)),ref_ntc1,ref_nsp1,ir,nspec
    integer, allocatable :: idat(:,:)
    real(dp), pointer :: d2signal(:,:), d2error(:,:), d2x(:), d2y(:)  !d2dx(:)
    type(IXTaxis)::s_axis,x_axis,y_axis
    character(len=long_len)::title(1)
    real(sp),allocatable::tcb(:)    

    !handle(1) is used for all initial purposes, and only after then do we use other elements in array 
    call IXFcheck_raw_handle(handle(1), status)
    if (status == IXCseverity_error) return
    perlocal=period
    if (perlocal == 0)then
    ! spectra spanning periods
      if (maxval(ispec) > (handle(1)%nper*(handle(1)%nsp1+1)))then     
         call IXFadd_status(status, IXCfacility_file, IXCseverity_error, &
              IXCerr_invparam, 'spectrum numbers not valid for raw file(IXFget_spectrum_d2)') 
         return
      endif
      perlocal=1      
    else
    ! spectra contained in one period only, not worried about spectrum zero
      if (maxval(ispec) > (handle(1)%nsp1))then     
         call IXFadd_status(status, IXCfacility_file, IXCseverity_error, &
              IXCerr_invparam, 'spectrum numbers not valid with period for raw file(IXFget_spectrum_d2)') 
         return
      endif      
    endif
    nspec=size(ispec)    
    call IXFmake_dataset_2d(d2,handle(1)%ntc1+1,nspec,.true.,.true.,.false.,.false.,status)
    call IXFget_ptr_dataset_2d(d2,x=d2x,y=d2y,signal=d2signal,error=d2error)
    d2signal=0.0d0
    d2error=0.0d0
    ! it is assumed that all rawfiles combined have the same time channel boundaries
    ref_ntc1=handle(1)%ntc1 !set reference ntc to check against
    ref_nsp1=handle(1)%nsp1
    call IXFget_char_c1(handle(1), 'TITL', title, status) 
    allocate(tcb(handle(1)%ntc1+1))   
    call IXFget_real1(handle(1), 'TIM1', tcb, status)
    d2x=dble(tcb)
    deallocate(tcb)     
    allocate(idat(handle(1)%ntc1+1,nspec)) ! +1 for time channel 0
    spno=ispec+((perlocal-1)*(handle(1)%nsp1+1))    
    do ir=1,size(handle)
      if (ir/=1)then
        call IXFcheck_raw_handle(handle(ir), status)
        if(handle(ir)%ntc1 /= ref_ntc1 .or. handle(ir)%nsp1 /= ref_nsp1)then
          call IXFadd_status(status, IXCfacility_file, IXCseverity_error, &
          IXCerr_invparam, 'time channels or spectra for multiple rawfiles incommensurate(IXFget_raw)') 
          return
        endif
          if (status == IXCseverity_error) return
      endif
      !loop over spectra
      call IXFget_data_i2(handle(ir), spno, idat, status)          
!      do i = 1,nspec
       ! use idat(2:ref_ntc1 + 1) as the first bin is junk (time zero bin)
        !fill up the signal error and division will be dealt with later
        d2signal = d2signal +   dble(idat(2:ref_ntc1+1,:))  
!      enddo
    enddo    
    deallocate(idat)    
    ! now everything has been combined divide by bin widths
    do i=1,nspec
      d2y(i) = dble(ispec(i))           
      d2error(:,i)=sqrt(d2signal(:,i))  /(d2x(2:ref_ntc1+1) - d2x(1:ref_ntc1))
      d2signal(:,i)=d2signal(:,i)  /(d2x(2:ref_ntc1+1) - d2x(1:ref_ntc1))
    enddo    
    ! add details about labels for dataset_2d raw file etc....    
    call IXFcreate_code_axis(x_axis,IXCcode_t,status)
    call IXFcreate_code_axis(s_axis,IXCcountsC,status)
    call IXFcreate_code_axis(y_axis,IXCspecnoC,status)
    ! whole title array is called
    call IXFset_dataset_2d(d2,status,title=title,s_axis=s_axis,x_axis=x_axis,y_axis=y_axis, &
    x_distribution=.true.,y_distribution=.false.)
  end subroutine IXFgetA_spectrum_d2

  subroutine IXFget_spectrum_array_d2(handle,ispec,d2,periods,status)
    use IXMdataset_2d
    implicit none
    type(IXTraw_file),intent(inout) :: handle
    type(IXTstatus), target :: status
    type(IXTdataset_2d),intent(out) :: d2(:)
    integer,intent(in) :: ispec(:),periods(:)
    integer(i4b)::i
    call IXFcheck_raw_handle(handle, status)
    if (status == IXCseverity_error) return

    if (maxval(periods) > handle%nper)then     
       call IXFadd_status(status, IXCfacility_file, IXCseverity_error, &
            IXCerr_invparam, 'period number not valid for raw file(IXFget_spectrum_array_d2)') 
       return    
    endif
    
    do i = 1, size(d2)
       call IXFget_spectrum_d2(handle, ispec, d2(i),periods(i), status)
    end do
  end subroutine IXFget_spectrum_array_d2


  subroutine IXFget_spectrum_array_d1(handle,ispec,d1,period,status)
    use IXMdataset_1d
    implicit none
    type(IXTraw_file) :: handle
    type(IXTstatus), target :: status
    type(IXTdataset_1d) :: d1(:)
    integer :: ispec(:), i,period
    call IXFcheck_raw_handle(handle, status)
    if (status == IXCseverity_error) return
    !if(maxval(ispec) > handle%nsp1)then

    if((period /= 0) .and. (maxval(ispec)>handle%nsp1))then
       call IXFadd_status(status, IXCfacility_file, IXCseverity_error, &
            IXCerr_invparam, 'period number and spectrum numbers invalid for raw file(IXFget_spectrum_array_d1)') 
       return    
    endif    
    do i = 1, size(d1)
       call IXFget_spectrum_d1(handle, ispec(i), d1(i),period, status)
    end do
  end subroutine IXFget_spectrum_array_d1

  subroutine IXFget_spectrum_d1(handle,ispec,d1,period,status)
    use IXMdataset_1d
    use IXMaxis
    implicit none
    type(IXTraw_file),intent(inout) :: handle
    type(IXTstatus), target :: status
    type(IXTdataset_1d),intent(out) :: d1
    integer(i4b),intent(in)::period
    type(IXTaxis)::s_axis,x_axis
    integer, allocatable :: idat(:), spec_work(:)
    real(dp), pointer :: xarray(:), yarray(:),earray(:)
    character(len=long_len) :: run_title(1)
    integer(i4b)::i,ispec,ntcmax,perlocal,spno
    real(sp),allocatable::tcb(:)   

    perlocal=period
    call IXFcheck_raw_handle(handle, status)
    if (status == IXCseverity_error) return

!  see if ispec is higher than total number of spectra   
    if (ispec > ((handle%nsp1+1)*handle%nper))then     
       call IXFadd_status(status, IXCfacility_file, IXCseverity_error, &
            IXCerr_invparam, 'spectrum number not valid for raw file(IXFget_spectrum_d1)') 
       return
    endif             
    if(perlocal > handle%nper)then
       call IXFadd_status(status, IXCfacility_file, IXCseverity_error, &
            IXCerr_invparam, 'period number not valid for raw file(IXFget_spectrum_array_d1)') 
       return    
    endif
    ntcmax = handle%ntc1+1
    allocate(tcb(ntcmax), idat(ntcmax))
    if(perlocal ==0)perlocal=1
    spno=((perlocal-1)*(handle%nsp1+1))+ispec
    call IXFget_data_i1(handle, spno, idat, status)
    call IXFget_real1(handle, 'TIM1', tcb,status)
    call IXFget_char_c1(handle, 'TITL', run_title,status) 

    ! this subroutine uses pointers to fill the dataset_1d 
    ! initialize the dataset_1d object first
!    call IXFinitialise(d1,status)
    ! create dataset_1d with x-array of length ntcmax
    call IXFmake_dataset_1d(d1,ntcmax,.true.,.true.,status)
    !get pointers of the dataset_1d array
    call IXFget_ptr_dataset_1d(d1,x=xarray,signal=yarray,error=earray)  
    xarray = dble(tcb)
    ! use idat(i + 1) as the first bin is junk (time zero bin)
    forall(i = 1:handle%ntc1)
       yarray(i) = dble(idat(i+1)) / (xarray(i+1) - xarray(i))
       earray(i) = sqrt(dble(idat(i+1))) / (xarray(i+1) - xarray(i))
    end forall

    !create the axis objects to fill dataset_1d with
    call IXFcreate_code_axis(x_axis,IXCcode_t,status)
    call IXFcreate_code_axis(s_axis,IXCcountsC,status)
    
    call IXFset_dataset_1d(d1, status, title=run_title,   &
         s_axis=s_axis, x_distribution=.true.,  x_axis=x_axis)
    deallocate(tcb, idat)

  end subroutine IXFget_spectrum_d1
  
  subroutine IXFgetsum_spectrum_d1(handle,ispec,d1,x1,x2,period,status)
    use IXMdataset_1d
    use IXMintegrate
    implicit none
    type(IXTraw_file),intent(inout) :: handle
    real(dp),intent(in)::x1,x2
    type(IXTstatus),intent(inout) :: status
    integer,intent(in) :: ispec(:),period !!ispec is array of required spectra  
    type(IXTdataset_1d),intent(out) :: d1
    integer(i4b) :: i, perlocal,spno,nspec
    integer, allocatable :: idat(:)
    real(dp), pointer :: d1signal(:), d1error(:), d1x(:)
    type(IXTaxis)::s_axis,x_axis
    character(len=long_len)::title(1)
    real(sp),allocatable::tcb(:)
    real(dp),allocatable::dbl_x(:),signal(:),error(:)
    
    call IXFcheck_raw_handle(handle, status)
    if (status == IXCseverity_error) return
    perlocal=period
    if (perlocal == 0)then
    ! spectra spanning periods
      if (maxval(ispec) > (handle%nper*(handle%nsp1+1)))then     
         call IXFadd_status(status, IXCfacility_file, IXCseverity_error, &
              IXCerr_invparam, 'spectrum numbers not valid for raw file(IXFget_spectrum_d2)') 
         return
      endif
      perlocal=1      
    else
    ! spectra contained in one period only, not worried about spectrum zero
      if (maxval(ispec) > (handle%nsp1))then     
         call IXFadd_status(status, IXCfacility_file, IXCseverity_error, &
              IXCerr_invparam, 'spectrum numbers not valid with period for raw file(IXFget_spectrum_d2)') 
         return
      endif      
    endif     
    ! initialize the dataset_2d object first
!    call IXFinitialise(d2,status)
    nspec=size(ispec)
    call IXFmake_dataset_1d(d1,nspec,.false.,.false.,status)
    !call IXFmake_dataset_2d(d1,handle%ntc1+1,size(ispec),.true.,.true.,.false.,.false.,status)
    call IXFget_ptr_dataset_1d(d1,x=d1x,signal=d1signal,error=d1error)
    allocate(tcb(handle%ntc1+1))
    allocate(dbl_x(handle%ntc1+1))   
    call IXFget_real1(handle, 'TIM1', tcb,status)
    call IXFget_char_c1(handle, 'TITL', title,status) 
    dbl_x=dble(tcb)
    deallocate(tcb)
    allocate(signal(handle%ntc1))
    allocate(error(handle%ntc1))
    allocate(idat(handle%ntc1+1)) ! +1 for time channel 0
    do i = 1,nspec
       spno=((perlocal-1)*(handle%nsp1+1))+ispec(i)
       call IXFget_data_i1(handle, spno, idat, status)           
       d1x(i) = dble(ispec(i))       
       signal =  dble(idat(2:handle%ntc1+1))     
       error =  sqrt(dble(idat(2:handle%ntc1+1)))    
       call IXFintegrate_1d_hist(d1signal(i),d1error(i),dbl_x,signal,error,.false.,x1,x2,status)
    end do
    deallocate(idat,signal,error,dbl_x)
    ! add details about labels for dataset_2d raw file etc....
        
    call IXFcreate_code_axis(x_axis,IXCcode_t,status)
    call IXFcreate_code_axis(s_axis,IXCcountsC,status)
    ! whole title array is called
    call IXFset_dataset_1d(d1,status,title=title,s_axis=s_axis,x_axis=x_axis, &
    x_distribution=.false.)
  end subroutine IXFgetsum_spectrum_d1
  !-----------------------------------------------------------------------------------------------------------------------
  subroutine IXFhead_raw_file(handle,status)
    implicit none
    type(IXTraw_file):: handle(:)
    type(IXTstatus):: status
    character(8)::sum_proton
    character(80)::hdr(size(handle))
    character(20)::user_j(size(handle)),user(8)
    character(len=long_len)::e_times(2,size(handle)),crpb(32),run_title(size(handle))
    integer(i4b)::len,j
    real(dp)::proton_sum,rrpb(32)
    proton_sum=0.0_dp
    
    len=size(handle)
    
    do j=1,len
      call IXFget_raw(handle(j), 'HDR',hdr(j) , status)
      call IXFget_raw(handle(j), 'TITL', run_title(j) , status)
      call IXFget_raw(handle(j), 'RRPB', rrpb , status)      
      call IXFget_raw(handle(j), 'USER', user , status)
      call IXFget_raw(handle(j), 'CRPB',crpb , status)  
      proton_sum=proton_sum + rrpb(8)
      user_j(j)=user(5)
      e_times(1,j)=crpb(17)
      e_times(2,j)=crpb(20)      
    enddo
!    do j=1,len
!      call IXFwrite_line(trim(handle(j)%runid),status)
!    enddo
    do j=1,len    
      call IXFwrite_line('Run ID : '//hdr(j)(1:8)//'        User: '//hdr(j)(9:28)//'  Inst: '// trim(user(j)),status)
    enddo
    
    if (len==1)then
      call IXFwrite_line('Protons: '//hdr(1)(73:80)//' uAhrs  Date: '//hdr(1)(53:72)//' to '//trim(e_times(1,1))//' '//trim(e_times(2,1)),status)  
    else
      write(sum_proton,'(f8.3)')proton_sum
      call IXFwrite_line('Protons: '//sum_proton//' uAhrs  Date: '//hdr(1)(53:72)//' to '//trim(e_times(1,len))//' '//trim(e_times(2,len)),status)      
    endif
    do j=1,len
      call IXFwrite_line(trim(run_title(j)),status)    
    end do

  end subroutine IXFhead_raw_file
!--------------------------------------
  subroutine special_tag(name,truename,p_index,t_index)
    implicit none
    character(len=*)::name
    character(len=256)::truename
    integer(i4b),allocatable::p_index(:)
    integer(i4b)::t_index

    t_index=0

    if(trim(name) == 'RID')then
      truename='HDR'
      allocate(p_index(2))
      p_index(1)=1
      p_index(2)=8
      t_index=4
      return
    endif

    if(trim(name) == 'SHORT_TITLE')then
      truename='HDR'
      allocate(p_index(2))
      p_index(1)=29
      p_index(2)=52
      t_index=4
      return
    endif

    if(trim(name) == 'START_DATE')then
      truename='HDR'
      allocate(p_index(2))
      p_index(1)=53
      p_index(2)=63
      t_index=4
      return
    endif

    if(trim(name) == 'START_TIME')then
      truename='HDR'
      allocate(p_index(2))
      p_index(1)=65
      p_index(2)=72
      t_index=4
      return
    endif

    if(trim(name) == 'UA_HOURS')then
      truename='HDR'
      allocate(p_index(2))
      p_index(1)=73
      p_index(2)=80
      t_index=4
      return
    endif

    
    if(trim(name) == 'USER_NAME')then
      truename='USER'
      allocate(p_index(1))
      p_index(1)=1
      t_index=3
      return
    endif
    
    if(trim(name) == 'USER_DAY1')then
      truename='USER'
      allocate(p_index(1))
      p_index(1)=2
      t_index=3
      return
    endif
    
    if(trim(name) == 'USER_DAY2')then
      truename='USER'
      allocate(p_index(1))
      p_index(1)=3
      t_index=3
      return
    endif

    if(trim(name) == 'USER_NIGHT')then
      truename='USER'
      allocate(p_index(1))
      p_index(1)=4
      t_index=3
      return
    endif

    if(trim(name) == 'USER_INST')then
      truename='USER'
      allocate(p_index(1))
      p_index(1)=5
      t_index=3
      return
    endif

    if(trim(name) == 'RUN_DURATION')then
      truename='IRPB'
      allocate(p_index(1))
      p_index(1)=1
      t_index=2
      return
    endif

    if(trim(name) == 'RUN_SCALAR')then
      truename='IRPB'
      allocate(p_index(1))
      p_index(1)=2
      t_index=2
      return
    endif

    if(trim(name) == 'GOOD_CHARGE')then
      truename='RRPB'
      allocate(p_index(1))
      p_index(1)=8
      t_index=1
      return
    endif

    if(trim(name) == 'TOTAL_CHARGE')then
      truename='RRPB'
      allocate(p_index(1))
      p_index(1)=9
      t_index=1
      return
    endif

    if(trim(name) == 'GOOD_FRAMES')then
      truename='IRPB'
      allocate(p_index(1))
      p_index(1)=10
      t_index=2
      return
    endif

    if(trim(name) == 'TOTAL_FRAMES')then
      truename='IRPB'
      allocate(p_index(1))
      p_index(1)=11
      t_index=2
      return
    endif

    if(trim(name) == 'END_DATE')then
      truename='CRPB'
      allocate(p_index(1))
      p_index(1)=17
      t_index=3
      return
    endif

    if(trim(name) == 'END_TIME')then
      truename='CRPB'
      allocate(p_index(1))
      p_index(1)=20
      t_index=3
      return
    endif
    
    ! this is just altering the search TAG in the RAW FILE
    ! not a section of an array, t_index=5
    if(trim(name) == 'TCHAN1')then
      truename='TIM1'
      t_index=5
      return
    endif
    
  end subroutine
!-----------------------------------------------------------------------------------------------------------------------

  recursive subroutine IXFget_generic(handle,name,value_r,value_i,value_c,index,status)
    use IXMtools
    implicit none    
    real(dp),pointer::value_r(:)
    integer(i4b),pointer::value_i(:)
    character(len=256),allocatable::value_c(:)
    integer(i4b)::index,length,err_r,err_i,err_c,t_index
    integer(i4b),allocatable::u_index(:)
    type(IXTraw_file)::handle
    character(len=*) :: name
    character(len=256)::truename
    type(IXTstatus)::status
    real(dp)::t_real
    integer(i4b)::t_int
    character(len=256)::t_char
    
    call upcase(name)
    value_r=>NULL()
    value_i=>NULL()
    if(allocated(value_c))deallocate(value_c)
      
    call IXFsize_raw(handle, name,length,status)
    if(status == IXCseverity_error)return
    ! first try real 
    call IXFalloc(value_r,length,status)    
    call IXFget_raw(handle,name,value_r,status,err_r)
    if(err_r /= 0)then
      ! failure with real => try int
      call IXFdealloc(value_r,status)
      value_r=>NULL()
      call IXFalloc(value_i,length,status)
      call IXFget_raw(handle,name,value_i,status,err_i)
      if(err_i/=0)then
      ! failure with int => try char
        call IXFdealloc(value_i,status)
        value_i=>NULL()
        allocate(value_c(length))
        call IXFget_raw(handle,name,value_c,status,err_c)
        if(err_c/=0)then
          deallocate(value_c)
          index=0
        else
          index=3
          return
        endif
      else
        index=2
        return
      endif        
    else
      index=1
      return
    endif
    ! if standard get fails call userdefined set
    call special_tag(name,truename,u_index,t_index)
    if (t_index /=0)then
      ! this call cannot fail by definition
      call IXFget_generic(handle,truename,value_r,value_i,value_c,index,status)
      select case (t_index)
        case(1)
          t_real=value_r(u_index(1))
          call IXFdealloc(value_r,status)
          call IXFalloc(value_r,1,status)
          value_r(1)=t_real
        case(2)
          t_int=value_i(u_index(1))
          call IXFdealloc(value_i,status)
          call IXFalloc(value_i,1,status)
          value_i(1)=t_int
        case(3)
          t_char=value_c(u_index(1))
          deallocate(value_c)
          allocate(value_c(1))
          value_c(1)=t_char
        case(4)
          t_char=value_c(1)(u_index(1):u_index(2))
          deallocate(value_c)
          allocate(value_c(1))          
          value_c(1)=t_char
        case(5)
          ! DO NOTHING
          ! only truename has been provided, no index into array has been
          ! required, appropriate value has been filled. DO NOTHING
      end select  
      return
    else
      call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error,&
           IXCerr_invparam,'Invalid raw file parameter: '//trim(name)//' (IXFget_generic)' )      
    endif
      
  end subroutine IXFget_generic

!-----------------------------------------------------------------------------------------------------------------------  
  subroutine IXFget_dp(handle, name, value,status, errcode)
    use IXMtools
    implicit none
    type(IXTraw_file) :: handle
    type(IXTstatus), target :: status
    character(len=*) :: name
    real(dp), intent(out) :: value
    real*4 :: value_real
    integer(i4b)::l_errcode
    integer(i4b),optional::errcode
    call upcase(name)
    value = IXCundef_dp
    call IXFcheck_raw_handle(handle, status)
    if (status == IXCseverity_error) return
    value_real = IXCundef_sp
    call IXFget_real(handle, name, value_real,status, l_errcode)
    value = dble(value_real)
    if(present(errcode))errcode=l_errcode
  end subroutine IXFget_dp

  subroutine IXFget_dp1(handle, name, value,status, errcode)
    use IXMtools
    implicit none
    type(IXTraw_file) :: handle
    type(IXTstatus), target :: status
    character(len=*) :: name
    real(dp), intent(out) :: value(:)
    real*4, allocatable :: value_real(:)
    integer(i4b)::l_errcode
    integer(i4b),optional::errcode
    call upcase(name)
    value = IXCundef_dp
    call IXFcheck_raw_handle(handle, status)
    if (status == IXCseverity_error) return
    allocate(value_real(size(value)))
    value_real = IXCundef_sp
    call IXFget_real1(handle, name, value_real,status, l_errcode)
    value = dble(value_real)
    deallocate(value_real)
    if(present(errcode))errcode=l_errcode
  end subroutine IXFget_dp1

  subroutine IXFget_dp2(handle, name, value,status, errcode)
    use IXMtools
    implicit none
    type(IXTraw_file) :: handle
    type(IXTstatus), target :: status
    character(len=*) :: name
    real(dp), intent(out) :: value(:,:)
    real*4, allocatable :: value_real(:)
    integer(i4b)::l_errcode
    integer(i4b),optional::errcode
    call upcase(name)
    value = IXCundef_dp
    call IXFcheck_raw_handle(handle, status)
    if (status == IXCseverity_error) return
    allocate(value_real(size(value)))
    value_real = IXCundef_sp
    call IXFget_real1(handle, name, value_real, status,l_errcode)
    value = reshape(value_real, shape(value), (/ IXCundef_sp /))
    deallocate(value_real)
    if(present(errcode))errcode=l_errcode
  end subroutine IXFget_dp2

  subroutine IXFget_char(handle, name, value,status,errcode)
    use IXMtools
    implicit none
    type(IXTraw_file) :: handle
    type(IXTstatus), target :: status
    character(len=*):: name
    character(len=*) :: value 
    integer(i4b),optional::errcode
    integer(i4b)::len_out, l_errcode
    call upcase(name)
    value = IXCundef_char
    call IXFcheck_raw_handle(handle, status)
    if (status == IXCseverity_error) return
    global_raw_status => status
    if (dae_access(handle)) then
	call IDFgetparc(handle%daeh, name, value, errcode)
    else
        call getparc(handle%runid, name, value, 1, len_out, l_errcode)
    endif
    global_raw_status => NULL()
    if(present(errcode))errcode=l_errcode
    
  end subroutine IXFget_char

  subroutine IXFget_char_c1(handle, name, value,status, errcode)
    use IXMtools
    implicit none
    type(IXTraw_file) :: handle
    type(IXTstatus), target :: status
    character(len=*) :: name
    character(len=*) :: value(:) ! really out
    integer(i4b):: i, j, ndims, len_out, l_errcode, dims_array(2)
    integer(i4b),optional::errcode
    call upcase(name)
    value = IXCundef_char
    call IXFcheck_raw_handle(handle, status)
    if (status == IXCseverity_error) return
    global_raw_status => status
    value = ' '
    if (dae_access(handle)) then
        dims_array(1) = len(value)
        dims_array(2) = size(value)
	ndims = 2
	call IDFgetparc(handle%daeh, name, value, dims_array, ndims, l_errcode)
! FAA: This assumes that value(1) is big enough to hold everything and we need
! to split to other array elements for e.g. CRPB
! first we need to remove any embedded NULL as this will make the string
! shorter when it is returned to matlab
	do i=1,len(value)
	    if (ichar(value(1)(i:i)) == 0) value(1)(i:) = ' '
	enddo
	do i=2,size(value)
	    j = 4 * i - 3
	    if (j <= len(value)) value(i) = value(1)(j:)
	enddo
    else
        call getparc(handle%runid, name, value, size(value), len_out, l_errcode)
!        call getparc(handle%runid, name, value, 1, len_out, errcode)
    endif
    global_raw_status => NULL()
    if(present(errcode))errcode=l_errcode
  end subroutine IXFget_char_c1

  subroutine IXFget_real(handle, name, value,status, errcode)
    use IXMtools
    implicit none
    type(IXTraw_file) :: handle
    type(IXTstatus), target :: status
    character(len=*) :: name
    real*4, intent(out) :: value
    integer(i4b):: len_out, l_errcode
    integer(i4b),optional::errcode
    call upcase(name)
    value = IXCundef_sp
    call IXFcheck_raw_handle(handle,status)
    if (status == IXCseverity_error) return
    global_raw_status => status
    if (dae_access(handle)) then
	call IDFgetparr(handle%daeh, name, value, l_errcode)
    else
        call getparr(handle%runid, name, value, 1, len_out, l_errcode)
    endif
    global_raw_status => NULL()
    if(present(errcode))errcode=l_errcode
  end subroutine IXFget_real

  subroutine IXFget_real1(handle, name, value,status, errcode)
    use IXMtools
    implicit none
    type(IXTraw_file) :: handle
    type(IXTstatus), target :: status
    character(len=*) :: name
    real*4, intent(out) :: value(:)
    integer(i4b):: len_out, l_errcode, dims_array(1)
    integer(i4b),optional::errcode
    call upcase(name)
    value = IXCundef_sp
    call IXFcheck_raw_handle(handle,status)
    if (status == IXCseverity_error) return
    global_raw_status => status
    if (dae_access(handle)) then
        dims_array(1) = size(value)
	call IDFgetparr(handle%daeh, name, value, dims_array, 1, l_errcode)
    else
        call getparr(handle%runid, name, value, size(value), len_out, l_errcode)
    endif
    global_raw_status => NULL()
    if(present(errcode))errcode=l_errcode
  end subroutine IXFget_real1

  subroutine IXFget_real2(handle, name, value, status,errcode)
    use IXMtools
    implicit none
    type(IXTraw_file) :: handle
    type(IXTstatus), target :: status
    character(len=*) :: name
    real*4, intent(out) :: value(:,:)
    integer(i4b):: len_out, l_errcode, dims_array(2)
    integer(i4b),optional::errcode
    call upcase(name)
    value = IXCundef_sp
    call IXFcheck_raw_handle(handle,status)
    if (status == IXCseverity_error) return
    global_raw_status => status
    if (dae_access(handle)) then
        dims_array = shape(value)
	call IDFgetparr(handle%daeh, name, value, dims_array, 2, l_errcode)
    else
        call getparr(handle%runid, name, value, size(value), len_out, l_errcode)
    endif
    global_raw_status => NULL()
    if(present(errcode))errcode=l_errcode
  end subroutine IXFget_real2

  subroutine IXFget_int(handle, name, value,status, errcode)
    use IXMtools
    implicit none
    type(IXTraw_file) :: handle
    type(IXTstatus), target :: status
    character(len=*) :: name
    integer :: value
    integer (i4b)::len_out, l_errcode
    integer(i4b),optional::errcode
    call upcase(name)
    value=IXCundef_i4b
    call IXFcheck_raw_handle(handle,status)
    if (status == IXCseverity_error) return
    global_raw_status => status
    if (dae_access(handle)) then
	call IDFgetpari(handle%daeh, name, value, l_errcode)
    else
        call getpari(handle%runid, name, value, 1, len_out, l_errcode)
    endif
    global_raw_status => NULL()
    if(present(errcode))errcode=l_errcode
  end subroutine IXFget_int

  subroutine IXFget_int1(handle, name, value, status, errcode)
    use IXMtools
    implicit none
    type(IXTraw_file) :: handle
    type(IXTstatus), target :: status
    character(len=*) :: name
    integer :: value(:)
    integer (i4b)::len_out, l_errcode, dims_array(1)
    integer(i4b),optional::errcode
    call upcase(name)
    value=IXCundef_i4b
    call IXFcheck_raw_handle(handle,status)
    if (status == IXCseverity_error) return
    global_raw_status => status
    if (dae_access(handle)) then
        dims_array(1) = size(value)
	call IDFgetpari(handle%daeh, name, value, dims_array, 1, l_errcode)
    else
        call getpari(handle%runid, name, value, size(value), len_out, l_errcode)
    endif
    global_raw_status => NULL()
    if(present(errcode))errcode=l_errcode
  end subroutine IXFget_int1

  subroutine IXFget_int2(handle, name, value,status, errcode)
    use IXMtools
    implicit none
    type(IXTraw_file) :: handle
    type(IXTstatus), target :: status
    character(len=*) :: name
    integer(i4b) :: value(:,:)
    integer len_out, l_errcode, dims_array(2)
    integer(i4b),optional::errcode
    call upcase(name)
    value=IXCundef_i4b
    call IXFcheck_raw_handle(handle,status)
    if (status == IXCseverity_error) return
    global_raw_status => status
    if (dae_access(handle)) then
        dims_array = shape(value)
	call IDFgetpari(handle%daeh, name, value, dims_array, 2, l_errcode)
    else
        call getpari(handle%runid, name, value, size(value), len_out, l_errcode)
    endif
    global_raw_status => NULL()
    if(present(errcode))errcode=l_errcode
  end subroutine IXFget_int2
  !-----------------------------------------------------------------------------------------------------------------------

  subroutine IXFget_data_i1(handle, spec_no, value, status)
    implicit none
    type(IXTraw_file) :: handle
    type(IXTstatus), target :: status
    integer len_value, ifsn, nos, errcode
    integer :: value(:), spec_no, dims_array(1)
    len_value = size(value)
    call IXFcheck_raw_handle(handle,status)
    if (status == IXCseverity_error) return
    global_raw_status => status
    if (dae_access(handle)) then
        dims_array(1) = size(value)
	call IDFgetdat(handle%daeh,spec_no,1,value,dims_array,1,errcode)
    else
        call getdat(handle%runid, spec_no, 1, value, len_value, errcode)
    endif
    global_raw_status => NULL()
  end subroutine IXFget_data_i1
  
  subroutine IXFget_data_i2(handle, spec_no, value, status)
    implicit none
    type(IXTraw_file) :: handle
    type(IXTstatus), target :: status
    integer len_value, spec_no(:), errcode,st_posn
    integer :: i, value(:,:),contig_start,num_contig,endspec,position ,dims_array(2)
    logical :: unloaded
    call IXFcheck_raw_handle(handle,status)
    if (status == IXCseverity_error) return
    global_raw_status => status
    !  call getdat(handle%runid, ifsn, nos, value, len_value, errcode)
    if (dae_access(handle)) then
       !      dims_array(1) = size(value,1)
       dims_array=shape(value)
       !do i = 1, size(value,2) !loop over number of spectra        
       unloaded=.true.    
       endspec=size(spec_no)
       position = 1
       do while(unloaded)       
          call get_contig(spec_no,contig_start,num_contig,position)
          ! this works
          call IDFgetdat(handle%daeh, contig_start, num_contig ,value(:,position-num_contig:position-1),(/ dims_array(1)*num_contig /),1,errcode)        
          !  neither of these two work
          !	     call DCgetdat(handle%daeh, contig_start, num_contig ,value(:,position-num_contig:position-1),(/ dims_array(2), dims_array(1) /),2,errcode)        
          !	     call DCgetdat(handle%daeh, contig_start, num_contig ,value(:,position-num_contig:position-1), dims_array,2,errcode)        
          if (errcode <0)then
             call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
                  IXCerr_invparam, 'IXTraw_file failure, error reading DAE data (IXFget_data_i2)')
             return
          endif
          if(position-1 == endspec)unloaded=.false.        
       enddo
    else
       len_value = size(value,1)
       do i = 1, size(value,2)
          call getdat(handle%runid, spec_no(i), 1, value(:,i), len_value, errcode)
       enddo
    endif
    global_raw_status => NULL()
  end subroutine IXFget_data_i2
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine get_contig(specno,sp_start,num_contig,position)
implicit none
logical::contig
integer(i4b)::specno(:),sp_start,num_contig,position,i,nspec
contig=.true.
num_contig=1
sp_start=specno(position)
nspec=size(specno)
do while(contig .and. position+1 <= nspec)
  if (specno(position+1)==specno(position)+1)then
    position=position+1
    num_contig=num_contig+1
  else
    contig=.false.
  endif
end do
position=position+1

end subroutine get_contig
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine close_raw_files(handle,status)
  implicit none
  type(IXTraw_file)::handle(:)
  type(IXTstatus)::status
  integer(i4b)::i
  call close_data_file()
  do i = 1, size(handle)
    call IXFdestroy_raw_file(handle(i),status)
  enddo
    
end subroutine close_raw_files

subroutine close_raw_file(handle,status)
  implicit none
  type(IXTraw_file)::handle
  type(IXTstatus)::status
  integer(i4b)::i
  call close_data_file()
  call IXFdestroy_raw_file(handle,status)
end subroutine close_raw_file

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module IXMraw_file

!---------------------------------------------------------------------------
! *** CCLRC ISIS Facility GET Routines ***
! *** Original routines by Kevin Knowles, modifications by Freddie Akeroyd 
!
! $Id: IXMraw_file.f90 1421 2008-07-11 16:08:00Z Dickon Champion $
!---------------------------------------------------------------------------

        SUBROUTINE CLOSE_DATA_FILE()
!
!  Forces the currently opened RAW file to be closed
!
!
        CHARACTER*120 FILNAM
        INTEGER*4 VER1,IFORMAT(10),IVER(10),NDET,NMON,NEFF, &
        NSEP,NTRG,NSP1,NTC1,ULEN,NPER,DATA_HEADER(32),IUNIT
        LOGICAL OPENFILE
        COMMON/crpt_SPECIALS/ FILNAM,VER1,IFORMAT,IVER, &
                NDET,NMON,NEFF,NSEP,NTRG,NSP1,NTC1,ULEN,OPENFILE,NPER, &
                DATA_HEADER
        SAVE /crpt_SPECIALS/
!
! for some reason 49 is alwyas used as the IUNIT to open any file so we have to use it here
        IUNIT=49
        FILNAM = ' ' 

	INQUIRE(UNIT=IUNIT,OPENED=OPENFILE)
	
	IF (OPENFILE) THEN
	    CLOSE(IUNIT)
	    OPENFILE=.FALSE.
    ENDIF
        
        RETURN
        END SUBROUTINE

!-------------------------------------------------------------------------
        SUBROUTINE GET_CRPT_SPECIALS(NTC,NSPEC,NPERIOD)
        CHARACTER*120 FILNAM
        INTEGER*4 VER1,IFORMAT(10),IVER(10),NDET,NMON,NEFF, &
          NSEP,NTRG,NSP1,NTC1,ULEN,NPER,DATA_HEADER(32)
        LOGICAL OPENFILE
	  INTEGER*4 NTC,NSPEC,NPERIOD
        COMMON/crpt_SPECIALS/ FILNAM,VER1,IFORMAT,IVER, &
                NDET,NMON,NEFF,NSEP,NTRG,NSP1,NTC1,ULEN,OPENFILE,NPER, &
                DATA_HEADER
        SAVE /crpt_SPECIALS/
!
	  CALL UPDATE_CRPT_SPECIALS
	  NTC=NTC1
	  NSPEC=NSP1
	  NPERIOD=NPER
        RETURN
        END SUBROUTINE
!-------------------------------------------------------------------------
        SUBROUTINE UPDATE_CRPT_SPECIALS
	  IMPLICIT NONE
        INTEGER*4 ITEMP(3),ISTATUS,I,TRUELEN,IERR
        CHARACTER*120 FILNAM
        INTEGER*4 VER1,IFORMAT(10),IVER(10),NDET,NMON,NEFF, &
         NSEP,NTRG,NSP1,NTC1,ULEN,NPER,DATA_HEADER(32)
        LOGICAL OPENFILE, DAE_ACCESS, CRPT_ACCESS
	  INTEGER*4 NTC,NSPEC,NPERIOD
	  INTEGER VAX_TO_LOCAL_INT, ERRCODE, FASTGET_INIT
	  EXTERNAL TRUELEN, VAX_TO_LOCAL_INT, VAX_TO_LOCAL_INTS
        COMMON/crpt_SPECIALS/ FILNAM,VER1,IFORMAT,IVER, &
                NDET,NMON,NEFF,NSEP,NTRG,NSP1,NTC1,ULEN,OPENFILE,NPER, &
                DATA_HEADER
        SAVE /crpt_SPECIALS/
	  IF (.NOT. (DAE_ACCESS(FILNAM) .OR. CRPT_ACCESS(FILNAM))) THEN
	       RETURN
	  ENDIF
!
! update bits of CRPT that may change druing a run
!
!  NDET,NMON,NEFF
        CALL GETSECT(IFORMAT(2)+67,3,ITEMP,49,IERR)
	  CALL VAX_TO_LOCAL_INTS(ITEMP, 3)
        NDET=ITEMP(1)
        NMON=ITEMP(2)
        NEFF=ITEMP(3)
!  NSEP
        IF (IVER(3).EQ.1) THEN
                CALL GETSECT(IFORMAT(3)+33,1,ITEMP,49,IERR)
        ELSE
                CALL GETSECT(IFORMAT(3)+65,1,ITEMP,49,IERR)
        ENDIF
        NSEP=VAX_TO_LOCAL_INT(ITEMP(1))
!  NTRG
        CALL GETSECT(IFORMAT(5)+1,1,ITEMP,49,IERR)
        NTRG=VAX_TO_LOCAL_INT(ITEMP(1))
        IF (NTRG.NE.1) THEN
!               WRITE(6,6030) NTRG
! 6030  FORMAT(' This version expects only 1 time regime. NTRG=',I9)
		write(6,*) 'NTRG problem',NTRG
        ENDIF
!  NPER
        CALL GETSECT(IFORMAT(5)+3,1,ITEMP,49,IERR)
        NPER=VAX_TO_LOCAL_INT(ITEMP(1))
!  NSP1,NTC1
        CALL GETSECT(IFORMAT(5)+260,2,ITEMP,49,IERR)
	  CALL VAX_TO_LOCAL_INTS(ITEMP, 2)
        NSP1=ITEMP(1)
        NTC1=ITEMP(2)
!--  USER section
!        IF (VER1 .EQ. 1)  THEN
!		ULEN=0
!		IVER(6)=0
!        ELSE
!          CALL GETSECT(IFORMAT(6),1,IVER(6),49,IERR)
!		CALL VAX_TO_LOCAL_INTS(IVER(6), 1)
!		 changed below to calculate ULEN by measuring the section size
!		 not by reading the ULEN value which is historically set to 1 by the ICP
!cc             CALL GETSECT(IFORMAT(6)+1,1,ITEMP,49,IERR)
!cc             ULEN=VAX_TO_LOCAL_INT(ITEMP(1))
!		ULEN = IFORMAT(7) - IFORMAT(6) -2
!        ENDIF
! DATA section header
!        do i=1,32
!                data_header(i)=0
!        enddo   
!        if ( (IVER(7) .GE. 2) .AND. (.NOT. DAE_ACCESS(RUNID)) ) then
!                CALL GETSECT(IFORMAT(7)+1,32,data_header,49,IERR)
!		CALL VAX_TO_LOCAL_INTS(data_header, 32)
!        endif
!  finally store the file name and set flags to true
!        FILNAM=RUNID
!        FOUND=.TRUE.
!       WRITE(6, *)' FILE OPENED'
        RETURN
        END SUBROUTINE

!-------------------------------------------------------------------------
        SUBROUTINE GETDAT(RUNID,IFSN,NOS,IDATA,LENGTH,ERRCODE)
        IMPLICIT NONE
!
! *** ERROR CODES ***
!
!    0 = all OK
!    1 = file RUNID not currently open
!    2 = file number is '00000' and so no access available
!    3 = asked for non-existent parameter
!    4 = TOO MANY SPECTRA ASKED FOR
!    5 = error in byte unpacking
!    6 = cannot understand data section
!
!
!  Gets data from a RAW file one or more spectra at a time
!
        CHARACTER*(*) RUNID
	LOGICAL CRPT_ACCESS, DAE_ACCESS
        INTEGER*4 ERRCODE,IFSN,NOS,LENGTH,IDATA(LENGTH)
!
        INTEGER*4 IWORK(128),IERR
        REAL*4 WORK(128)
        COMMON/crpt_WORK/IWORK
        SAVE /crpt_WORK/
        EQUIVALENCE (IWORK,WORK)
! temporary parameters
        INTEGER*4 IBASE,ILONG
        INTEGER I,J,ISTART,STATUS,ICOMPRESS,IBUFFER(33000)
!
!
        CHARACTER*120 FILNAM
	CHARACTER*128 MESS
        INTEGER*4 VER1,IFORMAT(10),IVER(10),NDET,NMON,NEFF, &
         NSEP,NTRG,NSP1,NTC1,ULEN,NPER,DATA_HEADER(32)
        LOGICAL OPENFILE
        COMMON/crpt_SPECIALS/ FILNAM,VER1,IFORMAT,IVER, &
                NDET,NMON,NEFF,NSEP,NTRG,NSP1,NTC1,ULEN,OPENFILE,NPER, &
                DATA_HEADER
        SAVE /crpt_SPECIALS/
	EXTERNAL CRPT_ACCESS, DAE_ACCESS
!
	  CALL UPDATE_CRPT_SPECIALS
        ERRCODE=0
!
!  decide whether it's CRPT or just a file
        IF (RUNID(4:8).EQ.'00000') THEN
!               WRITE(6,6000)
! 6000          FORMAT(' Accessing of current run not available')
!               GOTO 999
           ERRCODE=2
        ENDIF
!
!  check name is valid & open file according to RUNID
        IF ((FILNAM.NE.RUNID) .OR. (FILNAM.EQ.' ')) THEN
	   MESS = 'Access requested to '//RUNID//' when '//FILNAM//' open'
	   CALL FERROR_ADD('GETDAT', &
          MESS, &
          ' ')
           ERRCODE=1
           RETURN
        ENDIF
        IF (NOS*(NTC1+1) .GT. LENGTH) THEN
	   MESS = 'buffer not big enough for number of spectra requested'
	   CALL FERROR_ADD('GETDAT', &
          MESS, &
          ' ')
           ERRCODE=4
           RETURN
	ENDIF
!
!
!  read data into IDAT ....remembering there are NTC1+1 time channels and
! -----------------------------------------------------------------------
!  NSP1+1 spectra and now also NPER periods
!
        IF (IFSN.LT.0.OR.IFSN.GT.((NSP1+1)*NPER)-1) THEN
!               WRITE (6,6010) NSP1,IFSN
! 6010  FORMAT(' Sorry, only ',I6,' spectra. You asked for',I6)
                ERRCODE=4
                RETURN
        ENDIF
        IF (IFSN+NOS-1.GT.((NSP1+1)*NPER)-1) THEN
!               WRITE (6,6020) NSP1,IFSN+NOS-1
! 6020  FORMAT(' Sorry, only ',I6,' spectra. You asked for up to',I6)
                ERRCODE=4
                RETURN
        ENDIF
        IF (VER1.EQ.1) THEN
                IBASE=IFORMAT(6)
        ELSE
                IBASE=IFORMAT(7)
        ENDIF
        IF ( (IVER(7).LE.1) .OR. DAE_ACCESS(RUNID) ) THEN
! Original version of data section
                IBASE = IBASE + 1 + IFSN*(NTC1+1)
                ILONG = NOS*(NTC1+1)
!       TYPE *,' BASE and LONG',IBASE,ILONG
                CALL GETSECT(IBASE,ILONG,IDATA,49,IERR)
		CALL VAX_TO_LOCAL_INTS(IDATA, ILONG)
        ELSE
! New version of data section (may be compressed and not necessarily consecutive
! First pick up data section header, in particular compression type.
!CCCCC          CALL GETSECT(IBASE+1,32,DATA_header)
                ICOMPRESS=DATA_HEADER(1)
! The CRPT has the "compress" flag set, even though it isn't really compressed
                IF ( (ICOMPRESS .EQ. 0) .OR. CRPT_ACCESS(RUNID) ) THEN 
! uncompressed
                    IBASE = IBASE + DATA_HEADER(3) + IFSN*(NTC1+1)
                    ILONG=NOS*(NTC1+1)
                    CALL GETSECT(IBASE,ILONG,IDATA,49,IERR)
		    CALL VAX_TO_LOCAL_INTS(IDATA, ILONG)
                ELSEIF (ICOMPRESS.EQ.1) THEN 
! byte relative compression
                    J=1
                    DO I=IFSN,IFSN+NOS-1
                        ISTART=IBASE + DATA_HEADER(3) + 2*I
                        ILONG=2
                        CALL GETSECT(ISTART,ILONG,IBUFFER,49,IERR)
		    	CALL VAX_TO_LOCAL_INTS(IBUFFER, ILONG)
                        ISTART=IBASE + IBUFFER(2)
                        ILONG=IBUFFER(1)
                        CALL GETSECT(ISTART,ILONG,IBUFFER,49,IERR)
! *** We do not need to CALL VAX_TO_LOCAL_INTS() as handled in byte_rel_expn()
                        call byte_rel_expn(IBUFFER,ILONG*4,1,IDATA(J),NTC1+1,STATUS)
! *** odd number error codes are OK ***
                        IF (MOD(STATUS,2) .EQ. 0) THEN
                          ERRCODE=5
!                         TYPE *,' ERROR - decompressing spectrum',I
                          goto 999
                        ENDIF   
                        J = J + NTC1+1
                    ENDDO
                ELSE
                    ERRCODE=6
!                    TYPE *,' ERROR - cannot understand data section'
                ENDIF
        ENDIF
!
 999    RETURN
        END SUBROUTINE
!------------------------------------------------------------------
        SUBROUTINE GETPARC(RUNID,NAME,CVALUE,LENGTH_IN,LENGTH_OUT,ERRCODE)
        IMPLICIT NONE
!
! *** ERROR CODES ***
!
!    0 = all OK
!    1 = file RUNID not currently open
!    2 = file number is '00000' and so no access available
!    3 = asked for non-existent parameter
!    4 = other error
!
!
!  Gets character parameter(s) from RAW data file
!
        INTEGER*4 LENGTH_IN,LENGTH_OUT,ERRCODE,ILINES,I,ITEMP(33),IERR,NOTESECT
        CHARACTER*(*) RUNID
        CHARACTER*(*) NAME
        CHARACTER*(*) CVALUE(LENGTH_IN)
	INTEGER VAX_TO_LOCAL_INT
	EXTERNAL VAX_TO_LOCAL_INT
!
!
        CHARACTER*132 CTEMP
!	INTEGER*1 B(4)
        EQUIVALENCE (ITEMP,CTEMP)
!
!
        CHARACTER*120 FILNAM
        INTEGER*4 VER1,IFORMAT(10),IVER(10),NDET,NMON,NEFF, &
          NSEP,NTRG,NSP1,NTC1,ULEN,NPER,DATA_HEADER(32),K,SENUM
	INTEGER NLINES, VER9, LLEN, OFFSET, ILLEN
        LOGICAL OPENFILE
	CHARACTER*128 MESS
        COMMON/crpt_SPECIALS/ FILNAM,VER1,IFORMAT,IVER, &
                NDET,NMON,NEFF,NSEP,NTRG,NSP1,NTC1,ULEN,OPENFILE,NPER, &
                DATA_HEADER
        SAVE /crpt_SPECIALS/
!
	  CALL UPDATE_CRPT_SPECIALS
      ERRCODE=0
      CTEMP = ' '
      DO I = 1, LENGTH_IN
        CVALUE(I) = ' '
      ENDDO
! Unless we set it otherwise, assume returning a simple string
      LENGTH_OUT = 1
!
!  decide whether it's CRPT or just a file
        IF (RUNID(4:8).EQ.'00000') THEN
!               WRITE(6,6000)
! 6000          FORMAT(' Accessing of current run not available')
!               GOTO 999
           ERRCODE=2
        ENDIF
!
!  check name is valid & open file according to RUNID
        IF ((FILNAM .NE. RUNID) .OR. (FILNAM.EQ.' ')) THEN
           MESS = 'Access requested to '//RUNID//' when '//FILNAM//' open'
	   CALL FERROR_ADD('GETPARC', &
          MESS, &
          ' ')
           ERRCODE=1
           RETURN
        ENDIF
!
!  read variables into CVALUE
! ----------------------------
!
!  Format section
        IF (NAME.EQ.'HDR') THEN
                CALL GETSECT(1,20,ITEMP,49,IERR)
                CVALUE(1)=CTEMP
!  Run section
        ELSEIF (NAME.EQ.'TITL') THEN
                CALL GETSECT(IFORMAT(1)+2,20,ITEMP,49,IERR)
                CVALUE(1)=CTEMP
        ELSEIF (NAME.EQ.'USER') THEN
                CALL GETSECT(IFORMAT(1)+22,20,ITEMP,49,IERR)
                CVALUE(1)=CTEMP(1:20)
                CVALUE(2)=CTEMP(21:40)
                CVALUE(3)=CTEMP(41:60)
                CVALUE(4)=CTEMP(61:80)
                CALL GETSECT(IFORMAT(1)+42,20,ITEMP,49,IERR)
                CVALUE(5)=CTEMP(1:20)
                CVALUE(6)=CTEMP(21:40)
                CVALUE(7)=CTEMP(41:60)
                CVALUE(8)=CTEMP(61:80)
		LENGTH_OUT=8
!  Instrument section
        ELSEIF (NAME.EQ.'NAME') THEN
                CALL GETSECT(IFORMAT(2)+1,2,ITEMP,49,IERR)
                CVALUE(1)=CTEMP(1:8)
! LOG / Notes section
        ELSEIF (NAME.EQ.'NOTE') THEN
		IF (VER1 .EQ. 1) THEN
		    NOTESECT = 7
		ELSE
		    NOTESECT = 8
		ENDIF
		CALL GETSECT(IFORMAT(NOTESECT), 1, VER9, 49, IERR)
		VER9 = VAX_TO_LOCAL_INT(VER9)
!		WRITE(6,*) 'VER9 = ', ver9
!		write(6,*) 'Dumping section'
!		do i = 1, 100
!		    call getsect(iformat(notesect)+i, 1, b, 49, ierr)
!		    write(6,*) (b(j), char(b(j)), j=1,4)
!		enddo
		IF (VER9 .EQ. 0) THEN
                    ILINES=(IFORMAT(NOTESECT+1)-IFORMAT(NOTESECT))/20
		    NLINES=ILINES
		    OFFSET=IFORMAT(NOTESECT) + 2
		    ILLEN=20	! 20*4 characters
		    LLEN=80
!		    WRITE(6,*) 'Reading ',nlines, 'lines'
		    CTEMP = ' '
		    LENGTH_OUT=MIN(NLINES,LENGTH_IN)
                    DO I=1,LENGTH_OUT
		       K=OFFSET+(I-1)*ILLEN
                       CALL GETSECT(K,ILLEN,ITEMP,49,IERR)
                       CVALUE(I)=CTEMP(1:LLEN)
		    ENDDO
		ELSEIF (VER9 .EQ. 2) THEN
		    CALL GETSECT(IFORMAT(NOTESECT)+1, 1, NLINES, 49, IERR)
		    CALL VAX_TO_LOCAL_INTS(NLINES, 1)
!		    WRITE(6,*) 'Reading ',nlines, 'lines'
		    OFFSET = IFORMAT(NOTESECT) + 2
! Each line stored as a line length + data
		    LENGTH_OUT=MIN(NLINES,LENGTH_IN)
                    DO I=1,LENGTH_OUT
			CALL GETSECT(OFFSET, 1, LLEN, 49, IERR)
		        CALL VAX_TO_LOCAL_INTS(LLEN, 1)
!		        WRITE(6,*) 'Reading line length', llen
		        ILLEN = (LLEN-1)/4 + 1 
			CTEMP = ' '
                        CALL GETSECT(OFFSET+1,ILLEN,ITEMP,49,IERR)
                        CVALUE(I)=CTEMP(1:LLEN)
!			write(6,*) ctemp
			OFFSET = OFFSET + ILLEN + 1
		    ENDDO
 		ELSE
		    CALL GETSECT(IFORMAT(NOTESECT)+1, 1, NLINES, 49, IERR)
		    CALL VAX_TO_LOCAL_INTS(NLINES, 1)
		    ILLEN=20  ! 20*4 characters per line
		    LLEN=80
!		    WRITE(6,*) 'Reading ',nlines, 'lines'
		    OFFSET=IFORMAT(NOTESECT)+2
		    CTEMP = ' '
                    LENGTH_OUT=MIN(NLINES,LENGTH_IN)
                    DO I=1,LENGTH_OUT
			    K=OFFSET+(I-1)*ILLEN
                            CALL GETSECT(K,ILLEN,ITEMP,49,IERR)
                            CVALUE(I)=CTEMP(1:LLEN)
		    ENDDO
		ENDIF
                IF (NLINES.LE.0) THEN
                    CVALUE(1)=' No notes were made'
		    LENGTH_OUT=1
                ENDIF
		IF (LENGTH_OUT .LT. NLINES) THEN
           	    MESS = 'Not enough space to return all of NOTES section'
	            CALL FERROR_ADD('GETPARC', MESS, ' ')
		ENDIF
        ELSEIF (NAME.EQ.'CRPB') THEN
              CALL GETSECT(IFORMAT(1)+62,32,ITEMP,49,IERR)
!		CVALUE(1) = CTEMP
! uncommented three following rows DC 5/3/8
		CVALUE(17) = CTEMP(65:76)
		CVALUE(20) = CTEMP(77:84)
		LENGTH_OUT = 32
        ELSEIF (NAME.EQ.'CSPB') THEN
          IF (IVER(3).EQ.1) THEN
              CALL GETSECT(IFORMAT(3)+1,32,ITEMP,49,IERR)
!		CVALUE(1) = CTEMP
		CVALUE(20) = CTEMP(77:116)
		LENGTH_OUT = 32
          ELSE
              CALL GETSECT(IFORMAT(3)+1,64,ITEMP,49,IERR)
!		CVALUE(1) = CTEMP
		CVALUE(20) = CTEMP(77:116)
		LENGTH_OUT = 64
          ENDIF
! SE block section
        ELSEIF (NAME(1:3) .EQ.'CSE') THEN
		READ(NAME(4:5), '(I2.2)') SENUM
		IF ((IVER(3) .EQ. 1) .OR. (NSEP .LT. SENUM)) THEN
     		    MESS = 'Invalid SE block '//NAME
	   	    CALL FERROR_ADD('GETPARC', &
      			MESS, &
          	    ' ')
            ERRCODE=4
		ELSE
!		    WRITE(6,'(1X,A,I2.2)') 'Reading SE',SENUM
		    K = IFORMAT(3)+34+32*SENUM
                    CALL GETSECT(K,32,ITEMP,49,IERR)
!		    CVALUE(1) = CTEMP
		    CVALUE(1) = CTEMP(1:8)
		    CVALUE(5) = CTEMP(17:24)
		    LENGTH_OUT=32
!		    DO I=1,32
!			IJUNK=IVALUE(I)
!		        WRITE(6,'(4(1X,Z2,A,A1))') (BJUNK(J), '=', CHAR(BJUNK(J)), J=1,4)
!		    ENDDO
		ENDIF
!  non existant requests
        ELSE
                ERRCODE=3
!     	    	MESS = 'No such CHARACTER parameter as '//NAME
!	        CALL FERROR_ADD('GETPARC',  MESS, ' ')
!               TYPE *,' No such CHARACTER parameter as',NAME
        ENDIF
!
!
!
 999    RETURN
        END SUBROUTINE
!
!GET routines modified to cope with periods Feb89 !!
!-------------------------------------------------------------------------
        SUBROUTINE GETPARI(RUNID,NAME,IVALUE,LENGTH_IN, &
     			   LENGTH_OUT,ERRCODE)
        IMPLICIT NONE
!
! *** ERROR CODES ***
!
!    0 = all OK
!    1 = file RUNID not currently open
!    2 = file number is '00000' and so no access available
!    3 = asked for non-existent parameter
!    4 = other error
!
!  Gets named integer paramter(s) from a RAW data file
!  Whole sections may also be requested
!
        CHARACTER*(*) RUNID
        CHARACTER*(*) NAME
        INTEGER*4 ERRCODE,LENGTH_IN,LENGTH_OUT,LENGTH, &
                 IVALUE(LENGTH_IN),IFROM,IERR,NSP
!
!
        INTEGER*4 I, J, IWORK(128), SENUM, NOTESECT
        REAL*4 WORK(128)
        COMMON/crpt_WORK/IWORK
        SAVE /crpt_WORK/
        EQUIVALENCE (IWORK,WORK)
!
        CHARACTER*120 FILNAM
	CHARACTER*128 MESS
	INTEGER NTC,NDETY,NUSE
        INTEGER*4 VER1,IFORMAT(10),IVER(10),NDET,NMON,NEFF,K, &
                 NSEP,NTRG,NSP1,NTC1,ULEN,NPER,DATA_HEADER(32)
	INTEGER*4 IJUNK, OFFSET, LLEN, ILLEN
	INTEGER*1 BJUNK(4)
        LOGICAL OPENFILE
!
        COMMON/crpt_SPECIALS/ FILNAM,VER1,IFORMAT,IVER, &
               NDET,NMON,NEFF,NSEP,NTRG,NSP1,NTC1,ULEN,OPENFILE,NPER, &
               DATA_HEADER
        SAVE /crpt_SPECIALS/
	EQUIVALENCE (IJUNK,BJUNK)
	INTRINSIC CHAR
	  CALL UPDATE_CRPT_SPECIALS
	LENGTH=LENGTH_IN
	LENGTH_OUT=0
        ERRCODE=0
	IERR = 0
!
!  decide whether it's CRPT or just a file
        IF (RUNID(4:8) .EQ. '00000') THEN
	   	CALL FERROR_ADD('GETPARI', 'Runid is 00000', ' ')
                ERRCODE=2
                RETURN
        ENDIF
!
!  check name is valid & open file according to RUNID
!
      	CALL OPEN_DATA_FILE(RUNID,NTC,NDETY,NUSE,ERRCODE)
	IF (ERRCODE .EQ. 1) RETURN
!
!  read variables into IVALUE
! ----------------------------
!
!  From now on just decide what has been requested and return it
        IF     (NAME.EQ.'VER1') THEN
                IVALUE(1)=VER1
		LENGTH = 1
        ELSEIF (NAME.EQ.'SFMT') THEN
                CALL GETSECT(1,31,IVALUE,49,IERR)
		LENGTH = 31
		CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
!
!  run section
        ELSEIF (NAME.EQ.'SRUN') THEN
                CALL GETSECT(IFORMAT(1),94,IVALUE,49,IERR)
		LENGTH = 94
		CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
        ELSEIF (NAME.EQ.'VER2') THEN
                CALL GETSECT(IFORMAT(1),1,IVALUE,49,IERR)
		LENGTH = 1
		CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
        ELSEIF (NAME.EQ.'RUN') THEN
                CALL GETSECT(IFORMAT(1)+1,1,IVALUE,49,IERR)
		LENGTH = 1
		CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
        ELSEIF ((NAME.EQ.'RPB') .OR. (NAME.EQ.'IRPB')) THEN
                CALL GETSECT(IFORMAT(1)+62,32,IVALUE,49,IERR)
		LENGTH = 32
		CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
!
!  instrument section
        ELSEIF (NAME.EQ.'SINS') THEN
          IF (IVER(1).EQ.1) THEN
            CALL GETSECT(IFORMAT(2),70+(NMON*2)+(6+NEFF)*NDET, &
                        IVALUE,49,IERR)
		LENGTH = 70+(NMON*2)+(6+NEFF)*NDET
		CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
          ELSE
            CALL GETSECT(IFORMAT(2),70+(NMON*2)+(5+NEFF)*NDET, &
                        IVALUE,49,IERR)
		LENGTH = 70+(NMON*2)+(5+NEFF)*NDET
		CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
          ENDIF
        ELSEIF (NAME.EQ.'VER3') THEN
                IVALUE(1)=IVER(2)
		LENGTH = 1
        ELSEIF (NAME.EQ.'IVPB') THEN
                CALL GETSECT(IFORMAT(2)+3,64,IVALUE,49,IERR)
		LENGTH = 64
		CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
        ELSEIF (NAME.EQ.'NDET') THEN
                IVALUE(1)=NDET
		LENGTH = 1
        ELSEIF (NAME.EQ.'NMON') THEN
                IVALUE(1)=NMON
		LENGTH = 1
!==cmm    ELSEIF (NAME.EQ.'NEFF') THEN
!               IVALUE(1)=NEFF
!		LENGTH = 1
        ELSEIF (NAME.EQ.'NUSE') THEN
                IVALUE(1)=NEFF
		LENGTH = 1
        ELSEIF (NAME.EQ.'MDET') THEN
                CALL GETSECT(IFORMAT(2)+70,NMON,IVALUE,49,IERR)
		LENGTH = NMON
		CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
        ELSEIF (NAME.EQ.'MONP') THEN
                CALL GETSECT(IFORMAT(2)+70+NMON,NMON,IVALUE,49,IERR)
		LENGTH = NMON
		CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
        ELSEIF (NAME.EQ.'SPEC') THEN
                IFROM=IFORMAT(2)+70+2*NMON
                CALL GETSECT(IFROM,NDET,IVALUE,49,IERR)
		LENGTH = NDET
		CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
        ELSEIF (NAME.EQ.'CODE') THEN
                IF (IVER(2).NE.1) THEN
                IFROM=IFORMAT(2)+70+2*NMON+3*NDET
                CALL GETSECT(IFROM,NDET,IVALUE,49,IERR)
		LENGTH = NDET
		CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
		ELSE
		IERR=1
                ENDIF
        ELSEIF (NAME.EQ.'TIMR') THEN
          IF (VER1.EQ.1) THEN
                IFROM=IFORMAT(2)+70+2*NMON+NDET
          ELSE
                IFROM=IFORMAT(4)+65+3*NDET
          ENDIF
          CALL GETSECT(IFROM,NDET,IVALUE,49,IERR)
		LENGTH = NDET
		CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
!
!  sample environment section
        ELSEIF (NAME.EQ.'SSEN') THEN
          IF (IVER(3).EQ.1) THEN
                CALL GETSECT(IFORMAT(3),34+NSEP*24,IVALUE,49,IERR)
		LENGTH = 34+NSEP*24
		CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
          ELSE
                CALL GETSECT(IFORMAT(3),66+NSEP*32,IVALUE,49,IERR)
		LENGTH = 66+NSEP*24
		CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
          ENDIF
          IF (NSEP.NE.0) THEN
	   	CALL FERROR_ADD('GETPARI', &
     		'GETPAR needs adjusting to take account of SE', &
          	' ')
             ERRCODE=4
          ENDIF
        ELSEIF (NAME.EQ.'VER4') THEN
                CALL GETSECT(IFORMAT(3),1,IVALUE,49,IERR)
		LENGTH = 1
		CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
        ELSEIF ((NAME.EQ.'SPB ') .OR. (NAME.EQ.'ISPB')) THEN
          IF (IVER(3).EQ.1) THEN
                CALL GETSECT(IFORMAT(3)+1,32,IVALUE,49,IERR)
		LENGTH = 32
		CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
          ELSE
                CALL GETSECT(IFORMAT(3)+1,64,IVALUE,49,IERR)
		LENGTH = 64
		CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
          ENDIF
        ELSEIF (NAME.EQ.'NSEP') THEN
                IVALUE(1)=NSEP
		LENGTH = 1
        ELSEIF (NAME(1:2) .EQ.'SE') THEN
		READ(NAME(3:4), '(I2.2)') SENUM
		IF ((IVER(3) .EQ. 1) .OR. (NSEP .LT. SENUM)) THEN
     		    MESS = 'Invalid SE block '//NAME
	   	    CALL FERROR_ADD('GETPARI', &
     		    MESS, &
          	    ' ')
                    ERRCODE=4
		ELSE
!		    WRITE(6,'(1X,A,I2.2)') 'Reading SE',SENUM
		    K = IFORMAT(3)+34+32*SENUM
                    CALL GETSECT(K,32,IVALUE,49,IERR)
		    LENGTH = 32
		    CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
!		    DO I=1,32
!			IJUNK=IVALUE(I)
!		        WRITE(6,'(4(1X,Z2,A,A1))') (BJUNK(J), '=', CHAR(BJUNK(J)), J=1,4)
!		    ENDDO
		ENDIF
!  DAE section
        ELSEIF (NAME.EQ.'SDAE') THEN
          IF (IVER(4).EQ.1) THEN
                CALL GETSECT(IFORMAT(4),65+3*NDET,IVALUE,49,IERR)
		LENGTH = 65+3*NDET
          ELSE
                CALL GETSECT(IFORMAT(4),65+5*NDET,IVALUE,49,IERR)
		LENGTH = 65+5*NDET
          ENDIF
	  CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
        ELSEIF (NAME.EQ.'VER5') THEN
                CALL GETSECT(IFORMAT(4),1,IVALUE,49,IERR)
		LENGTH = 1
		CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
        ELSEIF (NAME.EQ.'DAEP') THEN
                CALL GETSECT(IFORMAT(4)+1,64,IVALUE,49,IERR)
		LENGTH = 64
		CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
        ELSEIF (NAME.EQ.'CRAT') THEN
                CALL GETSECT(IFORMAT(4)+65,NDET,IVALUE,49,IERR)
		LENGTH = NDET
		CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
        ELSEIF (NAME.EQ.'MODN') THEN
                CALL GETSECT(IFORMAT(4)+65+NDET,NDET,IVALUE,49,IERR)
		LENGTH = NDET
		CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
        ELSEIF (NAME.EQ.'MPOS') THEN
                CALL GETSECT(IFORMAT(4)+65+2*NDET,NDET,IVALUE,49,IERR)
		LENGTH = NDET
		CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
        ELSEIF (NAME.EQ.'UDET') THEN
                CALL GETSECT(IFORMAT(4)+65+4*NDET,NDET,IVALUE,49,IERR)
		LENGTH = NDET
		CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
!
!  TCB section
        ELSEIF (NAME.EQ.'STCB') THEN
                IF (NTRG.NE.1) THEN
	   	   CALL FERROR_ADD('GETPARI', &
      		   'GETPAR needs adjusting to take account of SE', &
           	   ' ')
                   ERRCODE=4
 		ELSE
                   CALL GETSECT(IFORMAT(5),288+NTC1+1,IVALUE,49,IERR)
		   LENGTH = 288+NTC1+1
		   CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
                ENDIF
        ELSEIF (NAME.EQ.'VER6') THEN
                CALL GETSECT(IFORMAT(5),1,IVALUE,49,IERR)
		LENGTH = 1
		CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
        ELSEIF (NAME.EQ.'NTRG') THEN
                IF (NTRG.NE.1) THEN
	   	   CALL FERROR_ADD('GETPARI', &
                  'Multiple time regimes....GETPAR needs changing', &
          	   ' ')
                   ERRCODE=4
		ELSE
                   IVALUE(1)=NTRG
		   LENGTH = 1
                ENDIF
        ELSEIF (NAME.EQ.'NFPP') THEN
                CALL GETSECT(IFORMAT(5)+2,1,IVALUE,49,IERR)
		LENGTH = 1
		CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
        ELSEIF (NAME.EQ.'NPER') THEN
                IVALUE(1)=NPER
		LENGTH = 1
        ELSEIF (NAME.EQ.'PMAP') THEN
                CALL GETSECT(IFORMAT(5)+4,256,IVALUE,49,IERR)
		LENGTH = 256
		CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
        ELSEIF (NAME.EQ.'NSP1') THEN
                IVALUE(1)=NSP1
		LENGTH = 1
        ELSEIF (NAME.EQ.'NTC1') THEN
                IVALUE(1)=NTC1
		LENGTH = 1
        ELSEIF (NAME.EQ.'TCM1') THEN
                CALL GETSECT(IFORMAT(5)+262,5,IVALUE,49,IERR)
		LENGTH = 5
		CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
        ELSEIF (NAME.EQ.'PRE1') THEN
                CALL GETSECT(IFORMAT(5)+287,1,IVALUE,49,IERR)
		LENGTH = 1
		CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
        ELSEIF (NAME.EQ.'TCB1') THEN
                CALL GETSECT(IFORMAT(5)+288,NTC1+1,IVALUE,49,IERR)
		LENGTH = NTC1+1
		CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
        ELSEIF (NAME.EQ.'DHDR') THEN
		DO I=1, 32
		    IVALUE(I) = DATA_HEADER(I)
		ENDDO
		LENGTH = 32
	ELSEIF (NAME .EQ. 'ULEN') THEN
		IVALUE(1) = ULEN
		LENGTH = 1
! User section
        ELSEIF (NAME.EQ.'VER7') THEN
		IF (VER1 .EQ. 1) THEN
	            CALL FERROR_ADD('GETPARI', &
           		    'No USER section, so no VER7 parameter', &
     			    ' ')
		ELSE
                    CALL GETSECT(IFORMAT(6),1,IVALUE,49,IERR)
		    LENGTH = 1
		    CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
		ENDIF
! data section section
        ELSEIF (NAME.EQ.'VER8') THEN
		IF (VER1 .EQ. 1) THEN
                    CALL GETSECT(IFORMAT(6),1,IVALUE,49,IERR)
		ELSE
                    CALL GETSECT(IFORMAT(7),1,IVALUE,49,IERR)
		ENDIF
		LENGTH = 1
		CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
	ELSEIF (NAME .EQ. 'CNT1') THEN
        	CALL GETDAT(RUNID,0,(NSP1+1)*NPER,IVALUE,LENGTH,ERRCODE)
		IF (ERRCODE .EQ. 0) THEN
		    LENGTH = (NSP1+1)*NPER*(NTC1+1)
		    CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
		ENDIF
! *** status section; only when running, and between format and run sections
! *** should be a max of 65 i*4 long
        ELSEIF ( (NAME.EQ.'STAT') .OR. (NAME .EQ. 'ISTAT') ) THEN
		LENGTH = MIN(65,IFORMAT(1)-32,LENGTH_IN)
	   	IF (LENGTH .GT. 0) THEN
                    CALL GETSECT(32,LENGTH,IVALUE,49,IERR)
		    CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
		ELSE
		    CALL FERROR_ADD('GETPARI','STATUS section is not available',' ')
	 	ENDIF
! NOTES section
        ELSEIF (NAME.EQ.'VER9') THEN
		IF (VER1 .EQ. 1) THEN
! Don't think this version number exists
!		    IVALUE(1) = 1
                    CALL GETSECT(IFORMAT(7),1,IVALUE,49,IERR)
		ELSE
                    CALL GETSECT(IFORMAT(8),1,IVALUE,49,IERR)
		ENDIF
		LENGTH = 1
		CALL VAX_TO_LOCAL_INTS(IVALUE, LENGTH)
! Max Line length in notes section (bytes)
        ELSEIF (NAME.EQ.'NTLL') THEN
		IF (VER1 .EQ. 1) THEN
		    NOTESECT = 7
		ELSE
		    NOTESECT = 8
		ENDIF
		IF (IVER(8) .LT. 2) THEN
		    IVALUE(1) = 80
		ELSE
! Get number of lines into J
  		    CALL GETSECT(IFORMAT(NOTESECT)+1, 1, J, 49, IERR)
		    OFFSET = IFORMAT(NOTESECT) + 2
! Each line stored as a line length + data
		    IF (J .LT. 1) THEN
			IVALUE(1) = 80
		    ELSE
		        IVALUE(1) = 0
		    ENDIF
                    DO I=1,J
			CALL GETSECT(OFFSET, 1, LLEN, 49, IERR)
			ILLEN = (LLEN - 1)/4 + 1
			IVALUE(1) = MAX(IVALUE(1), ILLEN*4)
			OFFSET = OFFSET + ILLEN + 1
		    ENDDO
                ENDIF
		LENGTH = 1
        ELSEIF (NAME.EQ.'FORM') THEN
		IVALUE(1) = IFORMAT(10)
		LENGTH = 1
! Number of lines in notes section
        ELSEIF (NAME.EQ.'NTNL') THEN
		IF (VER1 .EQ. 1) THEN
		    NOTESECT = 7
		ELSE
		    NOTESECT = 8
		ENDIF
		IF (IVER(8) .EQ. 0) THEN
                    IVALUE(1)=(IFORMAT(NOTESECT+1)-IFORMAT(NOTESECT))/20
                ELSE
		    CALL GETSECT(IFORMAT(NOTESECT)+1, 1, IVALUE, 49, IERR)
		    CALL VAX_TO_LOCAL_INTS(IVALUE, 1)
		ENDIF
		IF (IVALUE(1) .LT. 1) IVALUE(1) = 1
		LENGTH = 1

!  non existant requests

        ELSE
                ERRCODE=3
		LENGTH = 0
!     	    	MESS = 'No such INTEGER parameter as '//NAME
!	        CALL FERROR_ADD('GETPARI', 
!     1      		    MESS,
!     2			    ' ')
!               TYPE *,' No such INTEGER parameter as',NAME
        ENDIF
	IF ((IERR .NE. 0) .AND. (ERRCODE .EQ. 0)) THEN
     	    MESS = 'Error in reading data from file '//RUNID
	    CALL FERROR_ADD('GETPARI', &
            		    MESS, &
     			    ' ')
	    ERRCODE = 4
	ENDIF
!
	LENGTH_OUT=LENGTH
        RETURN
        END SUBROUTINE
!----------------------------------------------------------------------
        SUBROUTINE GETRUN(RUNID,IARRAY,LENGTH,IERROR)
        IMPLICIT NONE
!
! IERROR = 0 : ALL OK
! IERROR = 1 : FILE OPEN ERROR
! if error in GETDAT, returns 10+(error code in getdat)
!
!  This routine returns the whole of a run file into the given array
!
        CHARACTER*(*) RUNID
        INTEGER*4 LENGTH,IERROR,ERRCODE,IERR
        INTEGER*4 IARRAY(LENGTH)
!
        LOGICAL FOUND
        INTEGER start_of_data
!
!
        CHARACTER*120 FILNAM
        INTEGER*4 VER1,IFORMAT(10),IVER(10),NDET,NMON,NEFF, &
          NSEP,NTRG,NSP1,NTC1,ULEN,NPER,DATA_HEADER(32)
        LOGICAL OPENFILE
        COMMON/crpt_SPECIALS/ FILNAM,VER1,IFORMAT,IVER, &
               NDET,NMON,NEFF,NSEP,NTRG,NSP1,NTC1,ULEN,OPENFILE,NPER, &
               DATA_HEADER
	SAVE /crpt_SPECIALS/
!
!
	  CALL UPDATE_CRPT_SPECIALS
!  check name is valid & open file according to RUNID
	IERROR = 0
        IF ((FILNAM.NE.RUNID) .OR. (FILNAM.EQ.' ')) THEN
                CALL OPEN_FILE(RUNID,FOUND)
                IF (.NOT.FOUND) THEN
                        IERROR=1
                        RETURN
                ENDIF
        ENDIF
!  want whole file except for log section
! get this in 2 parts: first the file up to the data section+data version number
        IF (VER1.EQ.1) THEN
            start_of_data=IFORMAT(6)
        ELSE
            start_of_data=IFORMAT(7)
        ENDIF
        CALL GETSECT(1,start_of_data,IARRAY,49,IERR)
	CALL VAX_TO_LOCAL_INTS(IARRAY, start_of_data)
!       CALL GETSECT(1,start_of_data+1+(NSP1+1)*(NTC1+1)*NPER,IARRAY)
! and now the data...
        CALL GETDAT(RUNID,0,(NSP1+1)*NPER,IARRAY(start_of_data+1),LENGTH,ERRCODE)
	IF (ERRCODE .NE. 0) THEN
	   IERROR=10+ERRCODE
	   RETURN
	ENDIF
! for compressed file, log pointer points to wrong place.
! Recalculate for data version 2.
        IF (IARRAY(start_of_data).eq.2) then
                IARRAY(29)=IFORMAT(7)+1+(NSP1+1)*(NTC1+1)*NPER
        ENDIF
! we now have uncompressed data in a Version 1 format. Set data version to 1
        IARRAY(start_of_data)=1
        RETURN
        END SUBROUTINE
!-------------------------------------------------------------------------
      SUBROUTINE OPEN_DATA_FILE(RUNID,NTC,NDET,NUSE,ERRCODE)
      IMPLICIT NONE
!
! *** INPUT PARAMETERS (UNCHANGED BY THIS ROUTINE) ***
!
! RUNID - name of data file to open
!
! *** RETURNED VALUES ***
!
! NTC - number of time channels (minimum value for NTCMAX)
! NSP - number of spectra
! NDET - the number of detectors (minimum value for NDETMAX)
! NUSE - number of user-defined UTn tables
! ERRCODE - returned error code: 0 = all OK, a l'UNIX
!                                1 = file not found or error in opening it
!
      CHARACTER*(*) RUNID
      INTEGER*4 NTC,NDET,NUSE,ERRCODE,TRUELEN,NSP
        LOGICAL FOUND
!
! common block stuff
!
        CHARACTER*120 FILNAM
	CHARACTER*128 MESS
        INTEGER*4 VER1,IFORMAT(10),IVER(10),NDET1,NMON,NEFF, &
                 NSEP,NTRG,NSP1,NTC1,ULEN,NPER,DATA_HEADER(32)
        LOGICAL OPENFILE
!
        COMMON/CRPT_SPECIALS/ FILNAM,VER1,IFORMAT,IVER, &
               NDET1,NMON,NEFF,NSEP,NTRG,NSP1,NTC1,ULEN, &
               OPENFILE,NPER,DATA_HEADER
        SAVE /CRPT_SPECIALS/
!
      EXTERNAL OPEN_FILE, FERROR_ADD, TRUELEN
! Open file and set up CRPT_SPECIALS
      ERRCODE=0
      FOUND=.FALSE.
      IF (FILNAM .NE. RUNID) THEN
         FILNAM = ' '
         CALL OPEN_FILE(RUNID,FOUND)
         IF (.NOT. FOUND) THEN
     	    MESS = 'File '//RUNID(:TRUELEN(RUNID))//' not found'
	    CALL FERROR_ADD('OPEN_DATA_FILE', MESS, ' ')
            ERRCODE=1
            RETURN
         ENDIF
      ENDIF
      NTC=NTC1
      NDET=NDET1
      NUSE=NEFF
      NSP=NSP1
      RETURN
      END SUBROUTINE
!---------------------------------------------------------------------------
        SUBROUTINE OPEN_FILE(RUNID,FOUND)
        IMPLICIT NONE
!
!  Opens a RAW data file for reading. If a different fiole is already
!  open it will be closed first.
!
        CHARACTER*(*) RUNID
        LOGICAL FOUND
!
        INTEGER*4 ITEMP(3),ISTATUS,I,TRUELEN,IERR
!
        CHARACTER*120 FILNAM
	CHARACTER*120 CFILTMP
        INTEGER*4 IFILTMP(15),VER1,IFORMAT(10),IVER(10),NDET,NMON,NEFF, &
          NSEP,NTRG,NSP1,NTC1,ULEN,NPER,DATA_HEADER(32)
        LOGICAL OPENFILE, DAE_ACCESS, CRPT_ACCESS
        COMMON/crpt_SPECIALS/ FILNAM,VER1,IFORMAT,IVER, &
                NDET,NMON,NEFF,NSEP,NTRG,NSP1,NTC1,ULEN, &
                OPENFILE,NPER,DATA_HEADER
        SAVE /crpt_SPECIALS/
	INTEGER VAX_TO_LOCAL_INT, ERRCODE, FASTGET_INIT
	EXTERNAL TRUELEN, VAX_TO_LOCAL_INT, VAX_TO_LOCAL_INTS, &
     		DAE_ACCESS, CRPT_ACCESS
	EQUIVALENCE (CFILTMP,IFILTMP)
!
            
!  Check that the file exists
	I = TRUELEN(RUNID)
	IF ( DAE_ACCESS(RUNID) .OR. CRPT_ACCESS(RUNID) ) THEN
            FOUND=.TRUE.
        ELSE
            INQUIRE(FILE=RUNID(1:I),EXIST=FOUND)
	ENDIF
        IF (.NOT. FOUND) THEN
!               WRITE(6,6000)RUNID
! 6000          FORMAT(' File ',A,' not found.')
                RETURN
        ENDIF
!
!  Open new file it
!
	CFILTMP = RUNID
! *** Hack - we have FILNAM in common block, so temporarily assign it for getsect_orig.f to read
	FILNAM=RUNID
	ERRCODE = FASTGET_INIT(IFILTMP, 49)
        FILNAM=' '
	IF (ERRCODE .NE. 0) THEN
	    FOUND=.FALSE.
	    RETURN
        ENDIF
!
!  Now pick out the vital parameters for future use
!
!  version number
        CALL GETSECT(21,1,ITEMP,49,IERR)
        VER1=VAX_TO_LOCAL_INT(ITEMP(1))
!--  Format section
        CALL GETSECT(22,10,IFORMAT,49,IERR)
	CALL VAX_TO_LOCAL_INTS(IFORMAT, 10)
!--  RUN section
        CALL GETSECT(IFORMAT(1),1,IVER(1),49,IERR)
	CALL VAX_TO_LOCAL_INTS(IVER(1), 1)
!--  Instrument section
        CALL GETSECT(IFORMAT(2),1,IVER(2),49,IERR)
	CALL VAX_TO_LOCAL_INTS(IVER(2), 1)
!  NDET,NMON,NEFF
        CALL GETSECT(IFORMAT(2)+67,3,ITEMP,49,IERR)
	CALL VAX_TO_LOCAL_INTS(ITEMP, 3)
        NDET=ITEMP(1)
        NMON=ITEMP(2)
        NEFF=ITEMP(3)
!--  SE section
        CALL GETSECT(IFORMAT(3),1,IVER(3),49,IERR)
	CALL VAX_TO_LOCAL_INTS(IVER(3), 1)
!  NSEP
        IF (IVER(3).EQ.1) THEN
                CALL GETSECT(IFORMAT(3)+33,1,ITEMP,49,IERR)
        ELSE
                CALL GETSECT(IFORMAT(3)+65,1,ITEMP,49,IERR)
        ENDIF
        NSEP=VAX_TO_LOCAL_INT(ITEMP(1))
!--  DAE section
        CALL GETSECT(IFORMAT(4),1,IVER(4),49,IERR)
	CALL VAX_TO_LOCAL_INTS(IVER(4), 1)
!--  TCB section
        CALL GETSECT(IFORMAT(5),1,IVER(5),49,IERR)
	CALL VAX_TO_LOCAL_INTS(IVER(5), 1)
!  NTRG
        CALL GETSECT(IFORMAT(5)+1,1,ITEMP,49,IERR)
        NTRG=VAX_TO_LOCAL_INT(ITEMP(1))
        IF (NTRG.NE.1) THEN
!               WRITE(6,6030) NTRG
! 6030  FORMAT(' This version expects only 1 time regime. NTRG=',I9)
!		write(6,*) call ferror_add('NTRG problem',NTRG
			call ferror_add('GET','NTRG problem', ' ')
                FOUND=.FALSE.
                RETURN
        ENDIF
!  NPER
        CALL GETSECT(IFORMAT(5)+3,1,ITEMP,49,IERR)
        NPER=VAX_TO_LOCAL_INT(ITEMP(1))
!  NSP1,NTC1
        CALL GETSECT(IFORMAT(5)+260,2,ITEMP,49,IERR)
	CALL VAX_TO_LOCAL_INTS(ITEMP, 2)
        NSP1=ITEMP(1)
        NTC1=ITEMP(2)
!--  USER section
        IF (VER1 .EQ. 1)  THEN
		ULEN=0
		IVER(6)=0
        ELSE
              CALL GETSECT(IFORMAT(6),1,IVER(6),49,IERR)
		CALL VAX_TO_LOCAL_INTS(IVER(6), 1)
!		 changed below to calculate ULEN by measuring the section size
!		 not by reading the ULEN value which is historically set to 1 by the ICP
!             CALL GETSECT(IFORMAT(6)+1,1,ITEMP,49,IERR)
!             ULEN=VAX_TO_LOCAL_INT(ITEMP(1))
		ULEN = IFORMAT(7) - IFORMAT(6) -2
        ENDIF
!--  DATA and NOTES section
        IF (VER1 .EQ. 1) THEN
                CALL GETSECT(IFORMAT(6),1,IVER(7),49,IERR)
		IF (IFORMAT(7) .NE. 0) THEN
			CALL GETSECT(IFORMAT(7),1,IVER(8),49,IERR)
			CALL VAX_TO_LOCAL_INTS(IVER(8), 1)
		ELSE
			IVER(8) = 0
		ENDIF
        ELSE
                CALL GETSECT(IFORMAT(7),1,IVER(7),49,IERR)
                IF (IFORMAT(8) .GT. 0) THEN
			CALL GETSECT(IFORMAT(8),1,IVER(8),49,IERR)
			CALL VAX_TO_LOCAL_INTS(IVER(8), 1)
		ELSE
			IVER(8) = 0
		ENDIF
        ENDIF
	CALL VAX_TO_LOCAL_INTS(IVER(7), 1)

! DATA section header
        do i=1,32
                data_header(i)=0
        enddo   
        if ( (IVER(7) .GE. 2) .AND. (.NOT. DAE_ACCESS(RUNID)) ) then
                CALL GETSECT(IFORMAT(7)+1,32,data_header,49,IERR)
		CALL VAX_TO_LOCAL_INTS(data_header, 32)
        endif
!  finally store the file name and set flags to true
        FILNAM=RUNID
        FOUND=.TRUE.
!       WRITE(6, *)' FILE OPENED'
        RETURN
        END SUBROUTINE
!
      SUBROUTINE READ_DATA(RUNID,ERRCODE,ISPEC,DELT_WORK,SPEC_WORK, &
                          TTHE_WORK, &
                          L2_WORK,NDETMAX,TCB,IDAT,NTCMAX,L1,L2, &
                          TTHE,DELT,PHI,RUN_TITLE,DURATION, &
                          COMBINED_TIME,USER_NAME,INST_NAME,RUN_NO, &
     					  USER_TABLES,NUSE,QUICK)
!
! *** THIS SHOULD ONLY BE CALLED AFTER A CALL TO OPEN_DATA_FILE
!
! *** if (QUICK .EQ. 1)  only get spectrun data and not titles etc
!
! *** INPUT PARAMETERS (UNCHANGED BY THIS ROUTINE) ***
!
! RUNID
! ISPEC - the identifier of the spectrum to retrieve 
! NDETMAX - the size of the work arrays DELT_WORK, SPEC_WORK, 
!                                     L2_WORK and TTHE_WORK
! NTCMAX - maximum size of arrays IDAT and TCB
!
! *** RETURNED VALUES ***
!
! L1 - Primary flight path
! L2 - Secondary flight path (averaged over detectors)
! TTHE - two theta (averaged over detectors)
! DELT -
! IDAT - returned spectra file (of length NTC1+1)
! TCB - array of NTC1+1 time channel boundaries
! USER_TABLES - contains the UTn tables (NUSE of them) averaged over detectors
! ERRCODE - returned error code:   0 = All OK
!                                  1 = file not open
!                                  2 = NTCMAX, NDETMAX or NUSE inconsistent
!                                  3 = ISPEC out of range
!                                  4 = other read error
!				   5 = NUSE out of range
!
!
! *** ADDITIONAL PARAMETERS ***
!
! REQUIRES WORK ARRAYS SPEC_WORK(NDETMAX), DELT_WORK(NDETMAX), 
!  TTHE_WORK(NDETMAX) AND L2_WORK(NDETMAX)
!
      IMPLICIT NONE
      CHARACTER*(*) RUNID
      INTEGER*4 QUICK,NDETMAX,NTCMAX,IDAT(NTCMAX),ERRCODE,ISPEC, &
             NUSE,IERR
      INTEGER*4 I,J,SPEC_WORK(NDETMAX),NDETMATCH
      REAL*4 DELT_WORK(NDETMAX),L2_WORK(NDETMAX),TTHE_WORK(NDETMAX), &
            USER_TABLES(NUSE)
!
        REAL*4 TCB(NTCMAX)
        REAL*4 TTHE,L2,DELT,PHI,RNDET,DURATION
        REAL*4 L1,RVPBWK(64)
        INTEGER*4 IVPBWK(64),RPB(32),LENGTH_OUT
	INTEGER TRUELEN
	CHARACTER*8 ERROR1, ERROR2
	CHARACTER*2 UTNUM
	CHARACTER*5 RUN_NO
        CHARACTER*80 HEADER
        CHARACTER*20 USER_NAME
        CHARACTER*8 RUN_IDENT,INST_NAME
        CHARACTER*24 EXPT_TITLE
        CHARACTER*12 START_DATE
        CHARACTER*8 START_TIME,RUN_DURATION
	CHARACTER*21 COMBINED_TIME
        CHARACTER*80 RUN_TITLE
!
        CHARACTER*120 FILNAM
	CHARACTER*128 MESS
        INTEGER*4 VER1,IFORMAT(10),IVER(10),NDET,NMON,NEFF, &
                 NSEP,NTRG,NSP1,NTC1,ULEN,NPER,DATA_HEADER(32)
! *** CONV_ERR controls if an error occurs duing VAXF_TO_LOCAL
	INTEGER CONV_ERR,IDUM
        LOGICAL OPENFILE
        EQUIVALENCE (IVPBWK,RVPBWK)
!
        COMMON/CRPT_SPECIALS/ FILNAM,VER1,IFORMAT,IVER, &
               NDET,NMON,NEFF,NSEP,NTRG,NSP1,NTC1,ULEN, &
               OPENFILE,NPER,DATA_HEADER
        SAVE /CRPT_SPECIALS/
!
      EXTERNAL GETDAT,GETPARR,GETPARI,GETPARC, &
               TRUELEN,FERROR_ADD
      CONV_ERR = 0
      ERRCODE = 0
      IF ((RUNID .NE. FILNAM) .OR. (FILNAM .EQ. ' ')) THEN
	 CALL FERROR_ADD('READ_DATA', &
                        'Error in file specification', ' ')
         ERRCODE=1
         RETURN
      ENDIF
	  CALL UPDATE_CRPT_SPECIALS
      IF (NTC1+1 .GT. NTCMAX) THEN
	 WRITE(ERROR1, '(I8)') NTC1
	 WRITE(ERROR2, '(I8)') NTCMAX
         MESS = 'Too many time channels: NTC1 = '//ERROR1//', NTCMAX = '//ERROR2
	 CALL FERROR_ADD('READ_DATA', MESS, ' ')
         ERRCODE=2
         RETURN
      ENDIF

      IF ((ISPEC .GT. ((NSP1+1)*NPER)-1) .OR. (ISPEC .LT. 0)) THEN
	 WRITE(MESS, 175) ISPEC, ((NSP1+1)*NPER)-1
 175	 FORMAT('Invalid spectrum number = ',I5, &
        ' (spectra must be in the range 0 - ',I5,')')
	 CALL FERROR_ADD('READ_DATA', MESS, ' ')
         ERRCODE=3
         RETURN
      ENDIF
      IF (NDET .GT. NDETMAX) THEN
	 WRITE(ERROR1, '(I8)') NDET
	 WRITE(ERROR2, '(I8)') NDETMAX
	 MESS = 'Too many detectors: '//ERROR1//' > '//ERROR2
	 CALL FERROR_ADD('READ_DATA', MESS, ' ')
         ERRCODE=2
         RETURN
      ENDIF
      IF (NUSE .NE. NEFF) THEN
	 WRITE(ERROR1, '(I8)') NUSE
	 WRITE(ERROR2, '(I8)') NEFF
         MESS = 'Invalid number of user parameters: '// &
     		ERROR1//' .NE. '//ERROR2
	 CALL FERROR_ADD('READ_DATA', MESS, ' ')
         ERRCODE=2
         RETURN
      ENDIF
      IF (QUICK .EQ. 0) THEN
        CALL GETPARC(RUNID,'HDR',HEADER,1,LENGTH_OUT,IERR)
        IF (IERR .NE. 0) GOTO 999
! Run identifier e.g. LAD12345
        RUN_IDENT=HEADER(1:8)
        RUN_NO=HEADER(4:8)
! User Name
        USER_NAME=HEADER(9:28)
! Experiment short title
        EXPT_TITLE=HEADER(29:52)
! Start date
        START_DATE=HEADER(53:64)
! Start Time
        START_TIME=HEADER(65:72)
        COMBINED_TIME = START_DATE(1:TRUELEN(START_DATE))//' '// &
                     START_TIME(1:TRUELEN(START_TIME))
!
!      CALL GETPARC(RUNID,'TITL',RUN_TITLE,1,LENGTH_OUT,IERR)
!      IF (IERR .NE. 0) GOTO 999
        RUN_TITLE=EXPT_TITLE
!
        CALL GETPARC(RUNID,'NAME',INST_NAME,1,LENGTH_OUT,IERR)
        IF (IERR .NE. 0) GOTO 999
! time channel boundaries - NOTE: These are calculate by GETPARR
! from the integer TCB1 time chanel boundaries.
        CALL GETPARR(RUNID,'TIM1',TCB,NTCMAX,IDUM,IERR)
        IF (IERR .NE. 0) GOTO 999
! do not need VAXF_TO_LOCAL as stored as integers
      ENDIF
! *** end of >>> if ( quick=0) <<<    
      CALL GETPARR(RUNID,'RVPB',RVPBWK,64,IDUM,IERR)
      IF (IERR .NE. 0) GOTO 999
! obtained via equivalence statement
      L1=RVPBWK(23)
! spectrum table
      CALL GETPARI(RUNID,'SPEC',SPEC_WORK,NDETMAX,IDUM,IERR)
      IF (IERR .NE. 0) GOTO 999
! run duration (s)
      CALL GETPARI(RUNID, 'RPB', RPB, 32, IDUM,IERR)
      IF (IERR .NE. 0) GOTO 999
      DURATION=FLOAT(RPB(13))
!
      CALL GETPARR(RUNID,'DELT',DELT_WORK,NDETMAX,IDUM,IERR)
      IF (IERR .NE. 0) GOTO 999
!      CALL VAXF_TO_LOCAL(DELT_WORK,NDET,IERR)
!      IF (IERR .NE. 0) CONV_ERR = 1
      CALL GETPARR(RUNID,'LEN2',L2_WORK,NDETMAX,IDUM,IERR)
      IF (IERR .NE. 0) GOTO 999
!      CALL VAXF_TO_LOCAL(L2_WORK,NDET,IERR)
!      IF (IERR .NE. 0) CONV_ERR = 1
      CALL GETPARR(RUNID,'TTHE',TTHE_WORK,NDETMAX,IDUM,IERR)
      IF (IERR .NE. 0) GOTO 999
!      CALL VAXF_TO_LOCAL(TTHE_WORK,NDET,IERR)
!      IF (IERR .NE. 0) CONV_ERR = 1
!
! average TTHE, L2 and DELT over detectors used for given spectrum
!
      TTHE=0.0
      L2=0.0             
      DELT=0.0
      J = 0
      DO I=1,NDET
         IF (SPEC_WORK(I) .EQ. ISPEC) THEN
            TTHE=TTHE+TTHE_WORK(I)
            DELT=DELT+DELT_WORK(I)
            L2=L2+L2_WORK(I)
	    J = J + 1
         ENDIF
      ENDDO   
      NDETMATCH=J
! *** PHI ***
      CALL GETPARR(RUNID, 'PHI', DELT_WORK, NDETMAX, IDUM, IERR)
      PHI=0.0
      IF (IERR .EQ. 0) THEN
          DO I=1,NDET
             IF (SPEC_WORK(I) .EQ. ISPEC) THEN
                PHI=PHI+DELT_WORK(I)
             ENDIF
          ENDDO
      ENDIF
!
      DO I=1,NUSE
         USER_TABLES(I)=0.0
      ENDDO
      DO I=1,NUSE
         WRITE(UTNUM, '(I2.2)') I
         CALL GETPARR(RUNID, 'UT'//UTNUM, DELT_WORK, NDETMAX,IDUM, IERR)
         IF (IERR .NE. 0) GOTO 999
!         CALL VAXF_TO_LOCAL(DELT_WORK, NDET, IERR)
!         IF (IERR .NE. 0) CONV_ERR = 1
         DO J=1,NDET
            IF (SPEC_WORK(J) .EQ. ISPEC) THEN
               USER_TABLES(I)=USER_TABLES(I)+DELT_WORK(J)
            ENDIF
         ENDDO
      ENDDO
      IF (NDETMATCH .NE. 0) THEN
         RNDET=FLOAT(NDETMATCH)
         TTHE=TTHE/RNDET
         DELT=DELT/RNDET
         L2=L2/RNDET
         PHI=PHI/RNDET
         DO I=1,NUSE
            USER_TABLES(I)=USER_TABLES(I)/RNDET
         ENDDO
      ENDIF

      CALL GETDAT(RUNID,ISPEC,1,IDAT,NTCMAX,IERR)
      IF (IERR .NE. 0) GOTO 999
!
      IF (CONV_ERR .NE. 0) THEN
          CALL FERROR_ADD('INFORMATION', &
                         'Error during convertion VAX format to IEEE', &
                         'May be unimportant - check data')
      ENDIF
      RETURN
999   ERRCODE=4
      CALL FERROR_ADD('READ_DATA', 'Some other error', ' ')
      RETURN
      END SUBROUTINE
!
	INTEGER FUNCTION TRUELEN(STRING)
!
! *** return string length when trailing blanks are discarded
!
	IMPLICIT NONE
	CHARACTER*(*) STRING
	INTEGER I
	I=LEN(STRING)
 10	IF (STRING(I:I) .EQ. ' ') THEN
	   I=I-1
	   IF (I .GT. 0) GOTO 10
	ENDIF
	TRUELEN=I
	RETURN
	END FUNCTION
!
	SUBROUTINE FORT_FILE(IUNIT, WORK)
	INTEGER*1 WORK(120)
	CHARACTER*120 FILE_NAME
	INTEGER I, STAT, IUNIT, TRUELEN
	EXTERNAL TRUELEN
	FILE_NAME = ' '
	INQUIRE(UNIT=IUNIT, NAME=FILE_NAME, IOSTAT=STAT)
	DO I=1,TRUELEN(FILE_NAME)
	    WORK(I) = ICHAR(FILE_NAME(I:I))
	ENDDO
	DO I=TRUELEN(FILE_NAME)+1,120
	    WORK(I) = 0
	ENDDO
	RETURN
	END SUBROUTINE
!
        SUBROUTINE BYTE_REL_EXPN(INDATA,NIN,NFROM,OUTDATA,NOUT,ISTATUS)
        IMPLICIT NONE
!
! Expansion of byte-relative format into 32bit format
!
! Each integer is stored relative to the previous value in byte form.The first
! is relative to zero. This allows for numbers to be within + or - 127 of
! the previous value. Where a 32bit integer cannot be expressed in this way a 
! special byte code is used (-128) and the full 32bit integer stored elsewhere.
! The final space used is (NIN-1)/4 +1 + NEXTRA longwords, where NEXTRA is the
! number of extra longwords used in giving absolute values.
!
!
! Type definitions
!   Passed parameters
        INTEGER NIN
        INTEGER*1    INDATA(NIN)     
!                               Array of NIN compressed bytes
        INTEGER NFROM           
!                               pass back data from this 32bit word onwards
!
        INTEGER NOUT            
!                               Number of 32bit words expected
        INTEGER OUTDATA(NOUT)   
!                               outgoing expanded data
        INTEGER ISTATUS         
!                               Status return
!                                      =1  no problems!
!                                      =3  NOUT .lt.NIN/5
!                                      =2  NIN .le.0
!                                      =4  NOUT .gt.NIN
!                                      =6  number of channels lt NOUT
!   Local definitions
        INTEGER I,J,ITEMP
        INTEGER*1 BTEMP(4)
        EQUIVALENCE (ITEMP,BTEMP)       
!                               For byte UNpacking
!
!
! Assume innocent until proven guilty
        ISTATUS=1
! First check no slip-ups in the input parameters
        IF (NIN.LE.0) THEN
                ISTATUS=2
                GOTO 100
        ENDIF
        IF (NOUT+NFROM-1.gt.NIN) THEN
                ISTATUS=4
                GOTO 100
        ENDIF
!
! Set initial absolute value to zero and channel counter to zero
        ITEMP=0
        J=0
!
! Loop over all expected 32bit integers
        DO I=1,NFROM+NOUT-1
            J=J+1
            IF (J.GT.NIN) THEN  
! check there are enough bytes
                ISTATUS=6
                GOTO 100
            ENDIF
!
! if number is contained in a byte
            IF (INDATA(J).NE.-128) THEN
! add in offset to base
                ITEMP=ITEMP+INDATA(J)   
            ELSE
! Else skip marker and pick up new absolute value
                IF (J+4.GT.NIN) THEN
! check there are enough bytes
                    ISTATUS=6
                    GOTO 100
                ENDIF
! unpack 4 bytes
                BTEMP(1)=INDATA(J+1)    
                BTEMP(2)=INDATA(J+2)
                BTEMP(3)=INDATA(J+3)
                BTEMP(4)=INDATA(J+4)
		CALL VAX_TO_LOCAL_INTS(ITEMP, 1)
                J=J+4
            ENDIF
! update current value
            IF (I.GE.NFROM) THEN
                OUTDATA(I-NFROM+1)=ITEMP
            ENDIF
        ENDDO
!
!
! expansion OK, but excessive number of bytes given to the routine
 100    IF (NOUT.lt.NIN/5) ISTATUS=3
!       TYPE *,'J=',J,' I=',I
        RETURN
        END SUBROUTINE
!---------------------------------------------------------------------
        SUBROUTINE GETPARR(RUNID,NAME,RVALUE,LENGTH_IN, &
     			   LENGTH_OUT,ERRCODE)
        IMPLICIT NONE
!
! *** ERROR CODES ***
!
!    0 = all OK
!    1 = file RUNID not currently open
!    2 = file number is '00000' and so no access available
!    3 = asked for non-existent parameter
!    4 = other error
!

!  Gets REAL parameter(s) from RAW data file
!
        CHARACTER*(*) RUNID
        CHARACTER*(*) NAME
        INTEGER*4 ERRCODE,LENGTH_IN,IERR
	INTEGER*4 LENGTH_OUT,LENGTH
	INTEGER*4 RVALUE(LENGTH_IN)
!
!
        INTEGER*4 IWORK(128),ISTORE(64)
        REAL*4 WORK(128)
        COMMON/crpt_WORK/IWORK
        SAVE /crpt_WORK/
        EQUIVALENCE (IWORK,WORK)
!
        EQUIVALENCE (TEMP,ITEMP)
!
        INTEGER*4 ITABLE,IPRE1,I,ITEMP,VAX_TO_LOCAL_INT
        REAL*4 TEMP,EXTRA
!
        CHARACTER*120 FILNAM
	CHARACTER*128 MESS
        INTEGER*4 VER1,IFORMAT(10),IVER(10),NDET,NMON,NEFF, &
          NSEP,NTRG,NSP1,NTC1,ULEN,NPER,DATA_HEADER(32), SENUM, K
        LOGICAL OPENFILE, CONVERT, DAE_ACCESS, CRPT_ACCESS
        COMMON/crpt_SPECIALS/ FILNAM,VER1,IFORMAT,IVER, &
               NDET,NMON,NEFF,NSEP,NTRG,NSP1,NTC1,ULEN,OPENFILE,NPER, &
               DATA_HEADER
        SAVE /crpt_SPECIALS/
	EXTERNAL VAX_TO_LOCAL_INT,VAX_TO_LOCAL_INTS,VAXF_TO_LOCAL
	CALL UPDATE_CRPT_SPECIALS
! if we have local access (i.e. to PC), no conversion needed 
! check for "local_dae_server" variable
	mess = ' '
! comment out next two calls to icp_getenv on Linux
!	call icp_getenv('local_dae_server', mess)
!	if (mess .eq. ' ') call icp_getenv('local_crpt', mess)
	if ( (mess .ne. ' ') .and. (DAE_ACCESS(FILNAM) .OR. CRPT_ACCESS(FILNAM)) ) THEN
		CONVERT=.FALSE.
	ELSE
	    CONVERT=.TRUE.
	ENDIF 
!
        ERRCODE=0
        IERR=0
	LENGTH_OUT=0
	LENGTH=LENGTH_IN
!
!
!  decide whether it's CRPT or just a f
        IF (RUNID(4:8).EQ.'00000') THEN
!               WRITE(6,6000)
! 6000          FORMAT(' Accessing of current run not available')
!               GOTO 999
	   	CALL FERROR_ADD('GETPARR', &
           	'Runid is 00000', &
           	' ')
           ERRCODE=2
           RETURN
        ENDIF
!
!  check name is valid & open file according to RUNID
        IF ((FILNAM .NE. RUNID) .OR. (FILNAM .EQ. ' ')) THEN
           MESS = 'Access requested to '//RUNID// &
     		  ' when '//FILNAM//' open'
	   CALL FERROR_ADD('GETPARR', &
          MESS, &
          ' ')
           ERRCODE=1
           RETURN
        ENDIF
!
!  read variables into RVALUE
! ----------------------------
!
!  Instrument section
        ITABLE=IFORMAT(2)+70+2*NMON
        IF (NAME.EQ.'LEN2') THEN
                CALL GETSECT(ITABLE+2*NDET,NDET,RVALUE,49,IERR)
		LENGTH=NDET
		IF (CONVERT) CALL VAXF_TO_LOCAL(RVALUE, LENGTH, IERR)
        ELSEIF (NAME.EQ.'OMEG') THEN
          IF (IVER(2).EQ.1) THEN
                CALL GETSECT(ITABLE+5*NDET,NDET,RVALUE,49,IERR)
		LENGTH=NDET
		IF (CONVERT) CALL VAXF_TO_LOCAL(RVALUE, LENGTH, IERR)
	  ELSE
          	MESS = 'Cannot access '//NAME
	   	CALL FERROR_ADD('GETPARR', &
           	MESS, &
           	' ')
	        ERRCODE=4
          ENDIF
        ELSEIF (NAME.EQ.'DELT') THEN
          IF (IVER(2).NE.1) THEN
                CALL GETSECT(ITABLE+NDET,NDET,RVALUE,49,IERR)
		LENGTH=NDET
		IF (CONVERT) CALL VAXF_TO_LOCAL(RVALUE, LENGTH, IERR)
	  ELSE
          	MESS = 'Cannot access '//NAME
	   	CALL FERROR_ADD('GETPARR', &
          	MESS, &
          	' ')
	        ERRCODE=4
          ENDIF
        ELSEIF (NAME.EQ.'TTHE') THEN
                CALL GETSECT(ITABLE+4*NDET,NDET,RVALUE,49,IERR)
		LENGTH=NDET
		IF (CONVERT) CALL VAXF_TO_LOCAL(RVALUE, LENGTH, IERR)
        ELSEIF (NAME.EQ.'PHI') THEN
          IF (IVER(2).EQ.1) THEN
                CALL GETSECT(ITABLE+5*NDET,NDET,RVALUE,49,IERR)
		LENGTH=NDET
		IF (CONVERT) CALL VAXF_TO_LOCAL(RVALUE, LENGTH, IERR)
	  ELSE
          	MESS = 'GETPARR: No item '//NAME//' in RAW file'
! *** We do not flag an error here as then 'GET:SPECTRUM' would return an error vie READ_DATA
! *** comment out message until i find a RAW file that does have it set!
!	   	CALL FERROR_ADD('INFORMATION', MESS, ' ')
	        ERRCODE=4
          ENDIF
!==cmm   ELSEIF (NAME.EQ.'EF01') THEN
!                CALL GETSECT(ITABLE+6*NDET,NDET,RVALUE,49,IERR)
!		LENGTH=NDET
!		CALL VAXF_TO_LOCAL(RVALUE, LENGTH, IERR)
!        ELSEIF (NAME.EQ.'EF02') THEN
!                CALL GETSECT(ITABLE+7*NDET,NDET,RVALUE,49,IERR)
!		LENGTH=NDET
!		CALL VAXF_TO_LOCAL(RVALUE, LENGTH, IERR)
!        ELSEIF (NAME.EQ.'EF03') THEN
!                CALL GETSECT(ITABLE+8*NDET,NDET,RVALUE,49,IERR)
!		LENGTH=NDET
!		CALL VAXF_TO_LOCAL(RVALUE, LENGTH, IERR)
!        ELSEIF (NAME.EQ.'EF04') THEN
!                CALL GETSECT(ITABLE+9*NDET,NDET,RVALUE,49,IERR)
!		LENGTH=NDET
!		CALL VAXF_TO_LOCAL(RVALUE, LENGTH, IERR)
!        ELSEIF (NAME.EQ.'EF05') THEN
!                CALL GETSECT(ITABLE+10*NDET,NDET,RVALUE,49,IERR)
!		LENGTH=NDET
!		CALL VAXF_TO_LOCAL(RVALUE, LENGTH, IERR)
!        ELSEIF (NAME.EQ.'EF06') THEN
!                CALL GETSECT(ITABLE+11*NDET,NDET,RVALUE,49,IERR)
!		LENGTH=NDET
!		CALL VAXF_TO_LOCAL(RVALUE, LENGTH, IERR)
!
! UT should really start at (5+I)*NDET, but the ICP loads them over PHI
! 
	  ELSEIF (NAME(1:2) .EQ. 'UT') THEN
		READ(NAME(3:4),'(I2)') I
                CALL GETSECT(ITABLE+(4+I)*NDET,NDET,RVALUE,49,IERR)
		LENGTH=NDET
		IF (CONVERT) CALL VAXF_TO_LOCAL(RVALUE, LENGTH, IERR)
        ELSEIF (NAME.EQ.'RSPB ') THEN
          IF (IVER(3).EQ.1) THEN
                CALL GETSECT(IFORMAT(3)+1,32,RVALUE,49,IERR)
		LENGTH = 32
		IF (CONVERT) CALL VAXF_TO_LOCAL(RVALUE, LENGTH, IERR)
          ELSE
                CALL GETSECT(IFORMAT(3)+1,64,RVALUE,49,IERR)
		LENGTH = 64
		IF (CONVERT) CALL VAXF_TO_LOCAL(RVALUE, LENGTH, IERR)
          ENDIF
!  SE block section
        ELSEIF (NAME(1:3) .EQ.'RSE') THEN
		READ(NAME(4:5), '(I2.2)') SENUM
		IF ((IVER(3) .EQ. 1) .OR. (NSEP .LT. SENUM)) THEN
     		    MESS = 'Invalid SE block '//NAME
	   	    CALL FERROR_ADD('GETPARR', &
      		    MESS, &
          	    ' ')
                    ERRCODE=4
		ELSE
!		    WRITE(6,'(1X,A,I2.2)') 'Reading SE',SENUM
		    K = IFORMAT(3)+34+32*SENUM
                    CALL GETSECT(K,32,RVALUE,49,IERR)
		    LENGTH = 32
		    IF (CONVERT) CALL VAXF_TO_LOCAL(RVALUE, LENGTH, IERR)
!		    DO I=1,32
!			IJUNK=IVALUE(I)
!		        WRITE(6,'(4(1X,Z2,A,A1))') (BJUNK(J), '=', CHAR(BJUNK(J)), J=1,4)
!		    ENDDO
		ENDIF

!  Time channel boundaries section
!     time channel area definition
        ELSEIF (NAME.EQ.'TCP1') THEN
                CALL GETSECT(IFORMAT(5)+267,20,RVALUE,49,IERR)
		LENGTH=20
		IF (CONVERT) CALL VAXF_TO_LOCAL(RVALUE, LENGTH, IERR)
        ELSEIF (NAME.EQ.'TCB1') THEN

!		Encourage anyone relinking here to use TIM1 instead of
!		TCB1 which is an integer paramter. Continue to return
!		the value requested by duplicating the TIM1 code here
	   	CALL FERROR_ADD('GETPARR', &
        'Please edit your code to specify ''TIM1'' instead of ''TCB1''', &
        ' ')

		IF (VER1 .NE. 1) THEN
                    CALL GETSECT(IFORMAT(4)+1,64,ISTORE,49,IERR)
		    CALL VAX_TO_LOCAL_INTS(ISTORE, 64)
		    EXTRA=FLOAT(ISTORE(24)*4)
		ELSE
		    EXTRA=0.0
		ENDIF
!  if tcb's requested in real form then return as microsecs
                CALL GETSECT(IFORMAT(5)+287,1,ISTORE,49,IERR)
                IPRE1=VAX_TO_LOCAL_INT(ISTORE(1))
                CALL GETSECT(IFORMAT(5)+288,NTC1+1,RVALUE,49,IERR)
		CALL VAX_TO_LOCAL_INTS(RVALUE, NTC1+1)
!  conversion loop - from clock pulses to microsecs
                DO 101 I=1,NTC1+1
                   TEMP=FLOAT(RVALUE(I))/32.0*FLOAT(IPRE1) + EXTRA
                   RVALUE(I)=ITEMP
 101            CONTINUE
		LENGTH=NTC1+1
        ELSEIF (NAME.EQ.'TIM1') THEN
		IF (VER1 .NE. 1) THEN
                    CALL GETSECT(IFORMAT(4)+1,64,ISTORE,49,IERR)
		    CALL VAX_TO_LOCAL_INTS(ISTORE, 64)
		    EXTRA=FLOAT(ISTORE(24)*4)
		ELSE
		    EXTRA=0.0
		ENDIF
!  if tcb's requested in real form then return as microsecs
                CALL GETSECT(IFORMAT(5)+287,1,ISTORE,49,IERR)
                IPRE1=VAX_TO_LOCAL_INT(ISTORE(1))
                CALL GETSECT(IFORMAT(5)+288,NTC1+1,RVALUE,49,IERR)
		CALL VAX_TO_LOCAL_INTS(RVALUE, NTC1+1)
!  conversion loop - from clock pulses to microsecs
                DO 100 I=1,NTC1+1
                   TEMP=FLOAT(RVALUE(I))/32.0*FLOAT(IPRE1) + EXTRA
                   RVALUE(I)=ITEMP
 100            CONTINUE
		LENGTH=NTC1+1
	ELSEIF ( (NAME .EQ. 'DAT1') .OR. (NAME .EQ. 'UDAT') ) THEN
		IF ((VER1 .NE. 1) .AND. (ULEN .NE. 0)) THEN
		    CALL GETSECT(IFORMAT(6)+2, ULEN, RVALUE, 49, IERR)
		    LENGTH = ULEN
		    IF (CONVERT) CALL VAXF_TO_LOCAL(RVALUE, LENGTH, IERR)
		ELSE
		    RVALUE(1) = 0.0
		    LENGTH = 1
	    	    CALL FERROR_ADD('GETPARR', &
           		    'No user data in this file', &
     			    ' ')
		ENDIF
        ELSEIF (NAME .EQ. 'RRPB') THEN
                CALL GETSECT(IFORMAT(1)+62,32,RVALUE,49,IERR)
		LENGTH = 32
		IF (CONVERT) CALL VAXF_TO_LOCAL(RVALUE, LENGTH,IERR)
        ELSEIF (NAME.EQ.'RVPB') THEN
                CALL GETSECT(IFORMAT(2)+3,64,RVALUE,49,IERR)
		LENGTH = 64
		IF (CONVERT) CALL VAXF_TO_LOCAL(RVALUE, LENGTH,IERR)
        ELSEIF (NAME .EQ. 'RSTAT') THEN
		LENGTH = MIN(65,IFORMAT(1)-32)
	   	IF (LENGTH .GT. 0) THEN
                    CALL GETSECT(32,LENGTH,RVALUE,49,IERR)
		    IF (CONVERT) CALL VAXF_TO_LOCAL(RVALUE, LENGTH, IERR)
		ELSE
		    CALL FERROR_ADD('GETPARR','STATUS section is not available',' ')
	 	ENDIF
        ELSE
!  non existant requests
!     	        MESS = 'No such REAL parameter as '//NAME
!	        CALL FERROR_ADD('GETPARR', 
!     1      		    MESS,
!     2			    ' ')
                ERRCODE=3
		LENGTH = 0
                RETURN
        ENDIF
!
	IF ((IERR .NE. 0) .AND. (ERRCODE .EQ. 0)) THEN
       	    MESS = 'Error in reading data from file '//RUNID
	    CALL FERROR_ADD('GETPARR', &
      			    MESS, &
     			    ' ')
	    ERRCODE = 4
	ENDIF
!
	LENGTH_OUT=LENGTH
 999    RETURN
        END SUBROUTINE
!
	LOGICAL FUNCTION DAE_ACCESS(NAME)
	IMPLICIT NONE
	CHARACTER*(*) NAME
	LOGICAL DCE_NAME
	INTEGER I, TRUELEN
	EXTERNAL TRUELEN
	I = TRUELEN(NAME)
	DCE_NAME = (NAME(1:4) .EQ. '/.:/') .OR. (NAME(1:5) .EQ. '/.../')
	IF ( DCE_NAME .AND. (NAME(I-3:I) .EQ. '_dae') ) THEN
	    DAE_ACCESS = .TRUE.
	ELSE
	    DAE_ACCESS = .FALSE.
	ENDIF
	RETURN
	END FUNCTION
!	
	LOGICAL FUNCTION CRPT_ACCESS(NAME)
	IMPLICIT NONE
	CHARACTER*(*) NAME
	LOGICAL DCE_NAME
	INTEGER I, TRUELEN
	EXTERNAL TRUELEN
	I = TRUELEN(NAME)
	DCE_NAME = (NAME(1:4) .EQ. '/.:/') .OR. (NAME(1:5) .EQ. '/.../')
	IF ( DCE_NAME .AND. (NAME(I-4:I) .EQ. '_crpt') ) THEN
	    CRPT_ACCESS = .TRUE.
	ELSE IF ( (INDEX(NAME, 'CURRENT.RUN') .NE. 0) .OR. & 
              (INDEX(NAME, 'current.run') .NE. 0) ) THEN
	    CRPT_ACCESS = .TRUE.
	ELSE
	    CRPT_ACCESS = .FALSE.
	ENDIF
	RETURN
	END FUNCTION
	
	
	
! Old FORTRAN version of GETSECT
!
! $Id: IXMraw_file.f90 1421 2008-07-11 16:08:00Z Dickon Champion $
!
        SUBROUTINE GETSECT(ISTART,ILONG,IVALUE,IUNIT,IERR)
        IMPLICIT NONE
!  gets a section of the data file and places it in IVALUE
!   ISTART is the starting position in the file
!   ILONG is the length of the required section
!
!  if ( IERR .NE. 0) on return, error in reading from that record
!
        INTEGER*4 ILONG,IVALUE(ILONG),IWORK(128),IUNIT,IERR
        INTEGER*4 IBLOCK,ISTART,IFIRST,ILAST,I,J,ILEFT
!        COMMON/crpt_WORK/IWORK
!        SAVE /crpt_WORK/
!
! calculate which block to start from, and first and last elements to use
	IERR=0
        IBLOCK=(ISTART-1)/128 + 1
        IFIRST=ISTART - (IBLOCK-1)*128
        ILAST=IFIRST+ILONG-1
        IF (ILAST.GT.128) ILAST=128
! also keep a count of how many elements left to get
        ILEFT=ILONG
!
!
        J=1
! 5     TYPE *,' IBLOCK',IBLOCK
 5      READ(IUNIT, REC=IBLOCK, IOSTAT=IERR) IWORK
	IF (IERR .NE. 0) THEN
	    IF (IBLOCK .NE. 0) THEN
	        IERR = IBLOCK
	    ELSE
		IERR = -1
	    ENDIF
	    RETURN
	ENDIF
!
     DO I=IFIRST,ILAST
		IF (J .GT. ILONG) THEN
	        IERR = -1
	        RETURN
	    ENDIF
        IVALUE(J)=IWORK(I)
        J=J+1
      END DO
!
        ILEFT=ILEFT-(ILAST-IFIRST+1)
        IF (ILEFT.LE.0) GOTO 999
        IBLOCK=IBLOCK+1
        IFIRST=1
        ILAST=ILEFT+IFIRST-1
        IF (ILAST.GT.128) ILAST=128
        GOTO 5
!
!
 999    RETURN
        END SUBROUTINE
!
! *** dummy routine to replace one in getsect.cc
!
	INTEGER FUNCTION FASTGET_INIT(A,IUNIT)
        IMPLICIT NONE
	INTEGER A,IUNIT,reclen
        CHARACTER*120 FILNAM
        INTEGER*4 VER1,IFORMAT(10),IVER(10),NDET,NMON,NEFF, &
        NSEP,NTRG,NSP1,NTC1,ULEN,NPER,DATA_HEADER(32),ISTATUS,TRUELEN,I
        LOGICAL OPENFILE
        COMMON/crpt_SPECIALS/ FILNAM,VER1,IFORMAT,IVER, &
                NDET,NMON,NEFF,NSEP,NTRG,NSP1,NTC1,ULEN,OPENFILE,NPER, &
                DATA_HEADER
        SAVE /crpt_SPECIALS/
	INQUIRE(UNIT=IUNIT,OPENED=OPENFILE)
	IF (OPENFILE) THEN
	    CLOSE(IUNIT)
	    OPENFILE=.FALSE.
        ENDIF
        I=TRUELEN(FILNAM)
! *** For Linux (at least) RECL is measured in bytes rather than words
! *** and so you must set RECL=512
! *** OSF1/VMS use words so set RECL=128
#if BYTERECL
        reclen=512
#else
        reclen=128
#endif /* BYTERECL */
        OPEN(UNIT=IUNIT,FILE=FILNAM(1:I),RECL=reclen,STATUS='OLD', &
             ACCESS='DIRECT',IOSTAT=ISTATUS)
! READONLY, SHAREABLE on VMS - comment out if your OS doesn't support
        IF (ISTATUS .NE. 0) THEN
               WRITE(6,6010)ISTATUS
 6010          FORMAT(' File could not be opened. Status=',I12)
                FASTGET_INIT=1
                RETURN
        ENDIF
	FASTGET_INIT=0
	RETURN
	END FUNCTION
!

!
!---------------------------------------------------------------------
! DEC/CMS REPLACEMENT HISTORY, Element BYTE_REL_EXPN.FOR
! *1    11-AUG-1989 09:28:08 KJK "New routine coming in at 2.4.5"
! DEC/CMS REPLACEMENT HISTORY, Element BYTE_REL_EXPN.FOR

      SUBROUTINE FASTREAD_DATA(RUNID,ERRCODE,ISPEC,DELT_WORK,SPEC_WORK, &
                          TTHE_WORK,L2_WORK,PHI_WORK,USER_WORK, &
                          NDETMAX,TCB,IDAT,NTCMAX,L1,L2, &
                          TTHE,DELT,PHI,RUN_TITLE,DURATION, &
                          COMBINED_TIME,USER_NAME,INST_NAME,RUN_NO, &
     			   USER_TABLES,NUSE,QUICK)
!
! *** THIS SHOULD ONLY BE CALLED AFTER A CALL TO OPEN_DATA_FILE
!
! *** if (QUICK .EQ. 1)  only get spectrun data and not titles etc
! *** also it assumes that L2_WORK etc are unchanged from previous call
!
! *** INPUT PARAMETERS (UNCHANGED BY THIS ROUTINE) ***
!
! RUNID
! ISPEC - the identifier of the spectrum to retrieve 
! NDETMAX - the size of the work arrays DELT_WORK, SPEC_WORK, 
!                                     L2_WORK and TTHE_WORK
! NTCMAX - maximum size of arrays IDAT and TCB
!
! *** RETURNED VALUES ***
!
! L1 - Primary flight path
! L2 - Secondary flight path (averaged over detectors)
! TTHE - two theta (averaged over detectors)
! DELT -
! IDAT - returned spectra file (of length NTC1+1)
! TCB - array of NTC1+1 time channel boundaries
! USER_TABLES - contains the UTn tables (NUSE of them) averaged over detectors
! ERRCODE - returned error code:   0 = All OK
!                                  1 = file not open
!                                  2 = NTCMAX, NDETMAX or NUSE inconsistent
!                                  3 = ISPEC out of range
!                                  4 = other read error
!				   5 = NUSE out of range
!
!
! *** ADDITIONAL PARAMETERS ***
!
! REQUIRES WORK ARRAYS SPEC_WORK(NDETMAX), DELT_WORK(NDETMAX), 
!  TTHE_WORK(NDETMAX) AND L2_WORK(NDETMAX)
!
      IMPLICIT NONE
      CHARACTER*(*) RUNID
      INTEGER*4 QUICK,NDETMAX,NTCMAX,IDAT(NTCMAX),ERRCODE,ISPEC, &
            NUSE,IERR
      INTEGER*4 I,J,SPEC_WORK(NDETMAX),NDETMATCH
      REAL*4 DELT_WORK(NDETMAX),L2_WORK(NDETMAX),TTHE_WORK(NDETMAX), &
            USER_TABLES(NUSE),USER_WORK(NDETMAX,NUSE), &
            PHI_WORK(NDETMAX)
!
        REAL*4 TCB(NTCMAX)
        REAL*4 TTHE,L2,DELT,PHI,RNDET,DURATION
        REAL*4 L1,RVPBWK(64)
        INTEGER*4 IVPBWK(64),RPB(32),LENGTH_OUT
	INTEGER TRUELEN
	CHARACTER*8 ERROR1, ERROR2
	CHARACTER*4 UTNUM
	CHARACTER*5 RUN_NO
        CHARACTER*80 HEADER
        CHARACTER*20 USER_NAME
        CHARACTER*8 RUN_IDENT,INST_NAME
        CHARACTER*24 EXPT_TITLE
        CHARACTER*12 START_DATE
        CHARACTER*8 START_TIME,RUN_DURATION
	CHARACTER*21 COMBINED_TIME
        CHARACTER*80 RUN_TITLE
!
        CHARACTER*120 FILNAM
	CHARACTER*128 MESS
        INTEGER*4 VER1,IFORMAT(10),IVER(10),NDET,NMON,NEFF, &
                 NSEP,NTRG,NSP1,NTC1,ULEN,NPER,DATA_HEADER(32)
! *** CONV_ERR controls if an error occurs duing VAXF_TO_LOCAL
	INTEGER CONV_ERR,IDUM
        LOGICAL OPENFILE
        EQUIVALENCE (IVPBWK,RVPBWK)
!
        COMMON/CRPT_SPECIALS/ FILNAM,VER1,IFORMAT,IVER, &
               NDET,NMON,NEFF,NSEP,NTRG,NSP1,NTC1,ULEN, &
               OPENFILE,NPER,DATA_HEADER
        SAVE /CRPT_SPECIALS/
!
      EXTERNAL GETDAT,GETPARR,GETPARI,GETPARC,TRUELEN,FERROR_ADD
      CONV_ERR = 0
      ERRCODE = 0
      IF ((RUNID .NE. FILNAM) .OR. (FILNAM .EQ. ' ')) THEN
	 CALL FERROR_ADD('FASTREAD_DATA', &
                        'Error in file specification', ' ')
         ERRCODE=1
         RETURN
      ENDIF
      IF (NTC1+1 .GT. NTCMAX) THEN
	 WRITE(ERROR1, '(I8)') NTC1
	 WRITE(ERROR2, '(I8)') NTCMAX
         MESS = 'Too many time channels: NTC1 = '//ERROR1// &
     		', NTCMAX = '//ERROR2
	 CALL FERROR_ADD('FASTREAD_DATA', MESS, ' ')
         ERRCODE=2
         RETURN
      ENDIF

      IF ((ISPEC .GT. ((NSP1+1)*NPER)-1) .OR. (ISPEC .LT. 0)) THEN
	 WRITE(MESS, 175) ISPEC, ((NSP1+1)*NPER)-1
 175	 FORMAT('Invalid spectrum number = ',I5, &
        ' (spectra must be in the range 0 - ',I5,')')
	 CALL FERROR_ADD('FASTREAD_DATA', MESS, ' ')
         ERRCODE=3
         RETURN
      ENDIF
      IF (NDET .GT. NDETMAX) THEN
	 WRITE(ERROR1, '(I8)') NDET
	 WRITE(ERROR2, '(I8)') NDETMAX
	 MESS = 'Too many detectors: '//ERROR1//' > '//ERROR2
	 CALL FERROR_ADD('FASTREAD_DATA', MESS, ' ')
         ERRCODE=2
         RETURN
      ENDIF
      IF (NUSE .NE. NEFF) THEN
	 WRITE(ERROR1, '(I8)') NUSE
	 WRITE(ERROR2, '(I8)') NEFF
         MESS = 'Invalid number of user parameters: '// &
     		ERROR1//' .NE. '//ERROR2
	 CALL FERROR_ADD('FASTREAD_DATA', MESS, ' ')
         ERRCODE=2
         RETURN
      ENDIF
      IF (QUICK .EQ. 0) THEN
          CALL GETPARC(RUNID,'HDR',HEADER,1,LENGTH_OUT,IERR)
          IF (IERR .NE. 0) GOTO 999
! Run identifier e.g. LAD12345
          RUN_IDENT=HEADER(1:8)
          RUN_NO=HEADER(4:8)
! User Name
          USER_NAME=HEADER(9:28)
! Experiment short title
          EXPT_TITLE=HEADER(29:52)
! Start date
          START_DATE=HEADER(53:64)
! Start Time
          START_TIME=HEADER(65:72)
          COMBINED_TIME = START_DATE(1:TRUELEN(START_DATE))//' '// &
                         START_TIME(1:TRUELEN(START_TIME))
!
!          CALL GETPARC(RUNID,'TITL',RUN_TITLE,1,LENGTH_OUT,IERR)
!          IF (IERR .NE. 0) GOTO 999
          RUN_TITLE=EXPT_TITLE
!
          CALL GETPARC(RUNID,'NAME',INST_NAME,1,LENGTH_OUT,IERR)
          IF (IERR .NE. 0) GOTO 999
! time channel boundaries - NOTE: These are calculate by GETPARR
! from the integer TCB1 time chanel boundaries.
          CALL GETPARR(RUNID,'TIM1',TCB,NTCMAX,IDUM,IERR)
          IF (IERR .NE. 0) GOTO 999
          CALL GETPARR(RUNID,'DELT',DELT_WORK,NDETMAX,IDUM,IERR)
          IF (IERR .NE. 0) GOTO 999
          CALL GETPARR(RUNID,'LEN2',L2_WORK,NDETMAX,IDUM,IERR)
          IF (IERR .NE. 0) GOTO 999
          CALL GETPARR(RUNID,'TTHE',TTHE_WORK,NDETMAX,IDUM,IERR)
          IF (IERR .NE. 0) GOTO 999
          CALL GETPARR(RUNID, 'PHI', PHI_WORK, NDET, IDUM, IERR)
          IF (IERR .NE. 0) THEN
	      DO I=1,NDETMAX
	          PHI_WORK(I) = 0.0
              ENDDO
          ENDIF
! spectrum table
          CALL GETPARI(RUNID,'SPEC',SPEC_WORK,NDETMAX,IDUM,IERR)
          IF (IERR .NE. 0) GOTO 999
          DO I=1,NUSE
             WRITE(UTNUM, '(A,I2.2)', ERR=999) 'UT',I
             CALL GETPARR(RUNID, UTNUM, USER_WORK(1,I), NDETMAX,IDUM,IERR)
             IF (IERR .NE. 0) GOTO 999
          ENDDO
      ENDIF
! *** end of >>> if ( quick=0) <<<    
      CALL GETPARR(RUNID,'RVPB',RVPBWK,64,IDUM,IERR)
      IF (IERR .NE. 0) GOTO 999
! obtained via equivalence statement
      L1=RVPBWK(23)
! run duration (s)
      CALL GETPARI(RUNID, 'RPB', RPB, 32, IDUM,IERR)
      IF (IERR .NE. 0) GOTO 999
      DURATION=FLOAT(RPB(13))
!
! average TTHE, L2 and DELT over detectors used for given spectrum
!
      TTHE=0.0
      L2=0.0             
      DELT=0.0
      PHI=0.0
      DO I=1,NUSE
         USER_TABLES(I)=0.0
      ENDDO
      NDETMATCH=0
      DO I=1,NDET
         IF (SPEC_WORK(I) .EQ. ISPEC) THEN
            TTHE=TTHE+TTHE_WORK(I)
            DELT=DELT+DELT_WORK(I)
            L2=L2+L2_WORK(I)
            PHI=PHI+PHI_WORK(I)
            DO J=1,NUSE
               USER_TABLES(J)=USER_TABLES(J)+USER_WORK(I,J)
	    ENDDO
	    NDETMATCH = NDETMATCH + 1
         ENDIF
      ENDDO
!
      IF (NDETMATCH .NE. 0) THEN
         RNDET=FLOAT(NDETMATCH)
         TTHE=TTHE/RNDET
         DELT=DELT/RNDET
         L2=L2/RNDET
         PHI=PHI/RNDET
         DO I=1,NUSE
            USER_TABLES(I)=USER_TABLES(I)/RNDET
         ENDDO
      ENDIF

      CALL GETDAT(RUNID,ISPEC,1,IDAT,NTCMAX,IERR)
      IF (IERR .NE. 0) GOTO 999
!
      IF (CONV_ERR .NE. 0) THEN
          CALL FERROR_ADD('INFORMATION', &
                          'Error during convertion VAX format to IEEE', &
                          'May be unimportant - check data')
      ENDIF
      RETURN
999   ERRCODE=4
      CALL FERROR_ADD('FASTREAD_DATA', 'Some other error', ' ')
      RETURN
      END SUBROUTINE

      subroutine FERROR_ADD(source, message, solution)
      use IXMstatus
	  use IXMraw_file
	  implicit none
	  character(len=*) :: source, message, solution
	  if (associated(global_raw_status)) then
	      call IXFadd_status(global_raw_status, IXCfacility_file, IXCseverity_fatal, &
		                     IXCerr_outofmem, source//message//solution)
	  endif
	end subroutine

	  
	  

