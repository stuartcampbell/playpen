module IXMslice
  use IXMtype_definitions
  use IXMstatus
  use IXMmemory
	character(len=long_len), allocatable :: footer(:)
  	real(dp), allocatable :: pix(:,:)
!	save :: pix
contains

!-----------------------------------------------------------------------
! Read number of points in the slice
subroutine load_slice_header(filename, header,status)
	implicit none
	integer(i4b) :: i
	real(dp) :: header(6)
	character(len=*) :: filename
	type(IXtstatus)::status

	open(unit=1,file=filename,action='READ',err=999)
	read(1,*,err=999) (header(i),i=1,6)
	close(unit=1)  
	return  

999 header(1)=0	! convention for error reading file
call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'error reading slice file header (load_slice_header)')   
	close(unit=1)
	return 
end subroutine load_slice_header

!-----------------------------------------------------------------------
! Read slice data 
subroutine load_slice(filename,np,x,y,c,e,npix,npixtot,nfooter)
    implicit none      
    integer(i4b) :: np, npixtot, nfooter
	real(dp) :: x(np), y(np), c(np), e(np), npix(np)	! if pass from F77 code, need to declare sizes, not x(:) etc.
	character(len=*) :: filename

	integer(i4b) :: iunit, npixbuff, nfooterbuff, ip, npix_int, ipix, i
	real(dp) :: dummy(6)
	real(dp), allocatable :: old_pix(:,:)	! temporary storage if reallocate pix
	character(len=long_len), allocatable :: old_footer(:)	! temporary footer storage if reallocate footer

	iunit=1			! unit from which to read data
	npixbuff=10000	! initial size of pixel buff
	nfooterbuff=100	! initial size of footer buffer (>0)

	open(unit=iunit,file=filename,READONLY,err=999)
	read(iunit,*,err=999) (dummy(i),i=1,6)

	if (allocated(pix)) deallocate(pix)			! will be saved between calls from Matlab
	allocate (pix(7,npixbuff))	! allocate storage

	if (allocated(footer)) deallocate(footer)
	allocate (footer(nfooterbuff))


	! Read in the pixel information
	npixtot = 0
	do ip = 1, np
		read (iunit,*,err=999,end=999) x(ip), y(ip), c(ip), e(ip), npix(ip)
		npix_int = nint(npix(ip))
		! Reallocate pixel buffer if not large enough to hold the extra pixels from this point
		if (npixtot+npix_int > npixbuff) then
			if (allocated(old_pix)) deallocate(old_pix)
			allocate(old_pix(7,npixtot))
			old_pix=pix(:,1:npixtot)
			deallocate(pix)
			npixbuff=2_i4b*max(npixbuff,npix_int)	! guaranteed to be large enough to hold extra pixels
			allocate(pix(7,npixbuff))
			pix(:,1:npixtot)=old_pix
			deallocate(old_pix)
		endif
		! Read pixel information
		if (npix_int>0) then
			do ipix =npixtot+1,npixtot+npix_int
				read (iunit,*,err=999,end=999) (pix(i,ipix),i=1,7)
			end do
			npixtot = npixtot + npix_int 
		endif
	end do

	! Read in the footer (if any)
	nfooter = 0
100 if (nfooter == nfooterbuff) then
		if (allocated(old_footer)) deallocate(old_footer)
		allocate(old_footer(nfooterbuff))
		old_footer=footer
		deallocate(footer)
		nfooterbuff=2_i4b*nfooterbuff
		allocate(footer(nfooterbuff))
		footer(1:nfooter)=old_footer
		deallocate(old_footer)
	endif
	nfooter = nfooter + 1
	read (iunit,'(a)',err=999,end=900) footer(nfooter)
	goto 100

	! Close down in orderly fashion
900	close(unit=1) 
    nfooter = nfooter - 1
	if (allocated(old_pix)) deallocate(old_pix)
	if (allocated(old_footer)) deallocate(old_footer)
	return

999 npixtot=-1    ! convention for error reading file
	nfooter=-1
    close(unit=1)
	if (allocated(pix)) deallocate(pix)
	if (allocated(footer)) deallocate(footer)
	if (allocated(old_pix)) deallocate(old_pix)
	if (allocated(old_footer)) deallocate(old_footer)
    return
end subroutine load_slice

!-----------------------------------------------------------------------
! Transfer pixel info to an array of exactly the correct size, and cleanup
subroutine load_slice_pixels(npixtot,pix_out)
    implicit none      
    integer(i4b) :: npixtot
	real(dp) :: pix_out(7,npixtot)
	pix_out=pix(:,1:npixtot)
	deallocate(pix)
	return
end subroutine load_slice_pixels

!-----------------------------------------------------------------------
! Transfer as much of footer information as can into string, and cleanup
subroutine load_slice_footer(nfooter,footer_out)
    implicit none      
    integer(i4b) :: nfooter, i
	character(len=*):: footer_out(nfooter)
	do i=1,nfooter
		footer_out(i)=footer(i)
	end do
	deallocate(footer)
	return
end subroutine load_slice_footer



subroutine IXFget_slice(filename,header,x,y,c,e,npix,pix_o,footer_o,status)
  implicit none 
  character(len=*)::filename
  character(len=long_len),allocatable::footer_o(:)
  type(IXTstatus)::status  
  real(dp),pointer::y(:),x(:),pix_o(:,:),e(:),npix(:),c(:)
  real(dp)::header(6)
  integer(i4b)::nfooter,npixtot,nx,ny,np
  
!C     Read number of pixels 
      call load_slice_header(filename,header,status)
      if (status == IXCseverity_error)return 
      nx=nint(header(1))
      ny=nint(header(2))
    np=nx*ny
    call IXFalloc(x,np,status)
    call IXFalloc(y,np,status)
    call IXFalloc(c,np,status)
    call IXFalloc(e,np,status)    
    call IXFalloc(npix,np,status)
    

!     Call load_slice routine
      call load_slice(filename,np,x,y,c,e,npix,npixtot,nfooter)


      if (npixtot .lt. 0) then
        call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'error reading slice file (IXFget_slice)')
            return
      end if 

!     Get the individual pixel information
!     For a slice there will at least one pixel
      call IXFallocdims(pix_o,(/7,npixtot/),status)      
      
      call load_slice_pixels (npixtot,pix_o)
      

!     Get the footer information
      if (nfooter .ge. 1) then
!         transfer footer to temporary storage
!          nfooter=min(nfooter,nfooter_max)
          call IXFallocFortran(footer_o,nfooter,status)
          call load_slice_footer (nfooter,footer_o)          
      else
          call IXFallocFortran(footer_o,1,status)
          footer_o(1)=' '
      end if 

      end subroutine IXFget_slice

   subroutine IXFput_slice(filename,header,x,y,c,e,npix,pix_i,footer_i,status)
   implicit none
  character(len=*)::footer_i(:),filename
  type(IXTstatus)::status
  integer(i4b)::np,npixtot
  real(dp)::y(:),x(:),pix_i(:,:),header(6),e(:),c(:),npix(:)     
   


! footer comes in through the binding in the form of an allocatable array of characters 
   
!     Get no. points and no. pixels 
      np=size(x) !mxGetM(prhs(3))
      npixtot=size(pix_i,2) !mxGetN(prhs(8))

      call write_slice(np,npixtot,header,x,y,c,e,npix,pix_i, footer_i,filename,status)

   
   end subroutine IXFput_slice
!-----------------------------------------------------------------------
! Write slice data 
      subroutine write_slice(np, npixtot, header, x, y, c, e, npix, pix_i,  footer_i, filename, status)
      implicit none      
      integer(i4b):: np, npixtot
      real(dp):: header(6), x(np), y(np), c(np), e(np), npix(np), pix_i(7,npixtot), err
      character(len=*):: footer_i(:), filename
      integer(i4b):: nx, ny, i, j, npix_end
      real(dp):: xorig, yorig, dx, dy
      type(IXTstatus)::status
      err=0.0d0
      
      nx=nint(header(1))
      ny=nint(header(2))
      xorig=header(3)
      yorig=header(4)
      dx=header(5)
      dy=header(6)

      open(unit=1,file=filename,status='REPLACE',ERR=999)
      write(1,'(2i8,4g17.5)',ERR=999) nx,ny,xorig,yorig,dx,dy
      npix_end=0
      do i=1,np
          write (1,'(4g17.5,i8)',ERR=999)x(i),y(i),c(i),e(i),nint(npix(i))
          do j=npix_end+1,npix_end+npix(i)
              write (1,'(i8,6g17.5)',ERR=999) nint(pix_i(1,j)),pix_i(2,j),pix_i(3,j),pix_i(4,j),pix_i(5,j),pix_i(6,j),pix_i(7,j)
          end do
          npix_end=npix_end+npix(i)
      end do
      
          do i=1,size(footer_i)              
              write(1,'(a)') footer_i(i)
          end do
      

      close(unit=1)
      return

 999  call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'error reading spe file header (write_slice)')

      close(unit=1)
      return
      end subroutine write_slice


end module IXMslice