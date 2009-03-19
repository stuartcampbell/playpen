module IXMcut
  use IXMtype_definitions
  use IXMstatus
  use IXMmemory

	real(dp), allocatable :: pix(:,:)
!	save :: pix
	character(len=long_len), allocatable :: footer(:)

contains

subroutine IXFget_cut(filename,x,y,e,npix,pix_o,footer_o,status)
  implicit none
  integer(i4b)::np
  character(len=*)::filename
  character(len=long_len),allocatable::footer_o(:)
  type(IXTstatus)::status  
  real(dp),pointer::y(:),x(:),pix_o(:,:),e(:),npix(:)  
  integer(i4b)::nfooter,npixtot
  
!C     Read number of pixels 
      call load_cut_header(filename,np,status)
      if (status == IXCseverity_error)return 

    call IXFalloc(x,np,status)
    call IXFalloc(y,np,status)
    call IXFalloc(e,np,status)
    call IXFalloc(npix,np,status)
    

!     Call load_cut routine
      call load_cut(filename,np,x,y,e,npix,npixtot,nfooter)


      if (npixtot .lt. 0) then
        call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'error reading cut file (IXFget_cut)')
            return
      end if 

!     Get the individual pixel information
!     For a cut there will at least one pixel
      call IXFallocdims(pix_o,(/6,npixtot/),status)      
      
      call load_cut_pixels (npixtot,pix_o)
      

!     Get the footer information
      if (nfooter .ge. 1) then
!         transfer footer to temporary storage
!          nfooter=min(nfooter,nfooter_max)
          call IXFallocFortran(footer_o,nfooter,status)
          call load_cut_footer (nfooter,footer_o)          
      else
          call IXFallocFortran(footer_o,1,status)
          footer_o(1)=' '
      end if 

      end subroutine IXFget_cut

!-----------------------------------------------------------------------
! Routines for reading and writing Mslice cuts in Fortran
!
! T.G.Perring March 2008
!-----------------------------------------------------------------------

subroutine load_cut(filename,np,x,y,e,npix,npixtot,nfooter)
    implicit none      
    integer(i4b) :: np, npixtot, nfooter
	real(dp) :: x(:), y(:), e(:), npix(:)	! if pass from F77 code, need to declare sizes, not x(:) etc.
	character(len=*) :: filename

	integer(i4b) :: iunit, npixbuff, nfooterbuff, dummy, ip, npix_int, ipix, i
	real(dp), allocatable :: old_pix(:,:)	! temporary storage if reallocate pix
	character(len=long_len), allocatable :: old_footer(:)	! temporary footer storage if reallocate footer

	iunit=1			! unit from which to read data
	npixbuff=10000	! initial size of pixel buff
	nfooterbuff=100	! initial size of footer buffer (>0)

	open(unit=iunit,file=filename,action='READ',err=999)
	read(iunit,*,err=999) dummy

	if (allocated(pix)) deallocate(pix)			! will be saved between calls from Matlab
	allocate (pix(6,npixbuff))	! allocate storage

	if (allocated(footer)) deallocate(footer)
	allocate (footer(nfooterbuff))


	! Read in the pixel information
	npixtot = 0
	do ip = 1, np
		read (iunit,*,err=999,end=999) x(ip), y(ip), e(ip), npix(ip)
		npix_int = nint(npix(ip))
		! Reallocate pixel buffer if not large enough to hold the extra pixels from this point
		if (npixtot+npix_int > npixbuff) then
			if (allocated(old_pix)) deallocate(old_pix)
			allocate(old_pix(6,npixtot))
			old_pix=pix(:,1:npixtot)
			deallocate(pix)
			npixbuff=2_i4b*max(npixbuff,npix_int)	! guaranteed to be large enough to hold extra pixels
			allocate(pix(6,npixbuff))
			pix(:,1:npixtot)=old_pix
			deallocate(old_pix)
		endif
		! Read pixel information
		if (npix_int>0) then
			do ipix =npixtot+1,npixtot+npix_int
				read (iunit,*,err=999,end=999) (pix(i,ipix),i=1,6)
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
end subroutine load_cut



subroutine load_cut_header(filename,np,status)
	implicit none
	integer(i4b),intent(out) :: np
	character(len=*) :: filename
	type(IXTstatus)::status

	open(unit=1,file=filename,action='READ',err=999)
	read(1,*,err=999) np 
	close(unit=1)  
	return  

999 np=0	! convention for error reading file
call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'error reading cut file header (load_cut_header)')    
	close(unit=1)
	return 
end subroutine load_cut_header

! Transfer pixel info to an array of exactly the correct size, and cleanup
subroutine load_cut_pixels(npixtot,pix_out)
    implicit none      
    integer(i4b) :: npixtot
	real(dp) :: pix_out(6,npixtot)
	pix_out=pix(:,1:npixtot)
	deallocate(pix)
	return
end subroutine load_cut_pixels

!-----------------------------------------------------------------------
! Transfer as much of footer information as can into string, and cleanup
subroutine load_cut_footer(nfooter,footer_out)
    implicit none      
    integer(i4b) :: nfooter, i
	character(len=*):: footer_out(nfooter)
	do i=1,nfooter
		footer_out(i)=footer(i)
	end do
	deallocate(footer)
	return
end subroutine load_cut_footer

subroutine IXFput_cut(filename,x,y,e,npix,pix_i,footer_i,status)
implicit none
  character(len=long_len)::footer_i(:),filename
  type(IXTstatus)::status
  integer(i4b)::np,npixtot
  real(dp)::y(:),x(:),pix_i(:,:),e(:),npix(:)     
 
      np=size(x) !mxGetM(prhs(3))
      npixtot=size(pix_i,2) !mxGetN(prhs(8))

 call write_cut(np,npixtot, x, y, e, npix, pix_i, footer_i, filename, status)

end subroutine IXFput_cut



!-----------------------------------------------------------------------
! Write cut data 
      subroutine write_cut(np, npixtot, x, y, e, npix, pix_i, footer_i, filename, status)
      implicit none      
      integer(i4b):: np, npixtot
      real(dp),intent(in):: x(np), y(np), e(np), npix(np), pix_i(6,npixtot)
      character(len=*),intent(in):: footer_i(:), filename
      integer(i4b):: i, j, npix_end
      type(IXTstatus)::status

            
      open(unit=1,file=filename,status='REPLACE',ERR=999)
      write(1,'(i8)',ERR=999) np
      npix_end=0
      do i=1,np
          write (1,'(3g17.5,i8)',ERR=999) x(i),y(i),e(i),nint(npix(i))
          do j=npix_end+1,npix_end+npix(i)
              write (1,'(i8,5g17.5)',ERR=999) nint(pix_i(1,j)),pix_i(2,j),pix_i(3,j),pix_i(4,j),pix_i(5,j),pix_i(6,j)
          end do
          npix_end=npix_end+npix(i)
      end do

          do i=1,size(footer_i)             
              write(1,'(a)') footer_i(i)
          end do


      close(unit=1)
      return

 999  call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'error writing cut file header (write_cut)')
      close(unit=1)
      return
      end subroutine write_cut

end module IXMcut