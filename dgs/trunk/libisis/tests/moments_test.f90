program core_test
  use IXMlibcore
  use IXMtype_definitions
  use IXMstatus
  use IXMintegrate
  use IXMtools
  use IXMmoments_utils

  implicit none
  integer(i4b),parameter::nmax=10000
  integer(i4b) istatus,n,i
  real(dp) xin(nmax),yin(nmax),ein(nmax)
  real(dp),allocatable:: xout(:),yout(:),eout(:)
  real(dp) xmin,xmax,prominence
  real(dp) area, bkgd, c, c_fwhm, h, w, xbar, sig, gam1, gam2, sig_xbar, sig_sig, sig_gam1, sig_gam2, pk_min, pk_max, bkgd_min, bkgd_max
  real(dp) del, hh_lo, hh_hi
  type(IXTstatus) status
  
! ====== Read in point data:
  open (unit=1,file='c:\temp\test_in.dat',status='old')
  n=1
1 read(1,*,iostat=istatus) xin(n),yin(n),ein(n)
  n=n+1
  if (istatus==0 .and. n<=nmax) goto 1
  n=n-2
  close(1)
  write(6,*) 'Number of points read = ',n

! ====== Perform manipulation:
  call prompt('Give xmin, xmax : ')
  read(5,*) xmin,xmax
  call prompt('Give prominence : ')
  read(5,*) prominence
  call get_moments (xin(1:n), yin(1:n), ein(1:n), xmin, xmax, prominence, status, &
	& area, bkgd, c, c_fwhm, h, w, xbar, sig, gam1, gam2, sig_xbar, sig_sig, sig_gam1, sig_gam2, pk_min, pk_max, bkgd_min, bkgd_max)
  write(6,*) area,bkgd
  write(6,*) w,c,c_fwhm
  write(6,*) xbar,sig,gam1,gam2
  write(6,*) bkgd_min,pk_min,pk_max,bkgd_max

! ====== Write out results:
  allocate(xout(14),yout(13),eout(13))
  del = 0.1_dp
  hh_lo = c_fwhm-0.5_dp*w
  hh_hi = c_fwhm+0.5_dp*w
  xout = (/bkgd_min-w, bkgd_min, pk_min-del, pk_min+del, hh_lo-del, hh_lo+del, c-del, c+del,&
        & hh_hi-del, hh_hi+del, pk_max-del, pk_max+del, bkgd_max, bkgd_max+w/)
  yout = (/0_dp*bkgd, bkgd, bkgd+0.5_dp*h, bkgd, bkgd+0.5_dp*h, bkgd, bkgd+h, bkgd, bkgd+0.5_dp*h,&
        & bkgd, bkgd+0.5_dp*h, bkgd, 0_dp*bkgd/)
  eout = (/0,0,0,0,0,0,0,0,0,0,0,0,0/)
  open (unit=1,file='c:\temp\test_out.dat',status='replace')
  do i = 1,13
    write(1,*) xout(i),yout(i),eout(i)
  end do
  write(1,*) xout(14)

  write(6,*) 'Ending core test program'
  call IXFlibrary_finish(status)
  pause 'press any key to finish'
end program core_test

