program core_test
  use IXMlibcore
  use IXMtype_definitions
  use IXMstatus
  use IXMintegrate
  use IXMtools
  use IXMunspike

  implicit none
  integer(i4b),parameter::nmax=10000
  integer(i4b) istatus,n,i,nbad
  real(dp) xin(nmax),yin(nmax),ein(nmax)
  real(dp),allocatable:: xout(:),yout(:),eout(:)
  real(dp) ymin,ymax,fac,sfac
  type(IXTstatus) status
  
! ====== Read in point data:
  open (unit=1,file='c:\temp\test_in.dat',status='old')
  n=1
1 read(1,*,iostat=istatus) xin(n),yin(n),ein(n)
  n=n+1
  if (istatus==0 .and. n<=nmax) goto 1
  n=n-2
  close(1)


! ====== Perform manipulation:
  call prompt('Give ymin, ymax : ')
  read(5,*) ymin,ymax
  call prompt('Give fac, sfac : ')
  read(5,*) fac, sfac
  allocate(xout(n),yout(n),eout(n))
  xout = xin(1:n)
  call IXFunspike_1d (status, xin, yin, ein, yout, eout, ymin, ymax, fac, sfac, nbad)
  write(6,*) 'Number of bad points = ',nbad

! ====== Write out results:

  open (unit=1,file='c:\temp\test_out.dat',status='replace')
  do i = 1,n
    write(1,*) xout(i),yout(i),eout(i)
  end do
  close(1)
  

  write(6,*) 'Ending core test program'
  call IXFlibrary_finish(status)
  pause 'press any key to finish'
end program core_test

