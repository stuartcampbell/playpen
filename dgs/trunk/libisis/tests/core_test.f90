program core_test
  use IXMlibcore
  use IXMtype_definitions
  use IXMstatus
  use IXMmemory
  use IXMarraymanips
  use IXMintegrate
  use IXMunspike
 ! use IXMraw_file
  implicit none
  logical found
  real(dp) :: s(20,35),e(20,35),x(21),y(36),ix_v(35),ix_e(35),iy_v(20),iy_e(20),iy_x(21)
  integer(i4b)::i, vali(1)
  real(dp):: xmin,xmax,ymin,ymax
  logical::xdist,ydist,xhist,yhist
  type(IXTstatus) :: status
!  type(IXTraw_file) :: raw_file
  write(6,*) 'Starting core test program'
  if (IXFlibrary_init() /= 0) then
    stop 'error initialising library'
  endif
  xmin=11.5_dp
  xmax=26.3_dp

  ymin=15.78_dp
  ymax=23.3_dp

  xhist=.true.
  yhist=.true.
  xdist=.true.
  ydist=.true.

  x=(/(i,i=1,21)/)
  y=(/(i,i=1,36)/)
  ix_v=0.0_dp
  ix_e=0.0_dp

  iy_v=0.0_dp
  iy_e=0.0_dp

!  call random_number(s)
  s=1.0_dp
  call random_number(e)

!call IXFintegrate_2d_hist(s,e,status,x=x,xdist=xdist,xhist=xhist,xmin_in=xmin,xmax_in=xmax,val_ar=ix_v,err_ar=ix_e)
!call IXFintegrate_2d_hist(s,e,status,x=x,y=y,ydist=ydist,yhist=yhist,ymin_in=ymin,ymax_in=ymax,val_ar=iy_v,err_ar=iy_e,x_ar=iy_x)
  if(status == IXCseverity_error) stop 'error found'
  

  write(6,*) 'Ending core test program',iy_v(1:10)
  call IXFlibrary_finish(status)
  pause 'press any key to finish'
end program core_test

