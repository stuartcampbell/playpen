program classes_test
use IXMtype_definitions
use IXMlibcore
use IXMdataset_1d
use IXMdataset_2d
use IXMdatum
use IXMraw_file
use IXMrunfile
use IXMgroups
implicit none
real(dp), pointer :: array(:)
real(dp) :: s2(20,3004),e2(20,3004),x(21),y(3005),ix_v(20),ix_e(20),iy_v(35),iy_e(35),x1(35),s1(35),e1(35)
real(dp) :: xmin,xmax
real(dp):: ymin,ymax
real(dp), allocatable :: time_chans(:)
integer, allocatable :: counts(:,:)
type (IXTdatum)::iresA,iresB,iresC
integer(i4b)::i,nx,spec_lo,spec_hi
integer :: spec_nums(4) = (/ 1, 2, 3, 4 /)
type(IXTstatus) :: status
type(IXTdataset_1d) :: w1a, w1b,w1d, wres, d1d_array(4)
type(IXTdataset_2d) :: w2d,wr2d,wrb2d,wrg2d,w2res
    type(IXToperation)::op
logical::xhist,xdist,ydist,yhist
type(IXTraw_file) :: raw_file
type(IXTfileio) :: fio
type(IXTrunfile) :: rf
type(IXTgroups) :: ca
integer :: vali(1), ntimechan, nspec
write(6,*) 'Starting classes test program'
if (IXFlibrary_init() /= 0) then
    stop 'error initialising library'
endif



call IXFopen_raw('mar09948.raw', raw_file, status)
call IXFget_raw(raw_file, (/10/), wr2d,1, status)
call IXFget_raw(raw_file, (/11,12,13/), w2d,1, status)
call IXFdataset_2d_X_plus_dataset_2d(w2res,w2d,wr2d,status)
call IXFdataset_2d_Y_plus_dataset_2d(w2res,w2d,wr2d,status)

call IXFadd_groups(ca, 'g1', ' ', status)
call IXFadd_groups(ca, 'g2', 'g1', status)
call IXFadd_groups(ca, 'g3', 'g1', status)
!call IXFprint_groups(ca, 'g2', status)
call IXFfile_open(fio,'test.nxs',IXC_CREATE,status)
!call IXFfile_write(w1a,fio,'w',status)
call IXFfile_write(ca,fio,'ca',status)
call IXFfile_close(fio,status)
!call IXFfile_open(fio,'test.nxs',IXC_CREATE,status)
!call IXFfile_write(rf,fio,'r',status)
!call IXFfile_close(fio,status)
pause
call IXFfile_open(fio,'test.nxs',IXC_READ,status)
!call IXFfile_write(w1a,fio,'w',status)
call IXFfile_read(ca,fio,'ca',status)
call IXFfile_close(fio,status)
pause

spec_lo=11
spec_hi=16

  xmin=11.5_dp
  xmax=14.56_dp
  ymin=12.5_dp
  ymax=16.3_dp
  xdist=.true.
  ydist=.false.
  xhist=.true.
  yhist=.true.
  x=(/(i,i=1,21)/)
  y=(/(i,i=1,3005)/)
  x1=(/(i,i=1,35)/)
  ix_v=0.0_dp
  ix_e=0.0_dp
  iy_v=0.0_dp
  iy_e=0.0_dp
!  call random_number(s)
  s1=1.0_dp
  e1=1.0_dp
  s2=1.0_dp
  e2=1.0_dp
  call random_number(e1)
  call random_number(e2)
  call random_number(s2)
!check for y only
!e2(1,:)=e1
!e2(2,:)=e1
!e2(3,:)=e1
! check for x only when e1 is the right shape
!e2(:,1)=e1
!e2(:,2)=e1
!e2(:,3)=e1

!call IXFSetDdataset_1d(w1d,status,signal=s1,error=e1,x=x1,x_distribution=xdist,x_histogram=xhist)
!call IXFSet_dataset_2d(w2d,status,signal=s2,error=e2,x=x,  &
 !             x_distribution=xdist,x_histogram=xhist,y=y,y_distribution=ydist,y_histogram=yhist)

!call IXFintegrateXdataset_2d(w1a,w2d,xmin,xmax,status)

!call IXFintegrateYdataset_2d(w1b,w2d,ymin,ymax,status)

!call IXFintegrateXYdataset_2d(iresA,w2d,xmin,xmax,ymin,ymax,status)

!call IXFdataset_1d_integrate(iresB, w1d, ymin, ymax, status)

!call IXFintspecdataset_2d(iresC,w2D,xmin,xmax,spec_lo,spec_hi,status)

call IXFRebin_x_dataset_2d(wr2d,status,w2d,Xdesc=(/ 0.5_dp, 1.75_dp, 25.0_dp /))

call  IXFRebunch_XY_dataset_2d(wrb2d,w2d,3,300,status)

call IXFRegroup_XY_dataset_2d(wrg2d,w2d,(/ 0.5_dp, 1.75_dp, 25.0_dp /),(/ 0.5_dp, 300.0_dp, 3050.0_dp /),status)

call IXFdisplay(wres,status)
nx = 10
call IXFalloc(array, nx, status)
do i = 1,nx
 array(i) = 1.0
enddo
write(6,*) array

! Raw file testing

call IXFopen_raw('mar09948.raw', raw_file, status)
call IXFget_raw(raw_file, spec_nums, d1d_array,1, status)
call IXFget_raw(raw_file, spec_nums, w2d,1, status)
call IXFget_raw(raw_file, 'NTC1', ntimechan, status)
call IXFget_raw(raw_file, 'NSP1', nspec, status)
allocate(time_chans(ntimechan+1))
call IXFget_raw(raw_file, 'TIM1', time_chans, status)
allocate(counts(ntimechan+1, 4))
call IXFget_raw(raw_file, spec_nums, counts, status)


call IXFlibrary_finish(status)
pause 'press any key to finish'

end
