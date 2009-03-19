program dataset2d_test
use IXMlibcore
use IXMdataset_1d
use IXMdataset_2d
use IXMraw_file

implicit none

type(IXTdataset_2d)::w_hxdhyd,w_hxhyd,w_hxdhy,w_hxhy
type(IXTdataset_2d)::w_hxdpyd,w_hxpyd,w_hxdpy,w_hxpy
type(IXTdataset_2d)::w_pxdhyd,w_pxhyd,w_pxdhy,w_pxhy
type(IXTdataset_2d)::w_pxdpyd,w_pxpyd,w_pxdpy,w_pxpy 
type(IXTdataset_1d),allocatable::d1dalloc(:)
type(IXTdataset_2d),allocatable::d2dalloc(:)
real(dp)::xh(21),xp(20),yh(21),yp(20)
real(dp)::s(20,20),e(20,20)
type(IXTstatus)::status
integer(i4b)::i,xbunch,ybunch
real(dp)::xmin,xdelta,xmax,ymin,ydelta,ymax

type(IXTdataset_2d)::wRaw,newd2d
type(IXTdataset_1d)::timmy1d,new1d
type(IXTraw_file) :: raw_file
integer,allocatable :: spec_nums(:)
integer(i4b),allocatable ::another1d(:),another2d(:,:)
real(dp),allocatable :: test_alloc1d(:),test_alloc2d(:,:)
real(dp)::test_fix1d(922),test_fix2d(92,234)
integer::nspec,ntime 

write(6,*) 'Starting dataset_1d test program'
if (IXFlibrary_init() /= 0) then
    stop 'error initialising library'
endif

allocate(another1d(3500),another2d(1000,200))
another1d=(/(i,i=3500,1,-1)/)
!call random_number(another1d)
!call random_number(another2d)

call IXFopen_raw('mar09948.raw', raw_file, status)

!call IXFopen_raw('TSC05999.RAW', raw_file, status)

!call IXFopen_raw('map06215.RAW', raw_file, status)

! determine number of spectra in raw file
call IXFget_raw(raw_file, 'NSP1', nspec, status)
call IXFget_raw(raw_file, 'NTC1', ntime, status)

allocate (spec_nums(nspec))
!allocate (spec_nums(900))
spec_nums=(/(i,i=1,nspec)/)
!spec_nums=(/(i,i=1,900)/)
! this reads the raw file into a dataset_2d with y-dimension spec_nums(nspec)
call IXFget_raw(raw_file, spec_nums, wRaw, status)

!call IXFget_raw(raw_file, 24 , new1d,status)

!call IXFintegrate_x_dataset_2d(timmy1d,wRaw,2000.0_dp,20000.0_dp,status)

call IXFexpand_arrayd1d_dataset_2d(wRaw,status,d1dalloc)
call IXFexpand_arrayd1d_dataset_2d(wRaw,status,d1dalloc,(/ 1,4,5,-6,8,9,10 /))
call IXFexpand_arrayd2d_dataset_2d(wRaw,status,d2dalloc)
call IXFexpand_arrayd2d_dataset_2d(wRaw,status,d2dalloc,(/ 1,4,5,6,492,493,494,1094 /))
call IXFcheck(d1dalloc,status)
call IXFcheck(d2dalloc(4),status)

call IXFcontract_arrayd2d_dataset_2d(d2dalloc,newd2d,status)


allocate(test_alloc2d(3,5))
!call IXFget_alloc_dataset_2d(wRaw,x_alloc=test_alloc1d,s_alloc=test_alloc2d)
!call IXFget_dataset_2d(wRaw,y=test_fix1d,signal=test_fix2d,status=status)

deallocate (spec_nums)
deallocate(test_alloc2d)
call IXFdealloc(d1dalloc,status)
call IXFdealloc(d2dalloc,status)



xh=(/(i,i=1,41,2)/)
xp=(/(i,i=2,40,2)/)

yh=(/(i,i=1,41,2)/)
yp=(/(i,i=2,40,2)/)
 

xmin=5.0_dp
xdelta=1.5_dp
xmax=15.0_dp

ymin=25.0_dp
ydelta=1.75_dp
ymax=35.0_dp

xbunch=3
ybunch=4

  call random_number(e)
  call random_number(s)



call IXFSet_dataset_2d(w_hxdhyd,status,signal=s,error=e,  &
    x=xh,x_distribution=.true.,  &
    y=yh,y_distribution=.true.)


call IXFSet_dataset_2d(w_hxdhy,status,signal=s,error=e,  &
    x=xh,x_distribution=.true.,  &
    y=yh,y_distribution=.false.)

call IXFSet_dataset_2d(w_hxhyd,status,signal=s,error=e,  &
    x=xh,x_distribution=.false.,  &
    y=yh,y_distribution=.true.)

call IXFSet_dataset_2d(w_hxhy,status,signal=s,error=e,  &
    x=xh,x_distribution=.false.,  &
    y=yh,y_distribution=.false.)




call IXFSet_dataset_2d(w_hxpy,status,signal=s,error=e,  &
    x=xh,x_distribution=.false.,  &
    y=yp,y_distribution=.false.)

call IXFSet_dataset_2d(w_hxdpy,status,signal=s,error=e,  &
    x=xh,x_distribution=.true.,  &
    y=yp,y_distribution=.false.)
   
call IXFSet_dataset_2d(w_hxpyd,status,signal=s,error=e,  &
    x=xh,x_distribution=.false.,  &
    y=yp,y_distribution=.true.)
    
call IXFSet_dataset_2d(w_hxdpyd,status,signal=s,error=e,  &
    x=xh,x_distribution=.true.,  &
    y=yp,y_distribution=.true.)        




call IXFSet_dataset_2d(w_pxdhyd,status,signal=s,error=e,  &
    x=xp,x_distribution=.true.,  &
    y=yh,y_distribution=.true.)        


call IXFSet_dataset_2d(w_pxdhy,status,signal=s,error=e,  &
    x=xp,x_distribution=.true.,  &
    y=yh,y_distribution=.false.)        


call IXFSet_dataset_2d(w_pxhyd,status,signal=s,error=e,  &
    x=xp,x_distribution=.false.,  &
    y=yh,y_distribution=.true.)        


call IXFSet_dataset_2d(w_pxhy,status,signal=s,error=e,  &
    x=xp,x_distribution=.false.,  &
    y=yh,y_distribution=.false.)        


call IXFSet_dataset_2d(w_pxpy,status,signal=s,error=e,  &
    x=xp,x_distribution=.false.,  &
    y=yp,y_distribution=.false.)  

call IXFSet_dataset_2d(w_pxdpy,status,signal=s,error=e,  &
    x=xp,x_distribution=.true.,  &
    y=yp,y_distribution=.false.)  

call IXFSet_dataset_2d(w_pxpyd,status,signal=s,error=e,  &
    x=xp,x_distribution=.false.,  &
    y=yp,y_distribution=.true.)  

call IXFSet_dataset_2d(w_pxdpyd,status,signal=s,error=e,  &
    x=xp,x_distribution=.true.,  &
    y=yp,y_distribution=.true.)  






call arrayops(w_hxdhyd)
call arrayops(w_hxhyd)
call arrayops(w_hxdhy)
call arrayops(w_hxhy)
call arrayops(w_hxdpyd)
call arrayops(w_hxpyd)
call arrayops(w_hxdpy)
call arrayops(w_hxpy)
call arrayops(w_pxdhyd)
call arrayops(w_pxhyd)
call arrayops(w_pxdhy)
call arrayops(w_pxhy)
call arrayops(w_pxdpyd)
call arrayops(w_pxpyd)
call arrayops(w_pxdpy)
call arrayops(w_pxpy)


call integrate(w_hxdhyd,xmin,xmax,ymin,ymax)
call integrate(w_hxhyd,xmin,xmax,ymin,ymax)
call integrate(w_hxdhy,xmin,xmax,ymin,ymax)
call integrate(w_hxhy,xmin,xmax,ymin,ymax)
call integrate(w_hxdpyd,xmin,xmax,ymin,ymax)
call integrate(w_hxpyd,xmin,xmax,ymin,ymax)
call integrate(w_hxdpy,xmin,xmax,ymin,ymax)
call integrate(w_hxpy,xmin,xmax,ymin,ymax)
call integrate(w_pxdhyd,xmin,xmax,ymin,ymax)
call integrate(w_pxhyd,xmin,xmax,ymin,ymax)
call integrate(w_pxdhy,xmin,xmax,ymin,ymax)
call integrate(w_pxhy,xmin,xmax,ymin,ymax)
call integrate(w_pxdpyd,xmin,xmax,ymin,ymax)
call integrate(w_pxpyd,xmin,xmax,ymin,ymax)
call integrate(w_pxdpy,xmin,xmax,ymin,ymax)
call integrate(w_pxpy,xmin,xmax,ymin,ymax)


call rebunch(w_hxdhyd,xbunch,ybunch)
call rebunch(w_hxhyd,xbunch,ybunch)
call rebunch(w_hxdhy,xbunch,ybunch)
call rebunch(w_hxhy,xbunch,ybunch)
call rebunch(w_hxdpyd,xbunch,ybunch)
call rebunch(w_hxpyd,xbunch,ybunch)
call rebunch(w_hxdpy,xbunch,ybunch)
call rebunch(w_hxpy,xbunch,ybunch)
call rebunch(w_pxdhyd,xbunch,ybunch)
call rebunch(w_pxhyd,xbunch,ybunch)
call rebunch(w_pxdhy,xbunch,ybunch)
call rebunch(w_pxhy,xbunch,ybunch)
call rebunch(w_pxdpyd,xbunch,ybunch)
call rebunch(w_pxpyd,xbunch,ybunch)
call rebunch(w_pxdpy,xbunch,ybunch)
call rebunch(w_pxpy,xbunch,ybunch)


call regroup(w_hxdhyd,(/ xmin,xdelta,xmax /),(/ ymin,ydelta,ymax /))
call regroup(w_hxhyd,(/ xmin,xdelta,xmax /),(/ ymin,ydelta,ymax /))
call regroup(w_hxdhy,(/ xmin,xdelta,xmax /),(/ ymin,ydelta,ymax /))
call regroup(w_hxhy,(/ xmin,xdelta,xmax /),(/ ymin,ydelta,ymax /))
call regroup(w_hxdpyd,(/ xmin,xdelta,xmax /),(/ ymin,ydelta,ymax /))
call regroup(w_hxpyd,(/ xmin,xdelta,xmax /),(/ ymin,ydelta,ymax /))
call regroup(w_hxdpy,(/ xmin,xdelta,xmax /),(/ ymin,ydelta,ymax /))
call regroup(w_hxpy,(/ xmin,xdelta,xmax /),(/ ymin,ydelta,ymax /))
call regroup(w_pxdhyd,(/ xmin,xdelta,xmax /),(/ ymin,ydelta,ymax /))
call regroup(w_pxhyd,(/ xmin,xdelta,xmax /),(/ ymin,ydelta,ymax /))
call regroup(w_pxdhy,(/ xmin,xdelta,xmax /),(/ ymin,ydelta,ymax /))
call regroup(w_pxhy,(/ xmin,xdelta,xmax /),(/ ymin,ydelta,ymax /))
call regroup(w_pxdpyd,(/ xmin,xdelta,xmax /),(/ ymin,ydelta,ymax /))
call regroup(w_pxpyd,(/ xmin,xdelta,xmax /),(/ ymin,ydelta,ymax /))
call regroup(w_pxdpy,(/ xmin,xdelta,xmax /),(/ ymin,ydelta,ymax /))
call regroup(w_pxpy,(/ xmin,xdelta,xmax /),(/ ymin,ydelta,ymax /))


call rebin(w_hxdhyd,(/ xmin,xdelta,xmax /),(/ ymin,ydelta,ymax /))
call rebin(w_hxhyd,(/ xmin,xdelta,xmax /),(/ ymin,ydelta,ymax /))
call rebin(w_hxdhy,(/ xmin,xdelta,xmax /),(/ ymin,ydelta,ymax /))
call rebin(w_hxhy,(/ xmin,xdelta,xmax /),(/ ymin,ydelta,ymax /))
call rebin(w_hxdpyd,(/ xmin,xdelta,xmax /),(/ ymin,ydelta,ymax /))
call rebin(w_hxpyd,(/ xmin,xdelta,xmax /),(/ ymin,ydelta,ymax /))
call rebin(w_hxdpy,(/ xmin,xdelta,xmax /),(/ ymin,ydelta,ymax /))
call rebin(w_hxpy,(/ xmin,xdelta,xmax /),(/ ymin,ydelta,ymax /))
call rebin(w_pxdhyd,(/ xmin,xdelta,xmax /),(/ ymin,ydelta,ymax /))
call rebin(w_pxhyd,(/ xmin,xdelta,xmax /),(/ ymin,ydelta,ymax /))
call rebin(w_pxdhy,(/ xmin,xdelta,xmax /),(/ ymin,ydelta,ymax /))
call rebin(w_pxhy,(/ xmin,xdelta,xmax /),(/ ymin,ydelta,ymax /))
call rebin(w_pxdpyd,(/ xmin,xdelta,xmax /),(/ ymin,ydelta,ymax /))
call rebin(w_pxpyd,(/ xmin,xdelta,xmax /),(/ ymin,ydelta,ymax /))
call rebin(w_pxdpy,(/ xmin,xdelta,xmax /),(/ ymin,ydelta,ymax /))
call rebin(w_pxpy,(/ xmin,xdelta,xmax /),(/ ymin,ydelta,ymax /))



call IXFlibrary_finish(status)
pause 'press any key to finish'

end

subroutine rebin(w_in,rebx,reby)
use IXMdataset_2d
implicit none
type(IXTdataset_2d),intent(in)::w_in
type(IXTdataset_2d)::wres
type(IXTstatus)::status
real(dp)::rebx(3),reby(3)

call IXFRebin_XY_dataset_2d(wres,status,w_in,Xdesc=rebx,Ydesc=reby)
call IXFdestroy(wres,status)
call IXFclear_status(status)
call IXFRebin_Y_dataset_2d(wres,status,w_in,Ydesc=reby)
call IXFdestroy(wres,status)
call IXFclear_status(status)
call IXFRebin_X_dataset_2d(wres,status,w_in,Xdesc=rebx)
call IXFdestroy(wres,status)
call IXFclear_status(status)


end subroutine rebin

subroutine arrayops(w_in)
use IXMdataset_2d
implicit none
type(IXTdataset_2d),intent(in)::w_in
type(IXTdataset_2d)::wres
type(IXTstatus)::status
call IXFplus(wres,w_in,2.0_dp,status)
call IXFdestroy(wres,status)
call IXFminus(wres,w_in,2.0_dp,status)
call IXFdestroy(wres,status)
call IXFtimes(wres,w_in,2.0_dp,status)
call IXFdestroy(wres,status)
call IXFpower(wres,w_in,2.0_dp,status)
call IXFdestroy(wres,status)
call IXFexp(wres,w_in,status)
call IXFdestroy(wres,status)
call IXFlog(wres,w_in,status)
call IXFdestroy(wres,status)
call IXFsin(wres,w_in,status)
call IXFdestroy(wres,status)
call IXFcos(wres,w_in,status)
call IXFdestroy(wres,status)
call IXFtan(wres,w_in,status)
call IXFdestroy(wres,status)
call IXFsinh(wres,w_in,status)
call IXFdestroy(wres,status)
call IXFcosh(wres,w_in,status)
call IXFdestroy(wres,status)
call IXFtanh(wres,w_in,status)
call IXFdestroy(wres,status)

end subroutine arrayops

subroutine integrate(w_in,xmin,xmax,ymin,ymax)
use IXMdataset_1d
use IXMdataset_2d
use IXMdatum
implicit none
type(IXTdataset_2d),intent(in)::w_in
type(IXTdataset_1d)::wres
type(IXTdatum)::ires
type(IXTstatus)::status
real(dp)::xmin,xmax,ymin,ymax

call IXFintegrate_X_dataset_2d(wres,w_in,xmin,xmax,status)
call IXFdestroy(wres,status)
call IXFclear_status(status)
call IXFintegrate_Y_dataset_2d(wres,w_in,ymin,ymax,status)
call IXFdestroy(wres,status)
call IXFclear_status(status)
call IXFintegrate_XY_dataset_2d(ires,w_in,xmin,xmax,ymin,ymax,status)
call IXFdestroy(wres,status)
call IXFclear_status(status)

end subroutine integrate

subroutine rebunch(w_in,Xbunch,Ybunch)
use IXMdataset_2d
implicit none
type(IXTdataset_2d),intent(in)::w_in
type(IXTdataset_2d)::wres
type(IXTstatus)::status
integer(i4b)::Xbunch,Ybunch


call IXFRebunch_X_dataset_2d(wres,w_in,Xbunch,status)
call IXFdestroy(wres,status)
call IXFRebunch_Y_dataset_2d(wres,w_in,Ybunch,status)
call IXFdestroy(wres,status)
call IXFRebunch_XY_dataset_2d(wres,w_in,Xbunch,Ybunch,status)
call IXFdestroy(wres,status)

end subroutine rebunch

subroutine regroup(w_in,paramX,paramY)
use IXMdataset_2d
implicit none
type(IXTdataset_2d),intent(in)::w_in
type(IXTdataset_2d)::wres
type(IXTstatus)::status
real(dp)::paramX(3),paramY(3)

call IXFRegroup_X_dataset_2d(wres,w_in,paramX,status)
call IXFdestroy(wres,status)
call IXFclear_status(status)
call IXFRegroup_Y_dataset_2d(wres,w_in,paramY,status)
call IXFdestroy(wres,status)
call IXFclear_status(status)
call IXFRegroup_XY_dataset_2d(wres,w_in,paramX,paramY,status)
call IXFdestroy(wres,status)
call IXFclear_status(status)

end subroutine regroup

