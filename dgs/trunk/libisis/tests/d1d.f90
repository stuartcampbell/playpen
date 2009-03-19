program dataset1d_test
use IXMlibcore
use IXMdataset_1d
use IXMdatum
implicit none

type(IXTdataset_1d)::w_hd,w_h,w_pd,w_p,wres !four different input dataset_1d objects
!  h=histogram p=point d=distribution and combinations thereof

type(IXTdatum)::ires_hd,ires_h,ires_pd,ires_p
real(dp)::xh(21),xp(20),s(20),e(20)
type(IXTstatus)::status
integer(i4b)::i
real(dp)::xmin,delta,xmax

write(6,*) 'Starting dataset_1d test program'
if (IXFlibrary_init() /= 0) then
    stop 'error initialising library'
endif

  xh=(/(i,i=1,41,2)/)
  xp=(/(i,i=2,40,2)/)

  xmin=5.0_dp
  delta=1.5_dp
  xmax=35.0_dp
 ! call random_number(xh)
 ! call random_number(xp)
  call random_number(s)
  call random_number(e)

  !s=1.0_dp
  !e=1.0_dp

call IXFSet_dataset_1d(w_hd,status,signal=s,error=e,x=xh,x_distribution=.true.)

call IXFSet_dataset_1d(w_h,status,signal=s,error=e,x=xh,x_distribution=.false.)

call IXFSet_dataset_1d(w_pd,status,signal=s,error=e,x=xp,x_distribution=.true.)

call IXFSet_dataset_1d(w_p,status,signal=s,error=e,x=xp,x_distribution=.false.)


call arrayops(w_hd)
call arrayops(w_h)
call arrayops(w_pd)
call arrayops(w_p)



call integrate( w_hd, xmin, xmax)
call integrate( w_h, xmin, xmax)
call integrate( w_pd, xmin, xmax)
call integrate( w_p, xmin, xmax)

call rebunch(w_hd,3)
call rebunch(w_h,3)
call rebunch(w_pd,3)
call rebunch(w_p,3)

call rebin(w_hd,(/ xmin,delta,xmax /))
call rebin(w_h,(/ xmin,delta,xmax /))
call rebin(w_pd,(/ xmin,delta,xmax /))
call rebin(w_p,(/ xmin,delta,xmax /))

call regroup(w_hd,(/ xmin,delta,xmax /))
call regroup(w_h,(/ xmin,delta,xmax /))
call regroup(w_pd,(/ xmin,delta,xmax /))
call regroup(w_p,(/ xmin,delta,xmax /))

!call dataset_1dShift(w_hd,wres_hd,2.0_dp,status)
!call dataset_1dShift(w_h,wres_h,2.0_dp,status)
!call dataset_1dShift(w_pd,wres_pd,2.0_dp,status)
!call dataset_1dShift(w_p,wres_p,2.0_dp,status)


call IXFlibrary_finish(status)
pause 'press any key to finish'

end

subroutine rebin(w_in,rebx)
use IXMdataset_1d
implicit none
type(IXTdataset_1d),intent(in)::w_in
type(IXTdataset_1d)::wres
type(IXTstatus)::status
real(dp)::rebx(3)


call IXFRebin_dataset_1d(wres,w_in,Xdesc=rebx,status=status)
call IXFdestroy(wres,status)
call IXFstatus_clear(status)


end subroutine rebin

subroutine arrayops(w_in)
use IXMdataset_1d
implicit none
type(IXTdataset_1d),intent(in)::w_in
type(IXTdataset_1d)::wres
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

subroutine integrate(w_in,xmin,xmax)
use IXMdataset_1d
use IXMdatum
implicit none
type(IXTdataset_1d),intent(in)::w_in
type(IXTdatum)::ires
type(IXTstatus)::status
real(dp)::xmin,xmax

call IXFintegrate_dataset_1d(ires,w_in,xmin,xmax,status)


end subroutine integrate

subroutine rebunch(w_in,Xbunch)
use IXMdataset_1d
implicit none
type(IXTdataset_1d),intent(in)::w_in
type(IXTdataset_1d)::wres
type(IXTstatus)::status
integer(i4b)::Xbunch


call IXFrebunch_dataset_1d(wres,w_in,Xbunch,status)
call IXFdestroy(wres,status)

end subroutine rebunch

subroutine regroup(w_in,paramX)
use IXMdataset_1d
implicit none
type(IXTdataset_1d),intent(in)::w_in
type(IXTdataset_1d)::wres
type(IXTstatus)::status
real(dp)::paramX(3)

call IXFRegroup_dataset_1d(wres,w_in,paramX,status)
call IXFdestroy(wres,status)
call IXFstatus_clear(status)

end subroutine regroup
