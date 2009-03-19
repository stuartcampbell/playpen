module IXMregrid
  use IXMtype_definitions
  use IXMstatus
contains                            
  subroutine IXFregrid_ordela(lambda,thetaf,lamap,error_in,qxmin,qxmax,qzmin,qzmax,theta_inc,mapsize,offsp,error_out)
    implicit none
    real(dp),intent(in)::lambda(:),thetaf(:),lamap(:,:),error_in(:,:),theta_inc
    integer(i4b),intent(in)::mapsize
!    real(dp)::qxscale(:),qzscale(:)
    real(dp),intent(in)::qxmin,qxmax,qzmin,qzmax
    real(dp),intent(out)::error_out(:,:),offsp(:,:)

    real(dp):: dqz,dqx,thtaf1,lamda1,dthf,lam,dlam,ang,qx,qz,pi2qz,thetai,cthtai,sthtai
    real(dp)::ofsp11,ofsp12,ofsp21,ofsp22,offsp1,offsp2
    integer(i4b)::i,j,indx,indy,xind,yind,dxind,dyind,lenx,leny

! vertical spectrum spacing (mm)

!	>>correct thetai scale
!
! Slightly modifed version of JRPW''s code.  (DSS: 25/11/2002)
!
lenx=size(lambda)
leny=size(thetaf)

        thetai=theta_inc*deg_to_rad_dp
        sthtai=sin(thetai)
        cthtai=cos(thetai)

        dqz=(qzmax-qzmin)/real(mapsize)
        dqx=(qxmax-qxmin)/real(mapsize)
        thtaf1=thetaf(1)
        lamda1=lambda(1)
        dthf=real(leny-1)/(thetaf(leny)-thtaf1)
        dlam=real(lenx-1)/(lambda(lenx)-lamda1)
        do i=1,mapsize
          qz=(i-1)*dqz+qzmin
!          qzscale(i)=qz
          pi2qz=twopi_dp/qz
          do j=1,mapsize
            qx=(j-1)*dqx+qxmin
!            qxscale(j)=qx
            ang= (qz*cthtai-qx*sthtai)/(sqrt(qz*qz+qx*qx))
            ang= min(1.0,ang)
            ang= asin( ang )
            if(qx.eq.0.0)then
              ang=thetai
            else
              ang=pi_dp -ang -atan(qz/qx)
              if(ang.gt.pi_dp)ang=ang-pi_dp
            end if
            lam=pi2qz*(sthtai+sin(ang))
            yind=(ang-thtaf1)*dthf
            xind=(lam-lamda1)*dlam
            indy=int(yind)
            indx=int(xind)
            if (indy.ge.0 .and. indy.le.(leny-2) .and. indx.ge.0 .and. indx.le.(lenx-2))then
              dyind=yind-real(indy)
              dxind=xind-real(indx)
              ofsp11=lamap(indx+1,indy+1)
              ofsp12=lamap(indx+1,indy+2)
              ofsp21=lamap(indx+2,indy+1)
              ofsp22=lamap(indx+2,indy+2)
              offsp1=(1.0-dxind)*ofsp11+dxind*ofsp21
              offsp2=(1.0-dxind)*ofsp12+dxind*ofsp22
              offsp(i,j)=(1.0-dyind)*offsp1+dyind*offsp2
            else
              offsp(i,j)=0.0
            end if
          end do
	end do
	error_out=0.0d0  
    end subroutine IXFregrid_ordela
end module IXMregrid
