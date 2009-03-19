module IXMrebunch
  use IXMtype_definitions
  use IXMstatus
contains

  subroutine IXFrebunchHist(x,s,e,x_new,s_new,e_new,xdist,nbunch,status)
    use IXMmemory 
    implicit none
    real(dp),intent(in) :: x(:),s(:),e(:)
    real(dp),pointer:: x_new(:),s_new(:),e_new(:)
    integer(i4b),intent(in) :: nbunch
    integer(i4b) :: ntotal
    type(IXTstatus),intent(inout)::status
    logical,intent(in)::xdist

    ntotal=0

    call IXFrebunch_hist(nbunch,ntotal,s)

    call IXFalloc(x_new,ntotal+1, status)
    call IXFalloc(s_new,ntotal, status)
    call IXFalloc(e_new,ntotal, status)

    call IXFrebunch_hist(nbunch,ntotal,s,x,e,x_new,s_new,e_new,xdist)

  end subroutine IXFrebunchHist
    !*************************************************************************************************
  !*************************************************************************************************
  !*************************************************************************************************
  !*************************************************************************************************
  subroutine IXFrebunchPoints(x,s,e,x_new,s_new,e_new,nbunch,status)
    use IXMmemory
    implicit none
    real(dp),intent(in) :: x(:),s(:),e(:)
    real(dp),pointer:: x_new(:),s_new(:),e_new(:)
    integer(i4b),intent(in) :: nbunch
    integer(i4b) :: ntotal
    type(IXTstatus),intent(inout)::status


    ntotal=0

    call IXFrebunch_points(nbunch,ntotal,s)

    call IXFalloc(x_new,ntotal, status)
    call IXFalloc(s_new,ntotal, status)
    call IXFalloc(e_new,ntotal, status)

    call IXFrebunch_points(nbunch,ntotal,s,x,e,s_new,x_new,e_new)

  end subroutine IXFrebunchPoints
  !*************************************************************************************************
  !*************************************************************************************************
  !*************************************************************************************************
  !*************************************************************************************************
  subroutine IXFrebunch_hist(nbunch,ntotal,s,x,e,x_new,s_new,e_new,xdist)
    implicit none
    real(dp),intent(in)::s(:)
    real(dp),intent(in),optional :: x(:),e(:)
    real(dp),intent(inout),optional :: x_new(:),s_new(:),e_new(:)
    integer(i4b) :: nx, ns,nfull,i
    integer(i4b),intent(inout) :: ntotal
    integer(i4b),intent(in):: nbunch
    logical,intent(in),optional :: xdist

    ns= size(s)

    if(ntotal==0)then
       ntotal=((ns-1)/nbunch)+1 !total no. of bins in rebunched array
       return
    endif

    nfull=ns/nbunch !no.of full bins (ie made up of nbunch "bunched elements") in rebunched array
    nx=size(x)

    x_new(1:ntotal)=x(1:(nx-1):nbunch)
    x_new(ntotal+1)=x(nx)


    s_new=0.0_dp
    e_new=0.0_dp

    if(xdist)then

       ! fill up last bin in results with stray bins at the end
       if((ntotal - nfull) /= 0)then
          s_new(ntotal)=     sum( s( (nfull*nbunch)+1:ns)  * (x((nfull*nbunch)+2:ns+1)-x((nfull*nbunch)+1:ns))) / &
               (x_new(ntotal+1)-x_new(ntotal))
          e_new(ntotal)=sqrt(sum((e( (nfull*nbunch)+1:ns)  * (x((nfull*nbunch)+2:ns+1)-x((nfull*nbunch)+1:ns)))**2))/ &
               (x_new(ntotal+1)-x_new(ntotal))
       endif

       if(nfull /= 0)then
          ! sums up nbunch bins at a time 
          do i=1,nfull
             s_new(i)=sum( s(((i-1)*nbunch)+1:(i*nbunch))*(x(((i-1)*nbunch)+2:(i*nbunch)+1)-x(((i-1)*nbunch)+1:(i*nbunch))) ) / &
                    (x_new(i+1)-x_new(i))
             e_new(i)=&
                 sqrt(sum((e(((i-1)*nbunch)+1:(i*nbunch))*(x(((i-1)*nbunch)+2:(i*nbunch)+1)-x(((i-1)*nbunch)+1:(i*nbunch))))**2))/ &
                   (x_new(i+1)-x_new(i))

          end do
       endif
    else

       if((ntotal - nfull) /= 0)then

          s_new(ntotal)=sum(s((nfull*nbunch)+1:ns))
          e_new(ntotal)=sqrt(sum(e((nfull*nbunch)+1:ns)**2))

       endif

       if(nfull /= 0)then

          ! sums up nbunch bins at a time 
          do i=1,nfull
             s_new(i)=sum(s(((i-1)*nbunch)+1:(i*nbunch)))
             e_new(i)=sqrt(sum(e(((i-1)*nbunch)+1:(i*nbunch))**2))
          end do
       endif
    endif

  end subroutine IXFrebunch_hist


  !*************************************************************************************************
  !*************************************************************************************************
  !*************************************************************************************************

  !! this routine takes a point data set and groups together nbunch data points
  !! a weighted average of the associated x data is used for the output x data
  !! an average of the signal is also taken - with the assumption the routine would be used for smoothing data
  subroutine IXFrebunch_points(nbunch,ntotal,s_in,x_in,e_in,s_out,x_out,e_out)

    implicit none
    integer(i4b),intent(in)::nbunch !! number of bins to be bunched together
    integer(i4b),intent(inout)::ntotal
    real(dp),intent(in)::s_in(:) !! input signal array
    real(dp),intent(in),optional::x_in(:)!! input x array
    real(dp),intent(in),optional::e_in(:)!!input error array
    real(dp),intent(out),optional::s_out(:)!!output signal array
    real(dp),intent(out),optional::x_out(:) !!output x array
    real(dp),intent(out),optional::e_out(:) !!output error array

    integer(i4b)::ns,i,nfull
    real(dp)::rem,factor

    ns = size(s_in)

    if(ntotal==0)then
       ntotal=((ns-1)/nbunch)+1 !total no. of bins in rebunched array
       return
    endif

    nfull=ns/nbunch

    factor= dble(nbunch)

    do i=1,nfull
       s_out(i)=sum(s_in(((i-1)*nbunch)+1:(i*nbunch)))          / factor
       x_out(i)=sum(x_in(((i-1)*nbunch)+1:(i*nbunch)))          / factor
       e_out(i)=sqrt(sum(e_in(((i-1)*nbunch)+1:(i*nbunch))**2)) / factor
    end do

    if((ntotal - nfull) /= 0)then
       rem=dble(mod(ns,nbunch))
       s_out(ntotal)=sum(s_in((nfull*nbunch)+1:ns))	     / rem
       x_out(ntotal)=sum(x_in((nfull*nbunch)+1:ns))          / rem
       e_out(ntotal)=sqrt(sum(e_in((nfull*nbunch)+1:ns)**2)) / rem

    endif

  end subroutine IXFrebunch_points


  !*************************************************************************************************
  !*************************************************************************************************
  !*************************************************************************************************


  subroutine IXFrebunchHistX(x,s,e,x_new,s_new,e_new,xdist,nbunch,status)
    use IXMmemory 
    implicit none
    real(dp),intent(in) :: x(:),s(:,:),e(:,:)
    real(dp),pointer:: x_new(:),s_new(:,:),e_new(:,:)
    integer(i4b),intent(in) :: nbunch
    integer(i4b) :: ntotal
    type(IXTstatus),intent(inout)::status
    logical,intent(in)::xdist

    ntotal=0

    call IXFrebunch_histX_2d(nbunch,ntotal,s)

    call IXFalloc(x_new, ntotal+1 , status)
    call IXFallocdims(s_new,(/ ntotal, size(s,2) /), status)
    call IXFallocdims(e_new,(/ ntotal, size(s,2) /), status)

    call IXFrebunch_histX_2d(nbunch,ntotal,s,x,e,x_new,s_new,e_new,xdist)

  end subroutine IXFrebunchHistX

  !*************************************************************************************************
  !*************************************************************************************************
  !*************************************************************************************************


  subroutine IXFrebunch_histX_2d(nbunch,ntotal,s,x,e,x_new,s_new,e_new,xdist)
    implicit none
    real(dp),intent(in)::s(:,:)
    real(dp),intent(in),optional :: x(:),e(:,:)
    real(dp),intent(inout),optional :: x_new(:),s_new(:,:),e_new(:,:)
    integer(i4b) :: nx, ns,nfull,i,j
    integer(i4b),intent(inout) :: ntotal
    integer(i4b),intent(in):: nbunch
    logical,intent(in),optional :: xdist

    ns= size(s,1)

    if(ntotal==0)then
       ntotal=((ns-1)/nbunch)+1 !total no. of bins in rebunched array
       return
    endif

    nfull=ns/nbunch !no.of full bins (ie made up of nbunch "bunched elements") in rebunched array
    nx=size(x)

    x_new(1:ntotal)=x(1:(nx-1):nbunch)
    x_new(ntotal+1)=x(nx)


    s_new=0.0_dp
    e_new=0.0_dp

    if(xdist)then

       ! fill up last bin in results with stray bins at the end
       if((ntotal - nfull) /= 0)then
          do i=1,size(s_new,2)
             s_new(ntotal,i)=     sum( s((nfull*nbunch)+1:ns,i)*(x((nfull*nbunch)+2:ns+1)-x((nfull*nbunch)+1:ns)))     / &
                              (x_new(ntotal+1)-x_new(ntotal))
             e_new(ntotal,i)=sqrt(sum((e((nfull*nbunch)+1:ns,i)*(x((nfull*nbunch)+2:ns+1)-x((nfull*nbunch)+1:ns)))**2))/ &
                              (x_new(ntotal+1)-x_new(ntotal))
          enddo
       endif

       if(nfull /= 0)then
          ! sums up nbunch bins at a time 
          do i=1,nfull
             do j=1,size(s_new,2)
               s_new(i,j)=sum( s(((i-1)*nbunch)+1:(i*nbunch),j)*(x(((i-1)*nbunch)+2:(i*nbunch)+1)-x(((i-1)*nbunch)+1:(i*nbunch)))) &
                        /(x_new(i+1)-x_new(i))
               e_new(i,j)=&
                sqrt(sum((e(((i-1)*nbunch)+1:(i*nbunch),j)*(x(((i-1)*nbunch)+2:(i*nbunch)+1)-x(((i-1)*nbunch)+1:(i*nbunch))))**2))/&
                          (x_new(i+1)-x_new(i))
             enddo
          end do
       endif
    else

       if((ntotal - nfull) /= 0)then

          s_new(ntotal,:)=sum(s((nfull*nbunch)+1:ns,:),1)
          e_new(ntotal,:)=sqrt(sum(e((nfull*nbunch)+1:ns,:)**2,1))

       endif

       if(nfull /= 0)then

          ! sums up nbunch bins at a time 
          do i=1,nfull
             s_new(i,:)=sum(s(((i-1)*nbunch)+1:(i*nbunch),:),1)
             e_new(i,:)=sqrt(sum(e(((i-1)*nbunch)+1:(i*nbunch),:)**2,1))
          end do
       endif
    endif

  end subroutine IXFrebunch_histX_2d

  !*************************************************************************************************
  !*************************************************************************************************
  !*************************************************************************************************


  subroutine IXFrebunchHistY(y,s,e,y_new,s_new,e_new,ydist,nbunch,status)
    use IXMmemory 
    implicit none
    real(dp),intent(in) :: y(:),s(:,:),e(:,:)
    real(dp),pointer:: y_new(:),s_new(:,:),e_new(:,:)
    integer(i4b),intent(in) :: nbunch
    integer(i4b) :: ntotal
    type(IXTstatus),intent(inout)::status
    logical,intent(in)::ydist

    ntotal=0

    call IXFrebunch_histY_2d(nbunch,ntotal,s)

    call IXFalloc(y_new, ntotal+1 , status)
    call IXFallocdims(s_new,(/ size(s,1), ntotal  /), status)
    call IXFallocdims(e_new,(/ size(s,1), ntotal  /), status)

    call IXFrebunch_histY_2d(nbunch,ntotal,s,y,e,y_new,s_new,e_new,ydist)

  end subroutine IXFrebunchHistY

  !*************************************************************************************************
  !*************************************************************************************************
  !*************************************************************************************************


  subroutine IXFrebunch_histY_2d(nbunch,ntotal,s,y,e,y_new,s_new,e_new,ydist)
    implicit none
    real(dp),intent(in)::s(:,:)
    real(dp),intent(in),optional :: y(:),e(:,:)
    real(dp),intent(inout),optional :: y_new(:),s_new(:,:),e_new(:,:)
    integer(i4b) :: ny, ns,nfull,i,j
    integer(i4b),intent(inout) :: ntotal
    integer(i4b),intent(in):: nbunch
    logical,intent(in),optional :: ydist

    ns= size(s,2)

    if(ntotal==0)then
       ntotal=((ns-1)/nbunch)+1 !total no. of bins in rebunched array
       return
    endif

    nfull=ns/nbunch !no.of full bins (ie made up of nbunch "bunched elements") in rebunched array
    ny=size(y)

    y_new(1:ntotal)=y(1:(ny-1):nbunch)
    y_new(ntotal+1)=y(ny)


    s_new=0.0_dp
    e_new=0.0_dp

    if(ydist)then

       ! fill up last bin in results with stray bins at the end
       if((ntotal - nfull) /= 0)then
          do i=1,size(s_new,1)
             s_new(i,ntotal)=     sum( s(i,(nfull*nbunch)+1:ns)*(y((nfull*nbunch)+2:ns+1)-y((nfull*nbunch)+1:ns)))     / &
                 (y_new(ntotal+1)-y_new(ntotal))
             e_new(i,ntotal)=sqrt(sum((e(i,(nfull*nbunch)+1:ns)*(y((nfull*nbunch)+2:ns+1)-y((nfull*nbunch)+1:ns)))**2))/ &
                        (y_new(ntotal+1)-y_new(ntotal))
          enddo
       endif

       if(nfull /= 0)then
          ! sums up nbunch bins at a time 
          do i=1,nfull
             do j=1,size(s_new,1)
               s_new(j,i)=sum( s(j,((i-1)*nbunch)+1:(i*nbunch))*(y(((i-1)*nbunch)+2:(i*nbunch)+1)-y(((i-1)*nbunch)+1:(i*nbunch))))&
                /(y_new(i+1)-y_new(i))
               e_new(j,i)=&
                sqrt(sum((e(j,((i-1)*nbunch)+1:(i*nbunch))*(y(((i-1)*nbunch)+2:(i*nbunch)+1)-y(((i-1)*nbunch)+1:(i*nbunch))))**2))/&
        (y_new(i+1)-y_new(i))
             enddo
          end do
       endif
    else

       if((ntotal - nfull) /= 0)then

          s_new(:,ntotal)=sum(s(:,(nfull*nbunch)+1:ns),2)
          e_new(:,ntotal)=sqrt(sum(e(:,(nfull*nbunch)+1:ns)**2,2))

       endif

       if(nfull /= 0)then

          ! sums up nbunch bins at a time 
          do i=1,nfull
             s_new(:,i)=sum(s(:,((i-1)*nbunch)+1:(i*nbunch)),2)
             e_new(:,i)=sqrt(sum(e(:,((i-1)*nbunch)+1:(i*nbunch))**2,2))
          end do
       endif
    endif

  end subroutine IXFrebunch_histY_2d

  !*************************************************************************************************
  !*************************************************************************************************
  !*************************************************************************************************


  subroutine IXFrebunchXY(x,y,s,e,x_new,y_new,s_new,e_new,xdist,ydist,xhist,yhist,Xbunch,Ybunch,status)
    use IXMmemory 
    implicit none
    real(dp),intent(in) :: x(:),y(:),s(:,:),e(:,:)
    real(dp),pointer:: x_new(:),y_new(:),s_new(:,:),e_new(:,:)
    integer(i4b),intent(in) :: Xbunch,Ybunch
    integer(i4b) :: ntotal
    type(IXTstatus),intent(inout)::status
    logical,intent(in)::ydist,xdist,xhist,yhist
    real(dp),allocatable::stemp(:,:),etemp(:,:)

    ! run X rebunching part first with output to temporary array
    ntotal=0
    !this call is the same irrespective of histogtam or point data, just to find ntotal
    call IXFrebunch_histX_2d(Xbunch,ntotal,s)
    !******************

	if(xhist)then
	    call IXFalloc(x_new, ntotal+1 , status)
	else
		call IXFalloc(x_new,ntotal,status)
	endif

	allocate(stemp(ntotal,size(s,2)))
	allocate(etemp(ntotal,size(s,2)))


    if(xhist)then
       call IXFrebunch_histX_2d(Xbunch,ntotal,s,x,e,x_new,stemp,etemp,xdist)
    else 
       call IXFrebunch_pointsX(Xbunch,ntotal,s,x,e,stemp,x_new,etemp)
    endif

    ! rebunch Y part with source from temporary array

    ntotal=0

    !this call is the same irrespective of histogtam or point data, just to find ntotal
    call IXFrebunch_histY_2d(Ybunch,ntotal,stemp)
    !******************
	
	if (yhist)then
		call IXFalloc(y_new, ntotal+1 , status)
	else
		call IXFalloc(y_new, ntotal , status)
	endif

    call IXFallocdims(s_new,(/  size(stemp,1) ,ntotal /), status)
    call IXFallocdims(e_new,(/  size(stemp,1) ,ntotal /), status)
    if(yhist)then
       call IXFrebunch_histY_2d(Ybunch,ntotal,stemp,y,etemp,y_new,s_new,e_new,ydist)
    else
       call IXFrebunch_pointsY(Ybunch,ntotal,stemp,y,etemp,s_new,y_new,e_new)
    endif

    deallocate(stemp)
    deallocate(etemp)


  end subroutine IXFrebunchXY

  !*************************************************************************************************
  !*************************************************************************************************
  !*************************************************************************************************

  subroutine IXFrebunchPointsX(x,s,e,x_new,s_new,e_new,nbunch,status)
    use IXMmemory
    implicit none
    real(dp),intent(in) :: x(:),s(:,:),e(:,:)
    real(dp),pointer:: x_new(:),s_new(:,:),e_new(:,:)
    integer(i4b),intent(in) :: nbunch
    integer(i4b) :: ntotal
    type(IXTstatus),intent(inout)::status

    ntotal=0
    call IXFrebunch_pointsX(nbunch,ntotal,s)

    call IXFalloc(x_new,ntotal, status)
    call IXFallocdims(s_new,(/ ntotal, size(s,2) /), status)
    call IXFallocdims(e_new,(/ ntotal, size(s,2) /), status)

    call IXFrebunch_pointsX(nbunch,ntotal,s,x,e,s_new,x_new,e_new)

  end subroutine IXFrebunchPointsX

  !*************************************************************************************************
  !*************************************************************************************************
  !*************************************************************************************************

  subroutine IXFrebunchPointsY(y,s,e,y_new,s_new,e_new,nbunch,status)
    use IXMmemory
    implicit none
    real(dp),intent(in) :: y(:),s(:,:),e(:,:)
    real(dp),pointer:: y_new(:),s_new(:,:),e_new(:,:)
    integer(i4b),intent(in) :: nbunch
    integer(i4b) :: ntotal
    type(IXTstatus),intent(inout)::status

    ntotal=0
    call IXFrebunch_pointsY(nbunch,ntotal,s)

    call IXFalloc(y_new,ntotal, status)
    call IXFallocdims(s_new,(/  size(s,1), ntotal /), status)
    call IXFallocdims(e_new,(/  size(s,1), ntotal /), status)

    call IXFrebunch_pointsY(nbunch,ntotal,s,y,e,s_new,y_new,e_new)

  end subroutine IXFrebunchPointsY

  !*************************************************************************************************
  !*************************************************************************************************
  !*************************************************************************************************

  !! this routine takes a point data set and groups together nbunch data points
  !! a weighted average of the associated x data is used for the output x data
  !! an average of the signal is also taken - with the assumption the routine would be used for smoothing data
  subroutine IXFrebunch_pointsX(nbunch,ntotal,s_in,x_in,e_in,s_out,x_out,e_out)

    implicit none
    integer(i4b),intent(in)::nbunch !! number of bins to be bunched together
    integer(i4b),intent(inout)::ntotal
    real(dp),intent(in)::s_in(:,:) !! input signal array
    real(dp),intent(in),optional::x_in(:)!! input x array
    real(dp),intent(in),optional::e_in(:,:)!!input error array
    real(dp),intent(out),optional::s_out(:,:)!!output signal array
    real(dp),intent(out),optional::x_out(:) !!output x array
    real(dp),intent(out),optional::e_out(:,:) !!output error array

    integer(i4b)::ns,i,nfull
    real(dp)::rem,factor

    ns = size(s_in,1)

    if(ntotal==0)then
       ntotal=((ns-1)/nbunch)+1 !total no. of bins in rebunched array
       return
    endif

    nfull=ns/nbunch

    factor= dble(nbunch)

    do i=1,nfull
       s_out(i,:)=sum(s_in(((i-1)*nbunch)+1:(i*nbunch),:),1)          / factor
       x_out(i)=sum(x_in(((i-1)*nbunch)+1:(i*nbunch)))          / factor
       e_out(i,:)=sqrt(sum(e_in(((i-1)*nbunch)+1:(i*nbunch),:)**2,1)) / factor
    end do

    if((ntotal - nfull) /= 0)then
       rem=dble(mod(ns,nbunch))
       s_out(ntotal,:)=sum(s_in((nfull*nbunch)+1:ns,:),1)	         / rem
       x_out(ntotal)=sum(x_in((nfull*nbunch)+1:ns))          / rem
       e_out(ntotal,:)=sqrt(sum(e_in((nfull*nbunch)+1:ns,:)**2,1)) / rem

    endif

  end subroutine IXFrebunch_pointsX

  !*************************************************************************************************
  !*************************************************************************************************
  !*************************************************************************************************

  !! this routine takes a point data set and groups together nbunch data points
  !! a weighted average of the associated x data is used for the output x data
  !! an average of the signal is also taken - with the assumption the routine would be used for smoothing data
  subroutine IXFrebunch_pointsY(nbunch,ntotal,s_in,y_in,e_in,s_out,y_out,e_out)

    implicit none
    integer(i4b),intent(in)::nbunch !! number of bins to be bunched together
    integer(i4b),intent(inout)::ntotal
    real(dp),intent(in)::s_in(:,:) !! input signal array
    real(dp),intent(in),optional::y_in(:)!! input x array
    real(dp),intent(in),optional::e_in(:,:)!!input error array
    real(dp),intent(out),optional::s_out(:,:)!!output signal array
    real(dp),intent(out),optional::y_out(:) !!output x array
    real(dp),intent(out),optional::e_out(:,:) !!output error array

    integer(i4b)::ns,i,nfull
    real(dp)::rem,factor

    ns = size(s_in,2)

    if(ntotal==0)then
       ntotal=((ns-1)/nbunch)+1 !total no. of bins in rebunched array
       return
    endif

    nfull=ns/nbunch

    factor= dble(nbunch)

    do i=1,nfull
       s_out(:,i)=sum(s_in(:,((i-1)*nbunch)+1:(i*nbunch)),2)          / factor
       y_out(i)=sum(y_in(((i-1)*nbunch)+1:(i*nbunch)))          / factor
       e_out(:,i)=sqrt(sum(e_in(:,((i-1)*nbunch)+1:(i*nbunch))**2,2)) / factor
    end do

    if((ntotal - nfull) /= 0)then
       rem=dble(mod(ns,nbunch))
       s_out(:,ntotal)=sum(s_in(:,(nfull*nbunch)+1:ns),2)	         / rem
       y_out(ntotal)=sum(y_in((nfull*nbunch)+1:ns))          / rem
       e_out(:,ntotal)=sqrt(sum(e_in(:,(nfull*nbunch)+1:ns)**2,2)) / rem

    endif

  end subroutine IXFrebunch_pointsY




end module IXMrebunch

