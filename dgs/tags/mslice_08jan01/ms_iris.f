C==============================================================================   
C     >> mex ms_iris.f
C     This is a MEX-file for MATLAB   
C     to calculate the resolution convoluted cross-section for the IRIS spectrometer  
C     20-Sept-1999 compiled for PC/WINNT using Matlab 5.3 and 
C     Digital Visual Fortran Professional Edition 6.0
C==============================================================================	
      subroutine mexFunction(nlhs, plhs, nrhs, prhs)
C------------------------------------------------------------------------------
      implicit NONE
      integer plhs(*), prhs(*), nrhs, nlhs
C declare local variables to deal with pointers to variables passed by/to MATLAB
C declare calling functions
      integer mxCreateFull, mxGetM, mxGetN, mxIsNumeric, mxGetPr
C declare local operating variables of the interface funnction
      integer npixels_total, npixels_total_max, nbins, nbins_max
      parameter (npixels_total_max=1e5, nbins_max=1e3)
      double precision p(15),fit_pars(2), spec_pars(5), 
     c npixels(nbins_max), pixels(3,npixels_total_max), 
     c efixed, psi_samp, ar(3), br(3), cr(3), ub(3,3), 
     c y(nbins_max), error(nbins_max)

C === Check for proper number of MATLAB input and output arguments 
      if (nrhs .ne. 11) then
         call mexErrMsgTxt('11 inputs required.')
      else if (nlhs .ne. 2) then
         call mexErrMsgTxt
     c        ('Two outputs (y,error) required.')
      end if

C === Check to see if all inputs are numeric
      if (mxIsNumeric(prhs(1)) .ne. 1) then
         call mexErrMsgTxt('Input # 1 is not a numeric array.')
      else if (mxIsNumeric(prhs(2)) .ne. 1) then
         call mexErrMsgTxt('Input #2 is not a numeric array.')
      else if (mxIsNumeric(prhs(3)) .ne. 1) then
         call mexErrMsgTxt('Input #3 is not a numeric array.')      
      else if (mxIsNumeric(prhs(4)) .ne. 1) then
         call mexErrMsgTxt('Input #4 is not a numeric array.')      
      else if (mxIsNumeric(prhs(5)) .ne. 1) then
         call mexErrMsgTxt('Input #5 is not a numeric array.')      
      else if (mxIsNumeric(prhs(6)) .ne. 1) then
         call mexErrMsgTxt('Input #6 is not a numeric array.')      
      else if (mxIsNumeric(prhs(7)) .ne. 1) then
         call mexErrMsgTxt('Input #7 is not a numeric array.')      
      else if (mxIsNumeric(prhs(8)) .ne. 1) then
         call mexErrMsgTxt('Input #8 is not a numeric array.')      
      else if (mxIsNumeric(prhs(9)) .ne. 1) then
         call mexErrMsgTxt('Input #9 is not a numeric array.')      
      else if (mxIsNumeric(prhs(10)) .ne. 1) then
         call mexErrMsgTxt('Input #10 is not a numeric array.')
      else if (mxIsNumeric(prhs(11)) .ne. 1) then
         call mexErrMsgTxt('Input #11 is not a numeric array.')
      end if
      
C === Check sizes of input arguments 
C === par 1 = p(1,15) Mfit fitting parameters
C === variable (M,N)
C === mxgetM = number or rows
C === mxgetN = number of columns
      if((mxGetM(prhs(1)) .ne. 1).or.(mxGetN(prhs(1)) .ne. 15) ) then
         call mexErrMsgTxt('Input #1 p should be a (1,15) vector')
      end if
C === par 2 = fit_pars(1,2) [nmc icross]
      if((mxGetM(prhs(2)) .ne. 1).or.(mxGetN(prhs(2)) .ne. 2) ) then
         call mexErrMsgTxt('Input #2 fit_pars should be a (1,2) vector')
      end if
C === par 3 = spec_pars(1,5) [th_h th_v psi_h psi_v dt]
      if((mxGetM(prhs(3)) .ne. 1).or.(mxGetN(prhs(3)) .ne. 5) ) then
        call mexErrMsgTxt('Input #3 spec_pars should be a (1,5) vector')
      end if
C === par 4 = npixels(1,nbins)
      if(mxGetM(prhs(4)) .ne. 1) then
        call mexErrMsgTxt('Input #4 npixels should be (1,nbins) vector')
      end if
      nbins=mxGetN(prhs(4))
      if (nbins .gt. nbins_max) then
	 call mexErrMsgTxt
     c  ('No of bins exceeds nbins_max. Recompile code.')
      end if
C === par 5 = pixels(3,npixels_total) (energy, energy_bin, twotheta)
      if(mxGetM(prhs(5)) .ne. 3) then
         call mexErrMsgTxt('Input #5 pixels should be (3,npix) vector')
      end if
      npixels_total=mxGetN(prhs(5))
      if (npixels_total .gt. npixels_total_max) then
	 call mexErrMsgTxt
     c  ('No of pixels exceeds npixels_total_max. Recompile code.')
      end if
C === par 6 = efixed(1,1)
      if((mxGetM(prhs(6)) .ne. 1).or.(mxGetN(prhs(6)) .ne. 1) ) then
         call mexErrMsgTxt('Input #6 efixed should be a scalar')
      end if
C === par 7 = psi_samp(1,1)
      if((mxGetM(prhs(7)) .ne. 1).or.(mxGetN(prhs(7)) .ne. 1) ) then
         call mexErrMsgTxt('Input #7 psi_samp should be a scalar')
      end if
C === par 8 = ar(1,3) lattice parameters as a (1,3) vector
      if((mxGetM(prhs(8)) .ne. 1).or.(mxGetN(prhs(8)) .ne. 3) ) then
         call mexErrMsgTxt('Input #8 ar should be a (1,3) vector')
      end if
C === par 9 = br(1,3)
      if((mxGetM(prhs(9)) .ne. 1).or.(mxGetN(prhs(9)) .ne. 3) ) then
         call mexErrMsgTxt('Input #9 br should be a (1,3) vector')
      end if
C === par 10 = cr(1,3)
      if((mxGetM(prhs(10)) .ne. 1).or.(mxGetN(prhs(10)) .ne. 3) ) then
         call mexErrMsgTxt('Input #10 ar should be a (1,3) vector')
      end if
C === par 11 = ub(3,3)
      if((mxGetM(prhs(11)) .ne. 3).or.(mxGetN(prhs(11)) .ne. 3) ) then
         call mexErrMsgTxt('Input #11 ub should be a (3,3) matrix')
      end if

C === Get parameter values and pixel information
      ! fitting parameters p(1,15) real*8
      call mxCopyPtrToReal8(mxGetPr(prhs(1)),p,15)      
      ! fit parameters [nmc icross] real numbers
      call mxCopyPtrToReal8(mxGetPr(prhs(2)),fit_pars,2)
      ! spectrometer parameters [th_h th_v psi_h psi_v dt] real numbers
      call mxCopyPtrToReal8(mxGetPr(prhs(3)),spec_pars,5)
      ! npixels (1,nbins) real numbers
      call mxCopyPtrToReal8(mxGetPr(prhs(4)),npixels,nbins)
      ! pixels (3,npixels_total) real numbers
      call mxCopyPtrToReal8(mxGetPr(prhs(5)),pixels,npixels_total*3)      
      ! efixed real number
      call mxCopyPtrToReal8(mxGetPr(prhs(6)),efixed,1)  
      ! psi_samp real number
      call mxCopyPtrToReal8(mxGetPr(prhs(7)),psi_samp,1) 
      ! ar [x,y,z] (Angs^-1)
      call mxCopyPtrToReal8(mxGetPr(prhs(8)),ar,3)
      call mxCopyPtrToReal8(mxGetPr(prhs(9)),br,3)
      call mxCopyPtrToReal8(mxGetPr(prhs(10)),cr,3)
      ! ub (3,3) ub matrix
      call mxCopyPtrToReal8(mxGetPr(prhs(11)),ub,3*3)

C === Call computational subroutine ms_iris
      call ms_iris(y,error,p,fit_pars,spec_pars,npixels,nbins,
     c pixels,npixels_total,efixed,psi_samp,ar,br,cr,ub)

C === Create matrices for the return arguments
      plhs(1)=mxCreateFull(nbins,1,0)
      plhs(2)=mxCreateFull(nbins,1,0)

C === Load the output into two MATLAB arrays
      call mxCopyReal8ToPtr(y,mxGetPr(plhs(1)),nbins)
      call mxCopyReal8ToPtr(error,mxGetPr(plhs(2)),nbins)
      return
      end

C =================================================================================
C FORTRAN code to do the average the cross-section over the MC distribution of 
C points over the resolution volume and bin the results along a one-dimensional cut
C ================================================================================= 
      subroutine ms_iris(y,error,p,fit_pars,spec_pars,npixels,nbins,
     c pixels,npixels_total,efixed,psi_samp,ar,br,cr,ub)
C declare input and output variables
      implicit NONE
C     npixels_total     ! actual total number of pixels
C     nbins             ! actual total number of bins
      integer npixels_total, nbins, i, j, bin, pixels_in_bin, 
     c nmc, icross, seed
      double precision  y(nbins), error(nbins), p(15), 
     c spec_pars(5), pixels(3,npixels_total), efixed,
     c psi_samp, ar(3), br(3), cr(3), ub(3,3),
     c th_h, th_v, psi_h, psi_v, dt, vo, factor, L1, 
     c kix, kiy, kiz, kfx, kfy, kfz, Qx, Qy, Qz, th, thx, thy,  
     c energy_centre, de, en, ki, kf, fit_pars(2), npixels(nbins),
     c ave_cs, err_cs
      double precision randn, ms_sqw
      external randn, ms_sqw
      parameter (factor=2.355d0) ! conversion factor gaussian fwhm/variance 
      parameter (L1=36.54d0)	 ! incident flight path [m]  
!      seed=1234567890   !  random large number as a seed for the MC generator
      seed=int(secnds(0.0e0)*100.0e0)  ! random large number as a seed for the MC generator
!       different seed every time the ms_iris function is called
!       SECNDS (real-number) from VMS $ help fortran intrinsic secnds
!       A function that returns the number of seconds since midnight minus
!       the value of the argument.  The argument must be a REAL*4 data
!       type.  The return value is a REAL*4 data type.  The time returned
!       is accurate to .01 seconds.
      nmc=int(fit_pars(1))! number of monto-carlo points (integer)
      icross=int(fit_pars(2)) !  cross-section number (integer)

C === extract spectrometer parameters
      th_h=spec_pars(1)/factor	 ! horizontal [fwhm -> variance] [radians] for the incident beam 
      th_v=spec_pars(2)/factor  ! vertical [fwhm -> variance][ radians] for the incident beam 
      psi_h=spec_pars(3)/factor ! horizontal [fwhm -> variance][ radians] for the scattered beam 
      psi_v=spec_pars(4)/factor ! vertical [fwhm -> variance][radians] for the scattered beam 
      dt=spec_pars(5)           ! time resolution fwhm [s]
      vo=sqrt(efixed/5.227d0)*1.0d3 ! velocity of analysed neutrons [m/s]
 
      kix=0.0d0	! will contain the x-component of ki for one mc event for one pixel [Angs^-1]
      kiy=kix
      kiz=kix
      kfx=kix
      kfy=kix
      kfz=kix
      Qx=kix
      Qy=kiy
      Qz=kiz
      thx=kix	! will contain a mone-carlo distribution of mc angles in the horizontal plane 
      thy=kiy
      th=thx
      bin=1	        ! will index the bins
      pixels_in_bin=0	! will count partial number of pixels in each bin
      ! initialize intensity and error arrays
      y(bin)=0.0d0
      error(bin)=0.0d0

C === simulate scattering for each pixel
      do i=1,npixels_total 
         ave_cs=0.0d0   ! average of the monte-carlo dsitribution of intensities per each pixel
         err_cs=0.0d0   ! variance of the monte-carlo distribution of intensities per each pixel
         do j=1,nmc
C === generate NMC monte carlo scattering events, select a final energy froim the finite energy bin
C ===       energy(i)=pixels(1,i)     !  energy value (meV) of each pixel
C ===       energy_bin(i)=pixels(2,i) !  energy bin (meV) per pixel
            energy_centre=pixels(1,i)+(ran(seed)-0.5d0)*pixels(2,i)	! [meV]
C === calculate energy resulution fwhm [meV] for this energy
C === (~ constant over the small energy_bin(i) length) 
            de=dt*2.0d0*vo/L1*sqrt((energy_centre+efixed)**3/efixed) ! [meV]
C === generate a scattering event (normal distribution) centred around the nominal energy value and fwhm de
            en=energy_centre+(de/factor)*randn(seed)         ! [meV]
C === calculate ki and kf for this scattering event
            ki=sqrt((efixed+en)/2.072d0) !	[Angs^-1]
            kf=sqrt(efixed/2.072d0)	 !      [Angs^-1]
C === generate an angular orientation for ki
            thx=th_h*randn(seed)
            thy=th_v*randn(seed)
            th=atan(sqrt(tan(thx)**2+tan(thy)**2))
            kiy=ki*tan(thx)*cos(th)
            kiz=ki*tan(thy)*cos(th)
            kix=ki*cos(th)
C === generate an angular orientation for kf
            thx=psi_h*randn(seed)
            thy=psi_v*randn(seed)
            th=atan(sqrt(tan(thx)**2+tan(thy)**2))
            Qy=kf*tan(thx)*cos(th)
            Qz=kf*tan(thy)*cos(th)
            Qx=kf*cos(th)
C === rotate kf in spectrometer reference frame
C ===       theta(i)=pixels(3,i)    ! total scattering angle  (radians) for each pixel
            kfx=Qx*cos(pixels(3,i))-Qy*sin(pixels(3,i))
            kfy=Qx*sin(pixels(3,i))+Qy*cos(pixels(3,i))
            kfz=Qz
C === calculate wavevector transfer in spectrometer reference frame
            Qx=kix-kfx
            Qy=kiy-kfy
            Qz=kiz-kfz
C === transform wavevctor transfer in sample reference frame
            kfx=Qx*cos(psi_samp)+Qy*sin(psi_samp)
            kfy=-Qx*sin(psi_samp)+Qy*cos(psi_samp)
            kfz=Qz
C === transform in sample reference frame wavevector Q into hkl using ub matrix
            Qx=kfx*ub(1,1)+kfy*ub(1,2)+kfz*ub(1,3)
            Qy=kfx*ub(2,1)+kfy*ub(2,2)+kfz*ub(2,3)
            Qz=kfx*ub(3,1)+kfy*ub(3,2)+kfz*ub(3,3)
C === calculate cross-section for this scattering event
C === and store result in a temporary variable Qx 
            Qx=ms_sqw(icross,p,Qx,Qy,Qz,en,pixels(3,i),ar,br,cr) 
            ave_cs=ave_cs+Qx
            err_cs=err_cs+Qx**2
         end do ! j=1,nmc
         ave_cs=ave_cs/dfloat(nmc)    ! =  <cs>
         err_cs=err_cs/dfloat(nmc)    ! = <cs^2>
         if (nmc .gt. 1) then 
            err_cs=sqrt(abs(err_cs-ave_cs**2))*
     c           sqrt(1.0d0/(dfloat(nmc)-1.0d0))
         else
            err_cs=0.0d0
         end if
C === add pixel to current bin
         pixels_in_bin=pixels_in_bin+1   
         y(bin)=y(bin)+ave_cs
         error(bin)=error(bin)+err_cs**2
C === calculate average per bin and move on to the next bin 
C === if maximum number of pixels in that bin was reached by this latest pixel
         if (pixels_in_bin .eq. int(npixels(bin))) then      
            y(bin)=y(bin)/dfloat(pixels_in_bin)
            error(bin)=sqrt(error(bin))/dfloat(pixels_in_bin)
            if (bin .lt. nbins) then
               bin=bin+1
               pixels_in_bin=0
               y(bin)=0.0d0
               error(bin)=0.0d0
            end if
         end if   
      end do ! i=1,npixels_total
      return
      end

C ===============================================================================
C Fortran code to generate a random number with a normal distribution 
C from b. Jansson, Random Number Generators, p177
C ===============================================================================
      double precision function randn(seed)
      implicit NONE
      integer seed
      randn=sqrt(-2.0d0*log(ran(seed)))*cos(8.0d0*atan(1.0d0)*ran(seed))
      return
      end

C ===============================================================================
C Fortran code with cycloidal dispersion relation appropriate for Cs2CuCl4
C ===============================================================================
      subroutine cs2cucl4(A,B,C,D,h,k,l,J,Jp)
      implicit NONE
      double precision A,B,C,D,h,k,l,J,Jp,q,pi,
     c gam, gamp, gamp_kpq, gamp_kmq, theta, spin
      pi=4.0d0*atan(1.0d0)
      q=acos(Jp/(2.0d0*J))/pi  ! cycloidal ordering wavevector
      gam=cos(2.0d0*pi*k)
      gamp=cos(pi*k)*cos(pi*l)
      gamp_kmq=cos(pi*(k-q))*cos(pi*l)
      gamp_kpq=cos(pi*(k+q))*cos(pi*l)
      theta=2.0d0*pi*q
      spin=1/2.0d0
      A=-spin*J*(2.0d0*cos(theta)-(cos(theta)+1.0d0)*gam)+
     c 4.0d0*spin*Jp*cos(theta/2.0d0)
      B=2.0d0*spin*Jp*(gamp-(gamp_kpq+gamp_kmq)/2.0d0)
      C=spin*J*(cos(theta)-1.0d0)*gam
      D=-2.0d0*spin*Jp*(gamp+(gamp_kpq+gamp_kmq)/2.0d0)
      return
      end

C ===============================================================================
C Fortran code with fourier transforms of exchange interaction for an anisotropic 
C triangular lattice relevant to Cs2CuCl4 
C ===============================================================================
      double precision function jj(J,Jp,qk,ql)
      implicit NONE
      double precision J, Jp, qk, ql, pi
      pi=4.0d0*atan(1.0d0)                   
      jj=J*cos(2.0d0*pi*qk)+2*Jp*cos(pi*qk)*cos(pi*ql)
      return
      end

C ===============================================================================
! Calculates dispersion relation wq and (u,v) parameters for a 2d S=1/2 triangular 
! lattice Heisenberg antiferromagnet with J and zig-zag Jp couplings (>0 if AF) 
! at the (qk,ql) reciprocal lattice point in the 2d plane (b,c) relevant for Cs2CuCl4
C ===============================================================================
      subroutine uv_2dtr(u,v,wq,J,Jp,qk,ql)
      implicit NONE
      double precision u, v, wq, J, Jp, qk, ql, z, atanh,   
     c       pi, Q, a, b, Ak, Bk, thk, jj
      external jj 
      pi=4.0d0*atan(1.0d0)       
      if ((2.0d0*J) .gt. Jp) then
	 Q=1.0d0-1.0d0/pi*acos(Jp/2.0d0/J)
      else
         Q=1.0d0
      end if         
      a=-jj(J,Jp,Q,0.0d0)+jj(J,Jp,qk,ql)
      b=(jj(J,Jp,Q+qk,ql)+jj(J,Jp,Q-qk,-ql))/2.0d0-
     c   jj(J,Jp,Q,0.0d0) 
      Ak=(a+b)/2.0d0
      Bk=(a-b)/2.0d0
      wq=sqrt(abs(Ak**2-Bk**2))
      z=Bk/Ak
      atanh=log((1+z)*sqrt(1/(1-z**2))) ! atanh(z) analytical formula
      thk=atanh/2.0d0
      u=cosh(thk)
      v=sinh(thk) 
      return
      end       

C ===============================================================================
C Fortran code equivalent to the floor function in Matlab 
C ===============================================================================
      integer function floor(x)
      implicit NONE
      double precision x
      if (x .eq. dfloat(int(x))) then
         floor=int(x)
      elseif (x .gt. 0.0d0) then
         floor=int(x)
      else
         floor=int(x)-1
      end if
      return
      end

C ===============================================================================
C Fortran code equivalent to the ceil function in Matlab 
C ===============================================================================
      integer function ceil(x)
      implicit NONE
      double precision x
      if (x .eq. dfloat(int(x))) then
         ceil=int(x)
      elseif (x .gt. 0.0d0) then
         ceil=int(x)+1
      else
         ceil=int(x)
      end if
      return
      end

C ===============================================================================
C Fortran code to reduce a number x to the interval [x1,x2)
C ===============================================================================
      double precision function reduce(x,x1,x2)
      implicit NONE
      double precision x, x1, x2
      integer floor
      external floor
      reduce=x-(x2-x1)*dfloat(floor((x-x1)/(x2-x1)))
      return
      end

C ===============================================================================
C Fortran code to return global index in a table, 0 means out of table range 
C ===============================================================================
      integer function index(x,y,z,t)
      implicit NONE
      double precision x, y, z, t(12), reduce
      integer ix, iy, iz, nx, ny, nz
      external reduce
!      index=0
      nx=int(t(4)) 
      ny=int(t(8))
      nz=int(t(12))
      ! === reduce any (x,y) to the first Brillouin zone by periodicity
      ix=min(int((reduce(x,t(1),t(2))-t(1))/t(3))+1,nx)   
      iy=min(int((reduce(y,t(5),t(6))-t(5))/t(7))+1,ny) 
      ! === if outside the energy range then give default index=0 and translate into S=0           
      if ((z .lt. t(9)) .or. (z .gt. t(10))) then 
         index=0
      else
         iz=min(int((z-t(9))/t(11))+1,nz) 
         ! === calculate global 1d index into the 3d (x,y,z) table
         ix=12+iy+ny*(ix-1)+ny*nx*(iz-1)  
         if (ix .gt. (nx*ny*nz+12)) then
            call mexErrMsgTxt('Global index too large.')
         end if
         index=ix
      end if
      return
      end


C ===============================================================================
C Fortran code to calculate the complex gamma function 
C ===============================================================================
C     ALGORITHM 404 COLLECTED ALGORITHMS FROM ACM.
C     ALGORITHM APPEARED IN COMM. ACM, VOL. 14, NO. 01,
C     P. 048.
C     downloaded from http://www.netlib.org/toms/ 05-Nov-1999  
      double complex FUNCTION CGAMMA(Z)
      implicit NONE
      double COMPLEX Z,ZM,T,TT,SUM,TERM,DEN,PI,A
      double precision C(12),TOL,XDIST,X,Y
      LOGICAL REFLEK
      INTEGER IOUT,I,J,M
C SET IOUT FOR PROPER OUTPUT CHANNEL OF COMPUTER SYSTEM FOR
C ERROR MESSAGES
      IOUT = 3
      PI = DCMPLX(2.0d0*dacos(0.0d0),0.0d0)
      X = REAL(Z)
      Y = DIMAG(Z)
C TOL = LIMIT OF PRECISION OF COMPUTER SYSTEM IN SINGLE PRECISI
      TOL = 1.0D-7
      REFLEK = .TRUE.
C DETERMINE WHETHER Z IS TOO CLOSE TO A POLE
C CHECK WHETHER TOO CLOSE TO ORIGIN
      IF(X.GE.TOL) GO TO 20
C FIND THE NEAREST POLE AND COMPUTE DISTANCE TO IT
      XDIST = X-INT(X-0.5d0)
      ZM = DCMPLX(XDIST,Y)
      IF(CDABS(ZM).GE.TOL) GO TO 10
C IF Z IS TOO CLOSE TO A POLE, PRINT ERROR MESSAGE AND RETURN
C WITH CGAMMA = (1.E7,0.0E0)
      WRITE(IOUT,900) Z
      CGAMMA = (1.0D7,0.0D0)
      RETURN
C FOR REAL(Z) NEGATIVE EMPLOY THE REFLECTION FORMULA
C GAMMA(Z) = PI/(SIN(PI*Z)*GAMMA(1-Z))
C AND COMPUTE GAMMA(1-Z).  NOTE REFLEK IS A TAG TO INDICATE THA
C THIS RELATION MUST BE USED LATER.
10    IF(X.GE.0.0d0) GO TO 20
      REFLEK = .FALSE.
      Z = (1.0d0,0.0d0)-Z
      X = 1.0d0-X
      Y = -Y
C IF Z IS NOT TOO CLOSE TO A POLE, MAKE REAL(Z)>10 AND ARG(Z)<P
20    M = 0
40    IF(X.GE.10.0d0) GO TO 50
      X = X + 1.0d0
      M = M + 1
      GO TO 40
50    IF(ABS(Y).LT.X) GO TO 60
      X = X + 1.0d0
      M = M + 1
      GO TO 50
60    T = DCMPLX(X,Y)
      TT = T*T
      DEN = T
C COEFFICIENTS IN STIRLING*S APPROXIMATION FOR LN(GAMMA(T))
      C(1) = 1.0d0/12.0d0
      C(2) = -1.0d0/360.0d0
      C(3) = 1.0d0/1260.0d0
      C(4) = -1.0d0/1680.0d0
      C(5) = 1.0d0/1188.0d0
      C(6) = -691.0d0/360360.0d0
      C(7) = 1.0d0/156.0d0
      C(8) = -3617.0d0/122400.0d0
      C(9) = 43867.0d0/244188.0d0
      C(10) = -174611.0d0/125400.0d0
      C(11) = 77683.0d0/5796.0d0
      SUM = (T-(0.5d0,0.0d0))*CDLOG(T)-
     c     T+DCMPLX(0.5d0*DLOG(2.0d0*3.14159265358979d0),0.0d0)
      J = 1
70    TERM = C(J)/DEN
C TEST REAL AND IMAGINARY PARTS OF LN(GAMMA(Z)) SEPARATELY FOR
C CONVERGENCE.  IF Z IS REAL SKIP IMAGINARY PART OF CHECK.
      IF(ABS(REAL(TERM)/REAL(SUM)).GE.TOL) GO TO 80
      IF(Y.EQ.0.0d0) GO TO 100
      IF(ABS(DIMAG(TERM)/DIMAG(SUM)).LT.TOL) GO TO 100
80    SUM = SUM + TERM
      J = J + 1
      DEN = DEN*TT
C TEST FOR NONCONVERGENCE
      IF(J.EQ.12) GO TO 90
      GO TO 70
C STIRLING*S SERIES DID NOT CONVERGE.  PRINT ERROR MESSAGE AND
C PROCEDE.
90    WRITE(IOUT,910) Z
C RECURSION RELATION USED TO OBTAIN LN(GAMMA(Z))
C LN(GAMMA(Z)) = LN(GAMMA(Z+M)/(Z*(Z+1)*...*(Z+M-1)))
C = LN(GAMMA(Z+M)-LN(Z)-LN(Z+1)-...-LN(Z+M
100   IF(M.EQ.0) GO TO 120
      DO 110 I = 1,M
      A = DCMPLX(I*1.0d0-1.0d0,0.0d0)
110   SUM = SUM-CDLOG(Z+A)
C CHECK TO SEE IF REFLECTION FORMULA SHOULD BE USED
120   IF(REFLEK) GO TO 130
      SUM = CDLOG(PI/CDSIN(PI*Z))-SUM
      Z = (1.0d0,0.0d0) -Z
130   CGAMMA = CDEXP(SUM)
      RETURN
900   FORMAT(1X,2E14.7,10X,49H ARGUMENT OF GAMMA FUNCTION IS TOO CLOSE
     c TO A POLE)
910   FORMAT(44H ERROR - STIRLING*S SERIES HAS NOT CONVERGED/14X,4HZ = ,
     c 2E14.7)
      END

C ===============================================================================
C Fortran code to calculate function rho(x)=G(eta-ix)/G(1-eta-ix)
C where G is the complex gamma function and eta=1/4 
C ===============================================================================
      double complex function rho(x)
      implicit NONE
      double precision x
      double complex cgamma,x1,x2
      external cgamma
      x1=DCMPLX(0.25d0,-1.0d0*x)
      x2=DCMPLX(0.75d0,-1.0d0*x)
      rho=cgamma(x1)/cgamma(x2)
      return
      end

C ===============================================================================
C Fortran code with the neutron cross-section definition ms_sqw
C ===============================================================================
      double precision function ms_sqw(icross,p,Qx,Qy,Qz,en,
     c       twotheta,ar,br,cr)
      implicit NONE
      integer icross
      double precision p(15), Qx, Qy, Qz, en, twotheta, ar(3), br(3), 
     c cr(3), A_sw, J, dE, Jp, A_inc, sig_inc, cen, spin, temp, q, 
     c pi, eps, bose, modQ, fQ, t(7), w1, w2, nu, deltas, bigq,
     c A, B, C, D, theta, wdisp, alpha, deltaq, x, y, table(12)
      !=========================================================
      ! Define variables to deal with the one magnon and two-magnon cross-section tables  
      !========================================================= 
      integer nevents_1m_in, nevents_1m_out, nevents_2m, 
     c  maxsize_table, i, i1, index 
      character*40 filename
      external index
      double precision reduce, absnorm_1m_in, absnorm_1m_out,
     c  absnorm_2m
      external reduce
      parameter (maxsize_table=500012)
      double precision table_1m_in(maxsize_table), 
     c  table_1m_out(maxsize_table), table_2m(maxsize_table)
      logical read_tables /.FALSE./

      data eps /1.0d-5/, spin /0.5d0/, t 
     c/0.0232d0,34.969d0,0.4023d0,11.564d0,0.5882d0,3.843d0,-0.0137d0/

      save table_1m_in, table_1m_out, table_2m, read_tables
!=========================================================
      double complex rho
      external rho

C === initialize variables and calculate Bose factor + form factor
         pi=4.0d0*atan(1.0d0)   
         A_sw=p(1)              !  spin-wave amplitude of gaussian lineshape of area 1  
         J=abs(p(2))            !  exchange J(meV) >0 antiferromagnetic	
         dE=p(3)/2.355d0        !  fwhm->variance of Gaussian broadening of spin-wave lineshape (meV)
         Jp=p(4)*J              !  interchain coupling Jp(meV)>0 if antiferromagnetic
         A_inc=p(5)             !  incoherent scattering amplitude, peak area normalised to 1 
         sig_inc=p(6)/2.355d0	!  fwhm -> variance of the incoherent quasielastic scattering (meV)
         cen=p(7)               !  centre of incoherent quasielastic scattering (meV)
         temp=p(8)/11.605d0     !  <temp> is temperature (K -> meV)
         q=acos(Jp/(2.0d0*J))/pi ! cycloidal ordering wavevector (rlu) along b* 0.472
         deltaq=0.5d0-q         ! departure from antiferromagnetic order 0.5-0.472=0.028
C === calculate Bose factor
         if (abs(en) .lt. eps) then ! eps = small number to avoid divergence in calculating the bose factor 
            bose=1.0d0
         else
            bose=1.0d0/(1.0d0-exp(-en/temp))
         end if
C === calculate magnetic form factor for Cu2+ ions (spherical approximation)
C === [Qx,Qy,Qz]=[h,k,l] in rlu  ar,br,cr (1,3)(Angs^-1) expressed in a rectangular coordinate system
         modQ=(Qx*ar(1)+Qy*br(1)+Qz*cr(1))**2            ! |a*|^2
         modQ=modQ+(Qx*ar(2)+Qy*br(2)+Qz*cr(2))**2     
         modQ=modQ+(Qx*ar(3)+Qy*br(3)+Qz*cr(3))**2
         modQ=sqrt(modQ)        ! |Q| in Angs^{-1}
         temp=modQ/(4.0d0*pi)   ! <temp> is sin(2theta)/lambda in Angs^{-1}
         fQ=t(1)*exp(-t(2)*temp**2)+t(3)*exp(-t(4)*temp**2)+
     c        t(5)*exp(-t(6)*temp**2)+t(7)

C === calculate polarization factor, temp is a temporary variable
         temp=sqrt(ar(1)**2+ar(2)**2+ar(3)**2)    ! |a*| in Angs^-1 
         alpha=acos(Qx*temp/modQ) ! <alpha> is angle (radians) between vec{Q} and the a* axis 
            
      if (icross .eq. 1) then 
      ! ==========================================================================
      ! icross=1 ! non-dispersive mode 
      ! dE (meV) FWHM Gaussian broadening in energy 
      ! parameters=
      ! 1 A_sw      amplitude factor
      ! 2 J (meV)   non-dispersive excitation energy 
      ! 3 fwhm_sw   (meV) excitation intrinsic linewidth
      ! 4 unused
      ! 5 A_inc     incoherent quasielastic amplitude
      ! 6 fwhm_inc  (meV) quasielastic width
      ! 7 cen (meV) quasielastic peak centre
      ! 8 T (K)     temperature to calculate Bose factor (n(w)+1)
      ! 9 Const_bkg flat background 
      ! 10 Amp_bkg amplitude in an exponetially decreasing background term A*e-(x/tau)
      ! 11 Tau_bkg decay rate of the exponentially decreasing background units of xaxis
      ! 12 unused
      ! 13 unused
      ! 14 unused
      ! 15 =1 if the exponentially decaying bkg is a function of energy, and otherwise if a function of Qy

C === calculate dispersion relation  
         temp=A_sw/(sqrt(2.0d0*pi)*dE)*exp(-(en-J-cen)**2/(2.0d0*dE**2))

      else if (icross .eq. 2) then 
      ! ==========================================================================
      ! icross=2 ! isotropic spin waves with absolute amplitudes 
      ! intrachain J(meV) and interchain Jp(meV) exchanges (>0 antiferromagnetic)
      ! dE (meV) FWHM Gaussian broadening in energy thesis page 85
      ! parameters=
      ! 1 A_sw  spin-wave amplitude factor
      ! 2 J (meV) intra-chain antiferromagnetic 
      ! 3 fwhm_sw (meV) spin-wave linewidth
      ! 4 Jp/J (adimensional) interchain coupling if =0 then simple antiferromagnetic spin waves
      ! 5 A_inc incoherent quasielastic amplitude
      ! 6 fwhm_inc (meV) quasielastic width
      ! 7 cen (meV) quasielastic peak centre
      ! 8 T (K) temperature to calculate Bose factor (n(w)+1)
      ! 9 Const_bkg flat background 
      ! 10 Amp_bkg amplitude in an exponetially decreasing background term A*e-(x/tau)
      ! 11 Tau_bkg decay rate of the exponentially decreasing background units of xaxis
      ! 12 A_sw_m amplitude factor for w(Q-q) mode
      ! 13 A_sw_p amplitude factor for w(Q+q) mode
      ! 14 unused
      ! 15 =1 if the exponentially decaying bkg is a function of energy, and otherwise if a function of Qy

C === calculate dispersion relation wQ_{xx} do not include polarisation fcator 
         call cs2cucl4(A,B,C,D,Qx,Qy,Qz,J,Jp)
         wdisp=sqrt((A+B)**2-(C+D)**2)
         temp=A_sw*spin
     c*(A+B+C+D)/(wdisp+dE/1000.0d0)
     c/(sqrt(2.0d0*pi)*dE)*exp(-(en-wdisp-cen)**2/(2.0d0*dE**2))

C === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q-q) do not include polarization factor 
         call cs2cucl4(A,B,C,D,Qx,Qy-q,Qz,J,Jp)
         wdisp=sqrt((A-B)**2-(C-D)**2)
         temp=temp+p(12)*spin
     c*(A-B-C+D)/(wdisp+dE/1000.0d0)
     c/(sqrt(2.0d0*pi)*dE)*exp(-(en-wdisp-cen)**2/(2.0d0*dE**2))
       
C === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q+q) do not include polarization factor 
         call cs2cucl4(A,B,C,D,Qx,Qy+q,Qz,J,Jp)
         wdisp=sqrt((A-B)**2-(C-D)**2)
         temp=temp+p(13)*spin
     c*(A-B-C+D)/(wdisp+dE/1000.0d0)
     c/(sqrt(2.0d0*pi)*dE)*exp(-(en-wdisp-cen)**2/(2.0d0*dE**2))

      else if (icross .eq. 3) then
      ! ==========================================================================
      ! icross=3 ! polarised spin-waves with relative amplitudes
      ! intrachain J(meV) and interchain Jp(meV) exchanges (>0 antiferromagnetic)
      ! dE (meV) FWHM Gaussian broadening in energy thesis page 85
      ! parameters=
      ! 1 A_sw 
      ! 2 J (meV)
      ! 3 fwhm_sw (meV)
      ! 4 Jp/J (adimensional)
      ! 5 A_inc 
      ! 6 fwhm_inc (meV)
      ! 7 cen (meV)
      ! 8 T (K)
      ! 9 Const_bkg 
      ! 10 Amp_bkg 
      ! 11 Tau_bkg
      ! 12 A_sw_m (relative) amplitude factor for the w(Q-q) relative to w(Q) branch =1 in cycloidal model
      ! 13 A_sw_p (relative) amplitude factor for the w(Q+q) relative to w(Q) branch =1 in cycloidal model
      ! 14 unused
      ! 15 =1 if bkg(energy), otherwise bkg(Qy)

C === calculate dispersion relation for S_{xx}	: w1(Q) and include polarization factor (sin(alpha))**2
         call cs2cucl4(A,B,C,D,Qx,Qy,Qz,J,Jp)
         wdisp=sqrt((A+B)**2-(C+D)**2)
         temp=A_sw*spin*(sin(alpha)**2)
     c*(A+B+C+D)/(wdisp+dE/1000.0d0)
     c/(sqrt(2.0d0*pi)*dE)*exp(-(en-wdisp-cen)**2/(2.0d0*dE**2))
      
C === calculate dispersion relation for S_{yy}=S_{zz} : w2(Q-q) include polarization factor (1+cos(alpha)**2)
         call cs2cucl4(A,B,C,D,Qx,Qy-q,Qz,J,Jp)
         wdisp=sqrt((A-B)**2-(C-D)**2)
         temp=temp+A_sw*p(12)*spin/4.0d0*(1.0d0+cos(alpha)**2)
     c*(A-B-C+D)/(wdisp+dE/1000.0d0)
     c/(sqrt(2.0d0*pi)*dE)*exp(-(en-wdisp-cen)**2/(2.0d0*dE**2))
       
C === calculate dispersion relation for S_{yy}=S_{zz} : w2(Q+q) include polarization factor (1+cos(alpha)**2)  
         call cs2cucl4(A,B,C,D,Qx,Qy+q,Qz,J,Jp)
         wdisp=sqrt((A-B)**2-(C-D)**2)
         temp=temp+A_sw*p(13)*spin/4.0d0*(1.0d0+cos(alpha)**2)
     c*(A-B-C+D)/(wdisp+dE/1000.0d0)
     c/(sqrt(2.0d0*pi)*dE)*exp(-(en-wdisp-cen)**2/(2.0d0*dE**2))

      else if (icross .eq. 4) then
      ! =============================================================================================
      ! icross=4 ! 3 isotropic Muller ansatz continua with absolute amplitudes and mean upper bound 
      ! lower boundaries given by cycloidal spin wave dispersions 
      ! intrachain J(meV) and interchain Jp(meV) exchanges (>0 antiferromagnetic)
      ! dE (meV) FWHM Gaussian broadening in energy thesis page 85
      ! parameters=
      ! 1 A_xx amplitude for the Q continuum in {xx} 
      ! 2 J (meV)
      ! 3 dE (meV) rounding of the low-boundary divergence
      ! 4 Jp/J (adimensional)
      ! 5 A_inc 
      ! 6 fwhm_inc (meV)
      ! 7 cen (meV)
      ! 8 T (K)
      ! 9 Const_bkg 
      ! 10 Amp_bkg 
      ! 11 Tau_bkg
      ! 12 A_sw_m (absolute) amplitude factor for (Q-q) continuum in {yy} and {zz}
      ! 13 A_sw_p (absolute) amplitude factor for (Q+q) continuum in {yy} and {zz}
      ! 14 UBoundScale =1 if upper boundary of the continuum coincides with the Muller ansatz for the mean zb energy
      ! 15 =1 if bkg(energy), otherwise bkg(Qy)

C === calculate mean zone boundary energy
         call cs2cucl4(A,B,C,D,0.0d0,0.25d0,0.0d0,J,Jp)
         w1=sqrt((A+B)**2-(C+D)**2)   ! upper zone boundary energy k=1/4 
         call cs2cucl4(A,B,C,D,0.0d0,0.75d0,0.0d0,J,Jp)
         w2=sqrt((A+B)**2-(C+D)**2)   ! lower zone boundary energy k=3/4
         w2=(w1+w2)*p(14)*abs(sin(pi*Qy))   ! upper bound of the continuum assumed the same for all modes 
      ! take as the continuum upper boundary the Muller boundary for the average zone boundary energy

C === calculate dispersion relation for S_{xx}	: w1(Q) and do not include polarization factor
         call cs2cucl4(A,B,C,D,Qx,Qy,Qz,J,Jp)
         wdisp=sqrt((A+B)**2-(C+D)**2)
         if (((en-cen) .gt. wdisp) .and. ((en-cen) .le. w2)) then 
            temp=A_sw*spin
     c/sqrt(abs((en-cen)**2-wdisp**2)+(dE*2.355d0)**2)
         else
            temp=0.0d0
         end if
      
C === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q-q) do not include polarization factor
         call cs2cucl4(A,B,C,D,Qx,Qy-q,Qz,J,Jp)
         wdisp=sqrt((A-B)**2-(C-D)**2)
         if (((en-cen) .gt. wdisp) .and. ((en-cen) .le. w2)) then 
            temp=temp+
     c+p(12)*spin/sqrt(abs((en-cen)**2-wdisp**2)+(dE*2.355d0)**2)
         end if
       
C === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q+q) do not include polarization factor  
         call cs2cucl4(A,B,C,D,Qx,Qy+q,Qz,J,Jp)
         wdisp=sqrt((A-B)**2-(C-D)**2)
         if (((en-cen) .gt. wdisp) .and. ((en-cen) .le. w2)) then 
            temp=temp
     c+p(13)*spin/sqrt(abs((en-cen)**2-wdisp**2)+(dE*2.355d0)**2)
         end if

      else if (icross .eq. 5) then
      ! ============================================================================================
      ! icross=5 ! 3 polarised Muller ansatz continua with relative amplitudes and mean upper bound 
      ! lower boundaries given by cycloidal spin wave dispersions 
      ! intrachain J(meV) and interchain Jp(meV) exchanges (>0 antiferromagnetic)
      ! dE (meV) FWHM Gaussian broadening in energy thesis page 85
      ! parameters=
      ! 1 A_sw 
      ! 2 J (meV)
      ! 3 fwhm_sw (meV)
      ! 4 Jp/J (adimensional)
      ! 5 A_inc 
      ! 6 fwhm_inc (meV)
      ! 7 cen (meV)
      ! 8 T (K)
      ! 9 Const_bkg 
      ! 10 Amp_bkg 
      ! 11 Tau_bkg 
      ! 12 A_sw_m (relative) amplitude factor for the (Q-q) continuum relative to {xx} continuum 
      ! 13 A_sw_p (relative) amplitude factor for the (Q+q) continuum relative to {xx} continuum 
      ! 14 UBoundScale =1 if upper boundary of the continuum coincides with the Muller ansatz for the mean zb energy
      ! 15 =1 if bkg(energy), otherwise bkg(Qy)

C === calculate mean zone boundary energy
         call cs2cucl4(A,B,C,D,0.0d0,0.25d0,0.0d0,J,Jp)
         w1=sqrt((A+B)**2-(C+D)**2)   ! upper zone boundary energy k=1/4 
         call cs2cucl4(A,B,C,D,0.0d0,0.75d0,0.0d0,J,Jp)
         w2=sqrt((A+B)**2-(C+D)**2)   ! lower zone boundary energy k=3/4
         w2=(w1+w2)*p(14)*abs(sin(pi*Qy))   ! upper bound of the continuum assume the same for all modes 
      ! take as the continuum upper boundary the Muller boundary for the average zone boundary energy

C === calculate dispersion relation for S_{xx}	: w1(Q) and include polarization factor (sin(alpha))**2
         call cs2cucl4(A,B,C,D,Qx,Qy,Qz,J,Jp)
         wdisp=sqrt((A+B)**2-(C+D)**2)
         if (((en-cen) .gt. wdisp) .and. ((en-cen) .le. w2)) then 
            temp=A_sw*spin*(sin(alpha)**2)
     c/sqrt(abs((en-cen)**2-wdisp**2)+(dE*2.355d0)**2)
         else
            temp=0.0d0
         end if
      
C === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q-q) include polarization factor (1+cos(alpha)**2)
         call cs2cucl4(A,B,C,D,Qx,Qy-q,Qz,J,Jp)
         wdisp=sqrt((A-B)**2-(C-D)**2)
         if (((en-cen) .gt. wdisp) .and. ((en-cen) .le. w2)) then 
            temp=temp+A_sw*spin*p(12)*(1.0d0+cos(alpha)**2)
     c/sqrt(abs((en-cen)**2-wdisp**2)+(dE*2.355d0)**2)
         end if       
C === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q+q) include polarization factor (1+cos(alpha)**2)  
         call cs2cucl4(A,B,C,D,Qx,Qy+q,Qz,J,Jp)
         wdisp=sqrt((A-B)**2-(C-D)**2)
         if (((en-cen) .gt. wdisp) .and. ((en-cen) .le. w2)) then 
            temp=temp+A_sw*spin*p(13)*(1.0d0+cos(alpha)**2)
     c/sqrt(abs((en-cen)**2-wdisp**2)+(dE*2.355d0)**2)
         end if

      else if (icross .eq. 6) then
      ! ==========================================================================
      ! icross=6 ! spin-wave modes at low energies {yy} and {zz} (includes polarization factor) 
      !     + isotropic Muller ansatz for a continuum  at higher energies with mean upper boundary
      ! intrachain J(meV) and interchain Jp(meV) exchanges (>0 antiferromagnetic)
      ! dE (meV) FWHM Gaussian broadening in energy thesis page 85
      ! parameters=
      ! 1 A_sw amplitude of the continuum scattering 
      ! 2 J (meV)
      ! 3 fwhm_sw (meV)
      ! 4 Jp/J (adimensional)
      ! 5 A_inc 
      ! 6 fwhm_inc (meV)
      ! 7 cen (meV)
      ! 8 T (K)
      ! 9 Const_bkg 
      ! 10 Amp_bkg 
      ! 11 Tau_bkg
      ! 12 A_sw_m absolute amplitude factor for the w(Q-q)
      ! 13 A_sw_p absolute amplitude factor for the w(Q+q)
      ! 14 UBoundScale =1 if upper boundary of the continuum coincides with the Muller ansatz for the mean zb energy
      ! 15 =1 if bkg(energy), otherwise bkg(Qy)
            
C === calculate mean zone boundary energy
         call cs2cucl4(A,B,C,D,0.0d0,0.25d0,0.0d0,J,Jp)
         w1=sqrt((A+B)**2-(C+D)**2)   ! upper zone boundary energy k=1/4 
         call cs2cucl4(A,B,C,D,0.0d0,0.75d0,0.0d0,J,Jp)
         w2=sqrt((A+B)**2-(C+D)**2)   ! lower zone boundary energy k=3/4
         w2=(w1+w2)*p(14)*abs(sin(pi*Qy))   ! upper bound of the continuum assume the same for all modes 
      ! take as the continuum upper boundary the Muller boundary for the average zone boundary energy

C === calculate dispersion relation for S_{xx}	: w1(Q) and do not include polarization factor 
         call cs2cucl4(A,B,C,D,Qx,Qy,Qz,J,Jp)
         wdisp=sqrt((A+B)**2-(C+D)**2)
         if (((en-cen) .gt. wdisp) .and. ((en-cen) .le. w2)) then 
            temp=A_sw*spin
     c/sqrt(abs((en-cen)**2-wdisp**2)+(dE*2.355d0)**2)
         else
            temp=0.0d0
         end if

C === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q-q) include polarization factor (1+cos(alpha)**2)
         call cs2cucl4(A,B,C,D,Qx,Qy-q,Qz,J,Jp)
         wdisp=sqrt((A-B)**2-(C-D)**2)
         temp=temp+p(12)*spin/4.0d0*(1.0d0+cos(alpha)**2)
     c*(A-B-C+D)/(wdisp+dE/100.0d0)
     c*1.0d0/(sqrt(2.0d0*pi)*dE)*exp(-(en-wdisp-cen)**2/(2.0d0*dE**2))
       
C === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q+q) include polarization factor (1+cos(alpha)**2)  
         call cs2cucl4(A,B,C,D,Qx,Qy+q,Qz,J,Jp)
         wdisp=sqrt((A-B)**2-(C-D)**2)
         temp=temp+p(13)*spin/4.0d0*(1.0d0+cos(alpha)**2)
     c*(A-B-C+D)/(wdisp+dE/100.0d0)
     c*1.0d0/(sqrt(2.0d0*pi)*dE)*exp(-(en-wdisp-cen)**2/(2.0d0*dE**2))

      else if (icross .eq. 7) then
      ! ==================================================================================
      ! icross=7 ! 3 isotropic spinon continua with Muller ansatz and relative amplitudes
      ! lower boundaries given by cycloidal spin wave dispersions
      ! intrachain J(meV) and interchain Jp(meV) exchanges (>0 antiferromagnetic)
      ! upper boundary of the continuum given by the mean zone boundary energy
      ! dE (meV) FWHM Gaussian broadening in energy thesis page 85
      ! parameters=
      ! 1 A_xx amplitude for the continuum polarized along {xx} 
      ! 2 J (meV)
      ! 3 dE(meV) rounding of the low-boundary divergence
      ! 4 Jp/J (adimensional)
      ! 5 A_inc 
      ! 6 fwhm_inc (meV)
      ! 7 cen (meV)
      ! 8 T (K)
      ! 9 Const_bkg 
      ! 10 Amp_bkg 
      ! 11 Tau_bkg
      ! 12 A_yy_r (relative) amplitude factor for (Q-q) continuum in {yy} and {zz} relative to A_xx 
      ! 13 A_yy_r (relative) amplitude factor for (Q+q) continuum in {yy} and {zz} relative to A_xx
      ! 14 UBoundScale =1 if upper boundary of the continuum coincides with the Muller ansatz for the mean zb energy
      ! 15 =1 if bkg(energy), otherwise bkg(Qy)

C === calculate mean zone boundary energy
         call cs2cucl4(A,B,C,D,0.0d0,0.75d0,0.0d0,J,Jp)
         w1=sqrt((A+B)**2-(C+D)**2)   ! lower zone boundary energy k=3/4 
         call cs2cucl4(A,B,C,D,0.0d0,0.25d0,0.0d0,J,Jp)
         w2=sqrt((A+B)**2-(C+D)**2)   ! upper zone boundary energy k=1/4
         w2=(w1+w2)*p(14)*abs(sin(pi*Qy))   ! upper bound of the continuum assumed the same for all modes 
      ! take as the continuum upper boundary the Muller boundary for the average zone boundary energy

C === calculate dispersion relation for S_{xx}	: w1(Q) and do not include polarization factor 
         call cs2cucl4(A,B,C,D,Qx,Qy,Qz,J,Jp)
         wdisp=sqrt((A+B)**2-(C+D)**2)
         if (((en-cen) .gt. wdisp) .and. ((en-cen) .le. w2)) then 
            temp=A_sw*spin
     c/sqrt(abs((en-cen)**2-wdisp**2)+(dE*2.355d0)**2)
         else
            temp=0.0d0
         end if
      
C === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q-q) do not include polarization factor 
         call cs2cucl4(A,B,C,D,Qx,Qy-q,Qz,J,Jp)
         wdisp=sqrt((A-B)**2-(C-D)**2)
         if (((en-cen) .gt. wdisp) .and. ((en-cen) .le. w2)) then 
            temp=temp+
     c+p(12)*A_sw*spin/sqrt(abs((en-cen)**2-wdisp**2)+(dE*2.355d0)**2)
         end if
       
C === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q+q) do not include polarization factor 
         call cs2cucl4(A,B,C,D,Qx,Qy+q,Qz,J,Jp)
         wdisp=sqrt((A-B)**2-(C-D)**2)
         if (((en-cen) .gt. wdisp) .and. ((en-cen) .le. w2)) then 
            temp=temp
     c+p(13)*A_sw*spin/sqrt(abs((en-cen)**2-wdisp**2)+(dE*2.355d0)**2)
         end if

      else if (icross .eq. 8) then
      ! ===================================================================================================
      ! icross=8 ! 3 isotropic continuam with Muller ansatz lineshape and relative amplitudes 
      ! lower boundaries given by cycloidal spin wave dispersions 
      ! upper boundary of the continuum is at the Muller ansatz for the maximum zone boundary energy
      ! intrachain J(meV) and interchain Jp(meV) exchanges (>0 antiferromagnetic)
      ! dE (meV) FWHM Gaussian broadening in energy thesis page 85
      ! parameters=
      ! 1 A_xx amplitude for the continuum polarized along {xx} 
      ! 2 J (meV)
      ! 3 dE(meV) rounding of the low-boundary divergence
      ! 4 Jp/J (adimensional)
      ! 5 A_inc 
      ! 6 fwhm_inc (meV)
      ! 7 cen (meV)
      ! 8 T (K)
      ! 9 Const_bkg 
      ! 10 Amp_bkg 
      ! 11 Tau_bkg
      ! 12 A_yy_r (relative) amplitude factor for (Q-q) continuum in {yy} and {zz} relative to A_xx 
      ! 13 A_yy_r (relative) amplitude factor for (Q+q) continuum in {yy} and {zz} relative to A_xx
      ! 14 UBoundScale =1 if upper boundary of the continuum coincides with the Muller ansatz for the maximum zb energy
      ! 15 =1 if bkg(energy), otherwise bkg(Qy)

C === calculate mean zone boundary energy
         call cs2cucl4(A,B,C,D,0.0d0,0.25d0,0.0d0,J,Jp)
         w2=sqrt((A+B)**2-(C+D)**2)   ! upper zone boundary energy k=1/4
         w2=2.0d0*w2*p(14)*abs(sin(pi*Qy))   ! upper bound of the continuum assumed the same for all modes 
      ! take as the continuum upper boundary the Muller boundary for the maximum zone boundary energy

C === calculate dispersion relation for S_{xx}	: w1(Q) and include polarization factor (sin(alpha))**2
         call cs2cucl4(A,B,C,D,Qx,Qy,Qz,J,Jp)
         wdisp=sqrt((A+B)**2-(C+D)**2)
         if (((en-cen) .gt. wdisp) .and. ((en-cen) .le. w2)) then 
            temp=A_sw*spin
     c/sqrt(abs((en-cen)**2-wdisp**2)+(dE*2.355d0)**2)
         else
            temp=0.0d0
         end if
      
C === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q-q) include polarization factor (1+cos(alpha)**2)
         call cs2cucl4(A,B,C,D,Qx,Qy-q,Qz,J,Jp)
         wdisp=sqrt((A-B)**2-(C-D)**2)
         if (((en-cen) .gt. wdisp) .and. ((en-cen) .le. w2)) then 
            temp=temp+
     c+p(12)*A_sw*spin/sqrt(abs((en-cen)**2-wdisp**2)+(dE*2.355d0)**2)
         end if
       
C === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q+q) include polarization factor (1+cos(alpha)**2)  
         call cs2cucl4(A,B,C,D,Qx,Qy+q,Qz,J,Jp)
         wdisp=sqrt((A-B)**2-(C-D)**2)
         if (((en-cen) .gt. wdisp) .and. ((en-cen) .le. w2)) then 
            temp=temp
     c+p(13)*A_sw*spin/sqrt(abs((en-cen)**2-wdisp**2)+(dE*2.355d0)**2)
         end if

      else if (icross .eq. 9) then
      ! ============================================================================================
      ! icross=9 ! 3 isotropic spinon continua with Muller ansatz lineshape and relative amplitudes 
      ! lower boundaries given by cycloidal spin wave dispersion 
      ! upper bound modulated to mirror the lower-boundary dispersion
      ! intrachain J(meV) and interchain Jp(meV) exchanges (>0 antiferromagnetic)
      ! dE (meV) FWHM Gaussian broadening in energy thesis page 85
      ! parameters=
      ! 1 A_xx amplitude for the continuum polarized along {xx} 
      ! 2 J (meV)
      ! 3 dE(meV) rounding of the low-boundary divergence
      ! 4 Jp/J (adimensional)
      ! 5 A_inc 
      ! 6 fwhm_inc (meV)
      ! 7 cen (meV)
      ! 8 T (K)
      ! 9 Const_bkg 
      ! 10 Amp_bkg 
      ! 11 Tau_bkg
      ! 12 A_yy_m_r (relative) amplitude factor for (Q-q) continuum in {yy} and {zz} relative to A_xx 
      ! 13 A_yy_p_r (relative) amplitude factor for (Q+q) continuum in {yy} and {zz} relative to A_xx
      ! 14 UBoundScale =1 if upper boundary of the continuum coincides with the modulated boundary
      ! 15 =1 if bkg(energy), otherwise bkg(Qy)

C === calculate mean zone boundary energy
         call cs2cucl4(A,B,C,D,0.0d0,0.25d0,0.0d0,J,Jp)
         w1=sqrt((A+B)**2-(C+D)**2)   ! upper zone boundary energy for k=1/4

C === calculate dispersion relation for S_{xx}	: w1(Q) no polarization factor
         call cs2cucl4(A,B,C,D,Qx,Qy,Qz,J,Jp)
         wdisp=sqrt((A+B)**2-(C+D)**2)
         w2=2.0d0*w1*p(14)*abs(sin(pi*Qy)) 
      ! in case upper boundary w2 goes under the main dispersive mode put w2 just above wdisp 
         if (w2 .le. wdisp) then 
            w2=wdisp+2.355d0*dE
         end if
      ! take as the continuum upper boundary the Muller boundary for the maximum zone boundary energy
         if (((en-cen) .gt. wdisp) .and. ((en-cen) .le. w2)) then 
            temp=A_sw*spin
     c/sqrt(abs((en-cen)**2-wdisp**2)+(dE*2.355d0)**2)
         else
            temp=0.0d0
         end if
      
C === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q-q) do not include polarization factor
         call cs2cucl4(A,B,C,D,Qx,Qy-q,Qz,J,Jp)
         wdisp=sqrt((A-B)**2-(C-D)**2)
         w2=reduce(Qy,0.0d0,2.0d0) ! reduce in the interval [0,2)
         if ((w2 .gt. 0.0d0) .and. (w2 .le. (1.0d0-2.0d0*deltaq))) then 
            w2=2.0d0*w1*p(14)*abs(sin(w2*pi/(1.0d0-2.0d0*deltaq)))   ! shorter period
         else   ! longer period
            w2=2.0d0*w1*p(14)*
     c           abs(sin((2.0d0-w2)*pi/(1.0d0+2.0d0*deltaq)))
         end if
      ! in case upper boundary w2 goes under the main dispersive mode put w2 just above wdisp 
         if (w2 .le. wdisp) then 
            w2=wdisp+2.355d0*dE
         end if
         if (((en-cen) .gt. wdisp) .and. ((en-cen) .le. w2)) then 
            temp=temp+
     c+p(12)*A_sw*spin/sqrt(abs((en-cen)**2-wdisp**2)+(dE*2.355d0)**2)
         end if
       
C === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q+q) do not include polarization factor  
         call cs2cucl4(A,B,C,D,Qx,Qy+q,Qz,J,Jp)
         wdisp=sqrt((A-B)**2-(C-D)**2)
         w2=reduce(Qy,0.0d0,2.0d0) ! reduce in the interval [0,2)
         if ((w2 .gt. 0.0d0) .and. (w2 .le. (1.0d0+2.0d0*deltaq))) then 
            w2=2.0d0*w1*p(14)*abs(sin(w2*pi/(1.0d0+2.0d0*deltaq)))   ! longer period
         else   ! shorter period
            w2=2.0d0*w1*p(14)*
     c           abs(sin((2.0d0-w2)*pi/(1.0d0-2.0d0*deltaq)))
         end if
      ! in case upper boundary w2 goes under the main dispersive mode put w2 just above wdisp 
         if (w2 .le. wdisp) then 
            w2=wdisp+2.355d0*dE
         end if
         if (((en-cen) .gt. wdisp) .and. ((en-cen) .le. w2)) then 
            temp=temp
     c+p(13)*A_sw*spin/sqrt(abs((en-cen)**2-wdisp**2)+(dE*2.355d0)**2)
         end if

      else if (icross .eq. 10) then
      ! =============================================================================================
      ! icross=10 ! 3 isotropic spinon continua with Muller ansatz lineshape and absolute amplitudes
      ! lower boundaries given by cycloidal spin wave dispersions and modulated upper bound
      ! intrachain J(meV) and interchain Jp(meV) exchanges (>0 antiferromagnetic)
      ! dE (meV) FWHM Gaussian broadening in energy thesis page 85
      ! parameters=
      ! 1 A_xx amplitude for the continuum polarized along {xx} 
      ! 2 J (meV)
      ! 3 dE(meV) rounding of the low-boundary divergence
      ! 4 Jp/J (adimensional)
      ! 5 A_inc 
      ! 6 fwhm_inc (meV)
      ! 7 cen (meV)
      ! 8 T (K)
      ! 9 Const_bkg 
      ! 10 Amp_bkg 
      ! 11 Tau_bkg
      ! 12 A_yy_m absolute amplitude factor for (Q-q) continuum in {yy} and {zz} 
      ! 13 A_yy_p absolute amplitude factor for (Q+q) continuum in {yy} and {zz} 
      ! 14 UBoundScale =1 if upper boundary of the continuum coincides with the Muller ansatz for the mean zb energy
      ! 15 =1 if bkg(energy), otherwise bkg(Qy)

C === calculate mean zone boundary energy
         call cs2cucl4(A,B,C,D,0.0d0,0.25d0,0.0d0,J,Jp)
         w1=sqrt((A+B)**2-(C+D)**2)   ! upper zone boundary energy k=1/4

C === calculate dispersion relation for S_{xx}	: w1(Q) do not with polarization factor 
         call cs2cucl4(A,B,C,D,Qx,Qy,Qz,J,Jp)
         wdisp=sqrt((A+B)**2-(C+D)**2)
         w2=2.0d0*w1*p(14)*abs(sin(pi*Qy)) 
      ! take as the continuum upper boundary the Muller boundary for the maximum zone boundary energy
         if (((en-cen) .gt. wdisp) .and. ((en-cen) .le. w2)) then 
            temp=A_sw*spin
     c/sqrt(abs((en-cen)**2-wdisp**2)+(dE*2.355d0)**2)
         else
            temp=0.0d0
         end if
      
C === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q-q) do not include polarization factor 
         call cs2cucl4(A,B,C,D,Qx,Qy-q,Qz,J,Jp)
         wdisp=sqrt((A-B)**2-(C-D)**2)
         w2=reduce(Qy,0.0d0,2.0d0) ! reduce in the interval [0,2)
         if ((w2 .gt. 0.0d0) .and. (w2 .le. (1.0d0-2.0d0*deltaq))) then 
            w2=2.0d0*w1*p(14)*abs(sin(w2*pi/(1.0d0-2.0d0*deltaq)))   ! shorter period
         else   ! longer period
            w2=2.0d0*w1*p(14)*
     c           abs(sin((2.0d0-w2)*pi/(1.0d0+2.0d0*deltaq)))
         end if
         if (((en-cen) .gt. wdisp) .and. ((en-cen) .le. w2)) then 
            temp=temp+p(12)*spin
     c/sqrt(abs((en-cen)**2-wdisp**2)+(dE*2.355d0)**2)
         end if
       
C === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q+q) do not include polarization factor 
         call cs2cucl4(A,B,C,D,Qx,Qy+q,Qz,J,Jp)
         wdisp=sqrt((A-B)**2-(C-D)**2)
         w2=reduce(Qy,0.0d0,2.0d0) ! reduce in the interval [0,2)
         if ((w2 .gt. 0.0d0) .and. (w2 .le. (1.0d0+2.0d0*deltaq))) then 
            w2=2.0d0*w1*p(14)*abs(sin(w2*pi/(1.0d0+2.0d0*deltaq)))   ! longer period
         else   ! shorter period
            w2=2.0d0*w1*p(14)*
     c           abs(sin((2.0d0-w2)*pi/(1.0d0-2.0d0*deltaq)))
         end if
         if (((en-cen) .gt. wdisp) .and. ((en-cen) .le. w2)) then 
            temp=temp+p(13)*spin
     c/sqrt(abs((en-cen)**2-wdisp**2)+(dE*2.355d0)**2)
         end if

      else if (icross .eq. 11) then
      ! ============================================================================================
      ! icross=11 ! 3 polarised spinon continua with Muller ansatz lineshape and relative amplitudes
      ! lower boundaries given by cycloidal spin wave dispersions and modulated upper bound
      ! intrachain J(meV) and interchain Jp(meV) exchanges (>0 antiferromagnetic)
      ! dE (meV) FWHM Gaussian broadening in energy thesis page 85
      ! parameters=
      ! 1 A_xx amplitude for the continuum polarized along {xx} 
      ! 2 J (meV)
      ! 3 dE(meV) rounding of the low-boundary divergence
      ! 4 Jp/J (adimensional)
      ! 5 A_inc 
      ! 6 fwhm_inc (meV)
      ! 7 cen (meV)
      ! 8 T (K)
      ! 9 Const_bkg 
      ! 10 Amp_bkg 
      ! 11 Tau_bkg
      ! 12 A_yy_m (relative) amplitude factor for (Q-q) continuum in {yy} and {zz} 
      ! 13 A_yy_p (relative) amplitude factor for (Q+q) continuum in {yy} and {zz} 
      ! 14 UBoundScale =1 if upper boundary of the continuum coincides with the modulated upper bound
      ! 15 =1 if bkg(energy), otherwise bkg(Qy)

C === calculate mean zone boundary energy
         call cs2cucl4(A,B,C,D,0.0d0,0.25d0,0.0d0,J,Jp)
         w1=sqrt((A+B)**2-(C+D)**2)   ! upper zone boundary energy k=1/4

C === calculate dispersion relation for S_{xx}	: w1(Q) with polarization factor (sin(alpha)**2)
         call cs2cucl4(A,B,C,D,Qx,Qy,Qz,J,Jp)
         wdisp=sqrt((A+B)**2-(C+D)**2)
      ! take as the continuum upper boundary the Muller boundary for the maximum zone boundary energy
         w2=2.0d0*w1*p(14)*abs(sin(pi*Qy)) 
      ! in case upper boundary w2 goes under the main dispersive mode put w2 just above wdisp 
         if (w2 .le. wdisp) then 
            w2=wdisp+2.355d0*dE
         end if
         if (((en-cen) .gt. wdisp) .and. ((en-cen) .le. w2)) then 
            temp=A_sw*spin*(sin(alpha)**2)
     c/sqrt(abs((en-cen)**2-wdisp**2)+(dE*2.355d0)**2)
         else
            temp=0.0d0
         end if
      
C === calculate dispersion relation for S_{yy}=S_{zz} : w2(Q-q) include polarization factor (1+cos(alpha)**2)
         call cs2cucl4(A,B,C,D,Qx,Qy-q,Qz,J,Jp)
         wdisp=sqrt((A-B)**2-(C-D)**2)
         w2=reduce(Qy,0.0d0,2.0d0) ! reduce in the interval [0,2)
         if ((w2 .gt. 0.0d0) .and. (w2 .le. (1.0d0-2.0d0*deltaq))) then 
            w2=2.0d0*w1*p(14)*abs(sin(w2*pi/(1.0d0-2.0d0*deltaq)))   ! shorter period
         else   ! longer period
            w2=2.0d0*w1*p(14)*
     c           abs(sin((2.0d0-w2)*pi/(1.0d0+2.0d0*deltaq)))
         end if
      ! in case upper boundary w2 goes under the main dispersive mode put w2 just above wdisp 
         if (w2 .le. wdisp) then 
            w2=wdisp+2.355d0*dE
         end if
         if (((en-cen) .gt. wdisp) .and. ((en-cen) .le. w2)) then 
            temp=temp+p(12)*A_sw*spin*(1.0d0+cos(alpha)**2)/2.0d0 ! 13May00 introduce factor/2 to have observed scattering same as before 
     c/sqrt(abs((en-cen)**2-wdisp**2)+(dE*2.355d0)**2)         
         end if
       
C === calculate dispersion relation for S_{yy}=S_{zz} : w2(Q+q) include polarization factor (1+cos(alpha)**2)  
         call cs2cucl4(A,B,C,D,Qx,Qy+q,Qz,J,Jp)
         wdisp=sqrt((A-B)**2-(C-D)**2)
         w2=reduce(Qy,0.0d0,2.0d0) ! reduce in the interval [0,2)
         if ((w2 .gt. 0.0d0) .and. (w2 .le. (1.0d0+2.0d0*deltaq))) then 
            w2=2.0d0*w1*p(14)*abs(sin(w2*pi/(1.0d0+2.0d0*deltaq)))   ! longer period
         else   ! shorter period
            w2=2.0d0*w1*p(14)*
     c           abs(sin((2.0d0-w2)*pi/(1.0d0-2.0d0*deltaq)))
         end if
      ! in case upper boundary w2 goes under the main dispersive mode put w2 just above wdisp 
         if (w2 .le. wdisp) then 
            w2=wdisp+2.355d0*dE
         end if
         if (((en-cen) .gt. wdisp) .and. ((en-cen) .le. w2)) then 
            temp=temp+p(13)*A_sw*spin*(1.0d0+cos(alpha)**2)/2.0d0 ! 13May00 introduce factor/2 to have observed scattering same as before
     c/sqrt(abs((en-cen)**2-wdisp**2)+(dE*2.355d0)**2)
         end if

      else if (icross .eq. 12) then
      ! ============================================================================================
      ! icross=12 ! 3 isotropic spinon continua with Muller ansatz lineshape and relative amplitudes 
      ! lower boundaries given by cycloidal spin wave dispersion 
      ! upper bound modulated to mirror the lower-boundary dispersion and scaled continuum extent
      ! intrachain J(meV) and interchain Jp(meV) exchanges (>0 antiferromagnetic)
      ! dE (meV) FWHM Gaussian broadening in energy thesis page 85
      ! parameters=
      ! 1 A_xx amplitude for the continuum polarized along {xx} 
      ! 2 J (meV)
      ! 3 dE(meV) rounding of the low-boundary divergence
      ! 4 Jp/J (adimensional)
      ! 5 A_inc 
      ! 6 fwhm_inc (meV)
      ! 7 cen (meV)
      ! 8 T (K)
      ! 9 Const_bkg 
      ! 10 Amp_bkg 
      ! 11 Tau_bkg
      ! 12 A_yy_m_r (relative) amplitude factor for (Q-q) continuum in {yy} and {zz} relative to A_xx 
      ! 13 A_yy_p_r (relative) amplitude factor for (Q+q) continuum in {yy} and {zz} relative to A_xx
      ! 14 UBoundScale =1 if upper boundary of the continuum coincides with the modulated boundary
      ! 15 =1 if bkg(energy), otherwise bkg(Qy)

C === calculate mean zone boundary energy
         call cs2cucl4(A,B,C,D,0.0d0,0.25d0,0.0d0,J,Jp)
         w1=sqrt((A+B)**2-(C+D)**2)   ! upper zone boundary energy for k=1/4

C === calculate dispersion relation for S_{xx}	: w1(Q) no polarization factor
         call cs2cucl4(A,B,C,D,Qx,Qy,Qz,J,Jp)
         wdisp=sqrt((A+B)**2-(C+D)**2)
         w2=2.0d0*w1*abs(sin(pi*Qy))
         w2=wdisp+p(14)*abs(w2-wdisp) ! scale extent of continuum above wdisp by the factor p(14) 
      ! take as the continuum upper boundary the Muller boundary for the maximum zone boundary energy
         if (((en-cen) .gt. wdisp) .and. ((en-cen) .le. w2)) then 
            temp=A_sw*spin
     c/sqrt(abs((en-cen)**2-wdisp**2)+(dE*2.355d0)**2)
         else
            temp=0.0d0
         end if
      
C === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q-q) do not include polarization factor
         call cs2cucl4(A,B,C,D,Qx,Qy-q,Qz,J,Jp)
         wdisp=sqrt((A-B)**2-(C-D)**2)
         w2=reduce(Qy,0.0d0,2.0d0) ! reduce in the interval [0,2)
         if ((w2 .gt. 0.0d0) .and. (w2 .le. (1.0d0-2.0d0*deltaq))) then 
            w2=2.0d0*w1*abs(sin(w2*pi/(1.0d0-2.0d0*deltaq)))   ! shorter period
         else   ! longer period
            w2=2.0d0*w1*abs(sin((2.0d0-w2)*pi/(1.0d0+2.0d0*deltaq)))
         end if
         w2=wdisp+p(14)*abs(w2-wdisp)
         if (((en-cen) .gt. wdisp) .and. ((en-cen) .le. w2)) then 
            temp=temp+
     c+p(12)*A_sw*spin/sqrt(abs((en-cen)**2-wdisp**2)+(dE*2.355d0)**2)
         end if
       
C === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q+q) do not include polarization factor  
         call cs2cucl4(A,B,C,D,Qx,Qy+q,Qz,J,Jp)
         wdisp=sqrt((A-B)**2-(C-D)**2)
         w2=reduce(Qy,0.0d0,2.0d0) ! reduce in the interval [0,2)
         if ((w2 .gt. 0.0d0) .and. (w2 .le. (1.0d0+2.0d0*deltaq))) then 
            w2=2.0d0*w1*abs(sin(w2*pi/(1.0d0+2.0d0*deltaq)))   ! longer period
         else   ! shorter period
            w2=2.0d0*w1*abs(sin((2.0d0-w2)*pi/(1.0d0-2.0d0*deltaq)))
         end if
         w2=wdisp+p(14)*abs(w2-wdisp)
         if (((en-cen) .gt. wdisp) .and. ((en-cen) .le. w2)) then 
            temp=temp
     c+p(13)*A_sw*spin/sqrt(abs((en-cen)**2-wdisp**2)+(dE*2.355d0)**2)
         end if

      else if (icross .eq. 13) then
      ! ==========================================================================
      ! icross=13 ! Schultz' formula for the temperature dependence of the 
      ! spinon continuum in a spin-1/2 Heisenberg antiferromagnetic chain , 
      ! ferromagnetic + antiferromagnetic zone treated the same way
      ! intrachain J(meV) and interchain Jp(meV) exchanges (>0 antiferromagnetic)
      ! dE (meV) FWHM Gaussian broadening in energy, dispersion thesis page 85
      ! parameters=
      ! 1 A = amplitude 
      ! 2 J (meV)= intra-chain antiferromagnetic exchange
      ! 3 unused 
      ! 4 Jp/J (adimensional)= frustration ratio
      ! 5 A_inc = amplitude incoherent quasielastic scattering
      ! 6 fwhm_inc (meV)
      ! 7 cen (meV)
      ! 8 T (K)= temperature 
      ! 9 Const_bkg 
      ! 10 Amp_bkg 
      ! 11 Tau_bkg
      ! 12 unused 
      ! 13 unused 
      ! 14 unused 
      ! 15 =1 if bkg(energy), otherwise bkg(Qy)

C === calculate dispersion relation for S_{xx}	: w1(Q)
         call cs2cucl4(A,B,C,D,Qx,Qy,Qz,J,Jp)
         wdisp=sqrt((A+B)**2-(C+D)**2)
         temp=p(8)/11.605d0   ! <temp> is k_B*T in units of meV
         w1=(en-wdisp)/(4.0d0*pi*temp) ! (w-vq)/(4 pi k_B T)
         w2=(en+wdisp)/(4.0d0*pi*temp) ! (w+vq)/(4 pi k_B T)
         temp=A_sw/(4.0d0*pi)*dimag(rho(w1)*rho(w2))/temp ! <temp> is now the cross-section

      else if (icross .eq. 14) then
      ! ==========================================================================
      ! icross=14 ! Schultz' formula for the temperature dependence of the 
      ! spinon continuum in a spin-1/2 Heisenberg antiferromagnetic chain only valid in the 
      ! long wavelength limit around the AF zone centre 
      ! intrachain J(meV) and interchain Jp(meV) exchanges (>0 antiferromagnetic)
      ! dE (meV) FWHM Gaussian broadening in energy, dispersion thesis page 85
      ! parameters=
      ! 1 A = amplitude 
      ! 2 J (meV)= intra-chain antiferromagnetic exchange
      ! 3 unused 
      ! 4 Jp/J (adimensional)= frustration ratio
      ! 5 A_inc = amplitude incoherent quasielastic scattering
      ! 6 fwhm_inc (meV)
      ! 7 cen (meV)
      ! 8 T (K)= temperature 
      ! 9 Const_bkg 
      ! 10 Amp_bkg 
      ! 11 Tau_bkg
      ! 12 unused 
      ! 13 unused 
      ! 14 unused 
      ! 15 =1 if bkg(energy), otherwise bkg(Qy)

C === assume zone-boundary energy is J and displace q values by Deltaq long the chain direction 
         nu=J*2.0d0*pi  ! spin-wave velocity
         temp=p(8)/11.605d0   ! <temp> is k_B*T in units of meV
         w1=(en-nu*(Qy-0.5d0-deltaq))/(4.0d0*pi*temp) ! (w-vq)/(4 pi k_B T)
         w2=(en+nu*(Qy-0.5d0-deltaq))/(4.0d0*pi*temp) ! (w+vq)/(4 pi k_B T)
         temp=A_sw/(4.0d0*pi)*dimag(rho(w1)*rho(w2))/temp ! <temp> is now the cross-section
C =================================================================================================

      else if (icross .eq. 15) then
      ! ============================================================================================
      ! icross=15 ! 3 isotropic spinon continua with Muller ansatz lineshape and relative amplitudes 
      ! lower boundaries given by cycloidal spin wave dispersion 
      ! upper bound modulated to mirror the lower-boundary dispersion and top given by approx sum of min+max zb energies
      ! intrachain J(meV) and interchain Jp(meV) exchanges (>0 antiferromagnetic)
      ! dE (meV) FWHM Gaussian broadening in energy thesis page 85
      ! parameters=
      ! 1 A_xx amplitude for the continuum polarized along {xx} 
      ! 2 J (meV)
      ! 3 dE(meV) rounding of the low-boundary divergence
      ! 4 Jp/J (adimensional)
      ! 5 A_inc 
      ! 6 fwhm_inc (meV)
      ! 7 cen (meV)
      ! 8 T (K)
      ! 9 Const_bkg 
      ! 10 Amp_bkg 
      ! 11 Tau_bkg
      ! 12 A_yy_m_r (relative) amplitude factor for (Q-q) continuum in {yy} and {zz} relative to A_xx 
      ! 13 A_yy_p_r (relative) amplitude factor for (Q+q) continuum in {yy} and {zz} relative to A_xx
      ! 14 UBoundScale =1 if upper boundary of the continuum coincides with the modulated boundary
      ! 15 =1 if bkg(energy), otherwise bkg(Qy)

C === calculate mean zone boundary energy
         call cs2cucl4(A,B,C,D,0.0d0,0.25d0,0.0d0,J,Jp)
         w1=sqrt((A+B)**2-(C+D)**2)   ! upper zone boundary energy for k=0.25
         call cs2cucl4(A,B,C,D,0.0d0,0.75d0,0.0d0,J,Jp)
         w2=sqrt((A+B)**2-(C+D)**2)   ! lower zone boundary energy for k=0.75
         w1=(w1+w2)/2.0d0 ! take the average of the two zone boundary energies
 
C === calculate dispersion relation for S_{xx}	: w1(Q) no polarization factor
         call cs2cucl4(A,B,C,D,Qx,Qy,Qz,J,Jp)
         wdisp=sqrt((A+B)**2-(C+D)**2)
         w2=2.0d0*w1*p(14)*abs(sin(pi*Qy)) 
      ! in case upper boundary w2 goes under the main dispersive mode put w2 just above wdisp 
         if (w2 .le. wdisp) then 
            w2=wdisp+2.355d0*dE
         end if
      ! take as the continuum upper boundary the Muller boundary for the maximum zone boundary energy
         if (((en-cen) .gt. wdisp) .and. ((en-cen) .le. w2)) then 
            temp=A_sw*spin
     c/sqrt(abs((en-cen)**2-wdisp**2)+(dE*2.355d0)**2)
         else
            temp=0.0d0
         end if
      
C === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q-q) do not include polarization factor
         call cs2cucl4(A,B,C,D,Qx,Qy-q,Qz,J,Jp)
         wdisp=sqrt((A-B)**2-(C-D)**2)
         w2=reduce(Qy,0.0d0,2.0d0) ! reduce in the interval [0,2)
         if ((w2 .gt. 0.0d0) .and. (w2 .le. (1.0d0-2.0d0*deltaq))) then 
            w2=2.0d0*w1*p(14)*abs(sin(w2*pi/(1.0d0-2.0d0*deltaq)))   ! shorter period
         else   ! longer period
            w2=2.0d0*w1*p(14)*
     c           abs(sin((2.0d0-w2)*pi/(1.0d0+2.0d0*deltaq)))
         end if
      ! in case upper boundary w2 goes under the main dispersive mode put w2 just above wdisp 
         if (w2 .le. wdisp) then 
            w2=wdisp+2.355d0*dE
         end if
         if (((en-cen) .gt. wdisp) .and. ((en-cen) .le. w2)) then 
            temp=temp+
     c+p(12)*A_sw*spin/sqrt(abs((en-cen)**2-wdisp**2)+(dE*2.355d0)**2)
         end if
       
C === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q+q) do not include polarization factor  
         call cs2cucl4(A,B,C,D,Qx,Qy+q,Qz,J,Jp)
         wdisp=sqrt((A-B)**2-(C-D)**2)
         w2=reduce(Qy,0.0d0,2.0d0) ! reduce in the interval [0,2)
         if ((w2 .gt. 0.0d0) .and. (w2 .le. (1.0d0+2.0d0*deltaq))) then 
            w2=2.0d0*w1*p(14)*abs(sin(w2*pi/(1.0d0+2.0d0*deltaq)))   ! longer period
         else   ! shorter period
            w2=2.0d0*w1*p(14)*
     c           abs(sin((2.0d0-w2)*pi/(1.0d0-2.0d0*deltaq)))
         end if
      ! in case upper boundary w2 goes under the main dispersive mode put w2 just above wdisp 
         if (w2 .le. wdisp) then 
            w2=wdisp+2.355d0*dE
         end if
         if (((en-cen) .gt. wdisp) .and. ((en-cen) .le. w2)) then 
            temp=temp
     c+p(13)*A_sw*spin/sqrt(abs((en-cen)**2-wdisp**2)+(dE*2.355d0)**2)
         end if

      else if (icross .eq. 16) then
      ! ============================================================================================
      ! icross=16 ! 3 polarised spinon continua with Muller ansatz lineshape and relative amplitudes
      ! lower boundaries given by cycloidal spin wave dispersions 
      ! upper bound modulated to mirror the lower-boundary dispersion and top given by approx sum of min+max zb energies
      ! intrachain J(meV) and interchain Jp(meV) exchanges (>0 antiferromagnetic)
      ! dE (meV) FWHM Gaussian broadening in energy thesis page 85
      ! parameters=
      ! 1 A_xx amplitude for the continuum polarized along {xx} 
      ! 2 J (meV)
      ! 3 dE(meV) rounding of the low-boundary divergence
      ! 4 Jp/J (adimensional)
      ! 5 A_inc 
      ! 6 fwhm_inc (meV)
      ! 7 cen (meV)
      ! 8 T (K)
      ! 9 Const_bkg 
      ! 10 Amp_bkg 
      ! 11 Tau_bkg
      ! 12 A_yy_m (relative) amplitude factor for (Q-q) continuum in {yy} and {zz} 
      ! 13 A_yy_p (relative) amplitude factor for (Q+q) continuum in {yy} and {zz} 
      ! 14 UBoundScale =1 if upper boundary of the continuum coincides with the modulated upper bound
      ! 15 =1 if bkg(energy), otherwise bkg(Qy)

C === calculate mean zone boundary energy
         call cs2cucl4(A,B,C,D,0.0d0,0.25d0,0.0d0,J,Jp)
         w1=sqrt((A+B)**2-(C+D)**2)   ! upper zone boundary energy k=1/4
         call cs2cucl4(A,B,C,D,0.0d0,0.75d0,0.0d0,J,Jp)
         w2=sqrt((A+B)**2-(C+D)**2)   ! lower zone boundary energy for k=0.75
         w1=(w1+w2)/2.0d0 ! take the average of the two zone boundary energies

C === calculate dispersion relation for S_{xx}	: w1(Q) with polarization factor (sin(alpha)**2)
         call cs2cucl4(A,B,C,D,Qx,Qy,Qz,J,Jp)
         wdisp=sqrt((A+B)**2-(C+D)**2)
      ! take as the continuum upper boundary the Muller boundary for the maximum zone boundary energy
         w2=2.0d0*w1*p(14)*abs(sin(pi*Qy)) 
      ! in case upper boundary w2 goes under the main dispersive mode put w2 just above wdisp 
         if (w2 .le. wdisp) then 
            w2=wdisp+2.355d0*dE
         end if
         if (((en-cen) .gt. wdisp) .and. ((en-cen) .le. w2)) then 
            temp=A_sw*spin*(sin(alpha)**2)
     c/sqrt(abs((en-cen)**2-wdisp**2)+(dE*2.355d0)**2)
         else
            temp=0.0d0
         end if
      
C === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q-q) include polarization factor (1+cos(alpha)**2)
         call cs2cucl4(A,B,C,D,Qx,Qy-q,Qz,J,Jp)
         wdisp=sqrt((A-B)**2-(C-D)**2)
         w2=reduce(Qy,0.0d0,2.0d0) ! reduce in the interval [0,2)
         if ((w2 .gt. 0.0d0) .and. (w2 .le. (1.0d0-2.0d0*deltaq))) then 
            w2=2.0d0*w1*p(14)*abs(sin(w2*pi/(1.0d0-2.0d0*deltaq)))   ! shorter period
         else   ! longer period
            w2=2.0d0*w1*p(14)*
     c           abs(sin((2.0d0-w2)*pi/(1.0d0+2.0d0*deltaq)))
         end if
      ! in case upper boundary w2 goes under the main dispersive mode put w2 just above wdisp 
         if (w2 .le. wdisp) then 
            w2=wdisp+2.355d0*dE
         end if
         if (((en-cen) .gt. wdisp) .and. ((en-cen) .le. w2)) then 
            temp=temp+p(12)*A_sw*spin*(1.0d0+cos(alpha)**2)
     c/sqrt(abs((en-cen)**2-wdisp**2)+(dE*2.355d0)**2)
         end if
       
C === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q+q) include polarization factor (1+cos(alpha)**2)  
         call cs2cucl4(A,B,C,D,Qx,Qy+q,Qz,J,Jp)
         wdisp=sqrt((A-B)**2-(C-D)**2)
         w2=reduce(Qy,0.0d0,2.0d0) ! reduce in the interval [0,2)
         if ((w2 .gt. 0.0d0) .and. (w2 .le. (1.0d0+2.0d0*deltaq))) then 
            w2=2.0d0*w1*p(14)*abs(sin(w2*pi/(1.0d0+2.0d0*deltaq)))   ! longer period
         else   ! shorter period
            w2=2.0d0*w1*p(14)*
     c           abs(sin((2.0d0-w2)*pi/(1.0d0-2.0d0*deltaq)))
         end if
      ! in case upper boundary w2 goes under the main dispersive mode put w2 just above wdisp 
         if (w2 .le. wdisp) then 
            w2=wdisp+2.355d0*dE
         end if
         if (((en-cen) .gt. wdisp) .and. ((en-cen) .le. w2)) then 
            temp=temp+p(13)*A_sw*spin*(1.0d0+cos(alpha)**2)
     c/sqrt(abs((en-cen)**2-wdisp**2)+(dE*2.355d0)**2)
         end if

      else if (icross .eq. 17) then
      !=====================================================================
      ! numerical Monte Carlo calculation of 
      ! S(k,w) for a triangular antiferromagnet relevant to Cs2CuCl4 
      ! exchanges J along b, Jp zig-zag along c
      ! uses binned data produced by user$disk:[radu.progs.fortran]binmc_2dtr.for
      ! parameters=
      ! 1 overall amplitude 
      ! 2 J (meV)
      ! 3 dE(meV) (not used) rounding of the low-boundary divergence
      ! 4 <uv> average value of uv 
      ! 5 A_inc 
      ! 6 fwhm_inc (meV)
      ! 7 cen (meV)
      ! 8 T (K)
      ! 9 Const_bkg 
      ! 10 Amp_bkg 
      ! 11 Tau_bkg
      ! 12 A_1m_in (relative) amplitude of 1m in-plane relative to total out-of-plane 1m (1 by default)
      ! 13 A_2m (relative) amplitude of total 2m relative to total out-of-plane 1m (1 by default) 
      ! 14 DeltaS = given as parameter, experimental 0.125
      ! 15 =1 if bkg(energy), otherwise bkg(Qy)
      ! ====================================================================

         if (.not. read_tables) then
            ! === read 1+2 magnon tables
            filename='c:\mprogs\mslice\sqw_1matr_in.dat'
            open(unit=1,file=filename, status='unknown', readonly)
            call mexPrintf('Opening file '//filename);
            filename='c:\mprogs\mslice\sqw_1matr_out.dat'
            open(unit=2,file=filename, status='unknown', readonly)
            call mexPrintf('Opening file '//filename);
            filename='c:\mprogs\mslice\sqw_2matr.dat'
            open(unit=3,file=filename, status='unknown', readonly)
            call mexPrintf('Opening file '//filename);
            ! === first 12 elements are 
            ! 1 x1 
            ! 2 x2 
            ! 3 dx 
            ! 4 nx 
            ! 5 y1 
            ! 6 y2 
            ! 7 dy 
            ! 8 ny 
            ! 9 e1 
            ! 10 e2 
            ! 11 de 
            ! 12 ne       
            do i=1,12
               read(1,*) table_1m_in(i)
	       read(2,*) table_1m_out(i)
	       read(3,*) table_2m(i)
            end do

            ! === check table size 
            i=12+table_1m_in(4)*table_1m_in(8)*table_1m_in(12)
            if (i .gt. maxsize_table) then
               call mexErrMsgTxt('Table 1m_in exceeds maximum size.')
            end if
            i1=12+table_1m_out(4)*table_1m_out(8)*table_1m_out(12)

            if (i1 .gt. maxsize_table) then
               call mexErrMsgTxt('Table 1m_out exeeds maximum size.')
            end if 
            if (i1 .ne. i) then
               call mexErrMsgTxt('Tables 1m-in/out of different sizes.')
            end if 

            i1=12+table_2m(4)*table_2m(8)*table_2m(12)
            if (i1 .gt. maxsize_table) then
               call mexErrMsgTxt('Table 2m exeeds maximum size.')
            end if
            if (i1 .ne. i) then
               call mexErrMsgTxt
     c             ('Tables 2m and 1m_in of different sizes.')
            end if 

            ! === read elements of the array S(x,y,e) for 1 in- and out-of-plane and 2 magnon
            ! === and sum total number of MC events for normalization then close opened files 
            nevents_1m_in=0            
            nevents_1m_out=0
            nevents_2m=0	 
            do i=13,i1
               read(1,*) table_1m_in(i)
	       read(2,*) table_1m_out(i)
	       read(3,*) table_2m(i)
	       nevents_1m_in=nevents_1m_in+int(table_1m_in(i))
	       nevents_1m_out=nevents_1m_out+int(table_1m_out(i))
	       nevents_2m=nevents_2m+int(table_2m(i))
            end do
            read_tables=.TRUE.
            close(1)
            close(2)
            close(3)

            ! === determine absolute normalization factor for 1 and 2 magnon
            ! === S1m_in and S1m_out have the average over the NBZ 1/2*(S-DeltaS)(1+2*DeltaS+/-2<uv>) 
            ! === S2m     DeltaS*(DeltaS+1) (neglect corrections) 
            ! === see notebook 27 page 49  
            absnorm_1m_in=1.0d0/table_1m_in(3)/table_1m_in(7)/
     c         table_1m_in(11)/dfloat(nevents_1m_in)
            absnorm_1m_out=1.0d0/table_1m_out(3)/table_1m_out(7)/
     c         table_1m_out(11)/dfloat(nevents_1m_out)
            absnorm_2m=1.0d0/table_2m(3)/table_2m(7)/
     c         table_2m(11)/dfloat(nevents_2m)
            do i=13,i1
               table_1m_in(i)=table_1m_in(i)*absnorm_1m_in
               table_1m_out(i)=table_1m_out(i)*absnorm_1m_out               
               table_2m(i)=table_2m(i)*absnorm_2m
            end do
            ! === table_1m_in now contains the normalised (to unity) S(k,w) 1-magnon in-plane
            ! === table_1m_out S(k,w) 1-magnon out-of-plane
            ! === table_2m  S(k,w) 2-magnon
            call mexPrintf('Tables loaded.')
         end if

         ! === now calculate all contributions to S(k,w), start with in-plane 1-magnon 
         temp=0.0d0 ! this will contain S(k,w), initialize to zero  
         deltas=p(14) ! DeltaS = spin reduction
         absnorm_1m_in=(1+2.0d0*deltas+2.0d0*p(4)) 
!     c         *(spin-deltas)/2.0d0 ! factor in front of the delta function in S(k,w) 1 mag 
         ! === calculate S(k-Q,w) and S(k+Q,w) 1m (in-plane) 
         ! note that p(4) is not Jp but has been redefined as <uv>, so need to give Jp by hand 
         Jp=0.175d0*J
         q=acos(Jp/(2.0d0*J))/pi ! cycloidal ordering wavevector (rlu) along b* 0.472
         bigq=1.0d0-q ! Q in the triangular lattice formalism
         do i=1,12 ! initialize table limits
            table(i)=table_1m_in(i)
         end do
         ! === see notebook 27 page 40 for transormation from (x,y) to (k,l) in a triangular lattice
         x=(Qz+(Qy-bigq))/2.0d0+1.0d0 ! (Qx,Qy,Qz) are in rlu of h,k,l along a*,b*,c*
         y=(Qz-(Qy-bigq))/2.0d0+1.0d0
         i=index(x,y,en/p(2),table)
         if (i .gt. 0) then 
            temp=temp + spin*(1.0d0+ cos(alpha)**2)*p(12)*p(1)
     c         *absnorm_1m_in*table_1m_in(i)/4.0d0 ! (k-Q,w) first
         end if
         x=(Qz+(Qy+bigq))/2.0d0+1.0d0
         y=(Qz-(Qy+bigq))/2.0d0+1.0d0
         i=index(x,y,en/p(2),table)
         if (i .gt. 0) then 
            temp = temp+spin*(1.0d0+ cos(alpha)**2)*p(12)*p(1)
     c         *absnorm_1m_in*table_1m_in(i)/4.0d0 ! add (k+Q,w)
         end if
 
         ! === calculate now S(k,w) 1-magon (out-of-plane)
         absnorm_1m_out=(1+2.0d0*deltas-2.0d0*p(4))
!     c         *(spin-deltas)/2.0d0
         x=(Qz+Qy)/2.0d0+1.0d0
         y=(Qz-Qy)/2.0d0+1.0d0
         do i=1,12
            table(i)=table_1m_out(i)
         end do
         i=index(x,y,en/p(2),table)
         if (i .gt. 0) then 
            temp = temp + spin*(sin(alpha)**2)*p(1)*
     c         absnorm_1m_out*table_1m_out(i) ! (k,w) component
         end if

         ! === calculate S(k-Q,w) and S(k+Q,w) 2-magnon (all in-plane polarization)
         absnorm_2m=(((1+deltas)*deltas +p(4)**2)/
     c         (0.5d0*(spin-deltas-p(4)**2/(1+2*deltas))))
         do i=1,12
            table(i)=table_2m(i)
         end do
         x=(Qz+(Qy-bigq))/2.0d0+1.0d0
         y=(Qz-(Qy-bigq))/2.0d0+1.0d0
         i=index(x,y,en/p(2),table)
         if (i .gt. 0) then 
            temp = temp+spin*(1.0d0+ cos(alpha)**2)*p(13)
     c         *absnorm_2m*table_2m(i)/4.0d0  ! (k-Q,w) first
         end if
         x=(Qz+(Qy+bigq))/2.0d0+1.0d0
         y=(Qz-(Qy+bigq))/2.0d0+1.0d0
         i=index(x,y,en/p(2),table)
         if (i .gt. 0) then 
            temp = temp+spin*(1.0d0+ cos(alpha)**2)*p(13)
     c         *absnorm_2m*table_2m(i)/4.0d0 ! add (k+Q,w)
         end if  
         ! === p(13) = scaling of 2-magnon relative to 1m-out 
         ! === (=1 if sum rules are satisfied, see notebook 27 page 49)

         temp=temp*2.0d0 ! to have amplitides in the same units as the analytical form icross=3
         !                 factor of 2 is from transformation from (x,y) coords to (k,l) for 1BZ

      else if (icross .eq. 18) then
      ! ============================================================================================
      ! icross=18 ! 3 isotropic spinon continua with Muller ansatz (adjustable power)lineshape 
      ! and relative amplitudes 
      ! lower boundaries given by cycloidal spin wave dispersion 
      ! upper bound modulated to mirror the lower-boundary dispersion
      ! intrachain J(meV) and interchain Jp(meV) exchanges (>0 antiferromagnetic)
      ! dE (meV) FWHM Gaussian broadening in energy thesis page 85
      ! parameters=
      ! 1 A_xx amplitude for the continuum polarized along {xx} 
      ! 2 J (meV)
      ! 3 nu = negative power of (w^2-wk^2) in lineshape [in Muller Ansatz nu=1/2]
      ! 4 Jp/J (adimensional)
      ! 5 A_inc 
      ! 6 fwhm_inc (meV)
      ! 7 cen (meV)
      ! 8 T (K)
      ! 9 Const_bkg 
      ! 10 Amp_bkg 
      ! 11 Tau_bkg
      ! 12 A_yy_m_r (relative) amplitude factor for (Q-q) continuum in {yy} and {zz} relative to A_xx 
      ! 13 A_yy_p_r (relative) amplitude factor for (Q+q) continuum in {yy} and {zz} relative to A_xx
      ! 14 UBoundScale =1 if upper boundary of the continuum coincides with the modulated boundary
      ! 15 =1 if bkg(energy), otherwise bkg(Qy)

         dE=0.020d0
C === calculate mean zone boundary energy
         call cs2cucl4(A,B,C,D,0.0d0,0.25d0,0.0d0,J,Jp)
         w1=sqrt((A+B)**2-(C+D)**2)   ! upper zone boundary energy for k=1/4

C === calculate dispersion relation for S_{xx}	: w1(Q) no polarization factor
         call cs2cucl4(A,B,C,D,Qx,Qy,Qz,J,Jp)
         wdisp=sqrt((A+B)**2-(C+D)**2)
         w2=2.0d0*w1*p(14)*abs(sin(pi*Qy)) 
      ! in case upper boundary w2 goes under the main dispersive mode put w2 just above wdisp 
         if (w2 .le. wdisp) then 
            w2=wdisp+2.355d0*dE
         end if
      ! take as the continuum upper boundary the Muller boundary for the maximum zone boundary energy
         if (((en-cen) .gt. wdisp) .and. ((en-cen) .le. w2)) then 
            temp=A_sw*spin
     c/exp(p(3)*log(abs((en-cen)**2-wdisp**2)+(dE*2.355d0)**2))
         else
            temp=0.0d0
         end if
      
C === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q-q) do not include polarization factor
         call cs2cucl4(A,B,C,D,Qx,Qy-q,Qz,J,Jp)
         wdisp=sqrt((A-B)**2-(C-D)**2)
         w2=reduce(Qy,0.0d0,2.0d0) ! reduce in the interval [0,2)
         if ((w2 .gt. 0.0d0) .and. (w2 .le. (1.0d0-2.0d0*deltaq))) then 
            w2=2.0d0*w1*p(14)*abs(sin(w2*pi/(1.0d0-2.0d0*deltaq)))   ! shorter period
         else   ! longer period
            w2=2.0d0*w1*p(14)*
     c           abs(sin((2.0d0-w2)*pi/(1.0d0+2.0d0*deltaq)))
         end if
      ! in case upper boundary w2 goes under the main dispersive mode put w2 just above wdisp 
         if (w2 .le. wdisp) then 
            w2=wdisp+2.355d0*dE
         end if
         if (((en-cen) .gt. wdisp) .and. ((en-cen) .le. w2)) then 
            temp=temp+p(12)*A_sw*spin/
     c         exp(p(3)*log(abs((en-cen)**2-wdisp**2)+(dE*2.355d0)**2))
         end if
       
C === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q+q) do not include polarization factor  
         call cs2cucl4(A,B,C,D,Qx,Qy+q,Qz,J,Jp)
         wdisp=sqrt((A-B)**2-(C-D)**2)
         w2=reduce(Qy,0.0d0,2.0d0) ! reduce in the interval [0,2)
         if ((w2 .gt. 0.0d0) .and. (w2 .le. (1.0d0+2.0d0*deltaq))) then 
            w2=2.0d0*w1*p(14)*abs(sin(w2*pi/(1.0d0+2.0d0*deltaq)))   ! longer period
         else   ! shorter period
            w2=2.0d0*w1*p(14)*
     c           abs(sin((2.0d0-w2)*pi/(1.0d0-2.0d0*deltaq)))
         end if
      ! in case upper boundary w2 goes under the main dispersive mode put w2 just above wdisp 
         if (w2 .le. wdisp) then 
            w2=wdisp+2.355d0*dE
         end if
         if (((en-cen) .gt. wdisp) .and. ((en-cen) .le. w2)) then 
            temp=temp+p(13)*A_sw*spin/
     c          exp(p(3)*log(abs((en-cen)**2-wdisp**2)+(dE*2.355d0)**2))
         end if

      else if (icross .eq. 19) then
      ! ==========================================================================
      ! icross=19 ! polarised spin-waves with relative amplitudes
      ! intrachain J(meV) and interchain Jp(meV) exchanges (>0 antiferromagnetic)
      ! dE (meV) FWHM Gaussian broadening in energy thesis page 85
      ! parameters=
      ! 1 A_sw 
      ! 2 J (meV)
      ! 3 fwhm_sw (meV)
      ! 4 Jp/J (adimensional)
      ! 5 A_inc 
      ! 6 fwhm_inc (meV)
      ! 7 cen (meV)
      ! 8 T (K)
      ! 9 Const_bkg 
      ! 10 Amp_bkg 
      ! 11 Tau_bkg
      ! 12 A_sw_m (relative) amplitude factor for the w(Q-q) relative to w(Q) branch =1 in cycloidal model
      ! 13 A_sw_p (relative) amplitude factor for the w(Q+q) relative to w(Q) branch =1 in cycloidal model
      ! 14 unused
      ! 15 =1 if bkg(energy), otherwise bkg(Qy)

C === calculate dispersion relation for S_{xx}(k,w) : w(k) and include polarization factor (sin(alpha))**2
         call uv_2dtr(A,B,wdisp,J,Jp,Qy,Qz)  ! here u=A, v=B
         temp=A_sw*spin*(sin(alpha)**2)
     c*abs(A-B)**2
     c/(sqrt(2.0d0*pi)*dE)*exp(-(en-wdisp-cen)**2/(2.0d0*dE**2))
      
C === calculate dispersion relation for S_{yy} and S_{zz}(k,w): w(k-Q) include polarization factor (1+cos(alpha)**2)
         bigq=1.0d0-q           ! Q in the triangular lattice formalism         
         call uv_2dtr(A,B,wdisp,J,Jp,Qy-bigq,Qz)  ! here u=A, v=B
         temp=temp+A_sw*p(12)*spin/4.0d0*(1.0d0+cos(alpha)**2)
     c*abs(A+B)**2
     c/(sqrt(2.0d0*pi)*dE)*exp(-(en-wdisp-cen)**2/(2.0d0*dE**2))
       
C === calculate dispersion relation for S_{yy} and S_{zz}(k,w): w(k+Q) include polarization factor (1+cos(alpha)**2)  
         call uv_2dtr(A,B,wdisp,J,Jp,Qy+bigq,Qz)  ! here u=A, v=B
         temp=temp+A_sw*p(13)*spin/4.0d0*(1.0d0+cos(alpha)**2)
     c*abs(A+B)**2
     c/(sqrt(2.0d0*pi)*dE)*exp(-(en-wdisp-cen)**2/(2.0d0*dE**2))

      else if (icross .eq. 20) then
      ! ============================================================================================
      ! icross=20 ! 3 isotropic spinon continua with Muller ansatz lineshape and relative amplitudes 
      ! lower boundaries given by cycloidal spin wave dispersion 
      ! asymmetric upper boundary  
      ! intrachain J(meV) and interchain Jp(meV) exchanges (>0 antiferromagnetic)
      ! dE (meV) FWHM Gaussian broadening in energy thesis page 85
      ! parameters=
      ! 1 A_xx amplitude for the continuum polarized along {xx} 
      ! 2 J (meV)
      ! 3 dE(meV) rounding of the low-boundary divergence
      ! 4 Jp/J (adimensional)
      ! 5 A_inc 
      ! 6 fwhm_inc (meV)
      ! 7 cen (meV)
      ! 8 T (K)
      ! 9 Const_bkg 
      ! 10 Amp_bkg 
      ! 11 Tau_bkg
      ! 12 A_yy_r (relative) amplitude factor for (Q-q) and (Q+q) continuum in {yy} and {zz} relative to A_xx 
      ! 13 UBoundQ = 0.5 if symmetric <0.5 if skewed towards Qy=0,2... and away from Qy=1,3,5... 
      ! 14 UBoundAmp (meV) top of continuum upper boundary
      ! 15 =1 if bkg(energy), otherwise bkg(Qy)

C === calculate upper boundary 
         w2=reduce(Qy,0.0d0,2.0d0) ! reduce Qy in the interval [0,2)
         if (w2 .gt. 1.0d0) then 
            w2=2.0d0-w2 ! reduce now to [0,1] by symmetry
         end if
         if (w2 .lt. p(13)) then 
            w2=p(14)*abs(sin(pi*w2/2.0d0/p(13)))
         else
            w2=p(14)*abs(sin(pi*(1.0d0-w2)/2.0d0/(1.0d0-p(13))))
         end if
         ! w2 is now the upper boundary for all three continua
         temp=0.0d0 ! this will contain the temporary cross-section

C === calculate dispersion relation for S_{xx}	: w1(Q) no polarization factor
         call cs2cucl4(A,B,C,D,Qx,Qy,Qz,J,Jp)
         wdisp=sqrt((A+B)**2-(C+D)**2)
      ! in case upper boundary w2 goes under the main dispersive mode no contribution to cross-section
      ! use a Muller ansatz type lineshape 
         if (((en-cen) .gt. wdisp) .and. ((en-cen) .le. w2)) then 
            temp=temp+A_sw*spin
     c /sqrt(abs((en-cen)**2-wdisp**2)+(dE*2.355d0)**2)
         end if
      
C === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q-q) do not include polarization factor
         call cs2cucl4(A,B,C,D,Qx,Qy-q,Qz,J,Jp)
         wdisp=sqrt((A-B)**2-(C-D)**2)
      ! in case upper boundary w2 goes under the main dispersive mode then no contribution to cross-section
         if (((en-cen) .gt. wdisp) .and. ((en-cen) .le. w2)) then 
            temp=temp+p(12)*A_sw*spin
     c /sqrt(abs((en-cen)**2-wdisp**2)+(dE*2.355d0)**2)
         end if
       
C === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q+q) do not include polarization factor  
         call cs2cucl4(A,B,C,D,Qx,Qy+q,Qz,J,Jp)
         wdisp=sqrt((A-B)**2-(C-D)**2)
      ! in case upper boundary w2 goes under the main dispersive mode then no contribution to cross-section
         if (((en-cen) .gt. wdisp) .and. ((en-cen) .le. w2)) then 
            temp=temp+p(12)*A_sw*spin
     c /sqrt(abs((en-cen)**2-wdisp**2)+(dE*2.355d0)**2)
         end if

! =======================================================================
      end if ! icross=1,2,3,4,5,6,7,8,9,10,11,12,13,15,16,17,18,19,20

C === multiply by the form factor squared, Bose factor 
C === and add incoherent quasielastic scattering plus background model
      temp=(fQ**2)*bose*temp
     c+A_inc/sqrt(2.0d0*pi)/sig_inc*exp(-(en-cen)**2/(2.0d0*sig_inc**2))
     c+p(9)
      if (int(p(15)).eq.1) then
         temp=temp+p(10)*exp(-en/p(11)) ! for bkg(energy)      
      else
         temp=temp+p(10)*exp(-Qy/p(11)) ! for bkg(k)
      end if   
      ! === if background very small (<eps=1e-5) put effectively zero to avoid numbers like e-300
      if (abs(temp) .lt. eps) then 
         temp=0.0d0
      end if
      ms_sqw=temp
      return
      end ! function ms_sqw

















