C===============================================================================
C     >> mex spe2proj_df.f
C     This is a MEX-file for MATLAB.   
C     returns projections v1(ndet,ne),v2(ndet,ne),v3(ndet,ne) of pixels
C     onto the 4-dimensional (reciprocal space 3-axes, energy) axes U1,U2,U3 
C     24-Oct-2000 version for Digital Visual Fortran 6
C     use passing pointers in Fortran through the %VAL construct
C===============================================================================
      subroutine mexFunction(nlhs, plhs, nrhs, prhs)
C-----------------------------------------------------------------------
C     (integer) Replace integer by integer*8 on the DEC Alpha and the
C     SGI 64-bit platforms
      implicit NONE
      integer plhs(*), prhs(*), nrhs, nlhs
C declare local variables to deal with pointers to variables passed by/to MATLAB
      integer emode_pr, efixed_pr, en_pr, det_theta_pr, det_psi_pr,
     c        psi_samp_pr,U1_pr, U2_pr, U3_pr
      integer v1_pr, v2_pr, v3_pr 
C declare calling functions
      integer mxCreateFull, mxGetM, mxGetN, mxIsNumeric, mxGetPr 
C declare local operating variables of the interface funnction
      integer ndet, ne 
      double precision emode, efixed, psi_samp 

C     Check for proper number of MATLAB input and output arguments 
      if (nrhs .ne. 9) then
         call mexErrMsgTxt
     c ('Nine inputs (emode, efixed, en, det_theta, det_psi, psi_samp, 
     c U1, U2, U3) required.')
      elseif (nlhs .ne. 3) then
         call mexErrMsgTxt
     c  ('Three outputs (v1,v2,v3)required.')
      end if

C     Check to see if all inputs are numeric
      if         (mxIsNumeric(prhs(1)) .ne. 1) then
         call mexErrMsgTxt('Input #1 is not a numeric array.')
      elseif     (mxIsNumeric(prhs(2)) .ne. 1) then
         call mexErrMsgTxt('Input #2 is not a numeric array.')
      elseif     (mxIsNumeric(prhs(3)) .ne. 1) then
         call mexErrMsgTxt('Input #3 is not a numeric array.')
      elseif     (mxIsNumeric(prhs(4)) .ne. 1) then
         call mexErrMsgTxt('Input #4 is not a numeric array.')
      elseif     (mxIsNumeric(prhs(5)) .ne. 1) then
         call mexErrMsgTxt('Input #5 is not a numeric array.')
      elseif     (mxIsNumeric(prhs(6)) .ne. 1) then
         call mexErrMsgTxt('Input #6 is not a numeric array.')
      elseif     (mxIsNumeric(prhs(7)) .ne. 1) then
         call mexErrMsgTxt('Input #7 is not a numeric array.')
      elseif     (mxIsNumeric(prhs(8)) .ne. 1) then
         call mexErrMsgTxt('Input #8 is not a numeric array.')
      elseif     (mxIsNumeric(prhs(9)) .ne. 1) then
         call mexErrMsgTxt('Input #9 is not a numeric array.')
      end if

C     Check sizes of input arguments
      ne  =mxGetN(prhs(3))*mxGetM(prhs(3))  ! number of energy bins
      ndet=mxGetN(prhs(4))*mxGetM(prhs(4))  ! number of detectors 
      if     ((mxGetN(prhs(1))*mxGetM(prhs(1))) .ne. 1) then
	 call mexErrMsgTxt('Input #1(emode) should be a scalar.') 
      elseif ((mxGetN(prhs(2))*mxGetM(prhs(2))) .ne. 1) then
	 call mexErrMsgTxt('Input #2(efixed) should be a scalar.') 
      elseif ((mxGetN(prhs(5))*mxGetM(prhs(5))) .ne. ndet) then
         call mexErrMsgTxt
     c('Size of #5(det_psi) does not mach that of #4(det_theta)')
      elseif ((mxGetN(prhs(6))*mxGetM(prhs(6))) .ne. 1) then
	 call mexErrMsgTxt('Input #6(psi_samp) should be a scalar.') 
      elseif ((mxGetN(prhs(7))*mxGetM(prhs(7))) .ne. 4) then
         call mexErrMsgTxt('Input #7(U1) should be a 4-element vector')
      elseif ((mxGetN(prhs(8))*mxGetM(prhs(8))) .ne. 4) then
         call mexErrMsgTxt('Input #8(U2) should be a 4-element vector')
      elseif ((mxGetN(prhs(9))*mxGetM(prhs(9))) .ne. 4) then
         call mexErrMsgTxt('Input #9(U3) should be a 4-element vector')
      end if

C     Get pointers to input parameters
      emode_pr    =mxGetPr(prhs(1))
      efixed_pr   =mxGetPr(prhs(2))
      en_pr       =mxGetPr(prhs(3)) 
      det_theta_pr=mxGetPr(prhs(4))
      det_psi_pr  =mxGetPr(prhs(5))
      psi_samp_pr =mxGetPr(prhs(6))
      U1_pr       =mxGetPr(prhs(7))
      U2_pr       =mxGetPr(prhs(8))
      U3_pr       =mxGetPr(prhs(9))
      call mxCopyPtrToReal8(emode_pr,emode,1)
      call mxCopyPtrToReal8(efixed_pr,efixed,1)
      call mxCopyPtrToReal8(psi_samp_pr,psi_samp,1)

C     Create matrices for the return arguments 
      plhs(1)	    =mxCreateFull(ndet,ne,0) ! these are matlab pointsrs
      plhs(2)	    =mxCreateFull(ndet,ne,0)
      plhs(3)       =mxCreateFull(ndet,ne,0)
      v1_pr         =mxGetPr(plhs(1)) ! these are fortran pointers of the matlab mxArrays created above
      v2_pr         =mxGetPr(plhs(2)) 
      v3_pr         =mxGetPr(plhs(3))

C     Call the computational subroutine spe2proj_df
      call spe2proj_df(int(emode),efixed,%val(en_pr),ne,
     c     %val(det_theta_pr),%val(det_psi_pr),ndet,psi_samp,
     c     %val(U1_pr),%val(U2_pr),%val(U3_pr),
     c     %val(v1_pr),%val(v2_pr),%val(v3_pr))
      return
      end
C
C ===============================================================================
C actual FORTRAN code for the spe2proj_df algorithm
C =============================================================================== 
      subroutine spe2proj_df(emode,efixed,en,ne,
     c     det_theta,det_psi,ndet,psi_samp,
     c     U1,U2,U3,
     c     v1,v2,v3)
C declare input and output variables
      implicit NONE
      integer emode, ndet, ne 
      double precision efixed, en(*), det_theta(*), det_psi(*), 
     c     psi_samp, U1(*), U2(*), U3(*), v1(*), v2(*), v3(*) 
C declare local variables   
      integer i, j, k
      double precision ki, kf, kx, ky, kz, Qx, Qy, Qz, cos_psi, sin_psi
      ! store these numbers for faster calculations
      cos_psi=cos(psi_samp) 
      sin_psi=sin(psi_samp)    
      ki=sqrt(efixed/2.07d0)
      kf=sqrt(efixed/2.07d0)
      do i=1,ndet
         do j=1,ne
            k=i+ndet*(j-1)      ! global index in the (ndet,ne) matrix
            !  choose between direct/indirect geometry configurations
            if (emode.eq.1) then   
               ! For direct-geometry spectrometers like HET, MARI
               ! efixed = monochromatic incident energy ei(meV) 
               kf=sqrt((efixed-en(j))/2.07d0)
            elseif (emode .eq.2) then
               ! For indirect-geometry spectrometers like IRIS
               ! efixed = monochromatic scattered energy (meV) for a white incident beam  
               ! here all detectors are in the horizontal plane with Psi=0
               ! scattering geometry diagram in notebook computing 2, page 2-14
               ki=sqrt((efixed+en(j))/2.07d0)
            else ! error
               ndet=0
               return
            end if 
            ! (kx,ky,kz) in spectrometer reference frame
            kx=ki-cos(det_theta(i))*kf
            ky=  -sin(det_theta(i))*cos(det_psi(i))*kf
	    kz=  -sin(det_theta(i))*sin(det_psi(i))*kf
            ! (kx,ky,kz) in rotated reference frame
            Qx= cos_psi*kx+sin_psi*ky
            Qy=-sin_psi*kx+cos_psi*ky
            Qz= kz
              ! (v1,v2,v3) projected onto reciprocal space directions
            v1(k)=U1(1)*Qx+U1(2)*Qy+U1(3)*Qz+U1(4)*en(j)
            v2(k)=U2(1)*Qx+U2(2)*Qy+U2(3)*Qz+U2(4)*en(j)
            v3(k)=U3(1)*Qx+U3(2)*Qy+U3(3)*Qz+U3(4)*en(j)
         end do
      end do
      return
      end
