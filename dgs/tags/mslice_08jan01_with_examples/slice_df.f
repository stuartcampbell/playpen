C==============================================================================================   
C     >> mex slice_df.f
C     This is a MEX-file for MATLAB.   
C     to take a slice from a 3D data set of pixels with (vx,vy,vz) coordinates 
C     take only pixels with coordinate  vz_min<vz<vz_max and plot intensity 
C     (error calculated as well) onto a rectangular 2d grid in the 
C     (vx,vy) plane [vx_min to vx_max] and [vy_min to vy_max] 
C     10-Oct-2000 version for Digital Visual Fortran 6
C     use passing pointers in Fortran through the %VAL construct
C==============================================================================================	
      subroutine mexFunction(nlhs, plhs, nrhs, prhs)
C-----------------------------------------------------------------------
C     (integer) Replace integer by integer*8 on the DEC Alpha and the
C     SGI 64-bit platforms
      implicit NONE
      integer plhs(*), prhs(*), nrhs, nlhs
C declare local variables to deal with pointers to variables passed by/to MATLAB
      integer vx_pr, vy_pr, vz_pr, pixel_int_pr, pixel_err_pr, grid_pr
      integer intensity_pr, error_int_pr
C declare calling functions
      integer mxCreateFull, mxGetM, mxGetN, mxIsNumeric, mxGetPr
C declare local operating variables of the interface funnction
      integer ndet, ne, n, m
      double precision grid(8), vx_min, vx_max, bin_vx, 
     c    vy_min, vy_max, bin_vy, vz_min, vz_max
  
C     Check for proper number of MATLAB input and output arguments 
      if (nrhs .ne. 6) then
         call mexErrMsgTxt
     c ('Six inputs (vx,vy,vz,pixel_int,pixel_err,grid) required.')
      elseif (nlhs .ne. 2) then
         call mexErrMsgTxt('Two outputs (intensity,err_int) required.')
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
      end if

C     Check sizes of input arguments
      ndet=mxGetM(prhs(1))
      ne  =mxGetN(prhs(1)) 
      if     ((mxGetM(prhs(2)).ne.ndet).or.(mxGetN(prhs(2)).ne.ne)) then
         call mexErrMsgTxt
     c('Size of input #2 does not mach that of input #1')
      elseif ((mxGetM(prhs(3)).ne.ndet).or.(mxGetN(prhs(3)).ne.ne)) then
         call mexErrMsgTxt
     c('Size of input #3 does not mach that of input #1')
      elseif ((mxGetM(prhs(4)).ne.ndet).or.(mxGetN(prhs(4)).ne.ne)) then
         call mexErrMsgTxt
     c('Size of input #4 does not mach that of input #1')
      elseif ((mxGetM(prhs(5)).ne.ndet).or.(mxGetN(prhs(5)).ne.ne)) then
         call mexErrMsgTxt
     c('Size of input #5 does not mach that of input #1')
      elseif ((mxGetM(prhs(6))*mxGetN(prhs(6))) .ne. 8) then
	  call mexErrMsgTxt('Input #6 grid should have 8 elements.') 
      end if

C     Get vx,vy,vz,pixel_int,pixel_err and grid parameters
      vx_pr=mxGetPr(prhs(1))
      vy_pr=mxGetPr(prhs(2))
      vz_pr=mxGetPr(prhs(3)) 
      pixel_int_pr=mxGetPr(prhs(4))
      pixel_err_pr=mxGetPr(prhs(5))
      grid_pr=mxGetPr(prhs(6))
      call mxCopyPtrToReal8(grid_pr,grid,8)

      vx_min=grid(1)
      vx_max=grid(2)
      bin_vx=grid(3)
      vy_min=grid(4)
      vy_max=grid(5)
      bin_vy=grid(6)
      vz_min=grid(7)
      vz_max=grid(8)     
      n=int((vx_max+bin_vx-(vx_min-bin_vx/2.0d0))/bin_vx)
      m=int((vy_max+bin_vy-(vy_min-bin_vy/2.0d0))/bin_vy)

C     Create matrices for the return arguments
      plhs(1)	   =mxCreateFull(m,n,0)
      plhs(2)	   =mxCreateFull(m,n,0)
      intensity_pr =mxGetPr(plhs(1))
      error_int_pr =mxGetPr(plhs(2))

C     Call the computational subroutine slice_df
      call slice_df(%val(vx_pr),%val(vy_pr),%val(vz_pr),
     c     %val(pixel_int_pr),%val(pixel_err_pr), ndet*ne, 
     c     vx_min-bin_vx/2.0d0,bin_vx,n,
     c     vy_min-bin_vy/2.0d0,bin_vy,m,vz_min,vz_max,
     c     %val(intensity_pr),%val(error_int_pr))

      return
      end
C
C ===============================================================================
C actual FORTRAN code for the bin2dall_df algorithm
C distribute all data points into bins, then take average over each bin
C =============================================================================== 
      subroutine slice_df(vx,vy,vz,
     c     pixel_int,pixel_err,Npixels,
     c     vx_min,bin_vx,n,
     c     vy_min,bin_vy,m,vz_min,vz_max,
     c     intensity,error_int)
C declare input and output variables
      implicit NONE
C     m = number of rows, 
C     n = number of columns	
      integer m, n, number_pts(m*n), Npixels
      double precision vx(*), vy(*), vz(*), pixel_int(*), pixel_err(*), 
     c                 vx_min, bin_vx, vy_min, bin_vy, vz_min, vz_max,
     c                 intensity(*),error_int(*)
      double precision vx_max, vy_max
C declare local variables   
      integer i, j, ij, k
C initialize to zero the number of points and cummulative intensity in each bin
      do i=1,m
	 do j=1,n
	    ij=i+m*(j-1)
	    number_pts(ij)=0
	    intensity(ij)=0.0d0
	    error_int(ij)=0.0d0
	 end do
      end do 
C run through all pixels and if contributing then distribute then into bins
      vx_max=vx_min+dfloat(n)*bin_vx
      vy_max=vy_min+dfloat(m)*bin_vy 
      do k=1,Npixels
        if ((vx(k).ge.(vx_min)).and.(vx(k).lt.vx_max).and.
     c      (vy(k).ge.(vy_min)).and.(vy(k).lt.vy_max).and. 
     c      (vz(k).ge.(vz_min)).and.(vz(k).le.vz_max).and. 
     c      (pixel_int(k) .gt. -1d+30)) then    ! also test if pixel is not masked
          i=int((vy(k)-vy_min)/bin_vy+1.0d0)
	  j=int((vx(k)-vx_min)/bin_vx+1.0d0)
	  ij=i+m*(j-1)
	  number_pts(ij)=number_pts(ij)+1
	  intensity(ij)=intensity(ij)+pixel_int(k)
	  error_int(ij)=error_int(ij)+pixel_err(k)**2 
	end if	
      end do
C take the average over each bin	
      do i=1,m
	do j=1,n
           ij=i+m*(j-1)
	   if (number_pts(ij) .ge. 1) then
	      intensity(ij)=intensity(ij)/dfloat(number_pts(ij))
	      error_int(ij)=sqrt(error_int(ij))/dfloat(number_pts(ij))
           else
	      intensity(ij)=-1d+30
	      error_int(ij)=0.0d0
	   end if	
	end do
      end do

      return
      end

