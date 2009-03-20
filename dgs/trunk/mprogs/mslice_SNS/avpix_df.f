C==============================================================================
C     >> mex avpix_df.f
C     This is a MEX-file for MATLAB   
C     to calculate the averege of a quantity over a group of pixels with 
C     specific binning information and return averages and standard deviations
C     Radu Coldea 22-Oct-2000  compiled for PC/WINNT using Matlab 5.3 and 
C     Digital Visual Fortran Professional Edition 6
C     use passing pointers in Fortran through the %VAL construct
C==============================================================================	
      subroutine mexFunction(nlhs, plhs, nrhs, prhs)
C------------------------------------------------------------------------------
C     (integer) Replace integer by integer*8 on the DEC Alpha and the
C     SGI 64-bit platforms
      implicit NONE
      integer plhs(*), prhs(*), nrhs, nlhs
C declare local variables to deal with pointers to variables passed by/to MATLAB
      integer x_pr, perm_pr, number_pix_pr, xx_pr, std_dev_pr
C declare calling functions
      integer mxCreateFull, mxGetM, mxGetN, mxIsNumeric, mxGetPr
C declare local operating variables of the interface funnction
      integer Ntotal, Npixels, Nbins
  
C     Check for proper number of MATLAB input and output arguments 
      if (nrhs .ne. 3) then
         call mexErrMsgTxt('Three inputs (x,perm,number_pix) required.')
      else if (nlhs .ne. 2) then
         call mexErrMsgTxt
     c        ('Two outputs (xx,std_dev) required.')
      end if

C     Check to see if all inputs are numeric
      if      (mxIsNumeric(prhs(1)) .ne. 1) then
         call mexErrMsgTxt('Input # 1 is not a numeric array.')
      else if (mxIsNumeric(prhs(2)) .ne. 1) then
         call mexErrMsgTxt('Input #2 is not a numeric array.')
      else if (mxIsNumeric(prhs(3)) .ne. 1) then
         call mexErrMsgTxt('Input #3 is not a numeric array.')
      end if
      
C    Get x values, perm and number_pix information
      x_pr   =mxGetPr(prhs(1)) ! x-values of pixels (Ntotal values)
      Ntotal =mxGetN(prhs(1))*mxGetM(prhs(1)) ! total number of pixels  
      perm_pr=mxGetPr(prhs(2)) ! indexes of contributing pixels (Npixels values)
      Npixels=mxGetN(prhs(2))*mxGetM(prhs(2)) ! total number of contributing pixels     
      number_pix_pr=mxGetPr(prhs(3)) ! number of pixels per bin (Nbins values)
      Nbins  =mxGetN(prhs(3))*mxGetM(prhs(3)) ! total number of bins

C     Create matrices for the return arguments
      plhs(1)=mxCreateFull(mxGetM(prhs(3)),mxGetN(prhs(3)),0)
      plhs(2)=mxCreateFull(mxGetM(prhs(3)),mxGetN(prhs(3)),0)
      xx_pr     = mxGetPr(plhs(1))
      std_dev_pr= mxGetPr(plhs(2))

C     Call averaging subroutine avpix_df
      call avpix_df(%val(x_pr),Ntotal,%val(perm_pr),Npixels, 
     c       %val(number_pix_pr),Nbins,%val(xx_pr),%val(std_dev_pr))

C     If binning recipe inconsistent then clean up memory and return error message
      if (Ntotal .eq. 0) then 
         call mxFreeMatrix(plhs(1))
         call mxFreeMatrix(plhs(2))        
         call mexErrMsgTxt('Error, binning recipe and
     c given number of pixels not compatible')
      end if   
      return
      end
C
C ===============================================================================
C actual FORTRAN code to execute the pixel average avpix_df
C =============================================================================== 
      subroutine avpix_df(x,Ntotal,perm,Npixels,number_pix,Nbins,
     c                     xx,std_dev)
C declare input and output variables
      implicit NONE
      integer Ntotal, Npixels, Nbins
      double precision x(*), perm(*), number_pix(*), xx(*), std_dev(*)
      integer i, j, cumm_pixels
C     'i' will index pixels and 'j' will index bins 'cumm_pixels' 
C     will be the partial number of pixels in a bin 
C initialize variables and check consistency of binning recipe
      cumm_pixels=0
      do j=1,Nbins
         xx(j)     =0.0d0
         std_dev(j)=0.0d0
         cumm_pixels=cumm_pixels+int(number_pix(j))
      end do
C return if binning recipe inconsistent (convention put Ntotal=0)
      if (Npixels .ne. cumm_pixels) then
         Ntotal=0
         return
      end if
C run through all pixels, put them in bins and calculate average and standard deviation
      cumm_pixels=0
      j=1 ! current bin index
      do i=1,Npixels  ! run through all contributing pixels
         cumm_pixels=cumm_pixels+1 ! one more pixel in current bin
         xx(j)=xx(j)+x(int(perm(i)))! add to partial sum of x-values
         std_dev(j)=std_dev(j)+x(int(perm(i)))**2 ! add to partial sum of squared values
         if (cumm_pixels .eq. int(number_pix(j))) then ! have topped up current bin 
            xx(j)=xx(j)/number_pix(j) ! average(mean) of x-values
            if (int(number_pix(j)) .eq. 1) then 
               std_dev(j)=0.0d0
            else
               std_dev(j)=sqrt(std_dev(j)/number_pix(j)-xx(j)**2)*
     c              sqrt(number_pix(j)/(number_pix(j)-1.0d0)) 
               ! standard deviation of the mean
            end if 
         j=j+1 ! move on to next bin
         cumm_pixels=0 ! start with no pixels in next bin
         end if 
      end do 
      return
      end
