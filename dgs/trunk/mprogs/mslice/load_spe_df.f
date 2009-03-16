C====================================================================   
C     >> mex load_spe_df.f    
C     This is a MEX-file for MATLAB   
C     to load an ASCII spe file produced by homer/2d on ISIS alpha VMS cluster
C     07-Oct-2000 compiled for PC/WINNT using Matlab 5.3 and
C     Digital Visual Fortran Professional Edition 6
C====================================================================	
      subroutine mexFunction(nlhs, plhs, nrhs, prhs)
C--------------------------------------------------------------------
C     (integer) Replace integer by integer*8 on the DEC Alpha and the
C     SGI 64-bit platforms
      implicit NONE
C declare input/output variables of the mexFunction
      integer plhs(*), prhs(*), nrhs, nlhs
C declare pointers to output variables  
      integer data_S_pr, data_ERR_pr, data_en_pr
C declare external calling functions
      integer mxGetString, mxCreateFull, mxGetM, mxGetN, mxGetPr
      integer mxIsString
C declare local operating variables of the interface funnction
      integer ndet, ne, strlen, status
      character*120 filename

C     Check for proper number of MATLAB input and output arguments 
      if (nrhs .ne. 1) then
         call mexErrMsgTxt('One input <filename> required.')
      elseif (nlhs .ne. 3) then
         call mexErrMsgTxt
     c ('Three outputs (data_S,data_ERR,data_en) required.')
      elseif (mxIsString(prhs(1)) .ne. 1) then
         call mexErrMsgTxt('Input <filename> must be a string.')
      elseif (mxGetM(prhs(1)).ne.1) then
         call mexErrMsgTxt('Input <filename> must be a row vector.')
      end if

C     Get the length of the input string
      strlen=mxGetN(prhs(1))
      if (strlen .gt. 120) then 
         call mexErrMsgTxt 
     c        ('Input <filename> must be less than 120 chars long.')
      end if 
     
C     Get the string contents
      status=mxGetString(prhs(1),filename,strlen)
      if (status .ne. 0) then 
         call mexErrMsgTxt ('Error reading <filename> string.')
      end if 

C     Read ndet and ne values 
      call load_spe_header(ndet,ne,filename)
      if (ndet .lt. 1) then
         call mexErrMsgTxt 
     c ('File not found or error encountered during reading.')
      end if 

C     Create matrices for the return arguments, double precision real*8
      plhs(1)=mxCreateFull(ndet,ne,0)
      data_S_pr=mxGetPr(plhs(1))
      plhs(2)=mxCreateFull(ndet,ne,0)      
      data_ERR_pr=mxGetPr(plhs(2))
      plhs(3)=mxCreateFull(1,ne,0)      
      data_en_pr=mxGetPr(plhs(3))

C     Call load_spe routine, pass pointers
      call load_spe(ndet,ne,%val(data_S_pr), 
     c %val(data_ERR_pr),%val(data_en_pr),filename)

      if (ndet .lt. 1) then
         call mexErrMsgTxt 
     c ('Error encountered during reading the spe file.')
      end if 

      return
      end

C ========================================================
C Read header of spe file, get number of detectors(ndet) 
C and number of energy bins (ne)
      subroutine load_spe_header(ndet,ne,filename)
      implicit NONE
      integer ndet,ne
      character filename*120
      open(unit=1,file=filename,READONLY,ERR=999)
      read(1,*,ERR=999) ndet,ne 
      close(unit=1)  
      return  
  999 ndet=0
      close(unit=1)
      return 
      end    

C=========================================================
C Read spe data 
      subroutine load_spe(ndet,ne,data_S,data_ERR,
     c                    data_en,filename)
      implicit NONE      
      integer ndet,ne,idet,ien,dum(ndet+1)
C     Define pointers to arrays
      double precision data_S(*),data_ERR(*),en(ne+1), data_en(*)
      character filename*120
C Skip over the first two lines with ndet, ne and some text ###        
      open(unit=1,file=filename,READONLY,ERR=999)
      read(1,*,ERR=999) dum(1),dum(2)
C     print*, dum(1),dum(2)
      read(1,*,ERR=999)
C angles (not used)
      read(1,'(8F10.0)',ERR=999) (dum(idet),idet=1,ndet+1)
      read(1,*,ERR=999)
C energy bins
      read(1,'(8F10.0)',ERR=999) (en(ien),ien=1,ne+1)    
C read intensities + errors
      do idet=1,ndet
        read(1,*,ERR=999)
        read(1,'(8F10.0)',ERR=999) (data_S(idet+ndet*(ien-1)),ien=1,ne)
        read(1,*,ERR=999)
        read(1,'(8F10.0)',ERR=999)(data_ERR(idet+ndet*(ien-1)),ien=1,ne)
      enddo
C calculate centres of energy bins      
      do ien=1,ne
	   data_en(ien)=(en(ien)+en(ien+1))/2.0d0
      enddo
      close(unit=1)
      return
 999  ndet=0    ! convention for error reading file
      close(unit=1)
      return
      end

