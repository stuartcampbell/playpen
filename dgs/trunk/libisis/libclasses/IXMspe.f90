module IXMspe
  use IXMtype_definitions
  use IXMstatus
  use IXMmemory
contains

  subroutine IXFget_spe(filename,data_S,data_ERR,data_en,status)
  implicit none
  real(dp),pointer::data_S(:,:),data_ERR(:,:),data_en(:)
  type(IXTstatus)::status
  character(len=*),intent(in)::filename
  integer(i4b)::ndet,ne
  
  call load_spe_header(ndet,ne,filename,status)
  if(status == IXCseverity_error)return
  
  call IXFalloc(data_en,ne+1,status)
  call IXFallocdims(data_S,(/ ne,ndet /),status)
  call IXFallocdims(data_ERR,(/ ne,ndet /),status)
  
  call load_spe(ndet,ne,data_S,data_ERR,data_en,filename,status)
   
  end subroutine IXFget_spe

!  call IXFput_spe(filename,data_S,data_ERR,data_en,status)
      
      subroutine IXFput_spe(filename,data_S,data_ERR,data_en,status)
      implicit none      
      real(dp),pointer,intent(in)::data_S(:,:),data_ERR(:,:),data_en(:)
      type(IXTstatus)::status
!      double precision data_S(ne,ndet),data_ERR(ne,ndet),data_en(ne+1), err
      character(len=*):: filename
      integer(i4b):: ndet,ne,k,idet
      real(dp)::err
      err=0.0d0

      ne=size(data_en)-1
      ndet=size(data_S,2)
! Skip over the first two lines with ndet, ne and some text ###        
      open(unit=1,file=filename,status='REPLACE',ERR=999)
      write(1,'(2i8)',ERR=999) ndet,ne
! angles (not used)
      write (1,'(a)',ERR=999) '### Phi Grid'
      write (1,100,ERR=999) (dble(k)-0.5d0,k=1,ndet+1)
! energy bins
      write (1,'(a)',ERR=999) '### Energy Grid'
      write (1,100,ERR=999) (data_en(k),k=1,ne+1)    
! read intensities + errors
      do idet = 1, ndet
          write (1,'(a)',ERR=999) '### S(Phi,w)'
          write (1,100,ERR=999) (data_S(k,idet), k=1,ne)
          write (1,'(a)',ERR=999) '### Errors'
          write (1,100,ERR=999) (data_ERR(k,idet), k=1,ne)      
      end do
 100  format(1p,8e10.3)
      close(unit=1)
      return

 999  err=1.0d0    ! convention for error writing file
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'error writing spe (IXFput_spe)')

      close(unit=1)
      return
      end subroutine IXFput_spe


! Read header of spe file, get number of detectors(ndet) 
! and number of energy bins (ne)
      subroutine load_spe_header(ndet,ne,filename,status)
      implicit none
      integer(i4b):: ndet,ne
      character(len=*),intent(in):: filename
      type(IXTstatus)::status

      open(unit=1,file=filename,action='READ',ERR=999)
      read(1,*,ERR=999) ndet,ne 
      close(unit=1)  
      return  

  999 ndet=0    ! convention for error reading file
      call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'error reading spe file header (load_spe_header)')

      close(unit=1)
      return 
      end subroutine   

!-----------------------------------------------------------------------
! Read spe data 
      subroutine load_spe(ndet,ne,data_S,data_ERR,data_en,filename,status)
      implicit none      
      integer(i4b):: ndet,ne,idet,ien,dum(ndet+1)
!     Define pointers to arrays
      real(dp),intent(inout):: data_S(ne,ndet),data_ERR(ne,ndet),data_en(ne+1)
      character(len=*),intent(in):: filename
      type(IXTstatus)::status

! Skip over the first two lines with ndet, ne and some text ###        
      open(unit=1,file=filename,action='READ',ERR=999)
      read(1,*,ERR=999) dum(1),dum(2)
      read(1,*,ERR=999)
! angles (not used)
      read(1,'(8F10.0)',ERR=999) (dum(idet),idet=1,ndet+1)
      read(1,*,ERR=999)
! energy bins
      read(1,'(8F10.0)',ERR=999) (data_en(ien),ien=1,ne+1)    
! read intensities + errors
      do idet=1,ndet
          read(1,*,ERR=999)
          read(1,'(8F10.0)',ERR=999) (data_S(ien,idet),ien=1,ne)
          read(1,*,ERR=999)
          read(1,'(8F10.0)',ERR=999)(data_ERR(ien,idet),ien=1,ne)
      enddo
      close(unit=1)
      return

 999  ndet=0    ! convention for error reading file
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'Error encountered during reading the spe file. (load_spe)')

      close(unit=1)
      return
      end subroutine load_spe
end module IXMspe