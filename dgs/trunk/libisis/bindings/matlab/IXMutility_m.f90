subroutine IXBgetslice_utility(nlhs, plhs, nrhs, prhs, status)
use IXMmatlab_interface
use IXMslice
implicit none
  integer::nlhs,nrhs
  integer(cpointer_t)::plhs(nlhs),prhs(nrhs)
  character(len=long_len)::filename
  character(len=long_len),allocatable::footer_o(:)
  type(IXTstatus)::status  
  real(dp),pointer::y(:),x(:),pix_o(:,:),e(:),npix(:),c(:)
  real(dp)::header(6)  

  call IXBgetFromBinding(prhs(1),' ',1,0,filename,status)
 
  call IXFget_slice(filename,header,x,y,c,e,npix,pix_o,footer_o,status)
  if (status == IXCseverity_error) then
     plhs(1)=0
  else
     call IXBsendToBinding(plhs(1),  ' ', 1, 0, header , status)
     call IXBsendToBinding(plhs(2),  ' ', 1, 0, x , status)     
     call IXBsendToBinding(plhs(3),  ' ', 1, 0, y , status)     
     call IXBsendToBinding(plhs(4),  ' ', 1, 0, c , status)
     call IXBsendToBinding(plhs(5),  ' ', 1, 0, e , status)     
     call IXBsendToBinding(plhs(6),  ' ', 1, 0, npix , status)     
     call IXBsendToBinding(plhs(7),  ' ', 1, 0, pix_o , status)     
     call IXBsendToBinding(plhs(8),  ' ', 1, 0, footer_o , status)     
  endif

end subroutine IXBgetslice_utility
!
!
!
!
subroutine IXBputslice_utility(nlhs, plhs, nrhs, prhs, status)
use IXMmatlab_interface
use IXMslice
implicit none
  integer::nlhs,nrhs
  integer(cpointer_t)::plhs(nlhs),prhs(nrhs)
  character(len=long_len)::filename
  character(len=long_len),allocatable::footer_i(:)
  type(IXTstatus)::status
  real(dp),pointer::y(:),x(:),pix_i(:,:),e(:),c(:),npix(:)  
  real(dp)::header(6)
  call IXBgetFromBinding(prhs(1),' ',1,0,filename,status)
  call IXBgetFromBinding(prhs(2),' ',1,0,header,status)
  call IXBgetFromBindingPtr(prhs(3),' ',1,0,x,status)
  call IXBgetFromBindingPtr(prhs(4),' ',1,0,y,status)
  call IXBgetFromBindingPtr(prhs(5),' ',1,0,c,status)
  call IXBgetFromBindingPtr(prhs(6),' ',1,0,e,status)
  call IXBgetFromBindingPtr(prhs(7),' ',1,0,npix,status)
  call IXBgetFromBindingPtr(prhs(8),' ',1,0,pix_i,status)
  call IXBgetFromBindingAlloc(prhs(9),' ',1,0,footer_i,status)
    
  call IXFput_slice(filename,header,x,y,c,e,npix,pix_i,footer_i,status)
  if (status == IXCseverity_error) then
     plhs(1)=0
  else
     call IXBsendToBinding(plhs(1),  ' ', 1, 0, 0.0_dp , status)     
  endif

end subroutine IXBputslice_utility
!
!
subroutine IXBputcut_utility(nlhs, plhs, nrhs, prhs, status)
use IXMmatlab_interface
use IXMcut
implicit none
  integer::nlhs,nrhs
  integer(cpointer_t)::plhs(nlhs),prhs(nrhs)
  character(len=long_len)::filename
  character(len=long_len),allocatable::footer_i(:)
  type(IXTstatus)::status  
  real(dp),pointer::y(:),x(:),pix_i(:,:),e(:),npix(:)  
  call IXBgetFromBinding(prhs(1),' ',1,0,filename,status)
  call IXBgetFromBindingPtr(prhs(2),' ',1,0,x,status)
  call IXBgetFromBindingPtr(prhs(3),' ',1,0,y,status)
  call IXBgetFromBindingPtr(prhs(4),' ',1,0,e,status)
  call IXBgetFromBindingPtr(prhs(5),' ',1,0,npix,status)
  call IXBgetFromBindingPtr(prhs(6),' ',1,0,pix_i,status)
  call IXBgetFromBindingAlloc(prhs(7),' ',1,0,footer_i,status)
  
  
  call IXFput_cut(filename,x,y,e,npix,pix_i,footer_i,status)
  if (status == IXCseverity_error) then
     plhs(1)=0
  else
     call IXBsendToBinding(plhs(1),  ' ', 1, 0, 0.0_dp , status)     
  endif

end subroutine IXBputcut_utility
!
subroutine IXBgetcut_utility(nlhs, plhs, nrhs, prhs, status)
use IXMmatlab_interface
use IXMcut
implicit none
  integer::nlhs,nrhs
  integer(cpointer_t)::plhs(nlhs),prhs(nrhs)
  character(len=long_len)::filename
  character(len=long_len),allocatable::footer_o(:)
  type(IXTstatus)::status  
  real(dp),pointer::y(:),x(:),pix_o(:,:),e(:),npix(:)  
  call IXBgetFromBinding(prhs(1),' ',1,0,filename,status)
    
  call IXFget_cut(filename,x,y,e,npix,pix_o,footer_o,status)
  if (status == IXCseverity_error) then
     plhs(1)=0
  else
     call IXBsendToBinding(plhs(1),  ' ', 1, 0, x , status)     
     call IXBsendToBinding(plhs(2),  ' ', 1, 0, y , status)     
     call IXBsendToBinding(plhs(3),  ' ', 1, 0, e , status)     
     call IXBsendToBinding(plhs(4),  ' ', 1, 0, npix , status)     
     call IXBsendToBinding(plhs(5),  ' ', 1, 0, pix_o , status)     
     call IXBsendToBinding(plhs(6),  ' ', 1, 0, footer_o , status)     
  endif

end subroutine IXBgetcut_utility
!!
subroutine IXBgetspe_utility(nlhs, plhs, nrhs, prhs, status)
use IXMmatlab_interface
use IXMspe
implicit none
  integer::nlhs,nrhs
  integer(cpointer_t)::plhs(nlhs),prhs(nrhs)
  character(len=long_len)::filename
  type(IXTstatus)::status
  real(dp),pointer::data_S(:,:),data_ERR(:,:),data_en(:)
  call IXBgetFromBinding(prhs(1),' ',1,0,filename,status)
  
  call IXFget_spe(filename,data_S,data_ERR,data_en,status)
  if (status == IXCseverity_error) then
     plhs(1)=0
     plhs(2)=0
     plhs(3)=0
  else
     call IXBsendToBinding(plhs(1),  ' ', 1, 0,data_S , status)
     call IXBsendToBinding(plhs(2),  ' ', 1, 0,data_ERR , status)
     call IXBsendToBinding(plhs(3),  ' ', 1, 0,data_en , status)
  endif

end subroutine IXBgetspe_utility
!
subroutine IXBputspe_utility(nlhs, plhs, nrhs, prhs, status)
use IXMmatlab_interface
use IXMspe
implicit none
  integer::nlhs,nrhs
  integer(cpointer_t)::plhs(nlhs),prhs(nrhs)
  character(len=long_len)::filename
  type(IXTstatus)::status
  real(dp),pointer::data_S(:,:),data_ERR(:,:),data_en(:)
  call IXBgetFromBinding(prhs(1),' ',1,0,filename,status)
  call IXBgetFromBindingPtr(prhs(2),' ',1,0,data_S,status)
  call IXBgetFromBindingPtr(prhs(3),' ',1,0,data_ERR,status)
  call IXBgetFromBindingPtr(prhs(4),' ',1,0,data_en,status)
  
  call IXFput_spe(filename,data_S,data_ERR,data_en,status)
  if (status == IXCseverity_error) then
     plhs(1)=0
  else
     call IXBsendToBinding(plhs(1),  ' ', 1, 0, 0.0_dp , status)     
  endif

end subroutine IXBputspe_utility
!
subroutine IXBfiletype_utility(nlhs, plhs, nrhs, prhs, status)
  use IXMm_fileio
  implicit none
  integer::nlhs,nrhs
  integer(cpointer_t)::plhs(nlhs),prhs(nrhs)
  
  character(len=long_len)::filename
  integer(i4b)::filetype
  type(IXTstatus)::status
    
  call IXBgetFromBinding(prhs(1),' ',1,0,filename,status)

  filetype=IXFfile_type(filename)
  
  if (status == IXCseverity_error) then
     plhs(1)=0
  else
     call IXBsendToBinding(plhs(1),  ' ', 1, 0, filetype, status)
  endif
  
end subroutine IXBfiletype_utility

subroutine IXBtranslate_read_utility(nlhs, plhs, nrhs, prhs, status)
  use IXMm_path  
  implicit none
  integer::nlhs,nrhs
  integer(cpointer_t)::plhs(nlhs),prhs(nrhs)
  
  character(len=long_len)::filename
  character(long_len)::real_path,path_in
  type(IXTstatus)::status
    
  call IXBgetFromBinding(prhs(1),' ',1,0,path_in,status)  

  real_path=IXFtranslate_read(path_in)
  
  if (status == IXCseverity_error) then
     plhs(1)=0
  else
     call IXBsendToBinding(plhs(1),  ' ', 1, 0,real_path, status)
  endif
  
end subroutine IXBtranslate_read_utility

subroutine IXBtranslate_write_utility(nlhs, plhs, nrhs, prhs, status)
  use IXMm_path
  implicit none
  integer::nlhs,nrhs
  integer(cpointer_t)::plhs(nlhs),prhs(nrhs)
  
  character(len=long_len)::filename
  character(long_len)::real_path,path_in
  type(IXTstatus)::status
    
  call IXBgetFromBinding(prhs(1),' ',1,0,path_in,status)  

  real_path=IXFtranslate_write(path_in,IXC_WRITE)
  
  if (status == IXCseverity_error) then
     plhs(1)=0
  else
     call IXBsendToBinding(plhs(1),  ' ', 1, 0,real_path, status)
  endif
  
end subroutine IXBtranslate_write_utility

subroutine IXBtest_regrid_utility(nlhs, plhs, nrhs, prhs, status)
  use IXMm_base
  implicit none
  integer::nlhs,nrhs
  integer(cpointer_t)::plhs(nlhs),prhs(nrhs)
  
  real(dp),pointer::lambda(:),thetaf(:),lamap(:,:),error_in(:,:)
  integer(i4b)::mapsize
  real(dp)::qxmin,qxmax,qzmin,qzmax,theta_inc
  type(IXTstatus)::status
  real(dp),allocatable::error_out(:,:),offsp(:,:)
      
  call IXBgetFromBindingPtr(prhs(1),' ',1,0,lambda,status)
  call IXBgetFromBindingPtr(prhs(2),' ',1,0,thetaf,status)  
  call IXBgetFromBindingPtr(prhs(3),' ',1,0,lamap,status)  
  call IXBgetFromBindingPtr(prhs(4),' ',1,0,error_in,status)  
  call IXBgetFromBinding(prhs(5),' ',1,0,qxmin,status)
  call IXBgetFromBinding(prhs(6),' ',1,0,qxmax,status)
  call IXBgetFromBinding(prhs(7),' ',1,0,qzmin,status)
  call IXBgetFromBinding(prhs(8),' ',1,0,qzmax,status)
  call IXBgetFromBinding(prhs(9),' ',1,0,theta_inc,status)  
  call IXBgetFromBinding(prhs(10),' ',1,0,mapsize,status)


  allocate(error_out(mapsize,mapsize),offsp(mapsize,mapsize))

  call IXFregrid_ordela(lambda,thetaf,lamap,error_in,qxmin,qxmax,qzmin,qzmax,theta_inc,mapsize,offsp,error_out)
  
  if (status == IXCseverity_error) then
     plhs(1)=0
  else
     call IXBsendToBinding(plhs(1),  ' ', 1, 0,offsp, status)
     call IXBsendToBinding(plhs(2),  ' ', 1, 0,error_out, status)
  endif
  
end subroutine IXBtest_regrid_utility