module IXMm_runfile

#define IXD_TYPE runfile
#include "bindings_header.f90"

contains

#define IXD_TYPE runfile
#include "bindings_base.f90"

end module IXMm_runfile

#define IXD_TYPE runfile
#include "bindings_extra.f90"

subroutine IXBgetmondata_runfile(nlhs, plhs, nrhs, prhs, status)
  use IXMm_runfile
  use IXMm_dataset_2d
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTrunfile) :: runfile
  type(IXTdataset_2d),allocatable::data2d(:)
  call IXBgetFromBinding(prhs(2),' ', 1, 0, runfile,status)
  if (status == IXCseverity_error) return
  call IXFgetmondata_runfile(runfile,data2d,status)
  call IXBsendToBinding(plhs(1), prhs(1),' ', 1, 0, data2d, status)
end subroutine IXBgetmondata_runfile

subroutine IXBgetdetdata_runfile(nlhs, plhs, nrhs, prhs, status)
  use IXMm_runfile
  use IXMm_dataset_2d
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTrunfile) :: runfile
  type(IXTdataset_2d),allocatable::data2d(:)
  call IXBgetFromBinding(prhs(2),' ', 1, 0, runfile,status)
  if (status == IXCseverity_error) return
  call IXFgetdetdata_runfile(runfile,data2d,status)
  call IXBsendToBinding(plhs(1), prhs(1),' ', 1, 0, data2d, status)
end subroutine IXBgetdetdata_runfile

subroutine IXBgeteival_runfile(nlhs, plhs, nrhs, prhs, status)
  use IXMm_runfile
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTrunfile) :: runfile
  real(dp)::eival
  call IXBgetFromBinding(prhs(1),' ', 1, 0, runfile,status)
  if (status == IXCseverity_error) return
  call IXFgeteival_runfile(runfile,eival,status)
  call IXBsendToBinding(plhs(1), ' ', 1, 0, eival, status)
end subroutine IXBgeteival_runfile

subroutine IXBmoncorr_runfile(nlhs, plhs, nrhs, prhs, status)
  use IXMm_runfile
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTrunfile) :: runfile
  real(dp)::abs,energy,limit
  call IXBgetFromBinding(prhs(1),' ', 1, 0, runfile,status)
  call IXBgetFromBinding(prhs(2),' ', 1, 0, energy,status)
  call IXBgetFromBinding(prhs(3),' ', 1, 0, limit,status)
  if (status == IXCseverity_error) return
  call IXFmoncorr_runfile(runfile,abs,energy,limit,status)
  call IXBsendToBinding(plhs(1), ' ', 1, 0,abs, status)
end subroutine IXBmoncorr_runfile


subroutine IXBpopulate_mon_runfile(nlhs,plhs,nrhs,prhs,status)
  use IXMm_runfile
  use IXMm_data_source
  use IXMm_axis
  use IXMm_options
  implicit none
  integer::nlhs,nrhs
  integer(cpointer_t)::plhs(nlhs),prhs(nrhs)
  type(IXTrunfile), allocatable :: rf(:)
  type(IXTdata_source)::dso
  type(IXTstatus)::status
  integer(i4b):: n,period
  type(IXToptions)::opt
  type(IXTaxis)::m_axis
  real(dp),pointer::m_rebin(:)
  real(dp)::ei(2)
  
  n = ixGetNumberOfElements(prhs(2))
  if(n > 1)then
    call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
    IXCerr_invparam, 'no multiple runfiles allowed yet (IXBpopulate_runfile)')
  endif
  if (status == IXCseverity_error) return
  
  allocate(rf(n))
  call IXBgetFromBinding(prhs(2),' ',1,0,rf(1),status)
  call IXBgetFromBinding(prhs(3),' ',1,0,dso,status)
  call IXBgetFromBinding(prhs(4),' ',1,0,period,status)  
  call IXBgetFromBinding(prhs(5),' ',1,0,ei,status)
  call IXBgetfromBinding(prhs(6),' ',1,0,m_axis,status)
  call IXBgetFromBindingPtr(prhs(7),' ',1,0,m_rebin,status)   
  call IXBgetFromBinding(prhs(8),' ',1,0,opt,status)

  call IXFpopulate_mon_runfile(rf(1),status,dso,period,ei,m_axis,m_rebin,opt)
  
  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, rf(1), status)
  endif
  
  deallocate(rf)
  
end subroutine IXBpopulate_mon_runfile

subroutine IXBpopulate_det_runfile(nlhs,plhs,nrhs,prhs,status)
  use IXMm_runfile
  use IXMm_data_source
  use IXMm_axis
  use IXMm_options
  implicit none
  integer::nlhs,nrhs
  integer(cpointer_t)::plhs(nlhs),prhs(nrhs)
  type(IXTrunfile), allocatable :: rf(:)
  type(IXTdata_source)::dso
  type(IXTstatus)::status
  integer(i4b):: n,period,nchunk(2)
  type(IXToptions)::opt
  type(IXTaxis)::d_axis
  real(dp),pointer::d_rebin(:),bgrd(:),i_lim(:)
  real(dp)::ei(2)
  
  n = ixGetNumberOfElements(prhs(2))
  if(n > 1)then
    call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
    IXCerr_invparam, 'no multiple runfiles allowed yet (IXBpopulate_runfile)')
  endif
  if (status == IXCseverity_error) return
  
  allocate(rf(n))
  call IXBgetFromBinding(prhs(2),' ',1,0,rf(1),status)
  call IXBgetFromBinding(prhs(3),' ',1,0,dso,status)
  call IXBgetFromBinding(prhs(4),' ',1,0,period,status)
  call IXBgetFromBinding(prhs(5),' ',1,0,ei,status)  
  call IXBgetfromBinding(prhs(6),' ',1,0,d_axis,status)
  call IXBgetFromBindingPtr(prhs(7),' ',1,0,d_rebin,status)   
  call IXBgetFromBindingPtr(prhs(8),' ',1,0,bgrd,status)
  call IXBgetfromBindingPtr(prhs(9),' ',1,0,i_lim,status) 
  call IXBgetFromBinding(prhs(10),' ',1,0,opt,status)
  call IXBgetFromBinding(prhs(11),' ',1,0,nchunk,status)

  call IXFpopulate_det_runfile(rf(1),status,dso,period,nchunk,ei,d_axis,d_rebin,bgrd,i_lim,opt)
  
  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, rf(1), status)
  endif
  
  deallocate(rf)
  
end subroutine IXBpopulate_det_runfile

subroutine IXBpopulate_dso_runfile(nlhs,plhs,nrhs,prhs,status)
  use IXMm_runfile
  use IXMm_data_source
  use IXMm_axis
  use IXMm_options
  implicit none
  integer::nlhs,nrhs
  integer(cpointer_t)::plhs(nlhs),prhs(nrhs)
  type(IXTrunfile), allocatable :: rf(:)
  type(IXTdata_source)::dso
  type(IXTstatus)::status
  integer(i4b):: n,period,nchunk(2)
  type(IXToptions)::opt
  type(IXTaxis)::m_axis,d_axis
  real(dp),pointer::m_rebin(:),d_rebin(:),bgrd(:),i_lim(:)
  real(dp)::ei(2)
  integer(i4b)::monei_info(3)
  
  n = ixGetNumberOfElements(prhs(2))
  if(n > 1)then
    call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
    IXCerr_invparam, 'no multiple runfiles allowed yet (IXBpopulate_runfile)')
  endif
  if (status == IXCseverity_error) return
  
  allocate(rf(n))
  call IXBgetFromBinding(prhs(2),' ',1,0,rf(1),status)
  call IXBgetFromBinding(prhs(3),' ',1,0,dso,status)
  call IXBgetFromBinding(prhs(4),' ',1,0,period,status)
  call IXBgetFromBinding(prhs(5),' ',1,0,ei,status)
  call IXBgetFromBinding(prhs(6),' ',1,0,monei_info,status)  
  call IXBgetfromBinding(prhs(7),' ',1,0,m_axis,status)
  call IXBgetFromBindingPtr(prhs(8),' ',1,0,m_rebin,status)
  call IXBgetfromBinding(prhs(9),' ',1,0,d_axis,status)
  call IXBgetFromBindingPtr(prhs(10),' ',1,0,d_rebin,status) 
  call IXBgetfromBindingPtr(prhs(11),' ',1,0,i_lim,status)
  call IXBgetFromBindingPtr(prhs(12),' ',1,0,bgrd,status)
  call IXBgetFromBinding(prhs(13),' ',1,0,opt,status)
  call IXBgetFromBinding(prhs(14),' ',1,0,nchunk,status)

  call IXFpopulate_runfile(rf(1),status,dso,period,nchunk,monei_info,ei,m_axis,m_rebin,d_axis,d_rebin,i_lim,bgrd,opt)
  
  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, rf(1), status)
  endif
  
  deallocate(rf)
  
end subroutine IXBpopulate_dso_runfile

subroutine IXBunits_runfile(nlhs,plhs,nrhs,prhs,status)
  use IXMm_runfile
  use IXMm_axis
  implicit none
  integer::nlhs,nrhs
  integer(cpointer_t)::plhs(nlhs),prhs(nrhs)
  type(IXTrunfile), allocatable :: rf(:)
  type(IXTaxis)::axis_out
  type(IXTstatus)::status
  integer(i4b):: n

  
  n = ixGetNumberOfElements(prhs(2))
  if(n > 1)then
    call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
    IXCerr_invparam, 'no multiple runfiles allowed yet (IXBpopulate_runfile)')
  endif
  if (status == IXCseverity_error) return
  call IXBgetFromBinding(prhs(3),' ',1,0,axis_out,status)
  allocate(rf(n))
  call IXBgetFromBinding(prhs(2),' ',1,0,rf(1),status)
!  call IXFcheck(axis_out,status)
  call IXFunits_runfile(rf(1),status,axis_out)
  
  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, rf(1), status)
  endif
    
  deallocate(rf)
  
end subroutine IXBunits_runfile

subroutine IXBunitsrebin_runfile(nlhs,plhs,nrhs,prhs,status)
  use IXMm_runfile
  use IXMm_axis
  implicit none
  integer::nlhs,nrhs
  integer(cpointer_t)::plhs(nlhs),prhs(nrhs)
  type(IXTrunfile), allocatable :: rf(:)
  real(dp),pointer::params(:)
  type(IXTaxis)::axis_out
  type(IXTstatus)::status
  integer(i4b):: n

  n = ixGetNumberOfElements(prhs(2))
  if(n > 1)then
    call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
    IXCerr_invparam, 'no multiple runfiles allowed yet (IXBpopulate_runfile)')
  endif
  if (status == IXCseverity_error) return
  call IXBgetFromBinding(prhs(3),' ',1,0,axis_out,status)
  allocate(rf(n))
  call IXBgetFromBinding(prhs(2),' ',1,0,rf(1),status)
  call IXBgetFromBindingPtr(prhs(4),' ',1,0,params,status)  
  
!  call IXFcheck(axis_out,status)
  call IXFunits_runfile(rf(1),status,axis_out,params)
  
  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, rf(1), status)
  endif
  
  deallocate(rf)
  
end subroutine IXBunitsrebin_runfile





subroutine IXBbackground_runfile(nlhs,plhs,nrhs,prhs,status)
  use IXMm_runfile
  implicit none
  integer::nlhs,nrhs
  integer(cpointer_t)::plhs(nlhs),prhs(nrhs)
  type(IXTrunfile), allocatable :: rf(:)
  real(dp)::bmin,bmax
  type(IXTstatus)::status
  integer(i4b):: n

  
  n = ixGetNumberOfElements(prhs(2))
  if(n > 1)then
    call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
    IXCerr_invparam, 'no multiple runfiles allowed yet (IXBpopulate_runfile)')
  endif
  if (status == IXCseverity_error) return
  call IXBgetFromBinding(prhs(3),' ',1,0,bmin,status)
  call IXBgetFromBinding(prhs(4),' ',1,0,bmax,status)
  allocate(rf(n))
  call IXBgetFromBinding(prhs(2),' ',1,0,rf(1),status)
  call IXFbackground_runfile(rf(1),bmin,bmax,status)
  
  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, rf(1), status)
  endif
  
  deallocate(rf)

end subroutine IXBbackground_runfile

subroutine IXBremap_runfile(nlhs,plhs,nrhs,prhs,status)
  use IXMm_runfile
  use IXMm_data_source
  implicit none
  integer::nlhs,nrhs
  integer(cpointer_t)::plhs(nlhs),prhs(nrhs)
  type(IXTrunfile), allocatable :: rf(:)
  type(IXTdata_source)::dso
  type(IXTstatus)::status
  integer(i4b):: n
  
  n = ixGetNumberOfElements(prhs(2))
  if(n > 1)then
    call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
    IXCerr_invparam, 'no multiple runfiles allowed yet (IXBpopulate_runfile)')
  endif
  if (status == IXCseverity_error) return
  
  allocate(rf(n))
  call IXBgetFromBinding(prhs(2),' ',1,0,rf(1),status)
  call IXBgetFromBinding(prhs(3),' ',1,0,dso,status)

  call IXFremap_runfile(rf(1),dso,status)
  
  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, rf(1), status)
  endif
  
  deallocate(rf)
  
end subroutine IXBremap_runfile
  

subroutine IXBtubes_to_spectra_runfile(nlhs,plhs,nrhs,prhs,status)
  use IXMm_runfile
  use IXMm_mask  
  implicit none
  integer::nlhs,nrhs
  integer(cpointer_t)::plhs(nlhs),prhs(nrhs)
  type(IXTrunfile), allocatable :: rf(:)
  type(IXTdata_source)::dso
  type(IXTstatus)::status
  type(IXTmask)::tubes,spectra
  integer(i4b):: n
  
  n = ixGetNumberOfElements(prhs(2))
  if(n > 1)then
    call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
    IXCerr_invparam, 'no multiple runfiles allowed yet (IXBpopulate_runfile)')
  endif
  if (status == IXCseverity_error) return
  
  allocate(rf(n))
  call IXBgetFromBinding(prhs(2),' ',1,0,rf(1),status)
  call IXBgetFromBinding(prhs(3),' ',1,0,tubes,status)

  call IXFtubes_to_spectra_runfile(rf(1),tubes,spectra,status)
  
  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, spectra, status)
  endif
  
  deallocate(rf)
  
end subroutine IXBtubes_to_spectra_runfile


subroutine IXBgetei_runfile(nlhs,plhs,nrhs,prhs,status)
  use IXMm_runfile
  implicit none
  integer::nlhs,nrhs
  integer(cpointer_t)::plhs(nlhs),prhs(nrhs)
  type(IXTrunfile), allocatable :: rf(:)
  type(IXTstatus)::status
  real(dp)::Ei,ei_extras(6)
  integer(i4b):: n,monitor_no(2)
  
  n = ixGetNumberOfElements(prhs(2))
  if(n > 1)then
    call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
    IXCerr_invparam, 'no multiple runfiles allowed yet (IXBpopulate_runfile)')
  endif
  if (status == IXCseverity_error) return
  
  allocate(rf(n))
  call IXBgetFromBinding(prhs(1),' ',1,0,rf(1),status)
  call IXBgetFromBinding(prhs(2),' ',1,0,Ei,status)
  call IXBgetFromBinding(prhs(3),' ',1,0,monitor_no,status)

  call IXFgetei_runfile(rf(1),Ei,ei_extras,monitor_no,status)
  
  if (status == IXCseverity_error) then
     Ei=0.0_dp
     call IXBsendToBinding(plhs(1), ' ', 1, 0,Ei, status)
     ei_extras=0.0_dp
  else
     call IXBsendToBinding(plhs(1), ' ', 1, 0,Ei, status)
     call IXBsendToBinding(plhs(2), ' ', 1, 0,ei_extras, status)
  endif
  
  deallocate(rf)
  
end subroutine IXBgetei_runfile

subroutine IXBmonovan_runfile(nlhs,plhs,nrhs,prhs,status)
  use IXMm_runfile
  implicit none
  integer::nlhs,nrhs
  integer(cpointer_t)::plhs(nlhs),prhs(nrhs)
  type(IXTrunfile), allocatable :: rf(:)
  type(IXTstatus)::status
  real(dp)::tiny,huge,out_lo,out_hi,v_lo,v_hi,v_sig,abs_val
  logical::zero
  integer(i4b):: n
  
  n = ixGetNumberOfElements(prhs(2))
  if(n > 1)then
    call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
    IXCerr_invparam, 'no multiple runfiles allowed yet (IXBpopulate_runfile)')
  endif
  if (status == IXCseverity_error) return
  
  allocate(rf(n))
  call IXBgetFromBinding(prhs(1),' ',1,0,rf(1),status)
  call IXBgetFromBinding(prhs(2),' ',1,0,zero,status)
  call IXBgetFromBinding(prhs(3),' ',1,0,tiny,status)
  call IXBgetFromBinding(prhs(4),' ',1,0,huge,status)
  call IXBgetFromBinding(prhs(5),' ',1,0,out_lo,status)
  call IXBgetFromBinding(prhs(6),' ',1,0,out_hi,status)
  call IXBgetFromBinding(prhs(7),' ',1,0,v_lo,status)
  call IXBgetFromBinding(prhs(8),' ',1,0,v_hi,status)
  call IXBgetFromBinding(prhs(9),' ',1,0,v_sig,status)


  call IXFmonovan_runfile(rf(1),zero,tiny,huge,out_lo,out_hi,v_lo,v_hi,v_sig,abs_val,status)  
  if (status == IXCseverity_error) then
     abs_val=0.0_dp
  else
     call IXBsendToBinding(plhs(1), ' ', 1, 0,abs_val, status)
  endif 
  deallocate(rf)  
end subroutine IXBmonovan_runfile

subroutine IXBsolid_runfile(nlhs,plhs,nrhs,prhs,status)
  use IXMm_runfile
  use IXMm_data_source
  implicit none
  integer::nlhs,nrhs
  integer(cpointer_t)::plhs(nlhs),prhs(nrhs)
  type(IXTrunfile), allocatable :: rf(:)
  type(IXTrunfile)::wbrf
  type(IXTdata_source)::dso
  type(IXTstatus)::status
  integer(i4b):: n
  
  n = ixGetNumberOfElements(prhs(2))
  if(n > 1)then
    call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
    IXCerr_invparam, 'no multiple runfiles allowed yet (IXBpopulate_runfile)')
  endif
  if (status == IXCseverity_error) return
  
  allocate(rf(n))
  call IXBgetFromBinding(prhs(2),' ',1,0,rf(1),status)
  call IXBgetFromBinding(prhs(3),' ',1,0,dso,status)
  call IXBgetFromBinding(prhs(4),' ',1,0,wbrf,status)
  call IXFsolid_runfile(rf(1),dso,wbrf,status)
  
  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, rf(1), status)
  endif
  
  deallocate(rf)
  
end subroutine IXBsolid_runfile


subroutine IXBmon_norm_runfile(nlhs,plhs,nrhs,prhs,status)
  use IXMm_runfile
  implicit none
  integer::nlhs,nrhs
  integer(cpointer_t)::plhs(nlhs),prhs(nrhs)
  type(IXTrunfile), allocatable :: rf(:)
  real(dp)::limits(2),scale
  integer(i4b)::wk_ind
  type(IXTstatus)::status
  integer(i4b):: n
  
  n = ixGetNumberOfElements(prhs(2))
  if(n > 1)then
    call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
    IXCerr_invparam, 'no multiple runfiles allowed yet (IXBpopulate_runfile)')
  endif
  if (status == IXCseverity_error) return
  
  allocate(rf(n))
  call IXBgetFromBinding(prhs(2),' ',1,0,rf(1),status)
  call IXBgetFromBinding(prhs(3),' ',1,0,wk_ind,status)
  call IXBgetFromBinding(prhs(4),' ',1,0,limits,status)
  call IXBgetFromBinding(prhs(5),' ',1,0,scale,status)  
  call IXFmon_norm_runfile(rf(1),wk_ind,limits,scale,status)  
  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, rf(1), status)
  endif  
  deallocate(rf)
  
end subroutine IXBmon_norm_runfile


subroutine IXBpeak_norm_runfile(nlhs,plhs,nrhs,prhs,status)
  use IXMm_runfile
  implicit none
  integer::nlhs,nrhs
  integer(cpointer_t)::plhs(nlhs),prhs(nrhs)
  type(IXTrunfile), allocatable :: rf(:)
  integer(i4b)::wk_ind
  real(dp)::ei,scale
  type(IXTstatus)::status
  integer(i4b):: n
  
  n = ixGetNumberOfElements(prhs(2))
  if(n > 1)then
    call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
    IXCerr_invparam, 'no multiple runfiles allowed yet (IXBpopulate_runfile)')
  endif
  if (status == IXCseverity_error) return
  
  allocate(rf(n))
  call IXBgetFromBinding(prhs(2),' ',1,0,rf(1),status)
  call IXBgetFromBinding(prhs(3),' ',1,0,wk_ind,status)
  call IXBgetFromBinding(prhs(4),' ',1,0,ei,status)
  call IXBgetFromBinding(prhs(5),' ',1,0,scale,status)

  call IXFpeak_norm_runfile(rf(1),wk_ind,ei,scale,status)
  
  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, rf(1), status)
  endif
  
  deallocate(rf)
  
end subroutine IXBpeak_norm_runfile

subroutine IXBcharge_norm_runfile(nlhs,plhs,nrhs,prhs,status)
  use IXMm_runfile
  implicit none
  integer::nlhs,nrhs
  integer(cpointer_t)::plhs(nlhs),prhs(nrhs)
  type(IXTrunfile), allocatable :: rf(:)
  real(dp)::scale
  type(IXTstatus)::status
  integer(i4b):: n
  
  n = ixGetNumberOfElements(prhs(2))
  if(n > 1)then
    call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
    IXCerr_invparam, 'no multiple runfiles allowed yet (IXBcharge_norm_runfile)')
  endif
  if (status == IXCseverity_error) return
  
  allocate(rf(n))
  call IXBgetFromBinding(prhs(2),' ',1,0,rf(1),status)
  call IXBgetFromBinding(prhs(3),' ',1,0,scale,status)

  call IXFcharge_norm_runfile(rf(1),scale,status)
  
  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, rf(1), status)
  endif
  
  deallocate(rf)
  
end subroutine IXBcharge_norm_runfile

subroutine IXBabsolute_norm_runfile(nlhs,plhs,nrhs,prhs,status)
  use IXMm_runfile
  implicit none
  integer::nlhs,nrhs
  integer(cpointer_t)::plhs(nlhs),prhs(nrhs)
  type(IXTrunfile), allocatable :: rf(:)
  real(dp)::factor
  type(IXTstatus)::status
  integer(i4b):: n
  
  n = ixGetNumberOfElements(prhs(2))
  if(n > 1)then
    call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
    IXCerr_invparam, 'no multiple runfiles allowed yet (IXBabsolute_norm_runfile)')
  endif
  if (status == IXCseverity_error) return
  
  allocate(rf(n))
  call IXBgetFromBinding(prhs(2),' ',1,0,rf(1),status)
  call IXBgetFromBinding(prhs(3),' ',1,0,factor,status)

  call IXFabsolute_norm_runfile(rf(1),factor,status)
  
  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, rf(1), status)
  endif
  
  deallocate(rf)
  
end subroutine IXBabsolute_norm_runfile

subroutine IXBcorr_runfile(nlhs,plhs,nrhs,prhs,status)
  use IXMm_runfile
  implicit none
  integer::nlhs,nrhs
  integer(cpointer_t)::plhs(nlhs),prhs(nrhs)
  type(IXTrunfile), allocatable :: rf(:)
  type(IXTstatus)::status
  integer(i4b):: n
  
  n = ixGetNumberOfElements(prhs(2))
  if(n > 1)then
    call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
    IXCerr_invparam, 'no multiple runfiles allowed yet (IXBeffic_norm_runfile)')
  endif
  if (status == IXCseverity_error) return
  
  allocate(rf(n))
  call IXBgetFromBinding(prhs(2),' ',1,0,rf(1),status)

  call IXFcorr_runfile(rf(1),status)
  
  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, rf(1), status)
  endif
  
  deallocate(rf)
  
end subroutine IXBcorr_runfile

subroutine IXBdiag_runfile(nlhs,plhs,nrhs,prhs,status)
  use IXMm_runfile
  use IXMm_map
  use IXMm_mask
  implicit none
  integer::nlhs,nrhs
  integer(cpointer_t)::plhs(nlhs),prhs(nrhs)
  type(IXTrunfile), allocatable :: rf(:)
  type(IXTrunfile)::rf_v1,rf_v2,rf_ZC
  type(IXTmap)::bank
  type(IXTmask)::hmask,out_mask
  integer(i4b),allocatable::cause(:),c_file(:)
  real(dp),allocatable::c_value(:),c_error(:)
  type(IXTstatus)::status
  integer(i4b):: n
  real(dp)::s_out_lo,s_out_hi,sv_lo,sv_hi,sv_sig,tiny,huge
  real(dp)::v_out_lo,v_out_hi,vv_lo,vv_hi,vv_sig,r,r_sig    
  logical::v_zero,s_zero
  
  
  n = ixGetNumberOfElements(prhs(2))
  if(n > 1)then
    call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
    IXCerr_invparam, 'no multiple runfiles allowed yet (IXBeffic_norm_runfile)')
  endif
  if (status == IXCseverity_error) return
  
  allocate(rf(n))
  call IXBgetFromBinding(prhs(2),' ',1,0,rf(1),status)
  call IXBgetFromBinding(prhs(3),' ',1,0,rf_v1,status)
  call IXBgetFromBinding(prhs(4),' ',1,0,tiny ,status)
  call IXBgetFromBinding(prhs(5),' ',1,0,huge ,status)
  call IXBgetFromBinding(prhs(6),' ',1,0,s_zero ,status)
  call IXBgetFromBinding(prhs(7),' ',1,0,s_out_lo ,status)
  call IXBgetFromBinding(prhs(8),' ',1,0,s_out_hi ,status)
  call IXBgetFromBinding(prhs(9),' ',1,0,sv_lo ,status)
  call IXBgetFromBinding(prhs(10),' ',1,0,sv_hi ,status)
  call IXBgetFromBinding(prhs(11),' ',1,0,sv_sig ,status)
  call IXBgetFromBinding(prhs(12),' ',1,0,v_zero ,status)
  call IXBgetFromBinding(prhs(13),' ',1,0,v_out_lo ,status)
  call IXBgetFromBinding(prhs(14),' ',1,0,v_out_hi ,status)
  call IXBgetFromBinding(prhs(15),' ',1,0,vv_lo ,status)
  call IXBgetFromBinding(prhs(16),' ',1,0,vv_hi ,status)
  call IXBgetFromBinding(prhs(17),' ',1,0,vv_sig ,status)
  call IXBgetFromBinding(prhs(18),' ',1,0,r ,status)
  call IXBgetFromBinding(prhs(19),' ',1,0,r_sig ,status)  
  
  ! rf_v2/bank/hmask are 'optional' variables, will check for existence using if(VALID(obj))construct
  call IXBgetFromBinding(prhs(20),' ',1,0,rf_v2,status)
  call IXBgetFromBinding(prhs(21),' ',1,0,bank,status)
  call IXBgetFromBinding(prhs(22),' ',1,0,hmask,status)
  call IXBgetFromBinding(prhs(23),' ',1,0,rf_ZC,status)

  call IXFdiag_runfile(rf(1),rf_v1,tiny,huge,s_zero,s_out_lo,s_out_hi,sv_lo,sv_hi,sv_sig,  &
                                             v_zero,v_out_lo,v_out_hi,vv_lo,vv_hi,vv_sig,  &
                       out_mask,cause,c_value,c_error,c_file,r,r_sig,status,rf_v2,bank,hmask,rf_ZC)
  
  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
  !blank mask is supplied as output LHS variable  
     call IXBsendToBinding(plhs(1),prhs(1), ' ', 1, 0, out_mask, status)
     call IXBsendToBinding(plhs(2), ' ', 1, 0, cause, status)
     call IXBsendToBinding(plhs(3), ' ', 1, 0, c_value, status)
     call IXBsendToBinding(plhs(4), ' ', 1, 0, c_error, status)
     call IXBsendToBinding(plhs(5), ' ', 1, 0, c_file, status)
  endif
  
  deallocate(rf)
  
end subroutine IXBdiag_runfile
#define IXD_TYPE	runfile
#define IXD_NAME	Plus_runfile
#include "binary_ops.f90"

#define IXD_TYPE	runfile
#define IXD_NAME	Minus_runfile
#include "binary_ops.f90"

#define IXD_TYPE	runfile
#define IXD_NAME	Times_runfile
#include "binary_ops.f90"

#define IXD_TYPE	runfile
#define IXD_NAME	Divide_runfile
#include "binary_ops.f90"

#define IXD_TYPE	runfile
#define IXD_NAME	Power_runfile
#include "binary_ops.f90"

#define IXD_TYPE	runfile
#define IXD_NAME	Log_runfile
#include "unary_ops.f90"

#define IXD_TYPE	runfile
#define IXD_NAME	Log10_runfile
#include "unary_ops.f90"

#define IXD_TYPE	runfile
#define IXD_NAME	Exp_runfile
#include "unary_ops.f90"

#define IXD_TYPE	runfile
#define IXD_NAME	Sin_runfile
#include "unary_ops.f90"

#define IXD_TYPE	runfile
#define IXD_NAME	Cos_runfile
#include "unary_ops.f90"

#define IXD_TYPE	runfile
#define IXD_NAME	Tan_runfile
#include "unary_ops.f90"

#define IXD_TYPE	runfile
#define IXD_NAME	Sinh_runfile
#include "unary_ops.f90"

#define IXD_TYPE	runfile
#define IXD_NAME	Cosh_runfile
#include "unary_ops.f90"

#define IXD_TYPE	runfile
#define IXD_NAME	Tanh_runfile
#include "unary_ops.f90"

#define IXD_TYPE	runfile
#define IXD_NAME	deriv1x_runfile
#include "unary_ops.f90"

#define IXD_TYPE	runfile
#define IXD_NAME	deriv2x_runfile
#include "unary_ops.f90"


#define REBIN_OPERATION rebin_x_det_runfile
#include "advanced_unary_ops.f90"

#define REBIN_OPERATION rebin_x_mon_runfile
#include "advanced_unary_ops.f90"

#define REBIN_OPERATION rebin_x_runfile
#include "advanced_unary_ops.f90"

#define VALUE_TYPE real(dp),pointer
#define ARRAYVALUE_OPERATION regroup_x_det_runfile
#include "advanced_unary_ops.f90"

#define VALUE_TYPE real(dp),pointer
#define ARRAYVALUE_OPERATION regroup_x_mon_runfile
#include "advanced_unary_ops.f90"

#define VALUE_TYPE real(dp),pointer
#define ARRAYVALUE_OPERATION regroup_x_runfile
#include "advanced_unary_ops.f90"

#define VALUE_TYPE integer(i4b)
#define SINGLEVALUE_OPERATION rebunch_x_det_runfile
#include "advanced_unary_ops.f90"

#define VALUE_TYPE integer(i4b)
#define SINGLEVALUE_OPERATION rebunch_x_mon_runfile
#include "advanced_unary_ops.f90"

#define VALUE_TYPE integer(i4b)
#define SINGLEVALUE_OPERATION rebunch_x_runfile
#include "advanced_unary_ops.f90"

#define VALUE_TYPE real(dp)
#define SINGLEVALUE_OPERATION shift_x_runfile
#include "advanced_unary_ops.f90"



#define INT_OPERATION integrate_x_mon_runfile
#include "advanced_unary_ops.f90"

#define INT_OPERATION integrate_x_det_runfile
#include "advanced_unary_ops.f90"

#define INT_OPERATION integrate_x_runfile
#include "advanced_unary_ops.f90"
