! IXD_NAME IXD_TYPE IXD_OPERATION

#if defined(IXD_NAME) && defined(IXD_TYPE) && defined(IXD_OPERATION)

subroutine IXF&/**/
              &IXD_NAME (wres,w1,s)
    implicit none
    type(IXT&/**/
            &IXD_TYPE ),intent(in) :: w1
    type(IXT&/**/
            &IXD_TYPE ),intent(out) :: wres             
    type(IXTstatus) :: s
	call setup_unary_op_&/**/
	                    &IXD_TYPE (wres, w1, s)
    if (s == IXCseverity_error) return
	call IXFarray&/**/
	       &IXD_OPERATION (wres%signal,wres%error, w1%signal,w1%error,s)
	call finish_op_&/**/
	               &IXD_TYPE (wres,w1,s)
end subroutine

#undef IXD_NAME 
#undef IXD_TYPE 
#undef IXD_OPERATION
#endif /* defined(IXD_NAME) && defined(IXD_TYPE) && defined(IXD_OPERATION) */

#if defined(RUNFILE_OPERATION) 
 subroutine IXF&/**/
             &RUNFILE_OPERATION&/**/
             &_runfile(rfile_out,rfile,status)
    implicit none
    type(IXTrunfile),intent(in)::rfile    
    type(IXTrunfile),intent(out)::rfile_out
    type(IXTstatus),intent(inout)::status
    if( .not. IXFvalid(rfile) )then
      call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
      IXCerr_invparam,'Fatal error, invalid runfile object supplied(IXFoperation_runfile)')
    endif    
    if(IXFvalid(rfile%mon_data))call IXF&/**/
             &RUNFILE_OPERATION(rfile_out%mon_data,rfile%mon_data,status)

    if(IXFvalid(rfile%det_data))call IXF&/**/
             &RUNFILE_OPERATION(rfile_out%det_data,rfile%det_data,status)
! rfile2 information is copied         
    call finish_op_runfile(rfile_out,rfile,status)    
  end subroutine 

#undef RUNFILE_OPERATION
#endif /* defined(RUNFILE_OPERATION) */

#if defined(DATA_OPERATION) 
  subroutine IXF&/**/
             &DATA_OPERATION&/**/
             &_data(data_out,data_in2,status)
    implicit none
    type(IXTdata),intent(out)::data_out
    type(IXTdata),intent(in)::data_in2
    type(IXTstatus),intent(inout)::status
    integer(i4b)::i,d2_len
    
    d2_len=size(data_in2%datasets)
! d2d(1)[n,m] and scalar
    if(d2_len == 1)then
      call IXFalloc(data_out%datasets,d2_len,status)
      call IXF&/**/
             &DATA_OPERATION(data_out%datasets(1),data_in2%datasets(1),status)
      if(status == IXCseverity_error)return
      call IXFmark_valid(data_out)
      ! first argument gives bridge data with none to choose
      call IXFset_data(data_out,status,workspace=data_in2%workspace,bridge=data_in2%bridge)
    endif    
!**** d2d(m)[n,1] and scalar
    if(d2_len > 1)then
      call IXFalloc(data_out%datasets,d2_len,status)
      do i=1,d2_len
        call IXF&/**/
             &DATA_OPERATION(data_out%datasets(i),data_in2%datasets(i),status)
        if(status == IXCseverity_error)return        
      enddo
      call IXFmark_valid(data_out)           
      !first argument gives bridge data with none to choose
      call IXFset_data(data_out,status,workspace=data_in2%workspace,bridge=data_in2%bridge)
    endif             
  end subroutine
#undef DATA_OPERATION
#endif /* defined(DATA_OPERATION) */
