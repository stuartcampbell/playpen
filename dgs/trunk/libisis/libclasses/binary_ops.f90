! IXD_NAME IXD_TYPE IXD_OPERATION

#if defined(IXD_NAME) && defined(IXD_TYPE) && defined(IXD_OPERATION)

subroutine tt_&/**/
                &IXD_NAME (wres,w1,w2,s)
    implicit none
    type(IXT&/**/
            &IXD_TYPE),intent(out) :: wres
    type(IXT&/**/
            &IXD_TYPE),intent(in) :: w1,w2             
    type(IXTstatus),intent(inout) :: s
!#if defined(D2D)
!    if   (((size(w1%signal,2)==1) .and. (size(w2%signal,2)>1)) &
!      .or.((size(w2%signal,2)==1) .and. (size(w1%signal,2)>1)))then
!      call IXFdataset_2d_X_&/**/
!                        &IXD_OPERATION&/**/
!                        &_dataset_2d(wres,w1,w2,s)
!      return
!    endif
!#undef D2D
!#endif /* defined (D2D) */
	call setup_binary_op_&/**/
	                     &IXD_TYPE (wres, w1,w2, s)
    if (s == IXCseverity_error) return
	call IXFarray&/**/
	             &IXD_OPERATION (wres%signal,wres%error,w1%signal,w1%error, w2%signal,w2%error,s)
    if (s == IXCseverity_error) return
	call finish_op_&/**/
	                &IXD_TYPE (wres,w1,s)
end subroutine 

subroutine ts_&/**/
                &IXD_NAME(wres,w1,w2,s)
    implicit none
    type(IXT&/**/
            &IXD_TYPE),intent(out) :: wres
    type(IXT&/**/
            &IXD_TYPE),intent(in) :: w1
    type(IXTstatus),intent(inout) :: s
	real(dp),intent(in)::w2
	call setup_unary_op_&/**/
	                    &IXD_TYPE (wres, w1, s)
    if (s == IXCseverity_error) return
	call IXFarray&/**/
	             &IXD_OPERATION (wres%signal,wres%error,w1%signal,w1%error, w2,s)
    if (s == IXCseverity_error) return
	call finish_op_&/**/
	                &IXD_TYPE (wres,w1,s)
end subroutine 

subroutine st_&/**/
                &IXD_NAME (wres,w1,w2,s)
    implicit none
    type(IXT&/**/
            &IXD_TYPE),intent(out) :: wres
    type(IXT&/**/
            &IXD_TYPE),intent(in) :: w2 
    type(IXTstatus),intent(inout) :: s
	real(dp),intent(in)::w1
	call setup_unary_op_&/**/
	                    &IXD_TYPE (wres, w2, s)
    if (s == IXCseverity_error) return
	call IXFarray&/**/
	             &IXD_OPERATION (wres%signal,wres%error,w1, w2%signal,w2%error,s)
    if (s == IXCseverity_error) return
	call finish_op_&/**/
	                &IXD_TYPE (wres,w2,s)
end subroutine 

#undef IXD_NAME 
#undef IXD_TYPE 
#undef IXD_OPERATION

#endif /* defined(IXD_NAME) && defined(IXD_TYPE) && defined(IXD_OPERATION) */

! 1-D array operations with dataset_2d, optional error array
#if defined(IXD_DIM)  && defined(IXD_OPERATION)

  subroutine IXD_DIM&/**/
	                &IXD_OPERATION&/**/
	                &_dataset_2d(wres,d2d,vec_s,status)
    implicit none
    type(IXTdataset_2d),intent(in)::d2d
    type(IXTdataset_2d),intent(out)::wres
    real(dp),intent(in)::vec_s(:)
    type(IXTstatus)::status
    
    call setup_unary_op_dataset_2d(wres,d2d,status)    
    
    call IXD_DIM&/**/
               &2d&/**/
               &IXD_OPERATION(status,wres%signal,wres%error,d2d%signal,d2d%error,vec_s)
    if (status == IXCseverity_error)return
    call finish_op_dataset_2d(wres,d2d,status)
  end subroutine 


  
#undef IXD_DIM 
#undef IXD_OPERATION

#endif /* defined(IXD_DIM) && defined(IXD_OPERATION) */

!dataset_1d operations with IXTdataset_2d
! and operations between dataset_2d(n,1) with dataset_2d(n,n)

#if defined(IXD_DIMXY)  && defined(IXD_OPERATION)
  subroutine d2d1_dataset_1d_&/**/
                   &IXD_DIMXY&/**/
	                &IXD_OPERATION&/**/
	                &_dataset_2d(wres,d2d_1,d1d_2,status)
    implicit none
    type(IXTdataset_2d),intent(in)::d2d_1
    type(IXTdataset_1d),intent(in)::d1d_2
    type(IXTdataset_2d),intent(out)::wres
    real(dp),pointer::v_s(:),v_e(:)
    type(IXTstatus)::status
! no checks on units/histogram/distribution compatibility, just using values in the array    
    call setup_unary_op_dataset_2d(wres,d2d_1,status)
    call IXFget_ptr_dataset_1d(d1d_2,signal=v_s,error=v_e)

    call IXFarray_&/**/
               &IXD_DIMXY&/**/
               &2d&/**/
               &IXD_OPERATION(status,wres%signal,wres%error,d2d_1%signal,d2d_1%error,v_s,v_e)
    if (status == IXCseverity_error)return
    call finish_op_dataset_2d(wres,d2d_1,status)
    v_s=>NULL()
    v_e=>NULL()
  end subroutine
  
  subroutine d1d2_dataset_1d_&/**/
                   &IXD_DIMXY&/**/
	                &IXD_OPERATION&/**/
	                &_dataset_2d(wres,d1d_1,d2d_2,status)
    implicit none
    type(IXTdataset_2d),intent(in)::d2d_2
    type(IXTdataset_1d),intent(in)::d1d_1
    type(IXTdataset_2d),intent(out)::wres
    real(dp),pointer::v_s(:),v_e(:)
    type(IXTstatus)::status
! no checks on units/histogram/distrobution compatibility, just using values in the array    
    call setup_unary_op_dataset_2d(wres,d2d_2,status)
    call IXFget_ptr_dataset_1d(d1d_1,signal=v_s,error=v_e)

    call IXFarray_&/**/
               &IXD_DIMXY&/**/
               &2d&/**/
               &IXD_OPERATION(status,wres%signal,wres%error,d2d_2%signal,d2d_2%error,v_s,v_e)
    if (status == IXCseverity_error)return
    call finish_op_dataset_2d(wres,d2d_2,status)
    v_s=>NULL()
    v_e=>NULL()
  end subroutine

#undef IXD_DIMXY
#undef IXD_OPERATION
#endif /* defined(IXD_DIMXY) && defined(IXD_OPERATION) */



!shaped array operations  with IXTdataset_2d
#if  defined(IXD_OPERATION) && defined(IXD_TYPE) && defined(IXD_DIMS)
  subroutine TA_&/**/
	                &IXD_OPERATION&/**/
	                &_&/**/
	                &IXD_TYPE(wres,w1,w2,status)
    implicit none
    type(IXT&/**/
            &IXD_TYPE),intent(in):: w1
    type(IXT&/**/
            &IXD_TYPE),intent(out):: wres            
    real(dp),intent(in)::w2( IXD_DIMS )
    type(IXTstatus)::status    
    
    call setup_unary_op_&/**/
                        &IXD_TYPE (wres,w1,status)
    call IXFarray&/**/
            &IXD_OPERATION&/**/
	             &DA(wres%signal,wres%error,w1%signal,w1%error,w2,status)                   
    if (status == IXCseverity_error)return
	call finish_op_&/**/
	                &IXD_TYPE (wres,w1,status)
  end subroutine 

  subroutine AT_&/**/
	                &IXD_OPERATION&/**/
	                &_&/**/
	                &IXD_TYPE(wres,w1,w2,status)
    implicit none
    type(IXT&/**/
            &IXD_TYPE),intent(out):: wres                
    type(IXT&/**/
            &IXD_TYPE),intent(in):: w2
    real(dp),intent(in)::w1( IXD_DIMS )
    type(IXTstatus)::status
    
    call setup_unary_op_&/**/
                        &IXD_TYPE (wres,w2,status)
    call IXFarray&/**/
	             &IXD_OPERATION&/**/
	             &AD(wres%signal,wres%error,w1,w2%signal,w2%error,status)                  
    if (status == IXCseverity_error)return
	call finish_op_&/**/
	                &IXD_TYPE (wres,w2,status)   
  end subroutine 
#undef IXD_OPERATION
#undef IXD_DIMS
#undef IXD_TYPE
#endif /* defined(IXD_OPERATION) && defined(IXD_TYPE) && defined(IXD_DIMS) */



!array of dataset_1d operations with dataset_2d
#if  defined(IXD_OPERATION)
  subroutine T_AT_dataset1d_&/**/
	                &IXD_OPERATION&/**/
	                &_dataset_2d(w1,w2,status)
    use IXMarraymanips	                
    implicit none
    type(IXTdataset_2d)::w1
    type(IXTdataset_1d),intent(in)::w2(:)
    real(dp),pointer::res_v(:,:),res_e(:,:)
    real(dp),pointer::v_s(:),v_e(:)

    type(IXTstatus)::status
    integer(i4b)::i,len_d1d
         
    len_d1d=size(w2)
    
    if(len_d1d /= size(w1%y) )then
      call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'length ofdataset_1d array must be size of dataset_2d%y')    
      return
    endif
!! a check for types of units......    
    
    call IXFallocdims(res_v,shape(w1%signal),status)
    call IXFallocdims(res_e,shape(w1%signal),status)

    do i=1,len_d1d
      call IXFget_ptr_dataset_1d(w2(i),signal=v_s,error=v_e)
      call IXFarray&/**/
            &IXD_OPERATION(res_v(:,i),res_e(:,i),w1%signal(:,i),w1%signal(:,i),v_s,v_e,status)                   
      if (status == IXCseverity_error)return
    enddo
    call IXFset_dataset_2d(w1,status,signal=res_v,error=res_e)
    call IXFdealloc(res_v,status)    
    call IXFdealloc(res_e,status)
    v_s=>NULL()
    v_e=>NULL()
  end subroutine 

   subroutine AT_T_dataset1d_&/**/
	                &IXD_OPERATION&/**/
	                &_dataset_2d(w1,w2,status)
    use IXMarraymanips	                
    implicit none
    type(IXTdataset_2d)::w2
    type(IXTdataset_1d),intent(in)::w1(:)
    real(dp),pointer::res_v(:,:),res_e(:,:)
    real(dp),pointer::v_s(:),v_e(:)

    type(IXTstatus)::status
    integer(i4b)::i,len_d1d
         
    len_d1d=size(w1)
    
    if(len_d1d /= size(w2%y) )then
      call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'length ofdataset_1d array must be size of dataset_2d%y')    
      return
    endif
    
!! a check for types of units......    
    
    
    call IXFallocdims(res_v,shape(w2%signal),status)
    call IXFallocdims(res_e,shape(w2%signal),status)

    do i=1,len_d1d
      call IXFget_ptr_dataset_1d(w1(i),signal=v_s,error=v_e)
      call IXFarray&/**/
            &IXD_OPERATION(res_v(:,i),res_e(:,i),v_s,v_e,w2%signal(:,i),w2%signal(:,i),status)                   
      if (status == IXCseverity_error)return
    enddo
    call IXFset_dataset_2d(w2,status,signal=res_v,error=res_e)
    call IXFdealloc(res_v,status)    
    call IXFdealloc(res_e,status)
    v_s=>NULL()
    v_e=>NULL()    
  end subroutine 
!operations between dataset_2d(n,1) with dataset_2d(n,n)
!***************
  subroutine IXFdataset_2d_X_&/**/
	                &IXD_OPERATION&/**/
	                &_dataset_2d(wres,d2d_a,d2d_b,s)
    implicit none
    type(IXTdataset_2d),intent(in)::d2d_a,d2d_b
    type(IXTdataset_2d),intent(out)::wres
    type(IXTstatus)::s
    integer(i4b)::len_a,len_b
    
    len_a=size(d2d_a%signal,2)
    len_b=size(d2d_b%signal,2)
! these two loops may look like duplication of code, but are not since the
! IXMarraymanips routine being called is discriminating on the dimensionality of the arrays being passed
    if(len_b==1)then  !(len_a>len_b)
      call setup_binary_X_special_dataset_2d(wres,d2d_a,d2d_b,s)
      if (s == IXCseverity_error)return
      !IXMarraymanips subroutine        
      call IXFarray_X_2d&/**/
           &IXD_OPERATION(s,wres%signal,wres%error,d2d_a%signal,d2d_a%error,d2d_b%signal(:,1),d2d_b%error(:,1))
      if (s == IXCseverity_error)return
      call finish_op_dataset_2d(wres,d2d_a,s)
    endif
    
    if(len_a==1)then !(len_b>len_a)
      call setup_binary_X_special_dataset_2d(wres,d2d_b,d2d_a,s)
      if (s == IXCseverity_error)return        
      !IXMarraymanips subroutine        
      call IXFarray_X_2d&/**/
           &IXD_OPERATION(s,wres%signal,wres%error,d2d_a%signal(:,1),d2d_a%error(:,1),d2d_b%signal,d2d_b%error)
      if (s == IXCseverity_error)return
      call finish_op_dataset_2d(wres,d2d_b,s)
    endif
    
  end subroutine


  subroutine IXFdataset_2d_Y_&/**/
	                &IXD_OPERATION&/**/
	                &_dataset_2d(wres,d2d_a,d2d_b,s)
    implicit none
    type(IXTdataset_2d),intent(in)::d2d_a,d2d_b
    type(IXTdataset_2d),intent(out)::wres
    type(IXTstatus)::s
    integer(i4b)::len_a,len_b
    
    len_a=size(d2d_a%signal,2)
    len_b=size(d2d_b%signal,2)
! these two loops may look like duplication of code, but are not since the
! IXMarraymanips routine being called is discriminating on the dimensionality of the arrays being passed
    if(len_b==1)then  !(len_a>len_b)
      call setup_binary_Y_special_dataset_2d(wres,d2d_a,d2d_b,s)
      if (s == IXCseverity_error)return
      !IXMarraymanips subroutine        
      call IXFarray_Y_2d&/**/
           &IXD_OPERATION(s,wres%signal,wres%error,d2d_a%signal,d2d_a%error,d2d_b%signal(:,1),d2d_b%error(:,1))
      if (s == IXCseverity_error)return
      call finish_op_dataset_2d(wres,d2d_a,s)
    endif
    
    if(len_a==1)then !(len_b>len_a)
      call setup_binary_Y_special_dataset_2d(wres,d2d_b,d2d_a,s)
      if (s == IXCseverity_error)return        
      !IXMarraymanips subroutine        
      call IXFarray_Y_2d&/**/
           &IXD_OPERATION(s,wres%signal,wres%error,d2d_a%signal(:,1),d2d_a%error(:,1),d2d_b%signal,d2d_b%error)
      if (s == IXCseverity_error)return
      call finish_op_dataset_2d(wres,d2d_b,s)
    endif
    
  end subroutine

!operations between d2d(m)(n,1) and d2d(1)[n,m]
  subroutine T_AT_dataset2d_&/**/
	                &IXD_OPERATION&/**/
	                &_dataset_2d(wres,w1,w2,status)
    use IXMarraymanips	                
    implicit none
    type(IXTdataset_2d),intent(out)::wres
    type(IXTdataset_2d),intent(in)::w2(:),w1
    type(IXTstatus),intent(inout)::status
    integer(i4b)::i,len_d2d
         
    len_d2d=size(w2)
    
    if(len_d2d /= size(w1%y) )then
      call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'length of dataset_2d array must be size of dataset_2d%y')    
      return
    endif

!we do the initial checks with the first dataset in the array d2d
    call setup_binary_common_dataset_2d(w1,w2(1),status)
    call IXFallocdims(wres%signal,shape(w1%signal),status)
    call IXFallocdims(wres%error,shape(w1%signal),status)
    
    if (status == IXCseverity_error) return        
    do i=1,len_d2d        
      if (size(w1%x) /= size(w2(i)%x) ) then
         call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'no. of x array elements incompatible(IXF_arraydataset_OPERATION_dataset_2d)') 
         return   
      endif
    ! if length test passed then make final checks on x/y-array values 
    !check all elements of x_array are the same
    !OPENGENIE currently checks identically the same 
    !a tolerance can easily be put in be it in percent or 1e-6
      if (sum(abs(w1%x - w2(i)%x))/=0 ) then
      call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'x array elements incompatible(IXF_arraydataset_2d_OPERATION_dataset_2d)') 
         return
      endif    
    
      call IXFarray&/**/
            &IXD_OPERATION(wres%signal(:,i),wres%error(:,i),w1%signal(:,i),w1%error(:,i),w2(i)%signal(:,1),w2(i)%error(:,1),status)
    enddo
    
    call finish_op_dataset_2d(wres,w1,status)

  end subroutine 
  
    subroutine AT_T_dataset2d_&/**/
	                &IXD_OPERATION&/**/
	                &_dataset_2d(wres,w1,w2,status)
    use IXMarraymanips	                
    implicit none
    type(IXTdataset_2d),intent(out)::wres
    type(IXTdataset_2d),intent(in)::w2,w1(:)
    type(IXTstatus),intent(inout)::status
    integer(i4b)::i,len_d2d
         
    len_d2d=size(w1)
    
    if(len_d2d /= size(w2%y) )then
      call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'length of dataset_2d array must be size of dataset_2d%y')    
      return
    endif

!we do the initial checks with the first dataset in the array of d2d
    call setup_binary_common_dataset_2d(w1(1),w2,status)
    call IXFallocdims(wres%signal,shape(w2%signal),status)
    call IXFallocdims(wres%error,shape(w2%signal),status)
    
    if (status == IXCseverity_error) return        
    do i=1,len_d2d    
      if (size(w2%x) /= size(w1(i)%x) ) then
         call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'no. of x array elements incompatible(IXF_arraydataset_2d_OPERATION_dataset_2d)') 
         return   
      endif
    ! if length test passed then make final checks on x/y-array values 
    !check all elements of x_array are the same
    !OPENGENIE currently checks identically the same 
    !a tolerance can easily be put in be it in percent or 1e-6
      if (sum(abs(w2%x - w1(i)%x))/=0 ) then
      call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'x array elements incompatible(IXF_arraydataset_2d_OPERATION_dataset_2d)') 
         return
      endif    
    
      call IXFarray&/**/
            &IXD_OPERATION(wres%signal(:,i),wres%error(:,i),w1(i)%signal(:,1),w1(i)%error(:,1),w2%signal(:,1),w2%error(:,1),status)
    enddo
    
    call finish_op_dataset_2d(wres,w2,status)

  end subroutine 
#undef IXD_OPERATION
#endif /* defined(IXD_OPERATION) */


#if  defined(DATA_OPERATION)

  subroutine tt_&/**/
             &DATA_OPERATION&/**/
             &_data(data_out,data_in1,data_in2,arg_use,status)
    implicit none
    type(IXTdata),intent(out)::data_out
    type(IXTdata),intent(in)::data_in1,data_in2
    integer(i4b),intent(out)::arg_use
    type(IXTstatus),intent(inout)::status
    integer(i4b)::i,d1_len,d2_len,yy1,yy2
    arg_use=0
    d1_len=size(data_in1%datasets)
    d2_len=size(data_in2%datasets)    
! d2d(1)[n,m] and d2d(1)[n,m]
! d2d(1)[n,m] and d2d(1)[n,1] (m times)
    if((d1_len == d2_len) .and. (d2_len == 1))then
      call IXFalloc(data_out%datasets,d2_len,status)
!determine internal sizes     
      yy1=IXFsize_y(data_in1%datasets(1))
      yy2=IXFsize_y(data_in2%datasets(1))
!internal sizes different and first arg is 1
      if((yy1==1) .and. yy2>yy1)then
        call IXF&/**/
             &DATA_OPERATION&/**/
             &_X(data_out%datasets(1),data_in1%datasets(1),data_in2%datasets(1),status)
        if(status == IXCseverity_error)return 
        call IXFmark_valid(data_out)
      ! second argument gives bridge data as it is a full IXTdataset_2d
        call IXFset_data(data_out,status,workspace=data_in2%workspace,bridge=data_in2%bridge)
        arg_use=2
!internal sizes different and second arg is 1
      elseif((yy2==1) .and. yy1>yy2)then
        call IXF&/**/
             &DATA_OPERATION&/**/
             &_X(data_out%datasets(1),data_in1%datasets(1),data_in2%datasets(1),status)
        if(status == IXCseverity_error)return 
        call IXFmark_valid(data_out)
      ! first argument gives bridge data as it is a full IXTdataset_2d
        call IXFset_data(data_out,status,workspace=data_in1%workspace,bridge=data_in1%bridge)
        arg_use=1
      else
!internal sizes must be the same        
        call IXF&/**/
             &DATA_OPERATION(data_out%datasets(1),data_in1%datasets(1),data_in2%datasets(1),status)      
        if(status == IXCseverity_error)return 
        call IXFmark_valid(data_out)
      ! first argument gives bridge data with none to choose between them
        call IXFset_data(data_out,status,workspace=data_in1%workspace,bridge=data_in1%bridge)
        arg_use=1
      endif
    endif
!**** d2d(m)[n,1] and d2d(m)[n,1]
    if((d1_len == d2_len) .and. (d2_len > 1))then
      call IXFalloc(data_out%datasets,d2_len,status)
      do i=1,d2_len
        call IXF&/**/
             &DATA_OPERATION(data_out%datasets(i),data_in1%datasets(i),data_in2%datasets(i),status)
        if(status == IXCseverity_error)return        
      enddo
      call IXFmark_valid(data_out)           
      !first argument gives bridge data with none to choose
      call IXFset_data(data_out,status,workspace=data_in1%workspace,bridge=data_in1%bridge)
      arg_use=1
    endif        
!**** d2d(1)(n,m) and d2d(m)[n,1]
!output will be a single d2d
    if((d1_len == 1) .and. (d2_len > 1))then    
      call IXFalloc(data_out%datasets,d1_len,status)
      call IXF&/**/
             &DATA_OPERATION(data_out%datasets(1),data_in1%datasets(1),data_in2%datasets,status)
      if(status == IXCseverity_error)return      
      call IXFmark_valid(data_out)     
      call IXFset_data(data_out,status,workspace=data_in1%workspace,bridge=data_in1%bridge)
      arg_use=1
    endif     
!**** d2d(m)[n,1] and d2d(1)(n,m)
!output will be a single d2d
    if((d2_len == 1) .and. (d1_len > 1))then
      call IXFalloc(data_out%datasets,d2_len,status)
      call IXF&/**/
             &DATA_OPERATION(data_out%datasets(1),data_in1%datasets,data_in2%datasets(1),status)
      if(status == IXCseverity_error)return
      call IXFmark_valid(data_out)     
      call IXFset_data(data_out,status,workspace=data_in2%workspace,bridge=data_in2%bridge)
      arg_use=2
    endif     
  end subroutine

  subroutine ts_&/**/
             &DATA_OPERATION&/**/
             &_data(data_out,data_in1,scalar,status)
    implicit none
    type(IXTdata),intent(out)::data_out
    type(IXTdata),intent(in)::data_in1
    real(dp),intent(in)::scalar
    type(IXTstatus),intent(inout)::status
    integer(i4b)::i,d1_len
    
    d1_len=size(data_in1%datasets)
! d2d(1)[n,m] and scalar
    if(d1_len == 1)then
      call IXFalloc(data_out%datasets,d1_len,status)
      call IXF&/**/
             &DATA_OPERATION(data_out%datasets(1),data_in1%datasets(1),scalar,status)
      if(status == IXCseverity_error)return
      call IXFmark_valid(data_out)
      ! first argument gives bridge data with none to choose
      call IXFset_data(data_out,status,workspace=data_in1%workspace,bridge=data_in1%bridge)
    endif    
!**** d2d(m)[n,1] and scalar
    if(d1_len > 1)then
      call IXFalloc(data_out%datasets,d1_len,status)
      do i=1,d1_len
        call IXF&/**/
             &DATA_OPERATION(data_out%datasets(i),data_in1%datasets(i),scalar,status)
        if(status == IXCseverity_error)return        
      enddo
      call IXFmark_valid(data_out)           
      !first argument gives bridge data with none to choose
      call IXFset_data(data_out,status,workspace=data_in1%workspace,bridge=data_in1%bridge)
    endif             
  end subroutine

  subroutine st_&/**/
             &DATA_OPERATION&/**/
             &_data(data_out,scalar,data_in2,status)
    implicit none
    type(IXTdata),intent(out)::data_out
    type(IXTdata),intent(in)::data_in2
    real(dp),intent(in)::scalar
    type(IXTstatus),intent(inout)::status
    integer(i4b)::i,d2_len
    
    d2_len=size(data_in2%datasets)
! d2d(1)[n,m] and scalar
    if(d2_len == 1)then
      call IXFalloc(data_out%datasets,d2_len,status)
      call IXF&/**/
             &DATA_OPERATION(data_out%datasets(1),scalar,data_in2%datasets(1),status)
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
             &DATA_OPERATION(data_out%datasets(i),scalar,data_in2%datasets(i),status)
        if(status == IXCseverity_error)return        
      enddo
      call IXFmark_valid(data_out)           
      !first argument gives bridge data with none to choose
      call IXFset_data(data_out,status,workspace=data_in2%workspace,bridge=data_in2%bridge)
    endif             
  end subroutine
#undef DATA_OPERATION
#endif /* defined(DATA_OPERATION) */

#if  defined(RUNFILE_OPERATION)

  subroutine tt_&/**/
             &RUNFILE_OPERATION&/**/
             &_runfile(rfile_out,rfile1,rfile2,status)
    implicit none
    type(IXTrunfile),intent(in)::rfile1,rfile2
    type(IXTrunfile),intent(out)::rfile_out
    type(IXTstatus),intent(inout)::status
    type(IXTdata)::temp_data
    integer(i4b)::arg_d,arg_m,arg_use,arg_t
    logical::d1,d2,m1,m2
    
    d1=IXFvalid(rfile1%det_data)
    d2=IXFvalid(rfile2%det_data)
    m1=IXFvalid(rfile1%mon_data)
    m2=IXFvalid(rfile2%mon_data)
    !default values are 5 to make case construct below work properly
    arg_m=5
    arg_d=5
    arg_t=0
    if( (.not. IXFvalid(rfile1)) .or. (.not. IXFvalid(rfile2)))then
      call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
      IXCerr_invparam,'Fatal error, invalid runfile object supplied(IXFoperation_runfile)')
    endif

    if(  (.not. m2) .and. (.not.d2)) then
      call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
      IXCerr_invparam,'Fatal error, invalid runfile object supplied- incorrect det_data/mon_data(IXFoperation_runfile)')
    endif
    if( (.not. m1) .and. (.not.d1)) then
      call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
      IXCerr_invparam,'Fatal error, invalid runfile object supplied - incorrect det_data/mon_data(IXFoperation_runfile)')
    endif
    if (status == IXCseverity_error)return
    
    if( m1 .and. m2 .and. d1 .and. d2) then
      call IXF&/**/
             &RUNFILE_OPERATION(rfile_out%mon_data,rfile1%mon_data,rfile2%mon_data,arg_m,status)
      call IXF&/**/
             &RUNFILE_OPERATION(rfile_out%det_data,rfile1%det_data,rfile2%det_data,arg_d,status)     
    endif

    if( (.not. m1) .and. (.not. m2) .and. d1 .and. d2)then    
      call IXF&/**/
             &RUNFILE_OPERATION(rfile_out%det_data,rfile1%det_data,rfile2%det_data,arg_d,status)     
    endif

    if( (.not. d1) .and. (.not. d2) .and. m1 .and. m2)then    
      call IXF&/**/
             &RUNFILE_OPERATION(rfile_out%mon_data,rfile1%mon_data,rfile2%mon_data,arg_m,status)
    endif
    
    if( (.not. m1) .and. m2 .and. d1 .and.  d2) then
      call IXF&/**/
             &RUNFILE_OPERATION(rfile_out%det_data,rfile1%det_data,rfile2%det_data,arg_d,status)     
      call IXF&/**/
             &RUNFILE_OPERATION(rfile_out%mon_data,rfile1%det_data,rfile2%mon_data,arg_m,status)     
    endif
    
    if( m1 .and. m2 .and. (.not. d1) .and. d2) then
      call IXF&/**/
             &RUNFILE_OPERATION(rfile_out%det_data,rfile1%mon_data,rfile2%det_data,arg_d,status)     
      call IXF&/**/
             &RUNFILE_OPERATION(rfile_out%mon_data,rfile1%mon_data,rfile2%mon_data,arg_m,status)     
    endif
  
    if( m1 .and. (.not. m2) .and. d1 .and. d2) then
      call IXF&/**/
             &RUNFILE_OPERATION(rfile_out%det_data,rfile1%det_data,rfile2%det_data,arg_d,status)     
      call IXF&/**/
             &RUNFILE_OPERATION(rfile_out%mon_data,rfile1%mon_data,rfile2%det_data,arg_m,status)     
    endif

    if( m1 .and. m2 .and. d1 .and. (.not. d2)) then
      call IXF&/**/
             &RUNFILE_OPERATION(rfile_out%det_data,rfile1%det_data,rfile2%mon_data,arg_d,status)     
      call IXF&/**/
             &RUNFILE_OPERATION(rfile_out%mon_data,rfile1%mon_data,rfile2%mon_data,arg_m,status)     
    endif

    if( (.not. m1) .and. m2 .and. d1 .and. (.not.d2)) then
!do operation with temporary IXTdata object
!      call IXF&/**/
!             &RUNFILE_OPERATION(rfile_out%det_data,rfile1%det_data,rfile2%mon_data,arg_d,status)     
      call IXF&/**/
             &RUNFILE_OPERATION(temp_data,rfile1%det_data,rfile2%mon_data,arg_t,status)     
!arg_t=1 then set runfile info and resultant IXTdata to first argument
      if (arg_t==1)then
        call IXFcopy(temp_data,rfile_out%det_data,status)
        call IXFdestroy(temp_data,status)
        arg_d=0
      endif
!arg_t=2 then set runfile info and resultant IXTdata to second argument
      if (arg_t==2)then
        call IXFcopy(temp_data,rfile_out%det_data,status)
        call IXFdestroy(temp_data,status)
        arg_m=0
      endif

    endif
    
    if( m1 .and. (.not. m2) .and. (.not.d1) .and. d2) then
!      call IXF&/**/
!             &RUNFILE_OPERATION(rfile_out%det_data,rfile1%mon_data,rfile2%det_data,arg_d,status)
      call IXF&/**/
             &RUNFILE_OPERATION(temp_data,rfile1%mon_data,rfile2%det_data,arg_t,status)
!arg_t=1 then set runfile info and resultant IXTdata to first argument
      if (arg_t==1)then
        call IXFcopy(temp_data,rfile_out%mon_data,status)
        call IXFdestroy(temp_data,status)
        arg_m=0
      endif
!arg_t=2 then set runfile info and resultant IXTdata to second argument
      if (arg_t==2)then
        call IXFcopy(temp_data,rfile_out%det_data,status)      
        call IXFdestroy(temp_data,status)        
        arg_d=0
      endif
    endif

    if (status == IXCseverity_error)return
  
    arg_use=arg_m+arg_d+arg_t
    select case(arg_use)  
    case(3)
      call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
      IXCerr_invparam,'Cannot determine instrument information for resultant runfile(IXFoperation_runfile)')
    case(10)
      call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
      IXCerr_invparam,'runfile operation is invalid(IXFoperation_runfile)')
    case(2,6)
       call finish_op_runfile(rfile_out,rfile1,status)
    case(4,7)
       call finish_op_runfile(rfile_out,rfile2,status)
    end select

  end subroutine 

  subroutine ts_&/**/
             &RUNFILE_OPERATION&/**/
             &_runfile(rfile_out,rfile1,scalar,status)
    implicit none
    type(IXTrunfile),intent(in)::rfile1
    real(dp),intent(in)::scalar
    type(IXTrunfile),intent(out)::rfile_out
    type(IXTstatus),intent(inout)::status
    if( .not. IXFvalid(rfile1) )then
      call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
      IXCerr_invparam,'Fatal error, invalid runfile object supplied(IXFoperation_runfile)')
    endif    
    if(IXFvalid(rfile1%mon_data))call IXF&/**/
             &RUNFILE_OPERATION(rfile_out%mon_data,rfile1%mon_data,scalar,status)

    if(IXFvalid(rfile1%det_data))call IXF&/**/
             &RUNFILE_OPERATION(rfile_out%det_data,rfile1%det_data,scalar,status)
! rfile1 information is copied             
    call finish_op_runfile(rfile_out,rfile1,status)
  end subroutine 
  subroutine st_&/**/
             &RUNFILE_OPERATION&/**/
             &_runfile(rfile_out,scalar,rfile2,status)
    implicit none
    type(IXTrunfile),intent(in)::rfile2
    real(dp),intent(in)::scalar
    type(IXTrunfile),intent(out)::rfile_out
    type(IXTstatus),intent(inout)::status
    if( .not. IXFvalid(rfile2) )then
      call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
      IXCerr_invparam,'Fatal error, invalid runfile object supplied(IXFoperation_runfile)')
    endif    
    if(IXFvalid(rfile2%mon_data))call IXF&/**/
             &RUNFILE_OPERATION(rfile_out%mon_data,rfile2%mon_data,scalar,status)

    if(IXFvalid(rfile2%det_data))call IXF&/**/
             &RUNFILE_OPERATION(rfile_out%det_data,rfile2%det_data,scalar,status)
! rfile2 information is copied         
    call finish_op_runfile(rfile_out,rfile2,status)    
  end subroutine 

#undef RUNFILE_OPERATION
#endif /* defined(RUNFILE_OPERATION) */
