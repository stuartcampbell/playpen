
#if defined(IXD_TYPE)

  subroutine IXBgetFromBindingArray&/**/
                                   &IXD_TYPE (matlab_prhs, field, array_index, &
                                              op_count, value, status)
    implicit none
    character(len=*) :: field
    integer :: array_index, op_count
    integer(cpointer_t) :: matlab_prhs    !! Pointer to PRHS MATLAB structure
    type(IXT&/**/
            &IXD_TYPE ) :: value(:)
    type(IXTstatus) :: status
    type(IXToperation):: op
    if (matlab_prhs == 0) then
!       call IXFadd_status(status, IXCfacility_bindings, IXCseverity_info, &
!                          IXCerr_outofmem, 'getFromBindingArray: prhs=0')
       return
    endif
    if (size(value) == 1) then
        call IXBgetFromBinding(matlab_prhs, field, array_index, &
                               op_count, value(1), status)
    else
        call IXFoperationMake(op, IXFop_matlabreadMake(matlab_prhs), status)
        call IXFoperation_Run(op, ' ', value, status)
        call IXFoperationCleanup(op, status)
    endif
  end subroutine

  subroutine IXBgetFromBinding&/**/
                              &IXD_TYPE (matlab_prhs, field, array_index, &
                                              op_count, value, status)
    implicit none
    character(len=*):: field
    integer :: array_index, op_count
    integer(cpointer_t) :: matlab_prhs
    type(IXT&/**/
            &IXD_TYPE ) :: value
    type(IXTstatus) :: status
    type(IXToperation) :: op
    if (matlab_prhs == 0) then
!       call IXFadd_status(status, IXCfacility_bindings, IXCseverity_info, &
!                          IXCerr_outofmem, 'getFromBinding: prhs=0')
       return
    endif
    call IXFoperationMake(op, IXFop_matlabreadMake(matlab_prhs), status)
    call IXFoperation_run(op, ' ', value, status)
    call IXFoperationCleanup(op, status)
  end subroutine

  subroutine IXBsendToBindingArray&/**/
                                  &IXD_TYPE (matlab_plhs, matlab_prhs, field, array_index, &
                                             op_count, value, status)
    implicit none
    type(IXTstatus) :: status
    integer(cpointer_t) :: matlab_prhs, matlab_plhs    !! Pointer to MATLAB structure
    integer(cpointer_t), external :: IXBcreateClassArray
    character(len=30) :: class_name
    character(len=*) :: field    !! Structure field to extract
    type(IXT&/**/
            &IXD_TYPE ) :: value(:)
    integer :: array_index, op_count
    type(IXToperation)::op
    if (size(value) == 1) then
        call IXBsendToBinding(matlab_plhs, matlab_prhs, field, array_index, &
                                             op_count, value(1), status)
    else
        call ixGetClassName(matlab_prhs, class_name)
        matlab_plhs = IXBcreateClassArray(class_name, size(value), status)
!        call IXBcreateBindingPLHS(matlab_plhs,matlab_prhs,status)
        call IXFoperationMake(op,IXFop_matlabwriteMake(matlab_plhs),status)
        call IXFoperation_run(op,' ',value,status)
        call IXFoperationCleanup(op,status)
    endif
  end subroutine

  subroutine IXBsendToBinding&/**/
                             &IXD_TYPE (matlab_plhs, matlab_prhs, field, array_index, &
                                             op_count, value, status)
    implicit none
    type(IXTstatus) :: status
    integer(cpointer_t) :: matlab_prhs, matlab_plhs    !! Pointer to MATLAB structure
    character(len=*) :: field    !! Structure field to extract
    type(IXT&/**/
            &IXD_TYPE ) :: value
    integer :: array_index, op_count
    type(IXToperation) :: op
    integer(cpointer_t), external :: IXBcreateClassArray
    character(len=30) :: class_name
    call ixGetClassName(matlab_prhs, class_name)
    matlab_plhs = IXBcreateClassArray(class_name, 1,  status)
!****
#ifndef IXD_NO_BASE
    if (.not. IXFvalid(value))return
#endif
!****
!    call IXBcreateBindingPLHS(matlab_plhs, matlab_prhs, status)
    call IXFoperationMake(op, IXFop_matlabwriteMake(matlab_plhs), status)
    call IXFoperation_run(op, ' ', value, status)
    call IXFoperationCleanup(op,status)
  end subroutine


#undef IXD_TYPE

#endif /* defined(IXD_TYPE) */

