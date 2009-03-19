! the following are used to generate matlab calling functions
!
! GENERATE_MATLAB_DISPLAY(testclass) -> IXTtestclass_display_m
! for each of the following macros IXFtestclassOperation MUST be defined in the module definition
! and be a binary or unary operation
! GENERATE_MATLAB_BINARY_OP(testclass,_operation_m,Operation) -> IXFtestclass_operation_m
! GENERATE_MATLAB_UNARY_OP(testclass,_operation_m,Operation) -> IXFtestclass_operation_m
!
! These macros just concatenate their arguments e.g. CONCAT(a,b)  gives  ab
!

!
! GENERATE_MODULE_MATLAB_READ(testclass)  ->  IXFtestclassMatlabRead(w1,prhs,status)
!

!
! GENERATE_MATLAB_DISPLAY(testclass) -> IXTtestclass_display_m
!
! GENERATE_MODULE_MATLAB_READ(testclass)  ->  IXFtestclassMatlabRead(w1,prhs,status)
! GENERATE_MODULE_MATLAB_WRITE(testclass) -> IXFtestclassMatlabWrite(w1,plhs,prhs,status)

! IXD_TYPE

#if defined(IXD_TYPE)

module IXMm_&
             IXD_TYPE
  use IXMbinding_interface
  use IXM&
          IXD_TYPE
  implicit none
  interface IXBsendToBinding
      module procedure IXBsendToBinding&
                                        IXD_TYPE
      module procedure IXBsendToBindingArray&
                                             IXD_TYPE
  end interface
  interface IXBgetFromBinding
      module procedure IXBgetFromBinding&
                                         IXD_TYPE
      module procedure IXBgetFromBindingArray&
                                              IXD_TYPE
  end interface

contains

  subroutine IXBgetFromBindingArray&
                                    IXD_TYPE (matlab_prhs, field, array_index, &
                                              op_count, value, status)
	implicit none
	character(len=*):: field
	integer :: field_num, array_index, op_count
    integer(cpointer_t) :: matlab_prhs	!! Pointer to PRHS MATLAB structure
    type(IXT&
             IXD_TYPE ) :: value(:)
    type(IXTstatus) :: status
    type(IXToperation)::op
	if (matlab_prhs == 0) return
    call IXFoperationMake(op,IXFop_matlabreadMake(matlab_prhs),status)
    call IXFoperation_Run(op,' ',value,status)
  end subroutine

  subroutine IXBgetFromBinding&
                               IXD_TYPE (matlab_prhs, field, array_index, &
                                              op_count, value, status)
	implicit none
	character(len=*):: field
	integer :: field_num, array_index, op_count
    integer(cpointer_t) :: matlab_prhs
    type(IXT&
             IXD_TYPE ) :: value
    type(IXTstatus) :: status
    type(IXToperation)::op
	if (matlab_prhs == 0) return
    call IXFoperationMake(op,IXFop_matlabreadMake(matlab_prhs),status)
    call IXFoperation_run(op,' ',value,status)
  end subroutine

  subroutine IXBsendToBindingArray&
                                   IXD_TYPE (matlab_plhs, matlab_prhs, field, array_index, &
                                             op_count, value, status)
 	implicit none
	type(IXTstatus) :: status
    integer(cpointer_t) :: matlab_prhs, matlab_plhs	!! Pointer to MATLAB structure
    character(len=*) :: field	!! Structure field to extract
    type(IXT&
             IXD_TYPE ) :: value(:)
	integer :: array_index, op_count
    type(IXToperation)::op
    call IXBcreateBindingPLHS(matlab_plhs,matlab_prhs,status)
    call IXFoperationMake(op,IXFop_matlabwriteMake(matlab_plhs),status)
    call IXFoperation_run(op,' ',value,status)
  end subroutine

  subroutine IXBsendToBinding&
                              IXD_TYPE (matlab_plhs, matlab_prhs, field, array_index, &
                                             op_count, value, status)
 	implicit none
	type(IXTstatus) :: status
    integer(cpointer_t) :: matlab_prhs, matlab_plhs	!! Pointer to MATLAB structure
    character(len=*) :: field	!! Structure field to extract
    type(IXT&
             IXD_TYPE ) :: value
	integer :: array_index, op_count
    type(IXToperation)::op
    call IXBcreateBindingPLHS(matlab_plhs,matlab_prhs,status)
    call IXFoperationMake(op,IXFop_matlabwriteMake(matlab_plhs),status)
    call IXFoperation_run(op,' ',value,status)
  end subroutine

!
! GENERATE_MODULE_MATLAB_WRITE(testclass) -> IXFtestclassMatlabWrite(w1,plhs,prhs,status)
!

end module
!
! GENERATE_MATLAB_DISPLAY(testclass) -> IXTtestclass_create_m
! prhs(1) is default object
! prhs(2) is varargin
!

subroutine IXBdisplay_&
                      IXD_TYPE (nlhs,plhs,nrhs,prhs,s)
  use IXMm_&
            IXD_TYPE
  implicit none
  integer::nlhs,nrhs
  integer(cpointer_t)::plhs(nlhs),prhs(nrhs)
  type(IXT&
           IXD_TYPE), allocatable :: w1(:)
  type(IXTstatus)::s
  integer:: n
  n = IXBgetNumberOfElements(prhs(1))
  allocate(w1(n))
  call IXBgetFromBinding(prhs(1),' ',1,0,w1,s)
  if (s == IXCseverity_error) return
  call IXFdisplay(w1,s)
  deallocate(w1)
end subroutine

!! return 0 if all OK, 1 if an error

subroutine IXBcheck_&
                    IXD_TYPE (nlhs,plhs,nrhs,prhs,s)
  use IXMm_&
          IXD_TYPE
  implicit none
  integer::nlhs,nrhs,return_stat
  integer(cpointer_t)::plhs(nlhs),prhs(nrhs)
  type(IXT&
           IXD_TYPE), allocatable :: w1(:)
  type(IXTstatus)::s
  integer:: n
  n = IXBgetNumberOfElements(prhs(1))
  allocate(w1(n))
  call IXBgetFromBinding(prhs(1),' ',1,0,w1,s)
  if (s /= IXCseverity_error) then
      call IXFcheck(w1,s)
  endif
  if (s == IXCseverity_error) then
      return_stat = 1
  else
      return_stat = 0
  endif
  deallocate(w1)
  call IXBsendToBinding(plhs(1), ' ', 1, 0, return_stat, s)
end subroutine


subroutine IXBcreate_&
                     IXD_TYPE (nlhs, plhs, nrhs, prhs, s)
  use IXMm_&
            IXD_TYPE
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXT&
           IXD_TYPE), allocatable :: w1(:)
  type(IXTstatus) :: s
  integer:: n
  n = IXBgetNumberOfElements(prhs(1))
  allocate(w1(n))
  call IXBgetFromBinding(prhs(1),' ',1,0,w1,s)
  if (s == IXCseverity_error) return
  call IXBgetFromBinding(prhs(2),' ',1,0,w1,s)
  call IXFCheck(w1,s)
  if (s == IXCseverity_error) then
    plhs(1)=IXBduplicateObject(prhs(1))
  else
    call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, w1, s)
  endif
  deallocate(w1)
end subroutine

#undef IXD_TYPE

#endif /* defined(IXD_TYPE) */
