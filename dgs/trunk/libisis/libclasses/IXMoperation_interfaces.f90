!-----------------------------------------------------------------------------------------------------------------------------------
!MODULE: IXMoperation_interfaces
!-----------------------------------------------------------------------------------------------------------------------------------
!! @author Freddie Akeroyd, ISIS
!! @version $Revision: 770 $ ($Date: 2006-07-31 12:18:26 -0400 (Mon, 31 Jul 2006) $)
!! @see IXMoperation
!!
!! This module defines the getFromBinding, getFromBindingPtr, getFromBindingAlloc and sendToBinding
!! interfaces used during the operationRun interface
!!

module IXMoperation_interfaces
use IXMtype_definitions
implicit none
public :: IXBgetFromBinding, IXBgetFromBindingAlloc, IXBgetFromBindingPtr, &
           IXBsendToBinding, IXBgetNumberOfElements, IXBcreateClassArray
integer IXBgetNumberOfElements
integer(cpointer_t) :: IXBcreateClassArray
external IXBgetNumberOfElements, IXBcreateClassArray

interface IXBgetFromBinding

#define IXD_NAME    Char
#define IXD_TYPE    character(len=*)
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    i4b
#define IXD_TYPE    integer(i4b)
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    dp
#define IXD_TYPE    real(dp)
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    Logical
#define IXD_TYPE    logical
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    dp1
#define IXD_TYPE    real(dp)
#define IXD_DIMS    :
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    dp2
#define IXD_TYPE    real(dp)
#define IXD_DIMS    :,:
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    dp3
#define IXD_TYPE    real(dp)
#define IXD_DIMS    :,:,:
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    dp4
#define IXD_TYPE    real(dp)
#define IXD_DIMS    :,:,:,:
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    i1
#define IXD_TYPE    integer(i4b)
#define IXD_DIMS    :
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    i2
#define IXD_TYPE    integer(i4b)
#define IXD_DIMS    :,:
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    i3
#define IXD_TYPE    integer(i4b)
#define IXD_DIMS    :,:,:
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    i4
#define IXD_TYPE    integer(i4b)
#define IXD_DIMS    :,:,:,:
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    c1
#define IXD_TYPE    character(len=*)
#define IXD_DIMS    :
#include "IXMoperation_interfaces_get.f90"

end interface IXBgetFromBinding

interface IXBgetFromBindingPtr

#define IXD_NAME    Ptrdp1
#define IXD_TYPE    real(dp),pointer
#define IXD_DIMS    :
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    Ptrdp2
#define IXD_TYPE    real(dp),pointer
#define IXD_DIMS    :,:
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    Ptrdp3
#define IXD_TYPE    real(dp),pointer
#define IXD_DIMS    :,:,:
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    Ptrdp4
#define IXD_TYPE    real(dp),pointer
#define IXD_DIMS    :,:,:,:
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    Ptri1
#define IXD_TYPE    integer(i4b),pointer
#define IXD_DIMS    :
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    Ptri2
#define IXD_TYPE    integer(i4b),pointer
#define IXD_DIMS    :,:
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    Ptri3
#define IXD_TYPE    integer(i4b),pointer
#define IXD_DIMS    :,:,:
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    Ptri4
#define IXD_TYPE    integer(i4b),pointer
#define IXD_DIMS    :,:,:,:
#include "IXMoperation_interfaces_get.f90"

end interface IXBgetFromBindingPtr

interface IXBgetFromBindingAlloc

#define IXD_NAME    Allocdp1
#define IXD_TYPE    real(dp),allocatable
#define IXD_DIMS    :
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    Allocdp2
#define IXD_TYPE    real(dp),allocatable
#define IXD_DIMS    :,:
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    Allocdp3
#define IXD_TYPE    real(dp),allocatable
#define IXD_DIMS    :,:,:
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    Allocdp4
#define IXD_TYPE    real(dp),allocatable
#define IXD_DIMS    :,:,:,:
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    Alloci1
#define IXD_TYPE    integer(i4b),allocatable
#define IXD_DIMS    :
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    Alloci2
#define IXD_TYPE    integer(i4b),allocatable
#define IXD_DIMS    :,:
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    Alloci3
#define IXD_TYPE    integer(i4b),allocatable
#define IXD_DIMS    :,:,:
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    Alloci4
#define IXD_TYPE    integer(i4b),allocatable
#define IXD_DIMS    :,:,:,:
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    Allocc1
#define IXD_TYPE    character(len=*),allocatable
#define IXD_DIMS    :
#include "IXMoperation_interfaces_get.f90"

end interface IXBgetFromBindingAlloc

interface IXBsendToBinding

#define IXD_NAME    Char
#define IXD_TYPE    character(len=*)
#include "IXMoperation_interfaces_send.f90"

#define IXD_NAME    i4b
#define IXD_TYPE    integer(i4b)
#include "IXMoperation_interfaces_send.f90"

#define IXD_NAME    dp
#define IXD_TYPE    real(dp)
#include "IXMoperation_interfaces_send.f90"

#define IXD_NAME    Logical
#define IXD_TYPE    logical
#include "IXMoperation_interfaces_send.f90"

#define IXD_NAME    dp1
#define IXD_TYPE    real(dp)
#define IXD_DIMS    :
#include "IXMoperation_interfaces_send.f90"

#define IXD_NAME    dp2
#define IXD_TYPE    real(dp)
#define IXD_DIMS    :,:
#include "IXMoperation_interfaces_send.f90"

#define IXD_NAME    dp3
#define IXD_TYPE    real(dp)
#define IXD_DIMS    :,:,:
#include "IXMoperation_interfaces_send.f90"

#define IXD_NAME    dp4
#define IXD_TYPE    real(dp)
#define IXD_DIMS    :,:,:,:
#include "IXMoperation_interfaces_send.f90"

#define IXD_NAME    i1
#define IXD_TYPE    integer(i4b)
#define IXD_DIMS    :
#include "IXMoperation_interfaces_send.f90"

#define IXD_NAME    i2
#define IXD_TYPE    integer(i4b)
#define IXD_DIMS    :,:
#include "IXMoperation_interfaces_send.f90"

#define IXD_NAME    i3
#define IXD_TYPE    integer(i4b)
#define IXD_DIMS    :,:,:
#include "IXMoperation_interfaces_send.f90"

#define IXD_NAME    i4
#define IXD_TYPE    integer(i4b)
#define IXD_DIMS    :,:,:,:
#include "IXMoperation_interfaces_send.f90"

#define IXD_NAME    c1
#define IXD_TYPE    character(len=*)
#define IXD_DIMS    :
#include "IXMoperation_interfaces_send.f90"

end interface IXBsendToBinding

end module IXMoperation_interfaces
