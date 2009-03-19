module IXMm_base

#define IXD_TYPE base
#include "bindings_header.f90"

contains

#define IXD_TYPE base
!***
#define IXD_NO_BASE 1
!***
#include "bindings_base.f90"

end module IXMm_base

#define IXD_TYPE base
#include "bindings_extra.f90"
