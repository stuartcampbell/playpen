module IXMm_datum

#define IXD_TYPE datum
#include "bindings_header.f90"

contains
!***
#define IXD_NO_BASE 1
!***

#define IXD_TYPE datum
#include "bindings_base.f90"

end module IXMm_datum

#define IXD_TYPE datum
#include "bindings_extra.f90"

#define IXD_TYPE	datum
#define IXD_NAME	Log_Datum
#include "unary_ops.f90"

#define IXD_TYPE	datum
#define IXD_NAME	Exp_Datum
#include "unary_ops.f90"

#define IXD_TYPE	datum
#define IXD_NAME	Sin_Datum
#include "unary_ops.f90"

#define IXD_TYPE	datum
#define IXD_NAME	Cos_Datum
#include "unary_ops.f90"

#define IXD_TYPE	datum
#define IXD_NAME	Tan_Datum
#include "unary_ops.f90"

#define IXD_TYPE	datum
#define IXD_NAME	Sinh_Datum
#include "unary_ops.f90"

#define IXD_TYPE	datum
#define IXD_NAME	Cosh_Datum
#include "unary_ops.f90"

#define IXD_TYPE	datum
#define IXD_NAME	Tanh_Datum
#include "unary_ops.f90"

#define IXD_TYPE	datum
#define IXD_NAME	Plus_Datum
#include "binary_ops.f90"

#define IXD_TYPE	datum
#define IXD_NAME	Minus_Datum
#include "binary_ops.f90"

#define IXD_TYPE	datum
#define IXD_NAME	Times_Datum
#include "binary_ops.f90"

#define IXD_TYPE	datum
#define IXD_NAME	Divide_Datum
#include "binary_ops.f90"

#define IXD_TYPE	datum
#define IXD_NAME	Power_Datum
#include "binary_ops.f90"
