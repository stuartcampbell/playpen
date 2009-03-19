#ifndef ENDIAN_CONVERT
#define ENDIAN_CONVERT

/* #include "libisisconfig.h" */

/* 
 * $Id: endian_convert.h 1443 2008-11-24 11:53:39Z Freddie Akeroyd $
 */

#define fort_int int
#ifdef _WIN32
#define FORTRAN_CALL	__stdcall
#else
#define FORTRAN_CALL
#endif

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#if defined(__xAPPLE__) /* not neded now */
#define local_to_vax_short local_to_vax_short_
#define vax_to_local_short vax_to_local_short_
#define local_to_vax_int local_to_vax_int_
#define vax_to_local_int vax_to_local_int_
#define local_to_vax_shorts local_to_vax_shorts_
#define vax_to_local_shorts vax_to_local_shorts_
#define local_to_vax_ints local_to_vax_ints_
#define vax_to_local_ints vax_to_local_ints_
#define ieee_to_vax_float ieee_to_vax_float_
#define vax_to_ieee_float vax_to_ieee_float_
#define vaxf_to_local vaxf_to_local_
#define local_to_vaxf local_to_vaxf_
#define local_to_ieee_float local_to_ieee_float_
#elif defined(__unix)
#define local_to_vax_short local_to_vax_short__
#define vax_to_local_short vax_to_local_short__
#define local_to_vax_int local_to_vax_int__
#define vax_to_local_int vax_to_local_int__
#define local_to_vax_shorts local_to_vax_shorts__
#define vax_to_local_shorts vax_to_local_shorts__
#define local_to_vax_ints local_to_vax_ints__
#define vax_to_local_ints vax_to_local_ints__
#define ieee_to_vax_float ieee_to_vax_float__
#define vax_to_ieee_float vax_to_ieee_float__
#define vaxf_to_local vaxf_to_local__
#define local_to_vaxf local_to_vaxf__
#define local_to_ieee_float local_to_ieee_float__
#elif defined(_WIN32)
#define local_to_vax_short LOCAL_TO_VAX_SHORT
#define vax_to_local_short VAX_TO_LOCAL_SHORT
#define local_to_vax_int LOCAL_TO_VAX_INT
#define vax_to_local_int VAX_TO_LOCAL_INT
#define local_to_vax_shorts LOCAL_TO_VAX_SHORTS
#define vax_to_local_shorts VAX_TO_LOCAL_SHORTS
#define local_to_vax_ints LOCAL_TO_VAX_INTS
#define vax_to_local_ints VAX_TO_LOCAL_INTS
#define ieee_to_vax_float IEEE_TO_VAX_FLOAT
#define vax_to_ieee_float VAX_TO_IEEE_FLOAT
#define vaxf_to_local VAXF_TO_LOCAL
#define local_to_vaxf LOCAL_TO_VAXF
#define local_to_ieee_float LOCAL_TO_IEEE_FLOAT
#endif /* __unix */


unsigned short FORTRAN_CALL local_to_vax_short(const unsigned short* s);
unsigned short FORTRAN_CALL vax_to_local_short(const unsigned short* s);
unsigned FORTRAN_CALL local_to_vax_int(const fort_int* i);
unsigned FORTRAN_CALL vax_to_local_int(const fort_int* i);

void FORTRAN_CALL local_to_vax_shorts(unsigned short* sa, const int* n);
void FORTRAN_CALL vax_to_local_shorts(unsigned short* sa, const int* n);
void FORTRAN_CALL local_to_vax_ints(fort_int* ia, const fort_int* n);
void FORTRAN_CALL vax_to_local_ints(fort_int* ia, const fort_int* n);



/* these routines return 0 = success, 1 = failure */

/* convert an IEEE single float to VAX F FLOAT format */
/* int ieee_to_vax_float(float* fp); */

/* convert VAX F FLOAT into an IEEE single float */
/* int vax_to_ieee_float(float* fp); */

/* convert float array val[n] to and from vax float */
void FORTRAN_CALL vaxf_to_local(float *val, const int *n, int *errcode);
void FORTRAN_CALL local_to_vaxf(float *val, const int *n, int *errcode);
void FORTRAN_CALL local_to_ieee_float(float *val, const int *n, int *errcode);
void FORTRAN_CALL local_to_ieee_double(double *val, const int *n, int *errcode);
void FORTRAN_CALL ieee_float_to_local(float *val, const int *n, int *errcode);
void FORTRAN_CALL ieee_double_to_local(double *val, const int *n, int *errcode);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* ENDIAN_CONVERT */
