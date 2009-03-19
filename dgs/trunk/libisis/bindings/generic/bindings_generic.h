#ifndef BINDING_GENERIC_H 
#define BINDING_GENERIC_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#ifdef _WIN32
#define CALLING_CONVENTION		__stdcall
#define ixbdofortrancall		IXBDOFORTRANCALL
#define compare_nocase			stricmp
#else
#define CALLING_CONVENTION
#define ixbdofortrancall		ixbdofortrancall_
#define compare_nocase			strcasecmp
#endif /* _WIN32 */

/* 
 * The type  mexfunc_t  and the macro  declare_function()
 * must create the same function signature
 *
 * libisisexc.h contains a list of functions and makes use of the current
 * definition of declare_function() to generate the appropriate declarations
 */

#define declare_function(__NAME) \
  extern void CALLING_CONVENTION __NAME (int* nlhs, void *plhs[], \
				const int* nrhs, const void* prhs[], void* status);

  typedef void (CALLING_CONVENTION *mexfunc_t)(int* nlhs, void *plhs[],
				const int* nrhs, const void* prhs[], void* status);

/* 
 * used to store a the function name and its pointer 
 */
typedef struct 
{
	const char* name;
	mexfunc_t func;
} mexfunc_s_t;

/*
 * this is the fortran function that will ultimately be called by the binding entry point - 
 * it will be passed a pointer to the fortran function ("func") that it should
 * call after it has done any fortran specific initialisations
 */
extern void CALLING_CONVENTION ixbdofortrancall(mexfunc_t func, int* nlhs, void *plhs[],
					const int* nrhs, const void* prhs[], int* errcode);

/* 
 * this is the signature of the function which is used to report an error message 
 * during a binding call
 * "severity" is an IXCSEVERITY_* constant defined above and "status_arg"
 * is an opaque information structure used to record information about the message 
 */
typedef void* IXBstatus_arg_t;
typedef void (*IXBstatus_func_t)(IXBstatus_arg_t status_arg, int severity, const char* format, ... );

/*
 * severity codes used when reporting an error with the above function
 */
#define IXCSEVERITY_OK		1	/* No current error */
#define IXCSEVERITY_WARNING	2	/* Warning condition */
#define IXCSEVERITY_INFO	3	/* Informational message */
#define IXCSEVERITY_ERROR	4	/* Error conditon */
#define IXCSEVERITY_DEBUG	5	/* Debug message */
#define IXCSEVERITY_FATAL	6	/* Fatal (non-recoverable) */

/*
 * data type constants used by routines that have to allocate memory
 * for variables etc.
 *
 * A double value will be of type IXCTYPE_FLOAT64
 * A arrays of doubles will be of type (IXCTYPE_FLOAT64 | IXCTYPE_ARRAY)
 */
#define IXCTYPE_FLOAT64		(1 << 0)	/* double precision number (64 bit) */   
#define IXCTYPE_FLOAT32		(1 << 1)	/* single precision number (32 bit) */   
#define IXCTYPE_INT32		(1 << 2)	/* integer (32 bit) */
#define IXCTYPE_CHAR8		(1 << 3)	/* character (8 bit) */
#define IXCTYPE_CELL		(1 << 4)	/* cell array (list); members may be of different type */
#define IXCTYPE_ARRAY		(1 << 5)	/* contiguous array of another IXCTYPE_ */
#define IXCTYPE_STRUCT		(1 << 6)	/* class or structure */

void IXBinitialize_bindings(void* binding_func_array[], int len_array);

#define CONCAT(__a,__b) __a##__b		/* just joins its arguments together */

/* 
 * the declare_binding() macro creates both a declaration for the function 
 * and a typedef for the same function (function name + "_t" appended.
 * the typedef can be used to cast the void* pointer back to the correct
 * type for calling
 */

#define declare_binding(__NAME,__RETURNS,__ARGS) \
	extern __RETURNS __NAME __ARGS ; \
	typedef __RETURNS (* CONCAT(__NAME,_t) ) __ARGS;
/* 
 * These are the functions that must be implemented by any application that 
 * wishes to have a binding to LIBISISEXC
 * 
 * These binding routines are called from FORTRAN as IXB*  
 */ 

typedef void* IXBptr_t;		/* used to indicate a ponter to an external binding/scripting object */ 

declare_binding(IXBCwriteline,int,(const char* line, int len_line));
declare_binding(IXBCmakelhs,IXBptr_t,(IXBptr_t ext_ptr, int binding_alloc, IXBstatus_func_t func, IXBstatus_arg_t status_arg));
declare_binding(IXBCgetnumberofelements,int,(IXBptr_t ext_ptr, IXBstatus_func_t func, IXBstatus_arg_t status_arg));
declare_binding(IXBCduplicateobject,IXBptr_t,(IXBptr_t ext_ptr, IXBstatus_func_t func, IXBstatus_arg_t status_arg));
declare_binding(IXBCgetcell,IXBptr_t,(IXBptr_t ext_ptr, int element_index, IXBstatus_func_t func, IXBstatus_arg_t status_arg));
declare_binding(IXBCsetcell,int,(IXBptr_t ext_ptr, int element_index, IXBptr_t value, IXBstatus_func_t func, IXBstatus_arg_t status_arg));
declare_binding(IXBCgetstringlength,int,(IXBptr_t ext_ptr, IXBstatus_func_t func, IXBstatus_arg_t status_arg));
declare_binding(IXBCgetstring,int,(IXBptr_t ext_ptr, char* value, int max_len, IXBstatus_func_t func, IXBstatus_arg_t status_arg));
declare_binding(IXBCcreatestring,IXBptr_t,(const char* value, int len, IXBstatus_func_t func, IXBstatus_arg_t status_arg));
declare_binding(IXBCgetlogical,int,(IXBptr_t ext_ptr, IXBstatus_func_t func, IXBstatus_arg_t status_arg));
declare_binding(IXBCcreatelogical,IXBptr_t,(int val, IXBstatus_func_t func, IXBstatus_arg_t status_arg));
declare_binding(IXBCgetfloat64,double,(IXBptr_t ext_ptr, IXBstatus_func_t func, IXBstatus_arg_t status_arg));
declare_binding(IXBCcreatefloat64,IXBptr_t,(double d, IXBstatus_func_t func, IXBstatus_arg_t status_arg));
declare_binding(IXBCistype,int,(IXBptr_t ext_ptr, int type, IXBstatus_func_t func, IXBstatus_arg_t status_arg));
declare_binding(IXBCgetnumberofdimensions,int,(IXBptr_t ext_ptr, IXBstatus_func_t func, IXBstatus_arg_t status_arg));
declare_binding(IXBCgetdimensions,int,(IXBptr_t ext_ptr, int* dims_array, int max_dims, IXBstatus_func_t func, IXBstatus_arg_t status_arg));
declare_binding(IXBCgetfieldbyname,IXBptr_t,(IXBptr_t ext_ptr, const char* field, int array_index, IXBstatus_func_t func, IXBstatus_arg_t status_arg));
declare_binding(IXBCsetfieldbyname,int,(IXBptr_t ext_ptr, const char* field, int array_index, IXBptr_t value, IXBstatus_func_t func, IXBstatus_arg_t status_arg));
declare_binding(IXBCcreatefield,int,(IXBptr_t ext_ptr, const char* field, IXBstatus_func_t func, IXBstatus_arg_t status_arg));
declare_binding(IXBCallocarraydescriptor,IXBptr_t,(const int* dims_array, int ndims, int array_type, IXBstatus_func_t func, IXBstatus_arg_t status_arg));
declare_binding(IXBCgetarraydata,void*,(IXBptr_t ext_ptr, IXBstatus_func_t func, IXBstatus_arg_t status_arg));
declare_binding(IXBCdeallocarraydescriptor,int,(IXBptr_t ext_ptr, IXBstatus_func_t func, IXBstatus_arg_t status_arg));

static void* binding_func_array[] = {
IXBCwriteline,
IXBCmakelhs,
IXBCgetnumberofelements,
IXBCduplicateobject,
IXBCgetcell,
IXBCsetcell,
IXBCgetstringlength,
IXBCgetstring,
IXBCcreatestring,
IXBCgetlogical,
IXBCcreatelogical,
IXBCgetfloat64,
IXBCcreatefloat64,
IXBCistype,
IXBCgetnumberofdimensions,
IXBCgetdimensions,
IXBCgetfieldbyname,
IXBCsetfieldbyname,
IXBCcreatefield,
IXBCallocarraydescriptor,
IXBCgetarraydata,
IXBCdeallocarraydescriptor 
};

#define NBINDINGFUNCS (sizeof(binding_func_array) / sizeof(void*))	/* number of elements */

typedef enum binding_func_index {
IXBCwriteline_i = 0,
IXBCmakelhs_i,
IXBCgetnumberofelements_i,
IXBCduplicateobject_i,
IXBCgetcell_i,
IXBCsetcell_i,
IXBCgetstringlength_i,
IXBCgetstring_i,
IXBCcreatestring_i,
IXBCgetlogical_i,
IXBCcreatelogical_i,
IXBCgetfloat64_i,
IXBCcreatefloat64_i,
IXBCistype_i,
IXBCgetnumberofdimensions_i,
IXBCgetdimensions_i,
IXBCgetfieldbyname_i,
IXBCsetfieldbyname_i,
IXBCcreatefield_i,
IXBCallocarraydescriptor_i,
IXBCgetarraydata_i,
IXBCdeallocarraydescriptor_i 
};

#ifdef __cplusplus
extern }
#endif /* __cplusplus */

#endif /* BINDING_GENERIC_H */ 