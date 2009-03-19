#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#define MX_COMPAT_32
#include "mex.h"

#ifdef _WIN32
#define CALLING_MODE	__stdcall
#define ixCopyPtrToInteger2	IXCOPYPTRTOINTEGER2
#define ixCreateNumericArray IXCREATENUMERICARRAY
#define ixCreateDoubleScalar IXCREATEDOUBLESCALAR
#define ixDestroyArray IXDESTROYARRAY
#define ixDuplicateArray IXDUPLICATEARRAY
#define ixGetCell	IXGETCELL
#define ixGetData	IXGETDATA
#define ixGetDimensions	IXGETDIMENSIONS
#define ixGetM		IXGETM
#define ixGetN		IXGETN
#define ixGetNumberOfDimensions	IXGETNUMBEROFDIMENSIONS
#define ixGetNumberOfElements	IXGETNUMBEROFELEMENTS
#define ixGetFieldByNumber	IXGETFIELDBYNUMBER
#define ixGetScalar	IXGETSCALAR
#define ixGetStringStub	IXGETSTRINGSTUB
#define ixIsCell	IXISCELL
#define ixIsChar	IXISCHAR
#define ixIsDouble	IXISDOUBLE
#define ixIsInt32	IXISINT32
#define ixIsStruct	IXISSTRUCT
#define ixSetFieldByNumber	IXSETFIELDBYNUMBER
#define ixSetN		IXSETN
#define iexCallMATLABStub IEXCALLMATLABSTUB
#define iexPrintfStub IEXPRINTFSTUB
#define ixAddFieldStub	IXADDFIELDSTUB
#define ixCreateStringStub IXCREATESTRINGSTUB
#define ixGetClassNameStub	IXGETCLASSNAMESTUB
#define ixClassIDFromClassNameStub	IXCLASSIDFROMCLASSNAMESTUB
#define ixGetFieldStub	IXGETFIELDSTUB
#define ixGetFieldNumberStub	IXGETFIELDNUMBERSTUB
#define ixIsClassStub	IXISCLASSSTUB
#define ixCreateCharMatrixFromStringsStub IXCREATECHARMATRIXFROMSTRINGSSTUB
#define ixCreateCellArray	IXCREATECELLARRAY
#define ixSetCell	IXSETCELL
#else
#define CALLING_MODE
#define ixCopyPtrToInteger2 ixcopyptrtointeger2_
#define ixCreateNumericArray ixcreatenumericarray_
#define ixCreateDoubleScalar ixcreatedoublescalar_
#define ixDestroyArray ixdestroyarray_
#define ixDuplicateArray ixduplicatearray_
#define ixGetCell	ixgetcell_
#define ixGetData	ixgetdata_
#define ixGetDimensions	ixgetdimensions_
#define ixGetM		ixgetm_
#define ixGetN		ixgetn_
#define ixGetNumberOfDimensions	ixgetnumberofdimensions_
#define ixGetNumberOfElements	ixgetnumberofelements_
#define ixGetFieldByNumber	ixgetfieldbynumber_
#define ixGetScalar	ixgetscalar_
#define ixGetStringStub	ixgetstringstub_
#define ixIsCell	ixiscell_
#define ixIsChar	ixischar_
#define ixIsDouble	ixisdouble_
#define ixIsInt32	ixisint32_
#define ixIsStruct	ixisstruct_
#define ixSetFieldByNumber	ixsetfieldbynumber_
#define ixSetN		ixsetn_
#define iexCallMATLABStub iexcallmatlabstub_
#define iexPrintfStub iexprintfstub_
#define ixAddFieldStub	ixaddfieldstub_
#define ixCreateStringStub ixcreatestringstub_
#define ixGetClassNameStub	ixgetclassnamestub_
#define ixClassIDFromClassNameStub	ixclassidfromclassnamestub_
#define ixGetFieldStub	ixgetfieldstub_
#define ixGetFieldNumberStub	ixgetfieldnumberstub_
#define ixIsClassStub	ixisclassstub_
#define ixCreateCharMatrixFromStringsStub ixcreatecharmatrixfromstringsstub_
#define ixCreateCellArray	ixcreatecellarray_
#define ixSetCell	ixsetcell_
#endif

#ifdef PRINT_POINTERS
#  define ptrprint(ptr, func) mexPrintf("\n" #func ":" #ptr " = %p\n", ptr)
#else
#  define ptrprint(ptr, func)
#endif

#ifdef __unix__
/* 
 * from matlab explore.c example
 */
extern void get_characteristics(const mxArray  *array_ptr);
extern mxClassID analyze_class(const mxArray *array_ptr);
#else
#define  get_characteristics(__a)	1
#define  analyze_class(__a)		1
#endif

static void debug_print(const mxArray *val, const char* format, ... )
{
    static char buffer[1024];
    va_list arglist;
    va_start(arglist, format);
    vsprintf(buffer, format, arglist);
    va_end(arglist);
    mexPrintf("%s\n", buffer);     
    get_characteristics(val);
    analyze_class(val);
}


void CALLING_MODE ixCopyPtrToInteger2(short** ptr, short* target, int* n)
{
    ptrprint(ixCopyPtrToInteger2,*ptr);
    ptrprint(ixCopyPtrToInteger2,target);
    if ((ptr != NULL) && (*ptr != NULL) && (target != NULL) && (n != NULL))
    {
        memcpy(target, *ptr, (*n) * sizeof(short));
    }
    else
    {
	mexPrintf("Error in ixCopyPtrToInteger2");
    }
}

int CALLING_MODE iexPrintfStub(const char* buffer)
{
    return mexPrintf("%s", buffer);
}

int CALLING_MODE iexCallMATLABStub(int* nlhs, mxArray *plhs[], int* nrhs, mxArray *prhs[], 
                    const char *fcn_name)
{
    return mexCallMATLAB(*nlhs, plhs, *nrhs, prhs, fcn_name);
}

mxArray* CALLING_MODE ixCreateDoubleScalar(double* value)
{
    return mxCreateDoubleScalar(*value);
}

int CALLING_MODE ixAddFieldStub(mxArray **pa, const char *fieldname)
{
    return mxAddField(*pa, fieldname);
}

mxArray* CALLING_MODE ixCreateCharMatrixFromStringsStub(int *m, const char *str) /* todo */
{
    const char** str1;
    int n, i;
    mxArray* r;
    n = strlen(str); /* *m null terminated strings end-to-end of same length */
    str1 = (const char**)malloc((*m) * sizeof(const char*)); 
    for(i=0; i< *m; i++)
    {
	str1[i] = str + (n+1) * i;
    }
    r = mxCreateCharMatrixFromStrings(*m, str1);			
    free(str1);
    return r;
}

mxArray* CALLING_MODE ixCreateNumericArray(int* ndim, const int *dims, mxClassID* classid, 
                                mxComplexity* flag)
{
/*
*	int i;
*	mexPrintf("ixCreateNumericArray ");
*	for(i=0; i<*ndim; i++)
*	{
* 		mexPrintf("%d ", dims[i]);
*	}
*	mexPrintf("\n");
*/
    return mxCreateNumericArray(*ndim, dims, *classid, *flag);
}

mxArray* CALLING_MODE ixCreateStringStub(const char *str)
{
    return mxCreateString(str);
}

void CALLING_MODE ixDestroyArray(mxArray **pa)
{
    mxDestroyArray(*pa);
}

mxArray* CALLING_MODE ixDuplicateArray(const mxArray **pa)
{
    return mxDuplicateArray(*pa);
}

mxArray* CALLING_MODE ixGetCell(const mxArray **pa, int *i)
{
    return mxGetCell(*pa, *i - 1);	/* fortran starts at 1, C at 0 */
}

void CALLING_MODE ixGetClassNameStub(const mxArray **pa, char* result, const int* len)
{
    strncpy(result, mxGetClassName(*pa), *len);
}

mxClassID CALLING_MODE ixClassIDFromClassNameStub(const char* name)
{
    if (!strcmp(name, "double"))
    {
	return mxDOUBLE_CLASS;
    }
    else if (!strcmp(name, "char"))
    {
	return mxCHAR_CLASS;
    }
    else if (!strcmp(name, "int32"))
    {
	return mxINT32_CLASS;
    }
    else
    {
	mexPrintf("Error in ixClassIDFromClassName - %s\n", name);
	return mxUNKNOWN_CLASS;
    }
}

void* CALLING_MODE ixGetData(const mxArray **pa)
{
    return mxGetData(*pa);
}

void CALLING_MODE ixGetDimensions(const mxArray **pa, int* dims_array, int *max_dims)
{
    int ndims = mxGetNumberOfDimensions(*pa);
    const int* da = mxGetDimensions(*pa);
    if (ndims != *max_dims)
    {
 	mexPrintf("error in ixGetDimensions");
    }
/*
*	int i;
*	mexPrintf("ixGetDimenions ");
*	for(i=0; i<ndims; i++)
*	{
*		mexPrintf("%d ", da[i]);
*	}
*	mexPrintf("\n");
*/
	memcpy(dims_array, da, ndims * sizeof(int));
}

mxArray* CALLING_MODE ixGetFieldStub(const mxArray **pa, int *i, const char *fieldname)
{
/*    debug_print(*pa, "mxGetField array element %d field %s\n", *i - 1, fieldname); */
    return mxGetField(*pa, *i - 1, fieldname); /* fortran starts at 1, C 0 */
}

mxArray* CALLING_MODE ixGetFieldByNumber(const mxArray **pa, int *i, int *fieldnum)
{
    return mxGetFieldByNumber(*pa, *i - 1, *fieldnum - 1); /* fortran -> C */
}

int CALLING_MODE ixGetFieldNumberStub(const mxArray **pa, const char *name)
{
    return mxGetFieldNumber(*pa, name) + 1; /* c -> fortran */			
}

int CALLING_MODE ixGetNumberOfDimensions(const mxArray **pa)
{
    return mxGetNumberOfDimensions(*pa);
}

int CALLING_MODE ixGetNumberOfElements(const mxArray **pa)
{
    return mxGetNumberOfElements(*pa);
}

double CALLING_MODE ixGetScalar(const mxArray **pa)
{
    return mxGetScalar(*pa);
}

/* 
 * return a NULL terminated string of max length (including NULL) buflen i.e. buflen
 * should be n+1 characters long for an n character string
 */
int CALLING_MODE ixGetStringStub(const mxArray **pa, char *buf, const int* buflen)
{
    return mxGetString(*pa, buf, *buflen);
}

int CALLING_MODE ixIsCell(const mxArray **pa)
{
    return mxIsCell(*pa);
}

int CALLING_MODE ixIsChar(const mxArray **pa)
{
    return mxIsChar(*pa);
}

int CALLING_MODE ixIsClassStub(const mxArray **pa, const char* name)
{
    return mxIsClass(*pa, name);
}

int CALLING_MODE ixIsDouble(const mxArray **pa)
{
    return mxIsDouble(*pa);
}

int CALLING_MODE ixIsInt32(const mxArray **pa)
{
    return mxIsInt32(*pa);
}

int CALLING_MODE ixIsLogical(const mxArray **pa)
{
    return mxIsLogical(*pa);
}

int CALLING_MODE ixIsStruct(const mxArray **pa)
{
    return mxIsStruct(*pa);
}

void CALLING_MODE ixSetN(mxArray **pa, int *n)
{
    mxSetN(*pa, *n);
}

int CALLING_MODE ixGetN(const mxArray **pa)
{
    return mxGetN(*pa);
}

int CALLING_MODE ixGetM(const mxArray **pa)
{
    return mxGetM(*pa);
}

void CALLING_MODE ixSetCell(mxArray** pa, int* index, mxArray** val)
{
    mxSetCell(*pa, *index - 1, *val);
}

mxArray* CALLING_MODE ixCreateCellArray(int *ndims, const int* dims_array)
{
    return mxCreateCellArray(*ndims, dims_array);
}

void CALLING_MODE ixSetFieldByNumber(mxArray **pa, int *i, int *fieldnum, mxArray **value)
{
    mxSetFieldByNumber(*pa, *i - 1, *fieldnum - 1, *value); /* fortran -> C */
}
