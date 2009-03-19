#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "bindings_generic.h"

/* 
 * this is the function of type IXBstatus_func_t passed to IXBC functions 
 */
void IXBstatus_func(IXBstatus_arg_t status_arg, int severity, const char* format, ... )
{
	char buffer[256];
	va_list ap;
	va_start(ap, format);
	_vsnprintf(buffer, sizeof(buffer), format, ap);
	va_end(ap);
	printf("%s\n", buffer);	/* This will do for now - should call fortran status_add */
    return;
}

#define CALLING_CONVENTION		__stdcall
#define ixbcreatebindingplhs	IXBCREATEBINDINGPLHS
#define ixbgetnumberofelements	IXBGETNUMBEROFELEMENTS
#define ixbduplicateobject		IXBDUPLICATEOBJECT
#define ixbisstruct				IXBISSTRUCT
#define ixbiscell				IXBISCELL
#define ixbgetfieldnumber		IXBGETFIELDNUMBER
#define ixbaddfield				IXBADDFIELD
#define ixbgetfieldbynumber		IXBGETFIELDBYNUMBER
#define ixbsetfieldbynumber		IXBSETFIELDBYNUMBER
#define ixbgetfield				IXBGETFIELD
#define ixbgetcell				IXBGETCELL
#define ixbwriteline			IXBWRITELINE
#define ixbgetstring			IXBGETSTRING
#define ixbcreatestring			IXBCREATESTRING
#define ixbsetn					IXBSETN
#define ixbgetmatlablogical		IXBGETMATLABLOGICAL
#define ixbcreatematlablogical	IXBCREATEMATLABLOGICAL
#define ixbgetscalar			IXBGETSCALAR
#define ixbcreatescalardouble	IXBCREATESCALARDOUBLE
#define ixbisdouble				IXBISDOUBLE
#define ixbgetdata				IXBGETDATA
#define ixbgetpr				IXBGETPR
#define ixbgetnumberofdimensions	IXBGETNUMBEROFDIMENSIONS
#define ixbgetdimensions			IXBGETDIMENSIONS
#define ixbcopyptrtointeger4		IXBCOPYPTRTOINTEGER4
#define ixbclassidfromclassname			IXBCLASSIDFROMCLASSNAME
#define ixbcreatenumericarray			IXBCREATENUMERICARRAY
#define ixbexternalmakeresult	IXBEXTERNALMAKERESULT
#define ixbgetarraydata IXBGETARRAYDATA
#define	ixbdeallocarraydescriptor IXBDEALLOCARRAYDESCRIPTOR
#define	ixballocarraydescriptor IXBALLOCARRAYDESCRIPTOR
#define ixbgetfieldfrombinding	IXBGETFIELDFROMBINDING
#define ixbcreatebindingfieldifneeded	IXBCREATEBINDINGFIELDIFNEEDED
#define ixbexternalmakeresult		IXBEXTERNALMAKERESULT


void CALLING_CONVENTION ixbcreatebindingplhs(IXBptr_t* plhs, IXBptr_t* prhs, IXBstatus_arg_t status)
{
}

int CALLING_CONVENTION ixbgetnumberofelements(IXBptr_t* ext_ptr)
{
    return (*(IXBCgetnumberofelements_t)binding_func_array[IXBCgetnumberofelements_i])(*ext_ptr, IXBstatus_func, NULL);
}

IXBptr_t CALLING_CONVENTION ixbduplicateobject(IXBptr_t* ext_ptr)
{
    return (*(IXBCduplicateobject_t)binding_func_array[IXBCduplicateobject_i])(*ext_ptr, IXBstatus_func, NULL);
}

int CALLING_CONVENTION ixbisstruct(IXBptr_t* ext_ptr)
{
//    return (*(IXBCisstruct_t)binding_func_array[IXBCisstruct_i])(*ext_ptr, IXBstatus_func, NULL);
	return 0;
}

void CALLING_CONVENTION ixbgetfieldnumber(int a, int b, int c)
{
}

void CALLING_CONVENTION ixbaddfield(int a, int b, int c)
{
}

void CALLING_CONVENTION ixbgetfieldbynumber(int a, int b, int c)
{
}

void CALLING_CONVENTION ixbsetfieldbynumber(int a, int b, int c, int d)
{
}

void CALLING_CONVENTION ixbiscell(int a)
{
}

void CALLING_CONVENTION ixbgetfield(int a, int b, int c, int d)
{
}

void CALLING_CONVENTION ixbgetcell(int a, int b)
{
}

/*
 * write line on text to screen/console provided by binding
 */
void CALLING_CONVENTION ixbwriteline(const char* line, int* len_line, void* status)
{
	char *tmp;
	tmp = (char*)malloc(1+*len_line);
    if (tmp == NULL)
	{
		return;
	}
	strncpy(tmp, line, *len_line);
	tmp[*len_line] = '\0';
	printf("%s", tmp);
	free(tmp);
	return;
}

void CALLING_CONVENTION ixbgetstring(int a, int b, int c, int d)
{
}

void CALLING_CONVENTION ixbcreatestring(int a, int b)
{
}

void CALLING_CONVENTION ixbsetn(int a, int b)
{
}

void CALLING_CONVENTION ixbgetmatlablogical(int a)
{
}


void CALLING_CONVENTION ixbcreatematlablogical(int a)
{
}

void CALLING_CONVENTION ixbgetscalar(int a)
{
}

void CALLING_CONVENTION ixbcreatescalardouble(int a)
{
}

void CALLING_CONVENTION ixbisdouble(int a)
{
}

void CALLING_CONVENTION ixbgetdata(int a)
{
}

void CALLING_CONVENTION ixbgetpr(int a)
{
}

void CALLING_CONVENTION ixbgetnumberofdimensions(int a)
{
}

void CALLING_CONVENTION ixbgetdimensions(int a)
{
}

void CALLING_CONVENTION ixbcopyptrtointeger4(int a, int b, int c)
{
}

void CALLING_CONVENTION ixbclassidfromclassname(int a, int b)
{
}

void CALLING_CONVENTION ixbcreatenumericarray(int a, int b, int c, int d)
{
}

void CALLING_CONVENTION ixbgetfieldfrombinding(int a, int b, int c, int d, int aa, int ab, int ac, int ad)
{
}

void CALLING_CONVENTION ixbcreatebindingfieldifneeded(int a, int b, int c)
{
}

void CALLING_CONVENTION ixbexternalmakeresult(int a, int b)
{

}

void CALLING_CONVENTION ixballocarraydescriptor(int a, int b, int c, int d)
{

}

void CALLING_CONVENTION ixbgetarraydata(int a)
{

}

void CALLING_CONVENTION ixbdeallocarraydescriptor(IXBptr_t *ext_ptr)
{
    (*(IXBCdeallocarraydescriptor_t)binding_func_array[IXBCdeallocarraydescriptor_i])(*ext_ptr, IXBstatus_func, NULL);
}

void IXBinitialize_bindings(void* ext_binding_func_array[], int len_array)
{
	int i;
    if (len_array != NBINDINGFUNCS)
	{
		printf("error in IXBinitialize_bindings: %d != %d", len_array, NBINDINGFUNCS);
		return;
	}
	for(i=0; i<len_array; i++)
	{
		binding_func_array[i] = ext_binding_func_array[i];
	}
	return;
}

