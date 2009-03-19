#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "bindings_generic.h"

/* 
 * in the functions below items of type IXBptr_t (often called ext_ptr)
 * contain the address of a binding object. In the case of Python
 * IXBptr_t would contain a pointer to a PYObject (PYObject*), in the case 
 * of Matlab it would contain (mxarray*)
 */

/*
 * write a line of text to the screen/console window that
 * is being used by the bindings
 * return 0 on success, -1 on error
 */
int IXBCwriteline(const char* line, int len_line)
{
	char *tmp;
	tmp = (char*)malloc(1+len_line);
    if (tmp == NULL)
	{
		return -1;
	}
	strncpy(tmp, line, len_line);
	tmp[len_line] = '\0';
	printf("%s", tmp);
	free(tmp);
	return 0;
}

// A typical functon call is
//       LHS = function(RHS)
// sometime an object that is passed as a RHS needs to be returned as
// the result (LHS). This function is passed the RHS object and its job
// if to return an appropriate LHS pointer. In some cases it may be OK to just
// return the RHS directly. In matlab RHS variables are read-only and so a
// copy may need to be made etc.
// if "binding_alloc == 1" the object was created during a call to the binding
// (i.e. it was allocated by fortran rather tha being passed into the binding)
// the value of "binding_alloc" may help in thge decision of whether an 
// object needs e.g. to be added to the garbage collector or an extra
// reference count taken
//
// return NULL on error
IXBptr_t IXBCmakelhs(IXBptr_t ext_ptr, int binding_alloc, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
    (*func)(status_arg, IXCSEVERITY_INFO, "IXBCmakelhs called with %p at %d", ext_ptr, __LINE__);
    return NULL;
}

// return the number of elements in ext_ptr (total number of elements in an array, number of fields 
// in a structure, length of a list etc.)
// Return -1 on error
int IXBCgetnumberofelements(IXBptr_t ext_ptr, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
    (*func)(status_arg, IXCSEVERITY_INFO, "IXBCgetnumberofelements called with %p at %d", ext_ptr, __LINE__);
	return -1;
}

// Make a deep copy of the object pointed to by ext_pointer
// (a deep copy is a copy of all subobjects as well i.e. you cannot just
// copy pointer values but must allocate new objects)
// return NULL on error
IXBptr_t IXBCduplicateobject(IXBptr_t ext_ptr, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
    (*func)(status_arg, IXCSEVERITY_INFO, "IXBCduplicateobject called with %p at %d", ext_ptr, __LINE__);
	return ext_ptr;
}

// return a pointer to an element of a list or matlab cell array
// return NULL on error
IXBptr_t IXBCgetcell(IXBptr_t ext_ptr, int element_index, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
    (*func)(status_arg, IXCSEVERITY_INFO, "IXBCgetcell called with %p at %d", ext_ptr, __LINE__);
	return NULL;
}

// Set an element of a list or matlab cell array to the given value
// return 0 on success, -1 on error
int IXBCsetcell(IXBptr_t ext_ptr, int element_index, IXBptr_t value, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
    (*func)(status_arg, IXCSEVERITY_INFO, "IXBCsetcell called with %p at %d", ext_ptr, __LINE__);
	return -1;
}

// return length of a string item, -1 on error (not a string etc.)
int IXBCgetstringlength(IXBptr_t ext_ptr, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
	(*func)(status_arg, IXCSEVERITY_INFO, "IXBCgetstringlength called with %p at %d", ext_ptr, __LINE__);
	return -1;
}

// get the value of a string item
// return string length, -1 on error
int IXBCgetstring(IXBptr_t ext_ptr, char* value, int max_len, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
	int i;
	for(i=0; i<max_len; i++)
	{
		value[i] = '\0';
	}
	(*func)(status_arg, IXCSEVERITY_INFO, "IXBCgetstring called with %p at %d", ext_ptr, __LINE__);
	return -1;
}

// create a string item (length len) - return NULL on error
IXBptr_t IXBCcreatestring(const char* value, int len, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
	(*func)(status_arg, IXCSEVERITY_INFO, "IXBCcreatestring called with %p at %d", value, __LINE__);
	return NULL;
}

// return the value of a logical item
int IXBCgetlogical(IXBptr_t ext_ptr, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
	(*func)(status_arg, IXCSEVERITY_INFO, "IXBCgetlogical called with %p at %d", ext_ptr, __LINE__);
	return 0;
}

// create a new logical object, return NULL on error
IXBptr_t IXBCcreatelogical(int val, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
	(*func)(status_arg, IXCSEVERITY_INFO, "IXBCcreatelogical called at %d", __LINE__);
	return NULL;
}

// get the value of a double
double IXBCgetfloat64(IXBptr_t ext_ptr, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
	(*func)(status_arg, IXCSEVERITY_INFO, "IXBCgetfloat64 called with %p at %d", ext_ptr, __LINE__);
	return 0.0;
}

// create a new double object, return NULL on error
IXBptr_t IXBCcreatefloat64(double d, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
	(*func)(status_arg, IXCSEVERITY_INFO, "IXBCcreatefloat64 called at %d", __LINE__);
	return NULL;
}

// Test if ext_ptr referes to an object of type "type" 
// (which is an IXCTYPE_* constant defined in bindings_generic.h)
// return 1 if is type, 0 if is not, -1 if error/invalid type
int IXBCistype(IXBptr_t ext_ptr, int type, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
	(*func)(status_arg, IXCSEVERITY_INFO, "IXBCistype called with %p at %d", ext_ptr, __LINE__);
    switch(type)
	{
		case IXCTYPE_FLOAT64:
			return 0;

		default:
			(*func)(status_arg, IXCSEVERITY_ERROR, "IXBCistype called with invalid type %d", type);
			return -1;
	}
}

// return number of dimensions of an array descriptor
// returns -1 on error
int IXBCgetnumberofdimensions(IXBptr_t ext_ptr, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
    (*func)(status_arg, IXCSEVERITY_INFO, "IXBCgetnumberofdimensions called with %p at %d", ext_ptr, __LINE__);
	return -1;
}

// return array dimensions (dims_array) and number (ndims) for an array descriptor
// returns number of dimensions of array or -1 on error 
int IXBCgetdimensions(IXBptr_t ext_ptr, int* dims_array, int max_dims, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
	int i;
	for(i=0; i<max_dims; i++)
	{
		dims_array[i] = 0;
	}
    (*func)(status_arg, IXCSEVERITY_INFO, "IXBCgetdimensions called with %p at %d", ext_ptr, __LINE__);
	return -1;
}

// return to pointer to a given field of a structure or structure array element array_index
// return NULL on error
IXBptr_t IXBCgetfieldbyname(IXBptr_t ext_ptr, const char* field, int array_index, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
    (*func)(status_arg, IXCSEVERITY_INFO, "IXBCgetfieldbyname called with %p at %d", ext_ptr, __LINE__);
	return NULL;
}

// Set a given field of a structure or structure array element array_index to the
// specified value
// return 0 on success, -1 on error
int IXBCsetfieldbyname(IXBptr_t ext_ptr, const char* field, int array_index, IXBptr_t value, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
    (*func)(status_arg, IXCSEVERITY_INFO, "IXBCsetfieldbyname called with %p at %d", ext_ptr, __LINE__);
	return -1;
}

// if ext_ptr points to a structure or structure array, and "field" is not present, 
// then create it
// return 0 on success, -1 on error
int IXBCcreatefield(IXBptr_t ext_ptr, const char* field, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
    (*func)(status_arg, IXCSEVERITY_INFO, "IXBCcreatefield called with %p at %d", ext_ptr, __LINE__);
    return -1;
}

// return an external pointer to an array descriptor (which contains
// the array size information and a pointer to the data) of the correct size
// array_type is an IXCTYPE_* constant defined in bindings_generic.h
// return NULL on error
IXBptr_t IXBCallocarraydescriptor(const int* dims_array, int ndims, int array_type, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
    (*func)(status_arg, IXCSEVERITY_INFO, "IXBCallocarraydescriptor called at %d", __LINE__);
    return NULL;
}

// get pointer to the actual array data values from an array descriptor pointer
// return NULL on error
void* IXBCgetarraydata(IXBptr_t ext_ptr, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
    (*func)(status_arg, IXCSEVERITY_INFO, "IXBCgetarraydata called with %p at %d", ext_ptr, __LINE__);
    return NULL;
}

// deallocate array descritor (array size + data information) pointed to by ext_ptr
// return 0 on success, -1 on error
int IXBCdeallocarraydescriptor(IXBptr_t ext_ptr, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
    (*func)(status_arg, IXCSEVERITY_INFO, "IXBCdeallocarraydescriptor called with %p at %d", ext_ptr, __LINE__);
	return -1;
}
