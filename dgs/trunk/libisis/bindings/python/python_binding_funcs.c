#include <Python.h>
#include "numarray/libnumarray.h"
#include "numarray/arraybase.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "bindings_generic.h"

#define PYTHON_TRUE 1
#define PYTHON_FALSE 0

//Global level variables
const PyObject *objtyp = PyObject_Type(Py_BuildValue("O",Py_None));
const PyObject *floattyp = PyObject_Type(Py_BuildValue("f",NULL));
const PyObject *inttyp = PyObject_Type(Py_BuildValue("i",NULL));
const PyObject *doubletyp = PyObject_Type(Py_BuildValue("d",NULL));
const PyObject *chartyp = PyObject_Type(Py_BuildValue("c",NULL));
const PyObject *longtyp = PyObject_Type(Py_BuildValue("l",NULL));
const PyObject *longlongtyp = PyObject_Type(Py_BuildValue("L",NULL));
const PyObject *complextyp = PyObject_Type(Py_BuildValue("D",Py_None));
const PyObject *listtyp= PyObject_Type(PyList_New(0));
const PyObject *arraytyp= PyObject_Type(NA_NewArray(NULL, tInt32, 0, 0,0));


//Values for NumarrayType to be used
/*
typedef enum
{
tAny,
tBool,
tInt8, tUInt8,
tInt16, tUInt16,
tInt32, tUInt32,
tInt64, tUInt64,
tFloat32, tFloat64,
tComplex32, tComplex64,
tDefault = tFloat64,
#if LP64
tLong = tInt64
#else
tLong = tInt32
#endif
} NumarrayType;
*/



//For string array type?
//For Object type we need other values?

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

//how to print different error message?
PyObject * pythonintp(char *mod,char *func,PyObject *arg,IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
PyObject *pName, *pModule, *pDict, *pFunc;
PyObject *dest;
PyTupleObject *myarg;
int i,n;

Py_Initialize();

pName = PyString_FromString(mod);
pModule = PyImport_Import(pName);
Py_DECREF(pName);
printf("\n import success");
//This problem is there while using initilize
if (pModule != NULL) {
pDict = PyModule_GetDict(pModule);
}
else
{
	(*func)(status_arg, IXCSEVERITY_INFO, "IXBCpythontype called with %p at %d, error is module is null", ext_ptr, __LINE__);
	return NULL;
}
if (pDict == NULL)
{
	(*func)(status_arg, IXCSEVERITY_INFO, "IXBCpythontype called with %p at %d, error is dict is null", ext_ptr, __LINE__);
	return NULL;
}
pFunc = PyDict_GetItemString(pDict, func);
n = PyObject_Print(pFunc,stdout,0);
if (pFunc == NULL)
{
	(*func)(status_arg, IXCSEVERITY_INFO, "IXBCpythontype called with %p at %d, error is func is null", ext_ptr, __LINE__);       
	return NULL;
}
myarg = PyTuple_New(1);
PyTuple_SetItem(myarg, 0, arg);
n = PyObject_Print(myarg,stdout,0);
dest = PyObject_CallObject(pFunc,myarg);
if (dest == NULL)
{
	(*func)(status_arg, IXCSEVERITY_INFO, "IXBCpythontype called with %p at %d, error is dest is null", ext_ptr, __LINE__);
	return NULL;
}
//Py_Finalize();
return dest;
}

NumarrayType IXBCpythontype(int type,IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
	switch(type)
	{
		case IXCTYPE_FLOAT64	:
			return tFloat64;
		case IXCTYPE_FLOAT32	:
			return tFloat32;
		case IXCTYPE_INT32	:
			return tInt32;
		case IXCTYPE_CHAR8	:
			return tInt8;
		default:
			(*func)(status_arg, IXCSEVERITY_INFO, "IXBCpythontype called with %p at %d, error is no numarray type match", ext_ptr, __LINE__);
			return ;
	}
}


IXBptr_t IXBCpythonchktype(int type,IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{

	switch(type)
	{
		case IXCTYPE_FLOAT64	:
			return doubletyp;
		case IXCTYPE_FLOAT32	:
			return floattyp;
		case IXCTYPE_INT32	:
			return inttyp;
		case IXCTYPE_CHAR8	:
			return chartyp;
		case IXCTYPE_CELL	:
			return listyp;
		case IXCTYPE_ARRAY	:
			return arraytyp;
		case IXCTYPE_STRUCT	:
			//believe this is obj
			return objtyp;
		default:
			(*func)(status_arg, IXCSEVERITY_INFO, "IXBCpythontype called with %p at %d error is no match pythontype", ext_ptr, __LINE__);
			return ;
	}
}

int IXBCwriteline(const char* line, int len_line)
{
	//is this matlab specific logic
	//fwrite can be still used in C
	char *tmp;
	tmp = (char*)malloc(1+len_line);
    if (tmp == NULL)
	{
		return -1;
	}
	strncpy(tmp, line, len_line);//why to always copy in buffer ?
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
	
    //(*func)(status_arg, IXCSEVERITY_INFO, "IXBCmakelhs called with %p at %d", ext_ptr, __LINE__);
	return ext_ptr; //this work if ext_ptr is PyObject does not matter where it is created
}

// return the number of elements in ext_ptr (total number of elements in an array, number of fields 
// in a structure, length of a list etc.)
// Return -1 on error
int IXBCgetnumberofelements(IXBptr_t ext_ptr, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
	//int PyObject_Size(PyObject *);
    //(*func)(status_arg, IXCSEVERITY_INFO, "IXBCgetnumberofelements called with %p at %d", ext_ptr, __LINE__);
	return PyObject_Size(ext_ptr);
}

// Make a deep copy of the object pointed to by ext_pointer
// (a deep copy is a copy of all subobjects as well i.e. you cannot just
// copy pointer values but must allocate new objects)
// return NULL on error
//what is this? copy  object
IXBptr_t IXBCduplicateobject(IXBptr_t ext_ptr, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
	IXBptr_t *dest;
	dest = pythonintp("copy","deepcopy",ext_ptr,IXBstatus_func_t func, IXBstatus_arg_t status_arg);
    return dest;
}

// return a pointer to an element of a list or matlab cell array
// return NULL on error
IXBptr_t IXBCgetcell(IXBptr_t ext_ptr, int element_index, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
	//PyObject * = PyObject_GetItem(PyObject *obj, PyObejct *field);
    //(*func)(status_arg, IXCSEVERITY_INFO, "IXBCgetcell called with %p at %d", ext_ptr, __LINE__);
	return PyList_GetItem(ext_ptr,element_index);
}

// Set an element of a list or matlab cell array to the given value
// return 0 on success, -1 on error
int IXBCsetcell(IXBptr_t ext_ptr, int element_index, IXBptr_t value, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
	int status;
	//Only applicable to list
	//int = PyList_SetItem(PyObject *list, int index, PyObject *item);
    //(*func)(status_arg, IXCSEVERITY_INFO, "IXBCsetcell called with %p at %d", ext_ptr, __LINE__);
	status = PyList_SetItem(ext_ptr,element_index,value)
	if (status != 0)
	{//true
		return 0;
	}
	else
	{
		(*func)(status_arg, IXCSEVERITY_INFO, "IXBCsetcell called with %p at %d, error is cannot set cell value", ext_ptr, __LINE__);
	}
	
}

// return length of a string item, -1 on error (not a string etc.)
int IXBCgetstringlength(IXBptr_t ext_ptr, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
	//int = PyObject_Size(PyObject *);
	//(*func)(status_arg, IXCSEVERITY_INFO, "IXBCgetstringlength called with %p at %d", ext_ptr, __LINE__);
	return PyObject_Size(ext_ptr);
}

// get the value of a string item
// return string length, -1 on error
int IXBCgetstring(IXBptr_t ext_ptr, char* value, int max_len, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
	//char * = PyString_AsString(PyObject *);
	int i;
	for(i=0; i<max_len; i++)
	{
		value[i] = '\0';
	}
	value = PyString_AsString(ext_ptr);
	//(*func)(status_arg, IXCSEVERITY_INFO, "IXBCgetstring called with %p at %d", ext_ptr, __LINE__);
	return 0;
}

// create a string item (length len) - return NULL on error
IXBptr_t IXBCcreatestring(const char* value, int len, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
	IXBptr_t *str;
	str = Py_BuildValue("s",value);
	//(*func)(status_arg, IXCSEVERITY_INFO, "IXBCcreatestring called with %p at %d", value, __LINE__);
	return str;
}

// return the value of a logical item
int IXBCgetlogical(IXBptr_t ext_ptr, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
	//boolean function in python for c
	long boolvar;
	boolvar = PyInt_AsLong(ext_ptr);
	return PyBool_FromLong(boolvar); //true or false
	//(*func)(status_arg, IXCSEVERITY_INFO, "IXBCgetlogical called with %p at %d", ext_ptr, __LINE__);
	//return boolvar;
}

// create a new logical object, return NULL on error
IXBptr_t IXBCcreatelogical(int val, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
	//In the application you can apply bool function to get exactly the boolean type
	//Py_BuildValue("s","True"); //'True' is true
	//PyObject * = Py_BuildValue("i",1); //1 is true
	long boolvar;
	IXBptr_t *ext_ptr;
	ext_ptr = Py_BuildValue("i",val); //prepare int
	//boolvar = PyInt_AsLong(ext_ptr); 
	//return PyBool_FromLong(boolvar); 
	//(*func)(status_arg, IXCSEVERITY_INFO, "IXBCcreatelogical called at %d", __LINE__);
	return ext_ptr;
}

// get the value of a double
double IXBCgetfloat64(IXBptr_t ext_ptr, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
	
	//(*func)(status_arg, IXCSEVERITY_INFO, "IXBCgetfloat64 called with %p at %d", ext_ptr, __LINE__);
	return PyFloat_AsDouble(ext_ptr);
}

// create a new double object, return NULL on error
IXBptr_t IXBCcreatefloat64(double d, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
	//
	//(*func)(status_arg, IXCSEVERITY_INFO, "IXBCcreatefloat64 called at %d", __LINE__);
	return Py_BuildValue("d",d);
}

//Signature is changed
// Test if ext_ptr referes to an object of type "type" 
// (which is an IXCTYPE_* constant defined in bindings_generic.h)
// return 1 if is type, 0 if is not, -1 if error/invalid type
//int IXBCistype(IXBptr_t ext_ptr, int type, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
int IXBCistype(IXBptr_t ext_ptr, int type, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
	//type must be PyObject * type;
	PyTypeObject *mytype;
	int n;
    mytype = (PyTypeObject *) IXBCpythonchktype(type,func,status_arg); //several constants with respective values
	n = PyObject_TypeCheck(ext_ptr,mytype); //python true is 1 and false is 0
	if ( n == PYTHON_FALSE )
	{
		(*func)(status_arg, IXCSEVERITY_INFO, "IXBCistype called with %p at %d", ext_ptr, __LINE__);
		return -1;
	}	
	return 0;
	//beaware for if in C.. if(bool) then bool needs to be true i.e 0 not 1 which is false
    /*switch(type)
	{
		case IXCTYPE_FLOAT64:
			return 0;

		default:
			(*func)(status_arg, IXCSEVERITY_ERROR, "IXBCistype called with invalid type %d", type);
			return -1;
	}*/
}

// return number of dimensions of an array descriptor
// returns -1 on error
//?need a type of array
//translate
int IXBCgetnumberofdimensions(IXBptr_t ext_ptr, int type, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
	PyArrayObject *val;
	NumarrayType t;
	t = IXBCpythontype(type,func,status_arg);
	val = NA_InputArray(ext_ptr,t,NUM_C_ARRAY);
	if (val != NULL)
	{
		return val->nd;
	}
	else
	{
		//need to raise the error
		(*func)(status_arg, IXCSEVERITY_INFO, "IXBCgetnumberofdimensions called with %p at %d", ext_ptr, __LINE__);
		return -1;
	}
    //(*func)(status_arg, IXCSEVERITY_INFO, "IXBCgetnumberofdimensions called with %p at %d", ext_ptr, __LINE__);
	//return -1;
	return 0;
}

// return array dimensions (dims_array) and number (ndims) for an array descriptor
// returns number of dimensions of array or -1 on error 
int IXBCgetdimensions(IXBptr_t ext_ptr, int type, int* dims_array, int max_dims, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
	//what?
	int i;
	PyArrayObject *val;
	NumarrayType t;
	int row,col;
	if (max_dims < 2)
	{
		//must be atleast 2
		(*func)(status_arg, IXCSEVERITY_INFO, "IXBCgetnumberofdimensions called with %p at %d", ext_ptr, __LINE__);
		return -1;
	}
	for(i=0; i<max_dims; i++)
	{
		dims_array[i] = 0;
	}
	
	t = IXBCpythontype(type,func,status_arg);

	val = NA_InputArray(ext_ptr,t,NUM_C_ARRAY);
	if (val != NULL)
	{
		row=val->dimensions[0];
		col=val->dimensions[1];
		dims_array[0] = row;
		dims_array[1] = col;
		return row*col;
	}
	else
	{
		//need to raise the error
		(*func)(status_arg, IXCSEVERITY_INFO, "IXBCgetnumberofdimensions called with %p at %d, error is cannot create array", ext_ptr, __LINE__);
		return -1;
	}
    //(*func)(status_arg, IXCSEVERITY_INFO, "IXBCgetdimensions called with %p at %d", ext_ptr, __LINE__);
	//return -1;
}

// return to pointer to a given field of a structure or structure array element array_index
// return NULL on error
//need to change the type
IXBptr_t IXBCgetfieldbyname(IXBptr_t ext_ptr, int type, const char* field, int array_index, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
	PyArrayObject *val;
	NumarrayType t;
	int chk;
	chk = PyArray_Check(ext_ptr);
	if (chk == 1) //array
	{
		t = IXBCpythontype(type,func,status_arg);
		val = NA_InputArray(ext_ptr,t,NUM_C_ARRAY);
		if (val == NULL)
		{
			(*func)(status_arg, IXCSEVERITY_INFO, "IXBCgetnumberofdimensions called with %p at %d, error is cannot create array", ext_ptr, __LINE__);
			return -1;
		}
		return PyObject_GetAttrString(val->data[array_index],field);
	}
	else //just struct
	{
		return PyObject_GetAttrString(ext_ptr,field);
	}
}

// Set a given field of a structure or structure array element array_index to the
// specified value
// return 0 on success, -1 on error
//need to change the type
int IXBCsetfieldbyname(IXBptr_t ext_ptr, const char* field, int array_index, IXBptr_t value, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
	PyArrayObject *val;
	NumarrayType t;
	int chk;
	chk = PyArray_Check(ext_ptr);
	if (chk == 1) //array
	{
		t = IXBCpythontype(type,func,status_arg);
		val = NA_InputArray(ext_ptr,t,NUM_C_ARRAY);
		if (val == NULL)
		{
			(*func)(status_arg, IXCSEVERITY_INFO, "IXBCgetnumberofdimensions called with %p at %d, error is cannot create array", ext_ptr, __LINE__);
			return -1;
		}
		return PyObject_SetAttrString(val->data[array_index],field,value);
	}
	else //just struct
	{
		return PyObject_SetAttrString(ext_ptr,field,value);
	}
	
    //(*func)(status_arg, IXCSEVERITY_INFO, "IXBCsetfieldbyname called with %p at %d", ext_ptr, __LINE__);
	//return -1;
}

// if ext_ptr points to a structure or structure array, and "field" is not present, 
// then create it
// return 0 on success, -1 on error
int IXBCcreatefield(IXBptr_t ext_ptr, const char* field, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
	return PyObject_SetAttrString(ext_ptr,field,NULL);
    //(*func)(status_arg, IXCSEVERITY_INFO, "IXBCcreatefield called with %p at %d", ext_ptr, __LINE__);
    //return -1;
}

// return an external pointer to an array descriptor (which contains
// the array size information and a pointer to the data) of the correct size
// array_type is an IXCTYPE_* constant defined in bindings_generic.h
//you need col,row????
// return NULL on error
IXBptr_t IXBCallocarraydescriptor(const int* dims_array, int ndims, NumarrayType array_type, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
	void *data=NULL;
	//Python array object to be used???
	return NA_vNewArray(data, array_type,ndims,dims_array);
    //(*func)(status_arg, IXCSEVERITY_INFO, "IXBCallocarraydescriptor called at %d", __LINE__);                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
    //return NULL;
}

// get pointer to the actual array data values from an array descriptor pointer
// return NULL on error
void* IXBCgetarraydata(IXBptr_t ext_ptr, NumarrayType array_type,IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
	PyArrayObject *val;
	val = NA_InputArray(ext_ptr,array_type,NUM_C_ARRAY);
	if (val != NULL)
		return val->data;
	else
	{
		(*func)(status_arg, IXCSEVERITY_INFO, "IXBCgetarraydata called with %p at %d", ext_ptr, __LINE__);
	    return NULL;
	}
}

// deallocate array descritor (array size + data information) pointed to by ext_ptr
// return 0 on success, -1 on error
int IXBCdeallocarraydescriptor(IXBptr_t ext_ptr, IXBstatus_func_t func, IXBstatus_arg_t status_arg)
{
	Py_DECREF(ext_ptr);
    //(*func)(status_arg, IXCSEVERITY_INFO, "IXBCdeallocarraydescriptor called with %p at %d", ext_ptr, __LINE__);
	return 0;
}
