/*python must be first and then others*/
#include <Python.h>
#include "numarray/libnumarray.h"
#include "numarray/arraybase.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "bindings_generic.h" /* general binding helpers */

/*
 * 1. Entry point is  calledByPython() which is called by the python interpreter
 * 2. The function should construct a function name from its first two arguments 
 *    and then lookup this name to obtain a pointer. e.g. it might be called with 
 *    arguments ("IXTdataset_1d", "plus") and would then lookup "IXTdataset_1d_plus" 
 * 3. look for a special function names ending in "_varargin" - this indicates that
 *    we have passed a list items in the argument list, but want them unwound
 *    into real arguments. COnstruct an argument list as appropriate, also leaving
 *    out the two used above to lookup the name
 * 4. IXBdofortrancall() is then executed, being passed the function pointer
 *    calculated above and the new argument list
 */
//the entry point is python_c_interface.calledByPython but this can be renamed
//like import calledByPython from python_c_interface as libisisexec
//and use it

#include "libisisexc.h"			/* declarations for all libisis functions */

/*
 * the libisisexc mex function is called with the class name followed by the operation name
 * as the first two matlab arguments e.g. libisisexc("ixtestclass", "plus")
 * From this a FORTRAN function name is created (ixtestclass_plus) which is then called with 
 * the rest of the parameters
 */
#define BUFFER_LEN	64
#define MAX_ARGS	100
#define ERROR_LEN 256
#define PYTHON_TRUE 1


//plhs is return values
//prhs are passed values
//nlhs = total retur , nrhs = total input args

/* __declspec(dllexport) void calledByPython(int nlhs, void* plhs[],
					int nrhs, const void* prhs[]) */



__declspec(dllexport) void calledByPython(PyObject *vargs)

{
	int i, j, n, nrhs_2, func_called = 0, errcode = 0;
	static int first_call = 0;
	char error_buffer[ERROR_LEN];
	const void* new_prhs[MAX_ARGS];
	char classname[BUFFER_LEN+1], classop[BUFFER_LEN+1], funcname[2*BUFFER_LEN+2];
//Python variables
	int nlhs; //no of left params
	int nrhs; //no of right parms
	int listCheck; //check list object
	int tot; //tot list size
	PyObject *nestlist; //python nested object
	PyObject *pyprhs; //right pointers
	PyObject *pyplhs; //left pointers
	PyObject *ret; //python object 
	const void* lrhs[MAX_ARGS];
	const void* plhs[MAX_ARGS];

// Python parsing is first in order to know the values of arguments

	if (!PyArg_ParseTuple(vargs, "ssiOiO",&classname,&classop, &pyplhs,&pyprhs))
		printf("\nLIBISISEXC: Argument Type mismatch String|String|Object|Object Required");
		return NULL;
	
	//Check list on both pyprhs pyplhs
	listCheck=PyObject_Check(pyprhs);
	if (listCheck == PYTHON_TRUE)
	{
		printf("\nLIBISISEXC: Argument must be of list/tuple type");
		return NULL;
	}
	listCheck=PyObject_Check(pyplhs);
	if (listCheck == PYTHON_TRUE)
	{
		printf("\nLIBISISEXC: Argument must be of list/tuple type");
		return NULL;
	}
		
	//you have the passed array in PyObject. This is a list
	nrhs = PyObject_Size(pyprhs);
	nlhs = PyObject_Size(pyplhs);

	//Check not equal to 0
	if ( nrhs != 0 || nlhs != 0)
	{
		printf("\n list arguments cannot be 0");
		return NULL;
	}
	
	//for return 
	for (i = 0; i < nlhs; i++) {
                ret = PyObject_GetItem(pyplhs, i); //Python
                PyObject_Print(ret,stdout,0);
                plhs[i]=(void *)ret; //C
				plhs[i]=0;
        }

	printf("\nLIBISISEXC: Arguments for Function are parsed");

	if (first_call == 0)
	{
		first_call = 1;
		IXBinitialize_bindings(binding_func_array, NBINDINGFUNCS);
//		mexLock(); /* This locks DLL in memory */
	}

	
	if (nrhs >= MAX_ARGS)
	{
		printf("\nLIBISISEXC: too many varargin arguments");
		return NULL;
	}

	sprintf(funcname, "%s_%s", classname, classop);
	printf("\nLIBISISEXC: funcname is %s\n",funcname);

/*
 * look for the special case of function name ending in _varargin
 * If we find this, flatten any cell arays we find in prhs (varargin arrays)
 * and then call the relevant function (i.e. the name without varargin)
 */
	i = strlen(funcname);
	if (!compare_nocase(funcname + (i - 9), "_varargin"))
	{
		funcname[i-9] = '\0';	/* remove the trailing "_varargin" from the name */
		n = 0;
		for(i=0; i<nrhs; i++)
		{
			ret = PyObject_GetItem(pyprhs, i); //Python
			listCheck=PyObject_Check(ret);
			PyObject_Print(ret,stdout,0);
            
			// check for python list type probably
			if (ret == PYTHON_TRUE)
			{
				//get nested list elements
				tot=PyObject_Size(ret);
				for (j=0;j<tot;j++)
				{
					nestlist = PyObject_GetItem(pyprhs, j);
					PyObject_Print(nestlist,stdout,0);
					new_prhs[n++] = (void *)nestlist; //C
					if (n >= MAX_ARGS)
					{
						printf("\nLIBISISEXC: too many varargin arguments\n");
						return NULL;
					}
				}
			}
			else
			{
				new_prhs[n++] = (void *)ret;
				if (n >= MAX_ARGS)
				{
					printf("\nLIBISISEXC: too many varargin arguments\n");
					return NULL;
				}
			}

		}
		//nrhs_2 = n - 2; no need to do
	}
	else
	{
		for (i = 0; i < nrhs; i++) {
                ret = PyObject_GetItem(pyprhs, i); //Python
                PyObject_Print(ret,stdout,0);
                prhs[i]=(void *)ret; //C
        }
		
		// nrhs_2 = nrhs - 2;no need 
	}
	for(i=0; i< (sizeof(mex_functions) / sizeof(mexfunc_s_t)) && !func_called; i++)
	{
		if ( (mex_functions[i].name != NULL) && !compare_nocase(funcname, mex_functions[i].name))
		{
			func_called = 1;
			ixbdofortrancall(mex_functions[i].func, &nlhs, plhs, &nrhs_2, new_prhs, &errcode);
			if (errcode != 0)
			{
			    printf("\nLIBISISEXC: error returned from function \"%s\"", funcname);
				return NULL;
			}
		}
	}
    if (!func_called)
	{
		printf("\nLIBISISEXC: cannot find external function \"%s\"", funcname);		
		return NULL;
	}
}

static PyObject * python_c_interface(PyObject *self, PyObject *vargs)
{

PyObject *ret;
printf("\ninside the python_c_inteface function\n");
ret = calledByPython(vargs);
return ret;

}

/*
methods
*/
static PyMethodDef python_c_methods[] = {
{"calledByPython",python_c_interface, METH_VARARGS,
"Execute python to c apis."},
{NULL, NULL} /* Sentinel */
};

/*init function called by python interpreter*/
PyMODINIT_FUNC initpythonc(void) {
(void) Py_InitModule("pycinterface", python_c_methods);
import_libnumarray();
printf("\n\n called python to c interface");
}                                                                          