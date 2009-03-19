#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mex.h"

/*
 * $Id: libisisexc.c 1185 2007-07-09 09:22:48Z Dickon Champion $
 *
 * Gateway routine for MATLAB - LIBISISEXC 
 * provides mexFunction() and vectors it to the appropriate library rouitne
 *
 * Author: Freddie Akeroyd, CCLRC ISIS Facility 
 */

/*
 * Stuff needed for defining the functions we export, which are individually listed
 * in "libisisexc.h"
 */
#ifdef _WIN32
#define CALLING_CONVENTION		__stdcall
#define ixbdomatlabcall			IXBDOMATLABCALL
#define ixbcreatematlablogical	IXBCREATEMATLABLOGICAL
#define ixbgetmatlablogical		IXBGETMATLABLOGICAL
#define compare_nocase			stricmp
#else
#define CALLING_CONVENTION
#define ixbdomatlabcall			ixbdomatlabcall_
#define ixbcreatematlablogical	ixbcreatematlablogical_
#define ixbgetmatlablogical		ixbgetmatlablogical_
#include <strings.h>
#define compare_nocase			strcasecmp
#endif /* _WIN32 */


/* 
 * The type  mexfunc_t  and the macro  declare_function()
 * must create the same function signature
 */

#define declare_function(__name) \
  extern void CALLING_CONVENTION      __name(int* nlhs, mxArray *plhs[], \
				const int* nrhs, const mxArray* prhs[], void* status);

typedef void (CALLING_CONVENTION *mexfunc_t)(int* nlhs, mxArray *plhs[],
				const int* nrhs, const mxArray* prhs[], void* status);

/*
 */
typedef struct
{
	const char* name;
	mexfunc_t func;
} mexfunc_s_t;

extern void CALLING_CONVENTION ixbdomatlabcall(mexfunc_t func, int* nlhs, mxArray *plhs[],
					const int* nrhs, const mxArray* prhs[], int* errcode);


#include "libisisexc.h"

/*
 * the libisisexc mex function is called with the class name followed by the operation name
 * as the first two matlab arguments e.g. libisisexc("ixtestclass", "plus")
 * From this a FORTRAN function name is created (ixtestclass_plus) which is then called with 
 * the rest of the parameters
 */
#define BUFFER_LEN	64
#define MAX_ARGS	100

void mexFunction(int nlhs, mxArray *plhs[],
					int nrhs, const mxArray* prhs[])
{
	int i, j, n, nrhs_2, func_called = 0, errcode = 0;
	static int first_call = 0;
	static int call_depth = 0;
	char error_buffer[256];
	const mxArray* new_prhs[MAX_ARGS];
	char classname[BUFFER_LEN+1], classop[BUFFER_LEN+1], funcname[2*BUFFER_LEN+2];
	if (first_call == 0)
	{
		first_call = 1;
		mexLock();
	}
	if (nrhs < 2)
	{
		mexErrMsgTxt("LIBISISEXC: At least two arguments (\"class\", \"class operation\") are required");
	}
	if (nrhs >= MAX_ARGS)
	{
		mexErrMsgTxt("LIBISISEXC: too many varargin arguments");
	}
    if (mxGetString(prhs[0], classname, BUFFER_LEN) != 0)
	{
		mexErrMsgTxt("LIBISISEXC: cannot read argument 1 (class name)");
	}
    if (mxGetString(prhs[1], classop, BUFFER_LEN) != 0)
	{
		mexErrMsgTxt("LIBISISEXC: cannot read argument 2 (class operation name)");
	}
/*
 * NULLify out PLHS as we use this as a test to create them
 */
	for(i=0; i<nlhs; i++)
	{
		plhs[i] = NULL;
	}
    sprintf(funcname, "%s_%s", classname, classop);
	
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
			if (mxIsCell(prhs[i]))
			{
				for(j=0; j < mxGetNumberOfElements(prhs[i]); j++)
				{
					new_prhs[n++] = mxGetCell(prhs[i], j);
					if (n >= MAX_ARGS)
					{
						mexErrMsgTxt("LIBISISEXC: too many varargin arguments");
					}
				}
			}
			else
			{
				new_prhs[n++] = prhs[i];
				if (n >= MAX_ARGS)
				{
					mexErrMsgTxt("LIBISISEXC: too many varargin arguments");
				}
			}
		}
		nrhs_2 = n - 2;
	}
	else
	{
		for(i=0; i<nrhs; i++)
		{
			new_prhs[i] = prhs[i];
		}
		nrhs_2 = nrhs - 2;
	}
	++call_depth;
	if (call_depth > 1)
	{
		call_depth = 0;	/* need to reset */
/*		mexErrMsgTxt("LIBISISEXC: NOT re-entrant"); */
		mexWarnMsgTxt("LIBISISEXC: Possible attempt to make re-entrant call");
		mexWarnMsgTxt("LIBISISEXC: This is often caused by a matlab class constructor not checking for nargin > 0");
	}
	for(i=0; i< (sizeof(mex_functions) / sizeof(mexfunc_s_t)) && !func_called; i++)
	{
		if ( (mex_functions[i].name != NULL) && !compare_nocase(funcname, mex_functions[i].name))
		{
			func_called = 1;
			ixbdomatlabcall(mex_functions[i].func, &nlhs, plhs, &nrhs_2, new_prhs+2, &errcode);
/*			(*(mex_functions[i].func))(&nlhs, plhs, &nrhs_2, prhs+2); */
			if (errcode != 0)
			{
			    sprintf(error_buffer, "LIBISISEXC: error returned from function \"%s\"", funcname);
/*
				matlab will now stop after all errors coming from fortran DLL and not continue
				to reveres this change comments with the next line
*/				
				mexErrMsgTxt(error_buffer);
/*			    mexWarnMsgTxt(error_buffer);  */
			}
		}
	}
	--call_depth;
    if (!func_called)
	{
		sprintf(error_buffer, "LIBISISEXC: cannot find external function \"%s\"", funcname);
		mexErrMsgTxt(error_buffer);
	}
}


mxArray* CALLING_CONVENTION ixbcreatematlablogical(int* value)
{
	if (*value != 0)
	{
		return mxCreateLogicalScalar(1);
	}
	else
	{
		return mxCreateLogicalScalar(0);
	}
}

/* return -1 on error */
int CALLING_CONVENTION ixbgetmatlablogical(mxArray* arg[])
{
    if (mxIsLogical(arg[0]))
	{
		if (mxIsLogicalScalarTrue(arg[0]))
		{
			return 1;
		}
		else
		{
			return 0;
		}
	}
	else
	{
		return -1;
	}
}

/* create object of given class */
mxArray* CALLING_CONVENTION ixbcreateclassobject(const char* class_name)
{
    mxArray* plhs[1] = { NULL };
    if (mexCallMATLAB(1, plhs, 0, NULL, class_name) == 0)
    {
        return plhs[0];	/* SUCCESS */
    }
    else
    {
	return 0;
    }
}

/* create array of n objects of given class */
mxArray* CALLING_CONVENTION ixbcreateclassarray(const char* class_name, int* n)
{
    mxArray* plhs[1] = { NULL };
    mxArray* prhs[2];
    prhs[0] = ixbcreateclassobject(class_name);
    prhs[1] = mxCreateDoubleScalar((double)(*n));
    if (mexCallMATLAB(1, plhs, 2, prhs, "extend") == 0)
    {
        return plhs[0];	/* SUCCESS */
    }
    else
    {
	return 0;
    }
}
