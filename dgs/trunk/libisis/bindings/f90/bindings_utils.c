/*
 * $Id: bindings_utils.c 698 2006-05-10 12:24:56Z faa59 $
 *
 * Support routines for the FORTRAN bindings placeholders/stubs 
 *
 */

#include <stdlib.h>

/*
 * IXIcalloc is called by IXIallocArrayDescriptor in libcore/IXMmemory
 *
 * By defining this memory allocator we can test and make use of
 * IXFalloc and the memory bookeeping mechanism without the use of
 * external (e.g. MATLAB) bindings
 */

#ifdef _WIN32
  void* __stdcall IXICALLOC(int* size)
#else
  void* ixicalloc_(int* size)
#endif /* _WIN32 */
{
    if (size != NULL)
    {
        return malloc(*size);
    }
    else
    {
	return NULL;
    }
}

#ifdef _WIN32
  int __stdcall IXICDEALLOC(void** address)
#else
  int ixicdealloc_(void** address)
#endif /* _WIN32 */
{
	if ((address != NULL) && (*address != NULL))
	{
		free(*address);
	}
	return 0;
}
