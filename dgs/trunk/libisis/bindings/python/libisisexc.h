#ifndef LIBISISEXC_H 
#define LIBISISEXC_H 
/* 
 * !!! DO NOT EDIT !!! 
 * !!! Automatically generated from libisisexc.txt by $Id: libisisexc.h 389 2005-03-21 13:41:19Z faa59 $ !!! 
 */ 
#define ixbcreate_base IXBCREATE_BASE  
declare_function(ixbcreate_base) 
#define ixbdisplay_base IXBDISPLAY_BASE  
declare_function(ixbdisplay_base) 
#define ixbcheck_base IXBCHECK_BASE  
declare_function(ixbcheck_base) 
#define ixbplus_testclass IXBPLUS_TESTCLASS  
declare_function(ixbplus_testclass) 
#define ixbdisplay_testclass IXBDISPLAY_TESTCLASS  
declare_function(ixbdisplay_testclass) 
#define ixbcreate_testclass IXBCREATE_TESTCLASS  
declare_function(ixbcreate_testclass) 
#define ixbcheck_testclass IXBCHECK_TESTCLASS  
declare_function(ixbcheck_testclass) 
static mexfunc_s_t mex_functions[] = { 
{ "IXTbase_create", ixbcreate_base }, 
{ "IXTbase_display", ixbdisplay_base }, 
{ "IXTbase_check", ixbcheck_base }, 
{ "IXTtestclass_plus", ixbplus_testclass }, 
{ "IXTtestclass_display", ixbdisplay_testclass }, 
{ "IXTtestclass_create", ixbcreate_testclass }, 
{ "IXTtestclass_check", ixbcheck_testclass }, 
{ NULL, NULL } 
}; 
#endif /* LIBISISEXC_H */ 
