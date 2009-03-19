#ifdef _WIN32
#define CALL_MODE __stdcall
#define c_associate_integer4 C_ASSOCIATE_INTEGER4
#define c_associate_real8 C_ASSOCIATE_REAL8
#define c_associate_array_dp1 C_ASSOCIATE_ARRAY_DP1
#define c_associate_array_dp2 C_ASSOCIATE_ARRAY_DP2
#define c_associate_array_dp3 C_ASSOCIATE_ARRAY_DP3
#define c_associate_array_dp4 C_ASSOCIATE_ARRAY_DP4
#define c_associate_array_i1 C_ASSOCIATE_ARRAY_I1
#define c_associate_array_i2 C_ASSOCIATE_ARRAY_I2
#define c_associate_array_i3 C_ASSOCIATE_ARRAY_I3
#define c_associate_array_i4 C_ASSOCIATE_ARRAY_I4
#define c_associate_array_c1 C_ASSOCIATE_ARRAY_C1
#define c_hashmemory C_HASHMEMORY
#elif defined(__xAPPLE__) /* not needed now */
#define CALL_MODE
#define c_associate_integer4 c_associate_integer4_
#define c_associate_real8 c_associate_real8_
#define c_associate_array_dp1 c_associate_array_dp1_
#define c_associate_array_dp2 c_associate_array_dp2_
#define c_associate_array_dp3 c_associate_array_dp3_
#define c_associate_array_dp4 c_associate_array_dp4_
#define c_associate_array_i1 c_associate_array_i1_
#define c_associate_array_i2 c_associate_array_i2_
#define c_associate_array_i3 c_associate_array_i3_
#define c_associate_array_i4 c_associate_array_i4_
#define c_associate_array_c1 c_associate_array_c1_
#define c_hashmemory c_hashmemory_
#else
#define CALL_MODE
#define c_associate_integer4 c_associate_integer4__
#define c_associate_real8 c_associate_real8__
#define c_associate_array_dp1 c_associate_array_dp1__
#define c_associate_array_dp2 c_associate_array_dp2__
#define c_associate_array_dp3 c_associate_array_dp3__
#define c_associate_array_dp4 c_associate_array_dp4__
#define c_associate_array_i1 c_associate_array_i1__
#define c_associate_array_i2 c_associate_array_i2__
#define c_associate_array_i3 c_associate_array_i3__
#define c_associate_array_i4 c_associate_array_i4__
#define c_associate_array_c1 c_associate_array_c1__
#define c_hashmemory c_hashmemory__
#endif /* _WIN32 */

/* create hash between 1 and *hash to use as fortran array index */
int CALL_MODE c_hashmemory(const void* address, const int* hash)
{
    return ((unsigned long)address % (*hash) + 1);
}

void CALL_MODE c_associate_integer4(int** ptr, int* value)
{
    *value = **ptr;
}

void CALL_MODE c_associate_real8(double** ptr, double* value)
{
    *value = **ptr;
}

#if 0
typedef void (CALL_MODE *dp1func_t)(void*, double*, int*);
typedef void (CALL_MODE *dp2func_t)(void*, double*, int*, int*);
typedef void (CALL_MODE *dp3func_t)(void*, double*, int*, int*, int*);
typedef void (CALL_MODE *dp4func_t)(void*, double*, int*, int*, int*, int*);

void CALL_MODE c_associate_array_dp1(void* ptr, double** external_ptr, int* n1, dp1func_t func)
{
    (*func)(ptr, *external_ptr, n1);
}

void CALL_MODE c_associate_array_dp2(void* ptr, double** external_ptr, int* n1, int* n2, dp2func_t func)
{
    (*func)(ptr, *external_ptr, n1, n2);
}

void CALL_MODE c_associate_array_dp3(void* ptr, double** external_ptr, int* n1, int* n2, int* n3, dp3func_t func)
{
    (*func)(ptr, *external_ptr, n1, n2, n3);
}

void CALL_MODE c_associate_array_dp4(void* ptr, double** external_ptr, int* n1, int* n2, int* n3, int* n4, dp4func_t func)
{
    (*func)(ptr, *external_ptr, n1, n2, n3, n4);
}

typedef void (CALL_MODE *i1func_t)(void*, int*, int*);
typedef void (CALL_MODE *i2func_t)(void*, int*, int*, int*);
typedef void (CALL_MODE *i3func_t)(void*, int*, int*, int*, int*);
typedef void (CALL_MODE *i4func_t)(void*, int*, int*, int*, int*, int*);

void CALL_MODE c_associate_array_i1(void* ptr, int** external_ptr, int* n1, i1func_t func)
{
    (*func)(ptr, *external_ptr, n1);
}

void CALL_MODE c_associate_array_i2(void* ptr, int** external_ptr, int* n1, int* n2, i2func_t func)
{
    (*func)(ptr, *external_ptr, n1, n2);
}

void CALL_MODE c_associate_array_i3(void* ptr, int** external_ptr, int* n1, int* n2, int* n3, i3func_t func)
{
    (*func)(ptr, *external_ptr, n1, n2, n3);
}

void CALL_MODE c_associate_array_i4(void* ptr, int** external_ptr, int* n1, int* n2, int* n3, int* n4, i4func_t func)
{
    (*func)(ptr, *external_ptr, n1, n2, n3, n4);
}

typedef void (CALL_MODE *c1func_t)(void*, char*, int*);
void CALL_MODE c_associate_array_c1(void* ptr, char** external_ptr, int* n1, c1func_t func)
{
    (*func)(ptr, *external_ptr, n1);
}

#endif /* if 0 */
