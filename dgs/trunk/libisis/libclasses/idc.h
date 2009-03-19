#ifndef IDC_H
#define IDC_H

struct idc_info;
typedef struct idc_info* idc_handle_t;

typedef void (*idc_error_report_t)(int status, int code, const char* messsage);

#ifdef __cplusplus
extern "C" {
#endif


int IDCopen(const char* file, int mode, int options, idc_handle_t* fh);
int IDCclose(idc_handle_t* fh);

/* A ones allocate memory */
int IDCgetpari(idc_handle_t fh, const char* name, int* value, int dims_array[], int* ndims);
int IDCAgetpari(idc_handle_t fh, const char* name, int** value, int dims_array[], int* ndims);
int IDCgetparr(idc_handle_t fh, const char* name, float* value, int dims_array[], int* ndims);
int IDCAgetparr(idc_handle_t fh, const char* name, float** value, int dims_array[], int* ndims);
int IDCgetpard(idc_handle_t fh, const char* name, double* value, int dims_array[], int* ndims);
int IDCAgetparc(idc_handle_t fh, const char* name, char** value, int dims_array[], int* ndims);
int IDCgetdat(idc_handle_t fh, int ifsn, int nos, int* value, int dims_array[], int* ndims);
int IDCAgetdat(idc_handle_t fh, int ifsn, int nos, int** value, int dims_array[], int* ndims);

int IDCsetreportfunc(idc_error_report_t report_func);
int IDCreport(int status, int code, const char* format, ... );

#ifdef __cplusplus
}
#endif

#endif /* IDC_H */
