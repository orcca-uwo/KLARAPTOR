/*!
 \file kernel_invoker.h
 \author Davood Mohajerani <dmohajer@uwo.ca>
 \brief
 */

#ifndef KERNEL_INVOKER_H_
#define KERNEL_INVOKER_H_

#include "cuda.h"
#include "ptx_lookup_manager.h"
#include "cuda_timer.h"
#include <cassert>

#ifndef ERROR_CHECKING_ENABLED
#define ERROR_CHECKING_ENABLED 1
#endif
///////////////////////////////////////////////////////////

#define MAX_KERNEL_NAME_LEN 256

///////////////////////////////////////////////////////////

#if ERROR_CHECKING_ENABLED == 1
#define checkCudaErrors(err)do{\
    if(err != CUDA_SUCCESS){\
	const char * error_str;\
	cuGetErrorName(err, &error_str);\
	printf("[ERROR: %s : LINE %d]\n[CUDA_ERROR_[%d]: %s\n", __FILE__, __LINE__ , err, error_str);\
	exit(EXIT_FAILURE);\
    }\
}while(0);

///////////////////////////////////////////////////////////

#define checkErrors(err)do{\
    if(err != EXIT_SUCCESS){\
	printf("[FILE: %s][LINE: %d]\n", __FILE__, __LINE__);\
	exit(EXIT_FAILURE);\
    }\
}while(0);

#else

#define checkErrors(err)

#define checkCudaErrors(err)

#endif

///////////////////////////////////////////////////////////

typedef enum {
	DISABLED, ENABLED
} status_t;

///////////////////////////////////////////////////////////

//setting the initial value of cu_init_done=0.
struct kernel_invoker_data {
	status_t cu_init_done_status;
	int cu_init_done;
	CUdevice device;
	CUcontext context;
	CUfunction function;
} kernel_invoker_data_default = { DISABLED, 0 };

typedef kernel_invoker_data kernel_invoker_data_t;

///////////////////////////////////////////////////////////

kernel_invoker_data_t global_kernel_invoker_data = kernel_invoker_data_default;

///////////////////////////////////////////////////////////

//declaration
void
kernel_invoker_init(void) __attribute__ ((constructor));

///////////////////////////////////////////////////////////

//declaration
void
kernel_invoker_clear(void) __attribute__ ((destructor));

///////////////////////////////////////////////////////////
int
kernel_invoker_check_cu_init();

///////////////////////////////////////////////////////////

void
set_kernel_launch_params(int* launch_params, dim3 gridDim, dim3 blockDim);

///////////////////////////////////////////////////////////

int
kernel_invoker(char * kernel_name, int* launch_params, void **KernelParams);

///////////////////////////////////////////////////////////

#endif 
