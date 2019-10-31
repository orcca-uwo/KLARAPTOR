#ifndef BICG_UTILS_H_
#define BICG_UTILS_H_


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <sys/time.h>
#include <cuda.h>

#include "../polybench_common/polybenchUtilFuncts.h"
#include "../../mcwp/cuda_timer.h"
#include "../../mcwp/cuda_error_check.h"

//Error threshold for the results "not matching"
#define PERCENT_DIFF_ERROR_THRESHOLD 0.5

#define GPU_DEVICE 0

/* Problem size. */
//#define NX 4096
//#define NY 4096
//
///* Thread block dimensions */
//#define DIM_THREAD_BLOCK_X 256
//#define DIM_THREAD_BLOCK_Y 1
int NX = 4096, NY = 4096;

/* Thread block dimensions */
int DIM_THREAD_BLOCK_X = 256;
int DIM_THREAD_BLOCK_Y = 1;

#ifndef M_PI
#define M_PI 3.14159
#endif

/* Can switch DATA_TYPE between float and double */
typedef float DATA_TYPE;


#endif
