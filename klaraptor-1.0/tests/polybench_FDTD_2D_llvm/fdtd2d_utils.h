#ifndef FDTD2D_UTILS_H_
#define FDTD2D_UTILS_H_


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <unistd.h>
#include <sys/time.h>
#include <cuda.h>


#include "../cuda_utils/cuda_timer.h"
#include "../cuda_utils/cuda_error_check.h"
#include "../polybench_common/polybenchUtilFuncts.h"

//define the error threshold for the results "not matching"
#define PERCENT_DIFF_ERROR_THRESHOLD 10.05

#define GPU_DEVICE 0

/* Problem size */
#define tmax 500
//#define NX 2048
//#define NY 2048
//
///* Thread block dimensions */
//#define DIM_THREAD_BLOCK_X 32
//#define DIM_THREAD_BLOCK_Y 8

int NX = 2048;
int NY = 2048;

/* Thread block dimensions */
int DIM_THREAD_BLOCK_X = 32;
int DIM_THREAD_BLOCK_Y = 8;

/* Can switch DATA_TYPE between float and double */
typedef float DATA_TYPE;


#endif 
