#ifndef SYRK_UTILS_H_
#define SYRK_UTILS_H_


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <unistd.h>
#include <sys/time.h>
#include <cuda.h>

#include "../polybench_common/polybenchUtilFuncts.h"
#include "../../mcwp/cuda_timer.h"
#include "../../mcwp/cuda_error_check.h"

//define the error threshold for the results "not matching"
#define PERCENT_DIFF_ERROR_THRESHOLD 0.05

#define GPU_DEVICE 0

///* Problem size */
//#define N 1024
//#define M 1024
//
///* Thread block dimensions */
//#define DIM_THREAD_BLOCK_X 32
//#define DIM_THREAD_BLOCK_Y 8

/* Problem size */
int N = 1024;
int M = 1024;

/* Thread block dimensions */
int DIM_THREAD_BLOCK_X = 32;
int DIM_THREAD_BLOCK_Y = 8;

/* Declared constant values for alpha and beta (same as values in PolyBench 2.0) */
//#define alpha 12435
//#define beta 4546

/* Can switch DATA_TYPE between float and double */
typedef float DATA_TYPE;

DATA_TYPE alpha=12435;
DATA_TYPE beta=4546;


#endif
