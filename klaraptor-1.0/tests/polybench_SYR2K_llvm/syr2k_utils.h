#ifndef SYR2K_UTILS_H_
#define SYR2K_UTILS_H_


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
#define PERCENT_DIFF_ERROR_THRESHOLD 0.05

#define GPU_DEVICE 0

///* Problem size */
//#define N 2048
//#define M 2048
//
///* Thread block dimensions */
//#define DIM_THREAD_BLOCK_X 32
//#define DIM_THREAD_BLOCK_Y 8

/* Problem size */
int N = 2048;
int M = 2048;

/* Thread block dimensions */
int DIM_THREAD_BLOCK_X = 32;
int DIM_THREAD_BLOCK_Y = 8;

/* Declared constant values for ALPHA and BETA (same as values in PolyBench 2.0) */
#define ALPHA 12435
#define BETA 4546

/* Can switch DATA_TYPE between float and double */
typedef float DATA_TYPE;


#endif

