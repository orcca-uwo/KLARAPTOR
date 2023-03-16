#ifndef GEMM_UTILS_H_
#define GEMM_UTILS_H_


#include <unistd.h>
#include <stdio.h>
#include <time.h>
#include <sys/time.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <cuda.h>
#include <math.h>

#include "../polybench_common/polybenchUtilFuncts.h"
#include "../cuda_utils/cuda_timer.h"
#include "../cuda_utils/cuda_error_check.h"

#define GPU_DEVICE 0

//define the error threshold for the results "not matching"
#define PERCENT_DIFF_ERROR_THRESHOLD 0.05

/* Problem size */
//#define NI 512
//#define NJ 512
//#define NK 512
//
///* Thread block dimensions */
//#define DIM_THREAD_BLOCK_X 32
//#define DIM_THREAD_BLOCK_Y 8
int NI = 512, NJ = 512, NK = 512;

/* Thread block dimensions */
int DIM_THREAD_BLOCK_X = 32;
int DIM_THREAD_BLOCK_Y = 8;

/* Declared constant values for ALPHA and BETA (same as values in PolyBench 2.0) */
#define ALPHA 32412.0f
#define BETA 2123.0f

/* Can switch DATA_TYPE between float and double */
typedef float DATA_TYPE;


#endif 
