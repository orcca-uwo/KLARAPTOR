#ifndef THREE_DCONV_UTILS_H_
#define THREE_DCONV_UTILS_H_


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

//define the error threshold for the results "not matching"
#define PERCENT_DIFF_ERROR_THRESHOLD 0.5

#define GPU_DEVICE 0

/* Problem size */
//#define NI 256
//#define NJ 256
//#define NK 256
int NI = 256;
int NJ = 256;
int NK = 256;

/* Thread block dimensions */
//#define DIM_THREAD_BLOCK_X 32
//#define DIM_THREAD_BLOCK_Y 8
int DIM_THREAD_BLOCK_X = 32;
int DIM_THREAD_BLOCK_Y = 8;

/* Can switch DATA_TYPE between float and double */
typedef float DATA_TYPE;


#endif
