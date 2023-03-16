#ifndef GRAMSCHMIDT_UTILS_H_
#define GRAMSCHMIDT_UTILS_H_


#include <unistd.h>
#include <stdio.h>
#include <time.h>
#include <sys/time.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <cuda.h>

//#include "../polybench_common/polybenchUtilFuncts.h"
#include "../polybench_common/polybenchUtilFuncts.h"
#include "../cuda_utils/cuda_timer.h"
#include "../cuda_utils/cuda_error_check.h"

//define the error threshold for the results "not matching"
#define PERCENT_DIFF_ERROR_THRESHOLD 0.05

#define GPU_DEVICE 0

///* Problem size */
//#define M 2048
//#define N 2048
//
///* Thread block dimensions */
//#define DIM_THREAD_BLOCK_X 256
//#define DIM_THREAD_BLOCK_Y 1

/* Problem size */
int M = 2048;
int N = 2048;

/* Thread block dimensions */
int DIM_THREAD_BLOCK_X = 256;
int DIM_THREAD_BLOCK_Y = 1;

/* Can switch DATA_TYPE between float and double */
typedef float DATA_TYPE;


#endif 
