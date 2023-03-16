#ifndef THREE_MM_UTILS_H_
#define THREE_MM_UTILS_H_


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <unistd.h>
#include <sys/time.h>
#include <cuda.h>

#include "../polybench_common/polybenchUtilFuncts.h"
#include "../cuda_utils/cuda_timer.h"
#include "../cuda_utils/cuda_error_check.h"

#define GPU_DEVICE 0

//define the error threshold for the results "not matching"
#define PERCENT_DIFF_ERROR_THRESHOLD 0.05

/* Problem size. */
//# define NI 512
//# define NJ 512
//# define NK 512
//# define NL 512
//# define NM 512
//
///* Thread block dimensions */
//#define DIM_THREAD_BLOCK_X 32
//#define DIM_THREAD_BLOCK_Y 8
int NI = 512, NJ = 512, NK = 512, NL = 512, NM = 512;

/* Thread block dimensions */
int DIM_THREAD_BLOCK_X = 32;
int DIM_THREAD_BLOCK_Y = 8;

/* Can switch DATA_TYPE between float and double */
typedef float DATA_TYPE;


#endif
