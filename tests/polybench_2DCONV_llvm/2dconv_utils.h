#ifndef TWO_DCONV_UTILS_H_
#define TWO_DCONV_UTILS_H_

#include <unistd.h>
#include <stdio.h>
#include <time.h>
#include <sys/time.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <cuda.h>
#include <cmath>

#include "../cuda_utils/cuda_timer.h"
#include "../cuda_utils/cuda_error_check.h"
#include "../polybench_common/polybenchUtilFuncts.h"

//define the error threshold for the results "not matching"
#define PERCENT_DIFF_ERROR_THRESHOLD 0.05
#define GPU_DEVICE 0

int NI, NJ;

/* Problem size */
//#define NI 4096
//#define NJ 4096
/* Thread block dimensions */
int DIM_THREAD_BLOCK_X = 32;
int DIM_THREAD_BLOCK_Y = 8;

/* Can switch DATA_TYPE between float and double */
typedef float DATA_TYPE;

#endif 
