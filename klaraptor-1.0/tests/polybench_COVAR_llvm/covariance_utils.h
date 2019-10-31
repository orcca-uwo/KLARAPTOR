#ifndef COVARIANCE_UTILS_H_
#define COVARIANCE_UTILS_H_



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
#define PERCENT_DIFF_ERROR_THRESHOLD 1.05

#define GPU_DEVICE 0

///* Problem size */
//#define M 2048
//#define N 2048
//
///* Thread block dimensions for kernel 1*/
//#define DIM_THREAD_BLOCK_KERNEL_1_X 256
//#define DIM_THREAD_BLOCK_KERNEL_1_Y 1
//
///* Thread block dimensions for kernel 2*/
//#define DIM_THREAD_BLOCK_KERNEL_2_X 32
//#define DIM_THREAD_BLOCK_KERNEL_2_Y 8
//
///* Thread block dimensions for kernel 3*/
//#define DIM_THREAD_BLOCK_KERNEL_3_X 256
//#define DIM_THREAD_BLOCK_KERNEL_3_Y 1

/* Problem size */
int M = 2048, N = 2048;

/* Thread block dimensions for kernel 1*/
int DIM_THREAD_BLOCK_KERNEL_1_X = 256;
int DIM_THREAD_BLOCK_KERNEL_1_Y = 1;

/* Thread block dimensions for kernel 2*/
int DIM_THREAD_BLOCK_KERNEL_2_X = 32;
int DIM_THREAD_BLOCK_KERNEL_2_Y = 8;

/* Thread block dimensions for kernel 3*/
int DIM_THREAD_BLOCK_KERNEL_3_X = 256;
int DIM_THREAD_BLOCK_KERNEL_3_Y = 1;

#define sqrt_of_array_cell(x,j) sqrt(x[j])

#define FLOAT_N 3214212.01
#define EPS 0.005

/* Can switch DATA_TYPE between float and double */
typedef float DATA_TYPE;


#endif 
