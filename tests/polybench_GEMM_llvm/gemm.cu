///**
// * gemm.cu: This file is part of the PolyBench/GPU 1.0 test suite.
// *
// *
// * Contact: Scott Grauer-Gray <sgrauerg@gmail.com>
// * Louis-Noel Pouchet <pouchet@cse.ohio-state.edu>
// * Web address: http://www.cse.ohio-state.edu/~pouchet/software/polybench/GPU
// */

#include "gemm_utils.h"

#pragma kernel_info_size_param_idx_gemm_kernel = 3;
#pragma kernel_info_dim_gemm_kernel = 2;

void
gemm (DATA_TYPE *A, DATA_TYPE *B, DATA_TYPE *C)
{
  int i, j, k;

  for (i = 0; i < NI; i++)
    {
      for (j = 0; j < NJ; j++)
	{
	  C[i * NJ + j] *= BETA;

	  for (k = 0; k < NK; ++k)
	    {
	      C[i * NJ + j] += ALPHA * A[i * NK + k] * B[k * NJ + j];
	    }
	}
    }
}

void
init (DATA_TYPE *A, DATA_TYPE *B, DATA_TYPE *C)
{
  int i, j;

  for (i = 0; i < NI; i++)
    {
      for (j = 0; j < NK; j++)
	{
	  A[i * NK + j] = ((DATA_TYPE) i * j) / NI;
	}
    }

  for (i = 0; i < NK; i++)
    {
      for (j = 0; j < NJ; j++)
	{
	  B[i * NJ + j] = ((DATA_TYPE) i * j + 1) / NJ;
	}
    }

  for (i = 0; i < NI; i++)
    {
      for (j = 0; j < NJ; j++)
	{
	  C[i * NJ + j] = ((DATA_TYPE) i * j + 2) / NJ;
	}
    }
}

int
compareResults (DATA_TYPE* C, DATA_TYPE* C_outputFromGpu)
{
  int i, j, fail;
  fail = 0;

  // Compare C1 and C2
  for (i = 0; i < NI; i++)
    {
      for (j = 0; j < NJ; j++)
	{
	  if (percentDiff (
	      C[i * NJ + j],
	      C_outputFromGpu[i * NJ + j]) > PERCENT_DIFF_ERROR_THRESHOLD)
	    {
	      fail++;
	      return (EXIT_FAILURE);
	    }
	}
    }

  return (EXIT_SUCCESS);
  // Print results
//	printf("Non-Matching CPU-GPU Outputs Beyond Error Threshold of %4.2f Percent: %d\n", PERCENT_DIFF_ERROR_THRESHOLD, fail);
}

void
GPU_argv_init ()
{
  cudaDeviceProp deviceProp;
  cudaGetDeviceProperties (&deviceProp, GPU_DEVICE);
//	printf("setting device %d with name %s\n",GPU_DEVICE,deviceProp.name);
  printf ("[running on device %d: %s]\n", GPU_DEVICE, deviceProp.name);
  cudaSetDevice ( GPU_DEVICE);
}

__global__ void
gemm_kernel (DATA_TYPE *a, DATA_TYPE *b, DATA_TYPE *c, int NI, int NJ, int NK)
{
  int j = blockIdx.x * blockDim.x + threadIdx.x;
  int i = blockIdx.y * blockDim.y + threadIdx.y;

  if ((i < NI) && (j < NJ))
    {
      c[i * NJ + j] *= BETA;
      int k;
      for (k = 0; k < NK; k++)
	{
	  c[i * NJ + j] += ALPHA * a[i * NK + k] * b[k * NJ + j];
	}
    }
}

void
gemmCuda (DATA_TYPE* A, DATA_TYPE* B, DATA_TYPE* C, DATA_TYPE* C_outputFromGpu)
{
  double t_start, t_end;

  DATA_TYPE *A_gpu;
  DATA_TYPE *B_gpu;
  DATA_TYPE *C_gpu;

  cudaMalloc ((void **) &A_gpu, sizeof(DATA_TYPE) * NI * NK);
  cudaMalloc ((void **) &B_gpu, sizeof(DATA_TYPE) * NK * NJ);
  cudaMalloc ((void **) &C_gpu, sizeof(DATA_TYPE) * NI * NJ);

  cudaMemcpy (A_gpu, A, sizeof(DATA_TYPE) * NI * NK, cudaMemcpyHostToDevice);
  cudaMemcpy (B_gpu, B, sizeof(DATA_TYPE) * NK * NJ, cudaMemcpyHostToDevice);
  cudaMemcpy (C_gpu, C, sizeof(DATA_TYPE) * NI * NJ, cudaMemcpyHostToDevice);

  dim3 block (DIM_THREAD_BLOCK_X, DIM_THREAD_BLOCK_Y);
  dim3 grid ((size_t) (ceil (((float) NI) / ((float) block.x))),
	     (size_t) (ceil (((float) NJ) / ((float) block.y))));

//	t_start = rtclock();
  cuda_timer t;
  cuda_timer_init (t);
  cuda_timer_record_start (t);

  gemm_kernel <<<grid, block>>> (A_gpu, B_gpu, C_gpu, NI, NJ, NK);

  cudaCheckKernel()
  ;
  cuda_timer_record_stop (t);
  cudaThreadSynchronize ();

  cuda_timer_record_get_elapsed_time (t);
//	t_end = rtclock();
//	fprintf(stdout, "GPU Runtime: %0.6lfs\n", t_end - t_start);

  cudaMemcpy (C_outputFromGpu, C_gpu, sizeof(DATA_TYPE) * NI * NJ,
	      cudaMemcpyDeviceToHost);

  printf ("[trace: n=%d, bx=%d, by=%d, elapsed_gemm_kernel=%0.4f (ms)] ... ",
	  NI, DIM_THREAD_BLOCK_X, DIM_THREAD_BLOCK_Y, t.elapsed_time);
  cudaFree (A_gpu);
  cudaFree (B_gpu);
  cudaFree (C_gpu);
}

int
main (int argc, char *argv[])
{
//	double t_start, t_end;

  int n = 256, bx = 32, by = 8;

  if (argc > 1)
    n = atoi (argv[1]);
  if (argc > 2)
    bx = atoi (argv[2]);
  if (argc > 3)
    by = atoi (argv[3]);

  NI = NJ = NK = n;
  DIM_THREAD_BLOCK_X = bx;
  DIM_THREAD_BLOCK_Y = by;

  DATA_TYPE* A;
  DATA_TYPE* B;
  DATA_TYPE* C;
  DATA_TYPE* C_outputFromGpu;

  A = (DATA_TYPE*) malloc (NI * NK * sizeof(DATA_TYPE));
  B = (DATA_TYPE*) malloc (NK * NJ * sizeof(DATA_TYPE));
  C = (DATA_TYPE*) malloc (NI * NJ * sizeof(DATA_TYPE));
  C_outputFromGpu = (DATA_TYPE*) malloc (NI * NJ * sizeof(DATA_TYPE));

  init (A, B, C);

  GPU_argv_init ();

#pragma START_TRACING
  gemmCuda (A, B, C, C_outputFromGpu);
#pragma STOP_TRACING 

//	t_start = rtclock();
  gemm (A, B, C);
//	t_end = rtclock();
//	fprintf(stdout, "CPU Runtime: %0.6lfs\n", t_end - t_start);

  int s = compareResults (C, C_outputFromGpu);
  if (s == EXIT_SUCCESS)
    printf ("PASS\n");
  else
    printf ("FAIL\n");

  free (A);
  free (B);
  free (C);
  free (C_outputFromGpu);

  return 0;
}

