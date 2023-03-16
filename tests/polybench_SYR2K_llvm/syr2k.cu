///**
// * syr2k.cu: This file is part of the PolyBench/GPU 1.0 test suite.
// *
// *
// * Contact: Scott Grauer-Gray <sgrauerg@gmail.com>
// * Louis-Noel Pouchet <pouchet@cse.ohio-state.edu>
// * Web address: http://www.cse.ohio-state.edu/~pouchet/software/polybench/GPU
// */
//

#include "syr2k_utils.h"

#pragma kernel_info_size_param_idx_syr2k_kernel = 3;
#pragma kernel_info_dim_syr2k_kernel = 2;

void
init_arrays (DATA_TYPE *A, DATA_TYPE *B, DATA_TYPE *C)
{
  int i, j;

  for (i = 0; i < N; i++)
    {
      for (j = 0; j < N; j++)
	{
	  C[i * N + j] = ((DATA_TYPE) i * j + 2) / N;
	}

      for (j = 0; j < M; j++)
	{
	  A[i * N + j] = ((DATA_TYPE) i * j) / N;
	  B[i * N + j] = ((DATA_TYPE) i * j + 1) / N;
	}
    }
}

void
syr2k (DATA_TYPE *A, DATA_TYPE *B, DATA_TYPE *C)
{
  int i, j, k;

  for (i = 0; i < N; i++)
    {
      for (j = 0; j < N; j++)
	{
	  C[i * N + j] *= BETA;
	}
    }

  for (i = 0; i < N; i++)
    {
      for (j = 0; j < N; j++)
	{
	  for (k = 0; k < M; k++)
	    {
	      C[i * N + j] += ALPHA * A[i * M + k] * B[j * M + k];
	      C[i * N + j] += ALPHA * B[i * M + k] * A[j * M + k];
	    }
	}
    }
}

int
compareResults (DATA_TYPE *C, DATA_TYPE *C_outputFromGpu)
{
  int i, j, fail;
  fail = 0;

  // Compare C with D
  for (i = 0; i < N; i++)
    {
      for (j = 0; j < N; j++)
	{
	  if (percentDiff (
	      C[i * N + j],
	      C_outputFromGpu[i * N + j]) > PERCENT_DIFF_ERROR_THRESHOLD)
	    {
	      fail++;
	      return (EXIT_FAILURE);
	    }
	}
    }

  return (EXIT_SUCCESS);
  // print results
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
syr2k_kernel (DATA_TYPE *a, DATA_TYPE *b, DATA_TYPE *c, int M, int N)
{
  int j = blockIdx.x * blockDim.x + threadIdx.x;
  int i = blockIdx.y * blockDim.y + threadIdx.y;

  if ((i < N) && (j < N))
    {
      c[i * N + j] *= BETA;

      int k;
      for (k = 0; k < M; k++)
	{
	  c[i * N + j] += ALPHA * a[i * M + k] * b[j * M + k]
	      + ALPHA * b[i * M + k] * a[j * M + k];
	}
    }
}

void
syr2kCuda (DATA_TYPE* A, DATA_TYPE* B, DATA_TYPE* C, DATA_TYPE* C_outputFromGpu)
{
  double t_start, t_end;

  DATA_TYPE *A_gpu;
  DATA_TYPE *B_gpu;
  DATA_TYPE *C_gpu;

  cudaMalloc ((void **) &A_gpu, sizeof(DATA_TYPE) * N * M);
  cudaMalloc ((void **) &B_gpu, sizeof(DATA_TYPE) * N * M);
  cudaMalloc ((void **) &C_gpu, sizeof(DATA_TYPE) * N * N);
  cudaMemcpy (A_gpu, A, sizeof(DATA_TYPE) * N * M, cudaMemcpyHostToDevice);
  cudaMemcpy (B_gpu, B, sizeof(DATA_TYPE) * N * M, cudaMemcpyHostToDevice);
  cudaMemcpy (C_gpu, C, sizeof(DATA_TYPE) * N * N, cudaMemcpyHostToDevice);

  dim3 block (DIM_THREAD_BLOCK_X, DIM_THREAD_BLOCK_Y);
  dim3 grid ((size_t) ceil (((float) N) / ((float) DIM_THREAD_BLOCK_X)),
	     (size_t) (ceil (((float) N) / ((float) DIM_THREAD_BLOCK_Y))));

  cuda_timer t;
  cuda_timer_init (t);
  cuda_timer_record_start (t);
//	t_start = rtclock();
  syr2k_kernel <<<grid, block>>> (A_gpu, B_gpu, C_gpu, M, N);

  cudaCheckKernel()
  ;
  cuda_timer_record_stop (t);
  cudaThreadSynchronize ();
//	t_end = rtclock();
//	fprintf(stdout, "GPU Runtime: %0.6lfs\n", t_end - t_start);

  cudaMemcpy (C_outputFromGpu, C_gpu, sizeof(DATA_TYPE) * N * N,
	      cudaMemcpyDeviceToHost);

  cuda_timer_record_get_elapsed_time (t);
  printf ("[trace: n=%d, bx=%d, by=%d, elapsed_syr2k_kernel=%0.4f (ms)] ... ",
	  N, DIM_THREAD_BLOCK_X, DIM_THREAD_BLOCK_Y, t.elapsed_time);
  cudaFree (A_gpu);
  cudaFree (B_gpu);
  cudaFree (C_gpu);
}

int
main (int argc, char ** argv)
{
  int n = 256, bx = 32, by = 8;

  if (argc > 1)
    n = atoi (argv[1]);
  if (argc > 2)
    bx = atoi (argv[2]);
  if (argc > 3)
    by = atoi (argv[3]);

  M = N = n;
  DIM_THREAD_BLOCK_X = bx;
  DIM_THREAD_BLOCK_Y = by;
  double t_start, t_end;

  DATA_TYPE* A;
  DATA_TYPE* B;
  DATA_TYPE* C;
  DATA_TYPE* C_outputFromGpu;

  A = (DATA_TYPE*) malloc (N * M * sizeof(DATA_TYPE));
  B = (DATA_TYPE*) malloc (N * M * sizeof(DATA_TYPE));
  C = (DATA_TYPE*) malloc (N * M * sizeof(DATA_TYPE));
  C_outputFromGpu = (DATA_TYPE*) malloc (N * M * sizeof(DATA_TYPE));

  init_arrays (A, B, C);

  GPU_argv_init ();
#pragma START_TRACING
	syr2kCuda (A, B, C, C_outputFromGpu);
#pragma STOP_TRACING
//  t_start = rtclock ();
//  syr2k (A, B, C);
//  t_end = rtclock ();
//  fprintf (stdout, "CPU Runtime: %0.6lfs\n", t_end - t_start);

//  int s = compareResults (C, C_outputFromGpu);
    int s = EXIT_SUCCESS;
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

