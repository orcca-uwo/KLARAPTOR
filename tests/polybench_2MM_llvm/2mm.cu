///**
// * 2mm.cu: This file is part of the PolyBench/GPU 1.0 test suite.
// *
// *
// * Contact: Scott Grauer-Gray <sgrauerg@gmail.com>
// * Louis-Noel Pouchet <pouchet@cse.ohio-state.edu>
// * Web address: http://www.cse.ohio-state.edu/~pouchet/software/polybench/GPU
// */
//
#include "2mm_utils.h"

#pragma kernel_info_size_param_idx_mm2_kernel1 = 3;
#pragma kernel_info_size_param_idx_mm2_kernel2 = 3;
#pragma kernel_info_dim_mm2_kernel1 = 2;
#pragma kernel_info_dim_mm2_kernel2 = 2;

void
init_array (DATA_TYPE* A, DATA_TYPE* B, DATA_TYPE* C, DATA_TYPE* D)
{
  int i, j;

  for (i = 0; i < NI; i++)
    {
      for (j = 0; j < NK; j++)
	{
	  A[i * NI + j] = ((DATA_TYPE) i * j) / NI;
	}
    }

  for (i = 0; i < NK; i++)
    {
      for (j = 0; j < NJ; j++)
	{
	  B[i * NK + j] = ((DATA_TYPE) i * (j + 1)) / NJ;
	}
    }

  for (i = 0; i < NL; i++)
    {
      for (j = 0; j < NJ; j++)
	{
	  C[i * NL + j] = ((DATA_TYPE) i * (j + 3)) / NL;
	}
    }

  for (i = 0; i < NI; i++)
    {
      for (j = 0; j < NL; j++)
	{
	  D[i * NL + j] = ((DATA_TYPE) i * (j + 2)) / NK;
	}
    }
}

int
compareResults (DATA_TYPE *E, DATA_TYPE *E_outputFromGpu)
{
  int i, j, fail;
  fail = 0;

  for (i = 0; i < NL; i++)
    {
      for (j = 0; j < NI; j++)
	{
	  if (percentDiff (
	      E[i * NI + j],
	      E_outputFromGpu[i * NI + j]) > PERCENT_DIFF_ERROR_THRESHOLD)
	    {
	      fail++;
	      return (EXIT_FAILURE);
	    }
	}
    }

  return EXIT_SUCCESS;
  // print results
//	printf("Non-Matching CPU-GPU Outputs Beyond Error Threshold of %4.2f Percent: %d\n", PERCENT_DIFF_ERROR_THRESHOLD, fail);
}

void
GPU_argv_init ()
{
  cudaDeviceProp deviceProp;
  cudaGetDeviceProperties (&deviceProp, GPU_DEVICE);
//  printf ("setting device %d with name %s\n", GPU_DEVICE, deviceProp.name);
  printf ("[running on device %d: %s]\n", GPU_DEVICE, deviceProp.name);
  cudaSetDevice ( GPU_DEVICE);
}

__global__ void
mm2_kernel1 (DATA_TYPE *A, DATA_TYPE *B, DATA_TYPE *C, int NI, int NJ, int NK,
	     int NL)
{
  int j = blockIdx.x * blockDim.x + threadIdx.x;
  int i = blockIdx.y * blockDim.y + threadIdx.y;

  if ((i < NI) && (j < NJ))
    {
      int k;
      for (k = 0; k < NK; k++)
	{
	  C[i * NJ + j] += A[i * NK + k] * B[k * NJ + j];
	}
    }
}

__global__ void
mm2_kernel2 (DATA_TYPE *C, DATA_TYPE *D, DATA_TYPE *E, int NI, int NJ, int NK,
	     int NL)
{
  int j = blockIdx.x * blockDim.x + threadIdx.x;
  int i = blockIdx.y * blockDim.y + threadIdx.y;

  if ((i < NI) && (j < NL))
    {
      int k;
      for (k = 0; k < NJ; k++)
	{
	  E[i * NL + j] += C[i * NJ + k] * D[k * NL + j];
	}
    }
}

void
mm2_cpu (DATA_TYPE* A, DATA_TYPE* B, DATA_TYPE* C, DATA_TYPE* D, DATA_TYPE* E)
{
  int i, j, k;

  for (i = 0; i < NI; i++)
    {
      for (j = 0; j < NJ; j++)
	{
	  C[i * NJ + j] = 0.0;
	  for (k = 0; k < NK; ++k)
	    {
	      C[i * NJ + j] += A[i * NK + k] * B[k * NJ + j];
	    }
	}
    }

  for (i = 0; i < NI; i++)
    {
      for (j = 0; j < NL; j++)
	{
	  E[i * NL + j] = 0.0;
	  for (k = 0; k < NJ; ++k)
	    {
	      E[i * NL + j] += C[i * NJ + k] * D[k * NL + j];
	    }
	}
    }
}

void
mm2Cuda (DATA_TYPE* A, DATA_TYPE* B, DATA_TYPE* C, DATA_TYPE* D, DATA_TYPE* E,
	 DATA_TYPE* E_outputFromGpu)
{
  double t_start, t_end;

  DATA_TYPE *A_gpu;
  DATA_TYPE *B_gpu;
  DATA_TYPE *C_gpu;
  DATA_TYPE *D_gpu;
  DATA_TYPE *E_gpu;

  cudaMalloc ((void **) &A_gpu, sizeof(DATA_TYPE) * NI * NK);
  cudaMalloc ((void **) &B_gpu, sizeof(DATA_TYPE) * NK * NJ);
  cudaMalloc ((void **) &C_gpu, sizeof(DATA_TYPE) * NI * NJ);
  cudaMalloc ((void **) &D_gpu, sizeof(DATA_TYPE) * NJ * NL);
  cudaMalloc ((void **) &E_gpu, sizeof(DATA_TYPE) * NI * NL);

  cudaMemcpy (A_gpu, A, sizeof(DATA_TYPE) * NI * NK, cudaMemcpyHostToDevice);
  cudaMemcpy (B_gpu, B, sizeof(DATA_TYPE) * NK * NJ, cudaMemcpyHostToDevice);
  cudaMemcpy (C_gpu, C, sizeof(DATA_TYPE) * NK * NJ, cudaMemcpyHostToDevice);
  cudaMemcpy (D_gpu, D, sizeof(DATA_TYPE) * NJ * NL, cudaMemcpyHostToDevice);
  cudaMemcpy (E_gpu, E, sizeof(DATA_TYPE) * NI * NL, cudaMemcpyHostToDevice);

  dim3 block (DIM_THREAD_BLOCK_X, DIM_THREAD_BLOCK_Y);
  dim3 grid1 ((size_t) ceil (((float) NJ) / ((float) block.x)),
	      (size_t) ceil (((float) NI) / ((float) block.y)));
  dim3 grid2 ((size_t) ceil (((float) NL) / ((float) block.x)),
	      (size_t) ceil (((float) NI) / ((float) block.y)));
//  t_start = rtclock ();

  cuda_timer t_mm2_k1, t_mm2_k2;
  cuda_timer_init (t_mm2_k1);
  cuda_timer_init (t_mm2_k2);
  cuda_timer_record_start (t_mm2_k1);
  mm2_kernel1 <<<grid1, block>>> (A_gpu, B_gpu, C_gpu, NI, NJ, NK, NL);
  cudaCheckKernel()
  ;
  cuda_timer_record_stop (t_mm2_k1);
  cudaThreadSynchronize ();

  cuda_timer_record_start (t_mm2_k2);
  mm2_kernel2 <<<grid2, block>>> (C_gpu, D_gpu, E_gpu, NI, NJ, NK, NL);
  cudaCheckKernel ()
  ;
  cuda_timer_record_stop (t_mm2_k2);
  cudaThreadSynchronize ();

  cuda_timer_record_get_elapsed_time (t_mm2_k1);
  cuda_timer_record_get_elapsed_time (t_mm2_k2);

//  t_end = rtclock ();
//  fprintf (stdout, "GPU Runtime: %0.6lfs\n", t_end - t_start);

  cudaMemcpy (E_outputFromGpu, E_gpu, sizeof(DATA_TYPE) * NI * NL,
	      cudaMemcpyDeviceToHost);

  printf (
      "[trace: n=%d, bx=%d, by=%d, elapsed_mm2_kernel1=%0.4f (ms), elapsed_mm2_kernel2=%0.4f (ms)] ... ",
      NI, DIM_THREAD_BLOCK_X, DIM_THREAD_BLOCK_Y, t_mm2_k1.elapsed_time,
      t_mm2_k2.elapsed_time);
  cudaFree (A_gpu);
  cudaFree (B_gpu);
  cudaFree (C_gpu);
  cudaFree (D_gpu);
  cudaFree (E_gpu);

  cuda_timer_destroy (t_mm2_k1);
  cuda_timer_destroy (t_mm2_k2);

}

int
main (int argc, char** argv)
{
//	double t_start, t_end;

  int n = 256, bx = 32, by = 8;

  if (argc > 1)
    n = atoi (argv[1]);
  if (argc > 2)
    bx = atoi (argv[2]);
  if (argc > 3)
    by = atoi (argv[3]);

  NI = NJ = NK = NL = n;
  DIM_THREAD_BLOCK_X = bx;
  DIM_THREAD_BLOCK_Y = by;

  DATA_TYPE* C;
  DATA_TYPE* A;
  DATA_TYPE* B;
  DATA_TYPE* D;
  DATA_TYPE* E;
  DATA_TYPE* E_outputFromGpu;

  C = (DATA_TYPE*) malloc (NI * NJ * sizeof(DATA_TYPE));
  A = (DATA_TYPE*) malloc (NI * NK * sizeof(DATA_TYPE));
  B = (DATA_TYPE*) malloc (NK * NJ * sizeof(DATA_TYPE));
  D = (DATA_TYPE*) malloc (NJ * NL * sizeof(DATA_TYPE));
  E = (DATA_TYPE*) malloc (NI * NL * sizeof(DATA_TYPE));
  E_outputFromGpu = (DATA_TYPE*) malloc (NI * NL * sizeof(DATA_TYPE));

  init_array (A, B, C, D);
  GPU_argv_init ();

#pragma START_TRACING
  mm2Cuda (A, B, C, D, E, E_outputFromGpu);
#pragma STOP_TRACING

//	t_start = rtclock();
  mm2_cpu (A, B, C, D, E);
//	t_end = rtclock();
//	fprintf(stdout, "CPU Runtime: %0.6lfs\n", t_end - t_start);

  int s = compareResults (E, E_outputFromGpu);
  if (s == EXIT_SUCCESS)
    printf ("PASS\n");
  else
    printf ("FAIL\n");

  free (C);
  free (A);
  free (B);
  free (D);
  free (E);
  free (E_outputFromGpu);

  return 0;
}

