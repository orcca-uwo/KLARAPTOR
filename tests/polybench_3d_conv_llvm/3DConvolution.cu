///**
// * 3DConvolution.cu: This file is part of the PolyBench/GPU 1.0 test suite.
// *
// *
// * Contact: Scott Grauer-Gray <sgrauerg@gmail.com>
// * Louis-Noel Pouchet <pouchet@cse.ohio-state.edu>
// * Web address: http://www.cse.ohio-state.edu/~pouchet/software/polybench/GPU
// */
//

#include "3dconv_utils.h"
#pragma kernel_info_size_param_idx_convolution3D_kernel = 3;
#pragma kernel_info_dim_convolution3D_kernel = 2;

void
conv3D (DATA_TYPE* A, DATA_TYPE* B)
{
  int i, j, k;
  DATA_TYPE c11, c12, c13, c21, c22, c23, c31, c32, c33;

  c11 = +2;
  c21 = +5;
  c31 = -8;
  c12 = -3;
  c22 = +6;
  c32 = -9;
  c13 = +4;
  c23 = +7;
  c33 = +10;

  for (i = 1; i < NI - 1; ++i) // 0
    {
      for (j = 1; j < NJ - 1; ++j) // 1
	{
	  for (k = 1; k < NK - 1; ++k) // 2
	    {
	      //printf("i:%d\nj:%d\nk:%d\n", i, j, k);
	      B[i * (NK * NJ) + j * NK + k] = c11
		  * A[(i - 1) * (NK * NJ) + (j - 1) * NK + (k - 1)]
		  + c13 * A[(i + 1) * (NK * NJ) + (j - 1) * NK + (k - 1)]
		  + c21 * A[(i - 1) * (NK * NJ) + (j - 1) * NK + (k - 1)]
		  + c23 * A[(i + 1) * (NK * NJ) + (j - 1) * NK + (k - 1)]
		  + c31 * A[(i - 1) * (NK * NJ) + (j - 1) * NK + (k - 1)]
		  + c33 * A[(i + 1) * (NK * NJ) + (j - 1) * NK + (k - 1)]
		  + c12 * A[(i + 0) * (NK * NJ) + (j - 1) * NK + (k + 0)]
		  + c22 * A[(i + 0) * (NK * NJ) + (j + 0) * NK + (k + 0)]
		  + c32 * A[(i + 0) * (NK * NJ) + (j + 1) * NK + (k + 0)]
		  + c11 * A[(i - 1) * (NK * NJ) + (j - 1) * NK + (k + 1)]
		  + c13 * A[(i + 1) * (NK * NJ) + (j - 1) * NK + (k + 1)]
		  + c21 * A[(i - 1) * (NK * NJ) + (j + 0) * NK + (k + 1)]
		  + c23 * A[(i + 1) * (NK * NJ) + (j + 0) * NK + (k + 1)]
		  + c31 * A[(i - 1) * (NK * NJ) + (j + 1) * NK + (k + 1)]
		  + c33 * A[(i + 1) * (NK * NJ) + (j + 1) * NK + (k + 1)];
	    }
	}
    }
}

void
init (DATA_TYPE* A)
{
  int i, j, k;

  for (i = 0; i < NI; ++i)
    {
      for (j = 0; j < NJ; ++j)
	{
	  for (k = 0; k < NK; ++k)
	    {
	      A[i * (NK * NJ) + j * NK + k] = i % 12 + 2 * (j % 7)
		  + 3 * (k % 13);
	    }
	}
    }
}

int
compareResults (DATA_TYPE* B, DATA_TYPE* B_outputFromGpu)
{
  int i, j, k, fail;
  fail = 0;

  // Compare result from cpu and gpu...
  for (i = 1; i < NI - 1; ++i) // 0
    {
      for (j = 1; j < NJ - 1; ++j) // 1
	{
	  for (k = 1; k < NK - 1; ++k) // 2
	    {
	      if (percentDiff (
		  B[i * (NK * NJ) + j * NK + k],
		  B_outputFromGpu[i * (NK * NJ) + j * NK + k]) > PERCENT_DIFF_ERROR_THRESHOLD)
		{
		  fail++;
		  return (EXIT_FAILURE);

		}
	    }
	}
    }

  // Print results
//  printf (
//      "Non-Matching CPU-GPU Outputs Beyond Error Threshold of %4.2f Percent: %d\n",
//      PERCENT_DIFF_ERROR_THRESHOLD,
//      fail);
  return (EXIT_SUCCESS);
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
convolution3D_kernel (DATA_TYPE *A, DATA_TYPE *B, int i, int NI, int NJ, int NK)
{
  int k = blockIdx.x * blockDim.x + threadIdx.x;
  int j = blockIdx.y * blockDim.y + threadIdx.y;

  DATA_TYPE c11, c12, c13, c21, c22, c23, c31, c32, c33;

  c11 = +2;
  c21 = +5;
  c31 = -8;
  c12 = -3;
  c22 = +6;
  c32 = -9;
  c13 = +4;
  c23 = +7;
  c33 = +10;

  if ((i < (NI - 1)) && (j < (NJ - 1)) && (k < (NK - 1)) && (i > 0) && (j > 0)
      && (k > 0))
    {
      B[i * (NK * NJ) + j * NK + k] = c11
	  * A[(i - 1) * (NK * NJ) + (j - 1) * NK + (k - 1)]
	  + c13 * A[(i + 1) * (NK * NJ) + (j - 1) * NK + (k - 1)]
	  + c21 * A[(i - 1) * (NK * NJ) + (j - 1) * NK + (k - 1)]
	  + c23 * A[(i + 1) * (NK * NJ) + (j - 1) * NK + (k - 1)]
	  + c31 * A[(i - 1) * (NK * NJ) + (j - 1) * NK + (k - 1)]
	  + c33 * A[(i + 1) * (NK * NJ) + (j - 1) * NK + (k - 1)]
	  + c12 * A[(i + 0) * (NK * NJ) + (j - 1) * NK + (k + 0)]
	  + c22 * A[(i + 0) * (NK * NJ) + (j + 0) * NK + (k + 0)]
	  + c32 * A[(i + 0) * (NK * NJ) + (j + 1) * NK + (k + 0)]
	  + c11 * A[(i - 1) * (NK * NJ) + (j - 1) * NK + (k + 1)]
	  + c13 * A[(i + 1) * (NK * NJ) + (j - 1) * NK + (k + 1)]
	  + c21 * A[(i - 1) * (NK * NJ) + (j + 0) * NK + (k + 1)]
	  + c23 * A[(i + 1) * (NK * NJ) + (j + 0) * NK + (k + 1)]
	  + c31 * A[(i - 1) * (NK * NJ) + (j + 1) * NK + (k + 1)]
	  + c33 * A[(i + 1) * (NK * NJ) + (j + 1) * NK + (k + 1)];
    }
}

void
convolution3DCuda (DATA_TYPE* A, DATA_TYPE* B, DATA_TYPE* B_outputFromGpu)
{
  double t_start, t_end;

  DATA_TYPE *A_gpu;
  DATA_TYPE *B_gpu;

  cudaMalloc ((void **) &A_gpu, sizeof(DATA_TYPE) * NI * NJ * NK);
  cudaMalloc ((void **) &B_gpu, sizeof(DATA_TYPE) * NI * NJ * NK);
  cudaMemcpy (A_gpu, A, sizeof(DATA_TYPE) * NI * NJ * NK,
	      cudaMemcpyHostToDevice);
  cudaMemcpy (B_gpu, B, sizeof(DATA_TYPE) * NI * NJ * NK,
	      cudaMemcpyHostToDevice);

  dim3 block (DIM_THREAD_BLOCK_X, DIM_THREAD_BLOCK_Y);
  dim3 grid ((size_t) (ceil (((float) NK) / ((float) block.x))),
	     (size_t) (ceil (((float) NJ) / ((float) block.y))));

//  t_start = rtclock ();
  cuda_timer t_conv3d;
  cuda_timer_init (t_conv3d);

  int i;
  cuda_timer_record_start (t_conv3d);
  for (i = 1; i < NI - 1; ++i) // 0
    {
      convolution3D_kernel <<<grid, block>>> (A_gpu, B_gpu, i, NI, NJ, NK);
      cudaCheckKernel()
      ;
    }
  cuda_timer_record_stop (t_conv3d);

  cudaThreadSynchronize ();
//  t_end = rtclock ();
//  fprintf (stdout, "GPU Runtime: %0.6lfs\n", t_end - t_start);

  cuda_timer_record_get_elapsed_time (t_conv3d);
  cudaMemcpy (B_outputFromGpu, B_gpu, sizeof(DATA_TYPE) * NI * NJ * NK,
	      cudaMemcpyDeviceToHost);

  printf (
      "[trace: n=%d, bx=%d, by=%d, elapsed_convolution3D_kernel=%0.4f (ms)] ... ",
      NI, DIM_THREAD_BLOCK_X, DIM_THREAD_BLOCK_Y, t_conv3d.elapsed_time);
  cudaFree (A_gpu);
  cudaFree (B_gpu);

}

int
main (int argc, char *argv[])
{
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

//  double t_start, t_end;

  DATA_TYPE* A;
  DATA_TYPE* B;
  DATA_TYPE* B_outputFromGpu;

  A = (DATA_TYPE*) malloc (NI * NJ * NK * sizeof(DATA_TYPE));
  B = (DATA_TYPE*) malloc (NI * NJ * NK * sizeof(DATA_TYPE));
  B_outputFromGpu = (DATA_TYPE*) malloc (NI * NJ * NK * sizeof(DATA_TYPE));

  init (A);

  GPU_argv_init ();

#pragma START_TRACING
  convolution3DCuda (A, B, B_outputFromGpu);
#pragma STOP_TRACING

//  t_start = rtclock ();
  conv3D (A, B);
//  t_end = rtclock ();
//  fprintf (stdout, "CPU Runtime: %0.6lfs\n", t_end - t_start);

  int s = compareResults (B, B_outputFromGpu);
  if (s == EXIT_SUCCESS)
    printf ("PASS\n");
  else
    printf ("FAIL\n");

  free (A);
  free (B);
  free (B_outputFromGpu);

  return 0;
}

