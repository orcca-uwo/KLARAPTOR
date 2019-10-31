///**
// * atax.cu: This file is part of the PolyBench/GPU 1.0 test suite.
// *
// *
// * Contact: Scott Grauer-Gray <sgrauerg@gmail.com>
// * Louis-Noel Pouchet <pouchet@cse.ohio-state.edu>
// * Web address: http://www.cse.ohio-state.edu/~pouchet/software/polybench/GPU
// */
//

#include "atax_utils.h"

#pragma kernel_info_size_param_idx_atax_kernel1 = 3;
#pragma kernel_info_dim_atax_kernel1 = 2;

#pragma kernel_info_size_param_idx_atax_kernel2 = 3;
#pragma kernel_info_dim_atax_kernel2 = 2;

void
init_array (DATA_TYPE *x, DATA_TYPE *A)
{
  int i, j;

  for (i = 0; i < NX; i++)
    {
      x[i] = i * M_PI;
      for (j = 0; j < NY; j++)
	{
	  A[i * NY + j] = ((DATA_TYPE) i * (j)) / NX;
	}
    }
}

int
compareResults (DATA_TYPE *z, DATA_TYPE *z_outputFromGpu)
{
  int i, fail;
  fail = 0;

  for (i = 0; i < NY; i++)
    {
      if (percentDiff (z[i], z_outputFromGpu[i]) > PERCENT_DIFF_ERROR_THRESHOLD)
	{
	  fail++;
	  return (EXIT_FAILURE);
	}
    }

  // print results
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
//  cudaSetDevice ( GPU_DEVICE);
}

__global__ void
atax_kernel1 (DATA_TYPE *A, DATA_TYPE *x, DATA_TYPE *tmp, int NX, int NY)
{
  int i = blockIdx.x * blockDim.x + threadIdx.x;

  if (i < NX)
    {
      int j;
      for (j = 0; j < NY; j++)
	{
	  tmp[i] += A[i * NY + j] * x[j];
	}
    }
}

__global__ void
atax_kernel2 (DATA_TYPE *A, DATA_TYPE *y, DATA_TYPE *tmp, int NX, int NY)
{
  int j = blockIdx.x * blockDim.x + threadIdx.x;

  if (j < NY)
    {
      int i;
      for (i = 0; i < NX; i++)
	{
	  y[j] += A[i * NY + j] * tmp[i];
	}
    }
}

void
atax_cpu (DATA_TYPE* A, DATA_TYPE* x, DATA_TYPE* y, DATA_TYPE* tmp)
{
  int i, j;

  for (i = 0; i < NY; i++)
    {
      y[i] = 0;
    }

  for (i = 0; i < NX; i++)
    {
      tmp[i] = 0;

      for (j = 0; j < NY; j++)
	{
	  tmp[i] = tmp[i] + A[i * NY + j] * x[j];
	}

      for (j = 0; j < NY; j++)
	{
	  y[j] = y[j] + A[i * NY + j] * tmp[i];
	}
    }
}

void
ataxGpu (DATA_TYPE* A, DATA_TYPE* x, DATA_TYPE* y, DATA_TYPE* tmp,
	 DATA_TYPE* y_outputFromGpu)
{
  double t_start, t_end;

  DATA_TYPE *A_gpu;
  DATA_TYPE *x_gpu;
  DATA_TYPE *y_gpu;
  DATA_TYPE *tmp_gpu;

  cudaMalloc ((void **) &A_gpu, sizeof(DATA_TYPE) * NX * NY);
  cudaMalloc ((void **) &x_gpu, sizeof(DATA_TYPE) * NY);
  cudaMalloc ((void **) &y_gpu, sizeof(DATA_TYPE) * NY);
  cudaMalloc ((void **) &tmp_gpu, sizeof(DATA_TYPE) * NX);

  cudaMemcpy (A_gpu, A, sizeof(DATA_TYPE) * NX * NY, cudaMemcpyHostToDevice);
  cudaMemcpy (x_gpu, x, sizeof(DATA_TYPE) * NY, cudaMemcpyHostToDevice);
  cudaMemcpy (y_gpu, y, sizeof(DATA_TYPE) * NY, cudaMemcpyHostToDevice);
  cudaMemcpy (tmp_gpu, tmp, sizeof(DATA_TYPE) * NX, cudaMemcpyHostToDevice);

  dim3 block (DIM_THREAD_BLOCK_X, DIM_THREAD_BLOCK_Y);
  dim3 grid1 ((size_t) (ceil (((float) NX) / ((float) block.x))), 1);
  dim3 grid2 ((size_t) (ceil (((float) NY) / ((float) block.x))), 1);

//	t_start = rtclock();

  cuda_timer t1, t2;
  cuda_timer_init (t1);
  cuda_timer_init (t2);

  cuda_timer_record_start (t1);
  atax_kernel1 <<<grid1, block>>> (A_gpu, x_gpu, tmp_gpu, NX, NY);
  cudaCheckKernel ()
  ;
  cuda_timer_record_stop (t1);
  cudaThreadSynchronize ();

  cuda_timer_record_start (t2);
  atax_kernel2 <<<grid2, block>>> (A_gpu, y_gpu, tmp_gpu, NX, NY);
  cudaCheckKernel ()
  ;
  cuda_timer_record_stop (t2);
  cudaThreadSynchronize ();
//	t_end = rtclock();
//	fprintf(stdout, "GPU Runtime: %0.6lfs\n", t_end - t_start);

  cudaMemcpy (y_outputFromGpu, y_gpu, sizeof(DATA_TYPE) * NX,
	      cudaMemcpyDeviceToHost);

  cuda_timer_record_get_elapsed_time (t1);
  cuda_timer_record_get_elapsed_time (t2);
  printf ("[trace: n=%d, bx=%d, by=%d, "
	  "elapsed_atax_kernel1=%0.4f (ms), "
	  "elapsed_atax_kernel2=%0.4f (ms)] ... \n",
	  NX, DIM_THREAD_BLOCK_X, DIM_THREAD_BLOCK_Y, t1.elapsed_time,
	  t2.elapsed_time);

  cudaFree (A_gpu);
  cudaFree (x_gpu);
  cudaFree (y_gpu);
  cudaFree (tmp_gpu);
  cuda_timer_destroy (t1);
  cuda_timer_destroy (t2);
}

int
main (int argc, char** argv)
{
//	double t_start, t_end;

  int n = 256, bx = 32, by = 8, n_repeat=1, verify=0;


  bx=DIM_THREAD_BLOCK_X;
  by=DIM_THREAD_BLOCK_Y;

  if (argc > 1)
    n = atoi (argv[1]);
  if (argc > 2)
    bx = atoi (argv[2]);
  if (argc > 3)
    by = atoi (argv[3]);
  if (argc > 4)
    n_repeat = atoi (argv[4]);
	if (argc > 5)
    verify = atoi (argv[5]);

  NX = NY = n;
  DIM_THREAD_BLOCK_X = bx;
  DIM_THREAD_BLOCK_Y = by;

  DATA_TYPE* A;
  DATA_TYPE* x;
  DATA_TYPE* y;
  DATA_TYPE* y_outputFromGpu;
  DATA_TYPE* tmp;

  A = (DATA_TYPE*) malloc (NX * NY * sizeof(DATA_TYPE));
  x = (DATA_TYPE*) malloc (NY * sizeof(DATA_TYPE));
  y = (DATA_TYPE*) malloc (NY * sizeof(DATA_TYPE));
  y_outputFromGpu = (DATA_TYPE*) malloc (NY * sizeof(DATA_TYPE));
  tmp = (DATA_TYPE*) malloc (NX * sizeof(DATA_TYPE));

  init_array (x, A);

  GPU_argv_init ();
	
	for(int i=0;i<n_repeat;i++)
	  ataxGpu (A, x, y, tmp, y_outputFromGpu);

	if(verify)
	{
//  t_start = rtclock ();
	for(int i=0;i<n_repeat;i++)
  	atax_cpu (A, x, y, tmp);
//  t_end = rtclock ();
//  fprintf (stdout, "CPU Runtime: %0.6lfs\n", t_end - t_start);

  int s = compareResults (y, y_outputFromGpu);
  if (s == EXIT_SUCCESS)
    printf ("PASS\n");
  else
    printf ("FAIL\n");
	}
  free (A);
  free (x);
  free (y);
  free (y_outputFromGpu);
  free (tmp);

  return 0;
}

