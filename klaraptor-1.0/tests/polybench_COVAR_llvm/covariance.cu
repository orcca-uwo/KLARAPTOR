///**
// * covariance.cu: This file is part of the PolyBench/GPU 1.0 test suite.
// *
// *
// * Contact: Scott Grauer-Gray <sgrauerg@gmail.com>
// * Louis-Noel Pouchet <pouchet@cse.ohio-state.edu>
// * Web address: http://www.cse.ohio-state.edu/~pouchet/software/polybench/GPU
// */

#include "covariance_utils.h"

#pragma kernel_info_size_param_idx_mean_kernel = 2;
#pragma kernel_info_dim_mean_kernel = 2;

#pragma kernel_info_size_param_idx_reduce_kernel = 2;
#pragma kernel_info_dim_reduce_kernel = 2;

#pragma kernel_info_size_param_idx_covar_kernel = 2;
#pragma kernel_info_dim_covar_kernel = 2;


void
init_arrays (DATA_TYPE* data)
{
  int i, j;

  for (i = 1; i < (M + 1); i++)
    {
      for (j = 1; j < (N + 1); j++)
	{
	  data[i * (N + 1) + j] = ((DATA_TYPE) i * j) / M;
	}
    }
}

void
covariance (DATA_TYPE* data, DATA_TYPE* symmat, DATA_TYPE* mean)
{
  int i, j, j1, j2;

  /* Determine mean of column vectors of input data matrix */
  for (j = 1; j < (M + 1); j++)
    {
      mean[j] = 0.0;
      for (i = 1; i < (N + 1); i++)
	{
	  mean[j] += data[i * (M + 1) + j];
	}
      mean[j] /= FLOAT_N;
    }

  /* Center the column vectors. */
  for (i = 1; i < (N + 1); i++)
    {
      for (j = 1; j < (M + 1); j++)
	{
	  data[i * (M + 1) + j] -= mean[j];
	}
    }

  /* Calculate the m * m covariance matrix. */
  for (j1 = 1; j1 < (M + 1); j1++)
    {
      for (j2 = j1; j2 < (M + 1); j2++)
	{
	  symmat[j1 * (M + 1) + j2] = 0.0;
	  for (i = 1; i < N + 1; i++)
	    {
	      symmat[j1 * (M + 1) + j2] += data[i * (M + 1) + j1]
		  * data[i * (M + 1) + j2];
	    }
	  symmat[j2 * (M + 1) + j1] = symmat[j1 * (M + 1) + j2];
	}
    }
}

int
compareResults (DATA_TYPE* symmat, DATA_TYPE* symmat_outputFromGpu)
{
  int i, j, fail;
  fail = 0;

  for (i = 1; i < (M + 1); i++)
    {
      for (j = 1; j < (N + 1); j++)
	{
	  if (percentDiff (
	      symmat[i * (N + 1) + j],
	      symmat_outputFromGpu[i * (N + 1) + j]) > PERCENT_DIFF_ERROR_THRESHOLD)
	    {
	      fail++;
	      return (EXIT_FAILURE);
	    }
	}
    }
  return (EXIT_SUCCESS);
//	printf("Non-Matching CPU-GPU Outputs Beyond Error Threshold of %4.2f Percent: %d\n", PERCENT_DIFF_ERROR_THRESHOLD, fail);
}

void
GPU_argv_init ()
{
  cudaDeviceProp deviceProp;
  cudaGetDeviceProperties (&deviceProp, GPU_DEVICE);
//	printf("setting device %d with name %s\n",GPU_DEVICE,deviceProp.name);
  printf ("[running on device %d: %s]\n", GPU_DEVICE, deviceProp.name);

//  cudaSetDevice ( GPU_DEVICE);

  return;
}

__global__ void
mean_kernel (DATA_TYPE *mean, DATA_TYPE *data, int M, int N)
{
  int j = blockIdx.x * blockDim.x + threadIdx.x + 1;

  if ((j >= 1) && (j < (M + 1)))
    {
      mean[j] = 0.0;

      int i;
      for (i = 1; i < (N + 1); i++)
	{
	  mean[j] += data[i * (M + 1) + j];
	}
      mean[j] /= (DATA_TYPE) FLOAT_N;
    }
}

__global__ void
reduce_kernel (DATA_TYPE *mean, DATA_TYPE *data, int M, int N)
{
  int j = blockIdx.x * blockDim.x + threadIdx.x + 1;
  int i = blockIdx.y * blockDim.y + threadIdx.y + 1;

  if ((i >= 1) && (i < (N + 1)) && (j >= 1) && (j < (M + 1)))
    {
      data[i * (M + 1) + j] -= mean[j];
    }
}

__global__ void
covar_kernel (DATA_TYPE *symmat, DATA_TYPE *data, int M, int N)
{
  int j1 = blockIdx.x * blockDim.x + threadIdx.x + 1;
  int i, j2;

  if ((j1 >= 1) && (j1 < (M + 1)))
    {
      for (j2 = j1; j2 < (M + 1); j2++)
	{
	  symmat[j1 * (M + 1) + j2] = 0.0;
	  for (i = 1; i < (N + 1); i++)
	    {
	      symmat[j1 * (M + 1) + j2] += data[i * (M + 1) + j1]
		  * data[i * (M + 1) + j2];
	    }
	  symmat[j2 * (M + 1) + j1] = symmat[j1 * (M + 1) + j2];
	}
    }
}

void
covarianceCuda (DATA_TYPE* data, DATA_TYPE* symmat, DATA_TYPE* mean,
		DATA_TYPE* symmat_outputFromGpu)
{
  double t_start, t_end;

  DATA_TYPE *data_gpu;
  DATA_TYPE *mean_gpu;
  DATA_TYPE *symmat_gpu;

  cudaMalloc ((void **) &data_gpu, sizeof(DATA_TYPE) * (M + 1) * (N + 1));
  cudaMalloc ((void **) &symmat_gpu, sizeof(DATA_TYPE) * (M + 1) * (M + 1));
  cudaMalloc ((void **) &mean_gpu, sizeof(DATA_TYPE) * (M + 1));
  cudaMemcpy (data_gpu, data, sizeof(DATA_TYPE) * (M + 1) * (N + 1),
	      cudaMemcpyHostToDevice);
  cudaMemcpy (symmat_gpu, symmat, sizeof(DATA_TYPE) * (M + 1) * (M + 1),
	      cudaMemcpyHostToDevice);
  cudaMemcpy (mean_gpu, mean, sizeof(DATA_TYPE) * (M + 1),
	      cudaMemcpyHostToDevice);

  dim3 block1 (DIM_THREAD_BLOCK_KERNEL_1_X, DIM_THREAD_BLOCK_KERNEL_1_Y);
  dim3 grid1 (
      (size_t) (ceil ((float) M) / ((float) DIM_THREAD_BLOCK_KERNEL_1_X)), 1);

  dim3 block2 (DIM_THREAD_BLOCK_KERNEL_2_X, DIM_THREAD_BLOCK_KERNEL_2_Y);
  dim3 grid2 (
      (size_t) (ceil ((float) M) / ((float) DIM_THREAD_BLOCK_KERNEL_2_X)),
      (size_t) (ceil ((float) N) / ((float) DIM_THREAD_BLOCK_KERNEL_2_X)));

  dim3 block3 (DIM_THREAD_BLOCK_KERNEL_3_X, DIM_THREAD_BLOCK_KERNEL_3_Y);
  dim3 grid3 (
      (size_t) (ceil ((float) M) / ((float) DIM_THREAD_BLOCK_KERNEL_3_X)), 1);

//	t_start = rtclock();

  cuda_timer t1, t2, t3;

  cuda_timer_init (t1);
  cuda_timer_init (t2);
  cuda_timer_init (t3);

  cuda_timer_record_start (t1);
  mean_kernel <<<grid1, block1>>> (mean_gpu, data_gpu, M, N);
  cudaCheckKernel ()
  ;
  cuda_timer_record_stop (t1);
  cudaThreadSynchronize ();

  cuda_timer_record_start (t2);
  reduce_kernel <<<grid2, block2>>> (mean_gpu, data_gpu, M, N);
  cudaCheckKernel ()
  ;
  cuda_timer_record_stop (t2);
  cudaThreadSynchronize ();

  cuda_timer_record_start (t3);
  covar_kernel <<<grid3, block3>>> (symmat_gpu, data_gpu, M, N);
  cudaCheckKernel ()
  ;
  cuda_timer_record_stop (t3);
  cudaThreadSynchronize ();

  cudaMemcpy (symmat_outputFromGpu, symmat_gpu,
	      sizeof(DATA_TYPE) * (M + 1) * (N + 1), cudaMemcpyDeviceToHost);

  cuda_timer_record_get_elapsed_time(t1);
  cuda_timer_record_get_elapsed_time(t2);
  cuda_timer_record_get_elapsed_time(t3);
  printf ("[trace: n=%d, bx=%d, by=%d, "
	  "elapsed_mean_kernel=%0.4f (ms),\n"
	  " elapsed_reduce_kernel=%0.4f (ms), "
	  " elapsed_covar_kernel=%0.4f (ms)] ... ",
	  M, DIM_THREAD_BLOCK_KERNEL_3_X, DIM_THREAD_BLOCK_KERNEL_3_Y,
	  t1.elapsed_time, t2.elapsed_time, t3.elapsed_time);
  cudaFree (data_gpu);
  cudaFree (symmat_gpu);
  cudaFree (mean_gpu);
  cuda_timer_destroy (t1);
  cuda_timer_destroy (t2);
  cuda_timer_destroy (t3);
}

int
main (int argc, char** argv)
{
//  double t_start, t_end;

  DATA_TYPE* data;
  DATA_TYPE* symmat;
  DATA_TYPE* mean;
  DATA_TYPE* symmat_outputFromGpu;

  int n = 256, bx = 32, by = 8, n_repeat=1, verify=0;

  bx=DIM_THREAD_BLOCK_KERNEL_1_X;
  by=DIM_THREAD_BLOCK_KERNEL_1_Y;

  if (argc > 1)
    n = atoi (argv[1]);
  if (argc > 2)
    bx = atoi (argv[2]);
  if (argc > 3)
    by = atoi (argv[3]);
  if (argc>4)
  	n_repeat=atoi(argv[4]);
	if (argc>5)
  	verify=atoi(argv[5]);

  M = N = n;
  DIM_THREAD_BLOCK_KERNEL_1_X = bx;
  DIM_THREAD_BLOCK_KERNEL_1_Y = by;
  DIM_THREAD_BLOCK_KERNEL_2_X = bx;
  DIM_THREAD_BLOCK_KERNEL_2_Y = by;
  DIM_THREAD_BLOCK_KERNEL_3_X = bx;
  DIM_THREAD_BLOCK_KERNEL_3_Y = by;

  data = (DATA_TYPE*) malloc ((M + 1) * (N + 1) * sizeof(DATA_TYPE));
  symmat = (DATA_TYPE*) malloc ((M + 1) * (M + 1) * sizeof(DATA_TYPE));
  mean = (DATA_TYPE*) malloc ((M + 1) * sizeof(DATA_TYPE));
  symmat_outputFromGpu = (DATA_TYPE*) malloc (
      (M + 1) * (M + 1) * sizeof(DATA_TYPE));

  init_arrays (data);

  GPU_argv_init ();

	for(int i=0;i<n_repeat;i++)
	  covarianceCuda (data, symmat, mean, symmat_outputFromGpu);
	
	if(verify)
	{
//  t_start = rtclock ();
	for(int i=0;i<n_repeat;i++)
	  covariance (data, symmat, mean);
//  t_end = rtclock ();
//  fprintf (stdout, "CPU Runtime: %0.6lfs\n", t_end - t_start);

  int s = compareResults (symmat, symmat_outputFromGpu);
  if (s == EXIT_SUCCESS)
    printf ("PASS\n");
  else
    printf ("FAIL\n");
	}
  free (data);
  free (symmat);
  free (mean);
  free (symmat_outputFromGpu);

  return 0;
}

