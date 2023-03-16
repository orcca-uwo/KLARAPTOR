
#include "covariance_utils.h"

///////////////////////////////////////
/////// AUTOMATICALLY ANNOTATED ///////
///////////////////////////////////////
#include "kernel_invoker.h"
///////////////////////////////////////
///////////////////////////////////////
const int kernel_info_size_param_idx_mean_kernel __attribute__((used))  = 2;
const int kernel_info_dim_mean_kernel __attribute__((used))  = 2;
const int kernel_info_size_param_idx_reduce_kernel __attribute__((used))  = 2;
const int kernel_info_dim_reduce_kernel __attribute__((used))  = 2;
const int kernel_info_size_param_idx_covar_kernel __attribute__((used))  = 2;
const int kernel_info_dim_covar_kernel __attribute__((used))  = 2;
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
  
  for (j = 1; j < (M + 1); j++)
    {
      mean[j] = 0.0;
      for (i = 1; i < (N + 1); i++)
 {
   mean[j] += data[i * (M + 1) + j];
 }
      mean[j] /= FLOAT_N;
    }
  
  for (i = 1; i < (N + 1); i++)
    {
      for (j = 1; j < (M + 1); j++)
 {
   data[i * (M + 1) + j] -= mean[j];
 }
    }
  
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
       symmat[i * (N + 1) + j],       symmat_outputFromGpu[i * (N + 1) + j]) > PERCENT_DIFF_ERROR_THRESHOLD)
     {
       fail++;
       return (EXIT_FAILURE);
     }
 }
    }
  return (EXIT_SUCCESS);
}
void
GPU_argv_init ()
{
  cudaDeviceProp deviceProp;
  cudaGetDeviceProperties (&deviceProp, GPU_DEVICE);
  printf ("[running on device %d: %s]\n", GPU_DEVICE, deviceProp.name);
  cudaSetDevice ( GPU_DEVICE);
  return;
}
//__global__ void
//mean_kernel (DATA_TYPE *mean, DATA_TYPE *data, int M, int N)
//{
//  int j = blockIdx.x * blockDim.x + threadIdx.x + 1;
//  if ((j >= 1) && (j < (M + 1)))
//    {
//      mean[j] = 0.0;
//      int i;
//      for (i = 1; i < (N + 1); i++)
// {
//   mean[j] += data[i * (M + 1) + j];
// }
//      mean[j] /= (DATA_TYPE) FLOAT_N;
//    }
//}
//__global__ void
//reduce_kernel (DATA_TYPE *mean, DATA_TYPE *data, int M, int N)
//{
//  int j = blockIdx.x * blockDim.x + threadIdx.x + 1;
//  int i = blockIdx.y * blockDim.y + threadIdx.y + 1;
//  if ((i >= 1) && (i < (N + 1)) && (j >= 1) && (j < (M + 1)))
//    {
//      data[i * (M + 1) + j] -= mean[j];
//    }
//}
//__global__ void
//covar_kernel (DATA_TYPE *symmat, DATA_TYPE *data, int M, int N)
//{
//  int j1 = blockIdx.x * blockDim.x + threadIdx.x + 1;
//  int i, j2;
//  if ((j1 >= 1) && (j1 < (M + 1)))
//    {
//      for (j2 = j1; j2 < (M + 1); j2++)
// {
//   symmat[j1 * (M + 1) + j2] = 0.0;
//   for (i = 1; i < (N + 1); i++)
//     {
//       symmat[j1 * (M + 1) + j2] += data[i * (M + 1) + j1]
//    * data[i * (M + 1) + j2];
//     }
//   symmat[j2 * (M + 1) + j1] = symmat[j1 * (M + 1) + j2];
// }
//    }
//}
void
covarianceCuda (DATA_TYPE* data, DATA_TYPE* symmat, DATA_TYPE* mean,  DATA_TYPE* symmat_outputFromGpu)
{
  double t_start, t_end;
  DATA_TYPE *data_gpu;
  DATA_TYPE *mean_gpu;
  DATA_TYPE *symmat_gpu;
  cudaMalloc ((void **) &data_gpu, sizeof(DATA_TYPE) * (M + 1) * (N + 1));
  cudaMalloc ((void **) &symmat_gpu, sizeof(DATA_TYPE) * (M + 1) * (M + 1));
  cudaMalloc ((void **) &mean_gpu, sizeof(DATA_TYPE) * (M + 1));
  cudaMemcpy (data_gpu, data, sizeof(DATA_TYPE) * (M + 1) * (N + 1),       cudaMemcpyHostToDevice);
  cudaMemcpy (symmat_gpu, symmat, sizeof(DATA_TYPE) * (M + 1) * (M + 1),       cudaMemcpyHostToDevice);
  cudaMemcpy (mean_gpu, mean, sizeof(DATA_TYPE) * (M + 1),       cudaMemcpyHostToDevice);
  dim3 block1 (DIM_THREAD_BLOCK_KERNEL_1_X, DIM_THREAD_BLOCK_KERNEL_1_Y);
  dim3 grid1 (
      (size_t) (ceil ((float) M) / ((float) DIM_THREAD_BLOCK_KERNEL_1_X)), 1);
  dim3 block2 (DIM_THREAD_BLOCK_KERNEL_2_X, DIM_THREAD_BLOCK_KERNEL_2_Y);
  dim3 grid2 (
      (size_t) (ceil ((float) M) / ((float) DIM_THREAD_BLOCK_KERNEL_2_X)),      (size_t) (ceil ((float) N) / ((float) DIM_THREAD_BLOCK_KERNEL_2_X)));
  dim3 block3 (DIM_THREAD_BLOCK_KERNEL_3_X, DIM_THREAD_BLOCK_KERNEL_3_Y);
  dim3 grid3 (
      (size_t) (ceil ((float) M) / ((float) DIM_THREAD_BLOCK_KERNEL_3_X)), 1);
  cuda_timer t1, t2, t3;
  cuda_timer_init (t1);
  cuda_timer_init (t2);
  cuda_timer_init (t3);
  cuda_timer_record_start (t1);
  	
 ////////////////////////////////////////////////////////////////////////
 ////////// WARNING: AUTOMATICALLY ANNOTATED REGION BEGINS HERE /////////
 ////////////////////////////////////////////////////////////////////////
	
 

 char kernel_mean_kernel_0_name[] = "kernel_mean_kernel_sm_75";
 
 //launch_params: 3 for grid_dim, 3 for block_dim, 1 for dynamic_shared_mem_bytes;
 int kernel_mean_kernel_sm_75_0_launch_params[6];
 set_kernel_launch_params(kernel_mean_kernel_sm_75_0_launch_params, grid1, block1);
 
 void * kernel_mean_kernel_sm_75_0_kernel_params[]={&mean_gpu , &data_gpu , &M , &N};
 
 kernel_invoker(kernel_mean_kernel_0_name, kernel_mean_kernel_sm_75_0_launch_params, kernel_mean_kernel_sm_75_0_kernel_params);
 
 ////////////////////////////////////////////////////////////////////////
 ////////// WARNING: AUTOMATICALLY ANNOTATED REGION ENDS HERE ///////////
 ////////////////////////////////////////////////////////////////////////
	
  cudaCheckKernel ()
  ;
  cuda_timer_record_stop (t1);
  cudaThreadSynchronize ();
  cuda_timer_record_start (t2);
  	
 ////////////////////////////////////////////////////////////////////////
 ////////// WARNING: AUTOMATICALLY ANNOTATED REGION BEGINS HERE /////////
 ////////////////////////////////////////////////////////////////////////
	
 

 char kernel_reduce_kernel_0_name[] = "kernel_reduce_kernel_sm_75";
 
 //launch_params: 3 for grid_dim, 3 for block_dim, 1 for dynamic_shared_mem_bytes;
 int kernel_reduce_kernel_sm_75_0_launch_params[6];
 set_kernel_launch_params(kernel_reduce_kernel_sm_75_0_launch_params, grid2, block2);
 
 void * kernel_reduce_kernel_sm_75_0_kernel_params[]={&mean_gpu , &data_gpu , &M , &N};
 
 kernel_invoker(kernel_reduce_kernel_0_name, kernel_reduce_kernel_sm_75_0_launch_params, kernel_reduce_kernel_sm_75_0_kernel_params);
 
 ////////////////////////////////////////////////////////////////////////
 ////////// WARNING: AUTOMATICALLY ANNOTATED REGION ENDS HERE ///////////
 ////////////////////////////////////////////////////////////////////////
	
  cudaCheckKernel ()
  ;
  cuda_timer_record_stop (t2);
  cudaThreadSynchronize ();
  cuda_timer_record_start (t3);
  	
 ////////////////////////////////////////////////////////////////////////
 ////////// WARNING: AUTOMATICALLY ANNOTATED REGION BEGINS HERE /////////
 ////////////////////////////////////////////////////////////////////////
	
 

 char kernel_covar_kernel_0_name[] = "kernel_covar_kernel_sm_75";
 
 //launch_params: 3 for grid_dim, 3 for block_dim, 1 for dynamic_shared_mem_bytes;
 int kernel_covar_kernel_sm_75_0_launch_params[6];
 set_kernel_launch_params(kernel_covar_kernel_sm_75_0_launch_params, grid3, block3);
 
 void * kernel_covar_kernel_sm_75_0_kernel_params[]={&symmat_gpu , &data_gpu , &M , &N};
 
 kernel_invoker(kernel_covar_kernel_0_name, kernel_covar_kernel_sm_75_0_launch_params, kernel_covar_kernel_sm_75_0_kernel_params);
 
 ////////////////////////////////////////////////////////////////////////
 ////////// WARNING: AUTOMATICALLY ANNOTATED REGION ENDS HERE ///////////
 ////////////////////////////////////////////////////////////////////////
	
  cudaCheckKernel ()
  ;
  cuda_timer_record_stop (t3);
  cudaThreadSynchronize ();
  cudaMemcpy (symmat_outputFromGpu, symmat_gpu,       sizeof(DATA_TYPE) * (M + 1) * (N + 1), cudaMemcpyDeviceToHost);
  cuda_timer_record_get_elapsed_time(t1);
  cuda_timer_record_get_elapsed_time(t2);
  cuda_timer_record_get_elapsed_time(t3);
  printf ("[trace: n=%d, bx=%d, by=%d, "
   "elapsed_mean_kernel=%0.4f (ms),\n"
   " elapsed_reduce_kernel=%0.4f (ms), "
   " elapsed_covar_kernel=%0.4f (ms)] ... ",   M, DIM_THREAD_BLOCK_KERNEL_3_X, DIM_THREAD_BLOCK_KERNEL_3_Y,   t1.elapsed_time, t2.elapsed_time, t3.elapsed_time);
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
  DATA_TYPE* data;
  DATA_TYPE* symmat;
  DATA_TYPE* mean;
  DATA_TYPE* symmat_outputFromGpu;
  int n = 256, bx = 32, by = 8;
  if (argc > 1)
    n = atoi (argv[1]);
  if (argc > 2)
    bx = atoi (argv[2]);
  if (argc > 3)
    by = atoi (argv[3]);
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
#pragma START_TRACING
  covarianceCuda (data, symmat, mean, symmat_outputFromGpu);
#pragma STOP_TRACING
  covariance (data, symmat, mean);
  int s = compareResults (symmat, symmat_outputFromGpu);
  if (s == EXIT_SUCCESS)
    printf ("PASS\n");
  else
    printf ("FAIL\n");
  free (data);
  free (symmat);
  free (mean);
  free (symmat_outputFromGpu);
  return 0;
}
