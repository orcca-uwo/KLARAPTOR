
#include "fdtd2d_utils.h"

///////////////////////////////////////
/////// AUTOMATICALLY ANNOTATED ///////
///////////////////////////////////////
#include "kernel_invoker.h"
///////////////////////////////////////
///////////////////////////////////////
const int kernel_info_size_param_idx_fdtd_step1_kernel __attribute__((used))  = 5;
const int kernel_info_dim_fdtd_step1_kernel __attribute__((used))  = 2;
const int kernel_info_size_param_idx_fdtd_step2_kernel __attribute__((used))  = 4;
const int kernel_info_dim_fdtd_step2_kernel __attribute__((used))  = 2;
const int kernel_info_size_param_idx_fdtd_step3_kernel __attribute__((used))  = 4;
const int kernel_info_dim_fdtd_step3_kernel __attribute__((used))  = 2;
void
init_arrays (DATA_TYPE* _fict_, DATA_TYPE* ex, DATA_TYPE* ey, DATA_TYPE* hz)
{
  int i, j;
  for (i = 0; i < tmax; i++)
    {
      _fict_[i] = (DATA_TYPE) i;
    }
  for (i = 0; i < NX; i++)
    {
      for (j = 0; j < NY; j++)
 {
   ex[i * NY + j] = ((DATA_TYPE) i * (j + 1) + 1) / NX;
   ey[i * NY + j] = ((DATA_TYPE) (i - 1) * (j + 2) + 2) / NX;
   hz[i * NY + j] = ((DATA_TYPE) (i - 9) * (j + 4) + 3) / NX;
 }
    }
}
void
runFdtd (DATA_TYPE* _fict_, DATA_TYPE* ex, DATA_TYPE* ey, DATA_TYPE* hz)
{
  int t, i, j;
  for (t = 0; t < tmax; t++)
    {
      for (j = 0; j < NY; j++)
 {
   ey[0 * NY + j] = _fict_[t];
 }
      for (i = 1; i < NX; i++)
 {
   for (j = 0; j < NY; j++)
     {
       ey[i * NY + j] = ey[i * NY + j]
    - 0.5 * (hz[i * NY + j] - hz[(i - 1) * NY + j]);
     }
 }
      for (i = 0; i < NX; i++)
 {
   for (j = 1; j < NY; j++)
     {
       ex[i * (NY + 1) + j] = ex[i * (NY + 1) + j]
    - 0.5 * (hz[i * NY + j] - hz[i * NY + (j - 1)]);
     }
 }
      for (i = 0; i < NX; i++)
 {
   for (j = 0; j < NY; j++)
     {
       hz[i * NY + j] = hz[i * NY + j]
    - 0.7
        * (ex[i * (NY + 1) + (j + 1)] - ex[i * (NY + 1) + j]
     + ey[(i + 1) * NY + j] - ey[i * NY + j]);
     }
 }
    }
}
int
compareResults (DATA_TYPE* hz1, DATA_TYPE* hz2)
{
  int i, j, fail;
  fail = 0;
  for (i = 0; i < NX; i++)
    {
      for (j = 0; j < NY; j++)
 {
   if (percentDiff (hz1[i * NY + j],      hz2[i * NY + j]) > PERCENT_DIFF_ERROR_THRESHOLD)
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
}
//__global__ void
//fdtd_step1_kernel (DATA_TYPE* _fict_, DATA_TYPE *ex, DATA_TYPE *ey,     DATA_TYPE *hz, int t, int NX, int NY)
//{
//  int j = blockIdx.x * blockDim.x + threadIdx.x;
//  int i = blockIdx.y * blockDim.y + threadIdx.y;
//  if ((i < NX) && (j < NY))
//    {
//      if (i == 0)
// {
//   ey[i * NY + j] = _fict_[t];
// }
//      else
// {
//   ey[i * NY + j] = ey[i * NY + j]
//       - 0.5f * (hz[i * NY + j] - hz[(i - 1) * NY + j]);
// }
//    }
//}
//__global__ void
//fdtd_step2_kernel (DATA_TYPE *ex, DATA_TYPE *ey, DATA_TYPE *hz, int t, int NX,     int NY)
//{
//  int j = blockIdx.x * blockDim.x + threadIdx.x;
//  int i = blockIdx.y * blockDim.y + threadIdx.y;
//  if ((i < NX) && (j < NY) && (j > 0))
//    {
//      ex[i * (NY + 1) + j] = ex[i * (NY + 1) + j]
//   - 0.5f * (hz[i * NY + j] - hz[i * NY + (j - 1)]);
//    }
//}
//__global__ void
//fdtd_step3_kernel (DATA_TYPE *ex, DATA_TYPE *ey, DATA_TYPE *hz, int t, int NX,     int NY)
//{
//  int j = blockIdx.x * blockDim.x + threadIdx.x;
//  int i = blockIdx.y * blockDim.y + threadIdx.y;
//  if ((i < NX) && (j < NY))
//    {
//      hz[i * NY + j] = hz[i * NY + j]
//   - 0.7f
//       * (ex[i * (NY + 1) + (j + 1)] - ex[i * (NY + 1) + j]
//    + ey[(i + 1) * NY + j] - ey[i * NY + j]);
//    }
//}
void
fdtdCuda (DATA_TYPE* _fict_, DATA_TYPE* ex, DATA_TYPE* ey, DATA_TYPE* hz,   DATA_TYPE* hz_outputFromGpu)
{
  DATA_TYPE *_fict_gpu;
  DATA_TYPE *ex_gpu;
  DATA_TYPE *ey_gpu;
  DATA_TYPE *hz_gpu;
  cudaMalloc ((void **) &_fict_gpu, sizeof(DATA_TYPE) * tmax);
  cudaMalloc ((void **) &ex_gpu, sizeof(DATA_TYPE) * NX * (NY + 1));
  cudaMalloc ((void **) &ey_gpu, sizeof(DATA_TYPE) * (NX + 1) * NY);
  cudaMalloc ((void **) &hz_gpu, sizeof(DATA_TYPE) * NX * NY);
  cudaMemcpy (_fict_gpu, _fict_, sizeof(DATA_TYPE) * tmax,       cudaMemcpyHostToDevice);
  cudaMemcpy (ex_gpu, ex, sizeof(DATA_TYPE) * NX * (NY + 1),       cudaMemcpyHostToDevice);
  cudaMemcpy (ey_gpu, ey, sizeof(DATA_TYPE) * (NX + 1) * NY,       cudaMemcpyHostToDevice);
  cudaMemcpy (hz_gpu, hz, sizeof(DATA_TYPE) * NX * NY, cudaMemcpyHostToDevice);
  dim3 block (DIM_THREAD_BLOCK_X, DIM_THREAD_BLOCK_Y);
  dim3 grid ((size_t) ceil (((float) NY) / ((float) block.x)),      (size_t) ceil (((float) NX) / ((float) block.y)));
  cuda_timer * t1, *t2, *t3;
  t1 = (cuda_timer*) malloc (tmax * sizeof(cuda_timer));
  t2 = (cuda_timer*) malloc (tmax * sizeof(cuda_timer));
  t3 = (cuda_timer*) malloc (tmax * sizeof(cuda_timer));
  for (int t = 0; t < tmax; t++)
    {
      cuda_timer_init (t1[t]);
      cuda_timer_init (t2[t]);
      cuda_timer_init (t3[t]);
    }
  for (int t = 0; t < tmax; t++)
    {
      cuda_timer_record_start (t1[t]);
      	
 ////////////////////////////////////////////////////////////////////////
 ////////// WARNING: AUTOMATICALLY ANNOTATED REGION BEGINS HERE /////////
 ////////////////////////////////////////////////////////////////////////
	
 

 char kernel_fdtd_step1_kernel_0_name[] = "kernel_fdtd_step1_kernel_sm_75";
 
 //launch_params: 3 for grid_dim, 3 for block_dim, 1 for dynamic_shared_mem_bytes;
 int kernel_fdtd_step1_kernel_sm_75_0_launch_params[6];
 set_kernel_launch_params(kernel_fdtd_step1_kernel_sm_75_0_launch_params, grid, block);
 
 void * kernel_fdtd_step1_kernel_sm_75_0_kernel_params[]={&_fict_gpu , &ex_gpu , &ey_gpu , &hz_gpu , &t , &NX , &NY};
 
 kernel_invoker(kernel_fdtd_step1_kernel_0_name, kernel_fdtd_step1_kernel_sm_75_0_launch_params, kernel_fdtd_step1_kernel_sm_75_0_kernel_params);
 
 ////////////////////////////////////////////////////////////////////////
 ////////// WARNING: AUTOMATICALLY ANNOTATED REGION ENDS HERE ///////////
 ////////////////////////////////////////////////////////////////////////
	
      cudaCheckKernel()
      ;
      cuda_timer_record_stop (t1[t]);
      cudaThreadSynchronize ();
      cuda_timer_record_start (t2[t]);
      	
 ////////////////////////////////////////////////////////////////////////
 ////////// WARNING: AUTOMATICALLY ANNOTATED REGION BEGINS HERE /////////
 ////////////////////////////////////////////////////////////////////////
	
 

 char kernel_fdtd_step2_kernel_0_name[] = "kernel_fdtd_step2_kernel_sm_75";
 
 //launch_params: 3 for grid_dim, 3 for block_dim, 1 for dynamic_shared_mem_bytes;
 int kernel_fdtd_step2_kernel_sm_75_0_launch_params[6];
 set_kernel_launch_params(kernel_fdtd_step2_kernel_sm_75_0_launch_params, grid, block);
 
 void * kernel_fdtd_step2_kernel_sm_75_0_kernel_params[]={&ex_gpu , &ey_gpu , &hz_gpu , &t , &NX , &NY};
 
 kernel_invoker(kernel_fdtd_step2_kernel_0_name, kernel_fdtd_step2_kernel_sm_75_0_launch_params, kernel_fdtd_step2_kernel_sm_75_0_kernel_params);
 
 ////////////////////////////////////////////////////////////////////////
 ////////// WARNING: AUTOMATICALLY ANNOTATED REGION ENDS HERE ///////////
 ////////////////////////////////////////////////////////////////////////
	
      cudaCheckKernel()
      ;
      cuda_timer_record_stop (t2[t]);
      cudaThreadSynchronize ();
      cuda_timer_record_start (t3[t]);
      	
 ////////////////////////////////////////////////////////////////////////
 ////////// WARNING: AUTOMATICALLY ANNOTATED REGION BEGINS HERE /////////
 ////////////////////////////////////////////////////////////////////////
	
 

 char kernel_fdtd_step3_kernel_0_name[] = "kernel_fdtd_step3_kernel_sm_75";
 
 //launch_params: 3 for grid_dim, 3 for block_dim, 1 for dynamic_shared_mem_bytes;
 int kernel_fdtd_step3_kernel_sm_75_0_launch_params[6];
 set_kernel_launch_params(kernel_fdtd_step3_kernel_sm_75_0_launch_params, grid, block);
 
 void * kernel_fdtd_step3_kernel_sm_75_0_kernel_params[]={&ex_gpu , &ey_gpu , &hz_gpu , &t , &NX , &NY};
 
 kernel_invoker(kernel_fdtd_step3_kernel_0_name, kernel_fdtd_step3_kernel_sm_75_0_launch_params, kernel_fdtd_step3_kernel_sm_75_0_kernel_params);
 
 ////////////////////////////////////////////////////////////////////////
 ////////// WARNING: AUTOMATICALLY ANNOTATED REGION ENDS HERE ///////////
 ////////////////////////////////////////////////////////////////////////
	
      cudaCheckKernel()
      ;
      cuda_timer_record_stop (t3[t]);
      cudaThreadSynchronize ();
    }
  cudaMemcpy (hz_outputFromGpu, hz_gpu, sizeof(DATA_TYPE) * NX * NY,       cudaMemcpyDeviceToHost);
  float t1_total = 0, t2_total = 0, t3_total = 0;
  for (int t = 0; t < tmax; t++)
    {
      cuda_timer_record_get_elapsed_time (t1[t]);
      cuda_timer_record_get_elapsed_time (t2[t]);
      cuda_timer_record_get_elapsed_time (t3[t]);
      t1_total += t1[t].elapsed_time;
      t2_total += t2[t].elapsed_time;
      t3_total += t3[t].elapsed_time;
    }
  printf (
        "[trace: n=%d, bx=%d, by=%d, elapsed_fdtd_step1_kernel=%0.4f (ms),\n "
        "elapsed_fdtd_step2_kernel=%0.4f (ms), elapsed_fdtd_step3_kernel=%0.4f (ms)] ... ",        NX, DIM_THREAD_BLOCK_X, DIM_THREAD_BLOCK_Y, t1_total,        t2_total,        t3_total
        );
  cudaFree (_fict_gpu);
  cudaFree (ex_gpu);
  cudaFree (ey_gpu);
  cudaFree (hz_gpu);
  for (int t = 0; t < tmax; t++)
    {
      cuda_timer_destroy (t1[t]);
      cuda_timer_destroy (t2[t]);
      cuda_timer_destroy (t3[t]);
    }
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
  NX = NY = n;
  DIM_THREAD_BLOCK_X = bx;
  DIM_THREAD_BLOCK_Y = by;
  DATA_TYPE* _fict_;
  DATA_TYPE* ex;
  DATA_TYPE* ey;
  DATA_TYPE* hz;
  DATA_TYPE* hz_outputFromGpu;
  _fict_ = (DATA_TYPE*) malloc (tmax * sizeof(DATA_TYPE));
  ex = (DATA_TYPE*) malloc (NX * (NY + 1) * sizeof(DATA_TYPE));
  ey = (DATA_TYPE*) malloc ((NX + 1) * NY * sizeof(DATA_TYPE));
  hz = (DATA_TYPE*) malloc (NX * NY * sizeof(DATA_TYPE));
  hz_outputFromGpu = (DATA_TYPE*) malloc (NX * NY * sizeof(DATA_TYPE));
  init_arrays (_fict_, ex, ey, hz);
  GPU_argv_init ();
#pragma START_TRACING
  fdtdCuda (_fict_, ex, ey, hz, hz_outputFromGpu);
#pragma STOP_TRACING
  runFdtd (_fict_, ex, ey, hz);
  int s = compareResults (hz, hz_outputFromGpu);
  if (s == EXIT_SUCCESS)
    printf ("PASS\n");
  else
    printf ("FAIL\n");
  free (_fict_);
  free (ex);
  free (ey);
  free (hz);
  free (hz_outputFromGpu);
  return 0;
}
