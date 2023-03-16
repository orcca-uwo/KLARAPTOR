
#include "2mm_utils.h"

///////////////////////////////////////
/////// AUTOMATICALLY ANNOTATED ///////
///////////////////////////////////////
#include "kernel_invoker.h"
///////////////////////////////////////
///////////////////////////////////////
const int kernel_info_size_param_idx_mm2_kernel1 __attribute__((used))  = 3;
const int kernel_info_size_param_idx_mm2_kernel2 __attribute__((used))  = 3;
const int kernel_info_dim_mm2_kernel1 __attribute__((used))  = 2;
const int kernel_info_dim_mm2_kernel2 __attribute__((used))  = 2;
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
       E[i * NI + j],       E_outputFromGpu[i * NI + j]) > PERCENT_DIFF_ERROR_THRESHOLD)
     {
       fail++;
       return (EXIT_FAILURE);
     }
 }
    }
  return EXIT_SUCCESS;
  
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
//mm2_kernel1 (DATA_TYPE *A, DATA_TYPE *B, DATA_TYPE *C, int NI, int NJ, int NK,      int NL)
//{
//  int j = blockIdx.x * blockDim.x + threadIdx.x;
//  int i = blockIdx.y * blockDim.y + threadIdx.y;
//  if ((i < NI) && (j < NJ))
//    {
//      int k;
//      for (k = 0; k < NK; k++)
// {
//   C[i * NJ + j] += A[i * NK + k] * B[k * NJ + j];
// }
//    }
//}
//__global__ void
//mm2_kernel2 (DATA_TYPE *C, DATA_TYPE *D, DATA_TYPE *E, int NI, int NJ, int NK,      int NL)
//{
//  int j = blockIdx.x * blockDim.x + threadIdx.x;
//  int i = blockIdx.y * blockDim.y + threadIdx.y;
//  if ((i < NI) && (j < NL))
//    {
//      int k;
//      for (k = 0; k < NJ; k++)
// {
//   E[i * NL + j] += C[i * NJ + k] * D[k * NL + j];
// }
//    }
//}
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
mm2Cuda (DATA_TYPE* A, DATA_TYPE* B, DATA_TYPE* C, DATA_TYPE* D, DATA_TYPE* E,  DATA_TYPE* E_outputFromGpu)
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
  dim3 grid1 ((size_t) ceil (((float) NJ) / ((float) block.x)),       (size_t) ceil (((float) NI) / ((float) block.y)));
  dim3 grid2 ((size_t) ceil (((float) NL) / ((float) block.x)),       (size_t) ceil (((float) NI) / ((float) block.y)));
  cuda_timer t_mm2_k1, t_mm2_k2;
  cuda_timer_init (t_mm2_k1);
  cuda_timer_init (t_mm2_k2);
  cuda_timer_record_start (t_mm2_k1);
  	
 ////////////////////////////////////////////////////////////////////////
 ////////// WARNING: AUTOMATICALLY ANNOTATED REGION BEGINS HERE /////////
 ////////////////////////////////////////////////////////////////////////
	
 

 char kernel_mm2_kernel1_0_name[] = "kernel_mm2_kernel1_sm_75";
 
 //launch_params: 3 for grid_dim, 3 for block_dim, 1 for dynamic_shared_mem_bytes;
 int kernel_mm2_kernel1_sm_75_0_launch_params[6];
 set_kernel_launch_params(kernel_mm2_kernel1_sm_75_0_launch_params, grid1, block);
 
 void * kernel_mm2_kernel1_sm_75_0_kernel_params[]={&A_gpu , &B_gpu , &C_gpu , &NI , &NJ , &NK , &NL};
 
 kernel_invoker(kernel_mm2_kernel1_0_name, kernel_mm2_kernel1_sm_75_0_launch_params, kernel_mm2_kernel1_sm_75_0_kernel_params);
 
 ////////////////////////////////////////////////////////////////////////
 ////////// WARNING: AUTOMATICALLY ANNOTATED REGION ENDS HERE ///////////
 ////////////////////////////////////////////////////////////////////////
	
  cudaCheckKernel()
  ;
  cuda_timer_record_stop (t_mm2_k1);
  cudaThreadSynchronize ();
  cuda_timer_record_start (t_mm2_k2);
  	
 ////////////////////////////////////////////////////////////////////////
 ////////// WARNING: AUTOMATICALLY ANNOTATED REGION BEGINS HERE /////////
 ////////////////////////////////////////////////////////////////////////
	
 

 char kernel_mm2_kernel2_0_name[] = "kernel_mm2_kernel2_sm_75";
 
 //launch_params: 3 for grid_dim, 3 for block_dim, 1 for dynamic_shared_mem_bytes;
 int kernel_mm2_kernel2_sm_75_0_launch_params[6];
 set_kernel_launch_params(kernel_mm2_kernel2_sm_75_0_launch_params, grid2, block);
 
 void * kernel_mm2_kernel2_sm_75_0_kernel_params[]={&C_gpu , &D_gpu , &E_gpu , &NI , &NJ , &NK , &NL};
 
 kernel_invoker(kernel_mm2_kernel2_0_name, kernel_mm2_kernel2_sm_75_0_launch_params, kernel_mm2_kernel2_sm_75_0_kernel_params);
 
 ////////////////////////////////////////////////////////////////////////
 ////////// WARNING: AUTOMATICALLY ANNOTATED REGION ENDS HERE ///////////
 ////////////////////////////////////////////////////////////////////////
	
  cudaCheckKernel ()
  ;
  cuda_timer_record_stop (t_mm2_k2);
  cudaThreadSynchronize ();
  cuda_timer_record_get_elapsed_time (t_mm2_k1);
  cuda_timer_record_get_elapsed_time (t_mm2_k2);
  cudaMemcpy (E_outputFromGpu, E_gpu, sizeof(DATA_TYPE) * NI * NL,       cudaMemcpyDeviceToHost);
  printf (
      "[trace: n=%d, bx=%d, by=%d, elapsed_mm2_kernel1=%0.4f (ms), elapsed_mm2_kernel2=%0.4f (ms)] ... ",      NI, DIM_THREAD_BLOCK_X, DIM_THREAD_BLOCK_Y, t_mm2_k1.elapsed_time,      t_mm2_k2.elapsed_time);
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
  mm2_cpu (A, B, C, D, E);
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
