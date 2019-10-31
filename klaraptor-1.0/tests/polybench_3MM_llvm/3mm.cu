///**
// * 3mm.cu: This file is part of the PolyBench/GPU 1.0 test suite.
// *
// *
// * Contact: Scott Grauer-Gray <sgrauerg@gmail.com>
// * Louis-Noel Pouchet <pouchet@cse.ohio-state.edu>
// * Web address: http://www.cse.ohio-state.edu/~pouchet/software/polybench/GPU
// */

#include "3mm_utils.h"

#pragma kernel_info_size_param_idx_mm3_kernel1 = 3;
#pragma kernel_info_dim_mm3_kernel1 = 2;

#pragma kernel_info_size_param_idx_mm3_kernel2 = 3;
#pragma kernel_info_dim_mm3_kernel2 = 2;

#pragma kernel_info_size_param_idx_mm3_kernel3 = 3;
#pragma kernel_info_dim_mm3_kernel3 = 2;

void
init_array (DATA_TYPE* A, DATA_TYPE* B, DATA_TYPE* C, DATA_TYPE* D)
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
	  B[i * NJ + j] = ((DATA_TYPE) i * (j + 1)) / NJ;
	}
    }

  for (i = 0; i < NJ; i++)
    {
      for (j = 0; j < NM; j++)
	{
	  C[i * NM + j] = ((DATA_TYPE) i * (j + 3)) / NL;
	}
    }

  for (i = 0; i < NM; i++)
    {
      for (j = 0; j < NL; j++)
	{
	  D[i * NL + j] = ((DATA_TYPE) i * (j + 2)) / NK;
	}
    }
}

int
compareResults (DATA_TYPE *G, DATA_TYPE *G_outputFromGpu)
{
  int i, j, fail;
  fail = 0;

  for (i = 0; i < NI; i++)
    {
      for (j = 0; j < NL; j++)
	{
	  if (percentDiff (
	      G[i * NL + j],
	      G_outputFromGpu[i * NL + j]) > PERCENT_DIFF_ERROR_THRESHOLD)
	    {
	      fail++;
	      return (EXIT_FAILURE);
	    }
	}
    }

  // print results
//  printf (
//      "Non-Matching CPU-GPU Outputs Beyond Error Threshold of %4.2f Percent: %d\n",
//      PERCENT_DIFF_ERROR_THRESHOLD, fail);
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
mm3_kernel1 (DATA_TYPE *A, DATA_TYPE *B, DATA_TYPE *E, int NI, int NJ, int NK)
{
  int j = blockIdx.x * blockDim.x + threadIdx.x;
  int i = blockIdx.y * blockDim.y + threadIdx.y;

  if ((i < NI) && (j < NJ))
    {
      int k;
      for (k = 0; k < NK; k++)
	{
	  E[i * NJ + j] += A[i * NK + k] * B[k * NJ + j];
	}
    }
}

__global__ void
mm3_kernel2 (DATA_TYPE *C, DATA_TYPE *D, DATA_TYPE *F, int NJ, int NL, int NM)
{
  int j = blockIdx.x * blockDim.x + threadIdx.x;
  int i = blockIdx.y * blockDim.y + threadIdx.y;

  if ((i < NJ) && (j < NL))
    {
      int k;
      for (k = 0; k < NM; k++)
	{
	  F[i * NL + j] += C[i * NM + k] * D[k * NL + j];
	}
    }
}

__global__ void
mm3_kernel3 (DATA_TYPE *E, DATA_TYPE *F, DATA_TYPE *G, int NI, int NJ, int NL)
{
  int j = blockIdx.x * blockDim.x + threadIdx.x;
  int i = blockIdx.y * blockDim.y + threadIdx.y;

  if ((i < NI) && (j < NL))
    {
      int k;
      for (k = 0; k < NJ; k++)
	{
	  G[i * NL + j] += E[i * NJ + k] * F[k * NL + j];
	}
    }
}

void
mm3_cpu (DATA_TYPE *A, DATA_TYPE *B, DATA_TYPE *C, DATA_TYPE *D, DATA_TYPE *E,
	 DATA_TYPE *F, DATA_TYPE *G)
{
  int i, j, k;

  /* E := A*B */
  for (i = 0; i < NI; i++)
    {
      for (j = 0; j < NJ; j++)
	{
	  E[i * NJ + j] = 0;
	  for (k = 0; k < NK; ++k)
	    {
	      E[i * NJ + j] += A[i * NK + k] * B[k * NJ + j];
	    }
	}
    }

  /* F := C*D */
  for (i = 0; i < NJ; i++)
    {
      for (j = 0; j < NL; j++)
	{
	  F[i * NL + j] = 0;
	  for (k = 0; k < NM; ++k)
	    {
	      F[i * NL + j] += C[i * NM + k] * D[k * NL + j];
	    }
	}
    }

  /* G := E*F */
  for (i = 0; i < NI; i++)
    {
      for (j = 0; j < NL; j++)
	{
	  G[i * NL + j] = 0;
	  for (k = 0; k < NJ; ++k)
	    {
	      G[i * NL + j] += E[i * NJ + k] * F[k * NL + j];
	    }
	}
    }
}

void
mm3Cuda (DATA_TYPE* A, DATA_TYPE* B, DATA_TYPE* C, DATA_TYPE* D, DATA_TYPE* E,
	 DATA_TYPE* F, DATA_TYPE* G, DATA_TYPE* G_outputFromGpu)
{
  double t_start, t_end;

  DATA_TYPE *A_gpu;
  DATA_TYPE *B_gpu;
  DATA_TYPE *C_gpu;
  DATA_TYPE *D_gpu;
  DATA_TYPE *E_gpu;
  DATA_TYPE *F_gpu;
  DATA_TYPE *G_gpu;

  cudaMalloc ((void **) &A_gpu, sizeof(DATA_TYPE) * NI * NK);
  cudaMalloc ((void **) &B_gpu, sizeof(DATA_TYPE) * NK * NJ);
  cudaMalloc ((void **) &C_gpu, sizeof(DATA_TYPE) * NJ * NM);
  cudaMalloc ((void **) &D_gpu, sizeof(DATA_TYPE) * NM * NL);
  cudaMalloc ((void **) &E_gpu, sizeof(DATA_TYPE) * NI * NJ);
  cudaMalloc ((void **) &F_gpu, sizeof(DATA_TYPE) * NJ * NL);
  cudaMalloc ((void **) &G_gpu, sizeof(DATA_TYPE) * NI * NL);

  cudaMemcpy (A_gpu, A, sizeof(DATA_TYPE) * NI * NK, cudaMemcpyHostToDevice);
  cudaMemcpy (B_gpu, B, sizeof(DATA_TYPE) * NK * NJ, cudaMemcpyHostToDevice);
  cudaMemcpy (C_gpu, C, sizeof(DATA_TYPE) * NJ * NM, cudaMemcpyHostToDevice);
  cudaMemcpy (D_gpu, D, sizeof(DATA_TYPE) * NM * NL, cudaMemcpyHostToDevice);
  cudaMemcpy (E_gpu, E, sizeof(DATA_TYPE) * NI * NJ, cudaMemcpyHostToDevice);
  cudaMemcpy (F_gpu, F, sizeof(DATA_TYPE) * NJ * NL, cudaMemcpyHostToDevice);
  cudaMemcpy (G_gpu, G, sizeof(DATA_TYPE) * NI * NL, cudaMemcpyHostToDevice);

  dim3 block (DIM_THREAD_BLOCK_X, DIM_THREAD_BLOCK_Y);
  dim3 grid1 ((size_t) (ceil (((float) NJ) / ((float) DIM_THREAD_BLOCK_X))),
	      (size_t) (ceil ((float) NI / ((float) DIM_THREAD_BLOCK_Y))));
  dim3 grid2 ((size_t) (ceil (((float) NL) / ((float) DIM_THREAD_BLOCK_X))),
	      (size_t) (ceil ((float) NJ / ((float) DIM_THREAD_BLOCK_Y))));
  dim3 grid3 ((size_t) (ceil (((float) NL) / ((float) DIM_THREAD_BLOCK_X))),
	      (size_t) (ceil ((float) NI / ((float) DIM_THREAD_BLOCK_Y))));

  cuda_timer t1, t2, t3;

  cuda_timer_init (t1);
  cuda_timer_init (t2);
  cuda_timer_init (t3);

//  t_start = rtclock ();
  cuda_timer_record_start (t1);
  mm3_kernel1 <<<grid1, block>>> (A_gpu, B_gpu, E_gpu, NI, NJ, NK);
  cudaCheckKernel()
  ;
  cuda_timer_record_stop (t1);
  cudaThreadSynchronize ();

  cuda_timer_record_start (t2);
  mm3_kernel2 <<<grid2, block>>> (C_gpu, D_gpu, F_gpu, NJ, NL, NM);
  cudaCheckKernel()
  ;
  cuda_timer_record_stop (t2);
  cudaThreadSynchronize ();

  cuda_timer_record_start (t3);
  mm3_kernel3 <<<grid3, block>>> (E_gpu, F_gpu, G_gpu, NI, NJ, NL);
  cudaCheckKernel()
  ;
  cuda_timer_record_stop (t3);
  cudaThreadSynchronize ();
//  t_end = rtclock ();
  cudaMemcpy (G_outputFromGpu, G_gpu, sizeof(DATA_TYPE) * NI * NL,
	      cudaMemcpyDeviceToHost);

//  fprintf (stdout, "GPU Runtime: %0.6lfs\n", t_end - t_start);

 cuda_timer_record_get_elapsed_time(t1);
 cuda_timer_record_get_elapsed_time(t2);
 cuda_timer_record_get_elapsed_time(t3);

  printf (
        "[trace: n=%d, bx=%d, by=%d, "
        "elapsed_mm3_kernel1=%0.4f (ms), "
        "elapsed_mm3_kernel2=%0.4f (ms), "
        "elapsed_mm3_kernel3=%0.4f (ms)] ... ",
        NI, DIM_THREAD_BLOCK_X, DIM_THREAD_BLOCK_Y,
        t1.elapsed_time,
        t2.elapsed_time,
        t3.elapsed_time);

  cudaFree (A_gpu);
  cudaFree (B_gpu);
  cudaFree (C_gpu);
  cudaFree (D_gpu);
  cudaFree (E_gpu);
  cudaFree (F_gpu);
  cudaFree (G_gpu);
}

int
main (int argc, char** argv)
{
  int n = 256, bx = 32, by = 8, n_repeat=1, verify=0;

  bx=DIM_THREAD_BLOCK_X;
  by=DIM_THREAD_BLOCK_Y;

  if (argc > 1)
    n = atoi (argv[1]);
  if (argc > 2)
    bx = atoi (argv[2]);
  if (argc > 3)
    by = atoi (argv[3]);
  if (argc>4)
  	n_repeat=atoi(argv[4]);
	if (argc > 5)
    verify = atoi (argv[5]);

  NI = NJ = NK = NL = NM = n;
  DIM_THREAD_BLOCK_X = bx;
  DIM_THREAD_BLOCK_Y = by;

//	double t_start, t_end;

  DATA_TYPE* A;
  DATA_TYPE* B;
  DATA_TYPE* C;
  DATA_TYPE* D;
  DATA_TYPE* E;
  DATA_TYPE* F;
  DATA_TYPE* G;
  DATA_TYPE* G_outputFromGpu;

  A = (DATA_TYPE*) malloc (NI * NK * sizeof(DATA_TYPE));
  B = (DATA_TYPE*) malloc (NK * NJ * sizeof(DATA_TYPE));
  C = (DATA_TYPE*) malloc (NJ * NM * sizeof(DATA_TYPE));
  D = (DATA_TYPE*) malloc (NM * NL * sizeof(DATA_TYPE));
  E = (DATA_TYPE*) malloc (NI * NJ * sizeof(DATA_TYPE));
  F = (DATA_TYPE*) malloc (NJ * NL * sizeof(DATA_TYPE));
  G = (DATA_TYPE*) malloc (NI * NL * sizeof(DATA_TYPE));
  G_outputFromGpu = (DATA_TYPE*) malloc (NI * NL * sizeof(DATA_TYPE));

  init_array (A, B, C, D);

  GPU_argv_init ();

	for(int i=0;i<n_repeat;i++)
  	mm3Cuda (A, B, C, D, E, F, G, G_outputFromGpu);

	if(verify)
	{
//  t_start = rtclock ();
	for(int i=0;i<n_repeat;i++)
	  mm3_cpu (A, B, C, D, E, F, G);

//  t_end = rtclock ();

//  fprintf (stdout, "CPU Runtime: %0.6lfs\n", t_end - t_start);

  int s = compareResults (G, G_outputFromGpu);
  if (s == EXIT_SUCCESS)
    printf ("PASS\n");
  else
    printf ("FAIL\n");
	}
  free (A);
  free (B);
  free (C);
  free (D);
  free (E);
  free (F);
  free (G);
  free (G_outputFromGpu);

  return 0;
}

