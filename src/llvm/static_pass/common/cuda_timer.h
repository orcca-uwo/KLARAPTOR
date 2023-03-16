
/*
 * This header includes a set of functions for easier use
 * of CUDA timers.
 */

#ifndef CUDA_TIMER_H_
#define CUDA_TIMER_H_

//#include "new_types.h"
//
typedef unsigned long long int usfixn64;

typedef unsigned int usfixn32;

#include <stdio.h>
#include <string.h>

#define CUDA_TIMER_PRINT_WIDTH 32
/*************************************************/
typedef struct cuda_timer
{
  cudaEvent_t start, stop;
  float elapsed_time;
//  char name[64];
};

/*************************************************/
void
cuda_timer_init (cuda_timer& t, char *name = NULL)
{
  cudaEventCreate (&t.start);
  cudaEventCreate (&t.stop);
  t.elapsed_time = 0;

//  if(name!=NULL)
//    {
//      if(strlen(name)<64)
//	sprintf(t.name, name);
//      else
//	{
//	  printf("- cuda_timer name must be less than 64 chars!\n");
//
//	}
//    }
}

/*************************************************/
void
cuda_timer_record_start (cuda_timer&t)
{
  cudaEventRecord (t.start, 0);
}

/*************************************************/
void
cuda_timer_init_record_start (cuda_timer&t)
{
  cuda_timer_init (t);
  cuda_timer_record_start (t);
}

/*************************************************/
void
cuda_timer_record_stop (cuda_timer&t)
{
  cudaEventRecord (t.stop, 0);
}

/*************************************************/
void
cuda_timer_print_elapsed_time (float elapsed_time, char*msg)
{

  printf ("[%s: ", msg);
  //for(int i=strlen(msg);i<CUDA_TIMER_PRINT_WIDTH;i++)
    //printf(" ");
  printf ("%.3f (ms)]\n", elapsed_time);
}

/*************************************************/
void
cuda_timer_print_elapsed_time_for_bandwidth (usfixn64 data_size_bytes, float elapsed_time_ms,  char*msg)
{

  printf ("[%s]:", msg);
  for(int i=strlen(msg);i<CUDA_TIMER_PRINT_WIDTH;i++)
    printf(" ");
  double bw=((data_size_bytes*1.0)*1000);
  bw/=(elapsed_time_ms);
  bw/=(1.0*(1<<30));
  printf ("[% .6f (GB/s)] \n\n", bw);
}

/*************************************************/
void
cuda_timer_record_get_elapsed_time (cuda_timer&t, char*msg = NULL)
{
  cudaEventSynchronize (t.stop);
  cudaEventElapsedTime (&t.elapsed_time, t.start, t.stop);

  if (msg != NULL)
    {
//      printf ("[%s]:", msg);
//      printf ("[%.4f (ms)] \n", t.elapsed_time);
      cuda_timer_print_elapsed_time (t.elapsed_time, msg);
    }
}

/*************************************************/
void
cuda_timer_destroy (cuda_timer & t)
{
  cudaEventDestroy (t.start);
  cudaEventDestroy (t.stop);
}

/*************************************************/
#endif // end of CUDA_TIMER_H_
