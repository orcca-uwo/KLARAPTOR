#ifndef MCWP_H_
#define MCWP_H_

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
//#include <signal.h>

#include "../NumericalPolySupport/include/interpolator.h"
//#include "../NumericalPolySupport/include/linAlgIML.h"
#include "../kernel_mesh_points/kernel_mesh_points.h"

///////////////////////////////////////
//double eps =1000 * 1000 * 1000 * 1000.0;
// double eps = 1000.0 * 1000.0 * 1000 * 1000 * 1000.0 * 1000.0 * 1000.0;
//double eps = 0.00001;
double eps = 1;
//double eps = 0.00001;

//1000 * 1000 * 1000 * 1000.0;
//double eps=(1<<40);

//double eps = 0;

//typedef unsigned long long int u64;

const char * long_dashed_line = ""
    "-------------------------------------"
    "--------------------------------------"
    "\n";
const char * short_dashed_line = "--------------------------------------\n";
///////////////////////////////////////
#define LEN_KERNEL_NAME 64

///////////////////////////////////////
#ifndef VERBOSE 
#define VERBOSE 0
#endif 

///////////////////////////////////////
// disabled REP has some significantly negative
// effects on the calculation. IT IS NOT SAFE!

//#if ALLVARIABLE
//
//#ifndef REP_ENABLED
//#define REP_ENABLED 1
//#endif
//#ifndef REP_WITHOUT_N
//#define REP_WITHOUT_N 0
//#endif
//
//#else
//
//#ifndef REP_ENABLED
//#define REP_ENABLED 0
//#endif
//#ifndef REP_WITHOUT_N
//#define REP_WITHOUT_N 1
//#endif
//
//#endif

///////////////////////////////////////
typedef struct
{
  char *name;
  int idx;
} item_to_idx_dict;

///////////////////////////////////////
enum kernel_type_enum
{
  OneDimKernel, TwoDimKernel, ThreeDimProblem
};

///////////////////////////////////////
enum poly_params_status
{
  INIT, UNCHECKED, DONE, SUCCESSFUL_INTERP, FAILED_INTERP
};

///////////////////////////////////////

typedef struct mcwp_result_params_t
{
  int shared_mem_bytes_total;
  int mcwp_case;
  float occupancy;
  int n, b0, b1;

  mpq_t Comp_insts;
  mpq_t Uncoal_Mem_insts;
  mpq_t Coal_Mem_insts;
  mpq_t Synch_insts;
  mpq_t Coal_per_mw;
  mpq_t Uncoal_per_mw;
  mpq_t Active_blocks_per_SM;

  mpq_t Threads_per_block;
  mpq_t Blocks;
  mpq_t Active_warps_per_block;
  mpq_t Active_warps_per_SM;
  mpq_t CWP;
  mpq_t MWP;
  mpq_t MWP_peak_BW;
  mpq_t MWP_Without_BW_full;
  mpq_t CWP_full;
  mpq_t Mem_LD;

  mpq_t Rep;
  mpq_t Rep_without_B;
  mpq_t Exec_cycles_app;
  mpq_t Exec_cycles_per_thread;
  int is_best_for_same_n;

} mcwp_result_params_t;
///////////////////////////////////////

typedef struct
{
  int nvar;
  int* degree_bounds;
  int* denomdegree_bounds;
  Interpolator_t* interp[3];
  triple* point;
  AltArr_t* poly[3];
  AltArr_t* denompoly[3];
  enum poly_params_status status[3];
  int mesh_size;
//  int is_zero_poly[3];
  int mcwp_result_table_idx;
  mcwp_result_params_t * mcwp_result_table;
  char * kernel_name;
  char ** symbols;
} poly_params;

///////////////////////////////////////
int
ceiling_div (int x, int y)
{
  return ((x + y - 1) / y);
}

///////////////////////////////////////
float
read_float_from_file (char *path)
{

  FILE *file = fopen (path, "r");
  if (file == NULL)
    {
      printf ("ERROR: opening file=%s ... \n", path);
      exit (EXIT_FAILURE);
    }
  float value = 0;
  fscanf (file, "%f", &value);
  fclose (file);
  return value;
}

///////////////////////////////////////

//this function is supposed to get a command
//which prints 0 in the case of success and non-zero
//in any other case.
//
//
////////////////////////////////////////
//this function is supposed to get a command
//which prints 0 in the case of success and non-zero
//in any other case.

//ToDo: get cmd_str as input; for the moment it is 
// fixed and set to "./mem_inst_trace.bin".
///////////////////////////////////////

///////////////////////////////////////
int
read_mem_ld (int n, enum kernel_type_enum kernel_type, int block_size,
	     char *device_name)
{

  if (kernel_type == TwoDimKernel)
    {
      n = n * n;
//      printf ("two dim kernel, n=%d\n", n);
    }

  char cmd[64];
  sprintf (cmd, "cd ..; python get_mem_ld.py %d %d %s", n, block_size,
	   device_name);
  FILE* pipe;
  pipe = popen (cmd, "r");
  if (pipe == NULL)
    {
      printf ("ERROR: FAILED TO RUN [%s]\n", cmd);
      return (EXIT_FAILURE);
    }
  char buffer[32];
  int value = 0;
  if (fgets (buffer, sizeof(buffer) - 1, pipe) != NULL)
    {
      value = atoi (buffer);
    }
  else
    {
//		printf("ERROR: NOTHING READ!\n");
//      value = -1;
      exit (EXIT_FAILURE);
    }

  pclose (pipe);

#if VERBOSE>=2
  printf ("[Dynamic_Mem_LD=%d]\n", value);
#endif
  return value;
}

////////////////////////////////////////////
int
remove_trace_log_file ()
{
  return remove ("tracer_params.log");
}

///////////////////////////////////////
int
copy_file (char * src, char *dest)
{
  char cmd[1024];
  sprintf (cmd, "cp %s %s", src, dest);
  return system (cmd);
}

///////////////////////////////////////
char *
get_absolute_device_name (const char * relative_device_name)
{
  int n = strlen (relative_device_name);

  char * src, *dest;
  src = (char*) malloc (n);

  memcpy (src, relative_device_name, n);
  int len = 0;
  int last_slash_idx = 0;
  int last_dot_idx = 0;
  for (int i = 0; i < n; i++)
    {
      if (src[i] == '/')
	last_slash_idx = i;
      if (src[i] == '.')
	last_dot_idx = i;
    }

  last_slash_idx++;
  dest = (char*) malloc (last_dot_idx - last_slash_idx + 1);
  for (int i = last_slash_idx; i < last_dot_idx; i++)
    {
      dest[i - last_slash_idx] = src[i];
    }
  dest[last_dot_idx - last_slash_idx] = '\0';

  return dest;
}
///////////////////////////////////////
// check the constraint file for a kernel
// return 0 on success.
int
eval_constraints (char * kernel_name, int n, int b0, int b1)
{
  ////////////////////////////////////
  const char * eval_const_script = "../../src/mcwp/eval_const.py";
//  copy_file (eval_const_script, ".");
  ////////////////////////////////////
  char cmd[1024];
  sprintf (cmd, "python %s %s %d %d %d", eval_const_script, kernel_name, n, b0,
	   b1);

  FILE* pipe;
  pipe = popen (cmd, "r");
  if (pipe == NULL)
    {
      printf ("ERROR: FAILED TO RUN [%s]\n", cmd);
      return (EXIT_FAILURE);
    }
  char buffer[32];
  int value = 0;
  if (fgets (buffer, sizeof(buffer) - 1, pipe) != NULL)
    {
      value = atoi (buffer);
    }
  else
    {
      //		printf("ERROR: NOTHING READ!\n");
      value = -1;
    }

  pclose (pipe);
  return value;
}

///////////////////////////////////////
// ISSUES:
// - if the interpolation.bin get interrupted for any reason,
// then, the nvprof (cudabin) will keep working on the gpu.
// this has unintended consequence and will severely damage the
// accuracy of upcoming measurements on the same card.
// TODo: how to propagate sigint to this specific subprocess.

//void handle_sigint(int sig)
//{
//    exit(-1);
//}

int
exec_cmd (char * cmd_str_params)
{
//  signal(SIGINT, handle_sigint);

  char cubin_name[128];
  char cmd_str[512];
  FILE* cubin_name_file = fopen ("cudabin_name", "r");
  fscanf (cubin_name_file, "%s", cubin_name);
  fclose (cubin_name_file);

  sprintf (cmd_str, "../../src/trace_analysis/profiler.sh \"%s", cubin_name);

  FILE * params_file = fopen ("tracer_params.log", "a+");
  fprintf (params_file, "%s\n", cmd_str_params);

  fclose (params_file);

//	
//	const char split_tag=" ";
//	const char replaced_split_tag=",";
//	for (int i=0;i<strlen(cmd_str);i++)
//	{
//		
//	}

  const char suffix[64] = ">/dev/null 2>&1 && exit 0 || exit 1";
  char * cmd = (char*) malloc (128 + strlen (cmd_str_params));
  sprintf (cmd, "%s %s\" %s", cmd_str, cmd_str_params, suffix);

#if VERBOSE>=3
  printf("executing [%s ]... \n", cmd);
  printf("TEST\n");
#endif

  freopen ("/dev/null", "w", stdout);
  FILE* pipe;
  pipe = popen (cmd, "r");
  if (pipe == NULL)
    {
      printf ("ERROR: FAILED TO RUN [%s]\n", cmd);
      return (EXIT_FAILURE);
    }
  char buffer[32];
  int value = 0;
  if (fgets (buffer, sizeof(buffer) - 1, pipe) != NULL)
    {
      value = atoi (buffer);
    }
  else
    {
//		printf("ERROR: NOTHING READ!\n");
      value = -1;
    }

  pclose (pipe);


  freopen ("/dev/tty", "w", stdout);



  system ("python ../../src/trace_analysis/parse_profile_info.py sm_arch.config");


  return value;
}
///////////////////////////////////////
void
mpq_ceil (mpq_t x)
{
  mpz_t numer;
  mpz_t denom;
  mpz_init (numer);
  mpz_init (denom);
  mpz_set (numer, mpq_numref (x));
  mpz_set (denom, mpq_denref (x));
  mpz_cdiv_q (numer, numer, denom);
  mpq_set_z (x, numer);
  mpz_clear (numer);
  mpz_clear (denom);
}

///////////////////////////////////////

#define N_DEVICE_PARAMS 8
const item_to_idx_dict global_device_params_list[] =
  {
    { "Issue_cycles", 0 },
    { "Mem_bandwidth", 1 },
    { "Mem_LD", 2 },
    { "Departure_del_uncoal", 3 },
    { "Departure_del_coal", 4 },
    { "Active_SMs", 5 },
    { "Freq", 6 },
    { "Load_bytes_per_warp", 7 } };

///////////////////////////////////////
typedef struct device_params_t
{
  int Issue_cycles;
  int Mem_bandwidth;
  int Mem_LD;
  int Departure_del_uncoal;
  int Departure_del_coal;
  int Active_SMs;
  int Freq;
  int Load_bytes_per_warp;
} device_params_t;

///////////////////////////////////////
#define N_TRACE_PARAMS 11 
const item_to_idx_dict global_trace_params_list[] =
  {
    { "13.Total_insts", 0 },
    { "14.Comp_insts", 1 },
    { "15.Mem_insts", 2 },
    { "16.Uncoal_Mem_insts", 3 },
    { "17.Coal_Mem_insts", 4 },
    { "18.Synch_insts", 5 },
    { "19.Coal_per_mw", 6 },
    { "20.Uncoal_per_mw", 7 },
    { "n_active_blocks_per_sm", 8 },
    { "n_blocks", 9 },
    { "n_warps", 10 } };

///////////////////////////////////////
typedef struct trace_params_t
{
  int Total_insts;
  int Comp_insts;
  int Mem_insts;
  int Uncoal_Mem_insts;
  int Coal_Mem_insts;
  int Synch_insts;
  int Coal_per_mw;
  int Uncoal_per_mw;
  int n_active_blocks_per_sm;
  int n_blocks;
  int n_warps;
} trace_params_t;

///////////////////////////////////////
#define N_KERNEL_PARAMS 9
const item_to_idx_dict global_kernel_params_list[] =
  {
    { "grid_dim_x", 0 },
    { "grid_dim_y", 1 },
    { "grid_dim_z", 2 },
    { "block_dim_x", 3 },
    { "block_dim_y", 4 },
    { "block_dim_z", 5 },
    { "shared_mem_bytes_dynamic", 6 },
    { "active_blocks_per_sm", 7 },
    { "occupancy_percentage", 8 } };

///////////////////////////////////////
typedef struct kernel_params_t
{
  int grid_dim_x; // [0]
  int grid_dim_y; // [1]
  int grid_dim_z; // [2]
  int block_dim_x; // [3]
  int block_dim_y; // [4]
  int block_dim_z; // [5]
  int shared_mem_bytes_dynamic; // [6]
  int active_blocks_per_sm; // [7]
  int occupancy_percentage; // [8]
  int threads_per_block;
  int blocks_per_grid;
} kernel_params_t;
///////////////////////////////////////
#define N_PTX_PARAMS 2
const item_to_idx_dict global_ptx_params_list[] =
  {
    { "shared_mem_bytes_static", 0 },
    { "registers", 1 } };

///////////////////////////////////////
typedef struct ptx_params_t
{
  int shared_mem_bytes_static;
  int registers;
} ptx_params_t;

/////////////////////////////////////////
//#define N_RATIONAL_PARAMS 18
//const item_to_idx_dict global_rational_params_list[] =
//  {
//      {"shared_mem_bytes_total", 0},
//      {"occupancy", 1},
//      {"Comp_insts", 2},
//      {"Uncoal_Mem_insts", 3},
//      {"Coal_Mem_insts", 4},
//      {"Synch_insts", 5},
//      {"Coal_per_mw", 6},
//      {"Uncoal_per_mw", 7},
//      {"Active_blocks_per_SM", 8},
//      {"Threads_per_block", 9},
//      {"Blocks", 10},
//      {"Active_warps_per_block", 11},
//      {"Active_warps_per_SM", 12},
//      {"MWP", 13},
//      {"CWP", 14},
//      {"Rep", 15},
//      {"Rep_without_B", 16},
//      {"Exec_cycles_app", 17} 
//  };
///////////////////////////////////////
void
init_mcwp_result_params (mcwp_result_params_t * x)
{
  x->n = 0;
  x->b0 = 0;
  x->b1 = 0;

  x->shared_mem_bytes_total = 0;
  x->mcwp_case = 0;
  x->occupancy = 0;

  mpq_init (x->Comp_insts);
  mpq_init (x->Uncoal_Mem_insts);
  mpq_init (x->Coal_Mem_insts);
  mpq_init (x->Synch_insts);
  mpq_init (x->Coal_per_mw);
  mpq_init (x->Uncoal_per_mw);
  mpq_init (x->Active_blocks_per_SM);

  mpq_init (x->Threads_per_block);
  mpq_init (x->Blocks);
  mpq_init (x->Active_warps_per_block);
  mpq_init (x->Active_warps_per_SM);
  mpq_init (x->MWP);
  mpq_init (x->CWP);
  mpq_init (x->MWP_peak_BW);
  mpq_init (x->MWP_Without_BW_full);
  mpq_init (x->CWP_full);

  mpq_init (x->Mem_LD);

  mpq_init (x->Rep);
  mpq_init (x->Rep_without_B);
  mpq_init (x->Exec_cycles_app);
  mpq_init (x->Exec_cycles_per_thread);

  x->is_best_for_same_n = 0;
}
///////////////////////////////////////
void
print_mcwp_result_params (mcwp_result_params_t * x, FILE * output_file)
{
  gmp_fprintf (
      output_file, "["
      " N:%d, b0:%3d, b1:%3d, MWP:%2.0f, CWP:%2.0f, mcwp_case:%d, \n"
      " Exec_cycles_app:%.4f, Exec_cycles_per_thread:%.4f,\n"
      " occupancy:%.2f, Rep:%.2f, Active_blocks_per_SM:%.0f,\n"
      " shared_mem_bytes_total:%d, Blocks:%.0f, Threads_per_block:%.0f,\n"
      " Coal_per_mw:%.0f, Uncoal_per_mw:%.0f, Comp_insts:%.0f,\n"
      " Uncoal_Mem_insts:%.0f, Coal_Mem_insts:%.0f, Synch_insts:%.0f \n"
      "]",
      x->n, x->b0, x->b1, mpq_get_d (x->MWP), mpq_get_d (x->CWP), x->mcwp_case,
      mpq_get_d (x->Exec_cycles_app), mpq_get_d (x->Exec_cycles_per_thread),
      x->occupancy, mpq_get_d (x->Rep), mpq_get_d (x->Active_blocks_per_SM),
      x->shared_mem_bytes_total, mpq_get_d (x->Blocks),
      mpq_get_d (x->Threads_per_block), mpq_get_d (x->Coal_per_mw),
      mpq_get_d (x->Uncoal_per_mw), mpq_get_d (x->Comp_insts),
      mpq_get_d (x->Uncoal_Mem_insts), mpq_get_d (x->Coal_Mem_insts),
      mpq_get_d (x->Synch_insts));

  fprintf (
      output_file,
      "\n---------------------------------------------------------------------------\n");

//  mpq_init (x->Threads_per_block);
//  mpq_init (x->Blocks);
//  mpq_init (x->Active_warps_per_block);
//  mpq_init (x->Active_warps_per_SM);
//  mpq_init (x->Rep);
//  mpq_init (x->Rep_without_B);
//  mpq_init (x->Exec_cycles_app);
}

///////////////////////////////////////
#define PARAM_S32 0x1
#define PARAM_S64 0x2
#define PARAM_U32 0x3
#define PARAM_U64 0x4
#define PARAM_FLOAT 0x5
#define PARAM_DOUBLE 0x6
#define PARAM_STR 0x7
#define PARAM_MPZ 0x8
#define PARAM_MPQ 0x9
///////////////////////////////////////
int
read_value_from_file (FILE * file, char * name, char* delim, int param_type,
		      void *value)
{

//  printf ("Reading [%s]...\n", name);
  char line[1024];
  if (fgets (line, 1024, file) == NULL)
    {
      printf ("WARNING: Reached EOF while reading value of [%s] from file!\n",
	      name);
      return (1);
    }

  if ((strcmp (line, "\n") == 0) || (strcmp (line, "") == 0))
    fgets (line, 1024, file);

//  printf("line=%s \n", line);
  int n_s32;
  long int n_s64;
  unsigned int n_u32;
  unsigned long int n_u64;

  float n_float;
  double n_double;
  char n_str[256];
  mpz_t n_mpz;
  mpz_t n_mpq_num;
  mpz_t n_mpq_denom;
  mpq_t n_mpq;
  double n_mpq_double;

  char format_str[256];

  switch (param_type)
    {
    case PARAM_S32:
      sprintf (format_str, "[%s%s%%d]", name, delim);
      sscanf (line, format_str, &n_s32);
      memcpy (value, &n_s32, sizeof(int));
      break;

    case PARAM_S64:
      sprintf (format_str, "[%s%s%%ld]", name, delim);
      sscanf (line, format_str, &n_s64);
      memcpy (value, &n_s64, sizeof(long int));
      break;

    case PARAM_U32:
      sprintf (format_str, "[%s%s%%u]", name, delim);
      sscanf (line, format_str, &n_u32);
      memcpy (value, &n_u32, sizeof(unsigned int));
      break;

    case PARAM_U64:
      sprintf (format_str, "[%s%s%%lu]", name, delim);
      sscanf (line, format_str, &n_u64);
      memcpy (value, &n_u64, sizeof(unsigned long int));
      break;

    case PARAM_FLOAT:

      sprintf (format_str, "[%s%s%%f]", name, delim);
      sscanf (line, format_str, &n_float);
      memcpy (value, &n_float, sizeof(float));
      break;

    case PARAM_DOUBLE:
      sprintf (format_str, "[%s%s%%lf]", name, delim);
      sscanf (line, format_str, &n_double);
      memcpy (value, &n_double, sizeof(double));
      break;

    case PARAM_STR:
      // ToDo: define the limit on "n_str" size
      sprintf (format_str, "[%s%s%%s]", name, delim);
      sscanf (line, format_str, n_str);
//      mempcy (value, &n_str, sizeof(n_str));
      break;

    case PARAM_MPZ:

      sprintf (format_str, "[%s%s%%d]", name, delim);
      mpz_init (n_mpz);
      gmp_sscanf (line, format_str, n_mpz);
      mpz_set (*(mpz_t*) value, n_mpz);
      mpz_clear (n_mpz);
      break;

    case PARAM_MPQ:

//      mpz_inits (n_mpq_num, n_mpq_denom, NULL);
////      gmp_sscanf (line, "%s%s%d/%d", name, delim, n_mpq_num, n_mpq_denom);
//      gmp_sscanf (line, "%s%s%d/%d", name, delim, n_mpq_num, n_mpq_denom);
//      mpq_init (n_mpq);
//      mpq_set_num (n_mpq, n_mpq_num);
//      mpq_set_denom (n_mpq, n_mpq_denom);
//      mpq_set (*(mpq_t*) value, n_mpq);
//      mpq_clear (n_mpq);
//      mpz_clear (n_mpq_num);
//      mpz_clear (n_mpq_denom);

      mpz_inits (n_mpq_num, n_mpq_denom, NULL);
      mpq_init (n_mpq);

      // should use "lf" for double
      sprintf (format_str, "[%s%s%%lf]", name, delim);
      sscanf (line, format_str, &n_mpq_double);
//      n_mpq_double=12;
      mpq_set_d (n_mpq, n_mpq_double);

      mpq_set (*(mpq_t*) value, n_mpq);
      mpq_clear (n_mpq);
      mpz_clear (n_mpq_num);
      mpz_clear (n_mpq_denom);
      break;

    }

  return 0;

}
///////////////////////////////////////
///////////////////////////////////////
void
load_mcwp_result_params (poly_params * p, FILE * input_file, int n_lower_bound,
			 int n_upper_bound)
{
//  [N: 1024]
//  [b0: 32]
//  [b1: 1]
//  [MWP: 2]
//  [CWP: 2]
//  [mcwp_case: 1]
//  [Exec_cycles_app: 1070.6667]
//  [Exec_cycles_per_thread: 29978.6667]
//  [occupancy: 0.03]
//  [Rep: 1.00]
//  [Active_blocks_per_SM: 2]
//  [shared_mem_bytes_total: 0]
//  [Blocks: 32]
//  [Threads_per_block: 32]
//  [Coal_per_mw: 0]
//  [Uncoal_per_mw: 4]
//  [Comp_insts: 17]
//  [Uncoal_Mem_insts: 3]
//  [Coal_Mem_insts: 0]
//  [Synch_insts: 0]

  mcwp_result_params_t * x = (mcwp_result_params_t *) malloc (
      sizeof(mcwp_result_params_t));
  init_mcwp_result_params (x);

  char delim[8] = ": ";
  while (1)
    {
      if (read_value_from_file (input_file, "N", delim, PARAM_S32,
				(void*) &x->n))
	break;

      if (read_value_from_file (input_file, "b0", delim, PARAM_S32,
				(void*) &x->b0))
	break;

      if (read_value_from_file (input_file, "b1", delim, PARAM_S32,
				(void*) &x->b1))
	break;

      if (read_value_from_file (input_file, "MWP", delim, PARAM_MPQ,
				(void*) &x->MWP))
	break;
      if (read_value_from_file (input_file, "CWP", delim, PARAM_MPQ,
				(void*) &x->CWP))
	break;

      if (read_value_from_file (input_file, "mcwp_case", delim,
      PARAM_S32,
				(void*) &x->mcwp_case))
	break;

      if (read_value_from_file (input_file, "Exec_cycles_app", delim, PARAM_MPQ,
				(void*) &x->Exec_cycles_app))
	break;
      if (read_value_from_file (input_file, "Exec_cycles_per_thread", delim,
      PARAM_MPQ,
				(void*) &x->Exec_cycles_per_thread))
	break;

      if (read_value_from_file (input_file, "occupancy", delim, PARAM_FLOAT,
				(void*) &x->occupancy))
	break;
      if (read_value_from_file (input_file, "Rep", delim, PARAM_MPQ,
				(void*) &x->Rep))
	break;
      if (read_value_from_file (input_file, "Active_blocks_per_SM", delim,
      PARAM_MPQ,
				(void*) &x->Active_blocks_per_SM))
	break;

      if (read_value_from_file (input_file, "shared_mem_bytes_total", delim,
      PARAM_S32,
				(void*) &x->shared_mem_bytes_total))
	break;

      if (read_value_from_file (input_file, "Blocks", delim, PARAM_MPQ,
				(void*) &x->Blocks))
	break;

      if (read_value_from_file (input_file, "Threads_per_block", delim,
      PARAM_MPQ,
				(void*) &x->Threads_per_block))
	break;
      if (read_value_from_file (input_file, "Coal_per_mw", delim, PARAM_MPQ,
				(void*) &x->Coal_per_mw))
	break;

      if (read_value_from_file (input_file, "Uncoal_per_mw", delim, PARAM_MPQ,
				(void*) &x->Uncoal_per_mw))
	break;

      if (read_value_from_file (input_file, "Comp_insts", delim, PARAM_MPQ,
				(void*) &x->Comp_insts))
	break;
      if (read_value_from_file (input_file, "Uncoal_Mem_insts", delim,
      PARAM_MPQ,
				(void*) &x->Uncoal_Mem_insts))
	break;

      if (read_value_from_file (input_file, "Coal_Mem_insts", delim, PARAM_MPQ,
				(void*) &x->Coal_Mem_insts))
	break;

      if (read_value_from_file (input_file, "Synch_insts", delim, PARAM_MPQ,
				(void*) &x->Synch_insts))
	break;

      if ((n_lower_bound <= x->n) && (n_upper_bound >= x->n))
	add_mcwp_result_to_table (x, p);
    }
}

///////////////////////////////////////
void
print_best_mcwp_result_params (mcwp_result_params_t * x, FILE * output_file)
{

  fprintf (
      output_file,
      "==========================================================================\n");
  fprintf (output_file, "[*BEST*: ");
  fprintf (output_file, "N:%d, b0:%d, b1:%d, mcwp_case=%d, ", x->n, x->b0,
	   x->b1, x->mcwp_case);
  gmp_fprintf (output_file,
	       "mwp:%2.0f, cwp:%2.0f, clocks:%.4f, clocks_per_thread:%.4f\n "
	       "occupancy:%.2f, rep:%.2f, active_blocks_per_sm:%.0f, ",
	       mpq_get_d (x->MWP), mpq_get_d (x->CWP),
	       mpq_get_d (x->Exec_cycles_app),
	       mpq_get_d (x->Exec_cycles_per_thread), x->occupancy,
	       mpq_get_d (x->Rep), mpq_get_d (x->Active_blocks_per_SM));

  fprintf (output_file, "shared_mem_bytes=%d]\n", x->shared_mem_bytes_total);
  fprintf (
      output_file,
      "==========================================================================\n");

//  gmp_fprintf (
//      output_file,
//      "Comp_insts:%.0f, Uncoal_Mem_insts:%.0f, Coal_Mem_insts:%.0f, Synch_insts:%.0f, \n ",
//      mpq_get_d (x->Comp_insts), mpq_get_d (x->Uncoal_Mem_insts),
//      mpq_get_d (x->Coal_Mem_insts), mpq_get_d (x->Synch_insts));

//  gmp_fprintf (
//      output_file,
//      "Coal_per_mw:%.0f, Uncoal_per_mw:%.0f, Blocks:%.0f, Threads_per_block:%.0f \n",
//      mpq_get_d (x->Coal_per_mw), mpq_get_d (x->Uncoal_per_mw),
//      mpq_get_d (x->Blocks), mpq_get_d (x->Threads_per_block));

//  fprintf (output_file, "]\n\n");
//  mpq_init (x->Threads_per_block);
//  mpq_init (x->Blocks);
//  mpq_init (x->Active_warps_per_block);
//  mpq_init (x->Active_warps_per_SM);
//  mpq_init (x->Rep);
//  mpq_init (x->Rep_without_B);
//  mpq_init (x->Exec_cycles_app);
}

///////////////////////////////////////
int
memcpy_mcwp_result (mcwp_result_params_t* dest, mcwp_result_params_t* src)
{

  dest->n = src->n;
  dest->b0 = src->b0;
  dest->b1 = src->b1;

  dest->shared_mem_bytes_total = src->shared_mem_bytes_total;
  dest->mcwp_case = src->mcwp_case;
  dest->occupancy = src->occupancy;

  mpq_set (dest->Comp_insts, src->Comp_insts);
  mpq_set (dest->Uncoal_Mem_insts, src->Uncoal_Mem_insts);
  mpq_set (dest->Coal_Mem_insts, src->Coal_Mem_insts);
  mpq_set (dest->Synch_insts, src->Synch_insts);
  mpq_set (dest->Coal_per_mw, src->Coal_per_mw);
  mpq_set (dest->Uncoal_per_mw, src->Uncoal_per_mw);
  mpq_set (dest->Active_blocks_per_SM, src->Active_blocks_per_SM);

  mpq_set (dest->Threads_per_block, src->Threads_per_block);
  mpq_set (dest->Blocks, src->Blocks);
  mpq_set (dest->Active_warps_per_block, src->Active_warps_per_block);
  mpq_set (dest->Active_warps_per_SM, src->Active_warps_per_SM);
  mpq_set (dest->MWP, src->MWP);
  mpq_set (dest->CWP, src->CWP);

  mpq_set (dest->MWP_peak_BW, src->MWP_peak_BW);
  mpq_set (dest->MWP_Without_BW_full, src->MWP_Without_BW_full);
  mpq_set (dest->CWP_full, src->CWP_full);

  mpq_set (dest->Mem_LD, src->Mem_LD);

  mpq_set (dest->Rep, src->Rep);
  mpq_set (dest->Rep_without_B, src->Rep_without_B);
  mpq_set (dest->Exec_cycles_app, src->Exec_cycles_app);
  mpq_set (dest->Exec_cycles_per_thread, src->Exec_cycles_per_thread);
}
///////////////////////////////////////
int
add_mcwp_result_to_table (mcwp_result_params_t * result, poly_params * p)
{

  if (p->mcwp_result_table_idx == p->mesh_size)
    {
      printf ("ERROR: mcwp_result_table is FULL!\n");
      exit (-1);
    }

  init_mcwp_result_params (&(p->mcwp_result_table[p->mcwp_result_table_idx]));
  memcpy_mcwp_result (&(p->mcwp_result_table[p->mcwp_result_table_idx]),
		      result);

#if VERBOSE
  printf ("Added to table [idx=%d]\n", p->mcwp_result_table_idx);
  print_mcwp_result_params (result, stdout);
#endif 
//  print_mcwp_result_params (&p->mcwp_result_table[p->mcwp_result_table_idx]);
  p->mcwp_result_table_idx++;

  return p->mcwp_result_table_idx;
}

///////////////////////////////////////
void
print_mcwp_result_table (poly_params * p, FILE * output_file)
{
  int n_points = p->mcwp_result_table_idx;
  int current_n = -1;
  int n, b0, b1;
  int best_idx = 0;

#if VERBOSE
  printf ("PRINTING TABLE [n_points=%d]!\n", n_points);
#endif
  for (int i = 0; i < n_points; i++)
    {
//      printf ("IDX_PRINT=%d\n", i);

      n = p->mcwp_result_table[i].n;
      b0 = p->mcwp_result_table[i].b0;
      b1 = p->mcwp_result_table[i].b1;

      if (p->mcwp_result_table[i].is_best_for_same_n == 1)
	{
	  best_idx = i;
//	  printf("*** found best[n=%d, b0=%d, b1=%d]\n", n, b0 , b1);
	}

      if (current_n != n)
	{
	  if ((current_n != -1))
	    print_best_mcwp_result_params (&(p->mcwp_result_table[best_idx]),
					   output_file);
	  current_n = n;
	}
      print_mcwp_result_params (&(p->mcwp_result_table[i]), output_file);
      if (i == n_points - 1)
	print_best_mcwp_result_params (&(p->mcwp_result_table[best_idx]),
				       output_file);
    }
}
///////////////////////////////////////

//typedef struct program_params_t
//{
//  int grid_dim_x;
//  int grid_dim_y;
//  int grid_dim_z;
//  int block_dim_x;
//  int block_dim_y;
//  int block_dim_z;
//  int shared_mem_bytes;
//} program_params_t;

///////////////////////////////////////
void
min2 (mpq_t m, mpq_t x, mpq_t y)
{
  if (mpq_cmp (x, y) > 0)
    mpq_set (m, y);
  else
    mpq_set (m, x);
}

///////////////////////////////////////

void
min3 (mpq_t m, mpq_t x, mpq_t y, mpq_t z)
{
  if (mpq_cmp (y, z) > 0)
    mpq_set (m, z);
  else
    mpq_set (m, y);
  if (mpq_cmp (x, m) < 0)
    mpq_set (m, x);
}

///////////////////////////////////////
void
mwp_cwp (trace_params_t* trace_params, device_params_t* device_params,
	 kernel_params_t* kernel_params, ptx_params_t * ptx_params,
	 int inputsize_for_rep, mcwp_result_params_t * current_result)
{

  int mcwp_case;
  mpq_t mwp, cwp, clockcycles, rep, active_blocks_per_sm;
  mpq_inits (mwp, cwp, clockcycles, rep, active_blocks_per_sm, NULL);

//  printf ("rep without n: %d\n", REP_WITHOUT_N);
  // mpq_t* res = (mpq_t*) malloc(sizeof(mpq_t) * 3);
  // mpq_inits(res);

  //	{ "13.Total_insts", 0 },
  //	{ "14.Comp_insts", 1 },
  //	{ "15.Mem_insts", 2 },
  //	{ "16.Uncoal_Mem_insts", 3 },
  //	{ "17.Coal_Mem_insts", 4 },
  //	{ "18.Synch_insts", 5 },
  //	{ "19.Coal_per_mw", 6 },
  //	{ "20.Uncoal_per_mw", 7 },
  //	{ "n_active_blocks_per_sm", 8 },
  //	{ "n_blocks", 9 },
  //	{ "n_warps", 10 } };

  mpq_t Comp_insts;
  mpq_t Uncoal_Mem_insts;
  mpq_t Coal_Mem_insts;
  mpq_t Synch_insts;
  mpq_t Coal_per_mw;
  mpq_t Uncoal_per_mw;
  mpq_t Active_blocks_per_SM;

  mpq_init (Comp_insts);
  mpq_init (Uncoal_Mem_insts);
  mpq_init (Coal_Mem_insts);
  mpq_init (Synch_insts);
  mpq_init (Coal_per_mw);
  mpq_init (Uncoal_per_mw);
  mpq_init (Active_blocks_per_SM);

//  mpq_set_si (Comp_insts, trace_params[1], 1);
//  mpq_set_si (Uncoal_Mem_insts, trace_params[3], 1);
//  mpq_set_si (Coal_Mem_insts, trace_params[4], 1);
//  mpq_set_si (Synch_insts, trace_params[5], 1);
//  mpq_set_si (Coal_per_mw, trace_params[6], 1);
//  mpq_set_si (Uncoal_per_mw, trace_params[7], 1);
//  mpq_set_si (Active_blocks_per_SM, trace_params[8], 1);
//  mpq_set (active_blocks_per_sm, Active_blocks_per_SM);

  mpq_set_si (Comp_insts, trace_params->Comp_insts, 1);
  mpq_set_si (Uncoal_Mem_insts, trace_params->Uncoal_Mem_insts, 1);
  mpq_set_si (Coal_Mem_insts, trace_params->Coal_Mem_insts, 1);
  mpq_set_si (Synch_insts, trace_params->Synch_insts, 1);
  mpq_set_si (Coal_per_mw, trace_params->Coal_per_mw, 1);
  mpq_set_si (Uncoal_per_mw, trace_params->Uncoal_per_mw, 1);

//  mpq_set_ui (Comp_insts, trace_params->Comp_insts, 1);
//  mpq_set_ui (Uncoal_Mem_insts, trace_params->Uncoal_Mem_insts, 1);
//  mpq_set_ui (Coal_Mem_insts, trace_params->Coal_Mem_insts, 1);
//  mpq_set_ui (Synch_insts, trace_params->Synch_insts, 1);
//  mpq_set_ui (Coal_per_mw, trace_params->Coal_per_mw, 1);
//  mpq_set_ui (Uncoal_per_mw, trace_params->Uncoal_per_mw, 1);

  mpq_set_si (Active_blocks_per_SM, kernel_params->active_blocks_per_sm, 1);
  mpq_set (active_blocks_per_sm, Active_blocks_per_SM);

#if VERBOSE >=3
  gmp_printf("Comp_insts = %Qd\n", Comp_insts);
  gmp_printf("Uncoal_Mem_insts = %Qd\n", Uncoal_Mem_insts);
  gmp_printf("Coal_Mem_insts = %Qd\n", Coal_Mem_insts);
  gmp_printf("Synch_insts = %Qd\n", Synch_insts);
  gmp_printf("Coal_per_mw = %Qd\n", Coal_per_mw);
  gmp_printf("Uncoal_per_mw = %Qd\n", Uncoal_per_mw);
  gmp_printf("Active_blocks_per_SM = %Qd\n", Active_blocks_per_SM);
#endif
//	double Issue_cycles = device_params[0];
//	double Mem_bandwidth = device_params[1];
//	double Mem_LD = device_params[2];
//	double Departure_del_uncoal = device_params[3];
//	double Departure_del_coal = device_params[4];
//	double Active_blocks_per_SM = device_params[5];
//	double Active_SMs = device_params[6];
//	double Freq = device_params[7];
//	double Load_bytes_per_warp = device_params[8];

//  mpq_t mem_to_core_freq_ratio;
//  mpq_init (mem_to_core_freq_ratio);
//  mpq_set_ui (mem_to_core_freq_ratio, 2, 3);
//
//  gmp_printf ("mem_to_core_freq_ratio: %Qd\n", mem_to_core_freq_ratio);
//  mpq_mul (Coal_Mem_insts, Coal_Mem_insts, mem_to_core_freq_ratio);
//  mpq_mul (Uncoal_Mem_insts, Uncoal_Mem_insts, mem_to_core_freq_ratio);

//    gmp_printf ("Comp_cycles: %Qd\n", Comp_cycles);

  mpq_t Issue_cycles; //0
  mpq_t Mem_bandwidth; //1
  mpq_t Mem_LD; //2
  mpq_t Departure_del_uncoal;
  mpq_t Departure_del_coal;
  mpq_t Active_SMs;
  mpq_t Freq;
  mpq_t Load_bytes_per_warp;

  mpq_init (Issue_cycles);
  mpq_init (Mem_bandwidth);
  mpq_init (Mem_LD);
  mpq_init (Departure_del_uncoal);
  mpq_init (Departure_del_coal);
  mpq_init (Active_SMs);
  mpq_init (Freq);
  mpq_init (Load_bytes_per_warp);

//	{ "Issue_cycles", 0 },
//	{ "Mem_bandwidth", 1 },
//	{ "Mem_LD", 2 },
//	{ "Departure_del_uncoal", 3 },
//	{ "Departure_del_coal", 4 },
//	{ "Active_SMs", 5 },
//	{ "Freq", 6 },
//	{ "Load_bytes_per_warp", 7 } };

//  mpq_set_si (Issue_cycles, device_params[0], 1);
//  mpq_set_si (Mem_bandwidth, device_params[1], 1);
//  mpq_set_si (Mem_LD, device_params[2], 1);
//  mpq_set_si (Departure_del_uncoal, device_params[3], 1);
//  mpq_set_si (Departure_del_coal, device_params[4], 1);
//  mpq_set_si (Active_SMs, device_params[5], 1);
//  mpq_set_si (Freq, device_params[6], 1000);
//  mpq_set_si (Load_bytes_per_warp, device_params[7], 1);

  mpq_set_si (Issue_cycles, device_params->Issue_cycles, 1);
  mpq_set_si (Mem_bandwidth, device_params->Mem_bandwidth, 1);
  mpq_set_si (Mem_LD, device_params->Mem_LD, 1);
  mpq_set_si (Departure_del_uncoal, device_params->Departure_del_uncoal, 1);
  mpq_set_si (Departure_del_coal, device_params->Departure_del_coal, 1);
  mpq_set_si (Active_SMs, device_params->Active_SMs, 1);
  mpq_set_si (Freq, device_params->Freq, 1000);
  mpq_set_si (Load_bytes_per_warp, device_params->Load_bytes_per_warp, 1);

#if VERBOSE >=3
  gmp_printf("issue_cycles = %Qd\n", Issue_cycles);
  gmp_printf("Mem_bandwidth = %Qd\n", Mem_bandwidth);
  gmp_printf("Mem_LD %Qd\n", Mem_LD);
  gmp_printf("Departure_del_uncoal %Qd\n", Departure_del_uncoal);
  gmp_printf("Departure_del_coal %Qd\n", Departure_del_coal);
  gmp_printf("Active_SMs %Qd\n", Active_SMs);
  gmp_printf("Freq %Qd\n", Freq);
  gmp_printf("Load_bytes_per_warp %Qd\n", Load_bytes_per_warp);
  printf("===========================\n");
#endif
  if (mpq_cmp_ui (Active_blocks_per_SM, 0, 1) == 0)
    {
      printf ("ERROR: active_blocks_per_sm==0!\n");
      exit (EXIT_FAILURE);
    }
  //printf("Error check done\n");

  // double Threads_per_block = thread_per_block;
  // double Blocks = n_block;
  // double Active_warps_per_block = Threads_per_block / 32;
  // double Active_warps_per_SM = Active_warps_per_block * Active_blocks_per_SM;

  mpq_t Threads_per_block;
  mpq_t Blocks;
  mpq_t Active_warps_per_block;
  mpq_t Active_warps_per_SM;

  mpq_init (Threads_per_block);
  mpq_init (Blocks);
  mpq_init (Active_warps_per_block);
  mpq_init (Active_warps_per_SM);

  mpq_set_si (Threads_per_block, kernel_params->threads_per_block, 1);
  //gmp_printf("Threads_per_block is : %Qd\n",Threads_per_block);
  mpq_set_si (Blocks, kernel_params->blocks_per_grid, 1);
  //gmp_printf("Blocks is : %Qd\n",Blocks);
  mpq_t const_temp;
  mpq_init (const_temp);
  mpq_set_si (const_temp, 32, 1);

  // this is not correct, the number of warps is computed as ceiling div
  // ceil(div(n_threads_per_block/32));

  int n_warps_per_block = (kernel_params->threads_per_block + 31) / 32;

  //mpq_div(Active_warps_per_block, Threads_per_block, const_temp);
  mpq_set_si (Active_warps_per_block, n_warps_per_block, 1);
  mpq_mul (Active_warps_per_SM, Active_warps_per_block, Active_blocks_per_SM);

  mpq_set_si (const_temp, 1, 1);
  if (mpq_cmp (Active_warps_per_block, const_temp) < 0)
    {
      printf ("Active warps per block is less than 1!\n");
      exit (EXIT_FAILURE);
    }
#if VERBOSE >=3
  gmp_printf("Threads_per_block is : %Qd\n",Threads_per_block);
  gmp_printf("Blocks is : %Qd\n",Blocks);
  gmp_printf("Active_warps_per_block is : %Qd\n",Active_warps_per_block);
  gmp_printf("Active_warps_per_SM is : %Qd\n",Active_warps_per_SM);
#endif

  //double N = Active_warps_per_SM;
  //double Mem_L_Uncoal = Mem_LD + (Uncoal_per_mw - 1) * Departure_del_uncoal;
  mpq_t Mem_L_Uncoal;
  mpq_init (Mem_L_Uncoal);

  mpq_sub (Uncoal_per_mw, Uncoal_per_mw, const_temp);
  mpq_mul (Mem_L_Uncoal, Departure_del_uncoal, Uncoal_per_mw);
  mpq_add (Mem_L_Uncoal, Mem_LD, Mem_L_Uncoal);
  mpq_add (Uncoal_per_mw, Uncoal_per_mw, const_temp);

  //double Mem_L_Coal = Mem_LD;

  //double Weight_uncoal = Uncoal_Mem_insts
  //		/ (Uncoal_Mem_insts + Coal_Mem_insts);

  mpq_t Weight_uncoal;
  mpq_init (Weight_uncoal);
  mpq_add (Weight_uncoal, Uncoal_Mem_insts, Coal_Mem_insts);
  mpq_div (Weight_uncoal, Uncoal_Mem_insts, Weight_uncoal);
#if VERBOSE >=3
  gmp_printf("Weight_uncoal is : %Qd\n",Weight_uncoal);
#endif

  //double Weight_coal = Coal_Mem_insts / (Uncoal_Mem_insts + Coal_Mem_insts);

  mpq_t Weight_coal;
  mpq_init (Weight_coal);
  mpq_add (Weight_coal, Uncoal_Mem_insts, Coal_Mem_insts);
  mpq_div (Weight_coal, Coal_Mem_insts, Weight_coal);

  //double Mem_L = Mem_L_Coal * Weight_coal + Mem_L_Uncoal * Weight_uncoal;
  mpq_t Mem_L;
  mpq_init (Mem_L);
  mpq_t temp;
  mpq_init (temp);
  mpq_mul (Mem_L, Mem_LD, Weight_coal);
  mpq_mul (temp, Mem_L_Uncoal, Weight_uncoal);
  mpq_add (Mem_L, Mem_L, temp);

#if VERBOSE >=3
  gmp_printf("Mem_L is : %Qd\n",Mem_L);
#endif

  //double Departure_delay = Uncoal_per_mw * Weight_uncoal
  //		* Departure_del_uncoal + Weight_coal * Departure_del_coal;
  mpq_t Departure_delay;
  mpq_init (Departure_delay);
  mpq_mul (temp, Uncoal_per_mw, Weight_uncoal);
  mpq_mul (temp, temp, Departure_del_uncoal);
  mpq_mul (Departure_delay, Weight_coal, Departure_del_coal);
  mpq_add (Departure_delay, temp, Departure_delay);
#if VERBOSE >=3
  gmp_printf("Departure_delay is : %Qd\n",Departure_delay);
#endif

  //double MWP_Without_BW_full = Mem_L / Departure_delay;
  mpq_t MWP_Without_BW_full;
  mpq_init (MWP_Without_BW_full);
  mpq_div (MWP_Without_BW_full, Mem_L, Departure_delay);
  mpq_ceil (MWP_Without_BW_full);

  //double MWP_Without_BW = min2(MWP_Without_BW_full, Active_warps_per_SM);
  mpq_t MWP_Without_BW;
  mpq_init (MWP_Without_BW);
  mpq_ceil (MWP_Without_BW);
  min2 (MWP_Without_BW, MWP_Without_BW_full, Active_warps_per_SM);

  //double BW_per_warp = Freq * Load_bytes_per_warp / Mem_L;
  mpq_t BW_per_warp;
  mpq_init (BW_per_warp);
  mpq_div (BW_per_warp, Load_bytes_per_warp, Mem_L);
  mpq_mul (BW_per_warp, BW_per_warp, Freq);

#if VERBOSE >=3
  gmp_printf("BW_per_warp is : %Qd\n",BW_per_warp);
#endif

  //double MWP_peak_BW = Mem_bandwidth / (BW_per_warp * Active_SMs);
  mpq_t MWP_peak_BW;
  mpq_init (MWP_peak_BW);
  mpq_mul (MWP_peak_BW, BW_per_warp, Active_SMs);
  mpq_div (MWP_peak_BW, Mem_bandwidth, MWP_peak_BW);

#if VERBOSE >=3
  gmp_printf("MWP_peak_BW is : %Qd\n",MWP_peak_BW);
#endif

  //double MWP = min3(MWP_Without_BW, MWP_peak_BW, Active_warps_per_SM);
  mpq_t MWP;
  mpq_init (MWP);
  min3 (MWP, MWP_Without_BW, MWP_peak_BW, Active_warps_per_SM);

#if VERBOSE >=3
  gmp_printf("MWP is : %Qd\n",MWP);
#endif

  //double Mem_cycles = Coal_Mem_insts * Mem_L_Coal
  //		+ Mem_L_Uncoal * Uncoal_Mem_insts;
  mpq_t Mem_cycles;
  mpq_init (Mem_cycles);
  mpq_mul (temp, Coal_Mem_insts, Mem_LD);
  mpq_mul (Mem_cycles, Mem_L_Uncoal, Uncoal_Mem_insts);
  mpq_add (Mem_cycles, Mem_cycles, temp);
#if VERBOSE >=3
  gmp_printf("Mem_cycles is : %Qd\n",Mem_cycles);
#endif

//	double total_insts = Comp_insts + Uncoal_Mem_insts + Coal_Mem_insts;
  mpq_t total_insts;
  mpq_init (total_insts);
  mpq_add (total_insts, Comp_insts, Uncoal_Mem_insts);
  mpq_add (total_insts, total_insts, Coal_Mem_insts);

//	double Comp_cycles = Issue_cycles * total_insts;
  mpq_t Comp_cycles;
  mpq_init (Comp_cycles);
  mpq_mul (Comp_cycles, Issue_cycles, total_insts);
#if VERBOSE >=3
  gmp_printf("Comp_cycles is : %Qd\n",Comp_cycles);
#endif

//	double CWP_full = (Mem_cycles + Comp_cycles) / Comp_cycles;
  mpq_t CWP_full;
  mpq_init (CWP_full);
  mpq_add (CWP_full, Mem_cycles, Comp_cycles);
  mpq_div (CWP_full, CWP_full, Comp_cycles);
  mpq_ceil (CWP_full);

  //double CWP = min2(CWP_full, Active_warps_per_SM);
  mpq_t CWP;
  mpq_init (CWP);
  min2 (CWP, CWP_full, Active_warps_per_SM);

#if VERBOSE >=3
  gmp_printf("CWP is : %Qd\n",CWP);
#endif

//	double Mem_insts = Uncoal_Mem_insts + Coal_Mem_insts;
  mpq_t Mem_insts;
  mpq_init (Mem_insts);
  mpq_add (Mem_insts, Uncoal_Mem_insts, Coal_Mem_insts);

  //double Total_insts = Mem_insts + Comp_insts;
  mpq_t Total_insts;
  mpq_init (Total_insts);
  mpq_add (Total_insts, Mem_insts, Comp_insts);

//	double Rep = Blocks / (Active_blocks_per_SM * Active_SMs);
  mpq_t Rep;

  mpq_init (Rep);
  mpq_mul (Rep, Active_blocks_per_SM, Active_SMs);
  mpq_div (Rep, Blocks, Rep);
  mpq_ceil (Rep);
//  mpz_t numer;
//  mpz_t denom;
//  mpz_init (numer);
//  mpz_init (denom);
//  mpz_set (numer, mpq_numref(Rep));
//  mpz_set (denom, mpq_denref(Rep));
//  mpz_cdiv_q (numer, numer, denom);
//  mpq_set_z (Rep, numer);
  mpq_set (rep, Rep);
//  mpz_clear (numer);
//  mpz_clear (denom);
//#if REP_WITHOUT_N
  mpq_t Input_for_rep;
  mpq_init (Input_for_rep);
  mpq_set_si (Input_for_rep, inputsize_for_rep, 1);

  mpq_t Rep_without_B;
  mpq_init (Rep_without_B);
  mpq_mul (Rep_without_B, Rep, Active_SMs);
  mpq_div (Rep_without_B, Rep_without_B, Input_for_rep);

  ///////////////////////////////////////////////
  ///////// this part should be revisited.
  ///////////////////////////////////////////////
//  double n_active_sm=mpq_get_d(Active_SMs);
//  double n_active_blocks_per_sm=mpq_get_d(Active_blocks_per_SM);
//  double n_blocks=mpq_get_d(Blocks); 
//  double t_num=n_blocks+n_active_blocks_per_sm*n_active_sm-1;
//  double t_denom=n_active_blocks_per_sm*n_active_sm;
//  double new_rep =(int) (t_num/t_denom);
//  mpq_set_d(Rep_without_B, new_rep);
//  printf ("new_rep=%f\n", new_rep);
  ///////////////////////////////////////////////

//#endif

#if VERBOSE >=2
  gmp_printf("Rep is : %Qd\n",Rep);
#endif

  mpq_set (mwp, MWP);
  mpq_set (cwp, CWP);
  //res[0] = MWP;
  //res[1] = CWP;
  //    printf("MWP: %f\n",MWP);
  //    printf("CWP: %f\n",CWP);

//	double Exec_cycles_app;
  mpq_t Exec_cycles_app;
  mpq_init (Exec_cycles_app);

  mcwp_case = -1;
  if (mpq_equal (MWP, Active_warps_per_SM) != 0
      && mpq_equal (CWP, Active_warps_per_SM) != 0)
    {
      //        printf("Case:(MWP==N) && (CWP== N)\n");
      //	Exec_cycles_app = (Mem_cycles + Comp_cycles
      //			+ Comp_cycles * (MWP - 1) / Mem_insts) * Rep;
      mcwp_case = 1;
      //	printf("CASE 1!\n");
      mpq_sub (temp, MWP, const_temp);
      mpq_mul (Exec_cycles_app, Comp_cycles, temp);
      mpq_div (Exec_cycles_app, Exec_cycles_app, Mem_insts);
      mpq_add (Exec_cycles_app, Exec_cycles_app, Mem_cycles);
      mpq_add (Exec_cycles_app, Exec_cycles_app, Comp_cycles);

//#if REP_ENABLED
      mpq_mul (Exec_cycles_app, Exec_cycles_app, Rep);
//#endif 

//#if REP_WITHOUT_N
      // mpq_mul (Exec_cycles_app, Exec_cycles_app, Rep_without_B);
//#endif
    }
  else if (mpq_cmp (CWP, MWP) >= 0 || mpq_cmp (Comp_cycles, Mem_cycles) > 0)
    {
      //        printf("Case:(CWP > MWP) || (Comp_cycles > Mem_cycles)\n");
      //	Exec_cycles_app = (Mem_cycles * N / MWP
      //			+ Comp_cycles * (MWP - 1) / Mem_insts) * Rep;
      mcwp_case = 2;
      //printf("CASE 2!\n");
      mpq_sub (temp, MWP, const_temp);
      mpq_mul (Exec_cycles_app, Comp_cycles, temp);
      mpq_div (Exec_cycles_app, Exec_cycles_app, Mem_insts);

      mpq_mul (temp, Mem_cycles, Active_warps_per_SM);
      mpq_div (temp, temp, MWP);
      mpq_add (Exec_cycles_app, Exec_cycles_app, temp);
//#if REP_ENABLED
      mpq_mul (Exec_cycles_app, Exec_cycles_app, Rep);
//#endif
//#if REP_WITHOUT_N
//      mpq_mul (Exec_cycles_app, Exec_cycles_app, Rep_without_B);
//#endif
    }
  else
    {
      //        printf("Case:else\n");
//		Exec_cycles_app = (Active_warps_per_SM * Comp_cycles + Mem_L) * Rep;
      mcwp_case = 3;
      //printf("CASE 3!\n");
      mpq_mul (Exec_cycles_app, Active_warps_per_SM, Comp_cycles);
      mpq_add (Exec_cycles_app, Exec_cycles_app, Mem_L);
//#if REP_ENABLED
      mpq_mul (Exec_cycles_app, Exec_cycles_app, Rep);
//#endif 
//#if REP_WITHOUT_N
//      mpq_mul (Exec_cycles_app, Exec_cycles_app, Rep_without_B);
//#endif
    }
  //    printf("Exec_cycles_app: %f\n",Exec_cycles_app);
#if VERBOSE >=3
  gmp_printf("Exec_cycles_app is : %Qd\n",Exec_cycles_app);
#endif
  //double NpWB = min2(MWP, Active_warps_per_block);
  mpq_t NpWB;
  mpq_init (NpWB);
  min2 (NpWB, MWP, Active_warps_per_block);
#if VERBOSE >=3
  gmp_printf("NpWB %Qd\n",NpWB);
#endif

  //double Synch_cost = Departure_delay * (NpWB - 1) * Synch_insts
  //		* Active_blocks_per_SM * Rep;
  mpq_t Synch_cost;
  mpq_init (Synch_cost);
  mpq_sub (Synch_cost, NpWB, const_temp);

  mpq_mul (Synch_cost, Synch_cost, Departure_delay);

  mpq_mul (Synch_cost, Synch_cost, Synch_insts);
  mpq_mul (Synch_cost, Synch_cost, Active_blocks_per_SM);

//#if REP_ENABLED
  mpq_mul (Synch_cost, Synch_cost, Rep);
//#endif
//#if REP_WITHOUT_N
//  mpq_mul (Synch_cost, Synch_cost, Rep_without_B);
//#endif
#if VERBOSE >=3
  gmp_printf("Synch_cost %Qd\n",Synch_cost);
#endif
  //double Exec_cycles_with_synch = Exec_cycles_app + Synch_cost;
  //    printf("Exec_cycles_with_synch: %f\n",Exec_cycles_with_synch);
  mpq_t Exec_cycles_with_synch;
  mpq_init (Exec_cycles_with_synch);

  mpq_add (Exec_cycles_with_synch, Exec_cycles_app, Synch_cost);
#if VERBOSE >=3
  gmp_printf("Exec_cycles_with_synch %Qd\n",Exec_cycles_with_synch);
#endif

  mpq_set (clockcycles, Exec_cycles_with_synch);
#if VERBOSE >=3
  gmp_printf("mcwp done\n");
#endif
  //return res;

  current_result->shared_mem_bytes_total = ptx_params->shared_mem_bytes_static
      + kernel_params->shared_mem_bytes_dynamic;
  current_result->mcwp_case = mcwp_case;
  float occupancy = (kernel_params->occupancy_percentage * 1.0) / 100.0;
  current_result->occupancy = occupancy;

  mpq_set (current_result->Comp_insts, Comp_insts);
  mpq_set (current_result->Uncoal_Mem_insts, Uncoal_Mem_insts);
  mpq_set (current_result->Coal_Mem_insts, Coal_Mem_insts);
  mpq_set (current_result->Synch_insts, Synch_insts);
  mpq_set (current_result->Coal_per_mw, Coal_per_mw);
  mpq_set (current_result->Uncoal_per_mw, Uncoal_per_mw);
  mpq_set (current_result->Active_blocks_per_SM, Active_blocks_per_SM);

  mpq_set (current_result->Threads_per_block, Threads_per_block);
  mpq_set (current_result->Blocks, Blocks);
  mpq_set (current_result->Active_warps_per_block, Active_warps_per_block);
  mpq_set (current_result->Active_warps_per_SM, Active_warps_per_SM);
  mpq_set (current_result->MWP, MWP);
  mpq_set (current_result->CWP, CWP);
  mpq_set (current_result->MWP_peak_BW, MWP_peak_BW);
  mpq_set (current_result->MWP_Without_BW_full, MWP_Without_BW_full);
  mpq_set (current_result->CWP_full, CWP_full);

  mpq_set (current_result->Mem_LD, Mem_LD);

//  gmp_printf("MWP_peak_BW=%f\n", mpq_get_d(MWP_peak_BW));
//  gmp_printf("MWP_Without_BW_full=%f\n", mpq_get_d(MWP_Without_BW_full));
//  gmp_printf("CWP_full=%f\n", mpq_get_d(CWP_full));
//  printf(short_dashed_line);

//#if REP_ENABLED
  mpq_set (current_result->Rep, Rep);
//#endif
//#if REP_WITHOUT_N
  mpq_set (current_result->Rep_without_B, Rep_without_B);
//#endif

  mpq_set (current_result->Exec_cycles_app, Exec_cycles_app);
  mpq_div (Exec_cycles_app, Exec_cycles_app, Rep);
  mpq_mul (Exec_cycles_app, Exec_cycles_app, Rep_without_B);
  mpq_set (current_result->Exec_cycles_per_thread, Exec_cycles_app);

}

///////////////////////////////////////

int
read_params_from_file (char * param_path, int *dest_params, int n_params,
		       const item_to_idx_dict* reference_param_list)
{
  FILE *param_file = fopen (param_path, "r");
  if (param_file == NULL)
    {
      printf ("ERROR: opening params file: [%s] failed!\n", param_path);
      return (EXIT_FAILURE);
    }

  char buffer[256];
  int param_idx = 0;

  char left_tag = '[';
  char right_tag = ']';
  char delim_tag = ':';
  char space_tag = ' ';
  int param_value = 0;
  char param_value_str[16];
  char param_name[256];
  int buffer_idx = 0;

  int * tmp_params = (int*) malloc (n_params * sizeof(int));
  memset (tmp_params, 0x00, n_params * sizeof(int));

  while (fgets (buffer, 256, param_file) != NULL && param_idx < n_params)
    {
      sprintf (param_value_str, "");
      sprintf (param_name, "");
      param_value = 0;

      for (buffer_idx = 0; buffer_idx < strlen (buffer); buffer_idx++)
	{
	  if (buffer[buffer_idx] == left_tag || buffer[buffer_idx] == space_tag)

	    continue;
	  else if (buffer[buffer_idx] == delim_tag)
	    break;
	  else
	    sprintf (param_name, "%s%c", param_name, buffer[buffer_idx]);
	}

      for (buffer_idx; buffer_idx < strlen (buffer); buffer_idx++)
	{
	  if (buffer[buffer_idx] == delim_tag
	      || buffer[buffer_idx] == space_tag)
	    continue;
	  if (buffer[buffer_idx] == right_tag)
	    break;
	  else
	    sprintf (param_value_str, "%s%c", param_value_str,
		     buffer[buffer_idx]);
	}

      param_value = atof (param_value_str);

#if VERBOSE >=3
      printf("param_name=%s\n", param_name);
      printf("param_val_str=%s\n", param_value_str);
      printf("param_value=%d\n", param_value);
      printf("================\n");
#endif
      //check if the right parameters are being read from the file
      if (strcmp (param_name, reference_param_list[param_idx].name) == 0)
	{
	  tmp_params[param_idx] = param_value;
	  param_idx += 1;
	}
      else
	{

	  printf ("ERROR: in reading param list from %s!\n", param_path);
	  printf ("param_name=[%s]\n", param_name);
	  exit (EXIT_FAILURE);
	}
    }
  fclose (param_file);
  memcpy (dest_params, tmp_params, n_params * sizeof(int));
  free (tmp_params);
  return (EXIT_SUCCESS);
}

///////////////////////////////////////

//int
//read_params_from_file_u64 (char * param_path, u64 *dest_params, int n_params,
//		       const item_to_idx_dict* reference_param_list)
//{
//  FILE *param_file = fopen (param_path, "r");
//  if (param_file == NULL)
//    {
//      printf ("ERROR: opening params file: [%s] failed!\n", param_path);
//      return (EXIT_FAILURE);
//    }
//
//  char buffer[256];
//  int param_idx = 0;
//
//  char left_tag = '[';
//  char right_tag = ']';
//  char delim_tag = ':';
//  char space_tag = ' ';
//  u64 param_value = 0;
//  char param_value_str[64];
//  char param_name[256];
//  int buffer_idx = 0;
//
//  u64 * tmp_params = (u64*) malloc (n_params * sizeof(u64));
//  memset (tmp_params, 0x00, n_params * sizeof(u64));
//
//  while (fgets (buffer, 256, param_file) != NULL && param_idx < n_params)
//    {
//      sprintf (param_value_str, "");
//      sprintf (param_name, "");
//      param_value = 0;
//
//      for (buffer_idx = 0; buffer_idx < strlen (buffer); buffer_idx++)
//	{
//	  if (buffer[buffer_idx] == left_tag || buffer[buffer_idx] == space_tag)
//
//	    continue;
//	  else if (buffer[buffer_idx] == delim_tag)
//	    break;
//	  else
//	    sprintf (param_name, "%s%c", param_name, buffer[buffer_idx]);
//	}
//
//      for (buffer_idx; buffer_idx < strlen (buffer); buffer_idx++)
//	{
//	  if (buffer[buffer_idx] == delim_tag
//	      || buffer[buffer_idx] == space_tag)
//	    continue;
//	  if (buffer[buffer_idx] == right_tag)
//	    break;
//	  else
//	    sprintf (param_value_str, "%s%c", param_value_str,
//		     buffer[buffer_idx]);
//	}
//
//      param_value = atoll(param_value_str);
//
////#if VERBOSE >=3
//      printf("param_name=%s\n", param_name);
//      printf("param_val_str=%s\n", param_value_str);
//      printf("param_value=%llu\n", param_value);
//      printf("================\n");
////#endif
//      //check if the right parameters are being read from the file
//      if (strcmp (param_name, reference_param_list[param_idx].name) == 0)
//	{
//	  tmp_params[param_idx] = param_value;
//	  param_idx += 1;
//	}
//      else
//	{
//
//	  printf ("ERROR: in reading param list from %s!\n", param_path);
//	  printf ("param_name=[%s]\n", param_name);
//	  exit (EXIT_FAILURE);
//	}
//    }
//  fclose (param_file);
//  memcpy (dest_params, tmp_params, n_params * sizeof(u64));
//  free (tmp_params);
//  return (EXIT_SUCCESS);
//}

///////////////////////////////////////

//int
//write_program_params_to_file (char *path,
//			      const program_params_t * program_params)
//{
//  FILE * file = fopen (path, "w");
//  if (file == NULL)
//    {
//      printf ("ERROR: opening %s!\n", path);
//      exit (EXIT_FAILURE);
//    }
//
//  fprintf (file, "%d\n", program_params->grid_dim_x);
//  fprintf (file, "%d\n", program_params->grid_dim_y);
//  fprintf (file, "%d\n", program_params->grid_dim_z);
//  fprintf (file, "%d\n", program_params->block_dim_x);
//  fprintf (file, "%d\n", program_params->block_dim_y);
//  fprintf (file, "%d\n", program_params->block_dim_z);
//  fprintf (file, "%d", program_params->shared_mem_bytes);
//
//  fclose (file);
//}

///////////////////////////////////////

//int
//write_kernel_params_to_file (char *path, const kernel_params_t * kernel_params)
//{
//  FILE * file = fopen (path, "w");
//  if (file == NULL)
//    {
//      printf ("ERROR: opening %s!\n", path);
//      exit (EXIT_FAILURE);
//    }
//
//  fprintf (file, "%d\n", kernel_params->grid_dim_x);
//  fprintf (file, "%d\n", kernel_params->grid_dim_y);
//  fprintf (file, "%d\n", kernel_params->grid_dim_z);
//  fprintf (file, "%d\n", kernel_params->block_dim_x);
//  fprintf (file, "%d\n", kernel_params->block_dim_y);
//  fprintf (file, "%d\n", kernel_params->block_dim_z);
//  fprintf (file, "%d", kernel_params->shared_mem_bytes);
//
//  fclose (file);
//}
///////////////////////////////////////
int
read_kernel_params_from_file (kernel_params_t * kernel_params, char *path)
{

  read_params_from_file (path, kernel_params, N_KERNEL_PARAMS,
			 global_kernel_params_list);
//  return 0;

//  FILE * file = fopen (path, "r");
//  if (file == NULL)
//    {
//      printf ("ERROR: opening %s!\n", path);
//      exit (EXIT_FAILURE);
//    }
//
#if VERBOSE>=2
  printf("kernel_params_read_from from [%s]\n", path);
#endif 
//  fscanf (file, "%d", &kernel_params->grid_dim_x);
//  fscanf (file, "%d", &kernel_params->grid_dim_y);
//  fscanf (file, "%d", &kernel_params->grid_dim_z);
//  fscanf (file, "%d", &kernel_params->block_dim_x);
//  fscanf (file, "%d", &kernel_params->block_dim_y);
//  fscanf (file, "%d", &kernel_params->block_dim_z);
//  fscanf (file, "%d", &kernel_params->shared_mem_bytes);
//
  kernel_params->threads_per_block = kernel_params->block_dim_x
      * kernel_params->block_dim_y * kernel_params->block_dim_z;
  kernel_params->blocks_per_grid = kernel_params->grid_dim_x
      * kernel_params->grid_dim_y * kernel_params->grid_dim_z;

#if VERBOSE>=2
  printf("[grid_dim  (x,y,z)=(%d, %d, %d)]\n", kernel_params->grid_dim_x, kernel_params->grid_dim_y, kernel_params->grid_dim_z);
  printf("[block_dim (x,y,z)=(%d, %d, %d)]\n", kernel_params->block_dim_x, kernel_params->block_dim_y, kernel_params->block_dim_z);
//  printf("[n_blocks = %d , threads_per_block = %d, shared_mem_bytes = %d] \n=========\n",
//      kernel_params->blocks_per_grid, kernel_params->threads_per_block, kernel_params->shared_mem_bytes);
#endif

//  fclose (file);
}

///////////////////////////////////////
int
read_sm_arch (char * sm_arch_str)
{
  const char *path = "sm_arch.tmp";
  FILE * file = fopen (path, "r");
  if (file == NULL)
    {
      printf ("ERROR: opening %s!\n", path);
      exit (EXIT_FAILURE);
    }

#if VERBOSE>=2
  printf("reading sm_arch from [%s]\n", path);
#endif 

  fscanf (sm_arch_str, file);

  printf ("sm_arch=[%s]", sm_arch_str);
  fclose (file);

}
///////////////////////////////////////
int
read_ptx_params_from_file (ptx_params_t * ptx_params, char *path)
{
//  FILE * file = fopen (path, "r");
//  if (file == NULL)
//    {
//      printf ("ERROR: opening %s!\n", path);
//      exit (EXIT_FAILURE);
//    }
//
//#if VERBOSE>=2
//  printf("ptx_params_read_from from [%s]\n", path);
//#endif 
//  fscanf (file, "%d", &ptx_params->shared_mem_static);
//  fscanf (file, "%d", &ptx_params->registers);
//
//#if VERBOSE>=2
////  printf("[n_blocks = %d , threads_per_block = %d, shared_mem_bytes = %d] \n=========\n",
////      kernel_params->blocks_per_grid, kernel_params->threads_per_block, kernel_params->shared_mem_bytes);
//#endif 
//
//  fclose (file);
  read_params_from_file (path, ptx_params, N_PTX_PARAMS,
			 global_ptx_params_list);

  return;
}

///////////////////////////////////////

int
get_num_lines_in_file (char * path)
{

  FILE *file = fopen (path, "r");
  if (file == NULL)
    {
      printf ("ERROR: opening file=%s ... \n", path);
      exit (EXIT_FAILURE);
    }
  int n_lines = 0;
  for (char c = getc (file); c != EOF; c = getc (file))
    {
      if (c == '\n')
	n_lines++;
    }
  fclose (file);
  return n_lines;
}

///////////////////////////////////////
//
int
init_output_path_list (char *** file_path_list_out, char ** kernel_name_list,
		       char * prefix, char * suffix, int n_kernels)
{
  char ** file_path_list = (char **) malloc (n_kernels * sizeof(char *));
  int str_size = strlen (prefix) + strlen (suffix) + 1;
  for (int i = 0; i < n_kernels; i++)
    {
      file_path_list[i] = (char *) malloc (
	  (str_size + strlen (kernel_name_list[i])) * sizeof(char));
      strcpy (file_path_list[i], prefix);
      strcat (file_path_list[i], kernel_name_list[i]);
      strcat (file_path_list[i], suffix);
#if VERBOSE >=3
      printf("[file_path: %s]\n", file_path_list[i]);
#endif
    }

  *file_path_list_out = file_path_list;
  return 0;
}

///////////////////////////////////////
//
//int
//print_interpolation_result (char * kernel_name, int mcwp_case,
//			    poly_params kernel_poly_params, int k, char** syms,
//			    char*output_file)
//{
//
//  fprintf (stderr,
//	   "Interpolated a rational function for kernel %s in case %d: \n",
//	   kernel_name, mcwp_case);
//  printf ("In rational number:\n");
//  fprintf (stderr, "f%d := (", mcwp_case);
//  printPoly_AA (stderr, kernel_poly_params.poly[k], syms,
//		kernel_poly_params.nvar);
//  //printf("------------------------------------------------------\n");
//  fprintf (stderr, ") / (");
//  printPoly_AA (stderr, kernel_poly_params.denompoly[k], syms,
//		kernel_poly_params.nvar);
//  fprintf (stderr, ");");
//  printf ("\n");
//  printf ("In floating point:\n");
//
//  fprintf (stderr, "f%d := (", mcwp_case);
//  printPolyDouble_AA (stderr, kernel_poly_params.poly[k], syms,
//		      kernel_poly_params.nvar);
//  //printf("------------------------------------------------------\n");
//
//  fprintf (stderr, ") / (");
//  printPolyDouble_AA (stderr, kernel_poly_params.denompoly[k], syms,
//		      kernel_poly_params.nvar);
//  fprintf (stderr, ");");
//  printf ("\n");
//
//  fprintf (output_file,
//	   "Interpolated a rational function for kernel %s in case %d: \n",
//	   kernel_name, mcwp_case);
//  fprintf (output_file, "In rational number:\n");
//
//  fprintf (output_file, "f%d := (", mcwp_case);
//  printPoly_AA (output_file, kernel_poly_params.poly[k], syms,
//		kernel_poly_params.nvar);
//  fprintf (output_file, ") / (");
////	fprintf(output_file,
////			"------------------------------------------------------\n");
//  printPoly_AA (output_file, kernel_poly_params.denompoly[k], syms,
//		kernel_poly_params.nvar);
//  fprintf (output_file, ");");
//  fprintf (output_file, "\n");
//
//  fprintf (output_file, "In floating point:\n");
////	fprintf(output_file, "Numerator:\n");
//  fprintf (output_file, "f%d := (", mcwp_case);
//  printPolyDouble_AA (output_file, kernel_poly_params.poly[k], syms,
//		      kernel_poly_params.nvar);
////	fprintf(output_file, "Denominator:\n");
//  fprintf (output_file, ") / (");
////	fprintf(output_file,
////			"------------------------------------------------------\n");
//  printPolyDouble_AA (output_file, kernel_poly_params.denompoly[k], syms,
//		      kernel_poly_params.nvar);
//  fprintf (output_file, ");");
//  fprintf (output_file, "\n\n");
//}
///////////////////////////////////////
int
print_interpolation_result_for_kernel (poly_params * kernel_poly_params,
				       FILE * output_file)
{

//  char * output_str
//  printf("symbols[0]=%s\n", kernel_poly_params->symbols[0]);
//  printf("symbols[1]=%s\n", kernel_poly_params->symbols[1]);

  char * buffer;
  size_t size;
  FILE * file = open_memstream (&buffer, &size);
  for (int mcwp_case = 0; mcwp_case < 3; mcwp_case++)
    {
      if (kernel_poly_params->status[mcwp_case] != SUCCESSFUL_INTERP)
	continue;

//#if VERBOSE
//      if (kernel_poly_params->status[mcwp_case] == SUCCESSFUL_INTERP)
//	printf ("status=CASE %d is successfully interpoalted\n", mcwp_case + 1);
//#endif
      fprintf (
	  file,
	  "[Interpolated a rational function for kernel [%s] in case [%d]]...\n",
	  kernel_poly_params->kernel_name, mcwp_case);

      /////////////////////////////////////
      // rational
      /////////////////////////////////////
      fprintf (file, ""
	       "\n"
	       "-----------------\n"
	       "Rational number:"
	       "\n"
	       "-----------------"
	       "\n");
      fprintf (file, "f%d := (", mcwp_case);
      printPoly_AA (file, kernel_poly_params->poly[mcwp_case],
		    kernel_poly_params->symbols, kernel_poly_params->nvar);
      fprintf (file, ") \n/\n (");
      printPoly_AA (file, kernel_poly_params->denompoly[mcwp_case],
		    kernel_poly_params->symbols, kernel_poly_params->nvar);
      fprintf (file, ");\n");

      /////////////////////////////////////
      // floating point
      /////////////////////////////////////

      fprintf (file, ""
	       "\n"
	       "-----------------\n"
	       "Floating point:"
	       "\n"
	       "-----------------"
	       "\n");

      fprintf (file, "f%d := (", mcwp_case);
      printPolyDouble_AA (file, kernel_poly_params->poly[mcwp_case],
			  kernel_poly_params->symbols,
			  kernel_poly_params->nvar);

      fprintf (file, ") \n/\n (");
      printPolyDouble_AA (file, kernel_poly_params->denompoly[mcwp_case],
			  kernel_poly_params->symbols,
			  kernel_poly_params->nvar);
      fprintf (file, ");\n");

      fprintf (file, "\n\n=================================\n"
	       "=================================\n\n");

//  fprintf (output_file,
//	   "Interpolated a rational function for kernel %s in case %d: \n",
//	   kernel_name, mcwp_case);
//  fprintf (output_file, "In rational number:\n");
//
//  fprintf (output_file, "f%d := (", mcwp_case);
//  printPoly_AA (output_file, kernel_poly_params.poly[k], syms,
//		kernel_poly_params.nvar);
//  fprintf (output_file, ") / (");
////	fprintf(output_file,
////			"------------------------------------------------------\n");
//  printPoly_AA (output_file, kernel_poly_params.denompoly[k], syms,
//		kernel_poly_params.nvar);
//  fprintf (output_file, ");");
//  fprintf (output_file, "\n");
//
//  fprintf (output_file, "In floating point:\n");
////	fprintf(output_file, "Numerator:\n");
//  fprintf (output_file, "f%d := (", mcwp_case);
//  printPolyDouble_AA (output_file, kernel_poly_params.poly[k], syms,
//		      kernel_poly_params.nvar);
////	fprintf(output_file, "Denominator:\n");
//  fprintf (output_file, ") / (");
////	fprintf(output_file,
////			"------------------------------------------------------\n");
//  printPolyDouble_AA (output_file, kernel_poly_params.denompoly[k], syms,
//		      kernel_poly_params.nvar);
//  fprintf (output_file, ");");
//  fprintf (output_file, "\n\n");
    }

  fflush (file);
  fprintf (output_file, "%s\n", buffer);
}
///////////////////////////////////////

//void
//print_mcwp_result (FILE* path, int n, int b0, int b1, mpq_t mwp, mpq_t cwp,
//		   mpq_t clocks, int mcwp_case, float occupancy, mpq_t rep,
//		   mpq_t active_blocks_per_sm,
//		   enum kernel_type_enum kernel_type)
//{
//  if (kernel_type == OneDimKernel)
//    {
//      gmp_fprintf (
//	  path,
//	  "[N:%d, b:%-4d,  mwp:%2.0f, cwp:%2.0f, clocks:%.4f, case: %d,\n occupancy:%.2f, rep:%.2f, active_blocks_per_sm:%.0f]\n\n",
//	  n, b0, mpq_get_d (mwp), mpq_get_d (cwp), mpq_get_d (clocks),
//	  mcwp_case, occupancy, mpq_get_d (rep),
//	  mpq_get_d (active_blocks_per_sm));
//#if VERBOSE
//
//      gmp_printf("[N:%d, b:%-4d, mwp:%2.0f, cwp:%2.0f, clocks:%.4f,\n occupancy:%.2f, rep:%.2f, active_blocks_per_sm:%.0f]\n\n",
//	  n, b0,mpq_get_d(mwp),mpq_get_d(cwp),mpq_get_d(clocks),occupancy,mpq_get_d(rep), mpq_get_d(active_blocks_per_sm));
//#endif
//    }
//  else
//    {
//      gmp_fprintf (
//	  path,
//	  "[N:%d, b0:%-4d, b1:%-4d, mwp:%2.0f, cwp:%2.0f, clocks:%.4f, case: %d,\n occupancy:%.2f, rep:%.2f, active_blocks_per_sm:%.0f]\n\n",
//	  n, b0, b1, mpq_get_d (mwp), mpq_get_d (cwp), mpq_get_d (clocks),
//	  mcwp_case, occupancy, mpq_get_d (rep),
//	  mpq_get_d (active_blocks_per_sm));
//#if VERBOSE
//
//      gmp_printf("[N:%d, b0:%-4d, b1:%-4d, mwp:%2.0f, cwp:%2.0f, clocks:%.4f,\n occupancy:%.2f, rep:%.2f, active_blocks_per_sm:%.0f]\n\n",
//	  n, b0,b1,mpq_get_d(mwp),mpq_get_d(cwp),mpq_get_d(clocks),occupancy,mpq_get_d(rep), mpq_get_d(active_blocks_per_sm));
//#endif
//    }
//}

///////////////////////////////////////
//void
//print_best_mcwp_result (char* path, int n, int b0, int b1, mpq_t best_clock,
//			int best_case, int kernel_id,
//			enum kernel_type_enum kernel_type)
//{
//  if (kernel_type == OneDimKernel)
//    {
//      gmp_fprintf (
//	  path,
//	  "-------------------------------------------------------\n"
//	  "[kernel[%d]: Best config for N = %d, b = %d, clocks = %.4f, case = %d]\n"
//	  "-------------------------------------------------------\n\n",
//	  kernel_id, n, b0, mpq_get_d (best_clock), best_case);
//
//#if VERBOSE
//      gmp_printf(
//	  "-------------------------------------------------------\n"
//	  "[kernel[%d]: Best config for N = %d, b = %d, clocks = %.4f, case = %d]\n"
//	  "-------------------------------------------------------\n\n", kernel_id,n, b0,
//	  mpq_get_d(best_clock),best_case);
//#endif
//    }
//  else
//
//    {
//      gmp_fprintf (
//	  path,
//	  "-------------------------------------------------------\n"
//	  "[kernel[%d]: Best config for N = %d, b0 = %d, b1 = %d, clocks = %.4f, case = %d]\n"
//	  "-------------------------------------------------------\n\n",
//	  kernel_id, n, b0, b1, mpq_get_d (best_clock), best_case);
//
//#if VERBOSE
//      gmp_printf(
//	  "-------------------------------------------------------\n"
//	  "[kernel[%d]: Best config for N = %d, b0 = %d, b1 = %d, clocks = %.4f, case = %d]\n"
//	  "-------------------------------------------------------\n\n", kernel_id, n, b0,b1,
//	  mpq_get_d(best_clock),best_case);
//#endif
//    }
//}

///////////////////////////////////////
//
#endif
