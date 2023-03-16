#ifndef MWP_CWP_H_
#define MWP_CWP_H_

#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <gmp.h>
#include <limits.h>
#include <math.h>

#include "../../src/mcwp/mcwp.h"

/////////////////////////////////////////
#ifndef DEGREE_BOUND
#define DEGREE_BOUND 5
#endif

//percentage of error tolerance abs(evaluation-emulation)
#define EVALUATION_TOLERANCE (20)

/////////////////////////////////////////
poly_params*
do_interpolation (poly_params * kernel_poly_params, int n_kernels,
		  char *device_name, int mode);

/////////////////////////////////////
enum rational_program_param_enum
{
  shared_mem_bytes_total,
  occupancy,
  Exec_cycles_app,
  Blocks,
  MWP,
  CWP,
  MWP_peak_BW,
  MWP_Without_BW_full,
  CWP_full,
  Mem_LD,
  Coal_per_mw,
  Uncoal_per_mw,
  Active_blocks_per_SM,
  Threads_per_block,
  Active_warps_per_block,
  Active_warps_per_SM,
  Comp_insts,
  Uncoal_Mem_insts,
  Coal_Mem_insts,
  Synch_insts,
  Rep,
  Rep_without_B
};

/////////////////////////////////////////
void
get_kernel_param_name (char**param_name_in,
		       enum rational_program_param_enum rational_program_case)
{
  char * param_name = param_name_in;
  switch (rational_program_case)
    {

    case shared_mem_bytes_total:
      sprintf (param_name, "shared_mem_bytes_total");
      break;

    case occupancy:
      sprintf (param_name, "occupancy");
      break;
    case Exec_cycles_app:
      sprintf (param_name, "Exec_cycles_app");
      break;
    case Blocks:
      sprintf (param_name, "Blocks");
      break;
    case MWP:
      sprintf (param_name, "MWP");
      break;
    case CWP:
      sprintf (param_name, "CWP");
      break;
    case MWP_peak_BW:
      sprintf (param_name, "MWP_peak_BW");
      break;
    case MWP_Without_BW_full:
      sprintf (param_name, "MWP_Without_BW_full");
      break;
    case CWP_full:
      sprintf (param_name, "CWP_full");
      break;

    case Mem_LD:
      sprintf (param_name, "Mem_LD");
      break;

    case Coal_per_mw:
      sprintf (param_name, "Coal_per_mw");
      break;
    case Uncoal_per_mw:
      sprintf (param_name, "Uncoal_per_mw");
      break;
    case Active_blocks_per_SM:
      sprintf (param_name, "Active_blocks_per_SM");
      break;
    case Threads_per_block:
      sprintf (param_name, "Threads_per_block");
      break;
    case Active_warps_per_block:
      sprintf (param_name, "Active_warps_per_block");
      break;
    case Active_warps_per_SM:
      sprintf (param_name, "Active_warps_per_SM");
      break;
    case Comp_insts:
      sprintf (param_name, "Comp_insts");
      break;
    case Uncoal_Mem_insts:
      sprintf (param_name, "Uncoal_Mem_insts");
      break;
    case Coal_Mem_insts:
      sprintf (param_name, "Coal_Mem_insts");
      break;
    case Synch_insts:
      sprintf (param_name, "Synch_insts");
      break;
    case Rep:
      sprintf (param_name, "Rep");
      break;
    case Rep_without_B:
      sprintf (param_name, "Rep_without_B");
      break;
    }
}

/////////////////////////////////////////
void
get_kernel_param_value (mpq_t value,
			enum rational_program_param_enum rational_program_case,
			mcwp_result_params_t * mcwp_result)
{

//  printf ("CASE=%d\n", rational_program_case);

  switch (rational_program_case)
    {
    case occupancy:
      mpq_set_ui (value, 1000.0 * mcwp_result->occupancy, 1000);
      break;

    case Exec_cycles_app:
      mpq_set (value, mcwp_result->Exec_cycles_app);
      break;

    case shared_mem_bytes_total:
      mpq_set_ui (value, mcwp_result->shared_mem_bytes_total, 1);
      break;

    case Blocks:
      mpq_set (value, mcwp_result->Blocks);
      break;

    case Comp_insts:
      mpq_set (value, mcwp_result->Comp_insts);
      break;

    case Uncoal_Mem_insts:
      mpq_set (value, mcwp_result->Uncoal_Mem_insts);
      break;

    case Coal_Mem_insts:
      mpq_set (value, mcwp_result->Coal_Mem_insts);
      break;

    case Synch_insts:
      mpq_set (value, mcwp_result->Synch_insts);
      break;

    case Coal_per_mw:
      mpq_set (value, mcwp_result->Coal_per_mw);
      break;

    case Uncoal_per_mw:
      mpq_set (value, mcwp_result->Uncoal_per_mw);
      break;

    case Active_warps_per_SM:
      mpq_set (value, mcwp_result->Active_warps_per_SM);
//      gmp_printf ("value=%f\n", mpq_get_d (mcwp_result->Active_warps_per_SM));
      break;

    case Active_blocks_per_SM:
      mpq_set (value, mcwp_result->Active_blocks_per_SM);
      //      gmp_printf ("value=%f\n", mpq_get_d (mcwp_result->Active_warps_per_SM));
      break;

    case MWP_peak_BW:
      mpq_set (value, mcwp_result->MWP_peak_BW);
//      gmp_printf ("value=%f\n", mpq_get_d (mcwp_result->MWP_peak_BW));
      break;

    case MWP_Without_BW_full:
      mpq_set (value, mcwp_result->MWP_Without_BW_full);
//      gmp_printf ("value=%f\n", mpq_get_d (mcwp_result->MWP_Without_BW_full));
      break;

    case CWP_full:
      mpq_set (value, mcwp_result->CWP_full);
//      gmp_printf ("value=%f\n", mpq_get_d (mcwp_result->CWP_full));
      break;

    case Mem_LD:
      mpq_set (value, mcwp_result->Mem_LD);
//      gmp_printf ("value=%f\n", mpq_get_d (mcwp_result->Mem_LD));
      break;
    }
}
///////////////////////////////////////
void
init_kernel_names (char*** kernel_names_out, int n_kernels)
{
  FILE *kernel_name_list = fopen ("kernel_name_list.tmp", "r");
  if (kernel_name_list == NULL)
    {
      printf ("ERROR: opening kernel_name_list.tmp!\n");
      exit (EXIT_FAILURE);
    }

  char** kernel_names = (char **) malloc (n_kernels * sizeof(char *));
  for (int i = 0; i < n_kernels; i++)
    {
      kernel_names[i] = (char *) malloc (LEN_KERNEL_NAME * sizeof(char));
      fgets (kernel_names[i], LEN_KERNEL_NAME, kernel_name_list);
      kernel_names[i][strcspn (kernel_names[i], "\n")] = '\0';
    }
  fclose (kernel_name_list);

#if VERBOSE
  for (int i = 0; i < n_kernels; i++)
  printf("kernel [%d] : [%s]\n", i, kernel_names[i]);
#endif

  *kernel_names_out = kernel_names;
}

///////////////////////////////////////
int
init_mcwp_output_files (FILE** mcwp_result, FILE**interpolation_result,
			char** result_file_path_list,
			char** interpolation_file_path_list, int n_kernels)
{
  for (int k = 0; k < n_kernels; k++)
    {
      mcwp_result[k] = fopen (result_file_path_list[k], "w");
      setbuf (mcwp_result[k], NULL);
      if (mcwp_result[k] == NULL)
	{
	  printf ("ERROR: opening [%s]!\n", result_file_path_list[k]);
	  exit (EXIT_FAILURE);
	}
      interpolation_result[k] = fopen (interpolation_file_path_list[k], "w");
      setbuf (interpolation_result[k], NULL);
      if (interpolation_result[k] == NULL)
	{
	  printf ("ERROR: opening [%s]!\n", interpolation_file_path_list[k]);
	  exit (EXIT_FAILURE);
	}
    }
  return EXIT_SUCCESS;
}
///////////////////////////////////////
int
init_best_n_b0_b1_clock (int*best_n, int *best_b0, int *best_b1,
			 mpq_t* best_clock)
{
//  int n_bytes = 1 * sizeof(int);
//  memset (best_n, 0x00, n_bytes);
//  memset (best_b0, 0x00, n_bytes);
//  memset (best_b1, 0x00, n_bytes);
//
//  for (int i = 0; i < 1; i++)
//    {
//      mpq_init (best_clock[i]);
//      mpq_set_ui (best_clock[i], 0, 1);
//    }
  *best_n = 0;
  *best_b0 = 0;
  *best_b1 = 0;
  mpq_init (best_clock);
  mpq_set_ui (best_clock, 0, 1);
  return 0;
}

///////////////////////////////////////
int
find_best_results (poly_params * kernel_poly)
{
  int best_n, best_b0, best_b1;
  mpq_t best_clock;

//enum kernel_type_enum kernel_type = kernel_type_tmp;

  int n, b0, b1, mcwp_case;
  int n_points = kernel_poly->mcwp_result_table_idx;

  mpq_t clock;
  mpq_init (clock);
  mpq_set_ui (clock, 0, 1);

  int best_idx = 0;
  int current_n = -1;
  //finding the point with the minimum clock cycles
  //for each category of "n"

  current_n = -1;
  for (int i = 0; i < n_points; i++)
    {

      //get n for current point
      n = kernel_poly->mcwp_result_table[i].n;
      //make sure the current point is not marked as the best
      //before the evaulation is complete.
      kernel_poly->mcwp_result_table[i].is_best_for_same_n = 0;

      //new n is being visited.
      if (current_n != n)
	{
	  current_n = n;
	  init_best_n_b0_b1_clock (&best_n, &best_b0, &best_b1, best_clock);

	  if (current_n != -1)
	    {
	      kernel_poly->mcwp_result_table[best_idx].is_best_for_same_n = 1;
	    }
	}

      if (i == n_points - 1)
	{
	  kernel_poly->mcwp_result_table[best_idx].is_best_for_same_n = 1;
	}

      mpq_set (clock, kernel_poly->mcwp_result_table[i].Exec_cycles_app);
      if ((mpq_cmp (best_clock, clock) > 0) || (best_n == 0))
	{
	  best_idx = i;

	  b0 = kernel_poly->mcwp_result_table[i].b0;
	  b1 = kernel_poly->mcwp_result_table[i].b1;
	  mcwp_case = kernel_poly->mcwp_result_table[i].mcwp_case;

	  best_n = n;
	  best_b0 = b0;
	  best_b1 = b1;
	  mpq_set (best_clock, clock);
#if VERBOSE>=2
	  gmp_printf ("checking [n=%d, b0=%d, b1=%d, case=%d, clocks=%f] for best ...\n", n,
	      b0, b1, mcwp_case, mpq_get_d (clock));
#endif
	}
    }

#if VERBOSE>=2
  printf ("[Best results for kernel %s]\n", kernel_poly->kernel_name);
  for (int i = 0; i < n_points; i++)
    {

      if(kernel_poly->mcwp_result_table[i].is_best_for_same_n==1)
	{
	  n = kernel_poly->mcwp_result_table[i].n;
	  b0 = kernel_poly->mcwp_result_table[i].b0;
	  b1 = kernel_poly->mcwp_result_table[i].b1;
	  mcwp_case = kernel_poly->mcwp_result_table[i].mcwp_case;
	  mpq_set (clock, kernel_poly->mcwp_result_table[i].Exec_cycles_app);

	  gmp_printf ("BEST for [n=%d, b0=%d, b1=%d, case=%d, clocks=%f] for best ...\n", n,
	      b0, b1, mcwp_case, mpq_get_d (clock));
	}
    }
#endif
//  for (int i = 0; i < 3; i++)
//    {
//      gmp_printf ("[Best for case %d: n=%d, b0=%d, b1=%d, clocks=%0.4f]\n",
//		  i + 1, (best_n[i]), (best_b0[i]), (best_b1[i]),
//		  mpq_get_d (best_clock[i]));
//    }
}

///////////////////////////////////////
int
get_error_percentage (mpq_t t0, mpq_t t1)
{
  mpq_t x, y;
  mpq_inits (x, y, NULL);
  //should be: x>=y
  if (mpq_cmp (t0, t1) >= 0)
    {
      mpq_set (x, t0);
      mpq_set (y, t1);
    }
  else
    {
      mpq_set (x, t1);
      mpq_set (y, t0);
    }

//  gmp_printf ("y=%f\n", mpq_get_d (y));
  float percentage;

  float a, b;
  a = mpq_get_d (x);
  b = mpq_get_d (y);

  if (b != 0)
    {
      percentage = 100.0 * ((a - b) / a);
//      printf ("FIRST_PERCENTAGE = %d\n", (int) percentage);
    }
  else
    {
      percentage = abs (a - b);
//      printf ("SECOND_PERCENTAGE = %d\n", (int) percentage);
    }
//  if (mpq_cmp_ui (x, 0, 1) != 0)
//    {
//      mpq_div (y, y, x);
//      percentage = (100. * mpq_get_d (y));
//      percentage = 100 - percentage;
//    }
//  else
//    {
//      mpq_sub (y, y, x);
//      mpq_abs (y, y);
//      percentage = (int) mpq_get_d (y);
//    }
//  mpq_clear (x);
//  mpq_clear (y);

//  printf ("PERCENTAGE = %d\n", (int) percentage);
  return (int) percentage;
}
///////////////////////////////////////
int
evaluate_interpolation_result_for_kernel (
    poly_params * kernel_poly, FILE*file,
    enum rational_program_param_enum rational_program_case)
{
  int best_n[3], best_b0[3], best_b1[3];
  mpq_t best_clock[3];
  init_best_n_b0_b1_clock (best_n, best_b0, best_b1, best_clock);

//enum kernel_type_enum kernel_type = kernel_type_tmp;

  int n, b0, b1, mcwp_case;
  int n_points = kernel_poly->mcwp_result_table_idx;

  mpq_t clock;
  mpq_init (clock);
  mpq_set_ui (clock, 0, 1);

  int best_idx = 0;
  int current_n = -1;

  mpq_t num, denom;
  mpq_inits (num, denom, NULL);

  mpz_t t0, t1;
  mpz_inits (t0, t1, NULL);
  mpq_t* mpPoint = (mpq_t*) malloc (3 * sizeof(mpq_t));
  for (int i = 0; i < 3; ++i)
    {
      mpq_init (mpPoint[i]);
    }

//finding the point with the minimum clock cycles
//for each category of "n"

  char param_name[64];
  get_kernel_param_name (&param_name, rational_program_case);

  int n_pass = 0;
  printf ("evaluating interpolation of [%s] ... ", param_name);
  for (int i = 0; i < n_points; i++)
    {
      n = kernel_poly->mcwp_result_table[i].n;
      b0 = kernel_poly->mcwp_result_table[i].b0;
      b1 = kernel_poly->mcwp_result_table[i].b1;
      mcwp_case = kernel_poly->mcwp_result_table[i].mcwp_case - 1;
      mcwp_case = 0;

      get_kernel_param_value (clock, rational_program_case,
			      &kernel_poly->mcwp_result_table[i]);

      fprintf (file, "evaluate [n=%5d, b0=%4d, b1=%4d, case=%d][%s] ... \n", n,
	       b0, b1, mcwp_case, param_name);
#if VERBOSE
      fprintf (stdout, "evaluate [n=%5d, b0=%4d, b1=%4d, case=%d][%s] ... \n", n,
	  b0, b1, mcwp_case, param_name);
#endif

      if (kernel_poly->status[mcwp_case] = SUCCESSFUL_INTERP)
	{

//	  printf ("EVAL HERE\n");
	  mpq_set_si (mpPoint[0], n, 1);
	  mpq_set_si (mpPoint[1], b0, 1);
	  mpq_set_si (mpPoint[2], b1, 1);
//	  printf ("NVAR=%d\n", kernel_poly->nvar);
//	  printf ("NUMER\n");
	  evalPolyToVal_AA (kernel_poly->poly[mcwp_case], mpPoint,
			    kernel_poly->nvar, num);
//	  printf ("DENOM\n");
	  evalPolyToVal_AA (kernel_poly->denompoly[mcwp_case], mpPoint,
			    kernel_poly->nvar, denom);

//	  gmp_printf ("num=%f, denom=%f\n", mpq_get_d (num), mpq_get_d (denom));
//	  printf ("AFTER DENOM\n");
	}

      fprintf (file, "[EMULATED=%.4f]...", mpq_get_d (clock));
#if VERBOSE
      fprintf (stdout, "[EMULATED=%.4f]...", mpq_get_d (clock));
#endif

      if (mpq_cmp_ui (denom, 0, 1) != 0)
	{
	  mpq_div (num, num, denom);
	  int error_percentage = get_error_percentage (num, clock);

	  gmp_fprintf (file, "[EVAL=%f] ... ", mpq_get_d (num));
#if VERBOSE
	  gmp_fprintf (stdout, "[EVAL=%f] ... ", mpq_get_d (num));
#endif
	  if (error_percentage <= EVALUATION_TOLERANCE)
	    {
	      fprintf (file, "PASS\n");
        fprintf (file, "ERROR PERCENTAGE=%d >= TOLERANCE (%d) \n",
		       error_percentage, EVALUATION_TOLERANCE);
#if VERBOSE
	      fprintf (stdout, "PASS\n");
#endif
	      n_pass++;
	    }
	  else
	    {
	      fprintf (file, "FAIL\n");
	      fprintf (file, "ERROR PERCENTAGE=%d >= TOLERANCE (%d) \n",
		       error_percentage, EVALUATION_TOLERANCE);
	      fprintf (
		  file,
		  "TRY AGAIN WITH DIFFERENT LOWER BOUND AND UPPER BOUND\n");
	      //	      exit (EXIT_FAILURE);

#if VERBOSE
	      fprintf (stdout, "FAIL\n");
	      fprintf (stdout, "ERROR PERCENTAGE=%d >= TOLERANCE (%d) \n",
		  error_percentage, EVALUATION_TOLERANCE);
	      fprintf (
		  stdout,
		  "TRY AGAIN WITH DIFFERENT LOWER BOUND AND UPPER BOUND\n");
#endif 

	    }
	  fprintf (file, "\n");
	  fprintf (file, short_dashed_line);
#if VERBOSE
	  fprintf (stdout, "\n");
	  fprintf (stdout, short_dashed_line);
#endif
	}
    }
  printf ("[%d out of %d PASS]\n", n_pass, n_points);
  printf (short_dashed_line);

  fflush (file);
}

///////////////////////////////////////
int
init_kernel_poly_interpolator (poly_params * kernel_poly)
{
  for (int case_idx = 0; case_idx < 3; case_idx++)
    {
      kernel_poly->interp[case_idx] = rfInterpInit (
	  kernel_poly->nvar, kernel_poly->degree_bounds,
	  kernel_poly->denomdegree_bounds);

      kernel_poly->poly[case_idx] = NULL;
      kernel_poly->denompoly[case_idx] = NULL;
      kernel_poly->status[case_idx] = INIT;
    }
}
///////////////////////////////////////
int
free_kernel_poly_interpolator (poly_params * kernel_poly)
{
  for (int case_idx = 0; case_idx < 3; case_idx++)
    {
      interpFree (kernel_poly->interp[case_idx]);
      freePolynomial_AA (kernel_poly->poly[case_idx]);
      freePolynomial_AA (kernel_poly->denompoly[case_idx]);
    }
}
///////////////////////////////////////
int
reinit_kernel_poly_interpolator (poly_params * kernel_poly)
{
  free_kernel_poly_interpolator (kernel_poly);
  init_kernel_poly_interpolator (kernel_poly);
}
///////////////////////////////////////

int
interpolate_ec_rf_for_kernel (poly_params * kernel_poly, char *device_name)
{

  int best_n[3], best_b0[3], best_b1[3];
  mpq_t best_clock[3];
  init_best_n_b0_b1_clock (best_n, best_b0, best_b1, best_clock);

  int n, b0, b1, mcwp_case;
  int n_points = kernel_poly->mcwp_result_table_idx;

  mpq_t clock;
  mpq_init (clock);
  mpq_set_ui (clock, 0, 1);

  mpq_t* mpPoint = (mpq_t*) malloc (3 * sizeof(mpq_t));
  for (int i = 0; i < 3; ++i)
    {
      mpq_init (mpPoint[i]);
    }

//adding points to interpolator for all cases
  for (int i = 0; i < n_points; i++)
    {
      n = kernel_poly->mcwp_result_table[i].n;
      b0 = kernel_poly->mcwp_result_table[i].b0;
      b1 = kernel_poly->mcwp_result_table[i].b1;
      mcwp_case = kernel_poly->mcwp_result_table[i].mcwp_case;
      mpq_set (clock, kernel_poly->mcwp_result_table[i].Exec_cycles_app);

//adding current point (n,b0,b1) to interpolator mesh
      mpq_set_si (mpPoint[0], n, 1);
      mpq_set_si (mpPoint[1], b0, 1);
      mpq_set_si (mpPoint[2], b1, 1);

//      printf("adding n=%d, b0=%d, b1=%d to interpolator\n", n, b0, b1);
//      rfInterpAddPointValMP (kernel_poly->interp[mcwp_case - 1], mpPoint,
//			     clock);
      rfInterpAddPointValMP (kernel_poly->interp[mcwp_case - 1], mpPoint,
			     clock);
    }

  PolyInterpStatus_t stat;
  for (int case_idx = 0; case_idx < 3; case_idx++)
    {
      stat = rfInterpGetPoly (kernel_poly->interp[case_idx],
			      &kernel_poly->poly[case_idx],
			      &kernel_poly->denompoly[case_idx], eps);
      if (stat == POLY_INTERP_SUCCESS)
	{

#if VERBOSE 
	  fprintf (stderr, "[INTERPOLATION SUCCESSFUL for case %d!]...\n", case_idx + 1);
#endif
	  kernel_poly->status[case_idx] = SUCCESSFUL_INTERP;
	}
      else if (stat == POLY_INTERP_FAILURE)
	{
#if VERBOSE
	  fprintf (stderr, "[ERROR: Failed to interpolate for case %d!]...\n",
	      case_idx + 1);
#endif
	  kernel_poly->status[case_idx] = FAILED_INTERP;
	}
    }

  for (int i = 0; i < 3; ++i)
    {
      mpq_clear (mpPoint[i]);
    }
  free (mpPoint);

  return 0;
}
/////////////////////////////////////
int
is_zero_poly (AltArr_t** poly, int nvar)
{

  int is_zero = 0;

  mpq_t val;
  mpq_init (val);

  mpq_t mpPoint[3];
  mpq_inits (mpPoint[0], mpPoint[1], mpPoint[2], NULL);
  for (int i = 0; i < 3; i++)
    mpq_set_ui (mpPoint[i], 1, 1);

  evalPolyToVal_AA (poly, mpPoint, nvar, val);

  if (mpq_cmp_ui (val, 0, 1) == 0)
    is_zero = 1;

  for (int i = 0; i < 3; i++)
    mpq_clear (mpPoint[i]);

  mpq_clear (val);

  return is_zero;
}
///////////////////////////////////////

int
interpolate_kernel_param_to_header (
    poly_params * kernel_poly, char *device_name,
    enum rational_program_param_enum rational_program_case)
{
  reinit_kernel_poly_interpolator (kernel_poly);

  int best_n[3], best_b0[3], best_b1[3];
  mpq_t best_clock[3];
  init_best_n_b0_b1_clock (best_n, best_b0, best_b1, best_clock);

  int n, b0, b1;
  int n_points = kernel_poly->mcwp_result_table_idx;

  mpq_t clock;
  mpq_init (clock);
  mpq_set_ui (clock, 0, 1);

  mpq_t* mpPoint = (mpq_t*) malloc (3 * sizeof(mpq_t));
  for (int i = 0; i < 3; ++i)
    {
      mpq_init (mpPoint[i]);
    }

//  int rational_program_case = 0;

  char param_name[64];

  get_kernel_param_name (&param_name, rational_program_case);
  printf ("interpolating for [%s]\n", param_name);

//#if VERBOSE
//#endif

  int mcwp_case = 0;
//adding points to interpolator for all cases
  for (int i = 0; i < n_points; i++)
    {
      n = kernel_poly->mcwp_result_table[i].n;
      b0 = kernel_poly->mcwp_result_table[i].b0;
      b1 = kernel_poly->mcwp_result_table[i].b1;

//adding current point to interpolator
      mpq_set_si (mpPoint[0], n, 1);
      mpq_set_si (mpPoint[1], b0, 1);
      mpq_set_si (mpPoint[2], b1, 1);

      get_kernel_param_value (clock, rational_program_case,
			      &kernel_poly->mcwp_result_table[i]);

//      gmp_printf (
//	  "adding [MWP_Without_BW_full=%f] ... \n",
//	  mpq_get_d (kernel_poly->mcwp_result_table[i].MWP_Without_BW_full));

//      adding current point (n,b0,b1) to interpolator
//      gmp_printf ("adding [n=%Qd, b0=%Qd, b1=%Qd, value=%Qd]\n",
//	       (mpPoint[0]),  (mpPoint[1]),
//	       (mpPoint[2]),  (clock));
      rfInterpAddPointValMP (kernel_poly->interp[mcwp_case], mpPoint, clock);
    }

// kernel_kernelName_param_device.h
  char * prefix = "kernel";
//  char * param_name = "occupancy";
  int len_header_path = strlen (prefix) + strlen (kernel_poly->kernel_name)
      + strlen (param_name) + strlen (device_name) + 10;

  char * header_path = (char*) malloc (len_header_path);
  sprintf (header_path, "%s_%s_%s_%s.h", prefix, kernel_poly->kernel_name,
	   param_name, device_name);

  FILE * header_file = fopen (header_path, "w");

  char * buffer;
  size_t size;
  FILE * file = open_memstream (&buffer, &size);

  fprintf (file, "#ifndef EVAL_%s_%s_H_\n", param_name, device_name);
  fprintf (file, "#define EVAL_%s_%s_H_\n\n", param_name, device_name);
  fprintf (file, "/////////////////////\n");
  fprintf (file, "#include <stdio.h>\n");
  fprintf (file, "#include <stdlib.h>\n");
  fprintf (file, "#include <gmp.h>\n");
  fprintf (
      file,
      "#include \"../../src/NumericalPolySupport/include/interpolator.h\"\n\n");
  fprintf (file, "/////////////////////\n");

  PolyInterpStatus_t stat;
  char var_name[10];
//  for (int case_idx = 0; case_idx < 1; case_idx++)
  int case_idx = mcwp_case;
    {
      stat = rfInterpGetPoly (kernel_poly->interp[case_idx],
			      &kernel_poly->poly[case_idx],
			      &kernel_poly->denompoly[case_idx], eps);

      printf ("building header for rational program ...\n");
      //fprintf(stderr, "STAT=%d\n", stat);
      if (stat == POLY_INTERP_SUCCESS)
	{
	  kernel_poly->status[case_idx] = SUCCESSFUL_INTERP;

	  //should be moved to verbose mode.
#if VERBOSE
	  print_interpolation_result_for_kernel (kernel_poly, stdout);
	  fprintf (stderr, "[INTERPOLATION SUCCESSFUL for [%s]!]...\n", param_name);
#endif
//	  print_to_header (occupancy_header_file, &kernel_poly, case_idx);
	  char *numer_str, *denom_str;

	  fprintf (file, "AltArr_t* get_numer_%s(){\n", param_name);
	  sprintf (var_name, "numer", case_idx + 1);
	  if (is_zero_poly (kernel_poly->poly[mcwp_case], kernel_poly->nvar)
	      == 1)
	    {
	      fprintf (file, "AltArr_t* numer = makePolynomial_AA(1, %d);\n",
		       kernel_poly->nvar);
	      fprintf (file, "mpq_init(numer->elems[0].coef);\n");
	      fprintf (
		  file,
		  "mpz_set_str(mpq_numref(numer->elems[0].coef), \"0\",10);\n");
	      fprintf (
		  file,
		  "mpz_set_str(mpq_denref(numer->elems[0].coef), \"1\",10);\n");
	      fprintf (file, "numer->elems[0].degs=1;\n");
	      fprintf (file, "numer->size=1;\n");
	      fprintf (file, "return numer;\n}\n");
	    }
	  else

	    {
	      numer_str = polyToProgram_AA (kernel_poly->poly[case_idx],
					    var_name);
	      fprintf (file, "%s\n", numer_str);
	      fprintf (file, "return %s;\n}\n", var_name);
	    }

	  fprintf (file, "/////////////////////\n");

	  fprintf (file, "AltArr_t* get_denom_%s (){\n", param_name);

	  if (is_zero_poly (kernel_poly->poly[mcwp_case], kernel_poly->nvar)
	      == 1)
	    {
	      fprintf (file, "AltArr_t* denom = makePolynomial_AA(1, %d);\n",
		       kernel_poly->nvar);
	      fprintf (file, "mpq_init(denom->elems[0].coef);\n");
	      fprintf (
		  file,
		  "mpz_set_str(mpq_numref(denom->elems[0].coef), \"1\",10);\n");
	      fprintf (
		  file,
		  "mpz_set_str(mpq_denref(denom->elems[0].coef), \"1\",10);\n");
	      fprintf (file, "denom->elems[0].degs=1;\n");
	      fprintf (file, "denom->size=1;\n");
	      fprintf (file, "return denom;\n}\n");
	    }
	  else

	    {
	      sprintf (var_name, "denom", case_idx + 1);
	      denom_str = polyToProgram_AA (kernel_poly->denompoly[case_idx],
					    var_name);

	      fprintf (file, "%s\n", denom_str);
	      fprintf (file, "return %s;\n}\n", var_name);
	    }
	  fprintf (file, "/////////////////////\n");
	}
      else if (stat == POLY_INTERP_FAILURE)
	{

	  kernel_poly->status[case_idx] = FAILED_INTERP;
#if VERBOSE
	  fprintf (stderr, "[ERROR: Failed to interpolate for %s!]...\n",
	      param_name);
#endif

	  fprintf (file, "AltArr_t* get_numer_%s (){\n", param_name);
	  fprintf (file, "return NULL;\n}\n");
	  fprintf (file, "/////////////////////\n");

	  fprintf (file, "AltArr_t* get_denom_%s (){\n", param_name);
	  fprintf (file, "return NULL;\n}\n");
	  fprintf (file, "/////////////////////\n");

	}
    }

  fprintf (file, "/////////////////////\n");
  fprintf (file, "#endif\n");
  fflush (file);
  fprintf (header_file, "%s", buffer);
//#if VERBOSE
  fprintf (stderr, "written to [%s]\n", header_path);
//#endif

  printf (short_dashed_line);

  fclose (file);
  fclose (header_file);

  free (header_path);

  for (int i = 0; i < 3; ++i)
    {
      mpq_clear (mpPoint[i]);
    }
  free (mpPoint);

  return 0;
}

///////////////////////////////////////
int
init_kernel_poly_params (poly_params *kernel_poly_params, char ** kernel_names,
			 int * degree_bounds, int * denomdegree_bounds,
			 enum kernel_type_enum kernel_type, int nvar,
			 int n_kernels)
{

  char* syms[] =
    { "N", "B0", "B1" };
  int n_symbols = sizeof(syms) / sizeof(syms[0]);
  for (int k = 0; k < n_kernels; k++)
    {

//      init kernel_name
      int len = strlen (kernel_names[k]) + 1;
      kernel_poly_params[k].kernel_name = (char*) malloc (len);
      memcpy (kernel_poly_params[k].kernel_name, kernel_names[k], len);

//init kernel symbols
      kernel_poly_params[k].symbols = (char**) malloc (n_symbols);
      for (int i = 0; i < n_symbols; i++)
	{
	  kernel_poly_params[k].symbols[i] = (char*) malloc (
	      strlen (syms[i]) + 1);
//	  memcpy (kernel_poly_params[k].symbols[i], syms[i], strlen (syms[i]));
	  sprintf (kernel_poly_params[k].symbols[i], "%s", syms[i]);
	}

//init nvar, deg bounds, denom deg bounds
      kernel_poly_params[k].nvar = nvar;
      kernel_poly_params[k].degree_bounds = (int*) malloc (
	  sizeof(int) * kernel_poly_params[k].nvar);
      kernel_poly_params[k].denomdegree_bounds = (int*) malloc (
	  sizeof(int) * kernel_poly_params[k].nvar);

      for (int i = 0; i < kernel_poly_params[k].nvar; ++i)
	{
	  kernel_poly_params[k].degree_bounds[i] = degree_bounds[i];
	  kernel_poly_params[k].denomdegree_bounds[i] = denomdegree_bounds[i];
	}

      init_kernel_poly_interpolator (&kernel_poly_params[k]);

//init kernel type
      switch (kernel_type)
	{
	case OneDimKernel:
	  kernel_poly_params[k].point = (&global_p1k1_list);
	  break;

	case TwoDimKernel:
	  kernel_poly_params[k].point = (&global_p2k2_list);
	  break;
	}

//init table for storing mcwp results; 
      kernel_poly_params[k].mesh_size = get_n_kernel_mesh_points (
	  kernel_poly_params[k].point);
      kernel_poly_params[k].mcwp_result_table = (mcwp_result_params_t*) malloc (
	  kernel_poly_params[k].mesh_size * sizeof(mcwp_result_params_t));
      kernel_poly_params[k].mcwp_result_table_idx = 0;
#if VERBOSE
      printf("mcwp_result_table of size [%d] alloccated for kernel [%d]\n", kernel_poly_params[k].mesh_size, k);
#endif
    }

#if VERBOSE
  printf("Init interpolation done.\n");
#endif

}

typedef struct 
{
    int idx, n, b0, b1;
    double execution_time;
} exec_cycles;

// Comparison function for qsort for exec_cycles_app_array execution_time values
int compare_exec_cycles (const void * a, const void * b) {
  exec_cycles *exec_cycles_a = (exec_cycles *) a;
  exec_cycles *exec_cycles_b = (exec_cycles *) b;

  if (exec_cycles_a->execution_time < exec_cycles_b->execution_time) return -1;
  else if (exec_cycles_a->execution_time > exec_cycles_b->execution_time) return 1;
  else return 0;
}

// Comparison function for qsort for mcwp_result_params_t values
int compare_mcwp_result_params_t (const void * a, const void * b) {
  mcwp_result_params_t *mcwp_result_params_a = (mcwp_result_params_t *) a;
  mcwp_result_params_t *mcwp_result_params_b = (mcwp_result_params_t *) b;


  // use mpq_cmp to compare mpq_t values
  return mpq_cmp(mcwp_result_params_a->Exec_cycles_app, mcwp_result_params_b->Exec_cycles_app);
}

#define NOT_FOUND -1

// Finds the last occurrence of the given number in the given sorted array.
// Returns the index of the last occurrence if the number is found, and NOT_FOUND if it is not found.
int binarySearchLast(exec_cycles arr[], int size, int num) {
  int left = 0;
  int right = size - 1;
  int last = NOT_FOUND;
  while (left <= right) {
    int mid = left + (right - left) / 2;
    if (arr[mid].n < num) {
      left = mid + 1;
    } else if (arr[mid].n > num) {
      right = mid - 1;
    } else {
      last = mid;
      left = mid + 1;  // search the right half
    }
  }
  return last;
}

// Finds the first occurrence of the given number in the given sorted array.
// Returns the index of the first occurrence if the number is found, and NOT_FOUND if it is not found.
int binarySearchFirst(exec_cycles arr[], int size, int num) {
  int left = 0;
  int right = size - 1;
  int first = NOT_FOUND;
  while (left <= right) {
    int mid = left + (right - left) / 2;
    if (arr[mid].n < num) {
      left = mid + 1;
    } else if (arr[mid].n > num) {
      right = mid - 1;
    } else {
      first = mid;
      right = mid - 1;  // search the left half
    }
  }
  return first;
}

typedef struct 
{
    int n;
} qf_outlier;

// Program quartile fence algorithm to find the quartile fence of mwcp_result_params_t Exec_cycles_app 
// for each point in the mesh and remove upper outliers
void quartile_fence(poly_params *kernel_poly_params, int n_kernels, int n_lower_bound, int n_upper_bound, 
                    char* device_name, qf_outlier* num_outliers) {

  mcwp_result_params_t* mcwp_result_table;
  int mesh_size;
  int mcwp_result_table_idx;

  // print n_kernels
  printf("n_kernels is [%d]\n", n_kernels);

  for (int k = 0; k < n_kernels; k++) {
    
    mcwp_result_table = kernel_poly_params[k].mcwp_result_table;
    mesh_size = kernel_poly_params[k].mesh_size;
    mcwp_result_table_idx = kernel_poly_params[k].mcwp_result_table_idx;

    if (mcwp_result_table_idx == 0) {
      printf("quartile_fence: mcwp_result_table_idx is zero; nothing to do for kernel [%d]",k);
      continue;
    }

    // initialize an array of doubles to store the Exec_cycles_app values size is mcwp_result_table_idx
    exec_cycles exec_cycles_app_array[mcwp_result_table_idx];

    // copy the Exec_cycles_app, n, b0, b1 values from the mcwp_result_table to the exec_cycles_app_array
    for (unsigned int i = 0; i < mcwp_result_table_idx; i++) {
        exec_cycles_app_array[i].idx = i;
        exec_cycles_app_array[i].n = mcwp_result_table[i].n;
        exec_cycles_app_array[i].b0 = mcwp_result_table[i].b0;
        exec_cycles_app_array[i].b1 = mcwp_result_table[i].b1;
        exec_cycles_app_array[i].execution_time = mpq_get_d (mcwp_result_table[i].Exec_cycles_app);
    }

    // initialize the start and end indices array where the length is log2(n_upper_bound) - log2(n_lower_bound) + 1 * 2
    int start_indices[(int) (log2(n_upper_bound) - log2(n_lower_bound) + 1) * 2];

    // write a for loop that bit shifts n_lower_bound left by 1 up to n_upper_bound
    int n, m;
    for (n = n_lower_bound, m = 0; n <= n_upper_bound; n<<=1, m+=2) {

        // print n
        printf("n is [%d]\n", n);
        int first = binarySearchFirst(exec_cycles_app_array, mcwp_result_table_idx, n);
        int last = binarySearchLast(exec_cycles_app_array, mcwp_result_table_idx, n);

        if (first == NOT_FOUND || last == NOT_FOUND) {
            continue;
        } else {
            start_indices[m] = first;
            start_indices[m+1] = last;
        }

        // print first and last
        printf("first is [%d] and last is [%d]\n", first, last);
    }

    // print outliers to "kernel_kernel_poly_params.kernel_name_outliers_device_name.txt"
    FILE *fp;
    char filename[100];
    sprintf(filename, "kernel_%s_outliers_%s.txt", kernel_poly_params[k].kernel_name, device_name);
    fp = fopen(filename, "w");
    num_outliers[k].n = 0;
    // sort the exec_cycles_app_array based on start_indices
    for (int i = 0; i < (int) (log2(n_upper_bound) - log2(n_lower_bound) + 1) * 2; i+=2) {

        // print i
        printf("i is [%d]\n", i);

        int start = start_indices[i];
        int end = start_indices[i+1];
        int size = end - start + 1;
        qsort(exec_cycles_app_array + start, size, sizeof(exec_cycles), compare_exec_cycles);


    #if VERBOSE 
        // print the sorted list
        printf("\nAfter sorting the list is: \n");

        for( int j = start ; j < end+1; j++ ) {   
            printf("i:%d idx:%d n:%d b0:%d b1:%d val:%.4f\n", j, exec_cycles_app_array[j].idx, exec_cycles_app_array[j].n, exec_cycles_app_array[j].b0,exec_cycles_app_array[j].b1, exec_cycles_app_array[j].execution_time);
        }
    #endif

        int x = (int) (size * 0.25);

        // find the 1st quartile fence
        int q1 = x + start; 
        int q3 = end - x;

    #if VERBOSE >=2
        // print q1 and q3
        printf("quartile_fence: q1 is [%d] and q3 is [%d]\n", q1, q3);

        // print q1 and q3 values
        printf("quartile_fence: q1 value is [%f] and q3 value is [%f]\n", exec_cycles_app_array[q1].execution_time, exec_cycles_app_array[q3].execution_time);
    #endif

        double iqr = exec_cycles_app_array[q3].execution_time - exec_cycles_app_array[q1].execution_time;

        // find upper outer fence
        double upper_fence = exec_cycles_app_array[q3].execution_time + (1.5f * iqr);

    #if VERBOSE >=2
        //print upper fence
        printf("quartile_fence: upper_fence for kernel [%d] is [%f]\n", k, upper_fence);
    #endif

        // iterate through the exec_cycles_app_array and append the outliers to the file
        for (int i = start; i < end+1; i++) {
            if (exec_cycles_app_array[i].execution_time > upper_fence) {
                fprintf(fp, "%d %d %d %f\n", exec_cycles_app_array[i].n, exec_cycles_app_array[i].b0, exec_cycles_app_array[i].b1, 
                        exec_cycles_app_array[i].execution_time);
                num_outliers[k].n++;
            }
        }
    }
    fclose(fp);
  }
}

bool in(triple arr[], int size, triple num) {
  for (int i = 0; i < size; i++) {
    if (arr[i].N[0] == num.N[0] && arr[i].N[1] == num.N[1] && arr[i].N[2] == num.N[2]) {
      return true;
    }
  }
  return false;
}

// read in outliers file update kernel_poly_params
void read_outliers (poly_params *kernel_poly_params, int n_kernels, char* device_name, qf_outlier* num_outliers)
{
    for (int k = 0; k < n_kernels; k++) {
        // read in outliers file
        FILE *fp;
        char filename[100];
        sprintf(filename, "kernel_%s_outliers_%s.txt", kernel_poly_params[k].kernel_name, device_name);
        fp = fopen(filename, "r");

        // iterate through the outliers file and add outliers to a triple array
        int n, b0, b1; 
        double exec_time;
        triple outliers[num_outliers[k].n];

        mcwp_result_params_t* mcwp_result_table;
        int mcwp_result_table_idx;

        int i = 0;

        while (fscanf(fp, "%d %d %d %lf", &n, &b0, &b1, &exec_time) != EOF) {
            outliers[i].N[0] = n;
            outliers[i].N[1] = b0;
            outliers[i].N[2] = b1;
            i++;
        }

#if VERBOSE >=2
    // iterate through the outliers array and print the outliers
    for (int i = 0; i < num_outliers[k].n; i++) {
        printf("outlier: n:%d b0:%d b1:%d\n", outliers[i].N[0], outliers[i].N[1], outliers[i].N[2]);
    }
#endif
      
        mcwp_result_table = kernel_poly_params[k].mcwp_result_table;
        mcwp_result_table_idx = kernel_poly_params[k].mcwp_result_table_idx;

        int j = 0;
        for (int i = 0; i < mcwp_result_table_idx; i++) {
            triple p;
            p.N[0] = mcwp_result_table[i].n;
            p.N[1] = mcwp_result_table[i].b0;
            p.N[2] = mcwp_result_table[i].b1;
            if (!in(outliers, num_outliers[k].n, p)) {
                kernel_poly_params[k].mcwp_result_table[j] = mcwp_result_table[i];
                j++;
            } 
#if VERBOSE >=2
            else {
                printf("removing outlier: n:%d b0:%d b1:%d\n", mcwp_result_table[i].n, mcwp_result_table[i].b0, mcwp_result_table[i].b1);
            }
#endif
        }
        kernel_poly_params[k].mcwp_result_table_idx = j;
    }

#if VERBOSE >= 2
    // iterate through the kernel_poly_params and print the mcwp_result_table
    for (int k = 0; k < n_kernels; k++) {
        for (int j = 0; j < kernel_poly_params[k].mcwp_result_table_idx; j++) {
            printf("kernel_poly_params[%d].mcwp_result_table[%d]: n:%d b0:%d b1:%d\n", k, j, 
                    kernel_poly_params[k].mcwp_result_table->n, kernel_poly_params[k].mcwp_result_table->b0, 
                    kernel_poly_params[k].mcwp_result_table->b1);
        }
    }
#endif
}

///////////////////////////////////////
//int
//copy_kernel_poly_params (poly_params *kernel_poly_params, poly_params *src_kernel_poly_params)
//{
//
//  char* syms[] =
//    { "N", "B0", "B1" };
//  int n_symbols = sizeof(syms) / sizeof(syms[0]);
//
//
////  int nvar;
////  int* degree_bounds;
////  int* denomdegree_bounds;
////  Interpolator_t* interp[3];
////  triple* point;
////  AltArr_t* poly[3];
////  AltArr_t* denompoly[3];
////  enum poly_params_status status[3];
////  int mesh_size;
////  //  int is_zero_poly[3];
////  int mcwp_result_table_idx;
////  mcwp_result_params_t * mcwp_result_table;
////  char * kernel_name;
////  char ** symbols;
//
//  kernel_poly_params->kernel_name=(char*)malloc(strlen(kernel_poly_params->kernel_name));
//  sprintf(kernel_poly_params->kernel_name, "%s",)
//  int k=0;
////      init kernel_name
//      int len = strlen (kernel_names[k]) + 1;
//      kernel_poly_params[k].kernel_name = (char*) malloc (len);
//      memcpy (kernel_poly_params[k].kernel_name, kernel_names[k], len);
//
////init kernel symbols
//      kernel_poly_params[k].symbols = (char**) malloc (n_symbols);
//      for (int i = 0; i < n_symbols; i++)
//	{
//	  kernel_poly_params[k].symbols[i] = (char*) malloc (
//	      strlen (syms[i]) + 1);
////	  memcpy (kernel_poly_params[k].symbols[i], syms[i], strlen (syms[i]));
//	  sprintf (kernel_poly_params[k].symbols[i], "%s", syms[i]);
//	}
//
////init nvar, deg bounds, denom deg bounds
//      kernel_poly_params[k].nvar = nvar;
//      kernel_poly_params[k].degree_bounds = (int*) malloc (
//	  sizeof(int) * kernel_poly_params[k].nvar);
//      kernel_poly_params[k].denomdegree_bounds = (int*) malloc (
//	  sizeof(int) * kernel_poly_params[k].nvar);
//
//      for (int i = 0; i < kernel_poly_params[k].nvar; ++i)
//	{
//	  kernel_poly_params[k].degree_bounds[i] = degree_bounds[i];
//	  kernel_poly_params[k].denomdegree_bounds[i] = denomdegree_bounds[i];
//	}
//
//      init_kernel_poly_interpolator (&kernel_poly_params[k]);
//
////init kernel type
//      switch (kernel_type)
//	{
//	case OneDimKernel:
//	  kernel_poly_params[k].point = (&global_p1k1_list);
//	  break;
//
//	case TwoDimKernel:
//	  kernel_poly_params[k].point = (&global_p2k2_list);
//	  break;
//	}
//
////init table for storing mcwp results;
//      kernel_poly_params[k].mesh_size = get_n_kernel_mesh_points (
//	  kernel_poly_params[k].point);
//      kernel_poly_params[k].mcwp_result_table = (mcwp_result_params_t*) malloc (
//	  kernel_poly_params[k].mesh_size * sizeof(mcwp_result_params_t));
//      kernel_poly_params[k].mcwp_result_table_idx = 0;
//#if VERBOSE
//      printf("mcwp_result_table of size [%d] alloccated for kernel [%d]\n", kernel_poly_params[k].mesh_size, k);
//#endif
//    }
//
//#if VERBOSE
//  printf("Init interpolation done.\n");
//#endif
//
//}

///////////////////////////////////////
void
print_nvar_to_file (char **kernel_names, int n_kernels, int nvar)
{
  FILE * file;
  char file_path[256];
  for (int i = 0; i < n_kernels; i++)
    {
      sprintf (file_path, "kernel_%s.nvar", kernel_names[i]);
      file = fopen (file_path, "w");
      fprintf (file, "%d", nvar);
      fclose (file);
    }
}

///////////////////////////////////////
void
do_emulation (poly_params* kernel_poly_params, int n_kernels, char* device_name,
	      char* mem_inst_trace_str, int nvar, char* device_params_path,
	      enum kernel_type_enum kernel_type, int n_lower_bound,
	      int n_upper_bound, int
	      (*rep_div_n) (int))
{
  //  printf ("\ndevice_params_path=[%s]\n", device_params_path);
  //  printf ("\ndevice_name=[%s]\n", device_name);

  /////////////////////////////////////
  char** kernel_names;
  init_kernel_names (&kernel_names, n_kernels);
  print_nvar_to_file (kernel_names, n_kernels, nvar);

  /////////////////////////////////////
  device_params_t device_params;
  read_params_from_file (device_params_path, &device_params, N_DEVICE_PARAMS,
			 global_device_params_list);

  /////////////////////////////////////
  // ToDo: can this part be removed?
  FILE * n_sm_file = fopen ("device_n_sm.tmp", "w");
  fprintf (n_sm_file, "%d", device_params.Active_SMs);
  fclose (n_sm_file);

  ///////////////////////////////////////
  const char prefix_kernel[] = "kernel_";
  const char suffix_params[] = "_params.tmp";
  const char suffix_trace[] = "_trace.txt";
  const char suffix_trace_avg[] = "_trace.avg.txt";
  const char suffix_ptx_params[] = "_ptx_params.tmp";

  ///////////////////////////////////////
  char** param_file_path_list;
  init_output_path_list (&param_file_path_list, kernel_names, prefix_kernel,
			 suffix_params, n_kernels);

  char** ptx_param_file_path_list;
  init_output_path_list (&ptx_param_file_path_list, kernel_names, prefix_kernel,
			 suffix_ptx_params, n_kernels);

  char** trace_file_path_list;
  init_output_path_list (&trace_file_path_list, kernel_names, prefix_kernel,
			 suffix_trace, n_kernels);

  char** avg_file_path_list;
  init_output_path_list (&avg_file_path_list, kernel_names, prefix_kernel,
			 suffix_trace_avg, n_kernels);

  ///////////////////////////////////////
  kernel_params_t kernel_params;
  trace_params_t trace_params;
  ptx_params_t ptx_params;
  char mem_inst_trace[1024] = "";

  ///////////////////////////////////////
  mcwp_result_params_t current_result;
  init_mcwp_result_params (&current_result);

  ///////////////////////////////////////
  remove_trace_log_file ();
//running the emulation and collecting mcwp results.
  while (kernel_poly_params[0].point != NULL)
    {
      //      int success = 0;
      int current_n = kernel_poly_params[0].point->N[0];
      //      if (current_n >= 256)
      //	break;

      //      printf("current_n=%d\n", current_n);
      if (current_n < n_lower_bound || current_n > n_upper_bound)
	{

	  for (int k = 0; k < n_kernels; k++)
	    {
	      kernel_poly_params[k].point = get_next_kernel_mesh_point (
		  kernel_poly_params[k].point);
	    }
	  continue;
	}

      //      while (success < 2 * n_kernels && kernel_poly_params[0].point != NULL
      //	  && kernel_poly_params[0].point->N[0] == current_n)
      while (kernel_poly_params[0].point != NULL
	  && kernel_poly_params[0].point->N[0] == current_n)
	{

	  int n, b0, b1;
	  for (int j = 0; j < n_kernels; j++)
	    {
	      n = kernel_poly_params[j].point->N[0];
	      b0 = kernel_poly_params[j].point->N[1];
	      b1 = kernel_poly_params[j].point->N[2];
	      //#if VERBOSE
	      //	      printf ("=================================\n");

//	      printf ("checking [N=%4d, B0=%4d, B1=%4d] "
//		      "for kernel [%s (%d)]\n",
//		      n, b0, b1, kernel_poly_params[j].kernel_name, j);
//	      printf (long_dashed_line);
	      //#endif
	    }
	  printf ("checking [N=%4d, B0=%4d, B1=%4d]\n", n, b0, b1);
	  printf (long_dashed_line);

	  //calling ocelot tracer to generate stats for each kernel
	  //enumerated in kernel_name_list.tmp in the current directory.
	  sprintf (mem_inst_trace, mem_inst_trace_str, n, b0, b1);
	  //		int status = system(mem_inst_trace);

	  int status = exec_cmd (mem_inst_trace);
	  if (status == 1)
	    {
	      printf (
		  "ERROR: FAILED calling [mem_inst_trace.bin] (return value >1) \n");
	      printf ("Continue!\n");
	      printf ("=================================\n");
	      //			kernel_poly.point++;
	      for (int k = 0; k < n_kernels; k++)
		{
		  kernel_poly_params[k].point = get_next_kernel_mesh_point (
		      kernel_poly_params[k].point);
		}
	      continue;
	    }
	  if (status > 2)
	    {
	      printf ("ERROR: calling mem_inst_trace.bin -> FAILED!\n");
	      exit (EXIT_FAILURE);
	    }
#if VERBOSE >=2
	  printf("calling mem_inst_trace.bin -> done!\n");
#endif

	  //1. read trace and trace_avg results + remove the files
	  //2. compute mcwp
	  //3.
	  for (int j = 0; j < n_kernels; j++)
	    {
#if VERBOSE >=2
	      printf ("reading file [%s] ... \n", avg_file_path_list[j]);
#endif
//	      exit(0);
	      if (read_params_from_file (avg_file_path_list[j], &trace_params,
					 N_TRACE_PARAMS,
					 global_trace_params_list)
		  != EXIT_SUCCESS)
		{
		  printf ("kernel has not been traced; continue\n");
		  kernel_poly_params[j].point = get_next_kernel_mesh_point (
		      kernel_poly_params[j].point);
		  continue;
		}

	      read_kernel_params_from_file (&kernel_params,
					    param_file_path_list[j]);

	      //	      int n_blocks = kernel_params.blocks_per_grid;
	      //	      int threads_per_block = kernel_params.threads_per_block;
//	      remove (trace_file_path_list[j]);
//	      remove (avg_file_path_list[j]);
#if VERBOSE >=2
	      printf("removing trace file [%s] -> done!\n", trace_file_path_list[j]);
#endif

	      //alternatively, n_blocks = program_params[9] (provided by tracer);
	      device_params.Mem_LD = read_mem_ld (n, kernel_type, b0 * b1,
						  device_name);
	      //	  printf ("mem_ld=%d\n", device_params.Mem_LD);
	      read_ptx_params_from_file (&ptx_params,
					 ptx_param_file_path_list[j]);

	      mwp_cwp (&trace_params, &device_params, &kernel_params,
		       &ptx_params, (*rep_div_n) (n), &current_result);
//	      mwp_cwp (&trace_params, &device_params, &kernel_params,
//		       &ptx_params, 1, &current_result);
	      current_result.n = n;
	      current_result.b0 = b0;
	      current_result.b1 = b1;
	      int idx = add_mcwp_result_to_table (&current_result,
						  &kernel_poly_params[j]);
	      //	      printf("received_idx=%d\n", idx);
	      //	      printf("curren_table_idx++=%d\n", kernel_poly_params[j].mcwp_result_table_idx);
	      kernel_poly_params[j].point = get_next_kernel_mesh_point (
		  kernel_poly_params[j].point);
	    }
	}

	
    }
}
///////////////////////////////////////
#define MAX_EXAMPLE_NAME_SIZE  128
///////////////////////////////////////
void
parse_table_from_file (poly_params * kernel_poly_params, int n_kernels,
		       char *device_name, int nvar, char * device_params_path,
		       enum kernel_type_enum kernel_type, int n_lower_bound,
		       int n_upper_bound)
{

  char cwd[PATH_MAX];

  getcwd (cwd, sizeof(cwd));
  printf ("cwd=%s\n", cwd);
  int n = strlen (cwd);

  int cutoff_idx = -1;
  for (int i = n - 1; i >= 0; i--)
    if (cwd[i] == '/')
      {
	cutoff_idx = i + 1;
	break;
      }

  //ToDo: set a limit on max length of example name e.g. "VectorAddition"
  char example[MAX_EXAMPLE_NAME_SIZE] = "";
  for (int i = cutoff_idx; i < n; i++)
    sprintf (example + strlen (example), "%c", cwd[i]);

  /////////////////////////////////////

  //python ../../mcwp/parse_results.py [example] [device_name]
  //output: ../results/[example]/kernel_[kernel_name]_table_[device_name].txt

  char cmd_prefix[128] = "python ../../src/mcwp/parse_results.py >/dev/null 2>&1";
  char cmd[2048];
  char output_prefix[256] = "";
  char output_path[2048];

  sprintf (cmd, "%s %s %s", cmd_prefix, example, device_name);
  sprintf (output_prefix, "../results/%s", example);
#if VERBOSE>=2
  printf ("cmd=[%s]\n", cmd);
  printf("output_prefix=[%s]\n", output_prefix);
#endif 
  system (cmd);

  /////////////////////////////////////
  FILE * tmp_file;
  for (int i = 0; i < n_kernels; i++)
    {
      sprintf (output_path, "%s/kernel_%s_table_%s.txt", output_prefix,
	       kernel_poly_params[i].kernel_name, device_name);
      printf ("path=[%s]\n", output_path);
      tmp_file = fopen (output_path, "r");
      load_mcwp_result_params (&kernel_poly_params[i], tmp_file, n_lower_bound,
			       n_upper_bound);
    }
}

///////////////////////////////////////
poly_params**
test_mwp_cwp (int argc, char **argv, int n_kernels_in, int nvar_in,
	      int* degree_bounds_in, int* denomdegree_bounds_in,
	      char* mem_inst_trace_str, enum kernel_type_enum kernel_type, int
	      (*rep_div_n) (int))
{

  /////////////////////////////////////
  // setting the default n_lower_bound and n_upper_bound
  int n_lower_bound, n_upper_bound;
  switch (kernel_type)
    {
    case OneDimKernel:
      n_lower_bound = 2048;
      n_upper_bound = 16384;

      break;

    case TwoDimKernel:
      n_lower_bound = 32; // can be changed to 128
      n_upper_bound = 256;
      break;
    }

  /////////////////////////////////////
  //interpolating for three variables (n, b0, b1)
  //setting degree bounds to 3, or 5, or 7
  int nvar = 3;
  int *degree_bounds = (int*) malloc (sizeof(int) * nvar);
  int *denomdegree_bounds = (int*) malloc (sizeof(int) * nvar);

  for (int i = 0; i < nvar; ++i)
    {
      degree_bounds[i] = DEGREE_BOUND;
      denomdegree_bounds[i] = DEGREE_BOUND;
    }
  /////////////////////////////////////

//path of the file including device parameters
  char device_params_path[1024] = "";

  //mode {0: do emulation, 1: load from table}
  int mode = 0;

  if (argc == 1)
    {
      printf (short_dashed_line);
      printf (""
	      "args: \n"
	      " [1]:device_profile\n"
	      " [2]:mode {0:emulate (default), 1:load from file}\n"
	      " [3]:low_n [4]:upper_n\n");
      printf (short_dashed_line);
      exit (EXIT_FAILURE);
    }

  sprintf (device_params_path, "%s", argv[1]);

  if (argc > 2)
    mode = atoi (argv[2]);
  if (argc > 3)
    n_lower_bound = atoi (argv[3]);
  if (argc > 4)
    n_upper_bound = atoi (argv[4]);

  fprintf(stderr, "%d %d \n", n_lower_bound, n_upper_bound);  

  /////////////////////////////////////
  char device_name[32];
  sprintf (device_name, "%s", get_absolute_device_name (device_params_path));

  /////////////////////////////////////
  int n_kernels = get_num_lines_in_file ("kernel_name_list.tmp");
#if VERBOSE 
  printf("[number of kernels: %d]\n", n_kernels);
#endif
  /////////////////////////////////////
  char** kernel_names;
  init_kernel_names (&kernel_names, n_kernels);
  /////////////////////////////////////
  poly_params* kernel_poly_params = (poly_params*) malloc (
      n_kernels * sizeof(poly_params));
  init_kernel_poly_params (kernel_poly_params, kernel_names, degree_bounds,
			   denomdegree_bounds, kernel_type, nvar, n_kernels);

  /////////////////////////////////////
  // ToDo: can this part be removed?
  FILE * current_device_name = fopen ("current_device.tmp", "w");
  fprintf (current_device_name, "%s", device_name);
  fclose (current_device_name);

  /////////////////////////////////////
  if (mode == 0)
    do_emulation (kernel_poly_params, n_kernels, device_name,
		  mem_inst_trace_str, nvar, device_params_path, kernel_type,
		  n_lower_bound, n_upper_bound, (rep_div_n));

  /////////////////////////////////////
  if (mode == 1)
    parse_table_from_file (kernel_poly_params, n_kernels, device_name, nvar,
			   device_params_path, kernel_type, n_lower_bound,
			   n_upper_bound);
  
  qf_outlier num_outliers[n_kernels];
  quartile_fence(kernel_poly_params, n_kernels, n_lower_bound, n_upper_bound, device_name, num_outliers);

  // read in outliers file and remove outliers from kernel_poly_params
  read_outliers(kernel_poly_params, n_kernels, device_name, num_outliers);

  /////////////////////////////////////
  do_interpolation (kernel_poly_params, n_kernels, device_name, mode);

  /////////////////////////////////////
//    for(int j=0;j<n_kernels;j++)
//  {
//    free_kernel_poly_params(&kernel_poly_params[j]);
//  }

  return kernel_poly_params;
}
///////////////////////////////////////
poly_params*
do_interpolation (poly_params * kernel_poly_params, int n_kernels,
		  char *device_name, int mode)
{

//printf (short_dashed_line);
  printf ("[INTERPOLATING MCWP QUANTITIES ...]\n");
  printf (short_dashed_line);
///////////////////////////////////////
  char** kernel_names;
  init_kernel_names (&kernel_names, n_kernels);
//
  const char prefix_kernel[] = "kernel_";
  char suffix_results[64] = "_results";
  sprintf (suffix_results, "%s_%s.txt", suffix_results, device_name);

//#if ALLVARIABLE
//  const char suffix_interpolation[64] = "_interpolation_withn";
//#else
  const char suffix_interpolation[64] = "_interpolation";
//#endif
  sprintf (suffix_interpolation, "%s_%s.tmp", suffix_interpolation,
	   device_name);

  char** result_file_path_list;
  char** interpolation_file_path_list;

  init_output_path_list (&result_file_path_list, kernel_names, prefix_kernel,
			 suffix_results, n_kernels);
  init_output_path_list (&interpolation_file_path_list, kernel_names,
			 prefix_kernel, suffix_interpolation, n_kernels);

  FILE * mcwp_result;
  FILE * interpolation_result;
  FILE * evaluation_result = fopen ("evaluation_result.tmp", "w");
  FILE * ec_eval_file = fopen ("ec_evaluation_result.tmp", "w");

  for (int j = 0; j < n_kernels; j++)
    {

      ///////////////////////
      find_best_results (&(kernel_poly_params[j]));
      ///////////////////////
      interpolation_result = fopen (interpolation_file_path_list[j], "w");

      if (mode == 0)
	{
	  mcwp_result = fopen (result_file_path_list[j], "w");
      fprintf(stderr, "%s\n", result_file_path_list[j]);
	  print_mcwp_result_table (&(kernel_poly_params[j]), mcwp_result);
    //  print_mcwp_result_table (&(kernel_poly_params[j]), stdout);
	  fclose (mcwp_result);
	}

      ///////////////////////
//      interpolate_ec_rf_for_kernel (&(kernel_poly_params[j]), device_name);
      //evaluate_interpolation_result_for_kernel (&(kernel_poly_params[j]),
      //			ec_eval_file, Exec_cycles_app);
//      print_interpolation_result_for_kernel (&(kernel_poly_params[j]),
//					     interpolation_result);
      fclose (interpolation_result);
//      print_interpolation_result_for_kernel (&(kernel_poly_params[j]), stdout);
      ///////////////////////
//      reinit_kernel_poly_interpolator (&(kernel_poly_params[j]));
//      interpolate_kernel_param_to_header (&(kernel_poly_params[j]), device_name,
//					  Exec_cycles_app);
      //evaluate_interpolation_result_for_kernel (&(kernel_poly_params[j]),
      //			evaluation_result,
      //			Exec_cycles_app);

      ///////////////////////
//      reinit_kernel_poly_interpolator (&(kernel_poly_params[j]));
//      interpolate_kernel_param_to_header (&(kernel_poly_params[j]), device_name,
//					  MWP_Without_BW_full);
//      evaluate_interpolation_result_for_kernel (&(kernel_poly_params[j]),
//						evaluation_result,
//						MWP_Without_BW_full);
      ///////////////////////
//      reinit_kernel_poly_interpolator (&(kernel_poly_params[j]));
//      interpolate_kernel_param_to_header (&(kernel_poly_params[j]), device_name,
//					  Active_warps_per_SM);
//      evaluate_interpolation_result_for_kernel (&(kernel_poly_params[j]),
//						evaluation_result,
//						Active_warps_per_SM);

      ///////////////////////
//      reinit_kernel_poly_interpolator (&(kernel_poly_params[j]));
//      interpolate_kernel_param_to_header (&(kernel_poly_params[j]), device_name,
//					  MWP_peak_BW);
//      evaluate_interpolation_result_for_kernel (&(kernel_poly_params[j]),
//						evaluation_result, MWP_peak_BW);

      ///////////////////////
//      reinit_kernel_poly_interpolator (&(kernel_poly_params[j]));
//      interpolate_kernel_param_to_header (&(kernel_poly_params[j]), device_name,
//					  CWP_full);
//      evaluate_interpolation_result_for_kernel (&(kernel_poly_params[j]),
//						evaluation_result, CWP_full);

      ///////////////////////
//      reinit_kernel_poly_interpolator (&(kernel_poly_params[j]));
      interpolate_kernel_param_to_header (&(kernel_poly_params[j]), device_name,
					  Mem_LD);
      evaluate_interpolation_result_for_kernel (&(kernel_poly_params[j]),
						evaluation_result, Mem_LD);

      ///////////////////////
//      reinit_kernel_poly_interpolator (&(kernel_poly_params[j]));
      interpolate_kernel_param_to_header (&(kernel_poly_params[j]), device_name,
					  shared_mem_bytes_total);
      evaluate_interpolation_result_for_kernel (&(kernel_poly_params[j]),
						evaluation_result,
						shared_mem_bytes_total);

      ///////////////////////
//      reinit_kernel_poly_interpolator (&(kernel_poly_params[j]));
      interpolate_kernel_param_to_header (&(kernel_poly_params[j]), device_name,
					  Blocks);
      evaluate_interpolation_result_for_kernel (&(kernel_poly_params[j]),
						evaluation_result, Blocks);

      ///////////////////////
//      reinit_kernel_poly_interpolator (&(kernel_poly_params[j]));
      interpolate_kernel_param_to_header (&(kernel_poly_params[j]), device_name,
					  Comp_insts);
      evaluate_interpolation_result_for_kernel (&(kernel_poly_params[j]),
						evaluation_result, Comp_insts);

      ///////////////////////
//      reinit_kernel_poly_interpolator (&(kernel_poly_params[j]));
      interpolate_kernel_param_to_header (&(kernel_poly_params[j]), device_name,
					  Uncoal_Mem_insts);
      evaluate_interpolation_result_for_kernel (&(kernel_poly_params[j]),
						evaluation_result,
						Uncoal_Mem_insts);

      ///////////////////////
//      reinit_kernel_poly_interpolator (&(kernel_poly_params[j]));
      interpolate_kernel_param_to_header (&(kernel_poly_params[j]), device_name,
					  Coal_Mem_insts);
      evaluate_interpolation_result_for_kernel (&(kernel_poly_params[j]),
						evaluation_result,
						Coal_Mem_insts);

      ///////////////////////
//      reinit_kernel_poly_interpolator (&(kernel_poly_params[j]));
      interpolate_kernel_param_to_header (&(kernel_poly_params[j]), device_name,
					  Synch_insts);
      evaluate_interpolation_result_for_kernel (&(kernel_poly_params[j]),
						evaluation_result, Synch_insts);

      ///////////////////////
//      reinit_kernel_poly_interpolator (&(kernel_poly_params[j]));
      interpolate_kernel_param_to_header (&(kernel_poly_params[j]), device_name,
					  Coal_per_mw);
      evaluate_interpolation_result_for_kernel (&(kernel_poly_params[j]),
						evaluation_result, Coal_per_mw);

      ///////////////////////
//      reinit_kernel_poly_interpolator (&(kernel_poly_params[j]));
      interpolate_kernel_param_to_header (&(kernel_poly_params[j]), device_name,
					  Uncoal_per_mw);
      evaluate_interpolation_result_for_kernel (&(kernel_poly_params[j]),
						evaluation_result,
						Uncoal_per_mw);

      ///////////////////////
      // reinit_kernel_poly_interpolator (&(kernel_poly_params[j]));
      // interpolate_kernel_param_to_header (&(kernel_poly_params[j]), device_name,
      //	  Active_blocks_per_SM);
      //evaluate_interpolation_result_for_kernel (&(kernel_poly_params[j]),
      //			evaluation_result,
      //			Active_blocks_per_SM);
    }

}
///////////////////////////////////////

/* 1. run emulation and collect the results in result files
 * 2. form the kernel poly list from reading the result of the table.
 * 3. 
 * 
 */

#endif 

