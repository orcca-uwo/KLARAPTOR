#ifndef RATIONAL_FUNCTION_OCCUPANCY_H_
#define RATIONAL_FUNCTION_OCCUPANCY_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gmp.h>
#include "mcwp.h"

///////////////////////////////////////
void
print_to_header (FILE* occupancy_header_file, poly_params kernel_poly_params,
		 int k)
{
  if (k == 0)
    {
      fprintf (occupancy_header_file, "AltArr_t* get_numer_b_actual() {\n");
    }
  else if (k == 1)
    {
      fprintf (occupancy_header_file, "AltArr_t* get_numer_shared_mem() {\n");
    }

  char* pNumer = polyToProgram_AA (kernel_poly_params.poly[k], "Numer");
  fprintf (occupancy_header_file, "%s\n", pNumer);
  fprintf (occupancy_header_file, "return Numer;\n}\n");

  if (k == 0)
    {
      fprintf (occupancy_header_file, "AltArr_t* get_denom_b_actual() {\n");
    }
  else if (k == 1)
    {
      fprintf (occupancy_header_file, "AltArr_t* get_denom_shared_mem() {\n");
    }
  char* pDenom = polyToProgram_AA (kernel_poly_params.denompoly[k], "Denom");
  fprintf (occupancy_header_file, "%s\n", pDenom);
  fprintf (occupancy_header_file, "return Denom;\n}\n");

}

///////////////////////////////////////

int
test_mwp_cwp (int argc, char **argv, int n_kernels, int nvar,
	      int* degree_bounds, int* denomdegree_bounds,
	      char* mem_inst_trace_tmp, enum kernel_type_enum kernel_type, int
	      (*rep_div_n) (int))
{
  //path of the file including device parameters
  char device_params_path[1024] = "../../src/device_profiles/device_default.specs";
  if (argc > 1)
    sprintf (device_params_path, "%s", argv[1]);

  char device_name[32];
  sprintf (device_name, "%s", get_absolute_device_name (device_params_path));

  printf ("\ndevice_params_path=[%s]\n", device_params_path);
  printf ("\ndevice_name=[%s]\n", device_name);

  ////////////////////////////////////

#if VERBOSE 
  printf("[number of kernels: %d]\n", n_kernels);
#endif

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
      kernel_names[i][strcspn (kernel_names[i], "\n")] = 0;
//		printf("kernel: [%s]\n", kernel_names[i]);
    }
  fclose (kernel_name_list);

  const char prefix_kernel[] = "kernel_";
  const char suffix_params[] = "_params.tmp";
  const char suffix_trace[] = "_trace.txt";
  const char suffix_trace_avg[] = "_trace.avg.txt";
  char suffix_results[32] = "_results";
  sprintf (suffix_results, "%s_%s.txt", suffix_results, device_name);

  const char suffix_occupancy[] = "_occupancy.tmp";
  const char suffix_occ_functions[] = "_occ_functions.h";
  const char suffix_interpolation[] = "_interpolation.tmp";

  char** occ_functions_file_path_list = (char **) malloc (
      n_kernels * sizeof(char *));
  char** param_file_path_list = (char **) malloc (n_kernels * sizeof(char *));
  char** trace_file_path_list = (char **) malloc (n_kernels * sizeof(char *));
  char** avg_file_path_list = (char **) malloc (n_kernels * sizeof(char *));
  char** result_file_path_list = (char **) malloc (n_kernels * sizeof(char *));
  char** occupancy_file_path_list = (char **) malloc (
      n_kernels * sizeof(char *));
  char** interpolation_file_path_list = (char **) malloc (
      n_kernels * sizeof(char *));
  char mem_inst_trace[128] =
    { '\0' };

  init_output_path_list (occ_functions_file_path_list, kernel_names,
			 prefix_kernel, suffix_occ_functions, n_kernels);

  init_output_path_list (interpolation_file_path_list, kernel_names,
			 prefix_kernel, suffix_interpolation, n_kernels);

  init_output_path_list (param_file_path_list, kernel_names, prefix_kernel,
			 suffix_params, n_kernels);

  init_output_path_list (trace_file_path_list, kernel_names, prefix_kernel,
			 suffix_trace, n_kernels);

  init_output_path_list (avg_file_path_list, kernel_names, prefix_kernel,
			 suffix_trace_avg, n_kernels);

  init_output_path_list (result_file_path_list, kernel_names, prefix_kernel,
			 suffix_results, n_kernels);

  init_output_path_list (occupancy_file_path_list, kernel_names, prefix_kernel,
			 suffix_occupancy, n_kernels);

  int shared_mem_bytes = 0;

  mpq_t mwp, cwp, clockcycles, rep, active_blocks_per_sm;
  mpq_inits (mwp, cwp, clockcycles, rep, active_blocks_per_sm, NULL);

  int program_params[N_PROGRAM_PARAMS], device_params[N_DEVICE_PARAMS];
  memset (program_params, 0x00, N_PROGRAM_PARAMS * sizeof(int));
  memset (device_params, 0x00, N_DEVICE_PARAMS * sizeof(int));

  //reading device parameters from file
  read_params_from_file (device_params_path, device_params, N_DEVICE_PARAMS,
			 global_device_params_list);

  FILE * n_sm_file = fopen ("device_n_sm.tmp", "w");
  fprintf (n_sm_file, "%d", device_params[5]);
  fclose (n_sm_file);

  //(gridDimX, gridDimY, gridDimZ, blockDimX, blockDimY, blockDimZ, shared_mem_bytes
  program_params_t default_program_params =
    { 1, 1, 1, 1, 1, 1, 0 };

  kernel_params_t kernel_params;

  FILE** mcwp_result = (FILE**) malloc (n_kernels * sizeof(FILE*));
  FILE** interpolation_result = (FILE**) malloc (n_kernels * sizeof(FILE*));
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
  //mpf_t mpf_temp;
  //mpf_init(mpf_temp);
  int* best_n[3];	//= (int*) malloc(n_kernels * sizeof(int));
  int* best_b0[3];	//= (int*) malloc(n_kernels * sizeof(int));
  int* best_b1[3];	//= (int*) malloc(n_kernels * sizeof(int));
  //int* best_case = (int*) malloc(n_kernels * sizeof(int));
  mpq_t* best_clock[3];	// = (mpq_t*) malloc(n_kernels * sizeof(mpq_t));
  for (int i = 0; i < 3; i++)
    {
      best_n[i] = (int*) malloc (n_kernels * sizeof(int));
      best_b0[i] = (int*) malloc (n_kernels * sizeof(int));
      best_b1[i] = (int*) malloc (n_kernels * sizeof(int));
      best_clock[i] = (mpq_t*) malloc (n_kernels * sizeof(mpq_t));

      for (int j = 0; j < n_kernels; j++)
	{
	  best_n[i][j] = 0;
	  best_b0[i][j] = 0;
	  best_b1[i][j] = 0;
	  //best_case[i] = 0;
	  mpq_init (best_clock[i][j]);
	  mpq_set_si (best_clock[i][j], 0, 1);
	}
    }
  //enum kernel_type_enum kernel_type = kernel_type_tmp;
  poly_params* kernel_poly_params = (poly_params*) malloc (
      n_kernels * sizeof(poly_params));
//#if kernel_type==OneDimKernel
  //char* syms[] = { "B0","N" };
//#elif kernel_type==TwoDimKernel
  char* syms1[] =
    { "B0", "B1" };
  char* syms2[] =
    { "N", "B0", "B1" };
//#endif
  for (int k = 0; k < n_kernels; k++)
    {

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
      switch (kernel_type)
	{
	case OneDimKernel:
	  kernel_poly_params[k].point = (&global_p1k1_list);
	  break;

	case TwoDimKernel:
	  kernel_poly_params[k].point = (&global_p2k2_list);
	  break;
	}
    }
  PolyInterpStatus_t stat;
  mpq_t* mpPoint = (mpq_t*) malloc (3 * sizeof(mpq_t));
  mpq_t Val;
  mpq_init (Val);
  for (int i = 0; i < 3; ++i)
    {
      mpq_init (mpPoint[i]);
    }

  remove_trace_log_file ();

  for (int k = 0; k < n_kernels; k++)
    {
      kernel_poly_params[k].interp[0] = rfInterpInit (
	  kernel_poly_params[k].nvar, kernel_poly_params[k].degree_bounds,
	  kernel_poly_params[k].denomdegree_bounds);
      kernel_poly_params[k].poly[0] = NULL;
      kernel_poly_params[k].denompoly[0] = NULL;
      kernel_poly_params[k].status[0] = INIT;

      kernel_poly_params[k].interp[1] = rfInterpInit (
	  kernel_poly_params[k].nvar - 1, kernel_poly_params[k].degree_bounds,
	  kernel_poly_params[k].denomdegree_bounds);
      kernel_poly_params[k].poly[1] = NULL;
      kernel_poly_params[k].denompoly[1] = NULL;
      kernel_poly_params[k].status[1] = INIT;

    }

  //////////////////////////////////
  // begin interpolation
  //////////////////////////////////

  while (kernel_poly_params[0].point != NULL)
    {

#if VERBOSE
      printf("Init interpolation done.\n");
#endif

      int current_n = kernel_poly_params[0].point->N[0];

      while (kernel_poly_params[0].point != NULL
	  && kernel_poly_params[0].point->N[0] == current_n)
	{

	  int n, b0, b1;
	  for (int j = 0; j < n_kernels; j++)
	    {
	      n = kernel_poly_params[j].point->N[0];
	      b0 = kernel_poly_params[j].point->N[1];
	      b1 = kernel_poly_params[j].point->N[2];
#if VERBOSE
	      printf("=================================\n");
	      printf("checking [N=%d, B0=%d, B1=%d] for kernel[%d]\n", n, b0, b1,j);
#endif
	    }

	  //calling ocelot tracer to generate stats for each kernel
	  //enumerated in kernel_name_list.tmp in the current directory.
	  sprintf (mem_inst_trace, mem_inst_trace_tmp, n, b0, b1);
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
	      printf("reading file [%s] ... \n", avg_file_path_list[j]);
#endif
//				read_params_from_file(avg_file_path_list[j], program_params,
//						N_PROGRAM_PARAMS, global_program_params_list);
	      if (read_params_from_file (avg_file_path_list[j], program_params,
					 N_PROGRAM_PARAMS,
					 global_program_params_list)
		  != EXIT_SUCCESS)
		{
		  printf ("kernel has not been traced; continue\n");
		  kernel_poly_params[j].point = get_next_kernel_mesh_point (
		      kernel_poly_params[j].point);
		  continue;
		}

	      read_kernel_params_from_file (&kernel_params,
					    param_file_path_list[j]);

	      int n_blocks = kernel_params.blocks_per_grid;
	      int threads_per_block = kernel_params.threads_per_block;
#if VERBOSE >=2
	      printf("removing trace file [%s] -> done!\n", trace_file_path_list[j]);
#endif
	      remove (trace_file_path_list[j]);
	      remove (avg_file_path_list[j]);

	      //mpq_set_si(mpPoint[2], n, 1);
	      mpq_set_si (mpPoint[0], b0, 1);
	      mpq_set_si (mpPoint[1], b1, 1);

	      //mpq_set_si (Val, program_params[1], 1);
	      mpq_set_si (Val, kernel_params.shared_mem_bytes, 1);

#if VERBOSE
	      printf("b0: %d, b1: %d, shared_mem:%d\n", b0,b1, kernel_params.shared_mem_bytes);
#endif
	      rfInterpAddPointValMP (kernel_poly_params[j].interp[1], mpPoint,
				     Val);

#if VERBOSE
	      printf("b0: %d, b1: %d, n_blocks:%d, n_sm:%d\n", b0,b1, n_blocks,device_params[5]);
#endif			
	      mpq_set_si (mpPoint[0], n, 1);
	      mpq_set_si (mpPoint[1], b0, 1);
	      mpq_set_si (mpPoint[2], b1, 1);
	      mpq_set_si (Val, n_blocks, device_params[5]);
	      rfInterpAddPointValMP (kernel_poly_params[j].interp[0], mpPoint,
				     Val);
#if VERBOSE  >=2
	      printf("Get best done!\n");
#endif
	      kernel_poly_params[j].point = get_next_kernel_mesh_point (
		  kernel_poly_params[j].point);
	    }
	}
    }

  //////////////////////////////////
  // end of interpolation
  //////////////////////////////////

  for (int j = 0; j < n_kernels; j++)
    {
      FILE * occupancy_header_file = fopen (occ_functions_file_path_list[j],
					    "w");
      fprintf (occupancy_header_file, "#ifndef EVAL_OCC_H_\n");
      fprintf (occupancy_header_file, "#define EVAL_OCC_H_\n\n");

      fprintf (occupancy_header_file, "#include <stdio.h>\n");
      fprintf (occupancy_header_file, "#include <stdlib.h>\n");
      fprintf (occupancy_header_file, "#include <gmp.h>\n");
      fprintf (
	  occupancy_header_file,
	  "#include \"../../src/NumericalPolySupport/include/interpolator.h\"\n\n");
      for (int k = 0; k < 2; k++)
	{
	  stat = rfInterpGetPoly (kernel_poly_params[j].interp[k],
				  &kernel_poly_params[j].poly[k],
				  &kernel_poly_params[j].denompoly[k], eps);
	  if (stat == POLY_INTERP_SUCCESS)
	    {
//				fprintf(interpolation_result[j], "For Shared memory:\n");
//				fprintf(stderr, "For Actual blocks per SM:\n");
	      print_to_header (occupancy_header_file, kernel_poly_params[j], k);

//				fprintf(stderr,
//						"Interpolated a rational function for kernel %s in case %d: \n",
//						kernel_names[j], k + 1);
//				printf("In rational number:\n");
//				fprintf(stderr, "f%d := (", k + 1);
//				printPoly_AA(stderr, kernel_poly_params[j].poly[k], syms,
//						kernel_poly_params[j].nvar - k);
//				//printf("------------------------------------------------------\n");
//				fprintf(stderr, ") / (");
//				printPoly_AA(stderr, kernel_poly_params[j].denompoly[k], syms,
//						kernel_poly_params[j].nvar - k);
//				fprintf(stderr, ");");
//				printf("\n");

	    }
	  else if (stat == POLY_INTERP_FAILURE)
	    {

	      fprintf (stderr, "ERROR: Failed to interpolate for case %d!\n\n",
		       k + 1);
	      fprintf (interpolation_result[j],
		       "ERROR: Failed to interpolate for case %d!\n\n", k + 1);
	    }
	  else if (stat == POLY_INTERP_FAIL_BY_RESIDUAL)
	    {
	      fprintf (
		  stderr,
		  "ERROR: Failed to interpolate for case %d! Residual was too high!\n\n",
		  k + 1);
	      fprintf (
		  interpolation_result[j],
		  "ERROR: Failed to interpolate for case %d! Residual was too high!\n\n",
		  k + 1);
	    }

	  interpFree (kernel_poly_params[j].interp[k]);
	  freePolynomial_AA (kernel_poly_params[j].poly[k]);
	  freePolynomial_AA (kernel_poly_params[j].denompoly[k]);
	}
      fprintf (occupancy_header_file, "#endif\n");
      fclose (occupancy_header_file);
    }

  for (int k = 0; k < n_kernels; k++)
    {
      fclose (mcwp_result[k]);
      fclose (interpolation_result[k]);
    }

  for (int i = 0; i < 3; ++i)
    {
      mpq_clear (mpPoint[i]);
    }
  free (mpPoint);
  return 0;
}

///////////////////////////////////////

// add comments
// write functions
// follow c99 standard, loop variales inside the body
// take into account the cache friendliness
// exit(EXIT_FAILURE);

#endif
