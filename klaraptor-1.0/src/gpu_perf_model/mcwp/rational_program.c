/*!
 \file rational_program.c
 \author Linxiao Wang <lwang739@uwo.ca>
 \author Davood Mohajerani <dmohajer@uwo.ca>
 \brief
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gmp.h>
#include "mcwp/mcwp.h"
#include <math.h>

///////////////////////////////////////
//"INCLUDE_FILES_REPLACEMENT"
///////////////////////////////////////

#define MAX_BLOCK_SIZE 1024
#define MIN_BLOCK_SIZE 32

///////////////////////////////////////
#ifndef VERBOSE_RP
#define VERBOSE_RP 0
#endif

///////////////////////////////////////
#define N_SUPPORTED_CUDA_ARCH 14

///////////////////////////////////////

int int_flooring_div(int x, int y) {
	return (x / y);
}

/////////////////////////////////////////

int min(int x, int y) {
	if (x <= y)
		return x;
	else
		return y;
}

///////////////////////////////////////

int eval_rp(int n, int b0, int b1, int n_var, AltArr_t* Numer, AltArr_t* Denom) {
	int nvar = 3;
	mpq_t numVal;
	mpq_t denVal;
	mpq_t point[3];
	mpq_init(numVal);
	mpq_init(denVal);
	for (int i = 0; i < 3; ++i) {
		mpq_init(point[i]);
	}

	mpq_set_si(point[0], n, 1);
	mpq_set_si(point[1], b0, 1);
	mpq_set_si(point[2], b1, 1);

	evalPolyToVal_AA(Numer, point, nvar, numVal);
	evalPolyToVal_AA(Denom, point, nvar, denVal);

//  gmp_printf ("%f\n", mpq_get_d (numVal));
//  gmp_printf ("%f\n", mpq_get_d (denVal));
	if (mpq_sgn (denVal) == 0) {
		printf("[@%s][ERROR: Zero denominator error!]\n", __func__);
		exit(EXIT_FAILURE);
	}
	mpq_div(numVal, numVal, denVal);
	int dval = ceil(mpq_get_d(numVal));

//    printf("evalued_val=%d\n", dval);

	mpq_clear(numVal);
	mpq_clear(denVal);
	for (int i = 0; i < 3; ++i) {
		mpq_clear(point[i]);
	}

	return dval;
}

///////////////////////////////////////

int eval_constant_poly_ui(int *val_out, const AltArr_t* Numer) {
	int nvar = 0;
	mpq_t numVal;
	mpq_t denVal;
	mpq_t point[3];
	mpq_init(numVal);
	mpq_init(denVal);
	for (int i = 0; i < 3; ++i) {
		mpq_init(point[i]);
	}

//  mpq_set_si (point[0], n, 1);
//  mpq_set_si (point[1], b0, 1);
//  mpq_set_si (point[2], b1, 1);

	evalPolyToVal_AA(Numer, NULL, nvar, numVal);
//  evalPolyToVal_AA (Denom, point, nvar, denVal);

//  gmp_printf ("%f\n", mpq_get_d (numVal));
//  gmp_printf ("%f\n", mpq_get_d (denVal));
//  if (mpq_sgn (denVal) == 0)
//    {
//      printf ("[@%s][ERROR: Zero denominator error!]\n", __func__);
//      exit (EXIT_FAILURE);
//    }
//  mpq_div (numVal, numVal, denVal);
	int dval = ceil(mpq_get_d(numVal));

//  int val=mpq_get_ui()
//    printf("evalued_val=%d\n", dval);

	mpq_clear(numVal);
	mpq_clear(denVal);
	for (int i = 0; i < 3; ++i) {
		mpq_clear(point[i]);
	}

	return EXIT_SUCCESS;
}

///////////////////////////////////////

int eval_constant_poly(mpq_t result_qq, const AltArr_t* Numer) {
	int nvar = 0;
	mpq_t numVal;
	mpq_t denVal;
	mpq_t point[3];
	mpq_init(numVal);
	mpq_init(denVal);
	for (int i = 0; i < 3; ++i) {
		mpq_init(point[i]);
	}

//  mpq_set_si (point[0], n, 1);
//  mpq_set_si (point[1], b0, 1);
//  mpq_set_si (point[2], b1, 1);

	evalPolyToVal_AA(Numer, point, nvar, numVal);
//  evalPolyToVal_AA (Denom, point, nvar, denVal);

//  gmp_printf ("%f\n", mpq_get_d (numVal));
//  gmp_printf ("%f\n", mpq_get_d (denVal));
//  if (mpq_sgn (denVal) == 0)
//    {
//      printf ("[@%s][ERROR: Zero denominator error!]\n", __func__);
//      exit (EXIT_FAILURE);
//    }
//  mpq_div (numVal, numVal, denVal);
//  int dval = ceil (mpq_get_d (numVal));

	mpq_set(result_qq, numVal);

//  int val=mpq_get_ui()
//    printf("evalued_val=%d\n", dval);

	mpq_clear(numVal);
	mpq_clear(denVal);
	for (int i = 0; i < 3; ++i) {
		mpq_clear(point[i]);
	}

	return EXIT_SUCCESS;
}

///////////////////////////////////////

int eval_rp_at_single_point(mpq_t *result_qq, const single_point_t* mesh_point,
		const int nvar, const AltArr_t* Numer, const AltArr_t* Denom) {
	mpq_t numVal, denVal;
	mpq_inits(numVal, denVal, NULL);
	mpq_t *point_qq = (mpq_t*) malloc(nvar * sizeof(mpq_t));

	for (int i = 0; i < nvar; ++i) {
		mpq_init(point_qq[i]);
		mpq_set_ui(point_qq[i], mesh_point->N[i], 1);
	}

//  mpq_set_si (point[0], point->N[0], 1);
//  mpq_set_si (point[1], b0, 1);
//  mpq_set_si (point[2], b1, 1);

	if (Numer != NULL)
		evalPolyToVal_AA(Numer, point_qq, nvar, numVal);
	else {
		mpq_set_ui(numVal, 0, 1);
	}
	if (Denom != NULL)
		evalPolyToVal_AA(Denom, point_qq, nvar, denVal);
	else {
		mpq_set_ui(denVal, 1, 1);
	}

//  gmp_printf ("%f\n", mpq_get_d (numVal));
//  gmp_printf ("%f\n", mpq_get_d (denVal));
	if (mpq_sgn (denVal) == 0) {
		printf("[@%s][ERROR: Zero denominator error!]\n", __func__);
		exit(EXIT_FAILURE);
	}
	mpq_div(numVal, numVal, denVal);

	mpq_set(*result_qq, numVal);

//  mpq_ceil (numVal);
//  int dval = (int) (mpq_get_d (numVal));

	mpq_clears(numVal, denVal, NULL);
	for (int i = 0; i < nvar; i++) {
		mpq_clear(point_qq[i]);
	}
	free(point_qq);

	return EXIT_SUCCESS;
}

///////////////////////////////////////

float get_occupancy(int b0, int b1, int* arch_params, int B_active) {

	int W_max = arch_params[1];

	int T = b0 * b1;

	float occupancy = (float) (B_active * T) / ((float) 32 * W_max);
	return occupancy;
}

///////////////////////////////////////

float get_occupancy_via_n_warps(const int* arch_params, const int n_warps) {
	int W_max = arch_params[1];
	float occupancy = (float) (n_warps) / ((float) W_max);
	return occupancy;
}

///////////////////////////////////////

int compute_n_active_blocks_per_sm(int *n_Active_Blocks_per_SM,
		int *n_warps_per_SM, int *error_block, int *error_warp,
		const int n_blocks, const int n_sm, const single_point_t* point,
		const int n_register_per_thread, const int* arch_params,
		const int n_shared_mem_bytes_total) {
//  printf ("[@%s]\n", __func__);
	int R = n_register_per_thread;
	int B_max = arch_params[ARCH_PARAM_MAX_BLOCKS_PER_SM];
	int W_max = arch_params[ARCH_PARAM_MAX_WARPS_PER_SM];
	int T_max = arch_params[ARCH_PARAM_MAX_BLOCK_SIZE];
	int R_max = arch_params[ARCH_PARAM_MAX_REGISTERS_PER_BLOCK];
	int Z_max = arch_params[ARCH_PARAM_MAX_SHARED_MEM_PER_BLOCK];

//  for (int i = 0; i < ARCH_PARAM_ENUM_SIZE; i++)
//    printf ("[%-36s]:[%-8d]\n", arch_param_names_dict[i], arch_params[i]);

	int B_active = 0;
	int Z = n_shared_mem_bytes_total;
	Z = 0;

	int block_size = point->N[1] * point->N[2] * point->N[3];
	int T = block_size;

	int w_max_32 = 32 * W_max;

	int T_B_max = T * B_max;
	//limited by number of blocks per sm.

	int cases[4] = { 0, 0, 0, 0 };

	if ((T_B_max <= w_max_32) && (R * T_B_max <= R_max)
			&& (Z * B_max <= Z_max)) {
//      printf ("Case 1\n");
		B_active = B_max;
		cases[0] = 1;
	}
	//limited by number of warps per sm.
	else if ((w_max_32 <= T_B_max) && (w_max_32 * R <= R_max)
			&& (w_max_32 * Z <= Z_max * T)) {
//      printf ("Case 2\n");
//      printf ("T=%d\n", T);

		B_active = int_ceiling_div(w_max_32, T);
		cases[1] = 1;
	}
	//limited by number of regisers per thread
	else if ((R_max <= R * T_B_max) && (R_max <= w_max_32 * R)
			&& (R_max * Z <= R * T_B_max)) {
//      printf ("Case 3\n");
		B_active = int_ceiling_div(R_max, R * T);
		cases[2] = 1;
	}
	//limited by number of shared mem per block.
	else if ((Z_max <= B_max * Z) && (Z_max * T <= w_max_32 * Z)
			&& (Z_max * R * T <= Z * R_max)) {
//      printf ("Case 4\n");
		B_active = int_ceiling_div(Z_max, Z);
		cases[3] = 1;
	} else {
//      	printf("Active block is 0, fail to lunch the kernel!\n");
		return EXIT_FAILURE;
	}
//  for (int i = 0; i < 4; i++)
//    {
//      printf ("case[%d]=%d -- ", i, cases[i]);
//    }
//  printf ("\n");

/////////////////////////////////////
//maximum value for active blocks per sm is stored in B_active.

//get a rough estimate of number of blocks per sm.
	int current_n_Active_Blocks_per_SM;
	current_n_Active_Blocks_per_SM = int_ceiling_div(n_blocks, n_sm);
//  printf("nblocks=%d, current_n_Active_Blocks_per_SM=%d\n", n_blocks, current_n_Active_Blocks_per_SM);
	//the estimated value should not be larger than the device peak.
	current_n_Active_Blocks_per_SM = min(current_n_Active_Blocks_per_SM, B_max);
//  printf("Bactive_before =%d\n", B_active);
	//now, find the min/max of both estimates:
	B_active = min(current_n_Active_Blocks_per_SM, B_active);
//  printf("Bactive_after =%d\n", B_active);
//  if (current_n_Active_Blocks_per_SM != 0)
//    {
////      if (B_active > current_n_Active_Blocks_per_SM)
////	{
////	  printf ("[@%s][estimated B_active >current_n_Active_Blocks_per_SM]"
////		  "[swapping the result]\n",
////		  __func__);
////	  B_active = current_n_Active_Blocks_per_SM;
////	}
////      printf ("min (B_active, current_n_Active_Blocks_per_SM)=%d,%d", B_active,
////	      current_n_Active_Blocks_per_SM);
////      B_active = min(B_active, current_n_Active_Blocks_per_SM);
//      if (B_active<current_n_Active_Blocks_per_SM)
//	B_active=current_n_Active_Blocks_per_SM;
//    }
//  printf ("B_active=%d\n", B_active);

	int w_active;
	int n_warps_per_block = int_ceiling_div(block_size, 32);
	int w_active_alternative = n_warps_per_block * B_active;
	//* int_ceiling_div (n_blocks, n_sm);
	w_active = min(W_max, w_active_alternative);
	int n_active_blocks_per_sm = B_active; //int_ceiling_div ((B_active * 1000), 1000);
	///////////////////////////////////
	int n_active_blocks_per_sm_interp = *n_Active_Blocks_per_SM;
	int w_active_interp = *n_warps_per_SM;
	int applied_error_percentage = -1;
	int applied_error_percentage_w_active = -1;
	if (n_active_blocks_per_sm_interp && w_active_interp) {
		n_active_blocks_per_sm_interp = min(n_active_blocks_per_sm_interp,
				arch_params[ARCH_PARAM_MAX_BLOCKS_PER_SM]);
		applied_error_percentage = int_ceiling_div(B_active * 1000,
				n_active_blocks_per_sm_interp);
//  int n_active_blocks_per_sm = int_ceiling_div (
//      (B_active * applied_error_percentage), 1000);

		///////////////////////////////////

		w_active_interp = min(w_active_interp,
				arch_params[ARCH_PARAM_MAX_WARPS_PER_SM]);
		applied_error_percentage_w_active = int_ceiling_div(w_active * 1000,
				w_active_interp);
//      w_active = int_ceiling_div (w_active * 1000, 1000);

//  if (n_active_blocks_per_sm_interp > B_active)
//    n_active_blocks_per_sm_interp = B_active;
	}

//  printf ("[bx=%4d, by=%4d][block_interp=%4d]"
//	  "[warp_interp=%4d][Z=%3d][T=%3d][R=%3d]\n",
//	  point->N[1], point->N[2], applied_error_percentage,
//	  applied_error_percentage_w_active, Z, T, R);
	///////////////////////////////////
//  *error_block = applied_error_percentage;
//  *error_warp = applied_error_percentage_w_active;
//  *error_block = applied_error_percentage_w_active;

	*n_Active_Blocks_per_SM = n_active_blocks_per_sm;
	*n_warps_per_SM = w_active;
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int get_sm_arch_from_str(char * sm_arch) {
	int len = strlen(sm_arch);
	char tmp[3];
	for (int i = 0; i < 2; i++)
		tmp[i] = sm_arch[i + 3];
	tmp[2] = '\0';

	int sm_arch_val = atoi(tmp);
	return sm_arch_val;
}

//the following info is extracted from CUDA occupancy calculator by NVIDIA.
//TODO: replace the search method by lookup table using hex values and enums.
int set_arch_params(int * arch_params_out, const int sm_cc) {

	int arch_cc = sm_cc; //get_sm_arch_from_str (sm_arch);

	printf("arch_cc=%d\n", arch_cc);
	//  printf ("[looking for arch=sm_%d]\n", arch);
	int arch_index = -1;
	int sm_list[N_SUPPORTED_CUDA_ARCH] = { 20, 21, 30, 32, 35, 37, 50, 52, 53,
			60, 61, 62, 70, 75 };
	for (int i = 0; i < N_SUPPORTED_CUDA_ARCH; i++) {
		if (sm_list[i] == arch_cc) {
			arch_index = i;
			break;
		}
	}
	if (arch_index == -1) {
		printf("Invalid SM: %d\n", arch_cc);
		exit(EXIT_FAILURE);
	}

	//check.
	int max_registers_per_block_list[N_SUPPORTED_CUDA_ARCH] = { 32768, 32768,
			65536, 65536, 65536, 65536, 65536, 65536, 32768, 65536, 65536,
			65536, 65536, 65536 };

	//check.
	int max_blocks_per_sm_list[N_SUPPORTED_CUDA_ARCH] = { 8, 8, 16, 16, 16, 16,
			32, 32, 32, 32, 32, 32, 32, 32 };

	//check.
	int max_register_per_thread_list[N_SUPPORTED_CUDA_ARCH] = { 63, 63, 63, 255,
			255, 255, 255, 255, 255, 255, 255, 255, 255, 255 };

	//check.
	int max_warps_per_sm_list[N_SUPPORTED_CUDA_ARCH] = { 48, 48, 64, 64, 64, 64,
			64, 64, 64, 64, 64, 64, 64, 32 };

	//check.
	const int max_shared_mem_per_block[N_SUPPORTED_CUDA_ARCH] = { 49152, 49152,
			49152, 49152, 49152, 49152, 49152, 49152, 49152, 49152, 49152,
			49152, 98304, 65536 };

	//check.
	const int max_block_size = 1024;

//  int arch_params[5] =
//    { max_blocks_per_sm_list[arch_index], max_warps_per_sm_list[arch_index],
//	max_block_size, max_registers_per_block_list[arch_index],
//	max_shared_mem_per_block };

	int arch_params[5];
	arch_params[ARCH_PARAM_MAX_BLOCKS_PER_SM] =
			max_blocks_per_sm_list[arch_index];

	arch_params[ARCH_PARAM_MAX_WARPS_PER_SM] =
			max_warps_per_sm_list[arch_index];

	arch_params[ARCH_PARAM_MAX_BLOCK_SIZE] = max_block_size;

	arch_params[ARCH_PARAM_MAX_REGISTERS_PER_BLOCK] =
			max_registers_per_block_list[arch_index];

	arch_params[ARCH_PARAM_MAX_SHARED_MEM_PER_BLOCK] =
			max_shared_mem_per_block[arch_index];
	memcpy(arch_params_out, arch_params, 5 * sizeof(int));
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int device_params_init_from_const_polys(param_list_t *device_params) {
//  typedef enum device_param_names_enum
//  {
//    DEVICE_PARAM_Issue_cycles = 0,
//    DEVICE_PARAM_Mem_bandwidth = 1,
//    DEVICE_PARAM_Mem_LD = 2,
//    DEVICE_PARAM_Departure_del_uncoal = 3,
//    DEVICE_PARAM_Departure_del_coal = 4,
//    DEVICE_PARAM_Active_SMs = 5,
//    DEVICE_PARAM_Freq = 6,
//    DEVICE_PARAM_Load_bytes_per_warp = 7,
//    DEVICE_PARAM_Compute_Capability= 8,
//  } device_param_names_t;

	AltArr_t *func_ptr_list[DEVICE_PARAMS_ENUM_SIZE] = {
			get_numer_Issue_cycles(), get_numer_Mem_bandwidth(),
			get_numer_Mem_LD(), get_numer_Departure_del_coal(),
			get_numer_Active_SMs(), get_numer_Freq(),
			get_numer_Load_bytes_per_warp(), get_numer_Compute_Capability() };
	int mcwp_error;

	for (int i = 0; i < DEVICE_PARAMS_ENUM_SIZE; i++) {
		if (func_ptr_list[i] == NULL)
			continue;

		mcwp_error = eval_constant_poly(device_params->mpq_values[i],
				func_ptr_list[i]);
		check_mcwp_error(mcwp_error, "eval_constant_poly");
#if VERBOSE
		printf ("[%s]=[%d]\n", device_params->dict[i],
				mpq_get_ui (device_params->mpq_values[i]));
		print_short_dashed_line ();
#endif
		freePolynomial_AA(func_ptr_list[i]);

	}
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int ptx_params_init_from_const_polys(param_list_t *ptx_params) {

//  typedef enum ptx_param_names_enum
//  {
//    PTX_PARAM_shared_mem_bytes_static = 0, PTX_PARAM_registers = 1,
//  } ptx_param_names_t;

	AltArr_t *func_ptr_list[PTX_PARAMS_ENUM_SIZE] = {
			get_numer_shared_mem_bytes_static(), get_numer_registers() };
	int mcwp_error;

	for (int i = 0; i < PTX_PARAMS_ENUM_SIZE; i++) {
		if (func_ptr_list[i] == NULL)
			continue;
		mcwp_error = eval_constant_poly(ptx_params->mpq_values[i],
				func_ptr_list[i]);
		check_mcwp_error(mcwp_error, "eval_constant_poly");
#if VERBOSE
		printf ("[%s]=[%d]\n", ptx_params->dict[i],
				mpq_get_ui (ptx_params->mpq_values[i]));
		print_short_dashed_line ();
#endif
		freePolynomial_AA(func_ptr_list[i]);

	}
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int profiling_param_list_evaluate_for_mesh_point(param_list_t* profiling_params,
		single_point_t*point) {

//  AltArr_t* func_ptr_list[PROFILING_PARAMS_ENUM_SIZE] =
//    { get_numer_13_Total_insts (), get_numer_14_Comp_insts (),
//	get_numer_15_Mem_insts (), get_numer_dynamic_Mem_LD (),
//	get_numer_Active_blocks_per_SM (), get_numer_Blocks (),
//	get_numer_Threads_per_block (), get_numer_n_warps (),
//	get_numer_Active_warps_per_SM (), get_numer_active_cycles (),
//	get_numer_grid_dim_x (), get_numer_grid_dim_y (),
//	get_numer_grid_dim_z (), get_numer_block_dim_x (),
//	get_numer_block_dim_y (), get_numer_block_dim_z (),
//	get_numer_shared_mem_bytes_dynamic () };

	AltArr_t* numer_func_ptr_list[PROFILING_PARAMS_ENUM_SIZE] = { NULL,
			get_numer_Comp_insts(), get_numer_Mem_insts(),
			get_numer_dynamic_Mem_LD(), NULL,
			NULL, NULL, NULL,
			NULL,
			NULL,
			NULL, NULL,
			NULL, NULL,
			NULL, NULL, get_numer_shared_mem_bytes_dynamic() };

	AltArr_t* denom_func_ptr_list[PROFILING_PARAMS_ENUM_SIZE] = { NULL,
			get_denom_Comp_insts(), get_denom_Mem_insts(),
			get_denom_dynamic_Mem_LD(), NULL,
			NULL, NULL, NULL,
			NULL,
			NULL,
			NULL, NULL,
			NULL, NULL,
			NULL, NULL, get_denom_shared_mem_bytes_dynamic() };

	int mcwp_error;
	for (int i = 0; i < PROFILING_PARAMS_ENUM_SIZE; i++) {
		if ((numer_func_ptr_list[i] == NULL)
				|| (denom_func_ptr_list[i] == NULL))
			continue;
		mcwp_error = eval_rp_at_single_point(&profiling_params->mpq_values[i],
				point, 3, numer_func_ptr_list[i], denom_func_ptr_list[i]);
		check_mcwp_error(mcwp_error, "eval_rp_at_single_point");

#if VERBOSE
		gmp_printf ("[%s]=[%d]\n", profiling_params->dict[i],
				mpq_get_ui (profiling_params->mpq_values[i]));
		print_short_dashed_line ();
#endif
	}

	for (int i = 0; i < PROFILING_PARAMS_ENUM_SIZE; i++) {
		if (numer_func_ptr_list[i] != NULL)
			freePolynomial_AA(numer_func_ptr_list[i]);
	}
	for (int i = 0; i < PROFILING_PARAMS_ENUM_SIZE; i++) {
		if (denom_func_ptr_list[i] != NULL)
			freePolynomial_AA(denom_func_ptr_list[i]);
	}

	//this is to prevent the mcwp from failing, turn non-positive profiling values to 1.
	for (int i = 0; i < PROFILING_PARAMS_ENUM_SIZE; i++) {
		mpq_ceil((profiling_params->mpq_values[i]));
		if (mpq_cmp_zero((profiling_params->mpq_values[i])) <= 0)
			mpq_set_ui(profiling_params->mpq_values[i], 1, 1);
	}

	int block_size = point->N[1] * point->N[2];
	mpq_set_ui(profiling_params->mpq_values[PROFILING_PARAM_Threads_per_block],
			block_size, 1);
#if VERBOSE
	gmp_printf (
			"[%s]=[%d]\n",
			profiling_params->dict[PROFILING_PARAM_Threads_per_block],
			mpq_get_ui (
					profiling_params->mpq_values[PROFILING_PARAM_Threads_per_block]));
#endif

	mpq_add(profiling_params->mpq_values[PROFILING_PARAM_13_Total_insts],
			profiling_params->mpq_values[PROFILING_PARAM_14_Comp_insts],
			profiling_params->mpq_values[PROFILING_PARAM_15_Mem_insts]);

	return EXIT_SUCCESS;
}

///////////////////////////////////////

int get_index_of_first_valid_entry(int *sort_results, int n_points) {
	for (int i = 0; i < n_points; i++) {
		int idx = sort_results[i];
		if (idx != -1)
			return i;
	}
	return -1;
}

///////////////////////////////////////

int invalidate_sort_results_by_percentage(int *sort_results,
		const param_list_t * param_list, const int n_points,
		const int sort_metric, const int ascending, const int max_percentage) {
	mpq_t *pivot_qq;
	int pivot_idx = 0;
	for (int i = 1; i < n_points; i++) {
		if (sort_results[i] != -1) {
			pivot_idx = sort_results[i];
			break;
		}
	}
//  pivot_idx = get_index_of_first_valid_entry (sort_results, n_points);
	int max_rank_visited = 1;
	pivot_qq = &(param_list[pivot_idx].mpq_values[sort_metric]);

	//cmp=mpq_cmp(current, pivot);
	int cmp_sign = 1;
	if (!ascending)
		cmp_sign = -1;

//  int last_visited_idx = 1;

	int n_valid_ranks = 0;
	////set the pivot to the last element that has the maximum rank.
	for (int i = 0; i < n_points; i++) {
		int idx = sort_results[i];
		if (idx == -1)
			continue;
		mpq_t *current_qq = &(param_list[idx].mpq_values[sort_metric]);
		//the current value is larger than the current pivot.
		int cmp = mpq_cmp(*current_qq, *pivot_qq);
		if ((cmp * cmp_sign) > 0) {
			//if the max valid rank is not visited yet, set the pivot to the current value.
			{
//	      pivot_idx = idx;
				pivot_qq = current_qq;
				max_rank_visited++;
//	      last_visited_idx = i;
			}
		}
	}
	n_valid_ranks = max_rank_visited;
	int max_valid_rank =
			(int) ((1.0 * n_valid_ranks * max_percentage) / (100.0));

	printf("max_valid_rank=%d, n_valid=%d\n", max_valid_rank, n_valid_ranks);
	max_rank_visited = 1;
	pivot_qq = &(param_list[pivot_idx].mpq_values[sort_metric]);
	for (int i = 0; i < n_points; i++) {
		int idx = sort_results[i];
		if (idx == -1)
			continue;
		mpq_t *current_qq = &(param_list[idx].mpq_values[sort_metric]);
		//the current value is larger than the current pivot.
		int cmp = mpq_cmp(*current_qq, *pivot_qq);
		if ((cmp * cmp_sign) > 0) {
			//if the max valid rank is not visited yet, set the pivot to the current value.
			if (max_rank_visited <= max_valid_rank) {
//	      pivot_idx = idx;
				pivot_qq = current_qq;
				max_rank_visited++;
//	      last_visited_idx = i;
			} else {
				sort_results[i] = -1;
			}
		}
	}

//  for (int i = last_visited_idx; i < n_points; i++)
//    {
//      int idx = sort_results[i];
//      if (idx == -1)
//	continue;
//      //the current value goes beyond the max rank, should invalidate it.
//      else
//	{
////	  printf ("invalidating entry [%s][i=%d][idx=%d]\n",
////		  param_list->dict[sort_metric], i, idx);
//	  sort_results[i] = -1;
//	}
//    }
	return EXIT_SUCCESS;
}

///////////////////////////////////////

/**
 * set the sort results to -1 for values that have rank higher than
 * "max_valid_rank". if the data is sorted in the ascending order,
 * a higher rank means higher value, otherwise, if the data is sorted
 * in the descending order, a higher rank means lower values.
 */
int invalidate_sort_results_by_rank(int *sort_results,
		const param_list_t * param_list, const int n_points,
		const int sort_metric, const int ascending, const int max_valid_rank) {
	mpq_t *pivot_qq;
	int pivot_idx = 0;
	for (int i = 1; i < n_points; i++) {
		if (sort_results[i] != -1) {
			pivot_idx = sort_results[i];
			break;
		}
	}
//  pivot_idx = get_index_of_first_valid_entry (sort_results, n_points);
	int max_rank_visited = 1;
	pivot_qq = &(param_list[pivot_idx].mpq_values[sort_metric]);

	//cmp=mpq_cmp(current, pivot);
	int cmp_sign = 1;
	if (!ascending)
		cmp_sign = -1;

//  int last_visited_idx = 1;
	////set the pivot to the last element that has the maximum rank.
	for (int i = 0; i < n_points; i++) {
		int idx = sort_results[i];
		if (idx == -1)
			continue;
		mpq_t *current_qq = &(param_list[idx].mpq_values[sort_metric]);
		//the current value is larger than the current pivot.
		int cmp = mpq_cmp(*current_qq, *pivot_qq);
		if ((cmp * cmp_sign) > 0) {
			//if the max valid rank is not visited yet, set the pivot to the current value.
			if (max_rank_visited < max_valid_rank) {
//	      pivot_idx = idx;
				pivot_qq = current_qq;
				max_rank_visited++;
//	      last_visited_idx = i;
			} else if (max_rank_visited == max_valid_rank) {
				sort_results[i] = -1;
			}
		}
	}

//  for (int i = last_visited_idx; i < n_points; i++)
//    {
//      int idx = sort_results[i];
//      if (idx == -1)
//	continue;
//      //the current value goes beyond the max rank, should invalidate it.
//      else
//	{
////	  printf ("invalidating entry [%s][i=%d][idx=%d]\n",
////		  param_list->dict[sort_metric], i, idx);
//	  sort_results[i] = -1;
//	}
//    }
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int sort_by_param(int *sort_results, const param_list_t * param_list,
		const int n_points, const int sort_metric, const int ascending,
		const int max_valid_rank) {

	int cmp_sign = 1;
	if (!ascending)
		cmp_sign = -1;

	//only sort positive values; disqualify all the negative values.
	for (int i = 0; i < n_points; i++) {
		//the if is critical to avoid overwriting the existing sort_results.
		if (sort_results[i] != -1) {
			mpq_t * v_i =
					&((param_list[sort_results[i]]).mpq_values[sort_metric]);
			if (mpq_sgn(*v_i) < 0)
				sort_results[i] = -1;
		}
	}

	for (int i = 0; i < n_points; i++) {
		for (int j = 0; j < i; j++) {
			/////////////////////////////////
			if (sort_results[i] == -1)
				continue;
			if (sort_results[j] == -1)
				continue;

//	  printf ("check i,j=[%d,%d]\n", i, j);
			/////////////////////////////////
			mpq_t * v_i =
					&((param_list[sort_results[i]]).mpq_values[sort_metric]);
			mpq_t * v_j =
					&((param_list[sort_results[j]]).mpq_values[sort_metric]);

			/////////////////////////////////
//	  printf ("cmp %d %d\n", i, j);
			int cmp = mpq_cmp(*v_j, *v_i);
			if ((cmp * cmp_sign) > 0) {
//	      printf ("swapping [%4d]=[%10d] , [%4d]=[%10d]\n",
//		      sort_results[i], mpq_get_u64 (*v_i), sort_results[j],
//		      mpq_get_u64 (*v_j));
				int t = sort_results[i];
				sort_results[i] = sort_results[j];
				sort_results[j] = t;
			}
		}
	}

	if (max_valid_rank)
		invalidate_sort_results_by_rank(sort_results, param_list, n_points,
				sort_metric, ascending, max_valid_rank);

	return EXIT_SUCCESS;
}

///////////////////////////////////////

double find_min_u64(double* values, int size) {
	int visited_first = 0;
	double min_val = 0;
	int min_idx = 0;

	for (int i = 0; i < size; i++) {
		if (values[i] > 0) {
			if (visited_first == 0) {
				min_val = values[i];
				min_idx = i;
				visited_first = 1;
			}
			if (values[i] < min_val) {
				min_val = values[i];
				min_idx = i;
			}
		}
	}
	return min_val;
}

///////////////////////////////////////

double find_max_u64(double* values, int size) {
	int visited_first = 0;
	double max_val = 0;
	int max_idx = 0;

	for (int i = 0; i < size; i++) {
		if (values[i] > 0) {
			if (visited_first == 0) {
				max_val = values[i];
				max_idx = i;
				visited_first = 1;
			}
			if (values[i] > max_val) {
				max_val = values[i];
				max_idx = i;
			}
		}
	}
	return max_val;
}

///////////////////////////////////////

int remove_elements_from_ends(double* cc_array_in, int n_points, int n) {
	size_t cc_array_size = n_points * sizeof(double);
	double * cc_array = (double*) malloc(cc_array_size);
	memset(cc_array, 0x00, cc_array_size);
	int n_valid = 0;

	for (int i = 0; i < n_points; i++) {
		if (cc_array_in[i] <= 0)
			continue;
		cc_array[n_valid] = cc_array_in[i];
		n_valid++;
	}

	for (int i = 0; i < n_valid; i++) {
		for (int j = 0; j < i; j++) {
			if (cc_array[j] > cc_array[i]) {
				double t = cc_array[i];
				cc_array[i] = cc_array[j];
				cc_array[j] = t;
			}
		}
	}
	double lower_bound = cc_array[n], upper_bound = cc_array[n_valid - n];

	for (int i = 0; i < n_points; i++) {
		if ((cc_array_in[i] > upper_bound) || (cc_array_in[i] < lower_bound))
			cc_array_in[i] = -1;
	}

	free(cc_array);

	return EXIT_SUCCESS;
}

///////////////////////////////////////

int optimize_ecc(int *sort_results, const param_list_t * param_list,
		const int sort_metric, const int n_points, int *repeat) {
//   = RATIONAL_PROGRAM_PARAM_Exec_cycles_app;
	size_t cc_array_size = n_points * sizeof(double);
	double * cc_array = (double*) malloc(cc_array_size);
	memset(cc_array, 0x00, cc_array_size);

	//only sort positive values; disqualify all the negative values.
	for (int i = 0; i < n_points; i++) {
		//the if is critical to avoid overwriting the existing sort_results.
		if (sort_results[i] != -1) {
			mpq_t * v_i =
					&((param_list[sort_results[i]]).mpq_values[sort_metric]);
			if (mpq_sgn(*v_i) < 0) {
				sort_results[i] = -1;
				continue;
			}
			cc_array[i] = (double) mpq_get_ui(*v_i);
//	  printf ("cc_assigned=%f\n", cc_array[i]);
		}
	}

	/*
	 * 1. normalize the values to range (1,100)
	 * 2. compute the histogram
	 * 3. remove all except the most represented category of points
	 * 4. find the median value.
	 */

	double y_min = 1, y_max = 199;
	double x_min = find_min_u64(cc_array, n_points);
	double x_max = find_max_u64(cc_array, n_points);
	printf("min_x=%.0f, max_x=%.0f\n", x_min, x_max);
	double m = (y_max - y_min) / (x_max - x_min);

	//exterme case that might happen if the threshold for dominance_percentage
	//(repeat) is very low!
	if (x_min >= x_max) {
		*repeat = 0;
		free(cc_array);
		return EXIT_SUCCESS;
	}
	for (int i = 0; i < n_points; i++) {
		cc_array[i] = y_min + m * (cc_array[i] - x_min);
	}

	//WARNING: small bins = larger number of bins -> reduces the accuracy!
	double bin_size = 20;
	int n_bins = (int) ((y_max + bin_size - 1) / bin_size);
	size_t hist_size = (n_bins * sizeof(int));
	int *hist = (int*) malloc(hist_size);
	memset(hist, 0x00, hist_size);

	for (int i = 0; i < n_points; i++) {
		if (cc_array[i] <= 0)
			continue;
		int bin_idx = (int) (cc_array[i] / bin_size);
//      printf ("bin idx=%d\n", bin_idx);
		hist[bin_idx]++;
	}

	int total = 0;
	for (int i = 0; i < n_bins; i++) {
		total += hist[i];
	}

	//computing the histogram for determining the dominant clock cycles.
	int dominant_idx = 0;
	for (int i = 0; i < n_bins; i++) {
//      printf ("hist[%d]=%d\n", i, hist[i]);
		if (hist[i] > hist[dominant_idx]) {
			dominant_idx = i;
		}
	}
	float dominance_percentage = (100.0 * hist[dominant_idx]) / (1.0 * total);
	if (dominance_percentage >= 80.0)
		*repeat = 1;
	else
		*repeat = 0;

	printf("dominant_idx=%d\n", dominant_idx);
	double lower_bound, upper_bound;
	lower_bound = (dominant_idx - 1) * bin_size;
	upper_bound = (dominant_idx + 1) * bin_size;

	//remove all the values less than the lower bound or higher than upper bound.
	for (int i = 0; i < n_points; i++) {
		if (sort_results[i] == -1)
			continue;
		if ((cc_array[i] > upper_bound) || (cc_array[i] < lower_bound)) {
			sort_results[i] = -1;
			cc_array[i] = -1;
		}
	}

	int n_valid = 0;
	for (int i = 0; i < n_points; i++) {
		if (sort_results[i] == -1)
			continue;
//      printf ("cc[%d]=%f\n", i, cc_array[i]);
		n_valid++;
	}

	if (n_valid > 8) {
		int quarter_size = (n_valid) / 4.0;
		remove_elements_from_ends(cc_array, n_points, quarter_size);
	}

	free(cc_array);
	free(hist);
//  exit(EXIT_FAILURE);
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int extract_most_popular_occupancy(int *sort_results,
		const param_list_t * param_list, const int n_points) {

	int ascending = 1;
	int max_valid_rank = 0;
	int sort_metric = RATIONAL_PROGRAM_PARAM_occupancy;
	sort_by_param(sort_results, param_list, n_points, sort_metric, ascending,
			max_valid_rank);

	size_t n_bytes = 101 * sizeof(int);
	int *occ_count = (int *) malloc(n_bytes);
	memset(occ_count, 0x00, n_bytes);

	mpq_t val_qq;
	mpq_init(val_qq);

//  mpq_t max_val_qq_x100;
//  mpq_init(max_val_qq_x100);

	for (int i = 0; i < n_points; i++) {
		/////////////////////////////////
		if (sort_results[i] == -1)
			continue;

//	  printf ("check i,j=[%d,%d]\n", i, j);
		/////////////////////////////////
		mpq_t * v_i = &((param_list[sort_results[i]]).mpq_values[sort_metric]);

		mpq_set_ui(val_qq, 100, 1);
		mpq_mul(val_qq, val_qq, *v_i);
		int val = mpq_get_ui(val_qq);
//      printf ("val=%d\n", val);
		occ_count[val]++;
		/////////////////////////////////
//	  printf ("cmp %d %d\n", i, j
	}

	int max_occ_idx = -1;
	int max_occ_count = -1;

	int max_occ_idx_2nd = -1;
	int max_occ_count_2nd = -1;
	for (int i = 0; i < 101; i++) {
		if (occ_count[i]) {
//	  printf ("n_occ[%d]=%d\n", i, occ_count[i]);
			if (occ_count[i] > max_occ_count) {
				max_occ_count = occ_count[i];
				max_occ_idx = i;
			}
		}
	}

	for (int i = 0; i < 101; i++) {
		if (occ_count[i]) {
#if VERBOSE_RP
			printf ("n_occ[%d]=%d\n", i, occ_count[i]);
#endif
			if ((occ_count[i] < max_occ_count)
					&& (occ_count[i] > max_occ_count_2nd)) {
				max_occ_count_2nd = occ_count[i];
				max_occ_idx_2nd = i;
			}
		}
	}
#if VERBOSE_RP
	printf ("max_occ_idx=%d\n", max_occ_idx);
#endif
//  mpq_set_ui(max_val_qq_x100, max_occ_idx, 1);

	for (int i = 0; i < n_points; i++) {
		/////////////////////////////////
		if (sort_results[i] == -1)
			continue;

		//	  printf ("check i,j=[%d,%d]\n", i, j);
		/////////////////////////////////
		mpq_t * v_i = &((param_list[sort_results[i]]).mpq_values[sort_metric]);

		mpq_set_ui(val_qq, 100, 1);
		mpq_mul(val_qq, val_qq, *v_i);
		int val = mpq_get_ui(val_qq);
//      if (val < max_occ_idx)
		if (val < max_occ_idx_2nd) {
			sort_results[i] = -1;
#if VERBOSE_RP
			printf ("disabling i=%d with occ=%d\n", i, val);
#endif
		}
		/////////////////////////////////
		//	  printf ("cmp %d %d\n", i, j
	}

	mpq_clear(val_qq);
	free(occ_count);
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int compute_griddim_from_blockdim(char * kernel_name, int n, int * blockdim,
		int *griddim) {
	int verbose = 0;

	char conf_path[1024];
	sprintf(conf_path, "kernel_%s_gridconf.conf", kernel_name);

	char klapator_path[1024];
	sprintf(klapator_path, "%s", getenv("KLARAPTOR_PATH"));

	char cmd[1024];
	sprintf(cmd, "python %s/utils/parse_gridconf.py %s %d %d %d %d",
			klapator_path, conf_path, n, blockdim[0], blockdim[1], blockdim[2]);

	FILE *fp;
	char buffer[1024];

	/* Open the command for reading. */
	fp = popen(cmd, "r");
	if (fp == NULL) {
		printf("[@rp][ERROR: Failed to run command]...\n");
		exit(EXIT_FAILURE);
	}

	const char start_pattern[64] = "[eval][gx, gy, gz]";
	int pattern_found = 0;
	while (pattern_found == 0) {
		if (fgets(buffer, sizeof(buffer) - 1, fp) != NULL)
			if (strstr(buffer, start_pattern) != NULL) {
				pattern_found = 1;
				break;
			}
	}

	int gx = 1, gy = 1, gz = 1;
	if (pattern_found == 1)
		if (fgets(buffer, sizeof(buffer) - 1, fp) != NULL) {
			sscanf(buffer, "%d, %d, %d", &gx, &gy, &gz);
		}
	pclose(fp);

	if (verbose)
		printf("[@compute_griddim_from_blockdim][gx,gy,gz][%d,%d,%d]\n", gx, gy,
				gz);
	griddim[0] = gx;
	griddim[1] = gy;
	griddim[2] = gz;

	return EXIT_SUCCESS;

}

///////////////////////////////////////

int process_dominant_points(int *dominant_points, int *dominant_point_idx_in) {
	int dominant_point_idx = *dominant_point_idx_in;

	char klaraptor_path[PATH_MAX];
	sprintf(klaraptor_path, "%s", getenv("KLARAPTOR_PATH"));
	char cmd_path[1024];
	sprintf(cmd_path, "python2 %s/mcwp/lattice_points/lattice.py",
			klaraptor_path);

	const output_path = "dominant_points.txt";
	FILE * output_file = fopen(output_path, "w");

	int mask = (1 << 10) - 1;
	for (int i = 0; i < dominant_point_idx; i++) {
		int p = dominant_points[i];
		int bx2 = 0, by2 = 0, bz2 = 0;
		bz2 = (p & mask);
		p >>= 10;
		by2 = (p & mask);
		p >>= 10;
		bx2 = (p & mask);
		fprintf(output_file, "%d, %d, %d\n", bx2, by2, bz2);
#if VERBOSE_RP
		fprintf (stdout, "%d, %d, %d\n", bx2, by2, bz2);
#endif
	}
	fclose(output_file);

	char cmd[PATH_MAX];
	sprintf(cmd, "%s %s", cmd_path, output_path);
	int stat = system(cmd);
//  if (stat!=EXIT_SUCCESS)
//    return EXIT_FAILURE;

	char input_path[PATH_MAX];
	sprintf(input_path, "%s.sorted", output_path);

	int n = get_num_lines_in_file(input_path);
//  printf ("n=%d\n", n);
	FILE*input_file = fopen(input_path, "r");
	for (int i = 0; i < n; i++) {
		int bx2 = 0, by2 = 0, bz2 = 0;
		fscanf(input_file, "%d %d %d", &bx2, &by2, &bz2);
#if VERBOSE_RP
		printf ("just read bx2=%d, by2=%d, bz2=%d\n", bx2, by2, bz2);
#endif
		int p = (bx2 << 20) + (by2 << 10) + bz2;
		dominant_points[i] = p;
	}

	fclose(input_file);
	for (int i = n; i < dominant_point_idx; i++)
		dominant_points[i] = 0;
	dominant_point_idx = n;
	*dominant_point_idx_in = dominant_point_idx;

	return EXIT_SUCCESS;
}

///////////////////////////////////////

int is_dominant_point(const int bx2, const int by2, const int bz2,
		const int *dominant_points, const int dominant_point_idx) {

	int p = (bx2 << 20) + (by2 << 10) + bz2;
	for (int i = 0; i < dominant_point_idx; i++)
		if (p == dominant_points[i])
			return EXIT_SUCCESS;

	return EXIT_FAILURE;
}

///////////////////////////////////////

int dump_mcwp_estimates_to_file(const int n, const param_list_t* rp_param_list,
		const point_set_t * point_set, const char * kernel_name,
		const int sm_cc) {

	int n_points = point_set->size;
	size_t result_nbytes_per_point = 64; //64 character for [%d][%d, %d, %d][%d] (n)(bx, by, bz)(estimated_cc)
	size_t buffer_size = n_points * result_nbytes_per_point;
	char * buffer = (char*) malloc(buffer_size);
	single_point_t * ptr;
	int buffer_idx = 0;
//  buffer_idx += sprintf (buffer, "[N][bx, by, bz][ECC]\n");
	for (int i = 0; i < n_points; i++) {
		ptr = &point_set->values[i];
		uint64_t ecc =
				mpq_get_u64(
						rp_param_list[i].mpq_values[RATIONAL_PROGRAM_PARAM_Exec_cycles_app]);
		buffer_idx += sprintf(buffer + buffer_idx, "[%d][%d, %d, %d][%llu]\n",
				ptr->N[0], ptr->N[1], ptr->N[2], ptr->N[3], ecc);
	}

	char path[PATH_MAX];
	sprintf(path, "kernel_%s_n%06d_ecc_by_dp.tmp", kernel_name, n);
	FILE* f = fopen(path, "w");
	fwrite(buffer, 1, buffer_idx, f);
	fclose(f);
	free(buffer);

	return EXIT_SUCCESS;
}

///////////////////////////////////////

int check_profiling_validity(param_list_t*profiling_params) {
	for (int i = 0; i < PROFILING_PARAMS_ENUM_SIZE; i++) {
		if (i == PROFILING_PARAM_shared_mem_bytes_dynamic)
			continue;
		if (mpq_cmp_zero(profiling_params->mpq_values[i]) <= 0) {
			printf("invalid profiling param %d\n", i);
			return EXIT_FAILURE;
		}
	}
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int count_valid_entries(int *sort_results, int n_points) {
	int cnt = 0;
	for (int i = 0; i < n_points; i++) {
		if (sort_results[i] != -1)
			cnt++;
	}
	return cnt;
}

///////////////////////////////////////

/**
 * previously, we could not measure occupancy accurately. That
 * also includes inaccurate count of number of active warps per sm.
 */
int get_best_config(int n, int verbose) {

//  char * kernel_name, int n_var, char* sm_arch_str,
//  int n_register_in, device_params_t* device_params;

	int current_n = n;
/////////////////////////////////////
	int mcwp_error;
	kernel_info_t *ki = (kernel_info_t*) malloc(sizeof(kernel_info_t));
	mcwp_error = kernel_info_init(ki);
	check_mcwp_error(mcwp_error, "kernel_info_init");

	/////////////////////////////////////
#if VERBOSE_RP
	printf ("[kernel_name=%s]\n", ki->kernel_name);
	printf ("[device_name=%s]\n", ki->device_name);
#endif
	/////////////////////////////////////
	int sm_cc = ki->device_compute_capability;
	int arch_params[5];
	mcwp_error = set_arch_params(&arch_params, sm_cc);

	/////////////////////////////////////
	param_list_t * device_params = (param_list_t*) malloc(sizeof(param_list_t));
	param_list_t * ptx_params = (param_list_t*) malloc(sizeof(param_list_t));

	mcwp_error = param_list_init(device_params, PARAM_LIST_TYPE_DEVICE);
	check_mcwp_error(mcwp_error, "param_list_init: device");

	mcwp_error = param_list_init(ptx_params, PARAM_LIST_TYPE_PTX);
	check_mcwp_error(mcwp_error, "param_list_init: ptx");

	mcwp_error = device_params_init_from_const_polys(device_params);
	check_mcwp_error(mcwp_error, "device_params_init_from_const_polys");

	mcwp_error = ptx_params_init_from_const_polys(ptx_params);
	check_mcwp_error(mcwp_error, "ptx_params_init_from_const_polys");

	int n_register_per_thread = mpq_get_ui(
			ptx_params->mpq_values[PTX_PARAM_registers]);
	int n_shared_mem_static_bytes = mpq_get_ui(
			ptx_params->mpq_values[PTX_PARAM_shared_mem_bytes_static]);

	/////////////////////////////////////
	size_t n_var_size = ki->nvar * sizeof(int);

	/////////////////////////////////////
	//// make a copy of mesh point list. This works better as we need to
	//// change the value of "n" to "current_n" we are optimizing for.
	point_set_t * point_set = (point_set_t*) malloc(sizeof(point_set_t));
	point_set_copy(point_set, ki->eval_point_set);
	/////////////////////////////////////
	//setting "n" to current_n
	for (int i = 0; i < ki->eval_point_set->size; i++) {
		point_set->values[i].N[0] = current_n;
	}
	/////////////////////////////////////
//  int * diff_block = (int*) malloc (ki->eval_point_set->size * sizeof(int));
//  int * diff_warp = (int*) malloc (ki->eval_point_set->size * sizeof(int));
//  for (int i = 0; i < ki->eval_point_set->size; i++)
//    diff_block[i] = -1;
//  for (int i = 0; i < ki->eval_point_set->size; i++)
//    diff_warp[i] = -1;

	/////////////////////////////////////
	param_list_t* profiling_param_list = (param_list_t*) malloc(
			ki->eval_point_set->size * sizeof(param_list_t));

#if ALLOW_OMP_LOOPS
#pragma omp parallel for
#endif
	for (int i = 0; i < ki->eval_point_set->size; i++) {
		mcwp_error = param_list_init(&profiling_param_list[i],
				PARAM_LIST_TYPE_PROFILING);
		check_mcwp_error(mcwp_error, "param_list_init: profiling");
		profiling_param_list_evaluate_for_mesh_point(&profiling_param_list[i],
				&point_set->values[i]);

//      int bx=(point_set->values[i].N[1]);
//      int by=(point_set->values[i].N[2]);
//      int bz=(point_set->values[i].N[3]);
//      printf("bx=%d, by=%d, bz=%d\n", bx,by,bz);

	}
//  exit(EXIT_FAILURE);

	int max_n_active_blocks_per_sm = arch_params[ARCH_PARAM_MAX_BLOCKS_PER_SM];
	int max_n_active_warps_per_sm = arch_params[ARCH_PARAM_MAX_WARPS_PER_SM];
//  for (int i = 0; i < ki->eval_point_set->size; i++)
//    {
//
//      int n_Active_blocks_per_SM =
//	  mpq_get_ui (
//	      profiling_param_list[i].mpq_values[PROFILING_PARAM_Active_blocks_per_SM]);
//
//      int n_warps_per_sm =
//	  mpq_get_ui (
//	      profiling_param_list[i].mpq_values[PROFILING_PARAM_Active_warps_per_SM]);
//
//      if (n_Active_blocks_per_SM > max_n_active_blocks_per_sm)
//	{
//	  max_n_active_blocks_per_sm = n_Active_blocks_per_SM;
//	}
//
//      if (n_warps_per_sm > max_n_active_warps_per_sm)
//	{
//	  max_n_active_warps_per_sm = n_warps_per_sm;
//	}
//    }
//  max_n_active_blocks_per_sm = min (max_n_active_blocks_per_sm,
//				    arch_params[ARCH_PARAM_MAX_BLOCKS_PER_SM]);
//  max_n_active_warps_per_sm = min (max_n_active_warps_per_sm,
//				   arch_params[ARCH_PARAM_MAX_WARPS_PER_SM]);

//  printf ("max_n_blocks =%d, max_n_warps=%d\n", max_n_active_blocks_per_sm,
//	  max_n_active_warps_per_sm);

	uint64_t * n_warps_list = (uint64_t*) malloc(
			ki->eval_point_set->size * sizeof(uint64_t));
	memset(n_warps_list, 0x00, ki->eval_point_set->size * sizeof(uint64_t));

#if ALLOW_OMP_LOOPS
#pragma omp parallel for
#endif
	for (int i = 0; i < ki->eval_point_set->size; i++) {
//      mcwp_error = param_list_init (&profiling_param_list[i],
//				    PARAM_LIST_TYPE_PROFILING);
//      check_mcwp_error(mcwp_error, "param_list_init: profiling");
//      profiling_param_list_evaluate_for_mesh_point (&profiling_param_list[i],
//						    &point_set->values[i]);
		int n_dynamic_shared_mem_bytes =
				mpq_get_ui(
						profiling_param_list[i].mpq_values[PROFILING_PARAM_shared_mem_bytes_dynamic]);

		int n_total_shared_mem_bytes = n_shared_mem_static_bytes
				+ n_dynamic_shared_mem_bytes;

		//we cannot rely on the interpolation for number of active blocks and active SM's.
		int n_Active_blocks_per_SM = 1;
//	  mpq_get_ui (
//	      profiling_param_list[i].mpq_values[PROFILING_PARAM_Active_blocks_per_SM]);

		int n_warps_per_sm = 1;
//	  mpq_get_ui (
//	      profiling_param_list[i].mpq_values[PROFILING_PARAM_Active_warps_per_SM]);

		int block_dims[3];
		int grid_dims[3] = { 1, 1, 1 };
		block_dims[0] = point_set->values[i].N[1];
		block_dims[1] = point_set->values[i].N[2];
		block_dims[2] = point_set->values[i].N[3];

		compute_griddim_from_blockdim(ki->kernel_name, current_n, block_dims,
				grid_dims);

		int n_blocks = grid_dims[0] * grid_dims[1] * grid_dims[2];
		int n_sm = mpq_get_ui(
				device_params->mpq_values[DEVICE_PARAM_Active_SMs]);

		int B = block_dims[0] * block_dims[1] * block_dims[2];
//      int n_warps=n_blocks*((B+31)/32);

//      printf("n_sm=%d, n_Active_blocks_per_SM=%d\n", mpq_get_ui (device_params->mpq_values[DEVICE_PARAM_Active_SMs]), n_Active_blocks_per_SM);

		int old_n_blocks = n_Active_blocks_per_SM;
		int old_n_warps_per_sm = n_warps_per_sm;

//      int *error_block = &diff_block[i];
//      int *error_warp = &diff_warp[i];

		mcwp_error = compute_n_active_blocks_per_sm(&n_Active_blocks_per_SM,
				&n_warps_per_sm, NULL, NULL, n_blocks, n_sm,
				&point_set->values[i], n_register_per_thread, arch_params,
				n_dynamic_shared_mem_bytes);

		check_mcwp_error(mcwp_error, "adjust_n_active_blocks_per_sm");

		mpq_set_ui(
				profiling_param_list[i].mpq_values[PROFILING_PARAM_Active_blocks_per_SM],
				n_Active_blocks_per_SM, 1);

		mpq_set_ui(
				profiling_param_list[i].mpq_values[PROFILING_PARAM_Active_warps_per_SM],
				n_warps_per_sm, 1);

		int n_warps = n_Active_blocks_per_SM * ((B + 31) / 32);
		n_warps_list[i] = n_warps;
		n_warps_list[i] = n_warps_per_sm;

//      printf ("[point %d][adjusted n_blocks_per_sm][%4d]->[%4d]\n", i,
//	      old_n_blocks, n_Active_blocks_per_SM);
//      printf ("[point %d][adjusted n_warps_per_sm][%4d]->[%4d]\n", i,
//	      old_n_warps_per_sm, n_warps_per_sm);
	}

	/////////////////////////////////////
	//init param_list for rp values:
	param_list_t* rp_param_list = (param_list_t*) malloc(
			ki->eval_point_set->size * sizeof(param_list_t));

#if ALLOW_OMP_LOOPS
#pragma omp parallel for
#endif
	for (int i = 0; i < ki->eval_point_set->size; i++) {
		mcwp_error = param_list_init(&rp_param_list[i],
				PARAM_LIST_TYPE_RATIONAL_PROGRAM);
		check_mcwp_error(mcwp_error, "param_list_init: rp");
	}

	/////////////////////////////////////
	//sorted based on estimated: exec cycles, and occupancy.
	int * sort_results_by_clock_cycles; //index of sorted elements.
//  int * sort_results_by_occupancy; //index of sorted elements.
//  int * sort_results_by_n_warps_per_sm; //index of sorted elements.
	size_t sort_list_size = ki->eval_point_set->size * sizeof(int);
	sort_results_by_clock_cycles = (int*) malloc(sort_list_size);
//  sort_results_by_occupancy = (int*) malloc (sort_list_size);
//  sort_results_by_n_warps_per_sm = (int*) malloc (sort_list_size);

	for (int i = 0; i < ki->eval_point_set->size; i++) {
		sort_results_by_clock_cycles[i] = i;
	}

	/////////////////////////////////////
	for (int i = 0; i < ki->eval_point_set->size; i++) {
		int valid_profiling = check_profiling_validity(
				&profiling_param_list[i]);
		if (valid_profiling != EXIT_SUCCESS)
			sort_results_by_clock_cycles[i] = -1;
	}

	//run mcwp for each point.
#if ALLOW_OMP_LOOPS
#pragma omp parallel for
#endif
	for (int i = 0; i < ki->eval_point_set->size; i++) {
		if (sort_results_by_clock_cycles[i] == -1)
			continue;
		int bx, by, bz;
		bx = point_set->values[i].N[1];
		by = point_set->values[i].N[2];
		bz = point_set->values[i].N[3];
		int block_size = (bx * by * bz);
//      int warp_per_block = (block_size + 31) / 32;
		int warp_per_block = (block_size + 31) >> 5;

		mcwp_error = compute_mwp_cwp_values_for_single_mesh_point_in_dp(
				&rp_param_list[i], &profiling_param_list[i], ptx_params,
				device_params, arch_params, warp_per_block);
		check_mcwp_error(mcwp_error,
				"compute_mwp_cwp_values_for_single_mesh_point_in_dp");

		mpq_ceil(
				rp_param_list[i].mpq_values[RATIONAL_PROGRAM_PARAM_Exec_cycles_app]);
	}

	/////////////////////////////////////
	dump_mcwp_estimates_to_file(n, rp_param_list, point_set, ki->kernel_name,
			sm_cc);
	/////////////////////////////////////

	int ipc_vals_bytes = ki->eval_point_set->size * sizeof(float);
	float * ipc_vals_comp = (float*) malloc(ipc_vals_bytes);
	memset(ipc_vals_comp, 0x00, ipc_vals_bytes);

	float * ipc_vals_mem = (float*) malloc(ipc_vals_bytes);
	memset(ipc_vals_mem, 0x00, ipc_vals_bytes);

	int n_valid_ipc = 0;

	float ipc_vals_mem_avg = 0.0;
	float ipc_vals_comp_avg = 0.0;
	for (int i = 0; i < ki->eval_point_set->size; i++) {
		if (sort_results_by_clock_cycles[i] == -1)
			continue;

//			int ipc_metric=PROFILING_PARAM_13_Total_insts;

//      int ipc_metric = PROFILING_PARAM_14_Comp_insts;
//       printf("ipc_vals[%d]=%.2f\n", i, ipc_vals[i]);
		uint64_t n_comp_inst =
				mpq_get_u64(
						profiling_param_list[i].mpq_values[PROFILING_PARAM_14_Comp_insts]);
		uint64_t n_mem_inst =
				mpq_get_u64(
						profiling_param_list[i].mpq_values[PROFILING_PARAM_15_Mem_insts]);
		uint64_t ecc =
				mpq_get_u64(
						rp_param_list[i].mpq_values[RATIONAL_PROGRAM_PARAM_Exec_cycles_app]);

		ipc_vals_comp[i] = (1.0 * n_warps_list[i] * n_comp_inst) / (1.0 * ecc);
		ipc_vals_mem[i] = (1.0 * n_warps_list[i] * n_mem_inst) / (1.0 * ecc);

		ipc_vals_comp_avg += ipc_vals_comp[i];
		ipc_vals_mem_avg += ipc_vals_mem[i];
		n_valid_ipc++;
//      printf ("n_inst=%d, ipc_val=%.2f\n", n_inst, ipc_vals[i])	
		mpq_set_ui(
				profiling_param_list[i].mpq_values[PROFILING_PARAM_14_Comp_insts],
				10000.0 * (ipc_vals_comp[i]),1);
		mpq_ceil(
				profiling_param_list[i].mpq_values[PROFILING_PARAM_14_Comp_insts]);
	}
	/////////////////////////////////////
//  ipc_vals_mem_avg/=(1.0*n_valid_ipc);
//  ipc_vals_comp_avg/=(1.0*n_valid_ipc);
	float ipc_avg_ratio = ipc_vals_comp_avg / ipc_vals_mem_avg;
	printf("mem_avg=%f, comp_avg=%f, ratio=%f\n", ipc_vals_mem_avg,
			ipc_vals_comp_avg, ipc_avg_ratio);
	int is_compute_bound = 0;
	int compute_bound_ratio = 4;

	if (ipc_avg_ratio > compute_bound_ratio)
		is_compute_bound = 1;	 //compute bound

	if ((ipc_avg_ratio <
				compute_bound_ratio)&&(ipc_avg_ratio>=(compute_bound_ratio/2.0)))
		is_compute_bound = 2;	//50-50

	if ((ipc_avg_ratio<(compute_bound_ratio/2.0)))
		is_compute_bound = 0;	//memory bound

//  if (ipc_avg_ratio >= 4.0)
//    is_compute_bound = 4;
//  if (ipc_avg_ratio >= 6.0)
//    is_compute_bound = 6;
	/////////////////////////////////////

//  mcwp_error = sort_by_param (sort_results_by_clock_cycles,
//			      profiling_param_list, ki->eval_point_set->size,
//			      PROFILING_PARAM_Active_blocks_per_SM, 1,
//			      max_rank_n_warps_per_sm);
//
//  mcwp_error = sort_by_param (sort_results_by_clock_cycles, rp_param_list,
//			      ki->eval_point_set->size,
//			      RATIONAL_PROGRAM_PARAM_Exec_cycles_app, 1, 0);
//
//  /////////////////////////////////////

//	int n_valid=ki->eval_point_set->size;

	//// remove points until reaching 20% of the initial population.
//	int threshold=(int)(1.0*ki->eval_point_set->size*0.20);

//  mcwp_error = sort_by_param (sort_results_by_clock_cycles,
//			      profiling_param_list, ki->eval_point_set->size,
//			      PROFILING_PARAM_dynamic_Mem_LD, 1, 10);

	int repeat_optimization = 0;

	//50-50 case
	if (is_compute_bound == 2) {
  
  	printf("50-50CASE\n");
  //min dynamic memld;
  mcwp_error = sort_by_param (sort_results_by_clock_cycles,
			      profiling_param_list, ki->eval_point_set->size,
			      PROFILING_PARAM_dynamic_Mem_LD, 1, 1);

	//max active warps per sm
	mcwp_error = sort_by_param (sort_results_by_clock_cycles,
			      profiling_param_list, ki->eval_point_set->size,
			      PROFILING_PARAM_Active_warps_per_SM, 0, 1);
	//minimum ipc
	mcwp_error = sort_by_param (sort_results_by_clock_cycles,
			      profiling_param_list, ki->eval_point_set->size,
			      PROFILING_PARAM_14_Comp_insts, 1, 1);
	
	//min active blocks per sm
	mcwp_error = sort_by_param (sort_results_by_clock_cycles,
			      profiling_param_list, ki->eval_point_set->size,
			      PROFILING_PARAM_Active_blocks_per_SM, 1, 1);
	}
	if (is_compute_bound==0)
	{
		printf("MEMORY BOUND!\n"); 

		//optimize MEM_LD
		repeat_optimization = 1;
    while (repeat_optimization)
		{
			mcwp_error = optimize_ecc(sort_results_by_clock_cycles,
					profiling_param_list, PROFILING_PARAM_dynamic_Mem_LD,
					ki->eval_point_set->size, &repeat_optimization);
		}


		repeat_optimization = 1;
    while (repeat_optimization)
		{
			mcwp_error = optimize_ecc(sort_results_by_clock_cycles,
					rp_param_list, RATIONAL_PROGRAM_PARAM_Exec_cycles_app,
					ki->eval_point_set->size, &repeat_optimization);
		}

  //min dynamic memld;
  mcwp_error = sort_by_param (sort_results_by_clock_cycles,
			      profiling_param_list, ki->eval_point_set->size,
			      PROFILING_PARAM_dynamic_Mem_LD, 1, 1);

	}

	if (is_compute_bound==1)
	{
		printf("COMPUTE_BOUND\n");

	//min dynamic memld;
  mcwp_error = sort_by_param (sort_results_by_clock_cycles,
			      profiling_param_list, ki->eval_point_set->size,
			      PROFILING_PARAM_dynamic_Mem_LD, 1, 5);


	mcwp_error = sort_by_param (sort_results_by_clock_cycles,
			      rp_param_list, ki->eval_point_set->size,
			      RATIONAL_PROGRAM_PARAM_Exec_cycles_app, 1, 5);
	}

		//optimize IPC
//		repeat_optimization = 1;
//    while (repeat_optimization)
//		{
//			mcwp_error = optimize_ecc(sort_results_by_clock_cycles,
//					profiling_param_list, PROFILING_PARAM_14_Comp_insts,
//					ki->eval_point_set->size, &repeat_optimization);
//		}
//
//  repeat_optimization = 1;
//  while (repeat_optimization)
//    {
//      mcwp_error = optimize_ecc (sort_results_by_clock_cycles, rp_param_list,
//				 RATIONAL_PROGRAM_PARAM_occupancy,
//				 ki->eval_point_set->size,
//				 &repeat_optimization);
//    }

		////optimize ecc
//      repeat_optimization = 1;
//  while (repeat_optimization)
//  for (int i=0;i<2;i++)
//	{
//		repeat_optimization=1;
//	  mcwp_error = optimize_ecc (sort_results_by_clock_cycles,
//				     rp_param_list,
//				     RATIONAL_PROGRAM_PARAM_Exec_cycles_app,
//				     ki->eval_point_set->size,
//				     &repeat_optimization);
//	}

///}	//optimize MEMLD
//      repeat_optimization = 1;
//      while (repeat_optimization)
//	{
//		mcwp_error = optimize_ecc(sort_results_by_clock_cycles,
//				profiling_param_list, PROFILING_PARAM_dynamic_Mem_LD,
//				ki->eval_point_set->size, &repeat_optimization);
////	}

		////optimize ecc
		//      repeat_optimization = 1;
		////  while (repeat_optimization)
		//	{
//		mcwp_error = optimize_ecc(sort_results_by_clock_cycles, rp_param_list,
//				RATIONAL_PROGRAM_PARAM_Exec_cycles_app,
//				ki->eval_point_set->size, &repeat_optimization);
//		//	}
//		}

		//keep all with active blocks in between
//      mcwp_error = sort_by_param (sort_results_by_clock_cycles,
//				  profiling_param_list,
//				  ki->eval_point_set->size,
//				  PROFILING_PARAM_Active_blocks_per_SM, 0, 2);
//
//      mcwp_error = sort_by_param (sort_results_by_clock_cycles,
//				  profiling_param_list,
//				  ki->eval_point_set->size,
//				  PROFILING_PARAM_Active_blocks_per_SM, 1, 1);
//
//      mcwp_error = sort_by_param (sort_results_by_clock_cycles, rp_param_list,
//				  ki->eval_point_set->size,
//				  RATIONAL_PROGRAM_PARAM_Exec_cycles_app, 1, 0);

//
//////  mcwp_error = sort_by_param (sort_results_by_clock_cycles,
//////			      profiling_param_list, ki->eval_point_set->size,
//////			      PROFILING_PARAM_dynamic_Mem_LD, 1, 10);///
//////
////
		//optimize Active blocks per sm
//	repeat_optimization = 1;
//  while (repeat_optimization)
//    {
//      mcwp_error = optimize_ecc (sort_results_by_clock_cycles,
//      		profiling_param_list,
//      		PROFILING_PARAM_Active_blocks_per_SM,
//				 ki->eval_point_set->size,
//				 &repeat_optimization);
//    }

//	repeat_optimization = 1;
//  while (repeat_optimization)
//    {
//      mcwp_error = optimize_ecc (sort_results_by_clock_cycles, rp_param_list,
//      		RATIONAL_PROGRAM_PARAM_Exec_cycles_app,
//				 ki->eval_point_set->size,
//				 &repeat_optimization);
//    }

/////
//	for(int i=64;i>=1;i--)
//	{
//  	mcwp_error = sort_by_param (sort_results_by_clock_cycles,
//			      profiling_param_list, ki->eval_point_set->size,
//			      PROFILING_PARAM_Active_blocks_per_SM, (i+1)%2, i);
//	}

		////	remove one element from each end until nothing can be removed anymore!
//	for(int i=64;i>1;i--)
//	{
//  	mcwp_error = sort_by_param (sort_results_by_clock_cycles,
//			      profiling_param_list, ki->eval_point_set->size,
//			      PROFILING_PARAM_Active_blocks_per_SM, (i)%2, i);
//	}
//		mcwp_error = sort_by_param(sort_results_by_clock_cycles, rp_param_list,
//				ki->eval_point_set->size,
//				RATIONAL_PROGRAM_PARAM_Exec_cycles_app, 1, 0);
////
//  mcwp_error = sort_by_param (sort_results_by_clock_cycles, rp_param_list,
//			      ki->eval_point_set->size,
//			      RATIONAL_PROGRAM_PARAM_occupancy, 0, 1);
//
//	repeat_optimization = 1;
//  while (repeat_optimization)
//    {
//      mcwp_error = optimize_ecc (sort_results_by_clock_cycles, rp_param_list,
//      		RATIONAL_PROGRAM_PARAM_Exec_cycles_app,
//				 ki->eval_point_set->size,
//				 &repeat_optimization);
//    }

//  mcwp_error = sort_by_param (sort_results_by_clock_cycles,
//			      profiling_param_list, ki->eval_point_set->size,
//			      PROFILING_PARAM_Active_blocks_per_SM, 1, 1);

//	}

//	if (is_compute_bound == 2) {
//
//		repeat_optimization = 1;
//		while (repeat_optimization) {
//			mcwp_error = optimize_ecc(sort_results_by_clock_cycles,
//					profiling_param_list, PROFILING_PARAM_dynamic_Mem_LD,
//					ki->eval_point_set->size, &repeat_optimization);
//		}
//
////      mcwp_error = invalidate_sort_results_by_percentage (
////	  sort_results_by_clock_cycles, rp_param_list, ki->eval_point_set->size,
////	  RATIONAL_PROGRAM_PARAM_Exec_cycles_app, 1, 20);
////
//		mcwp_error = sort_by_param(sort_results_by_clock_cycles,
//				profiling_param_list, ki->eval_point_set->size,
//				PROFILING_PARAM_Active_blocks_per_SM, 0, 1);
//
//		mcwp_error = sort_by_param(sort_results_by_clock_cycles,
//				profiling_param_list, ki->eval_point_set->size,
//				PROFILING_PARAM_Active_warps_per_SM, 0, 1);
//		mcwp_error = sort_by_param(sort_results_by_clock_cycles, rp_param_list,
//				ki->eval_point_set->size,
//				RATIONAL_PROGRAM_PARAM_Exec_cycles_app, 1, 0);
//
//		//optimize IPC
////      repeat_optimization = 1;
////      //      while (repeat_optimization)
////	{
////	  mcwp_error = optimize_ecc (sort_results_by_clock_cycles,
////				     profiling_param_list,
////				     PROFILING_PARAM_active_cycles,
////				     ki->eval_point_set->size,
////				     &repeat_optimization);
////	}
//
//		//  repeat_optimization = 1;
//		//  while (repeat_optimization)
//		//    {
//		//      mcwp_error = optimize_ecc (sort_results_by_clock_cycles, rp_param_list,
//		//				 RATIONAL_PROGRAM_PARAM_occupancy,
//		//				 ki->eval_point_set->size,
//		//				 &repeat_optimization);
//		//    }
//
//		////optimize ecc
//		//      repeat_optimization = 1;
//		////  while (repeat_optimization)
//		//	{
//		//	  mcwp_error = optimize_ecc (sort_results_by_clock_cycles,
//		//				     rp_param_list,
//		//				     RATIONAL_PROGRAM_PARAM_Exec_cycles_app,
//		//				     ki->eval_point_set->size,
//		//				     &repeat_optimization);
//		//	}
//
//		////	//optimize MEMLD
//		//      repeat_optimization = 1;
//		//      while (repeat_optimization)
//		//	{
////      mcwp_error = optimize_ecc (sort_results_by_clock_cycles,
////				 profiling_param_list,
////				 PROFILING_PARAM_dynamic_Mem_LD,
////				 ki->eval_point_set->size,
////				 &repeat_optimization);
//		//	}
//
//		////optimize ecc
//		//      repeat_optimization = 1;
//		////  while (repeat_optimization)
//		//	{
////      mcwp_error = optimize_ecc (sort_results_by_clock_cycles, rp_param_list,
////				 RATIONAL_PROGRAM_PARAM_Exec_cycles_app,
////				 ki->eval_point_set->size,
////				 &repeat_optimization);
//		//	}
//
//		//keep all with active blocks in between
//		//      mcwp_error = sort_by_param (sort_results_by_clock_cycles,
//		//				  profiling_param_list,
//		//				  ki->eval_point_set->size,
//		//				  PROFILING_PARAM_Active_blocks_per_SM, 0, 2);
//		//
//		//      mcwp_error = sort_by_param (sort_results_by_clock_cycles,
//		//				  profiling_param_list,
//		//				  ki->eval_point_set->size,
//		//				  PROFILING_PARAM_Active_blocks_per_SM, 1, 1);
//		//
//		//      mcwp_error = sort_by_param (sort_results_by_clock_cycles, rp_param_list,
//		//				  ki->eval_point_set->size,
//		//				  RATIONAL_PROGRAM_PARAM_Exec_cycles_app, 1, 0);
//
//		//
//		//////  mcwp_error = sort_by_param (sort_results_by_clock_cycles,
//		//////			      profiling_param_list, ki->eval_point_set->size,
//		//////			      PROFILING_PARAM_dynamic_Mem_LD, 1, 10);///
//		//////
//		////
//		//optimize Active blocks per sm
//		//	repeat_optimization = 1;
//		//  while (repeat_optimization)
//		//    {
//		//      mcwp_error = optimize_ecc (sort_results_by_clock_cycles,
//		//      		profiling_param_list,
//		//      		PROFILING_PARAM_Active_blocks_per_SM,
//		//				 ki->eval_point_set->size,
//		//				 &repeat_optimization);
//		//    }
//
//		//	repeat_optimization = 1;
//		//  while (repeat_optimization)
//		//    {
//		//      mcwp_error = optimize_ecc (sort_results_by_clock_cycles, rp_param_list,
//		//      		RATIONAL_PROGRAM_PARAM_Exec_cycles_app,
//		//				 ki->eval_point_set->size,
//		//				 &repeat_optimization);
//		//    }
//
//		/////
//		//	for(int i=64;i>=1;i--)
//		//	{
//		//  	mcwp_error = sort_by_param (sort_results_by_clock_cycles,
//		//			      profiling_param_list, ki->eval_point_set->size,
//		//			      PROFILING_PARAM_Active_blocks_per_SM, (i+1)%2, i);
//		//	}
//
//		////	remove one element from each end until nothing can be removed anymore!
//		//	for(int i=64;i>1;i--)
//		//	{
//		//  	mcwp_error = sort_by_param (sort_results_by_clock_cycles,
//		//			      profiling_param_list, ki->eval_point_set->size,
//		//			      PROFILING_PARAM_Active_blocks_per_SM, (i)%2, i);
//		//	}
//
//		//
//		//  mcwp_error = sort_by_param (sort_results_by_clock_cycles, rp_param_list,
//		//			      ki->eval_point_set->size,
//		//			      RATIONAL_PROGRAM_PARAM_occupancy, 0, 1);
//		//
//		//	repeat_optimization = 1;
//		//  while (repeat_optimization)
//		//    {
//		//      mcwp_error = optimize_ecc (sort_results_by_clock_cycles, rp_param_list,
//		//      		RATIONAL_PROGRAM_PARAM_Exec_cycles_app,
//		//				 ki->eval_point_set->size,
//		//				 &repeat_optimization);
//		//    }
//
//		//  mcwp_error = sort_by_param (sort_results_by_clock_cycles,
//		//			      profiling_param_list, ki->eval_point_set->size,
//		//			      PROFILING_PARAM_Active_blocks_per_SM, 1, 1);
//
//	}
//
//	if (is_compute_bound == 0) {
//		printf("memory bound!\n");
//	}
	/////////////////////////////////////
	//  double max_occupancy = 0;
	//  int max_block_size = 0;
	int best_visited_point_idx = 0;
	/////////////////////////////////////
//#if VERBOSE_RP
	for (int i = 0; i < ki->eval_point_set->size; i++) {
		//      int idx = sort_results_by_n_warps_per_sm[i];
		int idx = sort_results_by_clock_cycles[i];
		if (idx == -1)
			continue;
		int bx, by, bz;
		bx = point_set->values[idx].N[1];
		by = point_set->values[idx].N[2];
		bz = point_set->values[idx].N[3];
		int block_size = bx * by * bz;
		double occupancy =
				mpq_get_d(
						rp_param_list[idx].mpq_values[RATIONAL_PROGRAM_PARAM_occupancy]);

		//      if (i == 0)
		//	{
		//	  max_occupancy = occupancy;
		//	  max_block_size = block_size;
		//	  best_visited_point_idx = idx;
		//	}
		//
		//      if (occupancy > max_occupancy)
		//	{
		//	  max_occupancy = occupancy;
		//	  max_block_size = block_size;
		//	  best_visited_point_idx = idx;
		//	}
		//      if ((occupancy == max_occupancy) && (block_size >= max_block_size))
		//	{
		//	  max_block_size = block_size;
		//	  best_visited_point_idx = idx;
		//	}

		int cc =
				mpq_get_ui(
						rp_param_list[idx].mpq_values[RATIONAL_PROGRAM_PARAM_Exec_cycles_app]);

		int mwp = mpq_get_ui(
				rp_param_list[idx].mpq_values[RATIONAL_PROGRAM_PARAM_MWP]);

		int cwp = mpq_get_ui(
				rp_param_list[idx].mpq_values[RATIONAL_PROGRAM_PARAM_CWP]);

		int active_warps_per_sm =
				mpq_get_ui(
						profiling_param_list[idx].mpq_values[PROFILING_PARAM_Active_warps_per_SM]);
		int active_blocks_per_sm =
				mpq_get_ui(
						profiling_param_list[idx].mpq_values[PROFILING_PARAM_Active_blocks_per_SM]);
		int dynamic_mem_ld =
				mpq_get_ui(
						profiling_param_list[idx].mpq_values[PROFILING_PARAM_dynamic_Mem_LD]);
		int ipc =
				mpq_get_ui(
						profiling_param_list[idx].mpq_values[PROFILING_PARAM_active_cycles]);

		printf(
				"[%02d][N=%4d, bx=%4d, by=%4d, bz=%4d]"
						"[occupancy=%.3f][MWP=%d][CWP=%d]"
						"[active_warps_per_sm=%d][active_blocks_sm=%d][cc=%d][mem_ld=%d][ipc_c,m,R=%.2f, %.2f, %.2f]\n",
				i, current_n, bx, by, bz, occupancy, mwp, cwp,
				active_warps_per_sm, active_blocks_per_sm, cc, dynamic_mem_ld,
				100.0 * ipc_vals_comp[idx], 100.0 * ipc_vals_mem[idx],
				ipc_vals_comp[idx] / ipc_vals_mem[idx]);
	}
//#endif
	/////////////////////////////////////

	const char * reporting_banner = "[best_ec][idx, bx, by, bz]\n";
	printf("%s", reporting_banner);
	for (int i = 0; i < ki->eval_point_set->size; i++) {
//      int i = best_block_size_idx;
		int idx = sort_results_by_clock_cycles[i];
		if (idx == -1) {
			continue;
//	  printf ("EXIT_FAILURE");
//	  exit (EXIT_FAILURE);
		}
		int bx, by, bz;
		bx = point_set->values[idx].N[1];
		by = point_set->values[idx].N[2];
		bz = point_set->values[idx].N[3];
		printf("%d, %d, %d, %d\n", i, bx, by, bz);
	}

/////////////////////////////////////
	free(sort_results_by_clock_cycles);
//  free (sort_results_by_occupancy);
/////////////////////////////////////
	param_list_clear(device_params);
	param_list_clear(ptx_params);
	free(device_params);
	free(ptx_params);
	point_set_clear(point_set);
	free(point_set);

/////////////////////////////////////
	for (int i = 0; i < ki->eval_point_set->size; i++) {
		param_list_clear(&profiling_param_list[i]);
	}
	for (int i = 0; i < ki->eval_point_set->size; i++) {
		param_list_clear(&rp_param_list[i]);
	}
/////////////////////////////////////
	free(profiling_param_list);
	free(rp_param_list);
//  kernel_info_clear (ki);
	free(ki);
/////////////////////////////////////
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int main(int argc, char **argv) {
	if (argc < 2) {
		printf("--args: [1]:N [2]:VERBOSE (default=0)\n", argv[0]);
		exit(EXIT_FAILURE);
	}
	int n = atoi(argv[1]);
	int verbose = 0;
//  if (argc > 2)
//    verbose = atoi (argv[2]);

	int mcwp_error = get_best_config(n, verbose);
	check_mcwp_error(mcwp_error, "get_best_config");
	return EXIT_SUCCESS;
}

///////////////////////////////////////
