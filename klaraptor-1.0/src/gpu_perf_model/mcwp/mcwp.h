/*!
 \file mcwp.h
 \author Linxiao Wang <lwang739@uwo.ca>
 \author Davood Mohajerani <dmohajer@uwo.ca>
 \brief
 */

#ifndef MCWP_H_
#define MCWP_H_
///////////////////////////////////////
#include "mcwp_common.h"
#include "profiling_utils.h"
///////////////////////////////////////
#define MAX_N_PARAMS_TO_INTERPOLATE 1
///////////////////////////////////////
//// TODO: adding proper constant conditions on pointers and values.
///////////////////////////////////////

int compute_mwp_cwp_values_for_single_mesh_point_adjust_memLD(
		poly_params * kernel_poly_params, profiling_info_t *pi, int point_idx,
		param_list_t* device_param_list) {

	mpq_t * device_params = device_param_list->mpq_values;

	mpq_t* ptx_param = kernel_poly_params->ptx_params->mpq_values;

	mpq_t* profiling_params =
			kernel_poly_params->profiling_params_list[point_idx].mpq_values;

	mpq_t* rp_params =
			kernel_poly_params->rational_program_params_list[point_idx].mpq_values;

	mpq_t one_mpq, temp_mpq;
	mpq_inits(one_mpq, temp_mpq, NULL);
	mpq_set_si(one_mpq, 1, 1);

	int found_closest_estimate = 0;

	int min_possible_mem_LD = 100;
	int max_possible_mem_LD = 2000;

	for (int i = min_possible_mem_LD; i <= max_possible_mem_LD; i++) {

		mpq_set_ui(profiling_params[PROFILING_PARAM_dynamic_Mem_LD], i, 1);
		/////////////////////////////////////
		/////////////////////////////////////

//  compute_dynamic_mem_ld (rp_params[RATIONAL_PROGRAM_PARAM_dynamic_Mem_LD],
//			  profiling_params[PROFILING_PARAM_14_Comp_insts],
//			  profiling_params[PROFILING_PARAM_15_Mem_insts],
//			  profiling_params[PROFILING_PARAM_active_cycles]);

		//double MWP_Without_BW_full = Mem_L / Departure_delay;
		mpq_div(rp_params[RATIONAL_PROGRAM_PARAM_MWP_Without_BW_full],
				profiling_params[PROFILING_PARAM_dynamic_Mem_LD],
				device_params[DEVICE_PARAM_Departure_del_coal]);
//      mpq_ceil (rp_params[RATIONAL_PROGRAM_PARAM_MWP_Without_BW_full]);

		//double MWP_Without_BW = min2(MWP_Without_BW_full, Active_warps_per_SM);
		mpq_min2(rp_params[RATIONAL_PROGRAM_PARAM_MWP_Without_BW],
				rp_params[RATIONAL_PROGRAM_PARAM_MWP_Without_BW_full],
				profiling_params[PROFILING_PARAM_Active_warps_per_SM]);

		//double BW_per_warp = Freq * Load_bytes_per_warp / Mem_L;
		mpq_div(rp_params[RATIONAL_PROGRAM_PARAM_MWP_BW_per_warp],
				device_params[DEVICE_PARAM_Load_bytes_per_warp],
				profiling_params[PROFILING_PARAM_dynamic_Mem_LD]);
		mpq_mul(rp_params[RATIONAL_PROGRAM_PARAM_MWP_BW_per_warp],
				rp_params[RATIONAL_PROGRAM_PARAM_MWP_BW_per_warp],
				device_params[DEVICE_PARAM_Freq]);

//      mpq_ceil (rp_params[RATIONAL_PROGRAM_PARAM_MWP_BW_per_warp]);

		//double MWP_peak_BW = Mem_bandwidth / (BW_per_warp * Active_SMs);
		mpq_mul(rp_params[RATIONAL_PROGRAM_PARAM_MWP_peak_BW],
				rp_params[RATIONAL_PROGRAM_PARAM_MWP_BW_per_warp],
				device_params[DEVICE_PARAM_Active_SMs]);
		mpq_div(rp_params[RATIONAL_PROGRAM_PARAM_MWP_peak_BW],
				device_params[DEVICE_PARAM_Mem_bandwidth],
				rp_params[RATIONAL_PROGRAM_PARAM_MWP_peak_BW]);

//      mpq_ceil (rp_params[RATIONAL_PROGRAM_PARAM_MWP_peak_BW]);

		//double MWP = min3(MWP_Without_BW, MWP_peak_BW, Active_warps_per_SM);
		mpq_min3(rp_params[RATIONAL_PROGRAM_PARAM_MWP],
				rp_params[RATIONAL_PROGRAM_PARAM_MWP_Without_BW],
				rp_params[RATIONAL_PROGRAM_PARAM_MWP_peak_BW],
				profiling_params[PROFILING_PARAM_Active_warps_per_SM]);

		//double Mem_cycles = Coal_Mem_insts * Mem_L_Coal + Mem_L_Uncoal * Uncoal_Mem_insts;
		mpq_mul(rp_params[RATIONAL_PROGRAM_PARAM_Mem_cycles],
				profiling_params[PROFILING_PARAM_15_Mem_insts],
				profiling_params[PROFILING_PARAM_dynamic_Mem_LD]);

//      mpq_ceil (rp_params[RATIONAL_PROGRAM_PARAM_Mem_cycles]);

		//double Comp_cycles = Issue_cycles * total_insts;
		mpq_mul(rp_params[RATIONAL_PROGRAM_PARAM_Comp_cycles],
				device_params[DEVICE_PARAM_Issue_cycles],
				profiling_params[PROFILING_PARAM_13_Total_insts]);
//      mpq_ceil (rp_params[RATIONAL_PROGRAM_PARAM_Comp_cycles]);

		//double CWP_full = (Mem_cycles + Comp_cycles) / Comp_cycles;
		mpq_add(rp_params[RATIONAL_PROGRAM_PARAM_CWP_full],
				rp_params[RATIONAL_PROGRAM_PARAM_Mem_cycles],
				rp_params[RATIONAL_PROGRAM_PARAM_Comp_cycles]);
		mpq_div(rp_params[RATIONAL_PROGRAM_PARAM_CWP_full],
				rp_params[RATIONAL_PROGRAM_PARAM_CWP_full],
				rp_params[RATIONAL_PROGRAM_PARAM_Comp_cycles]);
//      mpq_ceil (rp_params[RATIONAL_PROGRAM_PARAM_CWP_full]);

		//double CWP = min2(CWP_full, Active_warps_per_SM);
		mpq_min2(rp_params[RATIONAL_PROGRAM_PARAM_CWP],
				rp_params[RATIONAL_PROGRAM_PARAM_CWP_full],
				profiling_params[PROFILING_PARAM_Active_warps_per_SM]);
		///////////////////////////////////////////////
		//////////////////////////////////////////////

		int mcwp_case;
		mcwp_case = MCWP_CASE_UNDEFINED;
		if (mpq_equal(rp_params[RATIONAL_PROGRAM_PARAM_MWP],
				profiling_params[PROFILING_PARAM_Active_warps_per_SM]) != 0
				&& mpq_equal(rp_params[RATIONAL_PROGRAM_PARAM_CWP],
						profiling_params[PROFILING_PARAM_Active_warps_per_SM])
						!= 0) {
			//        printf("Case:(MWP==N) && (CWP== N)\n");
			//	Exec_cycles_app = (Mem_cycles + Comp_cycles
			//			+ Comp_cycles * (MWP - 1) / Mem_insts) * Rep;
			mcwp_case = MCWP_CASE_CWP_EQ_MWP;
			//	printf("CASE 1!\n");
			mpq_sub(temp_mpq, rp_params[RATIONAL_PROGRAM_PARAM_MWP], one_mpq);
			mpq_mul(rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
					rp_params[RATIONAL_PROGRAM_PARAM_Comp_cycles], temp_mpq);
			mpq_div(rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
					rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
					profiling_params[PROFILING_PARAM_15_Mem_insts]);
			mpq_add(rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
					rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
					rp_params[RATIONAL_PROGRAM_PARAM_Mem_cycles]);
			mpq_add(rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
					rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
					rp_params[RATIONAL_PROGRAM_PARAM_Comp_cycles]);

		} else if (mpq_cmp(rp_params[RATIONAL_PROGRAM_PARAM_CWP],
				rp_params[RATIONAL_PROGRAM_PARAM_MWP]) >= 0
				|| mpq_cmp(rp_params[RATIONAL_PROGRAM_PARAM_Comp_cycles],
						rp_params[RATIONAL_PROGRAM_PARAM_Mem_cycles]) > 0) {
			//        printf("Case:(CWP > MWP) || (Comp_cycles > Mem_cycles)\n");
			//	Exec_cycles_app = (Mem_cycles * N / MWP
			//			+ Comp_cycles * (MWP - 1) / Mem_insts) * Rep;
//      mcwp_case = 2;
			mcwp_case = MCWP_CASE_CWP_GE_MWP;
			//printf("CASE 2!\n");
			mpq_sub(temp_mpq, rp_params[RATIONAL_PROGRAM_PARAM_MWP], one_mpq);
			mpq_mul(rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
					rp_params[RATIONAL_PROGRAM_PARAM_Comp_cycles], temp_mpq);
			mpq_div(rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
					rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
					profiling_params[PROFILING_PARAM_15_Mem_insts]);

			mpq_mul(temp_mpq, rp_params[RATIONAL_PROGRAM_PARAM_Mem_cycles],
					profiling_params[PROFILING_PARAM_Active_warps_per_SM]);
			mpq_div(temp_mpq, temp_mpq, rp_params[RATIONAL_PROGRAM_PARAM_MWP]);
			mpq_add(rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
					rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
					temp_mpq);
		} else {
//		Exec_cycles_app = (Active_warps_per_SM * Comp_cycles + Mem_L) * Rep;
//      mcwp_case = 3;
			mcwp_case = MCWP_CASE_CWP_LT_MWP;
			//printf("CASE 3!\n");
			mpq_mul(rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
					profiling_params[PROFILING_PARAM_Active_warps_per_SM],
					rp_params[RATIONAL_PROGRAM_PARAM_Comp_cycles]);
			mpq_add(rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
					rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
					profiling_params[PROFILING_PARAM_dynamic_Mem_LD]);
		}

		mpq_add(rp_params[RATIONAL_PROGRAM_PARAM_shared_mem_bytes_total],
				profiling_params[PROFILING_PARAM_shared_mem_bytes_dynamic],
				ptx_param[PTX_PARAM_shared_mem_bytes_static]);

//      mpq_ceil (rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app]);
//// profiling_params + rational program params

		int error_percentage = difference_percentage_mpq(
				rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
				profiling_params[PROFILING_PARAM_active_cycles]);

		if (abs(error_percentage) > MCWP_ERROR_TOLERANCE_PERCENTAGE) {
//	  printf ("[FAILED DIFF]=%d %%\n", (error_percentage));
			continue;
		}
		found_closest_estimate = 1;
		if (abs(error_percentage) > MCWP_ERROR_TOLERANCE_PERCENTAGE_MIN) {
			continue;
		}

//  if (abs (error_percentage) > MCWP_ERROR_TOLERANCE_PERCENTAGE)
//    {
//      printf (
//	  "[@%s]\n--[ERROR: error_percentage>=MCWP_ERROR_TOLERANCE_PERCENTAGE]...\n",
//	  __func__);
//      return (EXIT_FAILURE);
//    }

//      mpq_ceil (rp_params[RATIONAL_PROGRAM_PARAM_MWP]);
//      mpq_ceil (rp_params[RATIONAL_PROGRAM_PARAM_CWP]);

#if VERBOSE
		gmp_printf ("MWP: %Qd\n", rp_params[RATIONAL_PROGRAM_PARAM_MWP]);
		gmp_printf ("CWP: %Qd\n", rp_params[RATIONAL_PROGRAM_PARAM_CWP]);
		gmp_printf ("[EXEC_MCWP=%Qd]\n",
				rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app]);
		gmp_printf ("[EXEC_CUDA=%Qd]\n",
				profiling_params[PROFILING_PARAM_active_cycles]);
//      printf (
//	  "N_BLOCKS_SM=%d\n",
//	  mpq_get_ui (profiling_params[PROFILING_PARAM_Active_blocks_per_SM]));
//      printf (
//	  "N_WARPS_SM=%d\n",
//	  mpq_get_ui (profiling_params[PROFILING_PARAM_Active_warps_per_SM]));

//      mpq_div (profiling_params[PROFILING_PARAM_dynamic_Mem_LD],
//	       profiling_params[PROFILING_PARAM_dynamic_Mem_LD],
//	       profiling_params[PROFILING_PARAM_Active_warps_per_SM]);

		printf ("COMPUTED_MEM_LD=%d\n",
				mpq_get_ui (profiling_params[PROFILING_PARAM_dynamic_Mem_LD]));
		printf ("[DIFF]=%d %%\n", (error_percentage));
		print_short_dashed_line ();
#endif

		break;
	}
	mpq_clears(one_mpq, temp_mpq, NULL);

	if (found_closest_estimate == 0)
		return EXIT_FAILURE;
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int compute_mwp_cwp_values_for_single_mesh_point(
		poly_params * kernel_poly_params, profiling_info_t *pi, int point_idx,
		param_list_t* device_param_list) {

	mpq_t * device_params = device_param_list->mpq_values;

	mpq_t* ptx_param = kernel_poly_params->ptx_params->mpq_values;

	mpq_t* profiling_params =
			kernel_poly_params->profiling_params_list[point_idx].mpq_values;

	mpq_t* rp_params =
			kernel_poly_params->rational_program_params_list[point_idx].mpq_values;

	mpq_t one_mpq, temp_mpq;
	mpq_inits(one_mpq, temp_mpq, NULL);
	mpq_set_si(one_mpq, 1, 1);

//  int found_closest_estimate = 0;

//  int min_possible_mem_LD = 100;
//  int max_possible_mem_LD = 20000;

//  for (int i = min_possible_mem_LD; i <= max_possible_mem_LD; i++)
//    {

//      mpq_set_ui (profiling_params[PROFILING_PARAM_dynamic_Mem_LD], i, 1);
	/////////////////////////////////////
	/////////////////////////////////////
//  compute_dynamic_mem_ld (rp_params[RATIONAL_PROGRAM_PARAM_dynamic_Mem_LD],
//			  profiling_params[PROFILING_PARAM_14_Comp_insts],
//			  profiling_params[PROFILING_PARAM_15_Mem_insts],
//			  profiling_params[PROFILING_PARAM_active_cycles]);

	//double MWP_Without_BW_full = Mem_L / Departure_delay;
	mpq_div(rp_params[RATIONAL_PROGRAM_PARAM_MWP_Without_BW_full],
			profiling_params[PROFILING_PARAM_dynamic_Mem_LD],
			device_params[DEVICE_PARAM_Departure_del_coal]);
	mpq_ceil(rp_params[RATIONAL_PROGRAM_PARAM_MWP_Without_BW_full]);

	//double MWP_Without_BW = min2(MWP_Without_BW_full, Active_warps_per_SM);
	mpq_min2(rp_params[RATIONAL_PROGRAM_PARAM_MWP_Without_BW],
			rp_params[RATIONAL_PROGRAM_PARAM_MWP_Without_BW_full],
			profiling_params[PROFILING_PARAM_Active_warps_per_SM]);

	//double BW_per_warp = Freq * Load_bytes_per_warp / Mem_L;
	mpq_div(rp_params[RATIONAL_PROGRAM_PARAM_MWP_BW_per_warp],
			device_params[DEVICE_PARAM_Load_bytes_per_warp],
			profiling_params[PROFILING_PARAM_dynamic_Mem_LD]);
	mpq_mul(rp_params[RATIONAL_PROGRAM_PARAM_MWP_BW_per_warp],
			rp_params[RATIONAL_PROGRAM_PARAM_MWP_BW_per_warp],
			device_params[DEVICE_PARAM_Freq]);

	mpq_ceil(rp_params[RATIONAL_PROGRAM_PARAM_MWP_BW_per_warp]);

	//double MWP_peak_BW = Mem_bandwidth / (BW_per_warp * Active_SMs);
	mpq_mul(rp_params[RATIONAL_PROGRAM_PARAM_MWP_peak_BW],
			rp_params[RATIONAL_PROGRAM_PARAM_MWP_BW_per_warp],
			device_params[DEVICE_PARAM_Active_SMs]);
	mpq_div(rp_params[RATIONAL_PROGRAM_PARAM_MWP_peak_BW],
			device_params[DEVICE_PARAM_Mem_bandwidth],
			rp_params[RATIONAL_PROGRAM_PARAM_MWP_peak_BW]);

	mpq_ceil(rp_params[RATIONAL_PROGRAM_PARAM_MWP_peak_BW]);

	//double MWP = min3(MWP_Without_BW, MWP_peak_BW, Active_warps_per_SM);
	mpq_min3(rp_params[RATIONAL_PROGRAM_PARAM_MWP],
			rp_params[RATIONAL_PROGRAM_PARAM_MWP_Without_BW],
			rp_params[RATIONAL_PROGRAM_PARAM_MWP_peak_BW],
			profiling_params[PROFILING_PARAM_Active_warps_per_SM]);

	//double Mem_cycles = Coal_Mem_insts * Mem_L_Coal + Mem_L_Uncoal * Uncoal_Mem_insts;
	mpq_mul(rp_params[RATIONAL_PROGRAM_PARAM_Mem_cycles],
			profiling_params[PROFILING_PARAM_15_Mem_insts],
			profiling_params[PROFILING_PARAM_dynamic_Mem_LD]);

	mpq_ceil(rp_params[RATIONAL_PROGRAM_PARAM_Mem_cycles]);

	//double Comp_cycles = Issue_cycles * total_insts;
	mpq_mul(rp_params[RATIONAL_PROGRAM_PARAM_Comp_cycles],
			device_params[DEVICE_PARAM_Issue_cycles],
			profiling_params[PROFILING_PARAM_13_Total_insts]);
	mpq_ceil(rp_params[RATIONAL_PROGRAM_PARAM_Comp_cycles]);

	//double CWP_full = (Mem_cycles + Comp_cycles) / Comp_cycles;
	mpq_add(rp_params[RATIONAL_PROGRAM_PARAM_CWP_full],
			rp_params[RATIONAL_PROGRAM_PARAM_Mem_cycles],
			rp_params[RATIONAL_PROGRAM_PARAM_Comp_cycles]);
	mpq_div(rp_params[RATIONAL_PROGRAM_PARAM_CWP_full],
			rp_params[RATIONAL_PROGRAM_PARAM_CWP_full],
			rp_params[RATIONAL_PROGRAM_PARAM_Comp_cycles]);
	mpq_ceil(rp_params[RATIONAL_PROGRAM_PARAM_CWP_full]);

	//double CWP = min2(CWP_full, Active_warps_per_SM);
	mpq_min2(rp_params[RATIONAL_PROGRAM_PARAM_CWP],
			rp_params[RATIONAL_PROGRAM_PARAM_CWP_full],
			profiling_params[PROFILING_PARAM_Active_warps_per_SM]);

	///////////////////////////////////////////////
	//////////////////////////////////////////////

	int mcwp_case;
	mcwp_case = MCWP_CASE_UNDEFINED;
	if (mpq_equal(rp_params[RATIONAL_PROGRAM_PARAM_MWP],
			profiling_params[PROFILING_PARAM_Active_warps_per_SM]) != 0
			&& mpq_equal(rp_params[RATIONAL_PROGRAM_PARAM_CWP],
					profiling_params[PROFILING_PARAM_Active_warps_per_SM])
					!= 0) {
		//        printf("Case:(MWP==N) && (CWP== N)\n");
		//	Exec_cycles_app = (Mem_cycles + Comp_cycles
		//			+ Comp_cycles * (MWP - 1) / Mem_insts) * Rep;
		mcwp_case = MCWP_CASE_CWP_EQ_MWP;
		//	printf("CASE 1!\n");
		mpq_sub(temp_mpq, rp_params[RATIONAL_PROGRAM_PARAM_MWP], one_mpq);
		mpq_mul(rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
				rp_params[RATIONAL_PROGRAM_PARAM_Comp_cycles], temp_mpq);
		mpq_div(rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
				rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
				profiling_params[PROFILING_PARAM_15_Mem_insts]);
		mpq_add(rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
				rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
				rp_params[RATIONAL_PROGRAM_PARAM_Mem_cycles]);
		mpq_add(rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
				rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
				rp_params[RATIONAL_PROGRAM_PARAM_Comp_cycles]);

	} else if (mpq_cmp(rp_params[RATIONAL_PROGRAM_PARAM_CWP],
			rp_params[RATIONAL_PROGRAM_PARAM_MWP]) >= 0
			|| mpq_cmp(rp_params[RATIONAL_PROGRAM_PARAM_Comp_cycles],
					rp_params[RATIONAL_PROGRAM_PARAM_Mem_cycles]) > 0) {
		//        printf("Case:(CWP > MWP) || (Comp_cycles > Mem_cycles)\n");
		//	Exec_cycles_app = (Mem_cycles * N / MWP
		//			+ Comp_cycles * (MWP - 1) / Mem_insts) * Rep;
//      mcwp_case = 2;
		mcwp_case = MCWP_CASE_CWP_GE_MWP;
		//printf("CASE 2!\n");
		mpq_sub(temp_mpq, rp_params[RATIONAL_PROGRAM_PARAM_MWP], one_mpq);
		mpq_mul(rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
				rp_params[RATIONAL_PROGRAM_PARAM_Comp_cycles], temp_mpq);
		mpq_div(rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
				rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
				profiling_params[PROFILING_PARAM_15_Mem_insts]);

		mpq_mul(temp_mpq, rp_params[RATIONAL_PROGRAM_PARAM_Mem_cycles],
				profiling_params[PROFILING_PARAM_Active_warps_per_SM]);
		mpq_div(temp_mpq, temp_mpq, rp_params[RATIONAL_PROGRAM_PARAM_MWP]);
		mpq_add(rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
				rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app], temp_mpq);
	} else {
//		Exec_cycles_app = (Active_warps_per_SM * Comp_cycles + Mem_L) * Rep;
//      mcwp_case = 3;
		mcwp_case = MCWP_CASE_CWP_LT_MWP;
		//printf("CASE 3!\n");
		mpq_mul(rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
				profiling_params[PROFILING_PARAM_Active_warps_per_SM],
				rp_params[RATIONAL_PROGRAM_PARAM_Comp_cycles]);
		mpq_add(rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
				rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
				profiling_params[PROFILING_PARAM_dynamic_Mem_LD]);
	}

	mpq_ceil(rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app]);
//// profiling_params + rational program params

//      int error_percentage = difference_percentage_mpq (
//	  rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
//	  profiling_params[PROFILING_PARAM_active_cycles]);
//      if (abs (error_percentage) > MCWP_ERROR_TOLERANCE_PERCENTAGE)
//	{
////	  printf ("[FAILED DIFF]=%d %%\n", (error_percentage));
//	  continue;
//	}

//      found_closest_estimate = 1;

//  if (abs (error_percentage) > MCWP_ERROR_TOLERANCE_PERCENTAGE)
//    {
//      printf (
//	  "[@%s]\n--[ERROR: error_percentage>=MCWP_ERROR_TOLERANCE_PERCENTAGE]...\n",
//	  __func__);
//      return (EXIT_FAILURE);
//    }

//      mpq_ceil (rp_params[RATIONAL_PROGRAM_PARAM_MWP]);
//      mpq_ceil (rp_params[RATIONAL_PROGRAM_PARAM_CWP]);

#if VERBOSE
	gmp_printf ("MWP: %Qd\n", rp_params[RATIONAL_PROGRAM_PARAM_MWP]);
	gmp_printf ("CWP: %Qd\n", rp_params[RATIONAL_PROGRAM_PARAM_CWP]);
	gmp_printf ("[EXEC_MCWP=%Qd]\n",
			rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app]);
	gmp_printf ("[EXEC_CUDA=%Qd]\n",
			profiling_params[PROFILING_PARAM_active_cycles]);
//      printf ("[DIFF]=%d %%\n", (error_percentage));
	print_short_dashed_line ();
#endif

	mpq_add(rp_params[RATIONAL_PROGRAM_PARAM_shared_mem_bytes_total],
			profiling_params[PROFILING_PARAM_shared_mem_bytes_dynamic],
			ptx_param[PTX_PARAM_shared_mem_bytes_static]);
//      break;
//    }
	mpq_clears(one_mpq, temp_mpq, NULL);

//  if (found_closest_estimate == 0)
//    return EXIT_FAILURE;
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int compute_mwp_cwp_values_for_single_mesh_point_in_dp(
		param_list_t* rp_param_list, const param_list_t* profiling_param_list,
		const param_list_t* ptx_param_list,
		const param_list_t* device_param_list, const int *arch_params,
		const int n_warps_per_block) {

	mpq_t * device_params = device_param_list->mpq_values;

	mpq_t* ptx_param = ptx_param_list->mpq_values;

	mpq_t* profiling_params = profiling_param_list->mpq_values;

	mpq_t* rp_params = rp_param_list->mpq_values;

	mpq_t one_mpq, temp_mpq;
	mpq_inits(one_mpq, temp_mpq, NULL);
	mpq_set_si(one_mpq, 1, 1);

	/////////////////////////////////////
	/////////////////////////////////////
//  compute_dynamic_mem_ld (rp_params[RATIONAL_PROGRAM_PARAM_dynamic_Mem_LD],
//			  profiling_params[PROFILING_PARAM_14_Comp_insts],
//			  profiling_params[PROFILING_PARAM_15_Mem_insts],
//			  profiling_params[PROFILING_PARAM_active_cycles]);

	//double MWP_Without_BW_full = Mem_L / Departure_delay;
	mpq_div(rp_params[RATIONAL_PROGRAM_PARAM_MWP_Without_BW_full],
			profiling_params[PROFILING_PARAM_dynamic_Mem_LD],
			device_params[DEVICE_PARAM_Departure_del_coal]);
//  mpq_ceil (rp_params[RATIONAL_PROGRAM_PARAM_MWP_Without_BW_full]);

	//double MWP_Without_BW = min2(MWP_Without_BW_full, Active_warps_per_SM);
	mpq_min2(rp_params[RATIONAL_PROGRAM_PARAM_MWP_Without_BW],
			rp_params[RATIONAL_PROGRAM_PARAM_MWP_Without_BW_full],
			profiling_params[PROFILING_PARAM_Active_warps_per_SM]);

	//double BW_per_warp = Freq * Load_bytes_per_warp / Mem_L;
	mpq_div(rp_params[RATIONAL_PROGRAM_PARAM_MWP_BW_per_warp],
			device_params[DEVICE_PARAM_Load_bytes_per_warp],
			profiling_params[PROFILING_PARAM_dynamic_Mem_LD]);
	mpq_mul(rp_params[RATIONAL_PROGRAM_PARAM_MWP_BW_per_warp],
			rp_params[RATIONAL_PROGRAM_PARAM_MWP_BW_per_warp],
			device_params[DEVICE_PARAM_Freq]);

	//double MWP_peak_BW = Mem_bandwidth / (BW_per_warp * Active_SMs);
	mpq_mul(rp_params[RATIONAL_PROGRAM_PARAM_MWP_peak_BW],
			rp_params[RATIONAL_PROGRAM_PARAM_MWP_BW_per_warp],
			device_params[DEVICE_PARAM_Active_SMs]);
	mpq_div(rp_params[RATIONAL_PROGRAM_PARAM_MWP_peak_BW],
			device_params[DEVICE_PARAM_Mem_bandwidth],
			rp_params[RATIONAL_PROGRAM_PARAM_MWP_peak_BW]);

	//double MWP = min3(MWP_Without_BW, MWP_peak_BW, Active_warps_per_SM);
	mpq_min3(rp_params[RATIONAL_PROGRAM_PARAM_MWP],
			rp_params[RATIONAL_PROGRAM_PARAM_MWP_Without_BW],
			rp_params[RATIONAL_PROGRAM_PARAM_MWP_peak_BW],
			profiling_params[PROFILING_PARAM_Active_warps_per_SM]);

	//double Mem_cycles = Coal_Mem_insts * Mem_L_Coal + Mem_L_Uncoal * Uncoal_Mem_insts;
	mpq_mul(rp_params[RATIONAL_PROGRAM_PARAM_Mem_cycles],
			profiling_params[PROFILING_PARAM_15_Mem_insts],
			profiling_params[PROFILING_PARAM_dynamic_Mem_LD]);

	//double Comp_cycles = Issue_cycles * total_insts;
	mpq_mul(rp_params[RATIONAL_PROGRAM_PARAM_Comp_cycles],
			device_params[DEVICE_PARAM_Issue_cycles],
			profiling_params[PROFILING_PARAM_13_Total_insts]);

	//double CWP_full = (Mem_cycles + Comp_cycles) / Comp_cycles;
	mpq_add(rp_params[RATIONAL_PROGRAM_PARAM_CWP_full],
			rp_params[RATIONAL_PROGRAM_PARAM_Mem_cycles],
			rp_params[RATIONAL_PROGRAM_PARAM_Comp_cycles]);
	mpq_div(rp_params[RATIONAL_PROGRAM_PARAM_CWP_full],
			rp_params[RATIONAL_PROGRAM_PARAM_CWP_full],
			rp_params[RATIONAL_PROGRAM_PARAM_Comp_cycles]);
//  mpq_ceil (rp_params[RATIONAL_PROGRAM_PARAM_CWP_full]);

	//double CWP = min2(CWP_full, Active_warps_per_SM);
	mpq_min2(rp_params[RATIONAL_PROGRAM_PARAM_CWP],
			rp_params[RATIONAL_PROGRAM_PARAM_CWP_full],
			profiling_params[PROFILING_PARAM_Active_warps_per_SM]);
	///////////////////////////////////////////////
	//////////////////////////////////////////////

	int mcwp_case;
	mcwp_case = MCWP_CASE_UNDEFINED;
	if (mpq_equal(rp_params[RATIONAL_PROGRAM_PARAM_MWP],
			profiling_params[PROFILING_PARAM_Active_warps_per_SM]) != 0
			&& mpq_equal(rp_params[RATIONAL_PROGRAM_PARAM_CWP],
					profiling_params[PROFILING_PARAM_Active_warps_per_SM])
					!= 0) {
		//        printf("Case:(MWP==N) && (CWP== N)\n");
		//	Exec_cycles_app = (Mem_cycles + Comp_cycles
		//			+ Comp_cycles * (MWP - 1) / Mem_insts) * Rep;
		mcwp_case = MCWP_CASE_CWP_EQ_MWP;
		//	printf("CASE 1!\n");
		mpq_sub(temp_mpq, rp_params[RATIONAL_PROGRAM_PARAM_MWP], one_mpq);
		mpq_mul(rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
				rp_params[RATIONAL_PROGRAM_PARAM_Comp_cycles], temp_mpq);
		mpq_div(rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
				rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
				profiling_params[PROFILING_PARAM_15_Mem_insts]);
		mpq_add(rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
				rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
				rp_params[RATIONAL_PROGRAM_PARAM_Mem_cycles]);
		mpq_add(rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
				rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
				rp_params[RATIONAL_PROGRAM_PARAM_Comp_cycles]);

	} else if (mpq_cmp(rp_params[RATIONAL_PROGRAM_PARAM_CWP],
			rp_params[RATIONAL_PROGRAM_PARAM_MWP]) >= 0
			|| mpq_cmp(rp_params[RATIONAL_PROGRAM_PARAM_Comp_cycles],
					rp_params[RATIONAL_PROGRAM_PARAM_Mem_cycles]) > 0) {
		//        printf("Case:(CWP > MWP) || (Comp_cycles > Mem_cycles)\n");
		//	Exec_cycles_app = (Mem_cycles * N / MWP
		//			+ Comp_cycles * (MWP - 1) / Mem_insts) * Rep;
//      mcwp_case = 2;
		mcwp_case = MCWP_CASE_CWP_GE_MWP;
		//printf("CASE 2!\n");
		mpq_sub(temp_mpq, rp_params[RATIONAL_PROGRAM_PARAM_MWP], one_mpq);
		mpq_mul(rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
				rp_params[RATIONAL_PROGRAM_PARAM_Comp_cycles], temp_mpq);
		mpq_div(rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
				rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
				profiling_params[PROFILING_PARAM_15_Mem_insts]);

		mpq_mul(temp_mpq, rp_params[RATIONAL_PROGRAM_PARAM_Mem_cycles],
				profiling_params[PROFILING_PARAM_Active_warps_per_SM]);
		mpq_div(temp_mpq, temp_mpq, rp_params[RATIONAL_PROGRAM_PARAM_MWP]);
		mpq_add(rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
				rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app], temp_mpq);
	} else {
//		Exec_cycles_app = (Active_warps_per_SM * Comp_cycles + Mem_L) * Rep;
//      mcwp_case = 3;
		mcwp_case = MCWP_CASE_CWP_LT_MWP;
		//printf("CASE 3!\n");
		mpq_mul(rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
				profiling_params[PROFILING_PARAM_Active_warps_per_SM],
				rp_params[RATIONAL_PROGRAM_PARAM_Comp_cycles]);
		mpq_add(rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
				rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
				profiling_params[PROFILING_PARAM_dynamic_Mem_LD]);
	}

//  mpq_ceil (rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app]);
//// profiling_params + rational program params

//  int error_percentage = difference_percentage_mpq (
//      rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app],
//      profiling_params[PROFILING_PARAM_active_cycles]);

//  if (abs (error_percentage) > MCWP_ERROR_TOLERANCE_PERCENTAGE)
//    {
//      printf (
//	  "[@%s]\n--[ERROR: error_percentage>=MCWP_ERROR_TOLERANCE_PERCENTAGE]...\n",
//	  __func__);
//      return (EXIT_FAILURE);
//    }

//  gmp_printf ("MWP: %d\n", mpq_get_ui (rp_params[RATIONAL_PROGRAM_PARAM_MWP]));
//  gmp_printf ("CWP: %d\n", mpq_get_ui (rp_params[RATIONAL_PROGRAM_PARAM_CWP]));
//
//
////  gmp_printf ("[EXEC_MCWP=%Qd]\n",
////	      rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app]);
////  gmp_printf ("[EXEC_CUDA=%Qd]\n",
////	      profiling_params[PROFILING_PARAM_active_cycles]);
////  printf ("[DIFF]=%d %%\n", (error_percentage));
//  print_short_dashed_line ();

	mpq_add(rp_params[RATIONAL_PROGRAM_PARAM_shared_mem_bytes_total],
			profiling_params[PROFILING_PARAM_shared_mem_bytes_dynamic],
			ptx_param[PTX_PARAM_shared_mem_bytes_static]);

//  mpq_set (rp_params[RATIONAL_PROGRAM_PARAM_occupancy],
//	   profiling_params[PROFILING_PARAM_Active_warps_per_SM]);
//  mpq_div_ui (rp_params[RATIONAL_PROGRAM_PARAM_occupancy],
//	      arch_params[ARCH_PARAM_MAX_WARPS_PER_SM]);

//  mpq_set (rp_params[RATIONAL_PROGRAM_PARAM_occupancy],
//	   profiling_params[PROFILING_PARAM_Active_blocks_per_SM]);

	mpq_t occ_by_warp, occ_by_blocks;
	mpq_inits(occ_by_warp, occ_by_blocks, NULL);

	mpq_set(occ_by_warp, profiling_params[PROFILING_PARAM_Active_warps_per_SM]);
	mpq_div_ui(occ_by_warp, arch_params[ARCH_PARAM_MAX_WARPS_PER_SM]);

	//////////////////
	mpq_set(occ_by_blocks,
			profiling_params[PROFILING_PARAM_Active_blocks_per_SM]);
	mpq_t n_warps_per_block_qq;
	mpq_init(n_warps_per_block_qq);
//  mpq_set_ui (n_warps_per_block_qq, n_warps_per_block, 1);
//  mpq_mul (occ_by_blocks, occ_by_blocks, n_warps_per_block_qq);
//  mpq_div_ui (occ_by_blocks,
//	      arch_params[ARCH_PARAM_MAX_WARPS_PER_SM]);
	mpq_div_ui(occ_by_blocks, arch_params[ARCH_PARAM_MAX_BLOCKS_PER_SM]);

	//pick the min/max? occupancy.
//  if (mpq_cmp (occ_by_warp, occ_by_blocks)> 0)
	mpq_set(rp_params[RATIONAL_PROGRAM_PARAM_occupancy], occ_by_warp);
//  else
//    mpq_set (rp_params[RATIONAL_PROGRAM_PARAM_occupancy], occ_by_blocks);

	mpq_clears(occ_by_warp, occ_by_blocks, NULL);
	mpq_clear(n_warps_per_block_qq);

//  if (mpq_sgn(rp_params[RATIONAL_PROGRAM_PARAM_Exec_cycles_app])<0)
//      printf("[WARNING: RATIONAL_PROGRAM_PARAM_Exec_cycles_app < 0]...\n");

	mpq_clears(one_mpq, temp_mpq, NULL);
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int get_num_lines_in_file(const char * path) {
	FILE *file = fopen(path, "r");
	int n_lines = 0;
	if (file == NULL) {
		printf("[@%s][ERROR: opening file=%s ... \n", __func__, path);
		exit(EXIT_FAILURE);
	}
	fseek(file, 0, SEEK_END);
	size_t buffer_size = ftell(file);
	fseek(file, 0, SEEK_SET);

	////critical to add the string terminator.
	char* buffer = (char*) malloc(buffer_size + 1);
	for (int i = 0; i <= buffer_size; i++)
		buffer[i] = '\0';

	size_t n_read = fread(buffer, 1, buffer_size, file);
//  printf("size=%d\n",buffer_size);
	if (n_read == buffer_size) {
		const char * line_delim = "\n";
		char * line = strtok(buffer, line_delim);
		while (line != NULL) {
//	  printf ("line=%s\n", line);
			n_lines++;
			line = strtok(NULL, line_delim);
			if (line == NULL)
				break;
		}
	}
	free(buffer);
	fclose(file);

//  printf("N_LINES=%d\n", n_lines);
	return n_lines;
}

///////////////////////////////////////

int init_output_path_list(char *** file_path_list_out, char ** kernel_name_list,
		char * prefix, char * suffix, int n_kernels) {
	char ** file_path_list = (char **) malloc(n_kernels * sizeof(char *));
	int str_size = strlen(prefix) + strlen(suffix) + 1;
	for (int i = 0; i < n_kernels; i++) {
		file_path_list[i] = (char *) malloc(
				(str_size + strlen(kernel_name_list[i])) * sizeof(char));
		strcpy(file_path_list[i], prefix);
		strcat(file_path_list[i], kernel_name_list[i]);
		strcat(file_path_list[i], suffix);
#if VERBOSE >=3
		printf("[file_path: %s]\n", file_path_list[i]);
#endif
	}

	*file_path_list_out = file_path_list;
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int clear_output_path_list(char ** file_path_list, int n_kernels) {
	for (int i = 0; i < n_kernels; i++) {
		free(file_path_list[i]);
	}
	free(file_path_list);
	return EXIT_SUCCESS;
}

///////////////////////////////////////

//rewrite using fread and strtok.
int init_kernel_names_from_file(char*** kernel_names_out, int*n_kernels_out,
		const char *kernel_name_list_path) {
	int n_kernels = get_num_lines_in_file(kernel_name_list_path);
	*n_kernels_out = n_kernels;
#if VERBOSE
	printf ("n_kernels=%d\n", n_kernels);
#endif
	FILE *kernel_name_list = fopen(kernel_name_list_path, "r");
	if (kernel_name_list == NULL) {
		printf("ERROR: opening kernel_name_list.tmp!\n");
		exit(EXIT_FAILURE);
	}

	char** kernel_names = (char **) malloc(n_kernels * sizeof(char *));
	for (int i = 0; i < n_kernels; i++) {
		kernel_names[i] = (char *) malloc(LEN_KERNEL_NAME * sizeof(char));
		fgets(kernel_names[i], LEN_KERNEL_NAME, kernel_name_list);
		kernel_names[i][strcspn(kernel_names[i], "\n")] = '\0';
	}
	fclose(kernel_name_list);

#if VERBOSE
	for (int i = 0; i < n_kernels; i++)
	printf ("kernel [%d] : [%s]\n", i, kernel_names[i]);
#endif

	*kernel_names_out = kernel_names;

	return EXIT_SUCCESS;
}

///////////////////////////////////////

//rewrite using fread and strtok.
int clear_kernel_names(char** kernel_names, int n_kernels) {
	for (int i = 0; i < n_kernels; i++) {
		free(kernel_names[i]);
	}
	free(kernel_names);
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int kernel_poly_interpolation_data_list_init(poly_params * kernel_poly_params) {
	poly_params * ptr = kernel_poly_params;
	int size = kernel_poly_params->interp_entity_table.size;
	int size_static =
			kernel_poly_params->interp_entity_table_for_static_params.size;

	//allocate space for the interpolation data list.
	size_t interpolation_data_list_size = (size * sizeof(interpolation_data_t));
	kernel_poly_params->interpolation_data_list =
			(interpolation_data_t*) malloc(interpolation_data_list_size);

	size_t interpolation_data_list_static_size = (size_static
			* sizeof(interpolation_data_t));
	kernel_poly_params->interpolation_data_list_for_static_params =
			(interpolation_data_t*) malloc(interpolation_data_list_static_size);

	for (int i = 0; i < size; i++) {
		interpolation_data_t* id_ptr =
				&kernel_poly_params->interpolation_data_list[i];
		id_ptr->interpolator = rfInterpInit(ptr->nvar, ptr->degree_bounds,
				ptr->denomdegree_bounds);
		id_ptr->num_poly = NULL;
		id_ptr->denom_poly = NULL;
		id_ptr->status = INIT;
	}

	for (int i = 0; i < size_static; i++) {
		interpolation_data_t* id_ptr =
				&kernel_poly_params->interpolation_data_list_for_static_params[i];
		id_ptr->interpolator = NULL; //we do not need interpolation in case of constants.
//      rfInterpInit (ptr->nvar, ptr->degree_bounds,
//					   ptr->denomdegree_bounds);
		id_ptr->num_poly = NULL;
		id_ptr->denom_poly = NULL;
		id_ptr->status = INIT;
	}
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int kernel_poly_interpolation_data_list_clear(
		poly_params * kernel_poly_params) {
	//should run freePolynomial_AA on each interp entity.
	poly_params* ptr = kernel_poly_params;
	for (int i = 0; i < ptr->interp_entity_table.size; i++) {
		interpolation_data_t* id_ptr =
				&kernel_poly_params->interpolation_data_list[i];

		if (id_ptr->status == SUCCESSFUL_INTERP) {
			freePolynomial_AA(id_ptr->num_poly);
			freePolynomial_AA(id_ptr->denom_poly);
//	  rfInterpFree (id_ptr->interpolator);
//	  printf ("freeing interp data for [%s][%d][%s]\n",
//		  kernel_poly_params->kernel_name, i,
//		  ptr->interp_entity_table.entity_list[i].param_name);
		}
	}

	for (int i = 0; i < ptr->interp_entity_table_for_static_params.size; i++) {
		interpolation_data_t* id_ptr =
				&kernel_poly_params->interpolation_data_list_for_static_params[i];
		freePolynomial_AA(id_ptr->num_poly);
		freePolynomial_AA(id_ptr->denom_poly);
//        rfInterpFree (id_ptr->interpolator);
	}
	free(ptr->interpolation_data_list);
	free(ptr->interpolation_data_list_for_static_params);
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int symbol_info_init(symbol_info_t ** si_out, kernel_type_enum_t kernel_type) {

	symbol_info_t * si = (symbol_info_t*) malloc(sizeof(symbol_info_t));
	*si_out = si;

	char** syms;
	int n_symbols;

	const char *syms_dict[] = { "N", "B0", "B1", "B2" };

	switch (kernel_type) {
	case OneDimKernel:
		n_symbols = 3;
		break;

	case TwoDimKernel:
		n_symbols = 3;
		break;
	case ThreeDimKernel:
		printf("[@%s][ERROR: ThreeDimKernel is not supported!]...\n");
		return EXIT_FAILURE;
		break;
	}

	syms = (char**) malloc(n_symbols * sizeof(char*));
	for (int i = 0; i < n_symbols; i++) {
		syms[i] = (char*) malloc(strlen(syms_dict[i]) + 1);
		strcpy(syms[i], syms_dict[i]);
	}

	si->n_symbols = n_symbols;
	si->syms = syms;

	return EXIT_SUCCESS;
}

///////////////////////////////////////

int symbol_info_clear(symbol_info_t *si) {
	for (int i = 0; i < si->n_symbols; i++) {
		free(si->syms[i]);
	}
	free(si->syms);
	free(si);
	return EXIT_SUCCESS;
}

///////////////////////////////////////

///////////////////////////////////////
//// pi is created during init of dci
//// pi = {buffer for all kernels + pointer to a mesh + maps of visited points.}
//// all kernels depend on the same pi -> pass the pi to init_kernel_poly_params.
int kernel_poly_params_init(poly_params **kernel_poly_params_out,
		const char ** kernel_names, const int nvar,
		const int n_params_to_interpolate, const int n_kernels,
		const profiling_info_t * pi) {
	poly_params *kernel_poly_params = (poly_params *) malloc(
			n_kernels * sizeof(poly_params));
	*kernel_poly_params_out = kernel_poly_params;

	/////////////////////////////////////
	int mcwp_error;
	/////////////////////////////////////
	//// set kernel_name from the corresponding list of names.
	for (int k = 0; k < n_kernels; k++) {
		int len = strlen(kernel_names[k]) + 1;
		kernel_poly_params[k].kernel_name = (char*) malloc(len);
		strncpy(kernel_poly_params[k].kernel_name, kernel_names[k], len);
	}

	/////////////////////////////////////
	////get symbols name and count for the type of kernel associated to pi.
	//// set kernel symbols.
	for (int k = 0; k < n_kernels; k++) {
		mcwp_error = symbol_info_init(&(kernel_poly_params[k].symbol_info),
				pi->kernel_type);
		check_mcwp_error(mcwp_error, "symbol_info_init");
	}

	/////////////////////////////////////
	////set nvar: number of variables for interpolation.
	for (int k = 0; k < n_kernels; k++) {
		kernel_poly_params[k].nvar = nvar;
	}

	/////////////////////////////////////
	////also, set deg_bounds and denom_deg_bounds for each var.
	////size_t deg_bounds_size = nvar * sizeof(int);
	for (int k = 0; k < n_kernels; k++) {
		mcwp_error = init_degree_bounds_for_each_var(
				&kernel_poly_params[k].degree_bounds,
				&kernel_poly_params[k].denomdegree_bounds,
				kernel_poly_params[k].nvar, pi->kernel_type);
		check_mcwp_error(mcwp_error, "init_degree_bounds_for_each_var");
	}
//  /////////////////////////////////////
//  ////init interpolator for each kernel.
//  for (int k = 0; k < n_kernels; k++)
//    {
//      kernel_poly_params[k].n_params_to_interpolate = n_params_to_interpolate;
//      mcwp_error = kernel_poly_interpolator_list_init (&kernel_poly_params[k]);
//      check_mcwp_error(mcwp_error, "kernel_poly_interpolator_list_init");
//    }

	/////////////////////////////////////
	////copy information of the mesh point from the associated pi.
	for (int k = 0; k < n_kernels; k++) {
		kernel_poly_params[k].point_set = pi->point_set;
		kernel_poly_params[k].n_points_in_mesh = pi->n_points_in_mesh;

		//// init table for storing mcwp results;
		kernel_poly_params[k].mesh_size =
				kernel_poly_params[k].n_points_in_mesh;
	}

	/////////////////////////////////////
	/////////////////////////////////////
//
//  for (int k = 0; k < n_kernels; k++)
//    {
//      kernel_poly_params[k].mcwp_result_table = (mcwp_result_params_t*) malloc (
//	  kernel_poly_params[k].mesh_size * sizeof(mcwp_result_params_t));
//      kernel_poly_params[k].mcwp_result_table_idx = 0;
//#if VERBOSE
//      printf (
//	  "[mcwp_result_table of size [%d] alloccated for kernel [%d]]...\n",
//	  kernel_poly_params[k].mesh_size, k);
//#endif
//    }
	/////////////////////////////////////
	/////////////////////////////////////

#if VERBOSE
	printf ("[Init interpolation done]...\n");
#endif

	/////////////////////////////////////
	/**
	 * read ptx params once per kernel. this information will be used to create
	 * headers that will be used for estimating occupancy.
	 */
	const char prefix_kernel[] = "kernel_";
	const char suffix_ptx_params[] = "_ptx_params.tmp";
	/////////////////////////////////////
	////only thing that should be read from the file is ptx params (n_registers, n_static_shared_mem_bytes).
	char** ptx_param_file_path_list;
	init_output_path_list(&ptx_param_file_path_list, kernel_names,
			prefix_kernel, suffix_ptx_params, n_kernels);

	//// allocate space for ptx params; then, call the init function.
	for (int k = 0; k < n_kernels; k++) {
		mcwp_error = ptx_params_init(&kernel_poly_params[k].ptx_params,
				ptx_param_file_path_list[k], pi->n_points_in_mesh);
		check_mcwp_error(mcwp_error, "ptx_params_init");
	}

	mcwp_error = clear_output_path_list(ptx_param_file_path_list, n_kernels);
	check_mcwp_error(mcwp_error, "clear_output_path_list");

	/////////////////////////////////////
	/**
	 * allocate space for kernel launch params, profiling params and rational
	 * program params of each kernel.
	 * For each of the mentioned param types, the max size of "param_list" is
	 * "n_points_in_mesh" for each kernel.
	 */

	size_t param_list_size_for_all_points = pi->n_points_in_mesh
			* sizeof(param_list_t);

	for (int k = 0; k < n_kernels; k++) {
		kernel_poly_params[k].profiling_params_list = (param_list_t*) malloc(
				param_list_size_for_all_points);
		kernel_poly_params[k].rational_program_params_list =
				(param_list_t*) malloc(param_list_size_for_all_points);

//      for (int i = 0; i < kernel_poly_params[k].n_points_in_mesh; i++)
		for (int i = 0; i < pi->n_points_in_mesh; i++) {
//	  printf ("pi init i=%d\n", i);
			mcwp_error = param_list_init(
					&kernel_poly_params[k].profiling_params_list[i],
					PARAM_LIST_TYPE_PROFILING);
//	  check_mcwp_error(mcwp_error,
//			   "[param_list_init: profiling_params_list]");
		}

		//      for (int i = 0; i < kernel_poly_params[k].n_points_in_mesh; i++)
		for (int i = 0; i < pi->n_points_in_mesh; i++) {
//	  printf ("rp init i=%d\n", i);
			mcwp_error = param_list_init(
					&kernel_poly_params[k].rational_program_params_list[i],
					PARAM_LIST_TYPE_RATIONAL_PROGRAM);
//	  check_mcwp_error(mcwp_error,
//			   "[param_list_init: rational_program_params_list]");
		}
	}

	/////////////////////////////////////
	//allocate space for visited maps for each kernel.
	for (int k = 0; k < n_kernels; k++) {
		kernel_poly_params[k].visitd_points_map = (int*) malloc(
				pi->n_points_in_mesh * sizeof(int));
	}

	/////////////////////////////////////

	return EXIT_SUCCESS;
}

///////////////////////////////////////

int kernel_poly_params_clear(poly_params *kernel_poly_params, int n_kernels) {
	/////////////////////////////////////
	int mcwp_error;
	/////////////////////////////////////
	//// set kernel_name from the corresponding list of names.
	for (int k = 0; k < n_kernels; k++) {
		free(kernel_poly_params[k].kernel_name);
	}

	/////////////////////////////////////
	////get symbols name and count for the type of kernel associated to pi.
	//// set kernel symbols.
	for (int k = 0; k < n_kernels; k++) {
		mcwp_error = symbol_info_clear(kernel_poly_params[k].symbol_info);
		check_mcwp_error(mcwp_error, "symbol_info_clear");
	}

	/////////////////////////////////////
	////init interpolator for each kernel.
//  for (int k = 0; k < n_kernels; k++)
//    {
//      mcwp_error = clear_kernel_poly_interpolator (&kernel_poly_params[k]);
//      check_mcwp_error(mcwp_error, "init_kernel_poly_interpolator");
//    }

	/////////////////////////////////////
	////set nvar: number of variables for interpolation.
	////also, set deg_bounds and denom_deg_bounds for each var.
	for (int k = 0; k < n_kernels; k++) {
//      free (kernel_poly_params[k].degree_bounds); // = (int*) malloc (deg_bounds_size);
//      free (kernel_poly_params[k].denomdegree_bounds);
		mcwp_error = clear_degree_bounds_for_each_var(
				kernel_poly_params[k].degree_bounds,
				kernel_poly_params[k].denomdegree_bounds);
	}

//  /////////////////////////////////////
//  ////clear interpolator list for each kernel.
//  //TODO add the clear function.
//  for (int k = 0; k < n_kernels; k++)
//    {
//      mcwp_error = kernel_poly_interpolator_list_clear (&kernel_poly_params[k]);
//      check_mcwp_error(mcwp_error, "kernel_poly_interpolator_list_clear");
//    }

	/////////////////////////////////////
	//// allocate space for ptx params; then, call the init function.
	for (int k = 0; k < n_kernels; k++) {
		mcwp_error = ptx_params_clear(kernel_poly_params[k].ptx_params,
				kernel_poly_params[k].n_points_in_mesh);
		check_mcwp_error(mcwp_error, "ptx_params_clear");
	}

	/////////////////////////////////////
	//// allocate space for ptx params; then, call the init function.
	for (int k = 0; k < n_kernels; k++) {
		for (int i = 0; i < kernel_poly_params[k].n_points_in_mesh; i++) {
			mcwp_error = param_list_clear(
					&kernel_poly_params[k].profiling_params_list[i]);
//	  check_mcwp_error(mcwp_error, "param_list_clear:profiling_params_list");
		}

		for (int i = 0; i < kernel_poly_params[k].n_points_in_mesh; i++) {
			mcwp_error = param_list_clear(
					&kernel_poly_params[k].rational_program_params_list[i]);
//	  check_mcwp_error(mcwp_error,
//			   "param_list_clear:rational_program_params_list");
		}
		free(kernel_poly_params[k].profiling_params_list);
		free(kernel_poly_params[k].rational_program_params_list);
	}

	/////////////////////////////////////
	for (int k = 0; k < n_kernels; k++) {
		free(kernel_poly_params[k].visitd_points_map);
	}
	/////////////////////////////////////
	free(kernel_poly_params);
	return EXIT_SUCCESS;
}

///////////////////////////////////////
//TODO: rename emulation to data collection. -> DONE.
int collect_data(data_collection_info_t * dci) {
	poly_params* kernel_poly_params = dci->kernel_poly_params;
	int n_kernels = dci->n_kernels;
	char ** kernel_names = dci->kernel_names;
	char* mem_inst_trace_str = dci->mem_inst_trace_str;
	int nvar = dci->nvar;
	char* device_params_path = dci->device_params_path;
//  kernel_type_enum_t kernel_type = dci->kernel_type;
//  int n_lower_bound = dci->n_lower_bound;
//  int n_upper_bound = dci->n_upper_bound;
	param_list_t* device_params = dci->device_params;
	profiling_info_t * pi = dci->profiling_info;

	/////////////////////////////////////
	print_long_dashed_line();
	////check the name of each kernel along with its id.
	for (int i = 0; i < n_kernels; i++) {
		printf("[@%s][kernel_%d]=[%s]\n", __func__, i, kernel_names[i]);
	}

	///////////////////////////////////////

	//running the emulation and collecting mcwp results.
	/**
	 * we assume all the kernels in the same src (.cu) file are of the same type.
	 * therefore, we iterate over the same mesh point for all of them.
	 * also, we assume all of them rely on the same N, B0, B1, B2.
	 */

	/**
	 * point_ptr will be used as the pointer to the current point of the mesh
	 * being used for all the kernels (assuming all have the same type).
	 */

	int n_points_in_mesh = pi->n_points_in_mesh;
//  /**
//   * allocate space for kernel launch params, profiling params and rational
//   * program params of each kernel.
//   * For each of the mentioned param types, the max size of "param_list" is
//   * "n_points_in_mesh" for each kernel.
//   */
//  for (int k = 0; k < n_kernels; k++)
//    {
//      size_t param_list_size_for_all_points = n_points_in_mesh
//	  * sizeof(param_list_t);
//      kernel_poly_params[k].profiling_params_list = (param_list_t*) malloc (
//	  param_list_size_for_all_points);
//      kernel_poly_params[k].rational_program_params_list =
//	  (param_list_t*) malloc (param_list_size_for_all_points);
//
//      for (int i = 0; i < n_points_in_mesh; i++)
//	{
//	  param_list_init (&kernel_poly_params[k].profiling_params_list[i],
//			   PARAM_LIST_TYPE_PROFILING);
//	}
//
//      for (int i = 0; i < n_points_in_mesh; i++)
//	{
//	  param_list_init (
//	      &kernel_poly_params[k].rational_program_params_list[i],
//	      PARAM_LIST_TYPE_RATIONAL_PROGRAM);
//	}
//    }

	//// this is for profiling all kernels.
//  profiling_info_t * pi = dci->profiling_info;

	profile_all_mesh_points(pi, mem_inst_trace_str);

	//copy the visited points map of pi to each kernel.
	//the space is already allocated in kernel_poly_params_init.
	for (int k = 0; k < n_kernels; k++) {
		memcpy(kernel_poly_params[k].visitd_points_map, pi->visited_points_map,
				n_points_in_mesh * sizeof(int));
	}

	/**
	 * parse the profiling info for each kernel. Calling this functino will fill
	 * in the kernel launch params list and profiling params list for each kernel.
	 */
	parse_all_profiling_results_for_all_kernels(pi, kernel_poly_params,
			n_kernels);

	for (int i = 0; i < n_points_in_mesh; i++) {
		if (!pi->visited_points_map[i])
			continue;

		for (int k = 0; k < n_kernels; k++) {
			if (!kernel_poly_params[k].visitd_points_map[i])
				continue;
//	  printf ("kernel-name=%s\n", kernel_poly_params[k].kernel_name);
			single_point_t *point_ptr = &pi->point_set->values[i];
//	  printf ("point: [n=%d][bx=%d][by=%d]\n", point_ptr->N[0],
//		  point_ptr->N[1], point_ptr->N[2]);

//	  param_list_print (&kernel_poly_params[k].profiling_params_list[i]);
			mpq_t * n_warps_qq =
					&(kernel_poly_params[k].profiling_params_list[i].mpq_values[PROFILING_PARAM_n_warps]);
			mpq_t * n_total_inst_qq =
					&(kernel_poly_params[k].profiling_params_list[i].mpq_values[PROFILING_PARAM_13_Total_insts]);
			mpq_t * n_mem_inst_qq =
					&(kernel_poly_params[k].profiling_params_list[i].mpq_values[PROFILING_PARAM_15_Mem_insts]);
			mpq_t * n_comp_inst_qq =
					&(kernel_poly_params[k].profiling_params_list[i].mpq_values[PROFILING_PARAM_14_Comp_insts]);

//	  printf ("nwarps=%d, insts=%d, comp_insts=%d, mem_insts=%d\n",
//		  mpq_get_ui (*n_warps_qq), mpq_get_ui (*n_total_inst_qq),
//		  mpq_get_ui (*n_comp_inst_qq), mpq_get_ui (*n_mem_inst_qq));

			int cnd0, cnd1, cnd2;
			cnd0 = (mpq_cmp_zero(*n_total_inst_qq) == 0);
			cnd1 = (mpq_cmp_zero(*n_mem_inst_qq) == 0);
			cnd2 = (mpq_cmp_zero(*n_comp_inst_qq) == 0);
			if (cnd0 || cnd1 || cnd2) {
				kernel_poly_params[k].visitd_points_map[i] = 0;
				continue;
			}
//	  printf ("cnd before -> %d %d %d\n", cnd0, cnd1, cnd2);
			mpq_div(*n_total_inst_qq, *n_total_inst_qq, *n_warps_qq);
			mpq_div(*n_mem_inst_qq, *n_mem_inst_qq, *n_warps_qq);
			mpq_ceil(*n_total_inst_qq);
			mpq_ceil(*n_mem_inst_qq);
			mpq_sub(*n_comp_inst_qq, *n_total_inst_qq, *n_mem_inst_qq);

			cnd0 = (mpq_cmp_zero(*n_total_inst_qq) == 0);
			cnd1 = (mpq_cmp_zero(*n_mem_inst_qq) == 0);
			cnd2 = (mpq_cmp_zero(*n_comp_inst_qq) == 0);
//	  printf ("cnd after -> %d %d %d\n", cnd0, cnd1, cnd2);
			if (cnd0 || cnd1 || cnd2) {
				kernel_poly_params[k].visitd_points_map[i] = 0;
				continue;
			}

			//	  param_list_print (
			//	      &kernel_poly_params[k].kernel_launch_params_list[i]);
		}
	}

//  printf ("end of adjusting total inst\n");
//  exit(EXIT_FAILURE);

//  //printing the retrieved values.
//  for (int i = 0; i < n_points_in_mesh; i++)
//    {
//      if (!pi->visited_points_map[i])
//	continue;
//
//      for (int k = 0; k < n_kernels; k++)
//	{
//	  if (!kernel_poly_params[k].visitd_points_map[i])
//	    continue;
//	  printf ("kernel-name=%s\n", kernel_poly_params[k].kernel_name);
//	  single_point_t *point_ptr = &pi->point_set->values[i];
//	  printf ("point: [n=%d][bx=%d][by=%d]\n", point_ptr->N[0],
//		  point_ptr->N[1], point_ptr->N[2]);
//	  param_list_print (&kernel_poly_params[k].profiling_params_list[i]);
////	  param_list_print (
////	      &kernel_poly_params[k].kernel_launch_params_list[i]);
//	}
//    }
//
//  exit(EXIT_FAILURE);
//  return;

	int max_mem_ld = 0;
	int max_mem_inst = 0;
	int max_comp_inst = 0;
	int max_total_inst = 0;
	/////////////////////////////////////
	for (int k = 0; k < n_kernels; k++) {
#if ALLOW_OMP_LOOPS
#pragma omp parallel for
#endif
		for (int i = 0; i < pi->n_points_in_mesh; i++) {
			if ((!pi->visited_points_map[i])
					|| (!kernel_poly_params[k].visitd_points_map[i]))
				continue;

			if ((!kernel_poly_params[k].visitd_points_map[i]))
				continue;
			single_point_t* point_ptr = &pi->point_set->values[i];
			int n, b0, b1, b2;
			n = point_ptr->N[0];
			b0 = point_ptr->N[1];
			b1 = point_ptr->N[2];
			b2 = 1;
#if VERBOSE
			printf ("MCWP for [%s][n=%4d, bx=%4d, by=%4d, bz=%4d]\n",
					kernel_poly_params[k].kernel_name, n, b0, b1, b2);
#endif
			int stat =
					compute_mwp_cwp_values_for_single_mesh_point_adjust_memLD(
							&kernel_poly_params[k], pi, i, device_params);
			if (stat != EXIT_SUCCESS) {
				continue;
			}
//	  int current_mem_ld =
//	      mpq_get_ui (
//		  kernel_poly_params[k].profiling_params_list[i].mpq_values[PROFILING_PARAM_dynamic_Mem_LD]);
//
//	  int current_total_inst =
//	      mpq_get_ui (
//		  kernel_poly_params[k].profiling_params_list[i].mpq_values[PROFILING_PARAM_13_Total_insts]);
//
//	  if (max_mem_ld == 0)
//	    max_mem_ld = current_mem_ld;
//	  if (current_mem_ld < max_mem_ld)
//	    max_mem_ld = current_mem_ld;
//
//	  if (current_total_inst > max_total_inst)
//	    {
//	      max_total_inst = current_total_inst;
//	      max_mem_inst =
//		  mpq_get_ui (
//		      kernel_poly_params[k].profiling_params_list[i].mpq_values[PROFILING_PARAM_15_Mem_insts]);
//	      max_comp_inst =
//		  mpq_get_ui (
//		      kernel_poly_params[k].profiling_params_list[i].mpq_values[PROFILING_PARAM_14_Comp_insts]);
//	    }
			//	  mwp_cwp (&trace_params, &device_params, &kernel_params, &ptx_params,
			//		   (*rep_div_n) (n), &current_result);
			////	      mwp_cwp (&trace_params, &device_params, &kernel_params,
			////		       &ptx_params, 1, &current_result);
			//	  int idx = add_mcwp_result_to_table (&current_result,
			//					      &kernel_poly_params[j]);
			//	      printf("received_idx=%d\n", idx);
			//	      printf("curren_table_idx++=%d\n", kernel_poly_params[j].mcwp_result_table_idx);
			//	      kernel_poly_params[j].point = get_next_kernel_mesh_point (
			//		  kernel_poly_params[j].point);
			//	  point_ptr = get_next_kernel_mesh_point (point_ptr);
			//#if VERBOSE
			//	  param_list_print (
			//	      &kernel_poly_params[k].rational_program_params_list[i]);
//  	  param_list_print (&kernel_poly_params[k].profiling_params_list[i]);
			//#endif
		}
	}

//  exit(EXIT_FAILURE);
//  for (int k = 0; k < n_kernels; k++)
//    {
//#if ALLOW_OMP_LOOPS
//#pragma omp parallel for
//#endif
//      for (int i = 0; i < pi->n_points_in_mesh; i++)
//	{
//	  if ((!pi->visited_points_map[i])
//	      || (!kernel_poly_params[k].visitd_points_map[i]))
//	    continue;
//
//	  if ((!kernel_poly_params[k].visitd_points_map[i]))
//	    continue;
//	  single_point_t* point_ptr = &pi->point_set->values[i];
//	  int n, b0, b1, b2;
//	  n = point_ptr->N[0];
//	  b0 = point_ptr->N[1];
//	  b1 = point_ptr->N[2];
//	  b2 = 1;
//#if VERBOSE
//	  printf ("MCWP for [%s][n=%4d, bx=%4d, by=%4d, bz=%4d]\n",
//	      kernel_poly_params[k].kernel_name, n, b0, b1, b2);
//#endif
//
////	  mpq_set_ui (
////	      kernel_poly_params[k].profiling_params_list[i].mpq_values[PROFILING_PARAM_dynamic_Mem_LD],
////	      max_mem_ld, 1);
////
////	  mpq_set_ui (
////	      kernel_poly_params[k].profiling_params_list[i].mpq_values[PROFILING_PARAM_13_Total_insts],
////	      max_total_inst, 1);
////	  mpq_set_ui (
////	      kernel_poly_params[k].profiling_params_list[i].mpq_values[PROFILING_PARAM_14_Comp_insts],
////	      max_comp_inst, 1);
////	  mpq_set_ui (
////	      kernel_poly_params[k].profiling_params_list[i].mpq_values[PROFILING_PARAM_15_Mem_insts],
////	      max_mem_inst, 1);
//
////    	  if (current_mem_ld>max_mem_ld)
////    	    max_mem_ld=current_mem_ld;
//	  //	  mwp_cwp (&trace_params, &device_params, &kernel_params, &ptx_params,
//	  //		   (*rep_div_n) (n), &current_result);
//	  ////	      mwp_cwp (&trace_params, &device_params, &kernel_params,
//	  ////		       &ptx_params, 1, &current_result);
//	  //	  int idx = add_mcwp_result_to_table (&current_result,
//	  //					      &kernel_poly_params[j]);
//	  //	      printf("received_idx=%d\n", idx);
//	  //	      printf("curren_table_idx++=%d\n", kernel_poly_params[j].mcwp_result_table_idx);
//	  //	      kernel_poly_params[j].point = get_next_kernel_mesh_point (
//	  //		  kernel_poly_params[j].point);
//	  //	  point_ptr = get_next_kernel_mesh_point (point_ptr);
//	  //#if VERBOSE
//	  //	  param_list_print (
//	  //	      &kernel_poly_params[k].rational_program_params_list[i]);
//	  //  	  param_list_print (&kernel_poly_params[k].profiling_params_list[i]);
//	  //#endif
//	}
//    }

	/////////////////////////////////////

	//// compute mcwp

	for (int k = 0; k < n_kernels; k++) {
#if ALLOW_OMP_LOOPS
#pragma omp parallel for
#endif
		for (int i = 0; i < pi->n_points_in_mesh; i++) {
			if ((!pi->visited_points_map[i])
					|| (!kernel_poly_params[k].visitd_points_map[i]))
				continue;

			if ((!kernel_poly_params[k].visitd_points_map[i]))
				continue;
			single_point_t* point_ptr = &pi->point_set->values[i];
			int n, b0, b1, b2;
			n = point_ptr->N[0];
			b0 = point_ptr->N[1];
			b1 = point_ptr->N[2];
			b2 = 1;
#if VERBOSE
			printf ("MCWP for [%s][n=%4d, bx=%4d, by=%4d, bz=%4d]\n",
					kernel_poly_params[k].kernel_name, n, b0, b1, b2);
#endif
			int stat = compute_mwp_cwp_values_for_single_mesh_point(
					&kernel_poly_params[k], pi, i, device_params);
			if (stat != EXIT_SUCCESS) {
				printf("FAILED MCWP for [%s][n=%4d, bx=%4d, by=%4d, bz=%4d]\n",
						kernel_poly_params[k].kernel_name, n, b0, b1, b2);
				kernel_poly_params[k].visitd_points_map[i] = 0;
				continue;
			}
//	  mwp_cwp (&trace_params, &device_params, &kernel_params, &ptx_params,
//		   (*rep_div_n) (n), &current_result);
////	      mwp_cwp (&trace_params, &device_params, &kernel_params,
////		       &ptx_params, 1, &current_result);
//	  int idx = add_mcwp_result_to_table (&current_result,
//					      &kernel_poly_params[j]);
			//	      printf("received_idx=%d\n", idx);
			//	      printf("curren_table_idx++=%d\n", kernel_poly_params[j].mcwp_result_table_idx);
//	      kernel_poly_params[j].point = get_next_kernel_mesh_point (
//		  kernel_poly_params[j].point);
//	  point_ptr = get_next_kernel_mesh_point (point_ptr);
//#if VERBOSE
//	  param_list_print (
//	      &kernel_poly_params[k].rational_program_params_list[i]);
//	  param_list_print (&kernel_poly_params[k].profiling_params_list[i]);
//#endif
		}
	}
//
//  for (int k = 0; k < n_kernels; k++)
//      {
//  #if ALLOW_OMP_LOOPS
//  #pragma omp parallel for
//  #endif
//        for (int i = 0; i < pi->n_points_in_mesh; i++)
//  	{
//  	  if ((!pi->visited_points_map[i])
//  	      || (!kernel_poly_params[k].visitd_points_map[i]))
//  	    continue;
//
//  	  if ((!kernel_poly_params[k].visitd_points_map[i]))
//  	    continue;
//  	  single_point_t* point_ptr = &pi->point_set->values[i];
//  	  int n, b0, b1, b2;
//  	  n = point_ptr->N[0];
//  	  b0 = point_ptr->N[1];
//  	  b1 = point_ptr->N[2];
//  	  b2 = 1;
//  #if VERBOSE
//  	  printf ("MCWP for [%s][n=%4d, bx=%4d, by=%4d, bz=%4d]\n",
//  	      kernel_poly_params[k].kernel_name, n, b0, b1, b2);
//  #endif
//  	  int stat = compute_mwp_cwp_values_for_single_mesh_point (
//  	      &kernel_poly_params[k], pi, i, device_params);
//  	  if (stat != EXIT_SUCCESS)
//  	    {
//  	      kernel_poly_params[k].visitd_points_map[i] = 0;
//  	      continue;
//  	    }
//  //	  mwp_cwp (&trace_params, &device_params, &kernel_params, &ptx_params,
//  //		   (*rep_div_n) (n), &current_result);
//  ////	      mwp_cwp (&trace_params, &device_params, &kernel_params,
//  ////		       &ptx_params, 1, &current_result);
//  //	  int idx = add_mcwp_result_to_table (&current_result,
//  //					      &kernel_poly_params[j]);
//  	  //	      printf("received_idx=%d\n", idx);
//  	  //	      printf("curren_table_idx++=%d\n", kernel_poly_params[j].mcwp_result_table_idx);
//  //	      kernel_poly_params[j].point = get_next_kernel_mesh_point (
//  //		  kernel_poly_params[j].point);
//  //	  point_ptr = get_next_kernel_mesh_point (point_ptr);
//  //#if VERBOSE
//  //	  param_list_print (
//  //	      &kernel_poly_params[k].rational_program_params_list[i]);
//  //	  param_list_print (
//  //	  	      &kernel_poly_params[k].profiling_params_list[i]);
//  //#endif
//  	}
//      }

//  exit(EXIT_SUCCESS);

//  (profiling_info_t*) malloc (sizeof(profiling_info_t));
//    profiling_info_init (pi, n_kernels, n_points_in_mesh, starting_point_ptr,
//  		       n_lower_bound, n_upper_bound);
//
//  profiling_info_clear (pi);

	return EXIT_SUCCESS;
}

///////////////////////////////////////
int interpolation_entity_table_init(interpolation_entity_table_t * et,
		const int capacity) {
	et->capacity = capacity;
	et->size = 0;
	et->entity_list = (interpolation_entity_t*) malloc(
			et->capacity * sizeof(interpolation_entity_t));

	return EXIT_SUCCESS;
}

///////////////////////////////////////
//does not check for duplicates. it is the callers responsibility to not pass duplicates.
int interpolation_entity_table_add_entry(interpolation_entity_table_t * et,
		param_list_t* param_list_ptr, int param_name_alias) {
	if (et->size > et->capacity) {
		printf("[@%s][et->size > et->capacity][table is full!]\n", __func__);
		return EXIT_FAILURE;
	}

	interpolation_entity_t * ptr = &et->entity_list[et->size];

	ptr->param_name_alias = param_name_alias;
	ptr->param_list_ptr = param_list_ptr;
	ptr->param_name = param_list_ptr->dict[param_name_alias];

#if VERBOSE
	printf ("[added entity to interpolation entity table]\n"
			"--[%s (type=%s)][idx=%d]\n",
			ptr->param_name,
			param_list_type_names_dict[ptr->param_list_ptr->type], et->size);
	print_short_dashed_line ();
#endif
	et->size++;
	return EXIT_SUCCESS;
}

///////////////////////////////////////
int interpolation_entity_table_clear(const interpolation_entity_table_t * et) {
	if (et->entity_list)
		free(et->entity_list);
	return EXIT_SUCCESS;
}

///////////////////////////////////////
int kernel_poly_params_interpolation_entity_table_init(
		data_collection_info_t * dci) {
	poly_params * kernel_poly_params = dci->kernel_poly_params;
	int n_kernels = dci->n_kernels;
	int mcwp_error = 0;

	/////////////////////////////////////
	int n_ptx_params = 2;
	int param_name_aliases_for_ptx[2] = { PTX_PARAM_registers,
			PTX_PARAM_shared_mem_bytes_static };

	int n_device_params = DEVICE_PARAMS_ENUM_SIZE;
	int param_name_aliases_for_device[DEVICE_PARAMS_ENUM_SIZE];
	for (int i = 0; i < DEVICE_PARAMS_ENUM_SIZE; i++)
		param_name_aliases_for_device[i] = i;

	int n_static_params = n_device_params + n_ptx_params;

	/////////////////////////////////////
	int param_name_aliases_for_profiling[PROFILING_PARAMS_ENUM_SIZE];
	int n_profiling_params = 0;

//  for (int i = 0; i < PROFILING_PARAMS_ENUM_SIZE; i++)
//    param_name_aliases_for_profiling[i] = i;
//  param_name_aliases_for_profiling[n_profiling_params++] =
//      PROFILING_PARAM_13_Total_insts;
	param_name_aliases_for_profiling[n_profiling_params++] =
			PROFILING_PARAM_14_Comp_insts;
	param_name_aliases_for_profiling[n_profiling_params++] =
			PROFILING_PARAM_15_Mem_insts;
	param_name_aliases_for_profiling[n_profiling_params++] =
			PROFILING_PARAM_dynamic_Mem_LD;
//  param_name_aliases_for_profiling[n_profiling_params++] =
//      PROFILING_PARAM_Active_blocks_per_SM;
//  param_name_aliases_for_profiling[n_profiling_params++] =
//      PROFILING_PARAM_Blocks;
//  param_name_aliases_for_profiling[n_profiling_params++] =
//      PROFILING_PARAM_n_warps;
//  param_name_aliases_for_profiling[n_profiling_params++] =
//      PROFILING_PARAM_Active_warps_per_SM;
	param_name_aliases_for_profiling[n_profiling_params++] =
			PROFILING_PARAM_shared_mem_bytes_dynamic;

	int n_rp_params = 0;
	int param_name_aliases_for_rp[RATIONAL_PROGRAM_PARAMS_ENUM_SIZE];
//  for (int i = 0; i < RATIONAL_PROGRAM_PARAMS_ENUM_SIZE; i++)
//    param_name_aliases_for_rp[i] = i;

//  param_name_aliases_for_rp[n_rp_params++] =
//      RATIONAL_PROGRAM_PARAM_shared_mem_bytes_total;

	int n_all_params = n_profiling_params + n_rp_params;
	/////////////////////////////////////

	//init interp table for all the kernels;
	for (int k = 0; k < n_kernels; k++) {
		interpolation_entity_table_t * et =
				&(kernel_poly_params[k].interp_entity_table);
		mcwp_error = interpolation_entity_table_init(et, n_all_params);
		check_mcwp_error(mcwp_error, "interpolation_entity_table_init");

		interpolation_entity_table_t * et_static =
				&(kernel_poly_params[k].interp_entity_table_for_static_params);
		mcwp_error = interpolation_entity_table_init(et_static,
				n_static_params);
		check_mcwp_error(mcwp_error,
				"interpolation_entity_table_init: for static params");
	}

	//add params to interp table.
	for (int k = 0; k < n_kernels; k++) {
		interpolation_entity_table_t * et =
				&(kernel_poly_params[k].interp_entity_table);

		interpolation_entity_table_t * et_static =
				&(kernel_poly_params[k].interp_entity_table_for_static_params);

		for (int i = 0; i < n_profiling_params; i++) {
			mcwp_error = interpolation_entity_table_add_entry(et,
					kernel_poly_params[k].profiling_params_list,
					param_name_aliases_for_profiling[i]);
			check_mcwp_error(mcwp_error,
					"interpolation_entity_table_add_entry : profiling_params_list");
		}

		for (int i = 0; i < n_rp_params; i++) {
			mcwp_error = interpolation_entity_table_add_entry(et,
					kernel_poly_params[k].rational_program_params_list,
					param_name_aliases_for_rp[i]);
			check_mcwp_error(mcwp_error,
					"interpolation_entity_table_add_entry : rational_program_params_list");
		}

		for (int i = 0; i < n_device_params; i++) {
			mcwp_error = interpolation_entity_table_add_entry(et_static,
					dci->device_params, param_name_aliases_for_device[i]);
			check_mcwp_error(mcwp_error,
					"interpolation_entity_table_add_entry : dci->device_params");
		}

		for (int i = 0; i < n_ptx_params; i++) {
			mcwp_error = interpolation_entity_table_add_entry(et_static,
					kernel_poly_params[k].ptx_params,
					param_name_aliases_for_ptx[i]);
			check_mcwp_error(mcwp_error,
					"interpolation_entity_table_add_entry : ptx_params");
		}

		////add other param name lists here.
		//      for (int i = 0; i < n_profiling_params; i++)
		//	{
		//	  mcwp_error = interpolation_entity_table_add_entry (
		//	      &et, dci->kernel_poly_params[k].profiling_params_list,
		//	      param_name_aliases_for_profiling[i]);
		//	  check_mcwp_error(mcwp_error, "interpolation_entity_table_add_entry");
		//	}
	}

	for (int k = 0; k < n_kernels; k++) {
		mcwp_error = kernel_poly_interpolation_data_list_init(
				&(kernel_poly_params[k]));
		check_mcwp_error(mcwp_error, "kernel_poly_interpolation_data_list_init");
	}

	return EXIT_SUCCESS;
}

///////////////////////////////////////
int kernel_poly_params_interpolation_entity_table_clear(
		data_collection_info_t * dci) {
	poly_params * kernel_poly_params = dci->kernel_poly_params;
	int n_kernels = dci->n_kernels;
	int mcwp_error = 0;

	for (int k = 0; k < n_kernels; k++) {
		mcwp_error = kernel_poly_interpolation_data_list_clear(
				&(kernel_poly_params[k]));
		check_mcwp_error(mcwp_error,
				"kernel_poly_interpolation_data_list_clear");
	}

	//clear interp table for all the kernels;
	for (int k = 0; k < n_kernels; k++) {
		interpolation_entity_table_t * et =
				&(kernel_poly_params[k].interp_entity_table);
		mcwp_error = interpolation_entity_table_clear(et);
		check_mcwp_error(mcwp_error, "interpolation_entity_table_clear");

		interpolation_entity_table_t * et_static =
				&(kernel_poly_params[k].interp_entity_table_for_static_params);
		mcwp_error = interpolation_entity_table_clear(et_static);
		check_mcwp_error(mcwp_error,
				"interpolation_entity_table_clear: static params");
	}

	return EXIT_SUCCESS;
}

//////////////////////////////////////
int kernel_poly_params_interpolation_data_add_points(
		data_collection_info_t * dci) {
	poly_params * kernel_poly_params = dci->kernel_poly_params;
	int n_kernels = dci->n_kernels;
	int mcwp_error = 0;

	//init interp table for all the kernels;
	for (int k = 0; k < n_kernels; k++) {
		poly_params * k_ptr = &kernel_poly_params[k];

		mpq_t **point_set_qq;
		point_set_qq = (mpq_t**) malloc(
				k_ptr->n_points_in_mesh * sizeof(mpq_t*));

		for (int i = 0; i < k_ptr->n_points_in_mesh; i++) {
			point_set_qq[i] = (mpq_t*) malloc(k_ptr->nvar * sizeof(mpq_t));
		}

		mpq_t *vals_qq;
		vals_qq = (mpq_t*) malloc(k_ptr->n_points_in_mesh * sizeof(mpq_t));

		int point_set_qq_idx = 0;

		//extract all the visited points and store them as a 2D mpq_t array (points, dims).
		for (int point_idx = 0; point_idx < k_ptr->n_points_in_mesh;
				point_idx++) {
			if (!k_ptr->visitd_points_map[point_idx])
				continue;

			single_point_t * point_ptr = &k_ptr->point_set->values[point_idx];
			for (int i = 0; i < k_ptr->nvar; i++) {
				mpq_init(point_set_qq[point_set_qq_idx][i]);
				mpq_set_ui(point_set_qq[point_set_qq_idx][i], point_ptr->N[i],
						1);
			}
			mpq_init(vals_qq[point_set_qq_idx]);
			point_set_qq_idx++;
		}

		//load the values for each interpolation entity.
		for (int i = 0; i < k_ptr->interp_entity_table.size; i++) {
			point_set_qq_idx = 0;
			//accessing the interpolator, num, and denom poly.
			interpolation_data_t * id = &(k_ptr->interpolation_data_list[i]);
			//accessing the param value list that contains "value" for the current point.
			interpolation_entity_t * et =
					&(k_ptr->interp_entity_table.entity_list[i]);
			for (int point_idx = 0; point_idx < k_ptr->n_points_in_mesh;
					point_idx++) {
				if (!k_ptr->visitd_points_map[point_idx])
					continue;
#if VERBOSE
				printf ("[kernel: %d]...[valid_point: %d]\n", k, point_idx);
#endif

				mpq_t * val =
						&et->param_list_ptr[point_idx].mpq_values[et->param_name_alias];
//	      rfInterpAddPointValMP (id->interpolator, point_qq, *val);

				mpq_set(vals_qq[point_set_qq_idx], *val);
				point_set_qq_idx++;

//#if VERBOSE
//	      gmp_printf (
//		  "adding point[%-30s]=[%-20Qd][n=%-4Qd, b0=%-4Qd, b1=%-4Qd]\n",
//		  et->param_name, *val, point_qq[0], point_qq[1], point_qq[2]);
//#endif
			}
			rfInterpAddPointsMP(id->interpolator, point_set_qq, vals_qq,
					point_set_qq_idx);
#if VERBOSE
			print_short_dashed_line ();
#endif
		}

		for (int point_idx = 0; point_idx < point_set_qq_idx; point_idx++) {
			for (int i = 0; i < k_ptr->nvar; i++)
				mpq_clear(point_set_qq[point_idx][i]);
			mpq_clear(vals_qq[point_idx]);
		}

		for (int i = 0; i < k_ptr->n_points_in_mesh; i++) {
			free(point_set_qq[i]);
		}
		free(point_set_qq);
		free(vals_qq);
	}
//
//  //add params to interp table.
//  for (int k = 0; k < n_kernels; k++)
//    {
//      interpolation_entity_table_t * et =
//	  &(kernel_poly_params[k].interp_entity_table);
//      for (int i = 0; i < n_profiling_params; i++)
//	{
//	  mcwp_error = interpolation_entity_table_add_entry (
//	      et, kernel_poly_params[k].profiling_params_list,
//	      param_name_aliases_for_profiling[i]);
//	  check_mcwp_error(
//	      mcwp_error,
//	      "interpolation_entity_table_add_entry : profiling_params_list");
//	}
//
//      ////add other param name lists here.
//      //      for (int i = 0; i < n_profiling_params; i++)
//      //	{
//      //	  mcwp_error = interpolation_entity_table_add_entry (
//      //	      &et, dci->kernel_poly_params[k].profiling_params_list,
//      //	      param_name_aliases_for_profiling[i]);
//      //	  check_mcwp_error(mcwp_error, "interpolation_entity_table_add_entry");
//      //	}
//    }
//
//  for (int k = 0; k < n_kernels; k++)
//    {
//      mcwp_error = kernel_poly_interpolation_data_list_init (
//	  &(kernel_poly_params[k]));
//      check_mcwp_error(mcwp_error, "kernel_poly_interpolation_data_list_init");
//    }

	return EXIT_SUCCESS;
}

///////////////////////////////////////
int kernel_poly_params_interpolation_data_add_points_v0(
		data_collection_info_t * dci) {
	poly_params * kernel_poly_params = dci->kernel_poly_params;
	int n_kernels = dci->n_kernels;
	int mcwp_error = 0;

	//init interp table for all the kernels;
	for (int k = 0; k < n_kernels; k++) {
		poly_params * k_ptr = &kernel_poly_params[k];
		for (int point_idx = 0; point_idx < k_ptr->n_points_in_mesh;
				point_idx++) {
			if (!k_ptr->visitd_points_map[point_idx])
				continue;

#if VERBOSE
			printf ("[kernel: %d]...[valid_point: %d]\n", k, point_idx);
#endif
			single_point_t * point_ptr = &k_ptr->point_set->values[point_idx];
			mpq_t point_qq[3];
			for (int i = 0; i < 3; i++) {
				mpq_init(point_qq[i]);
				mpq_set_ui(point_qq[i], point_ptr->N[i], 1);
			}
			for (int i = 0; i < k_ptr->interp_entity_table.size; i++) {
				//accessing the interpolator, num, and denom poly.
				interpolation_data_t * id = &(k_ptr->interpolation_data_list[i]);
				//accessing the param value list that contains "value" for the current point.
				interpolation_entity_t * et =
						&(k_ptr->interp_entity_table.entity_list[i]);

				mpq_t * val =
						&et->param_list_ptr[point_idx].mpq_values[et->param_name_alias];
				rfInterpAddPointValMP(id->interpolator, point_qq, *val);
#if VERBOSE
				gmp_printf (
						"adding point[%-30s]=[%-20Qd][n=%-4Qd, b0=%-4Qd, b1=%-4Qd]\n",
						et->param_name, *val, point_qq[0], point_qq[1], point_qq[2]);
#endif
			}
#if VERBOSE
			print_short_dashed_line ();
#endif

			for (int i = 0; i < 3; i++)
				mpq_clear(point_qq[i]);
		}

	}
//
//  //add params to interp table.
//  for (int k = 0; k < n_kernels; k++)
//    {
//      interpolation_entity_table_t * et =
//	  &(kernel_poly_params[k].interp_entity_table);
//      for (int i = 0; i < n_profiling_params; i++)
//	{
//	  mcwp_error = interpolation_entity_table_add_entry (
//	      et, kernel_poly_params[k].profiling_params_list,
//	      param_name_aliases_for_profiling[i]);
//	  check_mcwp_error(
//	      mcwp_error,
//	      "interpolation_entity_table_add_entry : profiling_params_list");
//	}
//
//      ////add other param name lists here.
//      //      for (int i = 0; i < n_profiling_params; i++)
//      //	{
//      //	  mcwp_error = interpolation_entity_table_add_entry (
//      //	      &et, dci->kernel_poly_params[k].profiling_params_list,
//      //	      param_name_aliases_for_profiling[i]);
//      //	  check_mcwp_error(mcwp_error, "interpolation_entity_table_add_entry");
//      //	}
//    }
//
//  for (int k = 0; k < n_kernels; k++)
//    {
//      mcwp_error = kernel_poly_interpolation_data_list_init (
//	  &(kernel_poly_params[k]));
//      check_mcwp_error(mcwp_error, "kernel_poly_interpolation_data_list_init");
//    }

	return EXIT_SUCCESS;
}

///////////////////////////////////////

int kernel_poly_params_set_static_params_to_const_poly(
		data_collection_info_t * dci) {

	poly_params * kernel_poly_params = dci->kernel_poly_params;
	int n_kernels = dci->n_kernels;
	int mcwp_error = 0;

	mpq_t one_qq;
	mpq_init(one_qq);
	mpq_set_ui(one_qq, 1, 1);

	//init interp table for all the kernels;
	for (int k = 0; k < n_kernels; k++) {
		poly_params * k_ptr = &kernel_poly_params[k];
		for (int i = 0; i < k_ptr->interp_entity_table_for_static_params.size;
				i++) {
			//accessing the interpolator, num, and denom poly.
			interpolation_data_t * id =
					&(k_ptr->interpolation_data_list_for_static_params[i]);
			//accessing the param value list that contains "value" for the current point.
			interpolation_entity_t * et_static =
					&(k_ptr->interp_entity_table_for_static_params.entity_list[i]);
			mpq_t *val =
					&et_static->param_list_ptr->mpq_values[et_static->param_name_alias];
#if VERBOSE
			gmp_printf ("setting param [%-30s][%-30Qd] to a const poly\n",
					et_static->param_name, *val);
#endif
			id->num_poly = makeConstPolynomial_AA(1, 0, *val);
			id->denom_poly = makeConstPolynomial_AA(1, 0, one_qq);
			id->status = SUCCESSFUL_INTERP;
		}
//#if VERBOSE
//      print_short_dashed_line ();
//#endif
	}
	mpq_clear(one_qq);

//
//  //add params to interp table.
//  for (int k = 0; k < n_kernels; k++)
//    {
//      interpolation_entity_table_t * et =
//	  &(kernel_poly_params[k].interp_entity_table);
//      for (int i = 0; i < n_profiling_params; i++)
//	{
//	  mcwp_error = interpolation_entity_table_add_entry (
//	      et, kernel_poly_params[k].profiling_params_list,
//	      param_name_aliases_for_profiling[i]);
//	  check_mcwp_error(
//	      mcwp_error,
//	      "interpolation_entity_table_add_entry : profiling_params_list");
//	}
//
//      ////add other param name lists here.
//      //      for (int i = 0; i < n_profiling_params; i++)
//      //	{
//      //	  mcwp_error = interpolation_entity_table_add_entry (
//      //	      &et, dci->kernel_poly_params[k].profiling_params_list,
//      //	      param_name_aliases_for_profiling[i]);
//      //	  check_mcwp_error(mcwp_error, "interpolation_entity_table_add_entry");
//      //	}
//    }
//
//  for (int k = 0; k < n_kernels; k++)
//    {
//      mcwp_error = kernel_poly_interpolation_data_list_init (
//	  &(kernel_poly_params[k]));
//      check_mcwp_error(mcwp_error, "kernel_poly_interpolation_data_list_init");
//    }

	return EXIT_SUCCESS;
}

///////////////////////////////////////
int kernel_poly_params_interpolate_performance_params(
		data_collection_info_t * dci) {

	poly_params * kernel_poly_params = dci->kernel_poly_params;
	int n_kernels = dci->n_kernels;

	//for each kernel, interpolate all params in its entity table.
	for (int k = 0; k < n_kernels; k++) {
		poly_params * k_ptr = &kernel_poly_params[k];
//#if ALLOW_OMP_LOOPS
//#pragma omp parallel for
//#endif
		for (int i = 0; i < k_ptr->interp_entity_table.size; i++) {
#if VERBOSE
			printf ("interpolating entity number [%d]\n", i);
#endif
			interpolation_entity_t * et =
					&(k_ptr->interp_entity_table.entity_list[i]);
			//accessing the interpolator, num, and denom poly.
			interpolation_data_t * id = &(k_ptr->interpolation_data_list[i]);
			int stat = rfInterpGetPoly(id->interpolator, &id->num_poly,
					&id->denom_poly,
					INTERP_MAX_RESIDUAL);

			rfInterpFree(id->interpolator);
			if (stat == POLY_INTERP_SUCCESS) {
				id->status = SUCCESSFUL_INTERP;
			} else {
				//TODO: better to communicate error using "PolyInterpStatus_t"
				id->status = FAILED_INTERP;
				printf("[kernel:%d]..."
						"[interpolating param: %d (%s)]"
						"[WARNING: FAILED INTERP]\n", k, i, et->param_name);
				continue;
			}
//#if VERBOSE
			printf("[kernel:%d]..."
					"[interpolating param: %d (%s)]\n", k, i, et->param_name);
//#endif
		}
		print_long_dashed_line();
	}
	return EXIT_SUCCESS;
}

///////////////////////////////////////

//needs: {param_name, device_name, num_poly_str, num_poly_str, denom_poly_str
int generate_header_for_param(const char * kernel_name, const char * param_name,
		const char *device_name, const AltArr_t *num_poly,
		const AltArr_t *denom_poly) {
#if VERBOSE
	printf ("[generating header for [%s] [%s]]\n", param_name, device_name);
#endif

	char klaraptor_path[4096];
	sprintf(klaraptor_path, "%s", getenv("KLARAPTOR_PATH"));
	const char * include_format_str = ""
			"#ifndef EVAL_%s_%s_H_\n"
			"#define EVAL_%s_%s_H_\n\n"
			"//////////////////////////////////////////\n"
			"#include <stdio.h>\n"
			"#include <stdlib.h>\n"
			"#include <gmp.h>\n"
			"#include \"%s/src/RatFunInterp/include/interpolator.h\"\n\n"
			"//////////////////////////////////////////\n";

	const char * end_of_header_str = "#endif";

	const char * numer_func_format_str = "AltArr_t* get_numer_%s()\n{\n";
	const char * denom_func_format_str = "AltArr_t* get_denom_%s()\n{\n";

	const char * separator_line =
			"\n//////////////////////////////////////////\n";
	const char * func_return_format_str = "\nreturn %s;\n}\n";
	const char * numer_var_name = "x";
	const char * denom_var_name = "x";

	char * numer_str = polyToProgram_AA(num_poly, numer_var_name);
	char * denom_str = polyToProgram_AA(denom_poly, denom_var_name);

	/**
	 * header -> replace (%s,%s):(param,device)
	 * numer_function_decl
	 * numer_function_body
	 * numer_function_return
	 * separator_line
	 * denom_function_decl
	 * denom_function_body
	 * denom_function_return
	 */

	size_t param_name_len = strlen(param_name);
	size_t device_name_len = strlen(device_name);

	size_t include_str_len = strlen(include_format_str) + 2 * param_name_len
			+ 2 * device_name_len + strlen(klaraptor_path);
	size_t numer_func_str_len = strlen(numer_func_format_str) + (param_name_len)
			+ strlen(numer_str) + strlen(func_return_format_str)
			+ strlen(numer_var_name) + strlen(separator_line);
	size_t denom_func_str_len = strlen(denom_func_format_str) + (param_name_len)
			+ strlen(denom_str) + strlen(func_return_format_str)
			+ strlen(denom_var_name) + strlen(separator_line);

	size_t header_str_len = include_str_len + numer_func_str_len
			+ denom_func_str_len + strlen(end_of_header_str)
			+ strlen(separator_line);

	char * header_str = (char*) malloc(header_str_len);

	int header_str_size = 0;
	header_str_size += sprintf(header_str + header_str_size, include_format_str,
			param_name, device_name, param_name, device_name, klaraptor_path);
	/////////////////////////////////////
	//declaration of get_numer_param () function.

	header_str_size += sprintf(header_str + header_str_size,
			numer_func_format_str, param_name);
	header_str_size += sprintf(header_str + header_str_size, "%s", numer_str);
	header_str_size += sprintf(header_str + header_str_size,
			func_return_format_str, numer_var_name);
	header_str_size += sprintf(header_str + header_str_size, "%s",
			separator_line);
	/////////////////////////////////////
	//declaration of get_denom_param () function.

	header_str_size += sprintf(header_str + header_str_size,
			denom_func_format_str, param_name);
	header_str_size += sprintf(header_str + header_str_size, "%s", denom_str);
	header_str_size += sprintf(header_str + header_str_size,
			func_return_format_str, denom_var_name);
	header_str_size += sprintf(header_str + header_str_size, "%s",
			separator_line);
	/////////////////////////////////////
	//end of header file.
	header_str_size += sprintf(header_str + header_str_size, "%s%s",
			end_of_header_str, separator_line);
	/////////////////////////////////////
//  printf ("%s", header_str);
	/////////////////////////////////////
	//writing result to file.
	/////////////////////////////////////
	char path[PATH_MAX];
	sprintf(path, "kernel_%s_%s_%s.h", kernel_name, param_name, device_name);
	FILE* output_file = fopen(path, "w");
	fwrite(header_str, 1, header_str_size, output_file);
	fclose(output_file);

	free(header_str);
	free(numer_str);
	free(denom_str);
	return EXIT_SUCCESS;
//  PolyInterpStatus_t stat;
//  char var_name[10];
//  //  for (int case_idx = 0; case_idx < 1; case_idx++)
//  int case_idx = mcwp_case;
//    {
//      stat = rfInterpGetPoly (kernel_poly->interp[case_idx],
//			      &kernel_poly->poly[case_idx],
//			      &kernel_poly->denompoly[case_idx], eps);
//
//      //      printf ("building header for rational program ...\n");
//      //      printf("STAT=%d\n", stat);
//      if (stat == POLY_INTERP_SUCCESS)
//	{
//	  kernel_poly->status[case_idx] = SUCCESSFUL_INTERP;
//
//	  //should be moved to verbose mode.
//#if VERBOSE
//	  print_interpolation_result_for_kernel (kernel_poly, stdout);
//	  fprintf (stderr, "[INTERPOLATION SUCCESSFUL for [%s]!]...\n",
//		   param_name);
//#endif
//	  //	  print_to_header (occupancy_header_file, &kernel_poly, case_idx);
//	  char *numer_str, *denom_str;
//
//	  fprintf (file, "AltArr_t* get_numer_%s(){\n", param_name);
//	  sprintf (var_name, "numer", case_idx + 1);
//	  if (is_zero_poly (kernel_poly->poly[mcwp_case], kernel_poly->nvar)
//	      == 1)
//	    {
//	      fprintf (file, "AltArr_t* numer = makePolynomial_AA(1, %d);\n",
//		       kernel_poly->nvar);
//	      fprintf (file, "mpq_init(numer->elems[0].coef);\n");
//	      fprintf (
//		  file,
//		  "mpz_set_str(mpq_numref(numer->elems[0].coef), \"0\",10);\n");
//	      fprintf (
//		  file,
//		  "mpz_set_str(mpq_denref(numer->elems[0].coef), \"1\",10);\n");
//	      fprintf (file, "numer->elems[0].degs=1;\n");
//	      fprintf (file, "numer->size=1;\n");
//	      fprintf (file, "return numer;\n}\n");
//	    }
//	  else
//
//	    {
//	      numer_str = polyToProgram_AA (kernel_poly->poly[case_idx],
//					    var_name);
//	      fprintf (file, "%s\n", numer_str);
//	      fprintf (file, "return %s;\n}\n", var_name);
//	    }
//
//	  fprintf (file, "/////////////////////\n");
//
//	  fprintf (file, "AltArr_t* get_denom_%s (){\n", param_name);
//
//	  if (is_zero_poly (kernel_poly->poly[mcwp_case], kernel_poly->nvar)
//	      == 1)
//	    {
//	      fprintf (file, "AltArr_t* denom = makePolynomial_AA(1, %d);\n",
//		       kernel_poly->nvar);
//	      fprintf (file, "mpq_init(denom->elems[0].coef);\n");
//	      fprintf (
//		  file,
//		  "mpz_set_str(mpq_numref(denom->elems[0].coef), \"1\",10);\n");
//	      fprintf (
//		  file,
//		  "mpz_set_str(mpq_denref(denom->elems[0].coef), \"1\",10);\n");
//	      fprintf (file, "denom->elems[0].degs=1;\n");
//	      fprintf (file, "denom->size=1;\n");
//	      fprintf (file, "return denom;\n}\n");
//	    }
//	  else
//
//	    {
//	      sprintf (var_name, "denom", case_idx + 1);
//	      denom_str = polyToProgram_AA (kernel_poly->denompoly[case_idx],
//					    var_name);
//
//	      fprintf (file, "%s\n", denom_str);
//	      fprintf (file, "return %s;\n}\n", var_name);
//	    }
//	  fprintf (file, "/////////////////////\n");
//	}
//      else if (stat == POLY_INTERP_FAILURE)
//	{
//
//	  kernel_poly->status[case_idx] = FAILED_INTERP;
//#if VERBOSE
//	  fprintf (stderr, "[ERROR: Failed to interpolate for %s!]...\n",
//		   param_name);
//#endif
//
//	  fprintf (file, "AltArr_t* get_numer_%s (){\n", param_name);
//	  fprintf (file, "return NULL;\n}\n");
//	  fprintf (file, "/////////////////////\n");
//
//	  fprintf (file, "AltArr_t* get_denom_%s (){\n", param_name);
//	  fprintf (file, "return NULL;\n}\n");
//	  fprintf (file, "/////////////////////\n");
//
//	}
//    }
//
//  fprintf (file, "/////////////////////\n");
//  fprintf (file, "#endif\n");
//  fflush (file);
//  fprintf (header_file, "%s", buffer);
//  //#if VERBOSE
//
//  //  fprintf (stderr, "written to [%s]\n", header_path);
//  fprintf (stderr, "[@interpolation][header for rational program]->[%s]\n",
//	   header_path);
//  //#endif
//  //  printf (short_dashed_line);
//
//  fclose (file);
//  fclose (header_file);
//
//  free (header_path);
}

///////////////////////////////////////

int kernel_poly_params_generate_rp_headers(data_collection_info_t * dci) {
	poly_params * kernel_poly_params = dci->kernel_poly_params;
	int n_kernels = dci->n_kernels;

	int mcwp_error;

	//for each kernel, interpolate all params in its entity table.
	for (int k = 0; k < n_kernels; k++) {
		poly_params * k_ptr = &kernel_poly_params[k];
#if ALLOW_OMP_LOOPS
#pragma omp parallel for
#endif
		for (int i = 0; i < k_ptr->interp_entity_table.size; i++) {
			interpolation_entity_t * et =
					&(k_ptr->interp_entity_table.entity_list[i]);
			//accessing the interpolator, num, and denom poly.
			interpolation_data_t * id = &(k_ptr->interpolation_data_list[i]);
			if (id->status != SUCCESSFUL_INTERP)
				continue;
#if VERBOSE
			printf ("[kernel:%d]..."
					"[generating header for param %d (%s)]\n",
					k, i, et->param_name);
#endif
			mcwp_error = generate_header_for_param(k_ptr->kernel_name,
					et->param_name, dci->device_name, id->num_poly,
					id->denom_poly);
			check_mcwp_error(mcwp_error, "generate_header_for_param");
#if VERBOSE
			print_short_dashed_line ();
#endif
		}
		//// generate header for static params
#if ALLOW_OMP_LOOPS
#pragma omp parallel for
#endif
		for (int i = 0; i < k_ptr->interp_entity_table_for_static_params.size;
				i++) {
			interpolation_entity_t * et_static =
					&(k_ptr->interp_entity_table_for_static_params.entity_list[i]);
			//accessing the interpolator, num, and denom poly.
			interpolation_data_t * id =
					&(k_ptr->interpolation_data_list_for_static_params[i]);
			if (id->status != SUCCESSFUL_INTERP)
				continue;
#if VERBOSE
			printf ("[kernel:%d]..."
					"[generating header for static param %d (%s)]\n",
					k, i, et_static->param_name);
#endif
			mcwp_error = generate_header_for_param(k_ptr->kernel_name,
					et_static->param_name, dci->device_name, id->num_poly,
					id->denom_poly);
			check_mcwp_error(mcwp_error, "generate_header_for_param");
//	  print_short_dashed_line ();
		}
	}
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int str_replace(char **str_out, const char *old_str,
		const char * replacement_str) {
	while (1) {
		char * str = *str_out;
		size_t replacement_str_len = strlen(replacement_str);
		size_t old_str_len = strlen(old_str);
		size_t str_len = strlen(str);

		char* suffix_str = strstr(str, old_str);
		if (suffix_str == NULL)
			break;

		size_t suffix_str_len = strlen(suffix_str);
//      printf ("suffix-str=%s\n", suffix_str);

		size_t prefix_str_len = str_len - suffix_str_len;
		suffix_str += old_str_len;
		suffix_str_len = strlen(suffix_str);

		size_t new_str_len = prefix_str_len + replacement_str_len
				+ suffix_str_len + 1;
		char * new_str = (char*) malloc(new_str_len);
		for (int i = 0; i < new_str_len; i++)
			new_str[i] = '\0';
		strncpy(new_str, str, prefix_str_len);
		strcat(new_str, replacement_str);
		strcat(new_str, suffix_str);
		free(*str_out);
		*str_out = new_str;
	}
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int generate_kernel_info_header(data_collection_info_t * dci,
		poly_params* kernel_poly_params) {
	char klaraptor_path[PATH_MAX];
	strcpy(klaraptor_path, getenv("KLARAPTOR_PATH"));

	char template_path[PATH_MAX];
	sprintf(template_path,
			"%s/src/gpu_perf_model/mcwp/kernel_info_header_template.h",
			klaraptor_path);

//  printf ("%s", template_path);

	FILE* file = fopen(template_path, "r");
	fseek(file, 0, SEEK_END);
	size_t file_size = ftell(file);
	rewind(file);
	char * buffer = (char*) malloc(file_size + 1);
	for (int i = 0; i < file_size + 1; i++)
		buffer[i] = '\0';
	fread(buffer, 1, file_size, file);
	fclose(file);

//  printf ("buffer=%s\n", buffer);

	/**
	 * the following patterns should be replaced:
	 *  KERNEL_VISITED_POINTS_MAP_INIT_
	 *  N_KERNEL_VARS_
	 *  STARTING_POINT_PTR_
	 *  N_POINTS_IN_MESH_
	 *  KERNEL_NAME_
	 *  DEVICE_NAME_
	 *  DEVICE_COMPUTE_CAPABILITY_
	 */

	int mcwp_error;

	int cc = mpq_get_ui(
			dci->device_params->mpq_values[DEVICE_PARAM_Compute_Capability]);
	char cc_str[32];
	sprintf(cc_str, "%d", cc);
//  printf("cc_str=%s\n", cc_str);
//  exit(EXIT_FAILURE);

	char n_kernel_vars_str[32];
	sprintf(n_kernel_vars_str, "%d", kernel_poly_params->nvar);

	char eval_point_set_str[256];
	kernel_type_enum_t kernel_type = dci->profiling_info->kernel_type;

	switch (kernel_type) {
	case OneDimKernel:
		sprintf(eval_point_set_str, "(\&global_bx)");
		break;

	case TwoDimKernel:
		sprintf(eval_point_set_str, "(\&global_bx_by)");
		break;

	case ThreeDimKernel:
		printf("[ERROR: THREE DIM PROBLEMS ARE NOT SUPPORTED!\n");
		return EXIT_FAILURE;
		//// TODO: add case of three dimensional kernels.
	}

	mcwp_error = str_replace(&buffer, "EVAL_POINT_SET_STR_",
			eval_point_set_str);

	mcwp_error = str_replace(&buffer, "N_KERNEL_VARS_", n_kernel_vars_str);

//  mcwp_error = str_replace (&buffer, "N_POINTS_IN_MESH_", n_points_in_mesh_str);

	mcwp_error = str_replace(&buffer, "KERNEL_NAME_",
			kernel_poly_params->kernel_name);

	mcwp_error = str_replace(&buffer, "DEVICE_NAME_", dci->device_name);

	mcwp_error = str_replace(&buffer, "DEVICE_COMPUTE_CAPABILITY_", cc_str);

	/////////////////////////////////
//  size_t n_char_per_line = 32;
//  size_t n_visited_points_str_size = 2 * kernel_poly_params->n_points_in_mesh
//      * n_char_per_line;
//  char * n_visited_points_str = (char*) malloc (n_visited_points_str_size);
//  for (int i = 0; i < n_visited_points_str_size; i++)
//    n_visited_points_str[i] = '\0';
//
//  const char * prefix_str = "int * x = k->visited_points_map;\n";
//
//  int current_size = 0;
//  current_size += sprintf (n_visited_points_str, "%s", prefix_str);
//  for (int i = 0; i < kernel_poly_params->n_points_in_mesh; i++)
//    {
//      current_size += sprintf (n_visited_points_str + current_size,
//			       "x[%d]=%d;\n", i,
//			       kernel_poly_params->visitd_points_map[i]);
//    }
//
//  mcwp_error = str_replace (&buffer, "KERNEL_VISITED_POINTS_MAP_INIT_",
//			    n_visited_points_str);
//  free (n_visited_points_str);
	//////////////////////////////
//  printf ("buffer=%s\n", buffer);

	char output_path[PATH_MAX];
	sprintf(output_path, "kernel_%s_kernel_info_%s.h",
			kernel_poly_params->kernel_name, dci->device_name);
	size_t output_size = strlen(buffer);
	FILE*output_file = fopen(output_path, "w");
	fwrite(buffer, 1, output_size, output_file);
	fclose(output_file);

	free(buffer);

	return EXIT_SUCCESS;
}

///////////////////////////////////////

//generate a header that will keep the basic information about the kernel.
int kernel_poly_params_generate_kernel_info_header(
		data_collection_info_t * dci) {
	poly_params * kernel_poly_params = dci->kernel_poly_params;
	int n_kernels = dci->n_kernels;

	//for each kernel, interpolate all params in its entity table.
	for (int k = 0; k < n_kernels; k++) {
		poly_params * k_ptr = &kernel_poly_params[k];
		generate_kernel_info_header(dci, k_ptr);
	}
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int build_rp_headers(data_collection_info_t * dci) {
//printf (short_dashed_line);
	printf("[INTERPOLATING MCWP PARAMETERS ...]\n");
//  print_short_dashed_line ();
	///////////////////////////////////////
	int mcwp_error = 0;

	mcwp_error = kernel_poly_params_interpolation_entity_table_init(dci);
	check_mcwp_error(mcwp_error,
			"kernel_poly_params_interpolation_entity_table_init");

	mcwp_error = kernel_poly_params_interpolation_data_add_points(dci);
	check_mcwp_error(mcwp_error,
			"kernel_poly_params_interpolation_data_add_points");

	mcwp_error = kernel_poly_params_set_static_params_to_const_poly(dci);
	check_mcwp_error(mcwp_error,
			"kernel_poly_params_set_static_params_to_const_poly");

	mcwp_error = kernel_poly_params_interpolate_performance_params(dci);
	check_mcwp_error(mcwp_error,
			"kernel_poly_params_interpolate_performance_params");

	mcwp_error = kernel_poly_params_generate_rp_headers(dci);
	check_mcwp_error(mcwp_error, "kernel_poly_params_generate_rp_headers");

	mcwp_error = kernel_poly_params_generate_kernel_info_header(dci);
	check_mcwp_error(mcwp_error,
			"kernel_poly_params_generate_kernel_info_headers");

//  //clear interpolation entity tables for each kernel.
	mcwp_error = kernel_poly_params_interpolation_entity_table_clear(dci);
	check_mcwp_error(mcwp_error,
			"kernel_poly_params_interpolation_entity_table_clear");

	return EXIT_SUCCESS;

}

///////////////////////////////////////

int get_nvar_for_kernel_type(int *nvar_out,
		const kernel_type_enum_t kernel_type) {
	int nvar = 0;
	int valid_result = 0;
	switch (kernel_type) {
	case OneDimKernel:
		nvar = 3;
		valid_result = 1;
		break;
	case TwoDimKernel:
		nvar = 3;
		valid_result = 1;
		break;
	case ThreeDimKernel:
		nvar = -1;
		valid_result = 0;
		break;
	}
	if (valid_result) {
		*nvar_out = nvar;
		return EXIT_SUCCESS;
	} else {
		*nvar_out = -1;
		return EXIT_FAILURE;
	}
}

///////////////////////////////////////

int get_degree_bounds_for_kernel_type(int *num_degree_bound_out,
		int *denom_degree_bound_out, const kernel_type_enum_t kernel_type) {
	int num_degree_bound = 0;
	int denom_degree_bound = 0;
	int valid_result = 0;
	switch (kernel_type) {
	case OneDimKernel:
		num_degree_bound = DEGREE_BOUND;
		denom_degree_bound = DEGREE_BOUND;
		valid_result = 1;
		break;
	case TwoDimKernel:
		num_degree_bound = DEGREE_BOUND;
		denom_degree_bound = DEGREE_BOUND;
		valid_result = 1;
		break;
	case ThreeDimKernel:
		num_degree_bound = -1;
		denom_degree_bound = -1;
		valid_result = 0;
		break;
	}
	if (valid_result) {
		*num_degree_bound_out = num_degree_bound;
		*denom_degree_bound_out = denom_degree_bound;
		return EXIT_SUCCESS;
	} else {
		*num_degree_bound_out = -1;
		*denom_degree_bound_out = -1;
		return EXIT_FAILURE;
	}
}

///////////////////////////////////////

int init_degree_bounds_for_each_var(int **degree_bounds_out,
		int ** denomdegree_bounds_out, int nvars,
		kernel_type_enum_t kernel_type) {
	int num_degree_bound = DEGREE_BOUND;
	int denom_degree_bound = DEGREE_BOUND;

	int mcwp_error = get_degree_bounds_for_kernel_type(&num_degree_bound,
			&denom_degree_bound, kernel_type);
	check_mcwp_error(mcwp_error, "get_degree_bounds_for_kernel_type");

	int *degree_bounds = (int*) malloc(sizeof(int) * nvars);
	int *denomdegree_bounds = (int*) malloc(sizeof(int) * nvars);
	for (int i = 0; i < nvars; i++) {
		degree_bounds[i] = num_degree_bound;
		denomdegree_bounds[i] = denom_degree_bound;
	}

	*degree_bounds_out = degree_bounds;
	*denomdegree_bounds_out = denomdegree_bounds;
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int clear_degree_bounds_for_each_var(int *degree_bounds,
		int *denomdegree_bounds) {
	free(degree_bounds);
	free(denomdegree_bounds);
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int data_collection_info_init(data_collection_info_t **dci_out,
		const char* mem_inst_trace_str, const kernel_type_enum_t kernel_type,
		const int n_lower_bound, const int n_upper_bound,
		const char* device_params_path, const char * kernel_name_list_path) {
	data_collection_info_t * dci = (data_collection_info_t*) malloc(
			sizeof(data_collection_info_t));
	*dci_out = dci;
	/////////////////////////////////////
	dci->device_params_path = device_params_path;
	dci->mem_inst_trace_str = mem_inst_trace_str;
	dci->n_params_to_interpolate = MAX_N_PARAMS_TO_INTERPOLATE;
	int mcwp_error;
	/////////////////////////////////////
	/**
	 * read device params: read once for all kernels and all the points.
	 * allocate space for device params; then, call the init function.
	 */
	mcwp_error = device_params_init(&dci->device_params, device_params_path);
	check_mcwp_error(mcwp_error, "device_params_init");

	/////////////////////////////////////
	//// also, set number of kernels here.
	mcwp_error = init_kernel_names_from_file(&(dci->kernel_names),
			&(dci->n_kernels), kernel_name_list_path);
	check_mcwp_error(mcwp_error, "init_kernel_names_from_file");

	/////////////////////////////////////
	mcwp_error = profiling_info_init(&(dci->profiling_info), kernel_type,
			n_lower_bound, n_upper_bound, dci->n_kernels);
	check_mcwp_error(mcwp_error, "profiling_info_init");

	/////////////////////////////////////
	//interpolating for three variables (n, b0, b1)
	//setting degree bounds to 3, or 5, or 7
	//TODO: does nvar depend on the kernel type?
	//what it should be for 1D, 2D, 3D kernels?
	mcwp_error = get_nvar_for_kernel_type(&(dci->nvar), kernel_type);
	check_mcwp_error(mcwp_error, "get_nvar_for_kernel_type");

//  /////////////////////////////////////
//  int *degree_bounds, *denomdegree_bounds;
//  mcwp_error = init_degree_bounds_for_each_var (&degree_bounds,
//						&denomdegree_bounds, nvar,
//						kernel_type);
//  check_mcwp_error(mcwp_error, "set_degree_bounds_for_each_var");

/////////////////////////////////////
	mcwp_error = kernel_poly_params_init(&(dci->kernel_poly_params),
			dci->kernel_names, dci->nvar, dci->n_params_to_interpolate,
			dci->n_kernels, dci->profiling_info);
	check_mcwp_error(mcwp_error, "kernel_poly_params_init");

	/////////////////////////////////////
	set_device_name(dci->device_name, dci->device_params_path);
	/////////////////////////////////////

//  dci->device_params_path = device_params_path;
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int data_collection_info_clear(data_collection_info_t *dci) {
	/////////////////////////////////////
	int mcwp_error;
	/////////////////////////////////////
	/**
	 * read device params: read once for all kernels and all the points.
	 * allocate space for device params; then, call the init function.
	 */
	mcwp_error = device_params_clear(dci->device_params);
	check_mcwp_error(mcwp_error, "device_params_clear");

	/////////////////////////////////////
	mcwp_error = clear_kernel_names(dci->kernel_names, dci->n_kernels);
	check_mcwp_error(mcwp_error, "clear_kernel_names");

	/////////////////////////////////////
	mcwp_error = profiling_info_clear(dci->profiling_info);
	check_mcwp_error(mcwp_error, "profiling_info_clear");

	/////////////////////////////////////
	mcwp_error = kernel_poly_params_clear(dci->kernel_poly_params,
			dci->n_kernels);
	check_mcwp_error(mcwp_error, "kernel_poly_params_clear");

	free(dci);
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int build_dp_headers(int argc, char **argv, char* mem_inst_trace_str,
		kernel_type_enum_t kernel_type, const char * kernel_name_list_path) {
	if (argc == 1) {
		print_short_dashed_line();
		printf(""
				"args: \n"
				" [1]:device_profile\n"
				" [2]:mode {0:emulate (only option)}\n" //, 1:load from file}\n"
				" [3]:low_n [4]:upper_n\n");
		print_short_dashed_line();
		return (EXIT_FAILURE);
	}

	/////////////////////////////////////
	//path of the file including device parameters
	char device_params_path[1024] = "";

	/////////////////////////////////////
	// setting the default n_lower_bound and n_upper_bound
	int n_lower_bound, n_upper_bound;
	switch (kernel_type) {
	case OneDimKernel:
		n_lower_bound = (1 << 14);  //16   * 1024
		n_upper_bound = (1 << 20);  //1024 * 1024
		break;

	case TwoDimKernel:
		n_lower_bound = 64;    // can be changed to 128
		n_upper_bound = 512;
		break;
	}

	/////////////////////////////////////

	if (argc > 1)
		strcpy(device_params_path, argv[1]);

	if (argc > 3)
		n_lower_bound = atoi(argv[3]);

	if (argc > 4)
		n_upper_bound = atoi(argv[4]);

	/////////////////////////////////////
	int mcwp_error;

	/////////////////////////////////////
	data_collection_info_t *dci;
	mcwp_error = data_collection_info_init(&dci, mem_inst_trace_str,
			kernel_type, n_lower_bound, n_upper_bound, device_params_path,
			kernel_name_list_path);
	check_mcwp_error(mcwp_error, "data_collection_info_init");

	/////////////////////////////////////
	mcwp_error = collect_data(dci);
	check_mcwp_error(mcwp_error, "collect_data");

	/////////////////////////////////////
	mcwp_error = build_rp_headers(dci);
	check_mcwp_error(mcwp_error, "build_rp_headers");
	/////////////////////////////////////
	data_collection_info_clear(dci);
	return EXIT_SUCCESS;
}

/////////////////////////////////////

#endif
