/*!
 \file interpolation_template.c
 \author Davood Mohajerani <dmohajer@uwo.ca>
 \author Linxiao Wang <lwang739@uwo.ca>
 \brief
 */

#include "mcwp/mwp_cwp.h"

///////////////////////////////////////

int build_dp_header_for_kernels(int argc, char **argv) {

//  int n_kernels = get_num_lines_in_file ("kernel_name_list.tmp");
	const char * kernel_name_list_path = "kernel_name_list.tmp";

	kernel_type_enum_t kernel_type_tmp = PROBLEM_DIM_REPLACEMENT_LINE;
	int nvar = PROBLEM_NVARS_REPLACEMENT_LINE;

//  int *degree_bounds = (int*) malloc (sizeof(int) * nvar);
//  int *denomdegree_bounds = (int*) malloc (sizeof(int) * nvar);
//
//  for (int i = 0; i < nvar; ++i)
//    {
//      degree_bounds[i] = 5;
//      denomdegree_bounds[i] = 5;
//    }
	char mem_inst_trace_tmp[128] = "%d %d %d 1";
	//int rep_n_pow = 2;
//  test_mwp_cwp (argc, argv, n_kernels, nvar, degree_bounds, denomdegree_bounds,
//		mem_inst_trace_tmp, kernel_type_tmp, rep_n);
	int stat = build_dp_headers(argc, argv, mem_inst_trace_tmp, kernel_type_tmp,
			kernel_name_list_path);
	return stat;

}

///////////////////////////////////////

int main(int argc, char ** argv) {
	return build_dp_header_for_kernels(argc, argv);
}

///////////////////////////////////////
