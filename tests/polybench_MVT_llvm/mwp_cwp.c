#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gmp.h>

//#include "mwp_cwp.h"
//#include "../../mcwp/mwp_cwp_all.h"

#include "../../mcwp/mwp_cwp.h"

///////////////////////////////////////
int rep_n(int n){
	//return pow(n,2);
	return 1;
}

int test_mcwp(int argc, char **argv) {

	int n_kernels = get_num_lines_in_file("kernel_name_list.tmp");

	enum kernel_type_enum kernel_type_tmp = TwoDimKernel;

	int nvar = 3;
	int *degree_bounds = (int*) malloc(sizeof(int) * nvar);
	int *denomdegree_bounds = (int*) malloc(sizeof(int) * nvar);

	for (int i = 0; i < nvar; ++i) {
		degree_bounds[i] = 5;
//		degree_bounds[1] = 7;
//		degree_bounds[2] = 7;
//		denomdegree_bounds[0] = 0;
//		denomdegree_bounds[1] = 7;
		denomdegree_bounds[i] = 5;
	}
	char mem_inst_trace_tmp[128] = "%d %d %d 1";
	//int rep_n_pow = 2;
	test_mwp_cwp(argc, argv, n_kernels, nvar, degree_bounds, denomdegree_bounds,
			mem_inst_trace_tmp, kernel_type_tmp, rep_n);
	return 0;
}

///////////////////////////////////////
int main(int argc, char ** argv) {
	return test_mcwp(argc, argv);
}


//#include <stdio.h>
//#include <stdlib.h>
//#include <string.h>
//#include <gmp.h>
//
//#include "../../mcwp/mcwp.h"
//
/////////////////////////////////////////
//
//int test_mwp_cwp(int argc, char **argv) {
//	//path of the file including device parameters
//	char device_params_path[1024] = "../../device_profiles/device_default.specs";
//
//	if (argc > 1)
//		sprintf(device_params_path, "%s", argv[1]);
//
//	printf("\ndevice_params_path=[%s]\n", device_params_path);
//
//	int n_kernels = get_num_lines_in_file("kernel_name_list.tmp");
//
//#if VERBOSE 
//	printf("[number of kernels: %d]\n", n_kernels);
//#endif
//
//	FILE *kernel_name_list = fopen("kernel_name_list.tmp", "r");
//	if (kernel_name_list == NULL) {
//		printf("ERROR: opening kernel_name_list.tmp!\n");
//		exit (EXIT_FAILURE);
//	}
//
//	char** kernel_names = (char **) malloc(n_kernels * sizeof(char *));
//	for (int i = 0; i < n_kernels; i++) {
//		kernel_names[i] = (char *) malloc(LEN_KERNEL_NAME * sizeof(char));
//		fgets(kernel_names[i], LEN_KERNEL_NAME, kernel_name_list);
//		kernel_names[i][strcspn(kernel_names[i], "\n")] = 0;
////		printf("kernel: [%s]\n", kernel_names[i]);
//	}
//	fclose(kernel_name_list);
//
//	const char prefix_kernel[] = "kernel_";
//	const char suffix_params[] = "_params.tmp";
//	const char suffix_trace[] = "_trace.txt";
//	const char suffix_trace_avg[] = "_trace.avg.txt";
//	const char suffix_results[] = "_results.txt";
//	const char suffix_occupancy[] = "_occupancy.tmp";
//	const char suffix_interpolation[] = "_interpolation.tmp";
//
//	char** param_file_path_list = (char **) malloc(n_kernels * sizeof(char *));
//	char** trace_file_path_list = (char **) malloc(n_kernels * sizeof(char *));
//	char** avg_file_path_list = (char **) malloc(n_kernels * sizeof(char *));
//	char** result_file_path_list = (char **) malloc(n_kernels * sizeof(char *));
//	char** occupancy_file_path_list = (char **) malloc(
//			n_kernels * sizeof(char *));
//	char** interpolation_file_path_list = (char **) malloc(
//			n_kernels * sizeof(char *));
//	char mem_inst_trace[128] = "./mem_inst_trace.bin";
//
//	init_output_path_list(interpolation_file_path_list, kernel_names,
//			prefix_kernel, suffix_interpolation, n_kernels);
//
//	init_output_path_list(param_file_path_list, kernel_names, prefix_kernel,
//			suffix_params, n_kernels);
//
//	init_output_path_list(trace_file_path_list, kernel_names, prefix_kernel,
//			suffix_trace, n_kernels);
//
//	init_output_path_list(avg_file_path_list, kernel_names, prefix_kernel,
//			suffix_trace_avg, n_kernels);
//
//	init_output_path_list(result_file_path_list, kernel_names, prefix_kernel,
//			suffix_results, n_kernels);
//
//	init_output_path_list(occupancy_file_path_list, kernel_names, prefix_kernel,
//			suffix_occupancy, n_kernels);
//
//	//int degree = 3;
//	int shared_mem_bytes = 0;
//
//	// double *mwp = (double *) malloc(n_kernels * degree * sizeof(double));
//	// double *cwp = (double *) malloc(n_kernels * degree * sizeof(double));
//	// double *clockcycles = (double *) malloc(
//	// 		n_kernels * degree * sizeof(double));
//	mpq_t mwp, cwp, clockcycles, rep, active_blocks_per_sm;
//	mpq_inits(mwp, cwp, clockcycles, rep, active_blocks_per_sm, NULL);
//
//	int program_params[N_PROGRAM_PARAMS], device_params[N_DEVICE_PARAMS];
//	memset(program_params, 0x00, N_PROGRAM_PARAMS * sizeof(int));
//	memset(device_params, 0x00, N_DEVICE_PARAMS * sizeof(int));
//
//	//reading device parameters from file
//	read_params_from_file(device_params_path, device_params, N_DEVICE_PARAMS,
//			global_device_params_list);
//
//	//(gridDimX, gridDimY, gridDimZ, blockDimX, blockDimY, blockDimZ, shared_mem_bytes
//	program_params_t default_program_params = { 1, 1, 1, 1, 1, 1, 0 };
//
//	kernel_params_t kernel_params;
//	enum kernel_type_enum kernel_type = TwoDimKernel;
//	poly_params kernel_poly_params;
//	char* syms[] = { "B0", "B1" };
//	kernel_poly_params.nvar = 2;
//	kernel_poly_params.degree_bounds = (int*) malloc(
//			sizeof(int) * kernel_poly_params.nvar);
//	kernel_poly_params.denomdegree_bounds = (int*) malloc(
//			sizeof(int) * kernel_poly_params.nvar);
//
//	for (int i = 0; i < kernel_poly_params.nvar; ++i) {
//		kernel_poly_params.degree_bounds[i] = 5;
//		kernel_poly_params.denomdegree_bounds[i] = 5;
//	}
////	for (int j = 0; j < 3; j++)
////	{
////		kernel_poly_params.interp[j] = rfInterpInit(kernel_poly_params.nvar,
////				kernel_poly_params.degree_bounds,
////				kernel_poly_params.denomdegree_bounds);
////		kernel_poly_params.poly[j] = NULL;
////		kernel_poly_params.denompoly[j] = NULL;
////		kernel_poly_params.status[j] = INIT;
////	}
//
//	switch (kernel_type) {
//	case OneDimKernel:
//		kernel_poly_params.point = (&global_p1k1_list);
//		break;
//
//	case TwoDimKernel:
//		kernel_poly_params.point = (&global_p2k2_list);
//		break;
//	}
//
//#if VERBOSE
//	printf("Init interpolation done.\n");
//#endif
//
//	//AltArr_t* poly = NULL;
//	PolyInterpStatus_t stat;
//	//= MORE_POINTS;
//	//= interpGetPoly(interp, &poly);
//	mpq_t* mpPoint = (mpq_t*) malloc(3 * sizeof(mpq_t));
//	mpq_t Val;
//	mpq_init(Val);
//	for (int i = 0; i < 3; ++i) {
//		mpq_init(mpPoint[i]);
//	}
//	int success = 0;
//	//stat == POLY_INTERP_MORE_POINTS
//	int thread_per_block = 0;
//	FILE** mcwp_result = (FILE**) malloc(n_kernels * sizeof(FILE*));
//	FILE** interpolation_result = (FILE**) malloc(n_kernels * sizeof(FILE*));
//	for (int k = 0; k < n_kernels; k++) {
//		mcwp_result[k] = fopen(result_file_path_list[k], "w");
//		setbuf(mcwp_result[k], NULL);
//		if (mcwp_result[k] == NULL) {
//			printf("ERROR: opening [%s]!\n", result_file_path_list[k]);
//			exit (EXIT_FAILURE);
//		}
//		interpolation_result[k] = fopen(interpolation_file_path_list[k], "w");
//		setbuf(interpolation_result[k], NULL);
//		if (interpolation_result[k] == NULL) {
//			printf("ERROR: opening [%s]!\n", interpolation_file_path_list[k]);
//			exit (EXIT_FAILURE);
//		}
//	}
//	mpf_t mpf_temp;
//	mpf_init(mpf_temp);
//	int* best_n = (int*) malloc(n_kernels * sizeof(int));
//	int* best_b0 = (int*) malloc(n_kernels * sizeof(int));
//	int* best_b1 = (int*) malloc(n_kernels * sizeof(int));
//	mpf_t* best_clock = (mpf_t*) malloc(n_kernels * sizeof(mpf_t));
//
//	for (int i = 0; i < n_kernels; i++) {
//		best_n[i] = 0;
//		best_b0[i] = 0;
//		best_b1[i] = 0;
//		mpf_init(best_clock[i]);
//		mpf_set_si(best_clock[i], 0);
//	}
//	remove_trace_log_file();
//	
//	
////	while (success < 2 * n_kernels && kernel_poly_params.point->N[0] != 0)
//	while (kernel_poly_params.point != NULL) {
//
//		for (int j = 0; j < 3; j++) {
//			kernel_poly_params.interp[j] = rfInterpInit(kernel_poly_params.nvar,
//					kernel_poly_params.degree_bounds,
//					kernel_poly_params.denomdegree_bounds);
//			kernel_poly_params.poly[j] = NULL;
//			kernel_poly_params.denompoly[j] = NULL;
//			kernel_poly_params.status[j] = INIT;
//		}
//
//#if VERBOSE
//		printf("Init interpolation done.\n");
//#endif
//
//		int success = 0;
//		int current_n = kernel_poly_params.point->N[0];
//
//		while (success < 3 * n_kernels && kernel_poly_params.point != NULL
//				&& kernel_poly_params.point->N[0] == current_n) {
//
//			int n, b0, b1;
//			n = kernel_poly_params.point->N[0];
//			b0 = kernel_poly_params.point->N[1];
//			b1 = kernel_poly_params.point->N[2];
//			int s = 1;
//#if VERBOSE
//			printf("=================================\n");
//			printf("checking [N=%d, B0=%d, B1=%d]\n", n, b0, b1);
//#endif
////		thread_per_block = b0 * b1;
////		shared_mem_bytes = thread_per_block * sizeof(int);
//
////			default_program_params.grid_dim_x = kernel_poly[j].point->N[0]
////					/ kernel_poly[j].point->N[1];
////			default_program_params.grid_dim_y = kernel_poly[j].point->N[0]
////					/ kernel_poly[j].point->N[2];
////			default_program_params.grid_dim_z = 1;
////			default_program_params.block_dim_x = kernel_poly[j].point->N[1];
////			default_program_params.block_dim_y = kernel_poly[j].point->N[2];
////			default_program_params.block_dim_z = 1;
////			default_program_params.shared_mem_bytes = shared_mem_bytes;
//
////		default_program_params.grid_dim_x = ceiling_div(n, b0);
////		default_program_params.grid_dim_y = ceiling_div(n, b1);
////		default_program_params.grid_dim_z = 1;
////
////		default_program_params.block_dim_x = b0;
////		default_program_params.block_dim_y = b1;
////		default_program_params.block_dim_z = 1;
////		default_program_params.shared_mem_bytes = shared_mem_bytes;
//
////		for (int j = 0; j < n_kernels; j++)
////		{
////			write_program_params_to_file(param_file_path_list[j],
////					&default_program_params);
////#if VERBOSE >=2
////			printf("writing default program params for [%s] -> done!\n",
////					param_file_path_list[j]);
////#endif
////		}
//
//			//calling ocelot tracer to generate stats for each kernel
//			//enumerated in kernel_name_list.tmp in the current directory.
//
//			//		sprintf(mem_inst_trace,
//			//				"(./mem_inst_trace.bin %d %d %d 2>/dev/null && echo 0 || echo -1)",
//			//				kernel_poly[0].point->N[0], kernel_poly[0].point->N[1],
//			//				kernel_poly[0].point->N[2]);
//
//			sprintf(mem_inst_trace, "%d %d %d %d", n, b0,
//					b1, s);
////		int status = system(mem_inst_trace);
//			int status = exec_cmd(mem_inst_trace);
//			if (status == 1) {
//				printf(
//						"ERROR: FAILED calling [mem_inst_trace.bin] (return value >1) \n");
//				printf("Continue!\n");
//				printf("=================================\n");
//				//			kernel_poly.point++;
//				kernel_poly_params.point = get_next_kernel_mesh_point(
//						kernel_poly_params.point);
//				continue;
//			}
//			if (status > 2) {
//				printf("ERROR: calling mem_inst_trace.bin -> FAILED!\n");
//				exit (EXIT_FAILURE);
//			}
//#if VERBOSE >=2
//			printf("calling mem_inst_trace.bin -> done!\n");
//#endif
//
//			//1. read trace and trace_avg results + remove the files 
//			//2. compute mcwp
//			//3. 
//			for (int j = 0; j < n_kernels; j++) {
//				if (read_params_from_file(avg_file_path_list[j], program_params,
//						N_PROGRAM_PARAMS, global_program_params_list)
//						!= EXIT_SUCCESS) {
//					printf("kernel has not been traced; continue\n");
//					//					kernel_poly_params.point = get_next_kernel_mesh_point(
//					//							kernel_poly_params.point);
//					kernel_poly_params.point = get_next_kernel_mesh_point(
//							kernel_poly_params.point);
//					continue;
//				}
//
//				read_kernel_params_from_file(&kernel_params,
//						param_file_path_list[j]);
//
//				int n_blocks = kernel_params.blocks_per_grid;
//				int threads_per_block = kernel_params.threads_per_block;
//#if VERBOSE >=2
//				printf("removing trace file [%s] -> done!\n", trace_file_path_list[j]);
//#endif
//				remove(trace_file_path_list[j]);
//				remove(avg_file_path_list[j]);
//				int mcwp_case = 0;
////			mwp_cwp(&mcwp_case, mwp, cwp, clockcycles, program_params,
////					device_params, thread_per_block, n * n / thread_per_block);
//				mwp_cwp(&mcwp_case, mwp, cwp, clockcycles, rep,
//						active_blocks_per_sm, program_params, device_params,
//						threads_per_block, n_blocks, pow(n, 2));
//
////			mpq_set_si(mpPoint[0], n, 1);
//				mpq_set_si(mpPoint[0], b0, 1);
//				mpq_set_si(mpPoint[1], b1, 1);
//				mpq_set(Val, clockcycles);
//
//#if VERBOSE >=2
//				printf(" Before checking, mcwp case:%d. Status: %d\n",mcwp_case,kernel_poly_params.status[mcwp_case -1]);
//#endif		
//
//				int k = mcwp_case - 1;
//
////				if (kernel_poly_params.status[k] == UNCHECKED) {
////#if VERBOSE
////					gmp_printf("Check the following poly on point(%Qd,%Qd,%Qd): \n",mpPoint[0],mpPoint[1],Val);
////					printPoly_AA(stderr, kernel_poly_params.poly[k], syms, kernel_poly_params.nvar);
////					printPoly_AA(stderr, kernel_poly_params.denompoly[k], syms, kernel_poly_params.nvar);
////#endif
////					//	stat = rfInterpCheckPoly(kernel_poly_params.interp[k],
////					//		kernel_poly_params.poly[k],
////					//	kernel_poly_params.denompoly[k], mpPoint, Val);
////					stat = rfInterpCheckPolyWithTolerance(
////							kernel_poly_params.interp[k],
////							kernel_poly_params.poly[k],
////							kernel_poly_params.denompoly[k], mpPoint, Val,
////							0.01);
////
////					if (stat == POLY_INTERP_MORE_POINTS) {
////						//	rfInterpAddPointValMP(kernel_poly_params.interp[k], mpPoint,
////						//		Val);
////						kernel_poly_params.status[k] = INIT;
////					} else {
////						kernel_poly_params.status[k] = DONE;
////						fprintf(stderr, "For N = %d: \n", current_n);
////						fprintf(interpolation_result[j], "For N = %d: \n",
////								current_n);
////						print_interpolation_result(kernel_names[j], mcwp_case,
////								kernel_poly_params, k, syms,
////								interpolation_result[j]);
////
////						success++;
////					}
////				}
////				if (kernel_poly_params.status[k] == INIT) {
//					rfInterpAddPointValMP(kernel_poly_params.interp[k], mpPoint,
//							Val);
////					stat = rfInterpGetPoly(kernel_poly_params.interp[k],
////							&kernel_poly_params.poly[k],
////							&kernel_poly_params.denompoly[k]);
////					if (stat == POLY_INTERP_SUCCESS) {
////						kernel_poly_params.status[k] = UNCHECKED;
////					} else if (stat == POLY_INTERP_FAILURE) {
////						printf(stderr, "ERROR: Failed to interpolate!\n");
////						//ToDo: shouldn't be EXIT_FAILURE?
////						exit (EXIT_SUCCESS);
////					}
////				}
//
//#if VERBOSE >=2
//				printf("After checking, mcwp case:%d. Status: %d\n", mcwp_case,kernel_poly_params.status[mcwp_case -1]);
//				printf("Add point -> done!\n");
//#endif 
//				mpf_set_q(mpf_temp, clockcycles);
//				float occupancy = read_float_from_file(
//						occupancy_file_path_list[j]);
//
//				print_mcwp_result(mcwp_result[j], n, b0, b1, mwp, cwp, clockcycles,
//						mcwp_case, occupancy, rep, active_blocks_per_sm);
//
//				if (mpf_cmp(best_clock[j], mpf_temp) > 0 || best_n[j] == 0) {
//					mpf_set(best_clock[j], mpf_temp);
//					best_n[j] = n;		//kernel_poly_params.point->N[0];
//					best_b0[j] = b0;		//kernel_poly_params.point->N[1];
//					best_b1[j] = b1;		//kernel_poly_params.point->N[1];
//
//				}
//
//#if VERBOSE  >=2
//				printf("Get best done!\n");
//#endif
//				kernel_poly_params.point = get_next_kernel_mesh_point(
//						kernel_poly_params.point);
//			}
//		}
//		for (int j = 0; j < n_kernels; j++) {
//			for (int k = 0; k < 3; k++) {
//				stat = rfInterpGetPoly(kernel_poly_params.interp[k],
//						&kernel_poly_params.poly[k],
//						&kernel_poly_params.denompoly[k], eps);
//				if (stat == POLY_INTERP_SUCCESS) {
//					fprintf(interpolation_result[j], "For N = %d:\n",
//							current_n);
//					fprintf(stderr, "For N = %d:\n", current_n);
//					print_interpolation_result(kernel_names[j], k + 1,
//							kernel_poly_params, k, syms,
//							interpolation_result[j]);
//				} else if (stat == POLY_INTERP_FAILURE) {
//
//					fprintf(stderr,
//							"ERROR: Failed to interpolate for case %d!\n\n",
//							k + 1);
//					fprintf(interpolation_result[j],
//							"ERROR: Failed to interpolate for case %d!\n\n",
//							k + 1);
//				}
//				interpFree(kernel_poly_params.interp[k]);
//				freePolynomial_AA(kernel_poly_params.poly[k]);
//				freePolynomial_AA(kernel_poly_params.denompoly[k]);
//			}
//			print_best_mcwp_result(mcwp_result[j], best_n[j], best_b0[j],
//					best_b1[j], best_clock[j], j);
//			best_n[j] = 0;
//			mpf_set_si(best_clock[j], 0);
//			best_b0[j] = 0;
//			best_b1[j] = 0;
//		}
//	}
//
//	for (int k = 0; k < n_kernels; k++) {
//		fclose(mcwp_result[k]);
//		fclose(interpolation_result[k]);
//	}
//
//	for (int i = 0; i < 3; ++i) {
//		mpq_clear(mpPoint[i]);
//	}
//	free(mpPoint);
//	return 0;
//}
//
/////////////////////////////////////////
//int main(int argc, char ** argv) {
//	return test_mwp_cwp(argc, argv);
//}
//
/////////////////////////////////////////
//
//// add comments
//// write functions
//// follow c99 standard, loop variales inside the body
//// take into account the cache friendliness
//// exit(EXIT_FAILURE);
//
////
///////////////////////////////////////////
////
////int test_mwp_cwp(int argc, char **argv)
////{
////	//path of the file including device parameters
////	char device_params_path[1024] = "../../device_profiles/device_default.specs";
////	// int data_size = 256 * 1024;
////
////	// if (argc > 1)
////	// {
////	// 	data_size = atoi(argv[1]);
////	// 	printf("Data size is %d\n",data_size);
////	// }
////	if (argc > 1)
////	{
////		sprintf(device_params_path, "%s", argv[1]);
////	}
////	printf("\ndevice_params_path=[%s]\n", device_params_path);
////	int n_kernels = 0;
////	n_kernels = get_num_lines_in_file("kernel_name_list.tmp");
////#if VERBOSE 
////	printf("[number of kernels: %d]\n", n_kernels);
////#endif
////
////	FILE *kernel_name_list = fopen("kernel_name_list.tmp", "r");
////	if (kernel_name_list == NULL)
////	{
////		printf("ERROR: opening kernel_name_list.tmp!\n");
////		exit(1);
////	}
////	char** kernel_names = (char **) malloc(n_kernels * sizeof(char *));
////	for (int i = 0; i < n_kernels; i++)
////	{
////		kernel_names[i] = (char *) malloc(LEN_KERNEL_NAME * sizeof(char));
////		fgets(kernel_names[i], LEN_KERNEL_NAME, kernel_name_list);
////		kernel_names[i][strcspn(kernel_names[i], "\n")] = 0;
////		//		printf("kernel: [%s]\n", kernel_names[i]);
////	}
////	fclose(kernel_name_list);
////
////	const char prefix_kernel[] = "kernel_";
////	const char suffix_params[] = "_params.tmp";
////	const char suffix_trace[] = "_trace.txt";
////	const char suffix_trace_avg[] = "_trace.avg.txt";
////	const char suffix_results[] = "_results.txt";
////	const char suffix_occupancy[] = "_occupancy.tmp";
////	const char suffix_interpolation[] = "_interpolation.tmp";
////
////	char** param_file_path_list = (char **) malloc(n_kernels * sizeof(char *));
////	char** trace_file_path_list = (char **) malloc(n_kernels * sizeof(char *));
////	char** avg_file_path_list = (char **) malloc(n_kernels * sizeof(char *));
////	char** result_file_path_list = (char **) malloc(n_kernels * sizeof(char *));
////	char** occupancy_file_path_list = (char **) malloc(
////			n_kernels * sizeof(char *));
////	char** interpolation_file_path_list = (char **) malloc(
////					n_kernels * sizeof(char *));
////	char mem_inst_trace[128] = "./mem_inst_trace.bin";
////	int str_size = 0;
////	
////	str_size = (strlen(prefix_kernel) + strlen(suffix_interpolation)) + 1;
////	for (int i = 0; i < n_kernels; i++)
////	{
////		interpolation_file_path_list[i] = (char *) malloc(
////				(str_size + strlen(kernel_names[i])) * sizeof(char));
////		strcpy(interpolation_file_path_list[i], prefix_kernel);
////		strcat(interpolation_file_path_list[i], kernel_names[i]);
////		strcat(interpolation_file_path_list[i], suffix_interpolation);
////#if VERBOSE
////		printf("[interpolation_file_path_list: %s]\n", interpolation_file_path_list[i]);
////#endif
////	}
////
////	str_size = (strlen(prefix_kernel) + strlen(suffix_params));
////	for (int i = 0; i < n_kernels; i++)
////	{
////		//		param_file_path_list[i] = (char *) malloc(
////		//				(strlen(prefix_kernel) + strlen(suffix_params)
////		//						+ strlen(kernel_names[i])) * sizeof(char));
////		param_file_path_list[i] = (char *) malloc(
////				(str_size + strlen(kernel_names[i])) * sizeof(char));
////		strcpy(param_file_path_list[i], prefix_kernel);
////		strcat(param_file_path_list[i], kernel_names[i]);
////		strcat(param_file_path_list[i], suffix_params);
////#if VERBOSE
////		printf("[param_file_path_list: %s]\n", param_file_path_list[i]);
////#endif
////	}
////
////	//setting up a list including path of trace files
////	str_size = strlen(prefix_kernel) + strlen(suffix_trace);
////	for (int i = 0; i < n_kernels; i++)
////	{
////		//		trace_file_path_list[i] = (char *) malloc(
////		//				(strlen(prefix_kernel) + strlen(suffix_trace)
////		//						+ strlen(kernel_names[i])) * sizeof(char));
////		trace_file_path_list[i] = (char *) malloc(
////				(str_size + strlen(kernel_names[i])) * sizeof(char));
////		strcpy(trace_file_path_list[i], prefix_kernel);
////		strcat(trace_file_path_list[i], kernel_names[i]);
////		strcat(trace_file_path_list[i], suffix_trace);
////#if VERBOSE
////		printf("[trace_file_path_list: %s]\n", trace_file_path_list[i]);
////#endif
////	}
////
////	//setting up a list including path of average statistics files generated by ocelot 
////	str_size = strlen(prefix_kernel) + strlen(suffix_trace_avg);
////	for (int i = 0; i < n_kernels; i++)
////	{
////		//		avg_file_path_list[i] = (char *) malloc(
////		//				(strlen(prefix_kernel) + strlen(suffix_trace_avg)
////		//						+ strlen(kernel_names[i])) * sizeof(char));
////		avg_file_path_list[i] = (char *) malloc(
////				(str_size + strlen(kernel_names[i])) * sizeof(char));
////		strcpy(avg_file_path_list[i], prefix_kernel);
////		strcat(avg_file_path_list[i], kernel_names[i]);
////		strcat(avg_file_path_list[i], suffix_trace_avg);
////
////#if VERBOSE
////		printf("[trace_avg_file_path_list: %s]\n", avg_file_path_list[i]);
////#endif
////	}
////
////	//setting up a list including path of result csv files for each kernel/mcwp.
////	str_size = strlen(prefix_kernel) + strlen(suffix_results);
////	for (int i = 0; i < n_kernels; i++)
////	{
////		//		result_file_path_list[i] = (char *) malloc(
////		//				(strlen(prefix_kernel) + strlen(suffix_results)
////		//						+ strlen(kernel_names[i])) * sizeof(char));
////		result_file_path_list[i] = (char *) malloc(
////				(str_size + strlen(kernel_names[i])) * sizeof(char));
////		strcpy(result_file_path_list[i], prefix_kernel);
////		strcat(result_file_path_list[i], kernel_names[i]);
////		strcat(result_file_path_list[i], suffix_results);
////#if VERBOSE
////		printf("[result_file_path_list: %s]\n", result_file_path_list[i]);
////#endif
////	}
////	str_size = strlen(prefix_kernel) + strlen(suffix_occupancy) + 1;
////	for (int i = 0; i < n_kernels; i++)
////	{
////		//		result_file_path_list[i] = (char *) malloc(
////		//				(strlen(prefix_kernel) + strlen(suffix_results)
////		//						+ strlen(kernel_names[i])) * sizeof(char));
////		occupancy_file_path_list[i] = (char *) malloc(
////				(str_size + strlen(kernel_names[i])) * sizeof(char));
////		strcpy(occupancy_file_path_list[i], prefix_kernel);
////		strcat(occupancy_file_path_list[i], kernel_names[i]);
////		strcat(occupancy_file_path_list[i], suffix_occupancy);
////#if VERBOSE
////		printf("[occupancy_file_path_list: %s]\n", occupancy_file_path_list[i]);
////#endif
////	}
////
////	//int degree = 3;
////
////	//	int thread_per_block_init = 32;
////	//int *thread_per_block = (int*) malloc(degree * sizeof(int));
////	//int point_index = 0;
////	//int *n_block = (int*) malloc(degree * sizeof(int));
////	int shared_mem_bytes = 0;
////
////	// double *mwp = (double *) malloc(n_kernels * degree * sizeof(double));
////	// double *cwp = (double *) malloc(n_kernels * degree * sizeof(double));
////	// double *clockcycles = (double *) malloc(
////	// 		n_kernels * degree * sizeof(double));
////	mpq_t mwp;
////	mpq_t cwp;
////	mpq_t clockcycles;
////	mpq_init(mwp);
////	mpq_init(cwp);
////	mpq_init(clockcycles);
////
////	int program_params[N_PROGRAM_PARAMS];
////	int device_params[N_DEVICE_PARAMS];
////	memset(program_params, 0x00, N_PROGRAM_PARAMS * sizeof(int));
////	memset(device_params, 0x00, N_DEVICE_PARAMS * sizeof(int));
////
////	//reading device parameters from file
////	read_params_from_file(device_params_path, device_params, N_DEVICE_PARAMS,
////			global_device_params_list);
////
////	// thread_per_block[0] = MIN_THREADS_PER_BLOCK;
////	// n_block[0] = data_size / thread_per_block[0];
////	// for (int i = 1; i < degree; i++)
////	// {
////	// 	thread_per_block[i] = thread_per_block[i - 1] * 2;
////	// 	n_block[i] = data_size / thread_per_block[i];
////	// }
////
////	program_params_t default_program_params;
////	default_program_params.grid_dim_x = 1;
////	default_program_params.grid_dim_y = 1;
////	default_program_params.grid_dim_z = 1;
////	default_program_params.block_dim_x = 1;
////	default_program_params.block_dim_y = 1;
////	default_program_params.block_dim_z = 1;
////	default_program_params.shared_mem_bytes = 0;
////
////	poly_params kernel_poly;
////	char* syms[] =
////	{ "N", "B0", "B1" };
////	kernel_poly.nvar = 3;
////	kernel_poly.degree_bounds = (int*) malloc(sizeof(int) * kernel_poly.nvar);
////	//for (int i = 0; i < kernel_poly.nvar; ++i)
////	//{
////		kernel_poly.degree_bounds[0] = 2;
////		kernel_poly.degree_bounds[1] = 1;
////		kernel_poly.degree_bounds[2] = 1;
////	//}
////	// degreeBounds[0] = 2;
////
////	for(int j = 0; j < 3; j++){
////		kernel_poly.interp[j] = interpInit(kernel_poly.nvar,
////				kernel_poly.degree_bounds);
////		kernel_poly.poly[j] = NULL;			
////		kernel_poly.status[j] = 0;
////	}
////	kernel_poly.point = (&global_p2k2_list);
////
////
////	//AltArr_t* poly = NULL;
////	PolyInterpStatus_t stat;
////	//= interpGetPoly(interp, &poly);
////	mpq_t* mpPoint = (mpq_t*) malloc(3 * sizeof(mpq_t));
////	mpq_t Val;
////	mpq_init(Val);
////	for (int i = 0; i < 3; ++i)
////	{
////		mpq_init(mpPoint[i]);
////	}
////	int success = 0;
////	//stat == POLY_INTERP_MORE_POINTS
////	int thread_per_block = 0;
////	FILE** mcwp_result = (FILE**) malloc(n_kernels * sizeof(FILE*));
////	FILE** interpolation_result = (FILE**) malloc(n_kernels * sizeof(FILE*));
////	for (int k = 0; k < n_kernels; k++)
////	{
////		mcwp_result[k] = fopen(result_file_path_list[k], "ab+");
////		setbuf(mcwp_result[k],NULL);
////		if (mcwp_result[k] == NULL)
////		{
////			printf("ERROR: opening [%s]!\n", result_file_path_list[k]);
////			exit (EXIT_FAILURE);
////		}
////		interpolation_result[k] = fopen(interpolation_file_path_list[k], "ab+");
////		setbuf(interpolation_result[k],NULL);
////		if (interpolation_result[k] == NULL)
////		{
////			printf("ERROR: opening [%s]!\n", interpolation_file_path_list[k]);
////			exit (EXIT_FAILURE);
////		}
////	}
////	mpf_t n_clockcycles;
////	mpf_init(n_clockcycles);
////	int* best_n = (int*) calloc(n_kernels, sizeof(int));
////	int* best_b0 = (int*) malloc(n_kernels * sizeof(int));
////	int* best_b1 = (int*) malloc(n_kernels * sizeof(int));
////	mpf_t* best_clock = (mpf_t*) malloc(n_kernels * sizeof(mpf_t));
////	for(int i = 0; i < n_kernels; i ++){
////		best_n[i] = 0;
////		best_b0[i] = 0;
////		best_b1[i] = 0;
////		mpf_init(best_clock[i]);
////		mpf_set_si(best_clock[i],0);
////	}
////	while (success < 1 * n_kernels && kernel_poly.point->N[0] != 0)
//////	for (int i = 0; i < degree; i++)
////	{
////#if VERBOSE
////		printf("[N: %d]\n", kernel_poly.point->N[0]);
////#endif
////		thread_per_block = kernel_poly.point->N[1] * (kernel_poly.point->N[2]);
////		shared_mem_bytes = 0;
////
////		int n = kernel_poly.point->N[0];
////		int B1 = kernel_poly.point->N[1]; //blockDimX
////		int B0 = kernel_poly.point->N[2]; //blockDimY
////		int s = 1; //granularity;
////
////		if (B1 % s)
////		{
////			printf("B1 mod s == %d mod %d !=0\n", B1, s);
////			kernel_poly.point++;
////			continue;
////		}
////		default_program_params.block_dim_x = B1 / s;
////		default_program_params.block_dim_y = B0;
////		default_program_params.block_dim_z = 1;
////
////		default_program_params.grid_dim_x = n / B1;
////		default_program_params.grid_dim_y = n / B0;
////		default_program_params.grid_dim_z = 1;
////
////		default_program_params.shared_mem_bytes = shared_mem_bytes;
////
////		for (int j = 0; j < n_kernels; j++)
////		{
////			write_program_params_to_file(param_file_path_list[j],
////					&default_program_params);
////#if VERBOSE
////			printf("writing default program params for [%s] -> done!\n",
////					param_file_path_list[j]);
////#endif
////		}
////
////		//calling ocelot tracer to generate stats for each kernel
////		//enumerated in kernel_name_list.tmp in the current directory.
////		//sprintf(mem_inst_trace,"./mem_inst_trace.bin %d %d %d",kernel_poly.point->N[0],kernel_poly.point->N[1],kernel_poly.point->N[2]);
////		sprintf(mem_inst_trace,
////				"((./mem_inst_trace.bin %d %d %d %d) && echo 0 || echo -1)", n,
////				B0, B1, s);
////		// the following line is for 1D jacobi1
////		//printf("cmd = %s\n", mem_inst_trace);
////		//	int status = system(mem_inst_trace);
////		int status = exec_cmd(mem_inst_trace);
////		if (status != 0)
////		{
////			printf("ERROR: calling mem_inst_trace.bin -> FAILED!\n");
////			exit (EXIT_FAILURE);
////		}
////#if VERBOSE
////		printf("calling mem_inst_trace.bin -> done!\n");
////#endif
////		for (int j = 0; j < n_kernels; j++)
////		{
////
////			read_params_from_file(avg_file_path_list[j], program_params,
////			N_PROGRAM_PARAMS, global_program_params_list);
////
////#if VERBOSE
////			printf("reading file [%s] -> done!\n", avg_file_path_list[j]);
////#endif
////
////			remove(trace_file_path_list[j]);
////			remove(avg_file_path_list[j]);
////
////#if VERBOSE
////			printf("removing trace file [%s] -> done!\n", trace_file_path_list[j]);
////#endif
////			int mcwp_case = 0;
////			mwp_cwp(&mcwp_case,mwp, cwp, clockcycles, program_params, device_params,
////					thread_per_block,
////					(kernel_poly.point->N[0] * kernel_poly.point->N[0])
////							/ thread_per_block);
////			// mwp[j * degree + i] = res[0];
////			// cwp[j * degree + i] = res[1];
////			// clockcycles[j * degree + i] = res[2];
////
////			mpq_set_si(mpPoint[0], kernel_poly.point->N[0], 1);
////			mpq_set_si(mpPoint[1], kernel_poly.point->N[1], 1);
////			mpq_set_si(mpPoint[2], kernel_poly.point->N[2], 1);
////			mpq_set(Val, clockcycles);
////
////			if(mcwp_case == 1 && kernel_poly.status[0] == 0){
////				interpAddPointValMP(kernel_poly.interp[0], mpPoint, Val);
////			}
////			else if(mcwp_case == 2 && kernel_poly.status[1] == 0){
////				interpAddPointValMP(kernel_poly.interp[1], mpPoint, Val);	
////			}
////			else if (mcwp_case == 3 && kernel_poly.status[2] == 0){
////				interpAddPointValMP(kernel_poly.interp[2], mpPoint, Val);	
////			}
////
////			mpf_set_q(n_clockcycles, clockcycles);
////			FILE *occup_temp = fopen(occupancy_file_path_list[j], "r");
////			if (occup_temp == NULL)
////			{
////				printf("ERROR: opening [%s]!\n", occupancy_file_path_list[j]);
////				exit (EXIT_FAILURE);
////			}
////			float occupancy = 0;
////			fscanf(occup_temp, "%f", &occupancy);
////			fclose(occup_temp);
////
////			gmp_fprintf(mcwp_result[j],
////					"N:%d, B0:%d, B1:%d, mwp:%Qd, cwp:%Qd, clockcycles:%.0Ff, case: %d, occupancy:%.2f\n",
////					kernel_poly.point->N[0], kernel_poly.point->N[1],
////					kernel_poly.point->N[2], mwp, cwp, n_clockcycles, mcwp_case,
////					occupancy);
////
////#if VERBOSE
////			gmp_printf("N:%d, b:%d, mwp:%Qd, cwp:%Qd, clockcycles:%.0Ff, occupancy:%.2f\n", kernel_poly.point->N[0],kernel_poly.point->N[1],mwp,cwp,n_clockcycles,occupancy);
////#endif
////
////			if ((kernel_poly.point->N[0] == best_n[j]
////					&& mpf_cmp(best_clock[j], n_clockcycles) > 0) || best_n[j] == 0)
////			{
////				mpf_set(best_clock[j], n_clockcycles);
////				best_b0[j] = kernel_poly.point->N[1];
////				best_b1[j] = kernel_poly.point->N[2];
////				best_n[j] = kernel_poly.point->N[0];
////			}
////			if ((kernel_poly.point +1)->N[0] != best_n[j])
////			{
////				fprintf(mcwp_result[j], "Best config for N = %d: ", best_n[j]);
////				gmp_fprintf(mcwp_result[j], "B0 = %d, B1 = %d, clockcycles = %Ff\n\n",
////				best_b0[j], best_b1[j], best_clock[j]);
////				best_n[j] = 0;
////				mpf_set_si(best_clock[j],0);
////				best_b0[j] = 0;
////				best_b1[j] = 0;
////						
////			}
////			for(int k = 0; k < 3; k ++){
////				stat = interpGetPoly(kernel_poly.interp[k], &kernel_poly.poly[k]);
////				if (stat == POLY_INTERP_SUCCESS && kernel_poly.status[k] == 0) {
////					kernel_poly.status[k] = 1;
////					fprintf(stderr, "Interpolated a polynomial for kernel %s in case %d: \n",kernel_names[j],k + 1);
////					fprintf(interpolation_result[j], "Interpolated a polynomial for kernel %s in case %d: \n",kernel_names[j],k + 1);
////				//	printLocalAA(kernel_poly.poly[k], syms, kernel_poly.nvar);
////					printf("In rational number:\n");
////					printPoly_AA(stderr, kernel_poly.poly[k], syms, kernel_poly.nvar);
////					printf("In double:\n");
////					printPolyDouble_AA(stderr, kernel_poly.poly[k], syms, kernel_poly.nvar);
////					printf("\n");
////					fprintf(interpolation_result[j],"In rational number:\n");
////					printPoly_AA(interpolation_result[j], kernel_poly.poly[k], syms, kernel_poly.nvar);
////					fprintf(interpolation_result[j],"In double:\n");
////					printPolyDouble_AA(interpolation_result[j], kernel_poly.poly[k], syms, kernel_poly.nvar);
////					fprintf(interpolation_result[j],"\n");
////					//fclose(mcwp_result[0]);
////					interpFree(kernel_poly.interp[k]);
////					freePolynomial_AA(kernel_poly.poly[k]);
////					success += kernel_poly.status[k];
////				} else if (stat == POLY_INTERP_FAILURE) {
////					fprintf(stderr, "ERROR: Failed to interpolate!\n");
////					break;
////				}
////			}
////			kernel_poly.point++;
////			if(kernel_poly.point->N[0] == 0){
////				printf("Run out of points!\n");
////				fprintf(interpolation_result[j],"Not enough points!\n");
////				return 0;
////			}
////		}
////	}
////
////	/////////////////////////
////	for(int k = 0; k < n_kernels; k ++){
////		fclose(mcwp_result[k]);
////		fclose(interpolation_result[k]);
////	}
////
////	for (int i = 0; i < 3; ++i)
////	{
////		mpq_clear(mpPoint[i]);
////	}
////	free(mpPoint);
////	//free(Val);
////
////	//freePolynomial_AA(poly);
////// 	for (int k = 0; k < n_kernels; k++)
////// 	{
////
////// 		FILE *mcwp_result = fopen(result_file_path_list[k], "ab+");
////// 		if (mcwp_result == NULL)
////// 		{
////// 			printf("ERROR: opening [%s]!\n", result_file_path_list[k]);
////// 			exit (EXIT_FAILURE);
////// 		}
////
////// 		fprintf(mcwp_result, "BlockSize,MWP,CWP,Clockcycles\n");
////// 		for (int d = 0; d < degree; d++)
////// 		{
////// 			fprintf(mcwp_result, "%f,%f,%f\n", mwp[k * degree + d],
////// 					cwp[k * degree + d], clockcycles[k * degree + d]);
////// 		}
////// 		fclose(mcwp_result);
////
////// #if VERBOSE
////// 		printf("kernel: %s\n", kernel_names[k]);
////// #endif 
////
////// 		char interpolation_path[256];
////// 		sprintf(interpolation_path, "%s%s_interpolation.tmp", prefix_kernel,
////// 				kernel_names[k]);
////// 		FILE* interpolation_file = fopen(interpolation_path, "w");
////// 		if (interpolation_path == NULL)
////// 		{
////// 			printf("ERROR: in opening [%s] \n", interpolation_path);
////// 			exit (EXIT_FAILURE);
////// 		}
////// 		printUnivarAA_tofile(interpolation_file,
////// 				univarInterpolateDoubles_AA(thread_per_block,
////// 						(clockcycles + k * degree), degree));
////// 		//#if VERBOSE
////// 		printf("writing result of interpolation to [%s] -> done!\n",
////// 				interpolation_path);
////// 		//#endif
////// 	}
////	return 0;
////}
////
///////////////////////////////////////////
////int main(int argc, char ** argv)
////{
////	return test_mwp_cwp(argc, argv);
////}
////
///////////////////////////////////////////
////
////// add comments
////// write functions
////// follow c99 standard, loop variales inside the body
////// take into account the cache friendliness
////// exit(EXIT_FAILURE);
