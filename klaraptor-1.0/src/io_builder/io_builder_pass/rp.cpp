/*!
 \file rp.cpp
 \author Davood Mohajerani <dmohajer@uwo.ca>
 \brief
 */

///////////////////////////////////////
#include "rp.h"
#include "config_history.h"
#include <string.h>
///////////////////////////////////////
#define DEFAULT_VISITED_KERNEL_CAPACITY 10
///////////////////////////////////////
static visited_kernels_t global_rp_visited_kernels = {
		VISITED_KERNELS_UNINITIALIZED };
///////////////////////////////////////

int get_value_from_process(char *cmd, int * dim3_blockdim) {

	int verbose = 0;
	FILE *fp;
	char buffer[1024];

	/* Open the command for reading. */
	fp = popen(cmd, "r");
	if (fp == NULL) {
		printf("[@rp][ERROR: Failed to run command]...\n");
		exit(EXIT_FAILURE);
	}

//  while (fgets(buffer, sizeof(buffer)-1, fp) != NULL)

	const char start_pattern[64] = "[best_ec][idx, bx, by, bz]";
	int pattern_found = 0;
	while (pattern_found == 0) {
		if (fgets(buffer, sizeof(buffer) - 1, fp) != NULL)
			if (strstr(buffer, start_pattern) != NULL) {
				pattern_found = 1;
				break;
			}
	}

	if (pattern_found == 1)
		for (int i = 0; i < 1; i++) {
			if (fgets(buffer, sizeof(buffer) - 1, fp) != NULL) {
//	    printf ("%s", buffer);
				int idx;
				sscanf(buffer, "%d, %d, %d, %d", &idx, &dim3_blockdim[0],
						&dim3_blockdim[1], &dim3_blockdim[2]);
//	    printf("%d, %d, %d, %d", idx, dim3_blockdim[0],
//		    &dim3_blockdim[1], dim3_blockdim[2]);
			} else
				break;
		}
	pclose(fp);

	//normalizing problematic values.
	for (int i = 0; i < 3; i++)
		if (dim3_blockdim[i] <= 0)
			dim3_blockdim[i] = 1;

	if (verbose)
		printf("[@get_value_from_process][BEST_BX_BY_BZ=%d,%d,%d]\n",
				dim3_blockdim[0], dim3_blockdim[1], dim3_blockdim[2]);

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

int rp_estimator(char * kernel_name, int n, int * launch_params) {
	int verbose = 0;

	int * gx = &launch_params[0];
	int * gy = &launch_params[1];
	int * gz = &launch_params[2];

	int * bx = &launch_params[3];
	int * by = &launch_params[4];
	int * bz = &launch_params[5];

	int blockdim[3] = { 1, 1, 1 };
	int griddim[3] = { 1, 1, 1 };

	//init, if already init, it successfully returns.

	int rp_error = 0;
	int kernel_idx = -1;
	rp_error = visited_kernels_init(&global_rp_visited_kernels,
	DEFAULT_VISITED_KERNEL_CAPACITY);
	check_rp_error(rp_error, "visited_kernels_init");

	rp_error = visited_kernels_get_kernel_idx(&kernel_idx,
			&global_rp_visited_kernels, kernel_name);
	check_rp_error(rp_error, "visited_kernels_get_kernel_idx");
//  printf("visited_idx=%d\n", kernel_idx);
	if (kernel_idx == -1) {

//      printf("adding kernel to visited history\n");
		rp_error = visited_kernels_add_kernel(&kernel_idx,
				&global_rp_visited_kernels, kernel_name);
		check_rp_error(rp_error, "visited_kernels_add_kernel");
//      printf("visited_idx added =%d\n", kernel_idx);
	}

	kernel_history_t* kh_ptr = &global_rp_visited_kernels.history[kernel_idx];
	rp_error = kernel_history_get_config(blockdim, griddim, kh_ptr, n);
	if (rp_error != EXIT_SUCCESS) {
//      if (verbose)
//      printf ("could not read from the history...launching DP...\n");
		//	return EXIT_SUCCESS;
		//	printf("    [@RP][RP init]: simulating the elapsed time of computing best config ...\n");
		//	sleep(1);
		//	printf("    [@RP][RP done]; best results computed ...\n");
		char rp_name[1024];
		sprintf(rp_name, "driver_program_%s.bin", kernel_name);
		char rp_cmd[1024];
		sprintf(rp_cmd, "./%s %d", rp_name, n); ///
//  system (rp_cmd);

//      if (verbose)
//      printf ("rp_cmd= [%s]\n", rp_cmd);
		//get best block dims for dp.
		get_value_from_process(rp_cmd, blockdim);
//      if (verbose)
//	printf ("[@RP][best][bx = %d, by = %d, bz = %d]\n", blockdim[0],
//		blockdim[1], blockdim[2]);
		//now, compute the griddim as a function of n, bx, by, bz;
		//the griddim must be expressed as
		////	computing griddim3 based on blockdim3 and n must be writtern as a function.
		compute_griddim_from_blockdim(kernel_name, n, blockdim, griddim);

		//add to kernel_history.
		rp_error = kernel_history_add_config(kh_ptr, n, blockdim, griddim);
		check_rp_error(rp_error, "kernel_history_add_config");
	}
//  else
//    {
//      printf("[RETRIEVED FROM HISTORY!\n");
//    }

	*gx = griddim[0];
	*gy = griddim[1];
	*gz = griddim[2];

	*bx = blockdim[0]; //2*n;
	*by = blockdim[1]; //3*n;
	*bz = blockdim[2]; //4*n;

	if (verbose)
		printf("[@RP][launch]"
				"[(gx, gy, gz)=(%d, %d, %d)]"
				"[(bx, by, bz)=(%d, %d, %d)]\n", *gx, *gy, *gz, *bx, *by, *bz);

	return EXIT_SUCCESS;
}

///////////////////////////////////////

/////////////////////////////////////////
//void
//rp_history_init(void) __attribute__ ((constructor))
//{
////  printf ("this is JUST CLEARING KERNEL HISTORY\n");
//}

///////////////////////////////////////

void rp_history_clear(void) __attribute__ ((destructor))
{
//  printf ("this is JUST CLEARING KERNEL HISTORY\n");
}

///////////////////////////////////////
