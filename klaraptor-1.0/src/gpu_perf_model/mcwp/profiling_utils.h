/*!
 \file profiling_utils.h
 \author Davood Mohajerani <dmohajer@uwo.ca>
 \brief
 */

#ifndef PROFILING_UTILS_H_
#define PROFILING_UTILS_H_

///////////////////////////////////////
#include "mcwp_data_types.h"

/////////////////////////////////////////
//char *
//get_absolute_device_name (const char * relative_device_name)
//{
//  int n = strlen (relative_device_name);
//
//  char * src, *dest;
//  src = (char*) malloc (n);
//
//  memcpy (src, relative_device_name, n);
//  int len = 0;
//  int last_slash_idx = 0;
//  int last_dot_idx = 0;
//  for (int i = 0; i < n; i++)
//    {
//      if (src[i] == '/')
//	last_slash_idx = i;
//      if (src[i] == '.')
//	last_dot_idx = i;
//    }
//
//  last_slash_idx++;
//  dest = (char*) malloc (last_dot_idx - last_slash_idx + 1);
//  for (int i = last_slash_idx; i < last_dot_idx; i++)
//    {
//      dest[i - last_slash_idx] = src[i];
//    }
//  dest[last_dot_idx - last_slash_idx] = '\0';
//
//  return dest;
//}
//
///////////////////////////////////////
// check the constraint file for a kernel
// return 0 on success.
int eval_constraints(char * kernel_name, int n, int b0, int b1) {
	////////////////////////////////////
	const char * eval_const_script = "../../mcwp/eval_const.py";
//  copy_file (eval_const_script, ".");
	////////////////////////////////////
	char cmd[1024];
	sprintf(cmd, "python %s %s %d %d %d", eval_const_script, kernel_name, n, b0,
			b1);

	FILE* pipe;
	pipe = popen(cmd, "r");
	if (pipe == NULL) {
		printf("ERROR: FAILED TO RUN [%s]\n", cmd);
		return (EXIT_FAILURE);
	}
	char buffer[32];
	int value = 0;
	if (fgets(buffer, sizeof(buffer) - 1, pipe) != NULL) {
		value = atoi(buffer);
	} else {
		//		printf("ERROR: NOTHING READ!\n");
		value = -1;
	}

	pclose(pipe);
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

//int profiler_init()

///////////////////////////////////////

char*
get_cubin_name() {
	FILE* cubin_name_file = fopen("cudabin_name", "r");
	fseek(cubin_name_file, 0L, SEEK_END);
	size_t buffer_size = ftell(cubin_name_file);
	rewind(cubin_name_file);

	char* buffer = (char*) malloc(buffer_size + 1);
	for (int i = 0; i < buffer_size + 1; i++)
		buffer[i] = '\0';
	fread(buffer, 1, buffer_size, cubin_name_file);

	int eol_idx = -1;
	for (int i = 0; i < buffer_size + 1; i++)
		if (buffer[i] == '\n') {
			eol_idx = i;
			buffer[i] = '\0';
			break;
		}

	fclose(cubin_name_file);

	return buffer;

}

///////////////////////////////////////

int profiling_info_init(profiling_info_t** pi_out,
		const kernel_type_enum_t kernel_type, const int lower_bound,
		const int upper_bound, const int n_kernels) {
	profiling_info_t* pi = (profiling_info_t*) malloc(sizeof(profiling_info_t));
	*pi_out = pi;
	/**
	 * 1. find pointer to head of the mesh.
	 * 2. find number of points in the mesh.
	 * 3. set the lower bound and upper bound.
	 * 4. allocate space for the buffer.
	 * 5. allocate space for a table of profiling info,
	 * 	-- each entry will be as larger as the "buffer_size".
	 * 6. initialize map of visited points.
	 * 7. get the cudabin_name (name of the CUDA binary for the source).
	 * 8.
	 */
	/////////////////////////////////////
	//find pointer to head of the mesh
	point_set_t * point_set;

	pi->kernel_type = kernel_type;
	switch (kernel_type) {
	case OneDimKernel:
		point_set = (&global_n_bx);
		break;

	case TwoDimKernel:
		point_set = (&global_n_bx_by);
		break;

	case ThreeDimKernel:
		printf("[ERROR: THREE DIM PROBLEMS ARE NOT SUPPORTED!\n");
		return EXIT_FAILURE;
		//// TODO: add case of three dimensional kernels.
	}

	pi->point_set = point_set;

//  single_point_t* ptr = &pi->point_set->values[0];
	/////////////////////////////////////
	//find n_points in mesh
	int n_points_in_mesh = point_set->size;
	pi->n_points_in_mesh = n_points_in_mesh;
	/////////////////////////////////////
	pi->lower_bound = lower_bound;
	pi->upper_bound = upper_bound;

	/////////////////////////////////////
	size_t max_line_size = 80; //80 chars in each line of profiling result.
	//// each profiling session will print profiling params + 8 extra line of info.
	size_t n_lines_per_profiling_result = PARAM_LIST_SIZE_PROFILING + 8;
	size_t buffer_size_per_kernel = n_lines_per_profiling_result
			* max_line_size;
	size_t buffer_size = n_kernels * buffer_size_per_kernel;

	/////////////////////////////////////
	pi->buffer_size_per_kernel = buffer_size_per_kernel;
	//maximum buffer size, allocated cautiously such that it will fit all
	//profiling info for "n_kernels".
	pi->buffer_size = buffer_size;
	//keep track of number of chars stored in the buffer.
	pi->current_buffer_idx = 0;
	//in each read, load the "buffer_chunk_size" characters into the buffer.
	pi->buffer_chunk_size = 1024;
	//allocate the buffer.
	pi->buffer = (char*) malloc(buffer_size);

	/////////////////////////////////////
	//allocate space for profiling result table.
	//each entry is as large as "buffer_size" chars.
	pi->profiling_result_table = (char**) malloc(
			n_points_in_mesh * sizeof(char*));

	for (int i = 0; i < n_points_in_mesh; i++) {
		pi->profiling_result_table[i] = NULL;
	}

	/////////////////////////////////////
	/**
	 * keep track of visited (profiled) points in the mesh.
	 * note that often a subset of mesh points will be profiled,
	 * not all of them!
	 */
	pi->visited_points_map = (int*) malloc(n_points_in_mesh * sizeof(int));
	memset(pi->visited_points_map, 0x00, n_points_in_mesh * sizeof(int));
	/////////////////////////////////////
	//// get cubin name from file.
	char *cubin_name;
	cubin_name = get_cubin_name();
#if VERBOSE
	printf ("--[cubin_name=[%s]]\n", cubin_name);
#endif
	const char *profiler_path = "/bin/klaraptor-profiler";

	char klaraptor_path[PATH_MAX];
	strcpy(klaraptor_path, getenv("KLARAPTOR_PATH"));

	sprintf(pi->profiler_str, "/bin/bash %s/%s ./%s", klaraptor_path,
			profiler_path, cubin_name);
	free(cubin_name);
#if VERBOSE
	printf ("--[profiler_str=[%s]]\n", pi->profiler_str);
#endif
	pi->profiler_str_len = strlen(pi->profiler_str);
	/////////////////////////////////////
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int profiling_info_rewind(profiling_info_t* pi) {
	pi->current_buffer_idx = 0;
	for (int i = 0; i < pi->buffer_size; i++)
		pi->buffer[i] = '\0';
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int profiling_info_clear(profiling_info_t* pi) {
	for (int i = 0; i < pi->n_points_in_mesh; i++)
		if (pi->profiling_result_table[i])
			free(pi->profiling_result_table[i]);
	free(pi->profiling_result_table);
	free(pi->visited_points_map);
	free(pi->buffer);
	free(pi);
	return EXIT_SUCCESS;
}

///////////////////////////////////////

//// use the profiler info in pi and performp profiling for params_str.
int profile_single_mesh_point(profiling_info_t * pi, int point_idx,
		char * params_str) {
//  const char suffix_str[64] = "";  //">/dev/null 2>&1 && exit 0 || exit 1";

	int params_str_len = strlen(params_str);
//  int suffix_str_len = strlen (suffix_str);

	size_t cmd_str_len = pi->profiler_str_len + params_str_len + 2;
	char * cmd_str = (char*) malloc(cmd_str_len);

	sprintf(cmd_str, "%s %s", pi->profiler_str, params_str);

//  printf ("[cmd_str]=[%s]\n", cmd_str);
#if VERBOSE>=3
	printf("executing [%s ]... \n", cmdcmd_str);
#endif

//  freopen ("/dev/null", "w", stdout);
	FILE* pipe;
	pipe = popen(cmd_str, "r");
	if (pipe == NULL) {
		printf("ERROR: FAILED TO RUN [%s]\n", cmd_str);
		return (EXIT_FAILURE);
	}

	int n_read_chars = 0;
	usleep(1000); //sleep 1 ms to give the CUDA device some rest time.
	profiling_info_rewind(pi);

	int n_buffers = 1;
	size_t buffer_size = 2048;
	char * local_buffer = (char*) malloc(buffer_size);
	for (int i = 0; i < buffer_size; i++)
		local_buffer[i] = '\0';
	int buffer_idx = 0;
	while (1) {
		n_read_chars = fread(local_buffer + buffer_idx, 1, buffer_size, pipe);
//      printf ("n_read_chars=%d\n", n_read_chars);
		if (n_read_chars == buffer_size) {
			buffer_idx += n_read_chars;
			n_buffers += 1;
			size_t new_buffer_size = buffer_size * n_buffers;
//	  printf ("new_buffer_size=%d\n", new_buffer_size);
			char *new_buffer = (char*) realloc(local_buffer, new_buffer_size);
			if (new_buffer == NULL) {
//	      printf("new buffer is NULL!\n");
				exit(EXIT_FAILURE);
			}
			local_buffer = new_buffer;
			for (int i = buffer_idx; i < new_buffer_size; i++)
				local_buffer[i] = '\0';
		} else {
			buffer_idx += n_read_chars;
			break;
		}
	}

	pi->profiling_result_table[point_idx] = (char*) malloc(buffer_idx + 1);
	strncpy(pi->profiling_result_table[point_idx], local_buffer, buffer_idx);
	free(local_buffer);

//  printf ("[@%s][copied profiling result for [point_idx=%d] to table!]...\n",
//	  __func__, point_idx);
//  printf ("pi->profiling_result_table[point_idx]=%s\n",
//	  pi->profiling_result_table[point_idx]);
	pclose(pipe);
	free(cmd_str);
	return EXIT_SUCCESS;
}

///////////////////////////////////////

/**
 * 1. profile all the points for the same binary and store the result in memory.
 * 2. parse the result for each kernel and store them in the kernel_params.
 * 3. run mcwp for each one.
 * 4. The separation provides the chance for parallelization.
 */
int profile_all_mesh_points(profiling_info_t * pi, char*params_str_pattern) {

	int bar_size = 0;
	char params_str[1024];
	//  while ((point_ptr != NULL) && (current_point_idx < n_points_in_mesh))
	printf("[@%s]\n", __func__);

	progress_bar(&bar_size, PROGRESS_BAR_START);
	for (int i = 0; i < pi->n_points_in_mesh; i++) {
		single_point_t * point_ptr = &(pi->point_set->values[i]);
		int current_n = point_ptr->N[0];
		//      printf ("[current_n]=%d\n", current_n);
		if ((current_n < pi->lower_bound) || (current_n > pi->upper_bound)) {
			//simply get the next point and continue;
			continue;
		}

		int n, b0, b1, b2;
		n = point_ptr->N[0];
		b0 = point_ptr->N[1];
		b1 = point_ptr->N[2];
		b2 = 1;

#if VERBOSE
		printf ("-- profiling [n=%4d, bx=%4d, by=%4d, bz=%4d]\n", n, b0, b1, b2);
#else
		progress_bar(&bar_size, PROGRESS_BAR_CONTINUE);
#endif
//      print_long_dashed_line ();
		sprintf(params_str, params_str_pattern, n, b0, b1);
		//// TODO: this is in fact profiling a single binary, not a general exec_cmd.
		int status = profile_single_mesh_point(pi, i, params_str);
		if (status != EXIT_SUCCESS) {
			printf("[ERROR: profiling_mesh_point has FAILED!]"
					"...skipping point [idx=%d]\n", i);
			print_short_dashed_line();
			continue;
		}
#if VERBOSE >=2
		printf("[PROFILING]:[DONE]...\n");
#endif

		pi->visited_points_map[i] = 1;
	}

#if !VERBOSE
	progress_bar(&bar_size, PROGRESS_BAR_STOP);
#endif
	printf("[@%s]:[DONE]...\n", __func__);
	print_long_dashed_line();
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int init_start_stop_ptr_list(char*** start_ptr_list_out,
		char *** stop_ptr_list_out, char * result_str, int n_kernels) {

	const char * begin_tag_str = "[KLARAPTOR_PROFILER_RESULT_BEGIN]";
	const char * end_tag_str = "[KLARAPTOR_PROFILER_RESULT_END]";

	char ** start_ptr_list = (char**) malloc(n_kernels * sizeof(char*));
	char** stop_ptr_list = (char**) malloc(n_kernels * sizeof(char*));

	*start_ptr_list_out = start_ptr_list;
	*stop_ptr_list_out = stop_ptr_list;

	int invalid_result = 0;

#pragma nounroll
	for (int k = 0; k < n_kernels; k++) {
		if (k == 0) {
			start_ptr_list[k] = strstr(result_str, begin_tag_str);
			if (start_ptr_list[k] == NULL) {
				printf("result_str=%s\n", result_str);
				printf("FAILED STRSTR 1\n");
//		  exit (EXIT_FAILURE);
				invalid_result = 1;
				break;
			}
			stop_ptr_list[k] = strstr(start_ptr_list[k], end_tag_str);
			if (stop_ptr_list[k] == NULL) {
				printf("FAILED STRSTR 2\n");
//		  exit (EXIT_FAILURE);
				invalid_result = 1;
				break;
			}
		} else {
			start_ptr_list[k] = strstr(stop_ptr_list[k - 1], begin_tag_str);
			if (start_ptr_list[k] == NULL) {
				printf("FAILED STRSTR 3\n");
//		  exit (EXIT_FAILURE);
				invalid_result = 1;
				break;
			}
			stop_ptr_list[k] = strstr(start_ptr_list[k], end_tag_str);
			if (stop_ptr_list[k] == NULL) {
				printf("FAILED STRSTR 4\n");
//		  exit (EXIT_FAILURE);
				invalid_result = 1;
				break;
			}
		}
	}
	if (invalid_result)
		return EXIT_FAILURE;
	else

		return EXIT_SUCCESS;
}

///////////////////////////////////////

int clear_start_stop_ptr_list(char** start_ptr_list, char ** stop_ptr_list) {
	free(start_ptr_list);
	free(stop_ptr_list);
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int parse_single_profiling_result(poly_params * kernel_poly_params,
		int n_kernels, char** start_ptr_list, char **stop_ptr_list,
		int point_idx) {

	int verbose = 0;

	const char * line_delim = "\n";
	//      printf("point_idx=%d\n", i);
	char param_name[256];  //= (char*) malloc (max_param_name_size);
	uint64_t param_value = 0;
	mpq_t param_value_mpq;
	mpq_init(param_value_mpq);

#pragma nounroll
	for (int k = 0; k < n_kernels; k++) {
		char *line = strtok(start_ptr_list[k], line_delim);
		if (line == NULL) {
			printf("IMMEDIATE BREAK 2!");
			break;
		}

		line = strtok(NULL, line_delim); //skip the line including begin_tag.
		if (line == NULL) {
			printf("IMMEDIATE BREAK 3!");
			break;
		}

		uint64_t kernel_idx = -1;
		uint64_t valid_profiling = -1;
		/**
		 * The first three lines of a profiling session report:
		 * 1. kernel_name (char*)
		 * 2. kernel_idx w.r.t. order in "kernel_name_list.tmp" (integer), and
		 * 3. validity of profiling session (integer: 0=FAIL, 1=SUCCESS)
		 */
#pragma nounroll
		for (int s = 0; s < 3; s++) {
			switch (s) {
			//		case 0:
			//	      parse_line_for_name_value_pair (line, param_name,
			//					      &current_kernel_name,
			//					      PARAM_LIST_VALUE_TYPE_STR);
			//		  break;

			case 1:
				parse_line_for_name_value_pair(line, param_name, &kernel_idx,
						PARAM_LIST_VALUE_TYPE_INTEGER);
				break;

			case 2:
				parse_line_for_name_value_pair(line, param_name,
						&valid_profiling, PARAM_LIST_VALUE_TYPE_INTEGER);
				break;
			}
			line = strtok(NULL, line_delim);
		}
		if ((valid_profiling == 0) || (kernel_idx != k)) {
			/**
			 * even though the point has been in the range of lower bound
			 * and upper bound, however, the profiling has failed for some
			 * reason. Therefore, must overwrite the validity of the point
			 * in "visited_points_map" only for the corresponding kernel.
			 */
			kernel_poly_params[kernel_idx].visitd_points_map[point_idx] = 0;
			//	      found_invalid_profiling = 1;
			if (verbose)
				printf("[WARNING: BROKEN PROFILING for KERNEL [%d]@[%d]]..."
						"CONTINUE FOR OTHER KERNELS AT THE POINT!\n",
						kernel_idx, point_idx);
			continue;
		}
		////      printf ("[current_kenrel_name=%s]\n", current_kernel_name);
		////      printf ("[kernel_idx=%d]\n", kernel_idx);
		////      printf ("[valid_profiling=%d]\n", valid_profiling);

		////first, read profiling params for the current point.
		////second, read kernel launch params for the current point.
		param_list_t *lptr =
				&kernel_poly_params[kernel_idx].profiling_params_list[point_idx];
		int n_params;
		n_params = lptr->size;
#pragma nounroll
		for (int np = 0; np < n_params; np++) {
			int stat = parse_line_for_name_value_pair(line, param_name,
					&param_value, PARAM_LIST_VALUE_TYPE_INTEGER);
			mpq_set_ui(param_value_mpq, param_value, 1);
			set_param_list_value_by_name(lptr, param_name, &param_value_mpq);
			if ((line == stop_ptr_list[k]) || (line == NULL)) {
				printf(
						"[@%s]"
								"[((line == stop_ptr)||(line==NULL))]...[UNEXPECTED!]\n",
						__func__);
				printf("ABORT IMMEDIATELY!\n");
				exit(EXIT_FAILURE);
				break;
			} else
				line = strtok(NULL, line_delim);
		}
	}
	mpq_clear(param_value_mpq);

	return EXIT_SUCCESS;
}

///////////////////////////////////////

int parse_all_profiling_results_for_all_kernels(profiling_info_t *pi,
		poly_params * kernel_poly_params, int n_kernels) {
//  size_t max_param_name_size = 256;
//  char current_kernel_name[MAX_KERNEL_NAME_LEN];

	/**
	 * search for "KLARAPTOR_PROFILER_RESULT_BEGIN" and "KLARAPTOR_PROFILER_RESULT_END"
	 * between the begin and end tags, look for kernel name
	 */

//#if ALLOW_OMP_LOOPS
//#pragma omp parallel for
//#endif
	for (int i = 0; i < pi->n_points_in_mesh; i++) {
		//skip the point if it has not been profiled in the first place.
		if (pi->visited_points_map[i] == 0) {
//	  printf("point [%d] was not visited!\n", i);
			continue;
		}

		char **start_ptr_list, **stop_ptr_list;
		int mcwp_error = init_start_stop_ptr_list(&start_ptr_list,
				&stop_ptr_list, pi->profiling_result_table[i], n_kernels);
//      check_mcwp_error(mcwp_error, "init_start_stop_ptr_list");
		if (mcwp_error == EXIT_SUCCESS) {
			mcwp_error = parse_single_profiling_result(kernel_poly_params,
					n_kernels, start_ptr_list, stop_ptr_list, i);
			check_mcwp_error(mcwp_error, "parse_single_profiling_result");
		}

		mcwp_error = clear_start_stop_ptr_list(start_ptr_list, stop_ptr_list);
		check_mcwp_error(mcwp_error, "clear_start_stop_ptr_list");
	}

	return EXIT_SUCCESS;
}

///////////////////////////////////////

#endif
