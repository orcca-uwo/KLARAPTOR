/*!
 \file mcpw_common.h
 \author Davood Mohajerani <dmohajer@uwo.ca>
 \brief
 */

#ifndef COMMON_H_
#define COMMON_H_

#include "mcwp_data_types.h"
///////////////////////////////////////

void print_long_dashed_line() {
	const char * long_dashed_line =
			"-----------------------------------------------------------------\n";
//  fwrite(long_dashed_line, )
	printf("%s", long_dashed_line);
}

///////////////////////////////////////

void print_short_dashed_line() {
	const char * short_dashed_line = "--------------------------------------\n";
//  fwrite(long_dashed_line, )
	printf("%s", short_dashed_line);
}

///////////////////////////////////////

void mpq_div_ui(mpq_t x, unsigned long int u) {
	mpq_t u_mpq;
	mpq_init(u_mpq);
	mpq_set_ui(u_mpq, u, 1);
	mpq_div(x, x, u_mpq);
	mpq_clear(u_mpq);
}

///////////////////////////////////////

void mpq_ceil(mpq_t x) {
	mpz_t numer;
	mpz_t denom;
	mpz_init(numer);
	mpz_init(denom);
	mpz_set(numer, mpq_numref(x));
	mpz_set(denom, mpq_denref(x));
//  mpq_get_num(numer, x);
//  mpq_get_den(denom, x);
	mpz_cdiv_q(numer, numer, denom);
	mpq_set_z(x, numer);
	mpz_clear(numer);
	mpz_clear(denom);
}

///////////////////////////////////////

int mpq_cmp_zero(mpq_t x) {
	return mpq_cmp_ui(x, 0, 1);
}

///////////////////////////////////////

int mpq_get_ui(mpq_t x) {
	mpq_t tmp;
	mpz_t tmp_zz;

	mpq_init(tmp);
	mpz_init(tmp_zz);

	mpq_set(tmp, x);
	mpq_ceil(tmp);
	mpq_get_num(tmp_zz, tmp);
	int val = mpz_get_ui(tmp_zz);
	mpz_clear(tmp_zz);
	mpq_clear(tmp);
	return val;
}

///////////////////////////////////////

uint64_t mpq_get_u64(mpq_t x) {
	uint64_t u32_mask = (1UL << 32) - 1;

	mpq_t tmp;
	mpz_t tmp_zz;

	mpq_init(tmp);
	mpq_set(tmp, x);
	mpq_ceil(tmp);

	mpz_init(tmp_zz);
	mpq_get_num(tmp_zz, tmp);
	uint64_t val_lo = mpz_get_ui(tmp_zz);
	val_lo = val_lo & (u32_mask);

	mpz_tdiv_q_2exp(tmp_zz, tmp_zz, 32);
	uint64_t val_hi = mpz_get_ui(tmp_zz);

	uint64_t val;
	val = (val_hi << 32);
	val += val_lo;
	mpz_clear(tmp_zz);
	mpq_clear(tmp);
	return val;
}

///////////////////////////////////////

void mpq_floor(mpq_t x) {
	mpz_t numer;
	mpz_t denom;
	mpz_init(numer);
	mpz_init(denom);
	mpz_set(numer, mpq_numref(x));
	mpz_set(denom, mpq_denref(x));
	mpz_tdiv_q(numer, numer, denom);
	mpq_set_z(x, numer);
	mpz_clear(numer);
	mpz_clear(denom);
}

///////////////////////////////////////

void mpq_min2(mpq_t m, mpq_t x, mpq_t y) {
	if (mpq_cmp(x, y) > 0)
		mpq_set(m, y);
	else
		mpq_set(m, x);
}

///////////////////////////////////////

void mpq_min3(mpq_t m, mpq_t x, mpq_t y, mpq_t z) {
	if (mpq_cmp(y, z) > 0)
		mpq_set(m, z);
	else
		mpq_set(m, y);
	if (mpq_cmp(x, m) < 0)
		mpq_set(m, x);
}

///////////////////////////////////////
///////////////////////////////////////

int int_ceiling_div(int x, int y) {
	return ((x + y - 1) / y);
}

///////////////////////////////////////
int difference_percentage_mpq(mpq_t x, mpq_t y) {
	int x_gt_y = 0;
	mpq_t diff_mpq, const_hundred_mpq;
	mpq_inits(diff_mpq, const_hundred_mpq, NULL);

	if (mpq_cmp(x, y) >= 0)
		x_gt_y = 1;

//  if (x_gt_y)
//    {
//      mpq_sub (diff_mpq, x, y);
//      mpq_div (diff_mpq, diff_mpq, x);
//    }
//  else
//    {
//      mpq_sub (diff_mpq, y, x);
//      mpq_div (diff_mpq, diff_mpq, y);
//    }
//  mpq_set_ui (const_mpq, 100, 1);
//  mpq_mul (diff_mpq, diff_mpq, const_mpq);

	if (x_gt_y) {
		mpq_div(diff_mpq, x, y);
	} else {
		mpq_div(diff_mpq, y, x);
	}
	mpq_set_ui(const_hundred_mpq, 100, 1);

	mpq_mul(diff_mpq, diff_mpq, const_hundred_mpq);
	mpq_ceil(diff_mpq);
	mpq_sub(diff_mpq, diff_mpq, const_hundred_mpq);

	int percentage = (int) (mpq_get_d(diff_mpq));
	if (x_gt_y)
		percentage *= -1;

	mpq_clears(diff_mpq, const_hundred_mpq, NULL);
	return percentage;
}

///////////////////////////////////////
int get_error_percentage(mpq_t t0, mpq_t t1) {
	mpq_t x, y;
	mpq_inits(x, y, NULL);
	//should be: x>=y
	if (mpq_cmp(t0, t1) >= 0) {
		mpq_set(x, t0);
		mpq_set(y, t1);
	} else {
		mpq_set(x, t1);
		mpq_set(y, t0);
	}

//  gmp_printf ("y=%f\n", mpq_get_d (y));
	float percentage;

	float a, b;
	a = mpq_get_d(x);
	b = mpq_get_d(y);

	if (b != 0) {
		percentage = 100.0 * ((a - b) / a);
//      printf ("FIRST_PERCENTAGE = %d\n", (int) percentage);
	} else {
		percentage = abs(a - b);
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

int param_list_init(param_list_t* param_list, param_list_type_t type) {
	int size = 0;
	item_to_idx_dict * dict;
	param_list_value_type_t value_type;
	switch (type) {
	case PARAM_LIST_TYPE_DEVICE:
		size = PARAM_LIST_SIZE_DEVICE;
		dict = device_param_names_dict;
		break;

	case PARAM_LIST_TYPE_PROFILING:
		size = PARAM_LIST_SIZE_PROFILING;
		dict = profiling_param_names_dict;
		break;

	case PARAM_LIST_TYPE_PTX:
		size = PARAM_LIST_SIZE_PTX;
		dict = ptx_param_names_dict;
		break;

	case PARAM_LIST_TYPE_RATIONAL_PROGRAM:
		size = PARAM_LIST_SIZE_RATIONAL_PROGRAM;
		dict = rational_program_param_names_dict;
		break;
	}

	////TODO: change type to class ?
	param_list->type = type;
	param_list->size = size;
	param_list->dict = dict;

	param_list->mpq_values = (mpq_t*) malloc(size * sizeof(mpq_t));
	for (int i = 0; i < size; i++)
		mpq_init(param_list->mpq_values[i]);
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int param_list_copy(param_list_t* dest_param_list,
		param_list_t* src_param_list) {
	if (dest_param_list->size != src_param_list->size)
		return EXIT_FAILURE;

	if (dest_param_list->type != src_param_list->type)
		return EXIT_FAILURE;

	if (dest_param_list->dict != src_param_list->dict)
		return EXIT_FAILURE;

	for (int i = 0; i < dest_param_list->size; i++)
		mpq_set(dest_param_list->mpq_values[i], src_param_list->mpq_values[i]);
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int param_list_clear(param_list_t* param_list) {
//  printf ("[@%s][name=%s][size=%d]\n", __func__,
//	  param_list_type_names_dict[param_list->type], param_list->size);

	for (int i = 0; i < param_list->size; i++)
		mpq_clear(param_list->mpq_values[i]);

	free(param_list->mpq_values);
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int set_param_list_value_by_index(param_list_t* param_list, int value_idx,
		void* value) {
	if (value_idx < param_list->size) {
		mpq_set(param_list->mpq_values[value_idx], value);
		return EXIT_SUCCESS;
	}
	return EXIT_FAILURE;
}

///////////////////////////////////////

int set_param_list_value_by_name(param_list_t* param_list, char* value_name,
		void* value) {
	for (int i = 0; i < param_list->size; i++) {
		if (strcmp(param_list->dict[i], value_name) == 0) {
			mpq_set(param_list->mpq_values[i], *((mpq_t*) value));
//	      	      gmp_printf ("[@%s][param_list=%s][%s]"
//	      		      "[%Qd]\n",
//	      		      __func__, param_list_type_names_dict[param_list->type],
//	      		      value_name, param_list->mpq_values[i]);
			return EXIT_SUCCESS;
		}
	}
	return EXIT_FAILURE;
}

///////////////////////////////////////

int param_list_print(param_list_t* param_list) {

	mpz_t num_zz, denom_zz;
	mpz_inits(num_zz, denom_zz, NULL);

	for (int i = 0; i < param_list->size; i++) {
		mpq_get_num(num_zz, param_list->mpq_values[i]);
		mpq_get_den(denom_zz, param_list->mpq_values[i]);
		gmp_printf("[%s]=[%Zd / %Zd]\n", param_list->dict[i], num_zz, denom_zz);
	}
	mpz_clears(num_zz, denom_zz, NULL);
	print_short_dashed_line();
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int parse_line_for_name_value_pair(char * line, char * param_name,
		void*param_value, param_list_value_type_t value_type) {
	if (line == NULL) {
		printf("[@%s][line==NULL]...ABORT\n", __func__);
		exit(EXIT_FAILURE);
	}

	const char left_tag = '[';
	const char right_tag = ']';
	const char delim_tag = ':';
	const char space_tag = ' ';

	char param_value_str[256];
	char * ptr = &line[0];
//  printf("line=%s\n", line);
	while (*ptr != left_tag) {
		ptr++;
	}
	ptr++;

	char *start_point = ptr;
	int str_size = 0;
	while (*ptr != delim_tag) {
		ptr++;
		str_size++;
	}

	strncpy(param_name, start_point, str_size);
	param_name[str_size] = '\0';
//      printf ("param_name = [%s]\n", param_name);

	ptr++;

	//skip all the spaces after delim tag.
	while (*ptr == space_tag) {
		ptr++;
	}
	str_size = 0;
	start_point = ptr;
	while (*ptr != right_tag) {
		ptr++;
		str_size++;
	}
	strncpy(param_value_str, start_point, str_size);
	param_value_str[str_size] = '\0';

	if (value_type == PARAM_LIST_VALUE_TYPE_INTEGER) {
		*((uint64_t*) param_value) = atoll(param_value_str);
	}

	if (value_type == PARAM_LIST_VALUE_TYPE_STR) {
		strcpy(((char*) param_value), param_value_str);
//      printf ("param_value_str=%s\n", param_value_str);
	}
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int read_param_list_from_file_v2(param_list_t* param_list,
		const char * param_path) {
	char param_name[128];
	const char *line_delim = "\n";

	FILE *param_file = fopen(param_path, "r");
	if (param_file == NULL) {
		printf("ERROR: opening params file: [%s] failed!\n", param_path);
		return (EXIT_FAILURE);
	}

	//// find size of the file.
	fseek(param_file, 0L, SEEK_END);
	size_t buffer_size = ftell(param_file);
	//// go back to beginning of the file.
	fseek(param_file, 0L, SEEK_SET);

	char * buffer = (char*) malloc((buffer_size + 1) * sizeof(char));

	int num = fread(buffer, sizeof(char), buffer_size, param_file);
	if (!num) {
		printf("[@%s][ERROR: in reading from file!]\n...)");
		exit(EXIT_FAILURE);
	}

	char * line = strtok(buffer, line_delim);
	int n_added_values = 0;

	uint64_t param_value = 0;
	mpq_t param_value_mpq;
	mpq_init(param_value_mpq);

	while (line != NULL) {
		parse_line_for_name_value_pair(line, param_name, &param_value,
				PARAM_LIST_VALUE_TYPE_INTEGER);
//  	  printf ("[%s]=[%d]\n", param_name, param_value);

		mpq_set_ui(param_value_mpq, param_value, 1);
		int stat = set_param_list_value_by_name(param_list, param_name,
				&param_value_mpq);
		if (stat == EXIT_SUCCESS) {
			n_added_values++;
		}
		//// to make sure we not try to add more elements than the actual size of the param list.
		if (n_added_values == param_list->size)
			break;
		line = strtok(NULL, line_delim);
		//      printf("HERE AT THE END!\n");
	}

	mpq_clear(param_value_mpq);
	free(buffer);
	fclose(param_file);

	return (EXIT_SUCCESS);
}

///////////////////////////////////////

int device_params_init(param_list_t** device_params,
		const char *device_params_path) {
	*device_params = (param_list_t*) malloc(sizeof(param_list_t));
	param_list_init(*device_params, PARAM_LIST_TYPE_DEVICE);
	read_param_list_from_file_v2(*device_params, device_params_path);

//  mpq_div_ui ((*device_params)->mpq_values[DEVICE_PARAM_Freq], 1000);

//  mpq_t bw_fraction_mpq;
//  mpq_init (bw_fraction_mpq);
//  mpq_set_ui (bw_fraction_mpq, 75, 100);
//  mpq_mul ((*device_params)->mpq_values[DEVICE_PARAM_Mem_bandwidth],
//	   (*device_params)->mpq_values[DEVICE_PARAM_Mem_bandwidth],
//	   bw_fraction_mpq);

//  mpq_set_ui (bw_fraction_mpq, 5, 1);
//  mpq_mul ((*device_params)->mpq_values[DEVICE_PARAM_Departure_del_coal],
//	   (*device_params)->mpq_values[DEVICE_PARAM_Departure_del_coal],
//	   bw_fraction_mpq);

//  mpq_set_ui (bw_fraction_mpq, 1, 4);
//    mpq_mul ((*device_params)->mpq_values[DEVICE_PARAM_Load_bytes_per_warp],
//  	   (*device_params)->mpq_values[DEVICE_PARAM_Load_bytes_per_warp],
//  	   bw_fraction_mpq);

//  mpq_clear (bw_fraction_mpq);
#if VERBOSE
	param_list_print (*device_params);
#endif
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int device_params_clear(param_list_t* device_params) {
	param_list_clear(device_params);
	free(device_params);
	return EXIT_SUCCESS;
}

///////////////////////////////////////

char *
get_absolute_device_name(const char * relative_device_name) {
	int n = strlen(relative_device_name);

	char * src, *dest;
	src = (char*) malloc(n);

	memcpy(src, relative_device_name, n);
	int len = 0;
	int last_slash_idx = 0;
	int last_dot_idx = 0;
	for (int i = 0; i < n; i++) {
		if (src[i] == '/')
			last_slash_idx = i;
		if (src[i] == '.')
			last_dot_idx = i;
	}

	last_slash_idx++;
	dest = (char*) malloc(last_dot_idx - last_slash_idx + 1);
	for (int i = last_slash_idx; i < last_dot_idx; i++) {
		dest[i - last_slash_idx] = src[i];
	}
	dest[last_dot_idx - last_slash_idx] = '\0';

	free(src);
	return dest;
}

///////////////////////////////////////

int set_device_name(char *device_name, const char *device_params_path) {
//  sprintf (device_name, "gtx760m");

	char *absolute_device_name;
	absolute_device_name = get_absolute_device_name(device_params_path);
	strcpy(device_name, absolute_device_name);
	free(absolute_device_name);

#if VERBOSE
	printf ("device_name=%s\n", device_name);
#endif

	const char* path = "current_device.tmp";
	FILE* file = fopen(path, "w");
	fprintf(file, "%s", device_name);
	fclose(file);
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int ptx_params_init(param_list_t** ptx_params_out, const char *ptx_params_path,
		int n_points_in_mesh) {
	int mcwp_error;
	/////////////////////////////////////
	////read the values from the file
	param_list_t* ptx_params = (param_list_t*) malloc(sizeof(param_list_t));
	mcwp_error = param_list_init(ptx_params, PARAM_LIST_TYPE_PTX);
	check_mcwp_error(mcwp_error, "param_list_init");

	mcwp_error = read_param_list_from_file_v2(ptx_params, ptx_params_path);
	check_mcwp_error(mcwp_error, "read_param_list_from_file_v2");
	/////////////////////////////////////
	*ptx_params_out = ptx_params;
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int ptx_params_clear(param_list_t* ptx_params, int n_points_in_mesh) {
	for (int i = 0; i < 1; i++)
		param_list_clear(&ptx_params[i]);
	free(ptx_params);
	return EXIT_SUCCESS;
}

///////////////////////////////////////

#endif
