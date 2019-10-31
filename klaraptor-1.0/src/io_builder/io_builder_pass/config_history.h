/*!
 \file config_history.h
 \author Davood Mohajerani <dmohajer@uwo.ca>
 \brief
 */

#ifndef CONFIG_HISTORY_H_
#define CONFIG_HISTORY_H_
///////////////////////////////////////
#include <stdio.h>
#include <string.h>

///////////////////////////////////////
#define check_rp_error(err, func)do{\
    if(err != EXIT_SUCCESS){\
	printf("[ERROR][%s:%d][\"%s\"]\n", __FILE__, __LINE__ , func);\
	exit(EXIT_FAILURE);\
    }\
    }while(0);
//    else\
//    {\
//	printf("[%-60s]...[PASS]\n", func);\
//    }
///////////////////////////////////////
typedef struct kernel_history_s {
	int *keys;
	int *dim_values;
	int *grid_dim_values;
	int size;
	int capacity;
} kernel_history_t;
///////////////////////////////////////
typedef enum visited_kernels_status_s {
	VISITED_KERNELS_UNINITIALIZED,
	VISITED_KERNELS_ENABLED,
	VISITED_KERNELS_DISABLED
} visited_kernels_status_t;
///////////////////////////////////////
typedef struct visited_kernels_s {
	visited_kernels_status_t status;
	int size;
	int capacity;
	char **kernel_names;
	kernel_history_t *history;
} visited_kernels_t;

///////////////////////////////////////
#define DEFAULT_KERNEL_HISTORY_CAPACITY 10

///////////////////////////////////////

//int
//kernel_info_dict_get_idx (int *kernel_idx, const char *dict_name,
//			  const char * kernel_name)
//{
//  //  printf("fetching kernel-name[%s]\n", kernel_name);
//  //	int verbose=0;
//  char kernel_param_str[1024];
//  kernel_info_dictionary_t * ptr = get_ptr_to_kernel_info_dict (dict_name);
//  sprintf (kernel_param_str, "%s_%s", ptr->prefix_str, kernel_name);
//
//  for (int i = 0; i < ptr->current_idx; i++)
//    {
//      if (strcmp (ptr->name_list[i], kernel_param_str) == 0)
//	{
//	  *kernel_idx = i;
//	  return EXIT_SUCCESS;
//	}
//    }
//  *kernel_idx = -1;
//  return EXIT_FAILURE;
//}
///////////////////////////////////////
///////////////////////////////////////
int kernel_history_init(kernel_history_t *h, const int capacity) {
	h->size = 0;
	h->capacity = capacity;
	size_t n_bytes = (h->capacity * sizeof(int));
	h->keys = (int*) malloc(n_bytes);
	h->dim_values = (int*) malloc(n_bytes);
	h->grid_dim_values = (int*) malloc(n_bytes);
	memset(h->keys, 0x00, n_bytes);
	memset(h->dim_values, 0x00, n_bytes);
	memset(h->grid_dim_values, 0x00, n_bytes);
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int kernel_history_clear(kernel_history_t *h) {
	free(h->keys);
	free(h->dim_values);
	free(h->grid_dim_values);
	h->size = 0;
	h->capacity = 0;
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int kernel_history_double_capacity(kernel_history_t *h) {
	size_t n_bytes = (h->capacity * sizeof(int));
	size_t double_n_bytes = (2 * h->capacity * sizeof(int));
	h->keys = (int*) realloc(h->keys, double_n_bytes);
	h->dim_values = (int*) realloc(h->dim_values, double_n_bytes);
	h->grid_dim_values = (int*) realloc(h->grid_dim_values, double_n_bytes);
	memset(&h->keys[h->capacity], 0x00, n_bytes);
	memset(&h->dim_values[h->capacity], 0x00, n_bytes);
	memset(&h->grid_dim_values[h->capacity], 0x00, n_bytes);
	h->capacity *= 2;
	return EXIT_SUCCESS;
}

///////////////////////////////////////

//get best config for n
int kernel_history_get_config(int *blockdim, int *griddim,
		const kernel_history_t *h, const int n) {

	for (int i = 0; i < h->size; i++)
		if (n == h->keys[i]) {
			//	printf("value is already recorded for the kernel\n");
			int val = h->dim_values[i];
			int mask = (1 << 10) - 1;
			int bx = 0, by = 0, bz = 0;
			int gx = 0, gy = 0, gz = 0;
			bz = val & mask;
			val >>= 10;
			by = val & mask;
			val >>= 10;
			bx = val & mask;

			blockdim[0] = bx;
			blockdim[1] = by;
			blockdim[2] = bz;

			val = h->grid_dim_values[i];
			gz = val & mask;
			val >>= 10;
			gy = val & mask;
			val >>= 10;
			gx = val & mask;

			griddim[0] = gx;
			griddim[1] = gy;
			griddim[2] = gz;

			return EXIT_SUCCESS;
		}
	for (int i = 0; i < 3; i++)
		blockdim[i] = -1;
	for (int i = 0; i < 3; i++)
		griddim[i] = -1;

	return EXIT_FAILURE;
}

///////////////////////////////////////

int kernel_history_add_config(kernel_history_t *h, const int n,
		const int*blockdim, const int *griddim) {

	int bx, by, bz;
	int gx, gy, gz;
	bx = blockdim[0];
	by = blockdim[1];
	bz = blockdim[2];

	gx = griddim[0];
	gy = griddim[1];
	gz = griddim[2];

	int val = 0;
	val += bx;
	val <<= 10;
	val += by;
	val <<= 10;
	val += bz;

	int grid_val = 0;
	grid_val += gx;
	grid_val <<= 10;
	grid_val += gy;
	grid_val <<= 10;
	grid_val += gz;

	for (int i = 0; i < h->size; i++)
		if (n == h->keys[i]) {
//	printf("value is already recorded for the kernel\n");
			return EXIT_SUCCESS;
		}
	if (h->size == h->capacity)
		kernel_history_double_capacity(h);

	h->keys[h->size] = n;
	h->dim_values[h->size] = val;
	h->grid_dim_values[h->size] = grid_val;
	h->size++;

	return EXIT_SUCCESS;
}

///////////////////////////////////////

int visited_kernels_init(visited_kernels_t* k, const int capacity) {
	if (k->status != VISITED_KERNELS_UNINITIALIZED)
		return EXIT_SUCCESS;

//  printf("visited_kernels_init");
	k->size = 0;
	k->capacity = capacity;
	size_t n_bytes = (k->capacity * sizeof(char**));
	const int max_kernel_name_len = 256;
	k->kernel_names = (char**) malloc(n_bytes);
	memset(k->kernel_names, 0x00, n_bytes);
	k->history = (kernel_history_t*) malloc(
			k->capacity * sizeof(kernel_history_t));
	for (int i = 0; i < k->capacity; i++) {
		kernel_history_init(&k->history[i], DEFAULT_KERNEL_HISTORY_CAPACITY);
	}
	k->status = VISITED_KERNELS_ENABLED;
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int visited_kernels_clear(visited_kernels_t* k) {
	for (int i = 0; i < k->size; i++) {
		free(k->kernel_names[i]);
	}
	free(k->kernel_names);
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int visited_kernels_double_capacity(visited_kernels_t* k) {
	size_t n_bytes = (k->capacity * sizeof(char**));
	size_t double_n_bytes = (k->capacity * sizeof(int));
	k->kernel_names = (char**) realloc(k->kernel_names, double_n_bytes);
	memset(&k->kernel_names[k->capacity], 0x00, n_bytes);
	k->capacity *= 2;
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int visited_kernels_get_kernel_idx(int * idx, const visited_kernels_t* k,
		const char * kernel_name) {
	for (int i = 0; i < k->size; i++) {
//      printf("comparing [%s]with[%s]\n",kernel_name, k->kernel_names[i] );
		if (strcmp(kernel_name, k->kernel_names[i]) == 0) {
			*idx = i;
			return EXIT_SUCCESS;
		}
	}
	*idx = -1;
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int visited_kernels_add_kernel(int * idx, visited_kernels_t* k,
		const char * kernel_name) {
	int rp_error = -1;
	if (k->size == k->capacity) {
		rp_error = (visited_kernels_double_capacity(k));
		check_rp_error(rp_error, "visited_kernels_double_capacity");
	}

	k->kernel_names[k->size] = (char*) malloc(strlen(kernel_name) + 1);
	strcpy(k->kernel_names[k->size], kernel_name);
	*idx = k->size;
	k->size++;
	return EXIT_SUCCESS;
}

///////////////////////////////////////

#endif
