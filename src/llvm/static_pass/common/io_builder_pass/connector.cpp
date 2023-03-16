#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "rp.h"

///////////////////////////////////////
#ifndef VERBOSE
#define VERBOSE 1
#endif
///////////////////////////////////////
///////////////////////////////////////

enum kernel_info_dict_status
{
  NOT_INITIALIZED, IS_INITIALIZED
};

struct kernel_info_dictionary_s
{
  enum kernel_info_dict_status is_initialized; //initially set to 0.
  char dict_name[256]; //set at declaration.
  char prefix_str[64]; //set at declaration.
  int *value_list; //set by method.
  char **name_list; //set by method.
  int size; //set by method.
  int current_idx; //increased per update.
} kernel_info_dictionary_default =
  { NOT_INITIALIZED };

typedef struct kernel_info_dictionary_s kernel_info_dictionary_t;

///////////////////////////////////////
//// declare all the required dictionaries here.
///////////////////////////////////////
static kernel_info_dictionary_t global_kernel_size_param_idx_map =
  { NOT_INITIALIZED, "global_kernel_size_param_idx_map",
      "kernel_info_size_param_idx" };
static kernel_info_dictionary_t global_kernel_dim_map =
  { NOT_INITIALIZED, "global_kernel_dim_map", "kernel_info_dim" };
#define MAX_KERNEL_NAME_LEN_IN_CONNECTOR 1024

///////////////////////////////////////
///////////////////////////////////////

//// get the pointer to the kernel info dictionary using the dict name.
kernel_info_dictionary_t *
get_ptr_to_kernel_info_dict (char *dict_name)
{
  if (strcmp (dict_name, "global_kernel_size_param_idx_map") == 0)
    return &global_kernel_size_param_idx_map;
  if (strcmp (dict_name, "global_kernel_dim_map") == 0)
    return &global_kernel_dim_map;

  printf ("[@%s]"
	  "[FATAL ERROR: "
	  "could not get pointer to kernel info dictionary[%s]]"
	  "..."
	  "[EXIT IMMEDIATELY!]\n",
	  __func__, dict_name);
  exit (EXIT_FAILURE);
}

///////////////////////////////////////
///////////////////////////////////////

void
init_kernel_info_dict (char * dict_name, int size)
{
#if VERBOSE
  printf("[@%s]"
      "[initializing kernel info dictionary [%s] of size=[%d]]\n",
      __func__, dict_name, size);
#endif
  kernel_info_dictionary_t *ptr = get_ptr_to_kernel_info_dict (dict_name);

  if (ptr->is_initialized == IS_INITIALIZED)
    {
#if VERBOSE
      printf("[kernel_idx_map is already initialized!]...ignoring\n");
#endif
      return;
    }

#if VERBOSE
  printf("[dict_name=%s][dict_param_prefix=%s]\n", ptr->dict_name,
      ptr->prefix_str);
#endif
  ptr->size = size;
  ptr->name_list = (char**) malloc (size * (sizeof(char*)));
  ptr->value_list = (int*) malloc (size * sizeof(int));
  ptr->is_initialized = IS_INITIALIZED;
  ptr->current_idx = 0;
  for (int i = 0; i < size; i++)
    {
      ptr->value_list[i] = -1;
      ptr->name_list[i] = (char*) malloc (
      MAX_KERNEL_NAME_LEN_IN_CONNECTOR);
      strcpy (ptr->name_list[i], "");
    }
}

///////////////////////////////////////
///////////////////////////////////////

void
add_to_kernel_info_dict (char* dict_name, char*kernel_param_str, int value)
{
  int verbose = 0;
  kernel_info_dictionary_t * ptr = get_ptr_to_kernel_info_dict (dict_name);
  int idx = ptr->current_idx;
  ptr->current_idx++;
  if (ptr->is_initialized != IS_INITIALIZED)
    {
#if VERBOSE
      printf("[accessing [%s] failed as it is not initialized yet!]"
	  "...ignoring\n", dict_name);
#endif
      return;
    }
  if (idx >= ptr->size)
    {
#if VERBOSE
      printf("[accessing [%s] failed as it has no more space!]...ignoring\n",
	  dict_name);
#endif
      return;
    }

  ptr->value_list[idx] = value;
  strcpy (ptr->name_list[idx], kernel_param_str);
#if VERBOSE
  printf("[add_to_kernel_idx_map][idx=%d][kernel_param_str=%s][value=%d]\n",
      idx, kernel_param_str, value);
#endif
}

///////////////////////////////////////
///////////////////////////////////////
int
init_cu_driver_call ()
{
#if VERBOSE
  printf("[INIT CUDA DRIVER CALL CONTEXTS, MODULES, etc!]...\n");
#endif
  return EXIT_SUCCESS;
}

///////////////////////////////////////
///////////////////////////////////////

int
get_value_from_kernel_info_dict (char *dict_name, char * kernel_name,
				 int *size_idx)
{
//	int verbose=0;
  char kernel_param_str[1024];
  kernel_info_dictionary_t * ptr = get_ptr_to_kernel_info_dict (dict_name);
  sprintf (kernel_param_str, "%s_%s", ptr->prefix_str, kernel_name);

  for (int i = 0; i < ptr->current_idx; i++)
    {
      if (strcmp (ptr->name_list[i], kernel_param_str) == 0)
	{
	  *size_idx = ptr->value_list[i];
#if VERBOSE
	  printf("[@%s][getting [%s=%d]]\n", __func__, kernel_param_str,
	      *size_idx);
#endif
	  return EXIT_SUCCESS;
	}
    }
  return EXIT_FAILURE;
}

///////////////////////////////////////
///////////////////////////////////////
int
strip_kernel_name (char *kernel_name)
{
  char tmp_name[MAX_KERNEL_NAME_LEN_IN_CONNECTOR];
  int n = strlen (kernel_name);
  int len = 0;
  //kernel_ : the first
  for (int i = 7; i < n - 6; i++)
    len += sprintf (kernel_name + len, "%c", kernel_name[i]);

  return EXIT_SUCCESS;
}

///////////////////////////////////////
///////////////////////////////////////

//char *kernel_name, int *n, int *x_vector
//get kernel_name, kernel_params, launch_params the same as kernel_invoker function
//return an integer array of the best configuration values.
void
get_best_config (const char *kernel_name_in, int *launch_params,
		 const void** kernel_params)
{
  //dereference the value of n using the idx-map, then, iterate over the kernel params.
  char kernel_name[MAX_KERNEL_NAME_LEN_IN_CONNECTOR];

  sprintf (kernel_name, "%s", kernel_name_in);
  strip_kernel_name (kernel_name);
#if VERBOSE
  printf("[kernel_name] = [%s] --> [%s] \n", kernel_name_in, kernel_name);
#endif
  int kernel_size_idx;
  int kernel_dim;
  if (get_value_from_kernel_info_dict ("global_kernel_size_param_idx_map",
				       kernel_name,
				       &kernel_size_idx) != EXIT_SUCCESS)
    {
      printf ("[@get_best_config]"
	      "[ERROR: Failed to get the global_kernel_param !]..."
	      "[EXIT IMMEDIATELY]\n");
      exit (EXIT_FAILURE);
    }

  if (get_value_from_kernel_info_dict ("global_kernel_dim_map", kernel_name,
				       &kernel_dim) != EXIT_SUCCESS)
    {
      printf ("[@get_best_config]"
	      "[ERROR: Failed to get the global_kernel_dim !]..."
	      "[EXIT IMMEDIATELY]\n");
      exit (EXIT_FAILURE);
    }

  int n = -1;
  //dereference the size parameter.
  n = **(int**) (&kernel_params[kernel_size_idx]);
  int local_n = n;
//	int best_config[6] =
//	{ 1, 1, 1, 1, 1, 1 };
//	memcpy(best_config, launch_params, 6 * sizeof(int));
#if VERBOSE
    {
      printf("----------------------------------------------\n");
      printf("    [@connector][kernel_size_idx=%d]\n", kernel_size_idx);
      printf("    [@connector][kernel_dim=%d]\n", kernel_dim);
      printf("    "
	  "[@connector]"
	  "[passing input value N=[%d] for kernel [%s] to RP]"
	  "... \n", local_n, kernel_name);
    }
#endif

  rp_estimator (kernel_name, local_n, launch_params);

#if VERBOSE
  const char result_str_format[64] =
  "    [@connector][RP result]: best_config: %s%s\n";
  const char result_griddim_format[64] = "[gx=%d, gy=%d, gz=%d]";
  const char result_blockdim_format[64] = "[bx=%d, by=%d, bz=%d]";
  char result_griddim_str[256];
  char result_blockdim_str[256];
  char result_str[1024];
  sprintf(result_griddim_str, result_griddim_format, launch_params[0],
      launch_params[1], launch_params[2]);
  sprintf(result_blockdim_str, result_blockdim_format, launch_params[3],
      launch_params[4], launch_params[5]);
  sprintf(result_str, result_str_format, result_griddim_str,
      result_blockdim_str);
  printf("%s", result_str);
#endif

//	memcpy(launch_params, best_config, 6 * sizeof(int));

#if VERBOSE
  printf("----------------------------------------------\n");
#endif
}

///////////////////////////////////////
///////////////////////////////////////
