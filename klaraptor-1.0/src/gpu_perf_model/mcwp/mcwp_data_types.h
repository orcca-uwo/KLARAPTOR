/*!
 \file mcwp_data_types.h
 \author Davood Mohajerani <dmohajer@uwo.ca>
 \author Linxiao Wang <lwang739@uwo.ca>
 \brief
 */

#ifndef MCWP_DATA_TYPES_H_
#define MCWP_DATA_TYPES_H_

#include "RatFunInterp/include/interpolator.h"
#include "kernel_mesh_points/kernel_mesh_points.h"

///////////////////////////////////////
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gmp.h>
#include <unistd.h>
#include <limits.h>
#include <stdint.h>
#include <omp.h>

///////////////////////////////////////
#define check_mcwp_error(err, func)do{\
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
typedef enum mcwp_case_enum_s {
	MCWP_CASE_UNDEFINED = -1,
	MCWP_CASE_CWP_EQ_MWP = 1,
	MCWP_CASE_CWP_GE_MWP = 2,
	MCWP_CASE_CWP_LT_MWP = 3
} mcwp_case_enum_t;

///////////////////////////////////////
#define MAX_KERNEL_NAME_LEN 128
///////////////////////////////////////

///////////////////////////////////////
//double eps =1000 * 1000 * 1000 * 1000.0;
double eps = 1000.0 * 1000.0 * 1000 * 1000; // * 1000.0 * 1000.0 * 1000.0;

#define INTERP_MAX_RESIDUAL (eps)
//double eps = 0.00001;
//double eps = 0.00001;
//1000 * 1000 * 1000 * 1000.0;
//double eps=(1<<40);
//double eps = 0;
//typedef unsigned long long int u64;

//const char * long_dashed_line = ""
//    "-------------------------------------"
//    "--------------------------------------"
//    "\n";
//const char * short_dashed_line = "--------------------------------------\n";
//
///////////////////////////////////////
#define MAX_EXAMPLE_NAME_SIZE  128
#define DEFAULT_N_VAR 3
/////////////////////////////////////////
#ifndef DEGREE_BOUND
#define DEGREE_BOUND 5
#endif

///////////////////////////////////////
//percentage of error tolerance abs (evaluation-emulation)
//#define EVALUATION_TOLERANCE (20)

///////////////////////////////////////
#define LEN_KERNEL_NAME MAX_KERNEL_NAME_LEN

///////////////////////////////////////
#ifndef VERBOSE
#define VERBOSE 0
#endif

///////////////////////////////////////
//// this leads to a memory related crash in "mcwp/rational_program.c" template in function "device_params_init_from_const_polys".
#ifndef ALLOW_OMP_LOOPS
#define ALLOW_OMP_LOOPS 0
#endif

///////////////////////////////////////
typedef struct {
	char *name;
	int idx;
} item_to_idx_dict;

///////////////////////////////////////

typedef struct mcwp_result_params_t {
	int shared_mem_bytes_total;
	int mcwp_case;
	float occupancy;
	int n, b0, b1;

	mpq_t Comp_insts;
	mpq_t Uncoal_Mem_insts;
	mpq_t Coal_Mem_insts;
	mpq_t Synch_insts;
	mpq_t Coal_per_mw;
	mpq_t Uncoal_per_mw;
	mpq_t Active_blocks_per_SM;

	mpq_t Threads_per_block;
	mpq_t Blocks;
	mpq_t Active_warps_per_block;
	mpq_t Active_warps_per_SM;
	mpq_t CWP;
	mpq_t MWP;
	mpq_t MWP_peak_BW;
	mpq_t MWP_Without_BW_full;
	mpq_t CWP_full;
	mpq_t Mem_LD;

	mpq_t Rep;
	mpq_t Rep_without_B;
	mpq_t Exec_cycles_app;
	mpq_t Exec_cycles_per_thread;
	int is_best_for_same_n;

} mcwp_result_params_t;
///////////////////////////////////////

///////////////////////////////////////
typedef enum kernel_type_enum {
	OneDimKernel, TwoDimKernel, ThreeDimKernel
} kernel_type_enum_t;

///////////////////////////////////////
//TODO: change enum names + define as a type.
typedef enum poly_params_status {
	INIT, UNCHECKED, DONE, SUCCESSFUL_INTERP, FAILED_INTERP
} poly_params_status_t;

///////////////////////////////////////
///////////////////////////////////////

#define ARCH_PARAM_ENUM_SIZE 5

const char * arch_param_names_dict[] = { "MAX_BLOCKS_PER_SM",
		"MAX_WARPS_PER_SM", "MAX_BLOCK_SIZE", "MAX_REGISTERS_PER_BLOCK",
		"MAX_SHARED_MEM_PER_BLOCK" };

typedef enum arch_param_enum_s {
	ARCH_PARAM_MAX_BLOCKS_PER_SM,
	ARCH_PARAM_MAX_WARPS_PER_SM,
	ARCH_PARAM_MAX_BLOCK_SIZE,
	ARCH_PARAM_MAX_REGISTERS_PER_BLOCK,
	ARCH_PARAM_MAX_SHARED_MEM_PER_BLOCK
} arch_param_t;

///////////////////////////////////////
///////////////////////////////////////

#define DEVICE_PARAMS_ENUM_SIZE 8
const char * device_param_names_dict[] = { "Issue_cycles", "Mem_bandwidth",
		"Mem_LD", "Departure_del_coal", "Active_SMs", "Freq",
		"Load_bytes_per_warp", "Compute_Capability" };

//ALL device params are use for DP interpolation.
typedef enum device_param_names_enum {
	DEVICE_PARAM_Issue_cycles = 0,
	DEVICE_PARAM_Mem_bandwidth,
	DEVICE_PARAM_Mem_LD,
	DEVICE_PARAM_Departure_del_coal,
	DEVICE_PARAM_Active_SMs,
	DEVICE_PARAM_Freq,
	DEVICE_PARAM_Load_bytes_per_warp,
	DEVICE_PARAM_Compute_Capability
} device_param_names_t;

///////////////////////////////////////
///////////////////////////////////////

#define PROFILING_PARAMS_ENUM_SIZE 17
const char* profiling_param_names_dict[] = { "Total_insts", "Comp_insts",
		"Mem_insts", "dynamic_Mem_LD", "Active_blocks_per_SM", "Blocks",
		"Threads_per_block", "n_warps", "Active_warps_per_SM", "active_cycles",
		"grid_dim_x", "grid_dim_y", "grid_dim_z", "block_dim_x", "block_dim_y",
		"block_dim_z", "shared_mem_bytes_dynamic" };

/* The following params are not NEEDED for DP interpolation:
 * - Threads_per_block (this will be set per point in dp),
 * - active_cycles,
 * - grid_dim_x/y/z,
 * - block_dim_x/y/z,
 */
typedef enum profiling_param_names_enum {
	PROFILING_PARAM_13_Total_insts = 0,
	PROFILING_PARAM_14_Comp_insts,
	PROFILING_PARAM_15_Mem_insts,
	PROFILING_PARAM_dynamic_Mem_LD,
	PROFILING_PARAM_Active_blocks_per_SM,
	PROFILING_PARAM_Blocks,
	PROFILING_PARAM_Threads_per_block,
	PROFILING_PARAM_n_warps,
	PROFILING_PARAM_Active_warps_per_SM,
	PROFILING_PARAM_active_cycles,

	PROFILING_PARAM_grid_dim_x,
	PROFILING_PARAM_grid_dim_y,
	PROFILING_PARAM_grid_dim_z,
	PROFILING_PARAM_block_dim_x,
	PROFILING_PARAM_block_dim_y,
	PROFILING_PARAM_block_dim_z,
	PROFILING_PARAM_shared_mem_bytes_dynamic
} profiling_param_names_t;

///////////////////////////////////////
///////////////////////////////////////

#define PTX_PARAMS_ENUM_SIZE 2
const char * ptx_param_names_dict[] = { "shared_mem_bytes_static", "registers" };
//Both ptx params are used for DP interpolation.
typedef enum ptx_param_names_enum {
	PTX_PARAM_shared_mem_bytes_static = 0, PTX_PARAM_registers = 1,
} ptx_param_names_t;

///////////////////////////////////////
///////////////////////////////////////

#define RATIONAL_PROGRAM_PARAMS_ENUM_SIZE 12
const char * rational_program_param_names_dict[] = { "MWP_Without_BW_full",
		"MWP_Without_BW", "MWP_BW_per_warp", "MWP_peak_BW", "MWP", "Mem_cycles",
		"Comp_cycles", "CWP_full", "CWP", "Exec_cycles_app",
		"shared_mem_bytes_total", "occupancy" };

//only shared_mem_bytes_total is used for DP interpolation.
typedef enum rational_program_param_names_enum {
	RATIONAL_PROGRAM_PARAM_MWP_Without_BW_full = 0,
	RATIONAL_PROGRAM_PARAM_MWP_Without_BW,
	RATIONAL_PROGRAM_PARAM_MWP_BW_per_warp,
	RATIONAL_PROGRAM_PARAM_MWP_peak_BW,
	RATIONAL_PROGRAM_PARAM_MWP,
	RATIONAL_PROGRAM_PARAM_Mem_cycles,
	RATIONAL_PROGRAM_PARAM_Comp_cycles,
	RATIONAL_PROGRAM_PARAM_CWP_full,
	RATIONAL_PROGRAM_PARAM_CWP,
	RATIONAL_PROGRAM_PARAM_Exec_cycles_app,
	RATIONAL_PROGRAM_PARAM_shared_mem_bytes_total,
	RATIONAL_PROGRAM_PARAM_occupancy,
} rational_program_param_names_t;

///////////////////////////////////////
///////////////////////////////////////

const char * param_list_type_names_dict[] = { "DEVICE", "PROFILING", "PTX",
		"RATIONAL_PROGRAM" };

///////////////////////////////////////
typedef enum param_list_type_enum {
	PARAM_LIST_TYPE_DEVICE = 0,
	PARAM_LIST_TYPE_PROFILING = 1,
	PARAM_LIST_TYPE_PTX = 2,
	PARAM_LIST_TYPE_RATIONAL_PROGRAM = 3,
} param_list_type_t;
///////////////////////////////////////
///////////////////////////////////////
typedef enum param_list_size_enum {
	PARAM_LIST_SIZE_DEVICE = DEVICE_PARAMS_ENUM_SIZE,
	PARAM_LIST_SIZE_PROFILING = PROFILING_PARAMS_ENUM_SIZE,
	PARAM_LIST_SIZE_PTX = PTX_PARAMS_ENUM_SIZE,
	PARAM_LIST_SIZE_RATIONAL_PROGRAM = RATIONAL_PROGRAM_PARAMS_ENUM_SIZE
} param_list_size_t;
///////////////////////////////////////
///////////////////////////////////////
const char* param_list_value_type_names_dict[] = { "INTEGER", "MPQ_T", "STR" };

///////////////////////////////////////

typedef enum param_list_value_type_enum {
	PARAM_LIST_VALUE_TYPE_INTEGER,
	PARAM_LIST_VALUE_TYPE_MPQ_T,
	PARAM_LIST_VALUE_TYPE_STR
} param_list_value_type_t;

///////////////////////////////////////
///////////////////////////////////////

//all param list values should be of type mpq_t.
typedef struct param_list_s {
	int size;
	param_list_type_t type;
//  param_list_value_type_t value_type;
//  int *integer_values;
	mpq_t * mpq_values;
	char ** dict;
} param_list_t;

///////////////////////////////////////

#define MCWP_ERROR_TOLERANCE_PERCENTAGE 20
#define MCWP_ERROR_TOLERANCE_PERCENTAGE_MIN 5

///////////////////////////////////////

typedef struct symbol_info_s {
	char **syms;
	int n_symbols;
} symbol_info_t;

///////////////////////////////////////

//an interpolation entity is set for each kernel.
//an interpolation data is set for each kernel.
///////////////////////////////////////
/** for each interpolation entity, we need the following:
 * - the ptr to param list (ptr)
 * - param name alias (int)
 */
typedef struct interpolation_entity_s {
	int param_name_alias;
	char * param_name;
	param_list_t * param_list_ptr;
} interpolation_entity_t;

/**
 * - now, a table to keep track of interpolation entities.
 * - interpolation entity table should be allocated per kernel.
 */
typedef struct interpolation_entity_table_s {
	int capacity; //maximum pre-allocated size.
	int size; //current size, initially zero.
	interpolation_entity_t * entity_list;
} interpolation_entity_table_t;

///////////////////////////////////////
/**
 * necessary data for performing interpolation on each entity:
 * - interpolator (ptr)
 * - altarr for num poly (ptr)
 * - altarr for denom poly (ptr)
 * - status (int)
 *
 * NOTE: interpolation data should be allocated per entity and for each kernel.
 */
typedef struct interpolation_data_s {
	/////////////////////////////
	Interpolator_t* interpolator;
	AltArr_t* num_poly;
	AltArr_t* denom_poly;
	poly_params_status_t status;
	/////////////////////////////
} interpolation_data_t;

///////////////////////////////////////

typedef struct {
	int nvar;
	int* degree_bounds;
	int* denomdegree_bounds;
	Interpolator_t* interp[3];
	AltArr_t* poly[3];
	AltArr_t* denompoly[3];
	enum poly_params_status status[3];

	////////////////////////////////////////////
	interpolation_entity_table_t interp_entity_table;
	interpolation_entity_table_t interp_entity_table_for_static_params;
	interpolation_data_t * interpolation_data_list;
	interpolation_data_t * interpolation_data_list_for_static_params;
	////////////////////////////////////////////
	//allocsize=1;
	//nvar=0;
	//make const poly aa

	point_set_t * point_set;
	int mesh_size;
	int n_points_in_mesh;
//  int is_zero_poly[3];
	int mcwp_result_table_idx;
	mcwp_result_params_t * mcwp_result_table;
	char * kernel_name;
	char ** symbols;
	symbol_info_t * symbol_info;

	param_list_t *ptx_params; //one per kernel
	param_list_t *profiling_params_list; //one per mesh point
	param_list_t *rational_program_params_list; //one per mesh point
//  triple* visitd_points_list;
	int* visitd_points_map;

} poly_params;

///////////////////////////////////////

typedef struct profiling_info_s {
	int n_points_in_mesh;
	int lower_bound;
	int upper_bound;

	size_t buffer_size;
	size_t buffer_size_per_kernel;
	size_t current_buffer_idx;
	size_t profiler_str_len;
	size_t buffer_chunk_size;

	point_set_t * point_set;
	kernel_type_enum_t kernel_type;

	/**
	 * keep a map of valid mesh points that have will be profiled.
	 * 0: not_profiled (default).
	 * 1: profiled.
	 */
	int * visited_points_map;
	char profiler_str[PATH_MAX];
	char *buffer;
	char **profiling_result_table;
} profiling_info_t;

///////////////////////////////////////

typedef struct data_collection_info_s {
	poly_params* kernel_poly_params;
	int n_kernels;
	char ** kernel_names;
	char* mem_inst_trace_str;

	int n_params_to_interpolate;

	int nvar;
	char* device_params_path;
	char device_name[1024];

	param_list_t * device_params;
	profiling_info_t * profiling_info;

} data_collection_info_t;

///////////////////////////////////////
typedef enum progress_type_enum {
	PROGRESS_BAR_START, PROGRESS_BAR_CONTINUE, PROGRESS_BAR_STOP
} progress_type_t;
///////////////////////////////////////
void progress_bar(int* current_size, progress_type_t status) {
	setbuf(stdout, NULL);
	if (status == PROGRESS_BAR_START) {
		printf("[");
		(*current_size)++;
		return;
	}
	if (status == PROGRESS_BAR_STOP) {
		printf("]\n");
		(*current_size)++;
		return;
	}
	if (status == PROGRESS_BAR_CONTINUE) {
		printf("#");
		(*current_size)++;
	}

	if ((*current_size & (63)) == 0) {
		printf("\n ");
		return;
	}

}
///////////////////////////////////////

#endif
