/*!
 \file kernel_invoker_with_profiler.cpp
 \author Davood Mohajerani <dmohajer@uwo.ca>
 \brief
 */

///////////////////////////////////////
//#include <bits/stdint-uintn.h>
#include <cuda.h>
#include <cuda_runtime_api.h>
#include <cupti_callbacks.h>
#include <cupti_driver_cbid.h>
#include <cupti_events.h>
#include <cupti_result.h>
#include <cupti_runtime_cbid.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <vector_types.h>
//#include <cstring>

#include "kernel_invoker.h"
//#include "ptx_lookup_common.h"

///////////////////////////////////////////////////////////

/* Set 50 characters as the size of each output line. This
 * will reduce runtime as less chars will be printed.
 */
#define MAX_OUTPUT_LINE_WIDTH 60
#define EVENT_LIST_SIZE 5
#define MAX_EVENT_LIST_SIZE 24
#define DEFAULT_DEVICE_NUM 0
#define LAUNCH_HISTORY_CAPACITY 10
#define KERNEL_PROFILER_N_REPEAT 4
#define KERNEL_PROFILER_N_REPEAT_LOG2 2

#ifndef VERBOSE
#define VERBOSE 1
#endif
///////////////////////////////////////////////////////////
const int event_list_size = EVENT_LIST_SIZE;

//// original list for future reference (112 events).
//const char * event_list[MAX_EVENT_LIST_SIZE] =
//  { "tex0_cache_sector_queries", "tex1_cache_sector_queries",
//      "tex2_cache_sector_queries", "tex3_cache_sector_queries",
//      "tex0_cache_sector_misses", "tex1_cache_sector_misses",
//      "tex2_cache_sector_misses", "tex3_cache_sector_misses",
//      "elapsed_cycles_sm", "fb_subp0_read_sectors", "fb_subp1_read_sectors",
//      "fb_subp0_write_sectors", "fb_subp1_write_sectors",
//      "l2_subp0_write_sector_misses", "l2_subp1_write_sector_misses",
//      "l2_subp2_write_sector_misses", "l2_subp3_write_sector_misses",
//      "l2_subp0_read_sector_misses", "l2_subp1_read_sector_misses",
//      "l2_subp2_read_sector_misses", "l2_subp3_read_sector_misses",
//      "l2_subp0_write_l1_sector_queries", "l2_subp1_write_l1_sector_queries",
//      "l2_subp2_write_l1_sector_queries", "l2_subp3_write_l1_sector_queries",
//      "l2_subp0_read_l1_sector_queries", "l2_subp1_read_l1_sector_queries",
//      "l2_subp2_read_l1_sector_queries", "l2_subp3_read_l1_sector_queries",
//      "l2_subp0_read_l1_hit_sectors", "l2_subp1_read_l1_hit_sectors",
//      "l2_subp2_read_l1_hit_sectors", "l2_subp3_read_l1_hit_sectors",
//      "l2_subp0_read_tex_sector_queries", "l2_subp1_read_tex_sector_queries",
//      "l2_subp2_read_tex_sector_queries", "l2_subp3_read_tex_sector_queries",
//      "l2_subp0_read_tex_hit_sectors", "l2_subp1_read_tex_hit_sectors",
//      "l2_subp2_read_tex_hit_sectors", "l2_subp3_read_tex_hit_sectors",
//      "l2_subp0_read_sysmem_sector_queries",
//      "l2_subp1_read_sysmem_sector_queries",
//      "l2_subp2_read_sysmem_sector_queries",
//      "l2_subp3_read_sysmem_sector_queries",
//      "l2_subp0_write_sysmem_sector_queries",
//      "l2_subp1_write_sysmem_sector_queries",
//      "l2_subp2_write_sysmem_sector_queries",
//      "l2_subp3_write_sysmem_sector_queries",
//      "l2_subp0_total_read_sector_queries",
//      "l2_subp1_total_read_sector_queries",
//      "l2_subp2_total_read_sector_queries",
//      "l2_subp3_total_read_sector_queries",
//      "l2_subp0_total_write_sector_queries",
//      "l2_subp1_total_write_sector_queries",
//      "l2_subp2_total_write_sector_queries",
//      "l2_subp3_total_write_sector_queries", "gld_inst_8bit", "gld_inst_16bit",
//      "gld_inst_32bit", "gld_inst_64bit", "gld_inst_128bit", "gst_inst_8bit",
//      "gst_inst_16bit", "gst_inst_32bit", "gst_inst_64bit", "gst_inst_128bit",
//      "prof_trigger_00", "prof_trigger_01", "prof_trigger_02",
//      "prof_trigger_03", "prof_trigger_04", "prof_trigger_05",
//      "prof_trigger_06", "prof_trigger_07", "warps_launched",
//      "threads_launched", "inst_issued1", "inst_issued2", "inst_executed",
//      "shared_load", "shared_store", "local_load", "local_store", "gld_request",
//      "gst_request", "atom_count", "atom_cas_count", "gred_count", "branch",
//      "divergent_branch", "active_cycles", "active_warps", "sm_cta_launched",
//      "local_load_transactions", "local_store_transactions",
//      "l1_shared_load_transactions", "l1_shared_store_transactions",
//      "__l1_global_load_transactions", "__l1_global_store_transactions",
//      "l1_local_load_hit", "l1_local_load_miss", "l1_local_store_hit",
//      "l1_local_store_miss", "l1_global_load_hit", "l1_global_load_miss",
//      "uncached_global_load_transaction", "global_store_transaction",
//      "shared_load_replay", "shared_store_replay",
//      "global_ld_mem_divergence_replays", "global_st_mem_divergence_replays" };

/////// events required for MCWP
// all the following parameters should be collected for ONE thread!
///////
//[13.Total_insts: ] = inst_executed/warps_launched (i.e. number of instructions per warp)
//[14.Comp_insts: ] = #13-#15
//[15.Mem_insts: ] = (gld_request+gst_request)/warps_launched (i.e. number of memory requests per warp)
//[16.Uncoal_Mem_insts: ] = 0; //consider all accesses of the same cost, i.e., there are no coalesced accesses.
//[17.Coal_Mem_insts: ] = Mem_insts;
//[18.Synch_insts: ] = ? 0 for the moment. TODO: what can be done?
//[19.Coal_per_mw: ] = 1;
//[20.Uncoal_per_mw: ] = 0;
//[n_active_blocks_per_sm: ] = sm_cta_launched.
//[n_blocks: ] = get it from gx.gy.gz (product of grid dimension).
//[n_warps: ] = warps_launched

///////////////////////////////////////////////////////////
#define METRIC_LIST_SIZE 10

const int metric_list_size = METRIC_LIST_SIZE;

const char * metric_name_list[METRIC_LIST_SIZE] = { "Total_insts", "Comp_insts",
		"Mem_insts", "dynamic_Mem_LD", "Active_blocks_per_SM", "Blocks",
		"Threads_per_block", "n_warps", "Active_warps_per_SM", "active_cycles" };

/////////////////////////////
/////////////////////////////

////WARNING: the following enum must be in the same order as the above list!
enum metric_name_list_enum {
	metric_Total_insts,
	metric_Comp_insts,
	metric_Mem_insts,
	metric_dynamic_Mem_LD,
	metric_Active_blocks_per_SM,
	metric_Blocks,
	metric_Threads_per_block,
	metric_n_warps,
	metric_Active_warps_per_SM,
	metric_active_cycles,
};

///////////////////////////////////////////////////////////
#define KERNEL_LAUNCH_PARAMS_ENUM_SIZE 7
const char* kernel_launch_param_names_dict[] = { "grid_dim_x", "grid_dim_y",
		"grid_dim_z", "block_dim_x", "block_dim_y", "block_dim_z",
		"shared_mem_bytes_dynamic" };

typedef enum kernel_launch_param_names_enum {
	KERNEL_LAUNCH_PARAM_grid_dim_x = 0,
	KERNEL_LAUNCH_PARAM_grid_dim_y = 1,
	KERNEL_LAUNCH_PARAM_grid_dim_z = 2,
	KERNEL_LAUNCH_PARAM_block_dim_x = 3,
	KERNEL_LAUNCH_PARAM_block_dim_y = 4,
	KERNEL_LAUNCH_PARAM_block_dim_z = 5,
	KERNEL_LAUNCH_PARAM_shared_mem_bytes_dynamic = 6,
} kernel_launch_param_names_t;

///////////////////////////////////////////////////////////

const char * event_list_cc_30[MAX_EVENT_LIST_SIZE] = { "active_cycles",
		"warps_launched", "inst_executed", "gld_request", "gst_request" };


///////////////////////////////////////////////////////////

const char * event_list_cc_50[MAX_EVENT_LIST_SIZE] = { "active_cycles",
		"warps_launched", "inst_executed", "global_load", "global_store" };

///////////////////////////////////////////////////////////

const char * event_list_cc_60[MAX_EVENT_LIST_SIZE] = { "active_cycles",
		"warps_launched", "inst_executed", "global_load", "global_store" };

/////////////////////////////
const char **event_list;
/////////////////////////////

//ToDo: make the switch-case more precise.
int set_event_list(int cc_minor, int cc_major) {
	printf("[@%s][setting CC to %d%d]\n", __func__, cc_major, cc_minor);
	switch (cc_major) {
	case 3:
		event_list = event_list_cc_30;
		break;

	case 5:
		event_list = event_list_cc_50;
		break;

	case 6:
		event_list = event_list_cc_60;
		break;
	}
	return EXIT_SUCCESS;
}
/////////////////////////////
#define N_SUPPORTED_SM 12
typedef enum sm_enum_s {
	sm_20,
	sm_21,
	sm_30,
	sm_32,
	sm_35,
	sm_37,
	sm_50,
	sm_52,
	sm_53,
	sm_60,
	sm_61,
	sm_62
} sm_enum_t;

// int max_registers_per_block_list[N_SUPPORTED_SM]={32768, 32768
// ,65536,65536,65536,65536,65536,65536,32768,65536,65536,65536};
int max_blocks_per_sm_list[N_SUPPORTED_SM] = { 8, 8, 16, 16, 16, 16, 32, 32, 32,
		32, 32, 32 };
// int max_register_per_thread_list[N_SUPPORTED_SM]={63,63,63,255,255,255,255,255,255,255,255,255};
int max_warps_per_sm_list[N_SUPPORTED_SM] = { 48, 48, 64, 64, 64, 64, 64, 64,
		64, 64, 64, 128 };
// int max_shared_mem_per_block=49152
// int max_block_size=1024

/////////////////////////////
////WARNING: the following enum must be in the same order as the above list!
enum event_list_enum {
	event_active_cycles,
	event_warps_launched,
	event_inst_executed,
	event_gld_request,
	event_gst_request
};

///////////////////////////////////////////////////////////
#define CHECK_CU_ERROR(err, cufunc)                                     \
    if (err != CUDA_SUCCESS)                                              \
      {                                                                   \
	printf ("%s:%d: error %d for CUDA Driver API function '%s'\n",    \
		__FILE__, __LINE__, err, cufunc);                         \
	exit(-1);                                                         \
      }

#if VERBOSE
#define CHECK_CUPTI_ERROR(err, cuptifunc)                               \
    if (err != CUPTI_SUCCESS)                                             \
      {                                                                   \
	const char *errstr;                                               \
	cuptiGetResultString(err, &errstr);                               \
	printf ("%s:%d:Error %s for CUPTI API function '%s'.\n",          \
		__FILE__, __LINE__, errstr, cuptifunc);                   \
	exit(-1);                                                         \
      }\
else\
{\
    printf("-- [%s]...[PASS]\n", cuptifunc);\
}\

#else

#define CHECK_CUPTI_ERROR(err, cuptifunc)                               \
    if (err != CUPTI_SUCCESS)                                             \
      {                                                                   \
	const char *errstr;                                               \
	cuptiGetResultString(err, &errstr);                               \
	printf ("%s:%d:Error %s for CUPTI API function '%s'.\n",          \
		__FILE__, __LINE__, errstr, cuptifunc);                   \
	exit(-1);                                                         \
      }\

#endif

///////////////////////////////////////////////////////////

//runtime API callback id's
const int cbid_list_runtime_size = 4;
const CUpti_runtime_api_trace_cbid cbid_list_runtime[4] = {
		CUPTI_RUNTIME_TRACE_CBID_cudaLaunch_v3020,
		CUPTI_RUNTIME_TRACE_CBID_cudaLaunch_ptsz_v7000,
		CUPTI_RUNTIME_TRACE_CBID_cudaLaunchKernel_v7000,
		CUPTI_RUNTIME_TRACE_CBID_cudaLaunchKernel_ptsz_v7000 };

///////////////////////////////////////////////////////////

//driver API callback id's
const int cbid_list_driver_size = 3;
const CUpti_driver_api_trace_cbid cbid_list_driver[3] = {
		CUPTI_DRIVER_TRACE_CBID_cuLaunch,
		CUPTI_DRIVER_TRACE_CBID_cuLaunchKernel,
		CUPTI_DRIVER_TRACE_CBID_cuLaunchKernel_ptsz };

///////////////////////////////////////////////////////////

typedef struct device_data {
	CUcontext context;
	CUdevice dev;

	int computeCapabilityMajor;
	int computeCapabilityMinor;
	int deviceNum;
	int deviceCount;
	char deviceName[32];
	int n_sm;
	int max_block_per_sm;
	int max_warp_per_sm;
} device_data_t;

///////////////////////////////////////////////////////////
typedef struct launch_history_s {
	uint64_t * values;
	int size;
	int capacity;
} launch_history_t;

///////////////////////////////////////////////////////////

typedef struct klaraptor_profiler_data {
//  int init;
	status_t status;
	status_t got_profiled;
	uint32_t profile_all;
	CUpti_SubscriberHandle subscriber;
//  device_data_t dd;
	CUpti_EventGroupSets *group_sets;
	CUpti_EventGroup current_group; //used for enabling one group per replay.
//  CUpti_EventID *event_id_list_unsorted;
	CUpti_EventID *event_id_list_sorted;
	uint64_t *event_values_list_sorted;
	uint64_t *event_values_list_unsorted;
	int n_events_counted;
	int kernel_idx;
	int launch_params[8];
	uint64_t metrics[METRIC_LIST_SIZE];
	int valid_profiling;
	launch_history_t launch_history;
} klaraptor_profiler_data_t;

///////////////////////////////////////////////////////////

typedef struct klaraptor_profiler_session_data {
	int n_kernels;
	int event_list_size;
	klaraptor_profiler_data_t * global_klaraptor_profiler_data;
	device_data_t dd;
	int n_event_id_list_bytes;
	CUpti_EventID *event_id_list_unsorted;
//  CUpti_EventID *event_id_list_sorted;
} klaraptor_profiler_session_data_t;

//static int global_klaraptor_profiler_data_init = PROFILER_DISABLED;
//klaraptor_profiler_data_t *global_klaraptor_profiler_data;

//static int global_session_data_init = PROFILER_DISABLED;
klaraptor_profiler_session_data_t *global_session_data;
status_t global_session_data_status = DISABLED;

///////////////////////////////////////////////////////////

typedef struct profiler_kernel_handle {
	//cuda module and function that will be assigned to the kernel.
	CUmodule cudaModule;
	CUfunction function;

	//kernel launch parameters.
	dim3 block_dim, grid_dim;
	int n_dynamic_shared_mem_bytes;
	int stream_idx;

	//for storing kernel entry name.
	char entry_name[MAX_KERNEL_NAME_LEN];

	//kernel parameters.
	void **KernelParams;

	//string referring to the kernel text, either as PTX or SASS.
	unsigned char *ptxstr;

} profiler_kernel_handle_t;

///////////////////////////////////////////////////////////
int launch_history_init(launch_history_t * h, int capacity) {
	h->capacity = capacity;
	h->size = 0;
	size_t values_size = capacity * sizeof(uint64_t);
	h->values = (uint64_t*) malloc(values_size);
	memset(h->values, 0x00, values_size);
	return EXIT_SUCCESS;
}

///////////////////////////////////////////////////////////
int launch_history_clear(launch_history_t * h) {
	h->capacity = 0;
	h->size = 0;
	free(h->values);
	return EXIT_SUCCESS;
}

///////////////////////////////////////////////////////////
int launch_history_double_capacity(launch_history_t * h) {
	int expand_factor = 2;
	size_t current_size = (h->capacity * sizeof(uint64_t));
	uint64_t* tmp = (uint64_t*) malloc(current_size);
	memcpy(tmp, h->values, current_size);
	free(h->values);
	size_t new_size = (current_size * expand_factor);
	h->values = (uint64_t*) malloc(new_size);
	memset(h->values, 0x00, new_size);
	memcpy(h->values, tmp, current_size);
	free(tmp);
	h->capacity *= expand_factor;
	return EXIT_SUCCESS;
}

///////////////////////////////////////////////////////////
int launch_history_add_entry(launch_history_t * h, const int *launch_params) {
	if (h->size == h->capacity) {
		launch_history_double_capacity(h);
	}

	int bx, by, bz;
	bx = launch_params[KERNEL_LAUNCH_PARAM_block_dim_x];
	by = launch_params[KERNEL_LAUNCH_PARAM_block_dim_y];
	bz = launch_params[KERNEL_LAUNCH_PARAM_block_dim_z];

	uint64_t val = 0;
	val += bx;
	val <<= 10;
	val += by;
	val <<= 10;
	val += bz;
	h->values[h->size] = val;
	h->size++;
	return EXIT_SUCCESS;
}

///////////////////////////////////////////////////////////
int launch_history_check_entry(launch_history_t * h,
		const int * launch_params) {
	int bx, by, bz;
	bx = launch_params[KERNEL_LAUNCH_PARAM_block_dim_x];
	by = launch_params[KERNEL_LAUNCH_PARAM_block_dim_y];
	bz = launch_params[KERNEL_LAUNCH_PARAM_block_dim_z];
	uint64_t val = 0;
	val += bx;
	val <<= 10;
	val += by;
	val <<= 10;
	val += bz;

	for (int i = 0; i < h->size; i++) {
		if (val == h->values[i])
			return EXIT_SUCCESS;
	}
	return EXIT_FAILURE;
}

///////////////////////////////////////////////////////////

/* read the measured values for all the events in the group;
 * then, add them to counters in pi.
 */
int get_value_from_group(klaraptor_profiler_data_t * pi,
		CUpti_EventGroup group) {
#if VERBOSE
	printf("--[NEW GROUP VISITED]...\n");
#endif
	uint64_t numInstances = 0;
	uint64_t numTotalInstances = 0;
	uint64_t *values = NULL;
	size_t valueSize = sizeof(numInstances);
	size_t num_instances_size = sizeof(numInstances);

	uint32_t num_events = 0;
	size_t num_events_size = sizeof(num_events);
	CUptiResult cuptiErr;

	////getting number of instances
	cuptiErr = cuptiEventGroupGetAttribute(group,
			CUPTI_EVENT_GROUP_ATTR_INSTANCE_COUNT, &num_instances_size,
			&numInstances);
	CHECK_CUPTI_ERROR(cuptiErr, "cuptiEventGroupGetAttribute");

	CUpti_EventDomainID groupDomain;
	size_t group_domain_size = sizeof(groupDomain);
	cuptiErr = cuptiEventGroupGetAttribute(group,
			CUPTI_EVENT_GROUP_ATTR_EVENT_DOMAIN_ID, &group_domain_size,
			&groupDomain);
	CHECK_CUPTI_ERROR(cuptiErr, "cuptiEventGroupGetAttribute");

	cuptiErr = cuptiDeviceGetEventDomainAttribute(global_session_data->dd.dev,
			groupDomain, CUPTI_EVENT_DOMAIN_ATTR_TOTAL_INSTANCE_COUNT,
			&num_instances_size, &numTotalInstances);
	CHECK_CUPTI_ERROR(cuptiErr, "cuptiEventGroupGetAttribute");

	cuptiErr = cuptiEventGroupGetAttribute(group,
			CUPTI_EVENT_GROUP_ATTR_NUM_EVENTS, &valueSize, &num_events);
	CHECK_CUPTI_ERROR(cuptiErr, "cuptiEventGroupGetAttribute");

#if VERBOSE
	printf("--[num instances=%lu]\n", numInstances);
	printf("--[num events per group=%d]\n", num_events);
#endif

	size_t event_ids_size = (sizeof(CUpti_EventID) * num_events);
	CUpti_EventID * event_ids = (CUpti_EventID *) malloc(event_ids_size);
	cuptiErr = cuptiEventGroupGetAttribute(group, CUPTI_EVENT_GROUP_ATTR_EVENTS,
			&event_ids_size, event_ids);
	CHECK_CUPTI_ERROR(cuptiErr, "cuptiEventGroupGetAttribute");

	size_t num_events_read = 0;
	size_t bytesRead = sizeof(uint64_t) * numInstances * num_events;
//  printf("bytes_read = %d\n", bytesRead);
	values = (uint64_t *) malloc(bytesRead);
	if (values == NULL) {
		printf("%s:%d: Out of memory\n", __FILE__, __LINE__);
		exit(-1);
	}
	memset(values, 0x00, bytesRead);
	cuptiErr = cuptiEventGroupReadAllEvents(group, CUPTI_EVENT_READ_FLAG_NONE,
			&bytesRead, values, &event_ids_size, event_ids, &num_events_read);
	CHECK_CUPTI_ERROR(cuptiErr, "cuptiEventGroupReadAllEvents");

//  klaraptor_profiler_data_t * pi = global_klaraptor_profiler_data;

	for (int e = 0; e < num_events_read; e++) {
		uint64_t max_val_among_instances;
		uint64_t sum_among_instances = 0;
//      uint64_t min_val_among_instances;
		for (int i = 0; i < numInstances; i++) {

			sum_among_instances += values[i * num_events + e];
			if (i == 0) {
				max_val_among_instances = values[i * num_events + e];
				continue;
			}
#if VERBOSE
			printf("--[eventId=%u][event=%d][inst=%d][%lu]\n", event_ids[e], e,
					i, values[i * num_events + e]);
#endif

			if (values[i * num_events + e] > max_val_among_instances) {
				max_val_among_instances = values[i * num_events + e];
			}

//	  if (values[i * num_events + e] < min_val_among_instances)
//	    {
//	      min_val_among_instances = values[i * num_events + e];
//	    }
//	      else if (values[i * num_events + e])
//		printf("REPLACING MAX WITH NON ZERO VAL!\n");
		}
//      printf ("event_cound=%d\n", pi->n_events_counted);
		pi->event_id_list_sorted[pi->n_events_counted] = event_ids[e];
		pi->event_values_list_sorted[pi->n_events_counted] +=
				((sum_among_instances * numTotalInstances) / numInstances);
//	  max_val_among_instances;
		pi->n_events_counted++;
#if VERBOSE
		printf("----------------------------\n");
#endif
	}

	free(values);
	free(event_ids);

	return EXIT_SUCCESS;
}

///////////////////////////////////////////////////////////

//callback function that will enable/disable the collection of events.
void CUPTIAPI
getEventValueCallback(void *userdata, CUpti_CallbackDomain domain,
		CUpti_CallbackId cbid, const CUpti_CallbackData *cbInfo) {
	CUptiResult cuptiErr;
//  RuntimeApiTrace_t *traceData = (RuntimeApiTrace_t*) userdata;
//  size_t bytesRead;

	klaraptor_profiler_data_t * pi = (klaraptor_profiler_data_t*) userdata;

//  printf ("hang out in callback!\n");

	int valid_cbid_visited = 0;

//  for (int i = 0; i < cbid_list_runtime_size; i++)
//    {
//      if (cbid == cbid_list_runtime[i])
//	{
//	  printf ("valid cbid for runtime [%d] \n", i);
//	  valid_cbid_visited = 1;
//	  break;
//	}
//    }

	for (int i = 0; i < cbid_list_driver_size; i++) {
		if (cbid == cbid_list_driver[i]) {
			valid_cbid_visited = 1;
#if VERBOSE
			printf("valid cbid for driver [code=%d] \n", i);
#endif
			break;
		}
	}

// This callback is enabled only for launch so we shouldn't see anything else.
//  if ((cbid != CUPTI_RUNTIME_TRACE_CBID_cudaLaunch_v3020)
//      && (cbid != CUPTI_RUNTIME_TRACE_CBID_cudaLaunchKernel_v7000))
	if (valid_cbid_visited == 0) {
		printf("%s:%d: unexpected cbid %d\n", __FILE__, __LINE__, cbid);
		exit(EXIT_FAILURE);
	}

//  printf ("hang out in callback!\n");
	if (cbInfo->callbackSite == CUPTI_API_ENTER) {
#if VERBOSE
		printf("[CUPTI_API_ENTER][%s]\n", cbInfo->functionName);
#endif
		//critical sync;
		cudaDeviceSynchronize();

		cuptiErr = cuptiSetEventCollectionMode(cbInfo->context,
				CUPTI_EVENT_COLLECTION_MODE_KERNEL);
		CHECK_CUPTI_ERROR(cuptiErr, "cuptiSetEventCollectionMode");

		cuptiErr = cuptiEventGroupEnable(pi->current_group);
		CHECK_CUPTI_ERROR(cuptiErr, "cuptiEventGroupEnable");
#if VERBOSE
		printf("--[counting started]...\n");
#endif
	}

	if (cbInfo->callbackSite == CUPTI_API_EXIT) {
#if VERBOSE
		printf("THIS IS CUPTI API EXIT!\n");
#endif
		//critical sync;

		get_value_from_group(pi, pi->current_group);
		cudaDeviceSynchronize();
		cuptiErr = cuptiEventGroupDisable(pi->current_group);
		CHECK_CUPTI_ERROR(cuptiErr, "cuptiEventGroupEnable");
	}
}

/////////////////////////////////////////////////////////

void kernel_profiler_set_unsorted_event_values() {

	//nothing to show!
	if (global_session_data_status == DISABLED) {
#if VERBOSE
		printf("@[%s]"
				"[global_session_data_status == DISABLED] ... ignoring \n",
				__func__);
#endif
		return;
	}

	klaraptor_profiler_session_data_t * si = global_session_data;

	//for all kernels in si.
	for (int k = 0; k < si->n_kernels; k++) {
		klaraptor_profiler_data_t * pi = &si->global_klaraptor_profiler_data[k];
		if (pi->got_profiled == DISABLED)
			continue;
//// extra check.
//      if (pi->kernel_idx!=k)
//	{
//	  printf("ERROR: idx!=k\n");
//	  exit(EXIT_FAILURE);
//	}

		/* find the corresponding event_id for the element "ordinal" in
		 * the event list. Event values are collected in the order that
		 * is that determined by CUPTI calls. Therefore, we need to retrieve
		 * the collected values for the order that we in the "event_list".
		 * The order of each event in the "event_list" is denoted as its "ordinal".
		 */

		for (int ordinal = 0; ordinal < si->event_list_size; ordinal++) {
			/* getting id for the ordinal. Note that the unsorted id list
			 * belongs to si rather than pi!
			 */
			CUpti_EventID unsorted_id = si->event_id_list_unsorted[ordinal];
			int i = -1;
			for (i = 0; i < pi->n_events_counted; i++) {
				CUpti_EventID sorted_id = pi->event_id_list_sorted[i];
				if (sorted_id == unsorted_id) {
//		  printf("FOUND!\n");
					break;
				}
			}
			if (i == -1) {
				printf(
						"[@%s][ERROR: corresponding idx in the sorted list was not found!]\n",
						__func__);
				exit(EXIT_FAILURE);
//	      continue;
			}
//	  printf (
//	      "setting event value for ordinal =[%d], unsorted_id=[%u], value=[%llu]\n",
//	      ordinal, unsorted_id, pi->event_values_list_sorted[i]);
			pi->event_values_list_unsorted[ordinal] =
					pi->event_values_list_sorted[i];
		}
	}
}

/////////////////////////////////////////////////////////

void kernel_profiler_print_event_values() {
	//nothing to show!
	if (global_session_data_status == DISABLED) {
#if VERBOSE
		printf("@[%s][global_session_data_status == DISABLED] ... ignoring \n",
				__func__);
#endif
		return;
	}

	klaraptor_profiler_session_data_t * si = global_session_data;

	//4 extra lines of info per kernel, each "output_line_size" chars wide.
	const int output_line_size = MAX_OUTPUT_LINE_WIDTH;
	size_t output_buffer_size = (si->n_kernels * (4 + si->event_list_size)
			* output_line_size);
	char * output_buffer = (char*) malloc(output_buffer_size);
	memset(output_buffer, 0x00, output_buffer_size);
	size_t output_buffer_current_size = 0;

	for (int k = 0; k < si->n_kernels; k++) {
		klaraptor_profiler_data_t * pi = &si->global_klaraptor_profiler_data[k];
		if (pi->got_profiled == DISABLED)
			continue;
//// extra check.
//      if (pi->kernel_idx!=k)
//	{
//	  printf("ERROR: idx!=k\n");
//	  exit(EXIT_FAILURE);
//	}

		output_buffer_current_size += sprintf(
				output_buffer + output_buffer_current_size,
				"[KLARAPTOR_PROFILER_RESULT_BEGIN]\n");
		output_buffer_current_size += sprintf(
				output_buffer + output_buffer_current_size,
				"[kernel_idx=%d][%s]\n", k,
				ptx_lookup_list->list[k].kernel_name);
		for (int ordinal = 0; ordinal < si->event_list_size; ordinal++) {
//	  printf ("[%-32s | ordinal=%2d | id=%2d | value=%8llu]\n",
//		  event_list[ordinal], ordinal, unsorted_id,
//		  pi->event_values_list_sorted[i]);
//	  output_buffer_current_size += sprintf (
//	      output_buffer + output_buffer_current_size, "[%-32s]:[%20llu]\n",
//	      event_list[ordinal], pi->event_values_list_sorted[i]);
			output_buffer_current_size += sprintf(
					output_buffer + output_buffer_current_size, "[%s]:[%llu]\n",
					event_list[ordinal],
					pi->event_values_list_unsorted[ordinal]);
		}
		output_buffer_current_size += sprintf(
				output_buffer + output_buffer_current_size,
				"[KLARAPTOR_PROFILER_RESULT_END]\n");
		output_buffer_current_size += sprintf(
				output_buffer + output_buffer_current_size,
				"---------------------------------------------\n");
	}

//  printf("%s", output_buffer);
	fwrite(output_buffer, output_buffer_current_size, 1, stdout);
	free(output_buffer);
//  printf (
//      "==========================================================================\n");
}

/////////////////////////////////////////////////////////

int div_and_ceil(uint64_t * result, uint64_t num, uint64_t denom) {
	if (denom == 0) {
		printf("[@%s][denom==0]\n", __func__);
		return (EXIT_FAILURE);
	}
	*result = ((num + denom - 1) / (denom));
	return EXIT_SUCCESS;
}

/////////////////////////////////////////////////////////

int div_and_floor(uint64_t * result, uint64_t num, uint64_t denom) {
	if (denom == 0) {
		printf("[@%s][denom==0]\n", __func__);
		return (EXIT_FAILURE);
	}
	*result = ((num) / (denom));
	return EXIT_SUCCESS;
}

/////////////////////////////////////////////////////////
/**
 * c.C + m.dynamic_Mem_LD = active_cycles
 * c=#comp_inst;
 * m=#mem_inst;
 * C=latency of one comp inst (1 cc);
 * dynamic_Mem_LD = (active_cycles - #comp_inst)/(#mem_inst);
 */

int compute_dynamic_Mem_LD(uint64_t * dynamic_mem_ld, uint64_t inst_executed,
		uint64_t n_active_warps_per_sm, uint64_t n_warps, uint64_t n_blocks,
		uint64_t comp_inst, uint64_t mem_inst, uint64_t active_cycles,
		int warp_per_sm_reached_max) {

	*dynamic_mem_ld = 400;
	return EXIT_SUCCESS;
//  uint64_t base = 1;
////  div_and_ceil (&active_cycles, active_cycles, n_warps);
////  float cpi = (active_cycles * 1.0) / (inst_executed * 1.0);

//  uint64_t mem_to_comp_ratio;
//  div_and_ceil (&mem_to_comp_ratio, comp_inst, mem_inst);

//  uint64_t cpi;
//  div_and_ceil (&cpi, active_cycles, inst_executed);
//  printf ("cpi=%d\n", mem_to_comp_ratio);
//  float comp_inst_cpi = 1.0;

	int n_sm = global_session_data->dd.n_sm;
	uint64_t result = 0;
	uint64_t cc_per_sm;
	uint64_t n_rounds = 1;
//  div_and_ceil (&n_rounds, n_warps, n_active_warps_per_sm);
//  div_and_ceil (&cc_per_sm, active_cycles, n_blocks);

	if (warp_per_sm_reached_max) {
		div_and_ceil(&n_rounds, n_warps, n_sm * n_active_warps_per_sm);
	}
	div_and_ceil(&cc_per_sm, active_cycles, n_warps);

	uint64_t mem_inst_exec = (inst_executed - comp_inst);
	mem_inst_exec /= n_warps;

	div_and_ceil(&result, ((active_cycles / n_warps) - comp_inst),
			mem_inst_exec);

	printf("mem_inst_exec=%llu\n", mem_inst_exec);
	printf("n_warps=%llu\n", n_warps);
//  div_and_ceil (&result, (active_cycles - (uint64_t) (base * comp_inst)),
//		(mem_to_comp_ratio * mem_inst));
	*dynamic_mem_ld = result;

	*dynamic_mem_ld = 500;
	return EXIT_SUCCESS;
}

/////////////////////////////////////////////////////////

/////// events required for MCWP
// all the following parameters should be collected for ONE thread!
///////
//[13.Total_insts: ] = inst_executed/warps_launched (i.e. number of instructions per warp)
//[14.Comp_insts: ] = #13-#15
//[15.Mem_insts: ] = (gld_8/16/32/64/128+gst_8/16/32/64/128)/threads_launched (i.e. number of memory requests per warp)
//[16.Uncoal_Mem_insts: ] = 0; //consider all accesses of the same cost, i.e., there are no coalesced accesses.
//[17.Coal_Mem_insts: ] = Mem_insts;
//[18.Synch_insts: ] = ? 0 for the moment. TODO: what can be done?
//[19.Coal_per_mw: ] = 1;
//[20.Uncoal_per_mw: ] = 0;
//[n_active_blocks_per_sm: ] = sm_cta_launched/n_blocks
//[n_blocks: ] = get it from gx.gy.gz (product of grid dimension).
//[n_warps: ] = warps_launched

int kernel_profiler_compute_mcwp_kernel_metric_values(
		klaraptor_profiler_data_t* pi, int n_sm) {

	klaraptor_profiler_session_data_t *si = global_session_data;

	int gx = pi->launch_params[KERNEL_LAUNCH_PARAM_grid_dim_x];
	int gy = pi->launch_params[KERNEL_LAUNCH_PARAM_grid_dim_y];
	int gz = pi->launch_params[KERNEL_LAUNCH_PARAM_grid_dim_z];
	int n_blocks = gx * gy * gz;
	int n_blocks_per_sm = (n_blocks + n_sm - 1) / n_sm;
	//has reached the max.
	if (n_blocks_per_sm > si->dd.max_block_per_sm) {
		n_blocks_per_sm = si->dd.max_block_per_sm;
	}

	int bx = pi->launch_params[KERNEL_LAUNCH_PARAM_block_dim_x];
	int by = pi->launch_params[KERNEL_LAUNCH_PARAM_block_dim_y];
	int bz = pi->launch_params[KERNEL_LAUNCH_PARAM_block_dim_z];
	uint64_t block_size = bx * by * bz;

	/* the profiling is accurate only for saturated device with
	 * full-size warps (i.e. 32 threads per warp, not less).
	 */
//  if ((n_blocks < n_sm) || (block_size < 32))
//    {
////#if VERBOSE
//      printf ("[@%s][ERROR: DEVICE IS NOT SATURATED! INVALID PROFILING"
//	      " (n_blocks < n_sm)||(block_size<32)]\n",
//	      __func__);
////#endif
//      for (int i = 0; i < metric_list_size; i++)
//	pi->metrics[i] = 0;
//      pi->valid_profiling = DISABLED;
//      return EXIT_SUCCESS;
//    }
	uint64_t* event_values = pi->event_values_list_unsorted;
	uint64_t* metric_list = pi->metrics;

	int stat;

//  uint64_t n_threads = event_values[event_threads_launched];

//  printf("event_values[event_warps_launched] * 32=%llu\n", event_values[event_warps_launched] * 32);
//  printf("event_values[event_threads_launched]=%llu\n", event_values[event_threads_launched]);

//  if ((event_values[event_threads_launched])>n_threads)
//    n_threads=(event_values[event_threads_launched]);

//  uint64_t total_insts = event_values[event_inst_executed];
//  stat = div_and_ceil (&metric_list[metric_Total_insts],
//		       event_values[event_inst_executed],
//		       event_values[event_warps_launched]);

	for (int i = 0; i < event_list_size; i++)
		event_values[i] >>= KERNEL_PROFILER_N_REPEAT_LOG2;
//    event_values[i]/=KERNEL_PROFILER_N_REPEAT;

	stat = div_and_ceil(&metric_list[metric_Total_insts],
			event_values[event_inst_executed], 1);

	if (stat != EXIT_SUCCESS) {
		printf("[ERROR:\n"
				"  metric_list[metric_Total_insts]\n"
				"  event_values[event_inst_executed]=%d\n"
				"  event_values[event_warps_launched]=%d\n",
				event_values[event_inst_executed],
				event_values[event_warps_launched]);
		return EXIT_FAILURE;
	}

//  uint64_t total_gst_inst, total_gld_inst;
//  total_gst_inst = event_values[event_gst_inst_128bit]
//      + event_values[event_gst_inst_64bit] + event_values[event_gst_inst_32bit]
//      + event_values[event_gst_inst_16bit] + event_values[event_gst_inst_8bit];
//
//  total_gld_inst = event_values[event_gld_inst_128bit]
//      + event_values[event_gld_inst_64bit] + event_values[event_gld_inst_32bit]
//      + event_values[event_gld_inst_16bit] + event_values[event_gld_inst_8bit];

//TODO: there is a problem here.
//  stat = div_and_ceil (
//      &metric_list[metric_Mem_insts],
//      (event_values[event_gld_request] + event_values[event_gst_request]),
//      event_values[event_warps_launched]);

	stat = div_and_ceil(&metric_list[metric_Mem_insts],
			(event_values[event_gld_request] + event_values[event_gst_request]),
			1);

	if (stat != EXIT_SUCCESS) {
		return EXIT_FAILURE;
	}

//  printf ("metric_list[metric_Mem_insts] =%llu\n",
//	  metric_list[metric_Mem_insts]);

	metric_list[metric_Comp_insts] = metric_list[metric_Total_insts]
			- metric_list[metric_Mem_insts];
//  metric_list[metric_Uncoal_Mem_insts] = 0;
//  metric_list[metric_Coal_Mem_insts] = metric_list[metric_Mem_insts];
//  metric_list[metric_Synch_insts] = 0;
//  metric_list[metric_Coal_per_mw] = 1;
//  metric_list[metric_Uncoal_per_mw] = 0;

	metric_list[metric_Blocks] = n_blocks;
	metric_list[metric_Threads_per_block] = block_size;
	metric_list[metric_n_warps] = event_values[event_warps_launched];

//  if (n_blocks_per_sm < (event_values[event_sm_cta_launched] * n_sm))
//    {
//      printf ("SMALLER!\n");
//      printf ("FAIL!\n");
//      exit (EXIT_FAILURE);
//    }
	metric_list[metric_Active_blocks_per_SM] = n_blocks_per_sm;
//      (event_values[event_sm_cta_launched] + metric_list[metric_n_blocks] - 1)
//	  / metric_list[metric_n_blocks];

//  printf ("metric_list[metric_n_active_blocks_per_sm]=%llu\n",
//	  metric_list[metric_n_active_blocks_per_sm]);

	int n_warp_per_sm = (event_values[event_warps_launched] + n_sm - 1)
			/ (n_sm);

	int warp_per_sm_reached_max = 0;
	if (n_warp_per_sm > si->dd.max_warp_per_sm) {
		n_warp_per_sm = si->dd.max_warp_per_sm;
		warp_per_sm_reached_max = 1;
	}
	metric_list[metric_Active_warps_per_SM] = n_warp_per_sm;

	////to be considered as impact of arithmetic latency. Issue cycles change per kernel call.
//  uint64_t cpi_x1000 = (1000000.0 * float (event_values[event_active_cycles]))
//      / (1000.0 * float (event_values[event_inst_executed]));
//  metric_list[metric_cpi_x1000] = cpi_x1000;
	metric_list[metric_active_cycles] = event_values[event_active_cycles];

//  stat = compute_dynamic_Mem_LD (&metric_list[metric_dynamic_Mem_LD],
//				 event_values[event_inst_executed],
//				 metric_list[metric_Active_warps_per_SM],
//				 event_values[event_warps_launched],
//				 metric_list[metric_Blocks],
//				 metric_list[metric_Comp_insts],
//				 metric_list[metric_Mem_insts],
//				 metric_list[metric_active_cycles],
//				 warp_per_sm_reached_max);

	metric_list[metric_dynamic_Mem_LD] = 400;

	if (stat != EXIT_SUCCESS)
		return EXIT_FAILURE;

	pi->valid_profiling = ENABLED;
	return EXIT_SUCCESS;
}

/////////////////////////////////////////////////////////

void kernel_profiler_compute_metric_values() {

	//nothing to show!
	if (global_session_data_status == DISABLED) {
#if VERBOSE
		printf("@[%s][global_session_data_status == DISABLED] ... ignoring \n",
				__func__);
#endif
		return;
	}

	klaraptor_profiler_session_data_t * si = global_session_data;

	for (int k = 0; k < si->n_kernels; k++) {
		klaraptor_profiler_data_t * pi = &si->global_klaraptor_profiler_data[k];
		if (pi->got_profiled == DISABLED)
			continue;
//// extra check.
//      if (pi->kernel_idx!=k)
//	{
//	  printf("ERROR: idx!=k\n");
//	  exit(EXIT_FAILURE);
//	}
		int stat = kernel_profiler_compute_mcwp_kernel_metric_values(pi,
				si->dd.n_sm);
		if (stat != EXIT_SUCCESS) {
			printf(
					"[@%s][ERROR:kernel_profiler_compute_mcwp_kernel_metric_values has FAILED!]...ABORT\n");
//	  exit (EXIT_FAILURE);
		}

	}
}

///////////////////////////////////////////////////////////

void kernel_profiler_print_metric_values() {

	//nothing to show!
	if (global_session_data_status == DISABLED) {
#if VERBOSE
		printf("@[%s][global_session_data_status == DISABLED] ... ignoring \n",
				__func__);
#endif
		return;
	}

	klaraptor_profiler_session_data_t * si = global_session_data;

	//6 extra lines of info per kernel, each "output_line_size" chars wide.
	const int output_line_size = MAX_OUTPUT_LINE_WIDTH;
//  size_t output_buffer_size = (si->n_kernels * (6 + si->event_list_size)
//      * output_line_size);

	size_t output_buffer_size = (si->n_kernels
			* (6 + METRIC_LIST_SIZE + KERNEL_LAUNCH_PARAMS_ENUM_SIZE)
			* output_line_size);

	char * output_buffer = (char*) malloc(output_buffer_size);
	memset(output_buffer, 0x00, output_buffer_size);
	size_t output_buffer_current_size = 0;

	for (int k = 0; k < si->n_kernels; k++) {
		klaraptor_profiler_data_t * pi = &si->global_klaraptor_profiler_data[k];
		if (pi->got_profiled == DISABLED)
			continue;
//// extra check.
//      if (pi->kernel_idx!=k)
//	{
//	  printf("ERROR: idx!=k\n");
//	  exit(EXIT_FAILURE);
//	}
//      printf (
//	  "==========================================================================\n");

		output_buffer_current_size += sprintf(
				output_buffer + output_buffer_current_size,
				"[KLARAPTOR_PROFILER_RESULT_BEGIN]\n");

		output_buffer_current_size += sprintf(
				output_buffer + output_buffer_current_size,
				"[kernel_name: %s]\n", ptx_lookup_list->list[k].kernel_name);

		output_buffer_current_size += sprintf(
				output_buffer + output_buffer_current_size,
				"[kernel_idx: %d]\n", k);

		output_buffer_current_size += sprintf(
				output_buffer + output_buffer_current_size,
				"[valid_profiling: %d]\n", pi->valid_profiling);

//      for (int metric_id = metric_Total_insts; metric_id <= metric_active_cycles;
//	  metric_id++)
		for (int metric_id = 0; metric_id < metric_list_size; metric_id++) {
//	  printf ("[%-32s | ordinal=%2d | id=%2d | value=%8llu]\n",
//		  event_list[ordinal], ordinal, unsorted_id,
//		  pi->event_values_list_sorted[i]);
//	  output_buffer_current_size += sprintf (
//	      output_buffer + output_buffer_current_size, "[%-32s]:[%20llu]\n",
//	      event_list[ordinal], pi->event_values_list_sorted[i]);
			output_buffer_current_size += sprintf(
					output_buffer + output_buffer_current_size, "[%s: %llu]\n",
					metric_name_list[metric_id], pi->metrics[metric_id]);
		}

		for (int launch_param_id = KERNEL_LAUNCH_PARAM_grid_dim_x;
				launch_param_id <= KERNEL_LAUNCH_PARAM_shared_mem_bytes_dynamic;
				launch_param_id++) {
			output_buffer_current_size += sprintf(
					output_buffer + output_buffer_current_size, "[%s: %llu]\n",
					kernel_launch_param_names_dict[launch_param_id],
					pi->launch_params[launch_param_id]);
		}

		output_buffer_current_size += sprintf(
				output_buffer + output_buffer_current_size,
				"[KLARAPTOR_PROFILER_RESULT_END]\n");
		output_buffer_current_size += sprintf(
				output_buffer + output_buffer_current_size,
				"---------------------------------------------\n");
	}

//  printf("%s", output_buffer);
	fwrite(output_buffer, output_buffer_current_size, 1, stdout);
	free(output_buffer);
//  printf (
//      "==========================================================================\n");
}

///////////////////////////////////////////////////////////
int set_device_limits(device_data_t * dd) {

	int cc = dd->computeCapabilityMinor + 10 * dd->computeCapabilityMajor;
	sm_enum_t sm_cc;
	switch (cc) {
	case 20:
		sm_cc = sm_20;
		break;
	case 21:
		sm_cc = sm_21;
		break;
	case 30:
		sm_cc = sm_30;
		break;
	case 32:
		sm_cc = sm_32;
		break;
	case 35:
		sm_cc = sm_35;
		break;
	case 37:
		sm_cc = sm_37;
		break;
	case 50:
		sm_cc = sm_50;
		break;
	case 52:
		sm_cc = sm_52;
		break;
	case 53:
		sm_cc = sm_53;
		break;
	case 60:
		sm_cc = sm_60;
		break;
	case 61:
		sm_cc = sm_61;
		break;
	case 62:
		sm_cc = sm_62;
		break;
	}
	dd->max_block_per_sm = max_blocks_per_sm_list[sm_cc];
	dd->max_warp_per_sm = max_warps_per_sm_list[sm_cc];
	return EXIT_SUCCESS;
}

///////////////////////////////////////////////////////////

int init_device_data(device_data_t * dd) {
	dd->context = global_kernel_invoker_data.context;
	dd->dev = global_kernel_invoker_data.device;
	dd->computeCapabilityMajor = 0;
	dd->computeCapabilityMinor = 0;
	dd->deviceNum = DEFAULT_DEVICE_NUM;
	dd->deviceCount = 0;

	CUresult err;
//  err = cuInit (0);
//  CHECK_CU_ERROR(err, "cuInit");

	err = cuDeviceGetCount(&(dd->deviceCount));
	CHECK_CU_ERROR(err, "cuDeviceGetCount");

	if (dd->deviceCount == 0) {
		printf("[ERROR: Found no CUDA supporting device!]...\n");
		return EXIT_FAILURE;
	}

#if VERBOSE
	printf("CUDA Device Number: %d\n", dd->deviceNum);
#endif

//  err = cuDeviceGet (&(dd->dev), dd->deviceNum);
//  CHECK_CU_ERROR(err, "cuDeviceGet");

	err = cuDeviceGetName(dd->deviceName, 32, dd->dev);
	CHECK_CU_ERROR(err, "cuDeviceGetName");

#if VERBOSE
	printf("CUDA Device Name: %s\n", dd->deviceName);
#endif

//// get CC
	err = cuDeviceGetAttribute(&dd->computeCapabilityMajor,
			CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MAJOR, dd->dev);
	CHECK_CU_ERROR(err, "cuDeviceGetAttribute");

	err = cuDeviceGetAttribute(&dd->computeCapabilityMinor,
			CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MINOR, dd->dev);
	CHECK_CU_ERROR(err, "cuDeviceGetAttribute");

//  err = cuDeviceGetAttribute (&dd->n_sm,
//  			      CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MINOR,
//  			      dd->dev);
//    CHECK_CU_ERROR(err, "cuDeviceGetAttribute");

	err = cuDeviceGetAttribute(&dd->n_sm,
			CU_DEVICE_ATTRIBUTE_MULTIPROCESSOR_COUNT, dd->deviceNum);
	CHECK_CU_ERROR(err, "cuDeviceGetAttribute");

//  err = cuCtxCreate (&dd->context, 0, dd->dev);
//  CHECK_CU_ERROR(err, "cuCtxCreate");

	if (set_device_limits(dd) != EXIT_SUCCESS)
		return EXIT_FAILURE;

	return EXIT_SUCCESS;
}

///////////////////////////////////////////////////////////

int kernel_profiler_kernel_init(
		klaraptor_profiler_data_t* global_klaraptor_profiler_data) {
	if (global_klaraptor_profiler_data->status == DISABLED) {
#if VERBOSE
		printf(
				"@[%s][global_klaraptor_profiler_data->status == DISABLED] ... FATAL!\n",
				__func__);
#endif
		exit(EXIT_FAILURE);
	}

	CUptiResult cuptiErr;

//reference to the global struct that holds profiling info.
	klaraptor_profiler_session_data * si = global_session_data;
	klaraptor_profiler_data_t * pi = global_klaraptor_profiler_data;

	/////////////////////////////////////////////////////
	pi->profile_all = 1;

	/////////////////////////////////////////////////////
	//// this array will be used to form a correspondence
	//// between sorted event-id's and their measured values.
	pi->n_events_counted = 0;

	pi->event_id_list_sorted = (CUpti_EventID*) malloc(
			si->n_event_id_list_bytes);

	int n_even_value_list_bytes = si->event_list_size * sizeof(uint64_t);
	pi->event_values_list_sorted = (uint64_t*) malloc(n_even_value_list_bytes);
	pi->event_values_list_unsorted = (uint64_t*) malloc(
			n_even_value_list_bytes);
	memset(pi->event_values_list_sorted, 0x00, n_even_value_list_bytes);
	memset(pi->event_values_list_sorted, 0x00, n_even_value_list_bytes);

	/////////////////////////////////////
	pi->group_sets = NULL;
	cuptiErr = cuptiEventGroupSetsCreate(si->dd.context,
			si->n_event_id_list_bytes, si->event_id_list_unsorted,
			&pi->group_sets);
	CHECK_CUPTI_ERROR(cuptiErr, "cuptiEventGroupSetsCreate");

	////setting necessary attribute to profile all domain instances.
	for (int s = 0; s < pi->group_sets->numSets; s++) {
		for (int i = 0; i < pi->group_sets->sets[s].numEventGroups; i++) {
			cuptiErr = cuptiEventGroupSetAttribute(
					pi->group_sets->sets[s].eventGroups[i],
					CUPTI_EVENT_GROUP_ATTR_PROFILE_ALL_DOMAIN_INSTANCES,
					sizeof(pi->profile_all), &pi->profile_all);
			CHECK_CUPTI_ERROR(cuptiErr, "cuptiEventGroupSetAttribute");
		}
	}

	/////////////////////////////////////////////////////
	////setting the values of metrics to zero.
	memset(pi->metrics, 0x00, METRIC_LIST_SIZE * sizeof(uint64_t));

	/////////////////////////////////////////////////////
	launch_history_init(&pi->launch_history, LAUNCH_HISTORY_CAPACITY);

	/////////////////////////////////////////////////////
	pi->status = ENABLED;
	return EXIT_SUCCESS;
}

///////////////////////////////////////////////////////////

int kernel_profiler_kernel_clear(
		klaraptor_profiler_data_t* global_klaraptor_profiler_data) {

	if (global_klaraptor_profiler_data->status == DISABLED) {
#if VERBOSE
		printf(
				"@[%s][global_klaraptor_profiler_data->status == DISABLED] ... ignoring \n",
				__func__);
#endif
		return EXIT_SUCCESS;
	}

	CUptiResult cuptiErr;

////reference to the global struct that holds profiling info.
	klaraptor_profiler_data_t * pi = global_klaraptor_profiler_data;

	free(pi->event_id_list_sorted);
	free(pi->event_values_list_sorted);
	free(pi->event_values_list_unsorted);
	/////////////////////////////////////////////////////
	launch_history_clear(&pi->launch_history);

	/////////////////////////////////////
	cuptiErr = cuptiEventGroupSetsDestroy(pi->group_sets);
	CHECK_CUPTI_ERROR(cuptiErr, "cuptiEventGroupSetsCreate");

	/////////////////////////////////////////////////////
	global_klaraptor_profiler_data->status = DISABLED;

	return EXIT_SUCCESS;
}

///////////////////////////////////////////////////////////

int kernel_profiler_kernel_subscribe(
		klaraptor_profiler_data_t* global_klaraptor_profiler_data) {

	if (global_klaraptor_profiler_data->status == DISABLED) {
#if VERBOSE
		printf(
				"@[%s][global_klaraptor_profiler_data->status == DISABLED] ... FATAL\n");
#endif
		exit(EXIT_FAILURE);
		return EXIT_FAILURE;
	}
#if VERBOSE
	printf("@[%s][kernel_idx=%d]\n", __func__,
			global_klaraptor_profiler_data->kernel_idx);
#endif
	CUptiResult cuptiErr;

//reference to the global struct that holds profiling info.
	klaraptor_profiler_session_data * si = global_session_data;
	klaraptor_profiler_data_t * pi = global_klaraptor_profiler_data;

	/////////////////////////////////////////////////////
	//// should be initialized before enabling callbacks!
	cuptiErr = cuptiSubscribe(&(pi->subscriber),
			(CUpti_CallbackFunc) getEventValueCallback, pi);

	CHECK_CUPTI_ERROR(cuptiErr, "cuptiSubscribe");
	/////////////////////////////////////////////////////

	for (int i = 0; i < cbid_list_driver_size; i++) {
		cuptiErr = cuptiEnableCallback(1, pi->subscriber,
				CUPTI_CB_DOMAIN_DRIVER_API, cbid_list_driver[i]);
		CHECK_CUPTI_ERROR(cuptiErr, "cuptiEnableCallback");
	}

	for (int i = 0; i < cbid_list_runtime_size; i++) {
		cuptiErr = cuptiEnableCallback(1, pi->subscriber,
				CUPTI_CB_DOMAIN_RUNTIME_API, cbid_list_runtime[i]);
		CHECK_CUPTI_ERROR(cuptiErr, "cuptiEnableCallback");
	}
	/////////////////////////////////////////////////////

	return EXIT_SUCCESS;
}

///////////////////////////////////////////////////////////

int kernel_profiler_kernel_unsubscribe(
		klaraptor_profiler_data_t* global_klaraptor_profiler_data) {

	if (global_klaraptor_profiler_data->status == DISABLED) {
#if VERBOSE
		printf(
				"@[%s][global_klaraptor_profiler_data->status == DISABLED] ... FATAL!\n");
#endif
		exit(EXIT_FAILURE);
		return EXIT_FAILURE;
	}

	CUptiResult cuptiErr;

//reference to the global struct that holds profiling info.
	klaraptor_profiler_session_data * si = global_session_data;
	klaraptor_profiler_data_t * pi = global_klaraptor_profiler_data;
	/////////////////////////////////////////////////////
	//// should be initialized before enabling callbacks!
	cuptiErr = cuptiUnsubscribe(pi->subscriber);
	CHECK_CUPTI_ERROR(cuptiErr, "cuptiUnsubscribe");

	return EXIT_SUCCESS;
}

///////////////////////////////////////////////////////////

int kernel_profiler_session_init() {
	if (global_session_data_status == ENABLED) {
#if VERBOSE
		printf("@[%s][global_session_data_status == ENABLED] ... ignoring \n",
				__func__);
#endif
		return EXIT_SUCCESS;
	}

	/////////////////////////////////////////////////////
	//// check for cu_init
	//// calling  "kernel_invoker_check_cu_init" is CRITICAL!
	checkErrors(kernel_invoker_check_cu_init());

	/////////////////////////////////////////////////////
	CUptiResult cuptiErr;
	////// find number of kernels in session.
	int n_kernels = ptx_lookup_list->size;

	/////////////////////////////////////////////////////
	//malloc global profiler session information data structure.
	global_session_data = (klaraptor_profiler_session_data_t*) malloc(
			sizeof(klaraptor_profiler_session_data_t));
	klaraptor_profiler_session_data_t * si = global_session_data;

	/////////////////////////////////////////////////////
	////init dd for session
	init_device_data(&si->dd);

	/////////////////////////////////////////////////////
	set_event_list(si->dd.computeCapabilityMinor,
			si->dd.computeCapabilityMajor);

	/////////////////////////////////////////////////////
	////// set number of kernels in session.
	si->n_kernels = n_kernels;

	/////////////////////////////////////////////////////
	//// this array will be used to form a correspondence
	//// between sorted event-id's and their measured values.

	si->event_list_size = event_list_size;
	size_t n_event_id_list_bytes = si->event_list_size * sizeof(CUpti_EventID);
	si->n_event_id_list_bytes = n_event_id_list_bytes;
	si->event_id_list_unsorted = (CUpti_EventID*) malloc(n_event_id_list_bytes);

	/////////////////////////////////////
	//getting event id's from their names.
	for (int i = 0; i < si->event_list_size; i++) {
		cuptiErr = cuptiEventGetIdFromName(si->dd.dev, event_list[i],
				&si->event_id_list_unsorted[i]);
		if (cuptiErr != CUPTI_SUCCESS) {
			printf("Invalid eventName: %s\n", event_list[i]);
			//      return -1;
		}
		CHECK_CUPTI_ERROR(cuptiErr, "cuptiEventGetIdFromName");
	}

	si->global_klaraptor_profiler_data = (klaraptor_profiler_data_t*) malloc(
			si->n_kernels * sizeof(klaraptor_profiler_data_t));
	for (int k = 0; k < si->n_kernels; k++) {
//reference to the struct that holds profiling info.
		si->global_klaraptor_profiler_data[k].status = ENABLED;
		si->global_klaraptor_profiler_data[k].got_profiled = DISABLED;
		si->global_klaraptor_profiler_data[k].kernel_idx = k;
		kernel_profiler_kernel_init(&si->global_klaraptor_profiler_data[k]);
	}

	global_session_data_status = ENABLED;
	return EXIT_SUCCESS;
}

///////////////////////////////////////////////////////////

int kernel_profiler_session_clear() {
//  printf("@[%s]\n", __func__);
	if (global_session_data_status == DISABLED) {
#if VERBOSE
		printf("@[%s][global_session_data_status == DISABLED] ... ignoring\n",
				__func__);
#endif
		return EXIT_SUCCESS;
	}

	CUptiResult cuptiErr;
	klaraptor_profiler_session_data_t * si = global_session_data;

	for (int k = 0; k < si->n_kernels; k++) {
//      printf("@[%s][clearing kernel[%d]]\n", __func__, k);
		kernel_profiler_kernel_clear(&si->global_klaraptor_profiler_data[k]);
	}

	free(si->event_id_list_unsorted);
	free(si->global_klaraptor_profiler_data);
////reference to the global struct that holds profiling info.
//  klaraptor_profiler_data_t * pi = global_klaraptor_profiler_data;
//
//  free (pi->event_id_list_unsorted);
//  free (pi->event_id_list_sorted);
//  free (pi->event_values_list_sorted);
//  /////////////////////////////////////
//  cuptiErr = cuptiEventGroupSetsDestroy (pi->group_sets);
//  CHECK_CUPTI_ERROR(cuptiErr, "cuptiEventGroupSetsCreate");
//
//  /////////////////////////////////////////////////////
//  //// should be initialized before enabling callbacks!
//  cuptiErr = cuptiUnsubscribe (pi->subscriber);
//  CHECK_CUPTI_ERROR(cuptiErr, "cuptiSubscribe");
//
//  /////////////////////////////////////////////////////
//  //malloc global profiler information data structure.
//  free (global_klaraptor_profiler_data);
//  global_klaraptor_profiler_data_init = PROFILER_DISABLED;

	global_session_data_status = DISABLED;
	return EXIT_SUCCESS;
}

/////////////////////////////////////////////////////////

void start_profiling() {
//  klaraptor_profiler_data_t * pi = global_klaraptor_profiler_data;
#if VERBOSE
	printf("START PROFILING...\n");
#endif
//  start_profiling_init_step1 (NULL);
//  printf ("n_groups_remained=%d\n", pi->n_all_groups);

}

///////////////////////////////////////////////////////////

void stop_profiling() {
//  klaraptor_profiler_data_t * pi = global_klaraptor_profiler_data;
//  pi->n_all_groups--;
#if VERBOSE
	printf("END OF PROFILING\n");
#endif
	//  start_profiling_init_step1 (NULL);
//  printf ("n_groups_remained=%d\n", pi->n_all_groups);
//  goto start;
}

///////////////////////////////////////////////////////////

void set_kernel_launch_params(int* launch_params, dim3 gridDim, dim3 blockDim)
//, int n_dynamic_shared_mem_bytes)
		{
	launch_params[0] = gridDim.x;
	launch_params[1] = gridDim.y;
	launch_params[2] = gridDim.z;

	launch_params[3] = blockDim.x;
	launch_params[4] = blockDim.y;
	launch_params[5] = blockDim.z;
//	launch_params[6]=n_dynamic_shared_mem_bytes;
}

///////////////////////////////////////////////////////////

//this function will be called at the beginning of kernel_invoker.
int kernel_invoker_check_cu_init() {
	kernel_invoker_data_t * ptr = &global_kernel_invoker_data;
//  printf ("[%s][ptr->cu_init_done_STATUS=%d]\n", __func__,
//	  ptr->cu_init_done_status);

//at this point, device is not set yet.
	if (ptr->cu_init_done_status == DISABLED) {
// CUDA initialization
		checkCudaErrors(cuInit(0));
		CUresult get_device_result;
////get the device.
		get_device_result = (cuDeviceGet(&ptr->device, 0));
//      if (get_device_result == CUDA_SUCCESS)
//	{
//	  printf (
//	      "[DEVICE IS ALREADY SET, PROBABLY via RUNTIME API!]...skipping [cuInit] \n");
//	  //check if a ctx is already allocated.
//	  checkCudaErrors(cuCtxSetCurrent (&ptr->context));
//	  if (ptr->context!=NULL)
//	  checkCudaErrors(cuCtxDestroy_v2 (ptr->context));
//	}

//check if a ctx is already allocated.
//      checkCudaErrors(cuCtxGetCurrent (&ptr->context));

//if there is not context, the returned value will be null.
//      if (ptr->context == NULL)
		{
//	  printf ("context is NULL! ... creating context\n");

			////Create driver context
			checkCudaErrors(cuCtxCreate(&ptr->context, 0, ptr->device));
		}
//      else
//	{
//	  printf ("[CONTEXT IS ALREADY CREATED!]\n");
//	}

//// set the flag.
		ptr->cu_init_done_status = ENABLED;
//      printf ("[cu_init_done]->[%d]\n", ptr->cu_init_done);
	}
	return EXIT_SUCCESS;
}

///////////////////////////////////////////////////////////

//definition
void kernel_invoker_init(void) {
//  printf ("@[%s] ... with_profiler\n", __func__);

	ptx_lookup_manager_init();
	kernel_profiler_session_init();
}

///////////////////////////////////////////////////////////

//definition
void kernel_invoker_clear(void) {
//  printf ("@[%s]\n", __func__);

	kernel_profiler_set_unsorted_event_values();
//  kernel_profiler_print_event_values ();
	kernel_profiler_compute_metric_values();
	kernel_profiler_print_metric_values();

	ptx_lookup_manager_clear();

	kernel_invoker_data_t * ptr = &global_kernel_invoker_data;
	if (ptr->cu_init_done_status == ENABLED) {
//      checkCudaErrors(cuCtxGetCurrent (&ptr->context));
//      if (ptr->context != NULL)
//	{
//	  checkCudaErrors(cuCtxDestroy (ptr->context));
//	}
		ptr->cu_init_done_status = DISABLED;
	}

	kernel_profiler_session_clear();
}

///////////////////////////////////////////////////////////

int kernel_profiler_kernel_handle_init(char * kernel_name, int* launch_params,
		void **KernelParams, int kernel_idx,
		profiler_kernel_handle_t * handle) {

	handle->grid_dim.x = launch_params[0];
	handle->grid_dim.y = launch_params[1];
	handle->grid_dim.z = launch_params[2];
	handle->block_dim.x = launch_params[3];
	handle->block_dim.y = launch_params[4];
	handle->block_dim.z = launch_params[5];
	handle->n_dynamic_shared_mem_bytes = launch_params[6];
	handle->stream_idx = launch_params[7];

	ptx_lookup_manager_get_ptx_entry_name(kernel_idx, handle->entry_name);
	ptx_lookup_manager_get_ptx_str(kernel_idx, &handle->ptxstr);
	checkCudaErrors(cuModuleLoadData(&handle->cudaModule, (handle->ptxstr)));
	// Get kernel function
	checkCudaErrors(
			cuModuleGetFunction(&handle->function, handle->cudaModule,
					handle->entry_name));

	handle->KernelParams = KernelParams;

	return EXIT_SUCCESS;
}

///////////////////////////////////////////////////////////

int kernel_profiler_kernel_set_launch_params(int * launch_params,
		klaraptor_profiler_data_t * pi) {
	memcpy(pi->launch_params, launch_params, 8 * sizeof(int));
	return EXIT_SUCCESS;
}

///////////////////////////////////////////////////////////

int kernel_profiler_kernel_handle_clear(profiler_kernel_handle_t * handle) {
	checkCudaErrors(cuModuleUnload(handle->cudaModule));
	return EXIT_SUCCESS;
}

///////////////////////////////////////////////////////////

int kernel_invoker_with_profiler(profiler_kernel_handle_t * handle) {
//  printf ("calling [kernel_invoker_check_cu_init]\n");
//  checkErrors(kernel_invoker_check_cu_init ());

//  grid_dim.x = launch_params[0];
//  grid_dim.y = launch_params[1];
//  grid_dim.z = launch_params[2];
//  block_dim.x = launch_params[3];
//  block_dim.y = launch_params[4];
//  block_dim.z = launch_params[5];
//  n_dynamic_shared_mem_bytes = 0; //launch_params[6];

#if VERBOSE
	{
//		std::cout << short_ln(60);
		std::cout << "[kernel_name : " << kernel_name << "]\n";
	}
#endif
//  CUdevice device;
//  CUmodule cudaModule;
//  CUcontext context;
//  CUfunction function;
//	CUlinkState linker;
//	int devCount;

//  // CUDA initialization
//  checkCudaErrors(cuInit (0));
//	checkCudaErrors(cuDeviceGetCount(&devCount));
//  checkCudaErrors(cuDeviceGet (&device, 0));

//	char name[128];
//	checkCudaErrors(cuDeviceGetName(name, 128, device));
//	if (verbose)
//		std::cout << "Using CUDA Device [0]: " << name << "\n";

//	int devMajor, devMinor;
//	checkCudaErrors(cuDeviceComputeCapability(&devMajor, &devMinor, device));
//	if (verbose)
//		std::cout << "Device Compute Capability: " << devMajor << "."
//				<< devMinor << "\n";
//
//	if (devMajor < 2)
//	{
//		std::cerr << "ERROR: Device 0 is not SM 2.0 or greater\n";
//		return 1;
//	}

//#if VERBOSE
//	std::cout << short_ln(60);
//#endif

//  int kernel_idx;
//  exit(EXIT_FAILURE);
//  char kernel_name_ptx[MAX_KERNEL_NAME_LEN];
//  char kernel_name_ptxsass[MAX_KERNEL_NAME_LEN];
//  char entry_name[MAX_KERNEL_NAME_LEN];
//
////  sprintf (kernel_name_ptx, "%s.ptx", kernel_name);
////  sprintf (kernel_name_ptxsass, "%s.ptxsass", kernel_name);
//
////  printf ("getting kernel idx...\n");
////  ptx_lookup_manager_get_kernel_idx (kernel_name_ptx, &kernel_idx);
////  ptx_lookup_manager_get_ptx_str (kernel_name_ptx, &ptxstr);
//  ptx_lookup_manager_get_ptx_entry_name (kernel_idx, entry_name);
//
//  unsigned char *ptxstr;
//  ptx_lookup_manager_get_ptx_str (kernel_idx, &ptxstr);

//  printf ("entry-name=[%s]\n", entry_name);
//	std::string decorated_kernel_name = decorate_kernel_name(kernel_name);
//	std::string ptxstr = get_ptx_str(std::string(kernel_name_ptx));
//	std::string ptxsass_str = get_ptxsass_str(decorated_kernel_name+"sass");
//#if VERBOSE
//	std::cout << short_ln(60);
//#endif

//  kernel_invoker_data_t * ptr = &global_kernel_invoker_data;

//////Create driver context
//  checkCudaErrors(cuCtxCreate_v2 (&context, 0, device));
//  checkCudaErrors(cuCtxCreate_v2 (&context, 0, ptr->device));

//	std::cout<<"decorated kernel name = "<<decorated_kernel_name+"sass"<<"\n";
//	std::cout<<ptxsass_str.length()<<"\n";

//  printf ("kernel_name_ptx=[%s]\n", kernel_name_ptx);
// Create module for object
//	checkCudaErrors(cuModuleLoadDataEx(&cudaModule, str.c_str(), 0, 0, 0));

//  printf ("ptxstr=%s\n", ptxstr);

//  checkCudaErrors(cuModuleLoad (&cudaModule, kernel_name_ptxsass));
//  checkCudaErrors(cuModuleLoad (&cudaModule, kernel_name_ptx));
//	checkCudaErrors(cuModuleLoad(&cudaModule, kernel_ptxobj.c_str()));

//  printf ("ptx-entry-name=[%s]\n", entry_name);

// Device data
//	CUdeviceptr devBufferA;
//	CUdeviceptr devBufferB;
//	CUdeviceptr devBufferC;

//	unsigned int blockSizeX = block_dim.x;
//	unsigned int blockSizeY = block_dim.y;
//	unsigned int blockSizeZ = block_dim.z;
//	unsigned int gridSizeX = grid_dim.x;
//	unsigned int gridSizeY = grid_dim.y;
//	unsigned int gridSizeZ = grid_dim.z;

//  char msg[64];
//  sprintf (msg, "[@driver][%s]", kernel_name);

//#if VERBOSE
//    {
//      std::cout<<"[@kernel_invoker][launch_params]";
//      std::cout<<"[gx="<<grid_dim.x<<"]";
//      std::cout<<"[gy="<<grid_dim.y<<"]";
//      std::cout<<"[gz="<<grid_dim.z<<"]";
//      std::cout<<"[bx="<<block_dim.x<<"]";
//      std::cout<<"[by="<<block_dim.y<<"]";
//      std::cout<<"[bz="<<block_dim.z<<"]";
//      std::cout<<"\n";
//  printf ("========================================\n");
//  printf ("[@invoker][%s]"
//	  "[(gx, gy, gz)=(%d, %d, %d)]"
//	  "[(bx, by, bz)=(%d, %d, %d)]\n",
//	  kernel_name, grid_dim.x, grid_dim.y, grid_dim.z, block_dim.x,
//	  block_dim.y, block_dim.z);
//    }
//#endif

//  cuda_timer t_kernel_launch;
//  cuda_timer_init_record_start (t_kernel_launch);

	CUresult err = cuLaunchKernel(handle->function, handle->grid_dim.x,
			handle->grid_dim.y, handle->grid_dim.z, handle->block_dim.x,
			handle->block_dim.y, handle->block_dim.z,
			handle->n_dynamic_shared_mem_bytes,
			NULL, handle->KernelParams, NULL);

	if (err != CUDA_SUCCESS) {
		return EXIT_FAILURE;
	} else
		return EXIT_SUCCESS;

//      checkCudaErrors(err);

//  cuda_timer_record_stop (t_kernel_launch);
//  cuda_timer_record_get_elapsed_time (t_kernel_launch, msg);
//  cuda_timer_destroy (t_kernel_launch);

//  checkCudaErrors(cuCtxDestroy (context));
//  free (ptxstr);
//  printf ("========================================\n");

//
//  stop_profiling ();

}

///////////////////////////////////////////////////////////

int kernel_invoker(char * kernel_name, int* launch_params,
		void **KernelParams) {
//finding kernel_idx here rather than doing it in "kernel_invoker_with_profiler".
	int kernel_idx;
	char kernel_name_ptx[MAX_KERNEL_NAME_LEN];
	sprintf(kernel_name_ptx, "%s.ptx", kernel_name);
//  printf ("[kernel_name][%s]\n", kernel_name);
	if (ptx_lookup_manager_get_kernel_idx(kernel_name_ptx,
			&kernel_idx)!=EXIT_SUCCESS) {
		printf("[ERROR: could not find kernel idx!]...FATAL!");
		exit(EXIT_FAILURE);
	}

//  printf ("@[%s][kernel_idx=%d]\n", __func__, kernel_idx);

	klaraptor_profiler_session_data * si = global_session_data;
	klaraptor_profiler_data_t * pi =
			&si->global_klaraptor_profiler_data[kernel_idx];
	profiler_kernel_handle_t handle;

	int already_launched = launch_history_check_entry(&pi->launch_history,
			launch_params);
	if (already_launched == EXIT_SUCCESS) {
#if VERBOSE
		printf("[@%s][kernel already profiled, skip]...\n");
#endif
		return EXIT_SUCCESS;
	}
	launch_history_add_entry(&pi->launch_history, launch_params);

	for (int t = 0; t < KERNEL_PROFILER_N_REPEAT; t++) {
		pi->got_profiled = ENABLED;
		kernel_profiler_kernel_handle_init(kernel_name, launch_params,
				KernelParams, kernel_idx, &handle);
		kernel_profiler_kernel_set_launch_params(launch_params, pi);

//profile as many times as needed.
//TODO: replace the loop with an IO mechanism with profiler.
//  for (int i = 0; i < pi->n_all_groups; i++)
		pi->n_events_counted = 0;
		kernel_profiler_kernel_subscribe(pi);

		int stop_profiling = 0;

		for (int s = 0; s < pi->group_sets->numSets; s++) {
			for (int g = 0; g < pi->group_sets->sets[s].numEventGroups; g++) {
#if VERBOSE
				printf("[set=%d][group=%d]\n", s, g);
#endif
				pi->current_group = pi->group_sets->sets[s].eventGroups[g];
				if (kernel_invoker_with_profiler(&handle) != EXIT_SUCCESS) {
					printf("KERNEL LAUNCH FAILED! stop profiling!\n");
					pi->valid_profiling = DISABLED;
					stop_profiling = 1;
					break;
				}
			}
			if (stop_profiling == 1) {
				break;
			}
		}

		kernel_profiler_kernel_unsubscribe(pi);
		kernel_profiler_kernel_handle_clear(&handle);
	}
	return EXIT_SUCCESS;
}

///////////////////////////////////////////////////////////
