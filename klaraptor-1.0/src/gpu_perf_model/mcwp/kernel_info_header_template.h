/*!
 \file kernel_info_header_template.h
 \author Davood Mohajerani <dmohajer@uwo.ca>
 \brief
 */

#ifndef EVAL_KERNEL_INFO_KERNEL_NAME__H_
#define EVAL_KERNEL_INFO_KERNEL_NAME__H_
///////////////////////////////////////
//#include "mcwp/mcwp_data_types.h"

///////////////////////////////////////

typedef struct kernel_info_s {
	int nvar;
	point_set_t * eval_point_set;
	int device_compute_capability;
	char kernel_name[256];
	char device_name[256];
} kernel_info_t;

///////////////////////////////////////

int kernel_info_init(kernel_info_t* k) {
	k->nvar = N_KERNEL_VARS_;
	k->eval_point_set = EVAL_POINT_SET_STR_;
	k->device_compute_capability = DEVICE_COMPUTE_CAPABILITY_;

	sprintf(k->kernel_name, "KERNEL_NAME_");
	sprintf(k->device_name, "DEVICE_NAME_");
	return EXIT_SUCCESS;
}

///////////////////////////////////////
#endif
