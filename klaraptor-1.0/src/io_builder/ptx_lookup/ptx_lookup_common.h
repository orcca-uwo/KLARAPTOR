/*!
 \file ptx_lookup_common.h
 \author Davood Mohajerani <dmohajer@uwo.ca>
 \brief
 */

#ifndef PTX_LOOKUP_COMMON_H_
#define PTX_LOOKUP_COMMON_H_

//#include <iostream>
//#include <string>
//#include <regex>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef VERBOSE
#define VERBOSE 0
#endif

///////////////////////////////////////////////////////////

extern int is_ptx_lookup_init;

///////////////////////////////////////////////////////////

/**
 *  size of each attribute is computed at the compile time
 *  such that there will be no need to do strlen at runtime.
 */
typedef struct ptx_lookup_entry {
	const char *kernel_name;
	int kernel_name_size;
	const char *ptx_entry_name;
	int ptx_entry_name_size;
	unsigned char *ptx_str;
	int ptx_str_size;
} ptx_lookup_entry_t;

///////////////////////////////////////////////////////////

typedef struct ptx_lookup_permanent_entry {
	int idx;
	char *kernel_name;
	unsigned char *ptx_str;
	char *ptx_entry_name;
	int kernel_name_size;
	int ptx_str_size;
	int ptx_entry_name_size;
} ptx_lookup_permanent_entry_t;

///////////////////////////////////////////////////////////

typedef struct ptx_lookup_list_struct {
	ptx_lookup_permanent_entry_t * list;
	int size;
} ptx_lookup_list_t;

///////////////////////////////////////////////////////////

extern ptx_lookup_list_t * ptx_lookup_list;

///////////////////////////////////////////////////////////

extern void
ptx_lookup_manager_init();

///////////////////////////////////////////////////////////

extern void
ptx_lookup_manager_insert(ptx_lookup_list_t * list, ptx_lookup_entry_t *element,
		int element_idx);

///////////////////////////////////////////////////////////
#endif
