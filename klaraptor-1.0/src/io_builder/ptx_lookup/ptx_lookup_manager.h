/*!
 \file ptx_lookup_manager.h
 \author Davood Mohajerani <dmohajer@uwo.ca>
 \brief
 */

#ifndef PTX_PTR_MANAGER_H_
#define PTX_PTR_MANAGER_H_

#include "ptx_lookup_common.h"

///////////////////////////////////////////////////////////

int is_ptx_lookup_init = 0;
ptx_lookup_list_t * ptx_lookup_list;

///////////////////////////////////////////////////////////

void ptx_lookup_manager_clear() {
	if (is_ptx_lookup_init) {
		ptx_lookup_permanent_entry_t * ptr;
		for (int i = 0; i < ptx_lookup_list->size; i++) {
			ptr = &ptx_lookup_list->list[i];
			free(ptr->kernel_name);
			free(ptr->ptx_str);
			free(ptr->ptx_entry_name);
			ptr->kernel_name_size = 0;
			ptr->ptx_str_size = 0;
			ptr->ptx_entry_name_size = 0;
		}
#if VERBOSE
		printf ("[size of ptx_lookup_list after clearing = %lu]...\n",
				ptx_lookup_list->size);
#endif
		is_ptx_lookup_init = 0;
		return;
	}

#if VERBOSE
	printf ("[ptx_lookup_list is already cleaned up!]...\n");
#endif
//  return EXIT_SUCCESS;
}

///////////////////////////////////////////////////////////

int ptx_lookup_manager_get_kernel_idx(char *kernel_name, int *kernel_idx) {

	///todo: ptx_lookup_manager_init can be moved to a function pre-main.
	//  ptx_lookup_manager_init ();

	ptx_lookup_permanent_entry_t * ptr;
	for (int i = 0; i < ptx_lookup_list->size; i++) {
		ptr = &ptx_lookup_list->list[i];
		if (strcmp(ptr->kernel_name, kernel_name) == 0) {
#if VERBOSE
			printf ("[found the matching kernel!]...");
#endif
			*kernel_idx = ptr->idx;
			return EXIT_SUCCESS;
		}
	}
//  printf("[ERROR: could not find kernel idx!]...FATAL!");
	printf("@[%s][ERROR: could not find kernel idx for kernel_name=%s]\n",
			__func__, kernel_name);
	exit(EXIT_FAILURE);
	return EXIT_FAILURE;
}

///////////////////////////////////////////////////////////

int ptx_lookup_manager_get_ptx_str(int kernel_idx, unsigned char ** ptx_str) {

#if VERBOSE
	printf ("[checking for kernel_idx[%d]\n", kernel_idx);
	printf ("[ptx_lookup->size = [%d]]\n", ptx_lookup_list->size);
#endif

	if (kernel_idx < ptx_lookup_list->size) {
		ptx_lookup_permanent_entry_t * ptr = &ptx_lookup_list->list[kernel_idx];
		*ptx_str = ptr->ptx_str;
		return EXIT_SUCCESS;
	}

	printf("[ERROR: Failed to find matching source for kernel_idx[%d]]...\n)",
			kernel_idx);
	return EXIT_FAILURE;
}

///////////////////////////////////////////////////////////

int ptx_lookup_manager_get_ptx_entry_name(int kernel_idx,
		char *ptx_entry_name) {
#if VERBOSE
	printf ("[checking for kernel_idx [%d]\n", kernel_idx);
	printf ("[ptx_lookup->size = [%d]]\n", ptx_lookup_list->size);
#endif

	if (kernel_idx < ptx_lookup_list->size) {
		ptx_lookup_permanent_entry_t * ptr = &ptx_lookup_list->list[kernel_idx];
		strcpy(ptx_entry_name, ptr->ptx_entry_name);
		return EXIT_SUCCESS;
	}

	printf("[ERROR: Failed to find PTX ENTRY NAME for kernel_idx[%d]]...\n)",
			kernel_idx);
	return EXIT_FAILURE;
}

/////////////////////////////////////////////////////////////
//
////ToDo: replace with a solution based on "strstr" and "strncpy".
////extract the entry name for ptx src; to be used in driver API call.
//int
//ptx_lookup_manager_set_ptx_entry_name_via_regex (
//    ptx_lookup_permanent_entry_t * ptr)
//{
////  char *ptx_str = ptr->ptx_str;
////  char *ptx_entry_name = ptr->ptx_entry_name;
//
//  std::string search_str (ptr->ptx_str);
//
////	search for .visible .entry ENTRY
////	TODO: make sure the following prefix does not include unknown escape sequence.
//  std::string prefix = "\.visible \.entry ";
////	std::string prefix = "\\.visible \\.entry ";
//  std::regex re1 (prefix + "[^\(]*", std::regex::ECMAScript);
//  std::smatch m1;
//
//  if (std::regex_search (search_str, m1, re1))
//    {
//      std::string result = m1[0];
////		search_str = m1.suffix().str();
//      //remove the prefix from it; the remaining string is the answer.
//      result.erase (0, prefix.length ());
////		std::cout << "["<<m1[0]<<"] --> [" << result<< "]\n";
//
//      //todo: change to strcpy
//      int size = strlen (result.c_str () + 1);
//      ptr->ptx_entry_name = (char*) malloc (size);
////      sprintf (ptr->ptx_entry_name, "%s", result.c_str ());
//      strcpy (ptr->ptx_entry_name, result.c_str ());
//      ptr->ptx_entry_name_size = size;
//      return EXIT_SUCCESS;
//    }
//  else
//    {
//      std::cout << "[ERROR: CANNOT FIND ENTRY NAME FOR PTX SOURCE!]...\n";
//      exit (EXIT_FAILURE);
//      return EXIT_FAILURE;
//    }
//}

///////////////////////////////////////////////////////////

//ToDo: replace with a solution based on "strstr" and "strncpy" -> DONE.
//extract the entry name for ptx src; to be used in driver API call.
//int
//ptx_lookup_manager_set_ptx_entry_name (ptx_lookup_permanent_entry_t * ptr)
//{
////search for .visible .entry ENTRY
//  char * search_str = ptr->ptx_str;
//  //TODO: make sure the following prefix does not include unknown escape sequence.
//  const char *match_str_begin = "\.visible \.entry ";
//  const char *match_str_end = "(";
//  char * start_char, *stop_char;
//
////  printf ("match_str_begin=[%s]\n", match_str_begin);
//  start_char = strstr (search_str, match_str_begin);
//  if (start_char == NULL)
//    {
//      printf ("[@%s][ERROR: COULD NOT FIND THE PATTERN!]\n", __func__);
//      return EXIT_FAILURE;
//    }
//
//  stop_char = strstr (start_char, match_str_end);
//  if (start_char == NULL)
//    {
//      printf ("[@%s][ERROR: COULD NOT FIND THE PATTERN!]\n", __func__);
//      return EXIT_FAILURE;
//    }
//
//  int start_len = strlen (start_char);
//  int stop_len = strlen (stop_char);
//  int match_str_len = strlen (match_str_begin);
//  int result_size = start_len - stop_len - match_str_len;
//
//  //setting values for ptx_entry ptr.
//  ptr->ptx_entry_name_size = result_size;
//  char ** result = &ptr->ptx_entry_name;
//  *result = (char*) malloc (result_size + 1); //+1 for the string terminator '\0'.
//  strncpy (*result, start_char + match_str_len, result_size);
//#if VERBOSE
//  printf ("[entry name = [%s]]\n", ptr->ptx_entry_name);
//#endif
//  return EXIT_SUCCESS;
//}

///////////////////////////////////////////////////////////

//void
//ptx_lookup_manager_printall (ptx_lookup_list_t * ptx_lookup)
//{
//  for (int i = 0; i < ptx_lookup->size; i++)
//    {
//      printf ("element[%d][%s]\n", i, ptx_lookup->list[i].kernel_name);
////      printf("ptx_str\n%s", ptx_lookup->list[i].ptx_str);
//    }
//}

///////////////////////////////////////////////////////////
//void
//parse_ptx (char * ptx_str)
//{
//
//  char * result=(char*)malloc(strlen(ptx_str));
//  char delim[] = "\n";
//  char *ptr = strtok (ptx_str, delim);
//
//  int size=0;
//  while (ptr != NULL)
//    {
//      printf ("'%s'\n", ptr);
//      strncpy(result+size, ptr, strlen(ptr));
//      size+=strlen(ptr);
//      ptr = strtok (NULL, delim);
//    }
//  strnc
//}

///////////////////////////////////////////////////////////

//the memory for storing element as an entry in table is already allocated,
//however, the memory for the element itself will be allocated in this function.
void ptx_lookup_manager_insert(ptx_lookup_list_t * ptx_lookup,
		ptx_lookup_entry_t *element, int element_idx) {
#if VERBOSE
	printf ("[adding an entry with idx=[%d][%s]]\n", element_idx,
			element->kernel_name);
#endif
	if (element_idx >= ptx_lookup->size) {
		printf("[@%s][ERROR: element_idx>ptx_lookup->size]\n", __func__);
		exit(EXIT_FAILURE);
	}

	ptx_lookup_permanent_entry_t * ptr = &ptx_lookup->list[element_idx];

	//// TODO: clarify the relationship between kernel order and its index:
	//// order of kernel in the lookup list is the same as its index.
	ptr->idx = element_idx;

	//set size and copy value;
	ptr->kernel_name_size = element->kernel_name_size + 1;
	ptr->kernel_name = (char*) malloc(ptr->kernel_name_size);
	strcpy(ptr->kernel_name, element->kernel_name);

	//set size and copy value;
	ptr->ptx_entry_name_size = element->ptx_entry_name_size + 1;
	ptr->ptx_entry_name = (char*) malloc(ptr->ptx_entry_name_size);
	strcpy(ptr->ptx_entry_name, element->ptx_entry_name);

	//memcpy works as we are just copying unsigned char bytes.
	ptr->ptx_str_size = element->ptx_str_size + 1;
	ptr->ptx_str = (unsigned char*) malloc(ptr->ptx_str_size);
	memcpy(ptr->ptx_str, element->ptx_str, ptr->ptx_str_size);
//  parse_ptx (ptr->ptx_str);

//  ptx_lookup_manager_set_ptx_entry_name (ptr);

//  ptx_lookup_manager_printall (ptx_lookup);
//  printf ("@[%s][kernel_name=%s][kernel_idx=%d]\n", __func__, ptr->kernel_name,
//	  ptr->idx);
}

///////////////////////////////////////////////////////////

#endif
