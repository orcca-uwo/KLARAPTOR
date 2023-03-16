#ifndef PTX_PTR_TABLE_MANAGER_H_
#define PTX_PTR_TABLE_MANAGER_H_

#include <iostream>
#include <map>
#include <string>
#include <algorithm>
#include <regex>
#include "ptx_lookup_table.h"

///////////////////////////////////////////////////////////

extern std::map<std::string, std::string> ptx_lookup_table;
//std::map<std::string, std::string> ptx_entry_name_lookup_table;

///////////////////////////////////////////////////////////

int get_ptx_str(char* kernel_name_in, char ** ptx_str)
{
	init_ptx_lookup_table();

//	std::cout << "checking for [" << kernel_name << "]\n";
//	for (auto e : ptx_lookup_table)
//	{
//		if (e.first == kernel_name)
//		{
////			std::cout<<e.second<<"\n";
////			std::cout << "found matching source for [" << kernel_name
////					<< "]...\n";
//			int n_bytes=(e.second.size()+1)*sizeof(char);
//			*ptx_str= (char*)malloc(n_bytes);
//			sprintf(*ptx_str, "%s", e.second.c_str());
////			return e.second;
//			return EXIT_SUCCESS;
//		}
//	}

	std::string kernel_name(kernel_name_in);

/*
    std::cout << "kernel_name: " << kernel_name << std::endl;

    for (auto e : ptx_lookup_table) 
        std::cout << e.first << e.second << std::endl;

*/

	if (ptx_lookup_table.find(kernel_name) != ptx_lookup_table.end())
	{
		//			std::cout << "found matching source for [" << kernel_name
		//					<< "]...\n";

		int n_bytes = (ptx_lookup_table[kernel_name].size() + 1) * sizeof(char);
		*ptx_str = (char*) malloc(n_bytes);
		sprintf(*ptx_str, "%s", ptx_lookup_table[kernel_name].c_str());
		return EXIT_SUCCESS;
	}
	std::cout << "Failed to find matching source for [" << kernel_name
			<< "]...\n";
	exit (EXIT_FAILURE);
	return EXIT_FAILURE;
}

///////////////////////////////////////////////////////////

//ToDo: replace with a solution based on "strstr" and "strncpy".
//extract the entry name for ptx src; to be used in driver API call.
int get_ptx_entry_name(char *ptx_str, char *ptx_entry_name)
{
	std::string search_str(ptx_str);

//	search for .visible .entry ENTRY
	std::string prefix = "\.visible \.entry ";
	std::regex re1(prefix + "[^\(]*", std::regex::ECMAScript);
	std::smatch m1;

	if (std::regex_search(search_str, m1, re1))
	{
		std::string result = m1[0];
//		search_str = m1.suffix().str();
		//remove the prefix from it; the remaining string is the answer.
		result.erase(0, prefix.length());
//		std::cout << "["<<m1[0]<<"] --> [" << result<< "]\n";
		sprintf(ptx_entry_name, "%s", result.c_str());
		return EXIT_SUCCESS;
	}
	else
	{
		std::cout << "[ERROR: CANNOT FIND ENTRY NAME FOR PTX SOURCE!]...\n";
		exit (EXIT_FAILURE);
		return EXIT_FAILURE;
	}
}

///////////////////////////////////////////////////////////

#endif
