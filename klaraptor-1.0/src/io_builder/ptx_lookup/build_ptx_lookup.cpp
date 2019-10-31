/*!
 \file build_ptx_lookup.cpp
 \author Davood Mohajerani <dmohajer@uwo.ca>
 \brief
 */

#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>
#include <cstdlib>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#ifndef VERBOSE
#define VERBOSE 0
#endif

///////////////////////////////////////

std::string short_ln(int s) {
	std::string line = "";
	for (int i = 0; i < s; i++)
		line += "-";
	line += "\n";
	return line;
}

///////////////////////////////////////

std::string normalize_name(std::string name) {
	std::string init_name = name;
	std::replace(name.begin(), name.end(), '.', '_');
	std::replace(name.begin(), name.end(), ' ', '_');
	std::replace(name.begin(), name.end(), '-', '_');
	return name;
}

///////////////////////////////////////

std::vector<std::string> get_strings(std::string input) {
	int n = input.size();
	char * result = (char*) malloc(n);
	sprintf(result, "");
	int readable = 1;
	int len = 0;

	std::vector < std::string > string_vector;
	for (int i = 0; i < n; i++) {
		unsigned int v = (unsigned int) (input[i]);
		if (v > 31 && v < 128) {
			readable = 1;
			sprintf(result + len, "%c", input[i]);
			len++;
		} else {
			readable = 0;
			//anything with len>=4 is worth recording.
			if (len > 3)
//	  printf("[readable][%s]\n", result);
					{
				string_vector.push_back(std::string(result));
			}
			sprintf(result, "");
			len = 0;
		}
	}

//  for(int i=0;i<string_vector.size();i++)
//    {
//      printf("[STRING][%d][%s]\n", i,string_vector[i].c_str() );
//    }

	return string_vector;
}

///////////////////////////////////////

std::string get_kernel_entry_name(std::string ptxstr) {

	std::vector < std::string > string_vector = get_strings(ptxstr);
	//all patterns={".text.", ".nv.info.", ".nv.shared.", ".nv.constant0."}
	std::string pattern = ".text.";
	std::string pattern_alt = ".visible .entry ";
//    std::string pattern_alt = ".globl       ";
	int n = string_vector.size();
	std::string result = "";
	int found = 0;
	for (int i = 0; i < n; i++) {
		size_t pos = string_vector[i].find(pattern);
		if (pos != std::string::npos) {
//	  printf("found a match!\n");
			result = std::string(string_vector[i].c_str() + pattern.length());
			found = 1;
			break;
		}
	}
//  if (found == 0)
//    {
//      for (int i = 0; i < n; i++)
//	{
//	  size_t pos = string_vector[i].find (pattern_alt);
//	  if (pos != std::string::npos)
//	    {
//	      //	  printf("found a match!\n");
//	      result = std::string (
//		  string_vector[i].c_str () + pattern_alt.length ());
//	      result.erase(result.size()-1);
//	      found = 1;
//	      break;
//	    }
//	}
//    }

#if VERBOSE
	printf ("[kernel-entry-name=%s]\n", result.c_str ());
#endif
	return result;
}

///////////////////////////////////////

std::string char_to_hex_str(std::string input) {
	std::string result = "";
	result += "{";
	int n = input.size();
	char hex[5];
	unsigned char u;
	;
	for (int i = 0; i < n - 1; i++) {
		sprintf(hex, "0x%02x", (unsigned char) input[i]);
		result += std::string(hex) + ",";
	}
	sprintf(hex, "0x%02x", (unsigned char) input[n - 1]);
	result += std::string(hex);
	result += "}";
	return result;
	/////////////////////////////////////////////
}

///////////////////////////////////////

int read_binary_str_from_file(const char * path, char ** str, int * size) {
#if VERBOSE
	printf ("[path=%s]\n", path);
#endif
	int str_size = 0;
	FILE * file = fopen(path, "rb");
	//get the size of ptxsass file in bytes.
	fseek(file, 0L, SEEK_END);
	str_size = ftell(file);
	fseek(file, 0L, SEEK_SET);

	*str = (char*) malloc(str_size);
	int buffer_idx = 0;
	while (fread((*str) + buffer_idx, 1, 1, file)) {
		buffer_idx++;
		//memory violation.
		if (buffer_idx == str_size + 1) {
			printf("[@%s][ERROR: buffer_idx==str_size]\n", __func__);
			return (EXIT_FAILURE);
		}
	}
	*size = str_size;
//  printf("buffer_idx=%d ", buffer_idx);
//  printf("read ptxsass=\n");
//  fwrite(*ptxsass_str, 1, ptxsass_str_size, stdout);
//
//  FILE* tmp=fopen("tmp.ptxsass","wb" );
//  fwrite(*ptxsass_str, 1, ptxsass_str_size, tmp);
//  fclose(tmp);

	return EXIT_SUCCESS;
}

///////////////////////////////////////

std::string decorate_ptx(std::string kernel_name) {
	std::cout << "adding kernel [" << kernel_name << "] to [ptx_lookup] ...\n";

	int stat;
	char *ptxsass_str;
	int ptxsass_size;
	std::string ptxsass_name = kernel_name + "sass";
	stat = read_binary_str_from_file(ptxsass_name.c_str(), &ptxsass_str,
			&ptxsass_size);

	if (stat != EXIT_SUCCESS) {
		printf("FAILED!\n");
	}

	char * ptx_str;
	int ptx_size;
	stat = read_binary_str_from_file(kernel_name.c_str(), &ptx_str, &ptx_size);

//  std::ifstream input_ptx;
//  input_ptx.open (kernel_name, std::ifstream::binary);
//  if (!input_ptx.is_open ())
//    {
//      std::cout << "[WARNING: cannot find ptx source for [" << kernel_name
//	  << "] ... ignoring \n";
//    }
//  std::string ptx ((std::istreambuf_iterator<char> (input_ptx)),
//		   std::istreambuf_iterator<char> ());
//
////	std::cout << ptx << "\n";
////	convert_string_to_vector(ptx);
//
//  std::string result = "std::pair<std::string, std::string> "
//      + normalize_name (kernel_name) + "(" + '\"' + (kernel_name) + '\"' + " , "
//      + to_multiline (ptx) + ");\n";
//
//  result = "ptx_lookup_entry_t " + normalize_name (kernel_name) + "={" + '\"'
//      + (kernel_name) + '\"' + " , " + to_multiline_c (ptx) + "};\n";

//  printf ("str_size=%d\n", ptxsass_size);
//  printf ("str_len=%lu\n", ptxsass_str_std.length ());

	std::string ptxsass_str_std = (std::string(ptxsass_str, ptxsass_size));

	std::string kernel_entry_name = get_kernel_entry_name(ptxsass_str_std);

	std::string char_array_name = "chr_array_" + normalize_name(kernel_name);
//  std::string char_array_decl = "unsigned char " + char_array_name + "["
//      + std::to_string (ptxsass_size) + "] = "
//      + char_to_hex_str (ptxsass_str_std) + ";\n";
	std::string char_array_decl = "unsigned char " + char_array_name + "[]="
			+ char_to_hex_str(ptxsass_str_std) + ";\n";

	std::string result = "";
	result += char_array_decl + "\n";
	result += "    ptx_lookup_entry_t " + normalize_name(kernel_name) + "={"
			+ '\"' + (kernel_name) + '\"' + ","
			+ std::to_string(kernel_name.size()) + "," + '\"'
			+ (kernel_entry_name) + '\"' + ","
			+ std::to_string(kernel_entry_name.size()) + "," + char_array_name
			+ "," + std::to_string(ptxsass_size) + "};\n";
	result += "////////////////////////////////////\n";
	return result;
}

///////////////////////////////////////

std::string form_func_init_ptx_lookup(std::vector<std::string> kernel_name_list,
		std::string entry_declarations) {
	std::string func =
			R"(
void ptx_lookup_manager_init()
{
//    printf("in ptx_lookup_manager_init\n");
    if(is_ptx_lookup_init==1)
    {
        //std::cout<<"[ptx_lookup is already initialized!] ... ignoring \n";
        return;
    }
)";

	///////////////////////////////////
	//// iterate over kernel name list

	int n_kernels = kernel_name_list.size();
	func +=
			"    ptx_lookup_list=(ptx_lookup_list_t*)malloc(sizeof(ptx_lookup_list_t));\n";
	func += "    ptx_lookup_list->size=" + std::to_string(n_kernels) + ";\n";
	func +=
			R"(
    int n_bytes=ptx_lookup_list->size*sizeof(ptx_lookup_permanent_entry_t);
    ptx_lookup_list->list = (ptx_lookup_permanent_entry_t*)malloc(n_bytes);

)";

	func += entry_declarations;
	int kernel_idx = 0;
	for (auto name : kernel_name_list) {
//	std::string pair = "kernel";
//    func += "	ptx_lookup.insert(" + normalize_name (name) + ");\n";
//    /////////////////////////////////
		std::string entry = "kernel";
		func += "    ptx_lookup_manager_insert(ptx_lookup_list , &"
				+ normalize_name(name) + ", " + std::to_string(kernel_idx)
				+ ");\n";
		kernel_idx++;
	}
	/////////////////////////////////
	func +=
			R"(
    //std::cout<<"set [ptx_lookup_init = 1] ...";
    //std::cout<<"[ptx_lookup_size : "<<ptx_lookup.size()<<"]";
    is_ptx_lookup_init=1;
};
)";

	return func;
}

///////////////////////////////////////

std::vector<std::string> fill_vector_from_file(std::string path) {
	std::ifstream file;
	file.open(path, std::ifstream::in);
	if (!file.is_open()) {
		std::cout << "[ERROR: cannot open [" << path
				<< "] in [fill_vector_from_file]\n";
		exit(EXIT_FAILURE);
	}

	std::vector < std::string > result;

	std::string line;
	while (std::getline(file, line)) {
		result.push_back(line);
//		std::cout<<line<<"\n";
	}
	file.close();

#if VERBOSE
	for (int i = 0; i < result.size(); i++)
	{
		std::cout << "[found (" << i << ")]=[" << result[i] << "] from list ["
		<< path << "]... \n";
	}
	std::cout << short_ln(60);
#endif 
	return result;
}

///////////////////////////////////////

int make_ptx_lookup(std::string kernel_name_list,
		std::string result_header_path) {
	///////////////////////////////
	///////////////////////////////

	std::vector < std::string > kernel_name_list_vector;
	kernel_name_list_vector = fill_vector_from_file(kernel_name_list);
	//	kernel_name_list

	///////////////////////////////
	///////////////////////////////

	std::string comment_line =
			"\n\n/////////////////////////////////////////////////\n";

	std::string prefix_str =
			R"(
////////////////////////////////////////////////////////////////////
//// WARNING: THIS SOURCE IS AUTOMATICALLY GENERATED! DO NOT MODIFY!
////////////////////////////////////////////////////////////////////
#include "ptx_lookup_common.h"
/////////////////////////////////////////////////
//int ptxsass_lookup_init;
ptx_lookup_list_t * ptx_lookup_list;
)";

	std::string suffix_str = "";

	///////////////////////////////
	///////////////////////////////
	std::string output_str;
	output_str += prefix_str;
	///////////////////////////////
	///////////////////////////////
	//// iterate over all kernel objects.

	std::string entry_declarations = "";

	int n_kernels = kernel_name_list_vector.size();
	//ptx_sass kernels
	for (int i = 0; i < n_kernels; i++) {
		entry_declarations += comment_line
				+ decorate_ptx(kernel_name_list_vector[i]);
	}
	std::cout << short_ln(60);
	/////////////////////////
	/////////////////////////
	std::string filler_func = form_func_init_ptx_lookup(kernel_name_list_vector,
			entry_declarations);

	output_str += comment_line + filler_func;
	output_str += comment_line + suffix_str;

	/////////////////////////
//  std::ofstream output_ptx(result_header_path, std::ofstream::binary);
////  output_ptx.open (result_header_path,);
////  output_ptx << output_str << "\n";
//  output_ptx.write(output_str.c_str(), output_str.size());
//  output_ptx.close ();

	FILE * output_ptx = fopen(result_header_path.c_str(), "wb");
	fwrite(output_str.data(), 1, output_str.size(), output_ptx);
	fclose(output_ptx);
	return EXIT_SUCCESS;
}

///////////////////////////////////////

int main(int argc, char ** argv) {
	std::string kernel_name_list = "list.tmp";
	std::string result_header_path_ptx = "ptx_lookup.cpp";
//	std::string result_header_path_ptxsass = "ptxsass_lookup.h";
	if (argc < 3) {
		std::cout << "=================================================\n";
		std::cout << "args [1]: kernel_name_list [2]:ptx_lookup_path.cpp\n";
		std::cout << "=================================================\n";
		return EXIT_FAILURE;
	}

	kernel_name_list = argv[1];
	result_header_path_ptx = argv[2];
//	result_header_path_ptxsass = argv[3];
#if VERBOSE
	std::cout << short_ln(60);
	std::cout << "kernel_name_list = [" << kernel_name_list << "]\n";
	std::cout << "result_header_path_ptx = [" << result_header_path_ptx << "]\n";
//	std::cout << "result_header_path_ptxsass = [" << result_header_path_ptxsass << "]\n";
	std::cout << short_ln(60);
#endif 
	make_ptx_lookup(kernel_name_list, result_header_path_ptx);
//	make_ptxsass_lookup(kernel_name_list, result_header_path_ptxsass);
	return 0;
}

///////////////////////////////////////
