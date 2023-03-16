#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>
#include <cstdlib>
///////////////////////////////////////
std::string short_ln(int s)
{
	std::string line = "";
	for (int i = 0; i < s; i++)
		line += "-";
	line += "\n";
	return line;
}

///////////////////////////////////////
std::string normalize_name(std::string name)
{
	std::string init_name = name;

	std::replace(name.begin(), name.end(), '.', '_');
	std::replace(name.begin(), name.end(), ' ', '_');
	std::replace(name.begin(), name.end(), '-', '_');

//	std::cout << "normalizing [" << init_name << "] to [" << name << "]\n";
	return name;
}

///////////////////////////////////////

std::string to_multiline(std::string input)
{
	std::string result;
	result = "R\"(" + input + ")\""; //+"(";//+input+'\"'+");";
	return result;
}

///////////////////////////////////////

void convert_string_to_vector(std::string x)
{
//	std::cout<<"[in convert_string_to_vector]...\n";
//
//	size_t n=x.size();
//	std::vector<wchar_t>data(n);
//	std::cout<<"n="<<n<<"\n";
////	std::cout<<x<<"\n";
//	for(int i=0;i<n;i++)
//		{
//		wchar_t c;
//		mbtowc(&c, x[i]);
//		data.push_back(c);
//		}
////	std::cout<<data<<"\n";
//
//	std::ofstream result_str;
//	result_str.open("result.str");
//	for (auto x:data)
//		result_str<<x<<"\n";
//	result_str.close();
}

///////////////////////////////////////

std::string decorate_ptx(std::string kernel_name)
{
	std::cout << "adding kernel [" << kernel_name << "] to [ptx_lookup_table] ...\n";
	std::ifstream input_ptx;
	input_ptx.open(kernel_name, std::ifstream::binary);
	if (!input_ptx.is_open())
	{
		std::cout << "[WARNING: cannot find ptx source for [" << kernel_name
				<< "] ... ignoring \n";
	}
	std::string ptx((std::istreambuf_iterator<char>(input_ptx)),
			std::istreambuf_iterator<char>());

//	std::cout << ptx << "\n";
//	convert_string_to_vector(ptx);

	std::string result = "std::pair<std::string, std::string> "
			+ normalize_name(kernel_name) + "(" + '\"' + (kernel_name) + '\"'
			+ " , " + to_multiline(ptx) + ");\n";

//	std::cout << result << "\n";
//	std::cout<<result.length()<<"\n";
	return result;
}

///////////////////////////////////////

std::string decorate_ptxsass(std::string kernel_name)
{
	std::cout << "adding kernel [" << kernel_name << "] to [ptx_lookup_table] ...\n";
	std::ifstream input_ptx;
	input_ptx.open(kernel_name, std::ifstream::binary);
	if (!input_ptx.is_open())
	{
		std::cout << "[WARNING: cannot find ptx source for [" << kernel_name
				<< "] ... ignoring \n";
	}
	std::string ptx((std::istreambuf_iterator<char>(input_ptx)),
			std::istreambuf_iterator<char>());

//	std::cout << ptx << "\n";
	convert_string_to_vector(ptx);

	std::string result = "std::pair<std::string, std::string> "
			+ normalize_name(kernel_name) + "(" + '\"' + (kernel_name) + '\"'
			+ " , " + to_multiline(ptx) + ");\n";

//	std::cout << result << "\n";
//	std::cout<<result.length()<<"\n";
	return result;
}

///////////////////////////////////////

std::string form_func_init_ptx_lookup_table(
		std::vector<std::string> kernel_name_list)
{
	std::string func =
			R"(
void init_ptx_lookup_table()
{
   if(is_ptx_lookup_table_init==1)
       {
           //std::cout<<"[ptx_lookup_table is already initialized!] ... ignoring \n";
           return;
       }
)";

	///////////////////////////////////
	//// iterate over kernel name list

	for (auto name : kernel_name_list)
//	std::string pair = "kernel";
		func += "	ptx_lookup_table.insert(" + normalize_name(name) + ");\n";
	///////////////////////////////////
	func +=
			R"(
//std::cout<<"set [ptx_lookup_table_init = 1] ...\n";
//std::cout<<"[ptx_lookup_table_size : "<<ptx_lookup_table.size()<<"]\n";
)";
	func += "is_ptx_lookup_table_init=1;\n";
	func += "}";

	return func;
}

///////////////////////////////////////

std::string form_func_init_ptxsass_lookup_table(
		std::vector<std::string> kernel_name_list)
{
	std::string func =
			R"(
void init_ptxsass_lookup_table()
{
   if(is_ptxsass_lookup_table_init==1)
       {
           //std::cout<<"[ptxsass_lookup_table is already initialized!] ... ignoring \n";
           return;
       }
)";

	///////////////////////////////////
	//// iterate over kernel name list

	for (auto name : kernel_name_list)
//	std::string pair = "kernel";
		func += "	ptxsass_lookup_table.insert(" + normalize_name(name) + ");\n";
	///////////////////////////////////
	func +=
			R"(
//std::cout<<"set [ptxsass_lookup_table_init = 1] ...\n";
//std::cout<<"[ptxsass_lookup_table_size : "<<ptxsass_lookup_table.size()<<"]\n";
)";
	func += "is_ptxsass_lookup_table_init=1;\n";
	func += "}";

	return func;
}

///////////////////////////////////////

std::vector<std::string> fill_vector_from_file(std::string path)
{
	std::ifstream file;
	file.open(path, std::ifstream::in);
	if (!file.is_open())
	{
		std::cout << "[ERROR: cannot open [" << path
				<< "] in [fill_vector_from_file]\n";
		exit(EXIT_FAILURE);
	}

	std::vector<std::string> result;

	std::string line;
	while (std::getline(file, line))
	{
		result.push_back(line);
//		std::cout<<line<<"\n";
	}
	file.close();

	for (int i = 0; i < result.size(); i++)
	{
		std::cout << "[found (" << i << ")]=[" << result[i] << "] from list ["
				<< path << "]... \n";
	}
	std::cout << short_ln(60);
	return result;
}

///////////////////////////////////////

int make_ptx_lookup_table(std::string kernel_name_list,
		std::string result_header_path)
{
	std::vector<std::string> kernel_name_list_vector;
	kernel_name_list_vector = fill_vector_from_file(kernel_name_list);
	//	kernel_name_list

	std::string comment_line =
			"\n\n/////////////////////////////////////////////////\n";

	std::string prefix_str =
			R"(
////////////////////////////////////////////////////////////////////
//// WARNING: THIS HEADER IS AUTOMATICALLY GENERATED! DO NOT MODIFY!
////////////////////////////////////////////////////////////////////
#ifndef PTX_LOOKUP_TABLE_H_
#define PTX_LOOKUP_TABLE_H_

#include <iostream>
#include <map>
#include <string>

static int is_ptx_lookup_table_init = 0;
)"
					+ comment_line
					+ R"(
std::map<std::string, std::string> ptx_lookup_table;)";

	std::string suffix_str = comment_line + R"(
#endif
)";

	/////////////////////////
	std::string output_str;
	output_str += prefix_str;
	/////////////////////////
	//// iterate over all kernel objects.

	for (int i = 0; i < kernel_name_list_vector.size(); i++)
		output_str += comment_line + decorate_ptx(kernel_name_list_vector[i]);

	std::cout << short_ln(60);
	/////////////////////////
	/////////////////////////
	std::string filler_func = form_func_init_ptx_lookup_table(
			kernel_name_list_vector);

	output_str += comment_line + filler_func;
	output_str += comment_line + suffix_str;
	/////////////////////////
	std::ofstream output_ptx;
	output_ptx.open(result_header_path);
	output_ptx << output_str << "\n";
	output_ptx.close();

	return EXIT_SUCCESS;
}

///////////////////////////////////////

int make_ptxsass_lookup_table(std::string kernel_name_list,
		std::string result_header_path)
{
	std::vector<std::string> kernel_name_list_vector;
	kernel_name_list_vector = fill_vector_from_file(kernel_name_list);

	for (auto &x : kernel_name_list_vector)
		x+="sass";
	//	kernel_name_list

	std::string comment_line =
			"\n\n/////////////////////////////////////////////////\n";

	std::string prefix_str =
			R"(
////////////////////////////////////////////////////////////////////
//// WARNING: THIS HEADER IS AUTOMATICALLY GENERATED! DO NOT MODIFY!
////////////////////////////////////////////////////////////////////
#ifndef PTXSASS_LOOKUP_TABLE_H_
#define PTXSASS_LOOKUP_TABLE_H_

#include <iostream>
#include <map>
#include <string>

static int is_ptxsass_lookup_table_init = 0;
)"
					+ comment_line
					+ R"(
std::map<std::string, std::string> ptxsass_lookup_table;)";

	std::string suffix_str = comment_line + R"(
#endif
)";

	/////////////////////////
	std::string output_str;
	output_str += prefix_str;
	/////////////////////////
	//// iterate over all kernel objects.

	for (int i = 0; i < kernel_name_list_vector.size(); i++)
	{
		std::string ptxsass_str= decorate_ptxsass(kernel_name_list_vector[i]);
//		std::cout<<"length of ptxsass str= "<<ptxsass_str.length()<<"\n";
		output_str += comment_line + ptxsass_str;
	}


	std::cout << short_ln(60);
	/////////////////////////
	/////////////////////////
	std::string filler_func = form_func_init_ptxsass_lookup_table(
			kernel_name_list_vector);

	output_str += comment_line + filler_func;
	output_str += comment_line + suffix_str;
	/////////////////////////
	std::ofstream output_ptx;
	output_ptx.open(result_header_path);
	output_ptx << output_str << "\n";
	output_ptx.close();

	return EXIT_SUCCESS;
}

///////////////////////////////////////

int main(int argc, char ** argv)
{
	std::string kernel_name_list = "list.tmp";
	std::string result_header_path_ptx = "ptx_lookup_table.h";
//	std::string result_header_path_ptxsass = "ptxsass_lookup_table.h";
	if (argc < 3)
	{
		std::cout << "=================================================\n";
		std::cout << "args [1]: kernel_name_list [2]:result_header_path_ptx\n";
		std::cout << "=================================================\n";
		return EXIT_FAILURE;
	}

	kernel_name_list = argv[1];
	result_header_path_ptx = argv[2];
//	result_header_path_ptxsass = argv[3];
	std::cout << short_ln(60);
	std::cout << "kernel_name_list = [" << kernel_name_list << "]\n";
	std::cout << "result_header_path_ptx = [" << result_header_path_ptx << "]\n";
//	std::cout << "result_header_path_ptxsass = [" << result_header_path_ptxsass << "]\n";
	std::cout << short_ln(60);
	make_ptx_lookup_table(kernel_name_list, result_header_path_ptx);
//	make_ptxsass_lookup_table(kernel_name_list, result_header_path_ptxsass);
	return 0;
}

///////////////////////////////////////
