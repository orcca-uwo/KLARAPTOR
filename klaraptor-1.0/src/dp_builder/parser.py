 
import os
import sys
import re

import random
import time

arch=""
#######################################
NEW_LINE_REPLACEMENT="____EMPTY___SPACE___"+str(random.seed(time.time()))
#######################################
header_files='''
#include <string>
#include <sstream>
#include <iostream>
#include <fstream>
#include <cuda_runtime_api.h>
#include <ocelot/api/interface/ocelot.h>
#include "../../trace_analysis/mem_inst_trace.h"
using namespace std;

trace::MemoryTraceGenerator global_mem_coalesced_generator;
trace::InstructionTraceGenerator global_inst_generator;

#ifndef OCELOT_KERNELS_PARAMETERS_DECLARED
#define OCELOT_KERNELS_PARAMETERS_DECLARED
	int GRID_DIM_X=1, GRID_DIM_Y=1, GRID_DIM_Z=1;
	int BLOCK_DIM_X=1, BLOCK_DIM_Y=1, BLOCK_DIM_Z=1;
	int SHARED_MEM_SIZE=0;
#endif


//#ifndef TRACER_KERNEL_PARAMS_STREAM

//#define TRACER_KERNEL_PARAMS_STREAM
	//std::ofstream tracer_kernel_params_stream;
	//FILE * tracer_kernel_params_stream;
//#endif


#ifndef TRACER_PTX_STREAM
#define TRACER_PTX_STREAM
	std::ifstream tracer_ptx_stream;
#endif

#ifndef GRID_DIM_DEFINTIONS
#define GRID_DIM_DEFINTIONS
	dim3 _griddim_init, _griddim_minimal;
	dim3 _blockdim_init;
#endif


#ifndef OUTPUT_STR
#define OUTPUT_STR
	char _default_output_str[1024];
#endif
'''
#######################################
def print_msg(m, len=50):
	len=66
	hrule=len*"="
	print (hrule+"\n"+str(m));
#######################################

pragma_list=[\
			["emulate",["fast", "full"],[0,1]],\
			["problem",["1d", "2d","3d"],[0,1,2]],\
			["block",["1d","2d","2d_same_param","3d"],[1,2,3]],\
			["work",["linear","square","cubic"],[0,1,2]]
			]

#######################################
def read_from_file(src_path):
	f=open(src_path, "r");
	content=f.read();
	f.close();
	return content

#######################################
def write_str_to_file(content, path):
	f=open(path,"w");
	f.write(content);
	f.close();

####################################### 
def remove_comment_from_content(content):

	# removing commented lines
	content=re.sub(r'//[^\n]*','',content);
	content=content.replace("\n",NEW_LINE_REPLACEMENT);
	content=re.sub(r'/\*[^/]*/','',content);
	content=content.replace(NEW_LINE_REPLACEMENT,'\n');
	content=re.sub(r'[ \t\r\f\v]',' ',content);

	return content

#######################################
## this function has problems with deeply
## nested code
def get_kernel_body_cu(content,v=0):
#	content=read_from_file(src_path);#.split("\n");

#	content=remove_comment_from_content(content)
	# splitting based on kernel
	kernels=content.split("__global__");
	if (len(kernels)<2):
		return []
	kernels=kernels[1:]
	kernel_list=[]
	end_point_list=[]
	
	for i  in range(len(kernels)):	
		stack=[]
		kernels[i]="__global__ "+kernels[i]		
		kernel=kernels[i]
		prev=0;
		symbol_stack=[];
		l_cnt=0;
		r_cnt=0;
		for c in range(len(kernel)):
			if kernel[c]=="{":
				symbol_stack+=["{"];
				r_cnt+=1;
				stack+=[kernel[prev:c]];	
				prev=c+1;

			if kernel[c]=="}":
				symbol_stack+=["}"];
				l_cnt+=1;	
				stack+=[kernel[prev:c]]
				prev=c+1;

			if l_cnt==r_cnt!=0:
				break;

		result=""
		for i in range(l_cnt+r_cnt):
			result=stack.pop()+symbol_stack.pop()+result
#			print(result)
#		print("result")
#		print(result)
#		print("================")
		if result!="":
			end_point_list.append(prev)
			kernel_list.append(result.replace(NEW_LINE_REPLACEMENT,"\n"))
#			print(kernel_list[-1])
	return [kernel_list,end_point_list]

####################################### 

def get_kernel_names_in_content(content,v=0):
#	content=read_from_file(src_path);#.split("\n");
#	content=content.replace("\n",NEW_LINE_REPLACEMENT);
	
	kernel_list=[]
	global_declarations=re.findall(r'__global__[^\(]*\(',content,re.MULTILINE)
	for x in global_declarations:
		x=(x.replace("__global__",""))
		x=(x.replace("void",""))
		x=re.sub("\(","",x);
		x=re.sub("[ ]*","",x);
		x=re.sub("[\n\t]*","",x)
		if v==1:
			print("appending to kernel_list+=["+x+"]");
		kernel_list.append(x);

	return kernel_list


####################################### 
def get_kernel_entry_ptx(ptx_path,v=0):
	content=read_from_file(ptx_path);
	content=content.replace("\n","");
	
	kernel_list=[]
	global_declarations=re.findall(r'.entry[^\(]*\(',content)
	for x in global_declarations:
		x=(x.replace(".entry",""))
		x=(x.replace("void",""))
		x=re.sub("\(","",x);
		x=re.sub("[ ]*","",x);
		if v==1:
			print("appending to kernel_list_ptx+=["+x+"]");
		kernel_list.append(x);

	return kernel_list

#######################################
def get_kernel_calls(content,kernel_list, verbose=0):
#	content=read_from_file(src_path);#.split("\n");
#	
#	verbose=1
	content=content.replace("\n","");
	
	kernel_call_list=[]
	associated_kernel_idx_list=[]
	# look for pattern: * <<< >>> ( * )	
	for i in range(len(kernel_list)):
		k=kernel_list[i]
		kernel_call_re=re.compile(k+"[ ]*<[ ]*<[ ]*<[^>]*>[^>]*>[^>]*>[ ]*\(]*[^;]*;",re.MULTILINE);##
		kernel_calls=re.findall(kernel_call_re,content);
		for x in kernel_calls:
#			x=re.sub(r'[ \n\t]',"",x);
			if verbose==1:
				print("kernel call found: ["+x+"]\n");

			kernel_call_list.append(x)
			associated_kernel_idx_list.append(i);
	return [kernel_call_list, associated_kernel_idx_list];

#######################################
def get_pragma_list(host_content):
	return []

#######################################
def replace_str_str_one_occurence(content, current_call, replaced_call, verbose=0):
	content=content.replace("\n",NEW_LINE_REPLACEMENT);
#	content=re.sub(current_call,"//"+current_call+"\n"+replaced_call,content,re.MULTILINE)	
#	print("current",current_call)
	current_re=re.compile(re.escape(current_call));
	content=current_re.sub((replaced_call),content,1)
	content=content.replace(NEW_LINE_REPLACEMENT, "\n");
	return content;

#######################################
def separately_write_kernel_body_list(kernel_body_list,kernel_prefix,
		include_filename, verbose=0):

	n=len(kernel_body_list)
	# name of each kernel present in the kernel_body_list
	kernel_name_file_list=n*[""]
	for i in range(n):
		kernel=kernel_body_list[i]
		kernel_name=get_kernel_names_in_content(kernel)[0]	
		kernel=kernel.replace(NEW_LINE_REPLACEMENT,"\n");
#		file_name=str(kernel_prefix)+str(i)+".cu"
		file_name=str(kernel_prefix)+kernel_name+".cu"##

		kernel='\n#include "'+include_filename+'"\n//////////////////\n'+kernel
		write_str_to_file(kernel,file_name)
		if verbose==1:
			print("writing kernel ["+kernel_name+"] -> ["+file_name+"] ... ")	
	
		kernel_name_file_list[i]=[kernel_name, file_name]
	
	return kernel_name_file_list


#######################################
def cuda_to_ptx(kernel_name_file_list, arch, opt_level):
	n=len(kernel_name_file_list)
	kernel_ptx_name_list=n*[""]
	klaraptor_path=os.getenv("KLARAPTOR_PATH")
	for i in range(n):
		file=kernel_name_file_list[i][1]
		bin="%s/src/dp_builder/cu_to_ptx.sh "%(klaraptor_path)
		cmd=bin+file+ " "+arch+" "+str(opt_level);
		cmd=cmd+" > /dev/null 2>&1"
#		print_msg("["+cmd+"]");	
		os.system(cmd);
		kernel_ptx_name_list[i]=file.replace(".cu","_"+arch+".ptx");
	
	return kernel_ptx_name_list;

#######################################
# @kernel_cu_name : name of kernel in original cuda code
# @kernel_ptx_filename : name of the file including cc-specific ptx of the kernel
# @kernel_ptx_entry: name of the entry point for the ptx of the kernel
#
def prepare_ocelot_call(kernel_cu_name,kernel_ptx_filename, 
		kernel_ptx_entry, sm_arch="sm_20"):
	
	kernel_params=""
	
	prefix='''
	tracer_ptx_stream.open("KERNEL_PTX_FILENAME",ifstream::in);
	ocelot::registerPTXModule(tracer_ptx_stream, "KERNEL_CU_NAME" );
	ocelot::launch( "KERNEL_CU_NAME", "KERNEL_PTX_ENTRY" );
	ocelot::unregisterModule("KERNEL_CU_NAME" );
	tracer_ptx_stream.close();	

	trace::analyze_instructions_trace("kernel_KERNEL_CU_NAME_trace.txt");
	trace::analyze_memory_trace("kernel_KERNEL_CU_NAME_trace.txt");
	//trace::analyze_active_blocks_per_sm("KERNEL_CU_NAME","SM_ARCH");
	trace::print_sm_arch("SM_ARCH");

	ocelot::clearTraceGenerators();
	'''
	
	prefix=kernel_params+prefix
	prefix=prefix.replace("KERNEL_CU_NAME", kernel_cu_name);
	prefix=prefix.replace("KERNEL_PTX_FILENAME", kernel_ptx_filename);
	prefix=prefix.replace("KERNEL_PTX_ENTRY", kernel_ptx_entry);
	prefix=prefix.replace("SM_ARCH", sm_arch);

#	print(prefix);
	return prefix;

######################################

def setup_ocelot_call_list(kernel_name_file_list,
		kernel_ptx_name_list,sm_arch,
		verbose=0):
	n=len(kernel_name_file_list);
	ocelot_call_list=n*[""];
	for i in range(n):
		ptx_entry_list=get_kernel_entry_ptx(kernel_ptx_name_list[i],verbose)
		if len(ptx_entry_list)!=1:
			print("ERROR: there should be exactly ONE entry point in the ptx file!")
			exit(-1);
		ocelot_call_list[i]=prepare_ocelot_call(kernel_name_file_list[i][0], \
				kernel_ptx_name_list[i], ptx_entry_list[0], sm_arch);
	return ocelot_call_list;


#######################################

def get_ocelot_configure_string():
	config='''
	{
		ocelot: "ocelot",
		version: "",
		executive: {
			devices:                  ["emulated"],
			preferredISA:             "nvidia",
			optimizationLevel:        "full",
			reconvergenceMechanism:   "ipdom",
			defaultDeviceID:          0,
			required:                 False,
			asynchronousKernelLaunch: True,
			port:                     2011,
			host:                     "127.0.0.1",
			workerThreadLimit:        8,
			warpSize:                 32
			},
		optimizations: {
                subkernelSize:        10000,
                simplifyCFG:          True,
                structuralTransform:  False,
                predicateToSelect:    False,
                linearScanAllocation: False,
                mimdThreadScheduling: False,
                syncElimination:      False,
                hoistSpecialValues:   False
        }

	}
	'''
	return config


#######################################
def check_str_is_digit(s):
	digit_str=re.compile(r"[-+]?\d+(\.0*)?$")
	status=digit_str.match(s)

	if status!=None:
		return True;
	return False;

#######################################
def prepare_cuda_call_config(call_args_str, kernel_cu_name):

	args=call_args_str.split(",");
	n_args=len(args)
	if n_args<2:
		print("ERROR: less than 2 args for kernell call")
		exit(-1);
	
	if n_args>4:
		print("ERROR: greater than 4 args for kernell call")
		exit(-1);

	for i in range(n_args):
		args[i]=args[i].replace(" ","");
	
	grid_dim3=args[0];
	block_dim3=args[1];
	shared_mem_bytes="0";
	stream_id="0"

	if n_args>=3:
		shared_mem_bytes=args[2];
	
	if n_args==4:
		stream_id=args[3];

	if check_str_is_digit(shared_mem_bytes)==True:
		shared_mem_bytes="int("+shared_mem_bytes+")"

	config_str='''
	
		//// reset and pass ocelot trace generators.
		global_mem_coalesced_generator.reset_stats();
		global_inst_generator.reset_stats();
		ocelot::addTraceGenerator(global_mem_coalesced_generator);
		ocelot::addTraceGenerator(global_inst_generator);

	_griddim_init.x=GRID_DIM_X;
	_griddim_init.y=GRID_DIM_Y;
	_griddim_init.z=GRID_DIM_Z;

	_griddim_minimal.x=GRID_DIM_X;
	_griddim_minimal.y=GRID_DIM_Y;
	_griddim_minimal.z=GRID_DIM_Z;

	_blockdim_init.x=BLOCK_DIM_X;
	_blockdim_init.y=BLOCK_DIM_Y;
	_blockdim_init.z=BLOCK_DIM_Z;

	trace::analyze_active_blocks_per_program(_griddim_init, _blockdim_init);
	trace::compute_minimal_blocks(_griddim_init, _griddim_minimal, KERNEL_EMULATION_METHOD);

	//cout<<"x,y,z= KERNEL_CU_NAME"<<_griddim_minimal.x<< " "<<_griddim_minimal.y <<" "<<_griddim_minimal.z<<endl;
	
	cudaConfigureCall(_griddim_minimal, _blockdim_init, SHARED_MEM_SIZE, STREAM_ID);	
	//tracer_kernel_params_stream=fopen("kernel_KERNEL_CU_NAME_params.tmp","w");
	
	//sprintf(_default_output_str,
	//"%d\\\\n%d\\\\n%d\\\\n%d\\\\n%d\\\\n%d\\\\n%d\\\\n",
	//_griddim_init.x, _griddim_init.y, _griddim_init.z, 
	//_blockdim_init.x, _blockdim_init.y, _blockdim_init.z,
	//SHARED_MEM_SIZE);
	trace::print_kernel_params_to_file("KERNEL_CU_NAME",
	_griddim_init, _blockdim_init,	SHARED_MEM_SIZE, "SM_ARCH");

	//fprintf(tracer_kernel_params_stream, "%s", _default_output_str);
	//fclose(tracer_kernel_params_stream);
	'''

	config_str=config_str.replace("GRID_DIM_X", grid_dim3+".x");
	config_str=config_str.replace("GRID_DIM_Y", grid_dim3+".y");
	config_str=config_str.replace("GRID_DIM_Z", grid_dim3+".z");

	config_str=config_str.replace("BLOCK_DIM_X", block_dim3+".x");
	config_str=config_str.replace("BLOCK_DIM_Y", block_dim3+".y");
	config_str=config_str.replace("BLOCK_DIM_Z", block_dim3+".z");
	
	config_str=config_str.replace("SHARED_MEM_SIZE", str(shared_mem_bytes));
	config_str=config_str.replace("STREAM_ID", str(stream_id));
	config_str=config_str.replace("KERNEL_CU_NAME", kernel_cu_name);
	config_str=config_str.replace("SM_ARCH", arch);
#	print(config_str)
	return config_str


#######################################
def get_kernelname_from_kernelcall(kernel_call):
	k=kernel_call;
	k=re.sub(r'[ ]*<[ ]*<[ ]*<[ ]*',"<<<",k);
	k=re.sub(r'>[ ]*>[ ]*>[ ]*',">>>",k);
	return (k.split("<<<")[0].replace(" ",""))

#######################################
def prepare_kernel_call_parameters_v0(kernel_call,verbose=0):
	if verbose==1:
		print("preparing kernel call: ["+kernel_call+"]");
	k=kernel_call;
	k=re.sub(r'[^<]*<[ ]*<[ ]*<[ ]*',"<<<",k);
	k=re.sub(r'>[ ]*>[ ]*>[ ]*',">>>",k);

#	dimensions=k.split(">>>")[0].replace("<<<","").split(",");
	params=k.split(">>>")[1].replace(";","");
	call_args=k.split(">>>")[0].replace("<<<","");

	kernel_cu_name=get_kernelname_from_kernelcall(kernel_call)
	cuda_call_args_str=prepare_cuda_call_config(call_args, kernel_cu_name)	

	params=re.sub(r'[ ]*\(',"",params);
	params=re.sub(r'\)[ ]*',"",params);
	params=params.split(",");
	for i in range(len(params)):
		params[i]=re.sub(r'[ ]*',"",params[i])

	current_size_str="0";
	prefix='''
	cudaSetupArgument( &PARAMETER, sizeof(PARAMETER), CURRENT_SIZE_STR );'''
		
	cuda_args=[]
	
#	config_args='''
#	cudaConfigureCall( 
#		dim3( GRID_DIM_X, GRID_DIM_Y, GRID_DIM_Z), 
#		dim3( BLOCK_DIM_X, BLOCK_DIM_Y, BLOCK_DIM_Z ), 
#		SHARED_MEM_SIZE, 0
#	);
#	'''

#	cuda_args.append(config_args);
	for p in params:
		setup_args=prefix;
#		print("p",p)

		if check_str_is_digit(p)==True:
#			print("p is numeral",p)
			numeral_var="tmp_value_"+p.replace(".","_DOT_")
			setup_args="double " + numeral_var+"="+p+";\n";
			setup_args+=prefix
			p=numeral_var
		
#		p=numeral_var
		setup_args=setup_args.replace("PARAMETER",p);
		setup_args=setup_args.replace("CURRENT_SIZE_STR",current_size_str);
		cuda_args.append(setup_args);
		current_size_str+="+sizeof("+p+")";
	
	cuda_args_str=""
	for x in cuda_args:
		cuda_args_str+=x;
	
	cuda_args_str+=cuda_call_args_str
	return cuda_args_str;

#######################################
## init_args = ( *** );
## separated by commas inside.

def split_function_arguments(init_args):
	params=init_args
	## remove blank space
	params=re.sub(r'[ ]*',"",params);

	## remove left (
	params=re.sub(r'^[ ]*\(',"",params);
	
	## remove right );
	params=re.sub(r'\)[ ]*;',"",params);
	
	## find and replace commas inside paranethesis to avoid
	## the confusion after splitting by comma

	comma_replacement="__COMMA__"
	n_left=0;
	n_right=0;
	new_params=""
	for i in range(len(params)):
		c=params[i]
		if c=="(":
			n_left+=1
		if c==")":
			n_right+=1;

		if c=="," and n_left!=0:
			new_params+=comma_replacement;
		else:
			new_params+=c;

		if n_left==n_right!=0:
			n_left=n_right=0
	
	params=new_params

	## split by comma
	params=params.split(",");

	## putting back commas in place
#	params=params.replace(comma_replacement,",");

#	print("params["+params+"]")
#	return;
	for i in range(len(params)):
#		params[i]=re.sub(r'',"",params[i])
		params[i]=params[i].replace(comma_replacement,",");
	return params

#######################################
def prepare_kernel_call_parameters(kernel_call, kernel_opts, verbose=0):
	if verbose==1:
		print("preparing kernel call: ["+kernel_call+"]");
	k=kernel_call;
	k=re.sub(r'[^<]*<[ ]*<[ ]*<[ ]*',"<<<",k);
	k=re.sub(r'>[ ]*>[ ]*>[ ]*',">>>",k);

#	dimensions=k.split(">>>")[0].replace("<<<","").split(",");

	call_args=k.split(">>>")[0].replace("<<<","");

	kernel_cu_name=get_kernelname_from_kernelcall(kernel_call)
	cuda_call_args_str=prepare_cuda_call_config(call_args, kernel_cu_name)
	
	emulation_method=kernel_opts[0];
#	print(emulation_method)
	if emulation_method=="full" or emulation_method==-1:
		cuda_call_args_str=cuda_call_args_str.replace("KERNEL_EMULATION_METHOD","0");
	elif emulation_method=="fast":
		cuda_call_args_str=cuda_call_args_str.replace("KERNEL_EMULATION_METHOD","1");


	params=split_function_arguments(k.split(">>>")[1]);
	current_size_str="0";
	prefix='''
	cudaSetupArgument( &(PARAMETER), sizeof(PARAMETER), CURRENT_SIZE_STR );'''
	
	cuda_args=[]
	
#	config_args='''
#	cudaConfigureCall( 
#		dim3( GRID_DIM_X, GRID_DIM_Y, GRID_DIM_Z), 
#		dim3( BLOCK_DIM_X, BLOCK_DIM_Y, BLOCK_DIM_Z ), 
#		SHARED_MEM_SIZE, 0
#	);
#	'''

#	cuda_args.append(config_args);
	for p in params:
		setup_args=prefix;
#		print("p",p)

		if check_str_is_digit(p)==True:
#			print("p is numeral",p)
			numeral_var="tmp_value_"+p.replace(".","_DOT_")
			setup_args="double " + numeral_var+"="+p+";\n";
			setup_args+=prefix
			p=numeral_var
		
#		p=numeral_var
		setup_args=setup_args.replace("PARAMETER",p);
		setup_args=setup_args.replace("CURRENT_SIZE_STR",current_size_str);
		cuda_args.append(setup_args);
		current_size_str+="+sizeof("+p+")";
	
	cuda_args_str=""
	for x in cuda_args:
		cuda_args_str+=x;
	
	cuda_args_str+=cuda_call_args_str
	return cuda_args_str;


#######################################
def get_kernel_specific_opts(content, kernel_name):

	pragma_values=len(pragma_list)*[-1]
	for i in range(len(pragma_list)):
		p=pragma_list[i]
		name=p[0]
		pragma_str=re.compile(r"\#pragma[ ]*kernel_"+kernel_name+"_"+name+"_[^\n]*")
		opts=re.findall(pragma_str, content)
		## if it exists, pick the last pragma of the current category.
		if len(opts)!=0:
			value=opts[-1];
#			print(opts[-1])
			pragma_val_str=re.compile(r"\#pragma[ ]*kernel_"+kernel_name+"_"+name+"_")
			value=re.sub(pragma_val_str, '', value);
			for j in range(len(p[1])):
				if value.lower() == p[1][j]:
#					print(value)
					pragma_values[i]=value;

	return pragma_values
#######################################
def get_strlist_of_includes(content, verbose=1):

	include_list=re.findall(r'\#[ ]*include.*',content)

	include_str=""
	for x in include_list:
		include_str+=str(x)+"\n"
#	exit(-1)
	return include_str


def gen_tracer_streams_header(kernel_name_list):
	return ""

#gen_tracer_streams_def

#######################################
#def check_start_stop_pragmas(content):
#	start=re.findall("#pragma START_TRACING", content);
#	stop=re.findall("#pragma STOP_TRACING", content);
#	if len(start)==len(stop)==1:
#		return 0;
#	else:
#		return 1;

#######################################


def parse(host_path="original_host.cu", arch="sm_20", opt_level=3):
	verbose=0;
#		kernel_ptx_path="kernel.ptx"
	host_content=read_from_file(host_path)
	host_content=remove_comment_from_content(host_content)
#	host_content=re.sub(r"[ ][ ]*","[ ]",host_content)
	host_content=re.sub(r"\n[\n]*","\\n",host_content)
	host_content=re.sub(r",\n*",",",host_content)
#		write_str_to_file(host_content, result_path);

	[kernel_body_list,length_list]=get_kernel_body_cu(host_content)

	include_filename="common_includes.h"
	include_str=get_strlist_of_includes(host_content);
	include_str='''
#ifndef COMMON_INCLUDES_HEADER_
#define COMMON_INCLUDES_HEADER_
//////////////////////////////////
'''+include_str+'''
/////////////////////////////////
#endif'''
	write_str_to_file(include_str,"common_includes.h")

	# extracting cuda kernels, then, writing them to separate .cu files.
	# returns a list of [name, file] in the following way:
	#		name : name of the kernel, 
	#		file : corresponding .cu file
	kernel_name_file_list=separately_write_kernel_body_list(kernel_body_list,
			"kernel_",include_filename, verbose);
	

	# comment out the kernel body in the resulting code.
	# then, it can be compiled with clang and without nvcc.
#	for i in range(len(kernel_body_list)):
#		body_start_idx=host_content.find("__global__");
#		current_body_str=host_content[body_start_idx:body_start_idx+length_list[i]]
#		
#		#replace_str has a unique value for each kernel.
#		replace_str="_EMPTY_KERNEL_REPLACEMENT_"+str(i)+"_"+kernel_name_file_list[i][0]+"\n"
#		host_content=host_content.replace(current_body_str, replace_str)	
#	
#	# replacing the temporary string with real kernel
#	for i in range(len(kernel_body_list)):
#		body=kernel_body_list[i]
#		replace_str="_EMPTY_KERNEL_REPLACEMENT_"+str(i)+"_"+kernel_name_file_list[i][0]+"\n"##
#		
#		body=remove_comment_from_content(body)
#		new_body=body.replace(NEW_LINE_REPLACEMENT,"\n")	
#		new_body=remove_comment_from_content(new_body)
#		new_body=re.sub("^","//",new_body);
#		new_body=re.sub("\n","\n//",new_body);
#	
#		host_content=host_content.replace(replace_str, new_body+"\n");		
	
#	return;
	# converting separated cuda kernels into ptx files.
	kernel_ptx_name_list=cuda_to_ptx(kernel_name_file_list, arch, opt_level);
	
	# preparing ocelot launch parameters for generated ptx files.
#	ocelot_call_list=setup_ocelot_call_list(kernel_name_file_list, kernel_ptx_name_list, arch);
	
	kernel_name_list=len(kernel_name_file_list)*[None]
	for i in range(len(kernel_name_file_list)):
		kernel_name_list[i]=(kernel_name_file_list[i][0]);
	

	kernel_name_list_str=""
	for name in kernel_name_list:
		kernel_name_list_str+= name  + "\n"
	write_str_to_file(kernel_name_list_str,"kernel_name_list.tmp")
#	return;
	## settings are defined per kernel.
	## only the last occurence of a pragma for a specific kernel will be
  ## valid.
	kernel_opts=len(kernel_name_list)*[None]
	for i in range(len(kernel_name_list)):
		current_opts=get_kernel_specific_opts(host_content,kernel_name_list[i]);
		kernel_opts[i]=current_opts
#		print("opts->",current_opts)


#	# get list of kernel calls in the original host code
	[kernel_call_list,associated_kernel_idx_list]=get_kernel_calls(host_content, kernel_name_list,verbose)
#		
#	# preparing new kernel calls
#	new_kernel_calls=len(kernel_call_list)*[""]
#	for i in range(len(kernel_call_list)):
#		kernel_call=kernel_call_list[i]	
#		idx=associated_kernel_idx_list[i]
#		kernel_call_params=prepare_kernel_call_parameters(kernel_call,kernel_opts[idx], verbose);
#		new_kernel_calls[i]=(kernel_call_params);
#		
##		print(kernel_call_params);	
#	
#	# replacing current kernel calls with new ones
#	for i in range(len(kernel_call_list)):
#		current_call=kernel_call_list[i];
#		replaced_call=new_kernel_calls[i];
#		# adding ocelot launch parameters after kernel lauch parameters
#		replaced_call+="\n"+ocelot_call_list[associated_kernel_idx_list[i]];
#		## adding extra brackets to protect from unintended consequences.
#		replaced_call="{"+replaced_call+"}"
#
##		if i==len(kernel_call_list)-1:
##			replaced_call+="\n\n"
##			replaced_call+='system("python accumulate_traces.py");'
#	
#		host_content=replace_str_str_one_occurence(host_content, current_call,\
#				"//"+current_call+"\n"+replaced_call)
#		host_content=remove_comment_from_content(host_content)
#
#		
##		tracer_streams_def=gen_tracer_streams_header(kernel_name_list);
##		write_str_to_file(tracer_streams_def, "tracer_streams.h");
#	
#	host_content=header_files+host_content;		
#
#### commenting the main function -> not needed anymore.
##	host_content=re.sub(r'int[ ]*main[ ]*\([^\)]*\)[ ]*[\n]*[\t]*{[^}]*}','',host_content);	
##	host_content=re.sub(r'int[ ]*main[ ]*\([^\)]*\)','',host_content);
#
#	trace_accumulate_cmd='''
#	system("python accumulate_traces.py");
#	//trace::accumulate_results();
#	'''
#
#	trace_accumulate_pragma='''#pragma STOP_TRACING'''
#	host_content=re.sub(r"\n[\n]*","\\n",host_content)
#	host_content=re.sub(trace_accumulate_pragma,trace_accumulate_cmd,host_content)
#
#	# writing result back to file.
#	write_str_to_file(host_content,result_path);
#
#	default_kernel_params_str=""
#
#	for name in kernel_name_list:
#		write_str_to_file(default_kernel_params_str,"kernel_"+name+"_params.tmp");
#	
#	write_str_to_file(default_kernel_params_str,"kernel_default_params.tmp");
#	write_str_to_file(get_ocelot_configure_string(),"configure.ocelot");
#
#
	kernel_name_list_str=""
	for name in kernel_name_list:
		kernel_name_list_str+= name  + "\n"
	write_str_to_file(kernel_name_list_str,"complete_kernel_name_list.tmp")


	kernel_name_list_str=""
	for name in kernel_name_list:
		kernel_name_list_str+= name  + "\n"
	write_str_to_file(kernel_name_list_str,"kernel_name_list.tmp")

	called_kernel_name_list=[]
	for i in range(len(kernel_call_list)):
		current_call=kernel_call_list[i];
		kernel_name=re.sub(r'[ ]*<[ ]*<[ ]*<.*',"",current_call)
		if kernel_name not in called_kernel_name_list:
			called_kernel_name_list.append(kernel_name)
	
	
	kernel_name_list_str=""
	for name in called_kernel_name_list:
		kernel_name_list_str+= name  + "\n"
	write_str_to_file(kernel_name_list_str,"kernel_name_list.tmp")

	write_str_to_file(arch.replace("sm_",""),"sm_arch.config");

#######################################
def main():

	global arch
	arch="sm_20"
	argc=len(sys.argv)
	src_path=""
	result_path=""
	opt_level=3;

	if argc<3:
		print("args: [1]:src_path, [2]:arch (sm_cc), [3]:opt_level (0,1,2,3)")
		exit(-1);
	else:
		src_path=sys.argv[1]	
		arch=sys.argv[2]
	if argc>3:
		opt_level=int(sys.argv[3])
	
	verbose=0;
	if verbose:
		print_msg("[@parser.py][src=%s, arch=%s, opt_level=%d]"%(src_path, arch, opt_level));
	parse(src_path, arch, opt_level)


#######################################

if __name__ == "__main__":
	main()
