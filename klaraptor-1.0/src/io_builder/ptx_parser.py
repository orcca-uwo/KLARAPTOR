 
import os
import sys
import re
import random
import time
import binascii

arch=""

#### launch_params = grid_dim, block_dim, n_dynamic_shared_mem_bytes, stream_id
#######################################
NEW_LINE_REPLACEMENT="____EMPTY___SPACE___"+str(random.seed(time.time()))

#######################################
include_str_prefix='''
#ifndef COMMON_INCLUDES_HEADER_
#define COMMON_INCLUDES_HEADER_
//////////////////////////////////
'''

#######################################
include_str_suffix='''
/////////////////////////////////
#endif'''


#######################################
## TODO: check for multiple definitions of the same pragma.

def parse_pragmas(content):
	#content=read_content_from_file(path)
	prefix="kernel_info"
	param_list=["size_param_idx", "dim"]
	unused_attr=" __attribute__((used)) "
	
	for p in param_list:
		prefix_pattern=re.compile(r"^[ ]*\#pragma[ ]*"+prefix+"_"+p+"_",re.MULTILINE);
		pattern=re.compile(r"^[ ]*\#pragma[ ]*"+prefix+"_"+p+"_.*", re.MULTILINE);
		res=re.findall(pattern, content);
		for x in res:
#			print(x)
			s=re.sub(prefix_pattern, "", x)
			s=re.sub("=","",s)
			s=re.sub(r'[ ][ ]*',' ',s);
			[kernel_name, val]=s.split(" ")	
			val=val.replace(";","");
#			print(kernel_name, val)
			s="const int kernel_info_";
			s+=p+"_"+kernel_name+unused_attr
			s+=" = "+str(val)+";"
#			print("CHANGED "+ x+"--->" + s);
			content=re.sub(x, s, content);
	return content;
#######################################
def get_element_attr_in_dict(input_dict, element_key, element_attr):
	for key in input_dict.keys():
		if key==element_key:
			return input_dict[key][element_attr];	
	return None;

#######################################
def set_element_attr_in_dict(input_dict, element_key, element_attr, value):
	for key in input_dict.keys():
		if key==element_key:
			input_dict[key][element_attr]=value;
			return;
	
	print("FAILED TO FIND THE ELEMENT IN DICT");
	return None;

#######################################
## corner cases where there are letters like 0x, 0f, 0e in the number
## do not work.
def check_str_is_digit(s):
	digit_str=re.compile(r"[-+]?\d+(\.0*)?$")
	status=digit_str.match(s)

	if status!=None:
		return True;
	return False;

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
def get_kernel_body_cu_dict(content,v=0):
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
		kernels[i]="__global__"+kernels[i]		
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
		if result!="":
			end_point_list.append(prev)
			kernel_list.append(result.replace(NEW_LINE_REPLACEMENT,"\n"))

	d={}
	n=len(kernels);
	for i in range(len(kernels)):
		k=kernels[i];
		name=get_kernel_names_in_content(k)[0];
		body=kernel_list[i];
		end_point=end_point_list[i];
#		print(k, name, body, end_point)
		d[name]={};
		d[name]["idx"]=i;
		d[name]["name"]=name;
		d[name]["body"]=body;
		d[name]["end_point"]=end_point;
		d[name]["cu_path"]="";
		d[name]["ptx_path"]="";
		d[name]["launch_params"]="";
		d[name]["kernel_params"]="";
		d[name]["opts"]=[]
		d[name]["call_list"]=[]
		###store signature as a list of types of variables.
		d[name]["signature"]=[]

	
#	for k in d.keys():
#		print(k,d[k])
#	get_element_attr_by_idx(d, 0, "name");
	
	return d;
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
def get_call_list_for_kernel(content, kernel_name, verbose=0):
#	content=read_from_file(src_path);#.split("\n");
#	verbose=1
	content=content.replace("\n","");
	
	kernel_list=[kernel_name]
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
				print("\nkernel call found: ["+x+"]");

			kernel_call_list.append(x)
			associated_kernel_idx_list.append(i);
	return kernel_call_list

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
def separately_write_kernel_body(kernel_body,kernel_prefix,
		include_filename, verbose=0):

	kernel_body_list=[kernel_body]
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
def cuda_to_ptx(kernel_name_file_list, arch):
	n=len(kernel_name_file_list)
	kernel_ptx_name_list=n*[""]
	for i in range(n):
		file=kernel_name_file_list[i][1]

		cmd="./cu_to_ptx.sh "+file+ " "+arch;	
		print(cmd)
		os.system(cmd);
		kernel_ptx_name_list[i]=file.replace(".cu","_"+arch+".ptx");
	
	return kernel_ptx_name_list;

#######################################
def set_launch_params(call_args_str, kernel_cu_name):
#	print(call_args_str)
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
 ////////////////////////////////////////////////////////////////////////
 ////////// WARNING: AUTOMATICALLY ANNOTATED REGION BEGINS HERE /////////
 ////////////////////////////////////////////////////////////////////////
	
 NEW_VAR_DECLARATIONS

 char kernel_KERNEL_CU_NAME_CALL_IDX_name[] = "kernel_KERNEL_CU_NAME_SM_ARCH";
 
 //launch_params: 3 for grid_dim, 3 for block_dim, 1 for dynamic_shared_mem_bytes;
 int kernel_KERNEL_CU_NAME_SM_ARCH_CALL_IDX_launch_params[8];
 //3 for griddim; 3 for blockdim; 1 for shared_mem_bytes; 1 
 set_kernel_launch_params(kernel_KERNEL_CU_NAME_SM_ARCH_CALL_IDX_launch_params, DIM3_GRID_DIM, DIM3_BLOCK_DIM);
 
 void * kernel_KERNEL_CU_NAME_SM_ARCH_CALL_IDX_kernel_params[]={KERNEL_PARAMS_CSV};
 
 kernel_invoker(kernel_KERNEL_CU_NAME_CALL_IDX_name,\
 kernel_KERNEL_CU_NAME_SM_ARCH_CALL_IDX_launch_params,\
 kernel_KERNEL_CU_NAME_SM_ARCH_CALL_IDX_kernel_params);
 
 ////////////////////////////////////////////////////////////////////////
 ////////// WARNING: AUTOMATICALLY ANNOTATED REGION ENDS HERE ///////////
 ////////////////////////////////////////////////////////////////////////
	'''

	config_str=config_str.replace("DIM3_GRID_DIM", grid_dim3);
	config_str=config_str.replace("DIM3_BLOCK_DIM", block_dim3);
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
def get_random_val_prefix():
	#n=random.randint(100, 1000000);
	## using urandom to be more authentic
	n = os.urandom(4)
	print(n)
	n = binascii.hexlify(n);
	n = int(n,16);

	return ("__tmp_rand_val_%d"%(n))
#######################################

def prepare_kernel_call_replacement(kernel_call, kernel_signature, call_idx, verbose=0):
	if verbose==1:
		print("preparing kernel call: ["+kernel_call+"]");
	k=kernel_call;
	k=re.sub(r'[^<]*<[ ]*<[ ]*<[ ]*',"<<<",k);
	k=re.sub(r'>[ ]*>[ ]*>[ ]*',">>>",k);

	launch_params=k.split(">>>")[0].replace("<<<","");
	kernel_params=split_function_arguments(k.split(">>>")[1]);	
	
	#### setting launch params.
#	print("[launch_params]:[" + launch_params +"]")
	kernel_cu_name=get_kernelname_from_kernelcall(kernel_call)
	launch_params_str=set_launch_params(launch_params, kernel_cu_name)	
	
#	print("[kernel_params]:[" + str(kernel_params) +"]")	
	#### setting kernel params.
	emulation_method=-1;
	current_size_str="0";	

	## store new declarations (constant to variable) in this list.
	new_decl_list=[]
	kernel_params_str="";
	n_kernel_params=len(kernel_params);
	for i in range(n_kernel_params):
		p=kernel_params[i];	
		current_param_type = kernel_signature[i]
		## assign the constant to a variable.
		if check_str_is_digit(p)==True:
#			print("p is numeral",p)
			new_var_prefix=get_random_val_prefix();
			new_var =new_var_prefix+"_"+p.replace(".","_DOT_") + "_CALL_IDX"
			new_var_decl =(current_param_type + new_var + "=(%s)"+p+";")%(current_param_type);
			new_decl_list.append(new_var_decl);
			p=new_var

		kernel_params_str+="&"+p;
		if i!=n_kernel_params-1:
			kernel_params_str+=" , ";
	
	new_decl_str = "";
	for d in new_decl_list:	
		new_decl_str+=d;
	
	result_str = launch_params_str;	
	## in case a call is repeated multiple times;	
	result_str =  result_str.replace("NEW_VAR_DECLARATIONS", new_decl_str);
	#print("kernel_params", kernel_params_str);
	result_str =  result_str.replace("KERNEL_PARAMS_CSV", kernel_params_str);
	result_str =  result_str.replace("CALL_IDX", str(call_idx));
	#print("result_str", result_str)
	
	return result_str;


#######################################
def get_strlist_of_includes(content, verbose=1):

	include_list=re.findall(r'\#[ ]*include.*',content)

	include_str=""
	for x in include_list:
		include_str+=str(x)+"\n"
#	exit(-1)
	return include_str

#######################################
def linearize_string(s):
	s=s.replace("\n",NEW_LINE_REPLACEMENT);
	return s;

#######################################
def normalize_string(s):
	s=s.replace(NEW_LINE_REPLACEMENT, "\n");
	return s;


#######################################
def get_kernel_signature(kernel_body):
	signature=""
#	print("[Extracting kernel signature]...")
	s=kernel_body
	s=re.findall('__global__[^\)]*\)',s)[0];
	s=re.sub('__global__[^\(]*\(',"",s);
	s=re.sub(',[ ]*',",",s);
	s=re.sub('[ ]*,',",",s);
	if "(" in s:
		print("[ERROR: unexpected ( in get_kernel_signature");
		exit(-1)

	type_list=[]
	s=s.split(",")
	for i in range(len(s)-1):
		token=s[i]
		if ")" in token:
			print("[ERROR: unexpected ) in get_kernel_signature");
			exit(-1)

	for token in s:
#		print(token)
		current_type="void *"
		if "*" in token:
			type=re.findall(r'[^\*]*[\*]*',token)
			if len(type)>0:
				current_type=type[0]
#				print("current", current_type)
		else:
			current_type=token.split(" ")[0]
#			print("current_type", current_type)
		type_list.append(current_type+" ")
	
	signature=type_list[:]
#	print("[Extracting kernel signature]..."+str(signature))	
	return signature

#######################################
def parse(host_path="original_host.cu", arch="sm_20"):
	verbose=0;
#		kernel_ptx_path="kernel.ptx"
	host_content=read_from_file(host_path)
	host_content=parse_pragmas(host_content)
	host_content=remove_comment_from_content(host_content)
#	host_content=re.sub(r"[ ][ ]*","[ ]",host_content)
	host_content=re.sub(r"\n[\n]*","\\n",host_content)
	host_content=re.sub(r",\n*",",",host_content)
	

	kernel_lookup=get_kernel_body_cu_dict(host_content)
	n_kernels_in_dict=len(kernel_lookup.keys());

	include_filename="common_includes.h"
	include_str=get_strlist_of_includes(host_content);
	common_include_str=include_str_prefix + include_str + include_str_suffix
	write_str_to_file(common_include_str,"common_includes.h")
		
	
	# extracting the body of cuda kernels, then, write them to separate .cu files.
	for name in kernel_lookup.keys():
		body=get_element_attr_in_dict(kernel_lookup, name, "body");
		cu_path=separately_write_kernel_body(body, "kernel_", include_filename, verbose);	
		set_element_attr_in_dict(kernel_lookup, name, "cu_path", cu_path[0]);
		signature=get_kernel_signature(body)
		set_element_attr_in_dict(kernel_lookup, name, "signature", signature);
	

	# comment out the kernel body in the resulting code.
	# then, it can be compiled with clang and without nvcc.
	### replacing each kernel body with a placeholder
	commented_kernel_str_template="_EMPTY_KERNEL_REPLACEMENT_KERNEL_IDX_KERNEL_NAME\n"
	for name in kernel_lookup.keys():
#		body_start_idx=host_content.find("__global__");
#		body_end_idx=get_element_attr_in_dict(kernel_lookup, name, "end_point");
#		current_body_str=host_content[body_start_idx:body_start_idx+body_end_idx]
		current_body_str = get_element_attr_in_dict(kernel_lookup, name, "body");
		### replace the current_body_str with placeholder
#		print("current\n", current_body_str);

#		continue;
#		#replace_str has a unique value for each kernel.
		idx=get_element_attr_in_dict(kernel_lookup, name, "idx");	
		commented_kernel=commented_kernel_str_template;
		commented_kernel=commented_kernel.replace("KERNEL_IDX", str(idx));
		commented_kernel=commented_kernel.replace("KERNEL_NAME", str(name));
		host_content = host_content.replace(current_body_str,commented_kernel);

#		### commenting the body of kernel and setting it as the new body.
		new_body=current_body_str.replace(NEW_LINE_REPLACEMENT,"\n")	
##		new_body=remove_comment_from_content(new_body)
		new_body=re.sub("^","//",new_body);
		new_body=re.sub("\n","\n//",new_body);
#		print("new_body (commented)\n", new_body)
		host_content=host_content.replace(commented_kernel, new_body);	


	# converting separated cuda kernels into ptx files.
	#	kernel_ptx_name_list=cuda_to_ptx(kernel_name_file_list, arch);

	# preparing ocelot launch parameters for generated ptx files.
	#	ocelot_call_list=setup_ocelot_call_list(kernel_name_file_list, kernel_ptx_name_list, arch);
	# kernel_name_list=len(kernel_name_file_list)*[None]	

	for name in kernel_lookup.keys():
		call_list=get_call_list_for_kernel(host_content, name, verbose)
		set_element_attr_in_dict(kernel_lookup, name, "call_list", call_list);

		if len(call_list)==0:
#			print("No kernel calls replaced for ["+name+"]");
			continue;
		if verbose:
			print("Preparing replacement calls for kernel ["+name+"]");
		signature=get_element_attr_in_dict(kernel_lookup, name, "signature");
		for call_idx in range(len(call_list)):
			c=call_list[call_idx];
			new_call=prepare_kernel_call_replacement(c, signature, call_idx, verbose);
#			print("new_call")
#			print(new_call)
			host_content=replace_str_str_one_occurence(host_content, c, new_call);


	new_include_str=include_str+'''
///////////////////////////////////////
/////// AUTOMATICALLY ANNOTATED ///////
///////////////////////////////////////
#include "kernel_invoker.h"
///////////////////////////////////////
///////////////////////////////////////
'''
	new_include_str = new_include_str.replace("\n",NEW_LINE_REPLACEMENT)
	include_str_mod = include_str.replace("\n",NEW_LINE_REPLACEMENT) 
	host_content=replace_str_str_one_occurence(host_content, include_str_mod, new_include_str)
#	print(host_content);	
	
	#### list of all kernels in the original source.
	kernel_name_list_str=""
	for name in kernel_lookup.keys():
		kernel_name_list_str+= name  + "\n"
	write_str_to_file(kernel_name_list_str,"complete_kernel_name_list.tmp")

	#### list of kernels that will be called the original source.
	kernel_name_list_str=""
	for name in kernel_lookup.keys():
		call_list=get_element_attr_in_dict(kernel_lookup, name, "call_list");
#		print(name, call_list);
		if len(call_list)!=0:	
			kernel_name_list_str+= name  + "\n"
	write_str_to_file(kernel_name_list_str,"kernel_name_list.tmp")

	dest_path=host_path.replace(".cu","_driver_call.cu");
	write_str_to_file(host_content, dest_path);	

	write_str_to_file(arch.replace("sm_",""),"sm_arch.config");

#######################################
def main():

	global arch
#	arch="sm_20"
#	argc=len(sys.argv)
#	src_path=""
#	result_path=""
	
	arch="sm_30"
	argc=len(sys.argv)
#	src_path="src.cu"
#	result_path=""
	if argc==3:
		src_path=sys.argv[1]
		arch=sys.argv[2]
		parse(src_path, arch)
	else:
		print("args: [1]:src_path, [2]:arch (sm_cc)")

#######################################

if __name__ == "__main__":
	main()
