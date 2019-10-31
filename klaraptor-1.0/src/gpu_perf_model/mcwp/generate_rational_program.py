
import re
import os
import sys

#################################################
device_params_list=["Issue_cycles",\
		"Mem_bandwidth",\
		"Mem_LD",\
		"Departure_del_coal",\
		"Active_SMs",\
		"Freq",\
		"Load_bytes_per_warp",\
		"Compute_Capability"]
#################################################
ptx_params_list=["registers","shared_mem_bytes_static"];
#################################################
profiling_params_list=["Blocks","shared_mem_bytes_total",\
		"Comp_insts",\
		"Coal_Mem_insts",\
		"Coal_per_mw",\
		"dynamic_Mem_LD","Active_warps_per_SM"]	
#################################################
#profiling_params_list=[\
#"Total_insts",\
#"Comp_insts",\
#"Mem_insts",\
#"dynamic_Mem_LD",\
#"Active_blocks_per_SM",\
#"Blocks",\
#"n_warps",\
#"Active_warps_per_SM",\

#################################################
profiling_params_list=[\
"Comp_insts",\
"Mem_insts",\
"dynamic_Mem_LD",\
"shared_mem_bytes_dynamic"]


param_list=["kernel_info"]
param_list+=device_params_list
param_list+=ptx_params_list
param_list+=profiling_params_list
#"Active_blocks_per_SM"]
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
def main():	
	
	device_path="current_device.tmp"

	if os.path.exists(device_path) ==False:
		print("ERROR: could not find ["+device_path+"]");
		exit(-1)

	device=read_from_file(device_path).split("\n")[0]

	klaraptor_path=os.getenv("KLARAPTOR_PATH")
	template_path="%s/src/gpu_perf_model/mcwp/rational_program.c"%(klaraptor_path)
	kernel_name_list=read_from_file("kernel_name_list.tmp").split("\n")[:-1];
	for k in kernel_name_list:
		
		include_str=""
		for p in param_list:
			target="kernel_"+k+"_"+p+"_"+device+".h"
			include_str+="#include "+'"'+target+'"\n'
		template=read_from_file(template_path)
		
		template=template.replace('//"INCLUDE_FILES_REPLACEMENT"',include_str)
		template=template.replace("KERNEL_NAME",k)

		#kernel_nvar=read_from_file("kernel_"+k+".nvar")
		#template=template.replace("KERNEL_N_VAR",kernel_nvar)

		dest="driver_program_"+k+".c"
		write_str_to_file(template,dest)
		verbose=0;
		if verbose:
			print("[driver program for kernel [%s]->[%s]]"%(k, dest));

#######################################

if __name__=="__main__":
	main()
