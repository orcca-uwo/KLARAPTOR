import re
import os
import sys


param_list=["Blocks","shared_mem_bytes_total",\
		"Comp_insts",\
		"Uncoal_Mem_insts",\
		"Coal_Mem_insts",\
		"Synch_insts",\
		"Coal_per_mw",\
		"Uncoal_per_mw","Mem_LD"]
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

	template_path="../../src/mcwp/rational_program.c"
	kernel_name_list=read_from_file("kernel_name_list.tmp").split("\n")[:-1];
	for k in kernel_name_list:
		
		include_str=""
		for p in param_list:
			target="kernel_"+k+"_"+p+"_"+device+".h"
			include_str+="#include "+'"'+target+'"\n'
		template=read_from_file(template_path)
		
		template=template.replace('"INCLUDE_FILES_REPLACEMENT"',include_str)
		template=template.replace("KERNEL_NAME",k)

		#kernel_nvar=read_from_file("kernel_"+k+".nvar")
		#template=template.replace("KERNEL_N_VAR",kernel_nvar)

		dest="rational_program_"+k+".c"
		write_str_to_file(template,dest)
		print("rational program written to ["+dest+"]\n")

#######################################

if __name__=="__main__":
	main()
