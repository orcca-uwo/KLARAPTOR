import os
import sys
import subprocess


klaraptor_path=os.environ['KLARAPTOR_PATH'];
MAKE_SCRIPT=("%s/trace_analysis/%s")%(klaraptor_path, "klaraptor_use.sh")
KERNEL_NAME_LIST="kernel_name_list.tmp"
#######################################
CONFIG_FILE_PARAMS_LIST=[]
CONFIG_FILE_PARAMS_LIST+=["CUDA_SRC"]
CONFIG_FILE_PARAMS_LIST+=["SM_ARCH"]
CONFIG_FILE_PARAMS_LIST+=["DEVICE_NAME"]
CONFIG_FILE_PARAMS_LIST+=["OPT_LEVEL"]
CONFIG_FILE_PARAMS_LIST+=["LOWER_BOUND"]
CONFIG_FILE_PARAMS_LIST+=["UPPER_BOUND"]
CONFIG_FILE_PARAMS_LIST+=["PROBLEM_DIM"]


#######################################
## [prefix=kernel/suffix]
KERNEL_SETTINGS_LIST=[]
KERNEL_SETTINGS_LIST+=["constraints"]
KERNEL_SETTINGS_LIST+=["data_profile"]

#######################################
def get_input(msg=""):
	return raw_input(msg);

#######################################
def print_args_help():
	help_str="args:\n"
	help_str+=" [1]:--conf config_file_path\n"
	help_str+=" [1]:--opt config_file_path\n"
	help_str+=" [1]:--clean"
	print(help_str)


#######################################
def exit_success():
	exit(0);

#######################################
def exit_failure():
	exit(-1);

#######################################
def exit_msg(msg, code):
	print("[EXITING] ... ["+str(msg)+"]");
	exit(code);

#######################################
def read_content_from_file(path):
	f=open(path,"r");
	content=f.read();
	f.close();
	return content;


#######################################
def write_content_to_file(content, path):
	f=open(path,"w");
	for x in content:
		f.write(x+"\n")
	f.close()

#######################################
def configure(path):
	print("[configuring opt profile][%s]"%(path));
	if os.path.exists(path):
		print("[WARNING]: The config file already exists!")
		msg=("[overwrite it? (default=no (n), yes (y))] ... ")
		overwrite=get_input(msg).lower()

		if overwrite not in ["yes", "y"]:
			return 0;

	
	cuda_src=""
	tmp_conf_params=[]
	for x in CONFIG_FILE_PARAMS_LIST:
		value_entered=0;
		while(value_entered==0):
			value=get_input("[Please enter value of [%s]] ... "%(x));
			if value:
				line="[%s=%s]"%(x,str(value))
				tmp_conf_params+=[line]
				value_entered=1;
				## find name of the source file		
				if x=="CUDA_SRC":
						cuda_src=value;

	###################################

	###################################
	os.system("python %s/trace_analysis/extract_kernel_name_list.py %s"%(klaraptor_path,cuda_src))
	## reading the list of kernels
	kernel_list=read_content_from_file(KERNEL_NAME_LIST).split("\n")[:-1]
	###################################
	for kernel_setting in KERNEL_SETTINGS_LIST:
		for k in kernel_list:
			start_id="[kernel_%s_%s_begin]"%(k, kernel_setting)
			stop_id="[kernel_%s_%s_end]"%(k, kernel_setting)
			hrule=35*"#"
			line="\n%s\n%s\n   ### please add [%s] for kernel [%s] here.\n%s\n%s\n"\
					%(hrule, start_id, kernel_setting, k, stop_id, hrule);
			tmp_conf_params+=[line]
	###################################

	write_content_to_file(tmp_conf_params, path);
	return 0;


#######################################
def parse_config_file(path):
	content=read_content_from_file(path);
	content=content.split("\n")[:-1]
	minimum_len=len(CONFIG_FILE_PARAMS_LIST);
	current_len=len(content)
	if current_len<minimum_len:
		print("[ERROR: insufficient number of parameters are listed in conf file]")
		exit_failure();
	
	param_list=[]
	for i in range(minimum_len):
		x=content[i]	
		[param, val]=x.split("=")[0:2]
		param=param.replace("[","");
		val=val.replace("]","")	
		if param in CONFIG_FILE_PARAMS_LIST:
			param_list+=[val];
	
#	print(CONFIG_FILE_PARAMS_LIST)
#	print(param_list)
	return param_list;

#######################################
def optimize(path):
	print("[optimizing for opt profile][%s]"%(path));
	result=parse_config_file(path)
#	print(result)

	cmd=[]
	cmd+=["/bin/bash"]
	cmd+=[(MAKE_SCRIPT)]
	n=len(result);
	for i in range(n):
		param=CONFIG_FILE_PARAMS_LIST[i];
		val=result[i];	
		arg=param.lower();
		arg=arg.replace("_","-");
		arg="--%s=%s"%(arg,val)
		cmd+=[arg]
#	print(cmd)
	subprocess.call(cmd[:])
	exit_success()


#######################################
def clean():
	cmd=[]
	cmd+=["/bin/bash"]
	cmd+=[(MAKE_SCRIPT)]
	cmd+=["--clean"];
	subprocess.call(cmd[:])
	exit_success()

#######################################
def main():
	verbose=1;
	argc=len(sys.argv)

	if argc<2:
		print_args_help()
		exit_failure()

	args=sys.argv[:]
	task=args[1]

	if task=="--clean":
		if verbose:
			print("CLEAN TMP FILES")
		stat=clean()
		exit_msg("config", stat);
		exit_success()

	if task=="--conf":
		if argc>=3:
			config_file_path=args[2];
			if verbose:
				print("CREATING CONFIG FILE", config_file_path)
			stat=configure(config_file_path)
			exit_msg("config", stat);
			exit_success()

	if task=="--opt":	
		if argc>=3:
			config_file_path=args[2];
			if verbose:
				print("OPT FROM CONFIG FILE", config_file_path)
			stat=optimize(config_file_path)
			exit_msg("optimize", stat);
			exit_success()

	print_args_help()
	exit_failure()

#######################################
if __name__=="__main__":
	main()
