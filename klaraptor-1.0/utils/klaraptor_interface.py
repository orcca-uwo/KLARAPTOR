import os
import sys
import subprocess
import re
import math
import time

klaraptor_path=os.environ['KLARAPTOR_PATH'];
MAKE_SCRIPT=("%s/utils/%s")%(klaraptor_path, "klaraptor_use.sh")
KERNEL_NAME_LIST="kernel_name_list.tmp"
hrule=66*"="

EXIT_SUCCESS=0;
EXIT_FAILURE=1;
#######################################
CONFIG_FILE_PARAMS_LIST=[]
CONFIG_FILE_PARAMS_LIST+=["CUDA_SRC"]     #0
CONFIG_FILE_PARAMS_LIST+=["SM_ARCH"]      #1
CONFIG_FILE_PARAMS_LIST+=["DEVICE_NAME"]  #2
CONFIG_FILE_PARAMS_LIST+=["OPT_LEVEL"]    #3
CONFIG_FILE_PARAMS_LIST+=["LOWER_BOUND"]  #4
CONFIG_FILE_PARAMS_LIST+=["UPPER_BOUND"]  #5
CONFIG_FILE_PARAMS_LIST+=["PROBLEM_DIM"]  #6


#######################################
## [prefix=kernel/suffix]
KERNEL_SETTINGS_LIST=[]
KERNEL_SETTINGS_LIST+=["constraints"]
KERNEL_SETTINGS_LIST+=["data_profile"]


SIZE_LABEL="N"
DIM_LABELS=["bx","by","bz"]

#######################################
def get_input(msg=""):
	return raw_input(msg);

#######################################
def print_args_help():
	help_str="args:\n"
	help_str+=" [1]:--conf config_file_path\n"
	help_str+=" [1]:--opt config_file_path\n"
	help_str+=" [1]:--perf-check config_file_path N_EXTRA_POINTS\n"
	help_str+=" [1]:--clean"
	print(help_str)


#######################################
def exit_success():
	exit(EXIT_SUCCESS);

#######################################
def exit_failure():
	exit(EXIT_FAILURE);

#######################################
def exit_msg(msg, code):
	if code==EXIT_SUCCESS:
		print("[%s]...[EXIT_SUCCESS]"%(str(msg)));
	else:
		print("[%s]...[EXIT_FAILURE]"%(str(msg)));	
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
	return EXIT_SUCCESS;

#######################################
def configure(path):
	print("[configuring opt profile][%s]"%(path));
	print(hrule)
	if os.path.exists(path):
		print("[WARNING]: The config file already exists!")
		msg=("[overwrite it? (default=no (n), yes (y))] ... ")
		overwrite=get_input(msg).lower()

		if overwrite not in ["yes", "y"]:
			return EXIT_SUCCESS;

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
	os.system("python %s/utils/extract_kernel_name_list.py %s"%(klaraptor_path,cuda_src))
	## reading the list of kernels
	kernel_list=read_content_from_file(KERNEL_NAME_LIST).split("\n")[:-1]
#	###################################
#	for kernel_setting in KERNEL_SETTINGS_LIST:
#		for k in kernel_list:
#			start_id="[kernel_%s_%s_begin]"%(k, kernel_setting)
#			stop_id="[kernel_%s_%s_end]"%(k, kernel_setting)	
#			line="\n%s\n%s\n   ### please add [%s] for kernel [%s] here.\n%s\n%s\n"\
#					%(hrule, start_id, kernel_setting, k, stop_id, hrule);
#			tmp_conf_params+=[line]
#	###################################

	write_content_to_file(tmp_conf_params, path);
	return EXIT_SUCCESS;


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
#	print("[optimizing for opt profile][%s]"%(path));
	print(hrule)
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
	
	t_start = time.time()	
	subprocess.call(cmd[:])
	t_end = time.time()	
	t_opt="%.3f (s)"%(t_end-t_start);
	write_content_to_file([t_opt], "klaraptor_opt_time.tmp");
	print("klaraptor_opt_time=%s"%(t_opt))
	return EXIT_SUCCESS;	


#######################################
def run_cmd_with_buffer(cmd):
#	print(cmd)
	p=subprocess.Popen(cmd,stdout=subprocess.PIPE)
	content=p.stdout.read();
#	print(content)
#	print(p.stdout.read());
#	exit (-1);
	return content; 

#######################################
def progress_percent(idx, n):
	idx=idx+1;
	p=int(100.0*(idx*1.0)/(n*1.0))
	sys.stdout.write('\r')
	delim=2;
	size=((p+delim-1)/delim);
	padding=(100/delim)-size;
	line="["+size*"."+padding*" "+"]"
	sys.stdout.write(line)
	sys.stdout.write("(%02d %%)"%(p))
	sys.stdout.flush()
#######################################
## return a list of lists 
## (the latter including lines for each run of the binary with one of the args).
def run_binary_with_arglist(binary_name, input_arg_list, n_repeat):
	
	result_list=[]
	pwd=os.getcwd();
	cmd=[pwd+"/"+binary_name]
	print("[running for [%s]]"%(binary_name));

	n_list=input_arg_list[0]
	point_list=input_arg_list[1]
	
	idx=0;

	kernel_dim=2;
	for n in n_list:
		current_n_result_list=[]

		t_start = time.time()	
		for point in point_list:
			arg_str="%s"%(str(n));
			display_arg_str="%s=%5s"%(SIZE_LABEL, str(n))
			for i in range(kernel_dim):
				dim=point[i]
				label=DIM_LABELS[i]
				arg_str+=" %s"%(str(dim))
				display_arg_str+=", %s=%-4s"%(label, str(dim))
			arg_str+=" %s"%(str(n_repeat));
#			print(arg_str)
#			display_arg_str+=" %s"%(str(n_repeat));
			#print("--checking [%s] [%s]"%(binary_name, display_arg_str))
#			print ".",
#			sys.stdout.flush()
			progress_percent(idx, len(n_list)*len(point_list));
			current_arg_list=arg_str.split(" ")
			s=run_cmd_with_buffer(cmd+current_arg_list).split("\n")[:]
#			print(cmd, point, s)
#			print(cmd, arg_str)
#			for x in s:
#				print(x)
#			print("------------------------")
#			print(s)
			current_n_result_list+=[[point,s]]
			idx+=1;
		t_stop=time.time()
		t_elapsed=t_stop-t_start
#		print("t_elapsed=",t_elapsed)
		result_list+=[[n,current_n_result_list,t_elapsed]];
	

	
	print("\n");
#	print(hrule+"\n")
	return result_list

#######################################
def get_timing_from_another_list(src, dest):
	print("src", src)
	return src;

#######################################
def to_float(str_val, precision):
	magnifier=(10.0)**precision;
	v=int(float(str_val)*magnifier)
	v=(float(v))/magnifier
	return v;
	
#######################################
def compare_results(kernel_name, \
		first_label, first_list,\
		second_label, second_list):
	
	if (len(first_list)!=len(second_list)):
		print("[@compare_results][ERROR:\
		(len(first_list)!=len(second_list))]")
		exit_failure();
	
	if len(first_list)!=len(second_list):
		print("[@compare_results][FATAL! len(first_list)!=len(second_list):!]") 
		exit(EXIT_FAILURE);

	size=len(first_list);

	result_str=""
	for i in range(size):
		### n
		n1=first_list[i][0]
		n2=second_list[i][0]
		
		### time
		v1=first_list[i][1]
		v2=second_list[i][1]
		val1=to_float(v1,6)
		val2=to_float(v2,6);
		
		### dims
		min_list1=first_list[i][2]
		min_list2=second_list[i][2]
		
		### first element -> dimensions
		p1=min_list1[0][0]
		p2=min_list2[0][0]
#		print(p1,p2)


		sys_elapsed1=to_float(first_list[i][3],3)
		sys_elapsed2=to_float(second_list[i][3],3)
#		print(sys_elapsed1, sys_elapsed2)
		
		if val2:
			ratio=val1/val2;
		else:
			ratio=0;
		ratio=to_float(ratio, 2);	
		ratio_sign=""
#		if ratio==1.0:
#			ratio_sign="="
		if ratio<1.0:
			percentage=int(100.0*(val2-val1)/(val1));
			ratio_sign="[%02d%% DECREASE]"%(percentage) ## down arrow in unicode.
#		if ratio>1.0:
#			ratio_sign="+"	
		if n1==n2:
			p1_str=""
			p2_str=""
			
			p1_str+="%s=%4d"%(DIM_LABELS[0], p1[0])
			p2_str+="%s=%4d"%(DIM_LABELS[0], p2[0])
			for i in range(1,len(p1)):
				p1_str+=", %s=%4d"%(DIM_LABELS[i], p1[i])
				p2_str+=", %s=%4d"%(DIM_LABELS[i], p2[i])

			current_line=\
					"[%s][%s=%d]\n"\
					"  [t_%-9s = %.3f (ms)] [%s] [t_sys=%.3f (s)]\n"\
					"  [t_%-9s = %.3f (ms)] [%s] [t_sys=%.3f (s)]\n"\
					"  [ratio       = %.2f X]%s\n"%(\
					kernel_name,\
					SIZE_LABEL, n1,\
					first_label, val1,  p1_str, sys_elapsed1,\
					second_label, val2, p2_str, sys_elapsed2,\
					ratio, ratio_sign);
			current_line+=hrule;
			result_str+=current_line+"\n";

		for i in range((len(min_list1))):#,40)):
			print("n=%d"%(n1), min_list1[i])
		print("====================================\n")

			
#			sys_ratio=to_float(sys_elapsed1/sys_elapsed2,2)
#			current_line=\
#					"  [t_sys_%-9s = %.3f (s)]\n"\
#					"  [t_sys_%-9s = %.3f (s)]\n"\
#					"  [ratio           = %.2f X]\n"%(\
#					first_label, sys_elapsed1,\
#					second_label, sys_elapsed2,\
#					sys_ratio);
#			current_line+=hrule;
#			result_str+=current_line+"\n";
	print(result_str)

	
	return result_str;


#######################################

## get the tracer_params.log and extend to larger values.
def set_n_list(lower_bound, upper_bound, n_extra_points):
	
	lower_bound_log2=int(math.log(lower_bound,2));
	upper_bound_log2=int(math.log(upper_bound,2));
#	print(lower_bound_log2, upper_bound_log2);

	n_list=[]	

	for n_log2 in range(lower_bound_log2, upper_bound_log2+n_extra_points):
		n=(1<<n_log2);
		n_list+=[n]

	return n_list;

#######################################
def parse_kernel_invoker_result(kernel_name, line_list):

	p_invoker_0=r"^\[@invoker\]\[kernel_%s[^\]]*\][ ]*"%(kernel_name)
	p_invoker_1=r"\[\(gx, gy, gz\)=\(\d+, \d+, \d+\)\]"
	p_invoker_2=r"\[\(bx, by, bz\)=\(\d+, \d+, \d+\)\]\s*"
	p_driver_0=r"\[\[@driver\]\[kernel_%s[^\]]*\][ ]*=[ ]*\["%(kernel_name)
	p_driver_1=r"\d*.\d*[ ]*\(ms\)\]"

	p_invoker=p_invoker_0+p_invoker_1+p_invoker_2
	p_driver=p_driver_0+p_driver_1
	result_dict={}

	pattern=re.compile(p_invoker+p_driver, re.MULTILINE);
	l="\n".join(line_list)
#	print(l)
#	print("================================")
#	print(pattern.pattern)
	m=pattern.findall(l)

	time_list=[]
	b_val=""
	g_val=""
	
	if len(m)==0:
		return None;
	for match in m:
#		print(match)	
		[dims, time]=match.split("\n")
		dims=re.sub(p_invoker_0,"", dims)
		g_dims=(re.compile(p_invoker_1).findall(dims))[0]
		b_dims=(re.compile(p_invoker_2).findall(dims))[0]

		g_dims=re.sub("[^=]*=","",g_dims)
		g_dims=re.sub("\(","",g_dims)
		g_dims=re.sub("\)","",g_dims)
		g_dims=re.sub("\]","",g_dims)
		g_dims=re.sub("\[","",g_dims)
		g_dims=re.sub(",","",g_dims)
		g_dims=re.sub(" ","_",g_dims)

		b_dims=re.sub("[^=]*=","",b_dims)
		b_dims=re.sub("\(","",b_dims)
		b_dims=re.sub("\)","",b_dims)
		b_dims=re.sub("\]","",b_dims)
		b_dims=re.sub("\[","",b_dims)
		b_dims=re.sub(",","",b_dims)
		b_dims=re.sub(" ","_",b_dims)	

		time=re.sub(p_driver_0,"",time)
		time=re.sub("\(ms\).*","",time)
		time=float(time)

		if b_val=="":
			b_val=b_dims
		elif b_val!=b_dims:
			print("bad value of b_dims")
			exit (-1)

		if g_val=="":
			g_val=g_dims
		elif g_val!=g_dims:
			print("bad value of g_dims")
			exit (-1)
		
		time_list.append(time);

#	print("#######################################")
	
#	print(g_val, b_val, time_list)
#	print("=======================")
	g_val=g_val.split("_")
	b_val=b_val.split("_")
	time_list=sorted(time_list)
	median_idx=(len(time_list))/2
	
#	print(time_list)
#	delta=(max(time_list)-min(time_list))/10.0
#	delta_count=11*[0]
#	t_min=min(time_list)
#	for t in time_list:
#		s=int((t-t_min)/delta)
#		delta_count[s]+=1;
#
#	avg=sum(time_list)/len(time_list)*1.0
#	avg=to_float(avg,3)
#
#	elapsed_time=avg
#
#	max_delta_idx=-1;
#	for i in range(len(delta_count)):
#		if delta_count[i]>max_delta_idx:
#			max_delta_idx=i;
#	break_idx=0;
#	max_delta=max_delta_idx*delta+t_min;
#	for i in range(len(time_list)):
#		if time_list[i]>=max_delta:
#			break_idx=i;
#			break;
#	elapsed_time=time_list[break_idx]
#	elapsed_time=t_min
	elapsed_time=time_list[median_idx]
#	print(time_list, median_idx)
#	print(l)
	return [g_val, b_val, elapsed_time]

#######################################
def get_running_time_list (cuda_result_list, kernel_name_list):
	time_pattern_str="@driver"
	dim_pattern_str="@invoker"

	final_result_list=[]
	for k in kernel_name_list:
#		print("kernel name ------------ %s"%(k))
		running_time_list=[];
		for result_list in cuda_result_list:
			n=result_list[0]
			current_res=result_list[1]
			sys_elapsed=result_list[2]	
			current_n_elapsed_list=[]
			for res in current_res:
#				print("parsing %s"%(res))
				point=res[0]
				lines=res[1]	
				matched_line="";
				matched_dim_line="";
				valid_time=0;
				valid_dim=0;
				
				return_val=parse_kernel_invoker_result(k, lines);
				if return_val==None:
					continue;
				else:
					[g_dims, b_dims, elapsed]=return_val[:]
				dims=b_dims[:]
				for i in range(len(dims)):
					dims[i]=int(dims[i])
#				[bx,by,bz]=dims[:]
#				get the block dims reported by kernel invoker!
#				do not rely on the mesh points in this script.
#				print(dims,	elapsed)
				current_n_elapsed_list+=[[dims,elapsed]]
			running_time_list+=[[n, current_n_elapsed_list,sys_elapsed]]
		final_result_list+=[[k,running_time_list]];
	
	return final_result_list;

#######################################
def transpose(x):
	m=len(x)
	n=len(x[0])
	y=[]
	for j in range(n):
		t=[]
		for i in range(m):
			t+=[x[i][j]]
		y+=[t]
	return y;

#######################################
def sort_list_by_idx(x, idx):
	n=len(x)
	for i in range(n):
		for j in range(i):
			if x[j][idx]>x[i][idx]:
				t=x[i][:]
				x[i]=x[j][:]
				x[j]=t[:]
	return x;

#######################################
def find_best_cuda_conf(cuda_result_list, kernel_name_list, label):	
	verbose=0;
	running_time_list = get_running_time_list (cuda_result_list, kernel_name_list)

	best_result=[]

	for i in range(len(kernel_name_list)):
		kernel_result_list = running_time_list[i]
		kernel_name = kernel_result_list[0];
		if kernel_name!=kernel_name_list[i]:
			print("[@find_best_cuda_con][FATAL ERROR: mismatch in order of the kernels")
			exit(EXIT_FAILURE);
		
		best_cuda_conf_list=[]
		for result_list in kernel_result_list[1]:
#			print("result_list>>", result_list)
			n=result_list[0]
			elapsed_list=result_list[1]
			elapsed_time_sys=result_list[2]
#			print("elapsed>>",elapsed_time_sys)
			## list of other points that have the same time as min, i.e. other minimums
			current_min_list=[] 
			
			elapsed_list=sort_list_by_idx(elapsed_list, 1);	
#			for e in elapsed_list:
#				print(e)
#			print("-----------------")
			t_min=(elapsed_list[0][1])
			current_min_list=elapsed_list[:];
			best_cuda_conf_list+=[[n, t_min, current_min_list, elapsed_time_sys]]
		best_result+=[[kernel_name,best_cuda_conf_list]]
	
	if verbose:
		for res in best_result:
			kernel_name=res[0]
			best_conf_list=res[1]
	#		print(best_conf_list)
			for best_conf in best_conf_list:
				print("[%s][%s][Best config for N=%5d][%.3f (ms)]]"\
						%(label, kernel_name, best_conf[0], best_conf[1])) 
		print(hrule)	
	return best_result;


#######################################
def find_best_cuda_conf_in_another_list(cuda_result_list, second_list, kernel_name_list, label):	
	verbose=0;
	running_time_list = get_running_time_list (cuda_result_list, kernel_name_list)
	best_result=[]
	for i in range(len(kernel_name_list)):
		kernel_result_list = running_time_list[i]
		kernel_name = kernel_result_list[0];
		if kernel_name!=kernel_name_list[i]:
			print("[@find_best_cuda_con][FATAL ERROR: mismatch in order of the kernels")
			exit(EXIT_FAILURE);
		
		best_cuda_conf_list=[]
		for result_list in kernel_result_list[1]:
#			print("result_list>>", result_list)
			n=result_list[0]
			elapsed_list=result_list[1]
			elapsed_time_sys=result_list[2]
			

			sl=second_list[:]
			### find the kernel name
			for entry in sl:
				if entry[0]==kernel_name:
					sl=entry[1][:]
					break;
			## find the N
			for entry in sl:
				print(n, entry[0])
				if entry[0]==n:
					sl=entry[2][:]
					break;

			for i in range(len(elapsed_list)):
				[d1,t1]=[elapsed_list[i][0]	, elapsed_list[i][1]]
				print(d1,t1)
				for e in sl:
					[d2,t2]=[e[0], e[1]]
#					print("d2,t2", d2,t2)
					## TODO: fix the z dim for 3d points:
					if d2[0]==d1[0] and d2[1]==d1[1]:#  and d2[2]==d1[2]:
						elapsed_list[i][1]=float(t2);
						print("changed %.3f -> %.3f for %s"%(t1,t2,d1))
#			print("elapsed>>",elapsed_time_sys)
			## list of other points that have the same time as min, i.e. other minimums
			current_min_list=[] 
			elapsed_list=sort_list_by_idx(elapsed_list, 1);
#			print("-----------------")
			t_min=(elapsed_list[0][1])
			print("t_min=", t_min)
			current_min_list=elapsed_list[:];
			best_cuda_conf_list+=[[n, t_min, current_min_list, elapsed_time_sys]]
		best_result+=[[kernel_name,best_cuda_conf_list]]
	
	if verbose:
		for res in best_result:
			kernel_name=res[0]
			best_conf_list=res[1]
	#		print(best_conf_list)
			for best_conf in best_conf_list:
				print("[%s][%s][Best config for N=%5d][%.3f (ms)]]"\
						%(label, kernel_name, best_conf[0], best_conf[1])) 
		print(hrule)	
	return best_result;


#######################################
def dump_cuda_result_to_file(cuda_result_list, kernel_name_list, label):	
	verbose=0;
	running_time_list = get_running_time_list (cuda_result_list, kernel_name_list)
	
	for i in range(len(kernel_name_list)):
		kernel_result_list = running_time_list[i]
		kernel_name = kernel_result_list[0];
		if kernel_name!=kernel_name_list[i]:
			print("[@dump_trace_result_to_file][FATAL ERROR: mismatch in order of the kernels")
			exit(EXIT_FAILURE);

		output_str="[N][bx, by, bz][ELAPSED_MS]\n"
		for result_list in kernel_result_list[1]:
#			print("result_list>>", result_list)
			n=result_list[0]
			elapsed_list=result_list[1]
			elapsed_time_sys=result_list[2]
#			print("elapsed>>",elapsed_time_sys)
			for res in elapsed_list:
				point=res[0]
				elapsed=res[1]
				s="[%d][%d, %d, %d][%.3f]\n"%(n, point[0], point[1], point[2], elapsed)
				output_str+=s;						
	
#		print(output_str)
		path="kernel_%s_elapsed_%s.tmp"%(kernel_name, label)
		f=open(path,"w")
		f.write(output_str);
		f.close();


#######################################
## todo: generate 3d mesh..
def generate_2d_mesh():
	point_list=[]
	for b2 in range(5, 11):
		for bx2 in range(b2+1):
			bx=(1<<bx2);
			by=(1<<(b2-bx2))
			point_list+=[[bx,by]]
#			print(by,bx)
#	for p in point_list:
#		print(p)
	
	return point_list;

#######################################
def perf_check(path, n_extra_points, n_repeat):
#	stat=optimize(path);
#	if stat!=EXIT_SUCCESS:
#		return EXIT_FAILURE;
	
	verbose=1;
	
	config=parse_config_file(path)
	cuda_src=config[0];
	lower_bound=int(config[4]);
	upper_bound=int(config[5]);

	cuda_bin=cuda_src.replace(".cu",".bin");
	instrumented_bin=cuda_src.replace(".cu","_instrumented.bin");
#	orig_trace_points="tracer_params.log";
	kernel_name_list_path="kernel_name_list.tmp";
	
	###########################
	necessary_files=[cuda_bin, instrumented_bin, kernel_name_list_path]
	for f in necessary_files:
		if os.path.exists(f)==False:
			print("[%s] does not exist!\n...make sure you have run 'opt' first!"%(f));
			return EXIT_FAILURE;

	if verbose:
		print("[All necessary files exist]:\n1.[%s] 2.[%s] 3.[%s]"\
				%(cuda_bin, instrumented_bin, kernel_name_list_path))
	print(hrule)
	###########################
	kernel_name_list=read_content_from_file(kernel_name_list_path).split("\n")[:-1]
	###########################
	## extend list of trace points for extra values of N
	## find the best config for each N
	mesh_points=generate_2d_mesh();
	n_list=set_n_list(lower_bound, upper_bound, n_extra_points) ## continue up to 4 more point
	

	trace_point_list=[n_list,mesh_points]
	cuda_bin_result=run_binary_with_arglist(cuda_bin, trace_point_list,\
			n_repeat)	

	## TODO: to be fixed for 1D and 3D kernels.
	fake_mesh_point_list=[[1,1]]
	trace_point_list=[n_list,fake_mesh_point_list]

	os.system("rm -rf kernel_*_ecc_by_dp.tmp > /dev/null")
	instrumented_bin_result=run_binary_with_arglist(instrumented_bin,\
			trace_point_list, n_repeat)	
	
	best_cuda_conf=find_best_cuda_conf(cuda_bin_result, kernel_name_list,\
			"EX_SEARCH")
	dump_cuda_result_to_file(cuda_bin_result, kernel_name_list,\
			"cuda")
#	print(best_cuda_conf)
	best_instrumented_conf=\
			find_best_cuda_conf_in_another_list(instrumented_bin_result,\
			best_cuda_conf,	kernel_name_list, "KLARAPTOR")

	for i in range(len(kernel_name_list)):
		k=kernel_name_list[i]
		os.system("printf '[N][bx, by, bz][ECC]\n' > kernel_%s_ecc_by_dp.tmp"%(k))
		os.system("cat kernel_%s_n*_ecc_by_dp.tmp >> kernel_%s_ecc_by_dp.tmp"%\
			(k, k))
		os.system("rm -rf kernel_%s_n*_ecc_by_dp.tmp > /dev/null"%(k))

#	print(best_instrumented_conf)

#	kernel_list=read_content_from_file(kernel_name_list).split("\n")[:-1]
	for i in range(len(kernel_name_list)):
		k=kernel_name_list[i]
	
		current_cuda_conf=best_cuda_conf[i][1]
		current_instrumented_conf=best_instrumented_conf[i][1]

#		print(best_cuda_conf[i][1]);
		result_path="kernel_%s_perfcheck.tmp"%(k)
		r=compare_results(k, "EX_SEARCH", current_cuda_conf, \
				"KLARAPTOR", current_instrumented_conf)
		write_content_to_file([r], result_path);
		print("[result is written to [%s]]"%(result_path));
		print(hrule)
	return EXIT_SUCCESS;	
#######################################
def clean():
	cmd=[]
	cmd+=["/bin/bash"]
	cmd+=[(MAKE_SCRIPT)]
	cmd+=["--clean"];
	subprocess.call(cmd[:])
	return EXIT_SUCCESS;	

#######################################
def main():
	verbose=0;
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

	
	if task=="--perf-check":
		if argc>=4:
			n_repeat=1;
			config_file_path=args[2];
			n_extra_points=int(args[3]);
			if argc>4:
				n_repeat=int(args[4]);
			if verbose:
				print("OPT FROM CONFIG FILE", config_file_path)	
			stat=perf_check(config_file_path, n_extra_points, n_repeat)
			exit_msg("perf_check", stat);
			exit_success()

	print_args_help()
	exit_failure()

#######################################
if __name__=="__main__":
	main()
