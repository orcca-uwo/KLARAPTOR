import os
import sys
import re
import math

#######################################
comma_replacement=""


#######################################
## kernel_KERNELNAME_ptx_params.tmp
ptx_params_template='''[shared_mem_bytes_static: REPLACE_STATIC_SMEM_BYTES]
[registers: REPLACE_N_REGISTERS]'''

#######################################
## kernel_KERNELNAME_params.tmp
kernel_params_template='''[grid_dim_x: REPLACE_GRID_DIM_X]
[grid_dim_y: REPLACE_GRID_DIM_Y]
[grid_dim_z: REPLACE_GRID_DIM_Z]
[block_dim_x: REPLACE_BLOCK_DIM_X]
[block_dim_y: REPLACE_BLOCK_DIM_Y]
[block_dim_z: REPLACE_BLOCK_DIM_Z]
[shared_mem_bytes_dynamic: REPLACE_DNYAMIC_SMEM_BYTES]
[active_blocks_per_sm: REPLACE_ACTIVE_BLOCKS_PER_SM]
[occupancy_percentage: REPLACE_OCCUPANCY_PERCENTAGE]
'''

#######################################
## kernel_KERNELNAME_trace.avg.txt
trace_params_template='''[13.Total_insts: REPLACE_TOTOL_INSTS]
[14.Comp_insts: REPLACE_COMP_INSTS]
[15.Mem_insts: REPLACE_MEM_INSTS]
[16.Uncoal_Mem_insts: REPLACE_UNCOAL_MEM_INSTS]
[17.Coal_Mem_insts: REPLACE_COAL_MEM_INSTS]
[18.Synch_insts: REPLACE_SYNCH_INSTS]
[19.Coal_per_mw: REPLACE_COAL_PER_MW]
[20.Uncoal_per_mw: REPLACE_UNCOAL_PER_MW]
[n_active_blocks_per_sm: REPLACE_ACTIVE_BLOCKS_PER_SM]
[n_blocks: REPLACE_N_BLOCKS]
[n_warps: REPLACE_N_WARPS]
'''

#######################################
def read_content_from_file(path):
	f=open(path,"r");
	content=f.read();
	f.close()
	return content;

#######################################
def write_content_to_file(content, path):
	f=open(path,"w")
	f.write(content);
	f.close()

#######################################
def handle_commas(line):
	
	new_line=""
	quoute_seen=0;
	for c in line:
		if c=='"':
			if quoute_seen==0:
				quoute_seen=1;
			elif quoute_seen==1:
				quoute_seen=0;

		if c==",":
			if quoute_seen==1:
				new_line+=comma_replacement
				continue;
	
		new_line+=c;

	return new_line


#######################################
def strip_kernel_name(kernel_name):
	return re.sub(r'\([^\)]*\)',"",kernel_name)


#######################################
def couple(title, value):
	return "["+str(title)+"="+str(value)+"]"


#######################################
def print_kernel_metrics_info_list(kernel_info_list):
	for k in kernel_info_list:
			print(couple("kernel", k[0]))
			print(couple("occupancy",k[1]))
			print(couple("ipc",k[2]))
			print(couple("n_insts",k[3]))
			print(couple("n_mem_insts",k[4]))
			print(couple("n_comp_insts",k[3]-k[4]))
			print(couple("n_transactions",k[5]))
			print(couple("gst_bw",k[6]))
			print(couple("gld_bw",k[7]))
			print(couple("gst_bytes",k[8]))
			print(couple("gld_bytes",k[9]))

			print("=========================")


#######################################
def print_kernel_trace_info_list(kernel_info_list):
	for k in kernel_info_list:
			print(couple("kernel", k[0]))
			print(couple("grid_x",k[1]))
			print(couple("grid_y",k[2]))
			print(couple("grid_z",k[3]))
			print(couple("block_x",k[4]))
			print(couple("block_y",k[5]))
			print(couple("block_z",k[6]))
			print(couple("n_registers",k[7]))
			print(couple("static_smem",k[8]))
			print(couple("dynamic_smem",k[9]))
			print("=========================")


#######################################
def parse_profiling_metrics(path, verbose=0):	
	content=read_content_from_file(path)
	content=content.split("\n")

	idx=0;

	if "Error" in content[0]:
		exit(-1);
	for x in content:
		if "Disconnected from process " in x:
			break;
		else:
			idx+=1;	
	idx+=1;
	
	titles=content[idx]
	idx+=1;

	nt=len(titles.split(","))
	content=content[idx:]	

	kernel_idx=4; metric_idx=9;
	
	value_idx=11;
	
	#####################################
	metric_list=[]
	metric_list+=["dram__bytes_read.sum"] #0
	metric_list+=["dram__bytes_write.sum"] #1
	metric_list+=["l1tex__t_bytes_pipe_lsu_mem_global_op_ld.sum.per_second"] #2	
	metric_list+=["l1tex__t_bytes_pipe_lsu_mem_global_op_st.sum"] #3
	metric_list+=["l1tex__t_bytes_pipe_lsu_mem_global_op_st.sum.per_second"] #4
	metric_list+=["l1tex__t_sectors_pipe_lsu_mem_global_op_ld.sum"] #5
	#metric_list+=["lts__t_bytes_equiv_l1sectormiss_pipe_lsu_mem_local_op_st.sum","lts__t_bytes_equiv_l1sectormiss_pipe_lsu_mem_global_op_st.sum",
	#			  "lts__t_bytes_equiv_l1sectormiss_pipe_lsu_mem_global_op_ld.sum"]
	metric_list+=["sm__warps_active.avg.pct_of_peak_sustained_active"] #6
	metric_list+=["smsp__average_inst_executed_per_warp.ratio"] #7
#	metric_list+=["l2_local_global_store_bytes","l2_global_load_bytes"] 
	metric_list+=["smsp__inst_executed.avg.per_cycle_active"] #8
	metric_list+=["smsp__inst_executed_op_global_ld.sum","smsp__inst_executed_op_global_st.sum"] #9,10

	#####################################
	[occupancy, ipc, n_insts, n_mem_insts, n_transactions]=[0,0,0,0,0]	
	[gst_throughput,gld_throughput,gst_bytes,gld_bytes]=[0,0,0,0]
	kernel_info_list=[]
	#####################################

	current_kernel=""
	for i in range(len(content)):
		x=content[i]
		x=handle_commas(x)

		x=x.replace('"',"")
		x=x.split(",")
			
		if len(x)<nt:
			continue;	

		[kernel_name, metric, val]=\
				[x[kernel_idx], x[metric_idx], x[value_idx]]
		
		if current_kernel!=kernel_name:
			if verbose:
				print("==================")
				print(couple("NEW KERNEL SCANNED", strip_kernel_name(kernel_name)))
			if current_kernel!="":	
				current_kernel=strip_kernel_name(current_kernel)	
			
				kernel_info_list.append([current_kernel, occupancy, ipc, n_insts,\
					n_mem_insts, n_transactions, \
					gst_throughput, gld_throughput, \
					gst_bytes, gld_bytes])
				[occupancy,ipc, n_insts, n_mem_insts, n_transactions]=[0,0,0,0,0]
				[gst_throughput,gld_throughput,gst_bytes,gld_bytes]=[0,0,0,0]
			current_kernel=kernel_name;
				
	
		if metric==metric_list[6]:
			occupancy=(float(val))
			if verbose:
				print(couple("occupancy",occupancy))

		if metric==metric_list[8]:
			ipc=float(val)
			if verbose:
				print(couple("ipc",ipc))

		if metric==metric_list[7]:
			n_insts=int(float(val))
			if verbose:
				print(couple("n_insts",n_insts))	
	
		if metric==metric_list[10] or metric==metric_list[9]:
			n_mem_insts+=int(float(val))
			if verbose:
				print(couple("n_mem_insts",n_mem_insts))	
			
		if metric==metric_list[3] or metric==metric_list[5]:
			n_transactions+=int(float(val))
			if verbose:
				print(couple("n_transactions",n_transactions))

		if metric==metric_list[4]:
			gst_throughput=convert_bw_to_mb(str(val))
			if verbose:
				print(couple("gst_throughput",gst_throughput))

		if metric==metric_list[2]:
			gld_throughput=convert_bw_to_mb(str(val))
			if verbose:
				print(couple("gld_throughput",gld_throughput))
	
		if metric==metric_list[1]:
			gst_bytes=int(val)
			if verbose:
				print(couple("gst_bytes",gst_bytes))

		if metric==metric_list[0]:
			gld_bytes=int(val)
			if verbose:
				print(couple("gld_bytes",gld_bytes))

	current_kernel=strip_kernel_name(current_kernel)
	kernel_info_list.append([current_kernel, occupancy, ipc, n_insts,\
		n_mem_insts, n_transactions, \
		gst_throughput, gld_throughput, \
		gst_bytes, gld_bytes])
#	print("==================")	
	#####################################	
	if verbose:
		print_kernel_metrics_info_list(kernel_info_list)
	return kernel_info_list;


#######################################
def parse_profiling_trace(path, verbose=0):

	content=read_content_from_file(path)
	content=content.split("\n");
    
	idx=0;
	for x in content:
		if "Disconnected from process " in x:
			break;
		else:
			idx+=1;	
	idx+=1;
	
	titles=content[idx]
	titles=titles.replace('"',"")
	titles=titles.split(",")
	
	idx+=2; 

	nt=len(titles);

	trace_items=[]
	trace_items+=["launch__grid_dim_x","launch__grid_dim_y","launch__grid_dim_z"]
	trace_items+=["launch__block_dim_x","launch__block_dim_y","launch__block_dim_z"]
	trace_items+=["launch__registers_per_thread","launch__shared_mem_per_block_static","launch__shared_mem_per_block_dynamic"]
	trace_items+=["Kernel Name"]

	grid_x_idx=titles.index(trace_items[0]); grid_y_idx=titles.index(trace_items[1]); grid_z_idx=titles.index(trace_items[2]);
	block_x_idx=titles.index(trace_items[3]); block_y_idx=titles.index(trace_items[4]); block_z_idx=titles.index(trace_items[5]);
	n_registers_idx=titles.index(trace_items[6]); 
	static_smem_idx=titles.index(trace_items[7]);
	dynamic_smem_idx=titles.index(trace_items[8]);
	kernel_name_idx=titles.index(trace_items[9]);

	content=content[idx:]		
	
	kernel_info_list=[]
	for i in range(len(content)):
		x=content[i]
		x=handle_commas(x)

		x=x.replace('"',"")
		x=x.split(",")
			
		if len(x)<nt:
			continue;	

		[grid_x, grid_y, grid_z]=[x[grid_x_idx], x[grid_y_idx], x[grid_z_idx]]
		[block_x, block_y, block_z]=[x[block_x_idx], x[block_y_idx],x[block_z_idx]]
		[n_registers]=[x[n_registers_idx]]
		[static_smem,dynamic_smem]=[x[static_smem_idx], x[dynamic_smem_idx]]
		[kernel_name]=[x[kernel_name_idx]]

		if grid_x == grid_y==grid_z=="" and block_x==block_y==block_z=="":
			continue;
		
		kernel_name=strip_kernel_name(kernel_name)
		kernel_info_list.append([\
				kernel_name, \
				int(grid_x),int(grid_y), int(grid_z),\
				int(block_x), int(block_y), int(block_z),\
				int(n_registers), \
				int(static_smem), int(dynamic_smem)])
	if verbose:
		print_kernel_trace_info_list(kernel_info_list)
	return kernel_info_list;


#######################################
def hash(v):
	h=""
	for x in v:
		h+=str(x)
	return h;
#######################################
def distinct_2dlist(input_list):
	output_list=[]
	hash_list=[]

	for k in input_list:
		h=hash(k)
		if h not in hash_list:
			hash_list.append(h)
			output_list.append(k)
	
#	print(output_list)
	return output_list


#######################################
def merge_profiling_lists(metric_list, trace_list):
	
	merged_lists=[]

	trace_list=distinct_2dlist(trace_list)

	for y in metric_list:
		for x in trace_list:
			if x[0]==y[0]:
				joint_list=x[:]
				joint_list.extend(y[1:])
				merged_lists.append(joint_list)
	
#	print(merged_lists)
	return merged_lists


#######################################
def dump_kernel_names(merged_lists):
	kernel_name_list=""

	for k in merged_lists:
#		print(k)
		kernel_name_list+=k[0]+"\n"
	
	write_content_to_file(kernel_name_list, "kernel_name_list.tmp");	


#######################################
def dump_ptx_params(merged_lists, arch):
	for k in merged_lists:
		## number of registers-per-thread in corresponding ptx 
		## is required for computing active_blocks_per_sm.
		path="kernel_"+k[0]+"_sm_"+str(arch)+".registers"
		n_registers=k[7]
		write_content_to_file(str(n_registers), path)

		## number of registers-per-thread in corresponding ptx 
		## is required for computing active_blocks_per_sm.
		path="kernel_"+k[0]+"_sm_"+str(arch)+".smem"
		smem_bytes_static=k[8]
		write_content_to_file(str(smem_bytes_static), path)

		path="kernel_"+k[0]+"_ptx_params.tmp"
		content=ptx_params_template
		content=content.replace("REPLACE_N_REGISTERS",str(n_registers))
		content=content.replace("REPLACE_STATIC_SMEM_BYTES",str(smem_bytes_static))
		write_content_to_file(content, path)
	return;

#######################################
def dump_kernel_params(merged_lists, arch, verbose=0):

	## must already have called dump_ptx_params for the same arch
	## otherwise, compute_active_blocks.py will not work properly.

	for k in merged_lists:
		current=kernel_params_template
		
		current=current.replace("REPLACE_GRID_DIM_X", str(k[1]))
		current=current.replace("REPLACE_GRID_DIM_Y", str(k[2]))
		current=current.replace("REPLACE_GRID_DIM_Z", str(k[3]))

		current=current.replace("REPLACE_BLOCK_DIM_X", str(k[4]))
		current=current.replace("REPLACE_BLOCK_DIM_Y", str(k[5]))
		current=current.replace("REPLACE_BLOCK_DIM_Z", str(k[6]))

		current=current.replace("REPLACE_DNYAMIC_SMEM_BYTES", str(0))
		current=current.replace("REPLACE_ACTIVE_BLOCKS_PER_SM", str(0))
		current=current.replace("REPLACE_OCCUPANCY_PERCENTAGE", str(int(k[10])))
	
		path="kernel_"+k[0]+"_params.tmp"
		write_content_to_file(current, path)	
	
		cmd="python ../../trace_analysis/compute_active_blocks.py "+k[0] + " sm_"+str(arch)
		os.system(cmd)
		path="kernel_"+k[0]+"_sm_"+str(arch)+"_active_blocks_per_sm.tmp"	
		active_blocks_per_sm=read_content_from_file(path)
			
		
		path="kernel_"+k[0]+"_occupancy.tmp"		
		estimated_occupancy=read_content_from_file(path)

		observed_occupancy=k[10]
	
		diff=float(observed_occupancy)-float(estimated_occupancy)
#		diff=abs(diff)
		if verbose:
			print(k[0] + ": occupancy[observed - estimated]="+str(diff))
		
#		print("active_blocks_per_sm_est = "+str(active_blocks_per_sm))
		active_blocks_per_sm=float(active_blocks_per_sm)
#		ratio=float(observed_occupancy)/float(estimated_occupancy)
#		active_blocks_per_sm*=ratio;
#		active_blocks_per_sm=int(math.floor(active_blocks_per_sm))
		if active_blocks_per_sm==0:
			active_blocks_per_sm=1.0
#		print("active_blocks_per_sm_new = "+str(active_blocks_per_sm))
		# path="kernel_"+k[0]+"_sm_"+str(arch)+"_active_blocks_per_sm.tmp"
		# write_content_to_file(str(active_blocks_per_sm), path)
		###################################
		## refilling the kernel param file
		#print(active_blocks_per_sm)
		current=kernel_params_template
		
		current=current.replace("REPLACE_GRID_DIM_X", str(k[1]))
		current=current.replace("REPLACE_GRID_DIM_Y", str(k[2]))
		current=current.replace("REPLACE_GRID_DIM_Z", str(k[3]))

		current=current.replace("REPLACE_BLOCK_DIM_X", str(k[4]))
		current=current.replace("REPLACE_BLOCK_DIM_Y", str(k[5]))
		current=current.replace("REPLACE_BLOCK_DIM_Z", str(k[6]))

		current=current.replace("REPLACE_DNYAMIC_SMEM_BYTES", str(k[9]))
		current=current.replace("REPLACE_ACTIVE_BLOCKS_PER_SM",str(active_blocks_per_sm))
		current=current.replace("REPLACE_OCCUPANCY_PERCENTAGE", str(int(k[10])))
			
		path="kernel_"+k[0]+"_params.tmp"
		write_content_to_file(current, path)

		path="kernel_"+k[0]+"_occupancy.tmp"
		write_content_to_file(str(k[10]), path)
        
		if verbose:
			print(current)
	
#######################################
def dump_trace_params(merged_lists, arch, verbose=0):

	for k in merged_lists:
		current = trace_params_template;	

		n_blocks=k[1]*k[2]*k[3]
		n_warps_per_block=((k[4]*k[5]*k[6])+31)/32
		n_warps=n_warps_per_block*n_blocks
	
		total_insts=k[12]
		mem_insts=(k[13]+n_warps-1)/n_warps
		comp_ints=total_insts-mem_insts		

		synch_insts=0;

		n_transactions=k[14]

		uncoal_mem_insts=mem_insts
		coal_mem_insts=0;

		uncoal_per_mw=(n_transactions+n_warps-1)/n_warps;		
		coal_per_mw=0;#n_transactions

		path="kernel_"+k[0]+"_sm_"+str(arch)+"_active_blocks_per_sm.tmp"
		active_blocks_per_sm=read_content_from_file(path)
	
		#[13.Total_insts: REPLACE_TOTOL_INSTS]
		#[14.Comp_insts: REPLACE_COMP_INSTS]
		#[15.Mem_insts: REPLACE_MEM_INSTS]
		#[16.Uncoal_Mem_insts: REPLACE_UNCOAL_MEM_INSTS]
		#[17.Coal_Mem_insts: REPLACE_COAL_MEM_INSTS]
		#[18.Synch_insts: REPLACE_SYNCH_INSTS]
		#[19.Coal_per_mw: REPLACE_COAL_PER_MW]
		#[20.Uncoal_per_mw: REPLACE_UNCOAL_PER_MW]
		#[n_active_blocks_per_sm: REPLACE_N_ACTIVE_BLOCKS_PER_SM]
		#[n_blocks: REPLACE_N_BLOCKS]
		#[n_warps: REPLACE_N_WARPS]

		current=current.replace("REPLACE_TOTOL_INSTS", str(total_insts))
		current=current.replace("REPLACE_COMP_INSTS", str(comp_ints))
		current=current.replace("REPLACE_MEM_INSTS", str(mem_insts))
		
		current=current.replace("REPLACE_UNCOAL_MEM_INSTS", str(uncoal_mem_insts))
		current=current.replace("REPLACE_COAL_MEM_INSTS",str(coal_mem_insts))
		current=current.replace("REPLACE_SYNCH_INSTS", str(synch_insts))
		
		current=current.replace("REPLACE_COAL_PER_MW", str(coal_per_mw))
		current=current.replace("REPLACE_UNCOAL_PER_MW", str(uncoal_per_mw))

		current=current.replace("REPLACE_ACTIVE_BLOCKS_PER_SM",str(active_blocks_per_sm))
		current=current.replace("REPLACE_N_BLOCKS",str(n_blocks))
		current=current.replace("REPLACE_N_WARPS",str(n_warps))	
		
		if verbose:
			print(current)
		path="kernel_"+k[0]+"_trace.avg.txt"
		write_content_to_file(current,path)
#		print(current,path)
	return
#	for k in merged_lists:
#		current=trace_template;

#######################################
# Convert bytes per second to megabytes per second
def convert_bw_to_mb(bw_str):
	value=""
	unit=""
	for c in bw_str:
		if c=="/":
			break;

		if (c>='0' and c<='9') or (c=="."):
			value+=c;
		else:
			unit+=c;

	value=float(value);
	unit=unit.lower();
	if unit=="mb":
		unit=1.0
	elif unit=="gb":
		unit=1000.0;
	elif unit=="kb":
		unit=0.001;
	
	# print(value, unit)
	return (value/125000.0);

#######################################
def dump_mem_ld_params(merged_lists, arch):
	## ToDo: read the following from device profile
	device_bw_mb=484*1000.0;
	core_freq=1582.0; #MHZ
#	mem_freq=5510.0; #MHZ

	for k in merged_lists:
		n_transactions=k[14]
		[gst_bw,gld_bw,gst_bytes,gld_bytes]=k[15:19]
		t=max((gst_bytes/gst_bw),(gld_bytes/gld_bw))
		t=max((gst_bytes/device_bw_mb),(gld_bytes/device_bw_mb))
		mem_ld=(t*core_freq);
#		print("mem_ld",mem_ld)


#######################################

def main():
	metrics_path="profiling.metrics"
	metrics_info_list=parse_profiling_metrics(metrics_path,0);

	trace_path="profiling.trace"
	trace_info_list=parse_profiling_trace(trace_path); #print(trace_info_list);
	#print(trace_info_list)
	merged_lists=merge_profiling_lists(metrics_info_list, trace_info_list)
	#print(merged_lists)
	arch=75

	argc=len(sys.argv)
	if argc>1:
		arch_path=(sys.argv[1]);
		arch=int(read_content_from_file(arch_path))
#		print("arch",arch)
	dump_kernel_names(merged_lists);
	dump_ptx_params(merged_lists, arch); ## step 0
	dump_kernel_params(merged_lists, arch); ## step 1
	dump_trace_params(merged_lists, arch); ## step 2
	#dump_mem_ld_params(merged_lists, arch);

main()



## 1. no need to decompose and compile kernels separately
##	  enough to pass binaries to the profiler.sh
## 2. for the moment, do not change the configuration of the files
## 3. compute_active_blocks.py can be attached to this program
## 4. accumulate_trace.py is not required anymore.
## 
