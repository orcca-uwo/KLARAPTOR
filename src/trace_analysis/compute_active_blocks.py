import sys
import os
import math
import re



#######################################
def ceiling_div(x,y):
	return int((x+y-1)/y)

#######################################
def flooring_div(x,y):
	return int(x/y)

#######################################
def read_content_from_file(file_name):	
	content_file=open(file_name,"r");
	content=content_file.read()
	content_file.close();
	return content

#######################################
sm_list=[
		"sm_20","sm_21", "sm_30", "sm_32", "sm_35",
		"sm_37","sm_50", "sm_52",	"sm_53", "sm_60",
		"sm_61" ,"sm_62", "sm_75"]


#######################################
max_registers_per_block_list=[32768, 32768
		,65536,65536,65536,65536,65536,65536,32768,65536,65536,65536,65536]
max_blocks_per_sm_list=[8,8,16,16,16,16,32,32,32,32,32,32,32]
max_register_per_thread_list=[63,63,63,255,255,255,255,255,255,255,255,255,255]
max_warps_per_sm_list=[48,48,64,64,64,64,64,64,64,64,64,128,64]
max_shared_mem_per_block=49152
max_block_size=1024

#######################################
def get_arch_register_block_warp_specs(arch):
	idx=0;
	arch=arch.lower()
	for i in range(len(sm_list)):
		if arch==sm_list[i]:
			idx=i;
			break;
	
	return [max_registers_per_block_list[idx],max_blocks_per_sm_list[idx],\
			max_register_per_thread_list[i], max_warps_per_sm_list[idx]]
#for i in range(len(sm_list)):
#	print(sm_list[i],max_blocks_per_sm_list[i],\
#			max_shared_mem_per_block, max_registers_per_block_list[i])

#######################################
def read_kernel_params(kernel_params_file):

	kernel_params=read_content_from_file(kernel_params_file).split("\n")[:-1]
	n=len(kernel_params)
	for i in range(n):
		x=kernel_params[i]
		x=re.sub(r'\[[^:]*: ',"",x);
		x=re.sub(r'\]',"",x);
		kernel_params[i]=int(x)
	
	return [kernel_params[0:3],kernel_params[3:6],kernel_params[6]]

#######################################
def get_active_blocks(kernel_name,arch="sm_60", verbose=0):

#	verbose=1
## get info based on sm_cc
	[max_register_per_block, max_resident_block_per_sm,\
			max_register_per_thread, max_warps_per_sm]=get_arch_register_block_warp_specs(arch);
	
	## initializing path of the output file
	result_file_name="kernel_"+kernel_name+"_"+arch+"_active_blocks_per_sm.tmp"
	if verbose:
		print("processing kernel ["+kernel_name+"]")
	
	####################	
	## check if static smem count is available, otherwise, set it to max.
	smem_file="kernel_"+kernel_name+"_"+arch+".smem"
	if os.path.isfile(smem_file):
		smem_bytes_static=int(read_content_from_file(smem_file))
	else:
		print("@WARNING: smem_file does not exist! -> setting to maximum");
		smem_bytes_static=max_shared_mem_per_block
	
	####################
	## check if kernel_params_file exists
	#### if not, read from default values;
	kernel_params_file="kernel_"+kernel_name+"_params.tmp"
	[grid_dims,block_dims,smem_bytes_dynamic]=read_kernel_params(kernel_params_file)

	n_actual_blocks=grid_dims[0]*grid_dims[1]*grid_dims[2];
	block_size=block_dims[0]*block_dims[1]*block_dims[2]	
	smem_bytes=smem_bytes_static+smem_bytes_dynamic
	

	####################
	registers_file="kernel_"+kernel_name+"_"+arch+".registers"
	if os.path.isfile(registers_file):
		register_per_thread=int(read_content_from_file(registers_file))
	else:
		print("@WARNING: registers_file does not exist! -> setting to maximum");
		register_per_thread=max_register_per_thread;

	
	####################
	if verbose:
		print("register_per_thread",register_per_thread)
		print("smem_bytes",smem_bytes)
	
	## one implicit register is always being used.
	register_per_thread+=1;

	B=block_size;
	R_B=max_register_per_block;
	Z_B=max_shared_mem_per_block;
#	T_B=max_block_size;
	B_SM=max_resident_block_per_sm;
	W_SM=max_warps_per_sm;
	Z=0;
	R=register_per_thread;
	if smem_bytes!=0:
		Z=smem_bytes;

	n_active_blocks=0;

	n_device_sm=40
	n_device_sm_file="device_n_sm.tmp"
	if os.path.isfile(n_device_sm_file):
		f=open(n_device_sm_file,"r")
		tn=int(f.read())
		f.close()
		if tn:
			n_device_sm=tn;
	
	if verbose:
		print("n_sm",n_device_sm)
		print("max_warps_per_sm", max_warps_per_sm)
		print("B",B)
	
#	print("case 1 values")
#	print((B*B_SM < 32*W_SM))
#	print((R*B*B_SM < R_B))
#	print((Z*B_SM < Z_B))
#
#	print("case 2 values")
#	print((32*W_SM < B*B_SM))
#	print((R*B*B_SM < R_B))
#	print((Z*32*W_SM < Z_B))
#
#	print("case 3 values");
#	print(R_B< R*B*B_SM) 
#	print(R_B<R*32*W_SM)
#	print(Z*R_B < R*B*Z_B)
#	
#	print("case 4 values")
#	print(Z_B < B_SM*Z) 
#	print(Z_B*B < 32*W_SM*Z)
#	print(Z_B * R *B  < Z* R_B)

	#### case1: limited by number of blocks
	if ((B*B_SM <= 32*W_SM) and (R*B*B_SM <= R_B) and (Z*B_SM <= Z_B)):
		n_active_blocks=B_SM;
		if verbose:
			print("case 1");
	
	#### case2: limited by number of warps
	elif ((32*W_SM <= B*B_SM) and (R*32*W_SM <= R_B) and (Z*32*W_SM <= Z_B*B)):
#		n_active_blocks=ceiling_div((32*W_SM),B);
		n_active_blocks=flooring_div((32*W_SM),B);##
		if verbose:
			print("case 2");
			print("n_active_blocks", n_active_blocks)

	#### case3: limited by register usage
	elif ((R_B<= R*B*B_SM) and (R_B<=R*32*W_SM) and (Z*R_B <= R*B*Z_B)):
		n_active_blocks=flooring_div(R_B,(R*B));
		if verbose:
			print("case 3");


	#### case4: limited by shared memory usage
	elif ((Z_B <= B_SM*Z) and (Z_B*B <= 32*W_SM*Z) and (Z_B * R *B  <= Z* R_B)):
		n_active_blocks=flooring_div(Z_B,Z);
#		print(Z_B,Z)
		if verbose:
			print("case 4");
	else:
		n_active_blocks=0;
		if verbose:
			print("This case is not covered");
			print(str(B)+"=block_size");
			print(str(R_B)+"=max_register_per_block");
			print(str(Z_B)+"=max_shared_mem_per_block");
			print(str(B_SM)+"=max_resident_block_per_sm");
			print(str(W_SM)+"=max_warps_per_sm");
			print(str(Z)+"=shared_mem_bytes");
			print(str(R)+"=register_per_thread");

#	n_actual_blocks=flooring_div(n_actual_blocks, n_device_sm)
#	if n_actual_blocks==0:
#		n_actual_blocks=1
#
	n_actual_blocks=ceiling_div(n_actual_blocks, n_device_sm)
	n_active_blocks=min(n_active_blocks,n_actual_blocks)
#	print("n_active_blocks",n_
#	n_warps=ceiling_div(block_size,32)
#	print("n_warps",n_warps);
#	print("phony_occupancy",(1.0*n_warps)/max_warps_per_sm)
	
	if verbose:
#		print("min_n_threads_per_block",int(min_n_threads_per_block))
		print("block_size",int(block_size))
		print("n_active_blocks",int(n_active_blocks))
#		print("========================")
    
	result_file=open(result_file_name,"w");
	result_file.write(str(int(n_active_blocks)))
	result_file.close();

#	occupancy_val=(1.0*n_active_blocks*block_size/32)/max_warps_per_sm	
#	nw_per_block=ceiling_div(block_size,32);

#	=ceiling_div(1.0*n_active_blocks*block_size,32)
	occupancy_val=ceiling_div((1000000*n_active_blocks*block_size),(32*max_warps_per_sm))
#	print("occ", occupancy_val);
#	occupancy_val=(int(occupancy_val*100))
	occupancy_val=(1.0*occupancy_val)/1000000;
	if verbose:
		print("occ", occupancy_val);
	occupancy_file_name="kernel_"+kernel_name+"_occupancy.tmp"
	occupancy_file=open(occupancy_file_name, "w");
	occupancy_file.write(str(occupancy_val))
	occupancy_file.close();


#######################################
#######################################
def main():
	argc=len(sys.argv);
	if argc<3:
		print("requires: [0]: kernel_name [1]: sm_cc");
		exit(1);
	else:
		kernel_name=sys.argv[1];
		arch=sys.argv[2];
		verbose=0;
		if argc>3:
			verbose=int(sys.argv[3]);
		get_active_blocks(kernel_name,arch, verbose);
		exit(0)

#######################################
if __name__ == "__main__":
	main()
