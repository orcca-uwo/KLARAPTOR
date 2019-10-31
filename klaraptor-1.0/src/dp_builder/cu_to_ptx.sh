#!/bin/bash

hrule="========================================\n";
suffix=".tmp"
#######################################

argc=$#
if [[ $argc -lt 2 ]]; then
	printf "args: [0]: cuda_file_name.cu [1]: sm_cc \n";
	exit -1;
fi;


if [[ -f include_path_list ]]; then
	pathlist=( $(cat include_path_list) );
	path_str=''
	for p in ${pathlist[@]}; do
#		echo $p
		path_str+="$p:"
	done
	export C_INCLUDE_PATH=$C_INCLUDE_PATH:$path_str;
	export CPLUS_INCLUDE_PATH=$CPLUS_INCLUDE_PATH:$path_str;
fi;

#######################################
cu_file="$1"
arch="$2"
opt_level=3;
verbose=0;

if [[ $verbose -eq 1 ]]; then
	printf "$hrule"
fi;
if [[ $argc -ge 3 ]]; then
	opt_level="$3";
	printf "[@cu_to_ptx.sh][Setting opt_level=$opt_level]...\n";
fi;

if [[ $verbose -eq 1 ]]; then
	printf "[@cu_to_ptx][cu_to_ptx_OPT_LEVEL=$opt_level]...\n"
	printf "$hrule"
fi;

#######################################
## check if the requested arch is available in nvcc
cc="${arch/sm_/}"
script_path="$(dirname $0)""/get_compatible_sm_list.py"
cc_list=( $(python $script_path) )
n_cc_list=${#cc_list[@]}
last_idx=$((n_cc_list-1))
highest_sm=${cc_list[last_idx]}
## if the requres cc is not feasible, set it to maximum available.
if [[ $cc -gt $highest_sm ]]; then
	printf "\n[CC=$cc is not available -> Setting NVCC arch to $highest_sm] ...\n"
	arch="sm_$highest_sm";
fi;
opts="-Xptxas=-O$opt_level -Xcompiler=-O$opt_level -Xptxas=-dlcm=cg"

#######################################
raw_name="${cu_file/.cu/}"
ll_file="$raw_name""-cuda-nvptx64-nvidia-cuda-""$arch"".ll"
ptx_file="$raw_name""_$arch"".ptx"
ptx_sass="$raw_name"_$arch".ptxsass"
register_info="$raw_name""_$arch"".registers""$suffix"
smem_info="$raw_name""_$arch"".smem""$suffix"
compute_cc="${arch/sm/compute}"

#printf "emitting [$ll_file] ... \n"
#clang -S -emit-llvm --cuda-gpu-arch="$arch" "$cu_file"
###llc -mcpu=sm_20 kernel-cuda-nvptx64-nvidia-cuda-sm_20.ll -o kernel.ptx


if [[ $verbose -eq 1 ]]; then
	printf "$hrule"
	printf "[@cu_to_ptx][generating [$ptx_file] from emitted llvm file]... \n"
#	printf "[ptx_sass][$ptx_sass]\n"
#	printf "[compute_cc][$compute_cc]\n"
	printf "$hrule"
fi;
#
#llc -mcpu="$arch" "$ll_file" -o "$ptx_file" 
nvcc="nvcc"
$nvcc -w -lcuda $opts -ptx -arch="$arch" $cu_file -o $ptx_file 
#set -x
$nvcc -w -lcuda -Xptxas=-v -Xptxas=-v -arch="$arch" $cu_file -c 2> $register_info
#set -x
#ptxas $ptx_file -m64 -O3 --gpu-name $compute_cc --output-file $ptx_sass 
ptxas $ptx_file -m64 -O3 --gpu-name $arch --output-file $ptx_sass 
#set +x
rm "${cu_file/.cu/.o}"
IFS=" "
n_smem=( $(cat $register_info|grep -i smem) )
n_registers=( $(cat $register_info|grep -i register) )

n_smem=${n_smem[6]} 
n_registers=${n_registers[4]}

#echo $((n_registers)) > $register_info
#echo $((n_smem)) > $smem_info

ptx_params_file="$raw_name""_ptx_params.tmp"

if [[ $verbose -eq 1 ]]; then
	printf "$ptx_params_file"
fi;
cat /dev/null > $ptx_params_file
printf "[shared_mem_bytes_static: ""$((n_smem))""]\n" >> $ptx_params_file
printf "[registers: ""$((n_registers))""]\n" >> $ptx_params_file

#######################################
printf "\n";

if [[ $verbose -eq 1 ]]; then
	printf "$hrule"
	printf "[@cu_to_ptx][DONE]...\n"
	printf "$hrule"
fi;
