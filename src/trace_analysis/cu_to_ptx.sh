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

printf "$hrule"
if [[ $argc -ge 3 ]]; then
	opt_level="$3";
	printf "[@cu_to_ptx.sh][Setting opt_level=$opt_level]...\n";
else
	opt_level="$(cat ../opt_level.mk)"
	opt_level="${opt_level/*TRACE_OPT_LEVEL:=/}"
	opt_level="${opt_level/\n*/}"
	printf "[@cu_to_ptx.sh][Setting opt_level to default in ../opt_level.mk]...\n";	
fi;
printf "[@cu_to_ptx][TRACE_OPT_LEVEL=$opt_level]...\n"
printf "$hrule"
#######################################

#######################################
## check if the requested arch is available in nvcc
cc="${arch/sm_/}"

cc_list=( $(cd ..; python get_compatible_sm_list.py;) )
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
register_info="$raw_name""_$arch"".registers""$suffix"
smem_info="$raw_name""_$arch"".smem""$suffix"

#printf "emitting [$ll_file] ... \n"
#clang -S -emit-llvm --cuda-gpu-arch="$arch" "$cu_file"
###llc -mcpu=sm_20 kernel-cuda-nvptx64-nvidia-cuda-sm_20.ll -o kernel.ptx


printf "$hrule"
printf "[@cu_to_ptx][generating [$ptx_file] from emitted llvm file]... \n"
printf "$hrule"

#llc -mcpu="$arch" "$ll_file" -o "$ptx_file" 
nvcc="nvcc"
$nvcc -w -lcuda $opts -ptx -arch="$arch" $cu_file -o $ptx_file 
sed -i 's/.target sm_.*/.target sm_20/' $ptx_file
$nvcc -w -lcuda -Xptxas=-v -Xptxas=-v -arch="$arch" $cu_file -c 2> $register_info
rm "${cu_file/.cu/.o}"
IFS=" "
n_smem=( $(cat $register_info|grep -i smem) )
n_registers=( $(cat $register_info|grep -i register) )

n_smem=${n_smem[6]} 
n_registers=${n_registers[4]}

#echo $((n_registers)) > $register_info
#echo $((n_smem)) > $smem_info

ptx_params_file="$raw_name""_ptx_params.tmp"
printf "$ptx_params_file"
cat /dev/null > $ptx_params_file
printf "[shared_mem_bytes_static: ""$((n_smem))""]\n" >> $ptx_params_file
printf "[registers: ""$((n_registers))""]\n" >> $ptx_params_file

#######################################
printf "\n";
printf "$hrule"
printf "[@cu_to_ptx][DONE]...\n"
printf "$hrule"
