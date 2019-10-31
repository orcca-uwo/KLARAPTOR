#!/bin/bash

verbose=0;

hrule()
{
	printf "==================================================\n"
}
LLVM_PASS_COMMON_PATH="$KLARAPTOR_PATH/src/io_builder"
LLVM_VERSION="$(cat $LLVM_PASS_COMMON_PATH/LLVM_VERSION.inc)"
LLVM_VERSION="${LLVM_VERSION/LLVM_VERSION:=/}"

######################################

#cwd="$(pwd)"
cu_to_ptx()
{
  argc=$#
  if [[ $argc -lt 2 ]]; then
    printf "please provide  [0]: cuda_file_name.cu [1]: sm_cc \n";
    exit -1;
  fi;


  if [[ -f include_path_list ]]; then
    pathlist=( $(cat include_path_list) );
    path_str=''
    for p in ${pathlist[@]}; do
  #    echo $p
      path_str+="$p:"
    done
    export C_INCLUDE_PATH=$C_INCLUDE_PATH:$path_str;
    export CPLUS_INCLUDE_PATH=$CPLUS_INCLUDE_PATH:$path_str;
  fi;

  #######################################
  cu_file="$1"
  arch="$2"
  opt_level=3

  #######################################
  if [[ $argc -ge 3 ]]; then
    opt_level="$3"
  #  printf "got opt level from user -> $opt_level\n"
  fi;

  #######################################
  ## check if the requested arch is available in nvcc
  cc="${arch/sm_/}"

  IFS=' '  
  cc_list=( $(python $LLVM_PASS_COMMON_PATH/get_compatible_sm_list.py;) )
  n_cc_list=${#cc_list[@]}
  last_idx=$((n_cc_list-1))
  highest_sm=${cc_list[last_idx]}
  ## if the requres cc is not feasible, set it to maximum available.
  if [[ $cc -gt $highest_sm ]]; then
    printf "\n[CC=$cc is not available -> Setting NVCC arch to $highest_sm] ...\n"
    arch="sm_$highest_sm";
  fi;
  #######################################
  #opt_level="$(cat ../opt_level.mk)"
  #opt_level="${opt_level/*TRACE_OPT_LEVEL:=/}"
  #opt_level="${opt_level/\n*/}"
  #opt_level=3
  if [[ $verbose -eq 1 ]]; then
  	printf "TRACE_OPT_LEVEL=$opt_level\n"
	fi;

  opts="-Xptxas=-O$opt_level -Xcompiler=-O$opt_level -Xptxas=-dlcm=cg"

  #######################################
  raw_name="${cu_file/.cu/}"
  ll_file="$raw_name""-cuda-nvptx64-nvidia-cuda-""$arch"".ll"
  ptx_file="$raw_name""_$arch"".ptx"
  ptx_sass="$raw_name""_$arch"".ptxsass"
  #ptx_obj2="$raw_name""_$arch"".ptxobj"
  ptx_fatbin="$raw_name""_$arch"".fatbin"
  register_info="$raw_name""_$arch"".registers"
  smem_info="$raw_name""_$arch"".smem"
  compute_cc="${arch/sm/sm}"

  #printf "emitting [$ll_file] ... \n"
  #clang -S -emit-llvm --cuda-gpu-arch="$arch" "$cu_file"
  ###llc -mcpu=sm_20 kernel-cuda-nvptx64-nvidia-cuda-sm_20.ll -o kernel.ptx

#  printf "[$ll_file] --> [$ptx_file] ... \n"
  #llc -mcpu="$arch" "$ll_file" -o "$ptx_file" 
  nvcc="nvcc"
#  $nvcc -w -lcuda $opts -ptx -arch="$arch" $cu_file -o $ptx_file   
  nvcc_opts="-w -lcuda $opts -ptx -arch=$arch $cu_file -o $ptx_file"
  if [[ $verbose -eq 1 ]]; then
  	echo "nvcc $nvcc_opts"
	fi;
  nvcc $nvcc_opts
	
	## create ptx_object (SASS)
	## todo: should it be m32?
	ptxas -m64 -O3 --gpu-name $compute_cc --output-file $ptx_sass $ptx_file

	## create fatbin using ptx_object  and ptx_asm
#	fatbinary --cuda -64 --create $ptx_fatbin\
#		--image=profile=$arch,file=$ptx_sass\
#		--image=profile=$compute_cc,file=$ptx_file
#
	#### reduce the target sm to sm_20 for working with depreacated devices.
  #### sed -i 's/.target sm_.*/.target sm_20/' $ptx_file
  
  
#  $nvcc -w -lcuda -Xptxas=-v -Xptxas=-v -arch="$arch" $cu_file -c 2> $register_info
#  rm "${cu_file/.cu/.o}"
#  IFS=" "
#  n_smem=( $(cat $register_info|grep -i smem) )
#  n_registers=( $(cat $register_info|grep -i register) )
#
#  n_smem=${n_smem[6]} 
#  n_registers=${n_registers[4]}
#
#  echo $((n_registers)) > $register_info
#  echo $((n_smem)) > $smem_info
#
#  ptx_params_file="$raw_name""_ptx_params.tmp"
##  printf "$ptx_params_file"
#  cat /dev/null > $ptx_params_file
#  printf "[shared_mem_bytes_static: ""$((n_smem))""]\n" >> $ptx_params_file
#  printf "[registers: ""$((n_registers))""]\n" >> $ptx_params_file
 	hrule 
}

###########################################################
###########################################################
build_ptx_lookup_src()
{
  #make clean 1>/dev/null 2>&1

  src="$1"
  arch="$2" ##sm_30
  result_list="kernel_name_list_for_ptx_lookup.txt" ## ready for creating ptx lookup table.
  opt_level=3
	
	result_header_ptx="ptx_lookup.cpp"
	#result_header_ptxsass="ptxsass_lookup_table.h"
  cat /dev/null > $result_header_ptx
  #cat /dev/null > $result_header_ptxsass
  ###########################################################
  ## extract .cu for each kernel, generate ptx
  python $LLVM_PASS_COMMON_PATH/ptx_lookup/ptx_parser.py $src $arch

  cat /dev/null > $result_list
  if [[ $verbose -eq 1 ]]; then
  	printf "[writing result to [$result_list]]...\n"
	fi;

  IFS=$'\n'
  kernel_list=( $(cat complete_kernel_name_list.tmp) );
  ptx_fatbin_str=""
  for k in ${kernel_list[@]} ; do
    name="kernel_"$k
    cu_name="$name"".cu"
    ptx_name="$name""_$arch"".ptx"
#    ptx_fatbin_name="$name""_$arch"".fatbin"
#    ptx_fatbin_str="$ptx_fatbin_str""$ptx_fatbin_name "
		if [[ $verbose -eq 1 ]]; then
	    printf "[$k] ---> [$cu_name] --> [$ptx_name]...\n" #" [$ptx_fatbin_name] ... \n";
	  fi;

    cu_to_ptx $cu_name $arch $opt_level  
    rm -rf $cu_name
    printf "$ptx_name\n" >> $result_list
  done;

#	ptx_fatbin_str="${ptx_fatbin_str/ $/}"
#  nvcc -dlink *.fatbin -o all_ptx.ptxobj
  ###########################################################
#  make build-ptx-lookup
#	PTX_LOOKUP:=build_ptx_lookup
#	build-ptx-lookup:
	clang++-$LLVM_VERSION -g -std=c++11 -O2 \
		$LLVM_PASS_COMMON_PATH/ptx_lookup/build_ptx_lookup.cpp\
		-o build_ptx_lookup.bin
  ./build_ptx_lookup.bin "$result_list" "$result_header_ptx" 
  rm -rf $result_list
  rm -rf common_includes.h
  rm -rf *.ptx
  rm -rf build_ptx_lookup.bin
#  rm -rf *.ptxsass
  #make clean 1>/dev/null 2>&1
  if [[ $verbose -eq 1 ]]; then
  	printf "\n[@$this][DONE]\n"
		hrule		
	fi;
}

##########################################################
build_ptx_lookup_so()
{
	so_name="${result_header_ptx/.cpp/.so}"
	so_name="lib""$so_name"
	clang++-$LLVM_VERSION -g -std=c++11 -O2 -fpic -shared "$result_header_ptx"\
		-o "$so_name"\
		-I$LLVM_PASS_COMMON_PATH/ptx_lookup
#	rm -rf "$result_header_ptx"
}

argc=$#
this="$0"
this="${this/*\//}"

if [[ $verbose -eq 1 ]]; then
	hrule
	printf "[@$this]...\n"
fi;

if [[ $argc -lt 2 ]]; then
  printf "args [1]:src.cu [2]:sm_cc \n"
 	hrule 
  exit -1;
fi;
build_ptx_lookup_src $1 $2
build_ptx_lookup_so
###########################################################
