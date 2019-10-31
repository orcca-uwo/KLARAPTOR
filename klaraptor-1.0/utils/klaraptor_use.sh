#!/bin/bash

#######################################
check_err()
{
	if [[ $? -ne 0 ]];then
		printf "ERROR!\n"
		exit -1;
	fi;
}
#######################################
hrule()
{
	n=67;
	s=""
	for ((i=0;i<$n;i++));do
		s+="=";
	done;
	s+="\n"
	printf "$s"
}
#######################################
init()
{
	LLVM_PASS_COMMON_PATH="$KLARAPTOR_PATH/src/io_builder"
	INCLUDE_OPTS="-I$LLVM_PASS_COMMON_PATH\
		-I$LLVM_PASS_COMMON_PATH/include\
		-I$CUDA_PATH/include"
	CUDA_OPTS="\
						--cuda-gpu-arch=sm_$SM_ARCH --cuda-path=$CUDA_PATH\
						-L$CUDA_PATH/lib64 -lcudart_static -ldl -lrt -pthread\
						-Wunused-command-line-argument\
						-lcuda\
						-w"

	LINK_OPTS="-L$LLVM_PASS_COMMON_PATH/lib\
		-L.\
		-lkernel_invoker\
		-lptx_lookup"

	LLVM_VERSION="$(cat $LLVM_PASS_COMMON_PATH/LLVM_VERSION.inc)"
	LLVM_VERSION="${LLVM_VERSION/LLVM_VERSION:=/}"
	#######################################
	CC="clang++-$LLVM_VERSION -w $INCLUDE_OPTS -fPIC -O2 -std=c++11 -D_GLIBCXX_USE_CXX11_ABI=1" 
	# -flto"
	#######################################
	PASS_DIR="$LLVM_PASS_COMMON_PATH/io_builder_pass"	
	PASS_SRC="io_builder_pass"
	PASS_SO_NAME="lib""$PASS_SRC"
	PASS_NAME="io_builder_pass"
	RP_CONNECTOR="$PASS_DIR""/rp_connector.bc"
	#######################################
	TARGET="$CUDA_SRC""_instrumented"
	#######################################
	DRIVER_API_SUFFIX="driver_call"
	BC_STR="$RP_CONNECTOR"" ""$CUDA_SRC"_"$DRIVER_API_SUFFIX"".bc"
	#######################################	
}
#RM_LIST=$(RM:.%=*.% ) 
#######################################
#all: io-builder rp-builder

#######################################
io_builder()
{	
	printf "[@io_builder]...\n"
	io_pass_builder_for_host > /dev/null 
	check_err
	link_with_rp_connector_bc > /dev/null
	check_err
	printf "[@io_builder][applying io_builder_pass to bitcode]...\n"
#	set -x;
#	printf "opt -load \$PASS_DIR/$PASS_SO_NAME -$PASS_NAME <all.bc> $TARGET.bc\n" 	
	opt-$LLVM_VERSION -load $PASS_DIR/$PASS_SO_NAME".so" -$PASS_NAME <all.bc> $TARGET.bc
	if [[ $? -ne 0 ]]; then
		opt-$LLVM_VERSION -load	$PASS_DIR/$PASS_SO_NAME"_cxx11_abi_disabled.so" -$PASS_NAME <all.bc> $TARGET.bc
	fi;
#	set +x;
	check_err
	
	$CC $TARGET.bc $CUDA_OPTS $LINK_OPTS -o $TARGET.bin	
	check_err
#	@#just for experimental purposes.
#	$CC -S -emit-llvm $TARGET.bc
	rm -rf *.ll *.bc
	printf "[@io_builder][applying io_builder_pass to bitcode][DONE]\n"
#	exit;
}
#######################################
link_with_rp_connector_bc()
{	
	build_host_ir
	check_err
	llvm-link-$LLVM_VERSION $BC_STR -S -o=all.bc
	check_err
#	printf "link_with_rp_connector_bc\n"
#	exit;
}
#######################################
test_cuda()
{
	build_host_ir
	check_err
	$CC "$CUDA_SRC"_"$DRIVER_API_SUFFIX".ll $CUDA_OPTS -o res.bin
	check_err
}
#######################################
build_host_ir()
{
	build_ptx_lookup_table
	check_err
	build_kernel_invoker
	check_err
	cmd="$CC $CUDA_OPTS "
	$cmd -c -emit-llvm "$CUDA_SRC"_"$DRIVER_API_SUFFIX".cu
	check_err
#	$cmd -S -emit-llvm "$CUDA_SRC"_"$DRIVER_API_SUFFIX".cu
}
#######################################
build_ptx_lookup_table()
{
	## remove
	printf "in build_ptx_lookup_table...\n";
	$LLVM_PASS_COMMON_PATH/ptx_lookup/build_ptx_lookup.sh $CUDA_SRC.cu sm_$SM_ARCH
	check_err
}

#######################################
build_kernel_invoker()
{

#	printf "building libkernel_invoker.so\n"
	(cd $LLVM_PASS_COMMON_PATH/kernel_invoker; make >/dev/null;)
	check_err
}
#######################################
#######################################
io_pass_builder_for_host()
{
	(cd $PASS_DIR; make;)
	check_err
}
#######################################
rp_builder()
{
	cp $KLARAPTOR_PATH/src/dp_builder/build_dp_for_kernels.sh .
	chmod u+x build_dp_for_kernels.sh
	
	./build_dp_for_kernels.sh --gen-interpolation-template $PROBLEM_DIM 
	check_err
	
	./build_dp_for_kernels.sh --interpolation $CUDA_SRC sm_$SM_ARCH $OPT_LEVEL	
	check_err
	printf "[@dp_builder][data collection + interpolation]...\n\n"
#	(cd $KLARAPTOR_PATH/src/device_specs; make run;)
	
	./interpolation.bin $KLARAPTOR_PATH/src/device_specs/$DEVICE_NAME.specs 0 \
		$INTERP_LOWER_BOUND $INTERP_UPPER_BOUND
	check_err
	printf "[@dp_builder][data collection + interpolation][DONE]\n"

	./build_dp_for_kernels.sh --dp $CUDA_SRC sm_$SM_ARCH $OPT_LEVEL
	check_err
#	./build_dp_for_kernels.sh --clean-intermediate
	rm -rf build_dp_for_kernels.sh
	rm -rf current_device.tmp
}	
#######################################
clean()
{

	RM=".o .out .s .ll .bin .bc .tmp .smem .registers .ptx .ptxsass"
	RM_LIST="${RM//./*.}"
	echo "$RM_LIST"
	rm -rf $RM_LIST
	rm -rf kernel_name_list.txt
	rm -rf $CUDA_SRC_$DRIVER_API_SUFFIX.cu
	rm -rf ptx_lookup_table.h libptx_lookup.so

	cp $KLARAPTOR_PATH/src/dp_builder/build_dp_for_kernels.sh .
	chmod u+x build_dp_for_kernels.sh
	./build_dp_for_kernels.sh --clean
	printf "DP CLEAN"
	
	rm -rf build_dp_for_kernels.sh
	rm -rf mwp_cwp.h
	rm -rf ptx_lookup.cpp
	rm -rf cudabin_name
}
#######################################
exit_error_msg()
{
	printf "args:
  [1]:--cuda-src=CUDA_SRC.cu
  [2]:--sm-arch=SM_ARCH
  [3]:--device-name=DEVICE_NAME
  [4]:--opt-evel=OPT_LEVEL (optional, default=3)
  [5]:--lower-bound=LOWER_BOUND (optional, default=32)
  [6]:--upper-bound=upper-bound (optional, default=32)\n"
	exit -1;
}

#######################################
exit_success()
{
	exit 0;
}
#######################################

argc=$#
########################################
if [[ $argc -eq 1 ]]; then
	if [[ "$1" == "--clean" ]];then
		clean
		exit_success
	fi;
	exit_error_msg
fi;
########################################
if [[ $argc -lt 3 ]]; then
	exit_error_msg
fi;
########################################

arg_list=("cuda-src" "sm-arch" "device-name" "opt-level" "lower-bound" "upper-bound" "problem-dim")
values=( $@ )
clean_values=( "" "" "" "3" "32" "32" "1")

for ((i=0;i<$argc;i++)); do
	arg_val=${values[$i]};
	arg=${arg_list[$i]};
	arg="--""$arg""="
	val="${arg_val/$arg/}"
#	echo "[$arg_val] = -- [$arg][$val]"
	clean_values[$i]=$val
done;

#echo "clean values"
#echo "${clean_values[@]}"

#CUDA_SRC=2DConvolution
#DEVICE_NAME=gtx1080ti
#SM_ARCH=60
#INTERP_LOWER_BOUND=32
#INTERP_UPPER_BOUND=32
#OPT_LEVEL=3

CUDA_SRC=${clean_values[0]}
CUDA_SRC="${CUDA_SRC/.cu/}"
SM_ARCH=${clean_values[1]}
DEVICE_NAME=${clean_values[2]}
OPT_LEVEL=${clean_values[3]}
INTERP_LOWER_BOUND=${clean_values[4]}
INTERP_UPPER_BOUND=${clean_values[5]}
PROBLEM_DIM=${clean_values[6]}

init
io_builder
rp_builder
#######################################
