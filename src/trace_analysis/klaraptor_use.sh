
#######################################
init()
{
	CUDA_OPTS="\
						--cuda-gpu-arch=sm_$SM_ARCH --cuda-path=$CUDA_PATH\
						-L$CUDA_PATH/lib64 -lcudart_static -ldl -lrt -pthread\
						-Wunused-command-line-argument\
						-I$CUDA_PATH/include\
						-lcuda\
						-w"
	CC="clang++-12 -I$LLVM_PASS_COMMON_PATH/ -w"
	#######################################
	PASS_DIR="$LLVM_PASS_COMMON_PATH/io_builder_pass"
	PASS_SRC="io_builder_pass"
	PASS_SO_NAME="lib""$PASS_SRC"".so"
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
	io_pass_builder_for_host 
	link_with_rp_connector_bc	
	opt-12 -load $PASS_DIR/$PASS_SO_NAME -$PASS_NAME <all.bc> $TARGET.bc
	$CC $TARGET.bc $CUDA_OPTS -o $TARGET.bin	
#	@#just for experimental purposes.
	$CC -S -emit-llvm $TARGET.bc
	rm -rf *.ll *.bc #ptx_lookup_table.h $(CUDA_SRC)_$(DRIVER_API_SUFFIX).cu
}
#######################################
link_with_rp_connector_bc()
{	
	build_host_ir
	llvm-link-12 $BC_STR -S -o=all.bc
}
#######################################
test_cuda()
{
	build_host_ir
	$CC -std=c++14 "$CUDA_SRC"_"$DRIVER_API_SUFFIX".ll $CUDA_OPTS -o res.bin
}
#######################################
build_host_ir()
{
	build_ptx_lookup_table

	$CC -std=c++14 $CUDA_OPTS -c -emit-llvm "$CUDA_SRC"_"$DRIVER_API_SUFFIX".cu
	$CC -std=c++14 $CUDA_OPTS -S -emit-llvm "$CUDA_SRC"_"$DRIVER_API_SUFFIX".cu	
}
#######################################
build_ptx_lookup_table()
{
	$LLVM_PASS_COMMON_PATH/build_ptx_lookup.sh $CUDA_SRC.cu sm_$SM_ARCH
}
#######################################
#######################################
io_pass_builder_for_host()
{
	(cd $PASS_DIR; make;)
}
#######################################
rp_builder()
{
	cp $KLARAPTOR_PATH/trace_analysis/build_dp_for_kernels.sh .
	./build_dp_for_kernels.sh --gen-interpolation-template $PROBLEM_DIM 
	./build_dp_for_kernels.sh --interpolation $CUDA_SRC sm_$SM_ARCH $OPT_LEVEL
	./interpolation.bin $KLARAPTOR_PATH/device_profiles/$DEVICE_NAME.specs 0 \
		$INTERP_LOWER_BOUND $INTERP_UPPER_BOUND
	./build_dp_for_kernels.sh --dp $CUDA_SRC sm_$SM_ARCH $OPT_LEVEL
	./build_dp_for_kernels.sh --clean-intermediate
}	

#######################################
clean()
{

	RM=".o .out .so .s .ll .bin .bc .tmp .smem .registers .ptx .ptxass"
	RM_LIST="${RM//./*.}"
	echo "$RM_LIST"
	rm -rf $RM_LIST
	rm -rf kernel_name_list.txt
	rm -rf $CUDA_SRC_$DRIVER_API_SUFFIX.cu
	rm -rf ptx_lookup_table.h	
	if [[ -e build_dp_for_kernels.sh ]]; then
		./build_dp_for_kernels.sh --clean
	fi;
	rm -rf build_dp_for_kernels.sh
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
	echo "[$arg_val] = -- [$arg][$val]"
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
