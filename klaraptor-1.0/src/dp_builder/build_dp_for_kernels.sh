#!/bin/bash

## file: 
## author:
## brief: 

hrule()
{
	printf "==================================================================\n"
}
#######################################
set_cc()
{
	
	LLVM_VERSION="$(cat $KLARAPTOR_PATH/src/dp_builder/LLVM_VERSION.inc)"
	LLVM_VERSION="${LLVM_VERSION/LLVM_VERSION:=/}"
	numpolysupport_path="$KLARAPTOR_PATH/src/RatFunInterp";
	NUMPOLYSUPPORT_LIBARGS="$(cd $numpolysupport_path;\
		chmod +x get_numpolysupport_libargs.sh;\
		./get_numpolysupport_libargs.sh;)"
#	echo "$NUMPOLYSUPPORT_LIBARGS"
	
	CC="clang-$LLVM_VERSION -O2" #-O3 -flto" ##-g
	DIR_NAME="$(echo ${PWD/*\//})"
#	echo $DIR_NAME
	RESULT_DIR="../results/""$DIR_NAME"
	VERBOSE=0;
	ALLVARIABLE=0
	##############################
	#-fcilkplus -lcilkrts
	COMPILER_OPTS="-std=gnu11 -Wno-declaration-after-statement -w" #-fopenmp
	LIBARG="$numpolysupport_path/libnumpolysupport.a\
					$NUMPOLYSUPPORT_LIBARGS\
					-L -lgmpxx -lgmp -lm -lc\
					-DVERBOSE=$VERBOSE\
					-DALLVARIABLE=$ALLVARIABLE"	
	INCLUDE_OPTS="-I$KLARAPTOR_PATH/src/gpu_perf_model/\
		-I$KLARAPTOR_PATH/src/"
}
#######################################
build_interpolation()
{
#	numpolysupport_path="$KLARAPTOR_PATH/src/RatFunInterp";
#	NUMPOLYSUPPORT_LIBARGS="$(cd $numpolysupport_path;\
#		chmod +x get_numpolysupport_libargs.sh;\
#		./get_numpolysupport_libargs.sh;)"
##	echo "$NUMPOLYSUPPORT_LIBARGS"		
#	CC="clang -g" #-O3 -flto" # -g"
#	DIR_NAME="$(echo ${PWD/*\//})"
##	echo $DIR_NAME
#	RESULT_DIR="../results/""$DIR_NAME"
#	VERBOSE=0;
#	ALLVARIABLE=0
#	##############################
#	#-fcilkplus -lcilkrts 
#	COMPILER_OPTS="-fopenmp -std=gnu11 -Wno-declaration-after-statement -w"
#	LIBARG="$numpolysupport_path/libnumpolysupport.a\
#					$NUMPOLYSUPPORT_LIBARGS\
#					-L -lgmpxx -lgmp -lm -lc\
#					-DVERBOSE=$VERBOSE\
#					-DALLVARIABLE=$ALLVARIABLE"
#	set -x
	
	cp $KLARAPTOR_PATH/src/gpu_perf_model/mcwp/mwp_cwp.h .
	$CC $INCLUDE_OPTS $COMPILER_OPTS -o interpolation.bin mwp_cwp.c $LIBARG
	rm mwp_cwp.h
	printf "[@build_data_collection][DONE]\n"
	hrule	
#	set +x 
	build_trace
}


#######################################
#######################################
build_trace()
{
	printf "[@building trace]...\n"
	CWD=$(pwd)
	TRACE_PATH=$KLARAPTOR_PATH/src/dp_builder
#	echo $TRACE_PATH
#	(cd $TRACE_PATH;\
#		./check_installation.sh;)
#		set -x
	python $TRACE_PATH/parser.py $CWD/$SRC.cu $SM_ARCH $OPT_LEVEL;

	#make -f Makefile.cuda
#	exit;
	build_cuda
}


#######################################
#######################################
gen_interpolation_template()
{
	source $KLARAPTOR_PATH/src/dp_builder/gen_interpolation_template.sh\
		$PROBLEM_DIM "$(pwd)"
}

#######################################
#######################################

clean_build_trace()
{
	RM_LIST1=(.o .ll .s .sm_*.ptx .tmp);
	RM_LIST2=(.cu _trace.txt _trace.avg.txt);
	for x in ${RM_LIST1[@]}; do
		rm -rf *.$x;
	done;

	for x in ${RM_LIST2[@]}; do
		rm -rf kernel_*.$x;
	done;

	rm -rf evaluation_result.tmp
	rm -rf ec_evaluation_result.tmp
	#rm -rf tracer_params.log
	rm -rf mwp_cwp.c
}

#######################################
#######################################
build_cuda()
{		
	printf "[@building cuda]...\n"
	SM_CC="${SM_ARCH/sm_/}"
	###################################
	DRIVER_API_SUFFIX=driver_call
	###################################
	GENCODE="-gencode arch=compute_$SM_CC,code=sm_$SM_CC"
	###################################	
	L1_CACHE="-Xptxas=-dlcm=cg"
	###################################
	NVCC_OPT_LEVEL=$OPT_LEVEL
	###################################
	PTX_OPTS="-Xcompiler=-O$NVCC_OPT_LEVEL \
		-Xptxas=-O$NVCC_OPT_LEVEL \
		-O$NVCC_OPT_LEVEL\
		-D_FORCE_INLINES -w\
		-I$KLARAPTOR_PATH/io_builder\
		$L1_CACHE\
		-lcuda -std=c++11"

	CUPTI_PATH="$CUDA_PATH/extras/CUPTI/"
	KERNEL_INVOKER_OPTS="\
		-I$CUDA_PATH/include\
		-L$CUDA_PATH/lib64\
		-I$CUPTI_PATH/include\
		-L$CUPTI_PATH/lib64\
		-I$KLARAPTOR_PATH/src/io_builder/include\
		-L$KLARAPTOR_PATH/src/io_builder/lib\
		-L.\
		-lkernel_invoker\
		-lptx_lookup"
	###################################
	OPTS="$GENCODE $PTX_OPTS $KERNEL_INVOKER_OPTS"
	###################################
#	set -x;
	nvcc $OPTS "$SRC"_"$DRIVER_API_SUFFIX".cu -o "$SRC.bin" 
#	set +x;
	echo "$SRC.bin" > cudabin_name
#	./$SRC.bin
# exit 0
}

#######################################
#######################################
clean_cuda()
{
	rm -rf cudabin_name
	#rm -rf $SRC.bin
}


#######################################
#######################################
build_dp()
{
	##############################
	ALL_OPTS="$COMPILER_OPTS $INCLUDE_OPTS"
	python $KLARAPTOR_PATH/src/gpu_perf_model/mcwp/generate_rational_program.py	
	for k in $(cat kernel_name_list.tmp); do
		dp="driver_program_""$k"
#		set -x
		$CC $ALL_OPTS "$dp".c -o "$dp".bin $LIBARG
#		set +x
		rm -rf "$dp".c
	done;
}

#######################################
#######################################
clean_dp()
{
	
	rm -rf *.o *.ll *.s *.tmp profiling.trace profiling.metrics

	#	rm -rf kernel_name_list.txt
	#	tracer_params.log\
	rm_list_dp_essential="rational_program_*.c\
		nohup.out mwp_cwp.h\	
		cudabin_name\
		kernel_*.smem\
		kernel_*.registers\
		common_includes.h\
		*sm_*.ptx\
		sm_arch.config"
	rm -rf $rm_list_dp_essential

#RM_LIST_ESSENTIAL:=
}

#######################################
#######################################
clean_intermediate()
{	
	printf "CLEAN INTERMEDIATE"
#	clean_cuda
	clean_build_trace	
#	*.tmp  
	rm -rf *.smem *.registers *.ptx *registers.tmp
	rm -rf *.o *.out *.s  *.ll  *.bc *.ptxsass 
	rm -rf *_driver_call.cu
	rm -rf ptx_lookup.cpp
	rm -rf profiling.metrics profiling.trace
	rm -rf sm_arch.config 

	rm_list_dp="configure.ocelot\
		cu_to_ptx.sh\
		kernel_*.cu\
		trace_host.cpp\
		kernel_*_trace.txt*\
		kernel_*_trace.avg.txt*\
		kernel_*_results*.txt\
		kernel_*_occupancy.tmp\
		kernel_*_interpolation.tmp\
		kernel_*.h\
		kernel_*.nvar"

	#		kernel_*_ptx_params.tmp"
	rm -rf $rm_list_dp

}

#######################################
error_input_args()
{
	printf "args: 
	[1]:--interpolation [2]:SRC (.cu file) [3]:SM_ARCH (sm_xx) [4]:OPT_LEVEL(1,2,3)
	[1]:--gen-interpolation-template
	[1]:--dp
	[1]:--clean\n"
	exit -1;
}
#######################################
exit_success()
{
	printf "[DONE]\n"
#	hrule	
	exit 0;
}

#######################################
exit_success_silent()
{	
#	hrule	
	exit 0;
}
#######################################
set_cc;

argc=$#

if [[ $argc -eq 0 ]]; then
	error_input_args
fi;

TASK="$1"
#echo "[@build.sh][TASK=$TASK]..."

hrule
if [[ $argc -ge 1 ]]; then
	###########################################	
	###########################################
	if [[ "$TASK" == "--dp" ]]; then
		build_dp
		exit_success_silent
	fi;
	###########################################	
	###########################################
	if [[ "$TASK" == "--clean-intermediate" ]]; then
		clean_intermediate;
		exit_success_silent	
	fi;	
	###########################################	
	###########################################
	if [[ "$TASK" == "--clean" ]]; then
		clean_intermediate;
		clean_dp;
		exit_success;
	fi;	
fi;

#######################################
#echo "$TASK"
if [[ "$TASK" == "--gen-interpolation-template" ]]; then	
	if [[ $argc -ge 2 ]]; then
		PROBLEM_DIM=$2;	
		printf "[@build_data_collection][generate-data-collector-template]...\n";
		gen_interpolation_template $PROBLEM_DIM
		exit_success
	fi;	
fi;

#######################################
if [[ "$TASK" == "--interpolation" ]]; then	
	if [[ $argc -ge 3 ]]; then
		SRC=$2;
		SM_ARCH=$3;
		OPT_LEVEL=3;

		if [[ $argc -ge 4 ]];then
			OPT_LEVEL="$4";
			printf "[@build_data_collection][setting OPT_LEVEL=$OPT_LEVEL]...\n";
		fi;
		#SRC="2DConvolution.cu"
		#SM_ARCH="sm_60"

		SRC="${SRC/.cu/}"
		build_interpolation
		clean_build_trace
		exit_success
	fi;	
fi;

error_input_args
#######################################
