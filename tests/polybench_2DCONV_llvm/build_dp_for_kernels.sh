
#!/bin/bash

#######################################
#######################################
build_interpolation()
{
	numpolysupport_path="$KLARAPTOR_PATH/NumericalPolySupport";
	NUMPOLYSUPPORT_LIBARGS="$(cd $numpolysupport_path;\
		chmod +x get_numpolysupport_libargs.sh;\
		./get_numpolysupport_libargs.sh;)"
	echo "$NUMPOLYSUPPORT_LIBARGS"		
	CC=gcc
	DIR_NAME="$(echo ${PWD/*\//})"
#	echo $DIR_NAME
	RESULT_DIR="../results/""$DIR_NAME"
	VERBOSE=0;
	ALLVARIABLE=0
	##############################
	COMPILER_OPTS="-std=gnu11 -Wno-declaration-after-statement -no-pie -w -O3 -flto"
	LIBARG="$numpolysupport_path/libnumpolysupport.a\
					$NUMPOLYSUPPORT_LIBARGS\
					-fPIC -L -lgmpxx -lgmp -lm -lc\
					-DVERBOSE=$VERBOSE\
					-DALLVARIABLE=$ALLVARIABLE"
	set -x
	cp $KLARAPTOR_PATH/mcwp/mwp_cwp.h .
	$CC $COMPILER_OPTS -o interpolation.bin mwp_cwp.c $LIBARG
	rm mwp_cwp.h
	set +x 
	build_trace
}


#######################################
#######################################
build_trace()
{
	CWD=$(pwd)
	TRACE_PATH=$KLARAPTOR_PATH/trace_analysis
#	(cd $TRACE_PATH;\
#		pwd;\
#		./check_installation.sh;)
	python $TRACE_PATH/parser.py $CWD/$SRC.cu $SM_ARCH $OPT_LEVEL;
	#make -f Makefile.cuda
	build_cuda
}


#######################################
#######################################
gen_interpolation_template()
{
	source $KLARAPTOR_PATH/trace_analysis/gen_interpolation_template.sh\
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
	SM_CC="${SM_ARCH/sm_/}"
	###################################
	DRIVER_API_SUFFIX=driver_call
	###################################
	GENCODE="-gencode arch=compute_$SM_CC,code=sm_$SM_CC"
	###################################	
	L1_CACHE="-Xptxas=-dlcm=ca"
	###################################
	NVCC_OPT_LEVEL=$OPT_LEVEL
	###################################
	PTX_OPTS="-Xcompiler=-O$NVCC_OPT_LEVEL \
		-Xptxas=-O$NVCC_OPT_LEVEL \
		-O$NVCC_OPT_LEVEL\
		-D_FORCE_INLINES -w\
		-I$LLVM_PASS_COMMON_PATH\
		$L1_CACHE\
		-lcuda -std=c++11"
	###################################
	OPTS="$GENCODE $PTX_OPTS"
	###################################
	set -x;
	nvcc $OPTS "$SRC"_"$DRIVER_API_SUFFIX".cu -o "$SRC.bin"
	set +x;
	echo "$SRC.bin" > cudabin_name
#    ./$SRC.bin
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
	numpolysupport_path="$KLARAPTOR_PATH/NumericalPolySupport";
	NUMPOLYSUPPORT_LIBARGS="$(cd $numpolysupport_path;\
		chmod +x get_numpolysupport_libargs.sh;\
		./get_numpolysupport_libargs.sh;)"
#	echo "$NUMPOLYSUPPORT_LIBARGS"		
	CC=gcc
	DIR_NAME="$(echo ${PWD/*\//})"
#	echo $DIR_NAME
	RESULT_DIR="../results/""$DIR_NAME"
	VERBOSE=0;
	ALLVARIABLE=0
	##############################
	COMPILER_OPTS="-std=gnu11 -Wno-declaration-after-statement -no-pie -w -O3 -flto"
	LIBARG="$numpolysupport_path/libnumpolysupport.a\
					$NUMPOLYSUPPORT_LIBARGS\
					-fPIC -L -lgmpxx -lgmp -lm -lc\
					-DVERBOSE=$VERBOSE\
					-DALLVARIABLE=$ALLVARIABLE"
	##############################
	python $KLARAPTOR_PATH/mcwp/generate_rational_program.py	
	for k in $(cat kernel_name_list.tmp); do
		dp="rational_program_""$k"
		$CC $COMPILER_OPTS "$dp".c -o "$dp".bin $LIBARG
        rm -rf "$dp".c
	done;
}

#######################################
#######################################
clean_dp()
{
	
	rm -rf *.o *.ll *.s *.tmp profiling.trace profiling.metrics

	rm -rf kernel_name_list.txt kernel_name_list_for_ptx_lookup.txt *outliers*
	rm_list_dp_essential="rational_program_*.c\
		nohup.out mwp_cwp.h\
		tracer_params.log\
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

	clean_cuda
	clean_build_trace	
#	*.tmp  *.smem  *.registers
	rm -rf *.o *.out  *.so  *.s  *.ll  *.bc *.ptxass  
	#rm -rf *_driver_call.cu
	rm -rf ptx_lookup_table.h
	rm -rf profiling.metrics profiling.trace

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
	printf "[@build.sh][DONE]\n"
	exit 0;
}
#######################################
argc=$#

if [[ $argc -eq 0 ]]; then
	error_input_args
fi;

TASK="$1"
echo "[@build.sh][TASK=$TASK]..."

if [[ $argc -ge 1 ]]; then
	###########################################	
	###########################################
	if [[ "$TASK" == "--dp" ]]; then
		build_dp
		exit_success	
	fi;
	###########################################	
	###########################################
	if [[ "$TASK" == "--clean-intermediate" ]]; then
		clean_intermediate;
		exit_success	
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
echo "$TASK"
if [[ "$TASK" == "--gen-interpolation-template" ]]; then	
	if [[ $argc -ge 2 ]]; then
		PROBLEM_DIM=$2;	
		printf "[@build_interpolation][generate-data-collector-template]...\n";
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
			printf "[@build_interpolation][setting OPT_LEVEL=$OPT_LEVEL]...\n";
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
