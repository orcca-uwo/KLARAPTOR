

include ../../NumericalPolySupport/make.inc

SHELL=/bin/bash
CC=gcc
DIR_NAME:="$(shell echo "$${PWD/*\//}";)"
RESULT_DIR=$(shell echo "../results/"$(DIR_NAME))
include ../verbose.mk
VERBOSE?=1

include ../all_variables.mk
ALLVARIABLE?=0
######################################
COMPILER_OPTS=-std=gnu11 -Wno-declaration-after-statement -no-pie -w -O3 -flto
##LIBS=-I -L -lgmp -L../../PolySupport/ -lpolysupport -lm  -DVERBOSE=1
LIBARG =  ../../NumericalPolySupport/libnumpolysupport.a\
					$(NUMPOLYSUPPORT_LIBARGS)\
					-L -lgmpxx -lgmp -lm -lc\
					-DVERBOSE=$(VERBOSE)\
					-DALLVARIABLE=$(ALLVARIABLE)

#######################################
all: trace interpolation

#######################################
trace:
	make -f Makefile_trace

#######################################
interpolation:
	#$(MAKE) -C ../../NumericalPolySupport/
	cp ../../mcwp/mwp_cwp.h .
	$(CC) $(COMPILER_OPTS) -o interpolation.bin mwp_cwp.c $(LIBARG)

#######################################
KERNEL_LIST=
$(foreach k,$(shell cat kernel_name_list.tmp),\
	$(eval KERNEL_LIST+=rational_program_$(k)))

rational-program: generate-rational-program
	$(foreach k,$(KERNEL_LIST),\
		$(shell $(CC) $(COMPILER_OPTS) $(k).c -o $(k).bin $(LIBARG)))


generate-rational-program:
	python ../../mcwp/generate_rational_program.py

#######################################
result:
	@echo copying results to $(RESULT_DIR) ...
	cp kernel_*_results*.txt $(RESULT_DIR) 
	cp kernel_*_interpolation*.tmp $(RESULT_DIR)

#######################################
cp-tracer-params:
	cp tracer_params.log $(RESULT_DIR)

#######################################
result-cuda:
	cp cuda_results_*.txt $(RESULT_DIR)

#######################################
result-rational-program:
	cp rational_program_*_result.txt $(RESULT_DIR)

#######################################
RM=rm -rf 
RM_LIST=""
RM_EXT_LIST= *.o *.ll *.s *.bin *sm_*.ptx *.tmp *.py *.bin\
						 configure.ocelot cu_to_ptx.sh\
						 kernel_*.cu trace_host.cpp kernel_*_trace.txt*\
						 kernel_*_trace.avg.txt* kernel_*_results*.txt\
						 kernel_*.smem kernel_*.registers kernel_*_occupancy.tmp\
						 kernel_*_interpolation.tmp common_includes.h\
						 kernel_*.h kernel_*.nvar\
						 rational_program_*.c\
						 nohup.out mwp_cwp.h
$(foreach ext, $(RM_EXT_LIST), $(eval RM_LIST+=$(ext)))

clean:
	$(RM) $(RM_LIST)
