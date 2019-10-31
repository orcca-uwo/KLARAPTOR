#######################################
SHELL:=/bin/bash
IO_BUILDER_PATH=../io_builder/
PROFILER=klaraptor_profiler
#######################################
SRC=kernel_invoker_with_profiler

include LLVM_VERSION.inc
LLVM_VERSION ?= 0
#######################################
CC=clang++-$(LLVM_VERSION) -w -O2 --cuda-host-only -std=c++11 -flto
#######################################
CUPTI_PATH:=$(CUDA_PATH)/extras/CUPTI
#######################################
INCLUDE_OPTS=\
						 -I/usr/lib/llvm-$(LLVM_VERSION)/include/\
						 -I$(CUDA_PATH)/include/\
						 -I$(CUPTI_PATH)/include\
						 -I$(IO_BUILDER_PATH)/include
#######################################
LD_OPTS=-L$(CUDA_PATH)/lib64\
				-L$(CUPTI_PATH)/lib64\
				-L$(IO_BUILDER_PATH)/lib\
				-lcupti\
				-lcudart_static -ldl -lrt -lcuda\
				-lkernel_invoker #-lptx_lookup
#				-L/usr/lib/llvm-$(LLVM_VERSION)/lib\
#######################################
profiler: $(SRC).cpp
	(cd $(IO_BUILDER_PATH); make;)
	$(CC)\
		-fPIC -shared -o lib$(SRC).so\
		-Wunused-command-line-argument\
		$(SRC).cpp\
	 $(INCLUDE_OPTS)\
	 $(LD_OPTS)
	chmod u+x $(PROFILER)
	#cp lib$(SRC).so ../lib	

#######################################
#kernel:
#	@klaraptor --opt klaraptor_settings.conf
#######################################
#profile: kernel
#	nvprof --events inst_executed ./kernel.bin

#######################################
RM=.o .out .so .s .ll .bin .bc .tmp .smem .registers  .ptx
RM_LIST=$(RM:.%=*.% ) 
clean: 
	rm -rf $(RM_LIST)
	chmod -x $(PROFILER)

#######################################
