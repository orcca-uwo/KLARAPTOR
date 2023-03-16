#######################################
SM_ARCH:=75
INTERP_LOWER_BOUND:=32
INTERP_UPPER_BOUND:=64
OPT_LEVEL=3
CUDA_SRC=2DConvolution
#######################################
SHELL:=/bin/bash
#######################################
CUDA_OPTS= \
					 --cuda-gpu-arch=sm_$(SM_ARCH) --cuda-path=$(CUDA_PATH)\
					-L$(CUDA_PATH)/lib64 -lcudart_static -ldl -lrt -pthread\
					-Wunused-command-line-argument\
					-I$(CUDA_PATH)/include\
					-lcuda\
					-w
CC=clang++-12 -I$(LLVM_PASS_COMMON_PATH)/ -w
#######################################
PASS_DIR=$(LLVM_PASS_COMMON_PATH)/io_builder_pass
PASS_SRC=io_builder_pass
PASS_SO_NAME:=lib$(PASS_SRC).so
PASS_NAME:=io_builder_pass
RP_CONNECTOR=$(PASS_DIR)/rp_connector.bc
#######################################
TARGET=$(CUDA_SRC)_instrumented
#######################################
DRIVER_API_SUFFIX=driver_call
BC_STR:=$(RP_CONNECTOR) $(CUDA_SRC)_$(DRIVER_API_SUFFIX).bc
#######################################
RM=.o .out .so .s .ll .bin .bc .tmp .smem .registers .ptx .ptxass
RM_LIST=$(RM:.%=*.% ) 
#######################################
all: io-builder rp-builder

#######################################
io-builder: io-pass-builder-for-host link-with-rp-connector-bc
	opt-12 -load $(PASS_DIR)/$(PASS_SO_NAME) -$(PASS_NAME) <all.bc> $(TARGET).bc
	$(CC) $(TARGET).bc $(CUDA_OPTS) -o $(TARGET).bin	
	@#just for experimental purposes.
	$(CC) -S -emit-llvm $(TARGET).bc
#	rm -rf *.ll *.bc #ptx_lookup_table.h $(CUDA_SRC)_$(DRIVER_API_SUFFIX).cu

#######################################
link-with-rp-connector-bc: build-host-ir
	llvm-link-12 $(BC_STR) -S -o=all.bc

#######################################
test-cuda:build-host-ir
	$(CC) -std=c++14 $(CUDA_SRC)_$(DRIVER_API_SUFFIX).ll $(CUDA_OPTS) -o res.bin
#######################################
build-host-ir: build-ptx-lookup-table
	$(CC) -std=c++14 $(CUDA_OPTS) -c -emit-llvm $(CUDA_SRC)_$(DRIVER_API_SUFFIX).cu
	$(CC) -std=c++14 $(CUDA_OPTS) -S -emit-llvm $(CUDA_SRC)_$(DRIVER_API_SUFFIX).cu	
#######################################
build-ptx-lookup-table: 
	$(LLVM_PASS_COMMON_PATH)/build_ptx_lookup.sh $(CUDA_SRC).cu sm_$(SM_ARCH)

#######################################
#######################################
io-pass-builder-for-host: 
	(cd $(PASS_DIR); make;)

#######################################
rp-builder:
	@cp ../../trace_analysis/build_dp_for_kernels.sh .
	@./build_dp_for_kernels.sh --interpolation $(CUDA_SRC) sm_$(SM_ARCH) $(OPT_LEVEL)
	@./interpolation.bin ../../device_profiles/geforcertx2070super.specs 0 \
	$(INTERP_LOWER_BOUND) $(INTERP_UPPER_BOUND)
	@./build_dp_for_kernels.sh --dp $(CUDA_SRC) sm_$(SM_ARCH) $(OPT_LEVEL)
	@./build_dp_for_kernels.sh --clean-intermediate
	

#######################################
clean:
	rm -rf $(RM_LIST)
	rm -rf kernel_name_list.txt
	rm -rf $(CUDA_SRC)_$(DRIVER_API_SUFFIX).cu
	rm -rf ptx_lookup_table.h
	@./build_dp_for_kernels.sh --clean
	@rm -rf build_dp_for_kernels.sh
#######################################
clean-all: clean



