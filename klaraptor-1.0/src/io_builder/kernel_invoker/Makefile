
#######################################
SHELL:=/bin/bash
#######################################
include ../LLVM_VERSION.inc
SRC=kernel_invoker
LLVM_VERSION ?= 0
OBJ=$(SRC:%.cpp=%.bc)
KLARAPTOR_PATH := ${KLARAPTOR_PATH}

IO_BUILDER_PATH := $(KLARAPTOR_PATH)/src/io_builder
IO_BUILDER_PATH :=../

CC=clang++-$(LLVM_VERSION) -w -O3 # -flto #-g
INCLUDE_OPTS=-I/usr/lib/llvm-$(LLVM_VERSION)/include/\
		-I$(IO_BUILDER_PATH)/include\
		-I$(CUDA_PATH)/include/\
		--cuda-path=$(CUDA_PATH)
LD_OPTS=-L/usr/lib/llvm-$(LLVM_VERSION)/lib\
				-L$(CUDA_PATH)/lib64\
				-lcudart_static -ldl -lrt -pthread -lcuda\


#######################################
all: lib

#######################################
#######################################
lib: $(SRC).cpp
	rm -rf *.so
	@$(CC)\
		-fPIC -shared -o lib$(SRC).so\
		-Wunused-command-line-argument\
		$(SRC).cpp\
	 $(INCLUDE_OPTS)\
	 $(LD_OPTS)
	#cp lib$(SRC).so ../lib


#######################################
#######################################
RM=.o .out .so .s .ll .bin .bc .tmp .smem .registers  .ptx
RM_LIST=$(RM:.%=*.% ) 
clean: 
	@#echo $(RM_LIST)
	rm -rf $(RM_LIST)

#######################################
