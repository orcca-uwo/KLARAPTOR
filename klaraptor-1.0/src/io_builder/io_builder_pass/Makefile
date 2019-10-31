
#######################################
SHELL:=/bin/bash
#######################################
include ../LLVM_VERSION.inc
LLVM_VERSION ?= 0
#PASS_DIR=. #$(LLVM_PASS_COMMON_PATH)/io_builder_pass
SRC=rp.cpp connector.cpp
PASS_SRC=io_builder_pass
PASS_SO_NAME:=lib$(PASS_SRC).so
PASS_NAME:=io_builder_pass
OBJ:=$(SRC:%.cpp=%.bc)

KLARAPTOR_PATH := ${KLARAPTOR_PATH}

CC=clang++-$(LLVM_VERSION) -w -O2 #-I$(KLARAPTOR_PATH)/src/io_builder/include
BC_STR:=$(SRC:%.cpp=%.bc) 

#######################################
all: $(PASS_SO_NAME) #link-bc	

#######################################
## 
#######################################
$(PASS_SO_NAME): rp_connector.bc
	$(CC)\
		-fno-rtti -std=c++11 -D_GLIBCXX_USE_CXX11_ABI=1\
		-fPIC -shared -o lib$(PASS_SRC).so\
		-I/usr/include/llvm-$(LLVM_VERSION)/\
		-I/usr/include/llvm-c-$(LLVM_VERSION)/\
		-L/usr/lib/llvm-$(LLVM_VERSION)/lib\
		$(PASS_SRC).cpp
	$(CC)\
		-fno-rtti -std=c++11 -D_GLIBCXX_USE_CXX11_ABI=0\
		-fPIC -shared -o lib$(PASS_SRC)_cxx11_abi_disabled.so\
		-I/usr/include/llvm-$(LLVM_VERSION)/\
		-I/usr/include/llvm-c-$(LLVM_VERSION)/\
		-L/usr/lib/llvm-$(LLVM_VERSION)/lib\
		$(PASS_SRC).cpp

#######################################
#######################################
rp_connector.bc: $(OBJ)
	llvm-link-$(LLVM_VERSION) $(BC_STR) -S -o=rp_connector.bc

#######################################
%.bc: %.cpp
	$(CC) -c -emit-llvm $^
	$(CC) -S -emit-llvm $^

#######################################
#######################################
RM=.o .out .so .s .ll .bin .bc .tmp .smem .registers  .ptx
RM_LIST=$(RM:.%=*.% ) 
clean: 
	@#echo $(RM_LIST)
	@rm -rf $(RM_LIST)

#######################################
