
#######################################
SHELL:=/bin/bash
#######################################
#PASS_DIR=. #$(LLVM_PASS_COMMON_PATH)/io_builder_pass
SRC=rp.cpp connector.cpp
PASS_SRC=io_builder_pass
PASS_SO_NAME:=lib$(PASS_SRC).so
PASS_NAME:=io_builder_pass
LLVM_VERSION=12
OBJ=$(SRC:%.cpp=%.bc)

CC=clang++-12 -I$(LLVM_PASS_COMMON_PATH)/ -w
BC_STR:=$(SRC:%.cpp=%.bc) 

#######################################
all: pass-builder link-bc	


#######################################
#######################################
pass-builder: $(PASS_SRC).cpp
	clang++-12 -fPIC -shared -o lib$(PASS_SRC).so\
		-I/usr/lib/llvm-$(LLVM_VERSION)/include\
		-L/usr/lib/llvm-$(LLVM_VERSION)/lib\
		$(PASS_SRC).cpp

#######################################
#######################################
link-bc: $(OBJ) 
	llvm-link-12 $(BC_STR) -S -o=rp_connector.bc

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
	rm -rf $(RM_LIST)

#######################################
