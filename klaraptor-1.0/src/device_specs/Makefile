SHELL:=/bin/bash
SRC=get_device_prop


SM=$(shell python ./get_compatible_sm_list.py)
#COMPUTE=20
ARCH=
$(foreach sm,$(SM),$(eval ARCH+=-gencode arch=compute_$(sm),code=sm_$(sm)))
#$(foreach sm,$(SM),$(eval ARCH+=-code=sm_$(sm)))

### -dlcm=ca must be enabled to see the effect of L1 cache.
OPT_LEVEL=0
COMPILER_OPTS=-w -DLINUXINTEL64 -lcuda\
							-O$(OPT_LEVEL) -Xptxas=-O$(OPT_LEVEL) -Xcompiler=-O$(OPT_LEVEL)\
							-Xptxas=-dlcm=cg\
							$(ARCH)
all: run
#######################################
$(SRC).bin: $(SRC).cu
	@nvcc $(SRC).cu $(COMPILER_OPTS) -o $(SRC).bin 	

#######################################
run: $(SRC).bin
	@#(cd ../scripts && ./set_power_mizer_mode.sh 1);
	@./$(SRC).bin 
	@#(cd ../scripts && ./set_power_mizer_mode.sh 0);
#######################################
clean:
	rm -rf $(SRC).bin *.specs *.mem
