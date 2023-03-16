SRC=get_device_prop


SM=$(shell python ../../tests/get_compatible_sm_list.py)
COMPUTE=
ARCH=
$(foreach sm,$(SM),$(eval ARCH+=-gencode arch=compute_$(sm),code=sm_$(sm)))
#$(foreach sm,$(SM),$(eval ARCH+=-code=sm_$(sm)))

### -dlcm=ca must be enabled to see the effect of L1 cache.
OPT_LEVEL=0
COMPILER_OPTS=-w -DLINUXINTEL64 -lcuda\
							-O$(OPT_LEVEL) -Xptxas=-O$(OPT_LEVEL) -Xcompiler=-O$(OPT_LEVEL)\
							-Xptxas=-dlcm=cg\
							$(ARCH)

#######################################
compile:
	@nvcc $(SRC).cu $(COMPILER_OPTS) -o $(SRC).bin 

#######################################
run: compile
	@./$(SRC).bin 

clean:
	rm -rf $(SRC).bin

