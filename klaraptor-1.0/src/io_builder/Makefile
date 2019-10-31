TARGETS=io_builder_pass\
				kernel_invoker\
				ptx_lookup

DIRS=$(TARGETS:%=%.dir)
#######################################
all: $(DIRS)
#######################################
%.dir: %
	@echo $<
	@(cd $<; make;)
#######################################
DIRS=$(TARGETS:%=%.clean)
clean: $(DIRS)

%.clean: %
	@echo $<
	@(cd $<; make clean;)
#######################################
