TARGETS=		llvm/static_pass/common/io_builder_pass\
				device_profiles\
				kernel_mesh_points\
				NumericalPolySupport

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