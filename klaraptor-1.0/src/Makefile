TARGETS=dp_builder\
				device_specs\
				io_builder\
				profiler\
				RatFunInterp

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
