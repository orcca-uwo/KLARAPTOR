TARGETS=build_dp_for_kernels.sh\
				cu_to_ptx.sh\
				gen_interpolation_template.sh

ENABLE_TARGETS:=$(TARGETS:%=%.enable)
DISALBE_TARGETS:=$(TARGETS:%=%.disable)

all: $(ENABLE_TARGETS)

clean: $(DISALBE_TARGETS)

%.enable: %
	chmod u+x $<

%.disable: %
	chmod -x $<
