TARGETS=\
	polybench_2DCONV_llvm\
	polybench_2MM_llvm\
	polybench_3d_conv_llvm\
	polybench_3MM_llvm\
	polybench_ATAX_llvm\
	polybench_BICG_llvm\
	polybench_common\
	polybench_CORR_llvm\
	polybench_COVAR_llvm\
	polybench_FDTD_2D_llvm\
	polybench_GEMM_llvm\
	polybench_GESUMMV_llvm\
	polybench_GRAMSCHM_llvm\
	polybench_MVT_llvm\
	polybench_SYR2K_llvm\
	polybench_SYRK_llvm\

DIRS=$(TARGETS:%=%.dir)
#######################################
all: $(DIRS)
#######################################
%.dir: %
	@echo $<
	@(cd $<; klaraptor --opt klaraptor_settings.conf;)
#######################################
DIRS=$(TARGETS:%=%.clean)
clean: $(DIRS)

%.clean: %
	@echo $<
	@(cd $<; klaraptor --clean)
#######################################
