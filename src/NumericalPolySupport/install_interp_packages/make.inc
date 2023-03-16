
#####
# Installation directories for CLAPACK, ATLAS, GMP
#####


CLAPACK_HOME = /home/taab/CLAPACK
ATLAS_HOME = /usr/lib/x86_64-linux-gnu/
ATLAS_LIB = $(ATLAS_HOME)
GMP_LIB = /data/d1/gmp-6.1.0/build/lib/


##### 
# LIB FILE PATHS
#####

LAPACKLIB = $(CLAPACK_HOME)/lapack_LINUX.a
BLASLIB   = $(CLAPACK_HOME)/libcblaswr.a $(ATLAS_LIB)/libcblas.a $(ATLAS_LIB)/libatlas.a 
F2CLIB    = $(CLAPACK_HOME)/F2CLIBS/libF77.a $(CLAPACK_HOME)/F2CLIBS/libI77.a

NUMPOLYSUPPORT_LIBARGS = $(LAPACKLIB) $(BLASLIB) $(F2CLIB)


