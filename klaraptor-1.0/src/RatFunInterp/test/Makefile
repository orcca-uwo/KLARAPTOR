
include ../make.inc

CPP = g++
CC  = gcc
INCLUDES = ../include
COMPILEARG = -I$(INCLUDES) -I$(ATLAS-INCLUDE) -I$(GMP-INCLUDE) -c -g -fopenmp 
#LIBARG = -L.. -lnumpolysupport -lm -lc -pie 
LIBARG = ../libnumpolysupport.a $(NUMPOLYSUPPORT_LIBARGS) -fopenmp -lgmpxx -lgmp -lm -lc

TARGET = test.bin

all: clean test 

test : $(TARGET)
	./$(TARGET)

test.bin : testLinAlgDouble.o
	$(CPP) -o $@ $^ $(LIBARG)

%.o : %.cpp
	$(CPP) $(COMPILEARG) $<

clean : 
	rm -rf *.o $(TARGET)


