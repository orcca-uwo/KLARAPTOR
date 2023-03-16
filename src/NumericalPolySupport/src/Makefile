
include ../make.inc

CC = gcc
INCLUDES = ../include
CCOMPILEARG = -I$(INCLUDES) -I$(CLAPACK_HOME) -c -O2 -march=native -Wall -std=c99 

CPPC = g++
CPPCOMPILEARG = -I$(INCLUDES) -I$(CLAPACK_HOME) -c -g

LIBARG = -L$(GMP_LIB) -lm -lc
EXECLIBARG = -lm -lc


OBJECTS = interpolator.o linAlgDouble.o 
#linAlgMagma.o

.PHONY: SMQP

all: PolySupportLib 

PolySupportLib : SMQP $(OBJECTS) #$(TARGET) 
	ld -r -o numpolysupport.o $(OBJECTS) SMQP/*.o 	

SMQP: 
	(cd SMQP; make);

%.o : %.c
	$(CC) $(CCOMPILEARG) $<

%.o : %.cpp
	$(CPPC) $(CPPCOMPILEARG) $<

clean : 
	(cd SMQP; make clean;)
	rm -rf *.o *.a .tempLapackObjs/

