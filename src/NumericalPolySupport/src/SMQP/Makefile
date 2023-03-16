CC = gcc
INCLUDES = ../../include/SMQP
CCOMPILEARG = -I$(INCLUDES) -c -g -Wall -std=c99 -fPIC 

vpath %.c ./ 

all: SMQP_Support-AA.o SMQP_Support_Test-AA.o

%.o : %.c
	$(CC) $(CCOMPILEARG) $<

test:

clean:
	rm -rf *.o
