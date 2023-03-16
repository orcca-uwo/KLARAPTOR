

all:
	python generate_mesh.py  > kernel_mesh_points.h

test:
	gcc -w -std=gnu99 test.c -o test.bin
	./test.bin

clean:
	rm -rf test.bin

