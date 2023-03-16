#!/bin/bash

make -f Makefile.cuda
bin=$(cat cudabin_name)

for((n2=4;n2<=25;n2++));do
	n=$((2**n2));
	for((bx2=1;bx2<=n2;bx2++));do
		bx=$((2**bx2));
		for((by2=1;by2<=n2;by2++));do
				by=$((2**by2));
			./$bin $n $bx $by;
			printf "=========================\n"
		done;
	done;
done;

