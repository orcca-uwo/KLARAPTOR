#!/bin/bash

libs=( "lapack" "cblas" "i77" "f77" "atlas")

for b in ${libs[@]};do
	name="lib$b"
	echo $name
	locate $name|grep -i "/usr"|grep -i "\.a$"
done;
