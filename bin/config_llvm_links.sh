#!/bin/bash

argc=$#
if [[ $argc -ne 1 ]]; then
	printf "args: [0]:llvm version\n"
	exit;
fi;

version=$1
links=("clang++" "clang" "llc" "lli" "opt" "llvm-link")

for x in ${links[@]}; do
	p="/usr/bin/$x-$version"
	x_local="/usr/local/bin/$x"
	sudo ln -sf $p $x_local
	printf "linking [$p] --> [$x_local]...\n"
done;

