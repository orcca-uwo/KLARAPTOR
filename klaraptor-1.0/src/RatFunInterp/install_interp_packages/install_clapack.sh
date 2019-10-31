#!/bin/bash

####
# Execute this script to unpack and install clapack3.tar 
# in the current working directory for a LINUX system. 
# clapack3.tar should exist in the current working directory.
####

## install atlas
#sudo apt-get install libatlas-base-dev -y
#if [[ ! $? -eq 0 ]]; then
#	exit -1;
#fi;

#######################################

url="https://www.netlib.org/clapack/clapack3.tgz"
name="${url/*\//}"
dir_name="${name/\.tgz/}"
dir_name=CLAPACK

#rm -rf $name

if [[ ! -f $name ]]; then
	wget $url
fi;
tar -xzf $name
cwd="$(pwd)"
cd $dir_name

cp INSTALL/make.inc.LINUX ./make.inc
make f2clib
cp F2CLIBS/f2c.h .
cd BLAS/SRC && make
cd ../../SRC && make single double
cd ../..

#tar -xzf atlas.tgz 

dest="$cwd/make.inc"
cd "$cwd/.."

dest="${dest/$(pwd)/.}"
#echo $pwd
ln -sf "$dest" "make.inc"
