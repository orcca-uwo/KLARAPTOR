#!/bin/bash

cwd="$(pwd)"

klaraptor_path=$(echo $KLARAPTOR_PATH;)
dest="$klaraptor_path/bin/klaraptor_path.conf"
target="$HOME/.bashrc"

#######################################
#######################################
output_str="
export KLARAPTOR_PATH=$klaraptor_path
#export LLVM_PASS_COMMON_PATH=$llvm_path
export PATH=\$PATH:\$KLARAPTOR_PATH/bin/
export LD_LIBRARY_PATH=\$LD_LIBRARY_PATH:\$KLARAPTOR_PATH/lib/
export CUPTI_PATH=\$CUDA_PATH/extras/CUPTI
export LD_LIBRARY_PATH=\$LD_LIBRARY_PATH:\$CUPTI_PATH/lib64
export LD_LIBRARY_PATH=\$LD_LIBRARY_PATH:\$CUDA_PATH/lib64

"
printf "$output_str" > $dest
printf "[adding KLARAPTOR_PATH to ~/.bashrc] ... \n"

#######################################
#######################################
source_line="source $dest"
if grep -Gxq "\s*$source_line" $target 
then
				printf "Path is already set! \n"
				printf ""
else
	output_str="	
######################################
######################################
#### ADDING KLARAPTOR_PATH [""$(date)""]
######################################
if [ -e $dest ]; then
	$source_line
fi;
######################################
######################################
"
	printf "$output_str" >> $target
fi

source $target

printf "[adding KLARAPTOR path is done]...\n"

