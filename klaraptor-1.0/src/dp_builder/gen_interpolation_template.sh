#!/bin/bash

verbose=0;

dim="$1"
cwd="$2"

dim_str=""
dim_str_suffix="DimKernel";
nvar=""

#######################################
if [[ "$dim" == "1" ]]; then
	dim_str="One"
	nvar="2"
fi;

if [[ "$dim" == "2" ]]; then
	dim_str="Two"
	nvar="3"
fi;

if [[ "$dim" == "3" ]]; then
	dim_str="Three"
	nvar="4"
fi;

#######################################
dim_str+="$dim_str_suffix"

if [[ $verbose -eq 1 ]]; then
	printf "[dim=$dim][$dim_str][nvar=$nvar]...\n"
fi;

template_path="$KLARAPTOR_PATH/src/gpu_perf_model/mcwp/interpolation_template.c"
dest="$cwd/mwp_cwp.c"

cp "$template_path" "$dest"
sed "s/PROBLEM_DIM_REPLACEMENT_LINE/$dim_str/" -i "$dest"
sed "s/PROBLEM_NVARS_REPLACEMENT_LINE/$nvar/" -i "$dest"
