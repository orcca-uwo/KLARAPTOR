#!/bin/bash

### python 2.xx
### CUDA >=9.2
### LLVM >= 8.0
### libatlas-base-dev
#clang opt llvm-link 

packages="$*"
llvm_version=8
#echo "PACKAGES=$packages"
#######################################
check_err()
{
	if [[ $? -ne 0 ]];then
		printf "ERROR!\n"
		exit -1;
	fi;
}
#######################################
## check if enviroment variable is set in .bashrc of the user.
check_env_var()
{
	var="$1"
	IFS=$'\n'	
	value=""
	env_list=( $(cat env.tmp) )	
	for e in ${env_list[@]}; do
#		printf "$e;\n"	
		if [[ "$e" == "$var="* ]]; then	
			value="$(eval $e; eval echo \${$var})"
			break;	
		fi;
	done;
	if [[ "$value" == "" ]]; then
		printf "
========================================
[ERROR: $var is NULL!]
Make sure you have set 'export $var' in your ~/.bashrc and try again.
========================================
"
	exit -1;
	fi;
	printf "Found $var=[$value] ...\n"
	eval $var="$value"
	return 0;
}
#######################################
check_bin_in_path()
{
	bin_name="$1"
	bin_path=$(which $bin_name)	
	if [[ "$bin_path" == "" ]]; then
		printf "[ERROR: cannot find $bin_name in PATH]...\n"
#		printf "    Make sure you have installed $bin_name and try again.\n"
		return -1;
	fi;
	printf "Found $bin_name in [$bin_path] ...\n"
	return 0;
}

#######################################
check_lib_installed()
{
	libname="$1"
	IFS=$'\n'
	found=( $(ldconfig -p|grep $libname) )
	n=${#found[@]}
	if [[ $n -lt 1 ]]; then
		printf "[ERROR: Found no candidate for $libname]...\n"
#		printf "    Make sure you have installed $libname and try again.\n"
		return -1;
	fi;
	for x in ${found[@]}; do 
		printf "\t[$libname]-->[$x]\n";
	done;
	return 0;
}
#######################################
check_basics()
{	
	check_bin_in_path "python2.7"
	if [[ $? -ne 0 ]]; then
		packages+="python2.7 "
	fi;
	check_lib_installed "libatlas"

	if [[ $? -ne 0 ]]; then
		packages+="libatlas-base-dev "
	fi;
}

#######################################
check_cuda()
{	
	###############################################
	#### check if CUDA_PATH is set
	CUDA_PATH=""
	check_env_var "CUDA_PATH"
	check_err
	###############################################
	#### check CUDA toolkit version
	min_cuda_version_str=9.2
	cuda_version_str="$(cd $CUDA_PATH/bin; ./nvcc -V|tail -1)"
	cuda_version_str="${cuda_version_str/*release /}"
	cuda_version_str="${cuda_version_str/,*/}"
	cuda_version="${cuda_version_str/./}"
	min_cuda_version="${min_cuda_version_str/./}"
	printf "Found nvcc version $cuda_version_str ...\n";
	if [[ $cuda_version -lt $min_cuda_version ]]; then
		printf "[ERROR: nvcc version must be >= $min_cuda_version_str ...]\n";
		exit -1;
	fi;
	
	###############################################
	#### check nvidia-smi exists
	check_bin_in_path "nvidia-smi"
	check_err
}

#######################################
check_llvm()
{
	version=$llvm_version
	check_lib_installed "libLLVM-$version"
	if [[ $? -ne 0 ]]; then
		packages+="libllvm$version llvm-$version "
	fi;
#	packages+="clang-$version lldb-$version lld-$version clangd-$version "
	#####################################
	check_bin_in_path "clang-$version"
	if [[ $? -ne 0 ]]; then
		packages+="clang-$version "
	fi;
	#####################################
	check_bin_in_path "lldb-$version"
	if [[ $? -ne 0 ]]; then
		packages+="lldb-$version "
	fi;
	#####################################
	check_bin_in_path "lld-$version"
	if [[ $? -ne 0 ]]; then
		packages+="lld-$version "
	fi;
	#####################################
	check_bin_in_path "clangd-$version"
	if [[ $? -ne 0 ]]; then
		packages+="clangd-$version "
	fi;

	#####################################
	printf "LLVM_VERSION:=$version\n" > ../src/io_builder/LLVM_VERSION.inc
	printf "LLVM_VERSION:=$version\n" > ../src/profiler/LLVM_VERSION.inc
	printf "LLVM_VERSION:=$version\n" > ../src/dp_builder/LLVM_VERSION.inc

#	source /etc/lsb-release
#	code="$DISTRIB_CODENAME"
#	echo $code
#	
#	repo="http://apt.llvm.org/$code/ llvm-toolchain-$code-$version main"

#	wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
#	add-apt-repository "$repo"
#	apt-get update
#	apt-get install -y llvm-$version clang-$LLVM_VERSION lld-$LLVM_VERSION
}

#######################################
install_packages()
{
	if [[ "$packages" == "" ]]; then
		return 0;
	fi;	
	#######################################
	if [[ "$EUID" -ne 0 ]]; then 
		printf "[This script needs to install a number of pacakages, running as sudo]...\n"
		sudo "$0" "$packages"
    exit $?
	fi
	#######################################
	printf "[packages to install: $packages]\n"
	#####################################
	## install llvm packages
	version="$llvm_version"
#	wget https://apt.llvm.org/llvm.sh
	check_err
	chmod +x llvm.sh
	./llvm.sh $version
	check_err
#	rm -rf llvm.sh
#
#clang-$LLVM_VERSION lldb-$LLVM_VERSION lld-$LLVM_VERSION clangd-$LLVM_VERSION##
	#####################################
	IFS=" "
	package_list=()
	for x in $(echo $packages); do
		echo $x;
		package_list+=( $x )	
	done;
	apt-get install -y ${package_list[@]}
#	apt-get install -y 
}
#######################################
#######################################
check_basics
check_cuda
check_llvm
#apt-get install $packages -y
install_packages

check_err
