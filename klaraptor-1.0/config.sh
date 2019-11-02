#!/bin/bash

########################################
func_list=("install" "clean" "help")
utils_dir="$(pwd)""/utils/"
config_dir="$(pwd)""/utils/"

print_ln()
{
	printf "==============================\n";
}


sudo_list=("prereq_packages" "cupti")
basic=("path" "links_permissions")
########################################
f_install()
{

	printenv > $config_dir/"env.tmp"
	##################################
	##################################
	opt="--install"
	if [[ $# -ge 1 ]]; then
		opt=$1	
	fi;
	##################################
	##################################
	
	for x in ${sudo_list[@]}; do
		echo $x;
		(cd $config_dir;\
			c="config_""$x"".sh";\
			echo $c;\
			chmod u+x $c;\
			./$c;\
			chmod -x $c;\
		)
		print_ln;
	done;
	##################################
	##################################
	
#	basic=("path" "interp")
	for x in ${basic[@]}; do
		(cd $config_dir;\
			c="config_""$x"".sh";\
			echo $c;\
			chmod u+x $c;\
			./$c $opt;\
			chmod -x $c;\
		)
		print_ln;
	done;
	##################################
	##################################
	if [[ "$opt" == "--install" ]]; then
		(cd $KLARAPTOR_PATH/src; make;);	
	fi;	
	##################################
	##################################

	printf "
#####################################################################
	- Please open another terminal and type 'klaraptor --help'.
	- In case you have troubles, try reloading nvidia kernel modules, 
		or simply perform a reboot.
#####################################################################
"
	/bin/bash
}

########################################
f_clean()
{
#	tmp_files=klaraptor_path.conf klaraptor_prof
#	rm -rf $tmp_files
#	chmod -x klaraptor
	(cd tests; make clean;)
	opt="--uninstall"
	for x in ${basic[@]}; do
		(cd $config_dir;\
			c="config_""$x"".sh";\
			echo $c;\
			chmod u+x $c;\
			./$c $opt;\
			chmod -x $c;\
		)
		print_ln;
	done;
	(cd $KLARAPTOR_PATH/src; make clean;);	
	(cd ./bin; rm -rf *;)
	(cd $config_dir; rm -rf env.tmp;)
	
}
########################################
f_help()
{
	s=""
	for f in ${func_list[@]};do
		s+="$f/"
	done;
	help_str="args: [1]:--{$s}\n"
	printf "$help_str"
	exit -1;
}

########################################
argc=$#
########################################
task=$1;
cwd="$(pwd)"
export KLARAPTOR_PATH="$cwd"
for f in ${func_list[@]}; do
	if [[ "--$f" == "$task" ]]; then
		func_name="f_""$f"
		$func_name
		exit 0;
	fi
done;
f_help
########################################





