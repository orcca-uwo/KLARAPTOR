#!/bin/bash

klaraptor_path="$KLARAPTOR_PATH"
profiler_path="$klaraptor_path/src/profiler/klaraptor_profiler"
interface="$klaraptor_path""/utils/klaraptor"
interface_script="$klaraptor_path""/utils/klaraptor_use.sh"

links=( $profiler_path $interface $interface_script )
exec_list=( $profiler_path $interface $interface_script )

###########################################################
install()
{
	###########################################################
	printf "[installing KLARAPTOR links]...\n";	
	echo $klaraptor_path
	for x in ${exec_list[@]}; do
		chmod u+x $x;
	done;

	for x in ${links[@]}; do
		(cd $klaraptor_path/bin; ln -sf $x .;)
	done;

	(cd $klaraptor_path/bin; ln -sf ./klaraptor_profiler klaraptor-profiler)
	printf "[installing KLARAPTOR links]...[DONE]\n"
}

###########################################################
###########################################################
uninstall()
{
	###########################################################
	printf "[uninstalling KLARAPTOR links]...\n";	
	echo $klaraptor_path
	
	for x in ${exec_list[@]}; do
		chmod -x $x;
	done;

	for x in ${links[@]}; do
		name="${name/.*\//}"
		(cd $klaraptor_path/bin; rm -rf $name;)
	done;	
	printf "[uninstalling KLARAPTOR links]...[DONE]\n"
	############################################################	
}

#######################################
argc=$#
t="install"
if [[ $argc -ge 1 ]]; then
	t="$1"
	t="${t/--/}"
fi;
$t
