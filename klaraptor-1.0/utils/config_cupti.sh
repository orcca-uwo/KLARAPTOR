#!/bin/bash

ans=-1;
if [[ $# -gt 0 ]]; then
	ans=$1
	if [[ $ans -ne 0 ]]; then
		exit 0;
	fi;
fi;

if [[ "$EUID" -ne 0 ]]; then	
	while [[ $ans -eq -1 ]]; do
		printf "Have you already set 'NVreg_RestrictProfilingToAdminUsers=0'? "
		printf "Answer [yes/no].\nIf you are not sure, answer 'no' ... "
		read user_ans

		if [[ $user_ans == "y" ]] || [[ $user_ans == "yes" ]]; then
			ans=1;
			exit 0;	
		fi;
		if [[ $user_ans == "n" ]] || [[ $user_ans == "no" ]]; then
			ans=0;
			break;
		fi; 
	done;

	if [[ $ans -eq 0 ]]; then
		printf "[This script adds a rule to /etc/modprobe.d, Please run as root!]...\n"
		sudo "$0" "$ans"
    exit $?	
	fi;
fi

driver_version="$(nvidia-smi -q|grep -i "driver version")"
driver_version="${driver_version/*: /}"
driver_version="${driver_version/.*/}"

module_line_1='options nvidia "NVreg_RestrictProfilingToAdminUsers=0"'
module_line_2="options nvidia_""$driver_version "
module_line_2+='"NVreg_RestrictProfilingToAdminUsers=0"'
module_name="disable_cupti_restrictions"

printf "[adding rules to]..."
cd /etc/modprobe.d/
if [[ -e "$module_name.conf" ]]; then
	d="$(date +%D)"
	d="${d//\//_}"
	mv $module_name".conf" "$module_name"".conf.$d.bak"
fi;

cat /dev/null > $module_name".conf"
printf "$module_line_1\n$module_line_2" >> $module_name".conf";

printf "[/etc/modprobe.d/$module_name.conf]...[DONE]...\n"
printf "
#####################################################################
## you need to reload nvidia modules, easiest way is to do a reboot 
## after installation is finished.
#####################################################################
"
read -p "Press any key to continue ..."
