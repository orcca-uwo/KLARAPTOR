#!/bin/bash

dest="make.tmp"
cat /dev/null > "$dest"
printf "include make.inc\n" >> "$dest"
printf "all:\n" >> "$dest"
printf "\t@echo \$(NUMPOLYSUPPORT_LIBARGS)" >> "$dest"
res=( $(make --no-print-directory -f "$dest") )
OLD_IFS="$IFS"
IFS=$'\n'

result_str=""
for x in ${res[@]}; do
	result_str+="$x ";	
done;

echo "$result_str"
rm -rf "$dest"
