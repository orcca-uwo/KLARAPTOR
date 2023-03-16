#!/bin/bash

python get_gpu_name.py
exit  
# detecting device product name using nvidia-smi
deviceName=$(nvidia-smi -q |grep -i "Product Name"|head -1)

cnt=0
tmp=""
# removing extra space from device name
for i in $deviceName; do
	cnt=$(($cnt+1))
	if [ $cnt -le 4 ]; then
		continue;
	fi;
	tmp=$tmp$i
done;
deviceName=$tmp

printf "$deviceName\n"
