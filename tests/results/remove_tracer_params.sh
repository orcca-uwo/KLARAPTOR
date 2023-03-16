#!/bin/bash

for x in $(find ./|grep tracer_params.log); do
	echo $x; 
	rm $x;
done;
