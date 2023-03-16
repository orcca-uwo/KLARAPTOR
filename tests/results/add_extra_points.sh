#!/bin/bash


for x in $(cat 2d_examples);do
	python add_extra_points_to_tracer_vm_log.py $x
done;
