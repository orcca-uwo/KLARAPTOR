#!/bin/bash
metrics=""

# achieved_occupancy
metrics+="sm__warps_active.avg.pct_of_peak_sustained_active,"

# ipc 
metrics+="smsp__inst_executed.avg.per_cycle_active,"

# inst_per_warp
metrics+="smsp__average_inst_executed_per_warp.ratio,"

# inst_executed_global_stores & inst_executed_global_loads
metrics+="smsp__inst_executed_op_global_st.sum,smsp__inst_executed_op_global_ld.sum,"

# gst_transactions & gld_transactions
metrics+="l1tex__t_sectors_pipe_lsu_mem_global_op_st.sum,l1tex__t_sectors_pipe_lsu_mem_global_op_ld.sum,"

# gst_throughput & gld_throughput
metrics+="l1tex__t_bytes_pipe_lsu_mem_global_op_st.sum.per_second,l1tex__t_bytes_pipe_lsu_mem_global_op_ld.sum.per_second,"

# l2_local_global_store_bytes & l2_global_load_bytes
# metrics+="lts__t_bytes_equiv_l1sectormiss_pipe_lsu_mem_local_op_st.sum,lts__t_bytes_equiv_l1sectormiss_pipe_lsu_mem_global_op_st.sum,lts__t_bytes_equiv_l1sectormiss_pipe_lsu_mem_global_op_ld.sum,"

# dram_write_bytes & dram_read_bytes
metrics+="dram__bytes_write.sum,dram__bytes_read.sum,"

trace=""

metrics+="launch__grid_dim_x,launch__grid_dim_y,launch__grid_dim_z,"
metrics+="launch__block_dim_x,launch__block_dim_y,launch__block_dim_z,"
metrics+="launch__registers_per_thread,launch__shared_mem_per_block_static,launch__shared_mem_per_block_dynamic"

#######################################
events=""
events+="warps_launched,"
events+="elapsed_cycles_sm,"

metric_target="profiling.metrics"
trace_target="profiling.trace"
ncu_metrics_opts="-c 6 --csv --metrics"
ncu_opts="--csv --metrics"

cat /dev/null > $metric_target
cat /dev/null > $trace_target

#env LD_PRELOAD=./libinjection_2.so LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:`pwd` ./$1

ncu $ncu_metrics_opts $metrics ./$1 >$metric_target
#ncu $ncu_opts $trace ./$1 >$trace_target 

#less $metric_target
#less $trace_target
