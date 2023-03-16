#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gmp.h>

#include "mwp_cwp.h"

///////////////////////////////////////
int rep_n(int n){
    //return pow(n,2);
    return 1;
}

///////////////////////////////////////
int test_mcwp(int argc, char **argv) {

    int n_kernels = get_num_lines_in_file("kernel_name_list.tmp");

    enum kernel_type_enum kernel_type_tmp = TwoDimKernel;
    int nvar = 3;

    int *degree_bounds = (int*) malloc(sizeof(int) * nvar);
    int *denomdegree_bounds = (int*) malloc(sizeof(int) * nvar);

    for (int i = 0; i < nvar; ++i) {
        degree_bounds[i] = 5;
        denomdegree_bounds[i] = 5;
    }
    char mem_inst_trace_tmp[128] = "%d %d %d 1";
    //int rep_n_pow = 2;
    test_mwp_cwp(argc, argv, n_kernels, nvar, degree_bounds, denomdegree_bounds,
            mem_inst_trace_tmp, kernel_type_tmp, rep_n);
    return 0;
}

///////////////////////////////////////
int main(int argc, char ** argv) {
    return test_mcwp(argc, argv);
}
