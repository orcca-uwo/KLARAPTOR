
#ifndef _LIN_ALG_DOUBLE_H_
#define _LIN_ALG_DOUBLE_H_

#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif


/**
 * Solve an (over-determined) system of equations, Ax = b;
 * A should be of size n x m, b of size n x 1. 
 * The solutions are returned in X, of size m x 1. X should be pre-allocated.
 * The returned integer, r, indicates that the first r elements of X are valid
 * solutions.
 */
long solveDoubleSystem(const double* A, const double* b, long n, long m, double* X);

double* getDoubleIdentity(int n);

void printDoubleMat(const double* A_mp, int n, int m);


#ifdef __cplusplus
}
#endif

#endif
