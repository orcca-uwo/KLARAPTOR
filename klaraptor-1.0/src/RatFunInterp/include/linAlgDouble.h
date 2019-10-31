
#ifndef _LIN_ALG_DOUBLE_H_
#define _LIN_ALG_DOUBLE_H_

#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif


/**
 * Solve an (over-determined, rank-deficient) system of equations, Ax = b;
 * A should be of size n x m, b of size n x 1. 
 * The solutions are returned in X, of size m x 1. X should be pre-allocated.
 * The returned integer, r, indicates that the first r elements of X are valid
 * solutions.
 */
long solveDoubleSystem(const double* A, const double* b, long n, long m, double* X);

/**
 * Solve an (over-determined, rank-deficient) system of equations, Ax = b.
 * This method assumes much about its input:
 *
 * A should be of size n by m, in a *column-major* format (equivalently a row-major matrix of size n by m which is then transposed).
 * A is modified in place so that the first min(m,n) rows of A are its right singular vectors, stored rowwise (but still column-major).
 * b is the right hand side of the system and should be of size n by 1.
 * The minimum norm least squares solution is returned in X which should be pre-allocated to be of size at least n by 1
 * where xSize points to the actual size of the array X points to. If that array is too small, 
 * the long xSize points to is updated to the array's new allocation. 
 *
 * s points to an array which returns in it the singular values of A. That array is of size sSize, which should be size at least n. 
 * If the array is reallocated the long sSize points to is updated to be the new size.
 *
 * work points to an array used is used as auxiliary work space for the computation. That array of size worksize, which should be at size at least n. 
 * but will likely be reallocated during the computation if too small. If the array is reallocated the long worksize points to is updated to be the new size.
 *
 * s returns the 
 * The returned integer indicates the rank of A and that the first r elements of X
 * are valid solutions.
 */
long solveDoubleSystemPreAlloc(double* A, const double* b, long n, long m, double** X, long* xSize, double** s, long* sSize, double** work, double* worksize);

/**
 * Returns a double identity matrix of size n by n.
 */
double* getDoubleIdentity(int n);

/**
 * Nicely print the double matrix A which is of size n by m. 
 */
void printDoubleMat(const double* A, int n, int m);


#ifdef __cplusplus
}
#endif

#endif
