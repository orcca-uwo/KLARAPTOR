
#ifndef _SMQP_SUPPORT_TEST_AA_H_
#define _SMQP_SUPPORT_TEST_AA_H_

#ifdef __cplusplus
extern "C" {
#endif

#include "SMQP_Support-AA.h"
#include <math.h>
#include <time.h>

/** 
 * Get the next degree_t for a polynomial given the previous term's degree_t
 * and a "step" to take. In the univariate case this is prev+step. 
 * In the multivatirate case we consider an integer of radix maxUniDeg with 
 * coefficients described our degrees_t. We step such that the returned value
 * is equal to prev + step in this radix maxUniDeg representation.
 * e.g: prev = [1,2,7], step = 5, maxUniDeg = 10. Then next is [1,3,2];
 */
degrees_t getNextDegrees (degrees_t prev, degree_t step, degree_t maxUniDeg, int nvar, int* sizes, unsigned long long int* masks);

/** 
 * Build a random polynomial given the the number of variables, nvar,
 * the number of terms, nterms, an (exclusive) upper bound on the absolute value
 * of the cofficients and a sparsity factor. 
 *
 * The sparsity factor is such that the difference in degree_t between sucsessive terms 
 * in the generated polynomial is 1 <= diff < sparsity;
 *
 */
Node* buildRandomPoly(int nvar, int nterms, unsigned long int coefBound, degree_t sparsity, int includeNeg);

/**
 * Build a random polynomial given the number of variables, nvar, 
 * and the maximum degree of each variable, as the maxDegs arrays. 
 *
 * coefBound is the maximum number of bits in the coefficients.
 *
 * if includeNeg == 0 then all cofficients will be positive, otherwise randomly
 * negative.
 *
 * sparsity is a percentage of zero terms between the term with monomial of maxDegs
 * and the constant term. A sparsity of 0 produces a dense polynomial, a sparisty of 1
 * produces a polynomial of only one term, the one whose monomial is maxDegs.
 *
 * returns the randomly generated polynomial.
 */
AltArr_t* buildRandomPolyFromMax(int nvar, const int* maxDegs, unsigned long int coefBound, float sparsity, int includeNeg);

#ifdef __cplusplus
}
#endif

#endif
