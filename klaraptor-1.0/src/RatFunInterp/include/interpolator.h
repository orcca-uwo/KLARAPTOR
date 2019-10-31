
#ifndef _INTERPOLATOR_H_
#define _INTERPOLATOR_H_

#include "linAlgDouble.h"
#include "SMQP/SMQP_Support-AA.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Possible status conditions when attempting to obtain an interpolating
 * polynomial for a collection of (point, value) pairs.
 *
 * SUCCESS means the interpolating polynomial was successfully found
 * FAILURE means the interpolating polynomail is impossible given the degree bounds.
 * MORE_POINTS means that not enough points are supplied for the given degree
 * bounds, or some of the points supplied are linearly dependent. 
 */
typedef enum polyInterpStatus {
	POLY_INTERP_FAILURE = 0x0,
	POLY_INTERP_FAIL_BY_RESIDUAL,
	POLY_INTERP_MORE_POINTS,
	POLY_INTERP_SUCCESS
} PolyInterpStatus_t;

/**
 * A struct encapsulating all the necessary data to perform interpolation
 * of a continually updating data set. 
 * This struct, along with all the following methods prefixed by "interp"
 * automatically handle dense multivariate polynomial interpolation. 
 *
 * The general strategy is to first initialize an interpolator with interpInit()
 * followed by repeatedly adding (point,value) pairs via interpAddPointVal() 
 * and checking with interpGetPoly() after each pair until a polynomial
 * can be found or one cannot possibly be found. (Returned status is anything
 * other than POLY_INTERP_MORE_POINTS).
 */
typedef struct interpolator {
	int nvar;
	int* degreeBounds;
	int* denomDegreeBounds;

	long numeratorNumTerms;
	long requiredPoints;

	int valAlloc;
	int valSize;
	mpq_t** mpPoints;
	mpq_t* mpVals;

	//A is a cache of sample matrix entries computed so far.
	//mpPoints[i] is evaluated and stored in the ith row of A.
	//Within a row of A, the first numeratorNumTerms 
	//are the evaluations for the numerator poly, the rest
	//are for evaluations of the denominator poly (multiplied by -mpVals[i]).
	long nCols; 
	long curRows;
	long allocRows;
	double* A;

	//Due to internal optimizations, not all columns of A are guaranteed to be filled with evaluations.
	long numSize;
	long denSize;

} Interpolator_t;


/******
 * Rational Function Interpolation
 ******/

/**
 * Create and initialize an interpolator given a number of variables and 
 * a degree bound (inclusive) for each variable for numerator and denominator. 
 */
Interpolator_t* rfInterpInit(int nvar, int* degreeBounds, int* denomDegreeBounds);

/**
 * Clean up an interpolator and any memory used by it.
 * The interp pointer is invalidated by a call to this functions.
 */
void rfInterpFree(Interpolator_t* interp);

/**
 * Obtain the minimum number of points required for the supplied interpolator
 * to be able to interpolate.
 */
static inline long rfInterpGetRequiredNumPoints(Interpolator_t* interp) {
	return interp->requiredPoints;
}

/**
 * Add a point,value pair to the interpolation as GMP rational numbers.
 */
void rfInterpAddPointValMP(Interpolator_t* interp, mpq_t* point, mpq_t val);

/**
 * Add many point,value pairs to the interpolation as GMP rational numbers.
 */ 
void rfInterpAddPointsMP(Interpolator_t* interp, mpq_t** points, mpq_t* vals, int nPoints);

/**
 * Try to obtain an interpolating rational function for the point value pairs
 * which have so far been added to the interpolator. The interpolating
 * polynomial is returned in poly, if one can be generated. Otherwise the
 * result is undefined.
 *
 * The returned PolyInterpStatus_t tells if the polynomial was successfully
 * generated and returned. If the status is POLY_INTERP_SUCCESS then the poly
 * is valid. If more points are needed to get a valid polynomial then 
 * POLY_INTERP_MORE_POINTS is returned.  
 */
// PolyInterpStatus_t rfInterpGetPoly(Interpolator_t* interp, AltArr_t** numPoly, AltArr_t** denomPoly);
// PolyInterpStatus_t rfInterpGetPoly(Interpolator_t* interp, AltArr_t** numPoly, AltArr_t** denomPoly, mpq_t* newpoint, mpq_t newval);
// PolyInterpStatus_t rfInterpGetPoly(Interpolator_t* interp, AltArr_t** numPoly, AltArr_t** denomPoly, double eps, mpq_t* newpoint, mpq_t newval);
PolyInterpStatus_t rfInterpGetPoly(Interpolator_t* interp, AltArr_t** numPoly, AltArr_t** denomPoly, double maximumResidual);

/**
 * Using one more point,value pair, ensure that the interpolated polynomial is correct.
 * Returns POLY_INTERP_SUCCESS if the polynomial is good, otherwise POLY_INTERP_MORE_POINTS.
 */
PolyInterpStatus_t rfInterpCheckPoly(Interpolator_t* interp, const AltArr_t* numPoly, const AltArr_t* denomPoly, mpq_t* point, mpq_t val);

/**
 * Using one more point,value pair, ensure that the interpolated polynomial is correct.
 * Tolerance is a decimal such that the rational function evaluated at the supplied point
 * must match the supplied value within the supplied tolerance. 
 * That is, | (eval - val) / val | < tolerance. 
 * Returns POLY_INTERP_SUCCESS if the polynomial is good, otherwise POLY_INTERP_MORE_POINTS.
 */
PolyInterpStatus_t rfInterpCheckPolyWithTolerance(Interpolator_t* interp, const AltArr_t* numPoly, const AltArr_t* denomPoly, mpq_t* point, mpq_t val, double tolerance);



/******
 * Helper Functions
 ******/

int enumerateMonomials(int* degrees, int* degreeBounds, int nvar); 


#ifdef __cplusplus
}
#endif

#endif



