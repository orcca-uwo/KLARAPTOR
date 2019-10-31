
#include "interpolator.h"
#include "../test/Unix_Timer.h"
#include <float.h>
#include <math.h>

#define INTERP_DEBUG 0
#define INTERP_OPT 0
#define INTERP_TIMING 0

float evalTime = 0.0f;

//foward declarations of internal methods
PolyInterpStatus_t _rfInterpGetPoly(Interpolator_t* interp, AltArr_t** numPoly, AltArr_t** denomPoly, double maximumResidual, int rfInterpGetPoly_EarlyTerm);

static inline int kroneckerIndex(int* degrees, int* degreeBounds, int nvar) {
	int ret = 0;
	int curWeight = 1;
	for (int k = nvar-1; k >= 0; --k) {
		ret += (degrees[k] * curWeight);
		curWeight *= (degreeBounds[k] + 1);
	}
	return ret;
}

int enumerateMonomials(int* degrees, int* degreeBounds, int nvar) {
	int idx = nvar-1;
	while(degrees[idx] + 1 > degreeBounds[idx]) {
		--idx;
		if (idx < 0) {
			return 0;
		}
	}

	++(degrees[idx]);
	
	for (int i = nvar-1; i > idx; --i) {
		degrees[i] = 0;
	} 
	return 1;
} 

int enumerateDegreeBounds(int* degrees, int* degreeBounds, int nvar) {
	int idx = 0;
	for (int i = 0; i < nvar-1; ++i) {
		if(degrees[i] >= degreeBounds[i]) {
			++idx;
		} else if (degrees[i] >= degrees[i+1]) {
			++idx;
		} else {
			break;
		}
	}

	for (int i = idx; i < nvar; ++i) {
		if(degrees[i] >= degreeBounds[i]) {
			++idx;
		} else {
			break;
		}
	}

	if (idx == nvar) {
		//Then we have reached the lowest degree bound on all variables.
		//Now check is the degree bounds are uneven.
		for (int i = 0; i < nvar; ++i) {
			if (degrees[i] < degreeBounds[i]) {
				++(degrees[i]);
				return 1;
			}
			degrees[i] = degreeBounds[i];
		}
		return 0;
	}

	++(degrees[idx]);
	return 1;
}


AltArr_t* buildPolyFromCoefArrayWithEps(int* degreeBounds, int nvar, double* X, double eps) {
	eps = eps < 0.0 ? -eps : eps;

	long size = 1;
	for (int i = 0; i < nvar; ++i) {
		// fprintf(stderr, "size: %ld, degreeBounds: %d\n",size, degreeBounds[i]);
		size *= (degreeBounds[i]+1);
	}


	long actualSize = 0;
	for (int i = 0; i < size; ++i) {
		if (fabs(X[i]) > eps) {
			++actualSize;
		}
	}

	if (actualSize == 0) {
		return NULL;
	}

	AltArr_t* aa = makePolynomial_AA(actualSize, nvar);

	//coef array X is in ~increasing~ lex order. We need decreasing. 
	int degrees[nvar];
	for (int k = 0; k < nvar; ++k) {
		degrees[k] = 0;	
	}
	degrees[nvar-1] = -1;

	AAElem_t* elems = aa->elems;
	int curIdx = actualSize - 1;
	int* sizes = getExpOffsetArray(nvar);

	for (int i = size-1; i >= 0; --i) {
		enumerateMonomials(degrees, degreeBounds, nvar);
		// for (int k = 0; k < nvar; ++k) {
		// 	fprintf(stderr, " %d", degrees[k]);
		// }
		// fprintf(stderr, "\n");seed
		// fprintf(stderr, "X[%d]: %g\n", size-1 - i, mpq_get_d(X[size-1 - i]));

		// fprintf(stderr, "X[]: %f\n", X[size-1-i]);
		// fprintf(stderr, "absX[]: %f\n", abs(X[size-1-i]));
		if (fabs(X[size-1 -i]) <= eps) {
			continue;
		} 

		elems[curIdx].degs = 0;
		for (int k = 0; k < nvar; ++k) {
			elems[curIdx].degs |= ((degrees_t) degrees[k]) << sizes[k];
		}

		// gmp_fprintf(stderr, "setting index %d to degs: %llx, coef: %Qd", curIdx, elems[curIdx].degs, X[size-1-i]);

		mpq_init(elems[curIdx].coef);
		mpq_set_d(elems[curIdx].coef, X[size-1 - i]);
		--curIdx;
	}

	aa->size = actualSize;
	
	free(sizes);

	// mpq_clear(absX);
	// mpq_clear(mpEps);

	return aa;
}


/******
* Rational Function Interpolation
******/

/**
 * Create and initialize an interpolator given a number of variables and 
 * a degree bound (inclusive) for each variable for numerator and denominator. 
 */
Interpolator_t* rfInterpInit(int nvar, int* degreeBounds, int* denomDegreeBounds) {
	Interpolator_t* interp = (Interpolator_t*) malloc(sizeof(Interpolator_t));

	interp->nvar = nvar;

	interp->valAlloc = 0;
	interp->valSize = 0;
	interp->mpPoints = NULL;
	interp->mpVals = NULL;

	interp->degreeBounds = (int*) malloc(sizeof(int)*nvar);
	memcpy(interp->degreeBounds, degreeBounds, sizeof(int)*nvar);
	interp->denomDegreeBounds = (int*) malloc(sizeof(int)*nvar);
	memcpy(interp->denomDegreeBounds, denomDegreeBounds, sizeof(int)*nvar);

	interp->numeratorNumTerms = 1;
	interp->requiredPoints = 1;
	for (int i = 0; i < nvar; ++i) {
		interp->numeratorNumTerms *= (interp->degreeBounds[i] + 1);
		interp->requiredPoints *= (interp->denomDegreeBounds[i] + 1);
	}
	interp->requiredPoints += interp->numeratorNumTerms;

	interp->nCols = interp->requiredPoints;
	interp->allocRows = interp->nCols;
	interp->A = (double*) calloc(sizeof(double), interp->allocRows*interp->nCols);

	//set first row to all 0s with one 1 for denom.
	//this row is constant except for position of the 1.
	for (int j = 0; j < interp->nCols; ++j) {
		interp->A[j] = 0.0;
	}
	interp->A[interp->numeratorNumTerms] = 1.0;
	interp->curRows = 1;

	interp->numSize = 1;
	interp->denSize = 1;

	return interp;
}

void rfInterpResetPointsVals(Interpolator_t* interp) {
	int valSize = interp->valSize;
	int i, j;
	for (i = 0; i < valSize; ++i) {
		mpq_clear(interp->mpVals[i]);
		for (j = 0; j < interp->nvar; ++j) {
			mpq_clear(interp->mpPoints[i][j]);
		}
		free(interp->mpPoints[i]);
	}
	free(interp->mpPoints);
	free(interp->mpVals);

	interp->mpPoints = NULL;
	interp->mpVals = NULL;
	interp->valSize = interp->valAlloc = 0;

	if (interp->A != NULL) {
		free(interp->A);
		interp->A = NULL;
	}
	interp->curRows = 0;
}


/**
 * Clean up an interpolator and any memory used by it.
 * The interp pointer is invalidated by a call to this functions.
 */
void rfInterpFree(Interpolator_t* interp) {
	if (interp->degreeBounds != NULL) {
		free(interp->degreeBounds);
	}
	if (interp->denomDegreeBounds != NULL) {
		free(interp->denomDegreeBounds);
	}
	rfInterpResetPointsVals(interp);
	
	free(interp);

}

double* _rfInterpGetSampleMatrix(Interpolator_t* interp, int* degreeBounds, int* denomDegreeBounds, long* lRows, long* lCols, long* lN2) {
	int nvar = interp->nvar;

	long rows = interp->valSize;
	long n = 1;
	long denomn = 1;
	for (int k = 0; k < nvar; ++k) {
		n *= (degreeBounds[k] + 1);
		denomn *= (denomDegreeBounds[k] + 1);
	}
	long n2 = n;
	n += denomn;

	*lRows = rows;
	*lCols = n;
	*lN2 = n2;

	double* A = interp->A;
	
	long evalN2 = interp->numeratorNumTerms;
	long evalN = interp->nCols;

	int i, j, k, enumSuccess;

	int degrees[nvar];

	// mpq_t* point;
	// mpq_t* val;
	// mpq_t varExp, tmpEval;
	// mpq_init(varExp);
	// mpq_init(tmpEval);

	// long numSize = interp->numSize;
	// long denSize = interp->denSize;


	// if (n2 > numSize || denomn > denSize) {
	// 	for (i = 1; i < rows; ++i) {

	// 		point = interp->mpPoints[i];
	// 		val = &(interp->mpVals[i]);
	// 		for (k = 0; k < nvar; ++k) {
	// 			degrees[k] = 0;
	// 		}

	// 		//just enumerate the first few since A is already filled
	// 		for (j = 1; j < numSize; ++j) {
	// 			enumSuccess = enumerateMonomials(degrees, degreeBounds, nvar);
	// 			if (!enumSuccess) {
	// 				fprintf(stderr, "Could not enumerate monomials! How can this happen? requiredPoints: %d\n", n);
	// 				exit(1);
	// 			}
	// 		}
	// 		//now actually compute new entries for A
	// 		if (numSize == 0) {
	// 			A[i*evalN] = 1.0;
	// 		}
	// 		for (j = numSize; j < n2; ++j) {
	// 			enumSuccess = enumerateMonomials(degrees, degreeBounds, nvar);
	// 			if (!enumSuccess) {
	// 				fprintf(stderr, "Could not enumerate monomials! How can this happen? requiredPoints: %d\n", n);
	// 				exit(1);
	// 			}

	// 			mpq_set_ui(tmpEval, 1ul, 1ul);
	// 			for (k = 0; k < nvar; ++k) {
	// 				mpz_pow_ui(mpq_numref(varExp), mpq_numref(point[k]), (unsigned long int) degrees[k]);
	// 				mpz_pow_ui(mpq_denref(varExp), mpq_denref(point[k]), (unsigned long int) degrees[k]);
	// 				mpq_canonicalize(varExp);
	// 				mpq_mul(tmpEval, tmpEval, varExp);
	// 			}
	// 			fprintf(stderr, "num   A[%d][%d] = %g\n", i, j, mpq_get_d(tmpEval));
	// 			A[i*evalN + j] = mpq_get_d(tmpEval);
	// 		}

	// 		//reset
	// 		for (k = 0; k < nvar; ++k) {
	// 			degrees[k] = 0;
	// 		}

	// 		for (j = 1; j < denSize; ++j) {
	// 			enumSuccess = enumerateMonomials(degrees, denomDegreeBounds, nvar);
	// 			if (!enumSuccess) {
	// 				fprintf(stderr, "Could not enumerate monomials! How can this happen? requiredPoints: %d\n", n);
	// 				exit(1);
	// 			}			
	// 		}

	// 		if (denSize == 0) {
	// 			A[i*evalN + evalN2] = -1.0 * mpq_get_d(*val);
	// 		}
	// 		for (j = denSize; j < denomn; ++j) {
	// 			enumSuccess = enumerateMonomials(degrees, denomDegreeBounds, nvar);
	// 			if (!enumSuccess) {
	// 				fprintf(stderr, "Could not enumerate monomials! How can this happen? requiredPoints: %d\n", n);
	// 				exit(1);
	// 			}
	// 			mpq_neg(tmpEval, *val);
	// 			for (k = 0; k < nvar; ++k) {
	// 				mpz_pow_ui(mpq_numref(varExp), mpq_numref(point[k]), (unsigned long int) degrees[k]);
	// 				mpz_pow_ui(mpq_denref(varExp), mpq_denref(point[k]), (unsigned long int) degrees[k]);
	// 				mpq_canonicalize(varExp);
	// 				mpq_mul(tmpEval, tmpEval, varExp);
	// 			}
	// 			fprintf(stderr, "denom A[%d][%d] = %g\n", i, j + evalN2, mpq_get_d(tmpEval));
	// 			A[i*evalN + j + evalN2] = mpq_get_d(tmpEval);
	// 		}
	// 		// fprintf(stderr, "finished row: %d\n", i);
	// 		// printDoubleMat(A, interp->curRows, interp->nCols);
	// 		// fprintf(stderr, "\n" );
	// 	}

	// 	interp->numSize = interp->numSize < n2 ? n2 : interp->numSize;
	// 	interp->denSize = interp->denSize < denomn ? denomn : interp->denSize;
	// }


	double* retA = malloc(sizeof(double)*rows*n);
	
	//strict copy for first row
	memcpy(retA, A, sizeof(double)*n2);
	memcpy(retA + n2, A + evalN2, sizeof(double)*denomn);


	for (i = 1; i < rows; ++i) {

		retA[i*n] = 1.0;
		for (k = 0; k < nvar; ++k) {
			degrees[k] = 0;
		}
		for (j = 1; j < n2; ++j) {
			enumerateMonomials(degrees, degreeBounds, nvar);
			k = kroneckerIndex(degrees, degreeBounds, nvar);
			retA[i*n + j] = A[i*evalN + k];
		}

		retA[i*n + n2] = A[i*evalN + evalN2];
		for (k = 0; k < nvar; ++k) {
			degrees[k] = 0;
		}
		for (j = n2+1; j < n; ++j) {
			enumerateMonomials(degrees, denomDegreeBounds, nvar);
			k = kroneckerIndex(degrees, denomDegreeBounds, nvar);
			retA[i*n + j] = A[i*evalN + k + evalN2];
		}
	}
	
	// mpq_clear(tmpEval);
	// mpq_clear(varExp);
	return retA;
}

void _rfInterpAddPolynomialRow(Interpolator_t* interp, int valIdx) {
	if (interp == NULL) {
		return;
	}

	if (interp->curRows + 1 >= interp->allocRows) {
		interp->allocRows += interp->nCols; //it starts square so this basically doubles.
		interp->A = (double*) realloc(interp->A, sizeof(double)*interp->nCols*interp->allocRows);
	}

	int nvar = interp->nvar;
	long n2 = interp->numeratorNumTerms;
	int degrees[nvar];

	int i = interp->curRows, n = interp->nCols;
	int j, k;
	int enumSuccess;
	for(k = 0; k < nvar; ++k) {
		degrees[k] = 0;
	}

	double* A = interp->A;

	mpq_t varExp, tmpEval;
	mpq_init(varExp);
	mpq_init(tmpEval);

	A[i*n] = 1.0;
	for(j = 1; j < n2; ++j) {
	// for(j = 1; j < interp->numSize; ++j) {
		enumSuccess = enumerateMonomials(degrees, interp->degreeBounds, nvar);
		if (!enumSuccess) {
			fprintf(stderr, "Could not enumerate monomials! How can this happen? requiredPoints: %d\n", n);
			exit(1);
		}

		mpq_set_ui(tmpEval, 1ul, 1ul);
		for (k = 0; k < nvar; ++k) {
			mpz_pow_ui(mpq_numref(varExp), mpq_numref(interp->mpPoints[valIdx][k]), (unsigned long int) degrees[k]);
			mpz_pow_ui(mpq_denref(varExp), mpq_denref(interp->mpPoints[valIdx][k]), (unsigned long int) degrees[k]);
			mpq_canonicalize(varExp);
			mpq_mul(tmpEval, tmpEval, varExp);
		}
		A[i*n + j] = mpq_get_d(tmpEval);
	}

	//reset
	for (int k = 0; k < nvar; ++k) {
		degrees[k] = 0;
	}

	A[i*n + n2] = -1.0 * mpq_get_d(interp->mpVals[valIdx]);
	for (j = n2+1; j < n; ++j) {
	// for (j = n2+1; j < interp->denSize; ++j) {
		enumSuccess = enumerateMonomials(degrees, interp->denomDegreeBounds, nvar);
		if (!enumSuccess) {
			fprintf(stderr, "Could not enumerate monomials! How can this happen? requiredPoints: %d\n", n);
			exit(1);
		}
		mpq_neg(tmpEval, interp->mpVals[valIdx]);
		for (k = 0; k < nvar; ++k) {
			mpz_pow_ui(mpq_numref(varExp), mpq_numref(interp->mpPoints[valIdx][k]), (unsigned long int) degrees[k]);
			mpz_pow_ui(mpq_denref(varExp), mpq_denref(interp->mpPoints[valIdx][k]), (unsigned long int) degrees[k]);
			mpq_canonicalize(varExp);
			mpq_mul(tmpEval, tmpEval, varExp);
		}
		A[i*n + j] = mpq_get_d(tmpEval);
	}


	mpq_clear(tmpEval);
	mpq_clear(varExp);

	++interp->curRows;

	interp->numSize = n2;
	interp->denSize = n-n2;

}

void rfInterpAddPointValMP(Interpolator_t* interp, mpq_t* point, mpq_t val) {
	if (interp->valSize == 0) {
		interp->valAlloc = 10;
		interp->mpVals = (mpq_t*) malloc(sizeof(mpq_t)*interp->valAlloc);
		interp->mpPoints = (mpq_t**) malloc(sizeof(mpq_t*)*interp->valAlloc);
		interp->mpPoints[0] = (mpq_t*) malloc(sizeof(mpq_t)*interp->nvar);
		for (int i = 0; i < interp->nvar; ++i) {
			mpq_init(interp->mpPoints[0][i]);
		}

		mpq_init(interp->mpVals[0]);
		mpq_set_si(interp->mpVals[0], 1l, 1l);
		interp->valSize = 1;
	}


	int valSize = interp->valSize;

	//That is to say, adding 1 would make it >= to alloc.
	if (valSize + 2 > interp->valAlloc) {
		interp->valAlloc += 10;
		interp->mpVals = (mpq_t*) realloc(interp->mpVals, interp->valAlloc*sizeof(mpq_t));
		interp->mpPoints = (mpq_t**) realloc(interp->mpPoints, interp->valAlloc*sizeof(mpq_t*));
	}

	mpq_init(interp->mpVals[interp->valSize]);
	mpq_set(interp->mpVals[interp->valSize], val);

	int nvar = interp->nvar;
	interp->mpPoints[interp->valSize] = (mpq_t*) malloc(nvar*sizeof(mpq_t));
	for (int i = 0; i < nvar; ++i) {
		mpq_init(interp->mpPoints[interp->valSize][i]);
		mpq_set(interp->mpPoints[interp->valSize][i], point[i]);
	}

	_rfInterpAddPolynomialRow(interp, interp->valSize);
	++(interp->valSize);

}

void rfInterpAddPointsMP(Interpolator_t* interp, mpq_t** points, mpq_t* vals, int nPoints) {
	int nvar = interp->nvar;

	if (interp->valSize == 0) {
		interp->valAlloc = nPoints+1;
		interp->mpVals = (mpq_t*) malloc(sizeof(mpq_t)*interp->valAlloc);
		interp->mpPoints = (mpq_t**) malloc(sizeof(mpq_t*)*interp->valAlloc);
		int nvar = interp->nvar;
		interp->mpPoints[0] = (mpq_t*) malloc(sizeof(mpq_t)*nvar);
		for (int k = 0; k < nvar; ++k) {
			mpq_init(interp->mpPoints[0][k]);
		}
		mpq_init(interp->mpVals[0]);
		mpq_set_ui(interp->mpVals[0], 1l, 1l);
		interp->valSize = 1;

		for (int i = 1; i <= nPoints; ++i) {
			interp->mpPoints[i] = (mpq_t*) malloc(sizeof(mpq_t)*nvar);
			for (int k = 0; k < nvar; ++k) {
				mpq_init(interp->mpPoints[i][k]);
				mpq_set(interp->mpPoints[i][k], points[i-1][k]);
			}

			mpq_init(interp->mpVals[i]);
			mpq_set(interp->mpVals[i], vals[i-1]);

			_rfInterpAddPolynomialRow(interp, i);
		}

		interp->valSize = nPoints + 1;
	} else {
		int valSize = interp->valSize;
		if (valSize + nPoints > interp->valAlloc) {
			interp->valAlloc += nPoints;
			interp->mpVals = (mpq_t*) realloc(interp->mpVals, interp->valAlloc*sizeof(mpq_t));
			interp->mpPoints = (mpq_t**) realloc(interp->mpPoints, interp->valAlloc*sizeof(mpq_t*));
		}

		for (int i = 0; i < nPoints; ++i) {
			mpq_init(interp->mpVals[interp->valSize + i]);
			mpq_set(interp->mpVals[interp->valSize + i], vals[i]);
			interp->mpPoints[interp->valSize + i] = (mpq_t*) malloc(sizeof(mpq_t)*nvar);
			for (int k = 0; k < nvar; ++k) {
				mpq_init(interp->mpPoints[interp->valSize +i][k]);
				mpq_set(interp->mpPoints[interp->valSize +i][k], points[i][k]);
			}

			_rfInterpAddPolynomialRow(interp, interp->valSize);
			++(interp->valSize);
		}
		
	}
}

int _rfInterpIncrementDeltas(int* deltaNum, int* degreeBounds, int* deltaDen, int* denomDegreeBounds, int* flipflop, int nvar) {	

	int enumSucc = 0;
	if (*flipflop) {
		enumSucc = enumerateDegreeBounds(deltaDen, denomDegreeBounds, nvar);
		if (!enumSucc) {
			enumSucc = enumerateDegreeBounds(deltaNum, degreeBounds, nvar);
			*flipflop = 0;
		} else {
			*flipflop = 1 - *flipflop;
		}
	} else {
		enumSucc = enumerateDegreeBounds(deltaNum, degreeBounds, nvar);
		if (!enumSucc) {
			enumSucc = enumerateDegreeBounds(deltaDen, denomDegreeBounds, nvar);
			*flipflop = 1;
		} else {
			*flipflop = 1 - *flipflop;
		}
	}

	return enumSucc;
}


double _rfInterpCheckPolyDouble(Interpolator_t* interp, AltArr_t* numPoly, AltArr_t* denomPoly) {

	mpq_t numVal;
	mpq_t denVal;
	mpq_init(numVal);
	mpq_init(denVal);

	int npoints = interp->valSize;
	double curRes = 0;
	double residual = 0;
	for (int i = 1; i < npoints; ++i) {
		mpq_t* point = interp->mpPoints[i];

#if INTERP_TIMING
	unsigned long long start = 0;
	_startTimer(&start);
#endif
		evalPolyToVal_AA(numPoly, point, interp->nvar, numVal);
		evalPolyToVal_AA(denomPoly, point, interp->nvar, denVal);
#if INTERP_TIMING
	_stopTimerAddElapsed(&start, &evalTime);
#endif
		if (mpq_sgn(denVal) == 0) {
			residual = DBL_MAX;
			break;
		}

		curRes = mpq_get_d(numVal);
		curRes /= mpq_get_d(denVal);
		// mpq_div(numVal, numVal, denVal);

		curRes -= mpq_get_d(interp->mpVals[i]);
		residual += (curRes*curRes);

		// mpq_sub(numVal, numVal, *val);
		// mpq_mul(numVal, numVal, numVal);
		// mpq_add(residual, residual, numVal);
	}

	mpq_clear(numVal);
	mpq_clear(denVal);	

	return residual;
}

PolyInterpStatus_t rfInterpGetPoly_EarlyTerm(Interpolator_t* interp, AltArr_t** numPoly, AltArr_t** denomPoly, double maximumResidual) {
	return _rfInterpGetPoly(interp, numPoly, denomPoly, maximumResidual, 1);
}

PolyInterpStatus_t rfInterpGetPoly(Interpolator_t* interp, AltArr_t** numPoly, AltArr_t** denomPoly, double maximumResidual) {	
	// return _rfInterpGetPoly(interp, numPoly, denomPoly, maximumResidual, 1);
	return _rfInterpGetPoly(interp, numPoly, denomPoly, maximumResidual, 0);
}

PolyInterpStatus_t _rfInterpGetPoly(Interpolator_t* interp, AltArr_t** numPoly, AltArr_t** denomPoly, double maximumResidual, int earlyTerm) {	
	//fprintf(stderr, "Valsize: %ld\n\n", interp->valSize);

	if (interp->valSize <= 2) {
		return POLY_INTERP_FAILURE;
	}

	long cols = interp->nCols;
	long rows = interp->valSize;
	long n2 = interp->numeratorNumTerms;
	int nvar = interp->nvar;

	

	int deltaNum[nvar];
	int deltaDen[nvar];
	for (int k = 0; k < nvar; ++k) {
		deltaNum[k] = 0;
		deltaDen[k] = 0;
	}
	
	// double* dA = (double*) malloc(sizeof(double)*rows*cols);
#if INTERP_OPT
	long xSize = cols > rows ? cols : rows;
	long sSize = rows;
	double workSize = xSize;
	double* ds = (double*) malloc(sizeof(double)*sSize*1);
	double* X = (double*) malloc(sizeof(double)*xSize*1);
	double* work = (double*) malloc(sizeof(double)*xSize*1);
#else
	double* X = (double*) malloc(sizeof(double)*cols*1);
#endif
	double* db = (double*) malloc(sizeof(double)*rows*1);
	for (int i = 0; i < rows; ++i) {
		db[i] = 0;
	}
	db[0] = 1;

	AltArr_t* bestNum = NULL, *bestDen = NULL;
	// mpq_t bestResidual;
	// mpq_init(bestResidual);
	// mpq_set_si(bestResidual, -1l, 1l);
	double bestResD = -1.0;

	int flipflop = 0; //0 means update numerator first

#if INTERP_TIMING
	unsigned long long start;
	float getMatrixTime = 0.0f;
	float transposeTime = 0.0f;
	float solveTime = 0.0f;
	float resTime = 0.0f;
#endif

	while(_rfInterpIncrementDeltas(deltaNum, interp->degreeBounds, deltaDen, interp->denomDegreeBounds, &flipflop, nvar)) {

		// fprintf(stderr, "DeltaNum: ");
		// for (int i = 0; i < nvar; ++i) {
		// 	fprintf(stderr, "%d ", deltaNum[i]);
		// }
		// fprintf(stderr, "\nDeltaDen: ");
		// for (int i = 0; i < nvar; ++i) {
		// 	fprintf(stderr, "%d ", deltaDen[i]);
		// }
		// fprintf(stderr, "\n\n");

#if INTERP_TIMING
		_startTimer(&start);
#endif
		double* dA = _rfInterpGetSampleMatrix(interp, deltaNum, deltaDen, &rows, &cols, &n2);
#if INTERP_TIMING
		_stopTimerAddElapsed(&start, &getMatrixTime);
#endif

		if (cols > rows) {
			free(dA);
			continue;
		}

#if INTERP_OPT
		double* dAt = (double*) malloc(sizeof(double)*rows*cols);
		//out of place transpose
		for (int i = 0; i < rows; ++i) {
			for (int j = 0; j < cols; ++j) {
				dAt[j*rows + i] = dA[i*cols + j];
			}
		}
#endif

	//	fprintf(stderr, "rows: %ld, cols: %ld, n2: %ld\n", rows, cols, n2);	
	//	fprintf(stderr, "Got sample A:\n");
	//	printDoubleMat(dA, 1, cols);
	//	fprintf(stderr, "\n\n");
		// continue;

		//iterate over the terms to normalize
		for (int l = 0; l < cols - n2; ++l) {
#if INTERP_OPT
			for (int i = 0; i < 1; ++i) {
				for (int j = 0; j < cols; ++j) {
					//if transposed:
					dAt[j*rows + i] = 0.0;
				}
			}
			dAt[(n2 + l)*rows] = 1.0;
#else
			for (int i = 0; i < 1; ++i) {
				for (int j = 0; j < cols; ++j) {
					//if not transposed:
					dA[i*cols + j] = 0.0;
				}
			}
			dA[0*cols + n2 + l] = 1.0;
#endif
		
#if INTERP_DEBUG
			fprintf(stderr, "Solving A:\n");
			printDoubleMat(dAt, cols, rows);
			fprintf(stderr, "Solving b:\n");
			printDoubleMat(db, rows, 1);
#endif

#if INTERP_OPT
			//setup work matrix which will get overwritten on solve call.
#if INTERP_TIMING
			_startTimer(&start);
#endif
			memcpy(dA, dAt, sizeof(double)*rows*cols);
			long rank = solveDoubleSystemPreAlloc(dA, db, rows, cols, &X, &xSize, &ds, &sSize, &work, &workSize);
#if INTERP_TIMING
			_stopTimerAddElapsed(&start, &solveTime);
#endif

#else
			long rank = solveDoubleSystem(dA, db, rows, cols, X);
#endif

#if INTERP_DEBUG
			fprintf(stderr, "Got rank: %ld   ", rank);
			fprintf(stderr, "X: ");
			for (int i = 0; i < cols; ++i) {
				 gmp_fprintf(stderr, " %g", X[i]);
			}
			fprintf(stderr, "\n");
#endif

			int failed = 0;
			for (int i = 0; i < cols; ++i) {
				if (X[i] != X[i] || isinf(X[i])) {
					failed = 1;
					break;
				}
			}
			if (failed) {
				continue;
			}

			double eps = 1e-30;
			AltArr_t* curNum = buildPolyFromCoefArrayWithEps(deltaNum, interp->nvar, X, eps);
			AltArr_t* curDen = buildPolyFromCoefArrayWithEps(deltaDen, interp->nvar, X + n2, eps);

			if (curDen == NULL) {
#if INTERP_DEBUG
				fprintf(stderr, "curDen was null\n");
#endif
				freePolynomial_AA(curNum);
				continue;
			}

#if INTERP_DEBUG
			char* syms[] = {"x", "y", "z"};
			fprintf(stderr, "num:\n");
			printPolyDouble_AA(stderr, curNum, syms, interp->nvar);
			fprintf(stderr, "\n");
			fprintf(stderr, "den:\n");
			printPolyDouble_AA(stderr, curDen, syms, interp->nvar);
			fprintf(stderr, "\n\n");
#endif

#if INTERP_TIMING
			_startTimer(&start);
#endif
			// mpq_t residual;
			// mpq_init(residual);
			// _rfInterpCheckPoly(interp, curNum, curDen, residual);
			// double dres = mpq_get_d(residual);
			double dres = _rfInterpCheckPolyDouble(interp, curNum, curDen);
			// fprintf(stderr, "residual: %g\n\n", dres);
#if INTERP_TIMING
			_stopTimerAddElapsed(&start, &resTime);
#endif
			if (bestResD < 0) {
			// if (mpq_sgn(bestResidual) < 0) {
				bestNum = curNum;
				bestDen = curDen;
				// mpq_set(bestResidual, residual);
				bestResD = dres;
			} else {
				if (dres < bestResD) {
				// if (mpq_cmp(residual, bestResidual) < 0) {
					freePolynomial_AA(bestNum);
					freePolynomial_AA(bestDen);
					bestNum = curNum;
					bestDen = curDen;
					// mpq_set(bestResidual, residual);
					bestResD = dres;
				} else {
					freePolynomial_AA(curNum);
					freePolynomial_AA(curDen);
				}
			}

			// mpq_clear(residual);
		} //for l loop

		free(dA);
#if INTERP_OPT
		free(dAt);
#endif
		if (bestResD < 0) {
		// if (mpq_sgn(bestResidual) < 0) {
			continue;
		}
		//mpq_abs(bestResidual, bestResidual);
		if (earlyTerm && bestResD < maximumResidual) {
		// if (earlyTerm && mpq_get_d(bestResidual) < maximumResidual) {
			break;
		}

	} //while delta loop

	*numPoly = bestNum;
	*denomPoly = bestDen;


#if INTERP_OPT
	free(work);
	free(ds);
#endif
	free(X);
	free(db);

	if (bestResD < 0 || bestResD > maximumResidual) {
	// if (mpq_sgn(bestResidual) < 0 || mpq_get_d(bestResidual) > maximumResidual) {
		// mpq_clear(bestResidual);
		freePolynomial_AA(*numPoly);
		freePolynomial_AA(*denomPoly);
		*numPoly = NULL;
		*denomPoly = NULL;
		return POLY_INTERP_FAIL_BY_RESIDUAL;
	}

#if INTERP_TIMING
	fprintf(stderr, "build matrix time: %f\n", getMatrixTime);
	fprintf(stderr, "solve time: %f\n", solveTime);
	fprintf(stderr, "residual time: %f\n", resTime);
	fprintf(stderr, "eval time: %f\n", evalTime);
#endif

	//now make denominator monic.
	if (*denomPoly != NULL && (*denomPoly)->size > 0) {
		mpq_t temp;
		mpq_init(temp);
		mpq_set(temp, (*denomPoly)->elems[0].coef);
		mpq_inv(temp, temp);
		multiplyByRational_AA_inp(*numPoly, temp);
		multiplyByRational_AA_inp(*denomPoly, temp);
		mpq_clear(temp);
	}

	//mpq_abs(bestResidual, bestResidual);
//	fprintf(stderr, "\n\nBest residual: %g, maximumResidual: %g, abs: %g, cmp: %d\n\n", mpq_get_d(bestResidual), maximumResidual, mpq_get_d(bestResidual), mpq_get_d(bestResidual) > maximumResidual);

	// mpq_clear(bestResidual);
	return POLY_INTERP_SUCCESS;
}

PolyInterpStatus_t rfInterpCheckPoly(Interpolator_t* interp, const AltArr_t* numPoly, const AltArr_t* denomPoly, mpq_t* point, mpq_t val) {
	return rfInterpCheckPolyWithTolerance(interp, numPoly, denomPoly, point, val, 0.0);
}

PolyInterpStatus_t rfInterpCheckPolyWithTolerance(Interpolator_t* interp, const AltArr_t* numPoly, const AltArr_t* denomPoly, mpq_t* point, mpq_t val, double tolerance) {
	mpq_t numVal;
	mpq_t denVal;
	mpq_init(numVal);
	mpq_init(denVal);

	evalPolyToVal_AA(numPoly, point, interp->nvar, numVal);
	evalPolyToVal_AA(denomPoly, point, interp->nvar, denVal);

	if (mpq_sgn(denVal) == 0) {
		mpq_clear(numVal);
		mpq_clear(denVal);
		return POLY_INTERP_MORE_POINTS;
	}


	mpq_div(numVal, numVal, denVal);

	int cmp = 1;
	double eps = 1e-8;
	if (tolerance < eps) {
		cmp = mpq_cmp(numVal, val);
	} else {
		mpq_sub(numVal, numVal, val);
		mpq_div(numVal, numVal, val);
		if (mpq_sgn(numVal) < 0) {
			mpq_neg(numVal, numVal);
		}
		double dVal = mpq_get_d(numVal);
		if (dVal < tolerance) {
			cmp = 0;
		}
	}

	// gmp_fprintf(stderr, "\n\n////////////////////////// Check Poly got val: %Qd, Expected: %Qd\n", numVal, val);
	// gmp_fprintf(stderr, "point: ");
	// for (int i = 0; i < interp->nvar; ++i) {
		// gmp_fprintf(stderr, "%Qd ", point[i]);
	// }
	// fprintf(stderr, "\n");

	mpq_clear(numVal);
	mpq_clear(denVal);

	if (cmp == 0) {
		return POLY_INTERP_SUCCESS;
	} else {
		return POLY_INTERP_MORE_POINTS;
	}
}
