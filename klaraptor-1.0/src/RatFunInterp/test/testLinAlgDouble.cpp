
#include <gmpxx.h>
#include "linAlgDouble.h"
#include "interpolator.h"
#include "SMQP/SMQP_Support_Test-AA.h"
#include "Unix_Timer.h"
#include <stdio.h>
#include <iostream>

char* syms[] = {"x", "y", "z", "t", "u", "v", "w"};
unsigned long int coefBound = 4;
float sparsity = 0.5f;
int includeNeg = 1;

bool isPolysEqualDouble(AltArr_t* a, AltArr_t* b, double eps) {
	if (a == NULL) {
		return (b == NULL);
	}
	if (b == NULL) {
		return 0;
	}

	if (a->size != b->size) {
		return 0;
	}

	double da, db;
	for (int i = 0; i < a->size; ++i) {
		if (a->elems[i].degs != b->elems[i].degs) {
			return 0;
		}
		da = mpq_get_d(a->elems[i].coef);
		db = mpq_get_d(b->elems[i].coef);
		if (abs(da - db) > eps) {
			return 0;
		}
	}

	return 1;

}


void testRationalFunction() {

	time_t seed = time(NULL);//1531444569;//time(NULL);
	fprintf(stderr, "seed: %ld\n\n", seed);
	srand(seed);

	int nvar = 2;

	int degreeBounds[nvar];
	int denomDegrees[nvar];
	for (int i = 0; i < nvar; ++i) {
		degreeBounds[i] = 2;
		denomDegrees[i] = 2;
	}
	// degreeBounds[0] = 2;
	// degreeBounds[1] = 1;
	// degreeBounds[2] = 1;
	// denomDegrees[0] = 2;
	// denomDegrees[1] = 1;
	// denomDegrees[2] = 1;

	int n = 1;
	for (int i = 0; i < nvar; ++i) {
		n *= (degreeBounds[i] + 1);
		n *= (denomDegrees[i] + 1);
	}

	// int n = 1 * (degreeBounds[0] + 1) * (degreeBounds[1] +1) + (denomDegrees[0]+1) * (denomDegrees[1]+1);
	// std::cerr << "n: " << n << std::endl;

	// degreeBounds[1] = 0;
	// degreeBounds[2] = 0;
	AltArr_t* p = buildRandomPolyFromMax(nvar, degreeBounds, coefBound, sparsity, includeNeg);
	// degreeBounds[1] = 2;
	// degreeBounds[2] = 2;
	// AltArr_t* p = makePolynomial_AA(1, nvar);
	// mpq_init(p->elems->coef);
	// mpq_set_si(p->elems->coef, 4l, 1l);
	// p->elems->degs = (1ll << EXP_OFFSET_1_V2);
	// p->size = 1;

	// mpq_t tempC;
	// mpq_init(tempC);
	// mpq_set_si(tempC, 1l, 1l);
	// AltArr_t* q = makeConstPolynomial_AA(1, nvar, tempC);
	// mpq_clear(tempC);

	AltArr_t* q = buildRandomPolyFromMax(nvar, denomDegrees, coefBound, sparsity, includeNeg);

	//test with higher degree bounds than the polynomials themselves.
	degreeBounds[0] += 2;
	degreeBounds[1] += 2;
	denomDegrees[0] += 2;
	denomDegrees[1] += 2;

	//now make denominator monic.
	mpq_t temp;
	mpq_init(temp);
	mpq_set(temp, q->elems[0].coef);
	mpq_inv(temp, temp);
	multiplyByRational_AA_inp(p, temp);
	multiplyByRational_AA_inp(q, temp);
	mpq_clear(temp);

	// std::cerr << "p: " << std::endl;
	// printPoly_AA(stderr, p, syms, nvar);
	// std::cerr << "\n";
	// std::cerr << "q: " << std::endl;
	// printPoly_AA(stderr, q, syms, nvar);
	// std::cerr << "\n";
	// std::cerr << "\n";

	mpq_t numVal;
	mpq_t denVal;
	mpq_t point[nvar];

	mpq_init(numVal);
	mpq_init(denVal);
	for (int i = 0; i < nvar; ++i) {
		mpq_init(point[i]);
	}

	Interpolator_t* interp = rfInterpInit(nvar, degreeBounds, denomDegrees);

	mpq_t div;
	mpq_init(div);
	AltArr_t* numPoly = NULL, *denomPoly = NULL;

	// mpq_set_si(point[1], 1l, 1l);
	// int points[] = {32,64,128,256,512};
	// for (int i = 0; i < 10; ++i){
	// 	for (int j = 0; j < 5; ++j) {
	// 		mpq_set_si(point[0], points[j], 1l);
	// 		evalPolyToVal_AA(p, point, nvar, numVal);
	// 		evalPolyToVal_AA(q, point, nvar, denVal);
	// 		mpq_div(numVal, numVal, denVal);
	// 		rfInterpAddPointValMP(interp, point, numVal);
	// 		std::cerr << "Point: " << point[0] << "," << point[1]  << std::endl;
	// 		std::cerr << "Val: " << numVal << std::endl << std::endl;
	// 	}
	// }

	int nPoints = rfInterpGetRequiredNumPoints(interp) + 3;
	mpq_t** points = (mpq_t**) malloc(sizeof(mpq_t*)*nPoints);
	mpq_t* vals = (mpq_t*) malloc(sizeof(mpq_t)*nPoints);
	for (int i = 0; i < nPoints; ++i) {
		points[i] = (mpq_t*) malloc(sizeof(mpq_t)*nvar);
		for (int k = 0; k < nvar; ++k) {
			mpq_init(points[i][k]);
			mpq_set_si(points[i][k], (long) ((rand()%50)+1)* ((rand() % 2) == 0 ? -1 : 1)  , 1l);
			mpq_set_si(div, 1l, (long) (rand()%50)+1);
			mpq_mul(points[i][k], points[i][k], div);
		}
		evalPolyToVal_AA(p, points[i], nvar, numVal);
		evalPolyToVal_AA(q, points[i], nvar, denVal);
		mpq_div(numVal, numVal, denVal);
		mpq_init(vals[i]);
		mpq_set(vals[i], numVal);
	}
	rfInterpAddPointsMP(interp, points, vals, nPoints);


	// for (int i = 0; i < rfInterpGetRequiredNumPoints(interp) + 3; ++i) {
	// 	// fprintf(stderr, "adding point i: %d\n", i);
	// 	for (int k = 0; k < nvar; ++k) {
	// 		mpq_set_si(point[k], (long) ((rand()%10)+1)* ((rand() % 2) == 0 ? -1 : 1)  , 1l);
	// 		mpq_set_si(div, 1l, (long) (rand()%20)+1);
	// 		mpq_mul(point[k], point[k], div);
	// 	}
	// 	// std::cerr << "Point: " << point[0] << "," << point[1] << "," << point[2] << std::endl;

	// 	evalPolyToVal_AA(p, point, nvar, numVal);
	// 	evalPolyToVal_AA(q, point, nvar, denVal);
	// 	if (mpq_sgn(denVal) == 0) {
	// 		continue;
	// 	}
	// 	mpq_div(numVal, numVal, denVal);
	// 	// double dval = mpq_get_d(numVal);
	// 	// std::cerr << "Val: " << dval << std::endl << std::endl;
	// 	rfInterpAddPointValMP(interp, point, numVal);
	// 	// std::cerr << "Add point: " << i << std::endl;
	// }

	// std::cerr << "solving!" << std::endl;
	double eps = 1e-2; 
	//Such a large eps because slight differences in the double coefficeints
	//From interpolation back to mpq_t, then doing evaluation using mpq_t
	//And then back to double. 
	unsigned long long timeStart;
	_startTimer(&timeStart);
	PolyInterpStatus_t stat = rfInterpGetPoly(interp, &numPoly, &denomPoly, eps);
	float elapsed = 0.f;
	_stopTimer(&timeStart, &elapsed);
	std::cerr << "elapsed: " << elapsed << std::endl;
	if (stat == POLY_INTERP_FAILURE) {
		std::cerr << "Rational Function Interpolator test: FAILED" << std::endl;
		std::cerr << "Interpolant could do accurately match the data. Should degree bounds be higher?" << std::endl;
		exit(1);
	}

	// std::cerr << "pI: " << std::endl;
	// printPolyDouble_AA(stderr, numPoly, syms, nvar);
	// std::cerr << std::endl;
	// std::cerr << "qI: " << std::endl;
	// printPolyDouble_AA(stderr, denomPoly, syms, nvar);
	// std::cerr << std::endl;

	// std::cerr << "\np: " << std::endl;
	// printPolyDouble_AA(stderr, p, syms, nvar);
	// std::cerr << std::endl;
	// std::cerr << "q: " << std::endl;
	// printPolyDouble_AA(stderr, q, syms, nvar);
	// std::cerr << std::endl;

	if (!isPolysEqualDouble(numPoly, p, eps) || !isPolysEqualDouble(denomPoly, q, eps)) {
		bool failed = 0;
		mpq_t testNum;
		mpq_t testDen;
		mpq_init(testNum);
		mpq_init(testDen);
		for (int i = 0; i < 500; ++i) {
			for (int k = 0; k < nvar; ++k) {
				mpq_set_si(point[k], (long) ((rand()%50)+1)* ((rand() % 2) == 0 ? -1 : 1), 1l);
				mpq_set_si(div, 1l, (long) (rand()%50)+1);
				mpq_mul(point[k], point[k], div);
			}

			evalPolyToVal_AA(p, point, nvar, numVal);
			evalPolyToVal_AA(q, point, nvar, denVal);
			evalPolyToVal_AA(numPoly, point, nvar, testNum);
			evalPolyToVal_AA(denomPoly, point, nvar, testDen);

			if (mpq_sgn(denVal) == 0 && mpq_sgn(numVal) == 0) {
				//singularity that might not exist in interpolant. skip it.
				continue;
			}
			if (mpq_sgn(denVal) == 0) {
				if (mpq_sgn(testDen) == 0) {
					continue;
				} else {
					failed = 1;
					break;
				}
			} else if (mpq_sgn(testDen) == 0) {
				failed = 1;
				break;
			}

			mpq_div(numVal, numVal, denVal);
			mpq_div(testNum, testNum, testDen);
			double testVal = mpq_get_d(testNum);
			double expVal = mpq_get_d(numVal);
			if (fabs(testVal - expVal) > eps) {
				fprintf(stderr, "i: %d, failing eps: %f\n", i, fabs(testVal-expVal));
				fprintf(stderr, "failing point: ");
				for (int k = 0; k < nvar; ++k) {
					gmp_fprintf(stderr, "%Qd, ", points[k]);
				}
				fprintf(stderr, "\n");
				gmp_fprintf(stderr, "testVal: %f, expVal: %f\n", testVal, expVal);
				gmp_fprintf(stderr, "testNum: %Qd, numVal: %Qd\n", testNum, numVal);
				failed = 1;
				break;
			}
		}
		mpq_clear(testNum);
		mpq_clear(testDen);

		if(failed) {
			std::cerr << "Rational Function Interpolator test: FAILED" << std::endl;
			std::cerr << "\nProducing numerator: " << std::endl;
			printPolyDouble_AA(stderr, p, syms, nvar);
			std::cerr << "\nInterpolated numerator: " << std::endl;
			printPolyDouble_AA(stderr, numPoly, syms, nvar);
			std::cerr << "\nProducing denominator: " << std::endl;
			printPolyDouble_AA(stderr, q, syms, nvar);
			std::cerr << "\nInterpolated denominator: " << std::endl;
			printPolyDouble_AA(stderr, denomPoly, syms, nvar);
			std::cerr << "\n";
			exit(1);
		}

		// std::cerr << "pI: " << std::endl;
		// printPolyDouble_AA(stderr, numPoly, syms, nvar);
		// std::cerr << "\nqI: " << std::endl;
		// printPolyDouble_AA(stderr, denomPoly, syms, nvar);
	
		std::cerr << "\nRational Function Interpolator test: (with high probability) PASSED" << std::endl;
	} else {
		std::cerr << "Rational Function Interpolator test: PASSED" << std::endl;
	}

	for (int i = 0; i < nPoints; ++i) {
		for (int k = 0; k < nvar; ++k) {
			mpq_clear(points[i][k]);
		}
		free(points[i]);
		mpq_clear(vals[i]);
	}

	free(points);
	free(vals);

	mpq_clear(div);
	freePolynomial_AA(p);
	freePolynomial_AA(q);
	freePolynomial_AA(numPoly);
	freePolynomial_AA(denomPoly);

	rfInterpFree(interp);

}


#include <omp.h>

int main(int argc, char** argv) {

	int nvar = 3;
	float sparsity = 0.5;
	int degBound = 1;
	long coefBound = 10000;
	int ntrials = 10;

	if (argc > 1 && atoi(argv[1]) > 0) {
		nvar = atoi(argv[1]);
	}
	if (argc > 2 && atof(argv[2]) >= 0.0) {
		sparsity = atof(argv[2]);
	}
	if (argc > 3 && atoi(argv[3]) > 0) {
		degBound = atoi(argv[3]);
	}
	if (argc > 4 && atoi(argv[4]) > 0) {
		coefBound = atoi(argv[4]);
	}
	if (argc > 5 && atoi(argv[5]) > 0) {
		ntrials = atoi(argv[5]);
	}

	// std::cerr << "nvar: " << nvar << " sparsity: " << sparsity << " degBound: " << degBound << " coefBound : " << coefBound << std::endl;


	int maxDegs[nvar];
	for (int i = 0; i < nvar; ++i) {
	 	maxDegs[i] = degBound;
	}


	// char* syms[] = {"x", "y", "z"};
	// AltArr_t* p =  buildRandomPolyFromMax(nvar, maxDegs, coefBound, sparsity, 1);
	// printPoly_AA(stderr, p, syms, nvar);
	// char* pprog = polyToProgram_AA(p, "ppoly");
	// fprintf(stderr, "%s", pprog);

	// char* pProg = polyToProgram_AA(p, "aa");
	// fprintf(stderr, "%s\n", pProg);
	// printPoly_AA(stderr, p, syms, nvar);
	// std::cerr << "\n\n";

    // #pragma omp parallel for
	// for (int i = 0; i < 10; ++i) {
		testRationalFunction();
	// }

	return 0;
}
