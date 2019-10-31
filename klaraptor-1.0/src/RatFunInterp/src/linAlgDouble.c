

#include "linAlgDouble.h"
#include <stdio.h>
#include <string.h>

#if defined(WITH_ATLAS) && WITH_ATLAS
#include "blaswrap.h"
#endif 

#include <math.h>

#include "f2c.h"
#include "clapack.h"

//forward declares
long solveDoubleSystem_SS(const double* A_in, const double* b_in, long n, long m, double* X);
long solveDoubleSystem_SY(const double* A_in, const double* b_in, long n, long m, double* X);
long solveDoubleSystem_SD(const double* A_in, const double* b_in, long n, long m, double* X);
long solveDoubleSystemPreAlloc_SS(double* A, const double* b, long n, long m, double** X_in, long* xSize, double** s_in, long* sSize, double** work_in, double* worksize_in);


long solveDoubleSystem(const double* A_in, const double* b_in, long n, long m, double* X) {
	return solveDoubleSystem_SS(A_in, b_in, n, m, X);
}

long solveDoubleSystemPreAlloc(double* A, const double* b, long n, long m, double** X_in, long* xSize, double** s_in, long* sSize, double** work_in, double* worksize_in) {
	return solveDoubleSystemPreAlloc_SS(A, b, n, m, X_in, xSize, s_in, sSize, work_in, worksize_in);
}

long solveDoubleSystemPreAlloc_SS(double* A, const double* b, long n, long m, double** X_in, long* xSize, double** s_in, long* sSize, double** work_in, double* worksize_in) {
	if (A == NULL || b == NULL || X_in == NULL || xSize == NULL || s_in == NULL || sSize == NULL || work_in == NULL || worksize_in == NULL) {
		return -1;
	}

	long nrhs = 1, nrows = n, ncolumns = m, lda = nrows;
	long ldb = n > m ? n : m;

	long xSizeNeeded = ldb > ncolumns ? ldb : ncolumns;
	if (*xSize < xSizeNeeded) {
		*X_in = (double*) realloc(*X_in, sizeof(double)*xSizeNeeded);
		*xSize = ldb;
	}
	memcpy(*X_in, b, sizeof(double)*ldb);

	// double rcond = 0.01;
	double rcond = -1.0;
	if (*sSize < nrows) {
		*s_in = (double*) realloc(*s_in, sizeof(double)*nrows);
		*sSize = nrows;
	}
	long rank, info;

        double worksize;
	long lwork = -1;
	dgelss_(&nrows, &ncolumns, &nrhs, A, &lda, *X_in, &ldb, *s_in, &rcond, &rank, &worksize, &lwork, &info);

	// Allocate workspace
	lwork = worksize;
	double* tmpWork = malloc(lwork * sizeof(tmpWork));
	// Do computationz
	dgelss_(&nrows, &ncolumns, &nrhs, A, &lda, *X_in, &ldb, *s_in, &rcond, &rank, tmpWork, &lwork, &info);
	// Free workspace
	free(tmpWork);



//	double worksize;
//    long lwork = -1;
//    dgelss_(&nrows, &ncolumns, &nrhs, A, &lda, *X_in, &ldb, *s_in, &rcond, &rank, &worksize, &lwork, &info);

    // Allocate workspace
//    lwork = worksize;
//    if (*worksize_in < worksize) {
//    	*work_in = (double*) realloc(*work_in, sizeof(double)*lwork);
//    	*worksize_in = lwork;
//    }
//    // Do computationz
//    dgelss_(&nrows, &ncolumns, &nrhs, A, &lda, *X_in, &ldb, *s_in, &rcond, &rank, *work_in, &lwork, &info);

    if (info > 0) {
    	fprintf(stderr, "System solving failed! Could not converge on %ld elements of the SVD of A\n", info);
    	exit(1);
    }
    if (info < 0) {
    	fprintf(stderr, "Invalid arguments to CLAPACK: %ld\n", info);
    	exit(1);
    }

	return rank;	
}

long solveDoubleSystem_SS(const double* A_in, const double* b_in, long n, long m, double* X) {

	long nrhs = 1, nrows = n, ncolumns = m, lda = nrows;
	long ldb = n > m ? n : m;

	double* A = (double*) malloc(sizeof(double)*nrows*ncolumns);
	double* b = (double*) malloc(sizeof(double)*1*ldb);
	memcpy(b, b_in, sizeof(double)*1*ldb);

	//out of place transpose
	for (int i = 0; i < n; ++i) {
		for (int j = 0; j < m; ++j) {
			A[j*n + i] = A_in[i*m + j];
		}
	}

	// fprintf(stderr, "At: \n");
	// printDoubleMat(A, m, n);
	// fprintf(stderr, "b: \n");
	// printDoubleMat(b, n, 1);

	// //clapack uses M x N for rows x columns. Opposite of us. 
	// fprintf(stderr, "nrhs: %ld, nrows: %ld, ncolumns: %ld, lda: %ld, ldb: %ld\n", nrhs, nrows, ncolumns, lda, ldb);

	// double rcond = 0.01;
	double rcond = -1.0;
	double* s = (double*) malloc(sizeof(double)*nrows);
	long rank, info;

	double worksize;
    long lwork = -1;
    dgelss_(&nrows, &ncolumns, &nrhs, A, &lda, b, &ldb, s, &rcond, &rank, &worksize, &lwork, &info);

    // Allocate workspace
    lwork = worksize;
    double* work = malloc(lwork * sizeof(work));
    // Do computationz
    dgelss_(&nrows, &ncolumns, &nrhs, A, &lda, b, &ldb, s, &rcond, &rank, work, &lwork, &info);
    // Free workspace
    free(work);

    // fprintf(stderr, "Got rank: %ld\n", rank);
    // for (int i = 0; i < nrows; ++i) {
    // 	fprintf(stderr, "s[%d]: %g\n", i, s[i]);
    // }

    free(s);

    if (info > 0) {
    	fprintf(stderr, "System solving failed! Could not converge on %ld elements of the SVD of A\n", info);
    	exit(1);
    }
    if (info < 0) {
    	fprintf(stderr, "Invalid arguments to CLAPACK: %ld\n", info);
    	exit(1);
    }

    memcpy(X, b, sizeof(double)*ncolumns);

	free(A);
	free(b);

	return rank;
}


long solveDoubleSystem_SD(const double* A_in, const double* b_in, long n, long m, double* X) {

	long nrhs = 1, nrows = n, ncolumns = m, lda = nrows;
	long ldb = n > m ? n : m;

	double* A = (double*) malloc(sizeof(double)*nrows*ncolumns);
	double* b = (double*) malloc(sizeof(double)*1*ldb);
	memcpy(b, b_in, sizeof(double)*1*ldb);

	//out of place transpose
	for (int i = 0; i < n; ++i) {
		for (int j = 0; j < m; ++j) {
			A[j*n + i] = A_in[i*m + j];
		}
	}

	// fprintf(stderr, "At: \n");
	// printDoubleMat(A, m, n);
	// fprintf(stderr, "b: \n");
	// printDoubleMat(b, n, 1);

	// //clapack uses M x N for rows x columns. Opposite of us. 
	// fprintf(stderr, "nrhs: %ld, nrows: %ld, ncolumns: %ld, lda: %ld, ldb: %ld\n", nrhs, nrows, ncolumns, lda, ldb);

	// double rcond = 0.01;
	double rcond = -1.0;
	double* s = (double*) malloc(sizeof(double)*nrows);
	long rank, info;

	int minnm = n > m ? n : m;
	//int smlsiz = 1; //very bad estimate
	//double nlvl = log((double)minnm / (smlsiz+1) ) / log(2);
	//int nlvl_i = nlvl + 1.0 < 0 ? 0 : (int) nlvl + 1;
	long nlvl_i = (int) (log((double)minnm) / log(2)) + 1;
	long iworksize = 3*minnm*nlvl_i + 11*minnm;
	iworksize = iworksize < 1 ? 1 : iworksize;
	//iworksize = 8192;
	integer* iwork = (integer*) malloc(sizeof(integer)*iworksize);

//	fprintf(stderr, "iworksize: %ld", iworksize);
	double worksize;
    long lwork = -1;
    dgelsd_(&nrows, &ncolumns, &nrhs, A, &lda, b, &ldb, s, &rcond, &rank, &worksize, &lwork, iwork, &info);
//	fprintf(stderr, "finished query, iworksize: %ld\n", iworksize);
    // Allocate workspace
    lwork = worksize *2;
    double* work = malloc(lwork * sizeof(double));
    // Do computationz
    dgelsd_(&nrows, &ncolumns, &nrhs, A, &lda, b, &ldb, s, &rcond, &rank, work, &lwork, iwork, &info);
    // Free workspace
    free(work);
    free(iwork);

    // fprintf(stderr, "Got rank: %ld\n", rank);
    // for (int i = 0; i < nrows; ++i) {
    // 	fprintf(stderr, "s[%d]: %g\n", i, s[i]);
    // }

    free(s);

    if (info > 0) {
    	fprintf(stderr, "System solving failed! Could not converge on %ld elements of the SVD of A\n", info);
    	exit(1);
    }
    if (info < 0) {
    	fprintf(stderr, "Invalid arguments to CLAPACK: %ld\n", info);
    	exit(1);
    }

    memcpy(X, b, sizeof(double)*ncolumns);

	free(A);
	free(b);

	return rank;
}

long solveDoubleSystem_SY(const double* A_in, const double* b_in, long n, long m, double* X) {

	long nrhs = 1, nrows = n, ncolumns = m, lda = nrows;
	long ldb = n > m ? n : m;

	double* A = (double*) malloc(sizeof(double)*nrows*ncolumns);
	double* b = (double*) malloc(sizeof(double)*1*ldb);
	memcpy(b, b_in, sizeof(double)*1*ldb);

	//out of place transpose
	for (int i = 0; i < n; ++i) {
		for (int j = 0; j < m; ++j) {
			A[j*n + i] = A_in[i*m + j];
		}
	}

	// fprintf(stderr, "At: \n");
	// printDoubleMat(A, m, n);
	// fprintf(stderr, "b: \n");
	// printDoubleMat(b, n, 1);

	// //clapack uses M x N for rows x columns. Opposite of us. 
	// fprintf(stderr, "nrhs: %ld, nrows: %ld, ncolumns: %ld, lda: %ld, ldb: %ld\n", nrhs, nrows, ncolumns, lda, ldb);

	// double rcond = 0.01;
	double rcond = 0.0001;
	integer jpvt[nrows];
	long rank, info;

	for (int i = 0; i < nrows; ++i) {
		jpvt[i] = 0;
	}

	double worksize;
    long lwork = -1;
    dgelsy_(&nrows, &ncolumns, &nrhs, A, &lda, b, &ldb, jpvt, &rcond, &rank, &worksize, &lwork, &info);

    // Allocate workspace
    lwork = worksize;
    double* work = malloc(lwork * sizeof(work));
    // Do computationz
    dgelsy_(&nrows, &ncolumns, &nrhs, A, &lda, b, &ldb, jpvt, &rcond, &rank, work, &lwork, &info);
    // Free workspace
    free(work);

    // fprintf(stderr, "Got rank: %ld\n", rank);
    // for (int i = 0; i < nrows; ++i) {
    // 	fprintf(stderr, "s[%d]: %g\n", i, s[i]);
    // }

    if (info > 0) {
    	fprintf(stderr, "System solving failed! Could not converge on %ld elements of the SVD of A\n", info);
    	exit(1);
    }
    if (info < 0) {
    	fprintf(stderr, "Invalid arguments to CLAPACK: %ld\n", info);
    	exit(1);
    }

    memcpy(X, b, sizeof(double)*ncolumns);

	free(A);
	free(b);

	return rank;
}

double* getDoubleIdentity(int n) {
	double* A = (double*) calloc(sizeof(double), n*n);

	for (int i = 0; i < n; ++i) {
		for (int j = 0; j < n; ++j) {
			if (i == j) {
				A[i*n + j] = 1.0;
			}
		}
	}

	return A;
}

void printDoubleMat(const double* A_mp, int n, int m) {
	long i, j;
	// fprintf(stderr, "Matrix([");
	// for (i = 0; i < n-1; i++)
	// {
	// 	fprintf(stderr, "[");
	// 	for (j = 0; j < m-1; j++)
	// 		fprintf(stderr, "%g, ", A_mp[i*m+j]);
	// 	for (j = m-1; j < m; j++)
	// 		fprintf(stderr, "%g", A_mp[i*m+j]);
	// 	fprintf(stderr, "],");
	// }
	// fprintf(stderr, "[");	
	// for (i = n-1; i < n; i++) {
	// 	for (j = 0; j < m-1; j++)
	// 		fprintf(stderr, "%g, ", A_mp[i*m+j]);
	// 	for (j = m-1; j < m; j++)
	// 		fprintf(stderr, "%g", A_mp[i*m+j]);
	// }
	// fprintf(stderr, "]]);\n");
	// fprintf(stderr, "\n\n");

	// long i, j;
	for (i = 0; i < n; i++)
	{
		fprintf(stdout, "  ");
		for (j = 0; j < m; j++)
			fprintf(stdout, "%.2g\t", A_mp[i*m+j]);
		fprintf(stdout, "\n");
	}
}



