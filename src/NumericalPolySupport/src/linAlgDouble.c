

#include "linAlgDouble.h"
#include <stdio.h>
#include <string.h>

#if defined(WITH_ATLAS) && WITH_ATLAS
#include "blaswrap.h"
#endif 

#include <math.h>

#include "f2c.h"
#include "clapack.h"

long solveDoubleSystem_SS(const double* A_in, const double* b_in, long n, long m, double* X);
long solveDoubleSystem_SY(const double* A_in, const double* b_in, long n, long m, double* X);
long solveDoubleSystem_SD(const double* A_in, const double* b_in, long n, long m, double* X);

long solveDoubleSystem(const double* A_in, const double* b_in, long n, long m, double* X) {
//	fprintf(stderr, "solving...\n");
	return solveDoubleSystem_SS(A_in, b_in, n, m, X);
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
    	fprintf(stderr, "System solving vailed! Could not converge on %ld elements of the SVD of A\n", info);
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
    	fprintf(stderr, "System solving vailed! Could not converge on %ld elements of the SVD of A\n", info);
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
    	fprintf(stderr, "System solving vailed! Could not converge on %ld elements of the SVD of A\n", info);
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
	double* A = (double*) malloc(sizeof(double)*n*n);

	for (int i = 0; i < n; ++i) {
		for (int j = 0; j < n; ++j) {
			A[i*n + j] = 0.0;
			if (i == j) {
				A[i*n + j] = 1.0;
			}
		}
	}

	return A;
}

void printDoubleMat(const double* A_mp, int n, int m) {
	long i, j;
	fprintf(stderr, "Matrix([");
	for (i = 0; i < n-1; i++)
	{
		fprintf(stderr, "[");
		for (j = 0; j < m-1; j++)
			fprintf(stderr, "%g, ", A_mp[i*m+j]);
		for (j = m-1; j < m; j++)
			fprintf(stderr, "%g", A_mp[i*m+j]);
		fprintf(stderr, "],");
	}
	fprintf(stderr, "[");	
	for (i = n-1; i < n; i++) {
		for (j = 0; j < m-1; j++)
			fprintf(stderr, "%g, ", A_mp[i*m+j]);
		for (j = m-1; j < m; j++)
			fprintf(stderr, "%g", A_mp[i*m+j]);
	}
	fprintf(stderr, "]]);\n");
	fprintf(stderr, "\n\n");

	// long i, j;
	for (i = 0; i < n; i++)
	{
		fprintf(stdout, "  ");
		for (j = 0; j < m; j++)
			fprintf(stdout, "%g\t", A_mp[i*m+j]);
		fprintf(stdout, "\n");
	}
}



