

#include "linAlgDouble.h"
#include <stdio.h>


int main() {

	int n = 6;
	int m = 5;

	double* A = getDoubleIdentity(n);
	double* b = (double*) malloc(sizeof(double)*n);

	double* x = (double*) malloc(sizeof(double)*n);

 // -0.09   0.14  -0.46   0.68   1.29
 // -1.56   0.20   0.29   1.09   0.51
 // -1.48  -0.43   0.89  -0.71  -0.96
 // -1.09   0.84   0.77   2.11  -1.27
 //  0.08   0.55  -1.13   0.14   1.74
 // -1.59  -0.72   1.06   1.24   0.34
	A[0] = -0.09;
	A[1] =  0.14;
	A[2] = -0.46;
	A[3] =  0.68;
	A[4] =  1.29;
	A[5] = -1.56;
	A[6] =  0.20;
	A[7] =  0.29;
	A[8] =  1.09;
	A[9] =  0.51;
	A[10] = -1.48;
	A[11] = -0.43;
	A[12] =  0.89;
	A[13] = -0.71;
	A[14] = -0.96;
	A[15] = -1.09;
	A[16] =  0.84;
	A[17] =  0.77;
	A[18] =  2.11;
	A[19] = -1.27;
	A[20] =  0.08;
	A[21] =  0.55;
	A[22] = -1.13;
	A[23] =  0.14;
	A[24] =  1.74;
	A[25] = -1.59;
	A[26] = -0.72;
	A[27] =  1.06;
	A[28] =  1.24;
	A[29] =  0.34;

 //  7.4
 //  4.2
 // -8.3
 //  1.8
 //  8.6
 //  2.1 
	b[0] =  7.4;
	b[1] =  4.2;
	b[2] = -8.3;
	b[3] =  1.8;
	b[4] =  8.6;
	b[5] =  2.1;


	fprintf(stderr, "A: \n");
	// std::cerr << "A: " << std::endl;
	printDoubleMat(A, n, m);
	fprintf(stderr, "b: \n");
	// std::cerr << "b: " << std::endl;
	printDoubleMat(b, n, 1);


	// long rank = solveDoubleSystem(A, b, n, m, x);
  


	return 0;
}