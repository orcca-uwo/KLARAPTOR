#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Subroutine */ int dlaptm_(integer *n, integer *nrhs, doublereal *alpha, 
	doublereal *d__, doublereal *e, doublereal *x, integer *ldx, 
	doublereal *beta, doublereal *b, integer *ldb)
{
    /* System generated locals */
    integer b_dim1, b_offset, x_dim1, x_offset, i__1, i__2;

    /* Local variables */
    static integer i__, j;


#define b_ref(a_1,a_2) b[(a_2)*b_dim1 + a_1]
#define x_ref(a_1,a_2) x[(a_2)*x_dim1 + a_1]


/*  -- LAPACK auxiliary routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 29, 1992   


    Purpose   
    =======   

    DLAPTM multiplies an N by NRHS matrix X by a symmetric tridiagonal   
    matrix A and stores the result in a matrix B.  The operation has the   
    form   

       B := alpha * A * X + beta * B   

    where alpha may be either 1. or -1. and beta may be 0., 1., or -1.   

    Arguments   
    =========   

    N       (input) INTEGER   
            The order of the matrix A.  N >= 0.   

    NRHS    (input) INTEGER   
            The number of right hand sides, i.e., the number of columns   
            of the matrices X and B.   

    ALPHA   (input) DOUBLE PRECISION   
            The scalar alpha.  ALPHA must be 1. or -1.; otherwise,   
            it is assumed to be 0.   

    D       (input) DOUBLE PRECISION array, dimension (N)   
            The n diagonal elements of the tridiagonal matrix A.   

    E       (input) DOUBLE PRECISION array, dimension (N-1)   
            The (n-1) subdiagonal or superdiagonal elements of A.   

    X       (input) DOUBLE PRECISION array, dimension (LDX,NRHS)   
            The N by NRHS matrix X.   

    LDX     (input) INTEGER   
            The leading dimension of the array X.  LDX >= max(N,1).   

    BETA    (input) DOUBLE PRECISION   
            The scalar beta.  BETA must be 0., 1., or -1.; otherwise,   
            it is assumed to be 1.   

    B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)   
            On entry, the N by NRHS matrix B.   
            On exit, B is overwritten by the matrix expression   
            B := alpha * A * X + beta * B.   

    LDB     (input) INTEGER   
            The leading dimension of the array B.  LDB >= max(N,1).   

    =====================================================================   


       Parameter adjustments */
    --d__;
    --e;
    x_dim1 = *ldx;
    x_offset = 1 + x_dim1 * 1;
    x -= x_offset;
    b_dim1 = *ldb;
    b_offset = 1 + b_dim1 * 1;
    b -= b_offset;

    /* Function Body */
    if (*n == 0) {
	return 0;
    }

/*     Multiply B by BETA if BETA.NE.1. */

    if (*beta == 0.) {
	i__1 = *nrhs;
	for (j = 1; j <= i__1; ++j) {
	    i__2 = *n;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		b_ref(i__, j) = 0.;
/* L10: */
	    }
/* L20: */
	}
    } else if (*beta == -1.) {
	i__1 = *nrhs;
	for (j = 1; j <= i__1; ++j) {
	    i__2 = *n;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		b_ref(i__, j) = -b_ref(i__, j);
/* L30: */
	    }
/* L40: */
	}
    }

    if (*alpha == 1.) {

/*        Compute B := B + A*X */

	i__1 = *nrhs;
	for (j = 1; j <= i__1; ++j) {
	    if (*n == 1) {
		b_ref(1, j) = b_ref(1, j) + d__[1] * x_ref(1, j);
	    } else {
		b_ref(1, j) = b_ref(1, j) + d__[1] * x_ref(1, j) + e[1] * 
			x_ref(2, j);
		b_ref(*n, j) = b_ref(*n, j) + e[*n - 1] * x_ref(*n - 1, j) + 
			d__[*n] * x_ref(*n, j);
		i__2 = *n - 1;
		for (i__ = 2; i__ <= i__2; ++i__) {
		    b_ref(i__, j) = b_ref(i__, j) + e[i__ - 1] * x_ref(i__ - 
			    1, j) + d__[i__] * x_ref(i__, j) + e[i__] * x_ref(
			    i__ + 1, j);
/* L50: */
		}
	    }
/* L60: */
	}
    } else if (*alpha == -1.) {

/*        Compute B := B - A*X */

	i__1 = *nrhs;
	for (j = 1; j <= i__1; ++j) {
	    if (*n == 1) {
		b_ref(1, j) = b_ref(1, j) - d__[1] * x_ref(1, j);
	    } else {
		b_ref(1, j) = b_ref(1, j) - d__[1] * x_ref(1, j) - e[1] * 
			x_ref(2, j);
		b_ref(*n, j) = b_ref(*n, j) - e[*n - 1] * x_ref(*n - 1, j) - 
			d__[*n] * x_ref(*n, j);
		i__2 = *n - 1;
		for (i__ = 2; i__ <= i__2; ++i__) {
		    b_ref(i__, j) = b_ref(i__, j) - e[i__ - 1] * x_ref(i__ - 
			    1, j) - d__[i__] * x_ref(i__, j) - e[i__] * x_ref(
			    i__ + 1, j);
/* L70: */
		}
	    }
/* L80: */
	}
    }
    return 0;

/*     End of DLAPTM */

} /* dlaptm_ */

#undef x_ref
#undef b_ref


