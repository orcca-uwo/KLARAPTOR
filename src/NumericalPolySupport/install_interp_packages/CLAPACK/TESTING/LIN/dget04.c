#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* Subroutine */ int dget04_(integer *n, integer *nrhs, doublereal *x, 
	integer *ldx, doublereal *xact, integer *ldxact, doublereal *rcond, 
	doublereal *resid)
{
    /* System generated locals */
    integer x_dim1, x_offset, xact_dim1, xact_offset, i__1, i__2;
    doublereal d__1, d__2, d__3;

    /* Local variables */
    static integer i__, j;
    static doublereal xnorm;
    extern doublereal dlamch_(char *);
    static integer ix;
    static doublereal diffnm;
    extern integer idamax_(integer *, doublereal *, integer *);
    static doublereal eps;


#define xact_ref(a_1,a_2) xact[(a_2)*xact_dim1 + a_1]
#define x_ref(a_1,a_2) x[(a_2)*x_dim1 + a_1]


/*  -- LAPACK test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 29, 1992   


    Purpose   
    =======   

    DGET04 computes the difference between a computed solution and the   
    true solution to a system of linear equations.   

    RESID =  ( norm(X-XACT) * RCOND ) / ( norm(XACT) * EPS ),   
    where RCOND is the reciprocal of the condition number and EPS is the   
    machine epsilon.   

    Arguments   
    =========   

    N       (input) INTEGER   
            The number of rows of the matrices X and XACT.  N >= 0.   

    NRHS    (input) INTEGER   
            The number of columns of the matrices X and XACT.  NRHS >= 0.   

    X       (input) DOUBLE PRECISION array, dimension (LDX,NRHS)   
            The computed solution vectors.  Each vector is stored as a   
            column of the matrix X.   

    LDX     (input) INTEGER   
            The leading dimension of the array X.  LDX >= max(1,N).   

    XACT    (input) DOUBLE PRECISION array, dimension( LDX, NRHS )   
            The exact solution vectors.  Each vector is stored as a   
            column of the matrix XACT.   

    LDXACT  (input) INTEGER   
            The leading dimension of the array XACT.  LDXACT >= max(1,N).   

    RCOND   (input) DOUBLE PRECISION   
            The reciprocal of the condition number of the coefficient   
            matrix in the system of equations.   

    RESID   (output) DOUBLE PRECISION   
            The maximum over the NRHS solution vectors of   
            ( norm(X-XACT) * RCOND ) / ( norm(XACT) * EPS )   

    =====================================================================   


       Quick exit if N = 0 or NRHS = 0.   

       Parameter adjustments */
    x_dim1 = *ldx;
    x_offset = 1 + x_dim1 * 1;
    x -= x_offset;
    xact_dim1 = *ldxact;
    xact_offset = 1 + xact_dim1 * 1;
    xact -= xact_offset;

    /* Function Body */
    if (*n <= 0 || *nrhs <= 0) {
	*resid = 0.;
	return 0;
    }

/*     Exit with RESID = 1/EPS if RCOND is invalid. */

    eps = dlamch_("Epsilon");
    if (*rcond < 0.) {
	*resid = 1. / eps;
	return 0;
    }

/*     Compute the maximum of   
          norm(X - XACT) / ( norm(XACT) * EPS )   
       over all the vectors X and XACT . */

    *resid = 0.;
    i__1 = *nrhs;
    for (j = 1; j <= i__1; ++j) {
	ix = idamax_(n, &xact_ref(1, j), &c__1);
	xnorm = (d__1 = xact_ref(ix, j), abs(d__1));
	diffnm = 0.;
	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
/* Computing MAX */
	    d__2 = diffnm, d__3 = (d__1 = x_ref(i__, j) - xact_ref(i__, j), 
		    abs(d__1));
	    diffnm = max(d__2,d__3);
/* L10: */
	}
	if (xnorm <= 0.) {
	    if (diffnm > 0.) {
		*resid = 1. / eps;
	    }
	} else {
/* Computing MAX */
	    d__1 = *resid, d__2 = diffnm / xnorm * *rcond;
	    *resid = max(d__1,d__2);
	}
/* L20: */
    }
    if (*resid * eps < 1.) {
	*resid /= eps;
    }

    return 0;

/*     End of DGET04 */

} /* dget04_ */

#undef x_ref
#undef xact_ref


