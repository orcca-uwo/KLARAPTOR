#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static real c_b4 = -1.f;
static real c_b5 = 1.f;
static integer c__1 = 1;

/* Subroutine */ int cptt02_(char *uplo, integer *n, integer *nrhs, real *d__,
	 complex *e, complex *x, integer *ldx, complex *b, integer *ldb, real 
	*resid)
{
    /* System generated locals */
    integer b_dim1, b_offset, x_dim1, x_offset, i__1;
    real r__1, r__2;

    /* Local variables */
    static integer j;
    static real anorm, bnorm, xnorm;
    extern doublereal slamch_(char *), clanht_(char *, integer *, 
	    real *, complex *);
    extern /* Subroutine */ int claptm_(char *, integer *, integer *, real *, 
	    real *, complex *, complex *, integer *, real *, complex *, 
	    integer *);
    extern doublereal scasum_(integer *, complex *, integer *);
    static real eps;


#define b_subscr(a_1,a_2) (a_2)*b_dim1 + a_1
#define b_ref(a_1,a_2) b[b_subscr(a_1,a_2)]
#define x_subscr(a_1,a_2) (a_2)*x_dim1 + a_1
#define x_ref(a_1,a_2) x[x_subscr(a_1,a_2)]


/*  -- LAPACK test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1992   


    Purpose   
    =======   

    CPTT02 computes the residual for the solution to a symmetric   
    tridiagonal system of equations:   
       RESID = norm(B - A*X) / (norm(A) * norm(X) * EPS),   
    where EPS is the machine epsilon.   

    Arguments   
    =========   

    UPLO    (input) CHARACTER*1   
            Specifies whether the superdiagonal or the subdiagonal of the   
            tridiagonal matrix A is stored.   
            = 'U':  E is the superdiagonal of A   
            = 'L':  E is the subdiagonal of A   

    N       (input) INTEGTER   
            The order of the matrix A.   

    NRHS    (input) INTEGER   
            The number of right hand sides, i.e., the number of columns   
            of the matrices B and X.  NRHS >= 0.   

    D       (input) REAL array, dimension (N)   
            The n diagonal elements of the tridiagonal matrix A.   

    E       (input) COMPLEX array, dimension (N-1)   
            The (n-1) subdiagonal elements of the tridiagonal matrix A.   

    X       (input) COMPLEX array, dimension (LDX,NRHS)   
            The n by nrhs matrix of solution vectors X.   

    LDX     (input) INTEGER   
            The leading dimension of the array X.  LDX >= max(1,N).   

    B       (input/output) COMPLEX array, dimension (LDB,NRHS)   
            On entry, the n by nrhs matrix of right hand side vectors B.   
            On exit, B is overwritten with the difference B - A*X.   

    LDB     (input) INTEGER   
            The leading dimension of the array B.  LDB >= max(1,N).   

    RESID   (output) REAL   
            norm(B - A*X) / (norm(A) * norm(X) * EPS)   

    =====================================================================   


       Quick return if possible   

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
    if (*n <= 0) {
	*resid = 0.f;
	return 0;
    }

/*     Compute the 1-norm of the tridiagonal matrix A. */

    anorm = clanht_("1", n, &d__[1], &e[1]);

/*     Exit with RESID = 1/EPS if ANORM = 0. */

    eps = slamch_("Epsilon");
    if (anorm <= 0.f) {
	*resid = 1.f / eps;
	return 0;
    }

/*     Compute B - A*X. */

    claptm_(uplo, n, nrhs, &c_b4, &d__[1], &e[1], &x[x_offset], ldx, &c_b5, &
	    b[b_offset], ldb);

/*     Compute the maximum over the number of right hand sides of   
          norm(B - A*X) / ( norm(A) * norm(X) * EPS ). */

    *resid = 0.f;
    i__1 = *nrhs;
    for (j = 1; j <= i__1; ++j) {
	bnorm = scasum_(n, &b_ref(1, j), &c__1);
	xnorm = scasum_(n, &x_ref(1, j), &c__1);
	if (xnorm <= 0.f) {
	    *resid = 1.f / eps;
	} else {
/* Computing MAX */
	    r__1 = *resid, r__2 = bnorm / anorm / xnorm / eps;
	    *resid = dmax(r__1,r__2);
	}
/* L10: */
    }

    return 0;

/*     End of CPTT02 */

} /* cptt02_ */

#undef x_ref
#undef x_subscr
#undef b_ref
#undef b_subscr


