#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static real c_b7 = -1.f;

/* Subroutine */ int sget10_(integer *m, integer *n, real *a, integer *lda, 
	real *b, integer *ldb, real *work, real *result)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, i__1;
    real r__1, r__2;

    /* Local variables */
    static real unfl;
    static integer j;
    static real anorm;
    extern doublereal sasum_(integer *, real *, integer *);
    extern /* Subroutine */ int scopy_(integer *, real *, integer *, real *, 
	    integer *);
    static real wnorm;
    extern /* Subroutine */ int saxpy_(integer *, real *, real *, integer *, 
	    real *, integer *);
    extern doublereal slamch_(char *), slange_(char *, integer *, 
	    integer *, real *, integer *, real *);
    static real eps;


#define a_ref(a_1,a_2) a[(a_2)*a_dim1 + a_1]
#define b_ref(a_1,a_2) b[(a_2)*b_dim1 + a_1]


/*  -- LAPACK test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 29, 1992   


    Purpose   
    =======   

    SGET10 compares two matrices A and B and computes the ratio   
    RESULT = norm( A - B ) / ( norm(A) * M * EPS )   

    Arguments   
    =========   

    M       (input) INTEGER   
            The number of rows of the matrices A and B.   

    N       (input) INTEGER   
            The number of columns of the matrices A and B.   

    A       (input) REAL array, dimension (LDA,N)   
            The m by n matrix A.   

    LDA     (input) INTEGER   
            The leading dimension of the array A.  LDA >= max(1,M).   

    B       (input) REAL array, dimension (LDB,N)   
            The m by n matrix B.   

    LDB     (input) INTEGER   
            The leading dimension of the array B.  LDB >= max(1,M).   

    WORK    (workspace) REAL array, dimension (M)   

    RESULT  (output) REAL   
            RESULT = norm( A - B ) / ( norm(A) * M * EPS )   

    =====================================================================   


       Quick return if possible   

       Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    b_dim1 = *ldb;
    b_offset = 1 + b_dim1 * 1;
    b -= b_offset;
    --work;

    /* Function Body */
    if (*m <= 0 || *n <= 0) {
	*result = 0.f;
	return 0;
    }

    unfl = slamch_("Safe minimum");
    eps = slamch_("Precision");

    wnorm = 0.f;
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	scopy_(m, &a_ref(1, j), &c__1, &work[1], &c__1);
	saxpy_(m, &c_b7, &b_ref(1, j), &c__1, &work[1], &c__1);
/* Computing MAX */
	r__1 = wnorm, r__2 = sasum_(n, &work[1], &c__1);
	wnorm = dmax(r__1,r__2);
/* L10: */
    }

/* Computing MAX */
    r__1 = slange_("1", m, n, &a[a_offset], lda, &work[1]);
    anorm = dmax(r__1,unfl);

    if (anorm > wnorm) {
	*result = wnorm / anorm / (*m * eps);
    } else {
	if (anorm < 1.f) {
/* Computing MIN */
	    r__1 = wnorm, r__2 = *m * anorm;
	    *result = dmin(r__1,r__2) / anorm / (*m * eps);
	} else {
/* Computing MIN */
	    r__1 = wnorm / anorm, r__2 = (real) (*m);
	    *result = dmin(r__1,r__2) / (*m * eps);
	}
    }

    return 0;

/*     End of SGET10 */

} /* sget10_ */

#undef b_ref
#undef a_ref


