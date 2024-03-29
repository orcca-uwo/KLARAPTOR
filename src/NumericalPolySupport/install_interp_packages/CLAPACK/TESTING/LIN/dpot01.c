#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static doublereal c_b14 = 1.;

/* Subroutine */ int dpot01_(char *uplo, integer *n, doublereal *a, integer *
	lda, doublereal *afac, integer *ldafac, doublereal *rwork, doublereal 
	*resid)
{
    /* System generated locals */
    integer a_dim1, a_offset, afac_dim1, afac_offset, i__1, i__2;

    /* Local variables */
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *, 
	    integer *);
    extern /* Subroutine */ int dsyr_(char *, integer *, doublereal *, 
	    doublereal *, integer *, doublereal *, integer *);
    static integer i__, j, k;
    static doublereal t;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *, 
	    integer *);
    extern logical lsame_(char *, char *);
    static doublereal anorm;
    extern /* Subroutine */ int dtrmv_(char *, char *, char *, integer *, 
	    doublereal *, integer *, doublereal *, integer *);
    extern doublereal dlamch_(char *), dlansy_(char *, char *, 
	    integer *, doublereal *, integer *, doublereal *);
    static doublereal eps;


#define a_ref(a_1,a_2) a[(a_2)*a_dim1 + a_1]
#define afac_ref(a_1,a_2) afac[(a_2)*afac_dim1 + a_1]


/*  -- LAPACK test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 29, 1992   


    Purpose   
    =======   

    DPOT01 reconstructs a symmetric positive definite matrix  A  from   
    its L*L' or U'*U factorization and computes the residual   
       norm( L*L' - A ) / ( N * norm(A) * EPS ) or   
       norm( U'*U - A ) / ( N * norm(A) * EPS ),   
    where EPS is the machine epsilon.   

    Arguments   
    ==========   

    UPLO    (input) CHARACTER*1   
            Specifies whether the upper or lower triangular part of the   
            symmetric matrix A is stored:   
            = 'U':  Upper triangular   
            = 'L':  Lower triangular   

    N       (input) INTEGER   
            The number of rows and columns of the matrix A.  N >= 0.   

    A       (input) DOUBLE PRECISION array, dimension (LDA,N)   
            The original symmetric matrix A.   

    LDA     (input) INTEGER   
            The leading dimension of the array A.  LDA >= max(1,N)   

    AFAC    (input/output) DOUBLE PRECISION array, dimension (LDAFAC,N)   
            On entry, the factor L or U from the L*L' or U'*U   
            factorization of A.   
            Overwritten with the reconstructed matrix, and then with the   
            difference L*L' - A (or U'*U - A).   

    LDAFAC  (input) INTEGER   
            The leading dimension of the array AFAC.  LDAFAC >= max(1,N).   

    RWORK   (workspace) DOUBLE PRECISION array, dimension (N)   

    RESID   (output) DOUBLE PRECISION   
            If UPLO = 'L', norm(L*L' - A) / ( N * norm(A) * EPS )   
            If UPLO = 'U', norm(U'*U - A) / ( N * norm(A) * EPS )   

    =====================================================================   


       Quick exit if N = 0.   

       Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    afac_dim1 = *ldafac;
    afac_offset = 1 + afac_dim1 * 1;
    afac -= afac_offset;
    --rwork;

    /* Function Body */
    if (*n <= 0) {
	*resid = 0.;
	return 0;
    }

/*     Exit with RESID = 1/EPS if ANORM = 0. */

    eps = dlamch_("Epsilon");
    anorm = dlansy_("1", uplo, n, &a[a_offset], lda, &rwork[1]);
    if (anorm <= 0.) {
	*resid = 1. / eps;
	return 0;
    }

/*     Compute the product U'*U, overwriting U. */

    if (lsame_(uplo, "U")) {
	for (k = *n; k >= 1; --k) {

/*           Compute the (K,K) element of the result. */

	    t = ddot_(&k, &afac_ref(1, k), &c__1, &afac_ref(1, k), &c__1);
	    afac_ref(k, k) = t;

/*           Compute the rest of column K. */

	    i__1 = k - 1;
	    dtrmv_("Upper", "Transpose", "Non-unit", &i__1, &afac[afac_offset]
		    , ldafac, &afac_ref(1, k), &c__1);

/* L10: */
	}

/*     Compute the product L*L', overwriting L. */

    } else {
	for (k = *n; k >= 1; --k) {

/*           Add a multiple of column K of the factor L to each of   
             columns K+1 through N. */

	    if (k + 1 <= *n) {
		i__1 = *n - k;
		dsyr_("Lower", &i__1, &c_b14, &afac_ref(k + 1, k), &c__1, &
			afac_ref(k + 1, k + 1), ldafac);
	    }

/*           Scale column K by the diagonal element. */

	    t = afac_ref(k, k);
	    i__1 = *n - k + 1;
	    dscal_(&i__1, &t, &afac_ref(k, k), &c__1);

/* L20: */
	}
    }

/*     Compute the difference  L*L' - A (or U'*U - A). */

    if (lsame_(uplo, "U")) {
	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
	    i__2 = j;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		afac_ref(i__, j) = afac_ref(i__, j) - a_ref(i__, j);
/* L30: */
	    }
/* L40: */
	}
    } else {
	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
	    i__2 = *n;
	    for (i__ = j; i__ <= i__2; ++i__) {
		afac_ref(i__, j) = afac_ref(i__, j) - a_ref(i__, j);
/* L50: */
	    }
/* L60: */
	}
    }

/*     Compute norm( L*U - A ) / ( N * norm(A) * EPS ) */

    *resid = dlansy_("1", uplo, n, &afac[afac_offset], ldafac, &rwork[1]);

    *resid = *resid / (doublereal) (*n) / anorm / eps;

    return 0;

/*     End of DPOT01 */

} /* dpot01_ */

#undef afac_ref
#undef a_ref


