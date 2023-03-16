#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static real c_b14 = 1.f;

/* Subroutine */ int spbt01_(char *uplo, integer *n, integer *kd, real *a, 
	integer *lda, real *afac, integer *ldafac, real *rwork, real *resid)
{
    /* System generated locals */
    integer a_dim1, a_offset, afac_dim1, afac_offset, i__1, i__2, i__3;

    /* Local variables */
    static integer klen;
    extern doublereal sdot_(integer *, real *, integer *, real *, integer *);
    extern /* Subroutine */ int ssyr_(char *, integer *, real *, real *, 
	    integer *, real *, integer *);
    static integer i__, j, k;
    static real t;
    extern logical lsame_(char *, char *);
    extern /* Subroutine */ int sscal_(integer *, real *, real *, integer *);
    static real anorm;
    extern /* Subroutine */ int strmv_(char *, char *, char *, integer *, 
	    real *, integer *, real *, integer *);
    static integer kc, ml, mu;
    extern doublereal slamch_(char *), slansb_(char *, char *, 
	    integer *, integer *, real *, integer *, real *);
    static real eps;


#define a_ref(a_1,a_2) a[(a_2)*a_dim1 + a_1]
#define afac_ref(a_1,a_2) afac[(a_2)*afac_dim1 + a_1]


/*  -- LAPACK test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 29, 1992   


    Purpose   
    =======   

    SPBT01 reconstructs a symmetric positive definite band matrix A from   
    its L*L' or U'*U factorization and computes the residual   
       norm( L*L' - A ) / ( N * norm(A) * EPS ) or   
       norm( U'*U - A ) / ( N * norm(A) * EPS ),   
    where EPS is the machine epsilon, L' is the conjugate transpose of   
    L, and U' is the conjugate transpose of U.   

    Arguments   
    =========   

    UPLO    (input) CHARACTER*1   
            Specifies whether the upper or lower triangular part of the   
            symmetric matrix A is stored:   
            = 'U':  Upper triangular   
            = 'L':  Lower triangular   

    N       (input) INTEGER   
            The number of rows and columns of the matrix A.  N >= 0.   

    KD      (input) INTEGER   
            The number of super-diagonals of the matrix A if UPLO = 'U',   
            or the number of sub-diagonals if UPLO = 'L'.  KD >= 0.   

    A       (input) REAL array, dimension (LDA,N)   
            The original symmetric band matrix A.  If UPLO = 'U', the   
            upper triangular part of A is stored as a band matrix; if   
            UPLO = 'L', the lower triangular part of A is stored.  The   
            columns of the appropriate triangle are stored in the columns   
            of A and the diagonals of the triangle are stored in the rows   
            of A.  See SPBTRF for further details.   

    LDA     (input) INTEGER.   
            The leading dimension of the array A.  LDA >= max(1,KD+1).   

    AFAC    (input) REAL array, dimension (LDAFAC,N)   
            The factored form of the matrix A.  AFAC contains the factor   
            L or U from the L*L' or U'*U factorization in band storage   
            format, as computed by SPBTRF.   

    LDAFAC  (input) INTEGER   
            The leading dimension of the array AFAC.   
            LDAFAC >= max(1,KD+1).   

    RWORK   (workspace) REAL array, dimension (N)   

    RESID   (output) REAL   
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
	*resid = 0.f;
	return 0;
    }

/*     Exit with RESID = 1/EPS if ANORM = 0. */

    eps = slamch_("Epsilon");
    anorm = slansb_("1", uplo, n, kd, &a[a_offset], lda, &rwork[1]);
    if (anorm <= 0.f) {
	*resid = 1.f / eps;
	return 0;
    }

/*     Compute the product U'*U, overwriting U. */

    if (lsame_(uplo, "U")) {
	for (k = *n; k >= 1; --k) {
/* Computing MAX */
	    i__1 = 1, i__2 = *kd + 2 - k;
	    kc = max(i__1,i__2);
	    klen = *kd + 1 - kc;

/*           Compute the (K,K) element of the result. */

	    i__1 = klen + 1;
	    t = sdot_(&i__1, &afac_ref(kc, k), &c__1, &afac_ref(kc, k), &c__1)
		    ;
	    afac_ref(*kd + 1, k) = t;

/*           Compute the rest of column K. */

	    if (klen > 0) {
		i__1 = *ldafac - 1;
		strmv_("Upper", "Transpose", "Non-unit", &klen, &afac_ref(*kd 
			+ 1, k - klen), &i__1, &afac_ref(kc, k), &c__1);
	    }

/* L10: */
	}

/*     UPLO = 'L':  Compute the product L*L', overwriting L. */

    } else {
	for (k = *n; k >= 1; --k) {
/* Computing MIN */
	    i__1 = *kd, i__2 = *n - k;
	    klen = min(i__1,i__2);

/*           Add a multiple of column K of the factor L to each of   
             columns K+1 through N. */

	    if (klen > 0) {
		i__1 = *ldafac - 1;
		ssyr_("Lower", &klen, &c_b14, &afac_ref(2, k), &c__1, &
			afac_ref(1, k + 1), &i__1);
	    }

/*           Scale column K by the diagonal element. */

	    t = afac_ref(1, k);
	    i__1 = klen + 1;
	    sscal_(&i__1, &t, &afac_ref(1, k), &c__1);

/* L20: */
	}
    }

/*     Compute the difference  L*L' - A  or  U'*U - A. */

    if (lsame_(uplo, "U")) {
	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
/* Computing MAX */
	    i__2 = 1, i__3 = *kd + 2 - j;
	    mu = max(i__2,i__3);
	    i__2 = *kd + 1;
	    for (i__ = mu; i__ <= i__2; ++i__) {
		afac_ref(i__, j) = afac_ref(i__, j) - a_ref(i__, j);
/* L30: */
	    }
/* L40: */
	}
    } else {
	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
/* Computing MIN */
	    i__2 = *kd + 1, i__3 = *n - j + 1;
	    ml = min(i__2,i__3);
	    i__2 = ml;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		afac_ref(i__, j) = afac_ref(i__, j) - a_ref(i__, j);
/* L50: */
	    }
/* L60: */
	}
    }

/*     Compute norm( L*L' - A ) / ( N * norm(A) * EPS ) */

    *resid = slansb_("I", uplo, n, kd, &afac[afac_offset], ldafac, &rwork[1]);

    *resid = *resid / (real) (*n) / anorm / eps;

    return 0;

/*     End of SPBT01 */

} /* spbt01_ */

#undef afac_ref
#undef a_ref


