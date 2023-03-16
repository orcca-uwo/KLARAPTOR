#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static doublereal c_b17 = 1.;

/* Subroutine */ int zpbt01_(char *uplo, integer *n, integer *kd, 
	doublecomplex *a, integer *lda, doublecomplex *afac, integer *ldafac, 
	doublereal *rwork, doublereal *resid)
{
    /* System generated locals */
    integer a_dim1, a_offset, afac_dim1, afac_offset, i__1, i__2, i__3, i__4, 
	    i__5;
    doublecomplex z__1;

    /* Builtin functions */
    double d_imag(doublecomplex *);

    /* Local variables */
    static integer klen;
    extern /* Subroutine */ int zher_(char *, integer *, doublereal *, 
	    doublecomplex *, integer *, doublecomplex *, integer *);
    static integer i__, j, k;
    extern logical lsame_(char *, char *);
    static doublereal anorm;
    extern /* Double Complex */ VOID zdotc_(doublecomplex *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *);
    extern /* Subroutine */ int ztrmv_(char *, char *, char *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *);
    static integer kc;
    extern doublereal dlamch_(char *);
    static integer ml, mu;
    extern doublereal zlanhb_(char *, char *, integer *, integer *, 
	    doublecomplex *, integer *, doublereal *);
    extern /* Subroutine */ int zdscal_(integer *, doublereal *, 
	    doublecomplex *, integer *);
    static doublereal akk, eps;


#define a_subscr(a_1,a_2) (a_2)*a_dim1 + a_1
#define a_ref(a_1,a_2) a[a_subscr(a_1,a_2)]
#define afac_subscr(a_1,a_2) (a_2)*afac_dim1 + a_1
#define afac_ref(a_1,a_2) afac[afac_subscr(a_1,a_2)]


/*  -- LAPACK test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    ZPBT01 reconstructs a Hermitian positive definite band matrix A from   
    its L*L' or U'*U factorization and computes the residual   
       norm( L*L' - A ) / ( N * norm(A) * EPS ) or   
       norm( U'*U - A ) / ( N * norm(A) * EPS ),   
    where EPS is the machine epsilon, L' is the conjugate transpose of   
    L, and U' is the conjugate transpose of U.   

    Arguments   
    =========   

    UPLO    (input) CHARACTER*1   
            Specifies whether the upper or lower triangular part of the   
            Hermitian matrix A is stored:   
            = 'U':  Upper triangular   
            = 'L':  Lower triangular   

    N       (input) INTEGER   
            The number of rows and columns of the matrix A.  N >= 0.   

    KD      (input) INTEGER   
            The number of super-diagonals of the matrix A if UPLO = 'U',   
            or the number of sub-diagonals if UPLO = 'L'.  KD >= 0.   

    A       (input) COMPLEX*16 array, dimension (LDA,N)   
            The original Hermitian band matrix A.  If UPLO = 'U', the   
            upper triangular part of A is stored as a band matrix; if   
            UPLO = 'L', the lower triangular part of A is stored.  The   
            columns of the appropriate triangle are stored in the columns   
            of A and the diagonals of the triangle are stored in the rows   
            of A.  See ZPBTRF for further details.   

    LDA     (input) INTEGER.   
            The leading dimension of the array A.  LDA >= max(1,KD+1).   

    AFAC    (input) COMPLEX*16 array, dimension (LDAFAC,N)   
            The factored form of the matrix A.  AFAC contains the factor   
            L or U from the L*L' or U'*U factorization in band storage   
            format, as computed by ZPBTRF.   

    LDAFAC  (input) INTEGER   
            The leading dimension of the array AFAC.   
            LDAFAC >= max(1,KD+1).   

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
    anorm = zlanhb_("1", uplo, n, kd, &a[a_offset], lda, &rwork[1]);
    if (anorm <= 0.) {
	*resid = 1. / eps;
	return 0;
    }

/*     Check the imaginary parts of the diagonal elements and return with   
       an error code if any are nonzero. */

    if (lsame_(uplo, "U")) {
	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
	    if (d_imag(&afac_ref(*kd + 1, j)) != 0.) {
		*resid = 1. / eps;
		return 0;
	    }
/* L10: */
	}
    } else {
	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
	    if (d_imag(&afac_ref(1, j)) != 0.) {
		*resid = 1. / eps;
		return 0;
	    }
/* L20: */
	}
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
	    zdotc_(&z__1, &i__1, &afac_ref(kc, k), &c__1, &afac_ref(kc, k), &
		    c__1);
	    akk = z__1.r;
	    i__1 = afac_subscr(*kd + 1, k);
	    afac[i__1].r = akk, afac[i__1].i = 0.;

/*           Compute the rest of column K. */

	    if (klen > 0) {
		i__1 = *ldafac - 1;
		ztrmv_("Upper", "Conjugate", "Non-unit", &klen, &afac_ref(*kd 
			+ 1, k - klen), &i__1, &afac_ref(kc, k), &c__1);
	    }

/* L30: */
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
		zher_("Lower", &klen, &c_b17, &afac_ref(2, k), &c__1, &
			afac_ref(1, k + 1), &i__1);
	    }

/*           Scale column K by the diagonal element. */

	    i__1 = afac_subscr(1, k);
	    akk = afac[i__1].r;
	    i__1 = klen + 1;
	    zdscal_(&i__1, &akk, &afac_ref(1, k), &c__1);

/* L40: */
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
		i__3 = afac_subscr(i__, j);
		i__4 = afac_subscr(i__, j);
		i__5 = a_subscr(i__, j);
		z__1.r = afac[i__4].r - a[i__5].r, z__1.i = afac[i__4].i - a[
			i__5].i;
		afac[i__3].r = z__1.r, afac[i__3].i = z__1.i;
/* L50: */
	    }
/* L60: */
	}
    } else {
	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
/* Computing MIN */
	    i__2 = *kd + 1, i__3 = *n - j + 1;
	    ml = min(i__2,i__3);
	    i__2 = ml;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		i__3 = afac_subscr(i__, j);
		i__4 = afac_subscr(i__, j);
		i__5 = a_subscr(i__, j);
		z__1.r = afac[i__4].r - a[i__5].r, z__1.i = afac[i__4].i - a[
			i__5].i;
		afac[i__3].r = z__1.r, afac[i__3].i = z__1.i;
/* L70: */
	    }
/* L80: */
	}
    }

/*     Compute norm( L*L' - A ) / ( N * norm(A) * EPS ) */

    *resid = zlanhb_("1", uplo, n, kd, &afac[afac_offset], ldafac, &rwork[1]);

    *resid = *resid / (doublereal) (*n) / anorm / eps;

    return 0;

/*     End of ZPBT01 */

} /* zpbt01_ */

#undef afac_ref
#undef afac_subscr
#undef a_ref
#undef a_subscr


