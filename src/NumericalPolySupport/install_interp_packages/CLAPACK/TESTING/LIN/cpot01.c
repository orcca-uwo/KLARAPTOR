#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static real c_b15 = 1.f;

/* Subroutine */ int cpot01_(char *uplo, integer *n, complex *a, integer *lda,
	 complex *afac, integer *ldafac, real *rwork, real *resid)
{
    /* System generated locals */
    integer a_dim1, a_offset, afac_dim1, afac_offset, i__1, i__2, i__3, i__4, 
	    i__5;
    real r__1;
    complex q__1;

    /* Builtin functions */
    double r_imag(complex *);

    /* Local variables */
    extern /* Subroutine */ int cher_(char *, integer *, real *, complex *, 
	    integer *, complex *, integer *);
    static integer i__, j, k;
    extern /* Subroutine */ int cscal_(integer *, complex *, complex *, 
	    integer *);
    extern /* Complex */ VOID cdotc_(complex *, integer *, complex *, integer 
	    *, complex *, integer *);
    extern logical lsame_(char *, char *);
    static real anorm;
    extern /* Subroutine */ int ctrmv_(char *, char *, char *, integer *, 
	    complex *, integer *, complex *, integer *);
    static complex tc;
    extern doublereal clanhe_(char *, char *, integer *, complex *, integer *,
	     real *);
    static real tr;
    extern doublereal slamch_(char *);
    static real eps;


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

    CPOT01 reconstructs a Hermitian positive definite matrix  A  from   
    its L*L' or U'*U factorization and computes the residual   
       norm( L*L' - A ) / ( N * norm(A) * EPS ) or   
       norm( U'*U - A ) / ( N * norm(A) * EPS ),   
    where EPS is the machine epsilon, L' is the conjugate transpose of L,   
    and U' is the conjugate transpose of U.   

    Arguments   
    ==========   

    UPLO    (input) CHARACTER*1   
            Specifies whether the upper or lower triangular part of the   
            Hermitian matrix A is stored:   
            = 'U':  Upper triangular   
            = 'L':  Lower triangular   

    N       (input) INTEGER   
            The number of rows and columns of the matrix A.  N >= 0.   

    A       (input) COMPLEX array, dimension (LDA,N)   
            The original Hermitian matrix A.   

    LDA     (input) INTEGER   
            The leading dimension of the array A.  LDA >= max(1,N)   

    AFAC    (input/output) COMPLEX array, dimension (LDAFAC,N)   
            On entry, the factor L or U from the L*L' or U'*U   
            factorization of A.   
            Overwritten with the reconstructed matrix, and then with the   
            difference L*L' - A (or U'*U - A).   

    LDAFAC  (input) INTEGER   
            The leading dimension of the array AFAC.  LDAFAC >= max(1,N).   

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
    anorm = clanhe_("1", uplo, n, &a[a_offset], lda, &rwork[1]);
    if (anorm <= 0.f) {
	*resid = 1.f / eps;
	return 0;
    }

/*     Check the imaginary parts of the diagonal elements and return with   
       an error code if any are nonzero. */

    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	if (r_imag(&afac_ref(j, j)) != 0.f) {
	    *resid = 1.f / eps;
	    return 0;
	}
/* L10: */
    }

/*     Compute the product U'*U, overwriting U. */

    if (lsame_(uplo, "U")) {
	for (k = *n; k >= 1; --k) {

/*           Compute the (K,K) element of the result. */

	    cdotc_(&q__1, &k, &afac_ref(1, k), &c__1, &afac_ref(1, k), &c__1);
	    tr = q__1.r;
	    i__1 = afac_subscr(k, k);
	    afac[i__1].r = tr, afac[i__1].i = 0.f;

/*           Compute the rest of column K. */

	    i__1 = k - 1;
	    ctrmv_("Upper", "Conjugate", "Non-unit", &i__1, &afac[afac_offset]
		    , ldafac, &afac_ref(1, k), &c__1);

/* L20: */
	}

/*     Compute the product L*L', overwriting L. */

    } else {
	for (k = *n; k >= 1; --k) {

/*           Add a multiple of column K of the factor L to each of   
             columns K+1 through N. */

	    if (k + 1 <= *n) {
		i__1 = *n - k;
		cher_("Lower", &i__1, &c_b15, &afac_ref(k + 1, k), &c__1, &
			afac_ref(k + 1, k + 1), ldafac);
	    }

/*           Scale column K by the diagonal element. */

	    i__1 = afac_subscr(k, k);
	    tc.r = afac[i__1].r, tc.i = afac[i__1].i;
	    i__1 = *n - k + 1;
	    cscal_(&i__1, &tc, &afac_ref(k, k), &c__1);

/* L30: */
	}
    }

/*     Compute the difference  L*L' - A (or U'*U - A). */

    if (lsame_(uplo, "U")) {
	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
	    i__2 = j - 1;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		i__3 = afac_subscr(i__, j);
		i__4 = afac_subscr(i__, j);
		i__5 = a_subscr(i__, j);
		q__1.r = afac[i__4].r - a[i__5].r, q__1.i = afac[i__4].i - a[
			i__5].i;
		afac[i__3].r = q__1.r, afac[i__3].i = q__1.i;
/* L40: */
	    }
	    i__2 = afac_subscr(j, j);
	    i__3 = afac_subscr(j, j);
	    i__4 = a_subscr(j, j);
	    r__1 = a[i__4].r;
	    q__1.r = afac[i__3].r - r__1, q__1.i = afac[i__3].i;
	    afac[i__2].r = q__1.r, afac[i__2].i = q__1.i;
/* L50: */
	}
    } else {
	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
	    i__2 = afac_subscr(j, j);
	    i__3 = afac_subscr(j, j);
	    i__4 = a_subscr(j, j);
	    r__1 = a[i__4].r;
	    q__1.r = afac[i__3].r - r__1, q__1.i = afac[i__3].i;
	    afac[i__2].r = q__1.r, afac[i__2].i = q__1.i;
	    i__2 = *n;
	    for (i__ = j + 1; i__ <= i__2; ++i__) {
		i__3 = afac_subscr(i__, j);
		i__4 = afac_subscr(i__, j);
		i__5 = a_subscr(i__, j);
		q__1.r = afac[i__4].r - a[i__5].r, q__1.i = afac[i__4].i - a[
			i__5].i;
		afac[i__3].r = q__1.r, afac[i__3].i = q__1.i;
/* L60: */
	    }
/* L70: */
	}
    }

/*     Compute norm( L*U - A ) / ( N * norm(A) * EPS ) */

    *resid = clanhe_("1", uplo, n, &afac[afac_offset], ldafac, &rwork[1]);

    *resid = *resid / (real) (*n) / anorm / eps;

    return 0;

/*     End of CPOT01 */

} /* cpot01_ */

#undef afac_ref
#undef afac_subscr
#undef a_ref
#undef a_subscr


