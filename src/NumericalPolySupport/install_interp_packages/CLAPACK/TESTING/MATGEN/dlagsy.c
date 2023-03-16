#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__1 = 1;
static doublereal c_b12 = 0.;
static doublereal c_b19 = -1.;
static doublereal c_b26 = 1.;

/* Subroutine */ int dlagsy_(integer *n, integer *k, doublereal *d__, 
	doublereal *a, integer *lda, integer *iseed, doublereal *work, 
	integer *info)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    doublereal d__1;

    /* Builtin functions */
    double d_sign(doublereal *, doublereal *);

    /* Local variables */
    extern /* Subroutine */ int dger_(integer *, integer *, doublereal *, 
	    doublereal *, integer *, doublereal *, integer *, doublereal *, 
	    integer *);
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *, 
	    integer *), dnrm2_(integer *, doublereal *, integer *);
    extern /* Subroutine */ int dsyr2_(char *, integer *, doublereal *, 
	    doublereal *, integer *, doublereal *, integer *, doublereal *, 
	    integer *);
    static integer i__, j;
    static doublereal alpha;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *, 
	    integer *), dgemv_(char *, integer *, integer *, doublereal *, 
	    doublereal *, integer *, doublereal *, integer *, doublereal *, 
	    doublereal *, integer *), daxpy_(integer *, doublereal *, 
	    doublereal *, integer *, doublereal *, integer *), dsymv_(char *, 
	    integer *, doublereal *, doublereal *, integer *, doublereal *, 
	    integer *, doublereal *, doublereal *, integer *);
    static doublereal wa, wb, wn;
    extern /* Subroutine */ int xerbla_(char *, integer *), dlarnv_(
	    integer *, integer *, integer *, doublereal *);
    static doublereal tau;


#define a_ref(a_1,a_2) a[(a_2)*a_dim1 + a_1]


/*  -- LAPACK auxiliary test routine (version 3.0)   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 29, 1992   


    Purpose   
    =======   

    DLAGSY generates a real symmetric matrix A, by pre- and post-   
    multiplying a real diagonal matrix D with a random orthogonal matrix:   
    A = U*D*U'. The semi-bandwidth may then be reduced to k by additional   
    orthogonal transformations.   

    Arguments   
    =========   

    N       (input) INTEGER   
            The order of the matrix A.  N >= 0.   

    K       (input) INTEGER   
            The number of nonzero subdiagonals within the band of A.   
            0 <= K <= N-1.   

    D       (input) DOUBLE PRECISION array, dimension (N)   
            The diagonal elements of the diagonal matrix D.   

    A       (output) DOUBLE PRECISION array, dimension (LDA,N)   
            The generated n by n symmetric matrix A (the full matrix is   
            stored).   

    LDA     (input) INTEGER   
            The leading dimension of the array A.  LDA >= N.   

    ISEED   (input/output) INTEGER array, dimension (4)   
            On entry, the seed of the random number generator; the array   
            elements must be between 0 and 4095, and ISEED(4) must be   
            odd.   
            On exit, the seed is updated.   

    WORK    (workspace) DOUBLE PRECISION array, dimension (2*N)   

    INFO    (output) INTEGER   
            = 0: successful exit   
            < 0: if INFO = -i, the i-th argument had an illegal value   

    =====================================================================   


       Test the input arguments   

       Parameter adjustments */
    --d__;
    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    --iseed;
    --work;

    /* Function Body */
    *info = 0;
    if (*n < 0) {
	*info = -1;
    } else if (*k < 0 || *k > *n - 1) {
	*info = -2;
    } else if (*lda < max(1,*n)) {
	*info = -5;
    }
    if (*info < 0) {
	i__1 = -(*info);
	xerbla_("DLAGSY", &i__1);
	return 0;
    }

/*     initialize lower triangle of A to diagonal matrix */

    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	i__2 = *n;
	for (i__ = j + 1; i__ <= i__2; ++i__) {
	    a_ref(i__, j) = 0.;
/* L10: */
	}
/* L20: */
    }
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	a_ref(i__, i__) = d__[i__];
/* L30: */
    }

/*     Generate lower triangle of symmetric matrix */

    for (i__ = *n - 1; i__ >= 1; --i__) {

/*        generate random reflection */

	i__1 = *n - i__ + 1;
	dlarnv_(&c__3, &iseed[1], &i__1, &work[1]);
	i__1 = *n - i__ + 1;
	wn = dnrm2_(&i__1, &work[1], &c__1);
	wa = d_sign(&wn, &work[1]);
	if (wn == 0.) {
	    tau = 0.;
	} else {
	    wb = work[1] + wa;
	    i__1 = *n - i__;
	    d__1 = 1. / wb;
	    dscal_(&i__1, &d__1, &work[2], &c__1);
	    work[1] = 1.;
	    tau = wb / wa;
	}

/*        apply random reflection to A(i:n,i:n) from the left   
          and the right   

          compute  y := tau * A * u */

	i__1 = *n - i__ + 1;
	dsymv_("Lower", &i__1, &tau, &a_ref(i__, i__), lda, &work[1], &c__1, &
		c_b12, &work[*n + 1], &c__1);

/*        compute  v := y - 1/2 * tau * ( y, u ) * u */

	i__1 = *n - i__ + 1;
	alpha = tau * -.5 * ddot_(&i__1, &work[*n + 1], &c__1, &work[1], &
		c__1);
	i__1 = *n - i__ + 1;
	daxpy_(&i__1, &alpha, &work[1], &c__1, &work[*n + 1], &c__1);

/*        apply the transformation as a rank-2 update to A(i:n,i:n) */

	i__1 = *n - i__ + 1;
	dsyr2_("Lower", &i__1, &c_b19, &work[1], &c__1, &work[*n + 1], &c__1, 
		&a_ref(i__, i__), lda);
/* L40: */
    }

/*     Reduce number of subdiagonals to K */

    i__1 = *n - 1 - *k;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        generate reflection to annihilate A(k+i+1:n,i) */

	i__2 = *n - *k - i__ + 1;
	wn = dnrm2_(&i__2, &a_ref(*k + i__, i__), &c__1);
	wa = d_sign(&wn, &a_ref(*k + i__, i__));
	if (wn == 0.) {
	    tau = 0.;
	} else {
	    wb = a_ref(*k + i__, i__) + wa;
	    i__2 = *n - *k - i__;
	    d__1 = 1. / wb;
	    dscal_(&i__2, &d__1, &a_ref(*k + i__ + 1, i__), &c__1);
	    a_ref(*k + i__, i__) = 1.;
	    tau = wb / wa;
	}

/*        apply reflection to A(k+i:n,i+1:k+i-1) from the left */

	i__2 = *n - *k - i__ + 1;
	i__3 = *k - 1;
	dgemv_("Transpose", &i__2, &i__3, &c_b26, &a_ref(*k + i__, i__ + 1), 
		lda, &a_ref(*k + i__, i__), &c__1, &c_b12, &work[1], &c__1);
	i__2 = *n - *k - i__ + 1;
	i__3 = *k - 1;
	d__1 = -tau;
	dger_(&i__2, &i__3, &d__1, &a_ref(*k + i__, i__), &c__1, &work[1], &
		c__1, &a_ref(*k + i__, i__ + 1), lda);

/*        apply reflection to A(k+i:n,k+i:n) from the left and the right   

          compute  y := tau * A * u */

	i__2 = *n - *k - i__ + 1;
	dsymv_("Lower", &i__2, &tau, &a_ref(*k + i__, *k + i__), lda, &a_ref(*
		k + i__, i__), &c__1, &c_b12, &work[1], &c__1);

/*        compute  v := y - 1/2 * tau * ( y, u ) * u */

	i__2 = *n - *k - i__ + 1;
	alpha = tau * -.5 * ddot_(&i__2, &work[1], &c__1, &a_ref(*k + i__, 
		i__), &c__1);
	i__2 = *n - *k - i__ + 1;
	daxpy_(&i__2, &alpha, &a_ref(*k + i__, i__), &c__1, &work[1], &c__1);

/*        apply symmetric rank-2 update to A(k+i:n,k+i:n) */

	i__2 = *n - *k - i__ + 1;
	dsyr2_("Lower", &i__2, &c_b19, &a_ref(*k + i__, i__), &c__1, &work[1],
		 &c__1, &a_ref(*k + i__, *k + i__), lda);

	a_ref(*k + i__, i__) = -wa;
	i__2 = *n;
	for (j = *k + i__ + 1; j <= i__2; ++j) {
	    a_ref(j, i__) = 0.;
/* L50: */
	}
/* L60: */
    }

/*     Store full symmetric matrix */

    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	i__2 = *n;
	for (i__ = j + 1; i__ <= i__2; ++i__) {
	    a_ref(j, i__) = a_ref(i__, j);
/* L70: */
	}
/* L80: */
    }
    return 0;

/*     End of DLAGSY */

} /* dlagsy_ */

#undef a_ref


