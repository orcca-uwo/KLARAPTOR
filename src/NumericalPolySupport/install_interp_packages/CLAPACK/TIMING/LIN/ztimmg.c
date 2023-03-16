#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__1 = 1;
static integer c__5 = 5;

/* Subroutine */ int ztimmg_(integer *iflag, integer *m, integer *n, 
	doublecomplex *a, integer *lda, integer *kl, integer *ku)
{
    /* Initialized data */

    static integer iseed[4] = { 0,0,0,1 };

    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3, i__4;
    doublereal d__1;
    doublecomplex z__1, z__2;

    /* Local variables */
    static integer i__, j, k;
    extern /* Subroutine */ int zcopy_(integer *, doublecomplex *, integer *, 
	    doublecomplex *, integer *);
    static integer jj, jn, mj, mu;
    extern /* Double Complex */ VOID zlarnd_(doublecomplex *, integer *, 
	    integer *);
    extern /* Subroutine */ int zlarnv_(integer *, integer *, integer *, 
	    doublecomplex *);


#define a_subscr(a_1,a_2) (a_2)*a_dim1 + a_1
#define a_ref(a_1,a_2) a[a_subscr(a_1,a_2)]


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    ZTIMMG generates a complex test matrix whose type is given by IFLAG.   
    All the matrices are Toeplitz (constant along a diagonal), with   
    random elements on each diagonal.   

    Arguments   
    =========   

    IFLAG   (input) INTEGER   
            The type of matrix to be generated.   
            = 0 or 1:   General matrix   
            = 2 or -2:  General banded matrix   
            = 3 or -3:  Hermitian positive definite matrix   
            = 4 or -4:  Hermitian positive definite packed   
            = 5 or -5:  Hermitian positive definite banded   
            = 6 or -6:  Hermitian indefinite matrix   
            = 7 or -7:  Hermitian indefinite packed   
            = 8 or -8:  Symmetric indefinite matrix   
            = 9 or -9:  Symmetric indefinite packed   
            = 10 or -10:  Symmetric indefinite banded   
            = 11 or -11:  Triangular matrix   
            = 12 or -12:  Triangular packed   
            = 13 or -13:  Triangular banded   
            = 14:         General tridiagonal   
            For Hermitian, symmetric, or triangular matrices, IFLAG > 0   
            indicates upper triangular storage and IFLAG < 0 indicates   
            lower triangular storage.   

    M       (input) INTEGER   
            The number of rows of the matrix to be generated.   

    N       (input) INTEGER   
            The number of columns of the matrix to be generated.   

    A       (output) COMPLEX*16 array, dimension (LDA,N)   
            The generated matrix.   

            If the absolute value of IFLAG is 1, 3, 6, or 8, the leading   
            M x N (or N x N) subblock is used to store the matrix.   
            If the matrix is symmetric, only the upper or lower triangle   
            of this block is referenced.   

            If the absolute value of IFLAG is 4, 7, or 9, the matrix is   
            Hermitian or symmetric and packed storage is used for the   
            upper or lower triangle.  The triangular matrix is stored   
            columnwise as a linear array, and the array A is treated as a   
            vector of length LDA.  LDA must be set to at least N*(N+1)/2.   

            If the absolute value of IFLAG is 2 or 5, the matrix is   
            returned in band format.  The columns of the matrix are   
            specified in the columns of A and the diagonals of the   
            matrix are specified in the rows of A, with the leading   
            diagonal in row   
                KL + KU + 1,  if IFLAG = 2   
                KU + 1,       if IFLAG = 5 or -2   
                1,            if IFLAG = -5   
            If IFLAG = 2, the first KL rows are not used to leave room   
            for pivoting in ZGBTRF.   

    LDA     (input) INTEGER   
            The leading dimension of A.  If the generated matrix is   
            packed, LDA >= N*(N+1)/2, otherwise LDA >= max(1,M).   

    KL      (input) INTEGER   
            The number of subdiagonals if IFLAG = 2, 5, or -5.   

    KU      (input) INTEGER   
            The number of superdiagonals if IFLAG = 2, 5, or -5.   

    =====================================================================   

       Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;

    /* Function Body */

    if (*m <= 0 || *n <= 0) {
	return 0;

    } else if (*iflag == 0 || *iflag == 1) {

/*        General matrix   

          Set first column and row to random values. */

	zlarnv_(&c__2, iseed, m, &a_ref(1, 1));
	i__1 = *n;
	i__2 = *m;
	for (j = 2; i__2 < 0 ? j >= i__1 : j <= i__1; j += i__2) {
/* Computing MIN */
	    i__3 = *m, i__4 = *n - j + 1;
	    mj = min(i__3,i__4);
	    zlarnv_(&c__2, iseed, &mj, &a_ref(1, j));
	    if (mj > 1) {
		i__3 = mj - 1;
		zcopy_(&i__3, &a_ref(2, j), &c__1, &a_ref(1, j + 1), lda);
	    }
/* L10: */
	}

/*        Fill in the rest of the matrix. */

	i__2 = *n;
	for (j = 2; j <= i__2; ++j) {
	    i__1 = *m;
	    for (i__ = 2; i__ <= i__1; ++i__) {
		i__3 = a_subscr(i__, j);
		i__4 = a_subscr(i__ - 1, j - 1);
		a[i__3].r = a[i__4].r, a[i__3].i = a[i__4].i;
/* L20: */
	    }
/* L30: */
	}

    } else if (*iflag == 2 || *iflag == -2) {

/*        General band matrix */

	if (*iflag == 2) {
	    k = *kl + *ku + 1;
	} else {
	    k = *ku + 1;
	}
/* Computing MIN */
	i__1 = *m, i__3 = *kl + 1;
	i__2 = min(i__1,i__3);
	zlarnv_(&c__2, iseed, &i__2, &a_ref(k, 1));
/* Computing MIN */
	i__2 = *n - 1;
	mu = min(i__2,*ku);
	i__2 = mu + 1;
	zlarnv_(&c__2, iseed, &i__2, &a_ref(k - mu, *n));
	i__2 = *n - 1;
	for (j = 2; j <= i__2; ++j) {
/* Computing MIN */
	    i__1 = j - 1;
	    mu = min(i__1,*ku);
	    zcopy_(&mu, &a_ref(k - mu, *n), &c__1, &a_ref(k - mu, j), &c__1);
/* Computing MIN */
	    i__3 = *m - j + 1, i__4 = *kl + 1;
	    i__1 = min(i__3,i__4);
	    zcopy_(&i__1, &a_ref(k, 1), &c__1, &a_ref(k, j), &c__1);
/* L40: */
	}

    } else if (*iflag == 3) {

/*        Hermitian positive definite, upper triangle */

	i__2 = *n - 1;
	zlarnv_(&c__2, iseed, &i__2, &a_ref(1, *n));
	i__2 = a_subscr(*n, *n);
	d__1 = (doublereal) (*n);
	a[i__2].r = d__1, a[i__2].i = 0.;
	for (j = *n - 1; j >= 1; --j) {
	    zcopy_(&j, &a_ref(*n - j + 1, *n), &c__1, &a_ref(1, j), &c__1);
/* L50: */
	}

    } else if (*iflag == -3) {

/*        Hermitian positive definite, lower triangle */

	i__2 = a_subscr(1, 1);
	d__1 = (doublereal) (*n);
	a[i__2].r = d__1, a[i__2].i = 0.;
	if (*n > 1) {
	    i__2 = *n - 1;
	    zlarnv_(&c__2, iseed, &i__2, &a_ref(2, 1));
	}
	i__2 = *n;
	for (j = 2; j <= i__2; ++j) {
	    i__1 = *n - j + 1;
	    zcopy_(&i__1, &a_ref(1, 1), &c__1, &a_ref(j, j), &c__1);
/* L60: */
	}

    } else if (*iflag == 4) {

/*        Hermitian positive definite packed, upper triangle */

	jn = (*n - 1) * *n / 2 + 1;
	i__2 = *n - 1;
	zlarnv_(&c__2, iseed, &i__2, &a_ref(jn, 1));
	i__2 = a_subscr(jn + *n - 1, 1);
	d__1 = (doublereal) (*n);
	a[i__2].r = d__1, a[i__2].i = 0.;
	jj = jn;
	for (j = *n - 1; j >= 1; --j) {
	    jj -= j;
	    ++jn;
	    zcopy_(&j, &a_ref(jn, 1), &c__1, &a_ref(jj, 1), &c__1);
/* L70: */
	}

    } else if (*iflag == -4) {

/*        Hermitian positive definite packed, lower triangle */

	i__2 = a_subscr(1, 1);
	d__1 = (doublereal) (*n);
	a[i__2].r = d__1, a[i__2].i = 0.;
	if (*n > 1) {
	    i__2 = *n - 1;
	    zlarnv_(&c__2, iseed, &i__2, &a_ref(2, 1));
	}
	jj = *n + 1;
	i__2 = *n;
	for (j = 2; j <= i__2; ++j) {
	    i__1 = *n - j + 1;
	    zcopy_(&i__1, &a_ref(1, 1), &c__1, &a_ref(jj, 1), &c__1);
	    jj = jj + *n - j + 1;
/* L80: */
	}

    } else if (*iflag == 5) {

/*        Hermitian positive definite banded, upper triangle */

	k = *kl;
/* Computing MIN */
	i__2 = *n - 1;
	mu = min(i__2,k);
	zlarnv_(&c__2, iseed, &mu, &a_ref(k + 1 - mu, *n));
	i__2 = a_subscr(k + 1, *n);
	d__1 = (doublereal) (*n);
	a[i__2].r = d__1, a[i__2].i = 0.;
	for (j = *n - 1; j >= 1; --j) {
/* Computing MIN */
	    i__2 = j, i__1 = k + 1;
	    mu = min(i__2,i__1);
	    zcopy_(&mu, &a_ref(k + 2 - mu, *n), &c__1, &a_ref(k + 2 - mu, j), 
		    &c__1);
/* L90: */
	}

    } else if (*iflag == -5) {

/*        Hermitian positive definite banded, lower triangle */

	k = *kl;
	i__2 = a_subscr(1, 1);
	d__1 = (doublereal) (*n);
	a[i__2].r = d__1, a[i__2].i = 0.;
/* Computing MIN */
	i__1 = *n - 1;
	i__2 = min(i__1,k);
	zlarnv_(&c__2, iseed, &i__2, &a_ref(2, 1));
	i__2 = *n;
	for (j = 2; j <= i__2; ++j) {
/* Computing MIN */
	    i__3 = *n - j + 1, i__4 = k + 1;
	    i__1 = min(i__3,i__4);
	    zcopy_(&i__1, &a_ref(1, 1), &c__1, &a_ref(1, j), &c__1);
/* L100: */
	}

    } else if (*iflag == 6) {

/*        Hermitian indefinite, upper triangle */

	zlarnv_(&c__2, iseed, n, &a_ref(1, *n));
	i__2 = a_subscr(*n, *n);
	i__1 = a_subscr(*n, *n);
	d__1 = a[i__1].r;
	a[i__2].r = d__1, a[i__2].i = 0.;
	for (j = *n - 1; j >= 1; --j) {
	    zcopy_(&j, &a_ref(*n - j + 1, *n), &c__1, &a_ref(1, j), &c__1);
/* L110: */
	}

    } else if (*iflag == -6) {

/*        Hermitian indefinite, lower triangle */

	zlarnv_(&c__2, iseed, n, &a_ref(1, 1));
	i__2 = a_subscr(1, 1);
	i__1 = a_subscr(1, 1);
	d__1 = a[i__1].r;
	a[i__2].r = d__1, a[i__2].i = 0.;
	i__2 = *n;
	for (j = 2; j <= i__2; ++j) {
	    i__1 = *n - j + 1;
	    zcopy_(&i__1, &a_ref(1, 1), &c__1, &a_ref(j, j), &c__1);
/* L120: */
	}

    } else if (*iflag == 7) {

/*        Hermitian indefinite packed, upper triangle */

	jn = (*n - 1) * *n / 2 + 1;
	zlarnv_(&c__2, iseed, n, &a_ref(jn, 1));
	i__2 = a_subscr(jn + *n - 1, 1);
	i__1 = a_subscr(jn + *n - 1, 1);
	d__1 = a[i__1].r;
	a[i__2].r = d__1, a[i__2].i = 0.;
	jj = jn;
	for (j = *n - 1; j >= 1; --j) {
	    jj -= j;
	    ++jn;
	    zcopy_(&j, &a_ref(jn, 1), &c__1, &a_ref(jj, 1), &c__1);
/* L130: */
	}

    } else if (*iflag == -7) {

/*        Hermitian indefinite packed, lower triangle */

	zlarnv_(&c__2, iseed, n, &a_ref(1, 1));
	i__2 = a_subscr(1, 1);
	i__1 = a_subscr(1, 1);
	d__1 = a[i__1].r;
	a[i__2].r = d__1, a[i__2].i = 0.;
	jj = *n + 1;
	i__2 = *n;
	for (j = 2; j <= i__2; ++j) {
	    i__1 = *n - j + 1;
	    zcopy_(&i__1, &a_ref(1, 1), &c__1, &a_ref(jj, 1), &c__1);
	    jj = jj + *n - j + 1;
/* L140: */
	}

    } else if (*iflag == 8) {

/*        Symmetric indefinite, upper triangle */

	zlarnv_(&c__2, iseed, n, &a_ref(1, *n));
	for (j = *n - 1; j >= 1; --j) {
	    zcopy_(&j, &a_ref(*n - j + 1, *n), &c__1, &a_ref(1, j), &c__1);
/* L150: */
	}

    } else if (*iflag == -8) {

/*        Symmetric indefinite, lower triangle */

	zlarnv_(&c__2, iseed, n, &a_ref(1, 1));
	i__2 = *n;
	for (j = 2; j <= i__2; ++j) {
	    i__1 = *n - j + 1;
	    zcopy_(&i__1, &a_ref(1, 1), &c__1, &a_ref(j, j), &c__1);
/* L160: */
	}

    } else if (*iflag == 9) {

/*        Symmetric indefinite packed, upper triangle */

	jn = (*n - 1) * *n / 2 + 1;
	zlarnv_(&c__2, iseed, n, &a_ref(jn, 1));
	jj = jn;
	for (j = *n - 1; j >= 1; --j) {
	    jj -= j;
	    ++jn;
	    zcopy_(&j, &a_ref(jn, 1), &c__1, &a_ref(jj, 1), &c__1);
/* L170: */
	}

    } else if (*iflag == -9) {

/*        Symmetric indefinite packed, lower triangle */

	zlarnv_(&c__2, iseed, n, &a_ref(1, 1));
	jj = *n + 1;
	i__2 = *n;
	for (j = 2; j <= i__2; ++j) {
	    i__1 = *n - j + 1;
	    zcopy_(&i__1, &a_ref(1, 1), &c__1, &a_ref(jj, 1), &c__1);
	    jj = jj + *n - j + 1;
/* L180: */
	}

    } else if (*iflag == 10) {

/*        Symmetric indefinite banded, upper triangle */

	k = *kl;
/* Computing MIN */
	i__2 = *n, i__1 = k + 1;
	mu = min(i__2,i__1);
	zlarnv_(&c__2, iseed, &mu, &a_ref(k + 2 - mu, *n));
	for (j = *n - 1; j >= 1; --j) {
/* Computing MIN */
	    i__2 = j, i__1 = k + 1;
	    mu = min(i__2,i__1);
	    zcopy_(&mu, &a_ref(k + 2 - mu, *n), &c__1, &a_ref(k + 2 - mu, j), 
		    &c__1);
/* L190: */
	}

    } else if (*iflag == -10) {

/*        Symmetric indefinite banded, lower triangle */

	k = *kl;
/* Computing MIN */
	i__1 = *n, i__3 = k + 1;
	i__2 = min(i__1,i__3);
	zlarnv_(&c__2, iseed, &i__2, &a_ref(1, 1));
	i__2 = *n;
	for (j = 2; j <= i__2; ++j) {
/* Computing MIN */
	    i__3 = *n - j + 1, i__4 = k + 1;
	    i__1 = min(i__3,i__4);
	    zcopy_(&i__1, &a_ref(1, 1), &c__1, &a_ref(1, j), &c__1);
/* L200: */
	}

    } else if (*iflag == 11) {

/*        Upper triangular */

	i__2 = *n - 1;
	zlarnv_(&c__2, iseed, &i__2, &a_ref(1, *n));
	i__2 = a_subscr(*n, *n);
	d__1 = (doublereal) (*n);
	zlarnd_(&z__2, &c__5, iseed);
	z__1.r = d__1 * z__2.r, z__1.i = d__1 * z__2.i;
	a[i__2].r = z__1.r, a[i__2].i = z__1.i;
	for (j = *n - 1; j >= 1; --j) {
	    zcopy_(&j, &a_ref(*n - j + 1, *n), &c__1, &a_ref(1, j), &c__1);
/* L210: */
	}

    } else if (*iflag == -11) {

/*        Lower triangular */

	i__2 = a_subscr(1, 1);
	d__1 = (doublereal) (*n);
	zlarnd_(&z__2, &c__5, iseed);
	z__1.r = d__1 * z__2.r, z__1.i = d__1 * z__2.i;
	a[i__2].r = z__1.r, a[i__2].i = z__1.i;
	if (*n > 1) {
	    i__2 = *n - 1;
	    zlarnv_(&c__2, iseed, &i__2, &a_ref(2, 1));
	}
	i__2 = *n;
	for (j = 2; j <= i__2; ++j) {
	    i__1 = *n - j + 1;
	    zcopy_(&i__1, &a_ref(1, 1), &c__1, &a_ref(j, j), &c__1);
/* L220: */
	}

    } else if (*iflag == 12) {

/*        Upper triangular packed */

	jn = (*n - 1) * *n / 2 + 1;
	i__2 = *n - 1;
	zlarnv_(&c__2, iseed, &i__2, &a_ref(jn, 1));
	i__2 = a_subscr(jn + *n - 1, 1);
	d__1 = (doublereal) (*n);
	zlarnd_(&z__2, &c__5, iseed);
	z__1.r = d__1 * z__2.r, z__1.i = d__1 * z__2.i;
	a[i__2].r = z__1.r, a[i__2].i = z__1.i;
	jj = jn;
	for (j = *n - 1; j >= 1; --j) {
	    jj -= j;
	    ++jn;
	    zcopy_(&j, &a_ref(jn, 1), &c__1, &a_ref(jj, 1), &c__1);
/* L230: */
	}

    } else if (*iflag == -12) {

/*        Lower triangular packed */

	i__2 = a_subscr(1, 1);
	d__1 = (doublereal) (*n);
	zlarnd_(&z__2, &c__5, iseed);
	z__1.r = d__1 * z__2.r, z__1.i = d__1 * z__2.i;
	a[i__2].r = z__1.r, a[i__2].i = z__1.i;
	if (*n > 1) {
	    i__2 = *n - 1;
	    zlarnv_(&c__2, iseed, &i__2, &a_ref(2, 1));
	}
	jj = *n + 1;
	i__2 = *n;
	for (j = 2; j <= i__2; ++j) {
	    i__1 = *n - j + 1;
	    zcopy_(&i__1, &a_ref(1, 1), &c__1, &a_ref(jj, 1), &c__1);
	    jj = jj + *n - j + 1;
/* L240: */
	}

    } else if (*iflag == 13) {

/*        Upper triangular banded */

	k = *kl;
/* Computing MIN */
	i__2 = *n - 1;
	mu = min(i__2,k);
	zlarnv_(&c__2, iseed, &mu, &a_ref(k + 1 - mu, *n));
	i__2 = a_subscr(k + 1, *n);
	d__1 = (doublereal) (k + 1);
	zlarnd_(&z__2, &c__5, iseed);
	z__1.r = d__1 * z__2.r, z__1.i = d__1 * z__2.i;
	a[i__2].r = z__1.r, a[i__2].i = z__1.i;
	for (j = *n - 1; j >= 1; --j) {
/* Computing MIN */
	    i__2 = j, i__1 = k + 1;
	    mu = min(i__2,i__1);
	    zcopy_(&mu, &a_ref(k + 2 - mu, *n), &c__1, &a_ref(k + 2 - mu, j), 
		    &c__1);
/* L250: */
	}

    } else if (*iflag == -13) {

/*        Lower triangular banded */

	k = *kl;
	i__2 = a_subscr(1, 1);
	d__1 = (doublereal) (k + 1);
	zlarnd_(&z__2, &c__5, iseed);
	z__1.r = d__1 * z__2.r, z__1.i = d__1 * z__2.i;
	a[i__2].r = z__1.r, a[i__2].i = z__1.i;
	if (*n > 1) {
/* Computing MIN */
	    i__1 = *n - 1;
	    i__2 = min(i__1,k);
	    zlarnv_(&c__2, iseed, &i__2, &a_ref(2, 1));
	}
	i__2 = *n;
	for (j = 2; j <= i__2; ++j) {
/* Computing MIN */
	    i__3 = *n - j + 1, i__4 = k + 1;
	    i__1 = min(i__3,i__4);
	    zcopy_(&i__1, &a_ref(1, 1), &c__1, &a_ref(1, j), &c__1);
/* L260: */
	}

    } else if (*iflag == 14) {

/*        General tridiagonal */

	i__2 = *n * 3 - 2;
	zlarnv_(&c__2, iseed, &i__2, &a[a_offset]);
    }

    return 0;

/*     End of ZTIMMG */

} /* ztimmg_ */

#undef a_ref
#undef a_subscr


