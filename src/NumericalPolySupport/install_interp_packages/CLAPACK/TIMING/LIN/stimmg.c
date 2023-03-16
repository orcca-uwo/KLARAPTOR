#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__1 = 1;

/* Subroutine */ int stimmg_(integer *iflag, integer *m, integer *n, real *a, 
	integer *lda, integer *kl, integer *ku)
{
    /* Initialized data */

    static integer iseed[4] = { 0,0,0,1 };

    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3, i__4;
    real r__1;

    /* Builtin functions */
    double r_sign(real *, real *);

    /* Local variables */
    static integer i__, j, k;
    extern /* Subroutine */ int scopy_(integer *, real *, integer *, real *, 
	    integer *);
    static integer jj, jn, mj, mu;
    extern /* Subroutine */ int slarnv_(integer *, integer *, integer *, real 
	    *);


#define a_ref(a_1,a_2) a[(a_2)*a_dim1 + a_1]


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    STIMMG generates a real test matrix whose type is given by IFLAG.   
    All the matrices are Toeplitz (constant along a diagonal), with   
    random elements on each diagonal.   

    Arguments   
    =========   

    IFLAG   (input) INTEGER   
            The type of matrix to be generated.   
            = 0 or 1:   General matrix   
            = 2 or -2:  General banded matrix   
            = 3 or -3:  Symmetric positive definite matrix   
            = 4 or -4:  Symmetric positive definite packed   
            = 5 or -5:  Symmetric positive definite banded   
            = 6 or -6:  Symmetric indefinite matrix   
            = 7 or -7:  Symmetric indefinite packed   
            = 8 or -8:  Symmetric indefinite banded   
            = 9 or -9:  Triangular   
            = 10 or -10:  Triangular packed   
            = 11 or -11:  Triangular banded   
            = 12:         General tridiagonal   
            = 13 or -13:  Positive definite tridiagonal   
            For symmetric or triangular matrices, IFLAG > 0 indicates   
            upper triangular storage and IFLAG < 0 indicates lower   
            triangular storage.   

    M       (input) INTEGER   
            The number of rows of the matrix to be generated.   

    N       (input) INTEGER   
            The number of columns of the matrix to be generated.   

    A       (output) REAL array, dimension (LDA,N)   
            The generated matrix.   

            If the absolute value of IFLAG is 1, 3, or 6, the leading   
            M x N (or N x N) subblock is used to store the matrix.   
            If the matrix is symmetric, only the upper or lower triangle   
            of this block is referenced.   

            If the absolute value of IFLAG is 4 or 7, the matrix is   
            symmetric and packed storage is used for the upper or lower   
            triangle.  The triangular matrix is stored columnwise as a   
            inear array, and the array A is treated as a vector of   
            length LDA.  LDA must be set to at least N*(N+1)/2.   

            If the absolute value of IFLAG is 2 or 5, the matrix is   
            returned in band format.  The columns of the matrix are   
            specified in the columns of A and the diagonals of the   
            matrix are specified in the rows of A, with the leading   
            diagonal in row   
                KL + KU + 1,  if IFLAG = 2   
                KU + 1,       if IFLAG = 5 or -2   
                1,            if IFLAG = -5   
            If IFLAG = 2, the first KL rows are not used to leave room   
            for pivoting in SGBTRF.   

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

	slarnv_(&c__2, iseed, m, &a_ref(1, 1));
	i__1 = *n;
	i__2 = *m;
	for (j = 2; i__2 < 0 ? j >= i__1 : j <= i__1; j += i__2) {
/* Computing MIN */
	    i__3 = *m, i__4 = *n - j + 1;
	    mj = min(i__3,i__4);
	    slarnv_(&c__2, iseed, &mj, &a_ref(1, j));
	    if (mj > 1) {
		i__3 = mj - 1;
		scopy_(&i__3, &a_ref(2, j), &c__1, &a_ref(1, j + 1), lda);
	    }
/* L10: */
	}

/*        Fill in the rest of the matrix. */

	i__2 = *n;
	for (j = 2; j <= i__2; ++j) {
	    i__1 = *m;
	    for (i__ = 2; i__ <= i__1; ++i__) {
		a_ref(i__, j) = a_ref(i__ - 1, j - 1);
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
	slarnv_(&c__2, iseed, &i__2, &a_ref(k, 1));
/* Computing MIN */
	i__2 = *n - 1;
	mu = min(i__2,*ku);
	i__2 = mu + 1;
	slarnv_(&c__2, iseed, &i__2, &a_ref(k - mu, *n));
	i__2 = *n - 1;
	for (j = 2; j <= i__2; ++j) {
/* Computing MIN */
	    i__1 = j - 1;
	    mu = min(i__1,*ku);
	    scopy_(&mu, &a_ref(k - mu, *n), &c__1, &a_ref(k - mu, j), &c__1);
/* Computing MIN */
	    i__3 = *m - j + 1, i__4 = *kl + 1;
	    i__1 = min(i__3,i__4);
	    scopy_(&i__1, &a_ref(k, 1), &c__1, &a_ref(k, j), &c__1);
/* L40: */
	}

    } else if (*iflag == 3) {

/*        Symmetric positive definite, upper triangle */

	i__2 = *n - 1;
	slarnv_(&c__2, iseed, &i__2, &a_ref(1, *n));
	a_ref(*n, *n) = (real) (*n);
	for (j = *n - 1; j >= 1; --j) {
	    scopy_(&j, &a_ref(*n - j + 1, *n), &c__1, &a_ref(1, j), &c__1);
/* L50: */
	}

    } else if (*iflag == -3) {

/*        Symmetric positive definite, lower triangle */

	a_ref(1, 1) = (real) (*n);
	if (*n > 1) {
	    i__2 = *n - 1;
	    slarnv_(&c__2, iseed, &i__2, &a_ref(2, 1));
	}
	i__2 = *n;
	for (j = 2; j <= i__2; ++j) {
	    i__1 = *n - j + 1;
	    scopy_(&i__1, &a_ref(1, 1), &c__1, &a_ref(j, j), &c__1);
/* L60: */
	}

    } else if (*iflag == 4) {

/*        Symmetric positive definite packed, upper triangle */

	jn = (*n - 1) * *n / 2 + 1;
	i__2 = *n - 1;
	slarnv_(&c__2, iseed, &i__2, &a_ref(jn, 1));
	a_ref(jn + *n - 1, 1) = (real) (*n);
	jj = jn;
	for (j = *n - 1; j >= 1; --j) {
	    jj -= j;
	    ++jn;
	    scopy_(&j, &a_ref(jn, 1), &c__1, &a_ref(jj, 1), &c__1);
/* L70: */
	}

    } else if (*iflag == -4) {

/*        Symmetric positive definite packed, lower triangle */

	a_ref(1, 1) = (real) (*n);
	if (*n > 1) {
	    i__2 = *n - 1;
	    slarnv_(&c__2, iseed, &i__2, &a_ref(2, 1));
	}
	jj = *n + 1;
	i__2 = *n;
	for (j = 2; j <= i__2; ++j) {
	    i__1 = *n - j + 1;
	    scopy_(&i__1, &a_ref(1, 1), &c__1, &a_ref(jj, 1), &c__1);
	    jj = jj + *n - j + 1;
/* L80: */
	}

    } else if (*iflag == 5) {

/*        Symmetric positive definite banded, upper triangle */

	k = *kl;
/* Computing MIN */
	i__2 = *n - 1;
	mu = min(i__2,k);
	slarnv_(&c__2, iseed, &mu, &a_ref(k + 1 - mu, *n));
	a_ref(k + 1, *n) = (real) (*n);
	for (j = *n - 1; j >= 1; --j) {
/* Computing MIN */
	    i__2 = j, i__1 = k + 1;
	    mu = min(i__2,i__1);
	    scopy_(&mu, &a_ref(k + 2 - mu, *n), &c__1, &a_ref(k + 2 - mu, j), 
		    &c__1);
/* L90: */
	}

    } else if (*iflag == -5) {

/*        Symmetric positive definite banded, lower triangle */

	k = *kl;
	a_ref(1, 1) = (real) (*n);
/* Computing MIN */
	i__1 = *n - 1;
	i__2 = min(i__1,k);
	slarnv_(&c__2, iseed, &i__2, &a_ref(2, 1));
	i__2 = *n;
	for (j = 2; j <= i__2; ++j) {
/* Computing MIN */
	    i__3 = *n - j + 1, i__4 = k + 1;
	    i__1 = min(i__3,i__4);
	    scopy_(&i__1, &a_ref(1, 1), &c__1, &a_ref(1, j), &c__1);
/* L100: */
	}

    } else if (*iflag == 6) {

/*        Symmetric indefinite, upper triangle */

	slarnv_(&c__2, iseed, n, &a_ref(1, *n));
	for (j = *n - 1; j >= 1; --j) {
	    scopy_(&j, &a_ref(*n - j + 1, *n), &c__1, &a_ref(1, j), &c__1);
/* L110: */
	}

    } else if (*iflag == -6) {

/*        Symmetric indefinite, lower triangle */

	slarnv_(&c__2, iseed, n, &a_ref(1, 1));
	i__2 = *n;
	for (j = 2; j <= i__2; ++j) {
	    i__1 = *n - j + 1;
	    scopy_(&i__1, &a_ref(1, 1), &c__1, &a_ref(j, j), &c__1);
/* L120: */
	}

    } else if (*iflag == 7) {

/*        Symmetric indefinite packed, upper triangle */

	jn = (*n - 1) * *n / 2 + 1;
	slarnv_(&c__2, iseed, n, &a_ref(jn, 1));
	jj = jn;
	for (j = *n - 1; j >= 1; --j) {
	    jj -= j;
	    ++jn;
	    scopy_(&j, &a_ref(jn, 1), &c__1, &a_ref(jj, 1), &c__1);
/* L130: */
	}

    } else if (*iflag == -7) {

/*        Symmetric indefinite packed, lower triangle */

	slarnv_(&c__2, iseed, n, &a_ref(1, 1));
	jj = *n + 1;
	i__2 = *n;
	for (j = 2; j <= i__2; ++j) {
	    i__1 = *n - j + 1;
	    scopy_(&i__1, &a_ref(1, 1), &c__1, &a_ref(jj, 1), &c__1);
	    jj = jj + *n - j + 1;
/* L140: */
	}

    } else if (*iflag == 8) {

/*        Symmetric indefinite banded, upper triangle */

	k = *kl;
/* Computing MIN */
	i__2 = *n, i__1 = k + 1;
	mu = min(i__2,i__1);
	slarnv_(&c__2, iseed, &mu, &a_ref(k + 2 - mu, *n));
	for (j = *n - 1; j >= 1; --j) {
/* Computing MIN */
	    i__2 = j, i__1 = k + 1;
	    mu = min(i__2,i__1);
	    scopy_(&mu, &a_ref(k + 2 - mu, *n), &c__1, &a_ref(k + 2 - mu, j), 
		    &c__1);
/* L150: */
	}

    } else if (*iflag == -8) {

/*        Symmetric indefinite banded, lower triangle */

	k = *kl;
/* Computing MIN */
	i__1 = *n, i__3 = k + 1;
	i__2 = min(i__1,i__3);
	slarnv_(&c__2, iseed, &i__2, &a_ref(1, 1));
	i__2 = *n;
	for (j = 2; j <= i__2; ++j) {
/* Computing MIN */
	    i__3 = *n - j + 1, i__4 = k + 1;
	    i__1 = min(i__3,i__4);
	    scopy_(&i__1, &a_ref(1, 1), &c__1, &a_ref(1, j), &c__1);
/* L160: */
	}

    } else if (*iflag == 9) {

/*        Upper triangular */

	slarnv_(&c__2, iseed, n, &a_ref(1, *n));
	r__1 = (real) (*n);
	a_ref(*n, *n) = r_sign(&r__1, &a_ref(*n, *n));
	for (j = *n - 1; j >= 1; --j) {
	    scopy_(&j, &a_ref(*n - j + 1, *n), &c__1, &a_ref(1, j), &c__1);
/* L170: */
	}

    } else if (*iflag == -9) {

/*        Lower triangular */

	slarnv_(&c__2, iseed, n, &a_ref(1, 1));
	r__1 = (real) (*n);
	a_ref(1, 1) = r_sign(&r__1, &a_ref(1, 1));
	i__2 = *n;
	for (j = 2; j <= i__2; ++j) {
	    i__1 = *n - j + 1;
	    scopy_(&i__1, &a_ref(1, 1), &c__1, &a_ref(j, j), &c__1);
/* L180: */
	}

    } else if (*iflag == 10) {

/*        Upper triangular packed */

	jn = (*n - 1) * *n / 2 + 1;
	slarnv_(&c__2, iseed, n, &a_ref(jn, 1));
	r__1 = (real) (*n);
	a_ref(jn + *n - 1, 1) = r_sign(&r__1, &a_ref(jn + *n - 1, 1));
	jj = jn;
	for (j = *n - 1; j >= 1; --j) {
	    jj -= j;
	    ++jn;
	    scopy_(&j, &a_ref(jn, 1), &c__1, &a_ref(jj, 1), &c__1);
/* L190: */
	}

    } else if (*iflag == -10) {

/*        Lower triangular packed */

	slarnv_(&c__2, iseed, n, &a_ref(1, 1));
	r__1 = (real) (*n);
	a_ref(1, 1) = r_sign(&r__1, &a_ref(1, 1));
	jj = *n + 1;
	i__2 = *n;
	for (j = 2; j <= i__2; ++j) {
	    i__1 = *n - j + 1;
	    scopy_(&i__1, &a_ref(1, 1), &c__1, &a_ref(jj, 1), &c__1);
	    jj = jj + *n - j + 1;
/* L200: */
	}

    } else if (*iflag == 11) {

/*        Upper triangular banded */

	k = *kl;
/* Computing MIN */
	i__2 = *n, i__1 = k + 1;
	mu = min(i__2,i__1);
	slarnv_(&c__2, iseed, &mu, &a_ref(k + 2 - mu, *n));
	r__1 = (real) (k + 1);
	a_ref(k + 1, *n) = r_sign(&r__1, &a_ref(k + 1, *n));
	for (j = *n - 1; j >= 1; --j) {
/* Computing MIN */
	    i__2 = j, i__1 = k + 1;
	    mu = min(i__2,i__1);
	    scopy_(&mu, &a_ref(k + 2 - mu, *n), &c__1, &a_ref(k + 2 - mu, j), 
		    &c__1);
/* L210: */
	}

    } else if (*iflag == -11) {

/*        Lower triangular banded */

	k = *kl;
/* Computing MIN */
	i__1 = *n, i__3 = k + 1;
	i__2 = min(i__1,i__3);
	slarnv_(&c__2, iseed, &i__2, &a_ref(1, 1));
	r__1 = (real) (k + 1);
	a_ref(1, 1) = r_sign(&r__1, &a_ref(1, 1));
	i__2 = *n;
	for (j = 2; j <= i__2; ++j) {
/* Computing MIN */
	    i__3 = *n - j + 1, i__4 = k + 1;
	    i__1 = min(i__3,i__4);
	    scopy_(&i__1, &a_ref(1, 1), &c__1, &a_ref(1, j), &c__1);
/* L220: */
	}

    } else if (*iflag == 12) {

/*        General tridiagonal */

	i__2 = *n * 3 - 2;
	slarnv_(&c__2, iseed, &i__2, &a[a_offset]);

    } else if (*iflag == 13 || *iflag == -13) {

/*        Positive definite tridiagonal */

	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {
	    a_ref(j, 1) = 2.f;
/* L230: */
	}
	i__2 = *n - 1;
	slarnv_(&c__2, iseed, &i__2, &a_ref(*n + 1, 1));
    }

    return 0;

/*     End of STIMMG */

} /* stimmg_ */

#undef a_ref


