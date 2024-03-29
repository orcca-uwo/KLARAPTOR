#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__5 = 5;
static integer c__2 = 2;

/* Subroutine */ int zlatsy_(char *uplo, integer *n, doublecomplex *x, 
	integer *ldx, integer *iseed)
{
    /* System generated locals */
    integer x_dim1, x_offset, i__1, i__2, i__3;
    doublecomplex z__1, z__2, z__3;

    /* Builtin functions */
    double sqrt(doublereal), z_abs(doublecomplex *);

    /* Local variables */
    static doublereal beta;
    static doublecomplex a, b, c__;
    static integer i__, j;
    static doublecomplex r__;
    static doublereal alpha;
    static integer n5;
    static doublereal alpha3;
    extern /* Double Complex */ VOID zlarnd_(doublecomplex *, integer *, 
	    integer *);


#define x_subscr(a_1,a_2) (a_2)*x_dim1 + a_1
#define x_ref(a_1,a_2) x[x_subscr(a_1,a_2)]


/*  -- LAPACK auxiliary test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 29, 1992   


    Purpose   
    =======   

    ZLATSY generates a special test matrix for the complex symmetric   
    (indefinite) factorization.  The pivot blocks of the generated matrix   
    will be in the following order:   
       2x2 pivot block, non diagonalizable   
       1x1 pivot block   
       2x2 pivot block, diagonalizable   
       (cycle repeats)   
    A row interchange is required for each non-diagonalizable 2x2 block.   

    Arguments   
    =========   

    UPLO    (input) CHARACTER   
            Specifies whether the generated matrix is to be upper or   
            lower triangular.   
            = 'U':  Upper triangular   
            = 'L':  Lower triangular   

    N       (input) INTEGER   
            The dimension of the matrix to be generated.   

    X       (output) COMPLEX*16 array, dimension (LDX,N)   
            The generated matrix, consisting of 3x3 and 2x2 diagonal   
            blocks which result in the pivot sequence given above.   
            The matrix outside of these diagonal blocks is zero.   

    LDX     (input) INTEGER   
            The leading dimension of the array X.   

    ISEED   (input/output) INTEGER array, dimension (4)   
            On entry, the seed for the random number generator.  The last   
            of the four integers must be odd.  (modified on exit)   

    =====================================================================   


       Initialize constants   

       Parameter adjustments */
    x_dim1 = *ldx;
    x_offset = 1 + x_dim1 * 1;
    x -= x_offset;
    --iseed;

    /* Function Body */
    alpha = (sqrt(17.) + 1.) / 8.;
    beta = alpha - .001;
    alpha3 = alpha * alpha * alpha;

/*     UPLO = 'U':  Upper triangular storage */

    if (*(unsigned char *)uplo == 'U') {

/*        Fill the upper triangle of the matrix with zeros. */

	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
	    i__2 = j;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		i__3 = x_subscr(i__, j);
		x[i__3].r = 0., x[i__3].i = 0.;
/* L10: */
	    }
/* L20: */
	}
	n5 = *n / 5;
	n5 = *n - n5 * 5 + 1;

	i__1 = n5;
	for (i__ = *n; i__ >= i__1; i__ += -5) {
	    zlarnd_(&z__2, &c__5, &iseed[1]);
	    z__1.r = alpha3 * z__2.r, z__1.i = alpha3 * z__2.i;
	    a.r = z__1.r, a.i = z__1.i;
	    zlarnd_(&z__2, &c__5, &iseed[1]);
	    z__1.r = z__2.r / alpha, z__1.i = z__2.i / alpha;
	    b.r = z__1.r, b.i = z__1.i;
	    z__3.r = b.r * 2., z__3.i = b.i * 2.;
	    z__2.r = z__3.r * 0. - z__3.i * 1., z__2.i = z__3.r * 1. + z__3.i 
		    * 0.;
	    z__1.r = a.r - z__2.r, z__1.i = a.i - z__2.i;
	    c__.r = z__1.r, c__.i = z__1.i;
	    z__1.r = c__.r / beta, z__1.i = c__.i / beta;
	    r__.r = z__1.r, r__.i = z__1.i;
	    i__2 = x_subscr(i__, i__);
	    x[i__2].r = a.r, x[i__2].i = a.i;
	    i__2 = x_subscr(i__ - 2, i__);
	    x[i__2].r = b.r, x[i__2].i = b.i;
	    i__2 = x_subscr(i__ - 2, i__ - 1);
	    x[i__2].r = r__.r, x[i__2].i = r__.i;
	    i__2 = x_subscr(i__ - 2, i__ - 2);
	    x[i__2].r = c__.r, x[i__2].i = c__.i;
	    i__2 = x_subscr(i__ - 1, i__ - 1);
	    zlarnd_(&z__1, &c__2, &iseed[1]);
	    x[i__2].r = z__1.r, x[i__2].i = z__1.i;
	    i__2 = x_subscr(i__ - 3, i__ - 3);
	    zlarnd_(&z__1, &c__2, &iseed[1]);
	    x[i__2].r = z__1.r, x[i__2].i = z__1.i;
	    i__2 = x_subscr(i__ - 4, i__ - 4);
	    zlarnd_(&z__1, &c__2, &iseed[1]);
	    x[i__2].r = z__1.r, x[i__2].i = z__1.i;
	    if (z_abs(&x_ref(i__ - 3, i__ - 3)) > z_abs(&x_ref(i__ - 4, i__ - 
		    4))) {
		i__2 = x_subscr(i__ - 4, i__ - 3);
		i__3 = x_subscr(i__ - 3, i__ - 3);
		z__1.r = x[i__3].r * 2., z__1.i = x[i__3].i * 2.;
		x[i__2].r = z__1.r, x[i__2].i = z__1.i;
	    } else {
		i__2 = x_subscr(i__ - 4, i__ - 3);
		i__3 = x_subscr(i__ - 4, i__ - 4);
		z__1.r = x[i__3].r * 2., z__1.i = x[i__3].i * 2.;
		x[i__2].r = z__1.r, x[i__2].i = z__1.i;
	    }
/* L30: */
	}

/*        Clean-up for N not a multiple of 5. */

	i__ = n5 - 1;
	if (i__ > 2) {
	    zlarnd_(&z__2, &c__5, &iseed[1]);
	    z__1.r = alpha3 * z__2.r, z__1.i = alpha3 * z__2.i;
	    a.r = z__1.r, a.i = z__1.i;
	    zlarnd_(&z__2, &c__5, &iseed[1]);
	    z__1.r = z__2.r / alpha, z__1.i = z__2.i / alpha;
	    b.r = z__1.r, b.i = z__1.i;
	    z__3.r = b.r * 2., z__3.i = b.i * 2.;
	    z__2.r = z__3.r * 0. - z__3.i * 1., z__2.i = z__3.r * 1. + z__3.i 
		    * 0.;
	    z__1.r = a.r - z__2.r, z__1.i = a.i - z__2.i;
	    c__.r = z__1.r, c__.i = z__1.i;
	    z__1.r = c__.r / beta, z__1.i = c__.i / beta;
	    r__.r = z__1.r, r__.i = z__1.i;
	    i__1 = x_subscr(i__, i__);
	    x[i__1].r = a.r, x[i__1].i = a.i;
	    i__1 = x_subscr(i__ - 2, i__);
	    x[i__1].r = b.r, x[i__1].i = b.i;
	    i__1 = x_subscr(i__ - 2, i__ - 1);
	    x[i__1].r = r__.r, x[i__1].i = r__.i;
	    i__1 = x_subscr(i__ - 2, i__ - 2);
	    x[i__1].r = c__.r, x[i__1].i = c__.i;
	    i__1 = x_subscr(i__ - 1, i__ - 1);
	    zlarnd_(&z__1, &c__2, &iseed[1]);
	    x[i__1].r = z__1.r, x[i__1].i = z__1.i;
	    i__ += -3;
	}
	if (i__ > 1) {
	    i__1 = x_subscr(i__, i__);
	    zlarnd_(&z__1, &c__2, &iseed[1]);
	    x[i__1].r = z__1.r, x[i__1].i = z__1.i;
	    i__1 = x_subscr(i__ - 1, i__ - 1);
	    zlarnd_(&z__1, &c__2, &iseed[1]);
	    x[i__1].r = z__1.r, x[i__1].i = z__1.i;
	    if (z_abs(&x_ref(i__, i__)) > z_abs(&x_ref(i__ - 1, i__ - 1))) {
		i__1 = x_subscr(i__ - 1, i__);
		i__2 = x_subscr(i__, i__);
		z__1.r = x[i__2].r * 2., z__1.i = x[i__2].i * 2.;
		x[i__1].r = z__1.r, x[i__1].i = z__1.i;
	    } else {
		i__1 = x_subscr(i__ - 1, i__);
		i__2 = x_subscr(i__ - 1, i__ - 1);
		z__1.r = x[i__2].r * 2., z__1.i = x[i__2].i * 2.;
		x[i__1].r = z__1.r, x[i__1].i = z__1.i;
	    }
	    i__ += -2;
	} else if (i__ == 1) {
	    i__1 = x_subscr(i__, i__);
	    zlarnd_(&z__1, &c__2, &iseed[1]);
	    x[i__1].r = z__1.r, x[i__1].i = z__1.i;
	    --i__;
	}

/*     UPLO = 'L':  Lower triangular storage */

    } else {

/*        Fill the lower triangle of the matrix with zeros. */

	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
	    i__2 = *n;
	    for (i__ = j; i__ <= i__2; ++i__) {
		i__3 = x_subscr(i__, j);
		x[i__3].r = 0., x[i__3].i = 0.;
/* L40: */
	    }
/* L50: */
	}
	n5 = *n / 5;
	n5 *= 5;

	i__1 = n5;
	for (i__ = 1; i__ <= i__1; i__ += 5) {
	    zlarnd_(&z__2, &c__5, &iseed[1]);
	    z__1.r = alpha3 * z__2.r, z__1.i = alpha3 * z__2.i;
	    a.r = z__1.r, a.i = z__1.i;
	    zlarnd_(&z__2, &c__5, &iseed[1]);
	    z__1.r = z__2.r / alpha, z__1.i = z__2.i / alpha;
	    b.r = z__1.r, b.i = z__1.i;
	    z__3.r = b.r * 2., z__3.i = b.i * 2.;
	    z__2.r = z__3.r * 0. - z__3.i * 1., z__2.i = z__3.r * 1. + z__3.i 
		    * 0.;
	    z__1.r = a.r - z__2.r, z__1.i = a.i - z__2.i;
	    c__.r = z__1.r, c__.i = z__1.i;
	    z__1.r = c__.r / beta, z__1.i = c__.i / beta;
	    r__.r = z__1.r, r__.i = z__1.i;
	    i__2 = x_subscr(i__, i__);
	    x[i__2].r = a.r, x[i__2].i = a.i;
	    i__2 = x_subscr(i__ + 2, i__);
	    x[i__2].r = b.r, x[i__2].i = b.i;
	    i__2 = x_subscr(i__ + 2, i__ + 1);
	    x[i__2].r = r__.r, x[i__2].i = r__.i;
	    i__2 = x_subscr(i__ + 2, i__ + 2);
	    x[i__2].r = c__.r, x[i__2].i = c__.i;
	    i__2 = x_subscr(i__ + 1, i__ + 1);
	    zlarnd_(&z__1, &c__2, &iseed[1]);
	    x[i__2].r = z__1.r, x[i__2].i = z__1.i;
	    i__2 = x_subscr(i__ + 3, i__ + 3);
	    zlarnd_(&z__1, &c__2, &iseed[1]);
	    x[i__2].r = z__1.r, x[i__2].i = z__1.i;
	    i__2 = x_subscr(i__ + 4, i__ + 4);
	    zlarnd_(&z__1, &c__2, &iseed[1]);
	    x[i__2].r = z__1.r, x[i__2].i = z__1.i;
	    if (z_abs(&x_ref(i__ + 3, i__ + 3)) > z_abs(&x_ref(i__ + 4, i__ + 
		    4))) {
		i__2 = x_subscr(i__ + 4, i__ + 3);
		i__3 = x_subscr(i__ + 3, i__ + 3);
		z__1.r = x[i__3].r * 2., z__1.i = x[i__3].i * 2.;
		x[i__2].r = z__1.r, x[i__2].i = z__1.i;
	    } else {
		i__2 = x_subscr(i__ + 4, i__ + 3);
		i__3 = x_subscr(i__ + 4, i__ + 4);
		z__1.r = x[i__3].r * 2., z__1.i = x[i__3].i * 2.;
		x[i__2].r = z__1.r, x[i__2].i = z__1.i;
	    }
/* L60: */
	}

/*        Clean-up for N not a multiple of 5. */

	i__ = n5 + 1;
	if (i__ < *n - 1) {
	    zlarnd_(&z__2, &c__5, &iseed[1]);
	    z__1.r = alpha3 * z__2.r, z__1.i = alpha3 * z__2.i;
	    a.r = z__1.r, a.i = z__1.i;
	    zlarnd_(&z__2, &c__5, &iseed[1]);
	    z__1.r = z__2.r / alpha, z__1.i = z__2.i / alpha;
	    b.r = z__1.r, b.i = z__1.i;
	    z__3.r = b.r * 2., z__3.i = b.i * 2.;
	    z__2.r = z__3.r * 0. - z__3.i * 1., z__2.i = z__3.r * 1. + z__3.i 
		    * 0.;
	    z__1.r = a.r - z__2.r, z__1.i = a.i - z__2.i;
	    c__.r = z__1.r, c__.i = z__1.i;
	    z__1.r = c__.r / beta, z__1.i = c__.i / beta;
	    r__.r = z__1.r, r__.i = z__1.i;
	    i__1 = x_subscr(i__, i__);
	    x[i__1].r = a.r, x[i__1].i = a.i;
	    i__1 = x_subscr(i__ + 2, i__);
	    x[i__1].r = b.r, x[i__1].i = b.i;
	    i__1 = x_subscr(i__ + 2, i__ + 1);
	    x[i__1].r = r__.r, x[i__1].i = r__.i;
	    i__1 = x_subscr(i__ + 2, i__ + 2);
	    x[i__1].r = c__.r, x[i__1].i = c__.i;
	    i__1 = x_subscr(i__ + 1, i__ + 1);
	    zlarnd_(&z__1, &c__2, &iseed[1]);
	    x[i__1].r = z__1.r, x[i__1].i = z__1.i;
	    i__ += 3;
	}
	if (i__ < *n) {
	    i__1 = x_subscr(i__, i__);
	    zlarnd_(&z__1, &c__2, &iseed[1]);
	    x[i__1].r = z__1.r, x[i__1].i = z__1.i;
	    i__1 = x_subscr(i__ + 1, i__ + 1);
	    zlarnd_(&z__1, &c__2, &iseed[1]);
	    x[i__1].r = z__1.r, x[i__1].i = z__1.i;
	    if (z_abs(&x_ref(i__, i__)) > z_abs(&x_ref(i__ + 1, i__ + 1))) {
		i__1 = x_subscr(i__ + 1, i__);
		i__2 = x_subscr(i__, i__);
		z__1.r = x[i__2].r * 2., z__1.i = x[i__2].i * 2.;
		x[i__1].r = z__1.r, x[i__1].i = z__1.i;
	    } else {
		i__1 = x_subscr(i__ + 1, i__);
		i__2 = x_subscr(i__ + 1, i__ + 1);
		z__1.r = x[i__2].r * 2., z__1.i = x[i__2].i * 2.;
		x[i__1].r = z__1.r, x[i__1].i = z__1.i;
	    }
	    i__ += 2;
	} else if (i__ == *n) {
	    i__1 = x_subscr(i__, i__);
	    zlarnd_(&z__1, &c__2, &iseed[1]);
	    x[i__1].r = z__1.r, x[i__1].i = z__1.i;
	    ++i__;
	}
    }

    return 0;

/*     End of ZLATSY */

} /* zlatsy_ */

#undef x_ref
#undef x_subscr


