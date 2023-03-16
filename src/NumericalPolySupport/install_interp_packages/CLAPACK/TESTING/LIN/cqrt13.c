#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__1 = 1;
static integer c__0 = 0;

/* Subroutine */ int cqrt13_(integer *scale, integer *m, integer *n, complex *
	a, integer *lda, real *norma, integer *iseed)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3, i__4;
    real r__1, r__2, r__3;
    complex q__1, q__2;

    /* Builtin functions */
    double r_sign(real *, real *);

    /* Local variables */
    static integer info, j;
    static real dummy[1];
    extern /* Subroutine */ int slabad_(real *, real *);
    extern doublereal clange_(char *, integer *, integer *, complex *, 
	    integer *, real *);
    extern /* Subroutine */ int clascl_(char *, integer *, integer *, real *, 
	    real *, integer *, integer *, complex *, integer *, integer *);
    extern doublereal slamch_(char *);
    static real bignum;
    extern /* Subroutine */ int clarnv_(integer *, integer *, integer *, 
	    complex *);
    extern doublereal scasum_(integer *, complex *, integer *);
    static real smlnum;


#define a_subscr(a_1,a_2) (a_2)*a_dim1 + a_1
#define a_ref(a_1,a_2) a[a_subscr(a_1,a_2)]


/*  -- LAPACK test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 29, 1992   


    Purpose   
    =======   

    CQRT13 generates a full-rank matrix that may be scaled to have large   
    or small norm.   

    Arguments   
    =========   

    SCALE   (input) INTEGER   
            SCALE = 1: normally scaled matrix   
            SCALE = 2: matrix scaled up   
            SCALE = 3: matrix scaled down   

    M       (input) INTEGER   
            The number of rows of the matrix A.   

    N       (input) INTEGER   
            The number of columns of A.   

    A       (output) COMPLEX array, dimension (LDA,N)   
            The M-by-N matrix A.   

    LDA     (input) INTEGER   
            The leading dimension of the array A.   

    NORMA   (output) REAL   
            The one-norm of A.   

    ISEED   (input/output) integer array, dimension (4)   
            Seed for random number generator   

    =====================================================================   


       Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    --iseed;

    /* Function Body */
    if (*m <= 0 || *n <= 0) {
	return 0;
    }

/*     benign matrix */

    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	clarnv_(&c__2, &iseed[1], m, &a_ref(1, j));
	if (j <= *m) {
	    i__2 = a_subscr(j, j);
	    i__3 = a_subscr(j, j);
	    r__2 = scasum_(m, &a_ref(1, j), &c__1);
	    i__4 = a_subscr(j, j);
	    r__3 = a[i__4].r;
	    r__1 = r_sign(&r__2, &r__3);
	    q__2.r = r__1, q__2.i = 0.f;
	    q__1.r = a[i__3].r + q__2.r, q__1.i = a[i__3].i + q__2.i;
	    a[i__2].r = q__1.r, a[i__2].i = q__1.i;
	}
/* L10: */
    }

/*     scaled versions */

    if (*scale != 1) {
	*norma = clange_("Max", m, n, &a[a_offset], lda, dummy);
	smlnum = slamch_("Safe minimum");
	bignum = 1.f / smlnum;
	slabad_(&smlnum, &bignum);
	smlnum /= slamch_("Epsilon");
	bignum = 1.f / smlnum;

	if (*scale == 2) {

/*           matrix scaled up */

	    clascl_("General", &c__0, &c__0, norma, &bignum, m, n, &a[
		    a_offset], lda, &info);
	} else if (*scale == 3) {

/*           matrix scaled down */

	    clascl_("General", &c__0, &c__0, norma, &smlnum, m, n, &a[
		    a_offset], lda, &info);
	}
    }

    *norma = clange_("One-norm", m, n, &a[a_offset], lda, dummy);
    return 0;

/*     End of CQRT13 */

} /* cqrt13_ */

#undef a_ref
#undef a_subscr


