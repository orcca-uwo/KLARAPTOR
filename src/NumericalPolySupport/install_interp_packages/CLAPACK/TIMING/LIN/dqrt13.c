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

/* Subroutine */ int dqrt13_(integer *scale, integer *m, integer *n, 
	doublereal *a, integer *lda, doublereal *norma, integer *iseed)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1;
    doublereal d__1;

    /* Builtin functions */
    double d_sign(doublereal *, doublereal *);

    /* Local variables */
    static integer info, j;
    extern doublereal dasum_(integer *, doublereal *, integer *);
    static doublereal dummy[1];
    extern /* Subroutine */ int dlabad_(doublereal *, doublereal *);
    extern doublereal dlamch_(char *), dlange_(char *, integer *, 
	    integer *, doublereal *, integer *, doublereal *);
    extern /* Subroutine */ int dlascl_(char *, integer *, integer *, 
	    doublereal *, doublereal *, integer *, integer *, doublereal *, 
	    integer *, integer *);
    static doublereal bignum;
    extern /* Subroutine */ int dlarnv_(integer *, integer *, integer *, 
	    doublereal *);
    static doublereal smlnum;


#define a_ref(a_1,a_2) a[(a_2)*a_dim1 + a_1]


/*  -- LAPACK test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 29, 1992   


    Purpose   
    =======   

    DQRT13 generates a full-rank matrix that may be scaled to have large   
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

    A       (output) DOUBLE PRECISION array, dimension (LDA,N)   
            The M-by-N matrix A.   

    LDA     (input) INTEGER   
            The leading dimension of the array A.   

    NORMA   (output) DOUBLE PRECISION   
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
	dlarnv_(&c__2, &iseed[1], m, &a_ref(1, j));
	if (j <= *m) {
	    d__1 = dasum_(m, &a_ref(1, j), &c__1);
	    a_ref(j, j) = a_ref(j, j) + d_sign(&d__1, &a_ref(j, j));
	}
/* L10: */
    }

/*     scaled versions */

    if (*scale != 1) {
	*norma = dlange_("Max", m, n, &a[a_offset], lda, dummy);
	smlnum = dlamch_("Safe minimum");
	bignum = 1. / smlnum;
	dlabad_(&smlnum, &bignum);
	smlnum /= dlamch_("Epsilon");
	bignum = 1. / smlnum;

	if (*scale == 2) {

/*           matrix scaled up */

	    dlascl_("General", &c__0, &c__0, norma, &bignum, m, n, &a[
		    a_offset], lda, &info);
	} else if (*scale == 3) {

/*           matrix scaled down */

	    dlascl_("General", &c__0, &c__0, norma, &smlnum, m, n, &a[
		    a_offset], lda, &info);
	}
    }

    *norma = dlange_("One-norm", m, n, &a[a_offset], lda, dummy);
    return 0;

/*     End of DQRT13 */

} /* dqrt13_ */

#undef a_ref


