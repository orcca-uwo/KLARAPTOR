#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__8 = 8;
static doublereal c_b6 = 0.;
static doublereal c_b13 = -1.;
static integer c__1 = 1;

doublereal dtzt01_(integer *m, integer *n, doublereal *a, doublereal *af, 
	integer *lda, doublereal *tau, doublereal *work, integer *lwork)
{
    /* System generated locals */
    integer a_dim1, a_offset, af_dim1, af_offset, i__1, i__2;
    doublereal ret_val;

    /* Local variables */
    static integer i__, j;
    static doublereal norma;
    extern /* Subroutine */ int daxpy_(integer *, doublereal *, doublereal *, 
	    integer *, doublereal *, integer *);
    static doublereal rwork[1];
    extern doublereal dlamch_(char *), dlange_(char *, integer *, 
	    integer *, doublereal *, integer *, doublereal *);
    extern /* Subroutine */ int dlaset_(char *, integer *, integer *, 
	    doublereal *, doublereal *, doublereal *, integer *), 
	    xerbla_(char *, integer *), dlatzm_(char *, integer *, 
	    integer *, doublereal *, integer *, doublereal *, doublereal *, 
	    doublereal *, integer *, doublereal *);


#define a_ref(a_1,a_2) a[(a_2)*a_dim1 + a_1]
#define af_ref(a_1,a_2) af[(a_2)*af_dim1 + a_1]


/*  -- LAPACK test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    DTZT01 returns   
         || A - R*Q || / ( M * eps * ||A|| )   
    for an upper trapezoidal A that was factored with DTZRQF.   

    Arguments   
    =========   

    M       (input) INTEGER   
            The number of rows of the matrices A and AF.   

    N       (input) INTEGER   
            The number of columns of the matrices A and AF.   

    A       (input) DOUBLE PRECISION array, dimension (LDA,N)   
            The original upper trapezoidal M by N matrix A.   

    AF      (input) DOUBLE PRECISION array, dimension (LDA,N)   
            The output of DTZRQF for input matrix A.   
            The lower triangle is not referenced.   

    LDA     (input) INTEGER   
            The leading dimension of the arrays A and AF.   

    TAU     (input) DOUBLE PRECISION array, dimension (M)   
            Details of the  Householder transformations as returned by   
            DTZRQF.   

    WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK)   

    LWORK   (input) INTEGER   
            The length of the array WORK.  LWORK >= m*n + m.   

    =====================================================================   


       Parameter adjustments */
    af_dim1 = *lda;
    af_offset = 1 + af_dim1 * 1;
    af -= af_offset;
    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    --tau;
    --work;

    /* Function Body */
    ret_val = 0.;

    if (*lwork < *m * *n + *m) {
	xerbla_("DTZT01", &c__8);
	return ret_val;
    }

/*     Quick return if possible */

    if (*m <= 0 || *n <= 0) {
	return ret_val;
    }

    norma = dlange_("One-norm", m, n, &a[a_offset], lda, rwork);

/*     Copy upper triangle R */

    dlaset_("Full", m, n, &c_b6, &c_b6, &work[1], m);
    i__1 = *m;
    for (j = 1; j <= i__1; ++j) {
	i__2 = j;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    work[(j - 1) * *m + i__] = af_ref(i__, j);
/* L10: */
	}
/* L20: */
    }

/*     R = R * P(1) * ... *P(m) */

    i__1 = *m;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = *n - *m + 1;
	dlatzm_("Right", &i__, &i__2, &af_ref(i__, *m + 1), lda, &tau[i__], &
		work[(i__ - 1) * *m + 1], &work[*m * *m + 1], m, &work[*m * *
		n + 1]);
/* L30: */
    }

/*     R = R - A */

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	daxpy_(m, &c_b13, &a_ref(1, i__), &c__1, &work[(i__ - 1) * *m + 1], &
		c__1);
/* L40: */
    }

    ret_val = dlange_("One-norm", m, n, &work[1], m, rwork);

    ret_val /= dlamch_("Epsilon") * (doublereal) max(*m,*n);
    if (norma != 0.) {
	ret_val /= norma;
    }

    return ret_val;

/*     End of DTZT01 */

} /* dtzt01_ */

#undef af_ref
#undef a_ref


