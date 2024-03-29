#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__7 = 7;
static doublecomplex c_b5 = {0.,0.};
static doublecomplex c_b6 = {1.,0.};

doublereal ztzt02_(integer *m, integer *n, doublecomplex *af, integer *lda, 
	doublecomplex *tau, doublecomplex *work, integer *lwork)
{
    /* System generated locals */
    integer af_dim1, af_offset, i__1, i__2, i__3;
    doublereal ret_val;
    doublecomplex z__1;

    /* Builtin functions */
    void d_cnjg(doublecomplex *, doublecomplex *);

    /* Local variables */
    static integer i__;
    static doublereal rwork[1];
    extern doublereal dlamch_(char *);
    extern /* Subroutine */ int xerbla_(char *, integer *);
    extern doublereal zlange_(char *, integer *, integer *, doublecomplex *, 
	    integer *, doublereal *);
    extern /* Subroutine */ int zlaset_(char *, integer *, integer *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, integer *), zlatzm_(char *, integer *, integer *, doublecomplex *, 
	    integer *, doublecomplex *, doublecomplex *, doublecomplex *, 
	    integer *, doublecomplex *);


#define af_subscr(a_1,a_2) (a_2)*af_dim1 + a_1
#define af_ref(a_1,a_2) af[af_subscr(a_1,a_2)]


/*  -- LAPACK test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    ZTZT02 returns   
         || I - Q'*Q || / ( M * eps)   
    where the matrix Q is defined by the Householder transformations   
    generated by ZTZRQF.   

    Arguments   
    =========   

    M       (input) INTEGER   
            The number of rows of the matrix AF.   

    N       (input) INTEGER   
            The number of columns of the matrix AF.   

    AF      (input) COMPLEX*16 array, dimension (LDA,N)   
            The output of ZTZRQF.   

    LDA     (input) INTEGER   
            The leading dimension of the array AF.   

    TAU     (input) COMPLEX*16 array, dimension (M)   
            Details of the Householder transformations as returned by   
            ZTZRQF.   

    WORK    (workspace) COMPLEX*16 array, dimension (LWORK)   

    LWORK   (input) INTEGER   
            length of WORK array. Must be >= N*N+N   

    =====================================================================   


       Parameter adjustments */
    af_dim1 = *lda;
    af_offset = 1 + af_dim1 * 1;
    af -= af_offset;
    --tau;
    --work;

    /* Function Body */
    ret_val = 0.;

    if (*lwork < *n * *n + *n) {
	xerbla_("ZTZT02", &c__7);
	return ret_val;
    }

/*     Quick return if possible */

    if (*m <= 0 || *n <= 0) {
	return ret_val;
    }

/*     Q := I */

    zlaset_("Full", n, n, &c_b5, &c_b6, &work[1], n);

/*     Q := P(1) * ... * P(m) * Q */

    for (i__ = *m; i__ >= 1; --i__) {
	i__1 = *n - *m + 1;
	zlatzm_("Left", &i__1, n, &af_ref(i__, *m + 1), lda, &tau[i__], &work[
		i__], &work[*m + 1], n, &work[*n * *n + 1]);
/* L10: */
    }

/*     Q := P(m)' * ... * P(1)' * Q */

    i__1 = *m;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = *n - *m + 1;
	d_cnjg(&z__1, &tau[i__]);
	zlatzm_("Left", &i__2, n, &af_ref(i__, *m + 1), lda, &z__1, &work[i__]
		, &work[*m + 1], n, &work[*n * *n + 1]);
/* L20: */
    }

/*     Q := Q - I */

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = (i__ - 1) * *n + i__;
	i__3 = (i__ - 1) * *n + i__;
	z__1.r = work[i__3].r - 1., z__1.i = work[i__3].i;
	work[i__2].r = z__1.r, work[i__2].i = z__1.i;
/* L30: */
    }

    ret_val = zlange_("One-norm", n, n, &work[1], n, rwork) / (
	    dlamch_("Epsilon") * (doublereal) max(*m,*n));
    return ret_val;

/*     End of ZTZT02 */

} /* ztzt02_ */

#undef af_ref
#undef af_subscr


