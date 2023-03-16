#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static complex c_b1 = {0.f,0.f};

/* Subroutine */ int cget03_(integer *n, complex *a, integer *lda, complex *
	ainv, integer *ldainv, complex *work, integer *ldwork, real *rwork, 
	real *rcond, real *resid)
{
    /* System generated locals */
    integer a_dim1, a_offset, ainv_dim1, ainv_offset, work_dim1, work_offset, 
	    i__1, i__2, i__3;
    complex q__1;

    /* Local variables */
    static integer i__;
    extern /* Subroutine */ int cgemm_(char *, char *, integer *, integer *, 
	    integer *, complex *, complex *, integer *, complex *, integer *, 
	    complex *, complex *, integer *);
    static real anorm;
    extern doublereal clange_(char *, integer *, integer *, complex *, 
	    integer *, real *), slamch_(char *);
    static real ainvnm, eps;


#define work_subscr(a_1,a_2) (a_2)*work_dim1 + a_1
#define work_ref(a_1,a_2) work[work_subscr(a_1,a_2)]


/*  -- LAPACK test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   


    Purpose   
    =======   

    CGET03 computes the residual for a general matrix times its inverse:   
       norm( I - AINV*A ) / ( N * norm(A) * norm(AINV) * EPS ),   
    where EPS is the machine epsilon.   

    Arguments   
    ==========   

    N       (input) INTEGER   
            The number of rows and columns of the matrix A.  N >= 0.   

    A       (input) COMPLEX array, dimension (LDA,N)   
            The original N x N matrix A.   

    LDA     (input) INTEGER   
            The leading dimension of the array A.  LDA >= max(1,N).   

    AINV    (input) COMPLEX array, dimension (LDAINV,N)   
            The inverse of the matrix A.   

    LDAINV  (input) INTEGER   
            The leading dimension of the array AINV.  LDAINV >= max(1,N).   

    WORK    (workspace) COMPLEX array, dimension (LDWORK,N)   

    LDWORK  (input) INTEGER   
            The leading dimension of the array WORK.  LDWORK >= max(1,N).   

    RWORK   (workspace) REAL array, dimension (N)   

    RCOND   (output) REAL   
            The reciprocal of the condition number of A, computed as   
            ( 1/norm(A) ) / norm(AINV).   

    RESID   (output) REAL   
            norm(I - AINV*A) / ( N * norm(A) * norm(AINV) * EPS )   

    =====================================================================   


       Quick exit if N = 0.   

       Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    ainv_dim1 = *ldainv;
    ainv_offset = 1 + ainv_dim1 * 1;
    ainv -= ainv_offset;
    work_dim1 = *ldwork;
    work_offset = 1 + work_dim1 * 1;
    work -= work_offset;
    --rwork;

    /* Function Body */
    if (*n <= 0) {
	*rcond = 1.f;
	*resid = 0.f;
	return 0;
    }

/*     Exit with RESID = 1/EPS if ANORM = 0 or AINVNM = 0. */

    eps = slamch_("Epsilon");
    anorm = clange_("1", n, n, &a[a_offset], lda, &rwork[1]);
    ainvnm = clange_("1", n, n, &ainv[ainv_offset], ldainv, &rwork[1]);
    if (anorm <= 0.f || ainvnm <= 0.f) {
	*rcond = 0.f;
	*resid = 1.f / eps;
	return 0;
    }
    *rcond = 1.f / anorm / ainvnm;

/*     Compute I - A * AINV */

    q__1.r = -1.f, q__1.i = 0.f;
    cgemm_("No transpose", "No transpose", n, n, n, &q__1, &ainv[ainv_offset],
	     ldainv, &a[a_offset], lda, &c_b1, &work[work_offset], ldwork);
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = work_subscr(i__, i__);
	i__3 = work_subscr(i__, i__);
	q__1.r = work[i__3].r + 1.f, q__1.i = work[i__3].i + 0.f;
	work[i__2].r = q__1.r, work[i__2].i = q__1.i;
/* L10: */
    }

/*     Compute norm(I - AINV*A) / (N * norm(A) * norm(AINV) * EPS) */

    *resid = clange_("1", n, n, &work[work_offset], ldwork, &rwork[1]);

    *resid = *resid * *rcond / eps / (real) (*n);

    return 0;

/*     End of CGET03 */

} /* cget03_ */

#undef work_ref
#undef work_subscr


