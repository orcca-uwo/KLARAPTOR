#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static real c_b13 = -1.f;
static real c_b15 = 0.f;

/* Subroutine */ int sppt03_(char *uplo, integer *n, real *a, real *ainv, 
	real *work, integer *ldwork, real *rwork, real *rcond, real *resid)
{
    /* System generated locals */
    integer work_dim1, work_offset, i__1, i__2;

    /* Local variables */
    static integer i__, j;
    extern logical lsame_(char *, char *);
    static real anorm;
    extern /* Subroutine */ int scopy_(integer *, real *, integer *, real *, 
	    integer *), sspmv_(char *, integer *, real *, real *, real *, 
	    integer *, real *, real *, integer *);
    static integer jj;
    extern doublereal slamch_(char *), slange_(char *, integer *, 
	    integer *, real *, integer *, real *);
    static real ainvnm;
    extern doublereal slansp_(char *, char *, integer *, real *, real *);
    static real eps;


#define work_ref(a_1,a_2) work[(a_2)*work_dim1 + a_1]


/*  -- LAPACK test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 29, 1992   


    Purpose   
    =======   

    SPPT03 computes the residual for a symmetric packed matrix times its   
    inverse:   
       norm( I - A*AINV ) / ( N * norm(A) * norm(AINV) * EPS ),   
    where EPS is the machine epsilon.   

    Arguments   
    ==========   

    UPLO    (input) CHARACTER*1   
            Specifies whether the upper or lower triangular part of the   
            symmetric matrix A is stored:   
            = 'U':  Upper triangular   
            = 'L':  Lower triangular   

    N       (input) INTEGER   
            The number of rows and columns of the matrix A.  N >= 0.   

    A       (input) REAL array, dimension (N*(N+1)/2)   
            The original symmetric matrix A, stored as a packed   
            triangular matrix.   

    AINV    (input) REAL array, dimension (N*(N+1)/2)   
            The (symmetric) inverse of the matrix A, stored as a packed   
            triangular matrix.   

    WORK    (workspace) REAL array, dimension (LDWORK,N)   

    LDWORK  (input) INTEGER   
            The leading dimension of the array WORK.  LDWORK >= max(1,N).   

    RWORK   (workspace) REAL array, dimension (N)   

    RCOND   (output) REAL   
            The reciprocal of the condition number of A, computed as   
            ( 1/norm(A) ) / norm(AINV).   

    RESID   (output) REAL   
            norm(I - A*AINV) / ( N * norm(A) * norm(AINV) * EPS )   

    =====================================================================   


       Quick exit if N = 0.   

       Parameter adjustments */
    --a;
    --ainv;
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
    anorm = slansp_("1", uplo, n, &a[1], &rwork[1]);
    ainvnm = slansp_("1", uplo, n, &ainv[1], &rwork[1]);
    if (anorm <= 0.f || ainvnm == 0.f) {
	*rcond = 0.f;
	*resid = 1.f / eps;
	return 0;
    }
    *rcond = 1.f / anorm / ainvnm;

/*     UPLO = 'U':   
       Copy the leading N-1 x N-1 submatrix of AINV to WORK(1:N,2:N) and   
       expand it to a full matrix, then multiply by A one column at a   
       time, moving the result one column to the left. */

    if (lsame_(uplo, "U")) {

/*        Copy AINV */

	jj = 1;
	i__1 = *n - 1;
	for (j = 1; j <= i__1; ++j) {
	    scopy_(&j, &ainv[jj], &c__1, &work_ref(1, j + 1), &c__1);
	    i__2 = j - 1;
	    scopy_(&i__2, &ainv[jj], &c__1, &work_ref(j, 2), ldwork);
	    jj += j;
/* L10: */
	}
	jj = (*n - 1) * *n / 2 + 1;
	i__1 = *n - 1;
	scopy_(&i__1, &ainv[jj], &c__1, &work_ref(*n, 2), ldwork);

/*        Multiply by A */

	i__1 = *n - 1;
	for (j = 1; j <= i__1; ++j) {
	    sspmv_("Upper", n, &c_b13, &a[1], &work_ref(1, j + 1), &c__1, &
		    c_b15, &work_ref(1, j), &c__1);
/* L20: */
	}
	sspmv_("Upper", n, &c_b13, &a[1], &ainv[jj], &c__1, &c_b15, &work_ref(
		1, *n), &c__1);

/*     UPLO = 'L':   
       Copy the trailing N-1 x N-1 submatrix of AINV to WORK(1:N,1:N-1)   
       and multiply by A, moving each column to the right. */

    } else {

/*        Copy AINV */

	i__1 = *n - 1;
	scopy_(&i__1, &ainv[2], &c__1, &work_ref(1, 1), ldwork);
	jj = *n + 1;
	i__1 = *n;
	for (j = 2; j <= i__1; ++j) {
	    i__2 = *n - j + 1;
	    scopy_(&i__2, &ainv[jj], &c__1, &work_ref(j, j - 1), &c__1);
	    i__2 = *n - j;
	    scopy_(&i__2, &ainv[jj + 1], &c__1, &work_ref(j, j), ldwork);
	    jj = jj + *n - j + 1;
/* L30: */
	}

/*        Multiply by A */

	for (j = *n; j >= 2; --j) {
	    sspmv_("Lower", n, &c_b13, &a[1], &work_ref(1, j - 1), &c__1, &
		    c_b15, &work_ref(1, j), &c__1);
/* L40: */
	}
	sspmv_("Lower", n, &c_b13, &a[1], &ainv[1], &c__1, &c_b15, &work_ref(
		1, 1), &c__1);

    }

/*     Add the identity matrix to WORK . */

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	work_ref(i__, i__) = work_ref(i__, i__) + 1.f;
/* L50: */
    }

/*     Compute norm(I - A*AINV) / (N * norm(A) * norm(AINV) * EPS) */

    *resid = slange_("1", n, n, &work[work_offset], ldwork, &rwork[1]);

    *resid = *resid * *rcond / eps / (real) (*n);

    return 0;

/*     End of SPPT03 */

} /* sppt03_ */

#undef work_ref


