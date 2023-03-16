#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static complex c_b12 = {-1.f,0.f};

/* Subroutine */ int ctrt02_(char *uplo, char *trans, char *diag, integer *n, 
	integer *nrhs, complex *a, integer *lda, complex *x, integer *ldx, 
	complex *b, integer *ldb, complex *work, real *rwork, real *resid)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, x_dim1, x_offset, i__1;
    real r__1, r__2;

    /* Local variables */
    static integer j;
    extern logical lsame_(char *, char *);
    static real anorm, bnorm;
    extern /* Subroutine */ int ccopy_(integer *, complex *, integer *, 
	    complex *, integer *), caxpy_(integer *, complex *, complex *, 
	    integer *, complex *, integer *), ctrmv_(char *, char *, char *, 
	    integer *, complex *, integer *, complex *, integer *);
    static real xnorm;
    extern doublereal slamch_(char *), clantr_(char *, char *, char *,
	     integer *, integer *, complex *, integer *, real *), scasum_(integer *, complex *, integer *);
    static real eps;


#define b_subscr(a_1,a_2) (a_2)*b_dim1 + a_1
#define b_ref(a_1,a_2) b[b_subscr(a_1,a_2)]
#define x_subscr(a_1,a_2) (a_2)*x_dim1 + a_1
#define x_ref(a_1,a_2) x[x_subscr(a_1,a_2)]


/*  -- LAPACK test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 29, 1992   


    Purpose   
    =======   

    CTRT02 computes the residual for the computed solution to a   
    triangular system of linear equations  A*x = b,  A**T *x = b,   
    or A**H *x = b.  Here A is a triangular matrix, A**T is the transpose   
    of A, A**H is the conjugate transpose of A, and x and b are N by NRHS   
    matrices.  The test ratio is the maximum over the number of right   
    hand sides of   
       norm(b - op(A)*x) / ( norm(op(A)) * norm(x) * EPS ),   
    where op(A) denotes A, A**T, or A**H, and EPS is the machine epsilon.   

    Arguments   
    =========   

    UPLO    (input) CHARACTER*1   
            Specifies whether the matrix A is upper or lower triangular.   
            = 'U':  Upper triangular   
            = 'L':  Lower triangular   

    TRANS   (input) CHARACTER*1   
            Specifies the operation applied to A.   
            = 'N':  A *x = b     (No transpose)   
            = 'T':  A**T *x = b  (Transpose)   
            = 'C':  A**H *x = b  (Conjugate transpose)   

    DIAG    (input) CHARACTER*1   
            Specifies whether or not the matrix A is unit triangular.   
            = 'N':  Non-unit triangular   
            = 'U':  Unit triangular   

    N       (input) INTEGER   
            The order of the matrix A.  N >= 0.   

    NRHS    (input) INTEGER   
            The number of right hand sides, i.e., the number of columns   
            of the matrices X and B.  NRHS >= 0.   

    A       (input) COMPLEX array, dimension (LDA,N)   
            The triangular matrix A.  If UPLO = 'U', the leading n by n   
            upper triangular part of the array A contains the upper   
            triangular matrix, and the strictly lower triangular part of   
            A is not referenced.  If UPLO = 'L', the leading n by n lower   
            triangular part of the array A contains the lower triangular   
            matrix, and the strictly upper triangular part of A is not   
            referenced.  If DIAG = 'U', the diagonal elements of A are   
            also not referenced and are assumed to be 1.   

    LDA     (input) INTEGER   
            The leading dimension of the array A.  LDA >= max(1,N).   

    X       (input) COMPLEX array, dimension (LDX,NRHS)   
            The computed solution vectors for the system of linear   
            equations.   

    LDX     (input) INTEGER   
            The leading dimension of the array X.  LDX >= max(1,N).   

    B       (input) COMPLEX array, dimension (LDB,NRHS)   
            The right hand side vectors for the system of linear   
            equations.   

    LDB     (input) INTEGER   
            The leading dimension of the array B.  LDB >= max(1,N).   

    WORK    (workspace) COMPLEX array, dimension (N)   

    RWORK   (workspace) REAL array, dimension (N)   

    RESID   (output) REAL   
            The maximum over the number of right hand sides of   
            norm(op(A)*x - b) / ( norm(op(A)) * norm(x) * EPS ).   

    =====================================================================   


       Quick exit if N = 0 or NRHS = 0   

       Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    x_dim1 = *ldx;
    x_offset = 1 + x_dim1 * 1;
    x -= x_offset;
    b_dim1 = *ldb;
    b_offset = 1 + b_dim1 * 1;
    b -= b_offset;
    --work;
    --rwork;

    /* Function Body */
    if (*n <= 0 || *nrhs <= 0) {
	*resid = 0.f;
	return 0;
    }

/*     Compute the 1-norm of A or A**H. */

    if (lsame_(trans, "N")) {
	anorm = clantr_("1", uplo, diag, n, n, &a[a_offset], lda, &rwork[1]);
    } else {
	anorm = clantr_("I", uplo, diag, n, n, &a[a_offset], lda, &rwork[1]);
    }

/*     Exit with RESID = 1/EPS if ANORM = 0. */

    eps = slamch_("Epsilon");
    if (anorm <= 0.f) {
	*resid = 1.f / eps;
	return 0;
    }

/*     Compute the maximum over the number of right hand sides of   
          norm(op(A)*x - b) / ( norm(op(A)) * norm(x) * EPS ) */

    *resid = 0.f;
    i__1 = *nrhs;
    for (j = 1; j <= i__1; ++j) {
	ccopy_(n, &x_ref(1, j), &c__1, &work[1], &c__1);
	ctrmv_(uplo, trans, diag, n, &a[a_offset], lda, &work[1], &c__1);
	caxpy_(n, &c_b12, &b_ref(1, j), &c__1, &work[1], &c__1);
	bnorm = scasum_(n, &work[1], &c__1);
	xnorm = scasum_(n, &x_ref(1, j), &c__1);
	if (xnorm <= 0.f) {
	    *resid = 1.f / eps;
	} else {
/* Computing MAX */
	    r__1 = *resid, r__2 = bnorm / anorm / xnorm / eps;
	    *resid = dmax(r__1,r__2);
	}
/* L10: */
    }

    return 0;

/*     End of CTRT02 */

} /* ctrt02_ */

#undef x_ref
#undef x_subscr
#undef b_ref
#undef b_subscr


