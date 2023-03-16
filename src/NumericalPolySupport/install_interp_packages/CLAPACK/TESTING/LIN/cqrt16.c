#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static complex c_b1 = {1.f,0.f};
static integer c__1 = 1;

/* Subroutine */ int cqrt16_(char *trans, integer *m, integer *n, integer *
	nrhs, complex *a, integer *lda, complex *x, integer *ldx, complex *b, 
	integer *ldb, real *rwork, real *resid)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, x_dim1, x_offset, i__1;
    real r__1, r__2;
    complex q__1;

    /* Local variables */
    static integer j;
    extern /* Subroutine */ int cgemm_(char *, char *, integer *, integer *, 
	    integer *, complex *, complex *, integer *, complex *, integer *, 
	    complex *, complex *, integer *);
    extern logical lsame_(char *, char *);
    static real anorm, bnorm;
    static integer n1, n2;
    static real xnorm;
    extern doublereal clange_(char *, integer *, integer *, complex *, 
	    integer *, real *), slamch_(char *), scasum_(
	    integer *, complex *, integer *);
    static real eps;


#define b_subscr(a_1,a_2) (a_2)*b_dim1 + a_1
#define b_ref(a_1,a_2) b[b_subscr(a_1,a_2)]
#define x_subscr(a_1,a_2) (a_2)*x_dim1 + a_1
#define x_ref(a_1,a_2) x[x_subscr(a_1,a_2)]


/*  -- LAPACK test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    CQRT16 computes the residual for a solution of a system of linear   
    equations  A*x = b  or  A'*x = b:   
       RESID = norm(B - A*X) / ( max(m,n) * norm(A) * norm(X) * EPS ),   
    where EPS is the machine epsilon.   

    Arguments   
    =========   

    TRANS   (input) CHARACTER*1   
            Specifies the form of the system of equations:   
            = 'N':  A *x = b   
            = 'T':  A^T*x = b, where A^T is the transpose of A   
            = 'C':  A^H*x = b, where A^H is the conjugate transpose of A   

    M       (input) INTEGER   
            The number of rows of the matrix A.  M >= 0.   

    N       (input) INTEGER   
            The number of columns of the matrix A.  N >= 0.   

    NRHS    (input) INTEGER   
            The number of columns of B, the matrix of right hand sides.   
            NRHS >= 0.   

    A       (input) COMPLEX array, dimension (LDA,N)   
            The original M x N matrix A.   

    LDA     (input) INTEGER   
            The leading dimension of the array A.  LDA >= max(1,M).   

    X       (input) COMPLEX array, dimension (LDX,NRHS)   
            The computed solution vectors for the system of linear   
            equations.   

    LDX     (input) INTEGER   
            The leading dimension of the array X.  If TRANS = 'N',   
            LDX >= max(1,N); if TRANS = 'T' or 'C', LDX >= max(1,M).   

    B       (input/output) COMPLEX array, dimension (LDB,NRHS)   
            On entry, the right hand side vectors for the system of   
            linear equations.   
            On exit, B is overwritten with the difference B - A*X.   

    LDB     (input) INTEGER   
            The leading dimension of the array B.  IF TRANS = 'N',   
            LDB >= max(1,M); if TRANS = 'T' or 'C', LDB >= max(1,N).   

    RWORK   (workspace) REAL array, dimension (M)   

    RESID   (output) REAL   
            The maximum over the number of right hand sides of   
            norm(B - A*X) / ( max(m,n) * norm(A) * norm(X) * EPS ).   

    =====================================================================   


       Quick exit if M = 0 or N = 0 or NRHS = 0   

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
    --rwork;

    /* Function Body */
    if (*m <= 0 || *n <= 0 || *nrhs == 0) {
	*resid = 0.f;
	return 0;
    }

    if (lsame_(trans, "T") || lsame_(trans, "C")) {
	anorm = clange_("I", m, n, &a[a_offset], lda, &rwork[1]);
	n1 = *n;
	n2 = *m;
    } else {
	anorm = clange_("1", m, n, &a[a_offset], lda, &rwork[1]);
	n1 = *m;
	n2 = *n;
    }

    eps = slamch_("Epsilon");

/*     Compute  B - A*X  (or  B - A'*X ) and store in B. */

    q__1.r = -1.f, q__1.i = 0.f;
    cgemm_(trans, "No transpose", &n1, nrhs, &n2, &q__1, &a[a_offset], lda, &
	    x[x_offset], ldx, &c_b1, &b[b_offset], ldb)
	    ;

/*     Compute the maximum over the number of right hand sides of   
          norm(B - A*X) / ( max(m,n) * norm(A) * norm(X) * EPS ) . */

    *resid = 0.f;
    i__1 = *nrhs;
    for (j = 1; j <= i__1; ++j) {
	bnorm = scasum_(&n1, &b_ref(1, j), &c__1);
	xnorm = scasum_(&n2, &x_ref(1, j), &c__1);
	if (anorm == 0.f && bnorm == 0.f) {
	    *resid = 0.f;
	} else if (anorm <= 0.f || xnorm <= 0.f) {
	    *resid = 1.f / eps;
	} else {
/* Computing MAX */
	    r__1 = *resid, r__2 = bnorm / anorm / xnorm / (max(*m,*n) * eps);
	    *resid = dmax(r__1,r__2);
	}
/* L10: */
    }

    return 0;

/*     End of CQRT16 */

} /* cqrt16_ */

#undef x_ref
#undef x_subscr
#undef b_ref
#undef b_subscr


