#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static complex c_b1 = {1.f,0.f};
static integer c__1 = 1;

/* Subroutine */ int cgbt02_(char *trans, integer *m, integer *n, integer *kl,
	 integer *ku, integer *nrhs, complex *a, integer *lda, complex *x, 
	integer *ldx, complex *b, integer *ldb, real *resid)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, x_dim1, x_offset, i__1, i__2, 
	    i__3;
    real r__1, r__2;
    complex q__1;

    /* Local variables */
    static integer j;
    extern /* Subroutine */ int cgbmv_(char *, integer *, integer *, integer *
	    , integer *, complex *, complex *, integer *, complex *, integer *
	    , complex *, complex *, integer *);
    extern logical lsame_(char *, char *);
    static real anorm, bnorm;
    static integer i1, i2, n1;
    static real xnorm;
    static integer kd;
    extern doublereal slamch_(char *), scasum_(integer *, complex *, 
	    integer *);
    static real eps;


#define a_subscr(a_1,a_2) (a_2)*a_dim1 + a_1
#define a_ref(a_1,a_2) a[a_subscr(a_1,a_2)]
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

    CGBT02 computes the residual for a solution of a banded system of   
    equations  A*x = b  or  A'*x = b:   
       RESID = norm( B - A*X ) / ( norm(A) * norm(X) * EPS).   
    where EPS is the machine precision.   

    Arguments   
    =========   

    TRANS   (input) CHARACTER*1   
            Specifies the form of the system of equations:   
            = 'N':  A *x = b   
            = 'T':  A'*x = b, where A' is the transpose of A   
            = 'C':  A'*x = b, where A' is the transpose of A   

    M       (input) INTEGER   
            The number of rows of the matrix A.  M >= 0.   

    N       (input) INTEGER   
            The number of columns of the matrix A.  N >= 0.   

    KL      (input) INTEGER   
            The number of subdiagonals within the band of A.  KL >= 0.   

    KU      (input) INTEGER   
            The number of superdiagonals within the band of A.  KU >= 0.   

    NRHS    (input) INTEGER   
            The number of columns of B.  NRHS >= 0.   

    A       (input) COMPLEX array, dimension (LDA,N)   
            The original matrix A in band storage, stored in rows 1 to   
            KL+KU+1.   

    LDA     (input) INTEGER   
            The leading dimension of the array A.  LDA >= max(1,KL+KU+1).   

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

    RESID   (output) REAL   
            The maximum over the number of right hand sides of   
            norm(B - A*X) / ( norm(A) * norm(X) * EPS ).   

    =====================================================================   


       Quick return if N = 0 pr NRHS = 0   

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

    /* Function Body */
    if (*m <= 0 || *n <= 0 || *nrhs <= 0) {
	*resid = 0.f;
	return 0;
    }

/*     Exit with RESID = 1/EPS if ANORM = 0. */

    eps = slamch_("Epsilon");
    kd = *ku + 1;
    anorm = 0.f;
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/* Computing MAX */
	i__2 = kd + 1 - j;
	i1 = max(i__2,1);
/* Computing MIN */
	i__2 = kd + *m - j, i__3 = *kl + kd;
	i2 = min(i__2,i__3);
/* Computing MAX */
	i__2 = i2 - i1 + 1;
	r__1 = anorm, r__2 = scasum_(&i__2, &a_ref(i1, j), &c__1);
	anorm = dmax(r__1,r__2);
/* L10: */
    }
    if (anorm <= 0.f) {
	*resid = 1.f / eps;
	return 0;
    }

    if (lsame_(trans, "T") || lsame_(trans, "C")) {
	n1 = *n;
    } else {
	n1 = *m;
    }

/*     Compute  B - A*X (or  B - A'*X ) */

    i__1 = *nrhs;
    for (j = 1; j <= i__1; ++j) {
	q__1.r = -1.f, q__1.i = 0.f;
	cgbmv_(trans, m, n, kl, ku, &q__1, &a[a_offset], lda, &x_ref(1, j), &
		c__1, &c_b1, &b_ref(1, j), &c__1);
/* L20: */
    }

/*     Compute the maximum over the number of right hand sides of   
          norm(B - A*X) / ( norm(A) * norm(X) * EPS ). */

    *resid = 0.f;
    i__1 = *nrhs;
    for (j = 1; j <= i__1; ++j) {
	bnorm = scasum_(&n1, &b_ref(1, j), &c__1);
	xnorm = scasum_(&n1, &x_ref(1, j), &c__1);
	if (xnorm <= 0.f) {
	    *resid = 1.f / eps;
	} else {
/* Computing MAX */
	    r__1 = *resid, r__2 = bnorm / anorm / xnorm / eps;
	    *resid = dmax(r__1,r__2);
	}
/* L30: */
    }

    return 0;

/*     End of CGBT02 */

} /* cgbt02_ */

#undef x_ref
#undef x_subscr
#undef b_ref
#undef b_subscr
#undef a_ref
#undef a_subscr


