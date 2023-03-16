#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    char srnamt[6];
} srnamc_;

#define srnamc_1 srnamc_

/* Table of constant values */

static complex c_b1 = {-1e10f,-1e10f};
static complex c_b12 = {0.f,0.f};
static complex c_b19 = {-1.f,0.f};
static complex c_b20 = {1.f,0.f};
static real c_b28 = -1.f;
static real c_b29 = 1.f;

/* Subroutine */ int cqlt01_(integer *m, integer *n, complex *a, complex *af, 
	complex *q, complex *l, integer *lda, complex *tau, complex *work, 
	integer *lwork, real *rwork, real *result)
{
    /* System generated locals */
    integer a_dim1, a_offset, af_dim1, af_offset, l_dim1, l_offset, q_dim1, 
	    q_offset, i__1, i__2;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static integer info;
    extern /* Subroutine */ int cgemm_(char *, char *, integer *, integer *, 
	    integer *, complex *, complex *, integer *, complex *, integer *, 
	    complex *, complex *, integer *), cherk_(char *, 
	    char *, integer *, integer *, real *, complex *, integer *, real *
	    , complex *, integer *);
    static real resid, anorm;
    static integer minmn;
    extern doublereal clange_(char *, integer *, integer *, complex *, 
	    integer *, real *);
    extern /* Subroutine */ int cgeqlf_(integer *, integer *, complex *, 
	    integer *, complex *, complex *, integer *, integer *);
    extern doublereal slamch_(char *);
    extern /* Subroutine */ int clacpy_(char *, integer *, integer *, complex 
	    *, integer *, complex *, integer *), claset_(char *, 
	    integer *, integer *, complex *, complex *, complex *, integer *);
    extern doublereal clansy_(char *, char *, integer *, complex *, integer *,
	     real *);
    extern /* Subroutine */ int cungql_(integer *, integer *, integer *, 
	    complex *, integer *, complex *, complex *, integer *, integer *);
    static real eps;


#define l_subscr(a_1,a_2) (a_2)*l_dim1 + a_1
#define l_ref(a_1,a_2) l[l_subscr(a_1,a_2)]
#define q_subscr(a_1,a_2) (a_2)*q_dim1 + a_1
#define q_ref(a_1,a_2) q[q_subscr(a_1,a_2)]
#define af_subscr(a_1,a_2) (a_2)*af_dim1 + a_1
#define af_ref(a_1,a_2) af[af_subscr(a_1,a_2)]


/*  -- LAPACK test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    CQLT01 tests CGEQLF, which computes the QL factorization of an m-by-n   
    matrix A, and partially tests CUNGQL which forms the m-by-m   
    orthogonal matrix Q.   

    CQLT01 compares L with Q'*A, and checks that Q is orthogonal.   

    Arguments   
    =========   

    M       (input) INTEGER   
            The number of rows of the matrix A.  M >= 0.   

    N       (input) INTEGER   
            The number of columns of the matrix A.  N >= 0.   

    A       (input) COMPLEX array, dimension (LDA,N)   
            The m-by-n matrix A.   

    AF      (output) COMPLEX array, dimension (LDA,N)   
            Details of the QL factorization of A, as returned by CGEQLF.   
            See CGEQLF for further details.   

    Q       (output) COMPLEX array, dimension (LDA,M)   
            The m-by-m orthogonal matrix Q.   

    L       (workspace) COMPLEX array, dimension (LDA,max(M,N))   

    LDA     (input) INTEGER   
            The leading dimension of the arrays A, AF, Q and R.   
            LDA >= max(M,N).   

    TAU     (output) COMPLEX array, dimension (min(M,N))   
            The scalar factors of the elementary reflectors, as returned   
            by CGEQLF.   

    WORK    (workspace) COMPLEX array, dimension (LWORK)   

    LWORK   (input) INTEGER   
            The dimension of the array WORK.   

    RWORK   (workspace) REAL array, dimension (M)   

    RESULT  (output) REAL array, dimension (2)   
            The test ratios:   
            RESULT(1) = norm( L - Q'*A ) / ( M * norm(A) * EPS )   
            RESULT(2) = norm( I - Q'*Q ) / ( M * EPS )   

    =====================================================================   


       Parameter adjustments */
    l_dim1 = *lda;
    l_offset = 1 + l_dim1 * 1;
    l -= l_offset;
    q_dim1 = *lda;
    q_offset = 1 + q_dim1 * 1;
    q -= q_offset;
    af_dim1 = *lda;
    af_offset = 1 + af_dim1 * 1;
    af -= af_offset;
    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    --tau;
    --work;
    --rwork;
    --result;

    /* Function Body */
    minmn = min(*m,*n);
    eps = slamch_("Epsilon");

/*     Copy the matrix A to the array AF. */

    clacpy_("Full", m, n, &a[a_offset], lda, &af[af_offset], lda);

/*     Factorize the matrix A in the array AF. */

    s_copy(srnamc_1.srnamt, "CGEQLF", (ftnlen)6, (ftnlen)6);
    cgeqlf_(m, n, &af[af_offset], lda, &tau[1], &work[1], lwork, &info);

/*     Copy details of Q */

    claset_("Full", m, m, &c_b1, &c_b1, &q[q_offset], lda);
    if (*m >= *n) {
	if (*n < *m && *n > 0) {
	    i__1 = *m - *n;
	    clacpy_("Full", &i__1, n, &af[af_offset], lda, &q_ref(1, *m - *n 
		    + 1), lda);
	}
	if (*n > 1) {
	    i__1 = *n - 1;
	    i__2 = *n - 1;
	    clacpy_("Upper", &i__1, &i__2, &af_ref(*m - *n + 1, 2), lda, &
		    q_ref(*m - *n + 1, *m - *n + 2), lda);
	}
    } else {
	if (*m > 1) {
	    i__1 = *m - 1;
	    i__2 = *m - 1;
	    clacpy_("Upper", &i__1, &i__2, &af_ref(1, *n - *m + 2), lda, &
		    q_ref(1, 2), lda);
	}
    }

/*     Generate the m-by-m matrix Q */

    s_copy(srnamc_1.srnamt, "CUNGQL", (ftnlen)6, (ftnlen)6);
    cungql_(m, m, &minmn, &q[q_offset], lda, &tau[1], &work[1], lwork, &info);

/*     Copy L */

    claset_("Full", m, n, &c_b12, &c_b12, &l[l_offset], lda);
    if (*m >= *n) {
	if (*n > 0) {
	    clacpy_("Lower", n, n, &af_ref(*m - *n + 1, 1), lda, &l_ref(*m - *
		    n + 1, 1), lda);
	}
    } else {
	if (*n > *m && *m > 0) {
	    i__1 = *n - *m;
	    clacpy_("Full", m, &i__1, &af[af_offset], lda, &l[l_offset], lda);
	}
	if (*m > 0) {
	    clacpy_("Lower", m, m, &af_ref(1, *n - *m + 1), lda, &l_ref(1, *n 
		    - *m + 1), lda);
	}
    }

/*     Compute L - Q'*A */

    cgemm_("Conjugate transpose", "No transpose", m, n, m, &c_b19, &q[
	    q_offset], lda, &a[a_offset], lda, &c_b20, &l[l_offset], lda);

/*     Compute norm( L - Q'*A ) / ( M * norm(A) * EPS ) . */

    anorm = clange_("1", m, n, &a[a_offset], lda, &rwork[1]);
    resid = clange_("1", m, n, &l[l_offset], lda, &rwork[1]);
    if (anorm > 0.f) {
	result[1] = resid / (real) max(1,*m) / anorm / eps;
    } else {
	result[1] = 0.f;
    }

/*     Compute I - Q'*Q */

    claset_("Full", m, m, &c_b12, &c_b20, &l[l_offset], lda);
    cherk_("Upper", "Conjugate transpose", m, m, &c_b28, &q[q_offset], lda, &
	    c_b29, &l[l_offset], lda);

/*     Compute norm( I - Q'*Q ) / ( M * EPS ) . */

    resid = clansy_("1", "Upper", m, &l[l_offset], lda, &rwork[1]);

    result[2] = resid / (real) max(1,*m) / eps;

    return 0;

/*     End of CQLT01 */

} /* cqlt01_ */

#undef af_ref
#undef af_subscr
#undef q_ref
#undef q_subscr
#undef l_ref
#undef l_subscr


