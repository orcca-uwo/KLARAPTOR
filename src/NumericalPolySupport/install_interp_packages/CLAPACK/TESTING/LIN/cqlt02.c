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
static complex c_b9 = {0.f,0.f};
static complex c_b14 = {-1.f,0.f};
static complex c_b15 = {1.f,0.f};
static real c_b23 = -1.f;
static real c_b24 = 1.f;

/* Subroutine */ int cqlt02_(integer *m, integer *n, integer *k, complex *a, 
	complex *af, complex *q, complex *l, integer *lda, complex *tau, 
	complex *work, integer *lwork, real *rwork, real *result)
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
    extern doublereal clange_(char *, integer *, integer *, complex *, 
	    integer *, real *), slamch_(char *);
    extern /* Subroutine */ int clacpy_(char *, integer *, integer *, complex 
	    *, integer *, complex *, integer *), claset_(char *, 
	    integer *, integer *, complex *, complex *, complex *, integer *);
    extern doublereal clansy_(char *, char *, integer *, complex *, integer *,
	     real *);
    extern /* Subroutine */ int cungql_(integer *, integer *, integer *, 
	    complex *, integer *, complex *, complex *, integer *, integer *);
    static real eps;


#define a_subscr(a_1,a_2) (a_2)*a_dim1 + a_1
#define a_ref(a_1,a_2) a[a_subscr(a_1,a_2)]
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

    CQLT02 tests CUNGQL, which generates an m-by-n matrix Q with   
    orthonornmal columns that is defined as the product of k elementary   
    reflectors.   

    Given the QL factorization of an m-by-n matrix A, CQLT02 generates   
    the orthogonal matrix Q defined by the factorization of the last k   
    columns of A; it compares L(m-n+1:m,n-k+1:n) with   
    Q(1:m,m-n+1:m)'*A(1:m,n-k+1:n), and checks that the columns of Q are   
    orthonormal.   

    Arguments   
    =========   

    M       (input) INTEGER   
            The number of rows of the matrix Q to be generated.  M >= 0.   

    N       (input) INTEGER   
            The number of columns of the matrix Q to be generated.   
            M >= N >= 0.   

    K       (input) INTEGER   
            The number of elementary reflectors whose product defines the   
            matrix Q. N >= K >= 0.   

    A       (input) COMPLEX array, dimension (LDA,N)   
            The m-by-n matrix A which was factorized by CQLT01.   

    AF      (input) COMPLEX array, dimension (LDA,N)   
            Details of the QL factorization of A, as returned by CGEQLF.   
            See CGEQLF for further details.   

    Q       (workspace) COMPLEX array, dimension (LDA,N)   

    L       (workspace) COMPLEX array, dimension (LDA,N)   

    LDA     (input) INTEGER   
            The leading dimension of the arrays A, AF, Q and L. LDA >= M.   

    TAU     (input) COMPLEX array, dimension (N)   
            The scalar factors of the elementary reflectors corresponding   
            to the QL factorization in AF.   

    WORK    (workspace) COMPLEX array, dimension (LWORK)   

    LWORK   (input) INTEGER   
            The dimension of the array WORK.   

    RWORK   (workspace) REAL array, dimension (M)   

    RESULT  (output) REAL array, dimension (2)   
            The test ratios:   
            RESULT(1) = norm( L - Q'*A ) / ( M * norm(A) * EPS )   
            RESULT(2) = norm( I - Q'*Q ) / ( M * EPS )   

    =====================================================================   


       Quick return if possible   

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
    if (*m == 0 || *n == 0 || *k == 0) {
	result[1] = 0.f;
	result[2] = 0.f;
	return 0;
    }

    eps = slamch_("Epsilon");

/*     Copy the last k columns of the factorization to the array Q */

    claset_("Full", m, n, &c_b1, &c_b1, &q[q_offset], lda);
    if (*k < *m) {
	i__1 = *m - *k;
	clacpy_("Full", &i__1, k, &af_ref(1, *n - *k + 1), lda, &q_ref(1, *n 
		- *k + 1), lda);
    }
    if (*k > 1) {
	i__1 = *k - 1;
	i__2 = *k - 1;
	clacpy_("Upper", &i__1, &i__2, &af_ref(*m - *k + 1, *n - *k + 2), lda,
		 &q_ref(*m - *k + 1, *n - *k + 2), lda);
    }

/*     Generate the last n columns of the matrix Q */

    s_copy(srnamc_1.srnamt, "CUNGQL", (ftnlen)6, (ftnlen)6);
    cungql_(m, n, k, &q[q_offset], lda, &tau[*n - *k + 1], &work[1], lwork, &
	    info);

/*     Copy L(m-n+1:m,n-k+1:n) */

    claset_("Full", n, k, &c_b9, &c_b9, &l_ref(*m - *n + 1, *n - *k + 1), lda);
    clacpy_("Lower", k, k, &af_ref(*m - *k + 1, *n - *k + 1), lda, &l_ref(*m 
	    - *k + 1, *n - *k + 1), lda);

/*     Compute L(m-n+1:m,n-k+1:n) - Q(1:m,m-n+1:m)' * A(1:m,n-k+1:n) */

    cgemm_("Conjugate transpose", "No transpose", n, k, m, &c_b14, &q[
	    q_offset], lda, &a_ref(1, *n - *k + 1), lda, &c_b15, &l_ref(*m - *
	    n + 1, *n - *k + 1), lda);

/*     Compute norm( L - Q'*A ) / ( M * norm(A) * EPS ) . */

    anorm = clange_("1", m, k, &a_ref(1, *n - *k + 1), lda, &rwork[1]);
    resid = clange_("1", n, k, &l_ref(*m - *n + 1, *n - *k + 1), lda, &rwork[
	    1]);
    if (anorm > 0.f) {
	result[1] = resid / (real) max(1,*m) / anorm / eps;
    } else {
	result[1] = 0.f;
    }

/*     Compute I - Q'*Q */

    claset_("Full", n, n, &c_b9, &c_b15, &l[l_offset], lda);
    cherk_("Upper", "Conjugate transpose", n, m, &c_b23, &q[q_offset], lda, &
	    c_b24, &l[l_offset], lda);

/*     Compute norm( I - Q'*Q ) / ( M * EPS ) . */

    resid = clansy_("1", "Upper", n, &l[l_offset], lda, &rwork[1]);

    result[2] = resid / (real) max(1,*m) / eps;

    return 0;

/*     End of CQLT02 */

} /* cqlt02_ */

#undef af_ref
#undef af_subscr
#undef q_ref
#undef q_subscr
#undef l_ref
#undef l_subscr
#undef a_ref
#undef a_subscr


