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

static doublecomplex c_b1 = {-1e10,-1e10};
static doublecomplex c_b9 = {0.,0.};
static doublecomplex c_b14 = {-1.,0.};
static doublecomplex c_b15 = {1.,0.};
static doublereal c_b23 = -1.;
static doublereal c_b24 = 1.;

/* Subroutine */ int zqlt02_(integer *m, integer *n, integer *k, 
	doublecomplex *a, doublecomplex *af, doublecomplex *q, doublecomplex *
	l, integer *lda, doublecomplex *tau, doublecomplex *work, integer *
	lwork, doublereal *rwork, doublereal *result)
{
    /* System generated locals */
    integer a_dim1, a_offset, af_dim1, af_offset, l_dim1, l_offset, q_dim1, 
	    q_offset, i__1, i__2;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static integer info;
    static doublereal resid, anorm;
    extern /* Subroutine */ int zgemm_(char *, char *, integer *, integer *, 
	    integer *, doublecomplex *, doublecomplex *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, doublecomplex *, 
	    integer *), zherk_(char *, char *, integer *, 
	    integer *, doublereal *, doublecomplex *, integer *, doublereal *,
	     doublecomplex *, integer *);
    extern doublereal dlamch_(char *), zlange_(char *, integer *, 
	    integer *, doublecomplex *, integer *, doublereal *);
    extern /* Subroutine */ int zlacpy_(char *, integer *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *), 
	    zlaset_(char *, integer *, integer *, doublecomplex *, 
	    doublecomplex *, doublecomplex *, integer *);
    extern doublereal zlansy_(char *, char *, integer *, doublecomplex *, 
	    integer *, doublereal *);
    extern /* Subroutine */ int zungql_(integer *, integer *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, doublecomplex *, 
	    integer *, integer *);
    static doublereal eps;


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

    ZQLT02 tests ZUNGQL, which generates an m-by-n matrix Q with   
    orthonornmal columns that is defined as the product of k elementary   
    reflectors.   

    Given the QL factorization of an m-by-n matrix A, ZQLT02 generates   
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

    A       (input) COMPLEX*16 array, dimension (LDA,N)   
            The m-by-n matrix A which was factorized by ZQLT01.   

    AF      (input) COMPLEX*16 array, dimension (LDA,N)   
            Details of the QL factorization of A, as returned by ZGEQLF.   
            See ZGEQLF for further details.   

    Q       (workspace) COMPLEX*16 array, dimension (LDA,N)   

    L       (workspace) COMPLEX*16 array, dimension (LDA,N)   

    LDA     (input) INTEGER   
            The leading dimension of the arrays A, AF, Q and L. LDA >= M.   

    TAU     (input) COMPLEX*16 array, dimension (N)   
            The scalar factors of the elementary reflectors corresponding   
            to the QL factorization in AF.   

    WORK    (workspace) COMPLEX*16 array, dimension (LWORK)   

    LWORK   (input) INTEGER   
            The dimension of the array WORK.   

    RWORK   (workspace) DOUBLE PRECISION array, dimension (M)   

    RESULT  (output) DOUBLE PRECISION array, dimension (2)   
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
	result[1] = 0.;
	result[2] = 0.;
	return 0;
    }

    eps = dlamch_("Epsilon");

/*     Copy the last k columns of the factorization to the array Q */

    zlaset_("Full", m, n, &c_b1, &c_b1, &q[q_offset], lda);
    if (*k < *m) {
	i__1 = *m - *k;
	zlacpy_("Full", &i__1, k, &af_ref(1, *n - *k + 1), lda, &q_ref(1, *n 
		- *k + 1), lda);
    }
    if (*k > 1) {
	i__1 = *k - 1;
	i__2 = *k - 1;
	zlacpy_("Upper", &i__1, &i__2, &af_ref(*m - *k + 1, *n - *k + 2), lda,
		 &q_ref(*m - *k + 1, *n - *k + 2), lda);
    }

/*     Generate the last n columns of the matrix Q */

    s_copy(srnamc_1.srnamt, "ZUNGQL", (ftnlen)6, (ftnlen)6);
    zungql_(m, n, k, &q[q_offset], lda, &tau[*n - *k + 1], &work[1], lwork, &
	    info);

/*     Copy L(m-n+1:m,n-k+1:n) */

    zlaset_("Full", n, k, &c_b9, &c_b9, &l_ref(*m - *n + 1, *n - *k + 1), lda);
    zlacpy_("Lower", k, k, &af_ref(*m - *k + 1, *n - *k + 1), lda, &l_ref(*m 
	    - *k + 1, *n - *k + 1), lda);

/*     Compute L(m-n+1:m,n-k+1:n) - Q(1:m,m-n+1:m)' * A(1:m,n-k+1:n) */

    zgemm_("Conjugate transpose", "No transpose", n, k, m, &c_b14, &q[
	    q_offset], lda, &a_ref(1, *n - *k + 1), lda, &c_b15, &l_ref(*m - *
	    n + 1, *n - *k + 1), lda);

/*     Compute norm( L - Q'*A ) / ( M * norm(A) * EPS ) . */

    anorm = zlange_("1", m, k, &a_ref(1, *n - *k + 1), lda, &rwork[1]);
    resid = zlange_("1", n, k, &l_ref(*m - *n + 1, *n - *k + 1), lda, &rwork[
	    1]);
    if (anorm > 0.) {
	result[1] = resid / (doublereal) max(1,*m) / anorm / eps;
    } else {
	result[1] = 0.;
    }

/*     Compute I - Q'*Q */

    zlaset_("Full", n, n, &c_b9, &c_b15, &l[l_offset], lda);
    zherk_("Upper", "Conjugate transpose", n, m, &c_b23, &q[q_offset], lda, &
	    c_b24, &l[l_offset], lda);

/*     Compute norm( I - Q'*Q ) / ( M * EPS ) . */

    resid = zlansy_("1", "Upper", n, &l[l_offset], lda, &rwork[1]);

    result[2] = resid / (doublereal) max(1,*m) / eps;

    return 0;

/*     End of ZQLT02 */

} /* zqlt02_ */

#undef af_ref
#undef af_subscr
#undef q_ref
#undef q_subscr
#undef l_ref
#undef l_subscr
#undef a_ref
#undef a_subscr


