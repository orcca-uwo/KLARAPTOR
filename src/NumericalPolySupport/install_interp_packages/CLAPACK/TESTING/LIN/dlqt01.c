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

static doublereal c_b6 = -1e10;
static doublereal c_b11 = 0.;
static doublereal c_b16 = -1.;
static doublereal c_b17 = 1.;

/* Subroutine */ int dlqt01_(integer *m, integer *n, doublereal *a, 
	doublereal *af, doublereal *q, doublereal *l, integer *lda, 
	doublereal *tau, doublereal *work, integer *lwork, doublereal *rwork, 
	doublereal *result)
{
    /* System generated locals */
    integer a_dim1, a_offset, af_dim1, af_offset, l_dim1, l_offset, q_dim1, 
	    q_offset, i__1;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static integer info;
    extern /* Subroutine */ int dgemm_(char *, char *, integer *, integer *, 
	    integer *, doublereal *, doublereal *, integer *, doublereal *, 
	    integer *, doublereal *, doublereal *, integer *);
    static doublereal resid, anorm;
    static integer minmn;
    extern /* Subroutine */ int dsyrk_(char *, char *, integer *, integer *, 
	    doublereal *, doublereal *, integer *, doublereal *, doublereal *,
	     integer *);
    extern doublereal dlamch_(char *), dlange_(char *, integer *, 
	    integer *, doublereal *, integer *, doublereal *);
    extern /* Subroutine */ int dgelqf_(integer *, integer *, doublereal *, 
	    integer *, doublereal *, doublereal *, integer *, integer *), 
	    dlacpy_(char *, integer *, integer *, doublereal *, integer *, 
	    doublereal *, integer *), dlaset_(char *, integer *, 
	    integer *, doublereal *, doublereal *, doublereal *, integer *), dorglq_(integer *, integer *, integer *, doublereal *, 
	    integer *, doublereal *, doublereal *, integer *, integer *);
    extern doublereal dlansy_(char *, char *, integer *, doublereal *, 
	    integer *, doublereal *);
    static doublereal eps;


#define q_ref(a_1,a_2) q[(a_2)*q_dim1 + a_1]
#define af_ref(a_1,a_2) af[(a_2)*af_dim1 + a_1]


/*  -- LAPACK test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    DLQT01 tests DGELQF, which computes the LQ factorization of an m-by-n   
    matrix A, and partially tests DORGLQ which forms the n-by-n   
    orthogonal matrix Q.   

    DLQT01 compares L with A*Q', and checks that Q is orthogonal.   

    Arguments   
    =========   

    M       (input) INTEGER   
            The number of rows of the matrix A.  M >= 0.   

    N       (input) INTEGER   
            The number of columns of the matrix A.  N >= 0.   

    A       (input) DOUBLE PRECISION array, dimension (LDA,N)   
            The m-by-n matrix A.   

    AF      (output) DOUBLE PRECISION array, dimension (LDA,N)   
            Details of the LQ factorization of A, as returned by DGELQF.   
            See DGELQF for further details.   

    Q       (output) DOUBLE PRECISION array, dimension (LDA,N)   
            The n-by-n orthogonal matrix Q.   

    L       (workspace) DOUBLE PRECISION array, dimension (LDA,max(M,N))   

    LDA     (input) INTEGER   
            The leading dimension of the arrays A, AF, Q and L.   
            LDA >= max(M,N).   

    TAU     (output) DOUBLE PRECISION array, dimension (min(M,N))   
            The scalar factors of the elementary reflectors, as returned   
            by DGELQF.   

    WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK)   

    LWORK   (input) INTEGER   
            The dimension of the array WORK.   

    RWORK   (workspace) DOUBLE PRECISION array, dimension (max(M,N))   

    RESULT  (output) DOUBLE PRECISION array, dimension (2)   
            The test ratios:   
            RESULT(1) = norm( L - A*Q' ) / ( N * norm(A) * EPS )   
            RESULT(2) = norm( I - Q*Q' ) / ( N * EPS )   

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
    eps = dlamch_("Epsilon");

/*     Copy the matrix A to the array AF. */

    dlacpy_("Full", m, n, &a[a_offset], lda, &af[af_offset], lda);

/*     Factorize the matrix A in the array AF. */

    s_copy(srnamc_1.srnamt, "DGELQF", (ftnlen)6, (ftnlen)6);
    dgelqf_(m, n, &af[af_offset], lda, &tau[1], &work[1], lwork, &info);

/*     Copy details of Q */

    dlaset_("Full", n, n, &c_b6, &c_b6, &q[q_offset], lda);
    if (*n > 1) {
	i__1 = *n - 1;
	dlacpy_("Upper", m, &i__1, &af_ref(1, 2), lda, &q_ref(1, 2), lda);
    }

/*     Generate the n-by-n matrix Q */

    s_copy(srnamc_1.srnamt, "DORGLQ", (ftnlen)6, (ftnlen)6);
    dorglq_(n, n, &minmn, &q[q_offset], lda, &tau[1], &work[1], lwork, &info);

/*     Copy L */

    dlaset_("Full", m, n, &c_b11, &c_b11, &l[l_offset], lda);
    dlacpy_("Lower", m, n, &af[af_offset], lda, &l[l_offset], lda);

/*     Compute L - A*Q' */

    dgemm_("No transpose", "Transpose", m, n, n, &c_b16, &a[a_offset], lda, &
	    q[q_offset], lda, &c_b17, &l[l_offset], lda);

/*     Compute norm( L - Q'*A ) / ( N * norm(A) * EPS ) . */

    anorm = dlange_("1", m, n, &a[a_offset], lda, &rwork[1]);
    resid = dlange_("1", m, n, &l[l_offset], lda, &rwork[1]);
    if (anorm > 0.) {
	result[1] = resid / (doublereal) max(1,*n) / anorm / eps;
    } else {
	result[1] = 0.;
    }

/*     Compute I - Q*Q' */

    dlaset_("Full", n, n, &c_b11, &c_b17, &l[l_offset], lda);
    dsyrk_("Upper", "No transpose", n, n, &c_b16, &q[q_offset], lda, &c_b17, &
	    l[l_offset], lda);

/*     Compute norm( I - Q*Q' ) / ( N * EPS ) . */

    resid = dlansy_("1", "Upper", n, &l[l_offset], lda, &rwork[1]);

    result[2] = resid / (doublereal) max(1,*n) / eps;

    return 0;

/*     End of DLQT01 */

} /* dlqt01_ */

#undef af_ref
#undef q_ref


