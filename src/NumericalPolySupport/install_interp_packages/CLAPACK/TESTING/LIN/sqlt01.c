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

static real c_b6 = -1e10f;
static real c_b13 = 0.f;
static real c_b20 = -1.f;
static real c_b21 = 1.f;

/* Subroutine */ int sqlt01_(integer *m, integer *n, real *a, real *af, real *
	q, real *l, integer *lda, real *tau, real *work, integer *lwork, real 
	*rwork, real *result)
{
    /* System generated locals */
    integer a_dim1, a_offset, af_dim1, af_offset, l_dim1, l_offset, q_dim1, 
	    q_offset, i__1, i__2;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static integer info;
    static real resid;
    extern /* Subroutine */ int sgemm_(char *, char *, integer *, integer *, 
	    integer *, real *, real *, integer *, real *, integer *, real *, 
	    real *, integer *);
    static real anorm;
    static integer minmn;
    extern /* Subroutine */ int ssyrk_(char *, char *, integer *, integer *, 
	    real *, real *, integer *, real *, real *, integer *);
    extern doublereal slamch_(char *), slange_(char *, integer *, 
	    integer *, real *, integer *, real *);
    extern /* Subroutine */ int sgeqlf_(integer *, integer *, real *, integer 
	    *, real *, real *, integer *, integer *), slacpy_(char *, integer 
	    *, integer *, real *, integer *, real *, integer *), 
	    slaset_(char *, integer *, integer *, real *, real *, real *, 
	    integer *), sorgql_(integer *, integer *, integer *, real 
	    *, integer *, real *, real *, integer *, integer *);
    extern doublereal slansy_(char *, char *, integer *, real *, integer *, 
	    real *);
    static real eps;


#define l_ref(a_1,a_2) l[(a_2)*l_dim1 + a_1]
#define q_ref(a_1,a_2) q[(a_2)*q_dim1 + a_1]
#define af_ref(a_1,a_2) af[(a_2)*af_dim1 + a_1]


/*  -- LAPACK test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    SQLT01 tests SGEQLF, which computes the QL factorization of an m-by-n   
    matrix A, and partially tests SORGQL which forms the m-by-m   
    orthogonal matrix Q.   

    SQLT01 compares L with Q'*A, and checks that Q is orthogonal.   

    Arguments   
    =========   

    M       (input) INTEGER   
            The number of rows of the matrix A.  M >= 0.   

    N       (input) INTEGER   
            The number of columns of the matrix A.  N >= 0.   

    A       (input) REAL array, dimension (LDA,N)   
            The m-by-n matrix A.   

    AF      (output) REAL array, dimension (LDA,N)   
            Details of the QL factorization of A, as returned by SGEQLF.   
            See SGEQLF for further details.   

    Q       (output) REAL array, dimension (LDA,M)   
            The m-by-m orthogonal matrix Q.   

    L       (workspace) REAL array, dimension (LDA,max(M,N))   

    LDA     (input) INTEGER   
            The leading dimension of the arrays A, AF, Q and R.   
            LDA >= max(M,N).   

    TAU     (output) REAL array, dimension (min(M,N))   
            The scalar factors of the elementary reflectors, as returned   
            by SGEQLF.   

    WORK    (workspace) REAL array, dimension (LWORK)   

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

    slacpy_("Full", m, n, &a[a_offset], lda, &af[af_offset], lda);

/*     Factorize the matrix A in the array AF. */

    s_copy(srnamc_1.srnamt, "SGEQLF", (ftnlen)6, (ftnlen)6);
    sgeqlf_(m, n, &af[af_offset], lda, &tau[1], &work[1], lwork, &info);

/*     Copy details of Q */

    slaset_("Full", m, m, &c_b6, &c_b6, &q[q_offset], lda);
    if (*m >= *n) {
	if (*n < *m && *n > 0) {
	    i__1 = *m - *n;
	    slacpy_("Full", &i__1, n, &af[af_offset], lda, &q_ref(1, *m - *n 
		    + 1), lda);
	}
	if (*n > 1) {
	    i__1 = *n - 1;
	    i__2 = *n - 1;
	    slacpy_("Upper", &i__1, &i__2, &af_ref(*m - *n + 1, 2), lda, &
		    q_ref(*m - *n + 1, *m - *n + 2), lda);
	}
    } else {
	if (*m > 1) {
	    i__1 = *m - 1;
	    i__2 = *m - 1;
	    slacpy_("Upper", &i__1, &i__2, &af_ref(1, *n - *m + 2), lda, &
		    q_ref(1, 2), lda);
	}
    }

/*     Generate the m-by-m matrix Q */

    s_copy(srnamc_1.srnamt, "SORGQL", (ftnlen)6, (ftnlen)6);
    sorgql_(m, m, &minmn, &q[q_offset], lda, &tau[1], &work[1], lwork, &info);

/*     Copy L */

    slaset_("Full", m, n, &c_b13, &c_b13, &l[l_offset], lda);
    if (*m >= *n) {
	if (*n > 0) {
	    slacpy_("Lower", n, n, &af_ref(*m - *n + 1, 1), lda, &l_ref(*m - *
		    n + 1, 1), lda);
	}
    } else {
	if (*n > *m && *m > 0) {
	    i__1 = *n - *m;
	    slacpy_("Full", m, &i__1, &af[af_offset], lda, &l[l_offset], lda);
	}
	if (*m > 0) {
	    slacpy_("Lower", m, m, &af_ref(1, *n - *m + 1), lda, &l_ref(1, *n 
		    - *m + 1), lda);
	}
    }

/*     Compute L - Q'*A */

    sgemm_("Transpose", "No transpose", m, n, m, &c_b20, &q[q_offset], lda, &
	    a[a_offset], lda, &c_b21, &l[l_offset], lda);

/*     Compute norm( L - Q'*A ) / ( M * norm(A) * EPS ) . */

    anorm = slange_("1", m, n, &a[a_offset], lda, &rwork[1]);
    resid = slange_("1", m, n, &l[l_offset], lda, &rwork[1]);
    if (anorm > 0.f) {
	result[1] = resid / (real) max(1,*m) / anorm / eps;
    } else {
	result[1] = 0.f;
    }

/*     Compute I - Q'*Q */

    slaset_("Full", m, m, &c_b13, &c_b21, &l[l_offset], lda);
    ssyrk_("Upper", "Transpose", m, m, &c_b20, &q[q_offset], lda, &c_b21, &l[
	    l_offset], lda);

/*     Compute norm( I - Q'*Q ) / ( M * EPS ) . */

    resid = slansy_("1", "Upper", m, &l[l_offset], lda, &rwork[1]);

    result[2] = resid / (real) max(1,*m) / eps;

    return 0;

/*     End of SQLT01 */

} /* sqlt01_ */

#undef af_ref
#undef q_ref
#undef l_ref


