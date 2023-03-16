#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublecomplex c_b1 = {0.,0.};
static doublecomplex c_b2 = {1.,0.};

/* Subroutine */ int zgerqs_(integer *m, integer *n, integer *nrhs, 
	doublecomplex *a, integer *lda, doublecomplex *tau, doublecomplex *b, 
	integer *ldb, doublecomplex *work, integer *lwork, integer *info)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, i__1;

    /* Local variables */
    extern /* Subroutine */ int ztrsm_(char *, char *, char *, char *, 
	    integer *, integer *, doublecomplex *, doublecomplex *, integer *,
	     doublecomplex *, integer *), 
	    xerbla_(char *, integer *), zlaset_(char *, integer *, 
	    integer *, doublecomplex *, doublecomplex *, doublecomplex *, 
	    integer *), zunmrq_(char *, char *, integer *, integer *, 
	    integer *, doublecomplex *, integer *, doublecomplex *, 
	    doublecomplex *, integer *, doublecomplex *, integer *, integer *);


#define a_subscr(a_1,a_2) (a_2)*a_dim1 + a_1
#define a_ref(a_1,a_2) a[a_subscr(a_1,a_2)]
#define b_subscr(a_1,a_2) (a_2)*b_dim1 + a_1
#define b_ref(a_1,a_2) b[b_subscr(a_1,a_2)]


/*  -- LAPACK routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    Compute a minimum-norm solution   
        min || A*X - B ||   
    using the RQ factorization   
        A = R*Q   
    computed by ZGERQF.   

    Arguments   
    =========   

    M       (input) INTEGER   
            The number of rows of the matrix A.  M >= 0.   

    N       (input) INTEGER   
            The number of columns of the matrix A.  N >= M >= 0.   

    NRHS    (input) INTEGER   
            The number of columns of B.  NRHS >= 0.   

    A       (input) COMPLEX*16 array, dimension (LDA,N)   
            Details of the RQ factorization of the original matrix A as   
            returned by ZGERQF.   

    LDA     (input) INTEGER   
            The leading dimension of the array A.  LDA >= M.   

    TAU     (input) COMPLEX*16 array, dimension (M)   
            Details of the orthogonal matrix Q.   

    B       (input/output) COMPLEX*16 array, dimension (LDB,NRHS)   
            On entry, the right hand side vectors for the linear system.   
            On exit, the solution vectors X.  Each solution vector   
            is contained in rows 1:N of a column of B.   

    LDB     (input) INTEGER   
            The leading dimension of the array B. LDB >= max(1,N).   

    WORK    (workspace) COMPLEX*16 array, dimension (LWORK)   

    LWORK   (input) INTEGER   
            The length of the array WORK.  LWORK must be at least NRHS,   
            and should be at least NRHS*NB, where NB is the block size   
            for this environment.   

    INFO    (output) INTEGER   
            = 0: successful exit   
            < 0: if INFO = -i, the i-th argument had an illegal value   

    =====================================================================   


       Test the input parameters.   

       Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    --tau;
    b_dim1 = *ldb;
    b_offset = 1 + b_dim1 * 1;
    b -= b_offset;
    --work;

    /* Function Body */
    *info = 0;
    if (*m < 0) {
	*info = -1;
    } else if (*n < 0 || *m > *n) {
	*info = -2;
    } else if (*nrhs < 0) {
	*info = -3;
    } else if (*lda < max(1,*m)) {
	*info = -5;
    } else if (*ldb < max(1,*n)) {
	*info = -8;
    } else if (*lwork < 1 || *lwork < *nrhs && *m > 0 && *n > 0) {
	*info = -10;
    }
    if (*info != 0) {
	i__1 = -(*info);
	xerbla_("ZGERQS", &i__1);
	return 0;
    }

/*     Quick return if possible */

    if (*n == 0 || *nrhs == 0 || *m == 0) {
	return 0;
    }

/*     Solve R*X = B(n-m+1:n,:) */

    ztrsm_("Left", "Upper", "No transpose", "Non-unit", m, nrhs, &c_b2, &
	    a_ref(1, *n - *m + 1), lda, &b_ref(*n - *m + 1, 1), ldb);

/*     Set B(1:n-m,:) to zero */

    i__1 = *n - *m;
    zlaset_("Full", &i__1, nrhs, &c_b1, &c_b1, &b[b_offset], ldb);

/*     B := Q' * B */

    zunmrq_("Left", "Conjugate transpose", n, nrhs, m, &a[a_offset], lda, &
	    tau[1], &b[b_offset], ldb, &work[1], lwork, info);

    return 0;

/*     End of ZGERQS */

} /* zgerqs_ */

#undef b_ref
#undef b_subscr
#undef a_ref
#undef a_subscr


