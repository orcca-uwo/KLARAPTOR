#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b6 = 0.;
static doublereal c_b7 = 1.;

/* Subroutine */ int dlaqzh_(logical *ilq, logical *ilz, integer *n, integer *
	ilo, integer *ihi, doublereal *a, integer *lda, doublereal *b, 
	integer *ldb, doublereal *q, integer *ldq, doublereal *z__, integer *
	ldz, doublereal *work, integer *info)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, q_dim1, q_offset, z_dim1, 
	    z_offset, i__1, i__2;

    /* Local variables */
    static integer iinfo, icols;
    static char compq[1], compz[1];
    static integer irows;
    extern /* Subroutine */ int dgghrd_(char *, char *, integer *, integer *, 
	    integer *, doublereal *, integer *, doublereal *, integer *, 
	    doublereal *, integer *, doublereal *, integer *, integer *), dgeqrf_(integer *, integer *, doublereal *, 
	    integer *, doublereal *, doublereal *, integer *, integer *), 
	    dlacpy_(char *, integer *, integer *, doublereal *, integer *, 
	    doublereal *, integer *), dlaset_(char *, integer *, 
	    integer *, doublereal *, doublereal *, doublereal *, integer *), dorgqr_(integer *, integer *, integer *, doublereal *, 
	    integer *, doublereal *, doublereal *, integer *, integer *), 
	    dormqr_(char *, char *, integer *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, doublereal *, integer *, 
	    doublereal *, integer *, integer *);


#define a_ref(a_1,a_2) a[(a_2)*a_dim1 + a_1]
#define b_ref(a_1,a_2) b[(a_2)*b_dim1 + a_1]
#define q_ref(a_1,a_2) q[(a_2)*q_dim1 + a_1]


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    This calls the LAPACK routines to perform the function of   
    QZHES.  It is similar in function to DGGHRD, except that   
    B is not assumed to be upper-triangular.   

    It reduces a pair of matrices (A,B) to a Hessenberg-triangular   
    pair (H,T).  More specifically, it computes orthogonal matrices   
    Q and Z, an (upper) Hessenberg matrix H, and an upper triangular   
    matrix T such that:   

      A = Q H Z'    and   B = Q T Z'   


    Arguments   
    =========   

    ILQ     (input) LOGICAL   
            = .FALSE. do not compute Q.   
            = .TRUE.  compute Q.   

    ILZ     (input) LOGICAL   
            = .FALSE. do not compute Z.   
            = .TRUE.  compute Z.   

    N       (input) INTEGER   
            The number of rows and columns in the matrices A, B, Q, and   
            Z.  N must be at least 0.   

    ILO     (input) INTEGER   
            Columns 1 through ILO-1 of A and B are assumed to be in   
            upper triangular form already, and will not be modified.   
            ILO must be at least 1.   

    IHI     (input) INTEGER   
            Rows IHI+1 through N of A and B are assumed to be in upper   
            triangular form already, and will not be touched.  IHI may   
            not be greater than N.   

    A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)   
            On entry, the first of the pair of N x N general matrices to   
            be reduced.   
            On exit, the upper triangle and the first subdiagonal of A   
            are overwritten with the Hessenberg matrix H, and the rest   
            is set to zero.   

    LDA     (input) INTEGER   
            The leading dimension of A as declared in the calling   
            program. LDA must be at least max ( 1, N ) .   

    B       (input/output) DOUBLE PRECISION array, dimension (LDB, N)   
            On entry, the second of the pair of N x N general matrices to   
            be reduced.   
            On exit, the transformed matrix T = Q' B Z, which is upper   
            triangular.   

    LDB     (input) INTEGER   
            The leading dimension of B as declared in the calling   
            program. LDB must be at least max ( 1, N ) .   

    Q       (output) DOUBLE PRECISION array, dimension (LDQ,N)   
            If ILQ = .TRUE., Q will contain the orthogonal matrix Q.   
            (See "Purpose", above.)   
            Will not be referenced if ILQ = .FALSE.   

    LDQ     (input) INTEGER   
            The leading dimension of the matrix Q. LDQ must be at   
            least 1 and at least N.   

    Z       (output) DOUBLE PRECISION array, dimension (LDZ,N)   
            If ILZ = .TRUE., Z will contain the orthogonal matrix Z.   
            (See "Purpose", above.)   
            May be referenced even if ILZ = .FALSE.   

    LDZ     (input) INTEGER   
            The leading dimension of the matrix Z. LDZ must be at   
            least 1 and at least N.   

    WORK    (workspace) DOUBLE PRECISION array, dimension (N)   
            Workspace.   

    INFO    (output) INTEGER   
            = 0:  successful exit.   
            < 0:  if INFO = -i, the i-th argument had an illegal value.   
            > 0:  errors that usually indicate LAPACK problems:   
                  = 2: error return from DGEQRF;   
                  = 3: error return from DORMQR;   
                  = 4: error return from DORGQR;   
                  = 5: error return from DGGHRD.   

    =====================================================================   


       Quick return if possible   

       Parameter adjustments */
    --work;
    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    b_dim1 = *ldb;
    b_offset = 1 + b_dim1 * 1;
    b -= b_offset;
    q_dim1 = *ldq;
    q_offset = 1 + q_dim1 * 1;
    q -= q_offset;
    z_dim1 = *ldz;
    z_offset = 1 + z_dim1 * 1;
    z__ -= z_offset;

    /* Function Body */
    if (*n == 0) {
	return 0;
    }

/*     Reduce B to triangular form, and initialize Q and/or Z */

    irows = *ihi + 1 - *ilo;
    icols = *n + 1 - *ilo;
    i__1 = *n * *ldz;
    dgeqrf_(&irows, &icols, &b_ref(*ilo, *ilo), ldb, &work[1], &z__[z_offset],
	     &i__1, &iinfo);
    if (iinfo != 0) {
	*info = 2;
	goto L10;
    }

    i__1 = *n * *ldz;
    dormqr_("L", "T", &irows, &icols, &irows, &b_ref(*ilo, *ilo), ldb, &work[
	    1], &a_ref(*ilo, *ilo), lda, &z__[z_offset], &i__1, &iinfo);
    if (iinfo != 0) {
	*info = 3;
	goto L10;
    }

    if (*ilq) {
	dlaset_("Full", n, n, &c_b6, &c_b7, &q[q_offset], ldq);
	i__1 = irows - 1;
	i__2 = irows - 1;
	dlacpy_("L", &i__1, &i__2, &b_ref(*ilo + 1, *ilo), ldb, &q_ref(*ilo + 
		1, *ilo), ldq);
	i__1 = *n * *ldz;
	dorgqr_(&irows, &irows, &irows, &q_ref(*ilo, *ilo), ldq, &work[1], &
		z__[z_offset], &i__1, &iinfo);
	if (iinfo != 0) {
	    *info = 4;
	    goto L10;
	}
    }

/*     Reduce to generalized Hessenberg form */

    if (*ilq) {
	*(unsigned char *)compq = 'V';
    } else {
	*(unsigned char *)compq = 'N';
    }

    if (*ilz) {
	*(unsigned char *)compz = 'I';
    } else {
	*(unsigned char *)compz = 'N';
    }

    dgghrd_(compq, compz, n, ilo, ihi, &a[a_offset], lda, &b[b_offset], ldb, &
	    q[q_offset], ldq, &z__[z_offset], ldz, &iinfo);
    if (iinfo != 0) {
	*info = 5;
	goto L10;
    }

/*     End */

L10:

    return 0;

/*     End of DLAQZH */

} /* dlaqzh_ */

#undef q_ref
#undef b_ref
#undef a_ref


