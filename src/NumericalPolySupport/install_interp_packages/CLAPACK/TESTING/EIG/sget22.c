#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static real c_b20 = 0.f;
static integer c__2 = 2;
static real c_b25 = 1.f;
static integer c__1 = 1;
static real c_b30 = -1.f;

/* Subroutine */ int sget22_(char *transa, char *transe, char *transw, 
	integer *n, real *a, integer *lda, real *e, integer *lde, real *wr, 
	real *wi, real *work, real *result)
{
    /* System generated locals */
    integer a_dim1, a_offset, e_dim1, e_offset, i__1, i__2;
    real r__1, r__2, r__3, r__4;

    /* Local variables */
    static integer ince, jcol, jvec;
    static real unfl, wmat[4]	/* was [2][2] */, temp1;
    static integer j, iecol;
    extern logical lsame_(char *, char *);
    static integer ipair;
    extern /* Subroutine */ int sgemm_(char *, char *, integer *, integer *, 
	    integer *, real *, real *, integer *, real *, integer *, real *, 
	    real *, integer *);
    static char norma[1];
    static real anorm;
    static char norme[1];
    static real enorm;
    static integer ierow;
    extern /* Subroutine */ int saxpy_(integer *, real *, real *, integer *, 
	    real *, integer *);
    extern doublereal slamch_(char *), slange_(char *, integer *, 
	    integer *, real *, integer *, real *);
    static real enrmin, enrmax;
    extern /* Subroutine */ int slaset_(char *, integer *, integer *, real *, 
	    real *, real *, integer *);
    static integer itrnse;
    static real errnrm, ulp;


#define wmat_ref(a_1,a_2) wmat[(a_2)*2 + a_1 - 3]
#define e_ref(a_1,a_2) e[(a_2)*e_dim1 + a_1]


/*  -- LAPACK test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    SGET22 does an eigenvector check.   

    The basic test is:   

       RESULT(1) = | A E  -  E W | / ( |A| |E| ulp )   

    using the 1-norm.  It also tests the normalization of E:   

       RESULT(2) = max | m-norm(E(j)) - 1 | / ( n ulp )   
                    j   

    where E(j) is the j-th eigenvector, and m-norm is the max-norm of a   
    vector.  If an eigenvector is complex, as determined from WI(j)   
    nonzero, then the max-norm of the vector ( er + i*ei ) is the maximum   
    of   
       |er(1)| + |ei(1)|, ... , |er(n)| + |ei(n)|   

    W is a block diagonal matrix, with a 1 by 1 block for each real   
    eigenvalue and a 2 by 2 block for each complex conjugate pair.   
    If eigenvalues j and j+1 are a complex conjugate pair, so that   
    WR(j) = WR(j+1) = wr and WI(j) = - WI(j+1) = wi, then the 2 by 2   
    block corresponding to the pair will be:   

       (  wr  wi  )   
       ( -wi  wr  )   

    Such a block multiplying an n by 2 matrix ( ur ui ) on the right   
    will be the same as multiplying  ur + i*ui  by  wr + i*wi.   

    To handle various schemes for storage of left eigenvectors, there are   
    options to use A-transpose instead of A, E-transpose instead of E,   
    and/or W-transpose instead of W.   

    Arguments   
    ==========   

    TRANSA  (input) CHARACTER*1   
            Specifies whether or not A is transposed.   
            = 'N':  No transpose   
            = 'T':  Transpose   
            = 'C':  Conjugate transpose (= Transpose)   

    TRANSE  (input) CHARACTER*1   
            Specifies whether or not E is transposed.   
            = 'N':  No transpose, eigenvectors are in columns of E   
            = 'T':  Transpose, eigenvectors are in rows of E   
            = 'C':  Conjugate transpose (= Transpose)   

    TRANSW  (input) CHARACTER*1   
            Specifies whether or not W is transposed.   
            = 'N':  No transpose   
            = 'T':  Transpose, use -WI(j) instead of WI(j)   
            = 'C':  Conjugate transpose, use -WI(j) instead of WI(j)   

    N       (input) INTEGER   
            The order of the matrix A.  N >= 0.   

    A       (input) REAL array, dimension (LDA,N)   
            The matrix whose eigenvectors are in E.   

    LDA     (input) INTEGER   
            The leading dimension of the array A.  LDA >= max(1,N).   

    E       (input) REAL array, dimension (LDE,N)   
            The matrix of eigenvectors. If TRANSE = 'N', the eigenvectors   
            are stored in the columns of E, if TRANSE = 'T' or 'C', the   
            eigenvectors are stored in the rows of E.   

    LDE     (input) INTEGER   
            The leading dimension of the array E.  LDE >= max(1,N).   

    WR      (input) REAL array, dimension (N)   
    WI      (input) REAL array, dimension (N)   
            The real and imaginary parts of the eigenvalues of A.   
            Purely real eigenvalues are indicated by WI(j) = 0.   
            Complex conjugate pairs are indicated by WR(j)=WR(j+1) and   
            WI(j) = - WI(j+1) non-zero; the real part is assumed to be   
            stored in the j-th row/column and the imaginary part in   
            the (j+1)-th row/column.   

    WORK    (workspace) REAL array, dimension (N*(N+1))   

    RESULT  (output) REAL array, dimension (2)   
            RESULT(1) = | A E  -  E W | / ( |A| |E| ulp )   
            RESULT(2) = max | m-norm(E(j)) - 1 | / ( n ulp )   

    =====================================================================   


       Initialize RESULT (in case N=0)   

       Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    e_dim1 = *lde;
    e_offset = 1 + e_dim1 * 1;
    e -= e_offset;
    --wr;
    --wi;
    --work;
    --result;

    /* Function Body */
    result[1] = 0.f;
    result[2] = 0.f;
    if (*n <= 0) {
	return 0;
    }

    unfl = slamch_("Safe minimum");
    ulp = slamch_("Precision");

    itrnse = 0;
    ince = 1;
    *(unsigned char *)norma = 'O';
    *(unsigned char *)norme = 'O';

    if (lsame_(transa, "T") || lsame_(transa, "C")) {
	*(unsigned char *)norma = 'I';
    }
    if (lsame_(transe, "T") || lsame_(transe, "C")) {
	*(unsigned char *)norme = 'I';
	itrnse = 1;
	ince = *lde;
    }

/*     Check normalization of E */

    enrmin = 1.f / ulp;
    enrmax = 0.f;
    if (itrnse == 0) {

/*        Eigenvectors are column vectors. */

	ipair = 0;
	i__1 = *n;
	for (jvec = 1; jvec <= i__1; ++jvec) {
	    temp1 = 0.f;
	    if (ipair == 0 && jvec < *n && wi[jvec] != 0.f) {
		ipair = 1;
	    }
	    if (ipair == 1) {

/*              Complex eigenvector */

		i__2 = *n;
		for (j = 1; j <= i__2; ++j) {
/* Computing MAX */
		    r__3 = temp1, r__4 = (r__1 = e_ref(j, jvec), dabs(r__1)) 
			    + (r__2 = e_ref(j, jvec + 1), dabs(r__2));
		    temp1 = dmax(r__3,r__4);
/* L10: */
		}
		enrmin = dmin(enrmin,temp1);
		enrmax = dmax(enrmax,temp1);
		ipair = 2;
	    } else if (ipair == 2) {
		ipair = 0;
	    } else {

/*              Real eigenvector */

		i__2 = *n;
		for (j = 1; j <= i__2; ++j) {
/* Computing MAX */
		    r__2 = temp1, r__3 = (r__1 = e_ref(j, jvec), dabs(r__1));
		    temp1 = dmax(r__2,r__3);
/* L20: */
		}
		enrmin = dmin(enrmin,temp1);
		enrmax = dmax(enrmax,temp1);
		ipair = 0;
	    }
/* L30: */
	}

    } else {

/*        Eigenvectors are row vectors. */

	i__1 = *n;
	for (jvec = 1; jvec <= i__1; ++jvec) {
	    work[jvec] = 0.f;
/* L40: */
	}

	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
	    ipair = 0;
	    i__2 = *n;
	    for (jvec = 1; jvec <= i__2; ++jvec) {
		if (ipair == 0 && jvec < *n && wi[jvec] != 0.f) {
		    ipair = 1;
		}
		if (ipair == 1) {
/* Computing MAX */
		    r__3 = work[jvec], r__4 = (r__1 = e_ref(j, jvec), dabs(
			    r__1)) + (r__2 = e_ref(j, jvec + 1), dabs(r__2));
		    work[jvec] = dmax(r__3,r__4);
		    work[jvec + 1] = work[jvec];
		} else if (ipair == 2) {
		    ipair = 0;
		} else {
/* Computing MAX */
		    r__2 = work[jvec], r__3 = (r__1 = e_ref(j, jvec), dabs(
			    r__1));
		    work[jvec] = dmax(r__2,r__3);
		    ipair = 0;
		}
/* L50: */
	    }
/* L60: */
	}

	i__1 = *n;
	for (jvec = 1; jvec <= i__1; ++jvec) {
/* Computing MIN */
	    r__1 = enrmin, r__2 = work[jvec];
	    enrmin = dmin(r__1,r__2);
/* Computing MAX */
	    r__1 = enrmax, r__2 = work[jvec];
	    enrmax = dmax(r__1,r__2);
/* L70: */
	}
    }

/*     Norm of A:   

   Computing MAX */
    r__1 = slange_(norma, n, n, &a[a_offset], lda, &work[1]);
    anorm = dmax(r__1,unfl);

/*     Norm of E:   

   Computing MAX */
    r__1 = slange_(norme, n, n, &e[e_offset], lde, &work[1]);
    enorm = dmax(r__1,ulp);

/*     Norm of error:   

       Error =  AE - EW */

    slaset_("Full", n, n, &c_b20, &c_b20, &work[1], n);

    ipair = 0;
    ierow = 1;
    iecol = 1;

    i__1 = *n;
    for (jcol = 1; jcol <= i__1; ++jcol) {
	if (itrnse == 1) {
	    ierow = jcol;
	} else {
	    iecol = jcol;
	}

	if (ipair == 0 && wi[jcol] != 0.f) {
	    ipair = 1;
	}

	if (ipair == 1) {
	    wmat_ref(1, 1) = wr[jcol];
	    wmat_ref(2, 1) = -wi[jcol];
	    wmat_ref(1, 2) = wi[jcol];
	    wmat_ref(2, 2) = wr[jcol];
	    sgemm_(transe, transw, n, &c__2, &c__2, &c_b25, &e_ref(ierow, 
		    iecol), lde, wmat, &c__2, &c_b20, &work[*n * (jcol - 1) + 
		    1], n);
	    ipair = 2;
	} else if (ipair == 2) {
	    ipair = 0;

	} else {

	    saxpy_(n, &wr[jcol], &e_ref(ierow, iecol), &ince, &work[*n * (
		    jcol - 1) + 1], &c__1);
	    ipair = 0;
	}

/* L80: */
    }

    sgemm_(transa, transe, n, n, n, &c_b25, &a[a_offset], lda, &e[e_offset], 
	    lde, &c_b30, &work[1], n);

    errnrm = slange_("One", n, n, &work[1], n, &work[*n * *n + 1]) 
	    / enorm;

/*     Compute RESULT(1) (avoiding under/overflow) */

    if (anorm > errnrm) {
	result[1] = errnrm / anorm / ulp;
    } else {
	if (anorm < 1.f) {
	    result[1] = dmin(errnrm,anorm) / anorm / ulp;
	} else {
/* Computing MIN */
	    r__1 = errnrm / anorm;
	    result[1] = dmin(r__1,1.f) / ulp;
	}
    }

/*     Compute RESULT(2) : the normalization error in E.   

   Computing MAX */
    r__3 = (r__1 = enrmax - 1.f, dabs(r__1)), r__4 = (r__2 = enrmin - 1.f, 
	    dabs(r__2));
    result[2] = dmax(r__3,r__4) / ((real) (*n) * ulp);

    return 0;

/*     End of SGET22 */

} /* sget22_ */

#undef e_ref
#undef wmat_ref


