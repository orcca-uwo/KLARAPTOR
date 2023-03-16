#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__7 = 7;
static integer c__1 = 1;
static complex c_b6 = {0.f,0.f};
static integer c__0 = 0;
static real c_b33 = -1.f;

doublereal cqrt12_(integer *m, integer *n, complex *a, integer *lda, real *s, 
	complex *work, integer *lwork, real *rwork)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3, i__4;
    real ret_val;

    /* Local variables */
    static integer iscl, info;
    static real anrm;
    extern doublereal snrm2_(integer *, real *, integer *);
    static integer i__, j;
    extern /* Subroutine */ int cgebd2_(integer *, integer *, complex *, 
	    integer *, real *, real *, complex *, complex *, complex *, 
	    integer *);
    extern doublereal sasum_(integer *, real *, integer *);
    static real dummy[1];
    extern /* Subroutine */ int saxpy_(integer *, real *, real *, integer *, 
	    real *, integer *), slabad_(real *, real *);
    extern doublereal clange_(char *, integer *, integer *, complex *, 
	    integer *, real *);
    static integer mn;
    extern /* Subroutine */ int clascl_(char *, integer *, integer *, real *, 
	    real *, integer *, integer *, complex *, integer *, integer *);
    extern doublereal slamch_(char *);
    extern /* Subroutine */ int claset_(char *, integer *, integer *, complex 
	    *, complex *, complex *, integer *), xerbla_(char *, 
	    integer *);
    static real bignum;
    extern /* Subroutine */ int slascl_(char *, integer *, integer *, real *, 
	    real *, integer *, integer *, real *, integer *, integer *), sbdsqr_(char *, integer *, integer *, integer *, integer 
	    *, real *, real *, real *, integer *, real *, integer *, real *, 
	    integer *, real *, integer *);
    static real smlnum, nrmsvl;


#define a_subscr(a_1,a_2) (a_2)*a_dim1 + a_1
#define a_ref(a_1,a_2) a[a_subscr(a_1,a_2)]


/*  -- LAPACK test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    CQRT12 computes the singular values `svlues' of the upper trapezoid   
    of A(1:M,1:N) and returns the ratio   

         || s - svlues||/(||svlues||*eps*max(M,N))   

    Arguments   
    =========   

    M       (input) INTEGER   
            The number of rows of the matrix A.   

    N       (input) INTEGER   
            The number of columns of the matrix A.   

    A       (input) COMPLEX array, dimension (LDA,N)   
            The M-by-N matrix A. Only the upper trapezoid is referenced.   

    LDA     (input) INTEGER   
            The leading dimension of the array A.   

    S       (input) REAL array, dimension (min(M,N))   
            The singular values of the matrix A.   

    WORK    (workspace) COMPLEX array, dimension (LWORK)   

    LWORK   (input) INTEGER   
            The length of the array WORK. LWORK >= M*N + 2*min(M,N) +   
            max(M,N).   

    RWORK   (workspace) REAL array, dimension (4*min(M,N))   

    =====================================================================   


       Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    --s;
    --work;
    --rwork;

    /* Function Body */
    ret_val = 0.f;

/*     Test that enough workspace is supplied */

    if (*lwork < *m * *n + (min(*m,*n) << 1) + max(*m,*n)) {
	xerbla_("CQRT12", &c__7);
	return ret_val;
    }

/*     Quick return if possible */

    mn = min(*m,*n);
    if ((real) mn <= 0.f) {
	return ret_val;
    }

    nrmsvl = snrm2_(&mn, &s[1], &c__1);

/*     Copy upper triangle of A into work */

    claset_("Full", m, n, &c_b6, &c_b6, &work[1], m);
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	i__2 = min(j,*m);
	for (i__ = 1; i__ <= i__2; ++i__) {
	    i__3 = (j - 1) * *m + i__;
	    i__4 = a_subscr(i__, j);
	    work[i__3].r = a[i__4].r, work[i__3].i = a[i__4].i;
/* L10: */
	}
/* L20: */
    }

/*     Get machine parameters */

    smlnum = slamch_("S") / slamch_("P");
    bignum = 1.f / smlnum;
    slabad_(&smlnum, &bignum);

/*     Scale work if max entry outside range [SMLNUM,BIGNUM] */

    anrm = clange_("M", m, n, &work[1], m, dummy);
    iscl = 0;
    if (anrm > 0.f && anrm < smlnum) {

/*        Scale matrix norm up to SMLNUM */

	clascl_("G", &c__0, &c__0, &anrm, &smlnum, m, n, &work[1], m, &info);
	iscl = 1;
    } else if (anrm > bignum) {

/*        Scale matrix norm down to BIGNUM */

	clascl_("G", &c__0, &c__0, &anrm, &bignum, m, n, &work[1], m, &info);
	iscl = 1;
    }

    if (anrm != 0.f) {

/*        Compute SVD of work */

	cgebd2_(m, n, &work[1], m, &rwork[1], &rwork[mn + 1], &work[*m * *n + 
		1], &work[*m * *n + mn + 1], &work[*m * *n + (mn << 1) + 1], &
		info);
	sbdsqr_("Upper", &mn, &c__0, &c__0, &c__0, &rwork[1], &rwork[mn + 1], 
		dummy, &mn, dummy, &c__1, dummy, &mn, &rwork[(mn << 1) + 1], &
		info);

	if (iscl == 1) {
	    if (anrm > bignum) {
		slascl_("G", &c__0, &c__0, &bignum, &anrm, &mn, &c__1, &rwork[
			1], &mn, &info);
	    }
	    if (anrm < smlnum) {
		slascl_("G", &c__0, &c__0, &smlnum, &anrm, &mn, &c__1, &rwork[
			1], &mn, &info);
	    }
	}

    } else {

	i__1 = mn;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    rwork[i__] = 0.f;
/* L30: */
	}
    }

/*     Compare s and singular values of work */

    saxpy_(&mn, &c_b33, &s[1], &c__1, &rwork[1], &c__1);
    ret_val = sasum_(&mn, &rwork[1], &c__1) / (slamch_("Epsilon") *
	     (real) max(*m,*n));
    if (nrmsvl != 0.f) {
	ret_val /= nrmsvl;
    }

    return ret_val;

/*     End of CQRT12 */

} /* cqrt12_ */

#undef a_ref
#undef a_subscr


