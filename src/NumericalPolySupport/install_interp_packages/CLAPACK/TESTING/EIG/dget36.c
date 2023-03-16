#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__1 = 1;
static integer c__5 = 5;
static integer c__10 = 10;
static doublereal c_b21 = 0.;
static doublereal c_b22 = 1.;
static integer c__200 = 200;

/* Subroutine */ int dget36_(doublereal *rmax, integer *lmax, integer *ninfo, 
	integer *knt, integer *nin)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_rsle(void);
    double d_sign(doublereal *, doublereal *);

    /* Local variables */
    static integer ifst, ilst;
    static doublereal work[200];
    static integer info1, info2, ifst1, ifst2, ilst1, ilst2, i__, j, n;
    static doublereal q[100]	/* was [10][10] */;
    extern /* Subroutine */ int dhst01_(integer *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, integer *, doublereal *, 
	    integer *, doublereal *, integer *, doublereal *);
    static doublereal t1[100]	/* was [10][10] */, t2[100]	/* was [10][
	    10] */;
    extern doublereal dlamch_(char *);
    extern /* Subroutine */ int dlacpy_(char *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, integer *), 
	    dlaset_(char *, integer *, integer *, doublereal *, doublereal *, 
	    doublereal *, integer *), dtrexc_(char *, integer *, 
	    doublereal *, integer *, doublereal *, integer *, integer *, 
	    integer *, doublereal *, integer *);
    static integer ifstsv;
    static doublereal result[2];
    static integer ilstsv, loc;
    static doublereal eps, res, tmp[100]	/* was [10][10] */;

    /* Fortran I/O blocks */
    static cilist io___2 = { 0, 0, 0, 0, 0 };
    static cilist io___7 = { 0, 0, 0, 0, 0 };



#define q_ref(a_1,a_2) q[(a_2)*10 + a_1 - 11]
#define t1_ref(a_1,a_2) t1[(a_2)*10 + a_1 - 11]
#define t2_ref(a_1,a_2) t2[(a_2)*10 + a_1 - 11]
#define tmp_ref(a_1,a_2) tmp[(a_2)*10 + a_1 - 11]


/*  -- LAPACK test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    DGET36 tests DTREXC, a routine for moving blocks (either 1 by 1 or   
    2 by 2) on the diagonal of a matrix in real Schur form.  Thus, DLAEXC   
    computes an orthogonal matrix Q such that   

       Q' * T1 * Q  = T2   

    and where one of the diagonal blocks of T1 (the one at row IFST) has   
    been moved to position ILST.   

    The test code verifies that the residual Q'*T1*Q-T2 is small, that T2   
    is in Schur form, and that the final position of the IFST block is   
    ILST (within +-1).   

    The test matrices are read from a file with logical unit number NIN.   

    Arguments   
    ==========   

    RMAX    (output) DOUBLE PRECISION   
            Value of the largest test ratio.   

    LMAX    (output) INTEGER   
            Example number where largest test ratio achieved.   

    NINFO   (output) INTEGER array, dimension (3)   
            NINFO(J) is the number of examples where INFO=J.   

    KNT     (output) INTEGER   
            Total number of examples tested.   

    NIN     (input) INTEGER   
            Input logical unit number.   

    =====================================================================   


       Parameter adjustments */
    --ninfo;

    /* Function Body */
    eps = dlamch_("P");
    *rmax = 0.;
    *lmax = 0;
    *knt = 0;
    ninfo[1] = 0;
    ninfo[2] = 0;
    ninfo[3] = 0;

/*     Read input data until N=0 */

L10:
    io___2.ciunit = *nin;
    s_rsle(&io___2);
    do_lio(&c__3, &c__1, (char *)&n, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&ifst, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&ilst, (ftnlen)sizeof(integer));
    e_rsle();
    if (n == 0) {
	return 0;
    }
    ++(*knt);
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	io___7.ciunit = *nin;
	s_rsle(&io___7);
	i__2 = n;
	for (j = 1; j <= i__2; ++j) {
	    do_lio(&c__5, &c__1, (char *)&tmp_ref(i__, j), (ftnlen)sizeof(
		    doublereal));
	}
	e_rsle();
/* L20: */
    }
    dlacpy_("F", &n, &n, tmp, &c__10, t1, &c__10);
    dlacpy_("F", &n, &n, tmp, &c__10, t2, &c__10);
    ifstsv = ifst;
    ilstsv = ilst;
    ifst1 = ifst;
    ilst1 = ilst;
    ifst2 = ifst;
    ilst2 = ilst;
    res = 0.;

/*     Test without accumulating Q */

    dlaset_("Full", &n, &n, &c_b21, &c_b22, q, &c__10);
    dtrexc_("N", &n, t1, &c__10, q, &c__10, &ifst1, &ilst1, work, &info1);
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = n;
	for (j = 1; j <= i__2; ++j) {
	    if (i__ == j && q_ref(i__, j) != 1.) {
		res += 1. / eps;
	    }
	    if (i__ != j && q_ref(i__, j) != 0.) {
		res += 1. / eps;
	    }
/* L30: */
	}
/* L40: */
    }

/*     Test with accumulating Q */

    dlaset_("Full", &n, &n, &c_b21, &c_b22, q, &c__10);
    dtrexc_("V", &n, t2, &c__10, q, &c__10, &ifst2, &ilst2, work, &info2);

/*     Compare T1 with T2 */

    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = n;
	for (j = 1; j <= i__2; ++j) {
	    if (t1_ref(i__, j) != t2_ref(i__, j)) {
		res += 1. / eps;
	    }
/* L50: */
	}
/* L60: */
    }
    if (ifst1 != ifst2) {
	res += 1. / eps;
    }
    if (ilst1 != ilst2) {
	res += 1. / eps;
    }
    if (info1 != info2) {
	res += 1. / eps;
    }

/*     Test for successful reordering of T2 */

    if (info2 != 0) {
	++ninfo[info2];
    } else {
	if ((i__1 = ifst2 - ifstsv, abs(i__1)) > 1) {
	    res += 1. / eps;
	}
	if ((i__1 = ilst2 - ilstsv, abs(i__1)) > 1) {
	    res += 1. / eps;
	}
    }

/*     Test for small residual, and orthogonality of Q */

    dhst01_(&n, &c__1, &n, tmp, &c__10, t2, &c__10, q, &c__10, work, &c__200, 
	    result);
    res = res + result[0] + result[1];

/*     Test for T2 being in Schur form */

    loc = 1;
L70:
    if (t2_ref(loc + 1, loc) != 0.) {

/*        2 by 2 block */

	if (t2_ref(loc, loc + 1) == 0. || t2_ref(loc, loc) != t2_ref(loc + 1, 
		loc + 1) || d_sign(&c_b22, &t2_ref(loc, loc + 1)) == d_sign(&
		c_b22, &t2_ref(loc + 1, loc))) {
	    res += 1. / eps;
	}
	i__1 = n;
	for (i__ = loc + 2; i__ <= i__1; ++i__) {
	    if (t2_ref(i__, loc) != 0.) {
		res += 1. / res;
	    }
	    if (t2_ref(i__, loc + 1) != 0.) {
		res += 1. / res;
	    }
/* L80: */
	}
	loc += 2;
    } else {

/*        1 by 1 block */

	i__1 = n;
	for (i__ = loc + 1; i__ <= i__1; ++i__) {
	    if (t2_ref(i__, loc) != 0.) {
		res += 1. / res;
	    }
/* L90: */
	}
	++loc;
    }
    if (loc < n) {
	goto L70;
    }
    if (res > *rmax) {
	*rmax = res;
	*lmax = *knt;
    }
    goto L10;

/*     End of DGET36 */

} /* dget36_ */

#undef tmp_ref
#undef t2_ref
#undef t1_ref
#undef q_ref


