#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;

/* Subroutine */ int sget32_(real *rmax, integer *lmax, integer *ninfo, 
	integer *knt)
{
    /* Initialized data */

    static integer itval[32]	/* was [2][2][8] */ = { 8,4,2,1,4,8,1,2,2,1,8,
	    4,1,2,4,8,9,4,2,1,4,9,1,2,2,1,9,4,1,2,4,9 };

    /* System generated locals */
    real r__1, r__2, r__3, r__4, r__5, r__6, r__7, r__8;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static integer info, isgn;
    static real tnrm, xnrm, b[4]	/* was [2][2] */, scale, x[4]	/* 
	    was [2][2] */;
    static integer n1, n2;
    static real xnorm;
    static integer ib;
    extern /* Subroutine */ int slasy2_(logical *, logical *, integer *, 
	    integer *, integer *, real *, integer *, real *, integer *, real *
	    , integer *, real *, real *, integer *, real *, integer *), 
	    slabad_(real *, real *);
    static real tl[4]	/* was [2][2] */, tr[4]	/* was [2][2] */;
    extern doublereal slamch_(char *);
    static real bignum;
    static integer itranl, itlscl;
    static logical ltranl;
    static integer ib1, ib2, ib3, itranr, itrscl;
    static logical ltranr;
    static real smlnum, den, val[3], eps;
    static integer itl;
    static real res, sgn;
    static integer itr;
    static real tmp;


#define b_ref(a_1,a_2) b[(a_2)*2 + a_1 - 3]
#define x_ref(a_1,a_2) x[(a_2)*2 + a_1 - 3]
#define itval_ref(a_1,a_2,a_3) itval[((a_3)*2 + (a_2))*2 + a_1 - 7]
#define tl_ref(a_1,a_2) tl[(a_2)*2 + a_1 - 3]
#define tr_ref(a_1,a_2) tr[(a_2)*2 + a_1 - 3]


/*  -- LAPACK test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 29, 1992   


    Purpose   
    =======   

    SGET32 tests SLASY2, a routine for solving   

            op(TL)*X + ISGN*X*op(TR) = SCALE*B   

    where TL is N1 by N1, TR is N2 by N2, and N1,N2 =1 or 2 only.   
    X and B are N1 by N2, op() is an optional transpose, an   
    ISGN = 1 or -1. SCALE is chosen less than or equal to 1 to   
    avoid overflow in X.   

    The test condition is that the scaled residual   

    norm( op(TL)*X + ISGN*X*op(TR) = SCALE*B )   
         / ( max( ulp*norm(TL), ulp*norm(TR)) * norm(X), SMLNUM )   

    should be on the order of 1. Here, ulp is the machine precision.   
    Also, it is verified that SCALE is less than or equal to 1, and   
    that XNORM = infinity-norm(X).   

    Arguments   
    ==========   

    RMAX    (output) REAL   
            Value of the largest test ratio.   

    LMAX    (output) INTEGER   
            Example number where largest test ratio achieved.   

    NINFO   (output) INTEGER   
            Number of examples returned with INFO.NE.0.   

    KNT     (output) INTEGER   
            Total number of examples tested.   

    =====================================================================   


       Get machine parameters */

    eps = slamch_("P");
    smlnum = slamch_("S") / eps;
    bignum = 1.f / smlnum;
    slabad_(&smlnum, &bignum);

/*     Set up test case parameters */

    val[0] = sqrt(smlnum);
    val[1] = 1.f;
    val[2] = sqrt(bignum);

    *knt = 0;
    *ninfo = 0;
    *lmax = 0;
    *rmax = 0.f;

/*     Begin test loop */

    for (itranl = 0; itranl <= 1; ++itranl) {
	for (itranr = 0; itranr <= 1; ++itranr) {
	    for (isgn = -1; isgn <= 1; isgn += 2) {
		sgn = (real) isgn;
		ltranl = itranl == 1;
		ltranr = itranr == 1;

		n1 = 1;
		n2 = 1;
		for (itl = 1; itl <= 3; ++itl) {
		    for (itr = 1; itr <= 3; ++itr) {
			for (ib = 1; ib <= 3; ++ib) {
			    tl_ref(1, 1) = val[itl - 1];
			    tr_ref(1, 1) = val[itr - 1];
			    b_ref(1, 1) = val[ib - 1];
			    ++(*knt);
			    slasy2_(&ltranl, &ltranr, &isgn, &n1, &n2, tl, &
				    c__2, tr, &c__2, b, &c__2, &scale, x, &
				    c__2, &xnorm, &info);
			    if (info != 0) {
				++(*ninfo);
			    }
			    res = (r__1 = (tl_ref(1, 1) + sgn * tr_ref(1, 1)) 
				    * x_ref(1, 1) - scale * b_ref(1, 1), dabs(
				    r__1));
			    if (info == 0) {
/* Computing MAX */
				r__4 = eps * (((r__1 = tr_ref(1, 1), dabs(
					r__1)) + (r__2 = tl_ref(1, 1), dabs(
					r__2))) * (r__3 = x_ref(1, 1), dabs(
					r__3)));
				den = dmax(r__4,smlnum);
			    } else {
/* Computing MAX */
				r__2 = (r__1 = x_ref(1, 1), dabs(r__1));
				den = smlnum * dmax(r__2,1.f);
			    }
			    res /= den;
			    if (scale > 1.f) {
				res += 1.f / eps;
			    }
			    res += (r__2 = xnorm - (r__1 = x_ref(1, 1), dabs(
				    r__1)), dabs(r__2)) / dmax(smlnum,xnorm) /
				     eps;
			    if (info != 0 && info != 1) {
				res += 1.f / eps;
			    }
			    if (res > *rmax) {
				*lmax = *knt;
				*rmax = res;
			    }
/* L10: */
			}
/* L20: */
		    }
/* L30: */
		}

		n1 = 2;
		n2 = 1;
		for (itl = 1; itl <= 8; ++itl) {
		    for (itlscl = 1; itlscl <= 3; ++itlscl) {
			for (itr = 1; itr <= 3; ++itr) {
			    for (ib1 = 1; ib1 <= 3; ++ib1) {
				for (ib2 = 1; ib2 <= 3; ++ib2) {
				    b_ref(1, 1) = val[ib1 - 1];
				    b_ref(2, 1) = val[ib2 - 1] * -4.f;
				    tl_ref(1, 1) = itval_ref(1, 1, itl) * val[
					    itlscl - 1];
				    tl_ref(2, 1) = itval_ref(2, 1, itl) * val[
					    itlscl - 1];
				    tl_ref(1, 2) = itval_ref(1, 2, itl) * val[
					    itlscl - 1];
				    tl_ref(2, 2) = itval_ref(2, 2, itl) * val[
					    itlscl - 1];
				    tr_ref(1, 1) = val[itr - 1];
				    ++(*knt);
				    slasy2_(&ltranl, &ltranr, &isgn, &n1, &n2,
					     tl, &c__2, tr, &c__2, b, &c__2, &
					    scale, x, &c__2, &xnorm, &info);
				    if (info != 0) {
					++(*ninfo);
				    }
				    if (ltranl) {
					tmp = tl_ref(1, 2);
					tl_ref(1, 2) = tl_ref(2, 1);
					tl_ref(2, 1) = tmp;
				    }
				    res = (r__1 = (tl_ref(1, 1) + sgn * 
					    tr_ref(1, 1)) * x_ref(1, 1) + 
					    tl_ref(1, 2) * x_ref(2, 1) - 
					    scale * b_ref(1, 1), dabs(r__1));
				    res += (r__1 = (tl_ref(2, 2) + sgn * 
					    tr_ref(1, 1)) * x_ref(2, 1) + 
					    tl_ref(2, 1) * x_ref(1, 1) - 
					    scale * b_ref(2, 1), dabs(r__1));
				    tnrm = (r__1 = tr_ref(1, 1), dabs(r__1)) 
					    + (r__2 = tl_ref(1, 1), dabs(r__2)
					    ) + (r__3 = tl_ref(1, 2), dabs(
					    r__3)) + (r__4 = tl_ref(2, 1), 
					    dabs(r__4)) + (r__5 = tl_ref(2, 2)
					    , dabs(r__5));
/* Computing MAX */
				    r__3 = (r__1 = x_ref(1, 1), dabs(r__1)), 
					    r__4 = (r__2 = x_ref(2, 1), dabs(
					    r__2));
				    xnrm = dmax(r__3,r__4);
/* Computing MAX */
				    r__1 = smlnum, r__2 = smlnum * xnrm, r__1 
					    = max(r__1,r__2), r__2 = tnrm * 
					    eps * xnrm;
				    den = dmax(r__1,r__2);
				    res /= den;
				    if (scale > 1.f) {
					res += 1.f / eps;
				    }
				    res += (r__1 = xnorm - xnrm, dabs(r__1)) /
					     dmax(smlnum,xnorm) / eps;
				    if (res > *rmax) {
					*lmax = *knt;
					*rmax = res;
				    }
/* L40: */
				}
/* L50: */
			    }
/* L60: */
			}
/* L70: */
		    }
/* L80: */
		}

		n1 = 1;
		n2 = 2;
		for (itr = 1; itr <= 8; ++itr) {
		    for (itrscl = 1; itrscl <= 3; ++itrscl) {
			for (itl = 1; itl <= 3; ++itl) {
			    for (ib1 = 1; ib1 <= 3; ++ib1) {
				for (ib2 = 1; ib2 <= 3; ++ib2) {
				    b_ref(1, 1) = val[ib1 - 1];
				    b_ref(1, 2) = val[ib2 - 1] * -2.f;
				    tr_ref(1, 1) = itval_ref(1, 1, itr) * val[
					    itrscl - 1];
				    tr_ref(2, 1) = itval_ref(2, 1, itr) * val[
					    itrscl - 1];
				    tr_ref(1, 2) = itval_ref(1, 2, itr) * val[
					    itrscl - 1];
				    tr_ref(2, 2) = itval_ref(2, 2, itr) * val[
					    itrscl - 1];
				    tl_ref(1, 1) = val[itl - 1];
				    ++(*knt);
				    slasy2_(&ltranl, &ltranr, &isgn, &n1, &n2,
					     tl, &c__2, tr, &c__2, b, &c__2, &
					    scale, x, &c__2, &xnorm, &info);
				    if (info != 0) {
					++(*ninfo);
				    }
				    if (ltranr) {
					tmp = tr_ref(1, 2);
					tr_ref(1, 2) = tr_ref(2, 1);
					tr_ref(2, 1) = tmp;
				    }
				    tnrm = (r__1 = tl_ref(1, 1), dabs(r__1)) 
					    + (r__2 = tr_ref(1, 1), dabs(r__2)
					    ) + (r__3 = tr_ref(1, 2), dabs(
					    r__3)) + (r__4 = tr_ref(2, 2), 
					    dabs(r__4)) + (r__5 = tr_ref(2, 1)
					    , dabs(r__5));
				    xnrm = (r__1 = x_ref(1, 1), dabs(r__1)) + 
					    (r__2 = x_ref(1, 2), dabs(r__2));
				    res = (r__1 = (tl_ref(1, 1) + sgn * 
					    tr_ref(1, 1)) * x_ref(1, 1) + sgn 
					    * tr_ref(2, 1) * x_ref(1, 2) - 
					    scale * b_ref(1, 1), dabs(r__1));
				    res += (r__1 = (tl_ref(1, 1) + sgn * 
					    tr_ref(2, 2)) * x_ref(1, 2) + sgn 
					    * tr_ref(1, 2) * x_ref(1, 1) - 
					    scale * b_ref(1, 2), dabs(r__1));
/* Computing MAX */
				    r__1 = smlnum, r__2 = smlnum * xnrm, r__1 
					    = max(r__1,r__2), r__2 = tnrm * 
					    eps * xnrm;
				    den = dmax(r__1,r__2);
				    res /= den;
				    if (scale > 1.f) {
					res += 1.f / eps;
				    }
				    res += (r__1 = xnorm - xnrm, dabs(r__1)) /
					     dmax(smlnum,xnorm) / eps;
				    if (res > *rmax) {
					*lmax = *knt;
					*rmax = res;
				    }
/* L90: */
				}
/* L100: */
			    }
/* L110: */
			}
/* L120: */
		    }
/* L130: */
		}

		n1 = 2;
		n2 = 2;
		for (itr = 1; itr <= 8; ++itr) {
		    for (itrscl = 1; itrscl <= 3; ++itrscl) {
			for (itl = 1; itl <= 8; ++itl) {
			    for (itlscl = 1; itlscl <= 3; ++itlscl) {
				for (ib1 = 1; ib1 <= 3; ++ib1) {
				    for (ib2 = 1; ib2 <= 3; ++ib2) {
					for (ib3 = 1; ib3 <= 3; ++ib3) {
					    b_ref(1, 1) = val[ib1 - 1];
					    b_ref(2, 1) = val[ib2 - 1] * -4.f;
					    b_ref(1, 2) = val[ib3 - 1] * -2.f;
/* Computing MIN */
					    r__1 = val[ib1 - 1], r__2 = val[
						    ib2 - 1], r__1 = min(r__1,
						    r__2), r__2 = val[ib3 - 1]
						    ;
					    b_ref(2, 2) = dmin(r__1,r__2) * 
						    8.f;
					    tr_ref(1, 1) = itval_ref(1, 1, 
						    itr) * val[itrscl - 1];
					    tr_ref(2, 1) = itval_ref(2, 1, 
						    itr) * val[itrscl - 1];
					    tr_ref(1, 2) = itval_ref(1, 2, 
						    itr) * val[itrscl - 1];
					    tr_ref(2, 2) = itval_ref(2, 2, 
						    itr) * val[itrscl - 1];
					    tl_ref(1, 1) = itval_ref(1, 1, 
						    itl) * val[itlscl - 1];
					    tl_ref(2, 1) = itval_ref(2, 1, 
						    itl) * val[itlscl - 1];
					    tl_ref(1, 2) = itval_ref(1, 2, 
						    itl) * val[itlscl - 1];
					    tl_ref(2, 2) = itval_ref(2, 2, 
						    itl) * val[itlscl - 1];
					    ++(*knt);
					    slasy2_(&ltranl, &ltranr, &isgn, &
						    n1, &n2, tl, &c__2, tr, &
						    c__2, b, &c__2, &scale, x,
						     &c__2, &xnorm, &info);
					    if (info != 0) {
			  ++(*ninfo);
					    }
					    if (ltranr) {
			  tmp = tr_ref(1, 2);
			  tr_ref(1, 2) = tr_ref(2, 1);
			  tr_ref(2, 1) = tmp;
					    }
					    if (ltranl) {
			  tmp = tl_ref(1, 2);
			  tl_ref(1, 2) = tl_ref(2, 1);
			  tl_ref(2, 1) = tmp;
					    }
					    tnrm = (r__1 = tr_ref(1, 1), dabs(
						    r__1)) + (r__2 = tr_ref(2,
						     1), dabs(r__2)) + (r__3 =
						     tr_ref(1, 2), dabs(r__3))
						     + (r__4 = tr_ref(2, 2), 
						    dabs(r__4)) + (r__5 = 
						    tl_ref(1, 1), dabs(r__5)) 
						    + (r__6 = tl_ref(2, 1), 
						    dabs(r__6)) + (r__7 = 
						    tl_ref(1, 2), dabs(r__7)) 
						    + (r__8 = tl_ref(2, 2), 
						    dabs(r__8));
/* Computing MAX */
					    r__5 = (r__1 = x_ref(1, 1), dabs(
						    r__1)) + (r__2 = x_ref(1, 
						    2), dabs(r__2)), r__6 = (
						    r__3 = x_ref(2, 1), dabs(
						    r__3)) + (r__4 = x_ref(2, 
						    2), dabs(r__4));
					    xnrm = dmax(r__5,r__6);
					    res = (r__1 = (tl_ref(1, 1) + sgn 
						    * tr_ref(1, 1)) * x_ref(1,
						     1) + sgn * tr_ref(2, 1) *
						     x_ref(1, 2) + tl_ref(1, 
						    2) * x_ref(2, 1) - scale *
						     b_ref(1, 1), dabs(r__1));
					    res += (r__1 = tl_ref(1, 1) * 
						    x_ref(1, 2) + sgn * 
						    tr_ref(1, 2) * x_ref(1, 1)
						     + sgn * tr_ref(2, 2) * 
						    x_ref(1, 2) + tl_ref(1, 2)
						     * x_ref(2, 2) - scale * 
						    b_ref(1, 2), dabs(r__1));
					    res += (r__1 = tl_ref(2, 1) * 
						    x_ref(1, 1) + sgn * 
						    tr_ref(1, 1) * x_ref(2, 1)
						     + sgn * tr_ref(2, 1) * 
						    x_ref(2, 2) + tl_ref(2, 2)
						     * x_ref(2, 1) - scale * 
						    b_ref(2, 1), dabs(r__1));
					    res += (r__1 = (tl_ref(2, 2) + 
						    sgn * tr_ref(2, 2)) * 
						    x_ref(2, 2) + sgn * 
						    tr_ref(1, 2) * x_ref(2, 1)
						     + tl_ref(2, 1) * x_ref(1,
						     2) - scale * b_ref(2, 2),
						     dabs(r__1));
/* Computing MAX */
					    r__1 = smlnum, r__2 = smlnum * 
						    xnrm, r__1 = max(r__1,
						    r__2), r__2 = tnrm * eps *
						     xnrm;
					    den = dmax(r__1,r__2);
					    res /= den;
					    if (scale > 1.f) {
			  res += 1.f / eps;
					    }
					    res += (r__1 = xnorm - xnrm, dabs(
						    r__1)) / dmax(smlnum,
						    xnorm) / eps;
					    if (res > *rmax) {
			  *lmax = *knt;
			  *rmax = res;
					    }
/* L140: */
					}
/* L150: */
				    }
/* L160: */
				}
/* L170: */
			    }
/* L180: */
			}
/* L190: */
		    }
/* L200: */
		}
/* L210: */
	    }
/* L220: */
	}
/* L230: */
    }

    return 0;

/*     End of SGET32 */

} /* sget32_ */

#undef tr_ref
#undef tl_ref
#undef itval_ref
#undef x_ref
#undef b_ref


