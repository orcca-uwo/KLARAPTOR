#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;

/* Subroutine */ int sget31_(real *rmax, integer *lmax, integer *ninfo, 
	integer *knt)
{
    /* Initialized data */

    static logical ltrans[2] = { FALSE_,TRUE_ };

    /* System generated locals */
    real r__1, r__2, r__3, r__4, r__5, r__6, r__7, r__8, r__9, r__10, r__11, 
	    r__12, r__13, r__14, r__15, r__16, r__17;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static integer info;
    static real unfl, smin, a[4]	/* was [2][2] */, b[4]	/* was [2][2] 
	    */, scale, x[4]	/* was [2][2] */;
    static integer ismin;
    static real d1, d2, vsmin[4], xnorm;
    extern /* Subroutine */ int slaln2_(logical *, integer *, integer *, real 
	    *, real *, real *, integer *, real *, real *, real *, integer *, 
	    real *, real *, real *, integer *, real *, real *, integer *);
    static real ca;
    static integer ia, ib, na;
    extern /* Subroutine */ int slabad_(real *, real *);
    static real wi;
    static integer nw;
    extern doublereal slamch_(char *);
    static real wr, bignum;
    static integer id1, id2, itrans;
    static real smlnum;
    static integer ica;
    static real den, vab[3], vca[5], vdd[4], eps;
    static integer iwi;
    static real res, tmp;
    static integer iwr;
    static real vwi[4], vwr[4];


#define a_ref(a_1,a_2) a[(a_2)*2 + a_1 - 3]
#define b_ref(a_1,a_2) b[(a_2)*2 + a_1 - 3]
#define x_ref(a_1,a_2) x[(a_2)*2 + a_1 - 3]


/*  -- LAPACK test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   


    Purpose   
    =======   

    SGET31 tests SLALN2, a routine for solving   

       (ca A - w D)X = sB   

    where A is an NA by NA matrix (NA=1 or 2 only), w is a real (NW=1) or   
    complex (NW=2) constant, ca is a real constant, D is an NA by NA real   
    diagonal matrix, and B is an NA by NW matrix (when NW=2 the second   
    column of B contains the imaginary part of the solution).  The code   
    returns X and s, where s is a scale factor, less than or equal to 1,   
    which is chosen to avoid overflow in X.   

    If any singular values of ca A-w D are less than another input   
    parameter SMIN, they are perturbed up to SMIN.   

    The test condition is that the scaled residual   

        norm( (ca A-w D)*X - s*B ) /   
              ( max( ulp*norm(ca A-w D), SMIN )*norm(X) )   

    should be on the order of 1.  Here, ulp is the machine precision.   
    Also, it is verified that SCALE is less than or equal to 1, and that   
    XNORM = infinity-norm(X).   

    Arguments   
    ==========   

    RMAX    (output) REAL   
            Value of the largest test ratio.   

    LMAX    (output) INTEGER   
            Example number where largest test ratio achieved.   

    NINFO   (output) INTEGER array, dimension (3)   
            NINFO(1) = number of examples with INFO less than 0   
            NINFO(2) = number of examples with INFO greater than 0   

    KNT     (output) INTEGER   
            Total number of examples tested.   

    =====================================================================   

       Parameter adjustments */
    --ninfo;

    /* Function Body   

       Get machine parameters */

    eps = slamch_("P");
    unfl = slamch_("U");
    smlnum = slamch_("S") / eps;
    bignum = 1.f / smlnum;
    slabad_(&smlnum, &bignum);

/*     Set up test case parameters */

    vsmin[0] = smlnum;
    vsmin[1] = eps;
    vsmin[2] = .01f;
    vsmin[3] = 1.f / eps;
    vab[0] = sqrt(smlnum);
    vab[1] = 1.f;
    vab[2] = sqrt(bignum);
    vwr[0] = 0.f;
    vwr[1] = .5f;
    vwr[2] = 2.f;
    vwr[3] = 1.f;
    vwi[0] = smlnum;
    vwi[1] = eps;
    vwi[2] = 1.f;
    vwi[3] = 2.f;
    vdd[0] = sqrt(smlnum);
    vdd[1] = 1.f;
    vdd[2] = 2.f;
    vdd[3] = sqrt(bignum);
    vca[0] = 0.f;
    vca[1] = sqrt(smlnum);
    vca[2] = eps;
    vca[3] = .5f;
    vca[4] = 1.f;

    *knt = 0;
    ninfo[1] = 0;
    ninfo[2] = 0;
    *lmax = 0;
    *rmax = 0.f;

/*     Begin test loop */

    for (id1 = 1; id1 <= 4; ++id1) {
	d1 = vdd[id1 - 1];
	for (id2 = 1; id2 <= 4; ++id2) {
	    d2 = vdd[id2 - 1];
	    for (ica = 1; ica <= 5; ++ica) {
		ca = vca[ica - 1];
		for (itrans = 0; itrans <= 1; ++itrans) {
		    for (ismin = 1; ismin <= 4; ++ismin) {
			smin = vsmin[ismin - 1];

			na = 1;
			nw = 1;
			for (ia = 1; ia <= 3; ++ia) {
			    a_ref(1, 1) = vab[ia - 1];
			    for (ib = 1; ib <= 3; ++ib) {
				b_ref(1, 1) = vab[ib - 1];
				for (iwr = 1; iwr <= 4; ++iwr) {
				    if (d1 == 1.f && d2 == 1.f && ca == 1.f) {
					wr = vwr[iwr - 1] * a_ref(1, 1);
				    } else {
					wr = vwr[iwr - 1];
				    }
				    wi = 0.f;
				    slaln2_(&ltrans[itrans], &na, &nw, &smin, 
					    &ca, a, &c__2, &d1, &d2, b, &c__2,
					     &wr, &wi, x, &c__2, &scale, &
					    xnorm, &info);
				    if (info < 0) {
					++ninfo[1];
				    }
				    if (info > 0) {
					++ninfo[2];
				    }
				    res = (r__1 = (ca * a_ref(1, 1) - wr * d1)
					     * x_ref(1, 1) - scale * b_ref(1, 
					    1), dabs(r__1));
				    if (info == 0) {
/* Computing MAX */
					r__2 = eps * (r__1 = (ca * a_ref(1, 1)
						 - wr * d1) * x_ref(1, 1), 
						dabs(r__1));
					den = dmax(r__2,smlnum);
				    } else {
/* Computing MAX */
					r__2 = smin * (r__1 = x_ref(1, 1), 
						dabs(r__1));
					den = dmax(r__2,smlnum);
				    }
				    res /= den;
				    if ((r__1 = x_ref(1, 1), dabs(r__1)) < 
					    unfl && (r__3 = b_ref(1, 1), dabs(
					    r__3)) <= smlnum * (r__2 = ca * 
					    a_ref(1, 1) - wr * d1, dabs(r__2))
					    ) {
					res = 0.f;
				    }
				    if (scale > 1.f) {
					res += 1.f / eps;
				    }
				    res += (r__2 = xnorm - (r__1 = x_ref(1, 1)
					    , dabs(r__1)), dabs(r__2)) / dmax(
					    smlnum,xnorm) / eps;
				    if (info != 0 && info != 1) {
					res += 1.f / eps;
				    }
				    ++(*knt);
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

			na = 1;
			nw = 2;
			for (ia = 1; ia <= 3; ++ia) {
			    a_ref(1, 1) = vab[ia - 1];
			    for (ib = 1; ib <= 3; ++ib) {
				b_ref(1, 1) = vab[ib - 1];
				b_ref(1, 2) = vab[ib - 1] * -.5f;
				for (iwr = 1; iwr <= 4; ++iwr) {
				    if (d1 == 1.f && d2 == 1.f && ca == 1.f) {
					wr = vwr[iwr - 1] * a_ref(1, 1);
				    } else {
					wr = vwr[iwr - 1];
				    }
				    for (iwi = 1; iwi <= 4; ++iwi) {
					if (d1 == 1.f && d2 == 1.f && ca == 
						1.f) {
					    wi = vwi[iwi - 1] * a_ref(1, 1);
					} else {
					    wi = vwi[iwi - 1];
					}
					slaln2_(&ltrans[itrans], &na, &nw, &
						smin, &ca, a, &c__2, &d1, &d2,
						 b, &c__2, &wr, &wi, x, &c__2,
						 &scale, &xnorm, &info);
					if (info < 0) {
					    ++ninfo[1];
					}
					if (info > 0) {
					    ++ninfo[2];
					}
					res = (r__1 = (ca * a_ref(1, 1) - wr *
						 d1) * x_ref(1, 1) + wi * d1 *
						 x_ref(1, 2) - scale * b_ref(
						1, 1), dabs(r__1));
					res += (r__1 = -wi * d1 * x_ref(1, 1) 
						+ (ca * a_ref(1, 1) - wr * d1)
						 * x_ref(1, 2) - scale * 
						b_ref(1, 2), dabs(r__1));
					if (info == 0) {
/* Computing MAX   
   Computing MAX */
					    r__6 = (r__3 = ca * a_ref(1, 1) - 
						    wr * d1, dabs(r__3)), 
						    r__7 = (r__4 = d1 * wi, 
						    dabs(r__4));
					    r__5 = eps * (dmax(r__6,r__7) * ((
						    r__1 = x_ref(1, 1), dabs(
						    r__1)) + (r__2 = x_ref(1, 
						    2), dabs(r__2))));
					    den = dmax(r__5,smlnum);
					} else {
/* Computing MAX */
					    r__3 = smin * ((r__1 = x_ref(1, 1)
						    , dabs(r__1)) + (r__2 = 
						    x_ref(1, 2), dabs(r__2)));
					    den = dmax(r__3,smlnum);
					}
					res /= den;
					if ((r__1 = x_ref(1, 1), dabs(r__1)) <
						 unfl && (r__2 = x_ref(1, 2), 
						dabs(r__2)) < unfl && (r__4 = 
						b_ref(1, 1), dabs(r__4)) <= 
						smlnum * (r__3 = ca * a_ref(1,
						 1) - wr * d1, dabs(r__3))) {
					    res = 0.f;
					}
					if (scale > 1.f) {
					    res += 1.f / eps;
					}
					res += (r__3 = xnorm - (r__1 = x_ref(
						1, 1), dabs(r__1)) - (r__2 = 
						x_ref(1, 2), dabs(r__2)), 
						dabs(r__3)) / dmax(smlnum,
						xnorm) / eps;
					if (info != 0 && info != 1) {
					    res += 1.f / eps;
					}
					++(*knt);
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

			na = 2;
			nw = 1;
			for (ia = 1; ia <= 3; ++ia) {
			    a_ref(1, 1) = vab[ia - 1];
			    a_ref(1, 2) = vab[ia - 1] * -3.f;
			    a_ref(2, 1) = vab[ia - 1] * -7.f;
			    a_ref(2, 2) = vab[ia - 1] * 21.f;
			    for (ib = 1; ib <= 3; ++ib) {
				b_ref(1, 1) = vab[ib - 1];
				b_ref(2, 1) = vab[ib - 1] * -2.f;
				for (iwr = 1; iwr <= 4; ++iwr) {
				    if (d1 == 1.f && d2 == 1.f && ca == 1.f) {
					wr = vwr[iwr - 1] * a_ref(1, 1);
				    } else {
					wr = vwr[iwr - 1];
				    }
				    wi = 0.f;
				    slaln2_(&ltrans[itrans], &na, &nw, &smin, 
					    &ca, a, &c__2, &d1, &d2, b, &c__2,
					     &wr, &wi, x, &c__2, &scale, &
					    xnorm, &info);
				    if (info < 0) {
					++ninfo[1];
				    }
				    if (info > 0) {
					++ninfo[2];
				    }
				    if (itrans == 1) {
					tmp = a_ref(1, 2);
					a_ref(1, 2) = a_ref(2, 1);
					a_ref(2, 1) = tmp;
				    }
				    res = (r__1 = (ca * a_ref(1, 1) - wr * d1)
					     * x_ref(1, 1) + ca * a_ref(1, 2) 
					    * x_ref(2, 1) - scale * b_ref(1, 
					    1), dabs(r__1));
				    res += (r__1 = ca * a_ref(2, 1) * x_ref(1,
					     1) + (ca * a_ref(2, 2) - wr * d2)
					     * x_ref(2, 1) - scale * b_ref(2, 
					    1), dabs(r__1));
				    if (info == 0) {
/* Computing MAX   
   Computing MAX */
					r__8 = (r__1 = ca * a_ref(1, 1) - wr *
						 d1, dabs(r__1)) + (r__2 = ca 
						* a_ref(1, 2), dabs(r__2)), 
						r__9 = (r__3 = ca * a_ref(2, 
						1), dabs(r__3)) + (r__4 = ca *
						 a_ref(2, 2) - wr * d2, dabs(
						r__4));
/* Computing MAX */
					r__10 = (r__5 = x_ref(1, 1), dabs(
						r__5)), r__11 = (r__6 = x_ref(
						2, 1), dabs(r__6));
					r__7 = eps * (dmax(r__8,r__9) * dmax(
						r__10,r__11));
					den = dmax(r__7,smlnum);
				    } else {
/* Computing MAX   
   Computing MAX   
   Computing MAX */
					r__10 = (r__1 = ca * a_ref(1, 1) - wr 
						* d1, dabs(r__1)) + (r__2 = 
						ca * a_ref(1, 2), dabs(r__2)),
						 r__11 = (r__3 = ca * a_ref(2,
						 1), dabs(r__3)) + (r__4 = ca 
						* a_ref(2, 2) - wr * d2, dabs(
						r__4));
					r__8 = smin / eps, r__9 = dmax(r__10,
						r__11);
/* Computing MAX */
					r__12 = (r__5 = x_ref(1, 1), dabs(
						r__5)), r__13 = (r__6 = x_ref(
						2, 1), dabs(r__6));
					r__7 = eps * (dmax(r__8,r__9) * dmax(
						r__12,r__13));
					den = dmax(r__7,smlnum);
				    }
				    res /= den;
				    if ((r__1 = x_ref(1, 1), dabs(r__1)) < 
					    unfl && (r__2 = x_ref(2, 1), dabs(
					    r__2)) < unfl && (r__3 = b_ref(1, 
					    1), dabs(r__3)) + (r__4 = b_ref(2,
					     1), dabs(r__4)) <= smlnum * ((
					    r__5 = ca * a_ref(1, 1) - wr * d1,
					     dabs(r__5)) + (r__6 = ca * a_ref(
					    1, 2), dabs(r__6)) + (r__7 = ca * 
					    a_ref(2, 1), dabs(r__7)) + (r__8 =
					     ca * a_ref(2, 2) - wr * d2, dabs(
					    r__8)))) {
					res = 0.f;
				    }
				    if (scale > 1.f) {
					res += 1.f / eps;
				    }
/* Computing MAX */
				    r__4 = (r__1 = x_ref(1, 1), dabs(r__1)), 
					    r__5 = (r__2 = x_ref(2, 1), dabs(
					    r__2));
				    res += (r__3 = xnorm - dmax(r__4,r__5), 
					    dabs(r__3)) / dmax(smlnum,xnorm) /
					     eps;
				    if (info != 0 && info != 1) {
					res += 1.f / eps;
				    }
				    ++(*knt);
				    if (res > *rmax) {
					*lmax = *knt;
					*rmax = res;
				    }
/* L80: */
				}
/* L90: */
			    }
/* L100: */
			}

			na = 2;
			nw = 2;
			for (ia = 1; ia <= 3; ++ia) {
			    a_ref(1, 1) = vab[ia - 1] * 2.f;
			    a_ref(1, 2) = vab[ia - 1] * -3.f;
			    a_ref(2, 1) = vab[ia - 1] * -7.f;
			    a_ref(2, 2) = vab[ia - 1] * 21.f;
			    for (ib = 1; ib <= 3; ++ib) {
				b_ref(1, 1) = vab[ib - 1];
				b_ref(2, 1) = vab[ib - 1] * -2.f;
				b_ref(1, 2) = vab[ib - 1] * 4.f;
				b_ref(2, 2) = vab[ib - 1] * -7.f;
				for (iwr = 1; iwr <= 4; ++iwr) {
				    if (d1 == 1.f && d2 == 1.f && ca == 1.f) {
					wr = vwr[iwr - 1] * a_ref(1, 1);
				    } else {
					wr = vwr[iwr - 1];
				    }
				    for (iwi = 1; iwi <= 4; ++iwi) {
					if (d1 == 1.f && d2 == 1.f && ca == 
						1.f) {
					    wi = vwi[iwi - 1] * a_ref(1, 1);
					} else {
					    wi = vwi[iwi - 1];
					}
					slaln2_(&ltrans[itrans], &na, &nw, &
						smin, &ca, a, &c__2, &d1, &d2,
						 b, &c__2, &wr, &wi, x, &c__2,
						 &scale, &xnorm, &info);
					if (info < 0) {
					    ++ninfo[1];
					}
					if (info > 0) {
					    ++ninfo[2];
					}
					if (itrans == 1) {
					    tmp = a_ref(1, 2);
					    a_ref(1, 2) = a_ref(2, 1);
					    a_ref(2, 1) = tmp;
					}
					res = (r__1 = (ca * a_ref(1, 1) - wr *
						 d1) * x_ref(1, 1) + ca * 
						a_ref(1, 2) * x_ref(2, 1) + 
						wi * d1 * x_ref(1, 2) - scale 
						* b_ref(1, 1), dabs(r__1));
					res += (r__1 = (ca * a_ref(1, 1) - wr 
						* d1) * x_ref(1, 2) + ca * 
						a_ref(1, 2) * x_ref(2, 2) - 
						wi * d1 * x_ref(1, 1) - scale 
						* b_ref(1, 2), dabs(r__1));
					res += (r__1 = ca * a_ref(2, 1) * 
						x_ref(1, 1) + (ca * a_ref(2, 
						2) - wr * d2) * x_ref(2, 1) + 
						wi * d2 * x_ref(2, 2) - scale 
						* b_ref(2, 1), dabs(r__1));
					res += (r__1 = ca * a_ref(2, 1) * 
						x_ref(1, 2) + (ca * a_ref(2, 
						2) - wr * d2) * x_ref(2, 2) - 
						wi * d2 * x_ref(2, 1) - scale 
						* b_ref(2, 2), dabs(r__1));
					if (info == 0) {
/* Computing MAX   
   Computing MAX */
					    r__12 = (r__1 = ca * a_ref(1, 1) 
						    - wr * d1, dabs(r__1)) + (
						    r__2 = ca * a_ref(1, 2), 
						    dabs(r__2)) + (r__3 = wi *
						     d1, dabs(r__3)), r__13 = 
						    (r__4 = ca * a_ref(2, 1), 
						    dabs(r__4)) + (r__5 = ca *
						     a_ref(2, 2) - wr * d2, 
						    dabs(r__5)) + (r__6 = wi *
						     d2, dabs(r__6));
/* Computing MAX */
					    r__14 = (r__7 = x_ref(1, 1), dabs(
						    r__7)) + (r__8 = x_ref(2, 
						    1), dabs(r__8)), r__15 = (
						    r__9 = x_ref(1, 2), dabs(
						    r__9)) + (r__10 = x_ref(2,
						     2), dabs(r__10));
					    r__11 = eps * (dmax(r__12,r__13) *
						     dmax(r__14,r__15));
					    den = dmax(r__11,smlnum);
					} else {
/* Computing MAX   
   Computing MAX   
   Computing MAX */
					    r__14 = (r__1 = ca * a_ref(1, 1) 
						    - wr * d1, dabs(r__1)) + (
						    r__2 = ca * a_ref(1, 2), 
						    dabs(r__2)) + (r__3 = wi *
						     d1, dabs(r__3)), r__15 = 
						    (r__4 = ca * a_ref(2, 1), 
						    dabs(r__4)) + (r__5 = ca *
						     a_ref(2, 2) - wr * d2, 
						    dabs(r__5)) + (r__6 = wi *
						     d2, dabs(r__6));
					    r__12 = smin / eps, r__13 = dmax(
						    r__14,r__15);
/* Computing MAX */
					    r__16 = (r__7 = x_ref(1, 1), dabs(
						    r__7)) + (r__8 = x_ref(2, 
						    1), dabs(r__8)), r__17 = (
						    r__9 = x_ref(1, 2), dabs(
						    r__9)) + (r__10 = x_ref(2,
						     2), dabs(r__10));
					    r__11 = eps * (dmax(r__12,r__13) *
						     dmax(r__16,r__17));
					    den = dmax(r__11,smlnum);
					}
					res /= den;
					if ((r__1 = x_ref(1, 1), dabs(r__1)) <
						 unfl && (r__2 = x_ref(2, 1), 
						dabs(r__2)) < unfl && (r__3 = 
						x_ref(1, 2), dabs(r__3)) < 
						unfl && (r__4 = x_ref(2, 2), 
						dabs(r__4)) < unfl && (r__5 = 
						b_ref(1, 1), dabs(r__5)) + (
						r__6 = b_ref(2, 1), dabs(r__6)
						) <= smlnum * ((r__7 = ca * 
						a_ref(1, 1) - wr * d1, dabs(
						r__7)) + (r__8 = ca * a_ref(1,
						 2), dabs(r__8)) + (r__9 = ca 
						* a_ref(2, 1), dabs(r__9)) + (
						r__10 = ca * a_ref(2, 2) - wr 
						* d2, dabs(r__10)) + (r__11 = 
						wi * d2, dabs(r__11)) + (
						r__12 = wi * d1, dabs(r__12)))
						) {
					    res = 0.f;
					}
					if (scale > 1.f) {
					    res += 1.f / eps;
					}
/* Computing MAX */
					r__6 = (r__1 = x_ref(1, 1), dabs(r__1)
						) + (r__2 = x_ref(1, 2), dabs(
						r__2)), r__7 = (r__3 = x_ref(
						2, 1), dabs(r__3)) + (r__4 = 
						x_ref(2, 2), dabs(r__4));
					res += (r__5 = xnorm - dmax(r__6,r__7)
						, dabs(r__5)) / dmax(smlnum,
						xnorm) / eps;
					if (info != 0 && info != 1) {
					    res += 1.f / eps;
					}
					++(*knt);
					if (res > *rmax) {
					    *lmax = *knt;
					    *rmax = res;
					}
/* L110: */
				    }
/* L120: */
				}
/* L130: */
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

    return 0;

/*     End of SGET31 */

} /* sget31_ */

#undef x_ref
#undef b_ref
#undef a_ref


