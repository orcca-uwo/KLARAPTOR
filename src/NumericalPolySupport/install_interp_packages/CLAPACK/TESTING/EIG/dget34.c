#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__16 = 16;
static integer c__0 = 0;
static integer c__1 = 1;
static integer c__4 = 4;
static integer c__5 = 5;
static logical c_true = TRUE_;
static integer c__2 = 2;
static integer c__32 = 32;
static integer c__3 = 3;
static doublereal c_b64 = 1.;

/* Subroutine */ int dget34_(doublereal *rmax, integer *lmax, integer *ninfo, 
	integer *knt)
{
    /* System generated locals */
    doublereal d__1, d__2, d__3, d__4, d__5, d__6, d__7, d__8, d__9;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    static integer info;
    static doublereal tnrm, work[32];
    static integer i__, j;
    static doublereal q[16]	/* was [4][4] */, t[16]	/* was [4][4] */;
    extern /* Subroutine */ int dhst01_(integer *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, integer *, doublereal *, 
	    integer *, doublereal *, integer *, doublereal *), dcopy_(integer 
	    *, doublereal *, integer *, doublereal *, integer *);
    static doublereal t1[16]	/* was [4][4] */;
    extern /* Subroutine */ int dlabad_(doublereal *, doublereal *);
    static integer ia, ib, ic;
    extern doublereal dlamch_(char *);
    extern /* Subroutine */ int dlaexc_(logical *, integer *, doublereal *, 
	    integer *, doublereal *, integer *, integer *, integer *, integer 
	    *, doublereal *, integer *);
    static doublereal vm[2], bignum, smlnum, result[2];
    static integer ia11, ia12, ia21, ia22, ic11, ic12, ic21, ic22, iam, icm;
    static doublereal val[9], eps, res;


#define t_ref(a_1,a_2) t[(a_2)*4 + a_1 - 5]
#define t1_ref(a_1,a_2) t1[(a_2)*4 + a_1 - 5]


/*  -- LAPACK test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 29, 1992   


    Purpose   
    =======   

    DGET34 tests DLAEXC, a routine for swapping adjacent blocks (either   
    1 by 1 or 2 by 2) on the diagonal of a matrix in real Schur form.   
    Thus, DLAEXC computes an orthogonal matrix Q such that   

        Q' * [ A B ] * Q  = [ C1 B1 ]   
             [ 0 C ]        [ 0  A1 ]   

    where C1 is similar to C and A1 is similar to A.  Both A and C are   
    assumed to be in standard form (equal diagonal entries and   
    offdiagonal with differing signs) and A1 and C1 are returned with the   
    same properties.   

    The test code verifies these last last assertions, as well as that   
    the residual in the above equation is small.   

    Arguments   
    ==========   

    RMAX    (output) DOUBLE PRECISION   
            Value of the largest test ratio.   

    LMAX    (output) INTEGER   
            Example number where largest test ratio achieved.   

    NINFO   (output) INTEGER array, dimension (2)   
            NINFO(J) is the number of examples where INFO=J occurred.   

    KNT     (output) INTEGER   
            Total number of examples tested.   

    =====================================================================   


       Get machine parameters   

       Parameter adjustments */
    --ninfo;

    /* Function Body */
    eps = dlamch_("P");
    smlnum = dlamch_("S") / eps;
    bignum = 1. / smlnum;
    dlabad_(&smlnum, &bignum);

/*     Set up test case parameters */

    val[0] = 0.;
    val[1] = sqrt(smlnum);
    val[2] = 1.;
    val[3] = 2.;
    val[4] = sqrt(bignum);
    val[5] = -sqrt(smlnum);
    val[6] = -1.;
    val[7] = -2.;
    val[8] = -sqrt(bignum);
    vm[0] = 1.;
    vm[1] = eps * 2. + 1.;
    dcopy_(&c__16, &val[3], &c__0, &t_ref(1, 1), &c__1);

    ninfo[1] = 0;
    ninfo[2] = 0;
    *knt = 0;
    *lmax = 0;
    *rmax = 0.;

/*     Begin test loop */

    for (ia = 1; ia <= 9; ++ia) {
	for (iam = 1; iam <= 2; ++iam) {
	    for (ib = 1; ib <= 9; ++ib) {
		for (ic = 1; ic <= 9; ++ic) {
		    t_ref(1, 1) = val[ia - 1] * vm[iam - 1];
		    t_ref(2, 2) = val[ic - 1];
		    t_ref(1, 2) = val[ib - 1];
		    t_ref(2, 1) = 0.;
/* Computing MAX */
		    d__4 = (d__1 = t_ref(1, 1), abs(d__1)), d__5 = (d__2 = 
			    t_ref(2, 2), abs(d__2)), d__4 = max(d__4,d__5), 
			    d__5 = (d__3 = t_ref(1, 2), abs(d__3));
		    tnrm = max(d__4,d__5);
		    dcopy_(&c__16, t, &c__1, t1, &c__1);
		    dcopy_(&c__16, val, &c__0, q, &c__1);
		    dcopy_(&c__4, &val[2], &c__0, q, &c__5);
		    dlaexc_(&c_true, &c__2, t, &c__4, q, &c__4, &c__1, &c__1, 
			    &c__1, work, &info);
		    if (info != 0) {
			++ninfo[info];
		    }
		    dhst01_(&c__2, &c__1, &c__2, t1, &c__4, t, &c__4, q, &
			    c__4, work, &c__32, result);
		    res = result[0] + result[1];
		    if (info != 0) {
			res += 1. / eps;
		    }
		    if (t_ref(1, 1) != t1_ref(2, 2)) {
			res += 1. / eps;
		    }
		    if (t_ref(2, 2) != t1_ref(1, 1)) {
			res += 1. / eps;
		    }
		    if (t_ref(2, 1) != 0.) {
			res += 1. / eps;
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
/* L40: */
    }

    for (ia = 1; ia <= 5; ++ia) {
	for (iam = 1; iam <= 2; ++iam) {
	    for (ib = 1; ib <= 5; ++ib) {
		for (ic11 = 1; ic11 <= 5; ++ic11) {
		    for (ic12 = 2; ic12 <= 5; ++ic12) {
			for (ic21 = 2; ic21 <= 4; ++ic21) {
			    for (ic22 = -1; ic22 <= 1; ic22 += 2) {
				t_ref(1, 1) = val[ia - 1] * vm[iam - 1];
				t_ref(1, 2) = val[ib - 1];
				t_ref(1, 3) = val[ib - 1] * -2.;
				t_ref(2, 1) = 0.;
				t_ref(2, 2) = val[ic11 - 1];
				t_ref(2, 3) = val[ic12 - 1];
				t_ref(3, 1) = 0.;
				t_ref(3, 2) = -val[ic21 - 1];
				t_ref(3, 3) = val[ic11 - 1] * (doublereal) 
					ic22;
/* Computing MAX */
				d__8 = (d__1 = t_ref(1, 1), abs(d__1)), d__9 =
					 (d__2 = t_ref(1, 2), abs(d__2)), 
					d__8 = max(d__8,d__9), d__9 = (d__3 = 
					t_ref(1, 3), abs(d__3)), d__8 = max(
					d__8,d__9), d__9 = (d__4 = t_ref(2, 2)
					, abs(d__4)), d__8 = max(d__8,d__9), 
					d__9 = (d__5 = t_ref(2, 3), abs(d__5))
					, d__8 = max(d__8,d__9), d__9 = (d__6 
					= t_ref(3, 2), abs(d__6)), d__8 = max(
					d__8,d__9), d__9 = (d__7 = t_ref(3, 3)
					, abs(d__7));
				tnrm = max(d__8,d__9);
				dcopy_(&c__16, t, &c__1, t1, &c__1);
				dcopy_(&c__16, val, &c__0, q, &c__1);
				dcopy_(&c__4, &val[2], &c__0, q, &c__5);
				dlaexc_(&c_true, &c__3, t, &c__4, q, &c__4, &
					c__1, &c__1, &c__2, work, &info);
				if (info != 0) {
				    ++ninfo[info];
				}
				dhst01_(&c__3, &c__1, &c__3, t1, &c__4, t, &
					c__4, q, &c__4, work, &c__32, result);
				res = result[0] + result[1];
				if (info == 0) {
				    if (t1_ref(1, 1) != t_ref(3, 3)) {
					res += 1. / eps;
				    }
				    if (t_ref(3, 1) != 0.) {
					res += 1. / eps;
				    }
				    if (t_ref(3, 2) != 0.) {
					res += 1. / eps;
				    }
				    if (t_ref(2, 1) != 0. && (t_ref(1, 1) != 
					    t_ref(2, 2) || d_sign(&c_b64, &
					    t_ref(1, 2)) == d_sign(&c_b64, &
					    t_ref(2, 1)))) {
					res += 1. / eps;
				    }
				}
				++(*knt);
				if (res > *rmax) {
				    *lmax = *knt;
				    *rmax = res;
				}
/* L50: */
			    }
/* L60: */
			}
/* L70: */
		    }
/* L80: */
		}
/* L90: */
	    }
/* L100: */
	}
/* L110: */
    }

    for (ia11 = 1; ia11 <= 5; ++ia11) {
	for (ia12 = 2; ia12 <= 5; ++ia12) {
	    for (ia21 = 2; ia21 <= 4; ++ia21) {
		for (ia22 = -1; ia22 <= 1; ia22 += 2) {
		    for (icm = 1; icm <= 2; ++icm) {
			for (ib = 1; ib <= 5; ++ib) {
			    for (ic = 1; ic <= 5; ++ic) {
				t_ref(1, 1) = val[ia11 - 1];
				t_ref(1, 2) = val[ia12 - 1];
				t_ref(1, 3) = val[ib - 1] * -2.;
				t_ref(2, 1) = -val[ia21 - 1];
				t_ref(2, 2) = val[ia11 - 1] * (doublereal) 
					ia22;
				t_ref(2, 3) = val[ib - 1];
				t_ref(3, 1) = 0.;
				t_ref(3, 2) = 0.;
				t_ref(3, 3) = val[ic - 1] * vm[icm - 1];
/* Computing MAX */
				d__8 = (d__1 = t_ref(1, 1), abs(d__1)), d__9 =
					 (d__2 = t_ref(1, 2), abs(d__2)), 
					d__8 = max(d__8,d__9), d__9 = (d__3 = 
					t_ref(1, 3), abs(d__3)), d__8 = max(
					d__8,d__9), d__9 = (d__4 = t_ref(2, 2)
					, abs(d__4)), d__8 = max(d__8,d__9), 
					d__9 = (d__5 = t_ref(2, 3), abs(d__5))
					, d__8 = max(d__8,d__9), d__9 = (d__6 
					= t_ref(3, 2), abs(d__6)), d__8 = max(
					d__8,d__9), d__9 = (d__7 = t_ref(3, 3)
					, abs(d__7));
				tnrm = max(d__8,d__9);
				dcopy_(&c__16, t, &c__1, t1, &c__1);
				dcopy_(&c__16, val, &c__0, q, &c__1);
				dcopy_(&c__4, &val[2], &c__0, q, &c__5);
				dlaexc_(&c_true, &c__3, t, &c__4, q, &c__4, &
					c__1, &c__2, &c__1, work, &info);
				if (info != 0) {
				    ++ninfo[info];
				}
				dhst01_(&c__3, &c__1, &c__3, t1, &c__4, t, &
					c__4, q, &c__4, work, &c__32, result);
				res = result[0] + result[1];
				if (info == 0) {
				    if (t1_ref(3, 3) != t_ref(1, 1)) {
					res += 1. / eps;
				    }
				    if (t_ref(2, 1) != 0.) {
					res += 1. / eps;
				    }
				    if (t_ref(3, 1) != 0.) {
					res += 1. / eps;
				    }
				    if (t_ref(3, 2) != 0. && (t_ref(2, 2) != 
					    t_ref(3, 3) || d_sign(&c_b64, &
					    t_ref(2, 3)) == d_sign(&c_b64, &
					    t_ref(3, 2)))) {
					res += 1. / eps;
				    }
				}
				++(*knt);
				if (res > *rmax) {
				    *lmax = *knt;
				    *rmax = res;
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

    for (ia11 = 1; ia11 <= 5; ++ia11) {
	for (ia12 = 2; ia12 <= 5; ++ia12) {
	    for (ia21 = 2; ia21 <= 4; ++ia21) {
		for (ia22 = -1; ia22 <= 1; ia22 += 2) {
		    for (ib = 1; ib <= 5; ++ib) {
			for (ic11 = 3; ic11 <= 4; ++ic11) {
			    for (ic12 = 3; ic12 <= 4; ++ic12) {
				for (ic21 = 3; ic21 <= 4; ++ic21) {
				    for (ic22 = -1; ic22 <= 1; ic22 += 2) {
					for (icm = 5; icm <= 7; ++icm) {
					    iam = 1;
					    t_ref(1, 1) = val[ia11 - 1] * vm[
						    iam - 1];
					    t_ref(1, 2) = val[ia12 - 1] * vm[
						    iam - 1];
					    t_ref(1, 3) = val[ib - 1] * -2.;
					    t_ref(1, 4) = val[ib - 1] * .5;
					    t_ref(2, 1) = -t_ref(1, 2) * val[
						    ia21 - 1];
					    t_ref(2, 2) = val[ia11 - 1] * (
						    doublereal) ia22 * vm[iam 
						    - 1];
					    t_ref(2, 3) = val[ib - 1];
					    t_ref(2, 4) = val[ib - 1] * 3.;
					    t_ref(3, 1) = 0.;
					    t_ref(3, 2) = 0.;
					    t_ref(3, 3) = val[ic11 - 1] * (
						    d__1 = val[icm - 1], abs(
						    d__1));
					    t_ref(3, 4) = val[ic12 - 1] * (
						    d__1 = val[icm - 1], abs(
						    d__1));
					    t_ref(4, 1) = 0.;
					    t_ref(4, 2) = 0.;
					    t_ref(4, 3) = -t_ref(3, 4) * val[
						    ic21 - 1] * (d__1 = val[
						    icm - 1], abs(d__1));
					    t_ref(4, 4) = val[ic11 - 1] * (
						    doublereal) ic22 * (d__1 =
						     val[icm - 1], abs(d__1));
					    tnrm = 0.;
					    for (i__ = 1; i__ <= 4; ++i__) {
			  for (j = 1; j <= 4; ++j) {
/* Computing MAX */
			      d__2 = tnrm, d__3 = (d__1 = t_ref(i__, j), abs(
				      d__1));
			      tnrm = max(d__2,d__3);
/* L190: */
			  }
/* L200: */
					    }
					    dcopy_(&c__16, t, &c__1, t1, &
						    c__1);
					    dcopy_(&c__16, val, &c__0, q, &
						    c__1);
					    dcopy_(&c__4, &val[2], &c__0, q, &
						    c__5);
					    dlaexc_(&c_true, &c__4, t, &c__4, 
						    q, &c__4, &c__1, &c__2, &
						    c__2, work, &info);
					    if (info != 0) {
			  ++ninfo[info];
					    }
					    dhst01_(&c__4, &c__1, &c__4, t1, &
						    c__4, t, &c__4, q, &c__4, 
						    work, &c__32, result);
					    res = result[0] + result[1];
					    if (info == 0) {
			  if (t_ref(3, 1) != 0.) {
			      res += 1. / eps;
			  }
			  if (t_ref(4, 1) != 0.) {
			      res += 1. / eps;
			  }
			  if (t_ref(3, 2) != 0.) {
			      res += 1. / eps;
			  }
			  if (t_ref(4, 2) != 0.) {
			      res += 1. / eps;
			  }
			  if (t_ref(2, 1) != 0. && (t_ref(1, 1) != t_ref(2, 2)
				   || d_sign(&c_b64, &t_ref(1, 2)) == d_sign(&
				  c_b64, &t_ref(2, 1)))) {
			      res += 1. / eps;
			  }
			  if (t_ref(4, 3) != 0. && (t_ref(3, 3) != t_ref(4, 4)
				   || d_sign(&c_b64, &t_ref(3, 4)) == d_sign(&
				  c_b64, &t_ref(4, 3)))) {
			      res += 1. / eps;
			  }
					    }
					    ++(*knt);
					    if (res > *rmax) {
			  *lmax = *knt;
			  *rmax = res;
					    }
/* L210: */
					}
/* L220: */
				    }
/* L230: */
				}
/* L240: */
			    }
/* L250: */
			}
/* L260: */
		    }
/* L270: */
		}
/* L280: */
	    }
/* L290: */
	}
/* L300: */
    }

    return 0;

/*     End of DGET34 */

} /* dget34_ */

#undef t1_ref
#undef t_ref


