#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    real ops, itcnt;
} latime_;

#define latime_1 latime_

/* Table of constant values */

static integer c__1 = 1;
static real c_b5 = 1.f;

/* Subroutine */ int slaic1_(integer *job, integer *j, real *x, real *sest, 
	real *w, real *gamma, real *sestpr, real *s, real *c__)
{
    /* System generated locals */
    real r__1, r__2, r__3, r__4;

    /* Builtin functions */
    double sqrt(doublereal), r_sign(real *, real *);

    /* Local variables */
    static real sine;
    extern doublereal sdot_(integer *, real *, integer *, real *, integer *);
    static real test, zeta1, zeta2, b, t, alpha, norma, s1, s2, absgam, 
	    absalp;
    extern doublereal slamch_(char *);
    static real cosine, absest, eps, tmp;


/*  -- LAPACK auxiliary routine (instrumented to count ops, version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   


    Purpose   
    =======   

    SLAIC1 applies one step of incremental condition estimation in   
    its simplest version:   

    Let x, twonorm(x) = 1, be an approximate singular vector of an j-by-j   
    lower triangular matrix L, such that   
             twonorm(L*x) = sest   
    Then SLAIC1 computes sestpr, s, c such that   
    the vector   
                    [ s*x ]   
             xhat = [  c  ]   
    is an approximate singular vector of   
                    [ L     0  ]   
             Lhat = [ w' gamma ]   
    in the sense that   
             twonorm(Lhat*xhat) = sestpr.   

    Depending on JOB, an estimate for the largest or smallest singular   
    value is computed.   

    Note that [s c]' and sestpr**2 is an eigenpair of the system   

        diag(sest*sest, 0) + [alpha  gamma] * [ alpha ]   
                                              [ gamma ]   

    where  alpha =  x'*w.   

    Arguments   
    =========   

    JOB     (input) INTEGER   
            = 1: an estimate for the largest singular value is computed.   
            = 2: an estimate for the smallest singular value is computed.   

    J       (input) INTEGER   
            Length of X and W   

    X       (input) REAL array, dimension (J)   
            The j-vector x.   

    SEST    (input) REAL   
            Estimated singular value of j by j matrix L   

    W       (input) REAL array, dimension (J)   
            The j-vector w.   

    GAMMA   (input) REAL   
            The diagonal element gamma.   

    SESTPR  (output) REAL   
            Estimated singular value of (j+1) by (j+1) matrix Lhat.   

    S       (output) REAL   
            Sine needed in forming xhat.   

    C       (output) REAL   
            Cosine needed in forming xhat.   

    =====================================================================   


       Parameter adjustments */
    --w;
    --x;

    /* Function Body */
    eps = slamch_("Epsilon");
    alpha = sdot_(j, &x[1], &c__1, &w[1], &c__1);

    absalp = dabs(alpha);
    absgam = dabs(*gamma);
    absest = dabs(*sest);

    if (*job == 1) {

/*        Estimating largest singular value   

          special cases */

	if (*sest == 0.f) {
	    s1 = dmax(absgam,absalp);
	    if (s1 == 0.f) {
		*s = 0.f;
		*c__ = 1.f;
		*sestpr = 0.f;
	    } else {
		latime_1.ops += 9;
		*s = alpha / s1;
		*c__ = *gamma / s1;
		tmp = sqrt(*s * *s + *c__ * *c__);
		*s /= tmp;
		*c__ /= tmp;
		*sestpr = s1 * tmp;
	    }
	    return 0;
	} else if (absgam <= eps * absest) {
	    latime_1.ops += 7;
	    *s = 1.f;
	    *c__ = 0.f;
	    tmp = dmax(absest,absalp);
	    s1 = absest / tmp;
	    s2 = absalp / tmp;
	    *sestpr = tmp * sqrt(s1 * s1 + s2 * s2);
	    return 0;
	} else if (absalp <= eps * absest) {
	    s1 = absgam;
	    s2 = absest;
	    if (s1 <= s2) {
		*s = 1.f;
		*c__ = 0.f;
		*sestpr = s2;
	    } else {
		*s = 0.f;
		*c__ = 1.f;
		*sestpr = s1;
	    }
	    return 0;
	} else if (absest <= eps * absalp || absest <= eps * absgam) {
	    s1 = absgam;
	    s2 = absalp;
	    if (s1 <= s2) {
		latime_1.ops += 8;
		tmp = s1 / s2;
		*s = sqrt(tmp * tmp + 1.f);
		*sestpr = s2 * *s;
		*c__ = *gamma / s2 / *s;
		*s = r_sign(&c_b5, &alpha) / *s;
	    } else {
		latime_1.ops += 8;
		tmp = s2 / s1;
		*c__ = sqrt(tmp * tmp + 1.f);
		*sestpr = s1 * *c__;
		*s = alpha / s1 / *c__;
		*c__ = r_sign(&c_b5, gamma) / *c__;
	    }
	    return 0;
	} else {

/*           normal case */

	    latime_1.ops += 8;
	    zeta1 = alpha / absest;
	    zeta2 = *gamma / absest;

	    b = (1.f - zeta1 * zeta1 - zeta2 * zeta2) * .5f;
	    *c__ = zeta1 * zeta1;
	    if (b > 0.f) {
		latime_1.ops += 5;
		t = *c__ / (b + sqrt(b * b + *c__));
	    } else {
		latime_1.ops += 4;
		t = sqrt(b * b + *c__) - b;
	    }

	    latime_1.ops += 12;
	    sine = -zeta1 / t;
	    cosine = -zeta2 / (t + 1.f);
	    tmp = sqrt(sine * sine + cosine * cosine);
	    *s = sine / tmp;
	    *c__ = cosine / tmp;
	    *sestpr = sqrt(t + 1.f) * absest;
	    return 0;
	}

    } else if (*job == 2) {

/*        Estimating smallest singular value   

          special cases */

	if (*sest == 0.f) {
	    *sestpr = 0.f;
	    if (dmax(absgam,absalp) == 0.f) {
		sine = 1.f;
		cosine = 0.f;
	    } else {
		sine = -(*gamma);
		cosine = alpha;
	    }
	    latime_1.ops += 7;
/* Computing MAX */
	    r__1 = dabs(sine), r__2 = dabs(cosine);
	    s1 = dmax(r__1,r__2);
	    *s = sine / s1;
	    *c__ = cosine / s1;
	    tmp = sqrt(*s * *s + *c__ * *c__);
	    *s /= tmp;
	    *c__ /= tmp;
	    return 0;
	} else if (absgam <= eps * absest) {
	    *s = 0.f;
	    *c__ = 1.f;
	    *sestpr = absgam;
	    return 0;
	} else if (absalp <= eps * absest) {
	    s1 = absgam;
	    s2 = absest;
	    if (s1 <= s2) {
		*s = 0.f;
		*c__ = 1.f;
		*sestpr = s1;
	    } else {
		*s = 1.f;
		*c__ = 0.f;
		*sestpr = s2;
	    }
	    return 0;
	} else if (absest <= eps * absalp || absest <= eps * absgam) {
	    s1 = absgam;
	    s2 = absalp;
	    if (s1 <= s2) {
		latime_1.ops += 9;
		tmp = s1 / s2;
		*c__ = sqrt(tmp * tmp + 1.f);
		*sestpr = absest * (tmp / *c__);
		*s = -(*gamma / s2) / *c__;
		*c__ = r_sign(&c_b5, &alpha) / *c__;
	    } else {
		latime_1.ops += 8;
		tmp = s2 / s1;
		*s = sqrt(tmp * tmp + 1.f);
		*sestpr = absest / *s;
		*c__ = alpha / s1 / *s;
		*s = -r_sign(&c_b5, gamma) / *s;
	    }
	    return 0;
	} else {

/*           normal case */

	    latime_1.ops += 14;
	    zeta1 = alpha / absest;
	    zeta2 = *gamma / absest;

/* Computing MAX */
	    r__3 = zeta1 * zeta1 + 1.f + (r__1 = zeta1 * zeta2, dabs(r__1)), 
		    r__4 = (r__2 = zeta1 * zeta2, dabs(r__2)) + zeta2 * zeta2;
	    norma = dmax(r__3,r__4);

/*           See if root is closer to zero or to ONE */

	    test = (zeta1 - zeta2) * 2.f * (zeta1 + zeta2) + 1.f;
	    if (test >= 0.f) {

/*              root is close to zero, compute directly */

		latime_1.ops += 20;
		b = (zeta1 * zeta1 + zeta2 * zeta2 + 1.f) * .5f;
		*c__ = zeta2 * zeta2;
		t = *c__ / (b + sqrt((r__1 = b * b - *c__, dabs(r__1))));
		sine = zeta1 / (1.f - t);
		cosine = -zeta2 / t;
		*sestpr = sqrt(t + eps * 4.f * eps * norma) * absest;
	    } else {

/*              root is closer to ONE, shift by that amount */

		latime_1.ops += 6;
		b = (zeta2 * zeta2 + zeta1 * zeta1 - 1.f) * .5f;
		*c__ = zeta1 * zeta1;
		if (b >= 0.f) {
		    latime_1.ops += 5;
		    t = -(*c__) / (b + sqrt(b * b + *c__));
		} else {
		    latime_1.ops += 4;
		    t = b - sqrt(b * b + *c__);
		}
		latime_1.ops += 10;
		sine = -zeta1 / t;
		cosine = -zeta2 / (t + 1.f);
		*sestpr = sqrt(t + 1.f + eps * 4.f * eps * norma) * absest;
	    }
	    latime_1.ops += 6;
	    tmp = sqrt(sine * sine + cosine * cosine);
	    *s = sine / tmp;
	    *c__ = cosine / tmp;
	    return 0;

	}
    }
    return 0;

/*     End of SLAIC1 */

} /* slaic1_ */

