#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Subroutine */ int dstect_(integer *n, doublereal *a, doublereal *b, 
	doublereal *shift, integer *num)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2, d__3, d__4;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static doublereal unfl, ovfl, ssun;
    static integer i__;
    static doublereal u, m1, m2;
    extern doublereal dlamch_(char *);
    static doublereal mx, sshift, tmp, tom, sun, sov;


/*  -- LAPACK test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 29, 1992   


    Purpose   
    =======   

       DSTECT counts the number NUM of eigenvalues of a tridiagonal   
       matrix T which are less than or equal to SHIFT. T has   
       diagonal entries A(1), ... , A(N), and offdiagonal entries   
       B(1), ..., B(N-1).   
       See W. Kahan "Accurate Eigenvalues of a Symmetric Tridiagonal   
       Matrix", Report CS41, Computer Science Dept., Stanford   
       University, July 21, 1966   

    Arguments   
    =========   

    N       (input) INTEGER   
            The dimension of the tridiagonal matrix T.   

    A       (input) DOUBLE PRECISION array, dimension (N)   
            The diagonal entries of the tridiagonal matrix T.   

    B       (input) DOUBLE PRECISION array, dimension (N-1)   
            The offdiagonal entries of the tridiagonal matrix T.   

    SHIFT   (input) DOUBLE PRECISION   
            The shift, used as described under Purpose.   

    NUM     (output) INTEGER   
            The number of eigenvalues of T less than or equal   
            to SHIFT.   

    =====================================================================   


       Get machine constants   

       Parameter adjustments */
    --b;
    --a;

    /* Function Body */
    unfl = dlamch_("Safe minimum");
    ovfl = dlamch_("Overflow");

/*     Find largest entry */

    mx = abs(a[1]);
    i__1 = *n - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* Computing MAX */
	d__3 = mx, d__4 = (d__1 = a[i__ + 1], abs(d__1)), d__3 = max(d__3,
		d__4), d__4 = (d__2 = b[i__], abs(d__2));
	mx = max(d__3,d__4);
/* L10: */
    }

/*     Handle easy cases, including zero matrix */

    if (*shift >= mx * 3.) {
	*num = *n;
	return 0;
    }
    if (*shift < mx * -3.) {
	*num = 0;
	return 0;
    }

/*     Compute scale factors as in Kahan's report   
       At this point, MX .NE. 0 so we can divide by it */

    sun = sqrt(unfl);
    ssun = sqrt(sun);
    sov = sqrt(ovfl);
    tom = ssun * sov;
    if (mx <= 1.) {
	m1 = 1. / mx;
	m2 = tom;
    } else {
	m1 = 1.;
	m2 = tom / mx;
    }

/*     Begin counting */

    *num = 0;
    sshift = *shift * m1 * m2;
    u = a[1] * m1 * m2 - sshift;
    if (u <= sun) {
	if (u <= 0.) {
	    ++(*num);
	    if (u > -sun) {
		u = -sun;
	    }
	} else {
	    u = sun;
	}
    }
    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	tmp = b[i__ - 1] * m1 * m2;
	u = a[i__] * m1 * m2 - tmp * (tmp / u) - sshift;
	if (u <= sun) {
	    if (u <= 0.) {
		++(*num);
		if (u > -sun) {
		    u = -sun;
		}
	    } else {
		u = sun;
	    }
	}
/* L20: */
    }
    return 0;

/*     End of DSTECT */

} /* dstect_ */

