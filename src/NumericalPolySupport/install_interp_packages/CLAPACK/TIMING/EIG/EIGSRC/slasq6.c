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

/* Subroutine */ int slasq6_(integer *i0, integer *n0, real *z__, integer *pp,
	 real *dmin__, real *dmin1, real *dmin2, real *dn, real *dnm1, real *
	dnm2)
{
    /* System generated locals */
    integer i__1;
    real r__1, r__2;

    /* Local variables */
    static real emin, temp, d__;
    static integer j4;
    extern doublereal slamch_(char *);
    static real safmin;
    static integer j4p2;


/*  -- LAPACK auxiliary routine (instrumented to count ops, version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       October 31, 1999   


    Purpose   
    =======   

    SLASQ6 computes one dqd (shift equal to zero) transform in   
    ping-pong form, with protection against underflow and overflow.   

    Arguments   
    =========   

    I0    (input) INTEGER   
          First index.   

    N0    (input) INTEGER   
          Last index.   

    Z     (input) REAL array, dimension ( 4*N )   
          Z holds the qd array. EMIN is stored in Z(4*N0) to avoid   
          an extra argument.   

    PP    (input) INTEGER   
          PP=0 for ping, PP=1 for pong.   

    DMIN  (output) REAL   
          Minimum value of d.   

    DMIN1 (output) REAL   
          Minimum value of d, excluding D( N0 ).   

    DMIN2 (output) REAL   
          Minimum value of d, excluding D( N0 ) and D( N0-1 ).   

    DN    (output) REAL   
          d(N0), the last value of d.   

    DNM1  (output) REAL   
          d(N0-1).   

    DNM2  (output) REAL   
          d(N0-2).   

    =====================================================================   


       Parameter adjustments */
    --z__;

    /* Function Body */
    if (*n0 - *i0 - 1 <= 0) {
	return 0;
    }

    safmin = slamch_("Safe minimum");
    j4 = (*i0 << 2) + *pp - 3;
    emin = z__[j4 + 4];
    d__ = z__[j4];
    *dmin__ = d__;

    if (*pp == 0) {
	i__1 = *n0 - 3 << 2;
	for (j4 = *i0 << 2; j4 <= i__1; j4 += 4) {
	    z__[j4 - 2] = d__ + z__[j4 - 1];
	    if (z__[j4 - 2] == 0.f) {
		z__[j4] = 0.f;
		d__ = z__[j4 + 1];
		*dmin__ = d__;
		emin = 0.f;
	    } else if (safmin * z__[j4 + 1] < z__[j4 - 2] && safmin * z__[j4 
		    - 2] < z__[j4 + 1]) {
		latime_1.ops += 2.f;
		temp = z__[j4 + 1] / z__[j4 - 2];
		z__[j4] = z__[j4 - 1] * temp;
		d__ *= temp;
	    } else {
		latime_1.ops += 4.f;
		z__[j4] = z__[j4 + 1] * (z__[j4 - 1] / z__[j4 - 2]);
		d__ = z__[j4 + 1] * (d__ / z__[j4 - 2]);
	    }
	    *dmin__ = dmin(*dmin__,d__);
/* Computing MIN */
	    r__1 = emin, r__2 = z__[j4];
	    emin = dmin(r__1,r__2);
/* L10: */
	}
    } else {
	i__1 = *n0 - 3 << 2;
	for (j4 = *i0 << 2; j4 <= i__1; j4 += 4) {
	    z__[j4 - 3] = d__ + z__[j4];
	    if (z__[j4 - 3] == 0.f) {
		z__[j4 - 1] = 0.f;
		d__ = z__[j4 + 2];
		*dmin__ = d__;
		emin = 0.f;
	    } else if (safmin * z__[j4 + 2] < z__[j4 - 3] && safmin * z__[j4 
		    - 3] < z__[j4 + 2]) {
		latime_1.ops += 2.f;
		temp = z__[j4 + 2] / z__[j4 - 3];
		z__[j4 - 1] = z__[j4] * temp;
		d__ *= temp;
	    } else {
		latime_1.ops += 4.f;
		z__[j4 - 1] = z__[j4 + 2] * (z__[j4] / z__[j4 - 3]);
		d__ = z__[j4 + 2] * (d__ / z__[j4 - 3]);
	    }
	    *dmin__ = dmin(*dmin__,d__);
/* Computing MIN */
	    r__1 = emin, r__2 = z__[j4 - 1];
	    emin = dmin(r__1,r__2);
/* L20: */
	}
    }

/*     Unroll last two steps. */

    latime_1.ops += 1.f;
    *dnm2 = d__;
    *dmin2 = *dmin__;
    j4 = (*n0 - 2 << 2) - *pp;
    j4p2 = j4 + (*pp << 1) - 1;
    z__[j4 - 2] = *dnm2 + z__[j4p2];
    if (z__[j4 - 2] == 0.f) {
	z__[j4] = 0.f;
	*dnm1 = z__[j4p2 + 2];
	*dmin__ = *dnm1;
	emin = 0.f;
    } else if (safmin * z__[j4p2 + 2] < z__[j4 - 2] && safmin * z__[j4 - 2] < 
	    z__[j4p2 + 2]) {
	latime_1.ops += 3.f;
	temp = z__[j4p2 + 2] / z__[j4 - 2];
	z__[j4] = z__[j4p2] * temp;
	*dnm1 = *dnm2 * temp;
    } else {
	latime_1.ops += 4.f;
	z__[j4] = z__[j4p2 + 2] * (z__[j4p2] / z__[j4 - 2]);
	*dnm1 = z__[j4p2 + 2] * (*dnm2 / z__[j4 - 2]);
    }
    *dmin__ = dmin(*dmin__,*dnm1);

    latime_1.ops += 1.f;
    *dmin1 = *dmin__;
    j4 += 4;
    j4p2 = j4 + (*pp << 1) - 1;
    z__[j4 - 2] = *dnm1 + z__[j4p2];
    if (z__[j4 - 2] == 0.f) {
	z__[j4] = 0.f;
	*dn = z__[j4p2 + 2];
	*dmin__ = *dn;
	emin = 0.f;
    } else if (safmin * z__[j4p2 + 2] < z__[j4 - 2] && safmin * z__[j4 - 2] < 
	    z__[j4p2 + 2]) {
	latime_1.ops += 3.f;
	temp = z__[j4p2 + 2] / z__[j4 - 2];
	z__[j4] = z__[j4p2] * temp;
	*dn = *dnm1 * temp;
    } else {
	latime_1.ops += 4.f;
	z__[j4] = z__[j4p2 + 2] * (z__[j4p2] / z__[j4 - 2]);
	*dn = z__[j4p2 + 2] * (*dnm1 / z__[j4 - 2]);
    }
    *dmin__ = dmin(*dmin__,*dn);

    z__[j4 + 2] = *dn;
    z__[(*n0 << 2) - *pp] = emin;
    return 0;

/*     End of SLASQ6 */

} /* slasq6_ */

