#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;

/* Subroutine */ int zlatm1_(integer *mode, doublereal *cond, integer *irsign,
	 integer *idist, integer *iseed, doublecomplex *d__, integer *n, 
	integer *info)
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal d__1;
    doublecomplex z__1, z__2;

    /* Builtin functions */
    double pow_dd(doublereal *, doublereal *), pow_di(doublereal *, integer *)
	    , log(doublereal), exp(doublereal), z_abs(doublecomplex *);

    /* Local variables */
    static doublereal temp;
    static integer i__;
    static doublereal alpha;
    static doublecomplex ctemp;
    extern doublereal dlaran_(integer *);
    extern /* Subroutine */ int xerbla_(char *, integer *);
    extern /* Double Complex */ VOID zlarnd_(doublecomplex *, integer *, 
	    integer *);
    extern /* Subroutine */ int zlarnv_(integer *, integer *, integer *, 
	    doublecomplex *);


/*  -- LAPACK auxiliary test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

       ZLATM1 computes the entries of D(1..N) as specified by   
       MODE, COND and IRSIGN. IDIST and ISEED determine the generation   
       of random numbers. ZLATM1 is called by CLATMR to generate   
       random test matrices for LAPACK programs.   

    Arguments   
    =========   

    MODE   - INTEGER   
             On entry describes how D is to be computed:   
             MODE = 0 means do not change D.   
             MODE = 1 sets D(1)=1 and D(2:N)=1.0/COND   
             MODE = 2 sets D(1:N-1)=1 and D(N)=1.0/COND   
             MODE = 3 sets D(I)=COND**(-(I-1)/(N-1))   
             MODE = 4 sets D(i)=1 - (i-1)/(N-1)*(1 - 1/COND)   
             MODE = 5 sets D to random numbers in the range   
                      ( 1/COND , 1 ) such that their logarithms   
                      are uniformly distributed.   
             MODE = 6 set D to random numbers from same distribution   
                      as the rest of the matrix.   
             MODE < 0 has the same meaning as ABS(MODE), except that   
                the order of the elements of D is reversed.   
             Thus if MODE is positive, D has entries ranging from   
                1 to 1/COND, if negative, from 1/COND to 1,   
             Not modified.   

    COND   - DOUBLE PRECISION   
             On entry, used as described under MODE above.   
             If used, it must be >= 1. Not modified.   

    IRSIGN - INTEGER   
             On entry, if MODE neither -6, 0 nor 6, determines sign of   
             entries of D   
             0 => leave entries of D unchanged   
             1 => multiply each entry of D by random complex number   
                  uniformly distributed with absolute value 1   

    IDIST  - CHARACTER*1   
             On entry, IDIST specifies the type of distribution to be   
             used to generate a random matrix .   
             1 => real and imaginary parts each UNIFORM( 0, 1 )   
             2 => real and imaginary parts each UNIFORM( -1, 1 )   
             3 => real and imaginary parts each NORMAL( 0, 1 )   
             4 => complex number uniform in DISK( 0, 1 )   
             Not modified.   

    ISEED  - INTEGER array, dimension ( 4 )   
             On entry ISEED specifies the seed of the random number   
             generator. The random number generator uses a   
             linear congruential sequence limited to small   
             integers, and so should produce machine independent   
             random numbers. The values of ISEED are changed on   
             exit, and can be used in the next call to ZLATM1   
             to continue the same random number sequence.   
             Changed on exit.   

    D      - COMPLEX*16 array, dimension ( MIN( M , N ) )   
             Array to be computed according to MODE, COND and IRSIGN.   
             May be changed on exit if MODE is nonzero.   

    N      - INTEGER   
             Number of entries of D. Not modified.   

    INFO   - INTEGER   
              0  => normal termination   
             -1  => if MODE not in range -6 to 6   
             -2  => if MODE neither -6, 0 nor 6, and   
                    IRSIGN neither 0 nor 1   
             -3  => if MODE neither -6, 0 nor 6 and COND less than 1   
             -4  => if MODE equals 6 or -6 and IDIST not in range 1 to 4   
             -7  => if N negative   

    =====================================================================   


       Decode and Test the input parameters. Initialize flags & seed.   

       Parameter adjustments */
    --d__;
    --iseed;

    /* Function Body */
    *info = 0;

/*     Quick return if possible */

    if (*n == 0) {
	return 0;
    }

/*     Set INFO if an error */

    if (*mode < -6 || *mode > 6) {
	*info = -1;
    } else if (*mode != -6 && *mode != 0 && *mode != 6 && (*irsign != 0 && *
	    irsign != 1)) {
	*info = -2;
    } else if (*mode != -6 && *mode != 0 && *mode != 6 && *cond < 1.) {
	*info = -3;
    } else if ((*mode == 6 || *mode == -6) && (*idist < 1 || *idist > 4)) {
	*info = -4;
    } else if (*n < 0) {
	*info = -7;
    }

    if (*info != 0) {
	i__1 = -(*info);
	xerbla_("ZLATM1", &i__1);
	return 0;
    }

/*     Compute D according to COND and MODE */

    if (*mode != 0) {
	switch (abs(*mode)) {
	    case 1:  goto L10;
	    case 2:  goto L30;
	    case 3:  goto L50;
	    case 4:  goto L70;
	    case 5:  goto L90;
	    case 6:  goto L110;
	}

/*        One large D value: */

L10:
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    i__2 = i__;
	    d__1 = 1. / *cond;
	    d__[i__2].r = d__1, d__[i__2].i = 0.;
/* L20: */
	}
	d__[1].r = 1., d__[1].i = 0.;
	goto L120;

/*        One small D value: */

L30:
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    i__2 = i__;
	    d__[i__2].r = 1., d__[i__2].i = 0.;
/* L40: */
	}
	i__1 = *n;
	d__1 = 1. / *cond;
	d__[i__1].r = d__1, d__[i__1].i = 0.;
	goto L120;

/*        Exponentially distributed D values: */

L50:
	d__[1].r = 1., d__[1].i = 0.;
	if (*n > 1) {
	    d__1 = -1. / (doublereal) (*n - 1);
	    alpha = pow_dd(cond, &d__1);
	    i__1 = *n;
	    for (i__ = 2; i__ <= i__1; ++i__) {
		i__2 = i__;
		i__3 = i__ - 1;
		d__1 = pow_di(&alpha, &i__3);
		d__[i__2].r = d__1, d__[i__2].i = 0.;
/* L60: */
	    }
	}
	goto L120;

/*        Arithmetically distributed D values: */

L70:
	d__[1].r = 1., d__[1].i = 0.;
	if (*n > 1) {
	    temp = 1. / *cond;
	    alpha = (1. - temp) / (doublereal) (*n - 1);
	    i__1 = *n;
	    for (i__ = 2; i__ <= i__1; ++i__) {
		i__2 = i__;
		d__1 = (doublereal) (*n - i__) * alpha + temp;
		d__[i__2].r = d__1, d__[i__2].i = 0.;
/* L80: */
	    }
	}
	goto L120;

/*        Randomly distributed D values on ( 1/COND , 1): */

L90:
	alpha = log(1. / *cond);
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    i__2 = i__;
	    d__1 = exp(alpha * dlaran_(&iseed[1]));
	    d__[i__2].r = d__1, d__[i__2].i = 0.;
/* L100: */
	}
	goto L120;

/*        Randomly distributed D values from IDIST */

L110:
	zlarnv_(idist, &iseed[1], n, &d__[1]);

L120:

/*        If MODE neither -6 nor 0 nor 6, and IRSIGN = 1, assign   
          random signs to D */

	if (*mode != -6 && *mode != 0 && *mode != 6 && *irsign == 1) {
	    i__1 = *n;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		zlarnd_(&z__1, &c__3, &iseed[1]);
		ctemp.r = z__1.r, ctemp.i = z__1.i;
		i__2 = i__;
		i__3 = i__;
		d__1 = z_abs(&ctemp);
		z__2.r = ctemp.r / d__1, z__2.i = ctemp.i / d__1;
		z__1.r = d__[i__3].r * z__2.r - d__[i__3].i * z__2.i, z__1.i =
			 d__[i__3].r * z__2.i + d__[i__3].i * z__2.r;
		d__[i__2].r = z__1.r, d__[i__2].i = z__1.i;
/* L130: */
	    }
	}

/*        Reverse if MODE < 0 */

	if (*mode < 0) {
	    i__1 = *n / 2;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		i__2 = i__;
		ctemp.r = d__[i__2].r, ctemp.i = d__[i__2].i;
		i__2 = i__;
		i__3 = *n + 1 - i__;
		d__[i__2].r = d__[i__3].r, d__[i__2].i = d__[i__3].i;
		i__2 = *n + 1 - i__;
		d__[i__2].r = ctemp.r, d__[i__2].i = ctemp.i;
/* L140: */
	    }
	}

    }

    return 0;

/*     End of ZLATM1 */

} /* zlatm1_ */

