#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublecomplex c_b1 = {0.,0.};
static integer c__3 = 3;

/* Subroutine */ int zlatm4_(integer *itype, integer *n, integer *nz1, 
	integer *nz2, logical *rsign, doublereal *amagn, doublereal *rcond, 
	doublereal *triang, integer *idist, integer *iseed, doublecomplex *a, 
	integer *lda)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3, i__4;
    doublereal d__1;
    doublecomplex z__1, z__2;

    /* Builtin functions */
    double pow_dd(doublereal *, doublereal *), pow_di(doublereal *, integer *)
	    , log(doublereal), exp(doublereal), z_abs(doublecomplex *);

    /* Local variables */
    static integer kbeg, isdb, kend, isde, klen, i__, k;
    static doublereal alpha;
    static doublecomplex ctemp;
    static integer jc, jd, jr;
    extern doublereal dlaran_(integer *);
    extern /* Double Complex */ VOID zlarnd_(doublecomplex *, integer *, 
	    integer *);
    extern /* Subroutine */ int zlaset_(char *, integer *, integer *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, integer *);


#define a_subscr(a_1,a_2) (a_2)*a_dim1 + a_1
#define a_ref(a_1,a_2) a[a_subscr(a_1,a_2)]


/*  -- LAPACK auxiliary test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    ZLATM4 generates basic square matrices, which may later be   
    multiplied by others in order to produce test matrices.  It is   
    intended mainly to be used to test the generalized eigenvalue   
    routines.   

    It first generates the diagonal and (possibly) subdiagonal,   
    according to the value of ITYPE, NZ1, NZ2, RSIGN, AMAGN, and RCOND.   
    It then fills in the upper triangle with random numbers, if TRIANG is   
    non-zero.   

    Arguments   
    =========   

    ITYPE   (input) INTEGER   
            The "type" of matrix on the diagonal and sub-diagonal.   
            If ITYPE < 0, then type abs(ITYPE) is generated and then   
               swapped end for end (A(I,J) := A'(N-J,N-I).)  See also   
               the description of AMAGN and RSIGN.   

            Special types:   
            = 0:  the zero matrix.   
            = 1:  the identity.   
            = 2:  a transposed Jordan block.   
            = 3:  If N is odd, then a k+1 x k+1 transposed Jordan block   
                  followed by a k x k identity block, where k=(N-1)/2.   
                  If N is even, then k=(N-2)/2, and a zero diagonal entry   
                  is tacked onto the end.   

            Diagonal types.  The diagonal consists of NZ1 zeros, then   
               k=N-NZ1-NZ2 nonzeros.  The subdiagonal is zero.  ITYPE   
               specifies the nonzero diagonal entries as follows:   
            = 4:  1, ..., k   
            = 5:  1, RCOND, ..., RCOND   
            = 6:  1, ..., 1, RCOND   
            = 7:  1, a, a^2, ..., a^(k-1)=RCOND   
            = 8:  1, 1-d, 1-2*d, ..., 1-(k-1)*d=RCOND   
            = 9:  random numbers chosen from (RCOND,1)   
            = 10: random numbers with distribution IDIST (see ZLARND.)   

    N       (input) INTEGER   
            The order of the matrix.   

    NZ1     (input) INTEGER   
            If abs(ITYPE) > 3, then the first NZ1 diagonal entries will   
            be zero.   

    NZ2     (input) INTEGER   
            If abs(ITYPE) > 3, then the last NZ2 diagonal entries will   
            be zero.   

    RSIGN   (input) LOGICAL   
            = .TRUE.:  The diagonal and subdiagonal entries will be   
                       multiplied by random numbers of magnitude 1.   
            = .FALSE.: The diagonal and subdiagonal entries will be   
                       left as they are (usually non-negative real.)   

    AMAGN   (input) DOUBLE PRECISION   
            The diagonal and subdiagonal entries will be multiplied by   
            AMAGN.   

    RCOND   (input) DOUBLE PRECISION   
            If abs(ITYPE) > 4, then the smallest diagonal entry will be   
            RCOND.  RCOND must be between 0 and 1.   

    TRIANG  (input) DOUBLE PRECISION   
            The entries above the diagonal will be random numbers with   
            magnitude bounded by TRIANG (i.e., random numbers multiplied   
            by TRIANG.)   

    IDIST   (input) INTEGER   
            On entry, DIST specifies the type of distribution to be used   
            to generate a random matrix .   
            = 1: real and imaginary parts each UNIFORM( 0, 1 )   
            = 2: real and imaginary parts each UNIFORM( -1, 1 )   
            = 3: real and imaginary parts each NORMAL( 0, 1 )   
            = 4: complex number uniform in DISK( 0, 1 )   

    ISEED   (input/output) INTEGER array, dimension (4)   
            On entry ISEED specifies the seed of the random number   
            generator.  The values of ISEED are changed on exit, and can   
            be used in the next call to ZLATM4 to continue the same   
            random number sequence.   
            Note: ISEED(4) should be odd, for the random number generator   
            used at present.   

    A       (output) COMPLEX*16 array, dimension (LDA, N)   
            Array to be computed.   

    LDA     (input) INTEGER   
            Leading dimension of A.  Must be at least 1 and at least N.   

    =====================================================================   


       Parameter adjustments */
    --iseed;
    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;

    /* Function Body */
    if (*n <= 0) {
	return 0;
    }
    zlaset_("Full", n, n, &c_b1, &c_b1, &a[a_offset], lda);

/*     Insure a correct ISEED */

    if (iseed[4] % 2 != 1) {
	++iseed[4];
    }

/*     Compute diagonal and subdiagonal according to ITYPE, NZ1, NZ2,   
       and RCOND */

    if (*itype != 0) {
	if (abs(*itype) >= 4) {
/* Computing MAX   
   Computing MIN */
	    i__3 = *n, i__4 = *nz1 + 1;
	    i__1 = 1, i__2 = min(i__3,i__4);
	    kbeg = max(i__1,i__2);
/* Computing MAX   
   Computing MIN */
	    i__3 = *n, i__4 = *n - *nz2;
	    i__1 = kbeg, i__2 = min(i__3,i__4);
	    kend = max(i__1,i__2);
	    klen = kend + 1 - kbeg;
	} else {
	    kbeg = 1;
	    kend = *n;
	    klen = *n;
	}
	isdb = 1;
	isde = 0;
	switch (abs(*itype)) {
	    case 1:  goto L10;
	    case 2:  goto L30;
	    case 3:  goto L50;
	    case 4:  goto L80;
	    case 5:  goto L100;
	    case 6:  goto L120;
	    case 7:  goto L140;
	    case 8:  goto L160;
	    case 9:  goto L180;
	    case 10:  goto L200;
	}

/*        |ITYPE| = 1: Identity */

L10:
	i__1 = *n;
	for (jd = 1; jd <= i__1; ++jd) {
	    i__2 = a_subscr(jd, jd);
	    a[i__2].r = 1., a[i__2].i = 0.;
/* L20: */
	}
	goto L220;

/*        |ITYPE| = 2: Transposed Jordan block */

L30:
	i__1 = *n - 1;
	for (jd = 1; jd <= i__1; ++jd) {
	    i__2 = a_subscr(jd + 1, jd);
	    a[i__2].r = 1., a[i__2].i = 0.;
/* L40: */
	}
	isdb = 1;
	isde = *n - 1;
	goto L220;

/*        |ITYPE| = 3: Transposed Jordan block, followed by the identity. */

L50:
	k = (*n - 1) / 2;
	i__1 = k;
	for (jd = 1; jd <= i__1; ++jd) {
	    i__2 = a_subscr(jd + 1, jd);
	    a[i__2].r = 1., a[i__2].i = 0.;
/* L60: */
	}
	isdb = 1;
	isde = k;
	i__1 = (k << 1) + 1;
	for (jd = k + 2; jd <= i__1; ++jd) {
	    i__2 = a_subscr(jd, jd);
	    a[i__2].r = 1., a[i__2].i = 0.;
/* L70: */
	}
	goto L220;

/*        |ITYPE| = 4: 1,...,k */

L80:
	i__1 = kend;
	for (jd = kbeg; jd <= i__1; ++jd) {
	    i__2 = a_subscr(jd, jd);
	    i__3 = jd - *nz1;
	    z__1.r = (doublereal) i__3, z__1.i = 0.;
	    a[i__2].r = z__1.r, a[i__2].i = z__1.i;
/* L90: */
	}
	goto L220;

/*        |ITYPE| = 5: One large D value: */

L100:
	i__1 = kend;
	for (jd = kbeg + 1; jd <= i__1; ++jd) {
	    i__2 = a_subscr(jd, jd);
	    z__1.r = *rcond, z__1.i = 0.;
	    a[i__2].r = z__1.r, a[i__2].i = z__1.i;
/* L110: */
	}
	i__1 = a_subscr(kbeg, kbeg);
	a[i__1].r = 1., a[i__1].i = 0.;
	goto L220;

/*        |ITYPE| = 6: One small D value: */

L120:
	i__1 = kend - 1;
	for (jd = kbeg; jd <= i__1; ++jd) {
	    i__2 = a_subscr(jd, jd);
	    a[i__2].r = 1., a[i__2].i = 0.;
/* L130: */
	}
	i__1 = a_subscr(kend, kend);
	z__1.r = *rcond, z__1.i = 0.;
	a[i__1].r = z__1.r, a[i__1].i = z__1.i;
	goto L220;

/*        |ITYPE| = 7: Exponentially distributed D values: */

L140:
	i__1 = a_subscr(kbeg, kbeg);
	a[i__1].r = 1., a[i__1].i = 0.;
	if (klen > 1) {
	    d__1 = 1. / (doublereal) (klen - 1);
	    alpha = pow_dd(rcond, &d__1);
	    i__1 = klen;
	    for (i__ = 2; i__ <= i__1; ++i__) {
		i__2 = a_subscr(*nz1 + i__, *nz1 + i__);
		i__3 = i__ - 1;
		d__1 = pow_di(&alpha, &i__3);
		z__1.r = d__1, z__1.i = 0.;
		a[i__2].r = z__1.r, a[i__2].i = z__1.i;
/* L150: */
	    }
	}
	goto L220;

/*        |ITYPE| = 8: Arithmetically distributed D values: */

L160:
	i__1 = a_subscr(kbeg, kbeg);
	a[i__1].r = 1., a[i__1].i = 0.;
	if (klen > 1) {
	    alpha = (1. - *rcond) / (doublereal) (klen - 1);
	    i__1 = klen;
	    for (i__ = 2; i__ <= i__1; ++i__) {
		i__2 = a_subscr(*nz1 + i__, *nz1 + i__);
		d__1 = (doublereal) (klen - i__) * alpha + *rcond;
		z__1.r = d__1, z__1.i = 0.;
		a[i__2].r = z__1.r, a[i__2].i = z__1.i;
/* L170: */
	    }
	}
	goto L220;

/*        |ITYPE| = 9: Randomly distributed D values on ( RCOND, 1): */

L180:
	alpha = log(*rcond);
	i__1 = kend;
	for (jd = kbeg; jd <= i__1; ++jd) {
	    i__2 = a_subscr(jd, jd);
	    d__1 = exp(alpha * dlaran_(&iseed[1]));
	    a[i__2].r = d__1, a[i__2].i = 0.;
/* L190: */
	}
	goto L220;

/*        |ITYPE| = 10: Randomly distributed D values from DIST */

L200:
	i__1 = kend;
	for (jd = kbeg; jd <= i__1; ++jd) {
	    i__2 = a_subscr(jd, jd);
	    zlarnd_(&z__1, idist, &iseed[1]);
	    a[i__2].r = z__1.r, a[i__2].i = z__1.i;
/* L210: */
	}

L220:

/*        Scale by AMAGN */

	i__1 = kend;
	for (jd = kbeg; jd <= i__1; ++jd) {
	    i__2 = a_subscr(jd, jd);
	    i__3 = a_subscr(jd, jd);
	    d__1 = *amagn * a[i__3].r;
	    a[i__2].r = d__1, a[i__2].i = 0.;
/* L230: */
	}
	i__1 = isde;
	for (jd = isdb; jd <= i__1; ++jd) {
	    i__2 = a_subscr(jd + 1, jd);
	    i__3 = a_subscr(jd + 1, jd);
	    d__1 = *amagn * a[i__3].r;
	    a[i__2].r = d__1, a[i__2].i = 0.;
/* L240: */
	}

/*        If RSIGN = .TRUE., assign random signs to diagonal and   
          subdiagonal */

	if (*rsign) {
	    i__1 = kend;
	    for (jd = kbeg; jd <= i__1; ++jd) {
		i__2 = a_subscr(jd, jd);
		if (a[i__2].r != 0.) {
		    zlarnd_(&z__1, &c__3, &iseed[1]);
		    ctemp.r = z__1.r, ctemp.i = z__1.i;
		    d__1 = z_abs(&ctemp);
		    z__1.r = ctemp.r / d__1, z__1.i = ctemp.i / d__1;
		    ctemp.r = z__1.r, ctemp.i = z__1.i;
		    i__2 = a_subscr(jd, jd);
		    i__3 = a_subscr(jd, jd);
		    d__1 = a[i__3].r;
		    z__1.r = d__1 * ctemp.r, z__1.i = d__1 * ctemp.i;
		    a[i__2].r = z__1.r, a[i__2].i = z__1.i;
		}
/* L250: */
	    }
	    i__1 = isde;
	    for (jd = isdb; jd <= i__1; ++jd) {
		i__2 = a_subscr(jd + 1, jd);
		if (a[i__2].r != 0.) {
		    zlarnd_(&z__1, &c__3, &iseed[1]);
		    ctemp.r = z__1.r, ctemp.i = z__1.i;
		    d__1 = z_abs(&ctemp);
		    z__1.r = ctemp.r / d__1, z__1.i = ctemp.i / d__1;
		    ctemp.r = z__1.r, ctemp.i = z__1.i;
		    i__2 = a_subscr(jd + 1, jd);
		    i__3 = a_subscr(jd + 1, jd);
		    d__1 = a[i__3].r;
		    z__1.r = d__1 * ctemp.r, z__1.i = d__1 * ctemp.i;
		    a[i__2].r = z__1.r, a[i__2].i = z__1.i;
		}
/* L260: */
	    }
	}

/*        Reverse if ITYPE < 0 */

	if (*itype < 0) {
	    i__1 = (kbeg + kend - 1) / 2;
	    for (jd = kbeg; jd <= i__1; ++jd) {
		i__2 = a_subscr(jd, jd);
		ctemp.r = a[i__2].r, ctemp.i = a[i__2].i;
		i__2 = a_subscr(jd, jd);
		i__3 = a_subscr(kbeg + kend - jd, kbeg + kend - jd);
		a[i__2].r = a[i__3].r, a[i__2].i = a[i__3].i;
		i__2 = a_subscr(kbeg + kend - jd, kbeg + kend - jd);
		a[i__2].r = ctemp.r, a[i__2].i = ctemp.i;
/* L270: */
	    }
	    i__1 = (*n - 1) / 2;
	    for (jd = 1; jd <= i__1; ++jd) {
		i__2 = a_subscr(jd + 1, jd);
		ctemp.r = a[i__2].r, ctemp.i = a[i__2].i;
		i__2 = a_subscr(jd + 1, jd);
		i__3 = a_subscr(*n + 1 - jd, *n - jd);
		a[i__2].r = a[i__3].r, a[i__2].i = a[i__3].i;
		i__2 = a_subscr(*n + 1 - jd, *n - jd);
		a[i__2].r = ctemp.r, a[i__2].i = ctemp.i;
/* L280: */
	    }
	}

    }

/*     Fill in upper triangle */

    if (*triang != 0.) {
	i__1 = *n;
	for (jc = 2; jc <= i__1; ++jc) {
	    i__2 = jc - 1;
	    for (jr = 1; jr <= i__2; ++jr) {
		i__3 = a_subscr(jr, jc);
		zlarnd_(&z__2, idist, &iseed[1]);
		z__1.r = *triang * z__2.r, z__1.i = *triang * z__2.i;
		a[i__3].r = z__1.r, a[i__3].i = z__1.i;
/* L290: */
	    }
/* L300: */
	}
    }

    return 0;

/*     End of ZLATM4 */

} /* zlatm4_ */

#undef a_ref
#undef a_subscr


