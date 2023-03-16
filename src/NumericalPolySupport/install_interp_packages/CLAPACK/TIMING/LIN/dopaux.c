#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__3 = 3;

doublereal dopaux_(char *subnam, integer *m, integer *n, integer *kl, integer 
	*ku, integer *nb)
{
    /* System generated locals */
    doublereal ret_val;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static doublereal adds;
    extern logical lsame_(char *, char *);
    static char c1[1], c2[2], c3[3];
    static doublereal mults, addfac, ek, em, en, mulfac;
    extern logical lsamen_(integer *, char *, char *);
    static doublereal enb;


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       March 31, 1993   


    Purpose   
    =======   

    DOPAUX computes an approximation of the number of floating point   
    operations used by the subroutine SUBNAM with the given values   
    of the parameters M, N, KL, KU, and NB.   

    This version counts operations for the LAPACK auxiliary routines.   

    Arguments   
    =========   

    SUBNAM  (input) CHARACTER*6   
            The name of the subroutine.   

    M       (input) INTEGER   
            The number of rows of the coefficient matrix.  M >= 0.   

    N       (input) INTEGER   
            The number of columns of the coefficient matrix.   
            If the matrix is square (such as in a solve routine) then   
            N is the number of right hand sides.  N >= 0.   

    KL      (input) INTEGER   
            The lower band width of the coefficient matrix.   
            If needed, 0 <= KL <= M-1.   

    KU      (input) INTEGER   
            The upper band width of the coefficient matrix.   
            If needed, 0 <= KU <= N-1.   

    NB      (input) INTEGER   
            The block size.  If needed, NB >= 1.   

    ===================================================================== */


    ret_val = 0.;
    mults = 0.;
    adds = 0.;
    *(unsigned char *)c1 = *(unsigned char *)subnam;
    s_copy(c2, subnam + 1, (ftnlen)2, (ftnlen)2);
    s_copy(c3, subnam + 3, (ftnlen)3, (ftnlen)3);
    if (*m <= 0 || ! (lsame_(c1, "S") || lsame_(c1, 
	    "D") || lsame_(c1, "C") || lsame_(c1, "Z"))) {
	return ret_val;
    }
    if (lsame_(c1, "S") || lsame_(c1, "D")) {
	mulfac = 1.;
	addfac = 1.;
    } else {
	mulfac = 6.;
	addfac = 2.;
    }
    em = (doublereal) (*m);
    en = (doublereal) (*n);
    enb = (doublereal) (*nb);

    if (lsamen_(&c__2, c2, "LA")) {

/*        xLAULM:  N  =>  M */

	if (lsamen_(&c__3, c3, "ULM") || lsamen_(&c__3, 
		c3, "UL2")) {
	    mults = em * .33333333333333331 * (em * em - 1.);
	    adds = em * (em * (em * .33333333333333331 - .5) + 
		    .16666666666666666);

/*        xLAUUM:  N  =>  M */

	} else if (lsamen_(&c__3, c3, "UUM") || lsamen_(
		&c__3, c3, "UU2")) {
	    mults = em * (em * (em * .16666666666666666 + .5) + 
		    .33333333333333331);
	    adds = em * .16666666666666666 * (em * em - 1.);

/*        xLACON:  N  =>  M */

	} else if (lsamen_(&c__3, c3, "CON")) {
	    mults = em * 3. + 3.;
	    adds = em * 4. - 3.;

/*        xLARF:  M, N  =>  M, N */

	} else if (lsamen_(&c__3, c3, "RF ")) {
	    mults = em * 2. * en + en;
	    adds = em * 2. * en;

/*        xLARFB:  M, N, SIDE, NB  =>  M, N, KL, NB   
             where KL <= 0 indicates SIDE = 'L'   
             and   KL > 0  indicates SIDE = 'R' */

	} else if (lsamen_(&c__3, c3, "RFB")) {

/*           KL <= 0:  Code requiring local array */

	    if (*kl <= 0) {
		mults = en * enb * (em * 2. + (enb + 1.) / 2.);
		adds = en * enb * (em * 2. + (enb - 1.) / 2.);

/*           KL > 0:  Code not requiring local array */

	    } else {
		mults = en * enb * (em * 2. + (-enb / 2. + 2.5));
		adds = en * enb * (em * 2. + (-enb / 2. - .5));
	    }

/*        xLARFG:  N  =>  M */

	} else if (lsamen_(&c__3, c3, "RFG")) {
	    mults = em * 2. + 4.;
	    adds = em + 1.;

/*        xLARFT:  M, NB  =>  M, N */

	} else if (lsamen_(&c__3, c3, "RFT")) {
	    mults = en * (en * (en * -.16666666666666666 + 1.) - 
		    .83333333333333337 + em / 2. * (en - 1.));
	    adds = en * ((1. - en * en) * .16666666666666666 + em / 2. * (en 
		    - 1.));

/*        xLATRD:  N, K  =>  M, N */

	} else if (lsamen_(&c__3, c3, "TRD")) {
	    ek = (doublereal) (*n);
	    mults = ek * (4.166666666666667 - ek * (ek * 1.6666666666666667 + 
		    1.5) + em * (ek * 2. + 2. + em));
	    adds = ek * (-.33333333333333331 - ek * 1.6666666666666667 * ek + 
		    em * (ek * 2. - 1. + em));
	}

    }

    ret_val = mulfac * mults + addfac * adds;

    return ret_val;

/*     End of DOPAUX */

} /* dopaux_ */

