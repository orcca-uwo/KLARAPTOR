#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__3 = 3;

doublereal sopaux_(char *subnam, integer *m, integer *n, integer *kl, integer 
	*ku, integer *nb)
{
    /* System generated locals */
    real ret_val;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static real adds;
    extern logical lsame_(char *, char *);
    static char c1[1], c2[2], c3[3];
    static real mults, addfac, ek, em, en, mulfac;
    extern logical lsamen_(integer *, char *, char *);
    static real enb;


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       March 31, 1993   


    Purpose   
    =======   

    SOPAUX computes an approximation of the number of floating point   
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


    ret_val = 0.f;
    mults = 0.f;
    adds = 0.f;
    *(unsigned char *)c1 = *(unsigned char *)subnam;
    s_copy(c2, subnam + 1, (ftnlen)2, (ftnlen)2);
    s_copy(c3, subnam + 3, (ftnlen)3, (ftnlen)3);
    if (*m <= 0 || ! (lsame_(c1, "S") || lsame_(c1, 
	    "D") || lsame_(c1, "C") || lsame_(c1, "Z"))) {
	return ret_val;
    }
    if (lsame_(c1, "S") || lsame_(c1, "D")) {
	mulfac = 1.f;
	addfac = 1.f;
    } else {
	mulfac = 6.f;
	addfac = 2.f;
    }
    em = (real) (*m);
    en = (real) (*n);
    enb = (real) (*nb);

    if (lsamen_(&c__2, c2, "LA")) {

/*        xLAULM:  N  =>  M */

	if (lsamen_(&c__3, c3, "ULM") || lsamen_(&c__3, 
		c3, "UL2")) {
	    mults = em * .33333333333333331f * (em * em - 1.f);
	    adds = em * (em * (em * .33333333333333331f - .5f) + 
		    .16666666666666666f);

/*        xLAUUM:  N  =>  M */

	} else if (lsamen_(&c__3, c3, "UUM") || lsamen_(
		&c__3, c3, "UU2")) {
	    mults = em * (em * (em * .16666666666666666f + .5f) + 
		    .33333333333333331f);
	    adds = em * .16666666666666666f * (em * em - 1.f);

/*        xLACON:  N  =>  M */

	} else if (lsamen_(&c__3, c3, "CON")) {
	    mults = em * 3.f + 3.f;
	    adds = em * 4.f - 3.f;

/*        xLARF:  M, N  =>  M, N */

	} else if (lsamen_(&c__3, c3, "RF ")) {
	    mults = em * 2.f * en + en;
	    adds = em * 2.f * en;

/*        xLARFB:  M, N, SIDE, NB  =>  M, N, KL, NB   
             where KL <= 0 indicates SIDE = 'L'   
             and   KL > 0  indicates SIDE = 'R' */

	} else if (lsamen_(&c__3, c3, "RFB")) {

/*           KL <= 0:  Code requiring local array */

	    if (*kl <= 0) {
		mults = en * enb * (em * 2.f + (enb + 1.f) / 2.f);
		adds = en * enb * (em * 2.f + (enb - 1.f) / 2.f);

/*           KL > 0:  Code not requiring local array */

	    } else {
		mults = en * enb * (em * 2.f + (-enb / 2.f + 2.5f));
		adds = en * enb * (em * 2.f + (-enb / 2.f - .5f));
	    }

/*        xLARFG:  N  =>  M */

	} else if (lsamen_(&c__3, c3, "RFG")) {
	    mults = em * 2.f + 4.f;
	    adds = em + 1.f;

/*        xLARFT:  M, NB  =>  M, N */

	} else if (lsamen_(&c__3, c3, "RFT")) {
	    mults = en * (en * (en * -.16666666666666666f + 1.f) - 
		    .83333333333333337f + em / 2.f * (en - 1.f));
	    adds = en * ((1.f - en * en) * .16666666666666666f + em / 2.f * (
		    en - 1.f));

/*        xLATRD:  N, K  =>  M, N */

	} else if (lsamen_(&c__3, c3, "TRD")) {
	    ek = (real) (*n);
	    mults = ek * (4.166666666666667f - ek * (ek * 1.6666666666666667f 
		    + 1.5f) + em * (ek * 2.f + 2.f + em));
	    adds = ek * (-.33333333333333331f - ek * 1.6666666666666667f * ek 
		    + em * (ek * 2.f - 1.f + em));
	}

    }

    ret_val = mulfac * mults + addfac * adds;

    return ret_val;

/*     End of SOPAUX */

} /* sopaux_ */

