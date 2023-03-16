#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__2 = 2;
static integer c__5 = 5;

doublereal sopbl3_(char *subnam, integer *m, integer *n, integer *k)
{
    /* System generated locals */
    real ret_val;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static real adds;
    extern logical lsame_(char *, char *);
    static char c1[1], c2[2], c3[3];
    static real mults, ek, em, en;
    extern logical lsamen_(integer *, char *, char *);


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       March 31, 1993   


    Purpose   
    =======   

    SOPBL3 computes an approximation of the number of floating point   
    operations used by a subroutine SUBNAM with the given values   
    of the parameters M, N, and K.   

    This version counts operations for the Level 3 BLAS.   

    Arguments   
    =========   

    SUBNAM  (input) CHARACTER*6   
            The name of the subroutine.   

    M       (input) INTEGER   
    N       (input) INTEGER   
    K       (input) INTEGER   
            M, N, and K contain parameter values used by the Level 3   
            BLAS.  The output matrix is always M x N or N x N if   
            symmetric, but K has different uses in different   
            contexts.  For example, in the matrix-matrix multiply   
            routine, we have   
               C = A * B   
            where C is M x N, A is M x K, and B is K x N.   
            In xSYMM, xTRMM, and xTRSM, K indicates whether the matrix   
            A is applied on the left or right.  If K <= 0, the matrix   
            is applied on the left, if K > 0, on the right.   

    =====================================================================   


       Quick return if possible */

    if (*m <= 0 || ! (lsame_(subnam, "S") || lsame_(
	    subnam, "D") || lsame_(subnam, "C") || lsame_(subnam, "Z"))) {
	ret_val = 0.f;
	return ret_val;
    }

    *(unsigned char *)c1 = *(unsigned char *)subnam;
    s_copy(c2, subnam + 1, (ftnlen)2, (ftnlen)2);
    s_copy(c3, subnam + 3, (ftnlen)3, (ftnlen)3);
    mults = 0.f;
    adds = 0.f;
    em = (real) (*m);
    en = (real) (*n);
    ek = (real) (*k);

/*     ----------------------   
       Matrix-matrix products   
          assume beta = 1   
       ---------------------- */

    if (lsamen_(&c__3, c3, "MM ")) {

	if (lsamen_(&c__2, c2, "GE")) {

	    mults = em * ek * en;
	    adds = em * ek * en;

	} else if (lsamen_(&c__2, c2, "SY") || lsamen_(&
		c__3, subnam, "CHE") || lsamen_(&c__3, 
		subnam, "ZHE")) {

/*           IF K <= 0, assume A multiplies B on the left. */

	    if (*k <= 0) {
		mults = em * em * en;
		adds = em * em * en;
	    } else {
		mults = em * en * en;
		adds = em * en * en;
	    }

	} else if (lsamen_(&c__2, c2, "TR")) {

	    if (*k <= 0) {
		mults = en * em * (em + 1.f) / 2.f;
		adds = en * em * (em - 1.f) / 2.f;
	    } else {
		mults = em * en * (en + 1.f) / 2.f;
		adds = em * en * (en - 1.f) / 2.f;
	    }

	}

/*     ------------------------------------------------   
       Rank-K update of a symmetric or Hermitian matrix   
       ------------------------------------------------ */

    } else if (lsamen_(&c__3, c3, "RK ")) {

	if (lsamen_(&c__2, c2, "SY") || lsamen_(&c__3, 
		subnam, "CHE") || lsamen_(&c__3, subnam,
		 "ZHE")) {

	    mults = ek * em * (em + 1.f) / 2.f;
	    adds = ek * em * (em + 1.f) / 2.f;
	}

/*     ------------------------------------------------   
       Rank-2K update of a symmetric or Hermitian matrix   
       ------------------------------------------------ */

    } else if (lsamen_(&c__3, c3, "R2K")) {

	if (lsamen_(&c__2, c2, "SY") || lsamen_(&c__3, 
		subnam, "CHE") || lsamen_(&c__3, subnam,
		 "ZHE")) {

	    mults = ek * em * em;
	    adds = ek * em * em + em;
	}

/*     -----------------------------------------   
       Solving system with many right hand sides   
       ----------------------------------------- */

    } else if (lsamen_(&c__5, subnam + 1, "TRSM ")) {

	if (*k <= 0) {
	    mults = en * em * (em + 1.f) / 2.f;
	    adds = en * em * (em - 1.f) / 2.f;
	} else {
	    mults = em * en * (en + 1.f) / 2.f;
	    adds = em * en * (en - 1.f) / 2.f;
	}

    }

/*     ------------------------------------------------   
       Compute the total number of operations.   
       For real and double precision routines, count   
          1 for each multiply and 1 for each add.   
       For complex and complex*16 routines, count   
          6 for each multiply and 2 for each add.   
       ------------------------------------------------ */

    if (lsame_(c1, "S") || lsame_(c1, "D")) {

	ret_val = mults + adds;

    } else {

	ret_val = mults * 6 + adds * 2;

    }

    return ret_val;

/*     End of SOPBL3 */

} /* sopbl3_ */

