#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__2 = 2;

doublereal sopbl2_(char *subnam, integer *m, integer *n, integer *kkl, 
	integer *kku)
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    real ret_val;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static real adds;
    extern logical lsame_(char *, char *);
    static char c1[1], c2[2], c3[3];
    static real mults, ek, em, en, kl, ku;
    extern logical lsamen_(integer *, char *, char *);


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       March 31, 1993   


    Purpose   
    =======   

    SOPBL2 computes an approximation of the number of floating point   
    operations used by a subroutine SUBNAM with the given values   
    of the parameters M, N, KL, and KU.   

    This version counts operations for the Level 2 BLAS.   

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

    KKL     (input) INTEGER   
            The lower band width of the coefficient matrix.   
            KL is set to max( 0, min( M-1, KKL ) ).   

    KKU     (input) INTEGER   
            The upper band width of the coefficient matrix.   
            KU is set to max( 0, min( N-1, KKU ) ).   

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
/* Computing MAX   
   Computing MIN */
    i__3 = *m - 1;
    i__1 = 0, i__2 = min(i__3,*kkl);
    kl = (real) max(i__1,i__2);
/* Computing MAX   
   Computing MIN */
    i__3 = *n - 1;
    i__1 = 0, i__2 = min(i__3,*kku);
    ku = (real) max(i__1,i__2);
    em = (real) (*m);
    en = (real) (*n);
    ek = kl;

/*     -------------------------------   
       Matrix-vector multiply routines   
       ------------------------------- */

    if (lsamen_(&c__3, c3, "MV ")) {

	if (lsamen_(&c__2, c2, "GE")) {

	    mults = em * (en + 1.f);
	    adds = em * en;

/*        Assume M <= N + KL and KL < M   
                 N <= M + KU and KU < N   
          so that the zero sections are triangles. */

	} else if (lsamen_(&c__2, c2, "GB")) {

	    mults = em * (en + 1.f) - (em - 1.f - kl) * (em - kl) / 2.f - (en 
		    - 1.f - ku) * (en - ku) / 2.f;
	    adds = em * (en + 1.f) - (em - 1.f - kl) * (em - kl) / 2.f - (en 
		    - 1.f - ku) * (en - ku) / 2.f;

	} else if (lsamen_(&c__2, c2, "SY") || lsamen_(&
		c__2, c2, "SP") || lsamen_(&c__3, 
		subnam, "CHE") || lsamen_(&c__3, subnam,
		 "ZHE") || lsamen_(&c__3, subnam, "CHP") || lsamen_(&c__3, subnam, "ZHP")) {

	    mults = em * (em + 1.f);
	    adds = em * em;

	} else if (lsamen_(&c__2, c2, "SB") || lsamen_(&
		c__3, subnam, "CHB") || lsamen_(&c__3, 
		subnam, "ZHB")) {

	    mults = em * (em + 1.f) - (em - 1.f - ek) * (em - ek);
	    adds = em * em - (em - 1.f - ek) * (em - ek);

	} else if (lsamen_(&c__2, c2, "TR") || lsamen_(&
		c__2, c2, "TP")) {

	    mults = em * (em + 1.f) / 2.f;
	    adds = (em - 1.f) * em / 2.f;

	} else if (lsamen_(&c__2, c2, "TB")) {

	    mults = em * (em + 1.f) / 2.f - (em - ek - 1.f) * (em - ek) / 2.f;
	    adds = (em - 1.f) * em / 2.f - (em - ek - 1.f) * (em - ek) / 2.f;

	}

/*     ---------------------   
       Matrix solve routines   
       --------------------- */

    } else if (lsamen_(&c__3, c3, "SV ")) {

	if (lsamen_(&c__2, c2, "TR") || lsamen_(&c__2, 
		c2, "TP")) {

	    mults = em * (em + 1.f) / 2.f;
	    adds = (em - 1.f) * em / 2.f;

	} else if (lsamen_(&c__2, c2, "TB")) {

	    mults = em * (em + 1.f) / 2.f - (em - ek - 1.f) * (em - ek) / 2.f;
	    adds = (em - 1.f) * em / 2.f - (em - ek - 1.f) * (em - ek) / 2.f;

	}

/*     ----------------   
       Rank-one updates   
       ---------------- */

    } else if (lsamen_(&c__3, c3, "R  ")) {

	if (lsamen_(&c__3, subnam, "SGE") || lsamen_(&
		c__3, subnam, "DGE")) {

	    mults = em * en + dmin(em,en);
	    adds = em * en;

	} else if (lsamen_(&c__2, c2, "SY") || lsamen_(&
		c__2, c2, "SP") || lsamen_(&c__3, 
		subnam, "CHE") || lsamen_(&c__3, subnam,
		 "CHP") || lsamen_(&c__3, subnam, "ZHE") || lsamen_(&c__3, subnam, "ZHP")) {

	    mults = em * (em + 1.f) / 2.f + em;
	    adds = em * (em + 1.f) / 2.f;

	}

    } else if (lsamen_(&c__3, c3, "RC ") || lsamen_(&
	    c__3, c3, "RU ")) {

	if (lsamen_(&c__3, subnam, "CGE") || lsamen_(&
		c__3, subnam, "ZGE")) {

	    mults = em * en + dmin(em,en);
	    adds = em * en;

	}

/*     ----------------   
       Rank-two updates   
       ---------------- */

    } else if (lsamen_(&c__3, c3, "R2 ")) {
	if (lsamen_(&c__2, c2, "SY") || lsamen_(&c__2, 
		c2, "SP") || lsamen_(&c__3, subnam, 
		"CHE") || lsamen_(&c__3, subnam, "CHP") || lsamen_(&c__3, subnam, "ZHE") || lsamen_(&c__3, subnam, "ZHP")) {

	    mults = em * (em + 1.f) + em * 2.f;
	    adds = em * (em + 1.f);

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

/*     End of SOPBL2 */

} /* sopbl2_ */

