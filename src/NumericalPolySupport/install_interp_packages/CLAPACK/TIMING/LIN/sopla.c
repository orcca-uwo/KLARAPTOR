#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__3 = 3;

doublereal sopla_(char *subnam, integer *m, integer *n, integer *kl, integer *
	ku, integer *nb)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    real ret_val;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static real adds;
    static logical sord, corz;
    static integer i__;
    extern logical lsame_(char *, char *);
    static char c1[1], c2[2], c3[3];
    static real mults, addfac, ek, em, en, wl, mulfac, wu;
    extern logical lsamen_(integer *, char *, char *);
    static real emn;


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       March 31, 1993   


    Purpose   
    =======   

    SOPLA computes an approximation of the number of floating point   
    operations used by the subroutine SUBNAM with the given values   
    of the parameters M, N, KL, KU, and NB.   

    This version counts operations for the LAPACK subroutines.   

    Arguments   
    =========   

    SUBNAM  (input) CHARACTER*6   
            The name of the subroutine.   

    M       (input) INTEGER   
            The number of rows of the coefficient matrix.  M >= 0.   

    N       (input) INTEGER   
            The number of columns of the coefficient matrix.   
            For solve routine when the matrix is square,   
            N is the number of right hand sides.  N >= 0.   

    KL      (input) INTEGER   
            The lower band width of the coefficient matrix.   
            If needed, 0 <= KL <= M-1.   
            For xGEQRS, KL is the number of right hand sides.   

    KU      (input) INTEGER   
            The upper band width of the coefficient matrix.   
            If needed, 0 <= KU <= N-1.   

    NB      (input) INTEGER   
            The block size.  If needed, NB >= 1.   

    Notes   
    =====   

    In the comments below, the association is given between arguments   
    in the requested subroutine and local arguments.  For example,   

    xGETRS:  N, NRHS  =>  M, N   

    means that arguments N and NRHS in SGETRS are passed to arguments   
    M and N in this procedure.   

    =====================================================================   


       --------------------------------------------------------   
       Initialize SOPLA to 0 and do a quick return if possible.   
       -------------------------------------------------------- */

    ret_val = 0.f;
    mults = 0.f;
    adds = 0.f;
    *(unsigned char *)c1 = *(unsigned char *)subnam;
    s_copy(c2, subnam + 1, (ftnlen)2, (ftnlen)2);
    s_copy(c3, subnam + 3, (ftnlen)3, (ftnlen)3);
    sord = lsame_(c1, "S") || lsame_(c1, "D");
    corz = lsame_(c1, "C") || lsame_(c1, "Z");
    if (*m <= 0 || ! (sord || corz)) {
	return ret_val;
    }

/*     ---------------------------------------------------------   
       If the coefficient matrix is real, count each add as 1   
       operation and each multiply as 1 operation.   
       If the coefficient matrix is complex, count each add as 2   
       operations and each multiply as 6 operations.   
       --------------------------------------------------------- */

    if (lsame_(c1, "S") || lsame_(c1, "D")) {
	addfac = 1.f;
	mulfac = 1.f;
    } else {
	addfac = 2.f;
	mulfac = 6.f;
    }
    em = (real) (*m);
    en = (real) (*n);
    ek = (real) (*kl);

/*     ---------------------------------   
       GE:  GEneral rectangular matrices   
       --------------------------------- */

    if (lsamen_(&c__2, c2, "GE")) {

/*        xGETRF:  M, N  =>  M, N */

	if (lsamen_(&c__3, c3, "TRF")) {
	    emn = (real) min(*m,*n);
	    adds = emn * (em * en - (em + en) * (emn + 1.f) / 2.f + (emn + 
		    1.f) * (emn * 2.f + 1.f) / 6.f);
	    mults = adds + emn * (em - (emn + 1.f) / 2.f);

/*        xGETRS:  N, NRHS  =>  M, N */

	} else if (lsamen_(&c__3, c3, "TRS")) {
	    mults = en * em * em;
	    adds = en * (em * (em - 1.f));

/*        xGETRI:  N  =>  M */

	} else if (lsamen_(&c__3, c3, "TRI")) {
	    mults = em * (em * (em * .66666666666666663f + .5f) + 
		    .83333333333333337f);
	    adds = em * (em * (em * .66666666666666663f - 1.5f) + 
		    .83333333333333337f);

/*        xGEQRF or xGEQLF:  M, N  =>  M, N */

	} else if (lsamen_(&c__3, c3, "QRF") || lsamen_(
		&c__3, c3, "QR2") || lsamen_(&c__3, c3, 
		"QLF") || lsamen_(&c__3, c3, "QL2")) {
	    if (*m >= *n) {
		mults = en * (em + 3.8333333333333335f + en / 2.f + en * (em 
			- en / 3.f));
		adds = en * (en * (em - en / 3.f + .5f) + .83333333333333337f)
			;
	    } else {
		mults = em * (en * 2.f + 3.8333333333333335f - em / 2.f + em *
			 (en - em / 3.f));
		adds = em * (en + .83333333333333337f - em / 2.f + em * (en - 
			em / 3.f));
	    }

/*        xGERQF or xGELQF:  M, N  =>  M, N */

	} else if (lsamen_(&c__3, c3, "RQF") || lsamen_(
		&c__3, c3, "RQ2") || lsamen_(&c__3, c3, 
		"LQF") || lsamen_(&c__3, c3, "LQ2")) {
	    if (*m >= *n) {
		mults = en * (em + 4.833333333333333f + en / 2.f + en * (em - 
			en / 3.f));
		adds = en * (em + .83333333333333337f + en * (em - en / 3.f - 
			.5f));
	    } else {
		mults = em * (en * 2.f + 4.833333333333333f - em / 2.f + em * 
			(en - em / 3.f));
		adds = em * (em / 2.f + .83333333333333337f + em * (en - em / 
			3.f));
	    }

/*        xGEQPF: M, N => M, N */

	} else if (lsamen_(&c__3, c3, "QPF")) {
	    emn = (real) min(*m,*n);
	    mults = en * 2 * en + emn * (em * 3 + en * 5 + em * 2 * en - (emn 
		    + 1) * (en + 4 + em - (emn * 2 + 1) / 3));
	    adds = en * en + emn * (em * 2 + en + em * 2 * en - (emn + 1) * (
		    en + 2 + em - (emn * 2 + 1) / 3));

/*        xGEQRS or xGERQS:  M, N, NRHS  =>  M, N, KL */

	} else if (lsamen_(&c__3, c3, "QRS") || lsamen_(
		&c__3, c3, "RQS")) {
	    mults = ek * (en * (2.f - ek) + em * (en * 2.f + (em + 1.f) / 2.f)
		    );
	    adds = ek * (en * (1.f - ek) + em * (en * 2.f + (em - 1.f) / 2.f))
		    ;

/*        xGELQS or xGEQLS:  M, N, NRHS  =>  M, N, KL */

	} else if (lsamen_(&c__3, c3, "LQS") || lsamen_(
		&c__3, c3, "QLS")) {
	    mults = ek * (em * (2.f - ek) + en * (em * 2.f + (en + 1.f) / 2.f)
		    );
	    adds = ek * (em * (1.f - ek) + en * (em * 2.f + (en - 1.f) / 2.f))
		    ;

/*        xGEBRD:  M, N  =>  M, N */

	} else if (lsamen_(&c__3, c3, "BRD")) {
	    if (*m >= *n) {
		mults = en * (en * (em * 2.f - en * .66666666666666663f + 2.f)
			 + 6.666666666666667f);
		adds = en * (en - em + 1.6666666666666667f + en * (em * 2.f - 
			en * .66666666666666663f));
	    } else {
		mults = em * (em * (en * 2.f - em * .66666666666666663f + 2.f)
			 + 6.666666666666667f);
		adds = em * (em - en + 1.6666666666666667f + em * (en * 2.f - 
			em * .66666666666666663f));
	    }

/*        xGEHRD:  N  =>  M */

	} else if (lsamen_(&c__3, c3, "HRD")) {
	    if (*m == 1) {
		mults = 0.f;
		adds = 0.f;
	    } else {
		mults = em * (em * (em * 1.6666666666666667f + .5f) - 
			1.1666666666666667f) - 13.f;
		adds = em * (em * (em * 1.6666666666666667f - 1.f) - 
			.66666666666666663f) - 8.f;
	    }

	}

/*     ----------------------------   
       GB:  General Banded matrices   
       ----------------------------   
          Note:  The operation count is overestimated because   
          it is assumed that the factor U fills in to the maximum   
          extent, i.e., that its bandwidth goes from KU to KL + KU. */

    } else if (lsamen_(&c__2, c2, "GB")) {

/*        xGBTRF:  M, N, KL, KU  =>  M, N, KL, KU */

	if (lsamen_(&c__3, c3, "TRF")) {
	    for (i__ = min(*m,*n); i__ >= 1; --i__) {
/* Computing MAX   
   Computing MIN */
		i__3 = *kl, i__4 = *m - i__;
		i__1 = 0, i__2 = min(i__3,i__4);
		wl = (real) max(i__1,i__2);
/* Computing MAX   
   Computing MIN */
		i__3 = *kl + *ku, i__4 = *n - i__;
		i__1 = 0, i__2 = min(i__3,i__4);
		wu = (real) max(i__1,i__2);
		mults += wl * (wu + 1.f);
		adds += wl * wu;
/* L10: */
	    }

/*        xGBTRS:  N, NRHS, KL, KU  =>  M, N, KL, KU */

	} else if (lsamen_(&c__3, c3, "TRS")) {
/* Computing MAX   
   Computing MIN */
	    i__3 = *kl, i__4 = *m - 1;
	    i__1 = 0, i__2 = min(i__3,i__4);
	    wl = (real) max(i__1,i__2);
/* Computing MAX   
   Computing MIN */
	    i__3 = *kl + *ku, i__4 = *m - 1;
	    i__1 = 0, i__2 = min(i__3,i__4);
	    wu = (real) max(i__1,i__2);
	    mults = en * (em * (wl + 1.f + wu) - (wl * (wl + 1.f) + wu * (wu 
		    + 1.f)) * .5f);
	    adds = en * (em * (wl + wu) - (wl * (wl + 1.f) + wu * (wu + 1.f)) 
		    * .5f);

	}

/*     --------------------------------------   
       PO:  POsitive definite matrices   
       PP:  Positive definite Packed matrices   
       -------------------------------------- */

    } else if (lsamen_(&c__2, c2, "PO") || lsamen_(&
	    c__2, c2, "PP")) {

/*        xPOTRF:  N  =>  M */

	if (lsamen_(&c__3, c3, "TRF")) {
	    mults = em * (em * (em * .16666666666666666f + .5f) + 
		    .33333333333333331f);
	    adds = em * .16666666666666666f * (em * em - 1.f);

/*        xPOTRS:  N, NRHS  =>  M, N */

	} else if (lsamen_(&c__3, c3, "TRS")) {
	    mults = en * (em * (em + 1.f));
	    adds = en * (em * (em - 1.f));

/*        xPOTRI:  N  =>  M */

	} else if (lsamen_(&c__3, c3, "TRI")) {
	    mults = em * (em * (em * .33333333333333331f + 1.f) + 
		    .66666666666666663f);
	    adds = em * (em * (em * .33333333333333331f - .5f) + 
		    .16666666666666666f);

	}

/*     ------------------------------------   
       PB:  Positive definite Band matrices   
       ------------------------------------ */

    } else if (lsamen_(&c__2, c2, "PB")) {

/*        xPBTRF:  N, K  =>  M, KL */

	if (lsamen_(&c__3, c3, "TRF")) {
	    mults = ek * (ek * (ek * -.33333333333333331f - 1.f) - 
		    .66666666666666663f) + em * (ek * (ek * .5f + 1.5f) + 1.f)
		    ;
	    adds = ek * (ek * (ek * -.33333333333333331f - .5f) - 
		    .16666666666666666f) + em * (ek / 2.f * (ek + 1.f));

/*        xPBTRS:  N, NRHS, K  =>  M, N, KL */

	} else if (lsamen_(&c__3, c3, "TRS")) {
	    mults = en * ((em * 2 - ek) * (ek + 1.f));
	    adds = en * (ek * (em * 2 - (ek + 1.f)));

	}

/*     ----------------------------------   
       PT:  Positive definite Tridiagonal   
       ---------------------------------- */

    } else if (lsamen_(&c__2, c2, "PT")) {

/*        xPTTRF:  N  =>  M */

	if (lsamen_(&c__3, c3, "TRF")) {
	    mults = (em - 1) * 2;
	    adds = em - 1;

/*        xPTTRS:  N, NRHS  =>  M, N */

	} else if (lsamen_(&c__3, c3, "TRS")) {
	    mults = en * (em * 3 - 2);
	    adds = en * ((em - 1) * 2);

/*        xPTSV:  N, NRHS  =>  M, N */

	} else if (lsamen_(&c__3, c3, "SV ")) {
	    mults = (em - 1) * 2 + en * (em * 3 - 2);
	    adds = em - 1 + en * ((em - 1) * 2);
	}

/*     --------------------------------------------------------   
       SY:  SYmmetric indefinite matrices   
       SP:  Symmetric indefinite Packed matrices   
       HE:  HErmitian indefinite matrices (complex only)   
       HP:  Hermitian indefinite Packed matrices (complex only)   
       -------------------------------------------------------- */

    } else if (lsamen_(&c__2, c2, "SY") || lsamen_(&
	    c__2, c2, "SP") || lsamen_(&c__3, subnam, 
	    "CHE") || lsamen_(&c__3, subnam, "ZHE") || lsamen_(&c__3, subnam, "CHP") || lsamen_(&c__3, subnam, "ZHP")) 
	    {

/*        xSYTRF:  N  =>  M */

	if (lsamen_(&c__3, c3, "TRF")) {
	    mults = em * (em * (em * .16666666666666666f + .5f) + 
		    3.3333333333333335f);
	    adds = em / 6.f * (em * em - 1.f);

/*        xSYTRS:  N, NRHS  =>  M, N */

	} else if (lsamen_(&c__3, c3, "TRS")) {
	    mults = en * em * em;
	    adds = en * (em * (em - 1.f));

/*        xSYTRI:  N  =>  M */

	} else if (lsamen_(&c__3, c3, "TRI")) {
	    mults = em * (em * em * .33333333333333331f + .66666666666666663f)
		    ;
	    adds = em * (em * em * .33333333333333331f - .33333333333333331f);

/*        xSYTRD, xSYTD2:  N  =>  M */

	} else if (lsamen_(&c__3, c3, "TRD") || lsamen_(
		&c__3, c3, "TD2")) {
	    if (*m == 1) {
		mults = 0.f;
		adds = 0.f;
	    } else {
		mults = em * (em * (em * .66666666666666663f + 2.5f) - 
			.16666666666666666f) - 15.f;
		adds = em * (em * (em * .66666666666666663f + 1.f) - 
			2.6666666666666665f) - 4.f;
	    }
	}

/*     -------------------   
       Triangular matrices   
       ------------------- */

    } else if (lsamen_(&c__2, c2, "TR") || lsamen_(&
	    c__2, c2, "TP")) {

/*        xTRTRS:  N, NRHS  =>  M, N */

	if (lsamen_(&c__3, c3, "TRS")) {
	    mults = en * em * (em + 1.f) / 2.f;
	    adds = en * em * (em - 1.f) / 2.f;

/*        xTRTRI:  N  =>  M */

	} else if (lsamen_(&c__3, c3, "TRI")) {
	    mults = em * (em * (em * .16666666666666666f + .5f) + 
		    .33333333333333331f);
	    adds = em * (em * (em * .16666666666666666f - .5f) + 
		    .33333333333333331f);

	}

    } else if (lsamen_(&c__2, c2, "TB")) {

/*        xTBTRS:  N, NRHS, K  =>  M, N, KL */

	if (lsamen_(&c__3, c3, "TRS")) {
	    mults = en * (em * (em + 1.f) / 2.f - (em - ek - 1.f) * (em - ek) 
		    / 2.f);
	    adds = en * (em * (em - 1.f) / 2.f - (em - ek - 1.f) * (em - ek) /
		     2.f);
	}

/*     --------------------   
       Trapezoidal matrices   
       -------------------- */

    } else if (lsamen_(&c__2, c2, "TZ")) {

/*        xTZRQF:  M, N => M, N */

	if (lsamen_(&c__3, c3, "RQF")) {
	    emn = (real) min(*m,*n);
	    mults = em * 3 * (en - em + 1) + (en * 2 - em * 2 + 3) * (em * em 
		    - emn * (emn + 1) / 2);
	    adds = (en - em + 1) * (em + em * 2 * em - emn * (emn + 1));
	}

/*     -------------------   
       Orthogonal matrices   
       ------------------- */

    } else if (sord && lsamen_(&c__2, c2, "OR") || corz 
	    && lsamen_(&c__2, c2, "UN")) {

/*        -MQR, -MLQ, -MQL, or -MRQ:  M, N, K, SIDE  =>  M, N, KL, KU   
             where KU<= 0 indicates SIDE = 'L'   
             and   KU> 0  indicates SIDE = 'R' */

	if (lsamen_(&c__3, c3, "MQR") || lsamen_(&c__3, 
		c3, "MLQ") || lsamen_(&c__3, c3, "MQL") || lsamen_(&c__3, c3, "MRQ")) {
	    if (*ku <= 0) {
		mults = ek * en * (em * 2.f + 2.f - ek);
		adds = ek * en * (em * 2.f + 1.f - ek);
	    } else {
		mults = ek * (em * (en * 2.f - ek) + (em + en + (1.f - ek) / 
			2.f));
		adds = ek * em * (en * 2.f + 1.f - ek);
	    }

/*        -GQR or -GQL:  M, N, K  =>  M, N, KL */

	} else if (lsamen_(&c__3, c3, "GQR") || lsamen_(
		&c__3, c3, "GQL")) {
	    mults = ek * (en * 2.f - ek - 1.6666666666666667f + (em * 2.f * 
		    en + ek * (ek * .66666666666666663f - em - en)));
	    adds = ek * (en - em + .33333333333333331f + (em * 2.f * en + ek *
		     (ek * .66666666666666663f - em - en)));

/*        -GLQ or -GRQ:  M, N, K  =>  M, N, KL */

	} else if (lsamen_(&c__3, c3, "GLQ") || lsamen_(
		&c__3, c3, "GRQ")) {
	    mults = ek * (em + en - ek - .66666666666666663f + (em * 2.f * en 
		    + ek * (ek * .66666666666666663f - em - en)));
	    adds = ek * (em - en + .33333333333333331f + (em * 2.f * en + ek *
		     (ek * .66666666666666663f - em - en)));

	}

    }

    ret_val = mulfac * mults + addfac * adds;

    return ret_val;

/*     End of SOPLA */

} /* sopla_ */

