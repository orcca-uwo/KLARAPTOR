#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__3 = 3;

doublereal sopgb_(char *subnam, integer *m, integer *n, integer *kl, integer *
	ku, integer *ipiv)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    real ret_val;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static real adds;
    static logical sord, corz;
    static integer i__, j;
    extern logical lsame_(char *, char *);
    static char c1[1], c2[2], c3[3];
    static real mults, addfac;
    static integer km, jp, ju;
    static real mulfac;
    extern logical lsamen_(integer *, char *, char *);


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       March 31, 1993   


    Purpose   
    =======   

    SOPGB counts operations for the LU factorization of a band matrix   
    xGBTRF.   

    Arguments   
    =========   

    SUBNAM  (input) CHARACTER*6   
            The name of the subroutine.   

    M       (input) INTEGER   
            The number of rows of the coefficient matrix.  M >= 0.   

    N       (input) INTEGER   
            The number of columns of the coefficient matrix.  N >= 0.   

    KL      (input) INTEGER   
            The number of subdiagonals of the matrix.  KL >= 0.   

    KU      (input) INTEGER   
            The number of superdiagonals of the matrix.  KU >= 0.   

    IPIV    (input)  INTEGER array, dimension (min(M,N))   
            The vector of pivot indices from SGBTRF or CGBTRF.   

    =====================================================================   


       Parameter adjustments */
    --ipiv;

    /* Function Body */
    ret_val = 0.f;
    mults = 0.f;
    adds = 0.f;
    *(unsigned char *)c1 = *(unsigned char *)subnam;
    s_copy(c2, subnam + 1, (ftnlen)2, (ftnlen)2);
    s_copy(c3, subnam + 3, (ftnlen)3, (ftnlen)3);
    sord = lsame_(c1, "S") || lsame_(c1, "D");
    corz = lsame_(c1, "C") || lsame_(c1, "Z");
    if (! (sord || corz)) {
	return ret_val;
    }
    if (lsame_(c1, "S") || lsame_(c1, "D")) {
	addfac = 1.f;
	mulfac = 1.f;
    } else {
	addfac = 2.f;
	mulfac = 6.f;
    }

/*     --------------------------   
       GB:  General Band matrices   
       -------------------------- */

    if (lsamen_(&c__2, c2, "GB")) {

/*        xGBTRF:  M, N, KL, KU  =>  M, N, KL, KU */

	if (lsamen_(&c__3, c3, "TRF")) {
	    ju = 1;
	    i__1 = min(*m,*n);
	    for (j = 1; j <= i__1; ++j) {
/* Computing MIN */
		i__2 = *kl, i__3 = *m - j;
		km = min(i__2,i__3);
		jp = ipiv[j];
/* Computing MAX   
   Computing MIN */
		i__4 = jp + *ku;
		i__2 = ju, i__3 = min(i__4,*n);
		ju = max(i__2,i__3);
		if (km > 0) {
		    mults += km * (ju + 1 - j);
		    adds += km * (ju - j);
		}
/* L10: */
	    }
	}

/*     ---------------------------------   
       GT:  General Tridiagonal matrices   
       --------------------------------- */

    } else if (lsamen_(&c__2, c2, "GT")) {

/*        xGTTRF:  N  =>  M */

	if (lsamen_(&c__3, c3, "TRF")) {
	    mults = (real) (*m - 1 << 1);
	    adds = (real) (*m - 1);
	    i__1 = *m - 2;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		if (ipiv[i__] != i__) {
		    mults += 1;
		}
/* L20: */
	    }

/*        xGTTRS:  N, NRHS  =>  M, N */

	} else if (lsamen_(&c__3, c3, "TRS")) {
	    mults = (real) ((*n << 2) * (*m - 1));
	    adds = (real) (*n * 3 * (*m - 1));

/*        xGTSV:   N, NRHS  =>  M, N */

	} else if (lsamen_(&c__3, c3, "SV ")) {
	    mults = (real) (((*n << 2) + 2) * (*m - 1));
	    adds = (real) ((*n * 3 + 1) * (*m - 1));
	    i__1 = *m - 2;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		if (ipiv[i__] != i__) {
		    mults += 1;
		}
/* L30: */
	    }
	}
    }

    ret_val = mulfac * mults + addfac * adds;
    return ret_val;

/*     End of SOPGB */

} /* sopgb_ */

