#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__3 = 3;
static integer c__0 = 0;

doublereal sopla2_(char *subnam, char *opts, integer *m, integer *n, integer *
	k, integer *l, integer *nb)
{
    /* System generated locals */
    address a__1[2];
    integer i__1[2], i__2, i__3, i__4;
    real ret_val;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen), s_cat(char *,
	     char **, integer *, integer *, ftnlen);

    /* Local variables */
    static char side[1], vect[1];
    static logical sord, corz;
    static char uplo[1];
    static integer iside;
    extern logical lsame_(char *, char *);
    extern doublereal sopla_(char *, integer *, integer *, integer *, integer 
	    *, integer *);
    static char c1[1], c2[2], c3[3];
    static integer mi, ni, nq;
    extern logical lsamen_(integer *, char *, char *);
    static integer ihi, ilo;
    static char sub2[6];


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 29, 1992   


    Purpose   
    =======   

    SOPLA2 computes an approximation of the number of floating point   
    operations used by the subroutine SUBNAM with character options   
    OPTS and parameters M, N, K, L, and NB.   

    This version counts operations for the LAPACK subroutines that   
    call other LAPACK routines.   

    Arguments   
    =========   

    SUBNAM  (input) CHARACTER*6   
            The name of the subroutine.   

    OPTS    (input) CHRACTER*(*)   
            A string of character options to subroutine SUBNAM.   

    M       (input) INTEGER   
            The number of rows of the coefficient matrix.   

    N       (input) INTEGER   
            The number of columns of the coefficient matrix.   

    K       (input) INTEGER   
            A third problem dimension, if needed.   

    L       (input) INTEGER   
            A fourth problem dimension, if needed.   

    NB      (input) INTEGER   
            The block size.  If needed, NB >= 1.   

    Notes   
    =====   

    In the comments below, the association is given between arguments   
    in the requested subroutine and local arguments.  For example,   

    xORMBR:  VECT // SIDE // TRANS, M, N, K   =>  OPTS, M, N, K   

    means that the character string VECT // SIDE // TRANS is passed to   
    the argument OPTS, and the integer parameters M, N, and K are passed   
    to the arguments M, N, and K,   

    =====================================================================   


       ---------------------------------------------------------   
       Initialize SOPLA2 to 0 and do a quick return if possible.   
       --------------------------------------------------------- */

    ret_val = 0.f;
    *(unsigned char *)c1 = *(unsigned char *)subnam;
    s_copy(c2, subnam + 1, (ftnlen)2, (ftnlen)2);
    s_copy(c3, subnam + 3, (ftnlen)3, (ftnlen)3);
    sord = lsame_(c1, "S") || lsame_(c1, "D");
    corz = lsame_(c1, "C") || lsame_(c1, "Z");
    if (*m <= 0 || ! (sord || corz)) {
	return ret_val;
    }

/*     -------------------   
       Orthogonal matrices   
       ------------------- */

    if (sord && lsamen_(&c__2, c2, "OR") || corz && 
	    lsamen_(&c__2, c2, "UN")) {

	if (lsamen_(&c__3, c3, "GBR")) {

/*           -GBR:  VECT, M, N, K  =>  OPTS, M, N, K */

	    *(unsigned char *)vect = *(unsigned char *)opts;
	    if (lsame_(vect, "Q")) {
/* Writing concatenation */
		i__1[0] = 3, a__1[0] = subnam;
		i__1[1] = 3, a__1[1] = "GQR";
		s_cat(sub2, a__1, i__1, &c__2, (ftnlen)6);
		if (*m >= *k) {
		    ret_val = sopla_(sub2, m, n, k, &c__0, nb);
		} else {
		    i__2 = *m - 1;
		    i__3 = *m - 1;
		    i__4 = *m - 1;
		    ret_val = sopla_(sub2, &i__2, &i__3, &i__4, &c__0, nb);
		}
	    } else {
/* Writing concatenation */
		i__1[0] = 3, a__1[0] = subnam;
		i__1[1] = 3, a__1[1] = "GLQ";
		s_cat(sub2, a__1, i__1, &c__2, (ftnlen)6);
		if (*k < *n) {
		    ret_val = sopla_(sub2, m, n, k, &c__0, nb);
		} else {
		    i__2 = *n - 1;
		    i__3 = *n - 1;
		    i__4 = *n - 1;
		    ret_val = sopla_(sub2, &i__2, &i__3, &i__4, &c__0, nb);
		}
	    }

	} else if (lsamen_(&c__3, c3, "MBR")) {

/*           -MBR:  VECT // SIDE // TRANS, M, N, K  =>  OPTS, M, N, K */

	    *(unsigned char *)vect = *(unsigned char *)opts;
	    *(unsigned char *)side = *(unsigned char *)&opts[1];
	    if (lsame_(side, "L")) {
		nq = *m;
		iside = 0;
	    } else {
		nq = *n;
		iside = 1;
	    }
	    if (lsame_(vect, "Q")) {
/* Writing concatenation */
		i__1[0] = 3, a__1[0] = subnam;
		i__1[1] = 3, a__1[1] = "MQR";
		s_cat(sub2, a__1, i__1, &c__2, (ftnlen)6);
		if (nq >= *k) {
		    ret_val = sopla_(sub2, m, n, k, &iside, nb);
		} else if (iside == 0) {
		    i__2 = *m - 1;
		    i__3 = nq - 1;
		    ret_val = sopla_(sub2, &i__2, n, &i__3, &iside, nb);
		} else {
		    i__2 = *n - 1;
		    i__3 = nq - 1;
		    ret_val = sopla_(sub2, m, &i__2, &i__3, &iside, nb);
		}
	    } else {
/* Writing concatenation */
		i__1[0] = 3, a__1[0] = subnam;
		i__1[1] = 3, a__1[1] = "MLQ";
		s_cat(sub2, a__1, i__1, &c__2, (ftnlen)6);
		if (nq > *k) {
		    ret_val = sopla_(sub2, m, n, k, &iside, nb);
		} else if (iside == 0) {
		    i__2 = *m - 1;
		    i__3 = nq - 1;
		    ret_val = sopla_(sub2, &i__2, n, &i__3, &iside, nb);
		} else {
		    i__2 = *n - 1;
		    i__3 = nq - 1;
		    ret_val = sopla_(sub2, m, &i__2, &i__3, &iside, nb);
		}
	    }

	} else if (lsamen_(&c__3, c3, "GHR")) {

/*           -GHR:  N, ILO, IHI  =>  M, N, K */

	    ilo = *n;
	    ihi = *k;
/* Writing concatenation */
	    i__1[0] = 3, a__1[0] = subnam;
	    i__1[1] = 3, a__1[1] = "GQR";
	    s_cat(sub2, a__1, i__1, &c__2, (ftnlen)6);
	    i__2 = ihi - ilo;
	    i__3 = ihi - ilo;
	    i__4 = ihi - ilo;
	    ret_val = sopla_(sub2, &i__2, &i__3, &i__4, &c__0, nb);

	} else if (lsamen_(&c__3, c3, "MHR")) {

/*           -MHR:  SIDE // TRANS, M, N, ILO, IHI  =>  OPTS, M, N, K, L */

	    *(unsigned char *)side = *(unsigned char *)opts;
	    ilo = *k;
	    ihi = *l;
	    if (lsame_(side, "L")) {
		mi = ihi - ilo;
		ni = *n;
		iside = -1;
	    } else {
		mi = *m;
		ni = ihi - ilo;
		iside = 1;
	    }
/* Writing concatenation */
	    i__1[0] = 3, a__1[0] = subnam;
	    i__1[1] = 3, a__1[1] = "MQR";
	    s_cat(sub2, a__1, i__1, &c__2, (ftnlen)6);
	    i__2 = ihi - ilo;
	    ret_val = sopla_(sub2, &mi, &ni, &i__2, &iside, nb);

	} else if (lsamen_(&c__3, c3, "GTR")) {

/*           -GTR:  UPLO, N  =>  OPTS, M */

	    *(unsigned char *)uplo = *(unsigned char *)opts;
	    if (lsame_(uplo, "U")) {
/* Writing concatenation */
		i__1[0] = 3, a__1[0] = subnam;
		i__1[1] = 3, a__1[1] = "GQL";
		s_cat(sub2, a__1, i__1, &c__2, (ftnlen)6);
		i__2 = *m - 1;
		i__3 = *m - 1;
		i__4 = *m - 1;
		ret_val = sopla_(sub2, &i__2, &i__3, &i__4, &c__0, nb);
	    } else {
/* Writing concatenation */
		i__1[0] = 3, a__1[0] = subnam;
		i__1[1] = 3, a__1[1] = "GQR";
		s_cat(sub2, a__1, i__1, &c__2, (ftnlen)6);
		i__2 = *m - 1;
		i__3 = *m - 1;
		i__4 = *m - 1;
		ret_val = sopla_(sub2, &i__2, &i__3, &i__4, &c__0, nb);
	    }

	} else if (lsamen_(&c__3, c3, "MTR")) {

/*           -MTR:  SIDE // UPLO // TRANS, M, N  =>  OPTS, M, N */

	    *(unsigned char *)side = *(unsigned char *)opts;
	    *(unsigned char *)uplo = *(unsigned char *)&opts[1];
	    if (lsame_(side, "L")) {
		mi = *m - 1;
		ni = *n;
		nq = *m;
		iside = -1;
	    } else {
		mi = *m;
		ni = *n - 1;
		nq = *n;
		iside = 1;
	    }

	    if (lsame_(uplo, "U")) {
/* Writing concatenation */
		i__1[0] = 3, a__1[0] = subnam;
		i__1[1] = 3, a__1[1] = "MQL";
		s_cat(sub2, a__1, i__1, &c__2, (ftnlen)6);
		i__2 = nq - 1;
		ret_val = sopla_(sub2, &mi, &ni, &i__2, &iside, nb)
			;
	    } else {
/* Writing concatenation */
		i__1[0] = 3, a__1[0] = subnam;
		i__1[1] = 3, a__1[1] = "MQR";
		s_cat(sub2, a__1, i__1, &c__2, (ftnlen)6);
		i__2 = nq - 1;
		ret_val = sopla_(sub2, &mi, &ni, &i__2, &iside, nb)
			;
	    }

	}
    }

    return ret_val;

/*     End of SOPLA2 */

} /* sopla2_ */

