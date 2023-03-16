#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__2 = 2;
static integer c__3 = 3;

/* Subroutine */ int atimck_(integer *ichk, char *subnam, integer *nn, 
	integer *nval, integer *nlda, integer *ldaval, integer *nout, integer 
	*info, ftnlen subnam_len)
{
    /* Format strings */
    static char fmt_9999[] = "(\002 *** Error for \002,a6,\002:  M > LDA for"
	    " M =\002,i6,\002, LDA =\002,i7)";
    static char fmt_9998[] = "(\002 *** Error for \002,a6,\002:  N > LDA for"
	    " N =\002,i6,\002, LDA =\002,i7)";
    static char fmt_9997[] = "(\002 *** Error for \002,a6,\002:  K > LDA for"
	    " K =\002,i6,\002, LDA =\002,i7)";
    static char fmt_9996[] = "(\002 *** Error for \002,a6,\002:  N*(N+1)/2 >"
	    " LA for N =\002,i6,\002, LA =\002,i7)";
    static char fmt_9994[] = "(\002 *** Error for \002,a6,\002:  2*K+1 > LDA"
	    " for K =\002,i6,\002, LDA =\002,i7,/\002 --> Increase LDA to at "
	    "least \002,i7)";
    static char fmt_9995[] = "(\002 *** Error for \002,a6,\002:  3*K+1 > LDA"
	    " for K =\002,i6,\002, LDA =\002,i7,/\002 --> Increase LDA to at "
	    "least \002,i7)";
    static char fmt_9993[] = "(\002 *** Error for \002,a6,\002:  K+1 > LDA f"
	    "or K =\002,i6,\002, LD\002,\002A =\002,i7)";
    static char fmt_9992[] = "(\002 *** Error for \002,a6,\002:  2*K+2 > LDA"
	    " for K =\002,i6,\002, \002,\002LDA =\002,i7)";

    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static char type__[2];
    static integer i__, j, n;
    extern logical lsamen_(integer *, char *, char *);
    static integer lda;

    /* Fortran I/O blocks */
    static cilist io___5 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___6 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___7 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___9 = { 0, 0, 0, fmt_9996, 0 };
    static cilist io___10 = { 0, 0, 0, fmt_9994, 0 };
    static cilist io___11 = { 0, 0, 0, fmt_9995, 0 };
    static cilist io___12 = { 0, 0, 0, fmt_9993, 0 };
    static cilist io___13 = { 0, 0, 0, fmt_9992, 0 };



/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       March 31, 1993   


    Purpose   
    =======   

    ATIMCK checks the input values of M, N, or K and LDA to determine   
    if they are valid for type TYPE.  The tests to be performed are   
    specified in the option variable ICHK.   

    On exit, INFO contains a count of the number of pairs (N,LDA) that   
    were invalid.   

    Arguments   
    =========   

    ICHK    (input) INTEGER   
            Specifies the type of comparison   
            = 1:  M <= LDA   
            = 2:  N <= LDA   
            = 3:  K <= LDA   
            = 4:  N*(N+1)/2 <= LA   
            = 0 or other value:  Determined from name passed in SUBNAM   

    SUBNAM  (input) CHARACTER*6   
            The name of the subroutine or path for which the input   
            values are to be tested.   

    NN      (input) INTEGER   
            The number of values of N contained in the vector NVAL.   

    NVAL    (input) INTEGER array, dimension( NN )   
            The values of the matrix size N.   

    NLDA    (input) INTEGER   
            The number of values of LDA contained in the vector LDAVAL.   

    LDAVAL  (input) INTEGER array, dimension( NLDA )   
            The values of the leading dimension of the array A.   

    NOUT    (input) INTEGER   
            The unit number for output.   

    INFO    (output) INTEGER   
            The number of pairs (N, LDA) that were invalid.   

    =====================================================================   


       Parameter adjustments */
    --ldaval;
    --nval;

    /* Function Body */
    s_copy(type__, subnam + 1, (ftnlen)2, (ftnlen)2);
    *info = 0;

/*     M, N, or K must be less than or equal to LDA. */

    if (*ichk == 1 || *ichk == 2 || *ichk == 3) {
	i__1 = *nlda;
	for (j = 1; j <= i__1; ++j) {
	    lda = ldaval[j];
	    i__2 = *nn;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		if (nval[i__] > lda) {
		    ++(*info);
		    if (*nout > 0) {
			if (*ichk == 1) {
			    io___5.ciunit = *nout;
			    s_wsfe(&io___5);
			    do_fio(&c__1, subnam, (ftnlen)6);
			    do_fio(&c__1, (char *)&nval[i__], (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&lda, (ftnlen)sizeof(
				    integer));
			    e_wsfe();
			} else if (*ichk == 2) {
			    io___6.ciunit = *nout;
			    s_wsfe(&io___6);
			    do_fio(&c__1, subnam, (ftnlen)6);
			    do_fio(&c__1, (char *)&nval[i__], (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&lda, (ftnlen)sizeof(
				    integer));
			    e_wsfe();
			} else {
			    io___7.ciunit = *nout;
			    s_wsfe(&io___7);
			    do_fio(&c__1, subnam, (ftnlen)6);
			    do_fio(&c__1, (char *)&nval[i__], (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&lda, (ftnlen)sizeof(
				    integer));
			    e_wsfe();
			}
		    }
		}
/* L10: */
	    }
/* L20: */
	}

/*     IF TYPE = 'PP', 'SP', or 'HP',   
       then N*(N+1)/2 must be less than or equal to LA = LDAVAL(1). */

    } else if (*ichk == 4) {
	lda = ldaval[1];
	i__1 = *nn;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    n = nval[i__];
	    if (n * (n + 1) / 2 > lda) {
		++(*info);
		if (*nout > 0) {
		    io___9.ciunit = *nout;
		    s_wsfe(&io___9);
		    do_fio(&c__1, subnam, (ftnlen)6);
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&lda, (ftnlen)sizeof(integer));
		    e_wsfe();
		}
	    }
/* L30: */
	}

/*     IF TYPE = 'GB', then K must satisfy   
          2*K+1 <= LDA,  if SUBNAM = 'xGBMV'   
          3*K+1 <= LDA,  otherwise. */

    } else if (lsamen_(&c__2, type__, "GB")) {
	if (lsamen_(&c__3, subnam + 3, "MV ")) {
	    i__1 = *nlda;
	    for (j = 1; j <= i__1; ++j) {
		lda = ldaval[j];
		i__2 = *nn;
		for (i__ = 1; i__ <= i__2; ++i__) {
		    if ((nval[i__] << 1) + 1 > lda) {
			++(*info);
			if (*nout > 0) {
			    io___10.ciunit = *nout;
			    s_wsfe(&io___10);
			    do_fio(&c__1, subnam, (ftnlen)6);
			    do_fio(&c__1, (char *)&nval[i__], (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&lda, (ftnlen)sizeof(
				    integer));
			    i__3 = (nval[i__] << 1) + 1;
			    do_fio(&c__1, (char *)&i__3, (ftnlen)sizeof(
				    integer));
			    e_wsfe();
			}
		    }
/* L40: */
		}
/* L50: */
	    }
	} else {
	    i__1 = *nlda;
	    for (j = 1; j <= i__1; ++j) {
		lda = ldaval[j];
		i__2 = *nn;
		for (i__ = 1; i__ <= i__2; ++i__) {
		    if (nval[i__] * 3 + 1 > lda) {
			++(*info);
			if (*nout > 0) {
			    io___11.ciunit = *nout;
			    s_wsfe(&io___11);
			    do_fio(&c__1, subnam, (ftnlen)6);
			    do_fio(&c__1, (char *)&nval[i__], (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&lda, (ftnlen)sizeof(
				    integer));
			    i__3 = nval[i__] * 3 + 1;
			    do_fio(&c__1, (char *)&i__3, (ftnlen)sizeof(
				    integer));
			    e_wsfe();
			}
		    }
/* L60: */
		}
/* L70: */
	    }
	}

/*     IF TYPE = 'PB' or 'TB', then K must satisfy   
          K+1 <= LDA. */

    } else if (lsamen_(&c__2, type__, "PB") || lsamen_(&
	    c__2, type__, "TB")) {
	i__1 = *nlda;
	for (j = 1; j <= i__1; ++j) {
	    lda = ldaval[j];
	    i__2 = *nn;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		if (nval[i__] + 1 > lda) {
		    ++(*info);
		    if (*nout > 0) {
			io___12.ciunit = *nout;
			s_wsfe(&io___12);
			do_fio(&c__1, subnam, (ftnlen)6);
			do_fio(&c__1, (char *)&nval[i__], (ftnlen)sizeof(
				integer));
			do_fio(&c__1, (char *)&lda, (ftnlen)sizeof(integer));
			e_wsfe();
		    }
		}
/* L80: */
	    }
/* L90: */
	}

/*     IF TYPE = 'SB' or 'HB', then K must satisfy   
          K+1   <= LDA,  if SUBNAM = 'xxxMV ' */

    } else if (lsamen_(&c__2, type__, "SB") || lsamen_(&
	    c__2, type__, "HB")) {
	if (lsamen_(&c__3, subnam + 3, "MV ")) {
	    i__1 = *nlda;
	    for (j = 1; j <= i__1; ++j) {
		lda = ldaval[j];
		i__2 = *nn;
		for (i__ = 1; i__ <= i__2; ++i__) {
		    if (nval[i__] + 1 > lda) {
			++(*info);
			if (*nout > 0) {
			    io___13.ciunit = *nout;
			    s_wsfe(&io___13);
			    do_fio(&c__1, subnam, (ftnlen)6);
			    do_fio(&c__1, (char *)&nval[i__], (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&lda, (ftnlen)sizeof(
				    integer));
			    e_wsfe();
			}
		    }
/* L100: */
		}
/* L110: */
	    }
	}

    }

    return 0;

/*     End of ATIMCK */

} /* atimck_ */

