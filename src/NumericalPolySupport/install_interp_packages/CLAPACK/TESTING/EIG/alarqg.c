#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* Subroutine */ int alarqg_(char *path, integer *nmats, logical *dotype, 
	integer *ntypes, integer *nin, integer *nout)
{
    /* Initialized data */

    static char intstr[10] = "0123456789";

    /* Format strings */
    static char fmt_9995[] = "(//\002 *** Not enough matrix types on input l"
	    "ine\002,/a79)";
    static char fmt_9994[] = "(\002 ==> Specify \002,i4,\002 matrix types on"
	    " this line or \002,\002adjust NTYPES on previous line\002)";
    static char fmt_9996[] = "(//\002 *** Invalid integer value in column"
	    " \002,i2,\002 of input\002,\002 line:\002,/a79)";
    static char fmt_9997[] = "(\002 *** Warning:  duplicate request of matri"
	    "x type \002,i2,\002 for \002,a3)";
    static char fmt_9999[] = "(\002 *** Invalid type request for \002,a3,"
	    "\002, type  \002,i4,\002: must satisfy  1 <= type <= \002,i2)";
    static char fmt_9998[] = "(/\002 *** End of file reached when trying to "
	    "read matrix \002,\002types for \002,a3,/\002 *** Check that you "
	    "are requesting the\002,\002 right number of types for each pat"
	    "h\002,/)";

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_rsfe(cilist *), do_fio(integer *, char *, ftnlen), e_rsfe(void),
	     i_len(char *, ftnlen), s_wsfe(cilist *), e_wsfe(void), s_wsle(
	    cilist *), e_wsle(void);
    /* Subroutine */ int s_stop(char *, ftnlen);

    /* Local variables */
    static char line[80];
    static integer lenp, nreq[100], i__, j, k;
    static char c1[1];
    static integer i1, ic, nt;
    static logical firstt;

    /* Fortran I/O blocks */
    static cilist io___4 = { 0, 0, 1, "(A80)", 0 };
    static cilist io___10 = { 0, 0, 0, fmt_9995, 0 };
    static cilist io___11 = { 0, 0, 0, fmt_9994, 0 };
    static cilist io___15 = { 0, 0, 0, fmt_9996, 0 };
    static cilist io___16 = { 0, 0, 0, fmt_9994, 0 };
    static cilist io___18 = { 0, 0, 0, 0, 0 };
    static cilist io___19 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___20 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___21 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___22 = { 0, 0, 0, 0, 0 };



/*  -- LAPACK test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       March 5, 1993   


    Purpose   
    =======   

    ALARQG handles input for the LAPACK test program.  It is called   
    to evaluate the input line which requested NMATS matrix types for   
    PATH.  The flow of control is as follows:   

    If NMATS = NTYPES then   
       DOTYPE(1:NTYPES) = .TRUE.   
    else   
       Read the next input line for NMATS matrix types   
       Set DOTYPE(I) = .TRUE. for each valid type I   
    endif   

    Arguments   
    =========   

    PATH    (input) CHARACTER*3   
            An LAPACK path name for testing.   

    NMATS   (input) INTEGER   
            The number of matrix types to be used in testing this path.   

    DOTYPE  (output) LOGICAL array, dimension (NTYPES)   
            The vector of flags indicating if each type will be tested.   

    NTYPES  (input) INTEGER   
            The maximum number of matrix types for this path.   

    NIN     (input) INTEGER   
            The unit number for input.  NIN >= 1.   

    NOUT    (input) INTEGER   
            The unit number for output.  NOUT >= 1.   

   ======================================================================   

       Parameter adjustments */
    --dotype;

    /* Function Body */

    if (*nmats >= *ntypes) {

/*        Test everything if NMATS >= NTYPES. */

	i__1 = *ntypes;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    dotype[i__] = TRUE_;
/* L10: */
	}
    } else {
	i__1 = *ntypes;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    dotype[i__] = FALSE_;
/* L20: */
	}
	firstt = TRUE_;

/*        Read a line of matrix types if 0 < NMATS < NTYPES. */

	if (*nmats > 0) {
	    io___4.ciunit = *nin;
	    i__1 = s_rsfe(&io___4);
	    if (i__1 != 0) {
		goto L90;
	    }
	    i__1 = do_fio(&c__1, line, (ftnlen)80);
	    if (i__1 != 0) {
		goto L90;
	    }
	    i__1 = e_rsfe();
	    if (i__1 != 0) {
		goto L90;
	    }
	    lenp = i_len(line, (ftnlen)80);
	    i__ = 0;
	    i__1 = *nmats;
	    for (j = 1; j <= i__1; ++j) {
		nreq[j - 1] = 0;
		i1 = 0;
L30:
		++i__;
		if (i__ > lenp) {
		    if (j == *nmats && i1 > 0) {
			goto L60;
		    } else {
			io___10.ciunit = *nout;
			s_wsfe(&io___10);
			do_fio(&c__1, line, (ftnlen)80);
			e_wsfe();
			io___11.ciunit = *nout;
			s_wsfe(&io___11);
			do_fio(&c__1, (char *)&(*nmats), (ftnlen)sizeof(
				integer));
			e_wsfe();
			goto L80;
		    }
		}
		if (*(unsigned char *)&line[i__ - 1] != ' ' && *(unsigned 
			char *)&line[i__ - 1] != ',') {
		    i1 = i__;
		    *(unsigned char *)c1 = *(unsigned char *)&line[i1 - 1];

/*              Check that a valid integer was read */

		    for (k = 1; k <= 10; ++k) {
			if (*(unsigned char *)c1 == *(unsigned char *)&intstr[
				k - 1]) {
			    ic = k - 1;
			    goto L50;
			}
/* L40: */
		    }
		    io___15.ciunit = *nout;
		    s_wsfe(&io___15);
		    do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
		    do_fio(&c__1, line, (ftnlen)80);
		    e_wsfe();
		    io___16.ciunit = *nout;
		    s_wsfe(&io___16);
		    do_fio(&c__1, (char *)&(*nmats), (ftnlen)sizeof(integer));
		    e_wsfe();
		    goto L80;
L50:
		    nreq[j - 1] = nreq[j - 1] * 10 + ic;
		    goto L30;
		} else if (i1 > 0) {
		    goto L60;
		} else {
		    goto L30;
		}
L60:
		;
	    }
	}
	i__1 = *nmats;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    nt = nreq[i__ - 1];
	    if (nt > 0 && nt <= *ntypes) {
		if (dotype[nt]) {
		    if (firstt) {
			io___18.ciunit = *nout;
			s_wsle(&io___18);
			e_wsle();
		    }
		    firstt = FALSE_;
		    io___19.ciunit = *nout;
		    s_wsfe(&io___19);
		    do_fio(&c__1, (char *)&nt, (ftnlen)sizeof(integer));
		    do_fio(&c__1, path, (ftnlen)3);
		    e_wsfe();
		}
		dotype[nt] = TRUE_;
	    } else {
		io___20.ciunit = *nout;
		s_wsfe(&io___20);
		do_fio(&c__1, path, (ftnlen)3);
		do_fio(&c__1, (char *)&nt, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&(*ntypes), (ftnlen)sizeof(integer));
		e_wsfe();
	    }
/* L70: */
	}
L80:
	;
    }
    return 0;

L90:
    io___21.ciunit = *nout;
    s_wsfe(&io___21);
    do_fio(&c__1, path, (ftnlen)3);
    e_wsfe();
    io___22.ciunit = *nout;
    s_wsle(&io___22);
    e_wsle();
    s_stop("", (ftnlen)0);

/*     End of ALARQG */

    return 0;
} /* alarqg_ */

