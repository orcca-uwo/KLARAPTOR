#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* Subroutine */ int atimin_(char *path, char *line, integer *nsubs, char *
	names, logical *timsub, integer *nout, integer *info, ftnlen path_len,
	 ftnlen line_len, ftnlen names_len)
{
    /* Format strings */
    static char fmt_9999[] = "(1x,a,\002:  Unrecognized path or subroutine n"
	    "ame\002,/)";
    static char fmt_9998[] = "(1x,a,\002 was not timed\002,/)";

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer i_len(char *, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static integer isub, i__;
    static char cname[6];
    extern logical lsame_(char *, char *);
    static integer lpath, istop, lcname, lnames;
    extern logical lsamen_(integer *, char *, char *);
    static integer istart;
    static logical req;

    /* Fortran I/O blocks */
    static cilist io___8 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___11 = { 0, 0, 0, fmt_9998, 0 };



#define names_ref(a_0,a_1) &names[(a_1)*names_len + a_0]


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 29, 1992   


    Purpose   
    =======   

    ATIMIN interprets the input line for the timing routines.   
    The LOGICAL array TIMSUB returns .true. for each routine to be   
    timed and .false. for the routines which are not to be timed.   

    Arguments   
    =========   

    PATH    (input) CHARACTER*(*)   
            The LAPACK path name of the calling routine.  The path name   
            may be at most 6 characters long.  If LINE(1:LEN(PATH)) is   
            the same as PATH, then the input line is searched for NSUBS   
            non-blank characters, otherwise, the input line is assumed to   
            specify a single subroutine name.   

    LINE    (input) CHARACTER*80   
            The input line to be evaluated.  The path or subroutine name   
            must begin in column 1 and the part of the line after the   
            name is used to indicate the routines to be timed.   
            See below for further details.   

    NSUBS   (input) INTEGER   
            The number of subroutines in the LAPACK path name of the   
            calling routine.   

    NAMES   (input) CHARACTER*(*) array, dimension (NSUBS)   
            The names of the subroutines in the LAPACK path name of the   
            calling routine.   

    TIMSUB  (output) LOGICAL array, dimension (NSUBS)   
            For each I from 1 to NSUBS, TIMSUB( I ) is set to .true. if   
            the subroutine NAMES( I ) is to be timed; otherwise,   
            TIMSUB( I ) is set to .false.   

    NOUT    (input) INTEGER   
            The unit number on which error messages will be printed.   

    INFO    (output) INTEGER   
            The return status of this routine.   
            = -1:  Unrecognized path or subroutine name   
            =  0:  Normal return   
            =  1:  Name was recognized, but no timing requested   

    Further Details   
    ======= =======   

    An input line begins with a subroutine or path name, optionally   
    followed by one or more non-blank characters indicating the specific   
    routines to be timed.   

    If the character string in PATH appears at the beginning of LINE,   
    up to NSUBS routines may be timed.  If LINE is blank after the path   
    name, all the routines in the path will be timed.  If LINE is not   
    blank after the path name, the rest of the line is searched   
    for NSUBS nonblank characters, and if the i-th such character is   
    't' or 'T', then the i-th subroutine in this path will be timed.   
    For example, the input line   
       SGE    T T T T   
    requests timing of the first 4 subroutines in the SGE path.   

    If the character string in PATH does not appear at the beginning of   
    LINE, then LINE is assumed to begin with a subroutine name.  The name   
    is assumed to end in column 6 or in column i if column i+1 is blank   
    and i+1 <= 6.  If LINE is completely blank after the subroutine name,   
    the routine will be timed.  If LINE is not blank after the subroutine   
    name, then the subroutine will be timed if the first non-blank after   
    the name is 't' or 'T'.   

    =====================================================================   



       Initialize   

       Parameter adjustments */
    --timsub;
    names -= names_len;

    /* Function Body */
    *info = 0;
    lcname = 1;
    for (i__ = 2; i__ <= 6; ++i__) {
	if (*(unsigned char *)&line[i__ - 1] == ' ') {
	    goto L20;
	}
	lcname = i__;
/* L10: */
    }
L20:
/* Computing MIN */
    i__1 = lcname + 1, i__2 = i_len(path, path_len);
    lpath = min(i__1,i__2);
/* Computing MIN */
    i__1 = lcname + 1, i__2 = i_len(names_ref(0, 1), names_len);
    lnames = min(i__1,i__2);
    s_copy(cname, line, (ftnlen)6, lcname);

    i__1 = *nsubs;
    for (i__ = 1; i__ <= i__1; ++i__) {
	timsub[i__] = FALSE_;
/* L30: */
    }
    istop = 0;

/*     Check for a valid path or subroutine name. */

    if (lcname <= i_len(path, path_len) && lsamen_(&lpath, cname, path)) {
	istart = 1;
	istop = *nsubs;
    } else if (lcname <= i_len(names_ref(0, 1), names_len)) {
	i__1 = *nsubs;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (lsamen_(&lnames, cname, names_ref(0, i__))) {
		istart = i__;
		istop = i__;
	    }
/* L40: */
	}
    }

    if (istop == 0) {
	io___8.ciunit = *nout;
	s_wsfe(&io___8);
	do_fio(&c__1, cname, (ftnlen)6);
	e_wsfe();
	*info = -1;
	goto L110;
    }

/*     Search the rest of the input line for 1 or NSUBS nonblank   
       characters, where 'T' or 't' means 'Time this routine'. */

    isub = istart;
    for (i__ = lcname + 1; i__ <= 80; ++i__) {
	if (*(unsigned char *)&line[i__ - 1] != ' ') {
	    timsub[isub] = lsame_(line + (i__ - 1), "T")
		    ;
	    ++isub;
	    if (isub > istop) {
		goto L60;
	    }
	}
/* L50: */
    }
L60:

/*     If no characters appear after the routine or path name, then   
       time the routine or all the routines in the path. */

    if (isub == istart) {
	i__1 = istop;
	for (i__ = istart; i__ <= i__1; ++i__) {
	    timsub[i__] = TRUE_;
/* L70: */
	}
    } else {

/*        Test to see if any timing was requested. */

	req = FALSE_;
	i__1 = isub - 1;
	for (i__ = istart; i__ <= i__1; ++i__) {
	    req = req || timsub[i__];
/* L80: */
	}
	if (! req) {
	    io___11.ciunit = *nout;
	    s_wsfe(&io___11);
	    do_fio(&c__1, cname, (ftnlen)6);
	    e_wsfe();
	    *info = 1;
	    goto L110;
	}
/* L90:   

         If fewer than NSUBS characters are specified for a path name,   
         the rest are assumed to be 'F'. */

	i__1 = istop;
	for (i__ = isub; i__ <= i__1; ++i__) {
	    timsub[i__] = FALSE_;
/* L100: */
	}
    }
L110:
    return 0;

/*     End of ATIMIN */

} /* atimin_ */

#undef names_ref


