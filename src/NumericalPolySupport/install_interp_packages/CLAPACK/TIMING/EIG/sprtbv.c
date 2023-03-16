#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* Subroutine */ int sprtbv_(char *subnam, integer *ntypes, logical *dotype, 
	integer *nsizes, integer *mm, integer *nn, integer *inparm, char *
	pnames, integer *nparms, integer *np1, integer *np2, real *ops, 
	integer *ldo1, integer *ldo2, real *times, integer *ldt1, integer *
	ldt2, real *rwork, logical *llwork, integer *nout, ftnlen subnam_len, 
	ftnlen pnames_len)
{
    /* Format strings */
    static char fmt_9999[] = "(///\002 ****** Results for \002,a,\002 *****"
	    "*\002)";
    static char fmt_9995[] = "(5x,:\002with \002,a,\002=\002,i5,3(:\002, "
	    "\002,a,\002=\002,i5))";
    static char fmt_9994[] = "(5x,:\002line \002,i2,\002 with \002,a,\002"
	    "=\002,i5,3(:\002, \002,a,\002=\002,i5))";
    static char fmt_9996[] = "(/\002 *** Time in seconds ***\002)";
    static char fmt_9997[] = "(/\002 *** Number of floating-point operations"
	    " ***\002)";
    static char fmt_9998[] = "(/\002 *** Speed in megaflops ***\002)";

    /* System generated locals */
    integer ops_dim1, ops_dim2, ops_offset, times_dim1, times_dim2, 
	    times_offset, i__1, i__2, i__3;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static integer ipar, i__, j, iline, iinfo;
    static logical ltemp;
    static integer jp, js, jt, ilines;
    extern doublereal smflop_(real *, real *, integer *);
    extern /* Subroutine */ int sprtbr_(char *, char *, integer *, logical *, 
	    integer *, integer *, integer *, integer *, logical *, real *, 
	    integer *, integer *, integer *, ftnlen, ftnlen);

    /* Fortran I/O blocks */
    static cilist io___1 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___6 = { 0, 0, 0, fmt_9995, 0 };
    static cilist io___7 = { 0, 0, 0, fmt_9995, 0 };
    static cilist io___9 = { 0, 0, 0, fmt_9994, 0 };
    static cilist io___10 = { 0, 0, 0, fmt_9994, 0 };
    static cilist io___11 = { 0, 0, 0, fmt_9996, 0 };
    static cilist io___12 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___18 = { 0, 0, 0, fmt_9998, 0 };



#define times_ref(a_1,a_2,a_3) times[((a_3)*times_dim2 + (a_2))*\
times_dim1 + a_1]
#define pnames_ref(a_0,a_1) &pnames[(a_1)*pnames_len + a_0]
#define ops_ref(a_1,a_2,a_3) ops[((a_3)*ops_dim2 + (a_2))*ops_dim1 + a_1]


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 29, 1992   


    Purpose   
    =======   

       SPRTBV prints out timing information for the eigenvalue routines.   
       The table has NTYPES block rows and NSIZES columns, with NPARMS   
       individual rows in each block row.  There are INPARM quantities   
       which depend on rows (currently, INPARM <= 4).   

    Arguments (none are modified)   
    =========   

    SUBNAM - CHARACTER*(*)   
             The label for the output.   

    NTYPES - INTEGER   
             The number of values of DOTYPE, and also the   
             number of sets of rows of the table.   

    DOTYPE - LOGICAL array of dimension( NTYPES )   
             If DOTYPE(j) is .TRUE., then block row j (which includes   
             data from RESLTS( i, j, k ), for all i and k) will be   
             printed.  If DOTYPE(j) is .FALSE., then block row j will   
             not be printed.   

    NSIZES - INTEGER   
             The number of values of NN, and also the   
             number of columns of the table.   

    MM   -   INTEGER array of dimension( NSIZES )   
             The values of M used to label each column.   

    NN   -   INTEGER array of dimension( NSIZES )   
             The values of N used to label each column.   

    INPARM - INTEGER   
             The number of different parameters which are functions of   
             the row number.  At the moment, INPARM <= 4.   

    PNAMES - CHARACTER*(*) array of dimension( INPARM )   
             The label for the columns.   

    NPARMS - INTEGER   
             The number of values for each "parameter", i.e., the   
             number of rows for each value of DOTYPE.   

    NP1    - INTEGER array of dimension( NPARMS )   
             The first quantity which depends on row number.   

    NP2    - INTEGER array of dimension( NPARMS )   
             The second quantity which depends on row number.   

    OPS    - REAL array of dimension( LDT1, LDT2, NSIZES )   
             The operation counts.  The first index indicates the row,   
             the second index indicates the block row, and the last   
             indicates the column.   

    LDO1   - INTEGER   
             The first dimension of OPS.  It must be at least   
             min( 1, NPARMS ).   

    LDO2   - INTEGER   
             The second dimension of OPS.  It must be at least   
             min( 1, NTYPES ).   

    TIMES  - REAL array of dimension( LDT1, LDT2, NSIZES )   
             The times (in seconds).  The first index indicates the row,   
             the second index indicates the block row, and the last   
             indicates the column.   

    LDT1   - INTEGER   
             The first dimension of RESLTS.  It must be at least   
             min( 1, NPARMS ).   

    LDT2   - INTEGER   
             The second dimension of RESLTS.  It must be at least   
             min( 1, NTYPES ).   

    RWORK  - REAL array of dimension( NSIZES*NTYPES*NPARMS )   
             Real workspace.   
             Modified.   

    LLWORK - LOGICAL array of dimension( NPARMS )   
             Logical workspace.  It is used to turn on or off specific   
             lines in the output.  If LLWORK(i) is .TRUE., then row i   
             (which includes data from OPS(i,j,k) or TIMES(i,j,k) for   
             all j and k) will be printed.  If LLWORK(i) is   
             .FALSE., then row i will not be printed.   
             Modified.   

    NOUT   - INTEGER   
             The output unit number on which the table   
             is to be printed.  If NOUT <= 0, no output is printed.   

    =====================================================================   



       First line   

       Parameter adjustments */
    --dotype;
    --nn;
    --mm;
    pnames -= pnames_len;
    --llwork;
    --np1;
    --np2;
    ops_dim1 = *ldo1;
    ops_dim2 = *ldo2;
    ops_offset = 1 + ops_dim1 * (1 + ops_dim2 * 1);
    ops -= ops_offset;
    times_dim1 = *ldt1;
    times_dim2 = *ldt2;
    times_offset = 1 + times_dim1 * (1 + times_dim2 * 1);
    times -= times_offset;
    --rwork;

    /* Function Body */
    io___1.ciunit = *nout;
    s_wsfe(&io___1);
    do_fio(&c__1, subnam, subnam_len);
    e_wsfe();

/*     Set up which lines are to be printed. */

    llwork[1] = TRUE_;
    ilines = 1;
    i__1 = *nparms;
    for (ipar = 2; ipar <= i__1; ++ipar) {
	llwork[ipar] = TRUE_;
	i__2 = ipar - 1;
	for (j = 1; j <= i__2; ++j) {
	    ltemp = FALSE_;
	    if (*inparm >= 1 && np1[j] != np1[ipar]) {
		ltemp = TRUE_;
	    }
	    if (*inparm >= 2 && np2[j] != np2[ipar]) {
		ltemp = TRUE_;
	    }
	    if (! ltemp) {
		llwork[ipar] = FALSE_;
	    }
/* L10: */
	}
	if (llwork[ipar]) {
	    ++ilines;
	}
/* L20: */
    }
    if (ilines == 1) {
	if (*inparm == 1) {
	    io___6.ciunit = *nout;
	    s_wsfe(&io___6);
	    do_fio(&c__1, pnames_ref(0, 1), pnames_len);
	    do_fio(&c__1, (char *)&np1[1], (ftnlen)sizeof(integer));
	    e_wsfe();
	} else if (*inparm == 2) {
	    io___7.ciunit = *nout;
	    s_wsfe(&io___7);
	    do_fio(&c__1, pnames_ref(0, 1), pnames_len);
	    do_fio(&c__1, (char *)&np1[1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, pnames_ref(0, 2), pnames_len);
	    do_fio(&c__1, (char *)&np2[1], (ftnlen)sizeof(integer));
	    e_wsfe();
	}
    } else {
	iline = 0;
	i__1 = *nparms;
	for (j = 1; j <= i__1; ++j) {
	    if (llwork[j]) {
		++iline;
		if (*inparm == 1) {
		    io___9.ciunit = *nout;
		    s_wsfe(&io___9);
		    do_fio(&c__1, (char *)&iline, (ftnlen)sizeof(integer));
		    do_fio(&c__1, pnames_ref(0, 1), pnames_len);
		    do_fio(&c__1, (char *)&np1[j], (ftnlen)sizeof(integer));
		    e_wsfe();
		} else if (*inparm == 2) {
		    io___10.ciunit = *nout;
		    s_wsfe(&io___10);
		    do_fio(&c__1, (char *)&iline, (ftnlen)sizeof(integer));
		    do_fio(&c__1, pnames_ref(0, 1), pnames_len);
		    do_fio(&c__1, (char *)&np1[j], (ftnlen)sizeof(integer));
		    do_fio(&c__1, pnames_ref(0, 2), pnames_len);
		    do_fio(&c__1, (char *)&np2[j], (ftnlen)sizeof(integer));
		    e_wsfe();
		}
	    }
/* L30: */
	}
    }

/*     Execution Times */

    io___11.ciunit = *nout;
    s_wsfe(&io___11);
    e_wsfe();
    sprtbr_("Type", "M,N ", ntypes, &dotype[1], nsizes, &mm[1], &nn[1], 
	    nparms, &llwork[1], &times[times_offset], ldt1, ldt2, nout, (
	    ftnlen)4, (ftnlen)4);

/*     Operation Counts */

    io___12.ciunit = *nout;
    s_wsfe(&io___12);
    e_wsfe();
    sprtbr_("Type", "M,N ", ntypes, &dotype[1], nsizes, &mm[1], &nn[1], 
	    nparms, &llwork[1], &ops[ops_offset], ldo1, ldo2, nout, (ftnlen)4,
	     (ftnlen)4);

/*     Megaflop Rates */

    iinfo = 0;
    i__1 = *nsizes;
    for (js = 1; js <= i__1; ++js) {
	i__2 = *ntypes;
	for (jt = 1; jt <= i__2; ++jt) {
	    if (dotype[jt]) {
		i__3 = *nparms;
		for (jp = 1; jp <= i__3; ++jp) {
		    i__ = jp + *nparms * (jt - 1 + *ntypes * (js - 1));
		    rwork[i__] = smflop_(&ops_ref(jp, jt, js), &times_ref(jp, 
			    jt, js), &iinfo);
/* L40: */
		}
	    }
/* L50: */
	}
/* L60: */
    }

    io___18.ciunit = *nout;
    s_wsfe(&io___18);
    e_wsfe();
    sprtbr_("Type", "M,N ", ntypes, &dotype[1], nsizes, &mm[1], &nn[1], 
	    nparms, &llwork[1], &rwork[1], nparms, ntypes, nout, (ftnlen)4, (
	    ftnlen)4);

    return 0;

/*     End of SPRTBV */

} /* sprtbv_ */

#undef ops_ref
#undef pnames_ref
#undef times_ref


