#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* Subroutine */ int sprtbr_(char *lab1, char *lab2, integer *ntypes, logical 
	*dotype, integer *nsizes, integer *mm, integer *nn, integer *nparms, 
	logical *doline, real *reslts, integer *ldr1, integer *ldr2, integer *
	nout, ftnlen lab1_len, ftnlen lab2_len)
{
    /* Format strings */
    static char fmt_9999[] = "(7x,a4,(12(\002(\002,i4,\002,\002,i4,\002)\002"
	    ",:)))";
    static char fmt_9998[] = "(3x,a4)";
    static char fmt_9997[] = "(3x,i4,4x,1p,(12(3x,g8.2)))";
    static char fmt_9996[] = "(11x,1p,(12(3x,g8.2)))";

    /* System generated locals */
    integer reslts_dim1, reslts_dim2, reslts_offset, i__1, i__2, i__3;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void),
	     s_wsle(cilist *), e_wsle(void);

    /* Local variables */
    static integer i__, j, k, iline;

    /* Fortran I/O blocks */
    static cilist io___1 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___3 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___6 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___8 = { 0, 0, 0, fmt_9996, 0 };
    static cilist io___9 = { 0, 0, 0, 0, 0 };



#define reslts_ref(a_1,a_2,a_3) reslts[((a_3)*reslts_dim2 + (a_2))*\
reslts_dim1 + a_1]


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 29, 1992   


    Purpose   
    =======   

       SPRTBR prints a table of timing data for the timing programs.   
       The table has NTYPES block rows and NSIZES columns, with NPARMS   
       individual rows in each block row.   

    Arguments (none are modified)   
    =========   

    LAB1   - CHARACTER*(*)   
             The label for the rows.   

    LAB2   - CHARACTER*(*)   
             The label for the columns.   

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

    NPARMS - INTEGER   
             The number of values of LDA, hence the   
             number of rows for each value of DOTYPE.   

    DOLINE - LOGICAL array of dimension( NPARMS )   
             If DOLINE(i) is .TRUE., then row i (which includes data   
             from RESLTS( i, j, k ) for all j and k) will be printed.   
             If DOLINE(i) is .FALSE., then row i will not be printed.   

    RESLTS - REAL array of dimension( LDR1, LDR2, NSIZES )   
             The timing results.  The first index indicates the row,   
             the second index indicates the block row, and the last   
             indicates the column.   

    LDR1   - INTEGER   
             The first dimension of RESLTS.  It must be at least   
             min( 1, NPARMS ).   

    LDR2   - INTEGER   
             The second dimension of RESLTS.  It must be at least   
             min( 1, NTYPES ).   

    NOUT   - INTEGER   
             The output unit number on which the table   
             is to be printed.  If NOUT <= 0, no output is printed.   

    =====================================================================   


       Parameter adjustments */
    --dotype;
    --nn;
    --mm;
    --doline;
    reslts_dim1 = *ldr1;
    reslts_dim2 = *ldr2;
    reslts_offset = 1 + reslts_dim1 * (1 + reslts_dim2 * 1);
    reslts -= reslts_offset;

    /* Function Body */
    if (*nout <= 0) {
	return 0;
    }
    if (*nparms <= 0) {
	return 0;
    }
    io___1.ciunit = *nout;
    s_wsfe(&io___1);
    do_fio(&c__1, lab2, lab2_len);
    i__1 = *nsizes;
    for (i__ = 1; i__ <= i__1; ++i__) {
	do_fio(&c__1, (char *)&mm[i__], (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&nn[i__], (ftnlen)sizeof(integer));
    }
    e_wsfe();
    io___3.ciunit = *nout;
    s_wsfe(&io___3);
    do_fio(&c__1, lab1, lab1_len);
    e_wsfe();

    i__1 = *ntypes;
    for (j = 1; j <= i__1; ++j) {
	iline = 0;
	if (dotype[j]) {
	    i__2 = *nparms;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		if (doline[i__]) {
		    ++iline;
		    if (iline <= 1) {
			io___6.ciunit = *nout;
			s_wsfe(&io___6);
			do_fio(&c__1, (char *)&j, (ftnlen)sizeof(integer));
			i__3 = *nsizes;
			for (k = 1; k <= i__3; ++k) {
			    do_fio(&c__1, (char *)&reslts_ref(i__, j, k), (
				    ftnlen)sizeof(real));
			}
			e_wsfe();
		    } else {
			io___8.ciunit = *nout;
			s_wsfe(&io___8);
			i__3 = *nsizes;
			for (k = 1; k <= i__3; ++k) {
			    do_fio(&c__1, (char *)&reslts_ref(i__, j, k), (
				    ftnlen)sizeof(real));
			}
			e_wsfe();
		    }
		}
/* L10: */
	    }
	    if (iline > 1 && j < *ntypes) {
		io___9.ciunit = *nout;
		s_wsle(&io___9);
		e_wsle();
	    }
	}
/* L20: */
    }
    return 0;


/*     End of SPRTBR */

} /* sprtbr_ */

#undef reslts_ref


