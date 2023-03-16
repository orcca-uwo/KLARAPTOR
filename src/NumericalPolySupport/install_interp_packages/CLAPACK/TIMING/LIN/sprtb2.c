#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* Subroutine */ int sprtb2_(char *lab1, char *lab2, char *lab3, integer *nn, 
	integer *nval, integer *nlda, real *reslts, integer *ldr1, integer *
	ldr2, integer *nout, ftnlen lab1_len, ftnlen lab2_len, ftnlen 
	lab3_len)
{
    /* Format strings */
    static char fmt_9999[] = "(6x,a4,i6,11i8)";
    static char fmt_9998[] = "(3x,a4)";
    static char fmt_9997[] = "(1x,a6,1x,12f8.1)";
    static char fmt_9996[] = "(8x,12f8.1)";

    /* System generated locals */
    integer reslts_dim1, reslts_dim2, reslts_offset, i__1, i__2, i__3;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void),
	     i_len(char *, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsle(cilist *), e_wsle(void);

    /* Local variables */
    static integer i__, j, k, ic;
    static char collab[6];
    static integer inb, lnb;

    /* Fortran I/O blocks */
    static cilist io___1 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___3 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___9 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___11 = { 0, 0, 0, fmt_9996, 0 };
    static cilist io___12 = { 0, 0, 0, 0, 0 };
    static cilist io___13 = { 0, 0, 0, 0, 0 };



#define reslts_ref(a_1,a_2,a_3) reslts[((a_3)*reslts_dim2 + (a_2))*\
reslts_dim1 + a_1]


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       March 31, 1993   


    Purpose   
    =======   

    SPRTB2 prints a table of timing data for the solve routines.   
    There are 4 rows to each table, corresponding to   
    NRHS = 1, 2, N/2, and N,  or  NRHS = 1, 2, K/2, K for the   
    band routines.   

    Arguments   
    =========   

    LAB1    (input) CHARACTER*(*)   
            The label for the rows.   

    LAB2    (input) CHARACTER*(*)   
            The label for the columns.   

    LAB3    CHARACTER*(*)   
            The name of the variable used in the row headers (usually   
            N or K).   

    NN      (input) INTEGER   
            The number of values of NVAL, and also the number of columns   
            of the table.   

    NVAL    (input) INTEGER array, dimension (NN)   
            The values of LAB2 used for the data in each column.   

    NLDA    (input) INTEGER   
            The number of values of LDA, hence the number of rows for   
            each value of NRHS.   

    RESLTS  (input) REAL array, dimension (LDR1, LDR2, NLDA)   
            The timing results for each value of N, K, and LDA.   

    LDR1    (input) INTEGER   
            The first dimension of RESLTS.  LDR1 >= 4.   

    LDR2    (input) INTEGER   
            The second dimension of RESLTS.  LDR2 >= max( 1, NN ).   

    NOUT    (input) INTEGER   
            The unit number on which the table is to be printed.   
            NOUT >= 0.   

    =====================================================================   


       Parameter adjustments */
    --nval;
    reslts_dim1 = *ldr1;
    reslts_dim2 = *ldr2;
    reslts_offset = 1 + reslts_dim1 * (1 + reslts_dim2 * 1);
    reslts -= reslts_offset;

    /* Function Body */
    if (*nout <= 0) {
	return 0;
    }
    io___1.ciunit = *nout;
    s_wsfe(&io___1);
    do_fio(&c__1, lab2, lab2_len);
    i__1 = *nn;
    for (i__ = 1; i__ <= i__1; ++i__) {
	do_fio(&c__1, (char *)&nval[i__], (ftnlen)sizeof(integer));
    }
    e_wsfe();
    io___3.ciunit = *nout;
    s_wsfe(&io___3);
    do_fio(&c__1, lab1, lab1_len);
    e_wsfe();

/*     Find the first and last non-blank characters in LAB3. */

    inb = 0;
    i__1 = i_len(lab3, lab3_len);
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (inb == 0 && *(unsigned char *)&lab3[i__ - 1] != ' ') {
	    inb = i__;
	}
	if (*(unsigned char *)&lab3[i__ - 1] != ' ') {
	    lnb = i__;
	}
/* L10: */
    }
    if (inb == 0) {
	inb = 1;
	lnb = 1;
    }

    for (i__ = 1; i__ <= 4; ++i__) {
	if (i__ == 1) {
	    s_copy(collab, "     1", (ftnlen)6, (ftnlen)6);
	} else if (i__ == 2) {
	    s_copy(collab, "     2", (ftnlen)6, (ftnlen)6);
	} else if (i__ == 3) {
	    s_copy(collab, "    /2", (ftnlen)6, (ftnlen)6);
/* Computing MAX */
	    i__2 = inb, i__3 = lnb - 3;
	    i__1 = max(i__2,i__3);
	    for (j = lnb; j >= i__1; --j) {
		ic = 4 - (lnb - j);
		*(unsigned char *)&collab[ic - 1] = *(unsigned char *)&lab3[j 
			- 1];
/* L20: */
	    }
	} else if (i__ == 4) {
	    s_copy(collab, " ", (ftnlen)6, (ftnlen)1);
/* Computing MAX */
	    i__2 = inb, i__3 = lnb - 5;
	    i__1 = max(i__2,i__3);
	    for (j = lnb; j >= i__1; --j) {
		ic = 6 - (lnb - j);
		*(unsigned char *)&collab[ic - 1] = *(unsigned char *)&lab3[j 
			- 1];
/* L30: */
	    }
	}
	io___9.ciunit = *nout;
	s_wsfe(&io___9);
	do_fio(&c__1, collab, (ftnlen)6);
	i__1 = *nn;
	for (j = 1; j <= i__1; ++j) {
	    do_fio(&c__1, (char *)&reslts_ref(i__, j, 1), (ftnlen)sizeof(real)
		    );
	}
	e_wsfe();
	i__1 = *nlda;
	for (k = 2; k <= i__1; ++k) {
	    io___11.ciunit = *nout;
	    s_wsfe(&io___11);
	    i__2 = *nn;
	    for (j = 1; j <= i__2; ++j) {
		do_fio(&c__1, (char *)&reslts_ref(i__, j, k), (ftnlen)sizeof(
			real));
	    }
	    e_wsfe();
/* L40: */
	}
	if (*nlda > 1) {
	    io___12.ciunit = *nout;
	    s_wsle(&io___12);
	    e_wsle();
	}
/* L50: */
    }
    if (*nlda == 1) {
	io___13.ciunit = *nout;
	s_wsle(&io___13);
	e_wsle();
    }


    return 0;

/*     End of SPRTB2 */

} /* sprtb2_ */

#undef reslts_ref


