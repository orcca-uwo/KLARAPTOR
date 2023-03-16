#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* Subroutine */ int dprtb5_(char *lab1, char *labm, char *labn, integer *nk, 
	integer *kval, integer *nm, integer *mval, integer *nval, integer *
	nlda, doublereal *reslts, integer *ldr1, integer *ldr2, integer *nout,
	 ftnlen lab1_len, ftnlen labm_len, ftnlen labn_len)
{
    /* Format strings */
    static char fmt_9999[] = "(6x,a4,i6,11i8)";
    static char fmt_9998[] = "(3x,a4)";
    static char fmt_9996[] = "(8x,12f8.1)";
    static char fmt_9997[] = "(1x,i6,1x,12f8.1)";

    /* System generated locals */
    integer reslts_dim1, reslts_dim2, reslts_offset, i__1, i__2, i__3;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void),
	     s_cmp(char *, char *, ftnlen, ftnlen), s_wsle(cilist *), e_wsle(
	    void);

    /* Local variables */
    static integer i__, j, k;

    /* Fortran I/O blocks */
    static cilist io___1 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___3 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___4 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___5 = { 0, 0, 0, fmt_9996, 0 };
    static cilist io___7 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___9 = { 0, 0, 0, fmt_9996, 0 };
    static cilist io___10 = { 0, 0, 0, 0, 0 };
    static cilist io___11 = { 0, 0, 0, 0, 0 };



#define reslts_ref(a_1,a_2,a_3) reslts[((a_3)*reslts_dim2 + (a_2))*\
reslts_dim1 + a_1]


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       March 31, 1993   


    Purpose   
    =======   

    DPRTB5 prints a table of timing data for the timing programs.   
    The table has NK block rows and NM columns, with NLDA   
    individual rows in each block row.  Each column depends on two   
    parameters M and N, specified as an ordered pair in the arrays MVAL   
    and NVAL.   

    Arguments   
    =========   

    LAB1    (input) CHARACTER*(*)   
            The label for the rows.   

    LABM    (input) CHARACTER*(*)   
            The first label for the columns.   

    LABN    (input) CHARACTER*(*)   
            The second label for the columns.   

    NK      (input) INTEGER   
            The number of values of KVAL, and also the number of block   
            rows of the table.   

    KVAL    (input) INTEGER array, dimension (NK)   
            The values of LAB1 used for the data in each block row.   

    NM      (input) INTEGER   
            The number of values of MVAL and NVAL, and also the number of   
            columns of the table.  Each column depends on the pair of   
            parameters (M,N).   

    MVAL    (input) INTEGER array, dimension (NM)   
            The values of the parameter M.   

    NVAL    (input) INTEGER array, dimension (NM)   
            The values of the parameter N.   

    NLDA    (input) INTEGER   
            The number of values of LDA, hence the number of rows for   
            each value of KVAL.   

    RESLTS  (input) DOUBLE PRECISION array, dimension (LDR1, LDR2, NLDA)   
            The timing results for each value of N, K, and LDA.   

    LDR1    (input) INTEGER   
            The first dimension of RESLTS.  LDR1 >= max(1,NK).   

    LDR2    (input) INTEGER   
            The second dimension of RESLTS.  LDR2 >= max(1,NM).   

    NOUT    (input) INTEGER   
            The unit number on which the table is to be printed.   
            NOUT >= 0.   

    =====================================================================   


       Parameter adjustments */
    --kval;
    --nval;
    --mval;
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
    do_fio(&c__1, labm, labm_len);
    i__1 = *nm;
    for (i__ = 1; i__ <= i__1; ++i__) {
	do_fio(&c__1, (char *)&mval[i__], (ftnlen)sizeof(integer));
    }
    e_wsfe();
    io___3.ciunit = *nout;
    s_wsfe(&io___3);
    do_fio(&c__1, labn, labn_len);
    i__1 = *nm;
    for (i__ = 1; i__ <= i__1; ++i__) {
	do_fio(&c__1, (char *)&nval[i__], (ftnlen)sizeof(integer));
    }
    e_wsfe();
    io___4.ciunit = *nout;
    s_wsfe(&io___4);
    do_fio(&c__1, lab1, lab1_len);
    e_wsfe();

    i__1 = *nk;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (s_cmp(lab1, " ", lab1_len, (ftnlen)1) == 0) {
	    io___5.ciunit = *nout;
	    s_wsfe(&io___5);
	    i__2 = *nm;
	    for (j = 1; j <= i__2; ++j) {
		do_fio(&c__1, (char *)&reslts_ref(1, j, 1), (ftnlen)sizeof(
			doublereal));
	    }
	    e_wsfe();
	} else {
	    io___7.ciunit = *nout;
	    s_wsfe(&io___7);
	    do_fio(&c__1, (char *)&kval[i__], (ftnlen)sizeof(integer));
	    i__2 = *nm;
	    for (j = 1; j <= i__2; ++j) {
		do_fio(&c__1, (char *)&reslts_ref(i__, j, 1), (ftnlen)sizeof(
			doublereal));
	    }
	    e_wsfe();
	}
	i__2 = *nlda;
	for (k = 2; k <= i__2; ++k) {
	    io___9.ciunit = *nout;
	    s_wsfe(&io___9);
	    i__3 = *nm;
	    for (j = 1; j <= i__3; ++j) {
		do_fio(&c__1, (char *)&reslts_ref(i__, j, k), (ftnlen)sizeof(
			doublereal));
	    }
	    e_wsfe();
/* L10: */
	}
	if (*nlda > 1) {
	    io___10.ciunit = *nout;
	    s_wsle(&io___10);
	    e_wsle();
	}
/* L20: */
    }
    if (*nlda == 1) {
	io___11.ciunit = *nout;
	s_wsle(&io___11);
	e_wsle();
    }
    return 0;


/*     End of DPRTB5 */

} /* dprtb5_ */

#undef reslts_ref


