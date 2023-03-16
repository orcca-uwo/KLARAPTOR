#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* Subroutine */ int dprtb3_(char *lab1, char *lab2, integer *nk, integer *
	kval, integer *lval, integer *nn, integer *nval, integer *nlda, 
	doublereal *reslts, integer *ldr1, integer *ldr2, integer *nout, 
	ftnlen lab1_len, ftnlen lab2_len)
{
    /* Format strings */
    static char fmt_9999[] = "(10x,a4,i7,11i8)";
    static char fmt_9998[] = "(1x,a11)";
    static char fmt_9996[] = "(13x,12f8.1)";
    static char fmt_9997[] = "(1x,\002(\002,i4,\002,\002,i4,\002) \002,12f8."
	    "1)";

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
    static cilist io___3 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___4 = { 0, 0, 0, fmt_9996, 0 };
    static cilist io___6 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___8 = { 0, 0, 0, fmt_9996, 0 };
    static cilist io___9 = { 0, 0, 0, 0, 0 };
    static cilist io___10 = { 0, 0, 0, 0, 0 };



#define reslts_ref(a_1,a_2,a_3) reslts[((a_3)*reslts_dim2 + (a_2))*\
reslts_dim1 + a_1]


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       March 31, 1993   


    Purpose   
    =======   

    DPRTB3 prints a table of timing data for the timing programs.   
    The table has NK block rows and NN columns, with NLDA   
    individual rows in each block row.  Each block row depends on two   
    parameters K and L, specified as an ordered pair in the arrays KVAL   
    and LVAL.   

    Arguments   
    =========   

    LAB1    (input) CHARACTER*(*)   
            The label for the rows.   

    LAB2    (input) CHARACTER*(*)   
            The label for the columns.   

    NK      (input) INTEGER   
            The number of values of KVAL, and also the number of block   
            rows of the table.   

    KVAL    (input) INTEGER array, dimension (NK)   
            The values of the parameter K.  Each block row depends on   
            the pair of parameters (K, L).   

    LVAL    (input) INTEGER array, dimension (NK)   
            The values of the parameter L.  Each block row depends on   
            the pair of parameters (K, L).   

    NN      (input) INTEGER   
            The number of values of NVAL, and also the number of columns   
            of the table.   

    NVAL    (input) INTEGER array, dimension (NN)   
            The values of N used for the data in each column.   

    NLDA    (input) INTEGER   
            The number of values of LDA, hence the number of rows for   
            each value of KVAL.   

    RESLTS  (input) DOUBLE PRECISION array, dimension (LDR1, LDR2, NLDA)   
            The timing results for each value of N, K, and LDA.   

    LDR1    (input) INTEGER   
            The first dimension of RESLTS.  LDR1 >= max(1,NK).   

    LDR2    (input) INTEGER   
            The second dimension of RESLTS.  LDR2 >= max(1,NN).   

    NOUT    (input) INTEGER   
            The unit number on which the table is to be printed.   
            NOUT >= 0.   

    =====================================================================   


       Parameter adjustments */
    --lval;
    --kval;
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

    i__1 = *nk;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (s_cmp(lab1, " ", lab1_len, (ftnlen)1) == 0) {
	    io___4.ciunit = *nout;
	    s_wsfe(&io___4);
	    i__2 = *nn;
	    for (j = 1; j <= i__2; ++j) {
		do_fio(&c__1, (char *)&reslts_ref(1, j, 1), (ftnlen)sizeof(
			doublereal));
	    }
	    e_wsfe();
	} else {
	    io___6.ciunit = *nout;
	    s_wsfe(&io___6);
	    do_fio(&c__1, (char *)&kval[i__], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&lval[i__], (ftnlen)sizeof(integer));
	    i__2 = *nn;
	    for (j = 1; j <= i__2; ++j) {
		do_fio(&c__1, (char *)&reslts_ref(i__, j, 1), (ftnlen)sizeof(
			doublereal));
	    }
	    e_wsfe();
	}
	i__2 = *nlda;
	for (k = 2; k <= i__2; ++k) {
	    io___8.ciunit = *nout;
	    s_wsfe(&io___8);
	    i__3 = *nn;
	    for (j = 1; j <= i__3; ++j) {
		do_fio(&c__1, (char *)&reslts_ref(i__, j, k), (ftnlen)sizeof(
			doublereal));
	    }
	    e_wsfe();
/* L10: */
	}
	if (*nlda > 1) {
	    io___9.ciunit = *nout;
	    s_wsle(&io___9);
	    e_wsle();
	}
/* L20: */
    }
    if (*nlda == 1) {
	io___10.ciunit = *nout;
	s_wsle(&io___10);
	e_wsle();
    }
    return 0;


/*     End of DPRTB3 */

} /* dprtb3_ */

#undef reslts_ref


