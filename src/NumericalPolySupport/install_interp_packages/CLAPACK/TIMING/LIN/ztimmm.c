#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublecomplex c_b1 = {1.,0.};
static integer c__6 = 6;
static integer c__1 = 1;
static integer c__2 = 2;
static integer c__0 = 0;

/* Subroutine */ int ztimmm_(char *vname, char *lab2, integer *nn, integer *
	nval, integer *nlda, integer *ldaval, doublereal *timmin, 
	doublecomplex *a, doublecomplex *b, doublecomplex *c__, doublereal *
	reslts, integer *ldr1, integer *ldr2, integer *nout, ftnlen vname_len,
	 ftnlen lab2_len)
{
    /* Initialized data */

    static char subnam[6*1] = "ZGEMM ";

    /* Format strings */
    static char fmt_9999[] = "(1x,a6,\002:  Unrecognized path or subroutine "
	    "name\002,/)";
    static char fmt_9998[] = "(1x,a6,\002 timing run not attempted\002,/)";
    static char fmt_9997[] = "(/\002 *** Speed of \002,a6,\002 in megaflops "
	    "***\002)";
    static char fmt_9996[] = "(5x,\002with LDA = \002,i5)";
    static char fmt_9995[] = "(5x,\002line \002,i2,\002 with LDA = \002,i5)";

    /* System generated locals */
    integer reslts_dim1, reslts_dim2, reslts_offset, i__1, i__2;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void),
	     s_wsle(cilist *), e_wsle(void);

    /* Local variables */
    static integer ilda, info;
    static doublereal time;
    static integer isub, i__, n;
    static char cname[6];
    extern /* Subroutine */ int zgemm_(char *, char *, integer *, integer *, 
	    integer *, doublecomplex *, doublecomplex *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, doublecomplex *, 
	    integer *);
    static doublereal s1, s2;
    extern doublereal dopbl3_(char *, integer *, integer *, integer *)
	    ;
    static integer ic, in;
    extern doublereal dsecnd_(void);
    extern /* Subroutine */ int atimck_(integer *, char *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, ftnlen);
    extern logical lsamen_(integer *, char *, char *);
    extern doublereal dmflop_(doublereal *, doublereal *, integer *);
    extern /* Subroutine */ int dprtbl_(char *, char *, integer *, integer *, 
	    integer *, integer *, integer *, doublereal *, integer *, integer 
	    *, integer *, ftnlen, ftnlen);
    static doublereal untime;
    static logical timsub[1];
    static integer idummy[1];
    extern /* Subroutine */ int ztimmg_(integer *, integer *, integer *, 
	    doublecomplex *, integer *, integer *, integer *);
    static integer lda, icl;
    static doublereal ops;

    /* Fortran I/O blocks */
    static cilist io___5 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___7 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___19 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___20 = { 0, 0, 0, fmt_9996, 0 };
    static cilist io___22 = { 0, 0, 0, fmt_9995, 0 };
    static cilist io___23 = { 0, 0, 0, 0, 0 };



#define subnam_ref(a_0,a_1) &subnam[(a_1)*6 + a_0 - 6]
#define reslts_ref(a_1,a_2,a_3) reslts[((a_3)*reslts_dim2 + (a_2))*\
reslts_dim1 + a_1]


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    ZTIMMM times ZGEMM.   

    Arguments   
    =========   

    VNAME   (input) CHARACTER*(*)   
            The name of the Level 3 BLAS routine to be timed.   

    LAB2    (input) CHARACTER*(*)   
            The name of the variable given in NVAL.   

    NN      (input) INTEGER   
            The number of values of N contained in the vector NVAL.   

    NVAL    (input) INTEGER array, dimension (NN)   
            The values of the matrix dimension N.   

    NLDA    (input) INTEGER   
            The number of values of LDA contained in the vector LDAVAL.   

    LDAVAL  (input) INTEGER array, dimension (NLDA)   
            The values of the leading dimension of the array A.   

    TIMMIN  (input) DOUBLE PRECISION   
            The minimum time a subroutine will be timed.   

    A       (workspace) COMPLEX*16 array, dimension (LDAMAX*NMAX)   
               where LDAMAX and NMAX are the maximum values permitted   
               for LDA and N.   

    B       (workspace) COMPLEX*16 array, dimension (LDAMAX*NMAX)   

    C       (workspace) COMPLEX*16 array, dimension (LDAMAX*NMAX)   

    RESLTS  (output) DOUBLE PRECISION array, dimension (LDR1,LDR2,NLDA)   
            The timing results for each subroutine over the relevant   
            values of N and LDA.   

    LDR1    (input) INTEGER   
            The first dimension of RESLTS.  LDR1 >= 1.   

    LDR2    (input) INTEGER   
            The second dimension of RESLTS.  LDR2 >= max(1,NN).   

    NOUT    (input) INTEGER   
            The unit number for output.   

    =====================================================================   

       Parameter adjustments */
    --nval;
    --ldaval;
    --a;
    --b;
    --c__;
    reslts_dim1 = *ldr1;
    reslts_dim2 = *ldr2;
    reslts_offset = 1 + reslts_dim1 * (1 + reslts_dim2 * 1);
    reslts -= reslts_offset;

    /* Function Body */

    s_copy(cname, vname, (ftnlen)6, vname_len);
    for (isub = 1; isub <= 1; ++isub) {
	timsub[isub - 1] = lsamen_(&c__6, cname, subnam_ref(0, isub));
	if (timsub[isub - 1]) {
	    goto L20;
	}
/* L10: */
    }
    io___5.ciunit = *nout;
    s_wsfe(&io___5);
    do_fio(&c__1, cname, (ftnlen)6);
    e_wsfe();
    goto L80;
L20:

/*     Check that N <= LDA for the input values. */

    atimck_(&c__2, cname, nn, &nval[1], nlda, &ldaval[1], nout, &info, (
	    ftnlen)6);
    if (info > 0) {
	io___7.ciunit = *nout;
	s_wsfe(&io___7);
	do_fio(&c__1, cname, (ftnlen)6);
	e_wsfe();
	goto L80;
    }

    i__1 = *nlda;
    for (ilda = 1; ilda <= i__1; ++ilda) {
	lda = ldaval[ilda];
	i__2 = *nn;
	for (in = 1; in <= i__2; ++in) {
	    n = nval[in];

/*           Time ZGEMM */

	    ztimmg_(&c__1, &n, &n, &a[1], &lda, &c__0, &c__0);
	    ztimmg_(&c__0, &n, &n, &b[1], &lda, &c__0, &c__0);
	    ztimmg_(&c__1, &n, &n, &c__[1], &lda, &c__0, &c__0);
	    ic = 0;
	    s1 = dsecnd_();
L30:
	    zgemm_("No transpose", "No transpose", &n, &n, &n, &c_b1, &a[1], &
		    lda, &b[1], &lda, &c_b1, &c__[1], &lda);
	    s2 = dsecnd_();
	    time = s2 - s1;
	    ++ic;
	    if (time < *timmin) {
		ztimmg_(&c__1, &n, &n, &c__[1], &lda, &c__0, &c__0);
		goto L30;
	    }

/*           Subtract the time used in ZTIMMG. */

	    icl = 1;
	    s1 = dsecnd_();
L40:
	    s2 = dsecnd_();
	    untime = s2 - s1;
	    ++icl;
	    if (icl <= ic) {
		ztimmg_(&c__1, &n, &n, &c__[1], &lda, &c__0, &c__0);
		goto L40;
	    }

	    time = (time - untime) / (doublereal) ic;
	    ops = dopbl3_("ZGEMM ", &n, &n, &n);
	    reslts_ref(1, in, ilda) = dmflop_(&ops, &time, &c__0);
/* L50: */
	}
/* L60: */
    }

/*     Print the table of results on unit NOUT. */

    io___19.ciunit = *nout;
    s_wsfe(&io___19);
    do_fio(&c__1, vname, vname_len);
    e_wsfe();
    if (*nlda == 1) {
	io___20.ciunit = *nout;
	s_wsfe(&io___20);
	do_fio(&c__1, (char *)&ldaval[1], (ftnlen)sizeof(integer));
	e_wsfe();
    } else {
	i__1 = *nlda;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    io___22.ciunit = *nout;
	    s_wsfe(&io___22);
	    do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&ldaval[i__], (ftnlen)sizeof(integer));
	    e_wsfe();
/* L70: */
	}
    }
    io___23.ciunit = *nout;
    s_wsle(&io___23);
    e_wsle();
    dprtbl_(" ", lab2, &c__1, idummy, nn, &nval[1], nlda, &reslts[
	    reslts_offset], ldr1, ldr2, nout, (ftnlen)1, lab2_len);

L80:
    return 0;

/*     End of ZTIMMM */

} /* ztimmm_ */

#undef reslts_ref
#undef subnam_ref


