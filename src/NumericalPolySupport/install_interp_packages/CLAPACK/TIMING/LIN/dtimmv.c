#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__6 = 6;
static integer c__1 = 1;
static integer c__0 = 0;
static integer c__2 = 2;
static doublereal c_b44 = 1.;

/* Subroutine */ int dtimmv_(char *vname, integer *nn, integer *nval, integer 
	*nk, integer *kval, integer *nlda, integer *ldaval, doublereal *
	timmin, doublereal *a, integer *lb, doublereal *b, doublereal *c__, 
	doublereal *reslts, integer *ldr1, integer *ldr2, integer *nout, 
	ftnlen vname_len)
{
    /* Initialized data */

    static char subnam[6*2] = "DGEMV " "DGBMV ";

    /* Format strings */
    static char fmt_9999[] = "(1x,a6,\002:  Unrecognized path or subroutine "
	    "name\002,/)";
    static char fmt_9998[] = "(1x,a6,\002 timing run not attempted\002,/)";
    static char fmt_9997[] = "(/\002 *** Speed of \002,a6,\002 in megaflops "
	    "***\002)";
    static char fmt_9996[] = "(5x,\002with LDA = \002,i5)";
    static char fmt_9995[] = "(5x,\002line \002,i2,\002 with LDA = \002,i5)";

    /* System generated locals */
    integer reslts_dim1, reslts_dim2, reslts_offset, i__1, i__2, i__3, i__4, 
	    i__5, i__6, i__7;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void),
	     s_wsle(cilist *), e_wsle(void);

    /* Local variables */
    static integer ilda, info;
    static doublereal time;
    static integer isub, nrhs, i__, k, n;
    static char cname[6];
    extern /* Subroutine */ int dgbmv_(char *, integer *, integer *, integer *
	    , integer *, doublereal *, doublereal *, integer *, doublereal *, 
	    integer *, doublereal *, doublereal *, integer *);
    extern logical lsame_(char *, char *);
    extern /* Subroutine */ int dgemv_(char *, integer *, integer *, 
	    doublereal *, doublereal *, integer *, doublereal *, integer *, 
	    doublereal *, doublereal *, integer *);
    static doublereal s1, s2;
    extern doublereal dopbl2_(char *, integer *, integer *, integer *, 
	    integer *);
    static integer ib, ic, ik, in, kl, ku;
    extern doublereal dsecnd_(void);
    extern /* Subroutine */ int atimck_(integer *, char *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, ftnlen);
    extern logical lsamen_(integer *, char *, char *);
    extern doublereal dmflop_(doublereal *, doublereal *, integer *);
    extern /* Subroutine */ int dtimmg_(integer *, integer *, integer *, 
	    doublereal *, integer *, integer *, integer *), dprtbl_(char *, 
	    char *, integer *, integer *, integer *, integer *, integer *, 
	    doublereal *, integer *, integer *, integer *, ftnlen, ftnlen);
    static doublereal untime;
    static logical timsub[2];
    static integer lda, ldb, icl;
    static doublereal ops;
    static char lab1[1], lab2[1];

    /* Fortran I/O blocks */
    static cilist io___5 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___9 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___10 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___11 = { 0, 0, 0, fmt_9996, 0 };
    static cilist io___13 = { 0, 0, 0, fmt_9995, 0 };
    static cilist io___14 = { 0, 0, 0, 0, 0 };



#define subnam_ref(a_0,a_1) &subnam[(a_1)*6 + a_0 - 6]
#define reslts_ref(a_1,a_2,a_3) reslts[((a_3)*reslts_dim2 + (a_2))*\
reslts_dim1 + a_1]


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       March 31, 1993   


    Purpose   
    =======   

    DTIMMV times individual BLAS 2 routines.   

    Arguments   
    =========   

    VNAME   (input) CHARACTER*(*)   
            The name of the Level 2 BLAS routine to be timed.   

    NN      (input) INTEGER   
            The number of values of N contained in the vector NVAL.   

    NVAL    (input) INTEGER array, dimension (NN)   
            The values of the matrix dimension N.   

    NK      (input) INTEGER   
            The number of values of K contained in the vector KVAL.   

    KVAL    (input) INTEGER array, dimension (NK)   
            The values of the bandwidth K.   

    NLDA    (input) INTEGER   
            The number of values of LDA contained in the vector LDAVAL.   

    LDAVAL  (input) INTEGER array, dimension (NLDA)   
            The values of the leading dimension of the array A.   

    TIMMIN  (input) DOUBLE PRECISION   
            The minimum time a subroutine will be timed.   

    A       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)   
               where LDAMAX and NMAX are the maximum values permitted   
               for LDA and N.   

    LB      (input) INTEGER   
            The length of B and C, needed when timing DGBMV.  If timing   
            DGEMV, LB >= LDAMAX*NMAX.   

    B       (workspace) DOUBLE PRECISION array, dimension (LB)   

    C       (workspace) DOUBLE PRECISION array, dimension (LB)   

    RESLTS  (output) DOUBLE PRECISION array, dimension (LDR1,LDR2,NLDA)   
            The timing results for each subroutine over the relevant   
            values of N and LDA.   

    LDR1    (input) INTEGER   
            The first dimension of RESLTS.  LDR1 >= max(1,NK).   

    LDR2    (input) INTEGER   
            The second dimension of RESLTS.  LDR2 >= max(1,NN).   

    NOUT    (input) INTEGER   
            The unit number for output.   

    =====================================================================   

       Parameter adjustments */
    --nval;
    --kval;
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
    for (isub = 1; isub <= 2; ++isub) {
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
    goto L150;
L20:

/*     Check that N or K <= LDA for the input values. */

    if (lsame_(cname + 2, "B")) {
	atimck_(&c__0, cname, nk, &kval[1], nlda, &ldaval[1], nout, &info, (
		ftnlen)6);
	*(unsigned char *)lab1 = 'M';
	*(unsigned char *)lab2 = 'K';
    } else {
	atimck_(&c__2, cname, nn, &nval[1], nlda, &ldaval[1], nout, &info, (
		ftnlen)6);
	*(unsigned char *)lab1 = ' ';
	*(unsigned char *)lab2 = 'N';
    }
    if (info > 0) {
	io___9.ciunit = *nout;
	s_wsfe(&io___9);
	do_fio(&c__1, cname, (ftnlen)6);
	e_wsfe();
	goto L150;
    }

/*     Print the table header on unit NOUT. */

    io___10.ciunit = *nout;
    s_wsfe(&io___10);
    do_fio(&c__1, vname, vname_len);
    e_wsfe();
    if (*nlda == 1) {
	io___11.ciunit = *nout;
	s_wsfe(&io___11);
	do_fio(&c__1, (char *)&ldaval[1], (ftnlen)sizeof(integer));
	e_wsfe();
    } else {
	i__1 = *nlda;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    io___13.ciunit = *nout;
	    s_wsfe(&io___13);
	    do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&ldaval[i__], (ftnlen)sizeof(integer));
	    e_wsfe();
/* L30: */
	}
    }
    io___14.ciunit = *nout;
    s_wsle(&io___14);
    e_wsle();

/*     Time DGEMV */

    if (timsub[0]) {
	i__1 = *nlda;
	for (ilda = 1; ilda <= i__1; ++ilda) {
	    lda = ldaval[ilda];
	    i__2 = *nn;
	    for (in = 1; in <= i__2; ++in) {
		n = nval[in];
		nrhs = n;
		ldb = lda;
		dtimmg_(&c__1, &n, &n, &a[1], &lda, &c__0, &c__0);
		dtimmg_(&c__0, &n, &nrhs, &b[1], &ldb, &c__0, &c__0);
		dtimmg_(&c__1, &n, &nrhs, &c__[1], &ldb, &c__0, &c__0);
		ic = 0;
		s1 = dsecnd_();
L40:
		ib = 1;
		i__3 = nrhs;
		for (i__ = 1; i__ <= i__3; ++i__) {
		    dgemv_("No transpose", &n, &n, &c_b44, &a[1], &lda, &b[ib]
			    , &c__1, &c_b44, &c__[ib], &c__1);
		    ib += ldb;
/* L50: */
		}
		s2 = dsecnd_();
		time = s2 - s1;
		++ic;
		if (time < *timmin) {
		    dtimmg_(&c__1, &n, &nrhs, &c__[1], &ldb, &c__0, &c__0);
		    goto L40;
		}

/*              Subtract the time used in DTIMMG. */

		icl = 1;
		s1 = dsecnd_();
L60:
		s2 = dsecnd_();
		untime = s2 - s1;
		++icl;
		if (icl <= ic) {
		    dtimmg_(&c__1, &n, &nrhs, &c__[1], &ldb, &c__0, &c__0);
		    goto L60;
		}

		time = (time - untime) / (doublereal) ic;
		ops = nrhs * dopbl2_("DGEMV ", &n, &n, &c__0, &c__0);
		reslts_ref(1, in, ilda) = dmflop_(&ops, &time, &c__0);
/* L70: */
	    }
/* L80: */
	}

	dprtbl_(lab1, lab2, &c__1, &nval[1], nn, &nval[1], nlda, &reslts[
		reslts_offset], ldr1, ldr2, nout, (ftnlen)1, (ftnlen)1);

    } else if (timsub[1]) {

/*        Time DGBMV */

	i__1 = *nlda;
	for (ilda = 1; ilda <= i__1; ++ilda) {
	    lda = ldaval[ilda];
	    i__2 = *nn;
	    for (in = 1; in <= i__2; ++in) {
		n = nval[in];
		i__3 = *nk;
		for (ik = 1; ik <= i__3; ++ik) {
/* Computing MIN   
   Computing MAX */
		    i__6 = 0, i__7 = kval[ik];
		    i__4 = n - 1, i__5 = max(i__6,i__7);
		    k = min(i__4,i__5);
		    kl = k;
		    ku = k;
		    ldb = n;
		    dtimmg_(&c__2, &n, &n, &a[1], &lda, &kl, &ku);
/* Computing MIN */
		    i__4 = k, i__5 = *lb / ldb;
		    nrhs = min(i__4,i__5);
		    dtimmg_(&c__0, &n, &nrhs, &b[1], &ldb, &c__0, &c__0);
		    dtimmg_(&c__1, &n, &nrhs, &c__[1], &ldb, &c__0, &c__0);
		    ic = 0;
		    s1 = dsecnd_();
L90:
		    ib = 1;
		    i__4 = nrhs;
		    for (i__ = 1; i__ <= i__4; ++i__) {
			dgbmv_("No transpose", &n, &n, &kl, &ku, &c_b44, &a[
				ku + 1], &lda, &b[ib], &c__1, &c_b44, &c__[ib]
				, &c__1);
			ib += ldb;
/* L100: */
		    }
		    s2 = dsecnd_();
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			dtimmg_(&c__1, &n, &nrhs, &c__[1], &ldb, &c__0, &c__0)
				;
			goto L90;
		    }

/*                 Subtract the time used in DTIMMG. */

		    icl = 1;
		    s1 = dsecnd_();
L110:
		    s2 = dsecnd_();
		    untime = s2 - s1;
		    ++icl;
		    if (icl <= ic) {
			dtimmg_(&c__1, &n, &nrhs, &c__[1], &ldb, &c__0, &c__0)
				;
			goto L110;
		    }

		    time = (time - untime) / (doublereal) ic;
		    ops = nrhs * dopbl2_("DGBMV ", &n, &n, &kl, &ku);
		    reslts_ref(in, ik, ilda) = dmflop_(&ops, &time, &c__0);
/* L120: */
		}
/* L130: */
	    }
/* L140: */
	}

	dprtbl_(lab1, lab2, nn, &nval[1], nk, &kval[1], nlda, &reslts[
		reslts_offset], ldr1, ldr2, nout, (ftnlen)1, (ftnlen)1);
    }

L150:
    return 0;

/*     End of DTIMMV */

} /* dtimmv_ */

#undef reslts_ref
#undef subnam_ref


