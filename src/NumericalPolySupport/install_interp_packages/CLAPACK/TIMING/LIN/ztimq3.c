#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__3 = 3;
static integer c__9 = 9;
static integer c__0 = 0;

/* Subroutine */ int ztimq3_(char *line, integer *nm, integer *mval, integer *
	nval, integer *nnb, integer *nbval, integer *nxval, integer *nlda, 
	integer *ldaval, doublereal *timmin, doublecomplex *a, doublecomplex *
	copya, doublecomplex *tau, doublecomplex *work, doublereal *rwork, 
	integer *iwork, doublereal *reslts, integer *ldr1, integer *ldr2, 
	integer *nout, ftnlen line_len)
{
    /* Initialized data */

    static char subnam[6*1] = "ZGEQP3";
    static integer modes[2] = { 2,3 };
    static integer iseed[4] = { 0,0,0,1 };

    /* Format strings */
    static char fmt_9996[] = "(1x,a6,\002 timing run not attempted\002,/)";
    static char fmt_9999[] = "(/\002 *** Speed of \002,a6,\002 in megaflops "
	    "***\002)";
    static char fmt_9998[] = "(5x,\002type of matrix:\002,i4)";
    static char fmt_9997[] = "(5x,\002line \002,i4,\002 with LDA = \002,i4)";

    /* System generated locals */
    integer reslts_dim1, reslts_dim2, reslts_offset, i__1, i__2, i__3, i__4, 
	    i__5;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void),
	     s_wsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_wsle(void);

    /* Local variables */
    static integer ilda;
    static doublereal cond;
    static integer mode;
    static doublereal dmax__;
    static integer info;
    static char path[3];
    static doublereal time;
    static integer i__, m, n;
    static char cname[6];
    static integer imode;
    extern doublereal dopla_(char *, integer *, integer *, integer *, integer 
	    *, integer *);
    static integer minmn;
    extern /* Subroutine */ int icopy_(integer *, integer *, integer *, 
	    integer *, integer *);
    static doublereal s1, s2;
    extern /* Subroutine */ int dprtb4_(char *, char *, char *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    doublereal *, integer *, integer *, integer *, ftnlen, ftnlen, 
	    ftnlen), zgeqp3_(integer *, integer *, doublecomplex *, integer *,
	     integer *, doublecomplex *, doublecomplex *, integer *, 
	    doublereal *, integer *);
    static integer ic, nb, im;
    extern doublereal dlamch_(char *), dsecnd_(void);
    static integer lw, nx;
    extern /* Subroutine */ int atimck_(integer *, char *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, ftnlen);
    extern doublereal dmflop_(doublereal *, doublereal *, integer *);
    extern /* Subroutine */ int atimin_(char *, char *, integer *, char *, 
	    logical *, integer *, integer *, ftnlen, ftnlen, ftnlen), xlaenv_(
	    integer *, integer *);
    static doublereal untime;
    extern /* Subroutine */ int zlacpy_(char *, integer *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *);
    static logical timsub[1];
    extern /* Subroutine */ int zlatms_(integer *, integer *, char *, integer 
	    *, char *, doublereal *, integer *, doublereal *, doublereal *, 
	    integer *, integer *, char *, doublecomplex *, integer *, 
	    doublecomplex *, integer *);
    static integer lda, icl, inb;
    static doublereal ops;

    /* Fortran I/O blocks */
    static cilist io___8 = { 0, 0, 0, fmt_9996, 0 };
    static cilist io___26 = { 0, 6, 0, 0, 0 };
    static cilist io___32 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___33 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___34 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___35 = { 0, 0, 0, 0, 0 };



#define subnam_ref(a_0,a_1) &subnam[(a_1)*6 + a_0 - 6]
#define reslts_ref(a_1,a_2,a_3) reslts[((a_3)*reslts_dim2 + (a_2))*\
reslts_dim1 + a_1]


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       December 22, 1999   

       Rewritten for timing qp3 code.   


    Purpose   
    =======   

    ZTIMQ3 times the Rank-Revealing QR factorization of a   
    COMPLEX*16 general matrix.   

    Two matrix types may be used for timing.  The number of types is   
    set in the parameter NMODE and the matrix types are set in the vector   
    MODES, using the following key:   
       2.  BREAK1    D(1:N-1)=1 and D(N)=1.0/COND in ZLATMS   
       3.  GEOM      D(I)=COND**(-(I-1)/(N-1)) in ZLATMS   
    These numbers are chosen to correspond with the matrix types in the   
    test code.   

    Arguments   
    =========   

    LINE    (input) CHARACTER*80   
            The input line that requested this routine.  The first six   
            characters contain either the name of a subroutine or a   
            generic path name.  The remaining characters may be used to   
            specify the individual routines to be timed.  See ATIMIN for   
            a full description of the format of the input line.   

    NM      (input) INTEGER   
            The number of values of M and N contained in the vectors   
            MVAL and NVAL.  The matrix sizes are used in pairs (M,N).   

    MVAL    (input) INTEGER array, dimension (NM)   
            The values of the matrix row dimension M.   

    NVAL    (input) INTEGER array, dimension (NM)   
            The values of the matrix column dimension N.   

    NK      (input) INTEGER   
            The number of values of K in the vector KVAL.   

    KVAL    (input) INTEGER array, dimension (NK)   
            The values of the matrix dimension K, used in DORMQR.   

    NNB     (input) INTEGER   
            The number of values of NB and NX contained in the   
            vectors NBVAL and NXVAL.  The blocking parameters are used   
            in pairs (NB,NX).   

    NBVAL   (input) INTEGER array, dimension (NNB)   
            The values of the blocksize NB.   

    NXVAL   (input) INTEGER array, dimension (NNB)   
            The values of the crossover point NX.   

    NLDA    (input) INTEGER   
            The number of values of LDA contained in the vector LDAVAL.   

    LDAVAL  (input) INTEGER array, dimension (NLDA)   
            The values of the leading dimension of the array A.   

    TIMMIN  (input) DOUBLE PRECISION   
            The minimum time a subroutine will be timed.   

    A       (workspace) COMPLEX*16 array, dimension (LDAMAX*NMAX)   
            where LDAMAX and NMAX are the maximum values of LDA and N.   

    COPYA   (workspace) COMPLEX*16 array, dimension (LDAMAX*NMAX)   

    WORK    (workspace) COMPLEX*16 array, dimension (3*max(MMAX,NMAX))   

    RWORK   (workspace) DOUBLE PRECISION array, dimension (2*NMAX)   

    IWORK   (workspace) INTEGER array, dimension (NMAX)   

    RESLTS  (workspace) DOUBLE PRECISION array, dimension   
                        (LDR1,LDR2,NLDA)   
            The timing results for each subroutine over the relevant   
            values of MODE, (M,N), and LDA.   

    LDR1    (input) INTEGER   
            The first dimension of RESLTS.  LDR1 >= max(1,NM).   

    LDR2    (input) INTEGER   
            The second dimension of RESLTS.  LDR2 >= max(1,NM).   

    NOUT    (input) INTEGER   
            The unit number for output.   

    =====================================================================   



       Parameter adjustments */
    --mval;
    --nval;
    --nbval;
    --nxval;
    --ldaval;
    --a;
    --copya;
    --tau;
    --work;
    --rwork;
    --iwork;
    reslts_dim1 = *ldr1;
    reslts_dim2 = *ldr2;
    reslts_offset = 1 + reslts_dim1 * (1 + reslts_dim2 * 1);
    reslts -= reslts_offset;

    /* Function Body   

       Extract the timing request from the input line. */

    s_copy(path, "Zomplex precision", (ftnlen)1, (ftnlen)17);
    s_copy(path + 1, "QP", (ftnlen)2, (ftnlen)2);
    atimin_(path, line, &c__1, subnam, timsub, nout, &info, (ftnlen)3, (
	    ftnlen)80, (ftnlen)6);
    if (! timsub[0] || info != 0) {
	goto L90;
    }

/*     Check that M <= LDA for the input values. */

    s_copy(cname, line, (ftnlen)6, (ftnlen)6);
    atimck_(&c__1, cname, nm, &mval[1], nlda, &ldaval[1], nout, &info, (
	    ftnlen)6);
    if (info > 0) {
	io___8.ciunit = *nout;
	s_wsfe(&io___8);
	do_fio(&c__1, cname, (ftnlen)6);
	e_wsfe();
	goto L90;
    }

/*     Set the condition number and scaling factor for the matrices   
       to be generated. */

    dmax__ = 1.;
    cond = 1. / dlamch_("Precision");

/*     Do for each type of matrix: */

    for (imode = 1; imode <= 2; ++imode) {
	mode = modes[imode - 1];


/*        *****************   
          * Timing xGEQP3 *   
          *****************   

          Do for each value of LDA: */

	i__1 = *nlda;
	for (ilda = 1; ilda <= i__1; ++ilda) {
	    lda = ldaval[ilda];

/*           Do for each pair of values (M,N): */

	    i__2 = *nm;
	    for (im = 1; im <= i__2; ++im) {
		m = mval[im];
		n = nval[im];
		minmn = min(m,n);

/*              Generate a test matrix of size m by n using the   
                singular value distribution indicated by MODE. */

		zlatms_(&m, &n, "Uniform", iseed, "Nonsymm", &rwork[1], &mode,
			 &cond, &dmax__, &m, &n, "No packing", &copya[1], &
			lda, &work[1], &info);

/*              Do for each pair of values ( NB, NX ) in NBVAL and NXVAL: */

		i__3 = *nnb;
		for (inb = 1; inb <= i__3; ++inb) {
		    nb = nbval[inb];
		    xlaenv_(&c__1, &nb);
		    nx = nxval[inb];
		    xlaenv_(&c__3, &nx);


/*                 ZGEQP3   

   Computing MAX */
		    i__4 = 1, i__5 = (n + 1) * nb;
		    lw = max(i__4,i__5);
		    i__4 = n;
		    for (i__ = 1; i__ <= i__4; ++i__) {
			iwork[n + i__] = 0;
/* L10: */
		    }
		    ic = 0;
		    s1 = dsecnd_();
L20:
		    icopy_(&n, &iwork[n + 1], &c__1, &iwork[1], &c__1);
		    zlacpy_("All", &m, &n, &copya[1], &lda, &a[1], &lda);
		    zgeqp3_(&m, &n, &a[1], &lda, &iwork[1], &tau[1], &work[1],
			     &lw, &rwork[1], &info);

		    if (info != 0) {
			s_wsle(&io___26);
			do_lio(&c__9, &c__1, ">>>Warning: INFO returned by ", 
				(ftnlen)29);
			do_lio(&c__9, &c__1, "ZGEQP3 is:", (ftnlen)10);
			do_lio(&c__3, &c__1, (char *)&info, (ftnlen)sizeof(
				integer));
			e_wsle();
			info = 0;
		    }

		    s2 = dsecnd_();
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			goto L20;
		    }

		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			icopy_(&n, &iwork[n + 1], &c__1, &iwork[1], &c__1);
			zlacpy_("All", &m, &n, &copya[1], &lda, &a[1], &lda);
			goto L20;
		    }

/*                 Subtract the time used in ZLACPY. */

		    icl = 1;
		    s1 = dsecnd_();
L30:
		    s2 = dsecnd_();
		    untime = s2 - s1;
		    ++icl;
		    if (icl <= ic) {
			icopy_(&n, &iwork[n + 1], &c__1, &iwork[1], &c__1);
			zlacpy_("All", &m, &n, &copya[1], &lda, &a[1], &lda);
			goto L30;
		    }

		    time = (time - untime) / (doublereal) ic;

/*                 The number of flops of yGEQP3 is approximately the   
                   the number of flops of yGEQPF. */

		    ops = dopla_("ZGEQPF", &m, &n, &c__0, &c__0, &nb);
		    reslts_ref(inb, im, ilda) = dmflop_(&ops, &time, &info);

/* L40: */
		}
/* L50: */
	    }
/* L60: */
	}

/*        Print the results for each value of K and type of matrix. */

	io___32.ciunit = *nout;
	s_wsfe(&io___32);
	do_fio(&c__1, subnam_ref(0, 1), (ftnlen)6);
	e_wsfe();
	io___33.ciunit = *nout;
	s_wsfe(&io___33);
	do_fio(&c__1, (char *)&imode, (ftnlen)sizeof(integer));
	e_wsfe();
	i__1 = *nlda;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    io___34.ciunit = *nout;
	    s_wsfe(&io___34);
	    do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&ldaval[i__], (ftnlen)sizeof(integer));
	    e_wsfe();
/* L70: */
	}
	io___35.ciunit = *nout;
	s_wsle(&io___35);
	e_wsle();
	dprtb4_("(  NB,  NX)", "M", "N", nnb, &nbval[1], &nxval[1], nm, &mval[
		1], &nval[1], nlda, &reslts_ref(1, 1, 1), ldr1, ldr2, nout, (
		ftnlen)11, (ftnlen)1, (ftnlen)1);

/* L80: */
    }


L90:
    return 0;

/*     End of ZTIMQ3 */

} /* ztimq3_ */

#undef reslts_ref
#undef subnam_ref


