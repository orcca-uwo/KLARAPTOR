#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__0 = 0;
static integer c__1 = 1;

/* Subroutine */ int ctimgb_(char *line, integer *nm, integer *mval, integer *
	nk, integer *kval, integer *nns, integer *nsval, integer *nnb, 
	integer *nbval, integer *nlda, integer *ldaval, real *timmin, complex 
	*a, complex *b, integer *iwork, real *reslts, integer *ldr1, integer *
	ldr2, integer *ldr3, integer *nout, ftnlen line_len)
{
    /* Initialized data */

    static char subnam[6*2] = "CGBTRF" "CGBTRS";

    /* Format strings */
    static char fmt_9999[] = "(1x,a6,\002 timing run not attempted\002,/)";
    static char fmt_9998[] = "(/\002 *** Speed of \002,a6,\002 in megaflops "
	    "***\002)";
    static char fmt_9997[] = "(5x,\002with LDA = \002,i5)";
    static char fmt_9996[] = "(5x,\002line \002,i2,\002 with LDA = \002,i5)";
    static char fmt_9995[] = "(/5x,a6,\002 with M =\002,i6,/)";

    /* System generated locals */
    integer reslts_dim1, reslts_dim2, reslts_dim3, reslts_offset, i__1, i__2, 
	    i__3, i__4, i__5, i__6, i__7;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void),
	     s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static integer ilda, info;
    static char path[3];
    static real time;
    static integer isub, nrhs, i__, k, m, n;
    static char cname[6];
    extern doublereal sopgb_(char *, integer *, integer *, integer *, integer 
	    *, integer *), sopla_(char *, integer *, integer *, 
	    integer *, integer *, integer *);
    static real s1, s2;
    static integer ic, nb, ik, im, kl, ku;
    extern /* Subroutine */ int cgbtrf_(integer *, integer *, integer *, 
	    integer *, complex *, integer *, integer *, integer *), atimck_(
	    integer *, char *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, ftnlen);
    extern doublereal second_(void);
    extern /* Subroutine */ int ctimmg_(integer *, integer *, integer *, 
	    complex *, integer *, integer *, integer *), atimin_(char *, char 
	    *, integer *, char *, logical *, integer *, integer *, ftnlen, 
	    ftnlen, ftnlen), cgbtrs_(char *, integer *, integer *, integer *, 
	    integer *, complex *, integer *, integer *, complex *, integer *, 
	    integer *), xlaenv_(integer *, integer *);
    extern doublereal smflop_(real *, real *, integer *);
    static real untime;
    static logical timsub[2];
    extern /* Subroutine */ int sprtbl_(char *, char *, integer *, integer *, 
	    integer *, integer *, integer *, real *, integer *, integer *, 
	    integer *, ftnlen, ftnlen);
    static integer lda, ldb, icl, inb;
    static real ops;

    /* Fortran I/O blocks */
    static cilist io___6 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___29 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___30 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___31 = { 0, 0, 0, fmt_9996, 0 };
    static cilist io___32 = { 0, 0, 0, fmt_9995, 0 };



#define subnam_ref(a_0,a_1) &subnam[(a_1)*6 + a_0 - 6]
#define reslts_ref(a_1,a_2,a_3,a_4) reslts[(((a_4)*reslts_dim3 + (a_3))*\
reslts_dim2 + (a_2))*reslts_dim1 + a_1]


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       March 31, 1993   


    Purpose   
    =======   

    CTIMGB times CGBTRF and -TRS.   

    Arguments   
    =========   

    LINE    (input) CHARACTER*80   
            The input line that requested this routine.  The first six   
            characters contain either the name of a subroutine or a   
            generic path name.  The remaining characters may be used to   
            specify the individual routines to be timed.  See ATIMIN for   
            a full description of the format of the input line.   

    NM      (input) INTEGER   
            The number of values of M contained in the vector MVAL.   

    MVAL    (input) INTEGER array, dimension (NM)   
            The values of the matrix size M.   

    NK      (input) INTEGER   
            The number of values of K contained in the vector KVAL.   

    KVAL    (input) INTEGER array, dimension (NK)   
            The values of the band width K.   

    NNS     (input) INTEGER   
            The number of values of NRHS contained in the vector NSVAL.   

    NSVAL   (input) INTEGER array, dimension (NNS)   
            The values of the number of right hand sides NRHS.   

    NNB     (input) INTEGER   
            The number of values of NB contained in the vector NBVAL.   

    NBVAL   (input) INTEGER array, dimension (NNB)   
            The values of the blocksize NB.   

    NLDA    (input) INTEGER   
            The number of values of LDA contained in the vector LDAVAL.   

    LDAVAL  (input) INTEGER array, dimension (NLDA)   
            The values of the leading dimension of the array A.   

    TIMMIN  (input) REAL   
            The minimum time a subroutine will be timed.   

    A       (workspace) COMPLEX array, dimension (LDAMAX*NMAX)   
            where LDAMAX and NMAX are the maximum values permitted   
            for LDA and N.   

    B       (workspace) COMPLEX array, dimension (LDAMAX*NMAX)   

    IWORK   (workspace) INTEGER array, dimension (NMAX)   

    RESLTS  (output) REAL array, dimension   
                     (LDR1,LDR2,LDR3,NSUBS)   
            The timing results for each subroutine over the relevant   
            values of N, K, NB, and LDA.   

    LDR1    (input) INTEGER   
            The first dimension of RESLTS.  LDR1 >= max(4,NNB).   

    LDR2    (input) INTEGER   
            The second dimension of RESLTS.  LDR2 >= max(1,NK).   

    LDR3    (input) INTEGER   
            The third dimension of RESLTS.  LDR3 >= max(1,NLDA).   

    NOUT    (input) INTEGER   
            The unit number for output.   

    =====================================================================   

       Parameter adjustments */
    --mval;
    --kval;
    --nsval;
    --nbval;
    --ldaval;
    --a;
    --b;
    --iwork;
    reslts_dim1 = *ldr1;
    reslts_dim2 = *ldr2;
    reslts_dim3 = *ldr3;
    reslts_offset = 1 + reslts_dim1 * (1 + reslts_dim2 * (1 + reslts_dim3 * 1)
	    );
    reslts -= reslts_offset;

    /* Function Body   

       Extract the timing request from the input line. */

    s_copy(path, "Complex precision", (ftnlen)1, (ftnlen)17);
    s_copy(path + 1, "GB", (ftnlen)2, (ftnlen)2);
    atimin_(path, line, &c__2, subnam, timsub, nout, &info, (ftnlen)3, (
	    ftnlen)80, (ftnlen)6);
    if (info != 0) {
	goto L120;
    }

/*     Check that 3*K+1 <= LDA for the input values. */

    s_copy(cname, line, (ftnlen)6, (ftnlen)6);
    atimck_(&c__0, cname, nk, &kval[1], nlda, &ldaval[1], nout, &info, (
	    ftnlen)6);
    if (info > 0) {
	io___6.ciunit = *nout;
	s_wsfe(&io___6);
	do_fio(&c__1, cname, (ftnlen)6);
	e_wsfe();
	goto L120;
    }

/*     Do for each value of the matrix size M: */

    i__1 = *nm;
    for (im = 1; im <= i__1; ++im) {
	m = mval[im];
	n = m;

/*        Do for each value of LDA: */

	i__2 = *nlda;
	for (ilda = 1; ilda <= i__2; ++ilda) {
	    lda = ldaval[ilda];

/*           Do for each value of the band width K: */

	    i__3 = *nk;
	    for (ik = 1; ik <= i__3; ++ik) {
		k = kval[ik];
/* Computing MAX   
   Computing MIN */
		i__6 = k, i__7 = m - 1;
		i__4 = 0, i__5 = min(i__6,i__7);
		kl = max(i__4,i__5);
/* Computing MAX   
   Computing MIN */
		i__6 = k, i__7 = n - 1;
		i__4 = 0, i__5 = min(i__6,i__7);
		ku = max(i__4,i__5);

/*              Time CGBTRF */

		if (timsub[0]) {

/*                 Do for each value of NB in NBVAL.  Only CGBTRF is   
                   timed in this loop since the other routines are   
                   independent of NB. */

		    i__4 = *nnb;
		    for (inb = 1; inb <= i__4; ++inb) {
			nb = nbval[inb];
			xlaenv_(&c__1, &nb);
			ic = 0;
			ctimmg_(&c__2, &m, &n, &a[1], &lda, &kl, &ku);
			s1 = second_();
L10:
			cgbtrf_(&m, &n, &kl, &ku, &a[1], &lda, &iwork[1], &
				info);
			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    ctimmg_(&c__2, &m, &n, &a[1], &lda, &kl, &ku);
			    goto L10;
			}

/*                    Subtract the time used in CTIMMG. */

			icl = 1;
			s1 = second_();
L20:
			s2 = second_();
			untime = s2 - s1;
			++icl;
			if (icl <= ic) {
			    ctimmg_(&c__2, &m, &n, &a[1], &lda, &kl, &ku);
			    goto L20;
			}

			time = (time - untime) / (real) ic;
			ops = sopgb_("CGBTRF", &m, &n, &kl, &ku, &iwork[1]);
			reslts_ref(inb, ik, ilda, 1) = smflop_(&ops, &time, &
				info);
/* L30: */
		    }
		} else {
		    ic = 0;
		    ctimmg_(&c__2, &m, &n, &a[1], &lda, &kl, &ku);
		}

/*              Generate another matrix and factor it using CGBTRF so   
                that the factored form can be used in timing the other   
                routines. */

		nb = 1;
		xlaenv_(&c__1, &nb);
		if (ic != 1) {
		    cgbtrf_(&m, &n, &kl, &ku, &a[1], &lda, &iwork[1], &info);
		}

/*              Time CGBTRS */

		if (timsub[1]) {
		    i__4 = *nns;
		    for (i__ = 1; i__ <= i__4; ++i__) {
			nrhs = nsval[i__];
			ldb = n;
			ic = 0;
			ctimmg_(&c__0, &n, &nrhs, &b[1], &ldb, &c__0, &c__0);
			s1 = second_();
L40:
			cgbtrs_("No transpose", &n, &kl, &ku, &nrhs, &a[1], &
				lda, &iwork[1], &b[1], &ldb, &info);
			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    ctimmg_(&c__0, &n, &nrhs, &b[1], &ldb, &c__0, &
				    c__0);
			    goto L40;
			}

/*                    Subtract the time used in CTIMMG. */

			icl = 1;
			s1 = second_();
L50:
			s2 = second_();
			untime = s2 - s1;
			++icl;
			if (icl <= ic) {
			    ctimmg_(&c__0, &n, &nrhs, &b[1], &ldb, &c__0, &
				    c__0);
			    goto L50;
			}

			time = (time - untime) / (real) ic;
			ops = sopla_("CGBTRS", &n, &nrhs, &kl, &ku, &c__0);
			reslts_ref(i__, ik, ilda, 2) = smflop_(&ops, &time, &
				info);
/* L60: */
		    }
		}
/* L70: */
	    }
/* L80: */
	}

/*        Print a table of results for each routine */

	for (isub = 1; isub <= 2; ++isub) {
	    if (! timsub[isub - 1]) {
		goto L100;
	    }

/*           Print header for routine names. */

	    if (im == 1 || s_cmp(cname, "CGB   ", (ftnlen)6, (ftnlen)6) == 0) 
		    {
		io___29.ciunit = *nout;
		s_wsfe(&io___29);
		do_fio(&c__1, subnam_ref(0, isub), (ftnlen)6);
		e_wsfe();
		if (*nlda == 1) {
		    io___30.ciunit = *nout;
		    s_wsfe(&io___30);
		    do_fio(&c__1, (char *)&ldaval[1], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		} else {
		    i__2 = *nlda;
		    for (i__ = 1; i__ <= i__2; ++i__) {
			io___31.ciunit = *nout;
			s_wsfe(&io___31);
			do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&ldaval[i__], (ftnlen)sizeof(
				integer));
			e_wsfe();
/* L90: */
		    }
		}
	    }

	    io___32.ciunit = *nout;
	    s_wsfe(&io___32);
	    do_fio(&c__1, subnam_ref(0, isub), (ftnlen)6);
	    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
	    e_wsfe();
	    if (isub == 1) {
		sprtbl_("NB", "K", nnb, &nbval[1], nk, &kval[1], nlda, &
			reslts_ref(1, 1, 1, 1), ldr1, ldr2, nout, (ftnlen)2, (
			ftnlen)1);
	    } else if (isub == 2) {
		sprtbl_("NRHS", "K", nns, &nsval[1], nk, &kval[1], nlda, &
			reslts_ref(1, 1, 1, 2), ldr1, ldr2, nout, (ftnlen)4, (
			ftnlen)1);
	    }
L100:
	    ;
	}
/* L110: */
    }
L120:


    return 0;

/*     End of CTIMGB */

} /* ctimgb_ */

#undef reslts_ref
#undef subnam_ref


