#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__1 = 1;
static integer c__4 = 4;
static real c_b24 = 100.f;
static real c_b25 = 1.f;
static integer c__0 = 0;

/* Subroutine */ int ctimlq_(char *line, integer *nm, integer *mval, integer *
	nval, integer *nk, integer *kval, integer *nnb, integer *nbval, 
	integer *nxval, integer *nlda, integer *ldaval, real *timmin, complex 
	*a, complex *tau, complex *b, complex *work, real *rwork, real *
	reslts, integer *ldr1, integer *ldr2, integer *ldr3, integer *nout, 
	ftnlen line_len)
{
    /* Initialized data */

    static char subnam[6*3] = "CGELQF" "CUNGLQ" "CUNMLQ";
    static char sides[1*2] = "L" "R";
    static char transs[1*2] = "N" "C";
    static integer iseed[4] = { 0,0,0,1 };

    /* Format strings */
    static char fmt_9999[] = "(1x,a6,\002 timing run not attempted\002,/)";
    static char fmt_9998[] = "(/\002 *** Speed of \002,a6,\002 in megaflops "
	    "***\002)";
    static char fmt_9997[] = "(5x,\002line \002,i2,\002 with LDA = \002,i5)";
    static char fmt_9996[] = "(5x,\002K = min(M,N)\002,/)";
    static char fmt_9995[] = "(/5x,a6,\002 with SIDE = '\002,a1,\002', TRANS"
	    " = '\002,a1,\002', \002,a1,\002 =\002,i6,/)";
    static char fmt_9994[] = "(\002 *** No pairs (M,N) found with M <= N: "
	    " \002,a6,\002 not timed\002)";

    /* System generated locals */
    integer reslts_dim1, reslts_dim2, reslts_dim3, reslts_offset, i__1, i__2, 
	    i__3, i__4, i__5, i__6;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void),
	     s_wsle(cilist *), e_wsle(void);

    /* Local variables */
    static integer ilda;
    static char labm[1], side[1];
    static integer info;
    static char path[3];
    static real time;
    static integer isub, muse[12], nuse[12], i__, k, m, n;
    static char cname[6];
    static integer iside, itoff, itran, minmn;
    extern doublereal sopla_(char *, integer *, integer *, integer *, integer 
	    *, integer *);
    extern /* Subroutine */ int icopy_(integer *, integer *, integer *, 
	    integer *, integer *);
    static char trans[1];
    static integer k1, i4, m1, n1;
    static real s1, s2;
    static integer ic;
    extern /* Subroutine */ int sprtb4_(char *, char *, char *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    real *, integer *, integer *, integer *, ftnlen, ftnlen, ftnlen), 
	    sprtb5_(char *, char *, char *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, real *, integer *, integer *, 
	    integer *, ftnlen, ftnlen, ftnlen);
    static integer nb, ik, im;
    extern /* Subroutine */ int cgelqf_(integer *, integer *, complex *, 
	    integer *, complex *, complex *, integer *, integer *);
    static integer lw, nx, reseed[4];
    extern /* Subroutine */ int atimck_(integer *, char *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, ftnlen);
    extern doublereal second_(void);
    extern /* Subroutine */ int clacpy_(char *, integer *, integer *, complex 
	    *, integer *, complex *, integer *), ctimmg_(integer *, 
	    integer *, integer *, complex *, integer *, integer *, integer *),
	     atimin_(char *, char *, integer *, char *, logical *, integer *, 
	    integer *, ftnlen, ftnlen, ftnlen), clatms_(integer *, integer *, 
	    char *, integer *, char *, real *, integer *, real *, real *, 
	    integer *, integer *, char *, complex *, integer *, complex *, 
	    integer *), cunglq_(integer *, integer *, 
	    integer *, complex *, integer *, complex *, complex *, integer *, 
	    integer *), xlaenv_(integer *, integer *), cunmlq_(char *, char *,
	     integer *, integer *, integer *, complex *, integer *, complex *,
	     complex *, integer *, complex *, integer *, integer *);
    extern doublereal smflop_(real *, real *, integer *);
    static real untime;
    static logical timsub[3];
    static integer lda, icl, inb, imx;
    static real ops;

    /* Fortran I/O blocks */
    static cilist io___9 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___29 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___31 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___32 = { 0, 0, 0, 0, 0 };
    static cilist io___33 = { 0, 0, 0, fmt_9996, 0 };
    static cilist io___34 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___49 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___50 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___51 = { 0, 0, 0, fmt_9995, 0 };
    static cilist io___53 = { 0, 0, 0, fmt_9995, 0 };
    static cilist io___54 = { 0, 0, 0, fmt_9994, 0 };



#define subnam_ref(a_0,a_1) &subnam[(a_1)*6 + a_0 - 6]
#define reslts_ref(a_1,a_2,a_3,a_4) reslts[(((a_4)*reslts_dim3 + (a_3))*\
reslts_dim2 + (a_2))*reslts_dim1 + a_1]


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       March 31, 1993   


    Purpose   
    =======   

    CTIMLQ times the LAPACK routines to perform the LQ factorization of   
    a COMPLEX general matrix.   

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
            The values of the matrix dimension K, used in CUNMLQ.   

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

    TIMMIN  (input) REAL   
            The minimum time a subroutine will be timed.   

    A       (workspace) COMPLEX array, dimension (LDAMAX*NMAX)   
            where LDAMAX and NMAX are the maximum values of LDA and N.   

    TAU     (workspace) COMPLEX array, dimension (min(M,N))   

    B       (workspace) COMPLEX array, dimension (LDAMAX*NMAX)   

    WORK    (workspace) COMPLEX array, dimension (LDAMAX*NBMAX)   
            where NBMAX is the maximum value of NB.   

    RWORK   (workspace) REAL array, dimension   
                        (min(MMAX,NMAX))   

    RESLTS  (workspace) REAL array, dimension   
                        (LDR1,LDR2,LDR3,2*NK)   
            The timing results for each subroutine over the relevant   
            values of (M,N), (NB,NX), and LDA.   

    LDR1    (input) INTEGER   
            The first dimension of RESLTS.  LDR1 >= max(1,NNB).   

    LDR2    (input) INTEGER   
            The second dimension of RESLTS.  LDR2 >= max(1,NM).   

    LDR3    (input) INTEGER   
            The third dimension of RESLTS.  LDR3 >= max(1,NLDA).   

    NOUT    (input) INTEGER   
            The unit number for output.   

    Internal Parameters   
    ===================   

    MODE    INTEGER   
            The matrix type.  MODE = 3 is a geometric distribution of   
            eigenvalues.  See CLATMS for further details.   

    COND    REAL   
            The condition number of the matrix.  The singular values are   
            set to values from DMAX to DMAX/COND.   

    DMAX    REAL   
            The magnitude of the largest singular value.   

    =====================================================================   

       Parameter adjustments */
    --mval;
    --nval;
    --kval;
    --nbval;
    --nxval;
    --ldaval;
    --a;
    --tau;
    --b;
    --work;
    --rwork;
    reslts_dim1 = *ldr1;
    reslts_dim2 = *ldr2;
    reslts_dim3 = *ldr3;
    reslts_offset = 1 + reslts_dim1 * (1 + reslts_dim2 * (1 + reslts_dim3 * 1)
	    );
    reslts -= reslts_offset;

    /* Function Body   

       Extract the timing request from the input line. */

    s_copy(path, "Complex precision", (ftnlen)1, (ftnlen)17);
    s_copy(path + 1, "LQ", (ftnlen)2, (ftnlen)2);
    atimin_(path, line, &c__3, subnam, timsub, nout, &info, (ftnlen)3, (
	    ftnlen)80, (ftnlen)6);
    if (info != 0) {
	goto L230;
    }

/*     Check that M <= LDA for the input values. */

    s_copy(cname, line, (ftnlen)6, (ftnlen)6);
    atimck_(&c__1, cname, nm, &mval[1], nlda, &ldaval[1], nout, &info, (
	    ftnlen)6);
    if (info > 0) {
	io___9.ciunit = *nout;
	s_wsfe(&io___9);
	do_fio(&c__1, cname, (ftnlen)6);
	e_wsfe();
	goto L230;
    }

/*     Do for each pair of values (M,N): */

    i__1 = *nm;
    for (im = 1; im <= i__1; ++im) {
	m = mval[im];
	n = nval[im];
	minmn = min(m,n);
	icopy_(&c__4, iseed, &c__1, reseed, &c__1);

/*        Do for each value of LDA: */

	i__2 = *nlda;
	for (ilda = 1; ilda <= i__2; ++ilda) {
	    lda = ldaval[ilda];

/*           Do for each pair of values (NB, NX) in NBVAL and NXVAL. */

	    i__3 = *nnb;
	    for (inb = 1; inb <= i__3; ++inb) {
		nb = nbval[inb];
		xlaenv_(&c__1, &nb);
		nx = nxval[inb];
		xlaenv_(&c__3, &nx);
/* Computing MAX */
		i__4 = 1, i__5 = m * max(1,nb);
		lw = max(i__4,i__5);

/*              Generate a test matrix of size M by N. */

		icopy_(&c__4, reseed, &c__1, iseed, &c__1);
		clatms_(&m, &n, "Uniform", iseed, "Nonsymm", &rwork[1], &c__3,
			 &c_b24, &c_b25, &m, &n, "No packing", &b[1], &lda, &
			work[1], &info);

		if (timsub[0]) {

/*                 CGELQF:  LQ factorization */

		    clacpy_("Full", &m, &n, &b[1], &lda, &a[1], &lda);
		    ic = 0;
		    s1 = second_();
L10:
		    cgelqf_(&m, &n, &a[1], &lda, &tau[1], &work[1], &lw, &
			    info);
		    s2 = second_();
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			clacpy_("Full", &m, &n, &b[1], &lda, &a[1], &lda);
			goto L10;
		    }

/*                 Subtract the time used in CLACPY. */

		    icl = 1;
		    s1 = second_();
L20:
		    s2 = second_();
		    untime = s2 - s1;
		    ++icl;
		    if (icl <= ic) {
			clacpy_("Full", &m, &n, &a[1], &lda, &b[1], &lda);
			goto L20;
		    }

		    time = (time - untime) / (real) ic;
		    ops = sopla_("CGELQF", &m, &n, &c__0, &c__0, &nb);
		    reslts_ref(inb, im, ilda, 1) = smflop_(&ops, &time, &info)
			    ;
		} else {

/*                 If CGELQF was not timed, generate a matrix and factor   
                   it using CGELQF anyway so that the factored form of   
                   the matrix can be used in timing the other routines. */

		    clacpy_("Full", &m, &n, &b[1], &lda, &a[1], &lda);
		    cgelqf_(&m, &n, &a[1], &lda, &tau[1], &work[1], &lw, &
			    info);
		}

		if (timsub[1]) {

/*                 CUNGLQ:  Generate orthogonal matrix Q from the LQ   
                   factorization */

		    clacpy_("Full", &minmn, &n, &a[1], &lda, &b[1], &lda);
		    ic = 0;
		    s1 = second_();
L30:
		    cunglq_(&minmn, &n, &minmn, &b[1], &lda, &tau[1], &work[1]
			    , &lw, &info);
		    s2 = second_();
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			clacpy_("Full", &minmn, &n, &a[1], &lda, &b[1], &lda);
			goto L30;
		    }

/*                 Subtract the time used in CLACPY. */

		    icl = 1;
		    s1 = second_();
L40:
		    s2 = second_();
		    untime = s2 - s1;
		    ++icl;
		    if (icl <= ic) {
			clacpy_("Full", &minmn, &n, &a[1], &lda, &b[1], &lda);
			goto L40;
		    }

		    time = (time - untime) / (real) ic;
		    ops = sopla_("CUNGLQ", &minmn, &n, &minmn, &c__0, &nb);
		    reslts_ref(inb, im, ilda, 2) = smflop_(&ops, &time, &info)
			    ;
		}

/* L50: */
	    }
/* L60: */
	}
/* L70: */
    }

/*     Print tables of results */

    for (isub = 1; isub <= 2; ++isub) {
	if (! timsub[isub - 1]) {
	    goto L90;
	}
	io___29.ciunit = *nout;
	s_wsfe(&io___29);
	do_fio(&c__1, subnam_ref(0, isub), (ftnlen)6);
	e_wsfe();
	if (*nlda > 1) {
	    i__1 = *nlda;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		io___31.ciunit = *nout;
		s_wsfe(&io___31);
		do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&ldaval[i__], (ftnlen)sizeof(integer));
		e_wsfe();
/* L80: */
	    }
	}
	io___32.ciunit = *nout;
	s_wsle(&io___32);
	e_wsle();
	if (isub == 2) {
	    io___33.ciunit = *nout;
	    s_wsfe(&io___33);
	    e_wsfe();
	}
	sprtb4_("(  NB,  NX)", "M", "N", nnb, &nbval[1], &nxval[1], nm, &mval[
		1], &nval[1], nlda, &reslts_ref(1, 1, 1, isub), ldr1, ldr2, 
		nout, (ftnlen)11, (ftnlen)1, (ftnlen)1);
L90:
	;
    }

/*     Time CUNMLQ separately.  Here the starting matrix is M by N, and   
       K is the free dimension of the matrix multiplied by Q. */

    if (timsub[2]) {

/*        Check that K <= LDA for the input values. */

	atimck_(&c__3, cname, nk, &kval[1], nlda, &ldaval[1], nout, &info, (
		ftnlen)6);
	if (info > 0) {
	    io___34.ciunit = *nout;
	    s_wsfe(&io___34);
	    do_fio(&c__1, subnam_ref(0, 3), (ftnlen)6);
	    e_wsfe();
	    goto L230;
	}

/*        Use only the pairs (M,N) where M <= N. */

	imx = 0;
	i__1 = *nm;
	for (im = 1; im <= i__1; ++im) {
	    if (mval[im] <= nval[im]) {
		++imx;
		muse[imx - 1] = mval[im];
		nuse[imx - 1] = nval[im];
	    }
/* L100: */
	}

/*        CUNMLQ:  Multiply by Q stored as a product of elementary   
          transformations   

          Do for each pair of values (M,N): */

	i__1 = imx;
	for (im = 1; im <= i__1; ++im) {
	    m = muse[im - 1];
	    n = nuse[im - 1];

/*           Do for each value of LDA: */

	    i__2 = *nlda;
	    for (ilda = 1; ilda <= i__2; ++ilda) {
		lda = ldaval[ilda];

/*              Generate an M by N matrix and form its LQ decomposition. */

		clatms_(&m, &n, "Uniform", iseed, "Nonsymm", &rwork[1], &c__3,
			 &c_b24, &c_b25, &m, &n, "No packing", &a[1], &lda, &
			work[1], &info);
/* Computing MAX */
		i__3 = 1, i__4 = m * max(1,nb);
		lw = max(i__3,i__4);
		cgelqf_(&m, &n, &a[1], &lda, &tau[1], &work[1], &lw, &info);

/*              Do first for SIDE = 'L', then for SIDE = 'R' */

		i4 = 0;
		for (iside = 1; iside <= 2; ++iside) {
		    *(unsigned char *)side = *(unsigned char *)&sides[iside - 
			    1];

/*                 Do for each pair of values (NB, NX) in NBVAL and   
                   NXVAL. */

		    i__3 = *nnb;
		    for (inb = 1; inb <= i__3; ++inb) {
			nb = nbval[inb];
			xlaenv_(&c__1, &nb);
			nx = nxval[inb];
			xlaenv_(&c__3, &nx);

/*                    Do for each value of K in KVAL */

			i__4 = *nk;
			for (ik = 1; ik <= i__4; ++ik) {
			    k = kval[ik];

/*                       Sort out which variable is which */

			    if (iside == 1) {
				k1 = m;
				m1 = n;
				n1 = k;
/* Computing MAX */
				i__5 = 1, i__6 = n1 * max(1,nb);
				lw = max(i__5,i__6);
			    } else {
				k1 = m;
				n1 = n;
				m1 = k;
/* Computing MAX */
				i__5 = 1, i__6 = m1 * max(1,nb);
				lw = max(i__5,i__6);
			    }

/*                       Do first for TRANS = 'N', then for TRANS = 'T' */

			    itoff = 0;
			    for (itran = 1; itran <= 2; ++itran) {
				*(unsigned char *)trans = *(unsigned char *)&
					transs[itran - 1];
				ctimmg_(&c__0, &m1, &n1, &b[1], &lda, &c__0, &
					c__0);
				ic = 0;
				s1 = second_();
L110:
				cunmlq_(side, trans, &m1, &n1, &k1, &a[1], &
					lda, &tau[1], &b[1], &lda, &work[1], &
					lw, &info);
				s2 = second_();
				time = s2 - s1;
				++ic;
				if (time < *timmin) {
				    ctimmg_(&c__0, &m1, &n1, &b[1], &lda, &
					    c__0, &c__0);
				    goto L110;
				}

/*                          Subtract the time used in CTIMMG. */

				icl = 1;
				s1 = second_();
L120:
				s2 = second_();
				untime = s2 - s1;
				++icl;
				if (icl <= ic) {
				    ctimmg_(&c__0, &m1, &n1, &b[1], &lda, &
					    c__0, &c__0);
				    goto L120;
				}

				time = (time - untime) / (real) ic;
				i__5 = iside - 1;
				ops = sopla_("CUNMLQ", &m1, &n1, &k1, &i__5, &
					nb);
				reslts_ref(inb, im, ilda, i4 + itoff + ik) = 
					smflop_(&ops, &time, &info);
				itoff = *nk;
/* L130: */
			    }
/* L140: */
			}
/* L150: */
		    }
		    i4 = *nk << 1;
/* L160: */
		}
/* L170: */
	    }
/* L180: */
	}

/*        Print tables of results */

	isub = 3;
	i4 = 1;
	if (imx >= 1) {
	    for (iside = 1; iside <= 2; ++iside) {
		*(unsigned char *)side = *(unsigned char *)&sides[iside - 1];
		if (iside == 1) {
		    io___49.ciunit = *nout;
		    s_wsfe(&io___49);
		    do_fio(&c__1, subnam_ref(0, isub), (ftnlen)6);
		    e_wsfe();
		    if (*nlda > 1) {
			i__1 = *nlda;
			for (i__ = 1; i__ <= i__1; ++i__) {
			    io___50.ciunit = *nout;
			    s_wsfe(&io___50);
			    do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&ldaval[i__], (ftnlen)
				    sizeof(integer));
			    e_wsfe();
/* L190: */
			}
		    }
		}
		for (itran = 1; itran <= 2; ++itran) {
		    *(unsigned char *)trans = *(unsigned char *)&transs[itran 
			    - 1];
		    i__1 = *nk;
		    for (ik = 1; ik <= i__1; ++ik) {
			if (iside == 1) {
			    n = kval[ik];
			    io___51.ciunit = *nout;
			    s_wsfe(&io___51);
			    do_fio(&c__1, subnam_ref(0, isub), (ftnlen)6);
			    do_fio(&c__1, side, (ftnlen)1);
			    do_fio(&c__1, trans, (ftnlen)1);
			    do_fio(&c__1, "N", (ftnlen)1);
			    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer))
				    ;
			    e_wsfe();
			    *(unsigned char *)labm = 'M';
			} else {
			    m = kval[ik];
			    io___53.ciunit = *nout;
			    s_wsfe(&io___53);
			    do_fio(&c__1, subnam_ref(0, isub), (ftnlen)6);
			    do_fio(&c__1, side, (ftnlen)1);
			    do_fio(&c__1, trans, (ftnlen)1);
			    do_fio(&c__1, "M", (ftnlen)1);
			    do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer))
				    ;
			    e_wsfe();
			    *(unsigned char *)labm = 'N';
			}
			sprtb5_("NB", "K", labm, nnb, &nbval[1], &imx, muse, 
				nuse, nlda, &reslts_ref(1, 1, 1, i4), ldr1, 
				ldr2, nout, (ftnlen)2, (ftnlen)1, (ftnlen)1);
			++i4;
/* L200: */
		    }
/* L210: */
		}
/* L220: */
	    }
	} else {
	    io___54.ciunit = *nout;
	    s_wsfe(&io___54);
	    do_fio(&c__1, subnam_ref(0, isub), (ftnlen)6);
	    e_wsfe();
	}
    }
L230:
    return 0;

/*     End of CTIMLQ */

} /* ctimlq_ */

#undef reslts_ref
#undef subnam_ref


