#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__4 = 4;
static integer c__2 = 2;
static integer c__1 = 1;
static integer c__0 = 0;

/* Subroutine */ int ctimpt_(char *line, integer *nm, integer *mval, integer *
	nns, integer *nsval, integer *nlda, integer *ldaval, real *timmin, 
	real *d__, complex *e, complex *b, real *reslts, integer *ldr1, 
	integer *ldr2, integer *ldr3, integer *nout, ftnlen line_len)
{
    /* Initialized data */

    static char subnam[6*4] = "CPTTRF" "CPTTRS" "CPTSV " "CPTSL ";
    static integer iseed[4] = { 0,0,0,1 };

    /* Format strings */
    static char fmt_9999[] = "(1x,a6,\002 timing run not attempted\002,/)";
    static char fmt_9997[] = "(/\002 *** Speed of \002,a6,\002 in megaflops "
	    "***\002)";
    static char fmt_9996[] = "(5x,\002line \002,i2,\002 with LDA = \002,i5)";
    static char fmt_9998[] = "(1x,a6,\002 with UPLO = '\002,a1,\002'\002,/)";

    /* System generated locals */
    integer reslts_dim1, reslts_dim2, reslts_dim3, reslts_offset, i__1, i__2, 
	    i__3, i__4;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void),
	     s_wsle(cilist *), e_wsle(void);

    /* Local variables */
    static integer ilda, ioff, info;
    static char path[3];
    static real time;
    static integer isub, nrhs;
    static char uplo[1];
    static integer i__, j, m, n;
    static char cname[6];
    static integer laval[1];
    extern doublereal sopla_(char *, integer *, integer *, integer *, integer 
	    *, integer *);
    extern /* Subroutine */ int cptsl_(integer *, complex *, complex *, 
	    complex *);
    static integer iuplo;
    extern /* Subroutine */ int cptsv_(integer *, integer *, real *, complex *
	    , complex *, integer *, integer *);
    static real s1, s2;
    static integer ic, im;
    extern /* Subroutine */ int atimck_(integer *, char *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, ftnlen);
    extern doublereal second_(void);
    extern /* Subroutine */ int ctimmg_(integer *, integer *, integer *, 
	    complex *, integer *, integer *, integer *), atimin_(char *, char 
	    *, integer *, char *, logical *, integer *, integer *, ftnlen, 
	    ftnlen, ftnlen), clarnv_(integer *, integer *, integer *, complex 
	    *);
    static real untime;
    extern doublereal smflop_(real *, real *, integer *);
    static logical timsub[4];
    extern /* Subroutine */ int cpttrf_(integer *, real *, complex *, integer 
	    *), sprtbl_(char *, char *, integer *, integer *, integer *, 
	    integer *, integer *, real *, integer *, integer *, integer *, 
	    ftnlen, ftnlen), cpttrs_(char *, integer *, integer *, real *, 
	    complex *, complex *, integer *, integer *);
    static integer ldb, icl;
    static real ops;

    /* Fortran I/O blocks */
    static cilist io___8 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___27 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___28 = { 0, 0, 0, fmt_9996, 0 };
    static cilist io___29 = { 0, 0, 0, 0, 0 };
    static cilist io___31 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___32 = { 0, 0, 0, fmt_9998, 0 };



#define subnam_ref(a_0,a_1) &subnam[(a_1)*6 + a_0 - 6]
#define reslts_ref(a_1,a_2,a_3,a_4) reslts[(((a_4)*reslts_dim3 + (a_3))*\
reslts_dim2 + (a_2))*reslts_dim1 + a_1]


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       March 31, 1993   


    Purpose   
    =======   

    CTIMPT times CPTTRF, -TRS, -SV, and -SL.   

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

    NNS     (input) INTEGER   
            The number of values of NRHS contained in the vector NSVAL.   

    NSVAL   (input) INTEGER array, dimension (NNS)   
            The values of the number of right hand sides NRHS.   

    NLDA    (input) INTEGER   
            The number of values of LDA contained in the vector LDAVAL.   

    LDAVAL  (input) INTEGER array, dimension (NLDA)   
            The values of the leading dimension of the array A.   

    TIMMIN  (input) REAL   
            The minimum time a subroutine will be timed.   

    D       (workspace) REAL array, dimension (NMAX)   
            where NMAX is the maximum value permitted for N.   

    E       (workspace) COMPLEX array, dimension (2*NMAX)   
            where NMAX is the maximum value permitted for N.   

    B       (workspace) COMPLEX array, dimension (LDAMAX*NMAX)   

    RESLTS  (output) REAL array, dimension   
                     (LDR1,LDR2,LDR3,NSUBS+1)   
            The timing results for each subroutine over the relevant   
            values of N.   

    LDR1    (input) INTEGER   
            The first dimension of RESLTS.  LDR1 >= 1.   

    LDR2    (input) INTEGER   
            The second dimension of RESLTS.  LDR2 >= max(1,NM).   

    LDR3    (input) INTEGER   
            The third dimension of RESLTS.  LDR3 >= max(1,NLDA).   

    NOUT    (input) INTEGER   
            The unit number for output.   

    =====================================================================   

       Parameter adjustments */
    --mval;
    --nsval;
    --ldaval;
    --d__;
    --e;
    --b;
    reslts_dim1 = *ldr1;
    reslts_dim2 = *ldr2;
    reslts_dim3 = *ldr3;
    reslts_offset = 1 + reslts_dim1 * (1 + reslts_dim2 * (1 + reslts_dim3 * 1)
	    );
    reslts -= reslts_offset;

    /* Function Body   

       Extract the timing request from the input line. */

    s_copy(path, "Complex precision", (ftnlen)1, (ftnlen)17);
    s_copy(path + 1, "PT", (ftnlen)2, (ftnlen)2);
    atimin_(path, line, &c__4, subnam, timsub, nout, &info, (ftnlen)3, (
	    ftnlen)80, (ftnlen)6);
    if (info != 0) {
	goto L280;
    }

/*     Check that N <= LDA for the input values. */

    for (isub = 2; isub <= 4; ++isub) {
	if (! timsub[isub - 1]) {
	    goto L10;
	}
	s_copy(cname, subnam_ref(0, isub), (ftnlen)6, (ftnlen)6);
	atimck_(&c__2, cname, nm, &mval[1], nlda, &ldaval[1], nout, &info, (
		ftnlen)6);
	if (info > 0) {
	    io___8.ciunit = *nout;
	    s_wsfe(&io___8);
	    do_fio(&c__1, cname, (ftnlen)6);
	    e_wsfe();
	    timsub[isub - 1] = FALSE_;
	}
L10:
	;
    }

/*     Do for each value of M: */

    i__1 = *nm;
    for (im = 1; im <= i__1; ++im) {

	m = mval[im];
	n = max(m,1);

/*        Time CPTTRF */

	if (timsub[0]) {
	    i__2 = n;
	    for (j = 1; j <= i__2; ++j) {
		d__[j] = 3.f;
/* L20: */
	    }
	    i__2 = n - 1;
	    clarnv_(&c__2, iseed, &i__2, &e[1]);
	    ic = 0;
	    s1 = second_();
L30:
	    cpttrf_(&m, &d__[1], &e[1], &info);
	    s2 = second_();
	    time = s2 - s1;
	    ++ic;
	    if (time < *timmin) {
		i__2 = n;
		for (j = 1; j <= i__2; ++j) {
		    d__[j] = 3.f;
/* L40: */
		}
		i__2 = n - 1;
		clarnv_(&c__2, iseed, &i__2, &e[1]);
		goto L30;
	    }

/*           Subtract the time used in STIMMG. */

	    icl = 1;
	    s1 = second_();
L50:
	    s2 = second_();
	    untime = s2 - s1;
	    ++icl;
	    if (icl <= ic) {
		i__2 = n;
		for (j = 1; j <= i__2; ++j) {
		    d__[j] = 3.f;
/* L60: */
		}
		i__2 = n - 1;
		clarnv_(&c__2, iseed, &i__2, &e[1]);
		goto L50;
	    }

	    time = (time - untime) / (real) ic;
	    ops = sopla_("CPTTRF", &m, &c__0, &c__0, &c__0, &c__0);
	    reslts_ref(1, im, 1, 1) = smflop_(&ops, &time, &info);

	} else {
	    ic = 0;
	    i__2 = n;
	    for (j = 1; j <= i__2; ++j) {
		d__[j] = 3.f;
/* L70: */
	    }
	    i__2 = n - 1;
	    clarnv_(&c__2, iseed, &i__2, &e[1]);
	}

/*        Generate another matrix and factor it using CPTTRF so   
          that the factored form can be used in timing the other   
          routines. */

	if (ic != 1) {
	    cpttrf_(&m, &d__[1], &e[1], &info);
	}

/*        Time CPTTRS */

	for (iuplo = 1; iuplo <= 2; ++iuplo) {
	    if (iuplo == 1) {
		*(unsigned char *)uplo = 'U';
		ioff = 0;
	    } else {
		*(unsigned char *)uplo = 'L';
		ioff = 3;
	    }
	    if (timsub[1]) {
		i__2 = *nlda;
		for (ilda = 1; ilda <= i__2; ++ilda) {
		    ldb = ldaval[ilda];
		    i__3 = *nns;
		    for (i__ = 1; i__ <= i__3; ++i__) {
			nrhs = nsval[i__];
			ctimmg_(&c__0, &m, &nrhs, &b[1], &ldb, &c__0, &c__0);
			ic = 0;
			s1 = second_();
L80:
			cpttrs_(uplo, &m, &nrhs, &d__[1], &e[1], &b[1], &ldb, 
				&info);
			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    ctimmg_(&c__0, &m, &nrhs, &b[1], &ldb, &c__0, &
				    c__0);
			    goto L80;
			}

/*                    Subtract the time used in STIMMG. */

			icl = 1;
			s1 = second_();
L90:
			s2 = second_();
			untime = s2 - s1;
			++icl;
			if (icl <= ic) {
			    ctimmg_(&c__0, &m, &nrhs, &b[1], &ldb, &c__0, &
				    c__0);
			    goto L90;
			}

			time = (time - untime) / (real) ic;
			ops = sopla_("CPTTRS", &m, &nrhs, &c__0, &c__0, &c__0);
			reslts_ref(i__, im, ilda, ioff + 2) = smflop_(&ops, &
				time, &info);
/* L100: */
		    }
/* L110: */
		}
	    }

	    if (timsub[2] && iuplo == 1) {
		i__2 = *nlda;
		for (ilda = 1; ilda <= i__2; ++ilda) {
		    ldb = ldaval[ilda];
		    i__3 = *nns;
		    for (i__ = 1; i__ <= i__3; ++i__) {
			nrhs = nsval[i__];
			i__4 = n;
			for (j = 1; j <= i__4; ++j) {
			    d__[j] = 3.f;
/* L120: */
			}
			i__4 = n - 1;
			clarnv_(&c__2, iseed, &i__4, &e[1]);
			ctimmg_(&c__0, &m, &nrhs, &b[1], &ldb, &c__0, &c__0);
			ic = 0;
			s1 = second_();
L130:
/*                     CALL CPTSV( UPLO, M, NRHS, D, E, B, LDB, INFO ) */
			cptsv_(&m, &nrhs, &d__[1], &e[1], &b[1], &ldb, &info);
			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    i__4 = n;
			    for (j = 1; j <= i__4; ++j) {
				d__[j] = 3.f;
/* L140: */
			    }
			    i__4 = n - 1;
			    clarnv_(&c__2, iseed, &i__4, &e[1]);
			    ctimmg_(&c__0, &m, &nrhs, &b[1], &ldb, &c__0, &
				    c__0);
			    goto L130;
			}

/*                    Subtract the time used in CTIMMG. */

			icl = 1;
			s1 = second_();
L150:
			s2 = second_();
			untime = s2 - s1;
			++icl;
			if (icl <= ic) {
			    i__4 = n;
			    for (j = 1; j <= i__4; ++j) {
				d__[j] = 3.f;
/* L160: */
			    }
			    i__4 = n - 1;
			    clarnv_(&c__2, iseed, &i__4, &e[1]);
			    ctimmg_(&c__0, &m, &nrhs, &b[1], &ldb, &c__0, &
				    c__0);
			    goto L150;
			}

			time = (time - untime) / (real) ic;
			ops = sopla_("CPTSV ", &m, &nrhs, &c__0, &c__0, &c__0);
			reslts_ref(i__, im, ilda, ioff + 3) = smflop_(&ops, &
				time, &info);
/* L170: */
		    }
/* L180: */
		}
	    }
/* L190: */
	}

	if (timsub[3]) {
	    i__2 = n;
	    for (j = 1; j <= i__2; ++j) {
		i__3 = j;
		e[i__3].r = 3.f, e[i__3].i = 0.f;
/* L200: */
	    }
	    i__2 = n - 1;
	    clarnv_(&c__2, iseed, &i__2, &e[n + 1]);
	    ctimmg_(&c__0, &m, &c__1, &b[1], &n, &c__0, &c__0);
	    ic = 0;
	    s1 = second_();
L210:
	    cptsl_(&m, &e[1], &e[n + 1], &b[1]);
	    s2 = second_();
	    time = s2 - s1;
	    ++ic;
	    if (time < *timmin) {
		i__2 = n;
		for (j = 1; j <= i__2; ++j) {
		    i__3 = j;
		    e[i__3].r = 3.f, e[i__3].i = 0.f;
/* L220: */
		}
		i__2 = n - 1;
		clarnv_(&c__2, iseed, &i__2, &e[n + 1]);
		ctimmg_(&c__0, &m, &c__1, &b[1], &n, &c__0, &c__0);
		goto L210;
	    }

/*           Subtract the time used in STIMMG. */

	    icl = 1;
	    s1 = second_();
L230:
	    s2 = second_();
	    untime = s2 - s1;
	    ++icl;
	    if (icl <= ic) {
		i__2 = n;
		for (j = 1; j <= i__2; ++j) {
		    i__3 = j;
		    e[i__3].r = 3.f, e[i__3].i = 0.f;
/* L240: */
		}
		i__2 = n - 1;
		clarnv_(&c__2, iseed, &i__2, &e[n + 1]);
		ctimmg_(&c__0, &m, &c__1, &b[1], &n, &c__0, &c__0);
		goto L230;
	    }

	    time = (time - untime) / (real) ic;
	    ops = sopla_("CPTSV ", &m, &c__1, &c__0, &c__0, &c__0);
	    reslts_ref(1, im, 1, 4) = smflop_(&ops, &time, &info);
	}
/* L250: */
    }

/*     Print a table of results for each timed routine. */

    for (isub = 1; isub <= 4; ++isub) {
	if (! timsub[isub - 1]) {
	    goto L270;
	}
	io___27.ciunit = *nout;
	s_wsfe(&io___27);
	do_fio(&c__1, subnam_ref(0, isub), (ftnlen)6);
	e_wsfe();
	if (*nlda > 1 && (timsub[1] || timsub[2])) {
	    i__1 = *nlda;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		io___28.ciunit = *nout;
		s_wsfe(&io___28);
		do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&ldaval[i__], (ftnlen)sizeof(integer));
		e_wsfe();
/* L260: */
	    }
	}
	io___29.ciunit = *nout;
	s_wsle(&io___29);
	e_wsle();
	if (isub == 1) {
	    sprtbl_(" ", "N", &c__1, laval, nm, &mval[1], &c__1, &reslts[
		    reslts_offset], ldr1, ldr2, nout, (ftnlen)1, (ftnlen)1);
	} else if (isub == 2) {
	    io___31.ciunit = *nout;
	    s_wsfe(&io___31);
	    do_fio(&c__1, "CPTTRS", (ftnlen)6);
	    do_fio(&c__1, "U", (ftnlen)1);
	    e_wsfe();
	    sprtbl_("NRHS", "N", nns, &nsval[1], nm, &mval[1], nlda, &
		    reslts_ref(1, 1, 1, 2), ldr1, ldr2, nout, (ftnlen)4, (
		    ftnlen)1);
	    io___32.ciunit = *nout;
	    s_wsfe(&io___32);
	    do_fio(&c__1, "CPTTRS", (ftnlen)6);
	    do_fio(&c__1, "L", (ftnlen)1);
	    e_wsfe();
	    sprtbl_("NRHS", "N", nns, &nsval[1], nm, &mval[1], nlda, &
		    reslts_ref(1, 1, 1, 5), ldr1, ldr2, nout, (ftnlen)4, (
		    ftnlen)1);
	} else if (isub == 3) {
/*            WRITE( NOUT, FMT = 126 ) 'CPTSV ', 'U' */
	    sprtbl_("NRHS", "N", nns, &nsval[1], nm, &mval[1], nlda, &
		    reslts_ref(1, 1, 1, 3), ldr1, ldr2, nout, (ftnlen)4, (
		    ftnlen)1);
/*            WRITE( NOUT, FMT = 126 ) 'CPTSV ', 'L'   
              CALL SPRTBL( 'NRHS', 'N', NNS, NSVAL, NM, MVAL, NLDA,   
       $                   RESLTS( 1, 1, 1, 6 ), LDR1, LDR2, NOUT ) */
	} else if (isub == 4) {
	    sprtbl_(" ", "N", &c__1, laval, nm, &mval[1], &c__1, &reslts_ref(
		    1, 1, 1, 4), ldr1, ldr2, nout, (ftnlen)1, (ftnlen)1);
	}
L270:
	;
    }

L280:
    return 0;

/*     End of CTIMPT */

} /* ctimpt_ */

#undef reslts_ref
#undef subnam_ref


