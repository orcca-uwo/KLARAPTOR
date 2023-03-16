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
static integer c__13 = 13;
static integer c__0 = 0;

/* Subroutine */ int stimpt_(char *line, integer *nm, integer *mval, integer *
	nns, integer *nsval, integer *nlda, integer *ldaval, real *timmin, 
	real *a, real *b, real *reslts, integer *ldr1, integer *ldr2, integer 
	*ldr3, integer *nout, ftnlen line_len)
{
    /* Initialized data */

    static char subnam[6*4] = "SPTTRF" "SPTTRS" "SPTSV " "SPTSL ";

    /* Format strings */
    static char fmt_9999[] = "(1x,a6,\002 timing run not attempted\002,/)";
    static char fmt_9998[] = "(/\002 *** Speed of \002,a6,\002 in megaflops "
	    "***\002)";
    static char fmt_9997[] = "(5x,\002line \002,i2,\002 with LDA = \002,i5)";

    /* System generated locals */
    integer reslts_dim1, reslts_dim2, reslts_dim3, reslts_offset, i__1, i__2, 
	    i__3, i__4;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void),
	     s_wsle(cilist *), e_wsle(void);

    /* Local variables */
    static integer ilda, info;
    static char path[3];
    static real time;
    static integer isub, nrhs, i__, m, n;
    static char cname[6];
    static integer laval[1];
    extern doublereal sopla_(char *, integer *, integer *, integer *, integer 
	    *, integer *);
    static real s1, s2;
    extern /* Subroutine */ int sptsl_(integer *, real *, real *, real *), 
	    sptsv_(integer *, integer *, real *, real *, real *, integer *, 
	    integer *);
    static integer ic, im;
    extern /* Subroutine */ int atimck_(integer *, char *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, ftnlen);
    extern doublereal second_(void);
    extern /* Subroutine */ int atimin_(char *, char *, integer *, char *, 
	    logical *, integer *, integer *, ftnlen, ftnlen, ftnlen);
    extern doublereal smflop_(real *, real *, integer *);
    static real untime;
    extern /* Subroutine */ int stimmg_(integer *, integer *, integer *, real 
	    *, integer *, integer *, integer *);
    static logical timsub[4];
    extern /* Subroutine */ int sprtbl_(char *, char *, integer *, integer *, 
	    integer *, integer *, integer *, real *, integer *, integer *, 
	    integer *, ftnlen, ftnlen), spttrf_(integer *, real *, real *, 
	    integer *), spttrs_(integer *, integer *, real *, real *, real *, 
	    integer *, integer *);
    static integer ldb, icl;
    static real ops;

    /* Fortran I/O blocks */
    static cilist io___7 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___22 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___23 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___24 = { 0, 0, 0, 0, 0 };



#define subnam_ref(a_0,a_1) &subnam[(a_1)*6 + a_0 - 6]
#define reslts_ref(a_1,a_2,a_3,a_4) reslts[(((a_4)*reslts_dim3 + (a_3))*\
reslts_dim2 + (a_2))*reslts_dim1 + a_1]


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       March 31, 1993   


    Purpose   
    =======   

    STIMPT times SPTTRF, -TRS, -SV, and -SL.   

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

    A       (workspace) REAL array, dimension (NMAX*2)   
            where NMAX is the maximum value permitted for N.   

    B       (workspace) REAL array, dimension (LDAMAX*NMAX)   

    RESLTS  (output) REAL array, dimension   
                     (LDR1,LDR2,LDR3,NSUBS)   
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
    --a;
    --b;
    reslts_dim1 = *ldr1;
    reslts_dim2 = *ldr2;
    reslts_dim3 = *ldr3;
    reslts_offset = 1 + reslts_dim1 * (1 + reslts_dim2 * (1 + reslts_dim3 * 1)
	    );
    reslts -= reslts_offset;

    /* Function Body   

       Extract the timing request from the input line. */

    s_copy(path, "Single precision", (ftnlen)1, (ftnlen)16);
    s_copy(path + 1, "PT", (ftnlen)2, (ftnlen)2);
    atimin_(path, line, &c__4, subnam, timsub, nout, &info, (ftnlen)3, (
	    ftnlen)80, (ftnlen)6);
    if (info != 0) {
	goto L170;
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
	    io___7.ciunit = *nout;
	    s_wsfe(&io___7);
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

/*        Time SPTTRF */

	if (timsub[0]) {
	    i__2 = n << 1;
	    stimmg_(&c__13, &m, &m, &a[1], &i__2, &c__0, &c__0);
	    ic = 0;
	    s1 = second_();
L20:
	    spttrf_(&m, &a[1], &a[n + 1], &info);
	    s2 = second_();
	    time = s2 - s1;
	    ++ic;
	    if (time < *timmin) {
		i__2 = n << 1;
		stimmg_(&c__13, &m, &m, &a[1], &i__2, &c__0, &c__0);
		goto L20;
	    }

/*           Subtract the time used in STIMMG. */

	    icl = 1;
	    s1 = second_();
L30:
	    s2 = second_();
	    untime = s2 - s1;
	    ++icl;
	    if (icl <= ic) {
		i__2 = n << 1;
		stimmg_(&c__13, &m, &m, &a[1], &i__2, &c__0, &c__0);
		goto L30;
	    }

	    time = (time - untime) / (real) ic;
	    ops = sopla_("SPTTRF", &m, &c__0, &c__0, &c__0, &c__0);
	    reslts_ref(1, im, 1, 1) = smflop_(&ops, &time, &info);

	} else {
	    ic = 0;
	    i__2 = n << 1;
	    stimmg_(&c__13, &m, &m, &a[1], &i__2, &c__0, &c__0);
	}

/*        Generate another matrix and factor it using SPTTRF so   
          that the factored form can be used in timing the other   
          routines. */

	if (ic != 1) {
	    spttrf_(&m, &a[1], &a[n + 1], &info);
	}

/*        Time SPTTRS */

	if (timsub[1]) {
	    i__2 = *nlda;
	    for (ilda = 1; ilda <= i__2; ++ilda) {
		ldb = ldaval[ilda];
		i__3 = *nns;
		for (i__ = 1; i__ <= i__3; ++i__) {
		    nrhs = nsval[i__];
		    stimmg_(&c__0, &m, &nrhs, &b[1], &ldb, &c__0, &c__0);
		    ic = 0;
		    s1 = second_();
L40:
		    spttrs_(&m, &nrhs, &a[1], &a[n + 1], &b[1], &ldb, &info);
		    s2 = second_();
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			stimmg_(&c__0, &m, &nrhs, &b[1], &ldb, &c__0, &c__0);
			goto L40;
		    }

/*                 Subtract the time used in STIMMG. */

		    icl = 1;
		    s1 = second_();
L50:
		    s2 = second_();
		    untime = s2 - s1;
		    ++icl;
		    if (icl <= ic) {
			stimmg_(&c__0, &m, &nrhs, &b[1], &ldb, &c__0, &c__0);
			goto L50;
		    }

		    time = (time - untime) / (real) ic;
		    ops = sopla_("SPTTRS", &m, &nrhs, &c__0, &c__0, &c__0);
		    reslts_ref(i__, im, ilda, 2) = smflop_(&ops, &time, &info)
			    ;
/* L60: */
		}
/* L70: */
	    }
	}

	if (timsub[2]) {
	    i__2 = *nlda;
	    for (ilda = 1; ilda <= i__2; ++ilda) {
		ldb = ldaval[ilda];
		i__3 = *nns;
		for (i__ = 1; i__ <= i__3; ++i__) {
		    nrhs = nsval[i__];
		    i__4 = n << 1;
		    stimmg_(&c__13, &m, &m, &a[1], &i__4, &c__0, &c__0);
		    stimmg_(&c__0, &m, &nrhs, &b[1], &ldb, &c__0, &c__0);
		    ic = 0;
		    s1 = second_();
L80:
		    sptsv_(&m, &nrhs, &a[1], &a[n + 1], &b[1], &ldb, &info);
		    s2 = second_();
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			i__4 = n << 1;
			stimmg_(&c__13, &m, &m, &a[1], &i__4, &c__0, &c__0);
			stimmg_(&c__0, &m, &nrhs, &b[1], &ldb, &c__0, &c__0);
			goto L80;
		    }

/*                 Subtract the time used in STIMMG. */

		    icl = 1;
		    s1 = second_();
L90:
		    s2 = second_();
		    untime = s2 - s1;
		    ++icl;
		    if (icl <= ic) {
			i__4 = n << 1;
			stimmg_(&c__13, &m, &m, &a[1], &i__4, &c__0, &c__0);
			stimmg_(&c__0, &m, &nrhs, &b[1], &ldb, &c__0, &c__0);
			goto L90;
		    }

		    time = (time - untime) / (real) ic;
		    ops = sopla_("SPTSV ", &m, &nrhs, &c__0, &c__0, &c__0);
		    reslts_ref(i__, im, ilda, 3) = smflop_(&ops, &time, &info)
			    ;
/* L100: */
		}
/* L110: */
	    }
	}

	if (timsub[3]) {
	    i__2 = n << 1;
	    stimmg_(&c__13, &m, &m, &a[1], &i__2, &c__0, &c__0);
	    stimmg_(&c__0, &m, &c__1, &b[1], &n, &c__0, &c__0);
	    ic = 0;
	    s1 = second_();
L120:
	    sptsl_(&m, &a[1], &a[n + 1], &b[1]);
	    s2 = second_();
	    time = s2 - s1;
	    ++ic;
	    if (time < *timmin) {
		i__2 = n << 1;
		stimmg_(&c__13, &m, &m, &a[1], &i__2, &c__0, &c__0);
		stimmg_(&c__0, &m, &c__1, &b[1], &n, &c__0, &c__0);
		goto L120;
	    }

/*           Subtract the time used in STIMMG. */

	    icl = 1;
	    s1 = second_();
L130:
	    s2 = second_();
	    untime = s2 - s1;
	    ++icl;
	    if (icl <= ic) {
		i__2 = n << 1;
		stimmg_(&c__13, &m, &m, &a[1], &i__2, &c__0, &c__0);
		stimmg_(&c__0, &m, &c__1, &b[1], &n, &c__0, &c__0);
		goto L130;
	    }

	    time = (time - untime) / (real) ic;
	    ops = sopla_("SPTSV ", &m, &c__1, &c__0, &c__0, &c__0);
	    reslts_ref(1, im, 1, 4) = smflop_(&ops, &time, &info);
	}
/* L140: */
    }

/*     Print a table of results for each timed routine. */

    for (isub = 1; isub <= 4; ++isub) {
	if (! timsub[isub - 1]) {
	    goto L160;
	}
	io___22.ciunit = *nout;
	s_wsfe(&io___22);
	do_fio(&c__1, subnam_ref(0, isub), (ftnlen)6);
	e_wsfe();
	if (*nlda > 1 && (timsub[1] || timsub[2])) {
	    i__1 = *nlda;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		io___23.ciunit = *nout;
		s_wsfe(&io___23);
		do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&ldaval[i__], (ftnlen)sizeof(integer));
		e_wsfe();
/* L150: */
	    }
	}
	io___24.ciunit = *nout;
	s_wsle(&io___24);
	e_wsle();
	if (isub == 1) {
	    sprtbl_(" ", "N", &c__1, laval, nm, &mval[1], &c__1, &reslts[
		    reslts_offset], ldr1, ldr2, nout, (ftnlen)1, (ftnlen)1);
	} else if (isub == 2) {
	    sprtbl_("NRHS", "N", nns, &nsval[1], nm, &mval[1], nlda, &
		    reslts_ref(1, 1, 1, 2), ldr1, ldr2, nout, (ftnlen)4, (
		    ftnlen)1);
	} else if (isub == 3) {
	    sprtbl_("NRHS", "N", nns, &nsval[1], nm, &mval[1], nlda, &
		    reslts_ref(1, 1, 1, 3), ldr1, ldr2, nout, (ftnlen)4, (
		    ftnlen)1);
	} else if (isub == 4) {
	    sprtbl_(" ", "N", &c__1, laval, nm, &mval[1], &c__1, &reslts_ref(
		    1, 1, 1, 4), ldr1, ldr2, nout, (ftnlen)1, (ftnlen)1);
	}
L160:
	;
    }

L170:
    return 0;

/*     End of STIMPT */

} /* stimpt_ */

#undef reslts_ref
#undef subnam_ref


