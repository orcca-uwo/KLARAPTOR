#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__4 = 4;
static integer c__1 = 1;
static integer c__0 = 0;

/* Subroutine */ int stimsp_(char *line, integer *nn, integer *nval, integer *
	nns, integer *nsval, integer *la, real *timmin, real *a, real *b, 
	real *work, integer *iwork, real *reslts, integer *ldr1, integer *
	ldr2, integer *ldr3, integer *nout, ftnlen line_len)
{
    /* Initialized data */

    static char uplos[1*2] = "U" "L";
    static char subnam[6*3] = "SSPTRF" "SSPTRS" "SSPTRI";

    /* Format strings */
    static char fmt_9999[] = "(1x,a6,\002 timing run not attempted\002,/)";
    static char fmt_9998[] = "(/\002 *** Speed of \002,a6,\002 in megaflops "
	    "***\002,/)";
    static char fmt_9997[] = "(5x,a6,\002 with UPLO = '\002,a1,\002'\002,/)";

    /* System generated locals */
    integer reslts_dim1, reslts_dim2, reslts_dim3, reslts_offset, i__1, i__2;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static integer info;
    static char path[3];
    static real time;
    static integer isub, nrhs;
    static char uplo[1];
    static integer i__, n;
    static char cname[6];
    static integer laval[1];
    extern logical lsame_(char *, char *);
    extern doublereal sopla_(char *, integer *, integer *, integer *, integer 
	    *, integer *);
    static integer iuplo;
    extern /* Subroutine */ int scopy_(integer *, real *, integer *, real *, 
	    integer *);
    static real s1, s2;
    static integer ic, in;
    extern /* Subroutine */ int atimck_(integer *, char *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, ftnlen);
    extern doublereal second_(void);
    extern /* Subroutine */ int atimin_(char *, char *, integer *, char *, 
	    logical *, integer *, integer *, ftnlen, ftnlen, ftnlen);
    extern doublereal smflop_(real *, real *, integer *);
    static real untime;
    extern /* Subroutine */ int stimmg_(integer *, integer *, integer *, real 
	    *, integer *, integer *, integer *);
    static logical timsub[3];
    extern /* Subroutine */ int sprtbl_(char *, char *, integer *, integer *, 
	    integer *, integer *, integer *, real *, integer *, integer *, 
	    integer *, ftnlen, ftnlen), ssptrf_(char *, integer *, real *, 
	    integer *, integer *), ssptri_(char *, integer *, real *, 
	    integer *, real *, integer *), ssptrs_(char *, integer *, 
	    integer *, real *, integer *, real *, integer *, integer *);
    static integer lda, ldb, icl, mat;
    static real ops;

    /* Fortran I/O blocks */
    static cilist io___8 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___26 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___27 = { 0, 0, 0, fmt_9997, 0 };



#define subnam_ref(a_0,a_1) &subnam[(a_1)*6 + a_0 - 6]
#define reslts_ref(a_1,a_2,a_3,a_4) reslts[(((a_4)*reslts_dim3 + (a_3))*\
reslts_dim2 + (a_2))*reslts_dim1 + a_1]


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       March 31, 1993   


    Purpose   
    =======   

    STIMSP times SSPTRF, -TRS, and -TRI.   

    Arguments   
    =========   

    LINE    (input) CHARACTER*80   
            The input line that requested this routine.  The first six   
            characters contain either the name of a subroutine or a   
            generic path name.  The remaining characters may be used to   
            specify the individual routines to be timed.  See ATIMIN for   
            a full description of the format of the input line.   

    NN      (input) INTEGER   
            The number of values of N contained in the vector NVAL.   

    NVAL    (input) INTEGER array, dimension (NN)   
            The values of the matrix size N.   

    NNS     (input) INTEGER   
            The number of values of NRHS contained in the vector NSVAL.   

    NSVAL   (input) INTEGER array, dimension (NNS)   
            The values of the number of right hand sides NRHS.   

    LA      (input) INTEGER   
            The size of the arrays A, B, and C.   

    TIMMIN  (input) REAL   
            The minimum time a subroutine will be timed.   

    A       (workspace) REAL array, dimension (LA)   

    B       (workspace) REAL array, dimension (LA)   

    WORK    (workspace) REAL array, dimension (NMAX)   

    IWORK   (workspace) INTEGER array, dimension (NMAX)   
            where NMAX is the maximum value of N permitted.   

    RESLTS  (output) REAL array, dimension   
                     (LDR1,LDR2,LDR3,NSUBS)   
            The timing results for each subroutine over the relevant   
            values of N.   

    LDR1    (input) INTEGER   
            The first dimension of RESLTS.  LDR1 >= max(4,NNB).   

    LDR2    (input) INTEGER   
            The second dimension of RESLTS.  LDR2 >= max(1,NN).   

    LDR3    (input) INTEGER   
            The third dimension of RESLTS.  LDR3 >= 2.   

    NOUT    (input) INTEGER   
            The unit number for output.   

    =====================================================================   

       Parameter adjustments */
    --nval;
    --nsval;
    --a;
    --b;
    --work;
    --iwork;
    reslts_dim1 = *ldr1;
    reslts_dim2 = *ldr2;
    reslts_dim3 = *ldr3;
    reslts_offset = 1 + reslts_dim1 * (1 + reslts_dim2 * (1 + reslts_dim3 * 1)
	    );
    reslts -= reslts_offset;

    /* Function Body   

       Extract the timing request from the input line. */

    s_copy(path, "Single precision", (ftnlen)1, (ftnlen)16);
    s_copy(path + 1, "SP", (ftnlen)2, (ftnlen)2);
    atimin_(path, line, &c__3, subnam, timsub, nout, &info, (ftnlen)3, (
	    ftnlen)80, (ftnlen)6);
    if (info != 0) {
	goto L120;
    }

/*     Check that N*(N+1)/2 <= LA for the input values. */

    s_copy(cname, line, (ftnlen)6, (ftnlen)6);
    laval[0] = *la;
    atimck_(&c__4, cname, nn, &nval[1], &c__1, laval, nout, &info, (ftnlen)6);
    if (info > 0) {
	io___8.ciunit = *nout;
	s_wsfe(&io___8);
	do_fio(&c__1, cname, (ftnlen)6);
	e_wsfe();
	goto L120;
    }

/*     Do first for UPLO = 'U', then for UPLO = 'L' */

    for (iuplo = 1; iuplo <= 2; ++iuplo) {
	*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];
	if (lsame_(uplo, "U")) {
	    mat = 7;
	} else {
	    mat = -7;
	}

/*        Do for each value of N in NVAL. */

	i__1 = *nn;
	for (in = 1; in <= i__1; ++in) {
	    n = nval[in];
	    lda = n * (n + 1) / 2;

/*           Time SSPTRF */

	    if (timsub[0]) {
		stimmg_(&mat, &n, &n, &a[1], &lda, &c__0, &c__0);
		ic = 0;
		s1 = second_();
L10:
		ssptrf_(uplo, &n, &a[1], &iwork[1], &info);
		s2 = second_();
		time = s2 - s1;
		++ic;
		if (time < *timmin) {
		    stimmg_(&mat, &n, &n, &a[1], &lda, &c__0, &c__0);
		    goto L10;
		}

/*              Subtract the time used in STIMMG. */

		icl = 1;
		s1 = second_();
L20:
		s2 = second_();
		untime = s2 - s1;
		++icl;
		if (icl <= ic) {
		    stimmg_(&mat, &n, &n, &a[1], &lda, &c__0, &c__0);
		    goto L20;
		}

		time = (time - untime) / (real) ic;
		ops = sopla_("SSPTRF", &n, &n, &c__0, &c__0, &c__0)
			;
		reslts_ref(1, in, iuplo, 1) = smflop_(&ops, &time, &info);

	    } else {
		ic = 0;
		stimmg_(&mat, &n, &n, &a[1], &lda, &c__0, &c__0);
	    }

/*           Generate another matrix and factor it using SSPTRF so   
             that the factored form can be used in timing the other   
             routines. */

	    if (ic != 1) {
		ssptrf_(uplo, &n, &a[1], &iwork[1], &info);
	    }

/*           Time SSPTRI */

	    if (timsub[2]) {
		scopy_(&lda, &a[1], &c__1, &b[1], &c__1);
		ic = 0;
		s1 = second_();
L30:
		ssptri_(uplo, &n, &b[1], &iwork[1], &work[1], &info);
		s2 = second_();
		time = s2 - s1;
		++ic;
		if (time < *timmin) {
		    scopy_(&lda, &a[1], &c__1, &b[1], &c__1);
		    goto L30;
		}

/*              Subtract the time used in SCOPY. */

		icl = 1;
		s1 = second_();
L40:
		s2 = second_();
		untime = s2 - s1;
		++icl;
		if (icl <= ic) {
		    scopy_(&lda, &a[1], &c__1, &b[1], &c__1);
		    goto L40;
		}

		time = (time - untime) / (real) ic;
		ops = sopla_("SSPTRI", &n, &n, &c__0, &c__0, &c__0)
			;
		reslts_ref(1, in, iuplo, 3) = smflop_(&ops, &time, &info);
	    }

/*           Time SSPTRS */

	    if (timsub[1]) {
		i__2 = *nns;
		for (i__ = 1; i__ <= i__2; ++i__) {
		    nrhs = nsval[i__];
		    ldb = n;
		    if (ldb % 2 == 0) {
			++ldb;
		    }
		    stimmg_(&c__0, &n, &nrhs, &b[1], &ldb, &c__0, &c__0);
		    ic = 0;
		    s1 = second_();
L50:
		    ssptrs_(uplo, &n, &nrhs, &a[1], &iwork[1], &b[1], &ldb, &
			    info);
		    s2 = second_();
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			stimmg_(&c__0, &n, &nrhs, &b[1], &ldb, &c__0, &c__0);
			goto L50;
		    }

/*                 Subtract the time used in STIMMG. */

		    icl = 1;
		    s1 = second_();
L60:
		    s2 = second_();
		    untime = s2 - s1;
		    ++icl;
		    if (icl <= ic) {
			stimmg_(&c__0, &n, &nrhs, &b[1], &ldb, &c__0, &c__0);
			goto L60;
		    }

		    time = (time - untime) / (real) ic;
		    ops = sopla_("SSPTRS", &n, &nrhs, &c__0, &c__0, &c__0);
		    reslts_ref(i__, in, iuplo, 2) = smflop_(&ops, &time, &
			    info);
/* L70: */
		}
	    }
/* L80: */
	}
/* L90: */
    }

/*     Print tables of results for each timed routine. */

    for (isub = 1; isub <= 3; ++isub) {
	if (! timsub[isub - 1]) {
	    goto L110;
	}
	io___26.ciunit = *nout;
	s_wsfe(&io___26);
	do_fio(&c__1, subnam_ref(0, isub), (ftnlen)6);
	e_wsfe();
	for (iuplo = 1; iuplo <= 2; ++iuplo) {
	    io___27.ciunit = *nout;
	    s_wsfe(&io___27);
	    do_fio(&c__1, subnam_ref(0, isub), (ftnlen)6);
	    do_fio(&c__1, uplos + (iuplo - 1), (ftnlen)1);
	    e_wsfe();
	    if (isub == 1) {
		sprtbl_(" ", "N", &c__1, laval, nn, &nval[1], &c__1, &
			reslts_ref(1, 1, iuplo, 1), ldr1, ldr2, nout, (ftnlen)
			1, (ftnlen)1);
	    } else if (isub == 2) {
		sprtbl_("NRHS", "N", nns, &nsval[1], nn, &nval[1], &c__1, &
			reslts_ref(1, 1, iuplo, 2), ldr1, ldr2, nout, (ftnlen)
			4, (ftnlen)1);
	    } else if (isub == 3) {
		sprtbl_(" ", "N", &c__1, laval, nn, &nval[1], &c__1, &
			reslts_ref(1, 1, iuplo, 3), ldr1, ldr2, nout, (ftnlen)
			1, (ftnlen)1);
	    }
/* L100: */
	}
L110:
	;
    }
L120:
    return 0;

/*     End of STIMSP */

} /* stimsp_ */

#undef reslts_ref
#undef subnam_ref


