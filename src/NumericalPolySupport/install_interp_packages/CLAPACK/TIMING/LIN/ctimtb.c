#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__0 = 0;

/* Subroutine */ int ctimtb_(char *line, integer *nn, integer *nval, integer *
	nk, integer *kval, integer *nns, integer *nsval, integer *nlda, 
	integer *ldaval, real *timmin, complex *a, complex *b, real *reslts, 
	integer *ldr1, integer *ldr2, integer *ldr3, integer *nout, ftnlen 
	line_len)
{
    /* Initialized data */

    static char subnam[6*1] = "CTBTRS";
    static char uplos[1*2] = "U" "L";

    /* Format strings */
    static char fmt_9999[] = "(1x,a6,\002 timing run not attempted\002,/)";
    static char fmt_9998[] = "(/\002 *** Speed of \002,a6,\002 in megaflops "
	    "***\002)";
    static char fmt_9997[] = "(5x,\002with LDA = \002,i5)";
    static char fmt_9996[] = "(5x,\002line \002,i2,\002 with LDA = \002,i5)";
    static char fmt_9995[] = "(/5x,a6,\002 with M =\002,i6,\002, UPLO = '"
	    "\002,a1,\002'\002,/)";

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
    static integer isub, nrhs;
    static char uplo[1];
    static integer i__, k, n;
    static char cname[6];
    extern logical lsame_(char *, char *);
    extern doublereal sopla_(char *, integer *, integer *, integer *, integer 
	    *, integer *);
    static integer iuplo, i3;
    static real s1, s2;
    static integer ic, ik, in;
    extern /* Subroutine */ int atimck_(integer *, char *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, ftnlen);
    extern doublereal second_(void);
    extern /* Subroutine */ int ctimmg_(integer *, integer *, integer *, 
	    complex *, integer *, integer *, integer *), atimin_(char *, char 
	    *, integer *, char *, logical *, integer *, integer *, ftnlen, 
	    ftnlen, ftnlen);
    extern doublereal smflop_(real *, real *, integer *);
    static real untime;
    extern /* Subroutine */ int ctbtrs_(char *, char *, char *, integer *, 
	    integer *, integer *, complex *, integer *, complex *, integer *, 
	    integer *);
    static logical timsub[1];
    extern /* Subroutine */ int sprtbl_(char *, char *, integer *, integer *, 
	    integer *, integer *, integer *, real *, integer *, integer *, 
	    integer *, ftnlen, ftnlen);
    static integer lda, ldb, icl, mat;
    static real ops;

    /* Fortran I/O blocks */
    static cilist io___7 = { 0, 0, 0, fmt_9999, 0 };
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

    CTIMTB times CTBTRS.   

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

    NK      (input) INTEGER   
            The number of values of K contained in the vector KVAL.   

    KVAL    (input) INTEGER array, dimension (NK)   
            The values of the band width K.   

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

    A       (workspace) COMPLEX array, dimension (LDAMAX*NMAX)   
            where LDAMAX and NMAX are the maximum values permitted   
            for LDA and N.   

    B       (workspace) COMPLEX array, dimension (LDAMAX*NMAX)   

    RESLTS  (output) REAL array, dimension   
                     (LDR1,LDR2,LDR3,NSUBS)   
            The timing results for each subroutine over the relevant   
            values of N, NB, and LDA.   

    LDR1    (input) INTEGER   
            The first dimension of RESLTS.  LDR1 >= max(1,NNB).   

    LDR2    (input) INTEGER   
            The second dimension of RESLTS.  LDR2 >= max(1,NN).   

    LDR3    (input) INTEGER   
            The third dimension of RESLTS.  LDR3 >= max(1,2*NLDA).   

    NOUT    (input) INTEGER   
            The unit number for output.   

    =====================================================================   

       Parameter adjustments */
    --nval;
    --kval;
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

    s_copy(path, "Complex precision", (ftnlen)1, (ftnlen)17);
    s_copy(path + 1, "TB", (ftnlen)2, (ftnlen)2);
    atimin_(path, line, &c__1, subnam, timsub, nout, &info, (ftnlen)3, (
	    ftnlen)80, (ftnlen)6);
    if (info != 0) {
	goto L110;
    }

/*     Check that K+1 <= LDA for the input values. */

    s_copy(cname, line, (ftnlen)6, (ftnlen)6);
    atimck_(&c__0, cname, nk, &kval[1], nlda, &ldaval[1], nout, &info, (
	    ftnlen)6);
    if (info > 0) {
	io___7.ciunit = *nout;
	s_wsfe(&io___7);
	do_fio(&c__1, cname, (ftnlen)6);
	e_wsfe();
	goto L110;
    }

/*     Do for each value of N: */

    i__1 = *nn;
    for (in = 1; in <= i__1; ++in) {
	n = nval[in];
	ldb = n;

/*        Do first for UPLO = 'U', then for UPLO = 'L' */

	for (iuplo = 1; iuplo <= 2; ++iuplo) {
	    *(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];
	    if (lsame_(uplo, "U")) {
		mat = 13;
	    } else {
		mat = -13;
	    }

/*           Do for each value of LDA: */

	    i__2 = *nlda;
	    for (ilda = 1; ilda <= i__2; ++ilda) {
		lda = ldaval[ilda];
		i3 = (iuplo - 1) * *nlda + ilda;

/*              Do for each value of the band width K: */

		i__3 = *nk;
		for (ik = 1; ik <= i__3; ++ik) {
		    k = kval[ik];
/* Computing MAX   
   Computing MIN */
		    i__6 = k, i__7 = n - 1;
		    i__4 = 0, i__5 = min(i__6,i__7);
		    k = max(i__4,i__5);

/*                 Time CTBTRS */

		    if (timsub[0]) {
			ctimmg_(&mat, &n, &n, &a[1], &lda, &k, &k);
			i__4 = *nns;
			for (i__ = 1; i__ <= i__4; ++i__) {
			    nrhs = nsval[i__];
			    ctimmg_(&c__0, &n, &nrhs, &b[1], &ldb, &c__0, &
				    c__0);
			    ic = 0;
			    s1 = second_();
L10:
			    ctbtrs_(uplo, "No transpose", "Non-unit", &n, &k, 
				    &nrhs, &a[1], &lda, &b[1], &ldb, &info);
			    s2 = second_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				ctimmg_(&c__0, &n, &nrhs, &b[1], &ldb, &c__0, 
					&c__0);
				goto L10;
			    }

/*                       Subtract the time used in CTIMMG. */

			    icl = 1;
			    s1 = second_();
L20:
			    s2 = second_();
			    untime = s2 - s1;
			    ++icl;
			    if (icl <= ic) {
				ctimmg_(&c__0, &n, &nrhs, &b[1], &ldb, &c__0, 
					&c__0);
				goto L20;
			    }

			    time = (time - untime) / (real) ic;
			    ops = sopla_("CTBTRS", &n, &nrhs, &k, &k, &c__0);
			    reslts_ref(i__, ik, i3, 1) = smflop_(&ops, &time, 
				    &info);
/* L30: */
			}
		    }
/* L40: */
		}
/* L50: */
	    }
/* L60: */
	}

/*        Print a table of results. */

	for (isub = 1; isub <= 1; ++isub) {
	    if (! timsub[isub - 1]) {
		goto L90;
	    }

/*           Print header for routine names. */

	    if (in == 1 || s_cmp(cname, "CTB   ", (ftnlen)6, (ftnlen)6) == 0) 
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
/* L70: */
		    }
		}
	    }

	    for (iuplo = 1; iuplo <= 2; ++iuplo) {
		io___32.ciunit = *nout;
		s_wsfe(&io___32);
		do_fio(&c__1, subnam_ref(0, isub), (ftnlen)6);
		do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		do_fio(&c__1, uplos + (iuplo - 1), (ftnlen)1);
		e_wsfe();
		i3 = (iuplo - 1) * *nlda + 1;
		if (isub == 1) {
		    sprtbl_("NRHS", "K", nns, &nsval[1], nk, &kval[1], nlda, &
			    reslts_ref(1, 1, i3, 1), ldr1, ldr2, nout, (
			    ftnlen)4, (ftnlen)1);
		}
/* L80: */
	    }
L90:
	    ;
	}
/* L100: */
    }

L110:
    return 0;

/*     End of CTIMTB */

} /* ctimtb_ */

#undef reslts_ref
#undef subnam_ref


