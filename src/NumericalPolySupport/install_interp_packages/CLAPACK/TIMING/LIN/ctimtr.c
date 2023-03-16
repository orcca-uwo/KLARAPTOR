#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__1 = 1;
static integer c__0 = 0;

/* Subroutine */ int ctimtr_(char *line, integer *nn, integer *nval, integer *
	nns, integer *nsval, integer *nnb, integer *nbval, integer *nlda, 
	integer *ldaval, real *timmin, complex *a, complex *b, real *reslts, 
	integer *ldr1, integer *ldr2, integer *ldr3, integer *nout, ftnlen 
	line_len)
{
    /* Initialized data */

    static char subnam[6*2] = "CTRTRI" "CTRTRS";
    static char uplos[1*2] = "U" "L";

    /* Format strings */
    static char fmt_9999[] = "(1x,a6,\002 timing run not attempted\002,/)";
    static char fmt_9998[] = "(/\002 *** Speed of \002,a6,\002 in megaflops "
	    "***\002)";
    static char fmt_9997[] = "(5x,\002line \002,i2,\002 with LDA = \002,i5)";
    static char fmt_9996[] = "(5x,a6,\002 with UPLO = '\002,a1,\002'\002,/)";

    /* System generated locals */
    integer reslts_dim1, reslts_dim2, reslts_dim3, reslts_offset, i__1, i__2, 
	    i__3;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void),
	     s_wsle(cilist *), e_wsle(void);

    /* Local variables */
    static integer ilda, info;
    static char path[3];
    static real time;
    static integer isub, nrhs;
    static char uplo[1];
    static integer i__, n;
    static char cname[6];
    extern logical lsame_(char *, char *);
    extern doublereal sopla_(char *, integer *, integer *, integer *, integer 
	    *, integer *);
    static integer iuplo, i3;
    static real s1, s2;
    static integer ic, nb, in;
    extern /* Subroutine */ int atimck_(integer *, char *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, ftnlen);
    extern doublereal second_(void);
    extern /* Subroutine */ int ctimmg_(integer *, integer *, integer *, 
	    complex *, integer *, integer *, integer *), atimin_(char *, char 
	    *, integer *, char *, logical *, integer *, integer *, ftnlen, 
	    ftnlen, ftnlen), xlaenv_(integer *, integer *);
    extern doublereal smflop_(real *, real *, integer *);
    static real untime;
    static logical timsub[2];
    extern /* Subroutine */ int sprtbl_(char *, char *, integer *, integer *, 
	    integer *, integer *, integer *, real *, integer *, integer *, 
	    integer *, ftnlen, ftnlen), ctrtri_(char *, char *, integer *, 
	    complex *, integer *, integer *), ctrtrs_(char *, 
	    char *, char *, integer *, integer *, complex *, integer *, 
	    complex *, integer *, integer *);
    static integer lda, ldb, icl, inb, mat;
    static real ops;

    /* Fortran I/O blocks */
    static cilist io___7 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___29 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___30 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___31 = { 0, 0, 0, 0, 0 };
    static cilist io___32 = { 0, 0, 0, fmt_9996, 0 };



#define subnam_ref(a_0,a_1) &subnam[(a_1)*6 + a_0 - 6]
#define reslts_ref(a_1,a_2,a_3,a_4) reslts[(((a_4)*reslts_dim3 + (a_3))*\
reslts_dim2 + (a_2))*reslts_dim1 + a_1]


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       March 31, 1993   


    Purpose   
    =======   

    CTIMTR times CTRTRI and -TRS.   

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
    --nsval;
    --nbval;
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
    s_copy(path + 1, "TR", (ftnlen)2, (ftnlen)2);
    atimin_(path, line, &c__2, subnam, timsub, nout, &info, (ftnlen)3, (
	    ftnlen)80, (ftnlen)6);
    if (info != 0) {
	goto L130;
    }

/*     Check that N <= LDA for the input values. */

    s_copy(cname, line, (ftnlen)6, (ftnlen)6);
    atimck_(&c__2, cname, nn, &nval[1], nlda, &ldaval[1], nout, &info, (
	    ftnlen)6);
    if (info > 0) {
	io___7.ciunit = *nout;
	s_wsfe(&io___7);
	do_fio(&c__1, cname, (ftnlen)6);
	e_wsfe();
	goto L130;
    }

/*     Do first for UPLO = 'U', then for UPLO = 'L' */

    for (iuplo = 1; iuplo <= 2; ++iuplo) {
	*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];
	if (lsame_(uplo, "U")) {
	    mat = 11;
	} else {
	    mat = -11;
	}

/*        Do for each value of N: */

	i__1 = *nn;
	for (in = 1; in <= i__1; ++in) {
	    n = nval[in];

/*           Do for each value of LDA: */

	    i__2 = *nlda;
	    for (ilda = 1; ilda <= i__2; ++ilda) {
		lda = ldaval[ilda];
		i3 = (iuplo - 1) * *nlda + ilda;

/*              Do for each value of NB in NBVAL.  Only the blocked   
                routines are timed in this loop since the other routines   
                are independent of NB. */

		if (timsub[0]) {
		    i__3 = *nnb;
		    for (inb = 1; inb <= i__3; ++inb) {
			nb = nbval[inb];
			xlaenv_(&c__1, &nb);

/*                    Time CTRTRI */

			ctimmg_(&mat, &n, &n, &a[1], &lda, &c__0, &c__0);
			ic = 0;
			s1 = second_();
L10:
			ctrtri_(uplo, "Non-unit", &n, &a[1], &lda, &info);
			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    ctimmg_(&mat, &n, &n, &a[1], &lda, &c__0, &c__0);
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
			    ctimmg_(&mat, &n, &n, &a[1], &lda, &c__0, &c__0);
			    goto L20;
			}

			time = (time - untime) / (real) ic;
			ops = sopla_("CTRTRI", &n, &n, &c__0, &c__0, &nb);
			reslts_ref(inb, in, i3, 1) = smflop_(&ops, &time, &
				info);
/* L30: */
		    }
		} else {

/*                 Generate a triangular matrix A. */

		    ctimmg_(&mat, &n, &n, &a[1], &lda, &c__0, &c__0);
		}

/*              Time CTRTRS */

		if (timsub[1]) {
		    i__3 = *nns;
		    for (i__ = 1; i__ <= i__3; ++i__) {
			nrhs = nsval[i__];
			ldb = lda;
			ctimmg_(&c__0, &n, &nrhs, &b[1], &ldb, &c__0, &c__0);
			ic = 0;
			s1 = second_();
L40:
			ctrtrs_(uplo, "No transpose", "Non-unit", &n, &nrhs, &
				a[1], &lda, &b[1], &ldb, &info);
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
			ops = sopla_("CTRTRS", &n, &nrhs, &c__0, &c__0, &c__0);
			reslts_ref(i__, in, i3, 2) = smflop_(&ops, &time, &
				info);
/* L60: */
		    }
		}
/* L70: */
	    }
/* L80: */
	}
/* L90: */
    }

/*     Print a table of results. */

    for (isub = 1; isub <= 2; ++isub) {
	if (! timsub[isub - 1]) {
	    goto L120;
	}
	io___29.ciunit = *nout;
	s_wsfe(&io___29);
	do_fio(&c__1, subnam_ref(0, isub), (ftnlen)6);
	e_wsfe();
	if (*nlda > 1) {
	    i__1 = *nlda;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		io___30.ciunit = *nout;
		s_wsfe(&io___30);
		do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&ldaval[i__], (ftnlen)sizeof(integer));
		e_wsfe();
/* L100: */
	    }
	}
	io___31.ciunit = *nout;
	s_wsle(&io___31);
	e_wsle();
	for (iuplo = 1; iuplo <= 2; ++iuplo) {
	    io___32.ciunit = *nout;
	    s_wsfe(&io___32);
	    do_fio(&c__1, subnam_ref(0, isub), (ftnlen)6);
	    do_fio(&c__1, uplos + (iuplo - 1), (ftnlen)1);
	    e_wsfe();
	    i3 = (iuplo - 1) * *nlda + 1;
	    if (isub == 1) {
		sprtbl_("NB", "N", nnb, &nbval[1], nn, &nval[1], nlda, &
			reslts_ref(1, 1, i3, 1), ldr1, ldr2, nout, (ftnlen)2, 
			(ftnlen)1);
	    } else if (isub == 2) {
		sprtbl_("NRHS", "N", nns, &nsval[1], nn, &nval[1], nlda, &
			reslts_ref(1, 1, i3, 2), ldr1, ldr2, nout, (ftnlen)4, 
			(ftnlen)1);
	    }
/* L110: */
	}
L120:
	;
    }

L130:
    return 0;

/*     End of CTIMTR */

} /* ctimtr_ */

#undef reslts_ref
#undef subnam_ref


