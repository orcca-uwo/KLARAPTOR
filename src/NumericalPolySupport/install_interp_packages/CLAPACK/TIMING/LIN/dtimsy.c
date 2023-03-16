#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__2 = 2;
static integer c__1 = 1;
static integer c__0 = 0;

/* Subroutine */ int dtimsy_(char *line, integer *nn, integer *nval, integer *
	nns, integer *nsval, integer *nnb, integer *nbval, integer *nlda, 
	integer *ldaval, doublereal *timmin, doublereal *a, doublereal *b, 
	doublereal *work, integer *iwork, doublereal *reslts, integer *ldr1, 
	integer *ldr2, integer *ldr3, integer *nout, ftnlen line_len)
{
    /* Initialized data */

    static char uplos[1*2] = "U" "L";
    static char subnam[6*3] = "DSYTRF" "DSYTRS" "DSYTRI";

    /* Format strings */
    static char fmt_9999[] = "(1x,a6,\002 timing run not attempted\002)";
    static char fmt_9998[] = "(/\002 *** Speed of \002,a6,\002 in megaflops "
	    "***\002)";
    static char fmt_9997[] = "(5x,\002line \002,i2,\002 with LDA = \002,i5)";
    static char fmt_9996[] = "(5x,a6,\002 with UPLO = '\002,a1,\002'\002,/)";

    /* System generated locals */
    integer reslts_dim1, reslts_dim2, reslts_dim3, reslts_offset, i__1, i__2, 
	    i__3, i__4, i__5;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void),
	     s_wsle(cilist *), e_wsle(void);

    /* Local variables */
    static integer ilda, info;
    static char path[3];
    static doublereal time;
    static integer isub, nrhs;
    static char uplo[1];
    static integer i__, n;
    static char cname[6];
    extern doublereal dopla_(char *, integer *, integer *, integer *, integer 
	    *, integer *);
    extern logical lsame_(char *, char *);
    static integer iuplo, i3, lwork;
    static doublereal s1, s2;
    static integer ic, nb, in;
    extern doublereal dsecnd_(void);
    extern /* Subroutine */ int atimck_(integer *, char *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, ftnlen), dlacpy_(
	    char *, integer *, integer *, doublereal *, integer *, doublereal 
	    *, integer *);
    extern doublereal dmflop_(doublereal *, doublereal *, integer *);
    extern /* Subroutine */ int atimin_(char *, char *, integer *, char *, 
	    logical *, integer *, integer *, ftnlen, ftnlen, ftnlen), dtimmg_(
	    integer *, integer *, integer *, doublereal *, integer *, integer 
	    *, integer *), dprtbl_(char *, char *, integer *, integer *, 
	    integer *, integer *, integer *, doublereal *, integer *, integer 
	    *, integer *, ftnlen, ftnlen), xlaenv_(integer *, integer *);
    static doublereal untime;
    static logical timsub[3];
    extern /* Subroutine */ int dsytrf_(char *, integer *, doublereal *, 
	    integer *, integer *, doublereal *, integer *, integer *),
	     dsytri_(char *, integer *, doublereal *, integer *, integer *, 
	    doublereal *, integer *), dsytrs_(char *, integer *, 
	    integer *, doublereal *, integer *, integer *, doublereal *, 
	    integer *, integer *);
    static integer lda, ldb, icl, inb, mat;
    static doublereal ops;

    /* Fortran I/O blocks */
    static cilist io___7 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___30 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___31 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___32 = { 0, 0, 0, 0, 0 };
    static cilist io___33 = { 0, 0, 0, fmt_9996, 0 };



#define subnam_ref(a_0,a_1) &subnam[(a_1)*6 + a_0 - 6]
#define reslts_ref(a_1,a_2,a_3,a_4) reslts[(((a_4)*reslts_dim3 + (a_3))*\
reslts_dim2 + (a_2))*reslts_dim1 + a_1]


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       March 31, 1993   


    Purpose   
    =======   

    DTIMSY times DSYTRF, -TRS, and -TRI.   

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

    TIMMIN  (input) DOUBLE PRECISION   
            The minimum time a subroutine will be timed.   

    A       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)   
            where LDAMAX and NMAX are the maximum values permitted   
            for LDA and N.   

    B       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)   

    WORK    (workspace) DOUBLE PRECISION array, dimension (NMAX)   

    IWORK   (workspace) INTEGER array, dimension (NMAX)   

    RESLTS  (output) DOUBLE PRECISION array, dimension   
                     (LDR1,LDR2,LDR3,NSUBS)   
            The timing results for each subroutine over the relevant   
            values of N, NB, and LDA.   

    LDR1    (input) INTEGER   
            The first dimension of RESLTS.  LDR1 >= max(4,NNB).   

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

    s_copy(path, "Double precision", (ftnlen)1, (ftnlen)16);
    s_copy(path + 1, "SY", (ftnlen)2, (ftnlen)2);
    atimin_(path, line, &c__3, subnam, timsub, nout, &info, (ftnlen)3, (
	    ftnlen)80, (ftnlen)6);
    if (info != 0) {
	goto L150;
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
	goto L150;
    }

/*     Do first for UPLO = 'U', then for UPLO = 'L' */

    for (iuplo = 1; iuplo <= 2; ++iuplo) {
	*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];
	if (lsame_(uplo, "U")) {
	    mat = 6;
	} else {
	    mat = -6;
	}

/*        Do for each value of N in NVAL. */

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

/*                 Time DSYTRF */

		    i__3 = *nnb;
		    for (inb = 1; inb <= i__3; ++inb) {
			nb = nbval[inb];
			xlaenv_(&c__1, &nb);
/* Computing MAX */
			i__4 = n << 1, i__5 = nb * n;
			lwork = max(i__4,i__5);
			dtimmg_(&mat, &n, &n, &a[1], &lda, &c__0, &c__0);
			ic = 0;
			s1 = dsecnd_();
L10:
			dsytrf_(uplo, &n, &a[1], &lda, &iwork[1], &b[1], &
				lwork, &info);
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    dtimmg_(&mat, &n, &n, &a[1], &lda, &c__0, &c__0);
			    goto L10;
			}

/*                    Subtract the time used in DTIMMG. */

			icl = 1;
			s1 = dsecnd_();
L20:
			s2 = dsecnd_();
			untime = s2 - s1;
			++icl;
			if (icl <= ic) {
			    dtimmg_(&mat, &n, &n, &b[1], &lda, &c__0, &c__0);
			    goto L20;
			}

			time = (time - untime) / (doublereal) ic;
			ops = dopla_("DSYTRF", &n, &n, &c__0, &c__0, &nb);
			reslts_ref(inb, in, i3, 1) = dmflop_(&ops, &time, &
				info);

/* L30: */
		    }
		} else {

/*                 If DSYTRF was not timed, generate a matrix and   
                   factor it using DSYTRF anyway so that the factored   
                   form of the matrix can be used in timing the other   
                   routines. */

		    dtimmg_(&mat, &n, &n, &a[1], &lda, &c__0, &c__0);
		    nb = 1;
		    xlaenv_(&c__1, &nb);
		    dsytrf_(uplo, &n, &a[1], &lda, &iwork[1], &b[1], &lwork, &
			    info);
		}

/*              Time DSYTRI */

		if (timsub[2]) {
		    dlacpy_(uplo, &n, &n, &a[1], &lda, &b[1], &lda)
			    ;
		    ic = 0;
		    s1 = dsecnd_();
L40:
		    dsytri_(uplo, &n, &b[1], &lda, &iwork[1], &work[1], &info);
		    s2 = dsecnd_();
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			dlacpy_(uplo, &n, &n, &a[1], &lda, &b[1], &lda);
			goto L40;
		    }

/*                 Subtract the time used in DLACPY. */

		    icl = 1;
		    s1 = dsecnd_();
L50:
		    s2 = dsecnd_();
		    untime = s2 - s1;
		    ++icl;
		    if (icl <= ic) {
			dlacpy_(uplo, &n, &n, &a[1], &lda, &b[1], &lda);
			goto L50;
		    }

		    time = (time - untime) / (doublereal) ic;
		    ops = dopla_("DSYTRI", &n, &n, &c__0, &c__0, &c__0);
		    reslts_ref(1, in, i3, 3) = dmflop_(&ops, &time, &info);
		}

/*              Time DSYTRS */

		if (timsub[1]) {
		    i__3 = *nns;
		    for (i__ = 1; i__ <= i__3; ++i__) {
			nrhs = nsval[i__];
			ldb = lda;
			dtimmg_(&c__0, &n, &nrhs, &b[1], &ldb, &c__0, &c__0);
			ic = 0;
			s1 = dsecnd_();
L60:
			dsytrs_(uplo, &n, &nrhs, &a[1], &lda, &iwork[1], &b[1]
				, &ldb, &info);
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    dtimmg_(&c__0, &n, &nrhs, &b[1], &ldb, &c__0, &
				    c__0);
			    goto L60;
			}

/*                    Subtract the time used in DTIMMG. */

			icl = 1;
			s1 = dsecnd_();
L70:
			s2 = dsecnd_();
			untime = s2 - s1;
			++icl;
			if (icl <= ic) {
			    dtimmg_(&c__0, &n, &nrhs, &b[1], &ldb, &c__0, &
				    c__0);
			    goto L70;
			}

			time = (time - untime) / (doublereal) ic;
			ops = dopla_("DSYTRS", &n, &nrhs, &c__0, &c__0, &c__0);
			reslts_ref(i__, in, i3, 2) = dmflop_(&ops, &time, &
				info);
/* L80: */
		    }
		}
/* L90: */
	    }
/* L100: */
	}
/* L110: */
    }

/*     Print tables of results for each timed routine. */

    for (isub = 1; isub <= 3; ++isub) {
	if (! timsub[isub - 1]) {
	    goto L140;
	}
	io___30.ciunit = *nout;
	s_wsfe(&io___30);
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
/* L120: */
	    }
	}
	io___32.ciunit = *nout;
	s_wsle(&io___32);
	e_wsle();
	for (iuplo = 1; iuplo <= 2; ++iuplo) {
	    io___33.ciunit = *nout;
	    s_wsfe(&io___33);
	    do_fio(&c__1, subnam_ref(0, isub), (ftnlen)6);
	    do_fio(&c__1, uplos + (iuplo - 1), (ftnlen)1);
	    e_wsfe();
	    i3 = (iuplo - 1) * *nlda + 1;
	    if (isub == 1) {
		dprtbl_("NB", "N", nnb, &nbval[1], nn, &nval[1], nlda, &
			reslts_ref(1, 1, i3, 1), ldr1, ldr2, nout, (ftnlen)2, 
			(ftnlen)1);
	    } else if (isub == 2) {
		dprtbl_("NRHS", "N", nns, &nsval[1], nn, &nval[1], nlda, &
			reslts_ref(1, 1, i3, 2), ldr1, ldr2, nout, (ftnlen)4, 
			(ftnlen)1);
	    } else if (isub == 3) {
		dprtbl_(" ", "N", &c__1, &nbval[1], nn, &nval[1], nlda, &
			reslts_ref(1, 1, i3, 3), ldr1, ldr2, nout, (ftnlen)1, 
			(ftnlen)1);
	    }
/* L130: */
	}
L140:
	;
    }

L150:
    return 0;

/*     End of DTIMSY */

} /* dtimsy_ */

#undef reslts_ref
#undef subnam_ref


