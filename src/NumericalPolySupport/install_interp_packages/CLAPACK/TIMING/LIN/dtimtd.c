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
static integer c__3 = 3;
static doublereal c_b27 = 100.;
static doublereal c_b28 = 1.;
static integer c_n1 = -1;
static integer c__0 = 0;

/* Subroutine */ int dtimtd_(char *line, integer *nm, integer *mval, integer *
	nn, integer *nval, integer *nnb, integer *nbval, integer *nxval, 
	integer *nlda, integer *ldaval, doublereal *timmin, doublereal *a, 
	doublereal *b, doublereal *d__, doublereal *tau, doublereal *work, 
	doublereal *reslts, integer *ldr1, integer *ldr2, integer *ldr3, 
	integer *nout, ftnlen line_len)
{
    /* Initialized data */

    static char subnam[6*4] = "DSYTRD" "TRED1 " "DORGTR" "DORMTR";
    static char sides[1*2] = "L" "R";
    static char transs[1*2] = "N" "T";
    static char uplos[1*2] = "U" "L";
    static integer iseed[4] = { 0,0,0,1 };

    /* Format strings */
    static char fmt_9999[] = "(1x,a6,\002 timing run not attempted\002,/)";
    static char fmt_9998[] = "(/\002 *** Speed of \002,a6,\002 in megaflops "
	    "*** \002)";
    static char fmt_9997[] = "(5x,\002line \002,i2,\002 with LDA = \002,i5)";
    static char fmt_9996[] = "(/5x,a6,\002 with UPLO = '\002,a1,\002'\002,/)";
    static char fmt_9995[] = "(/5x,a6,\002 with SIDE = '\002,a1,\002', UPLO "
	    "= '\002,a1,\002', TRANS = '\002,a1,\002', \002,a1,\002 =\002,i6,"
	    "/)";

    /* System generated locals */
    integer reslts_dim1, reslts_dim2, reslts_dim3, reslts_offset, i__1, i__2, 
	    i__3, i__4, i__5, i__6;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void),
	     s_wsle(cilist *), e_wsle(void);

    /* Local variables */
    static integer ilda;
    static char side[1];
    static integer info;
    static char path[3];
    static doublereal time;
    static integer isub;
    static char uplo[1];
    extern /* Subroutine */ int tred1_(integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    static integer i__, m, n;
    static char cname[6];
    static integer iside;
    extern doublereal dopla_(char *, integer *, integer *, integer *, integer 
	    *, integer *);
    static integer itoff, itran;
    extern /* Subroutine */ int icopy_(integer *, integer *, integer *, 
	    integer *, integer *);
    static char trans[1];
    static integer iuplo, i3, i4, m1, n1;
    static doublereal s1, s2;
    extern /* Subroutine */ int dprtb3_(char *, char *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, doublereal *, integer 
	    *, integer *, integer *, ftnlen, ftnlen);
    static integer ic, nb, im, in;
    extern doublereal dsecnd_(void);
    static integer lw, nx, reseed[4];
    extern /* Subroutine */ int atimck_(integer *, char *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, ftnlen), dlacpy_(
	    char *, integer *, integer *, doublereal *, integer *, doublereal 
	    *, integer *);
    extern doublereal dmflop_(doublereal *, doublereal *, integer *);
    extern /* Subroutine */ int atimin_(char *, char *, integer *, char *, 
	    logical *, integer *, integer *, ftnlen, ftnlen, ftnlen), dtimmg_(
	    integer *, integer *, integer *, doublereal *, integer *, integer 
	    *, integer *), dlatms_(integer *, integer *, char *, integer *, 
	    char *, doublereal *, integer *, doublereal *, doublereal *, 
	    integer *, integer *, char *, doublereal *, integer *, doublereal 
	    *, integer *), dprtbl_(char *, char *, 
	    integer *, integer *, integer *, integer *, integer *, doublereal 
	    *, integer *, integer *, integer *, ftnlen, ftnlen), xlaenv_(
	    integer *, integer *);
    static doublereal untime;
    extern /* Subroutine */ int dorgtr_(char *, integer *, doublereal *, 
	    integer *, doublereal *, doublereal *, integer *, integer *);
    static logical timsub[4];
    extern /* Subroutine */ int dormtr_(char *, char *, char *, integer *, 
	    integer *, doublereal *, integer *, doublereal *, doublereal *, 
	    integer *, doublereal *, integer *, integer *), dsytrd_(char *, integer *, doublereal *, integer *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, integer *,
	     integer *);
    static integer lda, icl, inb;
    static doublereal ops;
    static char lab1[1], lab2[1];

    /* Fortran I/O blocks */
    static cilist io___10 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___11 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___42 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___44 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___45 = { 0, 0, 0, 0, 0 };
    static cilist io___46 = { 0, 0, 0, fmt_9996, 0 };
    static cilist io___49 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___50 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___51 = { 0, 0, 0, fmt_9995, 0 };



#define subnam_ref(a_0,a_1) &subnam[(a_1)*6 + a_0 - 6]
#define reslts_ref(a_1,a_2,a_3,a_4) reslts[(((a_4)*reslts_dim3 + (a_3))*\
reslts_dim2 + (a_2))*reslts_dim1 + a_1]


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       March 31, 1993   


    Purpose   
    =======   

    DTIMTD times the LAPACK routines DSYTRD, DORGTR, and DORMTR and the   
    EISPACK routine TRED1.   

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

    NN      (input) INTEGER   
            The number of values of N contained in the vector NVAL.   

    NVAL    (input) INTEGER array, dimension (NN)   
            The values of the matrix column dimension N.   

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

    A       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)   
            where LDAMAX and NMAX are the maximum values of LDA and N.   

    B       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)   

    D       (workspace) DOUBLE PRECISION array, dimension (2*NMAX-1)   

    TAU     (workspace) DOUBLE PRECISION array, dimension (NMAX)   

    WORK    (workspace) DOUBLE PRECISION array, dimension (NMAX*NBMAX)   
            where NBMAX is the maximum value of NB.   

    RESLTS  (workspace) DOUBLE PRECISION array, dimension   
                        (LDR1,LDR2,LDR3,4*NN+3)   
            The timing results for each subroutine over the relevant   
            values of M, (NB,NX), LDA, and N.   

    LDR1    (input) INTEGER   
            The first dimension of RESLTS.  LDR1 >= max(1,NNB).   

    LDR2    (input) INTEGER   
            The second dimension of RESLTS.  LDR2 >= max(1,NM).   

    LDR3    (input) INTEGER   
            The third dimension of RESLTS.  LDR3 >= max(1,2*NLDA).   

    NOUT    (input) INTEGER   
            The unit number for output.   

    Internal Parameters   
    ===================   

    MODE    INTEGER   
            The matrix type.  MODE = 3 is a geometric distribution of   
            eigenvalues.  See DLATMS for further details.   

    COND    DOUBLE PRECISION   
            The condition number of the matrix.  The singular values are   
            set to values from DMAX to DMAX/COND.   

    DMAX    DOUBLE PRECISION   
            The magnitude of the largest singular value.   

    =====================================================================   

       Parameter adjustments */
    --mval;
    --nval;
    --nbval;
    --nxval;
    --ldaval;
    --a;
    --b;
    --d__;
    --tau;
    --work;
    reslts_dim1 = *ldr1;
    reslts_dim2 = *ldr2;
    reslts_dim3 = *ldr3;
    reslts_offset = 1 + reslts_dim1 * (1 + reslts_dim2 * (1 + reslts_dim3 * 1)
	    );
    reslts -= reslts_offset;

    /* Function Body   

       Extract the timing request from the input line. */

    s_copy(path, "Double precision", (ftnlen)1, (ftnlen)16);
    s_copy(path + 1, "TD", (ftnlen)2, (ftnlen)2);
    atimin_(path, line, &c__4, subnam, timsub, nout, &info, (ftnlen)3, (
	    ftnlen)80, (ftnlen)6);
    if (info != 0) {
	goto L240;
    }

/*     Check that M <= LDA for the input values. */

    s_copy(cname, line, (ftnlen)6, (ftnlen)6);
    atimck_(&c__2, cname, nm, &mval[1], nlda, &ldaval[1], nout, &info, (
	    ftnlen)6);
    if (info > 0) {
	io___10.ciunit = *nout;
	s_wsfe(&io___10);
	do_fio(&c__1, cname, (ftnlen)6);
	e_wsfe();
	goto L240;
    }

/*     Check that K <= LDA for DORMTR */

    if (timsub[3]) {
	atimck_(&c__3, cname, nn, &nval[1], nlda, &ldaval[1], nout, &info, (
		ftnlen)6);
	if (info > 0) {
	    io___11.ciunit = *nout;
	    s_wsfe(&io___11);
	    do_fio(&c__1, subnam_ref(0, 4), (ftnlen)6);
	    e_wsfe();
	    timsub[3] = FALSE_;
	}
    }

/*     Do first for UPLO = 'U', then for UPLO = 'L' */

    for (iuplo = 1; iuplo <= 2; ++iuplo) {
	*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];

/*        Do for each value of M: */

	i__1 = *nm;
	for (im = 1; im <= i__1; ++im) {
	    m = mval[im];
	    icopy_(&c__4, iseed, &c__1, reseed, &c__1);

/*           Do for each value of LDA: */

	    i__2 = *nlda;
	    for (ilda = 1; ilda <= i__2; ++ilda) {
		lda = ldaval[ilda];
		i3 = (iuplo - 1) * *nlda + ilda;

/*              Do for each pair of values (NB, NX) in NBVAL and NXVAL. */

		i__3 = *nnb;
		for (inb = 1; inb <= i__3; ++inb) {
		    nb = nbval[inb];
		    xlaenv_(&c__1, &nb);
		    nx = nxval[inb];
		    xlaenv_(&c__3, &nx);
/* Computing MAX */
		    i__4 = 1, i__5 = m * max(1,nb);
		    lw = max(i__4,i__5);

/*                 Generate a test matrix of order M. */

		    icopy_(&c__4, reseed, &c__1, iseed, &c__1);
		    dlatms_(&m, &m, "Uniform", iseed, "Symmetric", &tau[1], &
			    c__3, &c_b27, &c_b28, &m, &m, "No packing", &b[1],
			     &lda, &work[1], &info);

		    if (timsub[1] && inb == 1 && iuplo == 2) {

/*                    TRED1:  Eispack reduction using orthogonal   
                      transformations. */

			dlacpy_(uplo, &m, &m, &b[1], &lda, &a[1], &lda);
			ic = 0;
			s1 = dsecnd_();
L10:
			tred1_(&lda, &m, &a[1], &d__[1], &d__[m + 1], &d__[m 
				+ 1]);
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    dlacpy_(uplo, &m, &m, &b[1], &lda, &a[1], &lda);
			    goto L10;
			}

/*                    Subtract the time used in DLACPY. */

			icl = 1;
			s1 = dsecnd_();
L20:
			s2 = dsecnd_();
			untime = s2 - s1;
			++icl;
			if (icl <= ic) {
			    dlacpy_(uplo, &m, &m, &b[1], &lda, &a[1], &lda);
			    goto L20;
			}

			time = (time - untime) / (doublereal) ic;
			ops = dopla_("DSYTRD", &m, &m, &c_n1, &c_n1, &nb);
			reslts_ref(inb, im, ilda, 2) = dmflop_(&ops, &time, &
				info);
		    }

		    if (timsub[0]) {

/*                    DSYTRD:  Reduction to tridiagonal form */

			dlacpy_(uplo, &m, &m, &b[1], &lda, &a[1], &lda);
			ic = 0;
			s1 = dsecnd_();
L30:
			dsytrd_(uplo, &m, &a[1], &lda, &d__[1], &d__[m + 1], &
				tau[1], &work[1], &lw, &info);
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    dlacpy_(uplo, &m, &m, &b[1], &lda, &a[1], &lda);
			    goto L30;
			}

/*                    Subtract the time used in DLACPY. */

			icl = 1;
			s1 = dsecnd_();
L40:
			s2 = dsecnd_();
			untime = s2 - s1;
			++icl;
			if (icl <= ic) {
			    dlacpy_(uplo, &m, &m, &a[1], &lda, &b[1], &lda);
			    goto L40;
			}

			time = (time - untime) / (doublereal) ic;
			ops = dopla_("DSYTRD", &m, &m, &c_n1, &c_n1, &nb);
			reslts_ref(inb, im, i3, 1) = dmflop_(&ops, &time, &
				info);
		    } else {

/*                    If DSYTRD was not timed, generate a matrix and   
                      factor it using DSYTRD anyway so that the factored   
                      form of the matrix can be used in timing the other   
                      routines. */

			dlacpy_(uplo, &m, &m, &b[1], &lda, &a[1], &lda);
			dsytrd_(uplo, &m, &a[1], &lda, &d__[1], &d__[m + 1], &
				tau[1], &work[1], &lw, &info);
		    }

		    if (timsub[2]) {

/*                    DORGTR:  Generate the orthogonal matrix Q from the   
                      reduction to Hessenberg form A = Q*H*Q' */

			dlacpy_(uplo, &m, &m, &a[1], &lda, &b[1], &lda);
			ic = 0;
			s1 = dsecnd_();
L50:
			dorgtr_(uplo, &m, &b[1], &lda, &tau[1], &work[1], &lw,
				 &info);
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    dlacpy_(uplo, &m, &m, &a[1], &lda, &b[1], &lda);
			    goto L50;
			}

/*                    Subtract the time used in DLACPY. */

			icl = 1;
			s1 = dsecnd_();
L60:
			s2 = dsecnd_();
			untime = s2 - s1;
			++icl;
			if (icl <= ic) {
			    dlacpy_(uplo, &m, &m, &a[1], &lda, &b[1], &lda);
			    goto L60;
			}

			time = (time - untime) / (doublereal) ic;

/*                    Op count for DORGTR:  same as   
                         DORGQR( N-1, N-1, N-1, ... ) */

			i__4 = m - 1;
			i__5 = m - 1;
			i__6 = m - 1;
			ops = dopla_("DORGQR", &i__4, &i__5, &i__6, &c_n1, &
				nb);
			reslts_ref(inb, im, i3, 3) = dmflop_(&ops, &time, &
				info);
		    }

		    if (timsub[3]) {

/*                    DORMTR:  Multiply by Q stored as a product of   
                      elementary transformations */

			i4 = 3;
			for (iside = 1; iside <= 2; ++iside) {
			    *(unsigned char *)side = *(unsigned char *)&sides[
				    iside - 1];
			    i__4 = *nn;
			    for (in = 1; in <= i__4; ++in) {
				n = nval[in];
/* Computing MAX */
				i__5 = 1, i__6 = max(1,nb) * n;
				lw = max(i__5,i__6);
				if (iside == 1) {
				    m1 = m;
				    n1 = n;
				} else {
				    m1 = n;
				    n1 = m;
				}
				itoff = 0;
				for (itran = 1; itran <= 2; ++itran) {
				    *(unsigned char *)trans = *(unsigned char 
					    *)&transs[itran - 1];
				    dtimmg_(&c__0, &m1, &n1, &b[1], &lda, &
					    c__0, &c__0);
				    ic = 0;
				    s1 = dsecnd_();
L70:
				    dormtr_(side, uplo, trans, &m1, &n1, &a[1]
					    , &lda, &tau[1], &b[1], &lda, &
					    work[1], &lw, &info);
				    s2 = dsecnd_();
				    time = s2 - s1;
				    ++ic;
				    if (time < *timmin) {
					dtimmg_(&c__0, &m1, &n1, &b[1], &lda, 
						&c__0, &c__0);
					goto L70;
				    }

/*                             Subtract the time used in DTIMMG. */

				    icl = 1;
				    s1 = dsecnd_();
L80:
				    s2 = dsecnd_();
				    untime = s2 - s1;
				    ++icl;
				    if (icl <= ic) {
					dtimmg_(&c__0, &m1, &n1, &b[1], &lda, 
						&c__0, &c__0);
					goto L80;
				    }

				    time = (time - untime) / (doublereal) ic;

/*                             Op count for DORMTR, SIDE='L':  same as   
                                  DORMQR( 'L', TRANS, M-1, N, M-1, ...)   

                               Op count for DORMTR, SIDE='R':  same as   
                                  DORMQR( 'R', TRANS, M, N-1, N-1, ...) */

				    if (iside == 1) {
					i__5 = m1 - 1;
					i__6 = m1 - 1;
					ops = dopla_("DORMQR", &i__5, &n1, &
						i__6, &c_n1, &nb);
				    } else {
					i__5 = n1 - 1;
					i__6 = n1 - 1;
					ops = dopla_("DORMQR", &m1, &i__5, &
						i__6, &c__1, &nb);
				    }

				    reslts_ref(inb, im, i3, i4 + itoff + in) =
					     dmflop_(&ops, &time, &info);
				    itoff = *nn;
/* L90: */
				}
/* L100: */
			    }
			    i4 += *nn << 1;
/* L110: */
			}
		    }

/* L120: */
		}
/* L130: */
	    }
/* L140: */
	}
/* L150: */
    }

/*     Print tables of results for DSYTRD, TRED1, and DORGTR */

    for (isub = 1; isub <= 3; ++isub) {
	if (! timsub[isub - 1]) {
	    goto L180;
	}
	io___42.ciunit = *nout;
	s_wsfe(&io___42);
	do_fio(&c__1, subnam_ref(0, isub), (ftnlen)6);
	e_wsfe();
	if (*nlda > 1) {
	    i__1 = *nlda;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		io___44.ciunit = *nout;
		s_wsfe(&io___44);
		do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&ldaval[i__], (ftnlen)sizeof(integer));
		e_wsfe();
/* L160: */
	    }
	}
	if (isub == 2) {
	    io___45.ciunit = *nout;
	    s_wsle(&io___45);
	    e_wsle();
	    dprtb3_(" ", "N", &c__1, &nbval[1], &nxval[1], nm, &mval[1], nlda,
		     &reslts_ref(1, 1, 1, isub), ldr1, ldr2, nout, (ftnlen)1, 
		    (ftnlen)1);
	} else {
	    i3 = 1;
	    for (iuplo = 1; iuplo <= 2; ++iuplo) {
		io___46.ciunit = *nout;
		s_wsfe(&io___46);
		do_fio(&c__1, subnam_ref(0, isub), (ftnlen)6);
		do_fio(&c__1, uplos + (iuplo - 1), (ftnlen)1);
		e_wsfe();
		dprtb3_("(  NB,  NX)", "N", nnb, &nbval[1], &nxval[1], nm, &
			mval[1], nlda, &reslts_ref(1, 1, i3, isub), ldr1, 
			ldr2, nout, (ftnlen)11, (ftnlen)1);
		i3 += *nlda;
/* L170: */
	    }
	}
L180:
	;
    }

/*     Print tables of results for DORMTR */

    isub = 4;
    if (timsub[isub - 1]) {
	i4 = 3;
	for (iside = 1; iside <= 2; ++iside) {
	    if (iside == 1) {
		*(unsigned char *)lab1 = 'M';
		*(unsigned char *)lab2 = 'N';
		if (*nlda > 1) {
		    io___49.ciunit = *nout;
		    s_wsfe(&io___49);
		    do_fio(&c__1, subnam_ref(0, isub), (ftnlen)6);
		    e_wsfe();
		    i__1 = *nlda;
		    for (i__ = 1; i__ <= i__1; ++i__) {
			io___50.ciunit = *nout;
			s_wsfe(&io___50);
			do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&ldaval[i__], (ftnlen)sizeof(
				integer));
			e_wsfe();
/* L190: */
		    }
		}
	    } else {
		*(unsigned char *)lab1 = 'N';
		*(unsigned char *)lab2 = 'M';
	    }
	    for (itran = 1; itran <= 2; ++itran) {
		i__1 = *nn;
		for (in = 1; in <= i__1; ++in) {
		    i3 = 1;
		    for (iuplo = 1; iuplo <= 2; ++iuplo) {
			io___51.ciunit = *nout;
			s_wsfe(&io___51);
			do_fio(&c__1, subnam_ref(0, isub), (ftnlen)6);
			do_fio(&c__1, sides + (iside - 1), (ftnlen)1);
			do_fio(&c__1, uplos + (iuplo - 1), (ftnlen)1);
			do_fio(&c__1, transs + (itran - 1), (ftnlen)1);
			do_fio(&c__1, lab2, (ftnlen)1);
			do_fio(&c__1, (char *)&nval[in], (ftnlen)sizeof(
				integer));
			e_wsfe();
			dprtbl_("NB", lab1, nnb, &nbval[1], nm, &mval[1], 
				nlda, &reslts_ref(1, 1, i3, i4 + in), ldr1, 
				ldr2, nout, (ftnlen)2, (ftnlen)1);
			i3 += *nlda;
/* L200: */
		    }
/* L210: */
		}
		i4 += *nn;
/* L220: */
	    }
/* L230: */
	}
    }
L240:

/*     Print a table of results for each timed routine. */

    return 0;

/*     End of DTIMTD */

} /* dtimtd_ */

#undef reslts_ref
#undef subnam_ref


