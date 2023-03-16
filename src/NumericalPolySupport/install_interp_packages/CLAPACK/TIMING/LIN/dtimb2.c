#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__16 = 16;
static integer c__1 = 1;
static integer c__0 = 0;
static integer c__4 = 4;
static integer c__2 = 2;
static doublereal c_b76 = 1.;
static integer c_n2 = -2;

/* Subroutine */ int dtimb2_(char *line, integer *nm, integer *mval, integer *
	nn, integer *nval, integer *nk, integer *kval, integer *ninc, integer 
	*incval, integer *nlda, integer *ldaval, integer *la, doublereal *
	timmin, doublereal *a, doublereal *x, doublereal *y, doublereal *
	reslts, integer *ldr1, integer *ldr2, integer *nout, ftnlen line_len)
{
    /* Initialized data */

    static char trans[1*2] = "N" "T";
    static char uplos[1*2] = "U" "L";
    static char names[6*16] = "DGEMV " "DGBMV " "DSYMV " "DSBMV " "DSPMV " 
	    "DTRMV " "DTBMV " "DTPMV " "DTRSV " "DTBSV " "DTPSV " "DGER  " 
	    "DSYR  " "DSPR  " "DSYR2 " "DSPR2 ";

    /* Format strings */
    static char fmt_9999[] = "(1x,a6,\002 timing run not attempted\002,/)";
    static char fmt_9998[] = "(/\002 *** Speed of \002,a6,\002 in megaflops "
	    "***\002)";
    static char fmt_9997[] = "(5x,\002with LDA = \002,i5,\002 and INCX = INC"
	    "Y = \002,i5)";
    static char fmt_9996[] = "(5x,\002with LDA = \002,i5,\002 and INCX = "
	    "\002,i5)";
    static char fmt_9993[] = "(5x,\002line \002,i2,\002 with LDA = \002,i5"
	    ",\002 and INCX = INCY = \002,i5)";
    static char fmt_9992[] = "(5x,\002line \002,i2,\002 with LDA = \002,i5"
	    ",\002 and INCX = \002,i5)";
    static char fmt_9995[] = "(5x,\002with INCX = INCY = \002,i5)";
    static char fmt_9994[] = "(5x,\002with INCX = \002,i5)";
    static char fmt_9991[] = "(5x,\002line \002,i2,\002 with INCX = INCY ="
	    " \002,i5)";
    static char fmt_9990[] = "(5x,\002line \002,i2,\002 with INCX = \002,i5)";
    static char fmt_9989[] = "(/1x,\002DGEMV  with TRANS = '\002,a1,\002'"
	    "\002,/)";
    static char fmt_9988[] = "(/1x,\002DGBMV  with TRANS = '\002,a1,\002', M"
	    " = N and KL = K\002,\002U \002,\002= K\002,/)";
    static char fmt_9986[] = "(/1x,a6,\002 with UPLO = '\002,a1,\002'\002,/)";
    static char fmt_9987[] = "(/1x,a6,\002 with UPLO = '\002,a1,\002', TRANS"
	    " = '\002,a1,\002'\002,/)";
    static char fmt_9985[] = "(/1x,\002DGER\002,/)";
    static char fmt_9984[] = "(/////)";

    /* System generated locals */
    integer reslts_dim1, reslts_dim2, reslts_offset, i__1, i__2, i__3, i__4;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), s_wsfe(cilist *), do_fio(
	    integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static integer ilda;
    extern /* Subroutine */ int dger_(integer *, integer *, doublereal *, 
	    doublereal *, integer *, doublereal *, integer *, doublereal *, 
	    integer *);
    static integer iinc, imat, info;
    static char path[3];
    static doublereal time;
    static integer incx, isub;
    extern /* Subroutine */ int dspr_(char *, integer *, doublereal *, 
	    doublereal *, integer *, doublereal *);
    static char uplo[1];
    extern /* Subroutine */ int dsyr_(char *, integer *, doublereal *, 
	    doublereal *, integer *, doublereal *, integer *), dspr2_(
	    char *, integer *, doublereal *, doublereal *, integer *, 
	    doublereal *, integer *, doublereal *), dsyr2_(char *, 
	    integer *, doublereal *, doublereal *, integer *, doublereal *, 
	    integer *, doublereal *, integer *);
    static integer i__, j, k, m, n;
    static char cname[6];
    static integer laval[1];
    extern /* Subroutine */ int dgbmv_(char *, integer *, integer *, integer *
	    , integer *, doublereal *, doublereal *, integer *, doublereal *, 
	    integer *, doublereal *, doublereal *, integer *), dgemv_(
	    char *, integer *, integer *, doublereal *, doublereal *, integer 
	    *, doublereal *, integer *, doublereal *, doublereal *, integer *), dsbmv_(char *, integer *, integer *, doublereal *, 
	    doublereal *, integer *, doublereal *, integer *, doublereal *, 
	    doublereal *, integer *), dtbmv_(char *, char *, char *, 
	    integer *, integer *, doublereal *, integer *, doublereal *, 
	    integer *), dtbsv_(char *, char *, char *,
	     integer *, integer *, doublereal *, integer *, doublereal *, 
	    integer *);
    static integer iuplo;
    extern /* Subroutine */ int dspmv_(char *, integer *, doublereal *, 
	    doublereal *, doublereal *, integer *, doublereal *, doublereal *,
	     integer *), dtpmv_(char *, char *, char *, integer *, 
	    doublereal *, doublereal *, integer *);
    static integer i3;
    extern /* Subroutine */ int dtrmv_(char *, char *, char *, integer *, 
	    doublereal *, integer *, doublereal *, integer *), dtpsv_(char *, char *, char *, integer *, doublereal *, 
	    doublereal *, integer *), dsymv_(char *, 
	    integer *, doublereal *, doublereal *, integer *, doublereal *, 
	    integer *, doublereal *, doublereal *, integer *);
    static doublereal s1, s2;
    extern /* Subroutine */ int dtrsv_(char *, char *, char *, integer *, 
	    doublereal *, integer *, doublereal *, integer *);
    extern doublereal dopbl2_(char *, integer *, integer *, integer *, 
	    integer *);
    static integer ic, ik, im, in;
    extern doublereal dsecnd_(void);
    static integer nx, ny;
    extern /* Subroutine */ int atimck_(integer *, char *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, ftnlen);
    extern doublereal dmflop_(doublereal *, doublereal *, integer *);
    extern /* Subroutine */ int atimin_(char *, char *, integer *, char *, 
	    logical *, integer *, integer *, ftnlen, ftnlen, ftnlen), dtimmg_(
	    integer *, integer *, integer *, doublereal *, integer *, integer 
	    *, integer *), dprtbl_(char *, char *, integer *, integer *, 
	    integer *, integer *, integer *, doublereal *, integer *, integer 
	    *, integer *, ftnlen, ftnlen);
    static char transa[1];
    static logical ixandy;
    static doublereal untime;
    static logical timsub[16];
    static integer lda, icl, ita;
    static doublereal ops;

    /* Fortran I/O blocks */
    static cilist io___10 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___11 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___13 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___14 = { 0, 0, 0, fmt_9996, 0 };
    static cilist io___17 = { 0, 0, 0, fmt_9993, 0 };
    static cilist io___18 = { 0, 0, 0, fmt_9992, 0 };
    static cilist io___19 = { 0, 0, 0, fmt_9995, 0 };
    static cilist io___20 = { 0, 0, 0, fmt_9994, 0 };
    static cilist io___21 = { 0, 0, 0, fmt_9991, 0 };
    static cilist io___22 = { 0, 0, 0, fmt_9990, 0 };
    static cilist io___43 = { 0, 0, 0, fmt_9989, 0 };
    static cilist io___46 = { 0, 0, 0, fmt_9988, 0 };
    static cilist io___50 = { 0, 0, 0, fmt_9986, 0 };
    static cilist io___51 = { 0, 0, 0, fmt_9986, 0 };
    static cilist io___52 = { 0, 0, 0, fmt_9986, 0 };
    static cilist io___53 = { 0, 0, 0, fmt_9987, 0 };
    static cilist io___54 = { 0, 0, 0, fmt_9987, 0 };
    static cilist io___55 = { 0, 0, 0, fmt_9987, 0 };
    static cilist io___56 = { 0, 0, 0, fmt_9987, 0 };
    static cilist io___57 = { 0, 0, 0, fmt_9987, 0 };
    static cilist io___58 = { 0, 0, 0, fmt_9987, 0 };
    static cilist io___59 = { 0, 0, 0, fmt_9985, 0 };
    static cilist io___60 = { 0, 0, 0, fmt_9986, 0 };
    static cilist io___61 = { 0, 0, 0, fmt_9986, 0 };
    static cilist io___62 = { 0, 0, 0, fmt_9986, 0 };
    static cilist io___63 = { 0, 0, 0, fmt_9986, 0 };
    static cilist io___64 = { 0, 0, 0, fmt_9984, 0 };



#define names_ref(a_0,a_1) &names[(a_1)*6 + a_0 - 6]
#define reslts_ref(a_1,a_2,a_3) reslts[((a_3)*reslts_dim2 + (a_2))*\
reslts_dim1 + a_1]


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       March 31, 1993   


    Purpose   
    =======   

    DTIMB2 times the BLAS 2 routines.   

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
            The values of the matrix row dimension M.   

    NN      (input) INTEGER   
            The number of values of N contained in the vector NVAL.   

    NVAL    (input) INTEGER array, dimension (NN)   
            The values of the matrix column dimension N.   

    NK      (input) INTEGER   
            The number of values of K contained in the vector KVAL.   

    KVAL    (input) INTEGER array, dimension (NK)   
            The values of the band width K.   

    NINC    (input) INTEGER   
            The number of values of INCX contained in the vector INCVAL.   

    INCVAL  (input) INTEGER array, dimension (NINC)   
            The values of INCX, the increment between successive values   
            of the vector X.   

    NLDA    (input) INTEGER   
            The number of values of LDA contained in the vector LDAVAL.   

    LDAVAL  (input) INTEGER array, dimension (NLDA)   
            The values of the leading dimension of the array A.   

    LA      (input) INTEGER   
            The size of the array A.   

    TIMMIN  (input) DOUBLE PRECISION   
            The minimum time a subroutine will be timed.   

    A       (workspace) DOUBLE PRECISION array, dimension (LA)   

    X       (workspace) DOUBLE PRECISION array, dimension (NMAX*INCMAX)   
               where NMAX and INCMAX are the maximum values permitted   
               for N and INCX.   

    Y       (workspace) DOUBLE PRECISION array, dimension (NMAX*INCMAX)   
               where NMAX and INCMAX are the maximum values permitted   
               for N and INCX.   

    RESLTS  (output) DOUBLE PRECISION array, dimension (LDR1,LDR2,p),   
               where p = NLDA*NINC.   
            The timing results for each subroutine over the relevant   
            values of M, N, K, INCX, and LDA.   

    LDR1    (input) INTEGER   
            The first dimension of RESLTS.  LDR1 >= max(1,NM,NK).   

    LDR2    (input) INTEGER   
            The second dimension of RESLTS.  LDR2 >= max(1,NN).   

    NOUT    (input) INTEGER   
            The unit number for output.   

    =====================================================================   

       Parameter adjustments */
    --mval;
    --nval;
    --kval;
    --incval;
    --ldaval;
    --a;
    --x;
    --y;
    reslts_dim1 = *ldr1;
    reslts_dim2 = *ldr2;
    reslts_offset = 1 + reslts_dim1 * (1 + reslts_dim2 * 1);
    reslts -= reslts_offset;

    /* Function Body   

       Extract the timing request from the input line. */

    s_copy(path, "Double precision", (ftnlen)1, (ftnlen)16);
    s_copy(path + 1, "B2", (ftnlen)2, (ftnlen)2);
    atimin_(path, line, &c__16, names, timsub, nout, &info, (ftnlen)3, 
	    line_len, (ftnlen)6);
    if (info != 0) {
	goto L1070;
    }

/*     Time each routine */

    for (isub = 1; isub <= 16; ++isub) {
	if (! timsub[isub - 1]) {
	    goto L1060;
	}

/*        Check the input values.  The conditions are   
             M <= LDA for general storage   
             K <= LDA for banded storage   
             N*(N+1)/2 <= LA  for packed storage */

	s_copy(cname, names_ref(0, isub), (ftnlen)6, (ftnlen)6);
	if (s_cmp(cname + 1, "GE", (ftnlen)2, (ftnlen)2) == 0) {
	    atimck_(&c__1, cname, nm, &mval[1], nlda, &ldaval[1], nout, &info,
		     (ftnlen)6);
	} else if (*(unsigned char *)&cname[2] == 'B') {
	    atimck_(&c__0, cname, nk, &kval[1], nlda, &ldaval[1], nout, &info,
		     (ftnlen)6);
	} else if (*(unsigned char *)&cname[2] == 'P') {
	    laval[0] = *la;
	    atimck_(&c__4, cname, nn, &nval[1], &c__1, laval, nout, &info, (
		    ftnlen)6);
	} else {
	    atimck_(&c__2, cname, nn, &nval[1], nlda, &ldaval[1], nout, &info,
		     (ftnlen)6);
	}
	if (info > 0) {
	    io___10.ciunit = *nout;
	    s_wsfe(&io___10);
	    do_fio(&c__1, cname, (ftnlen)6);
	    e_wsfe();
	    goto L1060;
	}

/*        Print header. */

	io___11.ciunit = *nout;
	s_wsfe(&io___11);
	do_fio(&c__1, cname, (ftnlen)6);
	e_wsfe();
	ixandy = isub <= 5 || isub == 12 || isub == 15 || isub == 16;
	if (*(unsigned char *)&cname[2] != 'P') {
	    if (*nlda * *ninc == 1) {
		if (ixandy) {
		    io___13.ciunit = *nout;
		    s_wsfe(&io___13);
		    do_fio(&c__1, (char *)&ldaval[1], (ftnlen)sizeof(integer))
			    ;
		    do_fio(&c__1, (char *)&incval[1], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		} else {
		    io___14.ciunit = *nout;
		    s_wsfe(&io___14);
		    do_fio(&c__1, (char *)&ldaval[1], (ftnlen)sizeof(integer))
			    ;
		    do_fio(&c__1, (char *)&incval[1], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		}
	    } else {
		i__1 = *nlda;
		for (i__ = 1; i__ <= i__1; ++i__) {
		    i__2 = *ninc;
		    for (j = 1; j <= i__2; ++j) {
			if (ixandy) {
			    io___17.ciunit = *nout;
			    s_wsfe(&io___17);
			    i__3 = (i__ - 1) * *ninc + j;
			    do_fio(&c__1, (char *)&i__3, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&ldaval[i__], (ftnlen)
				    sizeof(integer));
			    do_fio(&c__1, (char *)&incval[j], (ftnlen)sizeof(
				    integer));
			    e_wsfe();
			} else {
			    io___18.ciunit = *nout;
			    s_wsfe(&io___18);
			    i__3 = (i__ - 1) * *ninc + j;
			    do_fio(&c__1, (char *)&i__3, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&ldaval[i__], (ftnlen)
				    sizeof(integer));
			    do_fio(&c__1, (char *)&incval[j], (ftnlen)sizeof(
				    integer));
			    e_wsfe();
			}
/* L10: */
		    }
/* L20: */
		}
	    }
	} else {
	    if (*ninc == 1) {
		if (ixandy) {
		    io___19.ciunit = *nout;
		    s_wsfe(&io___19);
		    do_fio(&c__1, (char *)&incval[1], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		} else {
		    io___20.ciunit = *nout;
		    s_wsfe(&io___20);
		    do_fio(&c__1, (char *)&incval[1], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		}
	    } else {
		i__1 = *ninc;
		for (j = 1; j <= i__1; ++j) {
		    if (ixandy) {
			io___21.ciunit = *nout;
			s_wsfe(&io___21);
			do_fio(&c__1, (char *)&j, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&incval[j], (ftnlen)sizeof(
				integer));
			e_wsfe();
		    } else {
			io___22.ciunit = *nout;
			s_wsfe(&io___22);
			do_fio(&c__1, (char *)&j, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&incval[j], (ftnlen)sizeof(
				integer));
			e_wsfe();
		    }
/* L30: */
		}
	    }
	}

/*        Time DGEMV */

	if (s_cmp(cname, "DGEMV ", (ftnlen)6, (ftnlen)6) == 0) {
	    for (ita = 1; ita <= 2; ++ita) {
		*(unsigned char *)transa = *(unsigned char *)&trans[ita - 1];
		i3 = 0;
		i__1 = *nlda;
		for (ilda = 1; ilda <= i__1; ++ilda) {
		    lda = ldaval[ilda];
		    i__2 = *ninc;
		    for (iinc = 1; iinc <= i__2; ++iinc) {
			incx = incval[iinc];
			++i3;
			i__3 = *nm;
			for (im = 1; im <= i__3; ++im) {
			    m = mval[im];
			    i__4 = *nn;
			    for (in = 1; in <= i__4; ++in) {
				n = nval[in];
				if (*(unsigned char *)transa == 'N') {
				    nx = n;
				    ny = m;
				} else {
				    nx = m;
				    ny = n;
				}
				dtimmg_(&c__1, &m, &n, &a[1], &lda, &c__0, &
					c__0);
				dtimmg_(&c__0, &c__1, &nx, &x[1], &incx, &
					c__0, &c__0);
				dtimmg_(&c__0, &c__1, &ny, &y[1], &incx, &
					c__0, &c__0);
				ic = 0;
				s1 = dsecnd_();
L40:
				dgemv_(transa, &m, &n, &c_b76, &a[1], &lda, &
					x[1], &incx, &c_b76, &y[1], &incx);
				s2 = dsecnd_();
				time = s2 - s1;
				++ic;
				if (time < *timmin) {
				    dtimmg_(&c__0, &c__1, &ny, &y[1], &incx, &
					    c__0, &c__0);
				    goto L40;
				}

/*                          Subtract the time used in DTIMMG. */

				icl = 1;
				s1 = dsecnd_();
L50:
				s2 = dsecnd_();
				untime = s2 - s1;
				++icl;
				if (icl <= ic) {
				    dtimmg_(&c__0, &c__1, &ny, &y[1], &incx, &
					    c__0, &c__0);
				    goto L50;
				}

				time = (time - untime) / (doublereal) ic;
				ops = dopbl2_(cname, &m, &n, &c__0, &c__0);
				reslts_ref(im, in, i3) = dmflop_(&ops, &time, 
					&c__0);
/* L60: */
			    }
/* L70: */
			}
/* L80: */
		    }
/* L90: */
		}
		io___43.ciunit = *nout;
		s_wsfe(&io___43);
		do_fio(&c__1, transa, (ftnlen)1);
		e_wsfe();
		i__1 = *ninc * *nlda;
		dprtbl_("M", "N", nm, &mval[1], nn, &nval[1], &i__1, &reslts[
			reslts_offset], ldr1, ldr2, nout, (ftnlen)1, (ftnlen)
			1);
/* L100: */
	    }

	} else if (s_cmp(cname, "DGBMV ", (ftnlen)6, (ftnlen)6) == 0) {

/*           Time DGBMV */

	    for (ita = 1; ita <= 2; ++ita) {
		*(unsigned char *)transa = *(unsigned char *)&trans[ita - 1];
		i3 = 0;
		i__1 = *nlda;
		for (ilda = 1; ilda <= i__1; ++ilda) {
		    lda = ldaval[ilda];
		    i__2 = *ninc;
		    for (iinc = 1; iinc <= i__2; ++iinc) {
			incx = incval[iinc];
			++i3;
			i__3 = *nk;
			for (ik = 1; ik <= i__3; ++ik) {
			    k = kval[ik];
			    i__4 = *nn;
			    for (in = 1; in <= i__4; ++in) {
				n = nval[in];
				m = n;
				dtimmg_(&c_n2, &m, &n, &a[1], &lda, &k, &k);
				dtimmg_(&c__0, &c__1, &n, &x[1], &incx, &c__0,
					 &c__0);
				dtimmg_(&c__0, &c__1, &m, &y[1], &incx, &c__0,
					 &c__0);
				ic = 0;
				s1 = dsecnd_();
L110:
				dgbmv_(transa, &m, &n, &k, &k, &c_b76, &a[1], 
					&lda, &x[1], &incx, &c_b76, &y[1], &
					incx);
				s2 = dsecnd_();
				time = s2 - s1;
				++ic;
				if (time < *timmin) {
				    dtimmg_(&c__0, &c__1, &m, &y[1], &incx, &
					    c__0, &c__0);
				    goto L110;
				}

/*                          Subtract the time used in DTIMMG. */

				icl = 1;
				s1 = dsecnd_();
L120:
				s2 = dsecnd_();
				untime = s2 - s1;
				++icl;
				if (icl <= ic) {
				    dtimmg_(&c__0, &c__1, &m, &y[1], &incx, &
					    c__0, &c__0);
				    goto L120;
				}

				time = (time - untime) / (doublereal) ic;
				ops = dopbl2_(cname, &m, &n, &k, &k);
				reslts_ref(ik, in, i3) = dmflop_(&ops, &time, 
					&c__0);
/* L130: */
			    }
/* L140: */
			}
/* L150: */
		    }
/* L160: */
		}
		io___46.ciunit = *nout;
		s_wsfe(&io___46);
		do_fio(&c__1, transa, (ftnlen)1);
		e_wsfe();
		i__1 = *ninc * *nlda;
		dprtbl_("K", "N", nk, &kval[1], nn, &nval[1], &i__1, &reslts[
			reslts_offset], ldr1, ldr2, nout, (ftnlen)1, (ftnlen)
			1);
/* L170: */
	    }

	} else if (s_cmp(cname, "DSYMV ", (ftnlen)6, (ftnlen)6) == 0) {

/*           Time DSYMV */

	    for (iuplo = 1; iuplo <= 2; ++iuplo) {
		*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];
		imat = 6;
		if (*(unsigned char *)uplo == 'L') {
		    imat = -6;
		}
		i3 = 0;
		i__1 = *nlda;
		for (ilda = 1; ilda <= i__1; ++ilda) {
		    lda = ldaval[ilda];
		    i__2 = *ninc;
		    for (iinc = 1; iinc <= i__2; ++iinc) {
			incx = incval[iinc];
			++i3;
			i__3 = *nn;
			for (in = 1; in <= i__3; ++in) {
			    n = nval[in];
			    dtimmg_(&imat, &n, &n, &a[1], &lda, &c__0, &c__0);
			    dtimmg_(&c__0, &c__1, &n, &x[1], &incx, &c__0, &
				    c__0);
			    dtimmg_(&c__0, &c__1, &n, &y[1], &incx, &c__0, &
				    c__0);
			    ic = 0;
			    s1 = dsecnd_();
L180:
			    dsymv_(uplo, &n, &c_b76, &a[1], &lda, &x[1], &
				    incx, &c_b76, &y[1], &incx);
			    s2 = dsecnd_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				dtimmg_(&c__0, &c__1, &n, &y[1], &incx, &c__0,
					 &c__0);
				goto L180;
			    }

/*                       Subtract the time used in DTIMMG. */

			    icl = 1;
			    s1 = dsecnd_();
L190:
			    s2 = dsecnd_();
			    untime = s2 - s1;
			    ++icl;
			    if (icl <= ic) {
				dtimmg_(&c__0, &c__1, &n, &y[1], &incx, &c__0,
					 &c__0);
				goto L190;
			    }

			    time = (time - untime) / (doublereal) ic;
			    ops = dopbl2_(cname, &n, &n, &c__0, &c__0);
			    reslts_ref(1, in, i3) = dmflop_(&ops, &time, &
				    c__0);
/* L200: */
			}
/* L210: */
		    }
/* L220: */
		}
		io___50.ciunit = *nout;
		s_wsfe(&io___50);
		do_fio(&c__1, cname, (ftnlen)6);
		do_fio(&c__1, uplo, (ftnlen)1);
		e_wsfe();
		i__1 = *ninc * *nlda;
		dprtbl_(" ", "N", &c__1, &nval[1], nn, &nval[1], &i__1, &
			reslts[reslts_offset], ldr1, ldr2, nout, (ftnlen)1, (
			ftnlen)1);
/* L230: */
	    }

	} else if (s_cmp(cname, "DSBMV ", (ftnlen)6, (ftnlen)6) == 0) {

/*           Time DSBMV */

	    for (iuplo = 1; iuplo <= 2; ++iuplo) {
		*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];
		imat = 8;
		if (*(unsigned char *)uplo == 'L') {
		    imat = -8;
		}
		i3 = 0;
		i__1 = *nlda;
		for (ilda = 1; ilda <= i__1; ++ilda) {
		    lda = ldaval[ilda];
		    i__2 = *ninc;
		    for (iinc = 1; iinc <= i__2; ++iinc) {
			incx = incval[iinc];
			++i3;
			i__3 = *nk;
			for (ik = 1; ik <= i__3; ++ik) {
			    k = kval[ik];
			    i__4 = *nn;
			    for (in = 1; in <= i__4; ++in) {
				n = nval[in];
				dtimmg_(&imat, &n, &n, &a[1], &lda, &k, &k);
				dtimmg_(&c__0, &c__1, &n, &x[1], &incx, &c__0,
					 &c__0);
				dtimmg_(&c__0, &c__1, &n, &y[1], &incx, &c__0,
					 &c__0);
				ic = 0;
				s1 = dsecnd_();
L240:
				dsbmv_(uplo, &n, &k, &c_b76, &a[1], &lda, &x[
					1], &incx, &c_b76, &y[1], &incx);
				s2 = dsecnd_();
				time = s2 - s1;
				++ic;
				if (time < *timmin) {
				    dtimmg_(&c__0, &c__1, &n, &y[1], &incx, &
					    c__0, &c__0);
				    goto L240;
				}

/*                          Subtract the time used in DTIMMG. */

				icl = 1;
				s1 = dsecnd_();
L250:
				s2 = dsecnd_();
				untime = s2 - s1;
				++icl;
				if (icl <= ic) {
				    dtimmg_(&c__0, &c__1, &n, &y[1], &incx, &
					    c__0, &c__0);
				    goto L250;
				}

				time = (time - untime) / (doublereal) ic;
				ops = dopbl2_(cname, &n, &n, &k, &k);
				reslts_ref(ik, in, i3) = dmflop_(&ops, &time, 
					&c__0);
/* L260: */
			    }
/* L270: */
			}
/* L280: */
		    }
/* L290: */
		}
		io___51.ciunit = *nout;
		s_wsfe(&io___51);
		do_fio(&c__1, cname, (ftnlen)6);
		do_fio(&c__1, uplo, (ftnlen)1);
		e_wsfe();
		i__1 = *ninc * *nlda;
		dprtbl_("K", "N", nk, &kval[1], nn, &nval[1], &i__1, &reslts[
			reslts_offset], ldr1, ldr2, nout, (ftnlen)1, (ftnlen)
			1);
/* L300: */
	    }

	} else if (s_cmp(cname, "DSPMV ", (ftnlen)6, (ftnlen)6) == 0) {

/*           Time DSPMV */

	    for (iuplo = 1; iuplo <= 2; ++iuplo) {
		*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];
		imat = 7;
		if (*(unsigned char *)uplo == 'L') {
		    imat = -7;
		}
		ilda = 1;
		lda = ldaval[ilda];
		i__1 = *ninc;
		for (iinc = 1; iinc <= i__1; ++iinc) {
		    incx = incval[iinc];
		    i__2 = *nn;
		    for (in = 1; in <= i__2; ++in) {
			n = nval[in];
			i__3 = n * (n + 1) / 2;
			dtimmg_(&imat, &n, &n, &a[1], &i__3, &c__0, &c__0);
			dtimmg_(&c__0, &c__1, &n, &x[1], &incx, &c__0, &c__0);
			dtimmg_(&c__0, &c__1, &n, &y[1], &incx, &c__0, &c__0);
			ic = 0;
			s1 = dsecnd_();
L310:
			dspmv_(uplo, &n, &c_b76, &a[1], &x[1], &incx, &c_b76, 
				&y[1], &incx);
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    dtimmg_(&c__0, &c__1, &n, &y[1], &incx, &c__0, &
				    c__0);
			    goto L310;
			}

/*                    Subtract the time used in DTIMMG. */

			icl = 1;
			s1 = dsecnd_();
L320:
			s2 = dsecnd_();
			untime = s2 - s1;
			++icl;
			if (icl <= ic) {
			    dtimmg_(&c__0, &c__1, &n, &y[1], &incx, &c__0, &
				    c__0);
			    goto L320;
			}

			time = (time - untime) / (doublereal) ic;
			ops = dopbl2_(cname, &n, &n, &c__0, &c__0);
			reslts_ref(1, in, iinc) = dmflop_(&ops, &time, &c__0);
/* L330: */
		    }
/* L340: */
		}
		io___52.ciunit = *nout;
		s_wsfe(&io___52);
		do_fio(&c__1, cname, (ftnlen)6);
		do_fio(&c__1, uplo, (ftnlen)1);
		e_wsfe();
		dprtbl_(" ", "N", &c__1, &nval[1], nn, &nval[1], ninc, &
			reslts[reslts_offset], ldr1, ldr2, nout, (ftnlen)1, (
			ftnlen)1);
/* L350: */
	    }

	} else if (s_cmp(cname, "DTRMV ", (ftnlen)6, (ftnlen)6) == 0) {

/*           Time DTRMV */

	    for (iuplo = 1; iuplo <= 2; ++iuplo) {
		*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];
		imat = 9;
		if (*(unsigned char *)uplo == 'L') {
		    imat = -9;
		}
		for (ita = 1; ita <= 2; ++ita) {
		    *(unsigned char *)transa = *(unsigned char *)&trans[ita - 
			    1];
		    i3 = 0;
		    i__1 = *nlda;
		    for (ilda = 1; ilda <= i__1; ++ilda) {
			lda = ldaval[ilda];
			i__2 = *ninc;
			for (iinc = 1; iinc <= i__2; ++iinc) {
			    incx = incval[iinc];
			    ++i3;
			    i__3 = *nn;
			    for (in = 1; in <= i__3; ++in) {
				n = nval[in];
				dtimmg_(&imat, &n, &n, &a[1], &lda, &c__0, &
					c__0);
				dtimmg_(&c__0, &c__1, &n, &x[1], &incx, &c__0,
					 &c__0);
				ic = 0;
				s1 = dsecnd_();
L360:
				dtrmv_(uplo, transa, "Non-unit", &n, &a[1], &
					lda, &x[1], &incx);
				s2 = dsecnd_();
				time = s2 - s1;
				++ic;
				if (time < *timmin) {
				    dtimmg_(&c__0, &c__1, &n, &x[1], &incx, &
					    c__0, &c__0);
				    goto L360;
				}

/*                          Subtract the time used in DTIMMG. */

				icl = 1;
				s1 = dsecnd_();
L370:
				s2 = dsecnd_();
				untime = s2 - s1;
				++icl;
				if (icl <= ic) {
				    dtimmg_(&c__0, &c__1, &n, &x[1], &incx, &
					    c__0, &c__0);
				    goto L370;
				}

				time = (time - untime) / (doublereal) ic;
				ops = dopbl2_(cname, &n, &n, &c__0, &c__0);
				reslts_ref(1, in, i3) = dmflop_(&ops, &time, &
					c__0);
/* L380: */
			    }
/* L390: */
			}
/* L400: */
		    }
		    io___53.ciunit = *nout;
		    s_wsfe(&io___53);
		    do_fio(&c__1, cname, (ftnlen)6);
		    do_fio(&c__1, uplo, (ftnlen)1);
		    do_fio(&c__1, transa, (ftnlen)1);
		    e_wsfe();
		    i__1 = *ninc * *nlda;
		    dprtbl_(" ", "N", &c__1, &nval[1], nn, &nval[1], &i__1, &
			    reslts[reslts_offset], ldr1, ldr2, nout, (ftnlen)
			    1, (ftnlen)1);
/* L410: */
		}
/* L420: */
	    }

	} else if (s_cmp(cname, "DTRSV ", (ftnlen)6, (ftnlen)6) == 0) {

/*           Time DTRSV */

	    for (iuplo = 1; iuplo <= 2; ++iuplo) {
		*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];
		imat = 9;
		if (*(unsigned char *)uplo == 'L') {
		    imat = -9;
		}
		for (ita = 1; ita <= 2; ++ita) {
		    *(unsigned char *)transa = *(unsigned char *)&trans[ita - 
			    1];
		    i3 = 0;
		    i__1 = *nlda;
		    for (ilda = 1; ilda <= i__1; ++ilda) {
			lda = ldaval[ilda];
			i__2 = *ninc;
			for (iinc = 1; iinc <= i__2; ++iinc) {
			    incx = incval[iinc];
			    ++i3;
			    i__3 = *nn;
			    for (in = 1; in <= i__3; ++in) {
				n = nval[in];
				dtimmg_(&imat, &n, &n, &a[1], &lda, &c__0, &
					c__0);
				dtimmg_(&c__0, &c__1, &n, &x[1], &incx, &c__0,
					 &c__0);
				ic = 0;
				s1 = dsecnd_();
L430:
				dtrsv_(uplo, transa, "Non-unit", &n, &a[1], &
					lda, &x[1], &incx);
				s2 = dsecnd_();
				time = s2 - s1;
				++ic;
				if (time < *timmin) {
				    dtimmg_(&c__0, &c__1, &n, &x[1], &incx, &
					    c__0, &c__0);
				    goto L430;
				}

/*                          Subtract the time used in DTIMMG. */

				icl = 1;
				s1 = dsecnd_();
L440:
				s2 = dsecnd_();
				untime = s2 - s1;
				++icl;
				if (icl <= ic) {
				    dtimmg_(&c__0, &c__1, &n, &x[1], &incx, &
					    c__0, &c__0);
				    goto L440;
				}

				time = (time - untime) / (doublereal) ic;
				ops = dopbl2_(cname, &n, &n, &c__0, &c__0);
				reslts_ref(1, in, i3) = dmflop_(&ops, &time, &
					c__0);
/* L450: */
			    }
/* L460: */
			}
/* L470: */
		    }
		    io___54.ciunit = *nout;
		    s_wsfe(&io___54);
		    do_fio(&c__1, cname, (ftnlen)6);
		    do_fio(&c__1, uplo, (ftnlen)1);
		    do_fio(&c__1, transa, (ftnlen)1);
		    e_wsfe();
		    i__1 = *ninc * *nlda;
		    dprtbl_(" ", "N", &c__1, &nval[1], nn, &nval[1], &i__1, &
			    reslts[reslts_offset], ldr1, ldr2, nout, (ftnlen)
			    1, (ftnlen)1);
/* L480: */
		}
/* L490: */
	    }

	} else if (s_cmp(cname, "DTBMV ", (ftnlen)6, (ftnlen)6) == 0) {

/*           Time DTBMV */

	    for (iuplo = 1; iuplo <= 2; ++iuplo) {
		*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];
		imat = 11;
		if (*(unsigned char *)uplo == 'L') {
		    imat = -11;
		}
		for (ita = 1; ita <= 2; ++ita) {
		    *(unsigned char *)transa = *(unsigned char *)&trans[ita - 
			    1];
		    i3 = 0;
		    i__1 = *nlda;
		    for (ilda = 1; ilda <= i__1; ++ilda) {
			lda = ldaval[ilda];
			i__2 = *ninc;
			for (iinc = 1; iinc <= i__2; ++iinc) {
			    incx = incval[iinc];
			    ++i3;
			    i__3 = *nk;
			    for (ik = 1; ik <= i__3; ++ik) {
				k = kval[ik];
				i__4 = *nn;
				for (in = 1; in <= i__4; ++in) {
				    n = nval[in];
				    dtimmg_(&imat, &n, &n, &a[1], &lda, &k, &
					    k);
				    dtimmg_(&c__0, &c__1, &n, &x[1], &incx, &
					    c__0, &c__0);
				    ic = 0;
				    s1 = dsecnd_();
L500:
				    dtbmv_(uplo, transa, "Non-unit", &n, &k, &
					    a[1], &lda, &x[1], &incx);
				    s2 = dsecnd_();
				    time = s2 - s1;
				    ++ic;
				    if (time < *timmin) {
					dtimmg_(&c__0, &c__1, &n, &x[1], &
						incx, &c__0, &c__0);
					goto L500;
				    }

/*                             Subtract the time used in DTIMMG. */

				    icl = 1;
				    s1 = dsecnd_();
L510:
				    s2 = dsecnd_();
				    untime = s2 - s1;
				    ++icl;
				    if (icl <= ic) {
					dtimmg_(&c__0, &c__1, &n, &x[1], &
						incx, &c__0, &c__0);
					goto L510;
				    }

				    time = (time - untime) / (doublereal) ic;
				    ops = dopbl2_(cname, &n, &n, &k, &k);
				    reslts_ref(ik, in, i3) = dmflop_(&ops, &
					    time, &c__0);
/* L520: */
				}
/* L530: */
			    }
/* L540: */
			}
/* L550: */
		    }
		    io___55.ciunit = *nout;
		    s_wsfe(&io___55);
		    do_fio(&c__1, cname, (ftnlen)6);
		    do_fio(&c__1, uplo, (ftnlen)1);
		    do_fio(&c__1, transa, (ftnlen)1);
		    e_wsfe();
		    i__1 = *ninc * *nlda;
		    dprtbl_("K", "N", nk, &kval[1], nn, &nval[1], &i__1, &
			    reslts[reslts_offset], ldr1, ldr2, nout, (ftnlen)
			    1, (ftnlen)1);
/* L560: */
		}
/* L570: */
	    }

	} else if (s_cmp(cname, "DTBSV ", (ftnlen)6, (ftnlen)6) == 0) {

/*           Time DTBSV */

	    for (iuplo = 1; iuplo <= 2; ++iuplo) {
		*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];
		imat = 11;
		if (*(unsigned char *)uplo == 'L') {
		    imat = -11;
		}
		for (ita = 1; ita <= 2; ++ita) {
		    *(unsigned char *)transa = *(unsigned char *)&trans[ita - 
			    1];
		    i3 = 0;
		    i__1 = *nlda;
		    for (ilda = 1; ilda <= i__1; ++ilda) {
			lda = ldaval[ilda];
			i__2 = *ninc;
			for (iinc = 1; iinc <= i__2; ++iinc) {
			    incx = incval[iinc];
			    ++i3;
			    i__3 = *nk;
			    for (ik = 1; ik <= i__3; ++ik) {
				k = kval[ik];
				i__4 = *nn;
				for (in = 1; in <= i__4; ++in) {
				    n = nval[in];
				    dtimmg_(&imat, &n, &n, &a[1], &lda, &k, &
					    k);
				    dtimmg_(&c__0, &c__1, &n, &x[1], &incx, &
					    c__0, &c__0);
				    ic = 0;
				    s1 = dsecnd_();
L580:
				    dtbsv_(uplo, transa, "Non-unit", &n, &k, &
					    a[1], &lda, &x[1], &incx);
				    s2 = dsecnd_();
				    time = s2 - s1;
				    ++ic;
				    if (time < *timmin) {
					dtimmg_(&c__0, &c__1, &n, &x[1], &
						incx, &c__0, &c__0);
					goto L580;
				    }

/*                             Subtract the time used in DTIMMG. */

				    icl = 1;
				    s1 = dsecnd_();
L590:
				    s2 = dsecnd_();
				    untime = s2 - s1;
				    ++icl;
				    if (icl <= ic) {
					dtimmg_(&c__0, &c__1, &n, &x[1], &
						incx, &c__0, &c__0);
					goto L590;
				    }

				    time = (time - untime) / (doublereal) ic;
				    ops = dopbl2_(cname, &n, &n, &k, &k);
				    reslts_ref(ik, in, i3) = dmflop_(&ops, &
					    time, &c__0);
/* L600: */
				}
/* L610: */
			    }
/* L620: */
			}
/* L630: */
		    }
		    io___56.ciunit = *nout;
		    s_wsfe(&io___56);
		    do_fio(&c__1, cname, (ftnlen)6);
		    do_fio(&c__1, uplo, (ftnlen)1);
		    do_fio(&c__1, transa, (ftnlen)1);
		    e_wsfe();
		    i__1 = *ninc * *nlda;
		    dprtbl_("K", "N", nk, &kval[1], nn, &nval[1], &i__1, &
			    reslts[reslts_offset], ldr1, ldr2, nout, (ftnlen)
			    1, (ftnlen)1);
/* L640: */
		}
/* L650: */
	    }

	} else if (s_cmp(cname, "DTPMV ", (ftnlen)6, (ftnlen)6) == 0) {

/*           Time DTPMV */

	    for (iuplo = 1; iuplo <= 2; ++iuplo) {
		*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];
		imat = 10;
		if (*(unsigned char *)uplo == 'L') {
		    imat = -10;
		}
		for (ita = 1; ita <= 2; ++ita) {
		    *(unsigned char *)transa = *(unsigned char *)&trans[ita - 
			    1];
		    ilda = 1;
		    lda = ldaval[ilda];
		    i__1 = *ninc;
		    for (iinc = 1; iinc <= i__1; ++iinc) {
			incx = incval[iinc];
			i__2 = *nn;
			for (in = 1; in <= i__2; ++in) {
			    n = nval[in];
			    i__3 = n * (n + 1) / 2;
			    dtimmg_(&imat, &n, &n, &a[1], &i__3, &c__0, &c__0)
				    ;
			    dtimmg_(&c__0, &c__1, &n, &x[1], &incx, &c__0, &
				    c__0);
			    ic = 0;
			    s1 = dsecnd_();
L660:
			    dtpmv_(uplo, transa, "Non-unit", &n, &a[1], &x[1],
				     &incx);
			    s2 = dsecnd_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				dtimmg_(&c__0, &c__1, &n, &x[1], &incx, &c__0,
					 &c__0);
				goto L660;
			    }

/*                       Subtract the time used in DTIMMG. */

			    icl = 1;
			    s1 = dsecnd_();
L670:
			    s2 = dsecnd_();
			    untime = s2 - s1;
			    ++icl;
			    if (icl <= ic) {
				dtimmg_(&c__0, &c__1, &n, &x[1], &incx, &c__0,
					 &c__0);
				goto L670;
			    }

			    time = (time - untime) / (doublereal) ic;
			    ops = dopbl2_(cname, &n, &n, &c__0, &c__0);
			    reslts_ref(1, in, iinc) = dmflop_(&ops, &time, &
				    c__0);
/* L680: */
			}
/* L690: */
		    }
		    io___57.ciunit = *nout;
		    s_wsfe(&io___57);
		    do_fio(&c__1, cname, (ftnlen)6);
		    do_fio(&c__1, uplo, (ftnlen)1);
		    do_fio(&c__1, transa, (ftnlen)1);
		    e_wsfe();
		    dprtbl_(" ", "N", &c__1, &nval[1], nn, &nval[1], ninc, &
			    reslts[reslts_offset], ldr1, ldr2, nout, (ftnlen)
			    1, (ftnlen)1);
/* L700: */
		}
/* L710: */
	    }

	} else if (s_cmp(cname, "DTPSV ", (ftnlen)6, (ftnlen)6) == 0) {

/*           Time DTPSV */

	    for (iuplo = 1; iuplo <= 2; ++iuplo) {
		*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];
		imat = 10;
		if (*(unsigned char *)uplo == 'L') {
		    imat = -10;
		}
		for (ita = 1; ita <= 2; ++ita) {
		    *(unsigned char *)transa = *(unsigned char *)&trans[ita - 
			    1];
		    ilda = 1;
		    lda = ldaval[ilda];
		    i__1 = *ninc;
		    for (iinc = 1; iinc <= i__1; ++iinc) {
			incx = incval[iinc];
			i__2 = *nn;
			for (in = 1; in <= i__2; ++in) {
			    n = nval[in];
			    i__3 = n * (n + 1) / 2;
			    dtimmg_(&imat, &n, &n, &a[1], &i__3, &c__0, &c__0)
				    ;
			    dtimmg_(&c__0, &c__1, &n, &x[1], &incx, &c__0, &
				    c__0);
			    ic = 0;
			    s1 = dsecnd_();
L720:
			    dtpsv_(uplo, transa, "Non-unit", &n, &a[1], &x[1],
				     &incx);
			    s2 = dsecnd_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				dtimmg_(&c__0, &c__1, &n, &x[1], &incx, &c__0,
					 &c__0);
				goto L720;
			    }

/*                       Subtract the time used in DTIMMG. */

			    icl = 1;
			    s1 = dsecnd_();
L730:
			    s2 = dsecnd_();
			    untime = s2 - s1;
			    ++icl;
			    if (icl <= ic) {
				dtimmg_(&c__0, &c__1, &n, &x[1], &incx, &c__0,
					 &c__0);
				goto L730;
			    }

			    time = (time - untime) / (doublereal) ic;
			    ops = dopbl2_(cname, &n, &n, &c__0, &c__0);
			    reslts_ref(1, in, iinc) = dmflop_(&ops, &time, &
				    c__0);
/* L740: */
			}
/* L750: */
		    }
		    io___58.ciunit = *nout;
		    s_wsfe(&io___58);
		    do_fio(&c__1, cname, (ftnlen)6);
		    do_fio(&c__1, uplo, (ftnlen)1);
		    do_fio(&c__1, transa, (ftnlen)1);
		    e_wsfe();
		    dprtbl_(" ", "N", &c__1, &nval[1], nn, &nval[1], ninc, &
			    reslts[reslts_offset], ldr1, ldr2, nout, (ftnlen)
			    1, (ftnlen)1);
/* L760: */
		}
/* L770: */
	    }

	} else if (s_cmp(cname, "DGER  ", (ftnlen)6, (ftnlen)6) == 0) {

/*           Time DGER */

	    i3 = 0;
	    i__1 = *nlda;
	    for (ilda = 1; ilda <= i__1; ++ilda) {
		lda = ldaval[ilda];
		i__2 = *ninc;
		for (iinc = 1; iinc <= i__2; ++iinc) {
		    incx = incval[iinc];
		    ++i3;
		    i__3 = *nm;
		    for (im = 1; im <= i__3; ++im) {
			m = mval[im];
			i__4 = *nn;
			for (in = 1; in <= i__4; ++in) {
			    n = nval[in];
			    dtimmg_(&c__0, &c__1, &m, &x[1], &incx, &c__0, &
				    c__0);
			    dtimmg_(&c__0, &c__1, &n, &y[1], &incx, &c__0, &
				    c__0);
			    dtimmg_(&c__1, &m, &n, &a[1], &lda, &c__0, &c__0);
			    ic = 0;
			    s1 = dsecnd_();
L780:
			    dger_(&m, &n, &c_b76, &x[1], &incx, &y[1], &incx, 
				    &a[1], &lda);
			    s2 = dsecnd_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				dtimmg_(&c__1, &m, &n, &a[1], &lda, &c__0, &
					c__0);
				goto L780;
			    }

/*                       Subtract the time used in DTIMMG. */

			    icl = 1;
			    s1 = dsecnd_();
L790:
			    s2 = dsecnd_();
			    untime = s2 - s1;
			    ++icl;
			    if (icl <= ic) {
				dtimmg_(&c__1, &m, &n, &a[1], &lda, &c__0, &
					c__0);
				goto L790;
			    }

			    time = (time - untime) / (doublereal) ic;
			    ops = dopbl2_(cname, &m, &n, &c__0, &c__0);
			    reslts_ref(im, in, i3) = dmflop_(&ops, &time, &
				    c__0);
/* L800: */
			}
/* L810: */
		    }
/* L820: */
		}
/* L830: */
	    }
	    io___59.ciunit = *nout;
	    s_wsfe(&io___59);
	    e_wsfe();
	    i__1 = *ninc * *nlda;
	    dprtbl_("M", "N", nm, &mval[1], nn, &nval[1], &i__1, &reslts[
		    reslts_offset], ldr1, ldr2, nout, (ftnlen)1, (ftnlen)1);

	} else if (s_cmp(cname, "DSYR  ", (ftnlen)6, (ftnlen)6) == 0) {

/*           Time DSYR */

	    for (iuplo = 1; iuplo <= 2; ++iuplo) {
		*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];
		imat = 6;
		if (*(unsigned char *)uplo == 'L') {
		    imat = -6;
		}
		i3 = 0;
		i__1 = *nlda;
		for (ilda = 1; ilda <= i__1; ++ilda) {
		    lda = ldaval[ilda];
		    i__2 = *ninc;
		    for (iinc = 1; iinc <= i__2; ++iinc) {
			incx = incval[iinc];
			++i3;
			i__3 = *nn;
			for (in = 1; in <= i__3; ++in) {
			    n = nval[in];
			    dtimmg_(&c__0, &c__1, &n, &x[1], &incx, &c__0, &
				    c__0);
			    dtimmg_(&c__0, &c__1, &n, &y[1], &incx, &c__0, &
				    c__0);
			    dtimmg_(&imat, &n, &n, &a[1], &lda, &c__0, &c__0);
			    ic = 0;
			    s1 = dsecnd_();
L840:
			    dsyr_(uplo, &n, &c_b76, &x[1], &incx, &a[1], &lda);
			    s2 = dsecnd_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				dtimmg_(&imat, &n, &n, &a[1], &lda, &c__0, &
					c__0);
				goto L840;
			    }

/*                       Subtract the time used in DTIMMG. */

			    icl = 1;
			    s1 = dsecnd_();
L850:
			    s2 = dsecnd_();
			    untime = s2 - s1;
			    ++icl;
			    if (icl <= ic) {
				dtimmg_(&imat, &n, &n, &a[1], &lda, &c__0, &
					c__0);
				goto L850;
			    }

			    time = (time - untime) / (doublereal) ic;
			    ops = dopbl2_(cname, &n, &n, &c__0, &c__0);
			    reslts_ref(1, in, i3) = dmflop_(&ops, &time, &
				    c__0);
/* L860: */
			}
/* L870: */
		    }
/* L880: */
		}
		io___60.ciunit = *nout;
		s_wsfe(&io___60);
		do_fio(&c__1, cname, (ftnlen)6);
		do_fio(&c__1, uplo, (ftnlen)1);
		e_wsfe();
		i__1 = *ninc * *nlda;
		dprtbl_(" ", "N", &c__1, &nval[1], nn, &nval[1], &i__1, &
			reslts[reslts_offset], ldr1, ldr2, nout, (ftnlen)1, (
			ftnlen)1);
/* L890: */
	    }

	} else if (s_cmp(cname, "DSYR2 ", (ftnlen)6, (ftnlen)6) == 0) {

/*           Time DSYR2 */

	    for (iuplo = 1; iuplo <= 2; ++iuplo) {
		*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];
		imat = 6;
		if (*(unsigned char *)uplo == 'L') {
		    imat = -6;
		}
		i3 = 0;
		i__1 = *nlda;
		for (ilda = 1; ilda <= i__1; ++ilda) {
		    lda = ldaval[ilda];
		    i__2 = *ninc;
		    for (iinc = 1; iinc <= i__2; ++iinc) {
			incx = incval[iinc];
			++i3;
			i__3 = *nn;
			for (in = 1; in <= i__3; ++in) {
			    n = nval[in];
			    dtimmg_(&c__0, &c__1, &n, &x[1], &incx, &c__0, &
				    c__0);
			    dtimmg_(&c__0, &c__1, &n, &y[1], &incx, &c__0, &
				    c__0);
			    dtimmg_(&imat, &n, &n, &a[1], &lda, &c__0, &c__0);
			    ic = 0;
			    s1 = dsecnd_();
L900:
			    dsyr2_(uplo, &n, &c_b76, &x[1], &incx, &y[1], &
				    incx, &a[1], &lda);
			    s2 = dsecnd_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				dtimmg_(&imat, &n, &n, &a[1], &lda, &c__0, &
					c__0);
				goto L900;
			    }

/*                       Subtract the time used in DTIMMG. */

			    icl = 1;
			    s1 = dsecnd_();
L910:
			    s2 = dsecnd_();
			    untime = s2 - s1;
			    ++icl;
			    if (icl <= ic) {
				dtimmg_(&imat, &n, &n, &a[1], &lda, &c__0, &
					c__0);
				goto L910;
			    }

			    time = (time - untime) / (doublereal) ic;
			    ops = dopbl2_(cname, &n, &n, &c__0, &c__0);
			    reslts_ref(1, in, i3) = dmflop_(&ops, &time, &
				    c__0);
/* L920: */
			}
/* L930: */
		    }
/* L940: */
		}
		io___61.ciunit = *nout;
		s_wsfe(&io___61);
		do_fio(&c__1, cname, (ftnlen)6);
		do_fio(&c__1, uplo, (ftnlen)1);
		e_wsfe();
		i__1 = *ninc * *nlda;
		dprtbl_(" ", "N", &c__1, &nval[1], nn, &nval[1], &i__1, &
			reslts[reslts_offset], ldr1, ldr2, nout, (ftnlen)1, (
			ftnlen)1);
/* L950: */
	    }

	} else if (s_cmp(cname, "DSPR  ", (ftnlen)6, (ftnlen)6) == 0) {

/*           Time DSPR */

	    for (iuplo = 1; iuplo <= 2; ++iuplo) {
		*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];
		imat = 7;
		if (*(unsigned char *)uplo == 'L') {
		    imat = -7;
		}
		ilda = 1;
		lda = ldaval[ilda];
		i__1 = *ninc;
		for (iinc = 1; iinc <= i__1; ++iinc) {
		    incx = incval[iinc];
		    i__2 = *nn;
		    for (in = 1; in <= i__2; ++in) {
			n = nval[in];
			dtimmg_(&c__0, &c__1, &n, &x[1], &incx, &c__0, &c__0);
			dtimmg_(&c__0, &c__1, &n, &y[1], &incx, &c__0, &c__0);
			i__3 = n * (n + 1) / 2;
			dtimmg_(&imat, &n, &n, &a[1], &i__3, &c__0, &c__0);
			ic = 0;
			s1 = dsecnd_();
L960:
			dspr_(uplo, &n, &c_b76, &x[1], &incx, &a[1]);
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    i__3 = n * (n + 1) / 2;
			    dtimmg_(&imat, &n, &n, &a[1], &i__3, &c__0, &c__0)
				    ;
			    goto L960;
			}

/*                    Subtract the time used in DTIMMG. */

			icl = 1;
			s1 = dsecnd_();
L970:
			s2 = dsecnd_();
			untime = s2 - s1;
			++icl;
			if (icl <= ic) {
			    i__3 = n * (n + 1) / 2;
			    dtimmg_(&imat, &n, &n, &a[1], &i__3, &c__0, &c__0)
				    ;
			    goto L970;
			}

			time = (time - untime) / (doublereal) ic;
			ops = dopbl2_(cname, &n, &n, &c__0, &c__0);
			reslts_ref(1, in, iinc) = dmflop_(&ops, &time, &c__0);
/* L980: */
		    }
/* L990: */
		}
		io___62.ciunit = *nout;
		s_wsfe(&io___62);
		do_fio(&c__1, cname, (ftnlen)6);
		do_fio(&c__1, uplo, (ftnlen)1);
		e_wsfe();
		dprtbl_(" ", "N", &c__1, &nval[1], nn, &nval[1], ninc, &
			reslts[reslts_offset], ldr1, ldr2, nout, (ftnlen)1, (
			ftnlen)1);
/* L1000: */
	    }

	} else if (s_cmp(cname, "DSPR2 ", (ftnlen)6, (ftnlen)6) == 0) {

/*           Time DSPR2 */

	    for (iuplo = 1; iuplo <= 2; ++iuplo) {
		*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];
		imat = 7;
		if (*(unsigned char *)uplo == 'L') {
		    imat = -7;
		}
		ilda = 1;
		lda = ldaval[ilda];
		i__1 = *ninc;
		for (iinc = 1; iinc <= i__1; ++iinc) {
		    incx = incval[iinc];
		    i__2 = *nn;
		    for (in = 1; in <= i__2; ++in) {
			n = nval[in];
			dtimmg_(&c__0, &c__1, &n, &x[1], &incx, &c__0, &c__0);
			dtimmg_(&c__0, &c__1, &n, &y[1], &incx, &c__0, &c__0);
			i__3 = n * (n + 1) / 2;
			dtimmg_(&imat, &n, &n, &a[1], &i__3, &c__0, &c__0);
			ic = 0;
			s1 = dsecnd_();
L1010:
			dspr2_(uplo, &n, &c_b76, &x[1], &incx, &y[1], &incx, &
				a[1]);
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    i__3 = n * (n + 1) / 2;
			    dtimmg_(&imat, &n, &n, &a[1], &i__3, &c__0, &c__0)
				    ;
			    goto L1010;
			}

/*                    Subtract the time used in DTIMMG. */

			icl = 1;
			s1 = dsecnd_();
L1020:
			s2 = dsecnd_();
			untime = s2 - s1;
			++icl;
			if (icl <= ic) {
			    i__3 = n * (n + 1) / 2;
			    dtimmg_(&imat, &n, &n, &a[1], &i__3, &c__0, &c__0)
				    ;
			    goto L1020;
			}

			time = (time - untime) / (doublereal) ic;
			ops = dopbl2_(cname, &n, &n, &c__0, &c__0);
			reslts_ref(1, in, iinc) = dmflop_(&ops, &time, &c__0);
/* L1030: */
		    }
/* L1040: */
		}
		io___63.ciunit = *nout;
		s_wsfe(&io___63);
		do_fio(&c__1, cname, (ftnlen)6);
		do_fio(&c__1, uplo, (ftnlen)1);
		e_wsfe();
		dprtbl_(" ", "N", &c__1, &nval[1], nn, &nval[1], ninc, &
			reslts[reslts_offset], ldr1, ldr2, nout, (ftnlen)1, (
			ftnlen)1);
/* L1050: */
	    }
	}
	io___64.ciunit = *nout;
	s_wsfe(&io___64);
	e_wsfe();
L1060:
	;
    }
L1070:

    return 0;

/*     End of DTIMB2 */

} /* dtimb2_ */

#undef reslts_ref
#undef names_ref


