#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublecomplex c_b1 = {1.,0.};
static integer c__21 = 21;
static integer c__1 = 1;
static integer c__0 = 0;
static integer c__4 = 4;
static integer c__2 = 2;
static integer c_n2 = -2;
static doublereal c_b561 = 1.;

/* Subroutine */ int ztimb2_(char *line, integer *nm, integer *mval, integer *
	nn, integer *nval, integer *nk, integer *kval, integer *ninc, integer 
	*incval, integer *nlda, integer *ldaval, integer *la, doublereal *
	timmin, doublecomplex *a, doublecomplex *x, doublecomplex *y, 
	doublereal *reslts, integer *ldr1, integer *ldr2, integer *nout, 
	ftnlen line_len)
{
    /* Initialized data */

    static char trans[1*3] = "N" "T" "C";
    static char uplos[1*2] = "U" "L";
    static char names[6*21] = "ZGEMV " "ZGBMV " "ZHEMV " "ZHBMV " "ZHPMV " 
	    "ZTRMV " "ZTBMV " "ZTPMV " "ZTRSV " "ZTBSV " "ZTPSV " "ZGERU " 
	    "ZGERC " "ZHER  " "ZHPR  " "ZHER2 " "ZHPR2 " "ZSYMV " "ZSYR  " 
	    "ZSPMV " "ZSPR  ";

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
    static char fmt_9989[] = "(/1x,\002ZGEMV  with TRANS = '\002,a1,\002'"
	    "\002,/)";
    static char fmt_9988[] = "(/1x,\002ZGBMV  with TRANS = '\002,a1,\002', M"
	    " = N and KL = K\002,\002U \002,\002= K\002,/)";
    static char fmt_9986[] = "(/1x,a6,\002 with UPLO = '\002,a1,\002'\002,/)";
    static char fmt_9987[] = "(/1x,a6,\002 with UPLO = '\002,a1,\002', TRANS"
	    " = '\002,a1,\002'\002,/)";
    static char fmt_9985[] = "(/1x,a6,/)";
    static char fmt_9984[] = "(/////)";

    /* System generated locals */
    integer reslts_dim1, reslts_dim2, reslts_offset, i__1, i__2, i__3, i__4;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), s_wsfe(cilist *), do_fio(
	    integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static integer ilda, iinc, imat, info;
    static char path[3];
    static doublereal time;
    static integer incx, isub;
    extern /* Subroutine */ int zher_(char *, integer *, doublereal *, 
	    doublecomplex *, integer *, doublecomplex *, integer *);
    static char uplo[1];
    extern /* Subroutine */ int zhpr_(char *, integer *, doublereal *, 
	    doublecomplex *, integer *, doublecomplex *), zspr_(char *
	    , integer *, doublecomplex *, doublecomplex *, integer *, 
	    doublecomplex *), zsyr_(char *, integer *, doublecomplex *
	    , doublecomplex *, integer *, doublecomplex *, integer *),
	     zher2_(char *, integer *, doublecomplex *, doublecomplex *, 
	    integer *, doublecomplex *, integer *, doublecomplex *, integer *), zhpr2_(char *, integer *, doublecomplex *, 
	    doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublecomplex *);
    static integer i__, j, k, m, n;
    static char cname[6];
    static integer laval[1];
    extern /* Subroutine */ int zgerc_(integer *, integer *, doublecomplex *, 
	    doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublecomplex *, integer *), zgbmv_(char *, integer *, integer *, 
	    integer *, integer *, doublecomplex *, doublecomplex *, integer *,
	     doublecomplex *, integer *, doublecomplex *, doublecomplex *, 
	    integer *), zhbmv_(char *, integer *, integer *, 
	    doublecomplex *, doublecomplex *, integer *, doublecomplex *, 
	    integer *, doublecomplex *, doublecomplex *, integer *);
    static integer iuplo;
    extern /* Subroutine */ int zgemv_(char *, integer *, integer *, 
	    doublecomplex *, doublecomplex *, integer *, doublecomplex *, 
	    integer *, doublecomplex *, doublecomplex *, integer *), 
	    zhemv_(char *, integer *, doublecomplex *, doublecomplex *, 
	    integer *, doublecomplex *, integer *, doublecomplex *, 
	    doublecomplex *, integer *);
    static integer i3;
    extern /* Subroutine */ int zgeru_(integer *, integer *, doublecomplex *, 
	    doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublecomplex *, integer *), ztbmv_(char *, char *, char *, 
	    integer *, integer *, doublecomplex *, integer *, doublecomplex *,
	     integer *);
    static doublereal s1, s2;
    extern /* Subroutine */ int zhpmv_(char *, integer *, doublecomplex *, 
	    doublecomplex *, doublecomplex *, integer *, doublecomplex *, 
	    doublecomplex *, integer *), ztbsv_(char *, char *, char *
	    , integer *, integer *, doublecomplex *, integer *, doublecomplex 
	    *, integer *), zspmv_(char *, integer *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, integer *, 
	    doublecomplex *, doublecomplex *, integer *), ztpmv_(char 
	    *, char *, char *, integer *, doublecomplex *, doublecomplex *, 
	    integer *);
    extern doublereal dopbl2_(char *, integer *, integer *, integer *, 
	    integer *);
    extern /* Subroutine */ int ztrmv_(char *, char *, char *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *), ztpsv_(char *, char *, char *, integer *, 
	    doublecomplex *, doublecomplex *, integer *), zsymv_(char *, integer *, doublecomplex *, doublecomplex 
	    *, integer *, doublecomplex *, integer *, doublecomplex *, 
	    doublecomplex *, integer *), ztrsv_(char *, char *, char *
	    , integer *, doublecomplex *, integer *, doublecomplex *, integer 
	    *);
    static integer ic, ik, im, in;
    extern doublereal dsecnd_(void);
    static integer nx, ny;
    extern /* Subroutine */ int atimck_(integer *, char *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, ftnlen);
    extern doublereal dmflop_(doublereal *, doublereal *, integer *);
    extern /* Subroutine */ int atimin_(char *, char *, integer *, char *, 
	    logical *, integer *, integer *, ftnlen, ftnlen, ftnlen), dprtbl_(
	    char *, char *, integer *, integer *, integer *, integer *, 
	    integer *, doublereal *, integer *, integer *, integer *, ftnlen, 
	    ftnlen);
    static char transa[1];
    static logical ixandy;
    static doublereal untime;
    static logical timsub[21];
    extern /* Subroutine */ int ztimmg_(integer *, integer *, integer *, 
	    doublecomplex *, integer *, integer *, integer *);
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
    static cilist io___53 = { 0, 0, 0, fmt_9986, 0 };
    static cilist io___54 = { 0, 0, 0, fmt_9986, 0 };
    static cilist io___55 = { 0, 0, 0, fmt_9987, 0 };
    static cilist io___56 = { 0, 0, 0, fmt_9987, 0 };
    static cilist io___57 = { 0, 0, 0, fmt_9987, 0 };
    static cilist io___58 = { 0, 0, 0, fmt_9987, 0 };
    static cilist io___59 = { 0, 0, 0, fmt_9987, 0 };
    static cilist io___60 = { 0, 0, 0, fmt_9987, 0 };
    static cilist io___61 = { 0, 0, 0, fmt_9985, 0 };
    static cilist io___62 = { 0, 0, 0, fmt_9985, 0 };
    static cilist io___63 = { 0, 0, 0, fmt_9986, 0 };
    static cilist io___64 = { 0, 0, 0, fmt_9986, 0 };
    static cilist io___65 = { 0, 0, 0, fmt_9986, 0 };
    static cilist io___66 = { 0, 0, 0, fmt_9986, 0 };
    static cilist io___67 = { 0, 0, 0, fmt_9986, 0 };
    static cilist io___68 = { 0, 0, 0, fmt_9986, 0 };
    static cilist io___69 = { 0, 0, 0, fmt_9984, 0 };



#define names_ref(a_0,a_1) &names[(a_1)*6 + a_0 - 6]
#define reslts_ref(a_1,a_2,a_3) reslts[((a_3)*reslts_dim2 + (a_2))*\
reslts_dim1 + a_1]


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    ZTIMB2 times the BLAS 2 routines.   

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

    A       (workspace) COMPLEX*16 array, dimension (LA)   

    X       (workspace) COMPLEX*16 array, dimension (NMAX*INCMAX)   
               where NMAX and INCMAX are the maximum values permitted   
               for N and INCX.   

    Y       (workspace) COMPLEX*16 array, dimension (NMAX*INCMAX)   
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

    s_copy(path, "Zomplex precision", (ftnlen)1, (ftnlen)17);
    s_copy(path + 1, "B2", (ftnlen)2, (ftnlen)2);
    atimin_(path, line, &c__21, names, timsub, nout, &info, (ftnlen)3, 
	    line_len, (ftnlen)6);
    if (info != 0) {
	goto L1350;
    }

/*     Time each routine */

    for (isub = 1; isub <= 21; ++isub) {
	if (! timsub[isub - 1]) {
	    goto L1340;
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
	    goto L1340;
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

/*        Time ZGEMV */

	if (s_cmp(cname, "ZGEMV ", (ftnlen)6, (ftnlen)6) == 0) {
	    for (ita = 1; ita <= 3; ++ita) {
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
				ztimmg_(&c__1, &m, &n, &a[1], &lda, &c__0, &
					c__0);
				ztimmg_(&c__0, &c__1, &nx, &x[1], &incx, &
					c__0, &c__0);
				ztimmg_(&c__0, &c__1, &ny, &y[1], &incx, &
					c__0, &c__0);
				ic = 0;
				s1 = dsecnd_();
L40:
				zgemv_(transa, &m, &n, &c_b1, &a[1], &lda, &x[
					1], &incx, &c_b1, &y[1], &incx);
				s2 = dsecnd_();
				time = s2 - s1;
				++ic;
				if (time < *timmin) {
				    ztimmg_(&c__0, &c__1, &ny, &y[1], &incx, &
					    c__0, &c__0);
				    goto L40;
				}

/*                          Subtract the time used in ZTIMMG. */

				icl = 1;
				s1 = dsecnd_();
L50:
				s2 = dsecnd_();
				untime = s2 - s1;
				++icl;
				if (icl <= ic) {
				    ztimmg_(&c__0, &c__1, &ny, &y[1], &incx, &
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

/*        Time ZGBMV */

	} else if (s_cmp(cname, "ZGBMV ", (ftnlen)6, (ftnlen)6) == 0) {
	    for (ita = 1; ita <= 3; ++ita) {
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
				ztimmg_(&c_n2, &m, &n, &a[1], &lda, &k, &k);
				ztimmg_(&c__0, &c__1, &n, &x[1], &incx, &c__0,
					 &c__0);
				ztimmg_(&c__0, &c__1, &m, &y[1], &incx, &c__0,
					 &c__0);
				ic = 0;
				s1 = dsecnd_();
L110:
				zgbmv_(transa, &m, &n, &k, &k, &c_b1, &a[1], &
					lda, &x[1], &incx, &c_b1, &y[1], &
					incx);
				s2 = dsecnd_();
				time = s2 - s1;
				++ic;
				if (time < *timmin) {
				    ztimmg_(&c__0, &c__1, &m, &y[1], &incx, &
					    c__0, &c__0);
				    goto L110;
				}

/*                          Subtract the time used in ZTIMMG. */

				icl = 1;
				s1 = dsecnd_();
L120:
				s2 = dsecnd_();
				untime = s2 - s1;
				++icl;
				if (icl <= ic) {
				    ztimmg_(&c__0, &c__1, &m, &y[1], &incx, &
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

/*        Time ZHEMV */

	} else if (s_cmp(cname, "ZHEMV ", (ftnlen)6, (ftnlen)6) == 0) {
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
			    ztimmg_(&imat, &n, &n, &a[1], &lda, &c__0, &c__0);
			    ztimmg_(&c__0, &c__1, &n, &x[1], &incx, &c__0, &
				    c__0);
			    ztimmg_(&c__0, &c__1, &n, &y[1], &incx, &c__0, &
				    c__0);
			    ic = 0;
			    s1 = dsecnd_();
L180:
			    zhemv_(uplo, &n, &c_b1, &a[1], &lda, &x[1], &incx,
				     &c_b1, &y[1], &incx);
			    s2 = dsecnd_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				ztimmg_(&c__0, &c__1, &n, &y[1], &incx, &c__0,
					 &c__0);
				goto L180;
			    }

/*                       Subtract the time used in ZTIMMG. */

			    icl = 1;
			    s1 = dsecnd_();
L190:
			    s2 = dsecnd_();
			    untime = s2 - s1;
			    ++icl;
			    if (icl <= ic) {
				ztimmg_(&c__0, &c__1, &n, &y[1], &incx, &c__0,
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

/*        Time ZSYMV */

	} else if (s_cmp(cname, "ZSYMV ", (ftnlen)6, (ftnlen)6) == 0) {
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
			i__3 = *nn;
			for (in = 1; in <= i__3; ++in) {
			    n = nval[in];
			    ztimmg_(&imat, &n, &n, &a[1], &lda, &c__0, &c__0);
			    ztimmg_(&c__0, &c__1, &n, &x[1], &incx, &c__0, &
				    c__0);
			    ztimmg_(&c__0, &c__1, &n, &y[1], &incx, &c__0, &
				    c__0);
			    ic = 0;
			    s1 = dsecnd_();
L240:
			    zsymv_(uplo, &n, &c_b1, &a[1], &lda, &x[1], &incx,
				     &c_b1, &y[1], &incx);
			    s2 = dsecnd_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				ztimmg_(&c__0, &c__1, &n, &y[1], &incx, &c__0,
					 &c__0);
				goto L240;
			    }

/*                       Subtract the time used in ZTIMMG. */

			    icl = 1;
			    s1 = dsecnd_();
L250:
			    s2 = dsecnd_();
			    untime = s2 - s1;
			    ++icl;
			    if (icl <= ic) {
				ztimmg_(&c__0, &c__1, &n, &y[1], &incx, &c__0,
					 &c__0);
				goto L250;
			    }

			    time = (time - untime) / (doublereal) ic;
			    ops = dopbl2_(cname, &n, &n, &c__0, &c__0);
			    reslts_ref(1, in, i3) = dmflop_(&ops, &time, &
				    c__0);
/* L260: */
			}
/* L270: */
		    }
/* L280: */
		}
		io___51.ciunit = *nout;
		s_wsfe(&io___51);
		do_fio(&c__1, cname, (ftnlen)6);
		do_fio(&c__1, uplo, (ftnlen)1);
		e_wsfe();
		i__1 = *ninc * *nlda;
		dprtbl_(" ", "N", &c__1, &nval[1], nn, &nval[1], &i__1, &
			reslts[reslts_offset], ldr1, ldr2, nout, (ftnlen)1, (
			ftnlen)1);
/* L290: */
	    }

/*        Time ZHBMV */

	} else if (s_cmp(cname, "ZHBMV ", (ftnlen)6, (ftnlen)6) == 0) {
	    for (iuplo = 1; iuplo <= 2; ++iuplo) {
		*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];
		imat = 5;
		if (*(unsigned char *)uplo == 'L') {
		    imat = -5;
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
				ztimmg_(&imat, &n, &n, &a[1], &lda, &k, &k);
				ztimmg_(&c__0, &c__1, &n, &x[1], &incx, &c__0,
					 &c__0);
				ztimmg_(&c__0, &c__1, &n, &y[1], &incx, &c__0,
					 &c__0);
				ic = 0;
				s1 = dsecnd_();
L300:
				zhbmv_(uplo, &n, &k, &c_b1, &a[1], &lda, &x[1]
					, &incx, &c_b1, &y[1], &incx);
				s2 = dsecnd_();
				time = s2 - s1;
				++ic;
				if (time < *timmin) {
				    ztimmg_(&c__0, &c__1, &n, &y[1], &incx, &
					    c__0, &c__0);
				    goto L300;
				}

/*                          Subtract the time used in ZTIMMG. */

				icl = 1;
				s1 = dsecnd_();
L310:
				s2 = dsecnd_();
				untime = s2 - s1;
				++icl;
				if (icl <= ic) {
				    ztimmg_(&c__0, &c__1, &n, &y[1], &incx, &
					    c__0, &c__0);
				    goto L310;
				}

				time = (time - untime) / (doublereal) ic;
				ops = dopbl2_(cname, &n, &n, &k, &k);
				reslts_ref(ik, in, i3) = dmflop_(&ops, &time, 
					&c__0);
/* L320: */
			    }
/* L330: */
			}
/* L340: */
		    }
/* L350: */
		}
		io___52.ciunit = *nout;
		s_wsfe(&io___52);
		do_fio(&c__1, cname, (ftnlen)6);
		do_fio(&c__1, uplo, (ftnlen)1);
		e_wsfe();
		i__1 = *ninc * *nlda;
		dprtbl_("K", "N", nk, &kval[1], nn, &nval[1], &i__1, &reslts[
			reslts_offset], ldr1, ldr2, nout, (ftnlen)1, (ftnlen)
			1);
/* L360: */
	    }

/*        Time ZHPMV */

	} else if (s_cmp(cname, "ZHPMV ", (ftnlen)6, (ftnlen)6) == 0) {
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
			ztimmg_(&imat, &n, &n, &a[1], &i__3, &c__0, &c__0);
			ztimmg_(&c__0, &c__1, &n, &x[1], &incx, &c__0, &c__0);
			ztimmg_(&c__0, &c__1, &n, &y[1], &incx, &c__0, &c__0);
			ic = 0;
			s1 = dsecnd_();
L370:
			zhpmv_(uplo, &n, &c_b1, &a[1], &x[1], &incx, &c_b1, &
				y[1], &incx);
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    ztimmg_(&c__0, &c__1, &n, &y[1], &incx, &c__0, &
				    c__0);
			    goto L370;
			}

/*                    Subtract the time used in ZTIMMG. */

			icl = 1;
			s1 = dsecnd_();
L380:
			s2 = dsecnd_();
			untime = s2 - s1;
			++icl;
			if (icl <= ic) {
			    ztimmg_(&c__0, &c__1, &n, &y[1], &incx, &c__0, &
				    c__0);
			    goto L380;
			}

			time = (time - untime) / (doublereal) ic;
			ops = dopbl2_(cname, &n, &n, &c__0, &c__0);
			reslts_ref(1, in, iinc) = dmflop_(&ops, &time, &c__0);
/* L390: */
		    }
/* L400: */
		}
		io___53.ciunit = *nout;
		s_wsfe(&io___53);
		do_fio(&c__1, cname, (ftnlen)6);
		do_fio(&c__1, uplo, (ftnlen)1);
		e_wsfe();
		dprtbl_(" ", "N", &c__1, &nval[1], nn, &nval[1], ninc, &
			reslts[reslts_offset], ldr1, ldr2, nout, (ftnlen)1, (
			ftnlen)1);
/* L410: */
	    }

/*        Time ZSPMV */

	} else if (s_cmp(cname, "ZSPMV ", (ftnlen)6, (ftnlen)6) == 0) {
	    for (iuplo = 1; iuplo <= 2; ++iuplo) {
		*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];
		imat = 9;
		if (*(unsigned char *)uplo == 'L') {
		    imat = -9;
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
			ztimmg_(&imat, &n, &n, &a[1], &i__3, &c__0, &c__0);
			ztimmg_(&c__0, &c__1, &n, &x[1], &incx, &c__0, &c__0);
			ztimmg_(&c__0, &c__1, &n, &y[1], &incx, &c__0, &c__0);
			ic = 0;
			s1 = dsecnd_();
L420:
			zspmv_(uplo, &n, &c_b1, &a[1], &x[1], &incx, &c_b1, &
				y[1], &incx);
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    ztimmg_(&c__0, &c__1, &n, &y[1], &incx, &c__0, &
				    c__0);
			    goto L420;
			}

/*                    Subtract the time used in ZTIMMG. */

			icl = 1;
			s1 = dsecnd_();
L430:
			s2 = dsecnd_();
			untime = s2 - s1;
			++icl;
			if (icl <= ic) {
			    ztimmg_(&c__0, &c__1, &n, &y[1], &incx, &c__0, &
				    c__0);
			    goto L430;
			}

			time = (time - untime) / (doublereal) ic;
			ops = dopbl2_(cname, &n, &n, &c__0, &c__0);
			reslts_ref(1, in, iinc) = dmflop_(&ops, &time, &c__0);
/* L440: */
		    }
/* L450: */
		}
		io___54.ciunit = *nout;
		s_wsfe(&io___54);
		do_fio(&c__1, cname, (ftnlen)6);
		do_fio(&c__1, uplo, (ftnlen)1);
		e_wsfe();
		dprtbl_(" ", "N", &c__1, &nval[1], nn, &nval[1], ninc, &
			reslts[reslts_offset], ldr1, ldr2, nout, (ftnlen)1, (
			ftnlen)1);
/* L460: */
	    }

/*        Time ZTRMV */

	} else if (s_cmp(cname, "ZTRMV ", (ftnlen)6, (ftnlen)6) == 0) {
	    for (iuplo = 1; iuplo <= 2; ++iuplo) {
		*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];
		imat = 11;
		if (*(unsigned char *)uplo == 'L') {
		    imat = -11;
		}
		for (ita = 1; ita <= 3; ++ita) {
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
				ztimmg_(&imat, &n, &n, &a[1], &lda, &c__0, &
					c__0);
				ztimmg_(&c__0, &c__1, &n, &x[1], &incx, &c__0,
					 &c__0);
				ic = 0;
				s1 = dsecnd_();
L470:
				ztrmv_(uplo, transa, "Non-unit", &n, &a[1], &
					lda, &x[1], &incx);
				s2 = dsecnd_();
				time = s2 - s1;
				++ic;
				if (time < *timmin) {
				    ztimmg_(&c__0, &c__1, &n, &x[1], &incx, &
					    c__0, &c__0);
				    goto L470;
				}

/*                          Subtract the time used in ZTIMMG. */

				icl = 1;
				s1 = dsecnd_();
L480:
				s2 = dsecnd_();
				untime = s2 - s1;
				++icl;
				if (icl <= ic) {
				    ztimmg_(&c__0, &c__1, &n, &x[1], &incx, &
					    c__0, &c__0);
				    goto L480;
				}

				time = (time - untime) / (doublereal) ic;
				ops = dopbl2_(cname, &n, &n, &c__0, &c__0);
				reslts_ref(1, in, i3) = dmflop_(&ops, &time, &
					c__0);
/* L490: */
			    }
/* L500: */
			}
/* L510: */
		    }
		    io___55.ciunit = *nout;
		    s_wsfe(&io___55);
		    do_fio(&c__1, cname, (ftnlen)6);
		    do_fio(&c__1, uplo, (ftnlen)1);
		    do_fio(&c__1, transa, (ftnlen)1);
		    e_wsfe();
		    i__1 = *ninc * *nlda;
		    dprtbl_(" ", "N", &c__1, &nval[1], nn, &nval[1], &i__1, &
			    reslts[reslts_offset], ldr1, ldr2, nout, (ftnlen)
			    1, (ftnlen)1);
/* L520: */
		}
/* L530: */
	    }

/*        Time ZTRSV */

	} else if (s_cmp(cname, "ZTRSV ", (ftnlen)6, (ftnlen)6) == 0) {
	    for (iuplo = 1; iuplo <= 2; ++iuplo) {
		*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];
		imat = 11;
		if (*(unsigned char *)uplo == 'L') {
		    imat = -11;
		}
		for (ita = 1; ita <= 3; ++ita) {
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
				ztimmg_(&imat, &n, &n, &a[1], &lda, &c__0, &
					c__0);
				ztimmg_(&c__0, &c__1, &n, &x[1], &incx, &c__0,
					 &c__0);
				ic = 0;
				s1 = dsecnd_();
L540:
				ztrsv_(uplo, transa, "Non-unit", &n, &a[1], &
					lda, &x[1], &incx);
				s2 = dsecnd_();
				time = s2 - s1;
				++ic;
				if (time < *timmin) {
				    ztimmg_(&c__0, &c__1, &n, &x[1], &incx, &
					    c__0, &c__0);
				    goto L540;
				}

/*                          Subtract the time used in ZTIMMG. */

				icl = 1;
				s1 = dsecnd_();
L550:
				s2 = dsecnd_();
				untime = s2 - s1;
				++icl;
				if (icl <= ic) {
				    ztimmg_(&c__0, &c__1, &n, &x[1], &incx, &
					    c__0, &c__0);
				    goto L550;
				}

				time = (time - untime) / (doublereal) ic;
				ops = dopbl2_(cname, &n, &n, &c__0, &c__0);
				reslts_ref(1, in, i3) = dmflop_(&ops, &time, &
					c__0);
/* L560: */
			    }
/* L570: */
			}
/* L580: */
		    }
		    io___56.ciunit = *nout;
		    s_wsfe(&io___56);
		    do_fio(&c__1, cname, (ftnlen)6);
		    do_fio(&c__1, uplo, (ftnlen)1);
		    do_fio(&c__1, transa, (ftnlen)1);
		    e_wsfe();
		    i__1 = *ninc * *nlda;
		    dprtbl_(" ", "N", &c__1, &nval[1], nn, &nval[1], &i__1, &
			    reslts[reslts_offset], ldr1, ldr2, nout, (ftnlen)
			    1, (ftnlen)1);
/* L590: */
		}
/* L600: */
	    }

/*        Time ZTBMV */

	} else if (s_cmp(cname, "ZTBMV ", (ftnlen)6, (ftnlen)6) == 0) {
	    for (iuplo = 1; iuplo <= 2; ++iuplo) {
		*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];
		imat = 13;
		if (*(unsigned char *)uplo == 'L') {
		    imat = -13;
		}
		for (ita = 1; ita <= 3; ++ita) {
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
				    ztimmg_(&imat, &n, &n, &a[1], &lda, &k, &
					    k);
				    ztimmg_(&c__0, &c__1, &n, &x[1], &incx, &
					    c__0, &c__0);
				    ic = 0;
				    s1 = dsecnd_();
L610:
				    ztbmv_(uplo, transa, "Non-unit", &n, &k, &
					    a[1], &lda, &x[1], &incx);
				    s2 = dsecnd_();
				    time = s2 - s1;
				    ++ic;
				    if (time < *timmin) {
					ztimmg_(&c__0, &c__1, &n, &x[1], &
						incx, &c__0, &c__0);
					goto L610;
				    }

/*                             Subtract the time used in ZTIMMG. */

				    icl = 1;
				    s1 = dsecnd_();
L620:
				    s2 = dsecnd_();
				    untime = s2 - s1;
				    ++icl;
				    if (icl <= ic) {
					ztimmg_(&c__0, &c__1, &n, &x[1], &
						incx, &c__0, &c__0);
					goto L620;
				    }

				    time = (time - untime) / (doublereal) ic;
				    ops = dopbl2_(cname, &n, &n, &k, &k);
				    reslts_ref(ik, in, i3) = dmflop_(&ops, &
					    time, &c__0);
/* L630: */
				}
/* L640: */
			    }
/* L650: */
			}
/* L660: */
		    }
		    io___57.ciunit = *nout;
		    s_wsfe(&io___57);
		    do_fio(&c__1, cname, (ftnlen)6);
		    do_fio(&c__1, uplo, (ftnlen)1);
		    do_fio(&c__1, transa, (ftnlen)1);
		    e_wsfe();
		    i__1 = *ninc * *nlda;
		    dprtbl_("K", "N", nk, &kval[1], nn, &nval[1], &i__1, &
			    reslts[reslts_offset], ldr1, ldr2, nout, (ftnlen)
			    1, (ftnlen)1);
/* L670: */
		}
/* L680: */
	    }

/*        Time ZTBSV */

	} else if (s_cmp(cname, "ZTBSV ", (ftnlen)6, (ftnlen)6) == 0) {
	    for (iuplo = 1; iuplo <= 2; ++iuplo) {
		*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];
		imat = 13;
		if (*(unsigned char *)uplo == 'L') {
		    imat = -13;
		}
		for (ita = 1; ita <= 3; ++ita) {
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
				    ztimmg_(&imat, &n, &n, &a[1], &lda, &k, &
					    k);
				    ztimmg_(&c__0, &c__1, &n, &x[1], &incx, &
					    c__0, &c__0);
				    ic = 0;
				    s1 = dsecnd_();
L690:
				    ztbsv_(uplo, transa, "Non-unit", &n, &k, &
					    a[1], &lda, &x[1], &incx);
				    s2 = dsecnd_();
				    time = s2 - s1;
				    ++ic;
				    if (time < *timmin) {
					ztimmg_(&c__0, &c__1, &n, &x[1], &
						incx, &c__0, &c__0);
					goto L690;
				    }

/*                             Subtract the time used in ZTIMMG. */

				    icl = 1;
				    s1 = dsecnd_();
L700:
				    s2 = dsecnd_();
				    untime = s2 - s1;
				    ++icl;
				    if (icl <= ic) {
					ztimmg_(&c__0, &c__1, &n, &x[1], &
						incx, &c__0, &c__0);
					goto L700;
				    }

				    time = (time - untime) / (doublereal) ic;
				    ops = dopbl2_(cname, &n, &n, &k, &k);
				    reslts_ref(ik, in, i3) = dmflop_(&ops, &
					    time, &c__0);
/* L710: */
				}
/* L720: */
			    }
/* L730: */
			}
/* L740: */
		    }
		    io___58.ciunit = *nout;
		    s_wsfe(&io___58);
		    do_fio(&c__1, cname, (ftnlen)6);
		    do_fio(&c__1, uplo, (ftnlen)1);
		    do_fio(&c__1, transa, (ftnlen)1);
		    e_wsfe();
		    i__1 = *ninc * *nlda;
		    dprtbl_("K", "N", nk, &kval[1], nn, &nval[1], &i__1, &
			    reslts[reslts_offset], ldr1, ldr2, nout, (ftnlen)
			    1, (ftnlen)1);
/* L750: */
		}
/* L760: */
	    }

/*        Time ZTPMV */

	} else if (s_cmp(cname, "ZTPMV ", (ftnlen)6, (ftnlen)6) == 0) {
	    for (iuplo = 1; iuplo <= 2; ++iuplo) {
		*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];
		imat = 12;
		if (*(unsigned char *)uplo == 'L') {
		    imat = -12;
		}
		for (ita = 1; ita <= 3; ++ita) {
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
			    ztimmg_(&imat, &n, &n, &a[1], &i__3, &c__0, &c__0)
				    ;
			    ztimmg_(&c__0, &c__1, &n, &x[1], &incx, &c__0, &
				    c__0);
			    ic = 0;
			    s1 = dsecnd_();
L770:
			    ztpmv_(uplo, transa, "Non-unit", &n, &a[1], &x[1],
				     &incx);
			    s2 = dsecnd_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				ztimmg_(&c__0, &c__1, &n, &x[1], &incx, &c__0,
					 &c__0);
				goto L770;
			    }

/*                       Subtract the time used in ZTIMMG. */

			    icl = 1;
			    s1 = dsecnd_();
L780:
			    s2 = dsecnd_();
			    untime = s2 - s1;
			    ++icl;
			    if (icl <= ic) {
				ztimmg_(&c__0, &c__1, &n, &x[1], &incx, &c__0,
					 &c__0);
				goto L780;
			    }

			    time = (time - untime) / (doublereal) ic;
			    ops = dopbl2_(cname, &n, &n, &c__0, &c__0);
			    reslts_ref(1, in, iinc) = dmflop_(&ops, &time, &
				    c__0);
/* L790: */
			}
/* L800: */
		    }
		    io___59.ciunit = *nout;
		    s_wsfe(&io___59);
		    do_fio(&c__1, cname, (ftnlen)6);
		    do_fio(&c__1, uplo, (ftnlen)1);
		    do_fio(&c__1, transa, (ftnlen)1);
		    e_wsfe();
		    dprtbl_(" ", "N", &c__1, &nval[1], nn, &nval[1], ninc, &
			    reslts[reslts_offset], ldr1, ldr2, nout, (ftnlen)
			    1, (ftnlen)1);
/* L810: */
		}
/* L820: */
	    }

/*        Time ZTPSV */

	} else if (s_cmp(cname, "ZTPSV ", (ftnlen)6, (ftnlen)6) == 0) {
	    for (iuplo = 1; iuplo <= 2; ++iuplo) {
		*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];
		imat = 12;
		if (*(unsigned char *)uplo == 'L') {
		    imat = -12;
		}
		for (ita = 1; ita <= 3; ++ita) {
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
			    ztimmg_(&imat, &n, &n, &a[1], &i__3, &c__0, &c__0)
				    ;
			    ztimmg_(&c__0, &c__1, &n, &x[1], &incx, &c__0, &
				    c__0);
			    ic = 0;
			    s1 = dsecnd_();
L830:
			    ztpsv_(uplo, transa, "Non-unit", &n, &a[1], &x[1],
				     &incx);
			    s2 = dsecnd_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				ztimmg_(&c__0, &c__1, &n, &x[1], &incx, &c__0,
					 &c__0);
				goto L830;
			    }

/*                       Subtract the time used in ZTIMMG. */

			    icl = 1;
			    s1 = dsecnd_();
L840:
			    s2 = dsecnd_();
			    untime = s2 - s1;
			    ++icl;
			    if (icl <= ic) {
				ztimmg_(&c__0, &c__1, &n, &x[1], &incx, &c__0,
					 &c__0);
				goto L840;
			    }

			    time = (time - untime) / (doublereal) ic;
			    ops = dopbl2_(cname, &n, &n, &c__0, &c__0);
			    reslts_ref(1, in, iinc) = dmflop_(&ops, &time, &
				    c__0);
/* L850: */
			}
/* L860: */
		    }
		    io___60.ciunit = *nout;
		    s_wsfe(&io___60);
		    do_fio(&c__1, cname, (ftnlen)6);
		    do_fio(&c__1, uplo, (ftnlen)1);
		    do_fio(&c__1, transa, (ftnlen)1);
		    e_wsfe();
		    dprtbl_(" ", "N", &c__1, &nval[1], nn, &nval[1], ninc, &
			    reslts[reslts_offset], ldr1, ldr2, nout, (ftnlen)
			    1, (ftnlen)1);
/* L870: */
		}
/* L880: */
	    }

/*        Time ZGERU */

	} else if (s_cmp(cname, "ZGERU ", (ftnlen)6, (ftnlen)6) == 0) {
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
			    ztimmg_(&c__0, &c__1, &m, &x[1], &incx, &c__0, &
				    c__0);
			    ztimmg_(&c__0, &c__1, &n, &y[1], &incx, &c__0, &
				    c__0);
			    ztimmg_(&c__1, &m, &n, &a[1], &lda, &c__0, &c__0);
			    ic = 0;
			    s1 = dsecnd_();
L890:
			    zgeru_(&m, &n, &c_b1, &x[1], &incx, &y[1], &incx, 
				    &a[1], &lda);
			    s2 = dsecnd_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				ztimmg_(&c__1, &m, &n, &a[1], &lda, &c__0, &
					c__0);
				goto L890;
			    }

/*                       Subtract the time used in ZTIMMG. */

			    icl = 1;
			    s1 = dsecnd_();
L900:
			    s2 = dsecnd_();
			    untime = s2 - s1;
			    ++icl;
			    if (icl <= ic) {
				ztimmg_(&c__1, &m, &n, &a[1], &lda, &c__0, &
					c__0);
				goto L900;
			    }

			    time = (time - untime) / (doublereal) ic;
			    ops = dopbl2_(cname, &m, &n, &c__0, &c__0);
			    reslts_ref(im, in, i3) = dmflop_(&ops, &time, &
				    c__0);
/* L910: */
			}
/* L920: */
		    }
/* L930: */
		}
/* L940: */
	    }
	    io___61.ciunit = *nout;
	    s_wsfe(&io___61);
	    do_fio(&c__1, cname, (ftnlen)6);
	    e_wsfe();
	    i__1 = *ninc * *nlda;
	    dprtbl_("M", "N", nm, &mval[1], nn, &nval[1], &i__1, &reslts[
		    reslts_offset], ldr1, ldr2, nout, (ftnlen)1, (ftnlen)1);

/*        Time ZGERC */

	} else if (s_cmp(cname, "ZGERC ", (ftnlen)6, (ftnlen)6) == 0) {
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
			    ztimmg_(&c__0, &c__1, &m, &x[1], &incx, &c__0, &
				    c__0);
			    ztimmg_(&c__0, &c__1, &n, &y[1], &incx, &c__0, &
				    c__0);
			    ztimmg_(&c__1, &m, &n, &a[1], &lda, &c__0, &c__0);
			    ic = 0;
			    s1 = dsecnd_();
L950:
			    zgerc_(&m, &n, &c_b1, &x[1], &incx, &y[1], &incx, 
				    &a[1], &lda);
			    s2 = dsecnd_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				ztimmg_(&c__1, &m, &n, &a[1], &lda, &c__0, &
					c__0);
				goto L950;
			    }

/*                       Subtract the time used in ZTIMMG. */

			    icl = 1;
			    s1 = dsecnd_();
L960:
			    s2 = dsecnd_();
			    untime = s2 - s1;
			    ++icl;
			    if (icl <= ic) {
				ztimmg_(&c__1, &m, &n, &a[1], &lda, &c__0, &
					c__0);
				goto L960;
			    }

			    time = (time - untime) / (doublereal) ic;
			    ops = dopbl2_(cname, &m, &n, &c__0, &c__0);
			    reslts_ref(im, in, i3) = dmflop_(&ops, &time, &
				    c__0);
/* L970: */
			}
/* L980: */
		    }
/* L990: */
		}
/* L1000: */
	    }
	    io___62.ciunit = *nout;
	    s_wsfe(&io___62);
	    do_fio(&c__1, cname, (ftnlen)6);
	    e_wsfe();
	    i__1 = *ninc * *nlda;
	    dprtbl_("M", "N", nm, &mval[1], nn, &nval[1], &i__1, &reslts[
		    reslts_offset], ldr1, ldr2, nout, (ftnlen)1, (ftnlen)1);

/*        Time ZHER */

	} else if (s_cmp(cname, "ZHER  ", (ftnlen)6, (ftnlen)6) == 0) {
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
			    ztimmg_(&c__0, &c__1, &n, &x[1], &incx, &c__0, &
				    c__0);
			    ztimmg_(&c__0, &c__1, &n, &y[1], &incx, &c__0, &
				    c__0);
			    ztimmg_(&imat, &n, &n, &a[1], &lda, &c__0, &c__0);
			    ic = 0;
			    s1 = dsecnd_();
L1010:
			    zher_(uplo, &n, &c_b561, &x[1], &incx, &a[1], &
				    lda);
			    s2 = dsecnd_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				ztimmg_(&imat, &n, &n, &a[1], &lda, &c__0, &
					c__0);
				goto L1010;
			    }

/*                       Subtract the time used in ZTIMMG. */

			    icl = 1;
			    s1 = dsecnd_();
L1020:
			    s2 = dsecnd_();
			    untime = s2 - s1;
			    ++icl;
			    if (icl <= ic) {
				ztimmg_(&imat, &n, &n, &a[1], &lda, &c__0, &
					c__0);
				goto L1020;
			    }

			    time = (time - untime) / (doublereal) ic;
			    ops = dopbl2_(cname, &n, &n, &c__0, &c__0);
			    reslts_ref(1, in, i3) = dmflop_(&ops, &time, &
				    c__0);
/* L1030: */
			}
/* L1040: */
		    }
/* L1050: */
		}
		io___63.ciunit = *nout;
		s_wsfe(&io___63);
		do_fio(&c__1, cname, (ftnlen)6);
		do_fio(&c__1, uplo, (ftnlen)1);
		e_wsfe();
		i__1 = *ninc * *nlda;
		dprtbl_(" ", "N", &c__1, &nval[1], nn, &nval[1], &i__1, &
			reslts[reslts_offset], ldr1, ldr2, nout, (ftnlen)1, (
			ftnlen)1);
/* L1060: */
	    }

/*        Time ZSYR */

	} else if (s_cmp(cname, "ZSYR  ", (ftnlen)6, (ftnlen)6) == 0) {
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
			i__3 = *nn;
			for (in = 1; in <= i__3; ++in) {
			    n = nval[in];
			    ztimmg_(&c__0, &c__1, &n, &x[1], &incx, &c__0, &
				    c__0);
			    ztimmg_(&c__0, &c__1, &n, &y[1], &incx, &c__0, &
				    c__0);
			    ztimmg_(&imat, &n, &n, &a[1], &lda, &c__0, &c__0);
			    ic = 0;
			    s1 = dsecnd_();
L1070:
			    zsyr_(uplo, &n, &c_b1, &x[1], &incx, &a[1], &lda);
			    s2 = dsecnd_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				ztimmg_(&imat, &n, &n, &a[1], &lda, &c__0, &
					c__0);
				goto L1070;
			    }

/*                       Subtract the time used in ZTIMMG. */

			    icl = 1;
			    s1 = dsecnd_();
L1080:
			    s2 = dsecnd_();
			    untime = s2 - s1;
			    ++icl;
			    if (icl <= ic) {
				ztimmg_(&imat, &n, &n, &a[1], &lda, &c__0, &
					c__0);
				goto L1080;
			    }

			    time = (time - untime) / (doublereal) ic;
			    ops = dopbl2_(cname, &n, &n, &c__0, &c__0);
			    reslts_ref(1, in, i3) = dmflop_(&ops, &time, &
				    c__0);
/* L1090: */
			}
/* L1100: */
		    }
/* L1110: */
		}
		io___64.ciunit = *nout;
		s_wsfe(&io___64);
		do_fio(&c__1, cname, (ftnlen)6);
		do_fio(&c__1, uplo, (ftnlen)1);
		e_wsfe();
		i__1 = *ninc * *nlda;
		dprtbl_(" ", "N", &c__1, &nval[1], nn, &nval[1], &i__1, &
			reslts[reslts_offset], ldr1, ldr2, nout, (ftnlen)1, (
			ftnlen)1);
/* L1120: */
	    }

/*        Time ZHER2 */

	} else if (s_cmp(cname, "ZHER2 ", (ftnlen)6, (ftnlen)6) == 0) {
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
			    ztimmg_(&c__0, &c__1, &n, &x[1], &incx, &c__0, &
				    c__0);
			    ztimmg_(&c__0, &c__1, &n, &y[1], &incx, &c__0, &
				    c__0);
			    ztimmg_(&imat, &n, &n, &a[1], &lda, &c__0, &c__0);
			    ic = 0;
			    s1 = dsecnd_();
L1130:
			    zher2_(uplo, &n, &c_b1, &x[1], &incx, &y[1], &
				    incx, &a[1], &lda);
			    s2 = dsecnd_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				ztimmg_(&imat, &n, &n, &a[1], &lda, &c__0, &
					c__0);
				goto L1130;
			    }

/*                       Subtract the time used in ZTIMMG. */

			    icl = 1;
			    s1 = dsecnd_();
L1140:
			    s2 = dsecnd_();
			    untime = s2 - s1;
			    ++icl;
			    if (icl <= ic) {
				ztimmg_(&imat, &n, &n, &a[1], &lda, &c__0, &
					c__0);
				goto L1140;
			    }

			    time = (time - untime) / (doublereal) ic;
			    ops = dopbl2_(cname, &n, &n, &c__0, &c__0);
			    reslts_ref(1, in, i3) = dmflop_(&ops, &time, &
				    c__0);
/* L1150: */
			}
/* L1160: */
		    }
/* L1170: */
		}
		io___65.ciunit = *nout;
		s_wsfe(&io___65);
		do_fio(&c__1, cname, (ftnlen)6);
		do_fio(&c__1, uplo, (ftnlen)1);
		e_wsfe();
		i__1 = *ninc * *nlda;
		dprtbl_(" ", "N", &c__1, &nval[1], nn, &nval[1], &i__1, &
			reslts[reslts_offset], ldr1, ldr2, nout, (ftnlen)1, (
			ftnlen)1);
/* L1180: */
	    }

/*        Time ZHPR */

	} else if (s_cmp(cname, "ZHPR  ", (ftnlen)6, (ftnlen)6) == 0) {
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
			ztimmg_(&c__0, &c__1, &n, &x[1], &incx, &c__0, &c__0);
			ztimmg_(&c__0, &c__1, &n, &y[1], &incx, &c__0, &c__0);
			i__3 = n * (n + 1) / 2;
			ztimmg_(&imat, &n, &n, &a[1], &i__3, &c__0, &c__0);
			ic = 0;
			s1 = dsecnd_();
L1190:
			zhpr_(uplo, &n, &c_b561, &x[1], &incx, &a[1]);
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    i__3 = n * (n + 1) / 2;
			    ztimmg_(&imat, &n, &n, &a[1], &i__3, &c__0, &c__0)
				    ;
			    goto L1190;
			}

/*                    Subtract the time used in ZTIMMG. */

			icl = 1;
			s1 = dsecnd_();
L1200:
			s2 = dsecnd_();
			untime = s2 - s1;
			++icl;
			if (icl <= ic) {
			    i__3 = n * (n + 1) / 2;
			    ztimmg_(&imat, &n, &n, &a[1], &i__3, &c__0, &c__0)
				    ;
			    goto L1200;
			}

			time = (time - untime) / (doublereal) ic;
			ops = dopbl2_(cname, &n, &n, &c__0, &c__0);
			reslts_ref(1, in, iinc) = dmflop_(&ops, &time, &c__0);
/* L1210: */
		    }
/* L1220: */
		}
		io___66.ciunit = *nout;
		s_wsfe(&io___66);
		do_fio(&c__1, cname, (ftnlen)6);
		do_fio(&c__1, uplo, (ftnlen)1);
		e_wsfe();
		dprtbl_(" ", "N", &c__1, &nval[1], nn, &nval[1], ninc, &
			reslts[reslts_offset], ldr1, ldr2, nout, (ftnlen)1, (
			ftnlen)1);
/* L1230: */
	    }

/*        Time ZSPR */

	} else if (s_cmp(cname, "ZSPR  ", (ftnlen)6, (ftnlen)6) == 0) {
	    for (iuplo = 1; iuplo <= 2; ++iuplo) {
		*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];
		imat = 9;
		if (*(unsigned char *)uplo == 'L') {
		    imat = -9;
		}
		ilda = 1;
		lda = ldaval[ilda];
		i__1 = *ninc;
		for (iinc = 1; iinc <= i__1; ++iinc) {
		    incx = incval[iinc];
		    i__2 = *nn;
		    for (in = 1; in <= i__2; ++in) {
			n = nval[in];
			ztimmg_(&c__0, &c__1, &n, &x[1], &incx, &c__0, &c__0);
			ztimmg_(&c__0, &c__1, &n, &y[1], &incx, &c__0, &c__0);
			i__3 = n * (n + 1) / 2;
			ztimmg_(&imat, &n, &n, &a[1], &i__3, &c__0, &c__0);
			ic = 0;
			s1 = dsecnd_();
L1240:
			zspr_(uplo, &n, &c_b1, &x[1], &incx, &a[1])
				;
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    i__3 = n * (n + 1) / 2;
			    ztimmg_(&imat, &n, &n, &a[1], &i__3, &c__0, &c__0)
				    ;
			    goto L1240;
			}

/*                    Subtract the time used in ZTIMMG. */

			icl = 1;
			s1 = dsecnd_();
L1250:
			s2 = dsecnd_();
			untime = s2 - s1;
			++icl;
			if (icl <= ic) {
			    i__3 = n * (n + 1) / 2;
			    ztimmg_(&imat, &n, &n, &a[1], &i__3, &c__0, &c__0)
				    ;
			    goto L1250;
			}

			time = (time - untime) / (doublereal) ic;
			ops = dopbl2_(cname, &n, &n, &c__0, &c__0);
			reslts_ref(1, in, iinc) = dmflop_(&ops, &time, &c__0);
/* L1260: */
		    }
/* L1270: */
		}
		io___67.ciunit = *nout;
		s_wsfe(&io___67);
		do_fio(&c__1, cname, (ftnlen)6);
		do_fio(&c__1, uplo, (ftnlen)1);
		e_wsfe();
		dprtbl_(" ", "N", &c__1, &nval[1], nn, &nval[1], ninc, &
			reslts[reslts_offset], ldr1, ldr2, nout, (ftnlen)1, (
			ftnlen)1);
/* L1280: */
	    }

/*        Time ZHPR2 */

	} else if (s_cmp(cname, "ZHPR2 ", (ftnlen)6, (ftnlen)6) == 0) {
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
			ztimmg_(&c__0, &c__1, &n, &x[1], &incx, &c__0, &c__0);
			ztimmg_(&c__0, &c__1, &n, &y[1], &incx, &c__0, &c__0);
			i__3 = n * (n + 1) / 2;
			ztimmg_(&imat, &n, &n, &a[1], &i__3, &c__0, &c__0);
			ic = 0;
			s1 = dsecnd_();
L1290:
			zhpr2_(uplo, &n, &c_b1, &x[1], &incx, &y[1], &incx, &
				a[1]);
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    i__3 = n * (n + 1) / 2;
			    ztimmg_(&imat, &n, &n, &a[1], &i__3, &c__0, &c__0)
				    ;
			    goto L1290;
			}

/*                    Subtract the time used in ZTIMMG. */

			icl = 1;
			s1 = dsecnd_();
L1300:
			s2 = dsecnd_();
			untime = s2 - s1;
			++icl;
			if (icl <= ic) {
			    i__3 = n * (n + 1) / 2;
			    ztimmg_(&imat, &n, &n, &a[1], &i__3, &c__0, &c__0)
				    ;
			    goto L1300;
			}

			time = (time - untime) / (doublereal) ic;
			ops = dopbl2_(cname, &n, &n, &c__0, &c__0);
			reslts_ref(1, in, iinc) = dmflop_(&ops, &time, &c__0);
/* L1310: */
		    }
/* L1320: */
		}
		io___68.ciunit = *nout;
		s_wsfe(&io___68);
		do_fio(&c__1, cname, (ftnlen)6);
		do_fio(&c__1, uplo, (ftnlen)1);
		e_wsfe();
		dprtbl_(" ", "N", &c__1, &nval[1], nn, &nval[1], ninc, &
			reslts[reslts_offset], ldr1, ldr2, nout, (ftnlen)1, (
			ftnlen)1);
/* L1330: */
	    }
	}
	io___69.ciunit = *nout;
	s_wsfe(&io___69);
	e_wsfe();
L1340:
	;
    }
L1350:

    return 0;

/*     End of ZTIMB2 */

} /* ztimb2_ */

#undef reslts_ref
#undef names_ref


