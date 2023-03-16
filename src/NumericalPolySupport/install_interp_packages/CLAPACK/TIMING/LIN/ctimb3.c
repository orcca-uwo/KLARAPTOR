#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static complex c_b1 = {1.f,0.f};
static integer c__9 = 9;
static integer c__1 = 1;
static integer c__0 = 0;
static real c_b156 = 1.f;

/* Subroutine */ int ctimb3_(char *line, integer *nm, integer *mval, integer *
	nn, integer *nval, integer *nk, integer *kval, integer *nlda, integer 
	*ldaval, real *timmin, complex *a, complex *b, complex *c__, real *
	reslts, integer *ldr1, integer *ldr2, integer *nout, ftnlen line_len)
{
    /* Initialized data */

    static char names[6*9] = "CGEMM " "CHEMM " "CSYMM " "CHERK " "CHER2K" 
	    "CSYRK " "CSYR2K" "CTRMM " "CTRSM ";
    static char trans[1*3] = "N" "T" "C";
    static char sides[1*2] = "L" "R";
    static char uplos[1*2] = "U" "L";

    /* Format strings */
    static char fmt_9999[] = "(1x,a6,\002 timing run not attempted\002,/)";
    static char fmt_9998[] = "(/\002 *** Speed of \002,a6,\002 in megaflops "
	    "***\002)";
    static char fmt_9997[] = "(5x,\002with LDA = \002,i5)";
    static char fmt_9996[] = "(5x,\002line \002,i2,\002 with LDA = \002,i5)";
    static char fmt_9995[] = "(/1x,\002CGEMM  with TRANSA = '\002,a1,\002', "
	    "TRANSB = '\002,a1,\002'\002)";
    static char fmt_9994[] = "(/1x,\002K = \002,i4,/)";
    static char fmt_9993[] = "(/1x,a6,\002 with SIDE = '\002,a1,\002', UPLO "
	    "= '\002,a1,\002'\002,/)";
    static char fmt_9992[] = "(/1x,a6,\002 with UPLO = '\002,a1,\002', TRANS"
	    " = '\002,a1,\002'\002,/)";
    static char fmt_9991[] = "(/1x,a6,\002 with SIDE = '\002,a1,\002', UPLO "
	    "= '\002,a1,\002',\002,\002 TRANS = '\002,a1,\002'\002,/)";
    static char fmt_9990[] = "(/////)";

    /* System generated locals */
    integer reslts_dim1, reslts_dim2, reslts_offset, i__1, i__2, i__3, i__4;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void),
	     s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static integer ilda;
    static char side[1];
    static integer imat, info;
    static char path[3];
    static real time;
    static integer isub;
    static char uplo[1];
    static integer i__, k, m, n;
    static char cname[6];
    extern /* Subroutine */ int cgemm_(char *, char *, integer *, integer *, 
	    integer *, complex *, complex *, integer *, complex *, integer *, 
	    complex *, complex *, integer *), chemm_(char *, 
	    char *, integer *, integer *, complex *, complex *, integer *, 
	    complex *, integer *, complex *, complex *, integer *), cherk_(char *, char *, integer *, integer *, real *, 
	    complex *, integer *, real *, complex *, integer *);
    static integer iside;
    extern logical lsame_(char *, char *);
    extern /* Subroutine */ int ctrmm_(char *, char *, char *, char *, 
	    integer *, integer *, complex *, complex *, integer *, complex *, 
	    integer *);
    static integer iuplo;
    extern /* Subroutine */ int csymm_(char *, char *, integer *, integer *, 
	    complex *, complex *, integer *, complex *, integer *, complex *, 
	    complex *, integer *), csyrk_(char *, char *, 
	    integer *, integer *, complex *, complex *, integer *, complex *, 
	    complex *, integer *), ctrsm_(char *, char *, 
	    char *, char *, integer *, integer *, complex *, complex *, 
	    integer *, complex *, integer *);
    static real s1, s2;
    extern /* Subroutine */ int cher2k_(char *, char *, integer *, integer *, 
	    complex *, complex *, integer *, complex *, integer *, real *, 
	    complex *, integer *);
    extern doublereal sopbl3_(char *, integer *, integer *, integer *)
	    ;
    static integer ic;
    extern /* Subroutine */ int csyr2k_(char *, char *, integer *, integer *, 
	    complex *, complex *, integer *, complex *, integer *, complex *, 
	    complex *, integer *);
    static integer ik, im, in;
    extern /* Subroutine */ int atimck_(integer *, char *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, ftnlen);
    extern doublereal second_(void);
    extern /* Subroutine */ int ctimmg_(integer *, integer *, integer *, 
	    complex *, integer *, integer *, integer *), atimin_(char *, char 
	    *, integer *, char *, logical *, integer *, integer *, ftnlen, 
	    ftnlen, ftnlen);
    static char transa[1], transb[1];
    extern doublereal smflop_(real *, real *, integer *);
    static real untime;
    static logical timsub[9];
    extern /* Subroutine */ int sprtbl_(char *, char *, integer *, integer *, 
	    integer *, integer *, integer *, real *, integer *, integer *, 
	    integer *, ftnlen, ftnlen);
    static integer lda, icl, ita, itb;
    static real ops;

    /* Fortran I/O blocks */
    static cilist io___9 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___11 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___12 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___14 = { 0, 0, 0, fmt_9996, 0 };
    static cilist io___34 = { 0, 0, 0, fmt_9995, 0 };
    static cilist io___35 = { 0, 0, 0, fmt_9994, 0 };
    static cilist io___41 = { 0, 0, 0, fmt_9993, 0 };
    static cilist io___42 = { 0, 0, 0, fmt_9993, 0 };
    static cilist io___43 = { 0, 0, 0, fmt_9992, 0 };
    static cilist io___44 = { 0, 0, 0, fmt_9992, 0 };
    static cilist io___45 = { 0, 0, 0, fmt_9992, 0 };
    static cilist io___46 = { 0, 0, 0, fmt_9992, 0 };
    static cilist io___47 = { 0, 0, 0, fmt_9991, 0 };
    static cilist io___48 = { 0, 0, 0, fmt_9991, 0 };
    static cilist io___49 = { 0, 0, 0, fmt_9990, 0 };



#define names_ref(a_0,a_1) &names[(a_1)*6 + a_0 - 6]
#define reslts_ref(a_1,a_2,a_3) reslts[((a_3)*reslts_dim2 + (a_2))*\
reslts_dim1 + a_1]


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       March 31, 1993   


    Purpose   
    =======   

    CTIMB3 times the Level 3 BLAS routines.   

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
            The values of K.  K is used as the intermediate matrix   
            dimension for CGEMM (the product of an M x K matrix and a   
            K x N matrix) and as the dimension of the rank-K update in   
            CHERK and CSYRK.   

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

    C       (workspace) COMPLEX array, dimension (LDAMAX*NMAX)   

    RESLTS  (output) REAL array, dimension (LDR1,LDR2,NLDA)   
            The timing results for each subroutine over the relevant   
            values of M, N, K, and LDA.   

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
    --ldaval;
    --a;
    --b;
    --c__;
    reslts_dim1 = *ldr1;
    reslts_dim2 = *ldr2;
    reslts_offset = 1 + reslts_dim1 * (1 + reslts_dim2 * 1);
    reslts -= reslts_offset;

    /* Function Body   


       Extract the timing request from the input line. */

    s_copy(path, "Complex precision", (ftnlen)1, (ftnlen)17);
    s_copy(path + 1, "B3", (ftnlen)2, (ftnlen)2);
    atimin_(path, line, &c__9, names, timsub, nout, &info, (ftnlen)3, 
	    line_len, (ftnlen)6);
    if (info != 0) {
	goto L690;
    }

/*     Check that M <= LDA. */

    s_copy(cname, line, (ftnlen)6, (ftnlen)6);
    atimck_(&c__1, cname, nm, &mval[1], nlda, &ldaval[1], nout, &info, (
	    ftnlen)6);
    if (info > 0) {
	io___9.ciunit = *nout;
	s_wsfe(&io___9);
	do_fio(&c__1, cname, (ftnlen)6);
	e_wsfe();
	goto L690;
    }

/*     Time each routine. */

    for (isub = 1; isub <= 9; ++isub) {
	if (! timsub[isub - 1]) {
	    goto L680;
	}

/*        Print header. */

	s_copy(cname, names_ref(0, isub), (ftnlen)6, (ftnlen)6);
	io___11.ciunit = *nout;
	s_wsfe(&io___11);
	do_fio(&c__1, cname, (ftnlen)6);
	e_wsfe();
	if (*nlda == 1) {
	    io___12.ciunit = *nout;
	    s_wsfe(&io___12);
	    do_fio(&c__1, (char *)&ldaval[1], (ftnlen)sizeof(integer));
	    e_wsfe();
	} else {
	    i__1 = *nlda;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		io___14.ciunit = *nout;
		s_wsfe(&io___14);
		do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&ldaval[i__], (ftnlen)sizeof(integer));
		e_wsfe();
/* L10: */
	    }
	}

/*        Time CGEMM */

	if (s_cmp(cname, "CGEMM ", (ftnlen)6, (ftnlen)6) == 0) {
	    for (ita = 1; ita <= 3; ++ita) {
		*(unsigned char *)transa = *(unsigned char *)&trans[ita - 1];
		for (itb = 1; itb <= 3; ++itb) {
		    *(unsigned char *)transb = *(unsigned char *)&trans[itb - 
			    1];
		    i__1 = *nk;
		    for (ik = 1; ik <= i__1; ++ik) {
			k = kval[ik];
			i__2 = *nlda;
			for (ilda = 1; ilda <= i__2; ++ilda) {
			    lda = ldaval[ilda];
			    i__3 = *nm;
			    for (im = 1; im <= i__3; ++im) {
				m = mval[im];
				i__4 = *nn;
				for (in = 1; in <= i__4; ++in) {
				    n = nval[in];
				    if (*(unsigned char *)transa == 'N') {
					ctimmg_(&c__1, &m, &k, &a[1], &lda, &
						c__0, &c__0);
				    } else {
					ctimmg_(&c__1, &k, &m, &a[1], &lda, &
						c__0, &c__0);
				    }
				    if (*(unsigned char *)transb == 'N') {
					ctimmg_(&c__0, &k, &n, &b[1], &lda, &
						c__0, &c__0);
				    } else {
					ctimmg_(&c__0, &n, &k, &b[1], &lda, &
						c__0, &c__0);
				    }
				    ctimmg_(&c__1, &m, &n, &c__[1], &lda, &
					    c__0, &c__0);
				    ic = 0;
				    s1 = second_();
L20:
				    cgemm_(transa, transb, &m, &n, &k, &c_b1, 
					    &a[1], &lda, &b[1], &lda, &c_b1, &
					    c__[1], &lda);
				    s2 = second_();
				    time = s2 - s1;
				    ++ic;
				    if (time < *timmin) {
					ctimmg_(&c__1, &m, &n, &c__[1], &lda, 
						&c__0, &c__0);
					goto L20;
				    }

/*                             Subtract the time used in CTIMMG. */

				    icl = 1;
				    s1 = second_();
L30:
				    s2 = second_();
				    untime = s2 - s1;
				    ++icl;
				    if (icl <= ic) {
					ctimmg_(&c__1, &m, &n, &c__[1], &lda, 
						&c__0, &c__0);
					goto L30;
				    }

				    time = (time - untime) / (real) ic;
				    ops = sopbl3_(cname, &m, &n, &k);
				    reslts_ref(im, in, ilda) = smflop_(&ops, &
					    time, &c__0);
/* L40: */
				}
/* L50: */
			    }
/* L60: */
			}
			if (ik == 1) {
			    io___34.ciunit = *nout;
			    s_wsfe(&io___34);
			    do_fio(&c__1, transa, (ftnlen)1);
			    do_fio(&c__1, transb, (ftnlen)1);
			    e_wsfe();
			}
			io___35.ciunit = *nout;
			s_wsfe(&io___35);
			do_fio(&c__1, (char *)&kval[ik], (ftnlen)sizeof(
				integer));
			e_wsfe();
			sprtbl_("M", "N", nm, &mval[1], nn, &nval[1], nlda, &
				reslts[reslts_offset], ldr1, ldr2, nout, (
				ftnlen)1, (ftnlen)1);
/* L70: */
		    }
/* L80: */
		}
/* L90: */
	    }

/*        Time CHEMM */

	} else if (s_cmp(cname, "CHEMM ", (ftnlen)6, (ftnlen)6) == 0) {
	    for (iside = 1; iside <= 2; ++iside) {
		*(unsigned char *)side = *(unsigned char *)&sides[iside - 1];
		for (iuplo = 1; iuplo <= 2; ++iuplo) {
		    *(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 
			    1];
		    if (lsame_(uplo, "U")) {
			imat = 6;
		    } else {
			imat = -6;
		    }
		    i__1 = *nlda;
		    for (ilda = 1; ilda <= i__1; ++ilda) {
			lda = ldaval[ilda];
			i__2 = *nm;
			for (im = 1; im <= i__2; ++im) {
			    m = mval[im];
			    i__3 = *nn;
			    for (in = 1; in <= i__3; ++in) {
				n = nval[in];
				if (iside == 1) {
				    ctimmg_(&imat, &m, &m, &a[1], &lda, &c__0,
					     &c__0);
				    ctimmg_(&c__0, &m, &n, &b[1], &lda, &c__0,
					     &c__0);
				} else {
				    ctimmg_(&c__0, &m, &n, &b[1], &lda, &c__0,
					     &c__0);
				    ctimmg_(&imat, &n, &n, &a[1], &lda, &c__0,
					     &c__0);
				}
				ctimmg_(&c__1, &m, &n, &c__[1], &lda, &c__0, &
					c__0);
				ic = 0;
				s1 = second_();
L100:
				chemm_(side, uplo, &m, &n, &c_b1, &a[1], &lda,
					 &b[1], &lda, &c_b1, &c__[1], &lda);
				s2 = second_();
				time = s2 - s1;
				++ic;
				if (time < *timmin) {
				    ctimmg_(&c__1, &m, &n, &c__[1], &lda, &
					    c__0, &c__0);
				    goto L100;
				}

/*                          Subtract the time used in CTIMMG. */

				icl = 1;
				s1 = second_();
L110:
				s2 = second_();
				untime = s2 - s1;
				++icl;
				if (icl <= ic) {
				    ctimmg_(&c__1, &m, &n, &c__[1], &lda, &
					    c__0, &c__0);
				    goto L110;
				}

				time = (time - untime) / (real) ic;
				i__4 = iside - 1;
				ops = sopbl3_(cname, &m, &n, &i__4)
					;
				reslts_ref(im, in, ilda) = smflop_(&ops, &
					time, &c__0);
/* L120: */
			    }
/* L130: */
			}
/* L140: */
		    }
		    io___41.ciunit = *nout;
		    s_wsfe(&io___41);
		    do_fio(&c__1, "CHEMM ", (ftnlen)6);
		    do_fio(&c__1, side, (ftnlen)1);
		    do_fio(&c__1, uplo, (ftnlen)1);
		    e_wsfe();
		    sprtbl_("M", "N", nm, &mval[1], nn, &nval[1], nlda, &
			    reslts[reslts_offset], ldr1, ldr2, nout, (ftnlen)
			    1, (ftnlen)1);
/* L150: */
		}
/* L160: */
	    }

/*        Time CSYMM */

	} else if (s_cmp(cname, "CSYMM ", (ftnlen)6, (ftnlen)6) == 0) {
	    for (iside = 1; iside <= 2; ++iside) {
		*(unsigned char *)side = *(unsigned char *)&sides[iside - 1];
		for (iuplo = 1; iuplo <= 2; ++iuplo) {
		    *(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 
			    1];
		    if (lsame_(uplo, "U")) {
			imat = 8;
		    } else {
			imat = -8;
		    }
		    i__1 = *nlda;
		    for (ilda = 1; ilda <= i__1; ++ilda) {
			lda = ldaval[ilda];
			i__2 = *nm;
			for (im = 1; im <= i__2; ++im) {
			    m = mval[im];
			    i__3 = *nn;
			    for (in = 1; in <= i__3; ++in) {
				n = nval[in];
				if (iside == 1) {
				    ctimmg_(&imat, &m, &m, &a[1], &lda, &c__0,
					     &c__0);
				    ctimmg_(&c__0, &m, &n, &b[1], &lda, &c__0,
					     &c__0);
				} else {
				    ctimmg_(&c__0, &m, &n, &b[1], &lda, &c__0,
					     &c__0);
				    ctimmg_(&imat, &n, &n, &a[1], &lda, &c__0,
					     &c__0);
				}
				ctimmg_(&c__1, &m, &n, &c__[1], &lda, &c__0, &
					c__0);
				ic = 0;
				s1 = second_();
L170:
				csymm_(side, uplo, &m, &n, &c_b1, &a[1], &lda,
					 &b[1], &lda, &c_b1, &c__[1], &lda);
				s2 = second_();
				time = s2 - s1;
				++ic;
				if (time < *timmin) {
				    ctimmg_(&c__1, &m, &n, &c__[1], &lda, &
					    c__0, &c__0);
				    goto L170;
				}

/*                          Subtract the time used in CTIMMG. */

				icl = 1;
				s1 = second_();
L180:
				s2 = second_();
				untime = s2 - s1;
				++icl;
				if (icl <= ic) {
				    ctimmg_(&c__1, &m, &n, &c__[1], &lda, &
					    c__0, &c__0);
				    goto L180;
				}

				time = (time - untime) / (real) ic;
				i__4 = iside - 1;
				ops = sopbl3_(cname, &m, &n, &i__4)
					;
				reslts_ref(im, in, ilda) = smflop_(&ops, &
					time, &c__0);
/* L190: */
			    }
/* L200: */
			}
/* L210: */
		    }
		    io___42.ciunit = *nout;
		    s_wsfe(&io___42);
		    do_fio(&c__1, "CSYMM ", (ftnlen)6);
		    do_fio(&c__1, side, (ftnlen)1);
		    do_fio(&c__1, uplo, (ftnlen)1);
		    e_wsfe();
		    sprtbl_("M", "N", nm, &mval[1], nn, &nval[1], nlda, &
			    reslts[reslts_offset], ldr1, ldr2, nout, (ftnlen)
			    1, (ftnlen)1);
/* L220: */
		}
/* L230: */
	    }

/*        Time CHERK */

	} else if (s_cmp(cname, "CHERK ", (ftnlen)6, (ftnlen)6) == 0) {
	    for (iuplo = 1; iuplo <= 2; ++iuplo) {
		*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];
		if (lsame_(uplo, "U")) {
		    imat = 6;
		} else {
		    imat = -6;
		}
		for (ita = 1; ita <= 3; ++ita) {
		    *(unsigned char *)transa = *(unsigned char *)&trans[ita - 
			    1];
		    if (*(unsigned char *)transa != 'T') {
			i__1 = *nlda;
			for (ilda = 1; ilda <= i__1; ++ilda) {
			    lda = ldaval[ilda];
			    i__2 = *nk;
			    for (ik = 1; ik <= i__2; ++ik) {
				k = kval[ik];
				if (*(unsigned char *)transa == 'N') {
				    ctimmg_(&c__1, &n, &k, &a[1], &lda, &c__0,
					     &c__0);
				} else {
				    ctimmg_(&c__1, &k, &n, &a[1], &lda, &c__0,
					     &c__0);
				}
				i__3 = *nn;
				for (in = 1; in <= i__3; ++in) {
				    n = nval[in];
				    ctimmg_(&imat, &n, &n, &c__[1], &lda, &
					    c__0, &c__0);
				    ic = 0;
				    s1 = second_();
L240:
				    cherk_(uplo, transa, &n, &k, &c_b156, &a[
					    1], &lda, &c_b156, &c__[1], &lda);
				    s2 = second_();
				    time = s2 - s1;
				    ++ic;
				    if (time < *timmin) {
					ctimmg_(&imat, &n, &n, &c__[1], &lda, 
						&c__0, &c__0);
					goto L240;
				    }

/*                             Subtract the time used in CTIMMG. */

				    icl = 1;
				    s1 = second_();
L250:
				    s2 = second_();
				    untime = s2 - s1;
				    ++icl;
				    if (icl <= ic) {
					ctimmg_(&imat, &n, &n, &c__[1], &lda, 
						&c__0, &c__0);
					goto L250;
				    }

				    time = (time - untime) / (real) ic;
				    ops = sopbl3_(cname, &n, &n, &k);
				    reslts_ref(ik, in, ilda) = smflop_(&ops, &
					    time, &c__0);
/* L260: */
				}
/* L270: */
			    }
/* L280: */
			}
			io___43.ciunit = *nout;
			s_wsfe(&io___43);
			do_fio(&c__1, cname, (ftnlen)6);
			do_fio(&c__1, uplo, (ftnlen)1);
			do_fio(&c__1, transa, (ftnlen)1);
			e_wsfe();
			sprtbl_("K", "N", nk, &kval[1], nn, &nval[1], nlda, &
				reslts[reslts_offset], ldr1, ldr2, nout, (
				ftnlen)1, (ftnlen)1);
		    }
/* L290: */
		}
/* L300: */
	    }

/*        Time CHER2K */

	} else if (s_cmp(cname, "CHER2K", (ftnlen)6, (ftnlen)6) == 0) {
	    for (iuplo = 1; iuplo <= 2; ++iuplo) {
		*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];
		if (lsame_(uplo, "U")) {
		    imat = 6;
		} else {
		    imat = -6;
		}
		for (itb = 1; itb <= 3; ++itb) {
		    *(unsigned char *)transb = *(unsigned char *)&trans[itb - 
			    1];
		    if (*(unsigned char *)transb != 'T') {
			i__1 = *nlda;
			for (ilda = 1; ilda <= i__1; ++ilda) {
			    lda = ldaval[ilda];
			    i__2 = *nk;
			    for (ik = 1; ik <= i__2; ++ik) {
				k = kval[ik];
				if (*(unsigned char *)transb == 'N') {
				    ctimmg_(&c__1, &n, &k, &a[1], &lda, &c__0,
					     &c__0);
				    ctimmg_(&c__0, &n, &k, &b[1], &lda, &c__0,
					     &c__0);
				} else {
				    ctimmg_(&c__1, &k, &n, &a[1], &lda, &c__0,
					     &c__0);
				    ctimmg_(&c__0, &k, &n, &b[1], &lda, &c__0,
					     &c__0);
				}
				i__3 = *nn;
				for (in = 1; in <= i__3; ++in) {
				    n = nval[in];
				    ctimmg_(&imat, &n, &n, &c__[1], &lda, &
					    c__0, &c__0);
				    ic = 0;
				    s1 = second_();
L310:
				    cher2k_(uplo, transb, &n, &k, &c_b1, &a[1]
					    , &lda, &b[1], &lda, &c_b156, &
					    c__[1], &lda);
				    s2 = second_();
				    time = s2 - s1;
				    ++ic;
				    if (time < *timmin) {
					ctimmg_(&imat, &n, &n, &c__[1], &lda, 
						&c__0, &c__0);
					goto L310;
				    }

/*                             Subtract the time used in CTIMMG. */

				    icl = 1;
				    s1 = second_();
L320:
				    s2 = second_();
				    untime = s2 - s1;
				    ++icl;
				    if (icl <= ic) {
					ctimmg_(&imat, &n, &n, &c__[1], &lda, 
						&c__0, &c__0);
					goto L320;
				    }

				    time = (time - untime) / (real) ic;
				    ops = sopbl3_(cname, &n, &n, &k);
				    reslts_ref(ik, in, ilda) = smflop_(&ops, &
					    time, &c__0);
/* L330: */
				}
/* L340: */
			    }
/* L350: */
			}
			io___44.ciunit = *nout;
			s_wsfe(&io___44);
			do_fio(&c__1, cname, (ftnlen)6);
			do_fio(&c__1, uplo, (ftnlen)1);
			do_fio(&c__1, transb, (ftnlen)1);
			e_wsfe();
			sprtbl_("K", "N", nk, &kval[1], nn, &nval[1], nlda, &
				reslts[reslts_offset], ldr1, ldr2, nout, (
				ftnlen)1, (ftnlen)1);
		    }
/* L360: */
		}
/* L370: */
	    }

/*        Time CSYRK */

	} else if (s_cmp(cname, "CSYRK ", (ftnlen)6, (ftnlen)6) == 0) {
	    for (iuplo = 1; iuplo <= 2; ++iuplo) {
		*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];
		if (lsame_(uplo, "U")) {
		    imat = 8;
		} else {
		    imat = -8;
		}
		for (ita = 1; ita <= 3; ++ita) {
		    *(unsigned char *)transa = *(unsigned char *)&trans[ita - 
			    1];
		    if (*(unsigned char *)transa != 'C') {
			i__1 = *nlda;
			for (ilda = 1; ilda <= i__1; ++ilda) {
			    lda = ldaval[ilda];
			    i__2 = *nk;
			    for (ik = 1; ik <= i__2; ++ik) {
				k = kval[ik];
				if (*(unsigned char *)transa == 'N') {
				    ctimmg_(&c__1, &n, &k, &a[1], &lda, &c__0,
					     &c__0);
				} else {
				    ctimmg_(&c__1, &k, &n, &a[1], &lda, &c__0,
					     &c__0);
				}
				i__3 = *nn;
				for (in = 1; in <= i__3; ++in) {
				    n = nval[in];
				    ctimmg_(&imat, &n, &n, &c__[1], &lda, &
					    c__0, &c__0);
				    ic = 0;
				    s1 = second_();
L380:
				    csyrk_(uplo, transa, &n, &k, &c_b1, &a[1],
					     &lda, &c_b1, &c__[1], &lda);
				    s2 = second_();
				    time = s2 - s1;
				    ++ic;
				    if (time < *timmin) {
					ctimmg_(&imat, &n, &n, &c__[1], &lda, 
						&c__0, &c__0);
					goto L380;
				    }

/*                             Subtract the time used in CTIMMG. */

				    icl = 1;
				    s1 = second_();
L390:
				    s2 = second_();
				    untime = s2 - s1;
				    ++icl;
				    if (icl <= ic) {
					ctimmg_(&imat, &n, &n, &c__[1], &lda, 
						&c__0, &c__0);
					goto L390;
				    }

				    time = (time - untime) / (real) ic;
				    ops = sopbl3_(cname, &n, &n, &k);
				    reslts_ref(ik, in, ilda) = smflop_(&ops, &
					    time, &c__0);
/* L400: */
				}
/* L410: */
			    }
/* L420: */
			}
			io___45.ciunit = *nout;
			s_wsfe(&io___45);
			do_fio(&c__1, cname, (ftnlen)6);
			do_fio(&c__1, uplo, (ftnlen)1);
			do_fio(&c__1, transa, (ftnlen)1);
			e_wsfe();
			sprtbl_("K", "N", nk, &kval[1], nn, &nval[1], nlda, &
				reslts[reslts_offset], ldr1, ldr2, nout, (
				ftnlen)1, (ftnlen)1);
		    }
/* L430: */
		}
/* L440: */
	    }

/*        Time CSYR2K */

	} else if (s_cmp(cname, "CSYR2K", (ftnlen)6, (ftnlen)6) == 0) {
	    for (iuplo = 1; iuplo <= 2; ++iuplo) {
		*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];
		if (lsame_(uplo, "U")) {
		    imat = 8;
		} else {
		    imat = -8;
		}
		for (itb = 1; itb <= 3; ++itb) {
		    *(unsigned char *)transb = *(unsigned char *)&trans[itb - 
			    1];
		    if (*(unsigned char *)transb != 'C') {
			i__1 = *nlda;
			for (ilda = 1; ilda <= i__1; ++ilda) {
			    lda = ldaval[ilda];
			    i__2 = *nk;
			    for (ik = 1; ik <= i__2; ++ik) {
				k = kval[ik];
				if (*(unsigned char *)transb == 'N') {
				    ctimmg_(&c__1, &n, &k, &a[1], &lda, &c__0,
					     &c__0);
				    ctimmg_(&c__0, &n, &k, &b[1], &lda, &c__0,
					     &c__0);
				} else {
				    ctimmg_(&c__1, &k, &n, &a[1], &lda, &c__0,
					     &c__0);
				    ctimmg_(&c__0, &k, &n, &b[1], &lda, &c__0,
					     &c__0);
				}
				i__3 = *nn;
				for (in = 1; in <= i__3; ++in) {
				    n = nval[in];
				    ctimmg_(&imat, &n, &n, &c__[1], &lda, &
					    c__0, &c__0);
				    ic = 0;
				    s1 = second_();
L450:
				    csyr2k_(uplo, transb, &n, &k, &c_b1, &a[1]
					    , &lda, &b[1], &lda, &c_b1, &c__[
					    1], &lda);
				    s2 = second_();
				    time = s2 - s1;
				    ++ic;
				    if (time < *timmin) {
					ctimmg_(&imat, &n, &n, &c__[1], &lda, 
						&c__0, &c__0);
					goto L450;
				    }

/*                             Subtract the time used in CTIMMG. */

				    icl = 1;
				    s1 = second_();
L460:
				    s2 = second_();
				    untime = s2 - s1;
				    ++icl;
				    if (icl <= ic) {
					ctimmg_(&imat, &n, &n, &c__[1], &lda, 
						&c__0, &c__0);
					goto L460;
				    }

				    time = (time - untime) / (real) ic;
				    ops = sopbl3_(cname, &n, &n, &k);
				    reslts_ref(ik, in, ilda) = smflop_(&ops, &
					    time, &c__0);
/* L470: */
				}
/* L480: */
			    }
/* L490: */
			}
			io___46.ciunit = *nout;
			s_wsfe(&io___46);
			do_fio(&c__1, cname, (ftnlen)6);
			do_fio(&c__1, uplo, (ftnlen)1);
			do_fio(&c__1, transb, (ftnlen)1);
			e_wsfe();
			sprtbl_("K", "N", nk, &kval[1], nn, &nval[1], nlda, &
				reslts[reslts_offset], ldr1, ldr2, nout, (
				ftnlen)1, (ftnlen)1);
		    }
/* L500: */
		}
/* L510: */
	    }

/*        Time CTRMM */

	} else if (s_cmp(cname, "CTRMM ", (ftnlen)6, (ftnlen)6) == 0) {
	    for (iside = 1; iside <= 2; ++iside) {
		*(unsigned char *)side = *(unsigned char *)&sides[iside - 1];
		for (iuplo = 1; iuplo <= 2; ++iuplo) {
		    *(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 
			    1];
		    if (lsame_(uplo, "U")) {
			imat = 11;
		    } else {
			imat = -11;
		    }
		    for (ita = 1; ita <= 3; ++ita) {
			*(unsigned char *)transa = *(unsigned char *)&trans[
				ita - 1];
			i__1 = *nlda;
			for (ilda = 1; ilda <= i__1; ++ilda) {
			    lda = ldaval[ilda];
			    i__2 = *nm;
			    for (im = 1; im <= i__2; ++im) {
				m = mval[im];
				i__3 = *nn;
				for (in = 1; in <= i__3; ++in) {
				    n = nval[in];
				    if (iside == 1) {
					ctimmg_(&imat, &m, &m, &a[1], &lda, &
						c__0, &c__0);
				    } else {
					ctimmg_(&imat, &n, &n, &a[1], &lda, &
						c__0, &c__0);
				    }
				    ctimmg_(&c__0, &m, &n, &b[1], &lda, &c__0,
					     &c__0);
				    ic = 0;
				    s1 = second_();
L520:
				    ctrmm_(side, uplo, transa, "Non-unit", &m,
					     &n, &c_b1, &a[1], &lda, &b[1], &
					    lda);
				    s2 = second_();
				    time = s2 - s1;
				    ++ic;
				    if (time < *timmin) {
					ctimmg_(&c__0, &m, &n, &b[1], &lda, &
						c__0, &c__0);
					goto L520;
				    }

/*                             Subtract the time used in CTIMMG. */

				    icl = 1;
				    s1 = second_();
L530:
				    s2 = second_();
				    untime = s2 - s1;
				    ++icl;
				    if (icl <= ic) {
					ctimmg_(&c__0, &m, &n, &b[1], &lda, &
						c__0, &c__0);
					goto L530;
				    }

				    time = (time - untime) / (real) ic;
				    i__4 = iside - 1;
				    ops = sopbl3_(cname, &m, &n, &i__4);
				    reslts_ref(im, in, ilda) = smflop_(&ops, &
					    time, &c__0);
/* L540: */
				}
/* L550: */
			    }
/* L560: */
			}
			io___47.ciunit = *nout;
			s_wsfe(&io___47);
			do_fio(&c__1, cname, (ftnlen)6);
			do_fio(&c__1, side, (ftnlen)1);
			do_fio(&c__1, uplo, (ftnlen)1);
			do_fio(&c__1, transa, (ftnlen)1);
			e_wsfe();
			sprtbl_("M", "N", nm, &mval[1], nn, &nval[1], nlda, &
				reslts[reslts_offset], ldr1, ldr2, nout, (
				ftnlen)1, (ftnlen)1);
/* L570: */
		    }
/* L580: */
		}
/* L590: */
	    }

/*        Time CTRSM */

	} else if (s_cmp(cname, "CTRSM ", (ftnlen)6, (ftnlen)6) == 0) {
	    for (iside = 1; iside <= 2; ++iside) {
		*(unsigned char *)side = *(unsigned char *)&sides[iside - 1];
		for (iuplo = 1; iuplo <= 2; ++iuplo) {
		    *(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 
			    1];
		    if (lsame_(uplo, "U")) {
			imat = 11;
		    } else {
			imat = -11;
		    }
		    for (ita = 1; ita <= 3; ++ita) {
			*(unsigned char *)transa = *(unsigned char *)&trans[
				ita - 1];
			i__1 = *nlda;
			for (ilda = 1; ilda <= i__1; ++ilda) {
			    lda = ldaval[ilda];
			    i__2 = *nm;
			    for (im = 1; im <= i__2; ++im) {
				m = mval[im];
				i__3 = *nn;
				for (in = 1; in <= i__3; ++in) {
				    n = nval[in];
				    if (iside == 1) {
					ctimmg_(&imat, &m, &m, &a[1], &lda, &
						c__0, &c__0);
				    } else {
					ctimmg_(&imat, &n, &n, &a[1], &lda, &
						c__0, &c__0);
				    }
				    ctimmg_(&c__0, &m, &n, &b[1], &lda, &c__0,
					     &c__0);
				    ic = 0;
				    s1 = second_();
L600:
				    ctrsm_(side, uplo, transa, "Non-unit", &m,
					     &n, &c_b1, &a[1], &lda, &b[1], &
					    lda);
				    s2 = second_();
				    time = s2 - s1;
				    ++ic;
				    if (time < *timmin) {
					ctimmg_(&c__0, &m, &n, &b[1], &lda, &
						c__0, &c__0);
					goto L600;
				    }

/*                             Subtract the time used in CTIMMG. */

				    icl = 1;
				    s1 = second_();
L610:
				    s2 = second_();
				    untime = s2 - s1;
				    ++icl;
				    if (icl <= ic) {
					ctimmg_(&c__0, &m, &n, &b[1], &lda, &
						c__0, &c__0);
					goto L610;
				    }

				    time = (time - untime) / (real) ic;
				    i__4 = iside - 1;
				    ops = sopbl3_(cname, &m, &n, &i__4);
				    reslts_ref(im, in, ilda) = smflop_(&ops, &
					    time, &c__0);
/* L620: */
				}
/* L630: */
			    }
/* L640: */
			}
			io___48.ciunit = *nout;
			s_wsfe(&io___48);
			do_fio(&c__1, cname, (ftnlen)6);
			do_fio(&c__1, side, (ftnlen)1);
			do_fio(&c__1, uplo, (ftnlen)1);
			do_fio(&c__1, transa, (ftnlen)1);
			e_wsfe();
			sprtbl_("M", "N", nm, &mval[1], nn, &nval[1], nlda, &
				reslts[reslts_offset], ldr1, ldr2, nout, (
				ftnlen)1, (ftnlen)1);
/* L650: */
		    }
/* L660: */
		}
/* L670: */
	    }
	}
	io___49.ciunit = *nout;
	s_wsfe(&io___49);
	e_wsfe();
L680:
	;
    }
L690:

    return 0;

/*     End of CTIMB3 */

} /* ctimb3_ */

#undef reslts_ref
#undef names_ref


