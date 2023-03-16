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
static real c_b26 = 100.f;
static real c_b27 = 1.f;
static integer c__0 = 0;
static integer c_n1 = -1;

/* Subroutine */ int stimhr_(char *line, integer *nm, integer *mval, integer *
	nn, integer *nval, integer *nnb, integer *nbval, integer *nxval, 
	integer *nlda, integer *ldaval, real *timmin, real *a, real *tau, 
	real *b, real *work, real *reslts, integer *ldr1, integer *ldr2, 
	integer *ldr3, integer *nout, ftnlen line_len)
{
    /* Initialized data */

    static char subnam[6*4] = "SGEHRD" "ORTHES" "SORGHR" "SORMHR";
    static char sides[1*2] = "L" "R";
    static char transs[1*2] = "N" "T";
    static integer iseed[4] = { 0,0,0,1 };

    /* Format strings */
    static char fmt_9999[] = "(1x,a6,\002 timing run not attempted\002,/)";
    static char fmt_9998[] = "(/\002 *** Speed of \002,a6,\002 in megaflops "
	    "*** \002)";
    static char fmt_9997[] = "(5x,\002line \002,i2,\002 with LDA = \002,i5)";
    static char fmt_9995[] = "(/5x,\002ILO = 1, IHI = N\002,/)";
    static char fmt_9994[] = "(/5x,\002ILO = 1, IHI = M if SIDE = 'L'\002,/5"
	    "x,\002             = N if SIDE = 'R'\002)";
    static char fmt_9996[] = "(/5x,a6,\002 with SIDE = '\002,a1,\002', TRANS"
	    " = '\002,a1,\002', \002,a1,\002 =\002,i6,/)";

    /* System generated locals */
    integer reslts_dim1, reslts_dim2, reslts_dim3, reslts_offset, i__1, i__2, 
	    i__3, i__4, i__5, i__6;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static integer ilda;
    static char side[1];
    static integer info;
    static char path[3];
    static real time;
    static integer isub, i__, m, n;
    static char cname[6];
    static integer iside, itoff, itran;
    extern doublereal sopla_(char *, integer *, integer *, integer *, integer 
	    *, integer *);
    extern /* Subroutine */ int icopy_(integer *, integer *, integer *, 
	    integer *, integer *);
    static char trans[1];
    static integer i4, m1, n1;
    static real s1, s2;
    static integer ic;
    extern /* Subroutine */ int sprtb3_(char *, char *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, real *, integer *, 
	    integer *, integer *, ftnlen, ftnlen);
    static integer nb, im, in, lw, nx, reseed[4];
    extern /* Subroutine */ int atimck_(integer *, char *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, ftnlen);
    extern doublereal second_(void);
    extern /* Subroutine */ int sgehrd_(integer *, integer *, integer *, real 
	    *, integer *, real *, real *, integer *, integer *), atimin_(char 
	    *, char *, integer *, char *, logical *, integer *, integer *, 
	    ftnlen, ftnlen, ftnlen), slacpy_(char *, integer *, integer *, 
	    real *, integer *, real *, integer *), xlaenv_(integer *, 
	    integer *);
    extern doublereal smflop_(real *, real *, integer *);
    static real untime;
    extern /* Subroutine */ int stimmg_(integer *, integer *, integer *, real 
	    *, integer *, integer *, integer *);
    static logical timsub[4];
    extern /* Subroutine */ int orthes_(integer *, integer *, integer *, 
	    integer *, real *, real *), slatms_(integer *, integer *, char *, 
	    integer *, char *, real *, integer *, real *, real *, integer *, 
	    integer *, char *, real *, integer *, real *, integer *), sorghr_(integer *, integer *, integer *, real *, 
	    integer *, real *, real *, integer *, integer *), sprtbl_(char *, 
	    char *, integer *, integer *, integer *, integer *, integer *, 
	    real *, integer *, integer *, integer *, ftnlen, ftnlen), sormhr_(
	    char *, char *, integer *, integer *, integer *, integer *, real *
	    , integer *, real *, real *, integer *, real *, integer *, 
	    integer *);
    static integer lda, icl, inb, ihi, ilo;
    static real ops;
    static char lab1[1], lab2[1];

    /* Fortran I/O blocks */
    static cilist io___9 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___10 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___40 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___42 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___43 = { 0, 0, 0, fmt_9995, 0 };
    static cilist io___46 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___47 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___48 = { 0, 0, 0, fmt_9994, 0 };
    static cilist io___49 = { 0, 0, 0, fmt_9996, 0 };



#define subnam_ref(a_0,a_1) &subnam[(a_1)*6 + a_0 - 6]
#define reslts_ref(a_1,a_2,a_3,a_4) reslts[(((a_4)*reslts_dim3 + (a_3))*\
reslts_dim2 + (a_2))*reslts_dim1 + a_1]


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       March 31, 1993   


    Purpose   
    =======   

    STIMHR times the LAPACK routines SGEHRD, SORGHR, and SORMHR and the   
    EISPACK routine ORTHES.   

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

    TIMMIN  (input) REAL   
            The minimum time a subroutine will be timed.   

    A       (workspace) REAL array, dimension (LDAMAX*NMAX)   
            where LDAMAX and NMAX are the maximum values of LDA and N.   

    TAU     (workspace) REAL array, dimension (min(M,N))   

    B       (workspace) REAL array, dimension (LDAMAX*NMAX)   

    WORK    (workspace) REAL array, dimension (LDAMAX*NBMAX)   
            where NBMAX is the maximum value of NB.   

    RESLTS  (workspace) REAL array, dimension   
                        (LDR1,LDR2,LDR3,4*NN+3)   
            The timing results for each subroutine over the relevant   
            values of M, (NB,NX), LDA, and N.   

    LDR1    (input) INTEGER   
            The first dimension of RESLTS.  LDR1 >= max(1,NNB).   

    LDR2    (input) INTEGER   
            The second dimension of RESLTS.  LDR2 >= max(1,NM).   

    LDR3    (input) INTEGER   
            The third dimension of RESLTS.  LDR3 >= max(1,NLDA).   

    NOUT    (input) INTEGER   
            The unit number for output.   

    Internal Parameters   
    ===================   

    MODE    INTEGER   
            The matrix type.  MODE = 3 is a geometric distribution of   
            eigenvalues.  See CLATMS for further details.   

    COND    REAL   
            The condition number of the matrix.  The singular values are   
            set to values from DMAX to DMAX/COND.   

    DMAX    REAL   
            The magnitude of the largest singular value.   

    =====================================================================   

       Parameter adjustments */
    --mval;
    --nval;
    --nbval;
    --nxval;
    --ldaval;
    --a;
    --tau;
    --b;
    --work;
    reslts_dim1 = *ldr1;
    reslts_dim2 = *ldr2;
    reslts_dim3 = *ldr3;
    reslts_offset = 1 + reslts_dim1 * (1 + reslts_dim2 * (1 + reslts_dim3 * 1)
	    );
    reslts -= reslts_offset;

    /* Function Body   

       Extract the timing request from the input line. */

    s_copy(path, "Single precision", (ftnlen)1, (ftnlen)16);
    s_copy(path + 1, "HR", (ftnlen)2, (ftnlen)2);
    atimin_(path, line, &c__4, subnam, timsub, nout, &info, (ftnlen)3, (
	    ftnlen)80, (ftnlen)6);
    if (info != 0) {
	goto L210;
    }

/*     Check that N <= LDA for the input values. */

    s_copy(cname, line, (ftnlen)6, (ftnlen)6);
    atimck_(&c__2, cname, nm, &mval[1], nlda, &ldaval[1], nout, &info, (
	    ftnlen)6);
    if (info > 0) {
	io___9.ciunit = *nout;
	s_wsfe(&io___9);
	do_fio(&c__1, cname, (ftnlen)6);
	e_wsfe();
	goto L210;
    }

/*     Check that K <= LDA for SORMHR */

    if (timsub[3]) {
	atimck_(&c__3, cname, nn, &nval[1], nlda, &ldaval[1], nout, &info, (
		ftnlen)6);
	if (info > 0) {
	    io___10.ciunit = *nout;
	    s_wsfe(&io___10);
	    do_fio(&c__1, subnam_ref(0, 4), (ftnlen)6);
	    e_wsfe();
	    timsub[3] = FALSE_;
	}
    }

/*     Do for each value of M: */

    i__1 = *nm;
    for (im = 1; im <= i__1; ++im) {
	m = mval[im];
	ilo = 1;
	ihi = m;
	icopy_(&c__4, iseed, &c__1, reseed, &c__1);

/*        Do for each value of LDA: */

	i__2 = *nlda;
	for (ilda = 1; ilda <= i__2; ++ilda) {
	    lda = ldaval[ilda];

/*           Do for each pair of values (NB, NX) in NBVAL and NXVAL. */

	    i__3 = *nnb;
	    for (inb = 1; inb <= i__3; ++inb) {
		nb = nbval[inb];
		xlaenv_(&c__1, &nb);
		nx = nxval[inb];
		xlaenv_(&c__3, &nx);
/* Computing MAX */
		i__4 = 1, i__5 = m * max(1,nb);
		lw = max(i__4,i__5);

/*              Generate a test matrix of size M by M. */

		icopy_(&c__4, reseed, &c__1, iseed, &c__1);
		slatms_(&m, &m, "Uniform", iseed, "Nonsym", &tau[1], &c__3, &
			c_b26, &c_b27, &m, &m, "No packing", &b[1], &lda, &
			work[1], &info);

		if (timsub[1] && inb == 1) {

/*                 ORTHES:  Eispack reduction using orthogonal   
                   transformations. */

		    slacpy_("Full", &m, &m, &b[1], &lda, &a[1], &lda);
		    ic = 0;
		    s1 = second_();
L10:
		    orthes_(&lda, &m, &c__1, &ihi, &a[1], &tau[1]);
		    s2 = second_();
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			slacpy_("Full", &m, &m, &b[1], &lda, &a[1], &lda);
			goto L10;
		    }

/*                 Subtract the time used in SLACPY. */

		    icl = 1;
		    s1 = second_();
L20:
		    s2 = second_();
		    untime = s2 - s1;
		    ++icl;
		    if (icl <= ic) {
			slacpy_("Full", &m, &m, &b[1], &lda, &a[1], &lda);
			goto L20;
		    }

		    time = (time - untime) / (real) ic;
		    ops = sopla_("SGEHRD", &m, &ilo, &ihi, &c__0, &nb);
		    reslts_ref(inb, im, ilda, 2) = smflop_(&ops, &time, &info)
			    ;
		}

		if (timsub[0]) {

/*                 SGEHRD:  Reduction to Hesenberg form */

		    slacpy_("Full", &m, &m, &b[1], &lda, &a[1], &lda);
		    ic = 0;
		    s1 = second_();
L30:
		    sgehrd_(&m, &ilo, &ihi, &a[1], &lda, &tau[1], &work[1], &
			    lw, &info);
		    s2 = second_();
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			slacpy_("Full", &m, &m, &b[1], &lda, &a[1], &lda);
			goto L30;
		    }

/*                 Subtract the time used in SLACPY. */

		    icl = 1;
		    s1 = second_();
L40:
		    s2 = second_();
		    untime = s2 - s1;
		    ++icl;
		    if (icl <= ic) {
			slacpy_("Full", &m, &m, &a[1], &lda, &b[1], &lda);
			goto L40;
		    }

		    time = (time - untime) / (real) ic;
		    ops = sopla_("SGEHRD", &m, &ilo, &ihi, &c__0, &nb);
		    reslts_ref(inb, im, ilda, 1) = smflop_(&ops, &time, &info)
			    ;
		} else {

/*                 If SGEHRD was not timed, generate a matrix and factor   
                   it using SGEHRD anyway so that the factored form of   
                   the matrix can be used in timing the other routines. */

		    slacpy_("Full", &m, &m, &b[1], &lda, &a[1], &lda);
		    sgehrd_(&m, &ilo, &ihi, &a[1], &lda, &tau[1], &work[1], &
			    lw, &info);
		}

		if (timsub[2]) {

/*                 SORGHR:  Generate the orthogonal matrix Q from the   
                   reduction to Hessenberg form A = Q*H*Q' */

		    slacpy_("Full", &m, &m, &a[1], &lda, &b[1], &lda);
		    ic = 0;
		    s1 = second_();
L50:
		    sorghr_(&m, &ilo, &ihi, &b[1], &lda, &tau[1], &work[1], &
			    lw, &info);
		    s2 = second_();
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			slacpy_("Full", &m, &m, &a[1], &lda, &b[1], &lda);
			goto L50;
		    }

/*                 Subtract the time used in SLACPY. */

		    icl = 1;
		    s1 = second_();
L60:
		    s2 = second_();
		    untime = s2 - s1;
		    ++icl;
		    if (icl <= ic) {
			slacpy_("Full", &m, &m, &a[1], &lda, &b[1], &lda);
			goto L60;
		    }

		    time = (time - untime) / (real) ic;

/*                 Op count for SORGHR:  same as   
                      SORGQR( IHI-ILO, IHI-ILO, IHI-ILO, ... ) */

		    i__4 = ihi - ilo;
		    i__5 = ihi - ilo;
		    i__6 = ihi - ilo;
		    ops = sopla_("SORGQR", &i__4, &i__5, &i__6, &c__0, &nb);
		    reslts_ref(inb, im, ilda, 3) = smflop_(&ops, &time, &info)
			    ;
		}

		if (timsub[3]) {

/*                 SORMHR:  Multiply by Q stored as a product of   
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
				*(unsigned char *)trans = *(unsigned char *)&
					transs[itran - 1];
				stimmg_(&c__0, &m1, &n1, &b[1], &lda, &c__0, &
					c__0);
				ic = 0;
				s1 = second_();
L70:
				sormhr_(side, trans, &m1, &n1, &ilo, &ihi, &a[
					1], &lda, &tau[1], &b[1], &lda, &work[
					1], &lw, &info);
				s2 = second_();
				time = s2 - s1;
				++ic;
				if (time < *timmin) {
				    stimmg_(&c__0, &m1, &n1, &b[1], &lda, &
					    c__0, &c__0);
				    goto L70;
				}

/*                          Subtract the time used in STIMMG. */

				icl = 1;
				s1 = second_();
L80:
				s2 = second_();
				untime = s2 - s1;
				++icl;
				if (icl <= ic) {
				    stimmg_(&c__0, &m1, &n1, &b[1], &lda, &
					    c__0, &c__0);
				    goto L80;
				}

				time = (time - untime) / (real) ic;

/*                          Op count for SORMHR, SIDE='L':  same as   
                            SORMQR( 'L', TRANS, IHI-ILO, N, IHI-ILO, ...)   

                            Op count for SORMHR, SIDE='R':  same as   
                            SORMQR( 'R', TRANS, M, IHI-ILO, IHI-ILO, ...) */

				if (iside == 1) {
				    i__5 = ihi - ilo;
				    i__6 = ihi - ilo;
				    ops = sopla_("SORMQR", &i__5, &n1, &i__6, 
					    &c_n1, &nb);
				} else {
				    i__5 = ihi - ilo;
				    i__6 = ihi - ilo;
				    ops = sopla_("SORMQR", &m1, &i__5, &i__6, 
					    &c__1, &nb);
				}

				reslts_ref(inb, im, ilda, i4 + itoff + in) = 
					smflop_(&ops, &time, &info);
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

/*     Print tables of results for SGEHRD, ORTHES, and SORGHR */

    for (isub = 1; isub <= 3; ++isub) {
	if (! timsub[isub - 1]) {
	    goto L160;
	}
	io___40.ciunit = *nout;
	s_wsfe(&io___40);
	do_fio(&c__1, subnam_ref(0, isub), (ftnlen)6);
	e_wsfe();
	if (*nlda > 1) {
	    i__1 = *nlda;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		io___42.ciunit = *nout;
		s_wsfe(&io___42);
		do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&ldaval[i__], (ftnlen)sizeof(integer));
		e_wsfe();
/* L150: */
	    }
	}
	io___43.ciunit = *nout;
	s_wsfe(&io___43);
	e_wsfe();
	if (isub == 2) {
	    sprtb3_(" ", "N", &c__1, &nbval[1], &nxval[1], nm, &mval[1], nlda,
		     &reslts_ref(1, 1, 1, isub), ldr1, ldr2, nout, (ftnlen)1, 
		    (ftnlen)1);
	} else {
	    sprtb3_("(  NB,  NX)", "N", nnb, &nbval[1], &nxval[1], nm, &mval[
		    1], nlda, &reslts_ref(1, 1, 1, isub), ldr1, ldr2, nout, (
		    ftnlen)11, (ftnlen)1);
	}
L160:
	;
    }

/*     Print tables of results for SORMHR */

    isub = 4;
    if (timsub[isub - 1]) {
	i4 = 3;
	for (iside = 1; iside <= 2; ++iside) {
	    if (iside == 1) {
		*(unsigned char *)lab1 = 'M';
		*(unsigned char *)lab2 = 'N';
		if (*nlda > 1) {
		    io___46.ciunit = *nout;
		    s_wsfe(&io___46);
		    do_fio(&c__1, subnam_ref(0, isub), (ftnlen)6);
		    e_wsfe();
		    i__1 = *nlda;
		    for (i__ = 1; i__ <= i__1; ++i__) {
			io___47.ciunit = *nout;
			s_wsfe(&io___47);
			do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&ldaval[i__], (ftnlen)sizeof(
				integer));
			e_wsfe();
/* L170: */
		    }
		    io___48.ciunit = *nout;
		    s_wsfe(&io___48);
		    e_wsfe();
		}
	    } else {
		*(unsigned char *)lab1 = 'N';
		*(unsigned char *)lab2 = 'M';
	    }
	    for (itran = 1; itran <= 2; ++itran) {
		i__1 = *nn;
		for (in = 1; in <= i__1; ++in) {
		    io___49.ciunit = *nout;
		    s_wsfe(&io___49);
		    do_fio(&c__1, subnam_ref(0, isub), (ftnlen)6);
		    do_fio(&c__1, sides + (iside - 1), (ftnlen)1);
		    do_fio(&c__1, transs + (itran - 1), (ftnlen)1);
		    do_fio(&c__1, lab2, (ftnlen)1);
		    do_fio(&c__1, (char *)&nval[in], (ftnlen)sizeof(integer));
		    e_wsfe();
		    sprtbl_("NB", lab1, nnb, &nbval[1], nm, &mval[1], nlda, &
			    reslts_ref(1, 1, 1, i4 + in), ldr1, ldr2, nout, (
			    ftnlen)2, (ftnlen)1);
/* L180: */
		}
		i4 += *nn;
/* L190: */
	    }
/* L200: */
	}
    }
L210:

/*     Print a table of results for each timed routine. */

    return 0;

/*     End of STIMHR */

} /* stimhr_ */

#undef reslts_ref
#undef subnam_ref


