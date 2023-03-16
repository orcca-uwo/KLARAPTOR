#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    doublereal ops, itcnt;
} latime_;

#define latime_1 latime_

/* Table of constant values */

static integer c__12 = 12;
static integer c__1 = 1;
static doublereal c_b23 = 1.;
static integer c__4 = 4;
static integer c__2 = 2;
static integer c__3 = 3;
static integer c__0 = 0;
static integer c__8 = 8;
static doublereal c_b218 = 0.;

/* Subroutine */ int dtim21_(char *line, integer *nsizes, integer *nn, 
	integer *ntypes, logical *dotype, integer *nparms, integer *nnb, 
	integer *nshfts, integer *maxbs, integer *ldas, doublereal *timmin, 
	integer *nout, integer *iseed, doublereal *a, doublereal *h__, 
	doublereal *z__, doublereal *w, doublereal *work, integer *lwork, 
	logical *llwork, integer *iwork, doublereal *times, integer *ldt1, 
	integer *ldt2, integer *ldt3, doublereal *opcnts, integer *ldo1, 
	integer *ldo2, integer *ldo3, integer *info, ftnlen line_len)
{
    /* Initialized data */

    static char subnam[9*12] = "DGEHRD   " "DHSEQR(E)" "DHSEQR(S)" "DHSEQR(V)"
	     "DTREVC(L)" "DTREVC(R)" "DHSEIN(L)" "DHSEIN(R)" "ORTHES   " 
	    "HQR      " "HQR2     " "INVIT    ";
    static integer inparm[12] = { 2,4,4,4,1,1,1,1,1,1,1,1 };
    static char pnames[4*4] = "LDA " "NB  " "NS  " "MAXB";
    static integer kmode[8] = { 4,3,1,5,4,3,1,5 };
    static integer kconds[8] = { 1,1,1,1,2,2,2,2 };

    /* Format strings */
    static char fmt_9999[] = "(1x,a,\002 timing run not attempted -- N > LD"
	    "A\002,/)";
    static char fmt_9998[] = "(1x,a,\002 timing run not attempted -- LWORK t"
	    "oo small.\002,/)";
    static char fmt_9997[] = "(\002 DTIM21: \002,a,\002 returned INFO=\002,i"
	    "6,\002.\002,/9x,\002N=\002,i6,\002, ITYPE=\002,i6,\002, IPAR="
	    "\002,i6,\002, ISEED=(\002,3(i5,\002,\002),i5,\002)\002)";

    /* System generated locals */
    integer opcnts_dim1, opcnts_dim2, opcnts_dim3, opcnts_offset, times_dim1, 
	    times_dim2, times_dim3, times_offset, i__1, i__2, i__3, i__4, 
	    i__5, i__6;
    doublereal d__1;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);
    double sqrt(doublereal);

    /* Local variables */
    static integer maxb, ipar;
    static doublereal time;
    static integer isub, nmax, j, n, imode;
    extern doublereal dopla_(char *, integer *, integer *, integer *, integer 
	    *, integer *);
    static integer iinfo, mbmax, nbmax;
    static doublereal conds;
    static integer itemp, lastl, nsmax;
    extern /* Subroutine */ int invit_(integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, logical *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, doublereal *, doublereal *)
	    ;
    static integer itype, j1, j2, j3, j4;
    static doublereal s1, s2, rtulp;
    static integer ic, jc, nb, in;
    extern doublereal dlamch_(char *);
    static integer jr;
    extern /* Subroutine */ int dgehrd_(integer *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, doublereal *, integer *, 
	    integer *);
    extern doublereal dsecnd_(void);
    static integer ldamin;
    static char adumma[1*1];
    extern /* Subroutine */ int dlatme_(integer *, char *, integer *, 
	    doublereal *, integer *, doublereal *, doublereal *, char *, char 
	    *, char *, char *, doublereal *, integer *, doublereal *, integer 
	    *, integer *, doublereal *, doublereal *, integer *, doublereal *,
	     integer *), dhsein_(char 
	    *, char *, char *, logical *, integer *, doublereal *, integer *, 
	    doublereal *, doublereal *, doublereal *, integer *, doublereal *,
	     integer *, integer *, integer *, doublereal *, integer *, 
	    integer *, integer *), dlacpy_(char *, 
	    integer *, integer *, doublereal *, integer *, doublereal *, 
	    integer *), dlaset_(char *, integer *, integer *, 
	    doublereal *, doublereal *, doublereal *, integer *);
    static integer ioldsd[4], iconds;
    extern /* Subroutine */ int dprtbe_(char *, integer *, logical *, integer 
	    *, integer *, integer *, char *, integer *, integer *, integer *, 
	    integer *, integer *, doublereal *, integer *, integer *, 
	    doublereal *, integer *, integer *, doublereal *, logical *, 
	    integer *, ftnlen, ftnlen), atimin_(char *, char *, integer *, 
	    char *, logical *, integer *, integer *, ftnlen, ftnlen, ftnlen), 
	    dhseqr_(char *, char *, integer *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, doublereal *, doublereal *,
	     integer *, doublereal *, integer *, integer *);
    static integer nsbmax;
    extern /* Subroutine */ int dtrevc_(char *, char *, logical *, integer *, 
	    doublereal *, integer *, doublereal *, integer *, doublereal *, 
	    integer *, integer *, integer *, doublereal *, integer *);
    static integer nshift, lastnl;
    extern /* Subroutine */ int xlaenv_(integer *, integer *);
    static doublereal untime;
    static logical runhrd, timsub[12];
    extern /* Subroutine */ int orthes_(integer *, integer *, integer *, 
	    integer *, doublereal *, doublereal *);
    static logical runqre;
    static doublereal ulpinv;
    static logical runhqr;
    static doublereal rtulpi;
    static integer mtypes;
    static logical runort, runqrs;
    static integer lda, ldh, ldt, ldw;
    extern /* Subroutine */ int hqr_(integer *, integer *, integer *, integer 
	    *, doublereal *, doublereal *, doublereal *, integer *);
    static doublereal ulp;
    extern /* Subroutine */ int hqr2_(integer *, integer *, integer *, 
	    integer *, doublereal *, doublereal *, doublereal *, doublereal *,
	     integer *);

    /* Fortran I/O blocks */
    static cilist io___14 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___15 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___45 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___50 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___53 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___56 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___57 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___58 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___59 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___64 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___65 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___66 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___67 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___68 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___69 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___70 = { 0, 0, 0, fmt_9997, 0 };



#define times_ref(a_1,a_2,a_3,a_4) times[(((a_4)*times_dim3 + (a_3))*\
times_dim2 + (a_2))*times_dim1 + a_1]
#define subnam_ref(a_0,a_1) &subnam[(a_1)*9 + a_0 - 9]
#define opcnts_ref(a_1,a_2,a_3,a_4) opcnts[(((a_4)*opcnts_dim3 + (a_3))*\
opcnts_dim2 + (a_2))*opcnts_dim1 + a_1]


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

       DTIM21 times the LAPACK routines for the DOUBLE PRECISION   
       non-symmetric eigenvalue problem.   

       For each N value in NN(1:NSIZES) and .TRUE. value in   
       DOTYPE(1:NTYPES), a matrix will be generated and used to test the   
       selected routines.  Thus, NSIZES*(number of .TRUE. values in   
       DOTYPE) matrices will be generated.   

    Arguments   
    =========   

    LINE    (input) CHARACTER*80   
            On entry, LINE contains the input line which requested   
            this routine.  This line may contain a subroutine name,   
            such as DGEHRD, indicating that only routine SGEHRD will   
            be timed, or it may contain a generic name, such as DHS.   
            In this case, the rest of the line is scanned for the   
            first 12 non-blank characters, corresponding to the twelve   
            combinations of subroutine and options:   
            LAPACK:   
            1: DGEHRD   
            2: DHSEQR(JOB='E')   
            3: DHSEQR(JOB='S')   
            4: DHSEQR(JOB='I')   
            5: DTREVC(JOB='L')   
            6: DTREVC(JOB='R')   
            7: DHSEIN(JOB='L')   
            8: DHSEIN(JOB='R')   
            EISPACK:   
             9: ORTHES (compare with DGEHRD)   
            10: HQR    (compare w/ DHSEQR -- JOB='E')   
            11: HQR2   (compare w/ DHSEQR(JOB='I') plus DTREVC(JOB='R'))   
            12: INVIT  (compare with DHSEIN)   
            If a character is 'T' or 't', the corresponding routine in   
            this path is timed.  If the entire line is blank, all the   
            routines in the path are timed.   

    NSIZES  (input) INTEGER   
            The number of values of N contained in the vector NN.   

    NN      (input) INTEGER array, dimension( NSIZES )   
            The values of the matrix size N to be tested.  For each   
            N value in the array NN, and each .TRUE. value in DOTYPE,   
            a matrix A will be generated and used to test the routines.   

    NTYPES  (input) INTEGER   
            The number of types in DOTYPE.  Only the first MAXTYP   
            elements will be examined.  Exception: if NSIZES=1 and   
            NTYPES=MAXTYP+1, and DOTYPE=MAXTYP*f,t, then the input   
            value of A will be used.   

    DOTYPE  (input) LOGICAL   
            If DOTYPE(j) is .TRUE., then a matrix of type j will be   
            generated.  The matrix A has the form X**(-1) T X, where   
            X is orthogonal (for j=1--4) or has condition sqrt(ULP)   
            (for j=5--8), and T has random O(1) entries in the upper   
            triangle and:   
            (j=1,5) evenly spaced entries 1, ..., ULP with random signs   
            (j=2,6) geometrically spaced entries 1, ..., ULP with random   
                    signs   
            (j=3,7) "clustered" entries 1, ULP,..., ULP with random   
                    signs   
            (j=4,8) real or complex conjugate paired eigenvalues   
                    randomly chosen from ( ULP, 1 )   
            on the diagonal.   

    NPARMS  (input) INTEGER   
            The number of values in each of the arrays NNB, NSHFTS,   
            MAXBS, and LDAS.  For each matrix A generated according to   
            NN and DOTYPE, tests will be run with (NB,NSHIFT,MAXB,LDA)=   
            (NNB(1), NSHFTS(1), MAXBS(1), LDAS(1)),...,   
            (NNB(NPARMS), NSHFTS(NPARMS), MAXBS(NPARMS), LDAS(NPARMS))   

    NNB     (input) INTEGER array, dimension( NPARMS )   
            The values of the blocksize ("NB") to be tested.   

    NSHFTS  (input) INTEGER array, dimension( NPARMS )   
            The values of the number of shifts ("NSHIFT") to be tested.   

    MAXBS   (input) INTEGER array, dimension( NPARMS )   
            The values of "MAXB", the size of largest submatrix to be   
            processed by DLAHQR (EISPACK method), to be tested.   

    LDAS    (input) INTEGER array, dimension( NPARMS )   
            The values of LDA, the leading dimension of all matrices,   
            to be tested.   

    TIMMIN  (input) DOUBLE PRECISION   
            The minimum time a subroutine will be timed.   

    NOUT    (input) INTEGER   
            If NOUT > 0 then NOUT specifies the unit number   
            on which the output will be printed.  If NOUT <= 0, no   
            output is printed.   

    ISEED   (input/output) INTEGER array, dimension( 4 )   
            The random seed used by the random number generator, used   
            by the test matrix generator.  It is used and updated on   
            each call to DTIM21   

    A       (workspace) DOUBLE PRECISION array,   
                        dimension( max(NN)*max(LDAS) )   
            (a) During the testing of DGEHRD, the original matrix to   
                be tested.   
            (b) Later, the Schur form of the original matrix.   

    H       (workspace) DOUBLE PRECISION array,   
                        dimension( max(NN)*max(LDAS) )   
            The Hessenberg form of the original matrix.   

    Z       (workspace) DOUBLE PRECISION array,   
                        dimension( max(NN)*max(LDAS) )   
            Various output arrays: from DGEHRD and DHSEQR, the   
            orthogonal reduction matrices; from DTREVC and DHSEIN,   
            the eigenvector matrices.   

    W       (workspace) DOUBLE PRECISION array,   
                        dimension( 2*max(LDAS) )   
            Treated as an LDA x 2 matrix whose 1st column holds WR, the   
            real parts of the eigenvalues, and whose 2nd column holds   
            WI, the imaginary parts of the eigenvalues of A.   

    WORK    (workspace) DOUBLE PRECISION array, dimension( LWORK )   

    LWORK   (input) INTEGER   
            Number of elements in WORK.  It must be at least   
            (a)  max(NN)*( 3*max(NNB) + 2 )   
            (b)  max(NN)*( max(NNB+NSHFTS) + 1 )   
            (c)  max(NSHFTS)*( max(NSHFTS) + max(NN) )   
            (d)  max(MAXBS)*( max(MAXBS) + max(NN) )   
            (e)  ( max(NN) + 2 )**2  +  max(NN)   
            (f)  NSIZES*NTYPES*NPARMS   

    LLWORK  (workspace) LOGICAL array, dimension( max( max(NN), NPARMS ))   

    IWORK   (workspace) INTEGER array, dimension( 2*max(NN) )   
            Workspace needed for parameters IFAILL and IFAILR in call   
            to DHSEIN.   

    TIMES   (output) DOUBLE PRECISION array,   
                     dimension (LDT1,LDT2,LDT3,NSUBS)   
            TIMES(i,j,k,l) will be set to the run time (in seconds) for   
            subroutine l, with N=NN(k), matrix type j, and LDA=LDAS(i),   
            MAXB=MAXBS(i), NBLOCK=NNB(i), and NSHIFT=NSHFTS(i).   

    LDT1    (input) INTEGER   
            The first dimension of TIMES.  LDT1 >= min( 1, NPARMS ).   

    LDT2    (input) INTEGER   
            The second dimension of TIMES.  LDT2 >= min( 1, NTYPES ).   

    LDT3    (input) INTEGER   
            The third dimension of TIMES.  LDT3 >= min( 1, NSIZES ).   

    OPCNTS  (output) DOUBLE PRECISION array,   
                     dimension (LDO1,LDO2,LDO3,NSUBS)   
            OPCNTS(i,j,k,l) will be set to the number of floating-point   
            operations executed by subroutine l, with N=NN(k), matrix   
            type j, and LDA=LDAS(i), MAXB=MAXBS(i), NBLOCK=NNB(i), and   
            NSHIFT=NSHFTS(i).   

    LDO1    (input) INTEGER   
            The first dimension of OPCNTS.  LDO1 >= min( 1, NPARMS ).   

    LDO2    (input) INTEGER   
            The second dimension of OPCNTS.  LDO2 >= min( 1, NTYPES ).   

    LDO3    (input) INTEGER   
            The third dimension of OPCNTS.  LDO3 >= min( 1, NSIZES ).   

    INFO    (output) INTEGER   
            Error flag.  It will be set to zero if no error occurred.   

    =====================================================================   

       Parameter adjustments */
    --nn;
    --dotype;
    --nnb;
    --nshfts;
    --maxbs;
    --ldas;
    --iseed;
    --a;
    --h__;
    --z__;
    --w;
    --work;
    --llwork;
    --iwork;
    times_dim1 = *ldt1;
    times_dim2 = *ldt2;
    times_dim3 = *ldt3;
    times_offset = 1 + times_dim1 * (1 + times_dim2 * (1 + times_dim3 * 1));
    times -= times_offset;
    opcnts_dim1 = *ldo1;
    opcnts_dim2 = *ldo2;
    opcnts_dim3 = *ldo3;
    opcnts_offset = 1 + opcnts_dim1 * (1 + opcnts_dim2 * (1 + opcnts_dim3 * 1)
	    );
    opcnts -= opcnts_offset;

    /* Function Body   

       Quick Return */

    *info = 0;
    if (*nsizes <= 0 || *ntypes <= 0 || *nparms <= 0) {
	return 0;
    }

/*     Extract the timing request from the input line. */

    atimin_("DHS", line, &c__12, subnam, timsub, nout, info, (ftnlen)3, (
	    ftnlen)80, (ftnlen)9);
    if (*info != 0) {
	return 0;
    }

/*     Compute Maximum Values */

    nmax = 0;
    i__1 = *nsizes;
    for (j1 = 1; j1 <= i__1; ++j1) {
/* Computing MAX */
	i__2 = nmax, i__3 = nn[j1];
	nmax = max(i__2,i__3);
/* L10: */
    }

    ldamin = max(1,nmax) << 1;
    nbmax = 0;
    nsmax = 0;
    mbmax = 0;
    nsbmax = 0;
    i__1 = *nparms;
    for (j1 = 1; j1 <= i__1; ++j1) {
/* Computing MIN */
	i__2 = ldamin, i__3 = ldas[j1];
	ldamin = min(i__2,i__3);
/* Computing MAX */
	i__2 = nbmax, i__3 = nnb[j1];
	nbmax = max(i__2,i__3);
/* Computing MAX */
	i__2 = nsmax, i__3 = nshfts[j1];
	nsmax = max(i__2,i__3);
/* Computing MAX */
	i__2 = mbmax, i__3 = maxbs[j1];
	mbmax = max(i__2,i__3);
/* Computing MAX */
	i__2 = nsbmax, i__3 = nnb[j1] + nshfts[j1];
	nsbmax = max(i__2,i__3);
/* L20: */
    }

/*     Check that N <= LDA for the input values. */

    if (nmax > ldamin) {
	*info = -10;
	io___14.ciunit = *nout;
	s_wsfe(&io___14);
	do_fio(&c__1, line, (ftnlen)6);
	e_wsfe();
	return 0;
    }

/*     Check LWORK   

   Computing MAX   
   Computing MAX */
    i__3 = nbmax * 3 + 2, i__4 = nsbmax + 1;
    i__1 = nmax * max(i__3,i__4), i__2 = nsmax * (nsmax + nmax), i__1 = max(
	    i__1,i__2), i__2 = mbmax * (mbmax + nmax), i__1 = max(i__1,i__2), 
	    i__2 = (nmax + 1) * (nmax + 4), i__1 = max(i__1,i__2), i__2 = *
	    nsizes * *ntypes * *nparms;
    if (*lwork < max(i__1,i__2)) {
	*info = -19;
	io___15.ciunit = *nout;
	s_wsfe(&io___15);
	do_fio(&c__1, line, (ftnlen)6);
	e_wsfe();
	return 0;
    }

/*     Check to see whether DGEHRD or DHSEQR must be run.   

       RUNQRE -- if DHSEQR must be run to get eigenvalues.   
       RUNQRS -- if DHSEQR must be run to get Schur form.   
       RUNHRD -- if DGEHRD must be run. */

    runqrs = FALSE_;
    runqre = FALSE_;
    runhrd = FALSE_;
    if (timsub[4] || timsub[5]) {
	runqrs = TRUE_;
    }
    if (timsub[6] || timsub[7]) {
	runqre = TRUE_;
    }
    if (timsub[1] || timsub[2] || timsub[3] || runqrs || runqre) {
	runhrd = TRUE_;
    }
    if (timsub[2] || timsub[3] || runqrs) {
	runqre = FALSE_;
    }
    if (timsub[3]) {
	runqrs = FALSE_;
    }

/*     Check to see whether ORTHES or HQR must be run.   

       RUNHQR -- if HQR must be run to get eigenvalues.   
       RUNORT -- if ORTHES must be run. */

    runhqr = FALSE_;
    runort = FALSE_;
    if (timsub[11]) {
	runhqr = TRUE_;
    }
    if (timsub[9] || timsub[10] || runhqr) {
	runort = TRUE_;
    }
    if (timsub[9] || timsub[10]) {
	runhqr = FALSE_;
    }
    if (timsub[8]) {
	runort = FALSE_;
    }

/*     Various Constants */

    ulp = dlamch_("Epsilon") * dlamch_("Base");
    ulpinv = 1. / ulp;
    rtulp = sqrt(ulp);
    rtulpi = 1. / rtulp;

/*     Zero out OPCNTS, TIMES */

    for (j4 = 1; j4 <= 12; ++j4) {
	i__1 = *nsizes;
	for (j3 = 1; j3 <= i__1; ++j3) {
	    i__2 = *ntypes;
	    for (j2 = 1; j2 <= i__2; ++j2) {
		i__3 = *nparms;
		for (j1 = 1; j1 <= i__3; ++j1) {
		    opcnts_ref(j1, j2, j3, j4) = 0.;
		    times_ref(j1, j2, j3, j4) = 0.;
/* L30: */
		}
/* L40: */
	    }
/* L50: */
	}
/* L60: */
    }

/*     Do for each value of N: */

    i__1 = *nsizes;
    for (in = 1; in <= i__1; ++in) {

	n = nn[in];

/*        Do for each .TRUE. value in DOTYPE: */

	mtypes = min(8,*ntypes);
	if (*ntypes == 9 && *nsizes == 1) {
	    mtypes = *ntypes;
	}
	i__2 = mtypes;
	for (itype = 1; itype <= i__2; ++itype) {
	    if (! dotype[itype]) {
		goto L610;
	    }

/*           Save random number seed for error messages */

	    for (j = 1; j <= 4; ++j) {
		ioldsd[j - 1] = iseed[j];
/* L70: */
	    }

/* -----------------------------------------------------------------------   

             Time the LAPACK Routines   

             Generate A */

	    if (itype <= 8) {
		imode = kmode[itype - 1];
		iconds = kconds[itype - 1];
		if (iconds == 1) {
		    conds = 1.;
		} else {
		    conds = rtulpi;
		}
		*(unsigned char *)&adumma[0] = ' ';
		dlatme_(&n, "S", &iseed[1], &work[1], &imode, &ulpinv, &c_b23,
			 adumma, "T", "T", "T", &work[n + 1], &c__4, &conds, &
			n, &n, &c_b23, &a[1], &n, &work[(n << 1) + 1], &iinfo);
	    }

/*           Time DGEHRD for each pair NNB(j), LDAS(j) */

	    if (timsub[0]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
/* Computing MIN */
		    i__4 = n, i__5 = nnb[ipar];
		    nb = min(i__4,i__5);

/*                 If this combination of (NB,LDA) has occurred before,   
                   just use that value. */

		    lastnl = 0;
		    i__4 = ipar - 1;
		    for (j = 1; j <= i__4; ++j) {
/* Computing MIN */
			i__5 = n, i__6 = nnb[j];
			if (lda == ldas[j] && nb == min(i__5,i__6)) {
			    lastnl = j;
			}
/* L80: */
		    }

		    if (lastnl == 0) {
			xlaenv_(&c__1, &nb);
			xlaenv_(&c__2, &c__2);
			xlaenv_(&c__3, &nb);

/*                    Time DGEHRD */

			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();
L90:
			dlacpy_("Full", &n, &n, &a[1], &n, &h__[1], &lda);

			i__4 = *lwork - n;
			dgehrd_(&n, &c__1, &n, &h__[1], &lda, &work[1], &work[
				n + 1], &i__4, &iinfo);

			if (iinfo != 0) {
			    io___45.ciunit = *nout;
			    s_wsfe(&io___45);
			    do_fio(&c__1, subnam_ref(0, 1), (ftnlen)9);
			    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer))
				    ;
			    do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(
				    integer));
			    e_wsfe();
			    *info = abs(iinfo);
			    goto L610;
			}

			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L90;
			}

/*                    Subtract the time used in DLACPY. */

			s1 = dsecnd_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    dlacpy_("Full", &n, &n, &a[1], &n, &z__[1], &lda);
/* L100: */
			}
			s2 = dsecnd_();
			untime = s2 - s1;

/* Computing MAX */
			d__1 = time - untime;
			times_ref(ipar, itype, in, 1) = max(d__1,0.) / (
				doublereal) ic;
			opcnts_ref(ipar, itype, in, 1) = dopla_("DGEHRD", &n, 
				&c__1, &n, &c__0, &nb);
		    } else {
			opcnts_ref(ipar, itype, in, 1) = opcnts_ref(lastnl, 
				itype, in, 1);
			times_ref(ipar, itype, in, 1) = times_ref(lastnl, 
				itype, in, 1);
		    }
/* L110: */
		}
		ldh = lda;
	    } else {
		if (runhrd) {
		    dlacpy_("Full", &n, &n, &a[1], &n, &h__[1], &n)
			    ;

		    i__3 = *lwork - n;
		    dgehrd_(&n, &c__1, &n, &h__[1], &n, &work[1], &work[n + 1]
			    , &i__3, &iinfo);

		    if (iinfo != 0) {
			io___50.ciunit = *nout;
			s_wsfe(&io___50);
			do_fio(&c__1, subnam_ref(0, 1), (ftnlen)9);
			do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
			do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(
				integer));
			e_wsfe();
			*info = abs(iinfo);
			goto L610;
		    }
		    ldh = n;
		}
	    }

/*           Time DHSEQR with JOB='E' for each 4-tuple   
             NNB(j), NSHFTS(j), MAXBS(j), LDAS(j) */

	    if (timsub[1]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
		    nb = 1;
		    nshift = nshfts[ipar];
		    maxb = maxbs[ipar];
		    xlaenv_(&c__4, &nshift);
		    xlaenv_(&c__8, &maxb);

/*                 Time DHSEQR with JOB='E' */

		    ic = 0;
		    latime_1.ops = 0.;
		    s1 = dsecnd_();
L120:
		    dlacpy_("Full", &n, &n, &h__[1], &ldh, &a[1], &lda);

		    dhseqr_("E", "N", &n, &c__1, &n, &a[1], &lda, &w[1], &w[
			    lda + 1], &z__[1], &lda, &work[1], lwork, &iinfo);

		    if (iinfo != 0) {
			io___53.ciunit = *nout;
			s_wsfe(&io___53);
			do_fio(&c__1, subnam_ref(0, 2), (ftnlen)9);
			do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(integer));
			do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(
				integer));
			e_wsfe();
			*info = abs(iinfo);
			goto L610;
		    }

		    s2 = dsecnd_();
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			goto L120;
		    }

/*                 Subtract the time used in DLACPY. */

		    s1 = dsecnd_();
		    i__4 = ic;
		    for (j = 1; j <= i__4; ++j) {
			dlacpy_("Full", &n, &n, &h__[1], &ldh, &z__[1], &lda);
/* L130: */
		    }
		    s2 = dsecnd_();
		    untime = s2 - s1;

/* Computing MAX */
		    d__1 = time - untime;
		    times_ref(ipar, itype, in, 2) = max(d__1,0.) / (
			    doublereal) ic;
		    opcnts_ref(ipar, itype, in, 2) = latime_1.ops / (
			    doublereal) ic;
/* L140: */
		}
		ldt = 0;
		ldw = lda;
	    } else {
		if (runqre) {
		    dlacpy_("Full", &n, &n, &h__[1], &ldh, &a[1], &n);

		    dhseqr_("E", "N", &n, &c__1, &n, &a[1], &n, &w[1], &w[n + 
			    1], &z__[1], &n, &work[1], lwork, &iinfo);

		    if (iinfo != 0) {
			io___56.ciunit = *nout;
			s_wsfe(&io___56);
			do_fio(&c__1, subnam_ref(0, 2), (ftnlen)9);
			do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
			do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(
				integer));
			e_wsfe();
			*info = abs(iinfo);
			goto L610;
		    }
		    ldt = 0;
		    ldw = n;
		}
	    }

/*           Time DHSEQR with JOB='S' for each 4-tuple   
             NNB(j), NSHFTS(j), MAXBS(j), LDAS(j) */

	    if (timsub[2]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
		    nshift = nshfts[ipar];
		    maxb = maxbs[ipar];
		    nb = 1;
		    xlaenv_(&c__4, &nshift);
		    xlaenv_(&c__8, &maxb);

/*                 Time DHSEQR with JOB='S' */

		    ic = 0;
		    latime_1.ops = 0.;
		    s1 = dsecnd_();
L150:
		    dlacpy_("Full", &n, &n, &h__[1], &ldh, &a[1], &lda);

		    dhseqr_("S", "N", &n, &c__1, &n, &a[1], &lda, &w[1], &w[
			    lda + 1], &z__[1], &lda, &work[1], lwork, &iinfo);

		    if (iinfo != 0) {
			io___57.ciunit = *nout;
			s_wsfe(&io___57);
			do_fio(&c__1, subnam_ref(0, 3), (ftnlen)9);
			do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(integer));
			do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(
				integer));
			e_wsfe();
			*info = abs(iinfo);
			goto L610;
		    }

		    s2 = dsecnd_();
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			goto L150;
		    }

/*                 Subtract the time used in DLACPY. */

		    s1 = dsecnd_();
		    i__4 = ic;
		    for (j = 1; j <= i__4; ++j) {
			dlacpy_("Full", &n, &n, &h__[1], &ldh, &z__[1], &lda);
/* L160: */
		    }
		    s2 = dsecnd_();
		    untime = s2 - s1;

/* Computing MAX */
		    d__1 = time - untime;
		    times_ref(ipar, itype, in, 3) = max(d__1,0.) / (
			    doublereal) ic;
		    opcnts_ref(ipar, itype, in, 3) = latime_1.ops / (
			    doublereal) ic;
/* L170: */
		}
		ldt = lda;
		ldw = lda;
	    } else {
		if (runqrs) {
		    dlacpy_("Full", &n, &n, &h__[1], &ldh, &a[1], &n);

		    dhseqr_("S", "N", &n, &c__1, &n, &a[1], &n, &w[1], &w[n + 
			    1], &z__[1], &n, &work[1], lwork, &iinfo);

		    if (iinfo != 0) {
			io___58.ciunit = *nout;
			s_wsfe(&io___58);
			do_fio(&c__1, subnam_ref(0, 3), (ftnlen)9);
			do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
			do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(
				integer));
			e_wsfe();
			*info = abs(iinfo);
			goto L610;
		    }
		    ldt = n;
		    ldw = n;
		}
	    }

/*           Time DHSEQR with JOB='I' for each 4-tuple   
             NNB(j), NSHFTS(j), MAXBS(j), LDAS(j) */

	    if (timsub[3]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
		    nshift = nshfts[ipar];
		    maxb = maxbs[ipar];
		    nb = 1;
		    xlaenv_(&c__4, &nshift);
		    xlaenv_(&c__8, &maxb);

/*                 Time DHSEQR with JOB='I' */

		    ic = 0;
		    latime_1.ops = 0.;
		    s1 = dsecnd_();
L180:
		    dlacpy_("Full", &n, &n, &h__[1], &ldh, &a[1], &lda);

		    dhseqr_("S", "I", &n, &c__1, &n, &a[1], &lda, &w[1], &w[
			    lda + 1], &z__[1], &lda, &work[1], lwork, &iinfo);

		    if (iinfo != 0) {
			io___59.ciunit = *nout;
			s_wsfe(&io___59);
			do_fio(&c__1, subnam_ref(0, 4), (ftnlen)9);
			do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(integer));
			do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(
				integer));
			e_wsfe();
			*info = abs(iinfo);
			goto L610;
		    }

		    s2 = dsecnd_();
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			goto L180;
		    }

/*                 Subtract the time used in DLACPY. */

		    s1 = dsecnd_();
		    i__4 = ic;
		    for (j = 1; j <= i__4; ++j) {
			dlacpy_("Full", &n, &n, &h__[1], &ldh, &z__[1], &lda);
/* L190: */
		    }
		    s2 = dsecnd_();
		    untime = s2 - s1;

/* Computing MAX */
		    d__1 = time - untime;
		    times_ref(ipar, itype, in, 4) = max(d__1,0.) / (
			    doublereal) ic;
		    opcnts_ref(ipar, itype, in, 4) = latime_1.ops / (
			    doublereal) ic;
/* L200: */
		}
		ldt = lda;
		ldw = lda;
	    }

/*           Time DTREVC and DHSEIN with various values of LDA   

             Select All Eigenvectors */

	    i__3 = n;
	    for (j = 1; j <= i__3; ++j) {
		llwork[j] = TRUE_;
/* L210: */
	    }

	    i__3 = *nparms;
	    for (ipar = 1; ipar <= i__3; ++ipar) {
		lda = ldas[ipar];

/*              If this value of LDA has come up before, just use   
                the value previously computed. */

		lastl = 0;
		i__4 = ipar - 1;
		for (j = 1; j <= i__4; ++j) {
		    if (lda == ldas[j]) {
			lastl = j;
		    }
/* L220: */
		}

/*              Time DTREVC */

		if ((timsub[4] || timsub[5]) && lastl == 0) {

/*                 Copy T (which is in A) if necessary to get right LDA. */

		    if (lda > ldt) {
			for (jc = n; jc >= 1; --jc) {
			    for (jr = n; jr >= 1; --jr) {
				a[jr + (jc - 1) * lda] = a[jr + (jc - 1) * 
					ldt];
/* L230: */
			    }
/* L240: */
			}
		    } else if (lda < ldt) {
			i__4 = n;
			for (jc = 1; jc <= i__4; ++jc) {
			    i__5 = n;
			    for (jr = 1; jr <= i__5; ++jr) {
				a[jr + (jc - 1) * lda] = a[jr + (jc - 1) * 
					ldt];
/* L250: */
			    }
/* L260: */
			}
		    }
		    ldt = lda;

/*                 Time DTREVC for Left Eigenvectors */

		    if (timsub[4]) {
			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();
L270:

			dtrevc_("L", "A", &llwork[1], &n, &a[1], &lda, &z__[1]
				, &lda, &z__[1], &lda, &n, &itemp, &work[1], &
				iinfo);

			if (iinfo != 0) {
			    io___64.ciunit = *nout;
			    s_wsfe(&io___64);
			    do_fio(&c__1, subnam_ref(0, 5), (ftnlen)9);
			    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer))
				    ;
			    do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(
				    integer));
			    e_wsfe();
			    *info = abs(iinfo);
			    goto L610;
			}
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L270;
			}

			times_ref(ipar, itype, in, 5) = time / (doublereal) 
				ic;
			opcnts_ref(ipar, itype, in, 5) = latime_1.ops / (
				doublereal) ic;
		    }

/*                 Time DTREVC for Right Eigenvectors */

		    if (timsub[5]) {
			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();
L280:
			dtrevc_("R", "A", &llwork[1], &n, &a[1], &lda, &z__[1]
				, &lda, &z__[1], &lda, &n, &itemp, &work[1], &
				iinfo);

			if (iinfo != 0) {
			    io___65.ciunit = *nout;
			    s_wsfe(&io___65);
			    do_fio(&c__1, subnam_ref(0, 6), (ftnlen)9);
			    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer))
				    ;
			    do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(
				    integer));
			    e_wsfe();
			    *info = abs(iinfo);
			    goto L610;
			}
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L280;
			}

			times_ref(ipar, itype, in, 6) = time / (doublereal) 
				ic;
			opcnts_ref(ipar, itype, in, 6) = latime_1.ops / (
				doublereal) ic;
		    }
		} else {
		    if (timsub[4]) {
			opcnts_ref(ipar, itype, in, 5) = opcnts_ref(lastl, 
				itype, in, 5);
			times_ref(ipar, itype, in, 5) = times_ref(lastl, 
				itype, in, 5);
		    }
		    if (timsub[5]) {
			opcnts_ref(ipar, itype, in, 6) = opcnts_ref(lastl, 
				itype, in, 6);
			times_ref(ipar, itype, in, 6) = times_ref(lastl, 
				itype, in, 6);
		    }
		}

/*              Time DHSEIN */

		if ((timsub[6] || timsub[7]) && lastl == 0) {

/*                 Copy H if necessary to get right LDA. */

		    if (lda > ldh) {
			for (jc = n; jc >= 1; --jc) {
			    for (jr = n; jr >= 1; --jr) {
				h__[jr + (jc - 1) * lda] = h__[jr + (jc - 1) *
					 ldh];
/* L290: */
			    }
			    w[jc + lda] = w[jc + ldh];
/* L300: */
			}
		    } else if (lda < ldh) {
			i__4 = n;
			for (jc = 1; jc <= i__4; ++jc) {
			    i__5 = n;
			    for (jr = 1; jr <= i__5; ++jr) {
				h__[jr + (jc - 1) * lda] = h__[jr + (jc - 1) *
					 ldh];
/* L310: */
			    }
			    w[jc + lda] = w[jc + ldh];
/* L320: */
			}
		    }
		    ldh = lda;

/*                 Copy W if necessary to get right LDA. */

		    if (lda > ldw) {
			for (j = n; j >= 1; --j) {
			    w[j + lda] = w[j + ldw];
/* L330: */
			}
		    } else if (lda < ldw) {
			i__4 = n;
			for (j = 1; j <= i__4; ++j) {
			    w[j + lda] = w[j + ldw];
/* L340: */
			}
		    }
		    ldw = lda;

/*                 Time DHSEIN for Left Eigenvectors */

		    if (timsub[6]) {
			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();
L350:

			dhsein_("L", "Q", "N", &llwork[1], &n, &h__[1], &lda, 
				&w[1], &w[lda + 1], &z__[1], &lda, &z__[1], &
				lda, &n, &itemp, &work[1], &iwork[1], &iwork[
				n + 1], &iinfo);

			if (iinfo != 0) {
			    io___66.ciunit = *nout;
			    s_wsfe(&io___66);
			    do_fio(&c__1, subnam_ref(0, 7), (ftnlen)9);
			    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer))
				    ;
			    do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(
				    integer));
			    e_wsfe();
			    *info = abs(iinfo);
			    goto L610;
			}
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L350;
			}

			times_ref(ipar, itype, in, 7) = time / (doublereal) 
				ic;
			opcnts_ref(ipar, itype, in, 7) = latime_1.ops / (
				doublereal) ic;
		    }

/*                 Time DHSEIN for Right Eigenvectors */

		    if (timsub[7]) {
			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();
L360:

			dhsein_("R", "Q", "N", &llwork[1], &n, &h__[1], &lda, 
				&w[1], &w[lda + 1], &z__[1], &lda, &z__[1], &
				lda, &n, &itemp, &work[1], &iwork[1], &iwork[
				n + 1], &iinfo);

			if (iinfo != 0) {
			    io___67.ciunit = *nout;
			    s_wsfe(&io___67);
			    do_fio(&c__1, subnam_ref(0, 8), (ftnlen)9);
			    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer))
				    ;
			    do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(
				    integer));
			    e_wsfe();
			    *info = abs(iinfo);
			    goto L610;
			}
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L360;
			}

			times_ref(ipar, itype, in, 8) = time / (doublereal) 
				ic;
			opcnts_ref(ipar, itype, in, 8) = latime_1.ops / (
				doublereal) ic;
		    }
		} else {
		    if (timsub[6]) {
			opcnts_ref(ipar, itype, in, 7) = opcnts_ref(lastl, 
				itype, in, 7);
			times_ref(ipar, itype, in, 7) = times_ref(lastl, 
				itype, in, 7);
		    }
		    if (timsub[7]) {
			opcnts_ref(ipar, itype, in, 8) = opcnts_ref(lastl, 
				itype, in, 8);
			times_ref(ipar, itype, in, 8) = times_ref(lastl, 
				itype, in, 8);
		    }
		}
/* L370: */
	    }

/* -----------------------------------------------------------------------   

             Time the EISPACK Routines   

             Restore random number seed */

	    for (j = 1; j <= 4; ++j) {
		iseed[j] = ioldsd[j - 1];
/* L380: */
	    }

/*           Re-generate A */

	    if (itype <= 8) {
		imode = kmode[itype - 1];
		if (iconds == 1) {
		    conds = 1.;
		} else {
		    conds = rtulpi;
		}
		dlatme_(&n, "S", &iseed[1], &work[1], &imode, &ulpinv, &c_b23,
			 adumma, "T", "T", "T", &work[n + 1], &c__4, &conds, &
			n, &n, &c_b23, &a[1], &n, &work[(n << 1) + 1], &iinfo);
	    }

/*           Time ORTHES for each LDAS(j) */

	    if (timsub[8]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];

/*                 If this value of LDA has come up before, just use   
                   the value previously computed. */

		    lastl = 0;
		    i__4 = ipar - 1;
		    for (j = 1; j <= i__4; ++j) {
			if (lda == ldas[j]) {
			    lastl = j;
			}
/* L390: */
		    }

		    if (lastl == 0) {

/*                    Time ORTHES */

			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();

L400:
			dlacpy_("Full", &n, &n, &a[1], &n, &h__[1], &lda);

			orthes_(&lda, &n, &c__1, &n, &h__[1], &work[1]);

			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L400;
			}

/*                    Subtract the time used in DLACPY. */

			s1 = dsecnd_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    dlacpy_("Full", &n, &n, &a[1], &n, &z__[1], &lda);
/* L410: */
			}
			s2 = dsecnd_();
			untime = s2 - s1;

/*                     OPS1 = ( 20*N**3 - 3*N**2 - 23*N ) / 6 - 17   

   Computing MAX */
			d__1 = time - untime;
			times_ref(ipar, itype, in, 9) = max(d__1,0.) / (
				doublereal) ic;
			opcnts_ref(ipar, itype, in, 9) = latime_1.ops / (
				doublereal) ic;
		    } else {
			opcnts_ref(ipar, itype, in, 9) = opcnts_ref(lastl, 
				itype, in, 9);
			times_ref(ipar, itype, in, 9) = times_ref(lastl, 
				itype, in, 9);
		    }
		    ldh = lda;
/* L420: */
		}
	    } else {
		if (runort) {
		    dlacpy_("Full", &n, &n, &a[1], &n, &h__[1], &n)
			    ;

		    orthes_(&n, &n, &c__1, &n, &h__[1], &work[1]);

		    ldh = n;
		}
	    }

/*           Time HQR for each LDAS(j) */

	    if (timsub[9]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];

/*                 If this value of LDA has come up before, just use   
                   the value previously computed. */

		    lastl = 0;
		    i__4 = ipar - 1;
		    for (j = 1; j <= i__4; ++j) {
			if (lda == ldas[j]) {
			    lastl = j;
			}
/* L430: */
		    }

		    if (lastl == 0) {

/*                    Time HQR */

			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();
L440:
			dlacpy_("Full", &n, &n, &h__[1], &ldh, &a[1], &lda);

			hqr_(&lda, &n, &c__1, &n, &a[1], &w[1], &w[lda + 1], &
				iinfo);

			if (iinfo != 0) {
			    io___68.ciunit = *nout;
			    s_wsfe(&io___68);
			    do_fio(&c__1, subnam_ref(0, 10), (ftnlen)9);
			    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer))
				    ;
			    do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(
				    integer));
			    e_wsfe();
			    *info = abs(iinfo);
			    goto L610;
			}
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L440;
			}

/*                    Subtract the time used in DLACPY. */

			s1 = dsecnd_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    dlacpy_("Full", &n, &n, &h__[1], &ldh, &z__[1], &
				    lda);
/* L450: */
			}
			s2 = dsecnd_();
			untime = s2 - s1;

/* Computing MAX */
			d__1 = time - untime;
			times_ref(ipar, itype, in, 10) = max(d__1,0.) / (
				doublereal) ic;
			opcnts_ref(ipar, itype, in, 10) = latime_1.ops / (
				doublereal) ic;
		    } else {
			opcnts_ref(ipar, itype, in, 10) = opcnts_ref(lastl, 
				itype, in, 10);
			times_ref(ipar, itype, in, 10) = times_ref(lastl, 
				itype, in, 10);
		    }
		    ldw = lda;
/* L460: */
		}
	    } else {
		if (runhqr) {
		    dlacpy_("Full", &n, &n, &a[1], &n, &h__[1], &n)
			    ;

		    hqr_(&n, &n, &c__1, &n, &a[1], &w[1], &w[n + 1], &iinfo);

		    ldw = n;
		}
	    }

/*           Time HQR2 for each LDAS(j) */

	    if (timsub[10]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];

/*                 If this value of LDA has come up before, just use   
                   the value previously computed. */

		    lastl = 0;
		    i__4 = ipar - 1;
		    for (j = 1; j <= i__4; ++j) {
			if (lda == ldas[j]) {
			    lastl = j;
			}
/* L470: */
		    }

		    if (lastl == 0) {

/*                    Time HQR2 */

			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();
L480:
			dlacpy_("Full", &n, &n, &h__[1], &ldh, &a[1], &lda);
			dlaset_("Full", &n, &n, &c_b218, &c_b23, &z__[1], &
				lda);

			hqr2_(&lda, &n, &c__1, &n, &a[1], &w[1], &w[lda + 1], 
				&z__[1], &iinfo);

			if (iinfo != 0) {
			    io___69.ciunit = *nout;
			    s_wsfe(&io___69);
			    do_fio(&c__1, subnam_ref(0, 11), (ftnlen)9);
			    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer))
				    ;
			    do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(
				    integer));
			    e_wsfe();
			    *info = abs(iinfo);
			    goto L610;
			}
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L480;
			}

/*                    Subtract the time used in DLACPY. */

			s1 = dsecnd_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    dlacpy_("Full", &n, &n, &h__[1], &ldh, &z__[1], &
				    lda);
/* L490: */
			}
			s2 = dsecnd_();
			untime = s2 - s1;

/* Computing MAX */
			d__1 = time - untime;
			times_ref(ipar, itype, in, 11) = max(d__1,0.) / (
				doublereal) ic;
			opcnts_ref(ipar, itype, in, 11) = latime_1.ops / (
				doublereal) ic;
		    } else {
			opcnts_ref(ipar, itype, in, 11) = opcnts_ref(lastl, 
				itype, in, 11);
			times_ref(ipar, itype, in, 11) = times_ref(lastl, 
				itype, in, 11);
		    }
		    ldw = lda;
/* L500: */
		}
	    }

/*           Time INVIT for each LDAS(j)   

             Select All Eigenvectors */

	    i__3 = n;
	    for (j = 1; j <= i__3; ++j) {
		llwork[j] = TRUE_;
/* L510: */
	    }

	    if (timsub[11]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];

/*                 If this value of LDA has come up before, just use   
                   the value previously computed. */

		    lastl = 0;
		    i__4 = ipar - 1;
		    for (j = 1; j <= i__4; ++j) {
			if (lda == ldas[j]) {
			    lastl = j;
			}
/* L520: */
		    }

		    if (lastl == 0) {

/*                    Copy H if necessary to get right LDA. */

			if (lda > ldh) {
			    for (jc = n; jc >= 1; --jc) {
				for (jr = n; jr >= 1; --jr) {
				    h__[jr + (jc - 1) * lda] = h__[jr + (jc - 
					    1) * ldh];
/* L530: */
				}
/* L540: */
			    }
			} else if (lda < ldh) {
			    i__4 = n;
			    for (jc = 1; jc <= i__4; ++jc) {
				i__5 = n;
				for (jr = 1; jr <= i__5; ++jr) {
				    h__[jr + (jc - 1) * lda] = h__[jr + (jc - 
					    1) * ldh];
/* L550: */
				}
/* L560: */
			    }
			}
			ldh = lda;

/*                    Copy W if necessary to get right LDA. */

			if (lda > ldw) {
			    for (j = n; j >= 1; --j) {
				w[j + lda] = w[j + ldw];
/* L570: */
			    }
			} else if (lda < ldw) {
			    i__4 = n;
			    for (j = 1; j <= i__4; ++j) {
				w[j + lda] = w[j + ldw];
/* L580: */
			    }
			}
			ldw = lda;

/*                    Time INVIT for right eigenvectors. */

			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();
L590:

			invit_(&lda, &n, &h__[1], &w[1], &w[lda + 1], &llwork[
				1], &n, &itemp, &z__[1], &iinfo, &work[(n << 
				1) + 1], &work[1], &work[n + 1]);

			if (iinfo != 0) {
			    io___70.ciunit = *nout;
			    s_wsfe(&io___70);
			    do_fio(&c__1, subnam_ref(0, 12), (ftnlen)9);
			    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer))
				    ;
			    do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(
				    integer));
			    e_wsfe();
			    *info = abs(iinfo);
			    goto L610;
			}
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L590;
			}

/*                    TIME = TIME / DOUBLE PRECISION( IC )   
                      OPS1 = OPS / DOUBLE PRECISION( IC )   
                      OPCNTS( IPAR, ITYPE, IN, 12 ) = OPS1   
                      TIMES( IPAR, ITYPE, IN, 12 ) = DMFLOP( OPS1, TIME,   
       $                  IINFO ) */

			times_ref(ipar, itype, in, 12) = time / (doublereal) 
				ic;
			opcnts_ref(ipar, itype, in, 12) = latime_1.ops / (
				doublereal) ic;
		    } else {
			opcnts_ref(ipar, itype, in, 12) = opcnts_ref(lastl, 
				itype, in, 12);
			times_ref(ipar, itype, in, 12) = times_ref(lastl, 
				itype, in, 12);
		    }
/* L600: */
		}
	    }

L610:
	    ;
	}
/* L620: */
    }

/* -----------------------------------------------------------------------   

       Print a table of results for each timed routine. */

    isub = 1;
    if (timsub[isub - 1]) {
	dprtbe_(subnam_ref(0, isub), &mtypes, &dotype[1], nsizes, &nn[1], &
		inparm[isub - 1], pnames, nparms, &ldas[1], &nnb[1], &nshfts[
		1], &maxbs[1], &opcnts_ref(1, 1, 1, isub), ldo1, ldo2, &
		times_ref(1, 1, 1, isub), ldt1, ldt2, &work[1], &llwork[1], 
		nout, (ftnlen)9, (ftnlen)4);
    }

    i__1 = *nparms;
    for (in = 1; in <= i__1; ++in) {
	nnb[in] = 1;
/* L630: */
    }

    for (isub = 2; isub <= 12; ++isub) {
	if (timsub[isub - 1]) {
	    dprtbe_(subnam_ref(0, isub), &mtypes, &dotype[1], nsizes, &nn[1], 
		    &inparm[isub - 1], pnames, nparms, &ldas[1], &nnb[1], &
		    nshfts[1], &maxbs[1], &opcnts_ref(1, 1, 1, isub), ldo1, 
		    ldo2, &times_ref(1, 1, 1, isub), ldt1, ldt2, &work[1], &
		    llwork[1], nout, (ftnlen)9, (ftnlen)4);
	}
/* L640: */
    }

    return 0;

/*     End of DTIM21 */


} /* dtim21_ */

#undef opcnts_ref
#undef subnam_ref
#undef times_ref


