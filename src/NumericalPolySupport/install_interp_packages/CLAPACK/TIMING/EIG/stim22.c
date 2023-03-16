#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    real ops, itcnt;
} latime_;

#define latime_1 latime_

/* Table of constant values */

static integer c__23 = 23;
static integer c__10 = 10;
static integer c__0 = 0;
static integer c__11 = 11;
static integer c__1 = 1;
static integer c__9 = 9;
static integer c__25 = 25;
static integer c__2 = 2;
static real c_b43 = 1.f;
static integer c__3 = 3;
static integer c__4 = 4;
static real c_b142 = 2.f;

/* Subroutine */ int stim22_(char *line, integer *nsizes, integer *nn, 
	integer *ntypes, logical *dotype, integer *nparms, integer *nnb, 
	integer *ldas, real *timmin, integer *nout, integer *iseed, real *a, 
	real *d__, real *e, real *e2, real *z__, real *z1, real *work, 
	integer *lwork, logical *llwork, integer *iwork, real *times, integer 
	*ldt1, integer *ldt2, integer *ldt3, real *opcnts, integer *ldo1, 
	integer *ldo2, integer *ldo3, integer *info, ftnlen line_len)
{
    /* Initialized data */

    static char subnam[9*23] = "SSYTRD   " "SORGTR   " "SORMTR   " "SSTEQR(N)"
	     "SSTEQR(V)" "SSTERF   " "SPTEQR(N)" "SPTEQR(V)" "SSTEBZ(I)" 
	    "SSTEBZ(V)" "SSTEIN   " "SSTEDC(N)" "SSTEDC(I)" "SSTEDC(V)" "SST"
	    "EGR(N)" "SSTEGR(V)" "TRED1    " "IMTQL1   " "IMTQL2   " "TQLRAT "
	    "  " "TRIDIB   " "BISECT   " "TINVIT   ";
    static integer inparm[23] = { 2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1 };
    static char pnames[4*4] = "LDA " "NB  " "bad1" "bad2";
    static integer kmode[4] = { 4,3,1,5 };

    /* Format strings */
    static char fmt_9999[] = "(1x,a,\002 timing run not attempted -- N > LD"
	    "A\002,/)";
    static char fmt_9998[] = "(1x,a,\002 timing run not attempted -- LWORK t"
	    "oo small.\002,/)";
    static char fmt_9997[] = "(\002 STIM22: \002,a,\002 returned INFO=\002,i"
	    "6,\002.\002,/9x,\002N=\002,i6,\002, ITYPE=\002,i6,\002, IPAR="
	    "\002,i6,\002, ISEED=(\002,3(i5,\002,\002),i5,\002)\002)";

    /* System generated locals */
    integer opcnts_dim1, opcnts_dim2, opcnts_dim3, opcnts_offset, times_dim1, 
	    times_dim2, times_dim3, times_offset, i__1, i__2, i__3, i__4, 
	    i__5;
    real r__1, r__2, r__3, r__4;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);
    double log(doublereal);
    integer pow_ii(integer *, integer *);

    /* Local variables */
    static integer ipar;
    static real time;
    static integer isub;
    static char uplo[1];
    extern /* Subroutine */ int tred1_(integer *, integer *, real *, real *, 
	    real *, real *);
    static integer i__, j, m, n, imode, lwedc, iinfo;
    extern doublereal sopla_(char *, integer *, integer *, integer *, integer 
	    *, integer *);
    static integer lastl, itype, j1, j2, j3, j4, lwevr;
    extern /* Subroutine */ int scopy_(integer *, real *, integer *, real *, 
	    integer *);
    static real s1, s2;
    extern doublereal sopla2_(char *, char *, integer *, integer *, integer *,
	     integer *, integer *);
    extern /* Subroutine */ int imtql1_(integer *, real *, real *, integer *),
	     imtql2_(integer *, integer *, real *, real *, real *, integer *);
    static integer ic, m11, nb, il, in, mm;
    static logical runtr1;
    static integer iu;
    static real vl;
    static integer liwedc;
    extern doublereal slamch_(char *);
    extern /* Subroutine */ int bisect_(integer *, real *, real *, real *, 
	    real *, real *, real *, integer *, integer *, real *, integer *, 
	    integer *, real *, real *);
    static real vu;
    static integer idumma[1];
    extern doublereal second_(void);
    static integer ioldsd[4];
    extern integer ilaenv_(integer *, char *, char *, integer *, integer *, 
	    integer *, integer *, ftnlen, ftnlen);
    extern /* Subroutine */ int tridib_(integer *, real *, real *, real *, 
	    real *, real *, real *, integer *, integer *, real *, integer *, 
	    integer *, real *, real *), atimin_(char *, char *, integer *, 
	    char *, logical *, integer *, integer *, ftnlen, ftnlen, ftnlen);
    static real abstol;
    extern /* Subroutine */ int sstedc_(char *, integer *, real *, real *, 
	    real *, integer *, real *, integer *, integer *, integer *, 
	    integer *);
    static integer infsok, nansok;
    extern /* Subroutine */ int slacpy_(char *, integer *, integer *, real *, 
	    integer *, real *, integer *), slaset_(char *, integer *, 
	    integer *, real *, real *, real *, integer *), xlaenv_(
	    integer *, integer *), sprtbe_(char *, integer *, logical *, 
	    integer *, integer *, integer *, char *, integer *, integer *, 
	    integer *, integer *, integer *, real *, integer *, integer *, 
	    real *, integer *, integer *, real *, logical *, integer *, 
	    ftnlen, ftnlen);
    static real untime;
    static logical timsub[23];
    extern /* Subroutine */ int slatms_(integer *, integer *, char *, integer 
	    *, char *, real *, integer *, real *, real *, integer *, integer *
	    , char *, real *, integer *, real *, integer *);
    static integer ilwork, liwevr, nsplit;
    extern /* Subroutine */ int sstebz_(char *, char *, integer *, real *, 
	    real *, integer *, integer *, real *, real *, real *, integer *, 
	    integer *, real *, integer *, integer *, real *, integer *, 
	    integer *);
    static real ulpinv;
    static logical runtrd;
    extern /* Subroutine */ int spteqr_(char *, integer *, real *, real *, 
	    real *, integer *, real *, integer *), sorgtr_(char *, 
	    integer *, real *, integer *, real *, real *, integer *, integer *
	    );
    static integer mtypes;
    extern /* Subroutine */ int sstegr_(char *, char *, integer *, real *, 
	    real *, real *, real *, integer *, integer *, real *, integer *, 
	    real *, real *, integer *, integer *, real *, integer *, integer *
	    , integer *, integer *), sstein_(integer *, real *
	    , real *, integer *, real *, integer *, integer *, real *, 
	    integer *, real *, integer *, integer *, integer *), sormtr_(char 
	    *, char *, char *, integer *, integer *, real *, integer *, real *
	    , real *, integer *, real *, integer *, integer *), ssteqr_(char *, integer *, real *, real *, real *, 
	    integer *, real *, integer *), ssterf_(integer *, real *, 
	    real *, integer *), ssytrd_(char *, integer *, real *, integer *, 
	    real *, real *, real *, real *, integer *, integer *), 
	    tinvit_(integer *, integer *, real *, real *, real *, integer *, 
	    real *, integer *, real *, integer *, real *, real *, real *, 
	    real *, real *), tqlrat_(integer *, real *, real *, integer *);
    static integer lda;
    static real rlb;
    static integer lgn, mmm;
    static real rub, ulp, eps1;

    /* Fortran I/O blocks */
    static cilist io___10 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___12 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___38 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___42 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___43 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___44 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___46 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___47 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___48 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___50 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___51 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___59 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___60 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___61 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___62 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___63 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___64 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___66 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___67 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___68 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___69 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___70 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___75 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___77 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___78 = { 0, 0, 0, fmt_9997, 0 };



#define times_ref(a_1,a_2,a_3,a_4) times[(((a_4)*times_dim3 + (a_3))*\
times_dim2 + (a_2))*times_dim1 + a_1]
#define subnam_ref(a_0,a_1) &subnam[(a_1)*9 + a_0 - 9]
#define opcnts_ref(a_1,a_2,a_3,a_4) opcnts[(((a_4)*opcnts_dim3 + (a_3))*\
opcnts_dim2 + (a_2))*opcnts_dim1 + a_1]


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 20, 2000   


    Purpose   
    =======   

       STIM22 times the LAPACK routines for the real symmetric   
       eigenvalue problem.   

       For each N value in NN(1:NSIZES) and .TRUE. value in   
       DOTYPE(1:NTYPES), a matrix will be generated and used to test the   
       selected routines.  Thus, NSIZES*(number of .TRUE. values in   
       DOTYPE) matrices will be generated.   

    Arguments   
    =========   

    LINE    (input) CHARACTER*80   
            On entry, LINE contains the input line which requested   
            this routine.  This line may contain a subroutine name,   
            such as SSYTRD, indicating that only routine SSYTRD will   
            be timed, or it may contain a generic name, such as SST.   
            In this case, the rest of the line is scanned for the   
            first 23 non-blank characters, corresponding to the eight   
            combinations of subroutine and options:   
            LAPACK:   
            1: SSYTRD   
            2: SORGTR   
            3: SORMTR   
            4: SSTEQR(VECT='N')   
            5: SSTEQR(VECT='V')   
            6: SSTERF   
            7: SPTEQR(VECT='N')   
            8: SPTEQR(VECT='V')   
            9: SSTEBZ(RANGE='I')   
            10: SSTEBZ(RANGE='V')   
            11: SSTEIN   
            12: SSTEDC(COMPQ='N')   
            13: SSTEDC(COMPQ='I')   
            14: SSTEDC(COMPQ='V')   
            15: SSTEGR(COMPQ='N')   
            16: SSTEGR(COMPQ='V')   
            EISPACK:   
            17: TRED1  (compare with SSYTRD)   
            18: IMTQL1 (compare w/ SSTEQR -- VECT='N')   
            19: IMTQL2 (compare w/ SSTEQR -- VECT='V')   
            20: TQLRAT (compare with SSTERF)   
            21: TRIDIB (compare with SSTEBZ -- RANGE='I')   
            22: BISECT (compare with SSTEBZ -- RANGE='V')   
            23: TINVIT (compare with SSTEIN)   
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
            generated.  The matrix A has the form X**(-1) D X, where   
            X is orthogonal and D is diagonal with:   
            (j=1)  evenly spaced entries 1, ..., ULP with random signs.   
            (j=2)  geometrically spaced entries 1, ..., ULP with random   
                   signs.   
            (j=3)  "clustered" entries 1, ULP,..., ULP with random   
                   signs.   
            (j=4)  entries randomly chosen from ( ULP, 1 ).   

    NPARMS  (input) INTEGER   
            The number of values in each of the arrays NNB and LDAS.   
            For each matrix A generated according to NN and DOTYPE,   
            tests will be run with (NB,LDA)=   
            (NNB(1),LDAS(1)),...,(NNB(NPARMS), LDAS(NPARMS))   

    NNB     (input) INTEGER array, dimension( NPARMS )   
            The values of the blocksize ("NB") to be tested.   

    LDAS    (input) INTEGER array, dimension( NPARMS )   
            The values of LDA, the leading dimension of all matrices,   
            to be tested.   

    TIMMIN  (input) REAL   
            The minimum time a subroutine will be timed.   

    NOUT    (input) INTEGER   
            If NOUT > 0 then NOUT specifies the unit number   
            on which the output will be printed.  If NOUT <= 0, no   
            output is printed.   

    ISEED   (input/output) INTEGER array, dimension( 4 )   
            The random seed used by the random number generator, used   
            by the test matrix generator.  It is used and updated on   
            each call to STIM22   

    A       (workspace) REAL array,   
                        dimension( max(NN)*max(LDAS) )   
            The original matrix to be tested.   

    D       (workspace) REAL array,   
                        dimension( max(NN) )   
            The diagonal of the tridiagonal generated by SSYTRD/TRED1.   

    E       (workspace) REAL array,   
                        dimension( max(NN) )   
            The off-diagonal of the tridiagonal generated by   
            SSYTRD/TRED1.   

    E2      (workspace) REAL array,   
                        dimension( max(NN) )   
            The square of the off-diagonal of the tridiagonal generated   
            by TRED1.  (Used by TQLRAT.)   

    Z       (workspace) REAL array,   
                        dimension( max(NN)*max(LDAS) )   
            Various output arrays.   

    WORK    (workspace) REAL array, dimension( LWORK )   

    LWORK   (input) INTEGER   
            Number of elements in WORK.  It must be at least   
            (a)  max( (NNB + 2 )*LDAS )   
            (b)  max( 5*LDAS )   
            (c)  NSIZES*NTYPES*NPARMS   
            (d)  2*LDAS + 1 + 3*maxNN + 2*maxNN*log2(maxNN) + 3*maxNN**2   
                 where maxNN = maximum matrix dimension in NN   
                       log2(x) = smallest integer power of 2 .ge. x   

    LLWORK  (workspace) LOGICAL array of dimension( NPARMS ),   

    IWORK   (workspace) INTEGER array of dimension   
            6 + 6*maxNN + 5*maxNN*log2(maxNN)   

    TIMES   (output) REAL array,   
                     dimension (LDT1,LDT2,LDT3,NSUBS)   
            TIMES(i,j,k,l) will be set to the run time (in seconds) for   
            subroutine l, with N=NN(k), matrix type j, and LDA=LDAS(i),   
            NBLOCK=NNB(i).   

    LDT1    (input) INTEGER   
            The first dimension of TIMES.  LDT1 >= min( 1, NPARMS ).   

    LDT2    (input) INTEGER   
            The second dimension of TIMES.  LDT2 >= min( 1, NTYPES ).   

    LDT3    (input) INTEGER   
            The third dimension of TIMES.  LDT3 >= min( 1, NSIZES ).   

    OPCNTS  (output) REAL array,   
                     dimension (LDO1,LDO2,LDO3,NSUBS)   
            OPCNTS(i,j,k,l) will be set to the number of floating-point   
            operations executed by subroutine l, with N=NN(k), matrix   
            type j, and LDA=LDAS(i), NBLOCK=NNB(i).   

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
    --ldas;
    --iseed;
    --a;
    --d__;
    --e;
    --e2;
    --z__;
    --z1;
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


       Extract the timing request from the input line. */

    atimin_("SST", line, &c__23, subnam, timsub, nout, info, (ftnlen)3, (
	    ftnlen)80, (ftnlen)9);

/*     Disable timing of SSTEGR if we're non-IEEE-754 compliant. */

    nansok = ilaenv_(&c__10, "SSTEGR", " ", &c__0, &c__0, &c__0, &c__0, (
	    ftnlen)6, (ftnlen)1);
    infsok = ilaenv_(&c__11, "SSTEGR", " ", &c__0, &c__0, &c__0, &c__0, (
	    ftnlen)6, (ftnlen)1);
    if (nansok != 1 || infsok != 1) {
	timsub[14] = FALSE_;
	timsub[15] = FALSE_;
    }

    if (*info != 0) {
	return 0;
    }

/*     Check that N <= LDA for the input values. */

    i__1 = *nsizes;
    for (j2 = 1; j2 <= i__1; ++j2) {
	i__2 = *nparms;
	for (j1 = 1; j1 <= i__2; ++j1) {
	    if (nn[j2] > ldas[j1]) {
		*info = -8;
		io___10.ciunit = *nout;
		s_wsfe(&io___10);
		do_fio(&c__1, line, (ftnlen)6);
		e_wsfe();
		return 0;
	    }
/* L10: */
	}
/* L20: */
    }

/*     Check LWORK */

    ilwork = *nsizes * *nparms * *ntypes;
    i__1 = *nparms;
    for (j1 = 1; j1 <= i__1; ++j1) {
/* Computing MAX */
	i__2 = ilwork, i__3 = ldas[j1] * 5, i__2 = max(i__2,i__3), i__3 = (
		nnb[j1] + 2) * ldas[j1];
	ilwork = max(i__2,i__3);
/* L30: */
    }
    if (ilwork > *lwork) {
	*info = -18;
	io___12.ciunit = *nout;
	s_wsfe(&io___12);
	do_fio(&c__1, line, (ftnlen)6);
	e_wsfe();
	return 0;
    }

/*     Check to see whether SSYTRD must be run.   

       RUNTRD -- if SSYTRD must be run. */

    runtrd = FALSE_;
    if (timsub[3] || timsub[4] || timsub[5] || timsub[6] || timsub[7] || 
	    timsub[8] || timsub[9] || timsub[10] || timsub[11] || timsub[12] 
	    || timsub[13] || timsub[14] || timsub[15]) {
	runtrd = TRUE_;
    }

/*     Check to see whether TRED1 must be run.   

       RUNTR1 -- if TRED1 must be run. */

    runtr1 = FALSE_;
    if (timsub[16] || timsub[17] || timsub[18] || timsub[19] || timsub[20] || 
	    timsub[21] || timsub[22]) {
	runtr1 = TRUE_;
    }

/*     Various Constants */

    ulp = slamch_("Epsilon") * slamch_("Base");
    ulpinv = 1.f / ulp;
    xlaenv_(&c__9, &c__25);

/*     Zero out OPCNTS, TIMES */

    for (j4 = 1; j4 <= 23; ++j4) {
	i__1 = *nsizes;
	for (j3 = 1; j3 <= i__1; ++j3) {
	    i__2 = *ntypes;
	    for (j2 = 1; j2 <= i__2; ++j2) {
		i__3 = *nparms;
		for (j1 = 1; j1 <= i__3; ++j1) {
		    opcnts_ref(j1, j2, j3, j4) = 0.f;
		    times_ref(j1, j2, j3, j4) = 0.f;
/* L40: */
		}
/* L50: */
	    }
/* L60: */
	}
/* L70: */
    }

/*     Do for each value of N: */

    i__1 = *nsizes;
    for (in = 1; in <= i__1; ++in) {

	n = nn[in];
	if (n > 0) {
	    lgn = (integer) (log((real) n) / log(2.f));
	    if (pow_ii(&c__2, &lgn) < n) {
		++lgn;
	    }
	    if (pow_ii(&c__2, &lgn) < n) {
		++lgn;
	    }
/* Computing 2nd power */
	    i__2 = n;
	    lwedc = (n << 2) + 1 + (n << 1) * lgn + i__2 * i__2 * 3;
	    liwedc = n * 6 + 6 + n * 5 * lgn;
	    lwevr = n * 18;
	    liwevr = n * 10;
	} else {
	    lwedc = 8;
	    liwedc = 12;
	    lwevr = 1;
	    liwevr = 1;
	}

/*        Do for each .TRUE. value in DOTYPE: */

	mtypes = min(4,*ntypes);
	if (*ntypes == 5 && *nsizes == 1) {
	    mtypes = *ntypes;
	}
	i__2 = mtypes;
	for (itype = 1; itype <= i__2; ++itype) {
	    if (! dotype[itype]) {
		goto L930;
	    }

/*           Save random number seed for error messages */

	    for (j = 1; j <= 4; ++j) {
		ioldsd[j - 1] = iseed[j];
/* L80: */
	    }

/* -----------------------------------------------------------------------   

             Time the LAPACK Routines   

             Generate A */

	    *(unsigned char *)uplo = 'L';
	    if (itype <= 4) {
		imode = kmode[itype - 1];
		slatms_(&n, &n, "S", &iseed[1], "S", &work[1], &imode, &
			ulpinv, &c_b43, &n, &n, uplo, &a[1], &n, &work[n + 1],
			 &iinfo);
	    }

/*           Time SSYTRD for each pair NNB(j), LDAS(j) */

	    if (timsub[0]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
/* Computing MIN */
		    i__4 = n, i__5 = nnb[ipar];
		    nb = min(i__4,i__5);
		    xlaenv_(&c__1, &nb);
		    xlaenv_(&c__2, &c__2);
		    xlaenv_(&c__3, &nb);

/*                 Time SSYTRD */

		    ic = 0;
		    latime_1.ops = 0.f;
		    s1 = second_();
L90:
		    slacpy_(uplo, &n, &n, &a[1], &n, &z__[1], &lda)
			    ;
		    i__4 = *lwork - n;
		    ssytrd_(uplo, &n, &z__[1], &lda, &d__[1], &e[1], &work[1],
			     &work[n + 1], &i__4, &iinfo);
		    if (iinfo != 0) {
			io___38.ciunit = *nout;
			s_wsfe(&io___38);
			do_fio(&c__1, subnam_ref(0, 1), (ftnlen)9);
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
			goto L590;
		    }

		    s2 = second_();
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			goto L90;
		    }

/*                 Subtract the time used in SLACPY. */

		    s1 = second_();
		    i__4 = ic;
		    for (j = 1; j <= i__4; ++j) {
			slacpy_(uplo, &n, &n, &a[1], &n, &z__[1], &lda);
/* L100: */
		    }
		    s2 = second_();
		    untime = s2 - s1;

/* Computing MAX */
		    r__1 = time - untime;
		    times_ref(ipar, itype, in, 1) = dmax(r__1,0.f) / (real) 
			    ic;
		    opcnts_ref(ipar, itype, in, 1) = sopla_("SSYTRD", &n, &
			    c__0, &c__0, &c__0, &nb);
/* L110: */
		}
	    } else {
		if (runtrd) {
		    slacpy_(uplo, &n, &n, &a[1], &n, &z__[1], &n);
		    i__3 = *lwork - n;
		    ssytrd_(uplo, &n, &z__[1], &n, &d__[1], &e[1], &work[1], &
			    work[n + 1], &i__3, &iinfo);
		    if (iinfo != 0) {
			io___42.ciunit = *nout;
			s_wsfe(&io___42);
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
			goto L590;
		    }
		}
	    }

/*           Time SORGTR for each pair NNB(j), LDAS(j) */

	    if (timsub[1]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
/* Computing MIN */
		    i__4 = n, i__5 = nnb[ipar];
		    nb = min(i__4,i__5);
		    xlaenv_(&c__1, &nb);
		    xlaenv_(&c__2, &c__2);
		    xlaenv_(&c__3, &nb);

/*                 Time SORGTR */

		    slacpy_(uplo, &n, &n, &a[1], &n, &z__[1], &lda)
			    ;
		    i__4 = *lwork - n;
		    ssytrd_(uplo, &n, &z__[1], &lda, &d__[1], &e[1], &work[1],
			     &work[n + 1], &i__4, &iinfo);
		    ic = 0;
		    latime_1.ops = 0.f;
		    s1 = second_();
L120:
		    slacpy_("F", &n, &n, &z__[1], &lda, &z1[1], &lda);
		    i__4 = *lwork - n;
		    sorgtr_(uplo, &n, &z1[1], &lda, &work[1], &work[n + 1], &
			    i__4, &iinfo);
		    if (iinfo != 0) {
			io___43.ciunit = *nout;
			s_wsfe(&io___43);
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
			goto L590;
		    }

		    s2 = second_();
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			goto L120;
		    }

/*                 Subtract the time used in SLACPY */

		    s1 = second_();
		    i__4 = ic;
		    for (j = 1; j <= i__4; ++j) {
			slacpy_("F", &n, &n, &z__[1], &lda, &z1[1], &lda);
/* L130: */
		    }
		    s2 = second_();
		    untime = s2 - s1;

/* Computing MAX */
		    r__1 = time - untime;
		    times_ref(ipar, itype, in, 2) = dmax(r__1,0.f) / (real) 
			    ic;
		    opcnts_ref(ipar, itype, in, 2) = sopla2_("SORGTR", uplo, &
			    n, &n, &n, &c__0, &nb);
/* L140: */
		}
	    }

/*           Time SORMTR for each pair NNB(j), LDAS(j) */

	    if (timsub[2]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
/* Computing MIN */
		    i__4 = n, i__5 = nnb[ipar];
		    nb = min(i__4,i__5);
		    xlaenv_(&c__1, &nb);
		    xlaenv_(&c__2, &c__2);
		    xlaenv_(&c__3, &nb);

/*                 Time SORMTR */

		    slacpy_(uplo, &n, &n, &a[1], &n, &z__[1], &lda)
			    ;
		    i__4 = *lwork - n;
		    ssytrd_(uplo, &n, &z__[1], &lda, &d__[1], &e[1], &work[1],
			     &work[n + 1], &i__4, &iinfo);
		    ic = 0;
		    latime_1.ops = 0.f;
		    s1 = second_();
L150:
		    scopy_(&n, &d__[1], &c__1, &work[lda + 1], &c__1);
		    i__4 = n - 1;
		    scopy_(&i__4, &e[1], &c__1, &work[(lda << 1) + 1], &c__1);
		    sstedc_("N", &n, &work[lda + 1], &work[(lda << 1) + 1], &
			    z1[1], &lda, &work[lda * 3 + 1], &lwedc, &iwork[1]
			    , &liwedc, &iinfo);
		    i__4 = *lwork - n;
		    sormtr_("L", uplo, "N", &n, &n, &z__[1], &lda, &work[1], &
			    z1[1], &lda, &work[n + 1], &i__4, &iinfo);
		    if (iinfo != 0) {
			io___44.ciunit = *nout;
			s_wsfe(&io___44);
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
			goto L590;
		    }

		    s2 = second_();
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			goto L150;
		    }

/*                 Subtract the time used in SCOPY and SSTEDC */

		    s1 = second_();
		    i__4 = ic;
		    for (j = 1; j <= i__4; ++j) {
			scopy_(&n, &d__[1], &c__1, &work[lda + 1], &c__1);
			i__5 = n - 1;
			scopy_(&i__5, &e[1], &c__1, &work[(lda << 1) + 1], &
				c__1);
			sstedc_("N", &n, &work[lda + 1], &work[(lda << 1) + 1]
				, &z1[1], &lda, &work[lda * 3 + 1], &lwedc, &
				iwork[1], &liwedc, &iinfo);
/* L160: */
		    }
		    s2 = second_();
		    untime = s2 - s1;

/* Computing MAX */
		    r__1 = time - untime;
		    times_ref(ipar, itype, in, 3) = dmax(r__1,0.f) / (real) 
			    ic;
		    opcnts_ref(ipar, itype, in, 3) = sopla2_("SORMTR", uplo, &
			    n, &n, &n, &c__0, &nb);
/* L170: */
		}
	    }

/*           Time SSTEQR, SSTERF, SPTEQR, SSTEBZ, SSTEIN, SSTEDC, SSTERV   
             for each distinct LDA=LDAS(j) */

	    if (timsub[3] || timsub[4] || timsub[5] || timsub[6] || timsub[7] 
		    || timsub[8] || timsub[9] || timsub[10] || timsub[11] || 
		    timsub[12] || timsub[13] || timsub[14] || timsub[15]) {
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
/* L180: */
		    }
		    if (lastl == 0) {

/*                    Time SSTEQR with VECT='N' */

			if (timsub[3]) {
			    ic = 0;
			    latime_1.ops = 0.f;
			    s1 = second_();
L190:
			    scopy_(&n, &d__[1], &c__1, &work[1], &c__1);
			    i__4 = n - 1;
			    scopy_(&i__4, &e[1], &c__1, &work[lda + 1], &c__1)
				    ;
			    ssteqr_("N", &n, &work[1], &work[lda + 1], &z__[1]
				    , &lda, &work[(lda << 1) + 1], &iinfo);
			    if (iinfo != 0) {
				io___46.ciunit = *nout;
				s_wsfe(&io___46);
				do_fio(&c__1, subnam_ref(0, 4), (ftnlen)9);
				do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&n, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
					integer));
				do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)
					sizeof(integer));
				e_wsfe();
				*info = abs(iinfo);
				goto L210;
			    }
			    s2 = second_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				goto L190;
			    }

/*                       Subtract the time used in SCOPY. */

			    s1 = second_();
			    i__4 = ic;
			    for (j = 1; j <= i__4; ++j) {
				scopy_(&n, &d__[1], &c__1, &work[1], &c__1);
				i__5 = n - 1;
				scopy_(&i__5, &e[1], &c__1, &work[lda + 1], &
					c__1);
/* L200: */
			    }
			    s2 = second_();
			    untime = s2 - s1;

/* Computing MAX */
			    r__1 = time - untime;
			    times_ref(ipar, itype, in, 4) = dmax(r__1,0.f) / (
				    real) ic;
			    opcnts_ref(ipar, itype, in, 4) = latime_1.ops / (
				    real) ic;
			}

/*                    Time SSTEQR with VECT='V' */

L210:
			if (timsub[4]) {
			    ic = 0;
			    latime_1.ops = 0.f;
			    s1 = second_();
L220:
			    scopy_(&n, &d__[1], &c__1, &work[1], &c__1);
			    i__4 = n - 1;
			    scopy_(&i__4, &e[1], &c__1, &work[lda + 1], &c__1)
				    ;
			    slaset_("Full", &lda, &n, &c_b43, &c_b142, &z__[1]
				    , &lda);
			    ssteqr_("V", &n, &work[1], &work[lda + 1], &z__[1]
				    , &lda, &work[(lda << 1) + 1], &iinfo);
			    if (iinfo != 0) {
				io___47.ciunit = *nout;
				s_wsfe(&io___47);
				do_fio(&c__1, subnam_ref(0, 5), (ftnlen)9);
				do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&n, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
					integer));
				do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)
					sizeof(integer));
				e_wsfe();
				*info = abs(iinfo);
				goto L240;
			    }
			    s2 = second_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				goto L220;
			    }

/*                       Subtract the time used in SCOPY. */

			    s1 = second_();
			    i__4 = ic;
			    for (j = 1; j <= i__4; ++j) {
				slaset_("Full", &lda, &n, &c_b43, &c_b142, &
					z__[1], &lda);
				scopy_(&n, &d__[1], &c__1, &work[1], &c__1);
				i__5 = n - 1;
				scopy_(&i__5, &e[1], &c__1, &work[lda + 1], &
					c__1);
/* L230: */
			    }
			    s2 = second_();
			    untime = s2 - s1;

/* Computing MAX */
			    r__1 = time - untime;
			    times_ref(ipar, itype, in, 5) = dmax(r__1,0.f) / (
				    real) ic;
			    opcnts_ref(ipar, itype, in, 5) = latime_1.ops / (
				    real) ic;
			}

/*                    Time SSTERF */

L240:
			if (timsub[5]) {
			    ic = 0;
			    latime_1.ops = 0.f;
			    s1 = second_();
L250:
			    scopy_(&n, &d__[1], &c__1, &work[1], &c__1);
			    i__4 = n - 1;
			    scopy_(&i__4, &e[1], &c__1, &work[lda + 1], &c__1)
				    ;
			    ssterf_(&n, &work[1], &work[lda + 1], &iinfo);
			    if (iinfo != 0) {
				io___48.ciunit = *nout;
				s_wsfe(&io___48);
				do_fio(&c__1, subnam_ref(0, 6), (ftnlen)9);
				do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&n, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
					integer));
				do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)
					sizeof(integer));
				e_wsfe();
				*info = abs(iinfo);
				goto L270;
			    }
			    s2 = second_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				goto L250;
			    }

/*                       Subtract the time used in SCOPY. */

			    s1 = second_();
			    i__4 = ic;
			    for (j = 1; j <= i__4; ++j) {
				scopy_(&n, &d__[1], &c__1, &work[1], &c__1);
				i__5 = n - 1;
				scopy_(&i__5, &e[1], &c__1, &work[lda + 1], &
					c__1);
/* L260: */
			    }
			    s2 = second_();
			    untime = s2 - s1;

/* Computing MAX */
			    r__1 = time - untime;
			    times_ref(ipar, itype, in, 6) = dmax(r__1,0.f) / (
				    real) ic;
			    opcnts_ref(ipar, itype, in, 6) = latime_1.ops / (
				    real) ic;
			}

/*                    Time SPTEQR with VECT='N' */

L270:
			if (timsub[6]) {

/*                       Modify the tridiagonal matrix to make it   
                         positive definite. */
			    e2[1] = dabs(d__[1]) + dabs(e[1]);
			    i__4 = n - 1;
			    for (i__ = 2; i__ <= i__4; ++i__) {
				e2[i__] = (r__1 = d__[i__], dabs(r__1)) + (
					r__2 = e[i__], dabs(r__2)) + (r__3 = 
					e[i__ - 1], dabs(r__3));
/* L280: */
			    }
			    e2[n] = (r__1 = d__[n], dabs(r__1)) + (r__2 = e[n 
				    - 1], dabs(r__2));
			    ic = 0;
			    latime_1.ops = 0.f;
			    s1 = second_();
L290:
			    scopy_(&n, &e2[1], &c__1, &work[1], &c__1);
			    i__4 = n - 1;
			    scopy_(&i__4, &e[1], &c__1, &work[lda + 1], &c__1)
				    ;
			    spteqr_("N", &n, &work[1], &work[lda + 1], &z__[1]
				    , &lda, &work[(lda << 1) + 1], &iinfo);
			    if (iinfo != 0) {
				io___50.ciunit = *nout;
				s_wsfe(&io___50);
				do_fio(&c__1, subnam_ref(0, 7), (ftnlen)9);
				do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&n, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
					integer));
				do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)
					sizeof(integer));
				e_wsfe();
				*info = abs(iinfo);
				goto L310;
			    }
			    s2 = second_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				goto L290;
			    }

/*                       Subtract the time used in SCOPY. */

			    s1 = second_();
			    i__4 = ic;
			    for (j = 1; j <= i__4; ++j) {
				scopy_(&n, &d__[1], &c__1, &work[1], &c__1);
				i__5 = n - 1;
				scopy_(&i__5, &e[1], &c__1, &work[lda + 1], &
					c__1);
/* L300: */
			    }
			    s2 = second_();
			    untime = s2 - s1;

/* Computing MAX */
			    r__1 = time - untime;
			    times_ref(ipar, itype, in, 7) = dmax(r__1,0.f) / (
				    real) ic;
			    opcnts_ref(ipar, itype, in, 7) = latime_1.ops / (
				    real) ic;
			}

/*                    Time SPTEQR with VECT='V' */

L310:
			if (timsub[7]) {

/*                       Modify the tridiagonal matrix to make it   
                         positive definite. */
			    e2[1] = dabs(d__[1]) + dabs(e[1]);
			    i__4 = n - 1;
			    for (i__ = 2; i__ <= i__4; ++i__) {
				e2[i__] = (r__1 = d__[i__], dabs(r__1)) + (
					r__2 = e[i__], dabs(r__2)) + (r__3 = 
					e[i__ - 1], dabs(r__3));
/* L320: */
			    }
			    e2[n] = (r__1 = d__[n], dabs(r__1)) + (r__2 = e[n 
				    - 1], dabs(r__2));
			    ic = 0;
			    latime_1.ops = 0.f;
			    s1 = second_();
L330:
			    scopy_(&n, &e2[1], &c__1, &work[1], &c__1);
			    i__4 = n - 1;
			    scopy_(&i__4, &e[1], &c__1, &work[lda + 1], &c__1)
				    ;
			    spteqr_("V", &n, &work[1], &work[lda + 1], &z__[1]
				    , &lda, &work[(lda << 1) + 1], &iinfo);
			    if (iinfo != 0) {
				io___51.ciunit = *nout;
				s_wsfe(&io___51);
				do_fio(&c__1, subnam_ref(0, 8), (ftnlen)9);
				do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&n, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
					integer));
				do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)
					sizeof(integer));
				e_wsfe();
				*info = abs(iinfo);
				goto L350;
			    }
			    s2 = second_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				goto L330;
			    }

/*                       Subtract the time used in SCOPY. */

			    s1 = second_();
			    i__4 = ic;
			    for (j = 1; j <= i__4; ++j) {
				scopy_(&n, &d__[1], &c__1, &work[1], &c__1);
				i__5 = n - 1;
				scopy_(&i__5, &e[1], &c__1, &work[lda + 1], &
					c__1);
/* L340: */
			    }
			    s2 = second_();
			    untime = s2 - s1;

/* Computing MAX */
			    r__1 = time - untime;
			    times_ref(ipar, itype, in, 8) = dmax(r__1,0.f) / (
				    real) ic;
			    opcnts_ref(ipar, itype, in, 8) = latime_1.ops / (
				    real) ic;
			}

/*                    Time SSTEBZ(I) */

L350:
			if (timsub[8]) {
			    il = 1;
			    iu = n;
			    abstol = 0.f;
			    ic = 0;
			    latime_1.ops = 0.f;
			    s1 = second_();
L360:
			    sstebz_("I", "B", &n, &vl, &vu, &il, &iu, &abstol,
				     &d__[1], &e[1], &mm, &nsplit, &work[1], &
				    iwork[1], &iwork[lda + 1], &work[(lda << 
				    1) + 1], &iwork[(lda << 1) + 1], &iinfo);
			    if (iinfo != 0) {
				io___59.ciunit = *nout;
				s_wsfe(&io___59);
				do_fio(&c__1, subnam_ref(0, 9), (ftnlen)9);
				do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&n, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
					integer));
				do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)
					sizeof(integer));
				e_wsfe();
				*info = abs(iinfo);
				goto L370;
			    }
			    s2 = second_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				goto L360;
			    }
			    untime = 0.f;

/* Computing MAX */
			    r__1 = time - untime;
			    times_ref(ipar, itype, in, 9) = dmax(r__1,0.f) / (
				    real) ic;
			    opcnts_ref(ipar, itype, in, 9) = latime_1.ops / (
				    real) ic;
			}

/*                    Time SSTEBZ(V) */

L370:
			if (timsub[9]) {
			    if (n == 1) {
				vl = d__[1] - dabs(d__[1]);
				vu = d__[1] + dabs(d__[1]);
			    } else {
				vl = d__[1] - dabs(e[1]);
				vu = d__[1] + dabs(e[1]);
				i__4 = n - 1;
				for (i__ = 2; i__ <= i__4; ++i__) {
/* Computing MIN */
				    r__3 = vl, r__4 = d__[i__] - (r__1 = e[
					    i__], dabs(r__1)) - (r__2 = e[i__ 
					    - 1], dabs(r__2));
				    vl = dmin(r__3,r__4);
/* Computing MAX */
				    r__3 = vu, r__4 = d__[i__] + (r__1 = e[
					    i__], dabs(r__1)) + (r__2 = e[i__ 
					    - 1], dabs(r__2));
				    vu = dmax(r__3,r__4);
/* L380: */
				}
/* Computing MIN */
				r__2 = vl, r__3 = d__[n] - (r__1 = e[n - 1], 
					dabs(r__1));
				vl = dmin(r__2,r__3);
/* Computing MAX */
				r__2 = vu, r__3 = d__[n] + (r__1 = e[n - 1], 
					dabs(r__1));
				vu = dmax(r__2,r__3);
			    }
			    abstol = 0.f;
			    ic = 0;
			    latime_1.ops = 0.f;
			    s1 = second_();
L390:
			    sstebz_("V", "B", &n, &vl, &vu, &il, &iu, &abstol,
				     &d__[1], &e[1], &mm, &nsplit, &work[1], &
				    iwork[1], &iwork[lda + 1], &work[(lda << 
				    1) + 1], &iwork[(lda << 1) + 1], &iinfo);
			    if (iinfo != 0) {
				io___60.ciunit = *nout;
				s_wsfe(&io___60);
				do_fio(&c__1, subnam_ref(0, 10), (ftnlen)9);
				do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&n, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
					integer));
				do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)
					sizeof(integer));
				e_wsfe();
				*info = abs(iinfo);
				goto L400;
			    }
			    s2 = second_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				goto L390;
			    }
			    untime = 0.f;

/* Computing MAX */
			    r__1 = time - untime;
			    times_ref(ipar, itype, in, 10) = dmax(r__1,0.f) / 
				    (real) ic;
			    opcnts_ref(ipar, itype, in, 10) = latime_1.ops / (
				    real) ic;
			}

/*                    Time SSTEIN */

L400:
			if (timsub[10]) {
			    ic = 0;
			    latime_1.ops = 0.f;
			    s1 = second_();
L410:
			    sstein_(&n, &d__[1], &e[1], &mm, &work[1], &iwork[
				    1], &iwork[lda + 1], &z__[1], &lda, &work[
				    lda + 1], &iwork[(lda << 1) + 1], &iwork[
				    lda * 3 + 1], &iinfo);
			    if (iinfo != 0) {
				io___61.ciunit = *nout;
				s_wsfe(&io___61);
				do_fio(&c__1, subnam_ref(0, 11), (ftnlen)9);
				do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&n, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
					integer));
				do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)
					sizeof(integer));
				e_wsfe();
				*info = abs(iinfo);
				goto L420;
			    }
			    s2 = second_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				goto L410;
			    }
			    untime = 0.f;

/* Computing MAX */
			    r__1 = time - untime;
			    times_ref(ipar, itype, in, 11) = dmax(r__1,0.f) / 
				    (real) ic;
			    opcnts_ref(ipar, itype, in, 11) = latime_1.ops / (
				    real) ic;
			}

/*                    Time SSTEDC with COMPQ='N' */

L420:
			if (timsub[11]) {
			    ic = 0;
			    latime_1.ops = 0.f;
			    s1 = second_();
L430:
			    scopy_(&n, &d__[1], &c__1, &work[1], &c__1);
			    i__4 = n - 1;
			    scopy_(&i__4, &e[1], &c__1, &work[lda + 1], &c__1)
				    ;
			    sstedc_("N", &n, &work[1], &work[lda + 1], &z__[1]
				    , &lda, &work[(lda << 1) + 1], &lwedc, &
				    iwork[1], &liwedc, &iinfo);
			    if (iinfo != 0) {
				io___62.ciunit = *nout;
				s_wsfe(&io___62);
				do_fio(&c__1, subnam_ref(0, 12), (ftnlen)9);
				do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&n, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
					integer));
				do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)
					sizeof(integer));
				e_wsfe();
				*info = abs(iinfo);
				goto L450;
			    }
			    s2 = second_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				goto L430;
			    }

/*                       Subtract the time used in SCOPY. */

			    s1 = second_();
			    i__4 = ic;
			    for (j = 1; j <= i__4; ++j) {
				scopy_(&n, &d__[1], &c__1, &work[1], &c__1);
				i__5 = n - 1;
				scopy_(&i__5, &e[1], &c__1, &work[lda + 1], &
					c__1);
/* L440: */
			    }
			    s2 = second_();
			    untime = s2 - s1;

/* Computing MAX */
			    r__1 = time - untime;
			    times_ref(ipar, itype, in, 12) = dmax(r__1,0.f) / 
				    (real) ic;
			    opcnts_ref(ipar, itype, in, 12) = latime_1.ops / (
				    real) ic;
			}

/*                    Time SSTEDC with COMPQ='I' */

L450:
			if (timsub[12]) {
			    ic = 0;
			    latime_1.ops = 0.f;
			    s1 = second_();
L460:
			    scopy_(&n, &d__[1], &c__1, &work[1], &c__1);
			    i__4 = n - 1;
			    scopy_(&i__4, &e[1], &c__1, &work[lda + 1], &c__1)
				    ;
			    slaset_("Full", &lda, &n, &c_b43, &c_b142, &z__[1]
				    , &lda);
			    sstedc_("I", &n, &work[1], &work[lda + 1], &z__[1]
				    , &lda, &work[(lda << 1) + 1], &lwedc, &
				    iwork[1], &liwedc, &iinfo);
			    if (iinfo != 0) {
				io___63.ciunit = *nout;
				s_wsfe(&io___63);
				do_fio(&c__1, subnam_ref(0, 13), (ftnlen)9);
				do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&n, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
					integer));
				do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)
					sizeof(integer));
				e_wsfe();
				*info = abs(iinfo);
				goto L480;
			    }
			    s2 = second_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				goto L460;
			    }

/*                       Subtract the time used in SCOPY. */

			    s1 = second_();
			    i__4 = ic;
			    for (j = 1; j <= i__4; ++j) {
				slaset_("Full", &lda, &n, &c_b43, &c_b142, &
					z__[1], &lda);
				scopy_(&n, &d__[1], &c__1, &work[1], &c__1);
				i__5 = n - 1;
				scopy_(&i__5, &e[1], &c__1, &work[lda + 1], &
					c__1);
/* L470: */
			    }
			    s2 = second_();
			    untime = s2 - s1;

/* Computing MAX */
			    r__1 = time - untime;
			    times_ref(ipar, itype, in, 13) = dmax(r__1,0.f) / 
				    (real) ic;
			    opcnts_ref(ipar, itype, in, 13) = latime_1.ops / (
				    real) ic;
			}
L480:

/*                    Time SSTEDC with COMPQ='V' */

			if (timsub[13]) {
			    ic = 0;
			    latime_1.ops = 0.f;
			    s1 = second_();
L490:
			    scopy_(&n, &d__[1], &c__1, &work[1], &c__1);
			    i__4 = n - 1;
			    scopy_(&i__4, &e[1], &c__1, &work[lda + 1], &c__1)
				    ;
			    slaset_("Full", &lda, &n, &c_b43, &c_b142, &z__[1]
				    , &lda);
			    sstedc_("V", &n, &work[1], &work[lda + 1], &z__[1]
				    , &lda, &work[(lda << 1) + 1], &lwedc, &
				    iwork[1], &liwedc, &iinfo);
			    if (iinfo != 0) {
				io___64.ciunit = *nout;
				s_wsfe(&io___64);
				do_fio(&c__1, subnam_ref(0, 14), (ftnlen)9);
				do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&n, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
					integer));
				do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)
					sizeof(integer));
				e_wsfe();
				*info = abs(iinfo);
				goto L510;
			    }
			    s2 = second_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				goto L490;
			    }

/*                       Subtract the time used in SCOPY. */

			    s1 = second_();
			    i__4 = ic;
			    for (j = 1; j <= i__4; ++j) {
				slaset_("Full", &lda, &n, &c_b43, &c_b142, &
					z__[1], &lda);
				scopy_(&n, &d__[1], &c__1, &work[1], &c__1);
				i__5 = n - 1;
				scopy_(&i__5, &e[1], &c__1, &work[lda + 1], &
					c__1);
/* L500: */
			    }
			    s2 = second_();
			    untime = s2 - s1;

/* Computing MAX */
			    r__1 = time - untime;
			    times_ref(ipar, itype, in, 14) = dmax(r__1,0.f) / 
				    (real) ic;
			    opcnts_ref(ipar, itype, in, 14) = latime_1.ops / (
				    real) ic;
			}
L510:

/*                    Time SSTEGR with COMPQ='N' */

			if (timsub[14]) {
			    abstol = 0.f;
			    vl = 0.f;
			    vu = 0.f;
			    il = 1;
			    iu = n;
			    ic = 0;
			    latime_1.ops = 0.f;
			    s1 = second_();
L520:
			    scopy_(&n, &d__[1], &c__1, &work[1], &c__1);
			    i__4 = n - 1;
			    scopy_(&i__4, &e[1], &c__1, &work[lda + 1], &c__1)
				    ;
			    sstegr_("N", "A", &n, &work[1], &work[lda + 1], &
				    vl, &vu, &il, &iu, &abstol, &m, &work[(
				    lda << 1) + 1], &z__[1], &lda, &iwork[1], 
				    &work[lda * 3 + 1], &lwevr, &iwork[(lda <<
				     1) + 1], &liwevr, info);
			    if (iinfo != 0) {
				io___66.ciunit = *nout;
				s_wsfe(&io___66);
				do_fio(&c__1, subnam_ref(0, 15), (ftnlen)9);
				do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&n, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
					integer));
				do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)
					sizeof(integer));
				e_wsfe();
				*info = abs(iinfo);
				goto L540;
			    }
			    s2 = second_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				goto L520;
			    }

/*                       Subtract the time used in SCOPY. */

			    s1 = second_();
			    i__4 = ic;
			    for (j = 1; j <= i__4; ++j) {
				slaset_("Full", &lda, &n, &c_b43, &c_b142, &
					z__[1], &lda);
				scopy_(&n, &d__[1], &c__1, &work[1], &c__1);
				i__5 = n - 1;
				scopy_(&i__5, &e[1], &c__1, &work[lda + 1], &
					c__1);
/* L530: */
			    }
			    s2 = second_();
			    untime = s2 - s1;

/* Computing MAX */
			    r__1 = time - untime;
			    times_ref(ipar, itype, in, 15) = dmax(r__1,0.f) / 
				    (real) ic;
			    opcnts_ref(ipar, itype, in, 15) = latime_1.ops / (
				    real) ic;
			}
L540:

/*                    Time SSTEGR with COMPQ='V' */

			if (timsub[15]) {
			    abstol = 0.f;
			    vl = 0.f;
			    vu = 0.f;
			    il = 1;
			    iu = n;
			    ic = 0;
			    latime_1.ops = 0.f;
			    s1 = second_();
L550:
			    scopy_(&n, &d__[1], &c__1, &work[1], &c__1);
			    i__4 = n - 1;
			    scopy_(&i__4, &e[1], &c__1, &work[lda + 1], &c__1)
				    ;
			    sstegr_("V", "A", &n, &work[1], &work[lda + 1], &
				    vl, &vu, &il, &iu, &abstol, &m, &work[(
				    lda << 1) + 1], &z__[1], &lda, &iwork[1], 
				    &work[lda * 3 + 1], &lwevr, &iwork[(lda <<
				     1) + 1], &liwevr, info);
			    if (iinfo != 0) {
				io___67.ciunit = *nout;
				s_wsfe(&io___67);
				do_fio(&c__1, subnam_ref(0, 16), (ftnlen)9);
				do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&n, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
					integer));
				do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)
					sizeof(integer));
				e_wsfe();
				*info = abs(iinfo);
				goto L570;
			    }
			    s2 = second_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				goto L550;
			    }

/*                       Subtract the time used in SCOPY. */

			    s1 = second_();
			    i__4 = ic;
			    for (j = 1; j <= i__4; ++j) {
				slaset_("Full", &lda, &n, &c_b43, &c_b142, &
					z__[1], &lda);
				scopy_(&n, &d__[1], &c__1, &work[1], &c__1);
				i__5 = n - 1;
				scopy_(&i__5, &e[1], &c__1, &work[lda + 1], &
					c__1);
/* L560: */
			    }
			    s2 = second_();
			    untime = s2 - s1;

/* Computing MAX */
			    r__1 = time - untime;
			    times_ref(ipar, itype, in, 16) = dmax(r__1,0.f) / 
				    (real) ic;
			    opcnts_ref(ipar, itype, in, 16) = latime_1.ops / (
				    real) ic;
			}
L570:

			;
		    } else {
			if (timsub[3]) {
			    opcnts_ref(ipar, itype, in, 4) = opcnts_ref(lastl,
				     itype, in, 4);
			    times_ref(ipar, itype, in, 4) = times_ref(lastl, 
				    itype, in, 4);
			}
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
			if (timsub[8]) {
			    opcnts_ref(ipar, itype, in, 9) = opcnts_ref(lastl,
				     itype, in, 9);
			    times_ref(ipar, itype, in, 9) = times_ref(lastl, 
				    itype, in, 9);
			}
			if (timsub[9]) {
			    opcnts_ref(ipar, itype, in, 10) = opcnts_ref(
				    lastl, itype, in, 10);
			    times_ref(ipar, itype, in, 10) = times_ref(lastl, 
				    itype, in, 10);
			}
			if (timsub[10]) {
			    opcnts_ref(ipar, itype, in, 11) = opcnts_ref(
				    lastl, itype, in, 11);
			    times_ref(ipar, itype, in, 11) = times_ref(lastl, 
				    itype, in, 11);
			}
			if (timsub[11]) {
			    opcnts_ref(ipar, itype, in, 12) = opcnts_ref(
				    lastl, itype, in, 12);
			    times_ref(ipar, itype, in, 12) = times_ref(lastl, 
				    itype, in, 12);
			}
			if (timsub[12]) {
			    opcnts_ref(ipar, itype, in, 13) = opcnts_ref(
				    lastl, itype, in, 13);
			    times_ref(ipar, itype, in, 13) = times_ref(lastl, 
				    itype, in, 13);
			}
			if (timsub[13]) {
			    opcnts_ref(ipar, itype, in, 14) = opcnts_ref(
				    lastl, itype, in, 14);
			    times_ref(ipar, itype, in, 14) = times_ref(lastl, 
				    itype, in, 14);
			}
			if (timsub[14]) {
			    opcnts_ref(ipar, itype, in, 15) = opcnts_ref(
				    lastl, itype, in, 15);
			    times_ref(ipar, itype, in, 15) = times_ref(lastl, 
				    itype, in, 15);
			}
			if (timsub[15]) {
			    opcnts_ref(ipar, itype, in, 16) = opcnts_ref(
				    lastl, itype, in, 16);
			    times_ref(ipar, itype, in, 16) = times_ref(lastl, 
				    itype, in, 16);
			}
		    }
/* L580: */
		}
	    }
L590:

/* -----------------------------------------------------------------------   

             Time the EISPACK Routines   

             Skip routines if N <= 0 (EISPACK requirement) */

	    if (n <= 0) {
		goto L930;
	    }

/*           Time TRED1 for each LDAS(j) */

	    if (timsub[16]) {
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
/* L600: */
		    }

		    if (lastl == 0) {

/*                    Time TRED1 */

			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L610:
			slacpy_("L", &n, &n, &a[1], &n, &z__[1], &lda);
			tred1_(&lda, &n, &z__[1], &d__[1], &e[1], &e2[1]);
			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L610;
			}

/*                    Subtract the time used in SLACPY. */

			s1 = second_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    slacpy_("L", &n, &n, &a[1], &n, &z__[1], &lda);
/* L620: */
			}
			s2 = second_();
			untime = s2 - s1;

/* Computing MAX */
			r__1 = time - untime;
			times_ref(ipar, itype, in, 17) = dmax(r__1,0.f) / (
				real) ic;
			opcnts_ref(ipar, itype, in, 17) = latime_1.ops / (
				real) ic;
		    } else {
			opcnts_ref(ipar, itype, in, 17) = opcnts_ref(lastl, 
				itype, in, 17);
			times_ref(ipar, itype, in, 17) = times_ref(lastl, 
				itype, in, 17);
		    }
/* L630: */
		}
	    } else {
		if (runtr1) {
		    slacpy_("L", &n, &n, &a[1], &n, &z__[1], &lda);
		    tred1_(&lda, &n, &z__[1], &d__[1], &e[1], &e2[1]);
		}
	    }

/*           Time IMTQL1 for each LDAS(j) */

	    if (timsub[17]) {
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
/* L640: */
		    }

		    if (lastl == 0) {

/*                    Time IMTQL1 */

			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L650:
			scopy_(&n, &d__[1], &c__1, &work[1], &c__1);
			i__4 = n - 1;
			scopy_(&i__4, &e[1], &c__1, &work[lda + 1], &c__1);
			imtql1_(&n, &work[1], &work[lda + 1], &iinfo);
			if (iinfo != 0) {
			    io___68.ciunit = *nout;
			    s_wsfe(&io___68);
			    do_fio(&c__1, subnam_ref(0, 18), (ftnlen)9);
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
			    goto L680;
			}
			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L650;
			}

/*                    Subtract the time used in SCOPY. */

			s1 = second_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    scopy_(&n, &d__[1], &c__1, &work[1], &c__1);
			    i__5 = n - 1;
			    scopy_(&i__5, &e[1], &c__1, &work[lda + 1], &c__1)
				    ;
/* L660: */
			}
			s2 = second_();
			untime = s2 - s1;

/* Computing MAX */
			r__1 = time - untime;
			times_ref(ipar, itype, in, 18) = dmax(r__1,0.f) / (
				real) ic;
			opcnts_ref(ipar, itype, in, 18) = latime_1.ops / (
				real) ic;
		    } else {
			opcnts_ref(ipar, itype, in, 18) = opcnts_ref(lastl, 
				itype, in, 18);
			times_ref(ipar, itype, in, 18) = times_ref(lastl, 
				itype, in, 18);
		    }
/* L670: */
		}
	    }

/*           Time IMTQL2 for each LDAS(j) */

L680:
	    if (timsub[18]) {
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
/* L690: */
		    }

		    if (lastl == 0) {

/*                    Time IMTQL2 */

			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L700:
			scopy_(&n, &d__[1], &c__1, &work[1], &c__1);
			i__4 = n - 1;
			scopy_(&i__4, &e[1], &c__1, &work[lda + 1], &c__1);
			slaset_("Full", &n, &n, &c_b43, &c_b142, &z__[1], &
				lda);
			imtql2_(&lda, &n, &work[1], &work[lda + 1], &z__[1], &
				iinfo);
			if (iinfo != 0) {
			    io___69.ciunit = *nout;
			    s_wsfe(&io___69);
			    do_fio(&c__1, subnam_ref(0, 19), (ftnlen)9);
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
			    goto L730;
			}
			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L700;
			}

/*                    Subtract the time used in SCOPY. */

			s1 = second_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    scopy_(&n, &d__[1], &c__1, &work[1], &c__1);
			    i__5 = n - 1;
			    scopy_(&i__5, &e[1], &c__1, &work[lda + 1], &c__1)
				    ;
			    slaset_("Full", &n, &n, &c_b43, &c_b142, &z__[1], 
				    &lda);
/* L710: */
			}
			s2 = second_();
			untime = s2 - s1;

/* Computing MAX */
			r__1 = time - untime;
			times_ref(ipar, itype, in, 19) = dmax(r__1,0.f) / (
				real) ic;
			opcnts_ref(ipar, itype, in, 19) = latime_1.ops / (
				real) ic;
		    } else {
			opcnts_ref(ipar, itype, in, 19) = opcnts_ref(lastl, 
				itype, in, 19);
			times_ref(ipar, itype, in, 19) = times_ref(lastl, 
				itype, in, 19);
		    }
/* L720: */
		}
	    }

/*           Time TQLRAT for each LDAS(j) */

L730:
	    if (timsub[19]) {
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
/* L740: */
		    }

		    if (lastl == 0) {

/*                    Time TQLRAT */

			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L750:
			scopy_(&n, &d__[1], &c__1, &work[1], &c__1);
			i__4 = n - 1;
			scopy_(&i__4, &e2[1], &c__1, &work[lda + 1], &c__1);
			tqlrat_(&n, &work[1], &work[lda + 1], &iinfo);
			if (iinfo != 0) {
			    io___70.ciunit = *nout;
			    s_wsfe(&io___70);
			    do_fio(&c__1, subnam_ref(0, 20), (ftnlen)9);
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
			    goto L780;
			}
			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L750;
			}

/*                    Subtract the time used in SCOPY. */

			s1 = second_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    scopy_(&n, &d__[1], &c__1, &work[1], &c__1);
			    i__5 = n - 1;
			    scopy_(&i__5, &e2[1], &c__1, &work[lda + 1], &
				    c__1);
/* L760: */
			}
			s2 = second_();
			untime = s2 - s1;

/* Computing MAX */
			r__1 = time - untime;
			times_ref(ipar, itype, in, 20) = dmax(r__1,0.f) / (
				real) ic;
			opcnts_ref(ipar, itype, in, 20) = latime_1.ops / (
				real) ic;
		    } else {
			opcnts_ref(ipar, itype, in, 20) = opcnts_ref(lastl, 
				itype, in, 20);
			times_ref(ipar, itype, in, 20) = times_ref(lastl, 
				itype, in, 20);
		    }
/* L770: */
		}
	    }

/*           Time TRIDIB for each LDAS(j) */

L780:
	    if (timsub[20]) {
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
/* L790: */
		    }

		    if (lastl == 0) {

/*                    Time TRIDIB */

			ic = 0;
			latime_1.ops = 0.f;
			eps1 = 0.f;
			rlb = 0.f;
			rub = 0.f;
			m11 = 1;
			mm = n;
			s1 = second_();
L800:
			scopy_(&n, &d__[1], &c__1, &work[1], &c__1);
			i__4 = n - 1;
			scopy_(&i__4, &e[1], &c__1, &work[lda + 1], &c__1);
			i__4 = n - 1;
			scopy_(&i__4, &e2[1], &c__1, &work[(lda << 1) + 1], &
				c__1);
			tridib_(&n, &eps1, &work[1], &work[lda + 1], &work[(
				lda << 1) + 1], &rlb, &rub, &m11, &mm, &work[
				lda * 3 + 1], &iwork[1], &iinfo, &work[(lda <<
				 2) + 1], &work[lda * 5 + 1]);
			if (iinfo != 0) {
			    io___75.ciunit = *nout;
			    s_wsfe(&io___75);
			    do_fio(&c__1, subnam_ref(0, 21), (ftnlen)9);
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
			    goto L830;
			}
			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L800;
			}

/*                    Subtract the time used in SCOPY. */

			s1 = second_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    scopy_(&n, &d__[1], &c__1, &work[1], &c__1);
			    i__5 = n - 1;
			    scopy_(&i__5, &e[1], &c__1, &work[lda + 1], &c__1)
				    ;
			    i__5 = n - 1;
			    scopy_(&i__5, &e2[1], &c__1, &work[(lda << 1) + 1]
				    , &c__1);
/* L810: */
			}
			s2 = second_();
			untime = s2 - s1;

/* Computing MAX */
			r__1 = time - untime;
			times_ref(ipar, itype, in, 21) = dmax(r__1,0.f) / (
				real) ic;
			opcnts_ref(ipar, itype, in, 21) = latime_1.ops / (
				real) ic;
		    } else {
			opcnts_ref(ipar, itype, in, 21) = opcnts_ref(lastl, 
				itype, in, 21);
			times_ref(ipar, itype, in, 21) = times_ref(lastl, 
				itype, in, 21);
		    }
/* L820: */
		}
	    }

/*           Time BISECT for each LDAS(j) */

L830:
	    if (timsub[21]) {
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
/* L840: */
		    }

		    if (lastl == 0) {

/*                    Time BISECT */

			vl = d__[1] - dabs(e[2]);
			vu = d__[1] + dabs(e[2]);
			i__4 = n - 1;
			for (i__ = 2; i__ <= i__4; ++i__) {
/* Computing MIN */
			    r__3 = vl, r__4 = d__[i__] - (r__1 = e[i__ + 1], 
				    dabs(r__1)) - (r__2 = e[i__], dabs(r__2));
			    vl = dmin(r__3,r__4);
/* Computing MAX */
			    r__3 = vu, r__4 = d__[i__] + (r__1 = e[i__ + 1], 
				    dabs(r__1)) + (r__2 = e[i__], dabs(r__2));
			    vu = dmax(r__3,r__4);
/* L850: */
			}
/* Computing MIN */
			r__2 = vl, r__3 = d__[n] - (r__1 = e[n], dabs(r__1));
			vl = dmin(r__2,r__3);
/* Computing MAX */
			r__2 = vu, r__3 = d__[n] + (r__1 = e[n], dabs(r__1));
			vu = dmax(r__2,r__3);
			ic = 0;
			latime_1.ops = 0.f;
			eps1 = 0.f;
			mm = n;
			mmm = 0;
			s1 = second_();
L860:
			scopy_(&n, &d__[1], &c__1, &work[1], &c__1);
			scopy_(&n, &e[1], &c__1, &work[lda + 1], &c__1);
			scopy_(&n, &e2[1], &c__1, &work[(lda << 1) + 1], &
				c__1);
			bisect_(&n, &eps1, &work[1], &work[lda + 1], &work[(
				lda << 1) + 1], &vl, &vu, &mm, &mmm, &work[
				lda * 3 + 1], &iwork[1], &iinfo, &work[(lda <<
				 2) + 1], &work[lda * 5 + 1]);
			if (iinfo != 0) {
			    io___77.ciunit = *nout;
			    s_wsfe(&io___77);
			    do_fio(&c__1, subnam_ref(0, 22), (ftnlen)9);
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
			    goto L890;
			}
			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L860;
			}

/*                    Subtract the time used in SCOPY. */

			s1 = second_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    scopy_(&n, &d__[1], &c__1, &work[1], &c__1);
			    scopy_(&n, &e[1], &c__1, &work[lda + 1], &c__1);
			    scopy_(&n, &e2[1], &c__1, &work[(lda << 1) + 1], &
				    c__1);
/* L870: */
			}
			s2 = second_();
			untime = s2 - s1;

/* Computing MAX */
			r__1 = time - untime;
			times_ref(ipar, itype, in, 22) = dmax(r__1,0.f) / (
				real) ic;
			opcnts_ref(ipar, itype, in, 22) = latime_1.ops / (
				real) ic;
		    } else {
			opcnts_ref(ipar, itype, in, 22) = opcnts_ref(lastl, 
				itype, in, 22);
			times_ref(ipar, itype, in, 22) = times_ref(lastl, 
				itype, in, 22);
		    }
/* L880: */
		}
	    }

/*           Time TINVIT for each LDAS(j) */

L890:
	    if (timsub[22]) {
		scopy_(&n, &work[lda * 3 + 1], &c__1, &work[1], &c__1);
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
/* L900: */
		    }

		    if (lastl == 0) {

/*                    Time TINVIT */

			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L910:
			tinvit_(&lda, &n, &d__[1], &e[1], &e2[1], &mmm, &work[
				1], &iwork[1], &z__[1], &iinfo, &work[lda + 1]
				, &work[(lda << 1) + 1], &work[lda * 3 + 1], &
				work[(lda << 2) + 1], &work[lda * 5 + 1]);
			if (iinfo != 0) {
			    io___78.ciunit = *nout;
			    s_wsfe(&io___78);
			    do_fio(&c__1, subnam_ref(0, 23), (ftnlen)9);
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
			    goto L930;
			}
			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L910;
			}
			untime = 0.f;

/* Computing MAX */
			r__1 = time - untime;
			times_ref(ipar, itype, in, 23) = dmax(r__1,0.f) / (
				real) ic;
			opcnts_ref(ipar, itype, in, 23) = latime_1.ops / (
				real) ic;
		    } else {
			opcnts_ref(ipar, itype, in, 23) = opcnts_ref(lastl, 
				itype, in, 23);
			times_ref(ipar, itype, in, 23) = times_ref(lastl, 
				itype, in, 23);
		    }
/* L920: */
		}
	    }

L930:
	    ;
	}
/* L940: */
    }

/* -----------------------------------------------------------------------   

       Print a table of results for each timed routine. */

    for (isub = 1; isub <= 23; ++isub) {
	if (timsub[isub - 1]) {
	    sprtbe_(subnam_ref(0, isub), &mtypes, &dotype[1], nsizes, &nn[1], 
		    &inparm[isub - 1], pnames, nparms, &ldas[1], &nnb[1], 
		    idumma, idumma, &opcnts_ref(1, 1, 1, isub), ldo1, ldo2, &
		    times_ref(1, 1, 1, isub), ldt1, ldt2, &work[1], &llwork[1]
		    , nout, (ftnlen)9, (ftnlen)4);
	}
/* L950: */
    }


    return 0;

/*     End of STIM22 */

} /* stim22_ */

#undef opcnts_ref
#undef subnam_ref
#undef times_ref


