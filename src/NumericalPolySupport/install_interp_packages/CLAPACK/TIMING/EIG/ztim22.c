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
static integer c__10 = 10;
static integer c__0 = 0;
static integer c__11 = 11;
static integer c__1 = 1;
static integer c__9 = 9;
static integer c__25 = 25;
static integer c__2 = 2;
static doublereal c_b43 = 1.;
static integer c__4 = 4;
static integer c__3 = 3;
static doublereal c_b342 = 0.;

/* Subroutine */ int ztim22_(char *line, integer *nsizes, integer *nn, 
	integer *ntypes, logical *dotype, integer *nparms, integer *nnb, 
	integer *ldas, doublereal *timmin, integer *nout, integer *iseed, 
	doublecomplex *a, doublereal *d__, doublereal *e, doublereal *e2, 
	doublecomplex *u, doublereal *ure, doublereal *uim, doublecomplex *
	tau, doublereal *taure, doublecomplex *z__, doublereal *zre, 
	doublereal *zim, doublecomplex *work, integer *lwork, doublereal *
	rwork, logical *llwork, integer *iwork, doublereal *times, integer *
	ldt1, integer *ldt2, integer *ldt3, doublereal *opcnts, integer *ldo1,
	 integer *ldo2, integer *ldo3, integer *info, ftnlen line_len)
{
    /* Initialized data */

    static char subnam[20*12] = "ZHETRD              " "ZSTEQR(N)           " 
	    "ZUNGTR+ZSTEQR(V)    " "ZPTEQR(N)           " "ZUNGTR+ZPTEQR(V) "
	    "   " "DSTEBZ+ZSTEIN+ZUNMTR" "ZUNGTR+ZSTEDC(V)    " "ZSTEDC(I)+ZU"
	    "NMTR    " "ZSTEGR(V)           " "HTRIDI              " "IMTQL1 "
	    "             " "IMTQL2+HTRIBK       ";
    static integer inparm[12] = { 2,1,2,1,2,2,1,1,1,1,1,1 };
    static char pnames[4*4] = "LDA " "NB  " "bad1" "bad2";
    static integer kmode[4] = { 4,3,1,5 };

    /* Format strings */
    static char fmt_9999[] = "(1x,a,\002 timing run not attempted -- N > LD"
	    "A\002,/)";
    static char fmt_9998[] = "(1x,a,\002 timing run not attempted -- LWORK t"
	    "oo small.\002,/)";
    static char fmt_9997[] = "(\002 ZTIM22: \002,a,\002 returned INFO=\002,i"
	    "6,\002.\002,/9x,\002N=\002,i6,\002, ITYPE=\002,i6,\002, IPAR="
	    "\002,i6,\002, ISEED=(\002,3(i5,\002,\002),i5,\002)\002)";

    /* System generated locals */
    integer opcnts_dim1, opcnts_dim2, opcnts_dim3, opcnts_offset, times_dim1, 
	    times_dim2, times_dim3, times_offset, i__1, i__2, i__3, i__4, 
	    i__5, i__6, i__7;
    doublereal d__1, d__2, d__3;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);
    double log(doublereal);
    integer pow_ii(integer *, integer *);
    double d_imag(doublecomplex *);

    /* Local variables */
    static integer ipar;
    static doublereal time;
    static integer isub;
    static char uplo[1];
    static integer i__, j, m, n, imode, lwedc;
    extern doublereal dopla_(char *, integer *, integer *, integer *, integer 
	    *, integer *);
    static integer iinfo;
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *, 
	    doublereal *, integer *);
    static integer lastl, itype, j1, j2, j3, j4, lwevr;
    static doublereal s1, s2;
    extern /* Subroutine */ int imtql1_(integer *, doublereal *, doublereal *,
	     integer *), imtql2_(integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, integer *);
    static integer ic, nb, il, in;
    extern doublereal dlamch_(char *);
    static integer iu;
    extern doublereal dsecnd_(void);
    static doublereal vl;
    static integer liwedc;
    static doublereal vu;
    static integer idumma[1];
    extern /* Subroutine */ int dlaset_(char *, integer *, integer *, 
	    doublereal *, doublereal *, doublereal *, integer *);
    static integer ioldsd[4];
    extern integer ilaenv_(integer *, char *, char *, integer *, integer *, 
	    integer *, integer *, ftnlen, ftnlen);
    static integer lrwedc;
    extern /* Subroutine */ int atimin_(char *, char *, integer *, char *, 
	    logical *, integer *, integer *, ftnlen, ftnlen, ftnlen), dprtbe_(
	    char *, integer *, logical *, integer *, integer *, integer *, 
	    char *, integer *, integer *, integer *, integer *, integer *, 
	    doublereal *, integer *, integer *, doublereal *, integer *, 
	    integer *, doublereal *, logical *, integer *, ftnlen, ftnlen);
    static doublereal abstol;
    extern /* Subroutine */ int htribk_(integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, integer *, doublereal *, doublereal *)
	    , htridi_(integer *, integer *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *);
    static integer nansok, infsok;
    extern /* Subroutine */ int dstebz_(char *, char *, integer *, doublereal 
	    *, doublereal *, integer *, integer *, doublereal *, doublereal *,
	     doublereal *, integer *, integer *, doublereal *, integer *, 
	    integer *, doublereal *, integer *, integer *), 
	    zstedc_(char *, integer *, doublereal *, doublereal *, 
	    doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublereal *, integer *, integer *, integer *, integer *),
	     xlaenv_(integer *, integer *);
    static doublereal untime;
    extern /* Subroutine */ int zhetrd_(char *, integer *, doublecomplex *, 
	    integer *, doublereal *, doublereal *, doublecomplex *, 
	    doublecomplex *, integer *, integer *);
    static logical timsub[12];
    extern /* Subroutine */ int zlacpy_(char *, integer *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *);
    static integer ilwork, liwevr, nsplit;
    extern /* Subroutine */ int zlatms_(integer *, integer *, char *, integer 
	    *, char *, doublereal *, integer *, doublereal *, doublereal *, 
	    integer *, integer *, char *, doublecomplex *, integer *, 
	    doublecomplex *, integer *);
    static doublereal ulpinv;
    static logical runtrd;
    extern /* Subroutine */ int zstegr_(char *, char *, integer *, doublereal 
	    *, doublereal *, doublereal *, doublereal *, integer *, integer *,
	     doublereal *, integer *, doublereal *, doublecomplex *, integer *
	    , integer *, doublereal *, integer *, integer *, integer *, 
	    integer *), zstein_(integer *, doublereal *, 
	    doublereal *, integer *, doublereal *, integer *, integer *, 
	    doublecomplex *, integer *, doublereal *, integer *, integer *, 
	    integer *);
    static integer mtypes;
    static logical runhtr;
    extern /* Subroutine */ int zpteqr_(char *, integer *, doublereal *, 
	    doublereal *, doublecomplex *, integer *, doublereal *, integer *), zsteqr_(char *, integer *, doublereal *, doublereal *, 
	    doublecomplex *, integer *, doublereal *, integer *), 
	    zungtr_(char *, integer *, doublecomplex *, integer *, 
	    doublecomplex *, doublecomplex *, integer *, integer *), 
	    zunmtr_(char *, char *, char *, integer *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, doublecomplex *, 
	    integer *, doublecomplex *, integer *, integer *);
    static integer lda, lgn, ldu;
    static doublereal ulp;

    /* Fortran I/O blocks */
    static cilist io___10 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___12 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___34 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___40 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___45 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___47 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___48 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___49 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___51 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___52 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___53 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___61 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___62 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___63 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___64 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___65 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___66 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___67 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___68 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___69 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___70 = { 0, 0, 0, fmt_9997, 0 };



#define times_ref(a_1,a_2,a_3,a_4) times[(((a_4)*times_dim3 + (a_3))*\
times_dim2 + (a_2))*times_dim1 + a_1]
#define subnam_ref(a_0,a_1) &subnam[(a_1)*20 + a_0 - 20]
#define opcnts_ref(a_1,a_2,a_3,a_4) opcnts[(((a_4)*opcnts_dim3 + (a_3))*\
opcnts_dim2 + (a_2))*opcnts_dim1 + a_1]


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 23, 2000   


    Purpose   
    =======   

       ZTIM22 times the LAPACK routines for the complex hermitian   
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
            such as ZHETRD, indicating that only routine CHETRD will   
            be timed, or it may contain a generic name, such as ZST.   
            In this case, the rest of the line is scanned for the   
            first 12 non-blank characters, corresponding to the twelve   
            combinations of subroutine and options:   
            LAPACK:   
               1: ZHETRD   
               2: ZSTEQR(VECT='N')   
               3: ZUNGTR+ZSTEQR(VECT='V') (compare with IMTQL2+HTRIBK)   
               4: ZPTEQR(VECT='N')   
               5: ZUNGTR+ZPTEQR(VECT='V')   
               6. DSTEBZ+ZSTEIN+ZUNMTR   
               7. ZUNGTR+ZSTEDC(COMPQ='V')   
               8. ZSTEDC(COMPQ='I')+ZUNMTR   
               9. ZSTEGR(COMPQ='V')   
            EISPACK:   
              10: HTRIDI (compare with ZHETRD)   
              11: IMTQL1 (compare w/ ZSTEQR -- VECT='N')   
              12: IMTQL2+HTRIBK (compare w/ ZUNGTR+ZSTEQR(VECT='V') )   
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
            X is unitary and D is diagonal with:   
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

    TIMMIN  (input) DOUBLE PRECISION   
            The minimum time a subroutine will be timed.   

    NOUT    (input) INTEGER   
            If NOUT > 0 then NOUT specifies the unit number   
            on which the output will be printed.  If NOUT <= 0, no   
            output is printed.   

    ISEED   (input/output) INTEGER array, dimension( 4 )   
            The random seed used by the random number generator, used   
            by the test matrix generator.  It is used and updated on   
            each call to ZTIM22   

    A       (workspace) COMPLEX*16 array, dimension( max(NN)*max(LDAS) )   
            The original matrix to be tested.   

    D       (workspace) DOUBLE PRECISION array, dimension( max(NN) )   
            The diagonal of the tridiagonal generated by ZHETRD/HTRIDI.   

    E       (workspace) DOUBLE PRECISION array, dimension( max(NN) )   
            The off-diagonal of the tridiagonal generated by   
            ZHETRD/HTRIDI.   

    E2      (workspace) DOUBLE PRECISION array, dimension( max(NN) )   
            The diagonal of a positive definite tridiagonal matrix   
            sent to ZPTEQR.  The off-diagonal is in array E.   

    U       (workspace) COMPLEX*16 array, dimension( max(NN)*max(LDAS) )   
            The array of Householder vectors output by ZHETRD.  This   
            array is used only when URE and UIM are not; thus, on   
            nearly all computers, URE may be EQUIVALENCEd with the   
            first half of U in the main (calling) routine, and UIM with   
            the second half, although this is a violation of the   
            FORTRAN-77 standard.   

    URE     (workspace) DOUBLE PRECISION array,   
                        dimension( max(NN)*max(LDAS) )   
            The array of the real parts of Householder vectors output by   
            HTRIDI.  This array is used only when U is not -- see the   
            note description of U.   

    UIM     (workspace) DOUBLE PRECISION array,   
                        dimension( max(NN)*max(LDAS) )   
            The array of the imaginary parts of Householder vectors   
            output by HTRIDI.  This array is used only when U is not --   
            see the description of U.   

    TAU     (workspace) COMPLEX*16 array, dimension( max(NN) )   
            The vector of coefficients for the Householder   
            transformations output by ZHETRD.  This array is used only   
            when TAURE is not; thus, on nearly all computers, TAURE may   
            be EQUIVALENCEd with TAU in the main (calling) routine,   
            although this is a violation of the FORTRAN-77 standard.   

    TAURE   (workspace) DOUBLE PRECISION array, dimension( 2*max(NN) )   
            The vector of complex (modulus 1) factors output by HTRIDI.   
            This vector is used only when TAU is not -- see the   
            description of TAU.   

    Z       (workspace) COMPLEX*16 array, dimension( max(NN)*max(LDAS) )   
            Various output arrays.  This array is used only when ZRE   
            and ZIM are not; thus, on nearly all computers, ZRE may be   
            EQUIVALENCEd with the first half of Z in the main (calling)   
            routine, and ZIM with the second half, although this is a   
            violation of the FORTRAN-77 standard.   

    ZRE     (workspace) DOUBLE PRECISION array,   
                        dimension( max(NN)*max(LDAS) )   
            Various output arrays (real parts).  This array is used   
            only when Z is not -- see the description of Z.   

    ZIM     (workspace) DOUBLE PRECISION array,   
                        dimension( max(NN)*max(LDAS) )   
            Various output arrays (imaginary parts).  This array is   
            used only when Z is not -- see the description of Z.   

    WORK    (workspace) COMPLEX*16 array, dimension( LWORK )   

    LWORK   (input) INTEGER   
            Number of elements in WORK.  It must be at least   
            max( (NNB + 2 )*LDAS, max(LDAS)*max(LDAS) )   

    RWORK   (workspace) DOUBLE PRECISION array, dimension   
                     ( max( 6*max(LDAS), NSIZES*NTYPES*NPARMS ),   
                       ( 1 + 3 * M + 2 * M * lg M + 3 * M**2 ) ),   
            where  M = max(lDAS), and lg M is the smallest integer k   
            such that 2^k >= N.   
            This should *not* be equivalenced to other arrays.   

    LLWORK  (workspace) LOGICAL array, dimension( NPARMS )   

    IWORK   (workspace) INTEGER array, dimension max( 5*max(LDAS),   
            ( 6 + 6*M + 5 * M * lg M ) ).   

    TIMES   (workspace) DOUBLE PRECISION array,   
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

    OPCNTS  (output) DOUBLE PRECISION array,   
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
    --u;
    --ure;
    --uim;
    --tau;
    --taure;
    --z__;
    --zre;
    --zim;
    --work;
    --rwork;
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

    atimin_("ZST", line, &c__12, subnam, timsub, nout, info, (ftnlen)3, (
	    ftnlen)80, (ftnlen)20);

/*     Disable timing of ZSTEGR if we're non-IEEE-754 compliant. */

    nansok = ilaenv_(&c__10, "ZSTEGR", " ", &c__0, &c__0, &c__0, &c__0, (
	    ftnlen)6, (ftnlen)1);
    infsok = ilaenv_(&c__11, "ZSTEGR", " ", &c__0, &c__0, &c__0, &c__0, (
	    ftnlen)6, (ftnlen)1);
    if (nansok != 1 || infsok != 1) {
	timsub[8] = FALSE_;
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

    ilwork = 0;
    i__1 = *nparms;
    for (j1 = 1; j1 <= i__1; ++j1) {
/* Computing MAX */
	i__2 = ilwork, i__3 = (nnb[j1] + 2) * ldas[j1];
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

/*     Check to see whether ZHETRD must be run.   

       RUNTRD -- if ZHETRD must be run. */

    runtrd = FALSE_;
    if (timsub[1] || timsub[2] || timsub[3] || timsub[4] || timsub[5] || 
	    timsub[6] || timsub[7] || timsub[8]) {
	runtrd = TRUE_;
    }

/*     Check to see whether HTRIDI must be run.   

       RUNHTR -- if HTRIDI must be run. */

    runhtr = FALSE_;
    if (timsub[9] || timsub[10] || timsub[11]) {
	runhtr = TRUE_;
    }

/*     Various Constants */

    ulp = dlamch_("Epsilon") * dlamch_("Base");
    ulpinv = 1. / ulp;
    xlaenv_(&c__9, &c__25);

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
	    lgn = (integer) (log((doublereal) n) / log(2.));
	    if (pow_ii(&c__2, &lgn) < n) {
		++lgn;
	    }
	    if (pow_ii(&c__2, &lgn) < n) {
		++lgn;
	    }
/* Computing 2nd power */
	    i__2 = n;
	    lwedc = (n << 2) + 1 + (n << 1) * lgn + i__2 * i__2 * 3;
/* Computing 2nd power */
	    i__2 = n;
	    lrwedc = n * 3 + 1 + (n << 1) * lgn + i__2 * i__2 * 3;
	    liwedc = n * 6 + 6 + n * 5 * lgn;
	    lwevr = n * 18;
	    liwevr = n * 10;
	} else {
	    lwedc = 8;
	    lrwedc = 7;
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
		goto L640;
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
		zlatms_(&n, &n, "S", &iseed[1], "S", &rwork[1], &imode, &
			ulpinv, &c_b43, &n, &n, uplo, &a[1], &n, &work[1], &
			iinfo);
		if (iinfo != 0) {
		    io___34.ciunit = *nout;
		    s_wsfe(&io___34);
		    do_fio(&c__1, "ZLATMS", (ftnlen)6);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    goto L640;
		}
	    }

/*           Time ZHETRD for each pair NNB(j), LDAS(j) */

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

/*                 Time ZHETRD */

		    ic = 0;
		    latime_1.ops = 0.;
		    s1 = dsecnd_();
L90:
		    zlacpy_(uplo, &n, &n, &a[1], &n, &u[1], &lda);
		    zhetrd_(uplo, &n, &u[1], &lda, &d__[1], &e[1], &tau[1], &
			    work[1], lwork, &iinfo);
		    if (iinfo != 0) {
			io___40.ciunit = *nout;
			s_wsfe(&io___40);
			do_fio(&c__1, subnam_ref(0, 1), (ftnlen)20);
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
			goto L190;
		    }

		    s2 = dsecnd_();
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			goto L90;
		    }

/*                 Subtract the time used in ZLACPY. */

		    s1 = dsecnd_();
		    i__4 = ic;
		    for (j = 1; j <= i__4; ++j) {
			zlacpy_(uplo, &n, &n, &a[1], &n, &z__[1], &lda);
/* L100: */
		    }
		    s2 = dsecnd_();
		    untime = s2 - s1;

/* Computing MAX */
		    d__1 = time - untime;
		    times_ref(ipar, itype, in, 1) = max(d__1,0.) / (
			    doublereal) ic;
		    opcnts_ref(ipar, itype, in, 1) = dopla_("ZHETRD", &n, &
			    c__0, &c__0, &c__0, &nb);
		    ldu = lda;
/* L110: */
		}
	    } else {
		if (runtrd) {
		    zlacpy_(uplo, &n, &n, &a[1], &n, &u[1], &n);
		    zhetrd_(uplo, &n, &u[1], &n, &d__[1], &e[1], &tau[1], &
			    work[1], lwork, &iinfo);
		    if (iinfo != 0) {
			io___45.ciunit = *nout;
			s_wsfe(&io___45);
			do_fio(&c__1, subnam_ref(0, 1), (ftnlen)20);
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
			goto L190;
		    }
		    ldu = n;
		}
	    }

/*           Time ZSTEQR for each distinct LDA=LDAS(j) */

	    if (timsub[1]) {
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
/* L120: */
		    }
		    if (lastl == 0) {

/*                    Time ZSTEQR with VECT='N' */

			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();
L130:
			dcopy_(&n, &d__[1], &c__1, &rwork[1], &c__1);
			i__4 = n - 1;
			dcopy_(&i__4, &e[1], &c__1, &rwork[lda + 1], &c__1);
			zsteqr_("N", &n, &rwork[1], &rwork[lda + 1], &z__[1], 
				&lda, &rwork[(lda << 1) + 1], &iinfo);
			if (iinfo != 0) {
			    io___47.ciunit = *nout;
			    s_wsfe(&io___47);
			    do_fio(&c__1, subnam_ref(0, 2), (ftnlen)20);
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
			    goto L150;
			}
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L130;
			}

/*                    Subtract the time used in DCOPY. */

			s1 = dsecnd_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    dcopy_(&n, &d__[1], &c__1, &rwork[1], &c__1);
			    i__5 = n - 1;
			    dcopy_(&i__5, &e[1], &c__1, &rwork[lda + 1], &
				    c__1);
/* L140: */
			}
			s2 = dsecnd_();
			untime = s2 - s1;

/* Computing MAX */
			d__1 = time - untime;
			times_ref(ipar, itype, in, 2) = max(d__1,0.) / (
				doublereal) ic;
			opcnts_ref(ipar, itype, in, 2) = latime_1.ops / (
				doublereal) ic;
		    } else {
			opcnts_ref(ipar, itype, in, 2) = opcnts_ref(lastl, 
				itype, in, 2);
			times_ref(ipar, itype, in, 2) = times_ref(lastl, 
				itype, in, 2);
		    }
L150:
		    ;
		}
	    }

/*           Time ZUNGTR + ZSTEQR(VECT='V') for each pair NNB(j), LDAS(j) */

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

/*                 Time ZUNGTR + ZSTEQR */

		    ic = 0;
		    latime_1.ops = 0.;
		    s1 = dsecnd_();
L160:
		    zlacpy_("L", &n, &n, &a[1], &n, &z__[1], &lda);
		    zungtr_("L", &n, &z__[1], &lda, &tau[1], &work[1], lwork, 
			    &iinfo);
		    if (iinfo != 0) {
			io___48.ciunit = *nout;
			s_wsfe(&io___48);
			do_fio(&c__1, "ZUNGTR", (ftnlen)6);
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
			goto L180;
		    }
		    dcopy_(&n, &d__[1], &c__1, &rwork[1], &c__1);
		    i__4 = n - 1;
		    dcopy_(&i__4, &e[1], &c__1, &rwork[lda + 1], &c__1);
		    zsteqr_("V", &n, &rwork[1], &rwork[lda + 1], &z__[1], &
			    lda, &rwork[(lda << 1) + 1], &iinfo);
		    if (iinfo != 0) {
			io___49.ciunit = *nout;
			s_wsfe(&io___49);
			do_fio(&c__1, subnam_ref(0, 3), (ftnlen)20);
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
			goto L180;
		    }

		    s2 = dsecnd_();
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			goto L160;
		    }

/*                 Subtract the time used in ZLACPY. */

		    s1 = dsecnd_();
		    i__4 = ic;
		    for (j = 1; j <= i__4; ++j) {
			dcopy_(&n, &d__[1], &c__1, &rwork[1], &c__1);
			i__5 = n - 1;
			dcopy_(&i__5, &e[1], &c__1, &rwork[lda + 1], &c__1);
			zlacpy_("L", &n, &n, &a[1], &n, &z__[1], &lda);
/* L170: */
		    }
		    s2 = dsecnd_();
		    untime = s2 - s1;

/* Computing MAX */
		    d__1 = time - untime;
		    times_ref(ipar, itype, in, 3) = max(d__1,0.) / (
			    doublereal) ic;
		    opcnts_ref(ipar, itype, in, 3) = latime_1.ops / (
			    doublereal) ic;
		    ldu = lda;
L180:
		    ;
		}
	    }

L190:

/*           Time ZPTEQR for each distinct LDA=LDAS(j) */

	    if (timsub[3]) {
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
/* L200: */
		    }
		    if (lastl == 0) {

/*                    Time ZPTEQR with VECT='N'   


                      Modify the tridiagonal matrix to make it   
                      positive definite. */
			e2[1] = abs(d__[1]) + abs(e[1]);
			i__4 = n - 1;
			for (i__ = 2; i__ <= i__4; ++i__) {
			    e2[i__] = (d__1 = d__[i__], abs(d__1)) + (d__2 = 
				    e[i__], abs(d__2)) + (d__3 = e[i__ - 1], 
				    abs(d__3));
/* L210: */
			}
			e2[n] = (d__1 = d__[n], abs(d__1)) + (d__2 = e[n - 1],
				 abs(d__2));
			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();
L220:
			dcopy_(&n, &e2[1], &c__1, &rwork[1], &c__1);
			i__4 = n - 1;
			dcopy_(&i__4, &e[1], &c__1, &rwork[lda + 1], &c__1);
			zpteqr_("N", &n, &rwork[1], &rwork[lda + 1], &z__[1], 
				&lda, &rwork[(lda << 1) + 1], &iinfo);
			if (iinfo != 0) {
			    io___51.ciunit = *nout;
			    s_wsfe(&io___51);
			    do_fio(&c__1, subnam_ref(0, 4), (ftnlen)20);
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
			    goto L240;
			}
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L220;
			}

/*                    Subtract the time used in DCOPY. */

			s1 = dsecnd_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    dcopy_(&n, &e2[1], &c__1, &rwork[1], &c__1);
			    i__5 = n - 1;
			    dcopy_(&i__5, &e[1], &c__1, &rwork[lda + 1], &
				    c__1);
/* L230: */
			}
			s2 = dsecnd_();
			untime = s2 - s1;

/* Computing MAX */
			d__1 = time - untime;
			times_ref(ipar, itype, in, 4) = max(d__1,0.) / (
				doublereal) ic;
			opcnts_ref(ipar, itype, in, 4) = latime_1.ops / (
				doublereal) ic;
		    } else {
			opcnts_ref(ipar, itype, in, 4) = opcnts_ref(lastl, 
				itype, in, 4);
			times_ref(ipar, itype, in, 4) = times_ref(lastl, 
				itype, in, 4);
		    }
L240:
		    ;
		}
	    }

/*           Time ZUNGTR + ZPTEQR(VECT='V') for each pair NNB(j), LDAS(j) */

	    if (timsub[4]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
/* Computing MIN */
		    i__4 = n, i__5 = nnb[ipar];
		    nb = min(i__4,i__5);
		    xlaenv_(&c__1, &nb);
		    xlaenv_(&c__2, &c__2);
		    xlaenv_(&c__3, &nb);

/*                 Time ZUNGTR + ZPTEQR */

		    ic = 0;
		    latime_1.ops = 0.;
		    s1 = dsecnd_();
L250:
		    zlacpy_("L", &n, &n, &a[1], &n, &z__[1], &lda);
		    zungtr_("L", &n, &z__[1], &lda, &tau[1], &work[1], lwork, 
			    &iinfo);
		    if (iinfo != 0) {
			io___52.ciunit = *nout;
			s_wsfe(&io___52);
			do_fio(&c__1, "ZUNGTR", (ftnlen)6);
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
			goto L290;
		    }

/*                 Modify the tridiagonal matrix to make it   
                   positive definite. */
		    e2[1] = abs(d__[1]) + abs(e[1]);
		    i__4 = n - 1;
		    for (i__ = 2; i__ <= i__4; ++i__) {
			e2[i__] = (d__1 = d__[i__], abs(d__1)) + (d__2 = e[
				i__], abs(d__2)) + (d__3 = e[i__ - 1], abs(
				d__3));
/* L260: */
		    }
		    e2[n] = (d__1 = d__[n], abs(d__1)) + (d__2 = e[n - 1], 
			    abs(d__2));

		    dcopy_(&n, &e2[1], &c__1, &rwork[1], &c__1);
		    i__4 = n - 1;
		    dcopy_(&i__4, &e[1], &c__1, &rwork[lda + 1], &c__1);
		    zpteqr_("V", &n, &rwork[1], &rwork[lda + 1], &z__[1], &
			    lda, &rwork[(lda << 1) + 1], &iinfo);
		    if (iinfo != 0) {
			io___53.ciunit = *nout;
			s_wsfe(&io___53);
			do_fio(&c__1, subnam_ref(0, 5), (ftnlen)20);
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
			goto L290;
		    }

		    s2 = dsecnd_();
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			goto L250;
		    }

/*                 Subtract the time used in ZLACPY. */

		    s1 = dsecnd_();
		    i__4 = ic;
		    for (j = 1; j <= i__4; ++j) {
			e2[1] = abs(d__[1]) + abs(e[1]);
			i__5 = n - 1;
			for (i__ = 2; i__ <= i__5; ++i__) {
			    e2[i__] = (d__1 = d__[i__], abs(d__1)) + (d__2 = 
				    e[i__], abs(d__2)) + (d__3 = e[i__ - 1], 
				    abs(d__3));
/* L270: */
			}
			e2[n] = (d__1 = d__[n], abs(d__1)) + (d__2 = e[n - 1],
				 abs(d__2));

			dcopy_(&n, &e2[1], &c__1, &rwork[1], &c__1);
			i__5 = n - 1;
			dcopy_(&i__5, &e[1], &c__1, &rwork[lda + 1], &c__1);
			zlacpy_("L", &n, &n, &a[1], &n, &z__[1], &lda);
/* L280: */
		    }
		    s2 = dsecnd_();
		    untime = s2 - s1;

/* Computing MAX */
		    d__1 = time - untime;
		    times_ref(ipar, itype, in, 5) = max(d__1,0.) / (
			    doublereal) ic;
		    opcnts_ref(ipar, itype, in, 5) = latime_1.ops / (
			    doublereal) ic;
		    ldu = lda;
L290:
		    ;
		}
	    }

/*           Time DSTEBZ+ZSTEIN+ZUNMTR for each pair NNB(j), LDAS(j) */

	    if (timsub[5]) {
		vl = 0.;
		vu = 0.;
		il = 1;
		iu = n;
		abstol = 0.;
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
/* Computing MIN */
		    i__4 = n, i__5 = nnb[ipar];
		    nb = min(i__4,i__5);
		    xlaenv_(&c__1, &nb);
		    xlaenv_(&c__2, &c__2);

/*                 Time DSTEBZ + ZSTEIN + ZUNMTR */

		    ic = 0;
		    latime_1.ops = 0.;
		    s1 = dsecnd_();
L300:

		    dstebz_("A", "B", &n, &vl, &vu, &il, &iu, &abstol, &d__[1]
			    , &e[1], &m, &nsplit, &rwork[1], &iwork[1], &
			    iwork[n + 1], &rwork[(n << 1) + 1], &iwork[(n << 
			    1) + 1], &iinfo);
		    if (iinfo != 0) {
			io___61.ciunit = *nout;
			s_wsfe(&io___61);
			do_fio(&c__1, "DSTEBZ", (ftnlen)6);
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
			goto L310;
		    }

		    zstein_(&n, &d__[1], &e[1], &n, &rwork[1], &iwork[1], &
			    iwork[n + 1], &z__[1], &lda, &rwork[n + 1], &
			    iwork[(n << 1) + 1], &iwork[n * 3 + 1], &iinfo);
		    if (iinfo != 0) {
			io___62.ciunit = *nout;
			s_wsfe(&io___62);
			do_fio(&c__1, subnam_ref(0, 6), (ftnlen)20);
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
			goto L310;
		    }

		    zunmtr_("L", "L", "N", &n, &n, &u[1], &ldu, &tau[1], &z__[
			    1], &lda, &work[1], lwork, &iinfo);
		    if (iinfo != 0) {
			io___63.ciunit = *nout;
			s_wsfe(&io___63);
			do_fio(&c__1, "ZUNMTR", (ftnlen)6);
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
			goto L310;
		    }

		    s2 = dsecnd_();
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			goto L300;
		    }
		    untime = 0.;

/* Computing MAX */
		    d__1 = time - untime;
		    times_ref(ipar, itype, in, 6) = max(d__1,0.) / (
			    doublereal) ic;
		    opcnts_ref(ipar, itype, in, 6) = latime_1.ops / (
			    doublereal) ic;
		    ldu = lda;
L310:
		    ;
		}
	    }

/*           Time ZUNGTR + ZSTEDC(COMPQ='V') for each pair NNB(j),   
             LDAS(j) */

	    if (timsub[6]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
/* Computing MIN */
		    i__4 = n, i__5 = nnb[ipar];
		    nb = min(i__4,i__5);
		    xlaenv_(&c__1, &nb);
		    xlaenv_(&c__2, &c__2);
		    xlaenv_(&c__3, &nb);

/*                 Time ZUNGTR + ZSTEDC */

		    ic = 0;
		    latime_1.ops = 0.;
		    s1 = dsecnd_();
L320:
		    zlacpy_("L", &n, &n, &a[1], &n, &z__[1], &lda);
		    zungtr_("L", &n, &z__[1], &lda, &tau[1], &work[1], lwork, 
			    &iinfo);
		    if (iinfo != 0) {
			io___64.ciunit = *nout;
			s_wsfe(&io___64);
			do_fio(&c__1, "ZUNGTR", (ftnlen)6);
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
			goto L340;
		    }
		    dcopy_(&n, &d__[1], &c__1, &rwork[1], &c__1);
		    i__4 = n - 1;
		    dcopy_(&i__4, &e[1], &c__1, &rwork[lda + 1], &c__1);
		    zstedc_("V", &n, &rwork[1], &rwork[lda + 1], &z__[1], &
			    lda, &work[1], &lwedc, &rwork[(lda << 1) + 1], &
			    lrwedc, &iwork[1], &liwedc, &iinfo);
		    if (iinfo != 0) {
			io___65.ciunit = *nout;
			s_wsfe(&io___65);
			do_fio(&c__1, subnam_ref(0, 7), (ftnlen)20);
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
			goto L340;
		    }

		    s2 = dsecnd_();
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			goto L320;
		    }

/*                 Subtract the time used in ZLACPY. */

		    s1 = dsecnd_();
		    i__4 = ic;
		    for (j = 1; j <= i__4; ++j) {
			dcopy_(&n, &d__[1], &c__1, &rwork[1], &c__1);
			i__5 = n - 1;
			dcopy_(&i__5, &e[1], &c__1, &rwork[lda + 1], &c__1);
			zlacpy_("L", &n, &n, &a[1], &n, &z__[1], &lda);
/* L330: */
		    }
		    s2 = dsecnd_();
		    untime = s2 - s1;

/* Computing MAX */
		    d__1 = time - untime;
		    times_ref(ipar, itype, in, 7) = max(d__1,0.) / (
			    doublereal) ic;
		    opcnts_ref(ipar, itype, in, 7) = latime_1.ops / (
			    doublereal) ic;
		    ldu = lda;
L340:
		    ;
		}
	    }

/*           Time ZSTEDC(COMPQ='I') + ZUNMTR for each pair NNB(j),   
             LDAS(j) */

	    if (timsub[7]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
/* Computing MIN */
		    i__4 = n, i__5 = nnb[ipar];
		    nb = min(i__4,i__5);
		    xlaenv_(&c__1, &nb);
		    xlaenv_(&c__2, &c__2);
		    xlaenv_(&c__3, &nb);

/*                 Time  ZSTEDC + ZUNMTR */

		    ic = 0;
		    latime_1.ops = 0.;
		    s1 = dsecnd_();
L350:
		    dcopy_(&n, &d__[1], &c__1, &rwork[1], &c__1);
		    i__4 = n - 1;
		    dcopy_(&i__4, &e[1], &c__1, &rwork[lda + 1], &c__1);
		    zstedc_("I", &n, &rwork[1], &rwork[lda + 1], &z__[1], &
			    lda, &work[1], &lwedc, &rwork[(lda << 1) + 1], &
			    lrwedc, &iwork[1], &liwedc, &iinfo);
		    if (iinfo != 0) {
			io___66.ciunit = *nout;
			s_wsfe(&io___66);
			do_fio(&c__1, subnam_ref(0, 8), (ftnlen)20);
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
			goto L370;
		    }

		    zunmtr_("L", "L", "N", &n, &n, &u[1], &ldu, &tau[1], &z__[
			    1], &lda, &work[1], lwork, &iinfo);
		    if (iinfo != 0) {
			io___67.ciunit = *nout;
			s_wsfe(&io___67);
			do_fio(&c__1, "ZUNMTR", (ftnlen)6);
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
			goto L370;
		    }

		    s2 = dsecnd_();
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			goto L350;
		    }

/*                 Subtract the time used in DCOPY. */

		    s1 = dsecnd_();
		    i__4 = ic;
		    for (j = 1; j <= i__4; ++j) {
			dcopy_(&n, &d__[1], &c__1, &rwork[1], &c__1);
			i__5 = n - 1;
			dcopy_(&i__5, &e[1], &c__1, &rwork[lda + 1], &c__1);
/* L360: */
		    }
		    s2 = dsecnd_();
		    untime = s2 - s1;

/* Computing MAX */
		    d__1 = time - untime;
		    times_ref(ipar, itype, in, 8) = max(d__1,0.) / (
			    doublereal) ic;
		    opcnts_ref(ipar, itype, in, 8) = latime_1.ops / (
			    doublereal) ic;
		    ldu = lda;
L370:
		    ;
		}
	    }

/*           Time ZSTEGR(COMPQ='V') for each pair NNB(j), LDAS(j) */

	    if (timsub[8]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
/* Computing MIN */
		    i__4 = n, i__5 = nnb[ipar];
		    nb = min(i__4,i__5);
		    xlaenv_(&c__1, &nb);
		    xlaenv_(&c__2, &c__2);
		    xlaenv_(&c__3, &nb);

		    abstol = 0.;
		    vl = 0.;
		    vu = 0.;
		    il = 1;
		    iu = n;
		    ic = 0;
		    latime_1.ops = 0.;
		    s1 = dsecnd_();
L380:
		    dcopy_(&n, &d__[1], &c__1, &rwork[1], &c__1);
		    i__4 = n - 1;
		    dcopy_(&i__4, &e[1], &c__1, &rwork[lda + 1], &c__1);
		    zstegr_("V", "A", &n, &rwork[1], &rwork[lda + 1], &vl, &
			    vu, &il, &iu, &abstol, &m, &rwork[(lda << 1) + 1],
			     &z__[1], &lda, &iwork[1], &rwork[lda * 3 + 1], &
			    lwevr, &iwork[(lda << 1) + 1], &liwevr, info);
		    if (iinfo != 0) {
			io___68.ciunit = *nout;
			s_wsfe(&io___68);
			do_fio(&c__1, subnam_ref(0, 9), (ftnlen)20);
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
			goto L400;
		    }
		    s2 = dsecnd_();
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			goto L380;
		    }

/*                 Subtract the time used in DCOPY. */

		    s1 = dsecnd_();
		    i__4 = ic;
		    for (j = 1; j <= i__4; ++j) {
			dcopy_(&n, &d__[1], &c__1, &rwork[1], &c__1);
			i__5 = n - 1;
			dcopy_(&i__5, &e[1], &c__1, &rwork[lda + 1], &c__1);
/* L390: */
		    }
		    s2 = dsecnd_();
		    untime = s2 - s1;

/* Computing MAX */
		    d__1 = time - untime;
		    times_ref(ipar, itype, in, 9) = max(d__1,0.) / (
			    doublereal) ic;
		    opcnts_ref(ipar, itype, in, 9) = latime_1.ops / (
			    doublereal) ic;
L400:
		    ;
		}
	    }

/* -----------------------------------------------------------------------   

             Time the EISPACK Routines   

             Skip routines if N <= 0 (EISPACK requirement) */

	    if (n <= 0) {
		goto L640;
	    }

/*           Time HTRIDI for each LDAS(j) */

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
/* L410: */
		    }

		    if (lastl == 0) {

/*                    Time HTRIDI */

			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();
L420:
			i__4 = n - 1;
			for (j2 = 0; j2 <= i__4; ++j2) {
			    i__5 = n;
			    for (j1 = 1; j1 <= i__5; ++j1) {
				i__6 = j1 + n * j2;
				ure[j1 + lda * j2] = a[i__6].r;
				uim[j1 + lda * j2] = d_imag(&a[j1 + n * j2]);
/* L430: */
			    }
/* L440: */
			}
			htridi_(&lda, &n, &ure[1], &uim[1], &d__[1], &e[1], &
				rwork[1], &taure[1]);
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L420;
			}

/*                    Subtract the time used in copying A. */

			s1 = dsecnd_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    i__5 = n - 1;
			    for (j2 = 0; j2 <= i__5; ++j2) {
				i__6 = n;
				for (j1 = 1; j1 <= i__6; ++j1) {
				    i__7 = j1 + n * j2;
				    zre[j1 + lda * j2] = a[i__7].r;
				    zim[j1 + lda * j2] = d_imag(&a[j1 + n * 
					    j2]);
/* L450: */
				}
/* L460: */
			    }
/* L470: */
			}
			s2 = dsecnd_();
			untime = s2 - s1;
/* Computing MAX */
			d__1 = time - untime;
			times_ref(ipar, itype, in, 10) = max(d__1,0.) / (
				doublereal) ic;
			opcnts_ref(ipar, itype, in, 10) = latime_1.ops / (
				doublereal) ic;
			ldu = lda;
		    } else {
			opcnts_ref(ipar, itype, in, 10) = opcnts_ref(lastl, 
				itype, in, 10);
			times_ref(ipar, itype, in, 10) = times_ref(lastl, 
				itype, in, 10);
		    }
/* L480: */
		}
	    } else {
		if (runhtr) {
		    i__3 = n - 1;
		    for (j2 = 0; j2 <= i__3; ++j2) {
			i__4 = n;
			for (j1 = 1; j1 <= i__4; ++j1) {
			    i__5 = j1 + n * j2;
			    ure[j1 + n * j2] = a[i__5].r;
			    uim[j1 + n * j2] = d_imag(&a[j1 + n * j2]);
/* L490: */
			}
/* L500: */
		    }
		    htridi_(&n, &n, &ure[1], &uim[1], &d__[1], &e[1], &rwork[
			    1], &taure[1]);
		    ldu = n;
		}
	    }

/*           Time IMTQL1 for each LDAS(j) */

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
/* L510: */
		    }

		    if (lastl == 0) {

/*                    Time IMTQL1 */

			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();
L520:
			dcopy_(&n, &d__[1], &c__1, &rwork[1], &c__1);
			i__4 = n - 1;
			dcopy_(&i__4, &e[1], &c__1, &rwork[lda + 1], &c__1);
			imtql1_(&n, &rwork[1], &rwork[lda + 1], &iinfo);
			if (iinfo != 0) {
			    io___69.ciunit = *nout;
			    s_wsfe(&io___69);
			    do_fio(&c__1, subnam_ref(0, 11), (ftnlen)20);
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
			    goto L550;
			}
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L520;
			}

/*                    Subtract the time used in DCOPY */

			s1 = dsecnd_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    dcopy_(&n, &d__[1], &c__1, &rwork[1], &c__1);
			    i__5 = n - 1;
			    dcopy_(&i__5, &e[1], &c__1, &rwork[lda + 1], &
				    c__1);
/* L530: */
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
/* L540: */
		}
	    }
L550:

/*           Time IMTQL2 + HTRIBK for each LDAS(j) */

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
/* L560: */
		    }

		    if (lastl == 0) {

/*                    Change leading dimension of U */

			if (lda > ldu) {
			    for (j2 = n - 1; j2 >= 1; --j2) {
				for (j1 = n; j1 >= 1; --j1) {
				    ure[j1 + lda * j2] = ure[j1 + ldu * j2];
				    uim[j1 + lda * j2] = uim[j1 + ldu * j2];
/* L570: */
				}
/* L580: */
			    }
			    ldu = lda;
			} else if (lda < ldu) {
			    i__4 = n - 1;
			    for (j2 = 1; j2 <= i__4; ++j2) {
				i__5 = n;
				for (j1 = 1; j1 <= i__5; ++j1) {
				    ure[j1 + lda * j2] = ure[j1 + ldu * j2];
				    uim[j1 + lda * j2] = uim[j1 + ldu * j2];
/* L590: */
				}
/* L600: */
			    }
			    ldu = lda;
			}

/*                    Time IMTQL2 + HTRIBK */

			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();
L610:
			dcopy_(&n, &d__[1], &c__1, &rwork[1], &c__1);
			i__4 = n - 1;
			dcopy_(&i__4, &e[1], &c__1, &rwork[lda + 1], &c__1);
			dlaset_("Full", &n, &n, &c_b342, &c_b43, &zre[1], &
				lda);
			imtql2_(&lda, &n, &rwork[1], &rwork[lda + 1], &zre[1],
				 &iinfo);
			if (iinfo != 0) {
			    io___70.ciunit = *nout;
			    s_wsfe(&io___70);
			    do_fio(&c__1, subnam_ref(0, 12), (ftnlen)20);
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
			    goto L640;
			}
			htribk_(&lda, &n, &ure[1], &uim[1], &taure[1], &n, &
				zre[1], &zim[1]);
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L610;
			}

/*                    Subtract the time used in copying */

			s1 = dsecnd_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    dcopy_(&n, &d__[1], &c__1, &rwork[1], &c__1);
			    i__5 = n - 1;
			    dcopy_(&i__5, &e[1], &c__1, &rwork[lda + 1], &
				    c__1);
			    dlaset_("Full", &n, &n, &c_b342, &c_b43, &zre[1], 
				    &lda);
/* L620: */
			}
			s2 = dsecnd_();
			untime = s2 - s1;

/* Computing MAX */
			d__1 = time - untime;
			times_ref(ipar, itype, in, 12) = max(d__1,0.) / (
				doublereal) ic;
			opcnts_ref(ipar, itype, in, 12) = latime_1.ops / (
				doublereal) ic;
		    } else {
			opcnts_ref(ipar, itype, in, 12) = opcnts_ref(lastl, 
				itype, in, 12);
			times_ref(ipar, itype, in, 12) = times_ref(lastl, 
				itype, in, 12);
		    }
/* L630: */
		}
	    }

L640:
	    ;
	}
/* L650: */
    }

/* -----------------------------------------------------------------------   

       Print a table of results for each timed routine. */

    for (isub = 1; isub <= 12; ++isub) {
	if (timsub[isub - 1]) {
	    dprtbe_(subnam_ref(0, isub), &mtypes, &dotype[1], nsizes, &nn[1], 
		    &inparm[isub - 1], pnames, nparms, &ldas[1], &nnb[1], 
		    idumma, idumma, &opcnts_ref(1, 1, 1, isub), ldo1, ldo2, &
		    times_ref(1, 1, 1, isub), ldt1, ldt2, &rwork[1], &llwork[
		    1], nout, (ftnlen)20, (ftnlen)4);
	}
/* L660: */
    }


    return 0;

/*     End of ZTIM22 */

} /* ztim22_ */

#undef opcnts_ref
#undef subnam_ref
#undef times_ref


