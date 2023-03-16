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

static integer c__18 = 18;
static integer c__1 = 1;
static integer c__3 = 3;
static logical c_true = TRUE_;
static real c_b26 = 1.f;
static integer c__2 = 2;
static integer c__8 = 8;
static logical c_false = FALSE_;
static integer c__4 = 4;
static integer c__0 = 0;
static integer c__5 = 5;
static real c_b419 = 0.f;

/* Subroutine */ int ctim51_(char *line, integer *nsizes, integer *nn, 
	integer *ntypes, logical *dotype, integer *nparms, integer *nnb, 
	integer *nshfts, integer *neisps, integer *minnbs, integer *minbks, 
	integer *ldas, real *timmin, integer *nout, integer *iseed, complex *
	a, real *ar, real *ai, complex *b, real *br, real *bi, complex *h__, 
	real *hr, real *hi, complex *t, real *tr, real *ti, complex *q, real *
	qr, real *qi, complex *z__, real *zr, real *zi, complex *w, real *wr, 
	complex *work, integer *lwork, real *rwork, logical *llwork, real *
	times, integer *ldt1, integer *ldt2, integer *ldt3, real *opcnts, 
	integer *ldo1, integer *ldo2, integer *ldo3, integer *info, ftnlen 
	line_len)
{
    /* Initialized data */

    static char subnam[11*18] = "CGGHRD(N)  " "CGGHRD(Q)  " "CGGHRD(Z)  " 
	    "CGGHRD(Q,Z)" "CHGEQZ(E)  " "CHGEQZ(S)  " "CHGEQZ(Q)  " "CHGEQZ("
	    "Z)  " "CHGEQZ(Q,Z)" "CTGEVC(L,A)" "CTGEVC(L,B)" "CTGEVC(R,A)" 
	    "CTGEVC(R,B)" "CQZHES(F)  " "CQZHES(T)  " "CQZVAL(F)  " "CQZVAL("
	    "T)  " "CQZVEC     ";
    static integer inparm[18] = { 2,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1 };
    static char pnames[6*6] = "   LDA" "    NB" "    NS" " NEISP" " MINNB" 
	    "MINBLK";
    static integer katype[4] = { 5,8,7,9 };

    /* Format strings */
    static char fmt_9999[] = "(1x,a,\002 timing run not attempted -- N > LD"
	    "A\002,/)";
    static char fmt_9998[] = "(1x,a,\002 timing run not attempted -- LWORK t"
	    "oo small.\002,/)";
    static char fmt_9997[] = "(\002 CTIM51: \002,a,\002 returned INFO=\002,i"
	    "6,\002.\002,/9x,\002N=\002,i6,\002, ITYPE=\002,i6,\002, IPAR="
	    "\002,i6,\002, ISEED=(\002,3(i5,\002,\002),i5,\002)\002)";

    /* System generated locals */
    integer opcnts_dim1, opcnts_dim2, opcnts_dim3, opcnts_offset, times_dim1, 
	    times_dim2, times_dim3, times_offset, i__1, i__2, i__3, i__4, 
	    i__5, i__6, i__7, i__8;
    real r__1, r__2;
    complex q__1, q__2, q__3, q__4;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);
    double r_sign(real *, real *), c_abs(complex *);
    void r_cnjg(complex *, complex *);
    double r_imag(complex *);

    /* Local variables */
    static integer ipar;
    static real time;
    static integer isub, nmax, j, n, minnb, iinfo;
    static complex ctemp;
    static integer itemp, lastl, neisp;
    extern doublereal sopla_(char *, integer *, integer *, integer *, integer 
	    *, integer *);
    static logical runeq, runes;
    static integer itype, j1, j2, j3, j4, n1;
    static real s1, s2;
    static logical runqz;
    extern /* Subroutine */ int clatm4_(integer *, integer *, integer *, 
	    integer *, logical *, real *, real *, real *, integer *, integer *
	    , complex *, integer *), cunm2r_(char *, char *, integer *, 
	    integer *, integer *, complex *, integer *, complex *, complex *, 
	    integer *, complex *, integer *);
    static integer ic, jc, nb, in, jr;
    extern /* Subroutine */ int clarfg_(integer *, complex *, complex *, 
	    integer *, complex *);
    static integer ldamin;
    extern doublereal slamch_(char *);
    extern /* Complex */ VOID clarnd_(complex *, integer *, integer *);
    extern doublereal second_(void);
    static integer minblk, ioldsd[4];
    extern /* Subroutine */ int clacpy_(char *, integer *, integer *, complex 
	    *, integer *, complex *, integer *), ctgevc_(char *, char 
	    *, logical *, integer *, complex *, integer *, complex *, integer 
	    *, complex *, integer *, complex *, integer *, integer *, integer 
	    *, complex *, real *, integer *), atimin_(char *, 
	    char *, integer *, char *, logical *, integer *, integer *, 
	    ftnlen, ftnlen, ftnlen), chgeqz_(char *, char *, char *, integer *
	    , integer *, integer *, complex *, integer *, complex *, integer *
	    , complex *, complex *, complex *, integer *, complex *, integer *
	    , complex *, integer *, real *, integer *)
	    ;
    static integer nbsmax;
    extern /* Subroutine */ int claqzh_(logical *, logical *, integer *, 
	    integer *, integer *, complex *, integer *, complex *, integer *, 
	    complex *, integer *, complex *, integer *, complex *, integer *);
    static integer nshift;
    extern /* Subroutine */ int cqzvec_(integer *, integer *, real *, real *, 
	    real *, real *, real *, real *, real *, real *, real *), cqzhes_(
	    integer *, integer *, real *, real *, real *, real *, logical *, 
	    real *, real *);
    static logical runhrd;
    static real untime;
    static logical runhes, timsub[18];
    extern /* Subroutine */ int cqzval_(integer *, integer *, real *, real *, 
	    real *, real *, real *, real *, real *, real *, logical *, real *,
	     real *, integer *), slacpy_(char *, integer *, integer *, real *,
	     integer *, real *, integer *), slaset_(char *, integer *,
	     integer *, real *, real *, real *, integer *), sprtbg_(
	    char *, integer *, logical *, integer *, integer *, integer *, 
	    char *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, real *, integer *, integer *, real *, 
	    integer *, integer *, real *, logical *, integer *, ftnlen, 
	    ftnlen), xlaenv_(integer *, integer *);
    static integer mtypes, lda, ldh, ldq, lds, ldw;
    static real ulp;

    /* Fortran I/O blocks */
    static cilist io___10 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___11 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___38 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___43 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___44 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___45 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___46 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___47 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___50 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___51 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___52 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___53 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___58 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___59 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___61 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___62 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___63 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___64 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___65 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___67 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___68 = { 0, 0, 0, fmt_9997, 0 };



#define times_ref(a_1,a_2,a_3,a_4) times[(((a_4)*times_dim3 + (a_3))*\
times_dim2 + (a_2))*times_dim1 + a_1]
#define subnam_ref(a_0,a_1) &subnam[(a_1)*11 + a_0 - 11]
#define opcnts_ref(a_1,a_2,a_3,a_4) opcnts[(((a_4)*opcnts_dim3 + (a_3))*\
opcnts_dim2 + (a_2))*opcnts_dim1 + a_1]


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    CTIM51 times the LAPACK routines for the COMPLEX non-symmetric   
    generalized eigenvalue problem   A x = w B x.   

    For each N value in NN(1:NSIZES) and .TRUE. value in   
    DOTYPE(1:NTYPES), a pair of matrices will be generated and used to   
    test the selected routines.  Thus, NSIZES*(number of .TRUE. values   
    in DOTYPE) matrices will be generated.   

    Arguments   
    =========   

    LINE    (input) CHARACTER*80   
            The input line which requested this routine.  This line may   
            contain a subroutine name, such as CGGHRD, indicating that   
            only routine CGGHRD will be timed, or it may contain a   
            generic name, such as CHG.  In this case, the rest of the   
            line is scanned for the first 18 non-blank characters,   
            corresponding to the eighteen combinations of subroutine and   
            options:   
            LAPACK:                                     Table Heading:   
             1: CGGHRD(no Q, no Z) (+CGEQRF, etc.)      'CGGHRD(N)'   
             2: CGGHRD(Q only)     (+CGEQRF, etc.)      'CGGHRD(Q)'   
             3: CGGHRD(Z only)     (+CGEQRF, etc.)      'CGGHRD(Z)'   
             4: CGGHRD(Q and Z)    (+CGEQRF, etc.)      'CGGHRD(Q,Z)'   
             5: CHGEQZ(Eigenvalues only)                'CHGEQZ(E)'   
             6: CHGEQZ(Schur form only)                 'CHGEQZ(S)'   
             7: CHGEQZ(Schur form and Q)                'CHGEQZ(Q)'   
             8: CHGEQZ(Schur form and Z)                'CHGEQZ(Z)'   
             9: CHGEQZ(Schur form, Q and Z)             'CHGEQZ(Q,Z)'   
            10: CTGEVC(SIDE='L', HOWMNY='A')            'CTGEVC(L,A)'   
            11: CTGEVC(SIDE='L', HOWMNY='B')            'CTGEVC(L,B)'   
            12: CTGEVC(SIDE='R', HOWMNY='A')            'CTGEVC(R,A)'   
            13: CTGEVC(SIDE='R', HOWMNY='B')            'CTGEVC(R,B)'   
            EISPACK:                       Compare w/:  Table Heading:   
            14: CQZHES w/ matz=.false.            1      'CQZHES(F)'   
            15: CQZHES w/ matz=.true.             3      'CQZHES(T)'   
            16: CQZVAL w/ matz=.false.            5      'CQZVAL(F)'   
            17: CQZVAL w/ matz=.true.             8      'CQZVAL(T)'   
            18: CQZVEC                           13      'CQZVEC'   
            If a character is 'T' or 't', the corresponding routine in   
            this path is timed.  If the entire line is blank, all the   
            routines in the path are timed.   

            Note that since QZHES does more than SGGHRD, the   
            "SGGHRD" timing also includes the time for the calls   
            to SGEQRF, SORMQR, and (if Q is computed) SORGQR   
            which are necessary to get the same functionality   
            as QZHES.   

    NSIZES  (input) INTEGER   
            The number of values of N contained in the vector NN.   

    NN      (input) INTEGER array, dimension (NSIZES)   
            The values of the matrix size N to be tested.  For each   
            N value in the array NN, and each .TRUE. value in DOTYPE,   
            a matrix A will be generated and used to test the routines.   

    NTYPES  (input) INTEGER   
            The number of types in DOTYPE.  Only the first MAXTYP   
            elements will be examined.  Exception: if NSIZES=1 and   
            NTYPES=MAXTYP+1, and DOTYPE=MAXTYP*f,t, then the input   
            value of A will be used.   

    DOTYPE  (input) LOGICAL   
            If DOTYPE(j) is .TRUE., then a pair of matrices (A,B) of   
            type j will be generated.  A and B have the form  U T1 V   
            and  U T2 V , resp., where U and V are orthogonal, T1 and   
            T2 are upper triangular.  T2 has random O(1) entries in the   
            strict upper triangle and ( 0, 1, 0, 1, 1, ..., 1, 0 ) on   
            the diagonal, while T1 has random O(1) entries in the strict   
            upper triangle, its diagonal will have the values:   
            (j=1)   0, 0, 1, 1, ULP,..., ULP, 0.   
            (j=2)   0, 0, 1, 1, 1-d, 1-2*d, ..., 1-(N-5)*d=ULP, 0.   

                                    2        N-5   
            (j=3)   0, 0, 1, 1, a, a , ..., a   =ULP, 0.   
            (j=4)   0, 0, 1, r1, r2, ..., r(N-4), 0, where r1, etc.   
                    are random numbers in (ULP,1).   

    NPARMS  (input) INTEGER   
            The number of values in each of the arrays NNB, NSHFTS,   
            NEISPS, and LDAS.  For each pair of matrices A,B generated   
            according to NN and DOTYPE, tests will be run with   
            (NB,NSHIFT,NEISP,LDA)= (NNB(1), NSHFTS(1), NEISPS(1),   
            LDAS(1)),..., (NNB(NPARMS), NSHFTS(NPARMS), NEISPS(NPARMS),   
            LDAS(NPARMS))   

    NNB     (input) INTEGER array, dimension (NPARMS)   
            The values of the blocksize ("NB") to be tested.  They must   
            be at least 1.  Currently, this is only used by CGEQRF,   
            etc., in the timing of CGGHRD.   

    NSHFTS  (input) INTEGER array, dimension (NPARMS)   
            The values of the number of shifts ("NSHIFT") to be tested.   
            (Currently not used.)   

    NEISPS  (input) INTEGER array, dimension (NPARMS)   
            The values of "NEISP", the size of largest submatrix to be   
            processed by CLAEQZ (EISPACK method), to be tested.   
            (Currently not used.)   

    MINNBS  (input) INTEGER array, dimension (NPARMS)   
            The values of "MINNB", the minimum size of a product of   
            transformations which may be applied as a blocked   
            transformation, to be tested.  (Currently not used.)   

    MINBKS  (input) INTEGER array, dimension (NPARMS)   
            The values of "MINBK", the minimum number of rows/columns   
            to be updated with a blocked transformation, to be tested.   
            (Currently not used.)   

    LDAS    (input) INTEGER array, dimension (NPARMS)   
            The values of LDA, the leading dimension of all matrices,   
            to be tested.   

    TIMMIN  (input) REAL   
            The minimum time a subroutine will be timed.   

    NOUT    (input) INTEGER   
            If NOUT > 0 then NOUT specifies the unit number   
            on which the output will be printed.  If NOUT <= 0, no   
            output is printed.   

    ISEED   (input/output) INTEGER array, dimension (4)   
            The random seed used by the random number generator, used   
            by the test matrix generator.  It is used and updated on   
            each call to CTIM51.   

    A       (workspace) COMPLEX array, dimension (max(NN)*max(LDAS))   
            (a) During the testing of CGGHRD, "A", the original   
                left-hand-side matrix to be tested.   
            (b) Later, "S", the Schur form of the original "A" matrix.   

    AR, AI  (workspace) REAL arrays, dimension   
                        (max(NN)*max(LDAS))   
            The real and imaginary parts of A, stored separately for   
            the benefit of CQZHES, CQZVAL, and CQZVEC.  These may be   
            equivalenced with A by the calling routine.   

    B       (workspace) COMPLEX array, dimension (max(NN)*max(LDAS))   
            (a) During the testing of CGGHRD, "B", the original   
                right-hand-side matrix to be tested.   
            (b) Later, "P", the Schur form of the original "B" matrix.   

    BR, BI  (workspace) REAL arrays, dimension   
                        (max(NN)*max(LDAS))   
            The real and imaginary parts of B, stored separately for   
            the benefit of CQZHES, CQZVAL, and CQZVEC.  These may be   
            equivalenced with B by the calling routine.   

    H       (workspace) COMPLEX array, dimension (max(NN)*max(LDAS))   
            (a) During the testing of CGGHRD and CHGEQZ, "H", the   
                Hessenberg form of the original "A" matrix.   
            (b) During the testing of CTGEVC, "L", the matrix of left   
                eigenvectors.   

    HR, HI  (workspace) REAL arrays, dimension   
                        (max(NN)*max(LDAS))   
            The real and imaginary parts of H, stored separately for   
            the benefit of CQZHES, CQZVAL, and CQZVEC.  These may be   
            equivalenced with H by the calling routine.   

    T       (workspace) COMPLEX array, dimension (max(NN)*max(LDAS))   
            (a) During the testing of CGGHRD and CHGEQZ, "T", the   
                triangular form of the original "B" matrix.   
            (b) During the testing of CTGEVC, "R", the matrix of right   
                eigenvectors.   

    TR, TI  (workspace) REAL arrays, dimension   
                        (max(NN)*max(LDAS))   
            The real and imaginary parts of T, stored separately for   
            the benefit of CQZHES, CQZVAL, and CQZVEC.  These may be   
            equivalenced with T by the calling routine.   

    Q       (workspace) COMPLEX array, dimension (max(NN)*max(LDAS))   
            The orthogonal matrix on the left generated by CGGHRD.  If   
            CHGEQZ computes only Q or Z, then that matrix is stored here.   
            If both Q and Z are computed, the Q matrix goes here.   

    QR, QI  (workspace) REAL arrays, dimension   
                        (max(NN)*max(LDAS))   
            The real and imaginary parts of Q, stored separately for   
            the benefit of CQZVAL.  These may be equivalenced with Q by   
            the calling routine.   

    Z       (workspace) COMPLEX array, dimension (max(NN)*max(LDAS))   
            The orthogonal matrix on the right generated by CGGHRD.   
            If CHGEQZ computes both Q and Z, the Z matrix is stored here.   
            Also used as scratch space for timing the CLACPY calls.   

    ZR, ZI  (workspace) REAL arrays, dimension   
                        (max(NN)*max(LDAS))   
            The real and imaginary parts of Z, stored separately for   
            the benefit of CQZHES, CQZVAL, and CQZVEC.  These may be   
            equivalenced with Z by the calling routine.   

    W       (workspace) COMPLEX array, dimension (2*max(LDAS))   
            Treated as an LDA x 2 matrix whose 1st column holds   
            ALPHA, the diagonal entries of "S", and whose 2nd column   
            holds BETA, the diagonal entries of "P".   

    WR      (workspace) REAL array, dimension (3*max(LDAS))   
            Treated as an LDA x 3 matrix whose 1st and 2nd columns hold   
            the real and imaginary parts of ALPHA (see the description   
            of W), and whose 3rd column holds BETA (real part only.)   
            This may be equivalenced to W by the calling routine.   

    WORK    (workspace) COMPLEX array, dimension (LWORK)   

    LWORK   (input) INTEGER [the following formulae are certainly wrong]   
            Number of elements in WORK.  LWORK >= 4*max(NN).   

    RWORK   (workspace) REAL array, dimension   
                        (max( 2*max(NN), NSIZES*NTYPES*NPARMS ))   

    LLWORK  (workspace) LOGICAL array, dimension (max( max(NN), NPARMS ))   

    TIMES   (output) REAL array, dimension   
                     (LDT1,LDT2,LDT3,NSUBS)   
            TIMES(i,j,k,l) will be set to the run time (in seconds) for   
            subroutine l, with N=NN(k), matrix type j, and LDA=LDAS(i),   
            NEISP=NEISPS(i), NBLOCK=NNB(i), NSHIFT=NSHFTS(i),   
            MINNB=MINNBS(i), and MINBLK=MINBKS(i).   

    LDT1    (input) INTEGER   
            The first dimension of TIMES.  LDT1 >= min( 1, NPARMS ).   

    LDT2    (input) INTEGER   
            The second dimension of TIMES.  LDT2 >= min( 1, NTYPES ).   

    LDT3    (input) INTEGER   
            The third dimension of TIMES.  LDT3 >= min( 1, NSIZES ).   

    OPCNTS  (output) REAL array, dimension   
                     (LDO1,LDO2,LDO3,NSUBS)   
            OPCNTS(i,j,k,l) will be set to the number of floating-point   
            operations executed by subroutine l, with N=NN(k), matrix   
            type j, and LDA=LDAS(i), NEISP=NEISPS(i), NBLOCK=NNB(i),   
            NSHIFT=NSHFTS(i), MINNB=MINNBS(i), and MINBLK=MINBKS(i).   

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
    --neisps;
    --minnbs;
    --minbks;
    --ldas;
    --iseed;
    --a;
    --ar;
    --ai;
    --b;
    --br;
    --bi;
    --h__;
    --hr;
    --hi;
    --t;
    --tr;
    --ti;
    --q;
    --qr;
    --qi;
    --z__;
    --zr;
    --zi;
    --w;
    --wr;
    --work;
    --rwork;
    --llwork;
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

    atimin_("CHG", line, &c__18, subnam, timsub, nout, info, (ftnlen)3, (
	    ftnlen)80, (ftnlen)11);
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
    nbsmax = 0;
    i__1 = *nparms;
    for (j1 = 1; j1 <= i__1; ++j1) {
/* Computing MIN */
	i__2 = ldamin, i__3 = ldas[j1];
	ldamin = min(i__2,i__3);
/* Computing MAX */
	i__2 = nbsmax, i__3 = nnb[j1] + nshfts[j1];
	nbsmax = max(i__2,i__3);
/* L20: */
    }

/*     Check that N <= LDA for the input values. */

    if (nmax > ldamin) {
	*info = -12;
	io___10.ciunit = *nout;
	s_wsfe(&io___10);
	do_fio(&c__1, line, (ftnlen)6);
	e_wsfe();
	return 0;
    }

/*     Check LWORK */

    if (*lwork < nmax << 2) {
	*info = -24;
	io___11.ciunit = *nout;
	s_wsfe(&io___11);
	do_fio(&c__1, line, (ftnlen)6);
	e_wsfe();
	return 0;
    }

/*     Check to see whether CGGHRD or CHGEQZ must be run.   
          RUNHRD -- if CGGHRD must be run.   
          RUNES  -- if CHGEQZ must be run to get Schur form.   
          RUNEQ  -- if CHGEQZ must be run to get Schur form and Q. */

    runhrd = FALSE_;
    runes = FALSE_;
    runeq = FALSE_;

    if (timsub[9] || timsub[11]) {
	runes = TRUE_;
    }
    if (timsub[10] || timsub[12]) {
	runeq = TRUE_;
    }
    if (timsub[4] || timsub[5] || timsub[6] || timsub[7] || timsub[8] || 
	    runes || runeq) {
	runhrd = TRUE_;
    }

    if (timsub[5] || timsub[6] || timsub[7] || timsub[8] || runeq) {
	runes = FALSE_;
    }
    if (timsub[6] || timsub[7] || timsub[8]) {
	runeq = FALSE_;
    }
    if (timsub[0] || timsub[1] || timsub[2] || timsub[3]) {
	runhrd = FALSE_;
    }

/*     Check to see whether CQZHES or CQZVAL must be run.   

       RUNHES -- if CQZHES must be run.   
       RUNQZ  -- if CQZVAL must be run (w/ MATZ=.TRUE.). */

    runhes = FALSE_;
    runqz = FALSE_;

    if (timsub[17]) {
	runqz = TRUE_;
    }
    if (timsub[15] || timsub[16] || runqz) {
	runhes = TRUE_;
    }
    if (timsub[16]) {
	runqz = FALSE_;
    }
    if (timsub[13] || timsub[14]) {
	runhes = FALSE_;
    }

/*     Various Constants */

    ulp = slamch_("Epsilon") * slamch_("Base");

/*     Zero out OPCNTS, TIMES */

    for (j4 = 1; j4 <= 18; ++j4) {
	i__1 = *nsizes;
	for (j3 = 1; j3 <= i__1; ++j3) {
	    i__2 = *ntypes;
	    for (j2 = 1; j2 <= i__2; ++j2) {
		i__3 = *nparms;
		for (j1 = 1; j1 <= i__3; ++j1) {
		    opcnts_ref(j1, j2, j3, j4) = 0.f;
		    times_ref(j1, j2, j3, j4) = 0.f;
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
	n1 = max(1,n);

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
/* L70: */
	    }

/*           Time the LAPACK Routines   

             Generate A and B */

	    if (itype <= 4) {

/*              Generate A (w/o rotation) */

		clatm4_(&katype[itype - 1], &n, &c__3, &c__1, &c_true, &c_b26,
			 &ulp, &c_b26, &c__2, &iseed[1], &a[1], &n1);
		if (3 <= n) {
		    i__3 = (n1 << 1) + 3;
		    a[i__3].r = 1.f, a[i__3].i = 0.f;
		}

/*              Generate B (w/o rotation) */

		clatm4_(&c__8, &n, &c__3, &c__1, &c_false, &c_b26, &c_b26, &
			c_b26, &c__2, &iseed[1], &b[1], &n1);
		if (2 <= n) {
		    i__3 = n1 + 2;
		    b[i__3].r = 1.f, b[i__3].i = 0.f;
		}

		if (n > 0) {

/*                 Include rotations   

                   Generate U, V as Householder transformations times a   
                   diagonal matrix.  (Note that CLARFG makes Q(jc+ic)   
                   and Z(jc+ic) real.) */

		    i__3 = n - 1;
		    for (jc = 1; jc <= i__3; ++jc) {
			ic = (jc - 1) * n1;
			i__4 = n;
			for (jr = jc; jr <= i__4; ++jr) {
			    i__5 = jr + ic;
			    clarnd_(&q__1, &c__3, &iseed[1]);
			    q[i__5].r = q__1.r, q[i__5].i = q__1.i;
			    i__5 = jr + ic;
			    clarnd_(&q__1, &c__3, &iseed[1]);
			    z__[i__5].r = q__1.r, z__[i__5].i = q__1.i;
/* L80: */
			}
			i__4 = n + 1 - jc;
			clarfg_(&i__4, &q[jc + ic], &q[jc + 1 + ic], &c__1, &
				work[jc]);
			i__4 = (n << 1) + jc;
			i__5 = jc + ic;
			r__2 = q[i__5].r;
			r__1 = r_sign(&c_b26, &r__2);
			work[i__4].r = r__1, work[i__4].i = 0.f;
			i__4 = jc + ic;
			q[i__4].r = 1.f, q[i__4].i = 0.f;
			i__4 = n + 1 - jc;
			clarfg_(&i__4, &z__[jc + ic], &z__[jc + 1 + ic], &
				c__1, &work[n + jc]);
			i__4 = n * 3 + jc;
			i__5 = jc + ic;
			r__2 = z__[i__5].r;
			r__1 = r_sign(&c_b26, &r__2);
			work[i__4].r = r__1, work[i__4].i = 0.f;
			i__4 = jc + ic;
			z__[i__4].r = 1.f, z__[i__4].i = 0.f;
/* L90: */
		    }
		    ic = (n - 1) * n1;
		    clarnd_(&q__1, &c__3, &iseed[1]);
		    ctemp.r = q__1.r, ctemp.i = q__1.i;
		    i__3 = n + ic;
		    q[i__3].r = 1.f, q[i__3].i = 0.f;
		    i__3 = n;
		    work[i__3].r = 0.f, work[i__3].i = 0.f;
		    i__3 = n * 3;
		    r__1 = c_abs(&ctemp);
		    q__1.r = ctemp.r / r__1, q__1.i = ctemp.i / r__1;
		    work[i__3].r = q__1.r, work[i__3].i = q__1.i;
		    clarnd_(&q__1, &c__3, &iseed[1]);
		    ctemp.r = q__1.r, ctemp.i = q__1.i;
		    i__3 = n + ic;
		    z__[i__3].r = 1.f, z__[i__3].i = 0.f;
		    i__3 = n << 1;
		    work[i__3].r = 0.f, work[i__3].i = 0.f;
		    i__3 = n << 2;
		    r__1 = c_abs(&ctemp);
		    q__1.r = ctemp.r / r__1, q__1.i = ctemp.i / r__1;
		    work[i__3].r = q__1.r, work[i__3].i = q__1.i;

/*                 Apply the diagonal matrices */

		    i__3 = n;
		    for (jc = 1; jc <= i__3; ++jc) {
			i__4 = n;
			for (jr = 1; jr <= i__4; ++jr) {
			    i__5 = jr + ic;
			    i__6 = (n << 1) + jr;
			    r_cnjg(&q__3, &work[n * 3 + jc]);
			    q__2.r = work[i__6].r * q__3.r - work[i__6].i * 
				    q__3.i, q__2.i = work[i__6].r * q__3.i + 
				    work[i__6].i * q__3.r;
			    i__7 = jr + ic;
			    q__1.r = q__2.r * a[i__7].r - q__2.i * a[i__7].i, 
				    q__1.i = q__2.r * a[i__7].i + q__2.i * a[
				    i__7].r;
			    a[i__5].r = q__1.r, a[i__5].i = q__1.i;
			    i__5 = jr + ic;
			    i__6 = (n << 1) + jr;
			    r_cnjg(&q__3, &work[n * 3 + jc]);
			    q__2.r = work[i__6].r * q__3.r - work[i__6].i * 
				    q__3.i, q__2.i = work[i__6].r * q__3.i + 
				    work[i__6].i * q__3.r;
			    i__7 = jr + ic;
			    q__1.r = q__2.r * b[i__7].r - q__2.i * b[i__7].i, 
				    q__1.i = q__2.r * b[i__7].i + q__2.i * b[
				    i__7].r;
			    b[i__5].r = q__1.r, b[i__5].i = q__1.i;
/* L100: */
			}
/* L110: */
		    }
		    i__3 = n - 1;
		    cunm2r_("L", "N", &n, &n, &i__3, &q[1], &n1, &work[1], &a[
			    1], &n1, &work[(n << 1) + 1], &iinfo);
		    if (iinfo != 0) {
			goto L120;
		    }
		    i__3 = n - 1;
		    cunm2r_("R", "C", &n, &n, &i__3, &z__[1], &n1, &work[n + 
			    1], &a[1], &n1, &work[(n << 1) + 1], &iinfo);
		    if (iinfo != 0) {
			goto L120;
		    }
		    i__3 = n - 1;
		    cunm2r_("L", "N", &n, &n, &i__3, &q[1], &n1, &work[1], &b[
			    1], &n1, &work[(n << 1) + 1], &iinfo);
		    if (iinfo != 0) {
			goto L120;
		    }
		    i__3 = n - 1;
		    cunm2r_("R", "C", &n, &n, &i__3, &z__[1], &n1, &work[n + 
			    1], &b[1], &n1, &work[(n << 1) + 1], &iinfo);
		    if (iinfo != 0) {
			goto L120;
		    }
		}
L120:
		;
	    }

/* . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .   

             Time CGGHRD   

             Time CGEQRF+CGGHRD('N','N',...) for each pair   
             (LDAS(j),NNB(j)) */

	    if (timsub[0]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
		    nb = nnb[ipar];
		    if (lda < n1) {
			times_ref(ipar, itype, in, 1) = 0.f;
			opcnts_ref(ipar, itype, in, 1) = 0.f;
			goto L160;
		    }

/*                 If this value of (NB,LDA) has occurred before,   
                   just use that value. */

		    lastl = 0;
		    i__4 = ipar - 1;
		    for (j = 1; j <= i__4; ++j) {
			if (lda == ldas[j] && nb == nnb[j]) {
			    lastl = j;
			}
/* L130: */
		    }

		    if (lastl == 0) {

/*                    Time CGGHRD, computing neither Q nor Z   
                      (Actually, time CGEQRF + CUNMQR + CGGHRD.) */

			xlaenv_(&c__1, &nb);
			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L140:
			clacpy_("Full", &n, &n, &a[1], &n1, &h__[1], &lda);
			clacpy_("Full", &n, &n, &b[1], &n1, &t[1], &lda);
			claqzh_(&c_false, &c_false, &n, &c__1, &n, &h__[1], &
				lda, &t[1], &lda, &q[1], &lda, &z__[1], &lda, 
				&work[1], &iinfo);
			if (iinfo != 0) {
			    io___38.ciunit = *nout;
			    s_wsfe(&io___38);
			    do_fio(&c__1, subnam_ref(0, 1), (ftnlen)11);
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
			    goto L140;
			}

/*                    Subtract the time used in CLACPY. */

			s1 = second_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    clacpy_("Full", &n, &n, &a[1], &n1, &z__[1], &lda);
			    clacpy_("Full", &n, &n, &b[1], &n1, &z__[1], &lda);
/* L150: */
			}
			s2 = second_();
			untime = s2 - s1;

/* Computing MAX */
			r__1 = time - untime;
			times_ref(ipar, itype, in, 1) = dmax(r__1,0.f) / (
				real) ic;
			opcnts_ref(ipar, itype, in, 1) = latime_1.ops / (real)
				 ic + sopla_("CGEQRF", &n, &n, &c__0, &c__0, &
				nb) + sopla_("CUNMQR", &n, &n, &
				c__0, &c__0, &nb);
			ldh = lda;
		    } else {
			opcnts_ref(ipar, itype, in, 1) = opcnts_ref(lastl, 
				itype, in, 1);
			times_ref(ipar, itype, in, 1) = times_ref(lastl, 
				itype, in, 1);
		    }
L160:
		    ;
		}
	    } else if (runhrd) {
		clacpy_("Full", &n, &n, &a[1], &n1, &h__[1], &n1);
		clacpy_("Full", &n, &n, &b[1], &n1, &t[1], &n1);
		claqzh_(&c_false, &c_false, &n, &c__1, &n, &h__[1], &n1, &t[1]
			, &n1, &q[1], &n1, &z__[1], &n1, &work[1], &iinfo);
		if (iinfo != 0) {
		    io___43.ciunit = *nout;
		    s_wsfe(&io___43);
		    do_fio(&c__1, subnam_ref(0, 1), (ftnlen)11);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    goto L930;
		}
		ldh = n;
	    }

/*           Time CGGHRD('I','N',...) for each pair (LDAS(j),NNB(j)) */

	    if (timsub[1]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
		    nb = nnb[ipar];
		    if (lda < n1) {
			times_ref(ipar, itype, in, 2) = 0.f;
			opcnts_ref(ipar, itype, in, 2) = 0.f;
			goto L200;
		    }

/*                 If this value of (NB,LDA) has occurred before,   
                   just use that value. */

		    lastl = 0;
		    i__4 = ipar - 1;
		    for (j = 1; j <= i__4; ++j) {
			if (lda == ldas[j] && nb == nnb[j]) {
			    lastl = j;
			}
/* L170: */
		    }

		    if (lastl == 0) {

/*                    Time CGGHRD, computing Q but not Z   
                      (Actually, CGEQRF + CUNMQR + CUNGQR + CGGHRD.) */

			xlaenv_(&c__1, &nb);
			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L180:
			clacpy_("Full", &n, &n, &a[1], &n1, &h__[1], &lda);
			clacpy_("Full", &n, &n, &b[1], &n1, &t[1], &lda);
			claqzh_(&c_true, &c_false, &n, &c__1, &n, &h__[1], &
				lda, &t[1], &lda, &q[1], &lda, &z__[1], &lda, 
				&work[1], &iinfo);
			if (iinfo != 0) {
			    io___44.ciunit = *nout;
			    s_wsfe(&io___44);
			    do_fio(&c__1, subnam_ref(0, 2), (ftnlen)11);
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
			    goto L180;
			}

/*                    Subtract the time used in CLACPY. */

			s1 = second_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    clacpy_("Full", &n, &n, &a[1], &n1, &z__[1], &lda);
			    clacpy_("Full", &n, &n, &b[1], &n1, &z__[1], &lda);
/* L190: */
			}
			s2 = second_();
			untime = s2 - s1;

/* Computing MAX */
			r__1 = time - untime;
			times_ref(ipar, itype, in, 2) = dmax(r__1,0.f) / (
				real) ic;
			opcnts_ref(ipar, itype, in, 2) = latime_1.ops / (real)
				 ic + sopla_("CGEQRF", &n, &n, &c__0, &c__0, &
				nb) + sopla_("CUNMQR", &n, &n, &
				c__0, &c__0, &nb) + sopla_("CUNGQR"
				, &n, &n, &c__0, &c__0, &nb);
			ldh = lda;
		    } else {
			opcnts_ref(ipar, itype, in, 2) = opcnts_ref(lastl, 
				itype, in, 2);
			times_ref(ipar, itype, in, 2) = times_ref(lastl, 
				itype, in, 2);
		    }
L200:
		    ;
		}
	    }

/*           Time CGGHRD('N','I',...) for each pair (LDAS(j),NNB(j)) */

	    if (timsub[2]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
		    nb = nnb[ipar];
		    if (lda < n1) {
			times_ref(ipar, itype, in, 3) = 0.f;
			opcnts_ref(ipar, itype, in, 3) = 0.f;
			goto L240;
		    }

/*                 If this value of (NB,LDA) has occurred before,   
                   just use that value. */

		    lastl = 0;
		    i__4 = ipar - 1;
		    for (j = 1; j <= i__4; ++j) {
			if (lda == ldas[j] && nb == nnb[j]) {
			    lastl = j;
			}
/* L210: */
		    }

		    if (lastl == 0) {

/*                    Time CGGHRD, computing Z but not Q   
                      (Actually, CGEQRF + CUNMQR + CGGHRD.) */

			xlaenv_(&c__1, &nb);
			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L220:
			clacpy_("Full", &n, &n, &a[1], &n1, &h__[1], &lda);
			clacpy_("Full", &n, &n, &b[1], &n1, &t[1], &lda);
			claqzh_(&c_false, &c_true, &n, &c__1, &n, &h__[1], &
				lda, &t[1], &lda, &q[1], &lda, &z__[1], &lda, 
				&work[1], &iinfo);
			if (iinfo != 0) {
			    io___45.ciunit = *nout;
			    s_wsfe(&io___45);
			    do_fio(&c__1, subnam_ref(0, 3), (ftnlen)11);
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
			    goto L220;
			}

/*                    Subtract the time used in CLACPY. */

			s1 = second_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    clacpy_("Full", &n, &n, &a[1], &n1, &z__[1], &lda);
			    clacpy_("Full", &n, &n, &b[1], &n1, &z__[1], &lda);
/* L230: */
			}
			s2 = second_();
			untime = s2 - s1;

/* Computing MAX */
			r__1 = time - untime;
			times_ref(ipar, itype, in, 3) = dmax(r__1,0.f) / (
				real) ic;
			opcnts_ref(ipar, itype, in, 3) = latime_1.ops / (real)
				 ic + sopla_("CGEQRF", &n, &n, &c__0, &c__0, &
				nb) + sopla_("CUNMQR", &n, &n, &
				c__0, &c__0, &nb);
			ldh = lda;
		    } else {
			opcnts_ref(ipar, itype, in, 3) = opcnts_ref(lastl, 
				itype, in, 3);
			times_ref(ipar, itype, in, 3) = times_ref(lastl, 
				itype, in, 3);
		    }
L240:
		    ;
		}
	    }

/*           Time CGGHRD('I','I',...) for each pair (LDAS(j),NNB(j)) */

	    if (timsub[3]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
		    nb = nnb[ipar];
		    if (lda < n1) {
			times_ref(ipar, itype, in, 4) = 0.f;
			opcnts_ref(ipar, itype, in, 4) = 0.f;
			goto L280;
		    }

/*                 If this value of (NB,LDA) has occurred before,   
                   just use that value. */

		    lastl = 0;
		    i__4 = ipar - 1;
		    for (j = 1; j <= i__4; ++j) {
			if (lda == ldas[j] && nb == nnb[j]) {
			    lastl = j;
			}
/* L250: */
		    }

		    if (lastl == 0) {

/*                    Time CGGHRD, computing Q and Z   
                      (Actually, CGEQRF + CUNMQR + CUNGQR + CGGHRD.) */

			xlaenv_(&c__1, &nb);
			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L260:
			clacpy_("Full", &n, &n, &a[1], &n1, &h__[1], &lda);
			clacpy_("Full", &n, &n, &b[1], &n1, &t[1], &lda);
			claqzh_(&c_true, &c_true, &n, &c__1, &n, &h__[1], &
				lda, &t[1], &lda, &q[1], &lda, &z__[1], &lda, 
				&work[1], &iinfo);
			if (iinfo != 0) {
			    io___46.ciunit = *nout;
			    s_wsfe(&io___46);
			    do_fio(&c__1, subnam_ref(0, 4), (ftnlen)11);
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
			    goto L260;
			}

/*                    Subtract the time used in CLACPY. */

			s1 = second_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    clacpy_("Full", &n, &n, &a[1], &n1, &z__[1], &lda);
			    clacpy_("Full", &n, &n, &b[1], &n1, &z__[1], &lda);
/* L270: */
			}
			s2 = second_();
			untime = s2 - s1;

/* Computing MAX */
			r__1 = time - untime;
			times_ref(ipar, itype, in, 4) = dmax(r__1,0.f) / (
				real) ic;
			opcnts_ref(ipar, itype, in, 4) = latime_1.ops / (real)
				 ic + sopla_("CGEQRF", &n, &n, &c__0, &c__0, &
				nb) + sopla_("CUNMQR", &n, &n, &
				c__0, &c__0, &nb) + sopla_("CUNGQR"
				, &n, &n, &c__0, &c__0, &nb);
			ldh = lda;
		    } else {
			opcnts_ref(ipar, itype, in, 4) = opcnts_ref(lastl, 
				itype, in, 4);
			times_ref(ipar, itype, in, 4) = times_ref(lastl, 
				itype, in, 4);
		    }
L280:
		    ;
		}
	    }

/* . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .   

             Time CHGEQZ   

             Time CHGEQZ with JOB='E' for each value of LDAS(j) */

	    if (timsub[4]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
		    if (lda < n1) {
			times_ref(ipar, itype, in, 5) = 0.f;
			opcnts_ref(ipar, itype, in, 5) = 0.f;
			goto L320;
		    }

/*                 If this value of LDA has occurred before,   
                   just use that value. */

		    lastl = 0;
		    i__4 = ipar - 1;
		    for (j = 1; j <= i__4; ++j) {
			if (lda == ldas[j]) {
			    lastl = j;
			}
/* L290: */
		    }

		    if (lastl == 0) {

/*                    Time CHGEQZ with JOB='E' */

			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L300:
			clacpy_("Full", &n, &n, &h__[1], &ldh, &a[1], &lda);
			clacpy_("Full", &n, &n, &t[1], &ldh, &b[1], &lda);
			chgeqz_("E", "N", "N", &n, &c__1, &n, &a[1], &lda, &b[
				1], &lda, &w[1], &w[lda + 1], &q[1], &lda, &
				z__[1], &lda, &work[1], lwork, &rwork[1], &
				iinfo);
			if (iinfo != 0) {
			    io___47.ciunit = *nout;
			    s_wsfe(&io___47);
			    do_fio(&c__1, subnam_ref(0, 5), (ftnlen)11);
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
			    goto L300;
			}

/*                    Subtract the time used in CLACPY. */

			s1 = second_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    clacpy_("Full", &n, &n, &h__[1], &ldh, &z__[1], &
				    lda);
			    clacpy_("Full", &n, &n, &t[1], &ldh, &z__[1], &
				    lda);
/* L310: */
			}
			s2 = second_();
			untime = s2 - s1;

/* Computing MAX */
			r__1 = time - untime;
			times_ref(ipar, itype, in, 5) = dmax(r__1,0.f) / (
				real) ic;
			opcnts_ref(ipar, itype, in, 5) = latime_1.ops / (real)
				 ic;
			lds = 0;
			ldq = 0;
		    } else {
			opcnts_ref(ipar, itype, in, 5) = opcnts_ref(lastl, 
				itype, in, 5);
			times_ref(ipar, itype, in, 5) = times_ref(lastl, 
				itype, in, 5);
		    }
L320:
		    ;
		}
	    }

/*           Time CHGEQZ with JOB='S', COMPQ=COMPZ='N' for each value   
             of LDAS(j) */

	    if (timsub[5]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
		    if (lda < n1) {
			times_ref(ipar, itype, in, 6) = 0.f;
			opcnts_ref(ipar, itype, in, 6) = 0.f;
			goto L360;
		    }

/*                 If this value of LDA has occurred before,   
                   just use that value. */

		    lastl = 0;
		    i__4 = ipar - 1;
		    for (j = 1; j <= i__4; ++j) {
			if (lda == ldas[j]) {
			    lastl = j;
			}
/* L330: */
		    }

		    if (lastl == 0) {

/*                 Time CHGEQZ with JOB='S', COMPQ=COMPZ='N' */

			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L340:
			clacpy_("Full", &n, &n, &h__[1], &ldh, &a[1], &lda);
			clacpy_("Full", &n, &n, &t[1], &ldh, &b[1], &lda);
			chgeqz_("S", "N", "N", &n, &c__1, &n, &a[1], &lda, &b[
				1], &lda, &w[1], &w[lda + 1], &q[1], &lda, &
				z__[1], &lda, &work[1], lwork, &rwork[1], &
				iinfo);
			if (iinfo != 0) {
			    io___50.ciunit = *nout;
			    s_wsfe(&io___50);
			    do_fio(&c__1, subnam_ref(0, 6), (ftnlen)11);
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
			    goto L340;
			}

/*                 Subtract the time used in CLACPY. */

			s1 = second_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    clacpy_("Full", &n, &n, &h__[1], &ldh, &z__[1], &
				    lda);
			    clacpy_("Full", &n, &n, &t[1], &ldh, &z__[1], &
				    lda);
/* L350: */
			}
			s2 = second_();
			untime = s2 - s1;

/* Computing MAX */
			r__1 = time - untime;
			times_ref(ipar, itype, in, 6) = dmax(r__1,0.f) / (
				real) ic;
			opcnts_ref(ipar, itype, in, 6) = latime_1.ops / (real)
				 ic;
			lds = lda;
			ldq = 0;
		    } else {
			opcnts_ref(ipar, itype, in, 6) = opcnts_ref(lastl, 
				itype, in, 6);
			times_ref(ipar, itype, in, 6) = times_ref(lastl, 
				itype, in, 6);
		    }
L360:
		    ;
		}
	    } else if (runes) {
		clacpy_("Full", &n, &n, &h__[1], &ldh, &a[1], &n1);
		clacpy_("Full", &n, &n, &t[1], &ldh, &b[1], &n1);
		chgeqz_("S", "N", "N", &n, &c__1, &n, &a[1], &n1, &b[1], &n1, 
			&w[1], &w[n1 + 1], &q[1], &n1, &z__[1], &n1, &work[1],
			 lwork, &rwork[1], &iinfo);
		if (iinfo != 0) {
		    io___51.ciunit = *nout;
		    s_wsfe(&io___51);
		    do_fio(&c__1, subnam_ref(0, 6), (ftnlen)11);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    goto L930;
		}
		lds = n1;
		ldq = 0;
	    }

/*           Time CHGEQZ with JOB='S', COMPQ='I', COMPZ='N' for each   
             value of LDAS(j) */

	    if (timsub[6]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
		    if (lda < n1) {
			times_ref(ipar, itype, in, 7) = 0.f;
			opcnts_ref(ipar, itype, in, 7) = 0.f;
			goto L400;
		    }

/*                 If this value of LDA has occurred before,   
                   just use that value. */

		    lastl = 0;
		    i__4 = ipar - 1;
		    for (j = 1; j <= i__4; ++j) {
			if (lda == ldas[j]) {
			    lastl = j;
			}
/* L370: */
		    }

		    if (lastl == 0) {

/*                 Time CHGEQZ with JOB='S', COMPQ='I', COMPZ='N' */

			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L380:
			clacpy_("Full", &n, &n, &h__[1], &ldh, &a[1], &lda);
			clacpy_("Full", &n, &n, &t[1], &ldh, &b[1], &lda);
			chgeqz_("S", "I", "N", &n, &c__1, &n, &a[1], &lda, &b[
				1], &lda, &w[1], &w[lda + 1], &q[1], &lda, &
				z__[1], &lda, &work[1], lwork, &rwork[1], &
				iinfo);
			if (iinfo != 0) {
			    io___52.ciunit = *nout;
			    s_wsfe(&io___52);
			    do_fio(&c__1, subnam_ref(0, 7), (ftnlen)11);
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
			    goto L380;
			}

/*                 Subtract the time used in CLACPY. */

			s1 = second_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    clacpy_("Full", &n, &n, &h__[1], &ldh, &z__[1], &
				    lda);
			    clacpy_("Full", &n, &n, &t[1], &ldh, &z__[1], &
				    lda);
/* L390: */
			}
			s2 = second_();
			untime = s2 - s1;

/* Computing MAX */
			r__1 = time - untime;
			times_ref(ipar, itype, in, 7) = dmax(r__1,0.f) / (
				real) ic;
			opcnts_ref(ipar, itype, in, 7) = latime_1.ops / (real)
				 ic;
			lds = lda;
			ldq = lda;
		    } else {
			opcnts_ref(ipar, itype, in, 7) = opcnts_ref(lastl, 
				itype, in, 7);
			times_ref(ipar, itype, in, 7) = times_ref(lastl, 
				itype, in, 7);
		    }
L400:
		    ;
		}
	    } else if (runeq) {
		clacpy_("Full", &n, &n, &h__[1], &ldh, &a[1], &n1);
		clacpy_("Full", &n, &n, &t[1], &ldh, &b[1], &n1);
		chgeqz_("S", "I", "N", &n, &c__1, &n, &a[1], &n1, &b[1], &n1, 
			&w[1], &w[n1 + 1], &q[1], &n1, &z__[1], &n1, &work[1],
			 lwork, &rwork[1], &iinfo);
		if (iinfo != 0) {
		    io___53.ciunit = *nout;
		    s_wsfe(&io___53);
		    do_fio(&c__1, subnam_ref(0, 7), (ftnlen)11);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    goto L930;
		}
		lds = n1;
		ldq = n1;
	    }

/*           Time CHGEQZ with JOB='S', COMPQ='N', COMPZ='I' for each   
             value of LDAS(j) */

	    if (timsub[7]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
		    if (lda < n1) {
			times_ref(ipar, itype, in, 8) = 0.f;
			opcnts_ref(ipar, itype, in, 8) = 0.f;
			goto L440;
		    }

/*                 If this value of LDA has occurred before,   
                   just use that value. */

		    lastl = 0;
		    i__4 = ipar - 1;
		    for (j = 1; j <= i__4; ++j) {
			if (lda == ldas[j]) {
			    lastl = j;
			}
/* L410: */
		    }

		    if (lastl == 0) {

/* Computing MIN */
			i__4 = n, i__5 = nnb[ipar];
			nb = min(i__4,i__5);
			nshift = nshfts[ipar];
			neisp = neisps[ipar];
			minnb = minnbs[ipar];
			minblk = minbks[ipar];
			xlaenv_(&c__1, &nb);
			xlaenv_(&c__2, &minnb);
			xlaenv_(&c__8, &neisp);
			xlaenv_(&c__4, &nshift);
			xlaenv_(&c__5, &minblk);

/*                 Time CHGEQZ with JOB='S', COMPQ='N', COMPZ='I'   
                   (Note that the "Z" matrix is stored in the array Q) */

			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L420:
			clacpy_("Full", &n, &n, &h__[1], &ldh, &a[1], &lda);
			clacpy_("Full", &n, &n, &t[1], &ldh, &b[1], &lda);
			chgeqz_("S", "N", "I", &n, &c__1, &n, &a[1], &lda, &b[
				1], &lda, &w[1], &w[lda + 1], &z__[1], &lda, &
				q[1], &lda, &work[1], lwork, &rwork[1], &
				iinfo);
			if (iinfo != 0) {
			    io___58.ciunit = *nout;
			    s_wsfe(&io___58);
			    do_fio(&c__1, subnam_ref(0, 8), (ftnlen)11);
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
			    goto L420;
			}

/*                 Subtract the time used in CLACPY. */

			s1 = second_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    clacpy_("Full", &n, &n, &h__[1], &ldh, &z__[1], &
				    lda);
			    clacpy_("Full", &n, &n, &t[1], &ldh, &z__[1], &
				    lda);
/* L430: */
			}
			s2 = second_();
			untime = s2 - s1;

/* Computing MAX */
			r__1 = time - untime;
			times_ref(ipar, itype, in, 8) = dmax(r__1,0.f) / (
				real) ic;
			opcnts_ref(ipar, itype, in, 8) = latime_1.ops / (real)
				 ic;
			lds = lda;
			ldq = lda;
		    } else {
			opcnts_ref(ipar, itype, in, 8) = opcnts_ref(lastl, 
				itype, in, 8);
			times_ref(ipar, itype, in, 8) = times_ref(lastl, 
				itype, in, 8);
		    }
L440:
		    ;
		}
	    }

/*           Time CHGEQZ with JOB='S', COMPQ='I', COMPZ='I' for each   
             value of LDAS(j) */

	    if (timsub[8]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
		    if (lda < n1) {
			times_ref(ipar, itype, in, 9) = 0.f;
			opcnts_ref(ipar, itype, in, 9) = 0.f;
			goto L480;
		    }

/*                 If this value of LDA has occurred before,   
                   just use that value. */

		    lastl = 0;
		    i__4 = ipar - 1;
		    for (j = 1; j <= i__4; ++j) {
			if (lda == ldas[j]) {
			    lastl = j;
			}
/* L450: */
		    }

		    if (lastl == 0) {

/*                 Time CHGEQZ with JOB='S', COMPQ='I', COMPZ='I' */

			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L460:
			clacpy_("Full", &n, &n, &h__[1], &ldh, &a[1], &lda);
			clacpy_("Full", &n, &n, &t[1], &ldh, &b[1], &lda);
			chgeqz_("S", "I", "I", &n, &c__1, &n, &a[1], &lda, &b[
				1], &lda, &w[1], &w[lda + 1], &q[1], &lda, &
				z__[1], &lda, &work[1], lwork, &rwork[1], &
				iinfo);
			if (iinfo != 0) {
			    io___59.ciunit = *nout;
			    s_wsfe(&io___59);
			    do_fio(&c__1, subnam_ref(0, 9), (ftnlen)11);
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
			    goto L460;
			}

/*                 Subtract the time used in CLACPY. */

			s1 = second_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    clacpy_("Full", &n, &n, &h__[1], &ldh, &z__[1], &
				    lda);
			    clacpy_("Full", &n, &n, &t[1], &ldh, &z__[1], &
				    lda);
/* L470: */
			}
			s2 = second_();
			untime = s2 - s1;

/* Computing MAX */
			r__1 = time - untime;
			times_ref(ipar, itype, in, 9) = dmax(r__1,0.f) / (
				real) ic;
			opcnts_ref(ipar, itype, in, 9) = latime_1.ops / (real)
				 ic;
			lds = lda;
			ldq = lda;
		    } else {
			opcnts_ref(ipar, itype, in, 9) = opcnts_ref(lastl, 
				itype, in, 9);
			times_ref(ipar, itype, in, 9) = times_ref(lastl, 
				itype, in, 9);
		    }
L480:
		    ;
		}
	    }

/* . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .   

             Time CTGEVC */

	    if (timsub[9] || timsub[10] || timsub[11] || timsub[12]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
		    if (lda < n1) {
			for (j = 10; j <= 13; ++j) {
			    if (timsub[j - 1]) {
				times_ref(ipar, itype, in, j) = 0.f;
				opcnts_ref(ipar, itype, in, j) = 0.f;
			    }
/* L490: */
			}
			goto L610;
		    }

/*                 If this value of LDA has come up before, just use   
                   the value previously computed. */

		    lastl = 0;
		    i__4 = ipar - 1;
		    for (j = 1; j <= i__4; ++j) {
			if (lda == ldas[j]) {
			    lastl = j;
			}
/* L500: */
		    }

/*                 Time CTGEVC if this is a new value of LDA */

		    if (lastl == 0) {

/*                    Copy S (which is in A) and P (which is in B)   
                      if necessary to get right LDA. */

			if (lda > lds) {
			    for (jc = n; jc >= 1; --jc) {
				for (jr = n; jr >= 1; --jr) {
				    i__4 = jr + (jc - 1) * lda;
				    i__5 = jr + (jc - 1) * lds;
				    a[i__4].r = a[i__5].r, a[i__4].i = a[i__5]
					    .i;
				    i__4 = jr + (jc - 1) * lda;
				    i__5 = jr + (jc - 1) * lds;
				    b[i__4].r = b[i__5].r, b[i__4].i = b[i__5]
					    .i;
/* L510: */
				}
/* L520: */
			    }
			} else if (lda < lds) {
			    i__4 = n;
			    for (jc = 1; jc <= i__4; ++jc) {
				i__5 = n;
				for (jr = 1; jr <= i__5; ++jr) {
				    i__6 = jr + (jc - 1) * lda;
				    i__7 = jr + (jc - 1) * lds;
				    a[i__6].r = a[i__7].r, a[i__6].i = a[i__7]
					    .i;
				    i__6 = jr + (jc - 1) * lda;
				    i__7 = jr + (jc - 1) * lds;
				    b[i__6].r = b[i__7].r, b[i__6].i = b[i__7]
					    .i;
/* L530: */
				}
/* L540: */
			    }
			}
			lds = lda;

/*                    Time CTGEVC for Left Eigenvectors only,   
                      without back transforming */

			if (timsub[9]) {
			    ic = 0;
			    latime_1.ops = 0.f;
			    s1 = second_();
L550:
			    ctgevc_("L", "A", &llwork[1], &n, &a[1], &lda, &b[
				    1], &lda, &h__[1], &lda, &t[1], &lda, &n, 
				    &itemp, &work[1], &rwork[1], &iinfo);
			    if (iinfo != 0) {
				io___61.ciunit = *nout;
				s_wsfe(&io___61);
				do_fio(&c__1, subnam_ref(0, 10), (ftnlen)11);
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
				goto L930;
			    }
			    s2 = second_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				goto L550;
			    }

			    times_ref(ipar, itype, in, 10) = time / (real) ic;
			    opcnts_ref(ipar, itype, in, 10) = latime_1.ops / (
				    real) ic;
			}

/*                    Time CTGEVC for Left Eigenvectors only,   
                      with back transforming */

			if (timsub[10]) {
			    ic = 0;
			    latime_1.ops = 0.f;
			    s1 = second_();
L560:
			    clacpy_("Full", &n, &n, &q[1], &ldq, &h__[1], &
				    lda);
			    ctgevc_("L", "B", &llwork[1], &n, &a[1], &lda, &b[
				    1], &lda, &h__[1], &lda, &t[1], &lda, &n, 
				    &itemp, &work[1], &rwork[1], &iinfo);
			    if (iinfo != 0) {
				io___62.ciunit = *nout;
				s_wsfe(&io___62);
				do_fio(&c__1, subnam_ref(0, 11), (ftnlen)11);
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
				goto L930;
			    }
			    s2 = second_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				goto L560;
			    }

/*                       Subtract the time used in CLACPY. */

			    s1 = second_();
			    i__4 = ic;
			    for (j = 1; j <= i__4; ++j) {
				clacpy_("Full", &n, &n, &q[1], &ldq, &h__[1], 
					&lda);
/* L570: */
			    }
			    s2 = second_();
			    untime = s2 - s1;

/* Computing MAX */
			    r__1 = time - untime;
			    times_ref(ipar, itype, in, 11) = dmax(r__1,0.f) / 
				    (real) ic;
			    opcnts_ref(ipar, itype, in, 11) = latime_1.ops / (
				    real) ic;
			}

/*                    Time CTGEVC for Right Eigenvectors only,   
                      without back transforming */

			if (timsub[11]) {
			    ic = 0;
			    latime_1.ops = 0.f;
			    s1 = second_();
L580:
			    ctgevc_("R", "A", &llwork[1], &n, &a[1], &lda, &b[
				    1], &lda, &h__[1], &lda, &t[1], &lda, &n, 
				    &itemp, &work[1], &rwork[1], &iinfo);
			    if (iinfo != 0) {
				io___63.ciunit = *nout;
				s_wsfe(&io___63);
				do_fio(&c__1, subnam_ref(0, 12), (ftnlen)11);
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
				goto L930;
			    }
			    s2 = second_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				goto L580;
			    }

			    times_ref(ipar, itype, in, 12) = time / (real) ic;
			    opcnts_ref(ipar, itype, in, 12) = latime_1.ops / (
				    real) ic;
			}

/*                    Time CTGEVC for Right Eigenvectors only,   
                      with back transforming */

			if (timsub[12]) {
			    ic = 0;
			    latime_1.ops = 0.f;
			    s1 = second_();
L590:
			    clacpy_("Full", &n, &n, &q[1], &ldq, &t[1], &lda);
			    ctgevc_("R", "B", &llwork[1], &n, &a[1], &lda, &b[
				    1], &lda, &h__[1], &lda, &t[1], &lda, &n, 
				    &itemp, &work[1], &rwork[1], &iinfo);
			    if (iinfo != 0) {
				io___64.ciunit = *nout;
				s_wsfe(&io___64);
				do_fio(&c__1, subnam_ref(0, 13), (ftnlen)11);
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
				goto L930;
			    }
			    s2 = second_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				goto L590;
			    }

/*                       Subtract the time used in CLACPY. */

			    s1 = second_();
			    i__4 = ic;
			    for (j = 1; j <= i__4; ++j) {
				clacpy_("Full", &n, &n, &q[1], &ldq, &t[1], &
					lda);
/* L600: */
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

		    } else {

/*                    If this LDA has previously appeared, use the   
                      previously computed value(s). */

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
		    }
L610:
		    ;
		}
	    }

/*           Time the EISPACK Routines   

             Restore random number seed */

	    for (j = 1; j <= 4; ++j) {
		iseed[j] = ioldsd[j - 1];
/* L620: */
	    }

/*           Re-generate A */

	    if (itype <= 4) {

/*              Generate A (w/o rotation) */

		clatm4_(&katype[itype - 1], &n, &c__3, &c__1, &c_true, &c_b26,
			 &ulp, &c_b26, &c__2, &iseed[1], &h__[1], &n1);
		if (n >= 3) {
		    i__3 = (n1 << 1) + 3;
		    h__[i__3].r = 1.f, h__[i__3].i = 0.f;
		}

/*              Generate B (w/o rotation) */

		clatm4_(&c__8, &n, &c__3, &c__1, &c_false, &c_b26, &c_b26, &
			c_b26, &c__2, &iseed[1], &t[1], &n1);
		if (n >= 2) {
		    i__3 = n1 + 2;
		    t[i__3].r = 1.f, t[i__3].i = 0.f;
		}

		if (n > 0) {

/*                 Include rotations   

                   Generate U, V as Householder transformations times a   
                   diagonal matrix.  (Note that CLARFG makes Q(jc+ic)   
                   and Z(jc+ic) real.) */

		    i__3 = n - 1;
		    for (jc = 1; jc <= i__3; ++jc) {
			ic = (jc - 1) * n1;
			i__4 = n;
			for (jr = jc; jr <= i__4; ++jr) {
			    i__5 = jr + ic;
			    clarnd_(&q__1, &c__3, &iseed[1]);
			    q[i__5].r = q__1.r, q[i__5].i = q__1.i;
			    i__5 = jr + ic;
			    clarnd_(&q__1, &c__3, &iseed[1]);
			    z__[i__5].r = q__1.r, z__[i__5].i = q__1.i;
/* L630: */
			}
			i__4 = n + 1 - jc;
			clarfg_(&i__4, &q[jc + ic], &q[jc + 1 + ic], &c__1, &
				work[jc]);
			i__4 = (n << 1) + jc;
			i__5 = jc + ic;
			r__2 = q[i__5].r;
			r__1 = r_sign(&c_b26, &r__2);
			work[i__4].r = r__1, work[i__4].i = 0.f;
			i__4 = jc + ic;
			q[i__4].r = 1.f, q[i__4].i = 0.f;
			i__4 = n + 1 - jc;
			clarfg_(&i__4, &z__[jc + ic], &z__[jc + 1 + ic], &
				c__1, &work[n + jc]);
			i__4 = n * 3 + jc;
			i__5 = jc + ic;
			r__2 = z__[i__5].r;
			r__1 = r_sign(&c_b26, &r__2);
			work[i__4].r = r__1, work[i__4].i = 0.f;
			i__4 = jc + ic;
			z__[i__4].r = 1.f, z__[i__4].i = 0.f;
/* L640: */
		    }
		    ic = (n - 1) * n1;
		    clarnd_(&q__1, &c__3, &iseed[1]);
		    ctemp.r = q__1.r, ctemp.i = q__1.i;
		    i__3 = n + ic;
		    q[i__3].r = 1.f, q[i__3].i = 0.f;
		    i__3 = n;
		    work[i__3].r = 0.f, work[i__3].i = 0.f;
		    i__3 = n * 3;
		    r__1 = c_abs(&ctemp);
		    q__1.r = ctemp.r / r__1, q__1.i = ctemp.i / r__1;
		    work[i__3].r = q__1.r, work[i__3].i = q__1.i;
		    clarnd_(&q__1, &c__3, &iseed[1]);
		    ctemp.r = q__1.r, ctemp.i = q__1.i;
		    i__3 = n + ic;
		    z__[i__3].r = 1.f, z__[i__3].i = 0.f;
		    i__3 = n << 1;
		    work[i__3].r = 0.f, work[i__3].i = 0.f;
		    i__3 = n << 2;
		    r__1 = c_abs(&ctemp);
		    q__1.r = ctemp.r / r__1, q__1.i = ctemp.i / r__1;
		    work[i__3].r = q__1.r, work[i__3].i = q__1.i;

/*                 Apply the diagonal matrices */

		    i__3 = n;
		    for (jc = 1; jc <= i__3; ++jc) {
			i__4 = n;
			for (jr = 1; jr <= i__4; ++jr) {
			    i__5 = jr + ic;
			    i__6 = (n << 1) + jr;
			    r_cnjg(&q__3, &work[n * 3 + jc]);
			    q__2.r = work[i__6].r * q__3.r - work[i__6].i * 
				    q__3.i, q__2.i = work[i__6].r * q__3.i + 
				    work[i__6].i * q__3.r;
			    i__7 = jr + ic;
			    q__1.r = q__2.r * h__[i__7].r - q__2.i * h__[i__7]
				    .i, q__1.i = q__2.r * h__[i__7].i + 
				    q__2.i * h__[i__7].r;
			    h__[i__5].r = q__1.r, h__[i__5].i = q__1.i;
			    i__5 = jr + ic;
			    i__6 = (n << 1) + jr;
			    i__7 = n * 3 + jc;
			    q__3.r = work[i__6].r * work[i__7].r - work[i__6]
				    .i * work[i__7].i, q__3.i = work[i__6].r *
				     work[i__7].i + work[i__6].i * work[i__7]
				    .r;
			    r_cnjg(&q__4, &work[n * 3 + jc]);
			    q__2.r = q__3.r * q__4.r - q__3.i * q__4.i, 
				    q__2.i = q__3.r * q__4.i + q__3.i * 
				    q__4.r;
			    i__8 = jr + ic;
			    q__1.r = q__2.r * t[i__8].r - q__2.i * t[i__8].i, 
				    q__1.i = q__2.r * t[i__8].i + q__2.i * t[
				    i__8].r;
			    t[i__5].r = q__1.r, t[i__5].i = q__1.i;
/* L650: */
			}
/* L660: */
		    }
		    i__3 = n - 1;
		    cunm2r_("L", "N", &n, &n, &i__3, &q[1], &n1, &work[1], &
			    h__[1], &n1, &work[(n << 1) + 1], &iinfo);
		    if (iinfo != 0) {
			goto L670;
		    }
		    i__3 = n - 1;
		    cunm2r_("R", "C", &n, &n, &i__3, &z__[1], &n1, &work[n + 
			    1], &h__[1], &n1, &work[(n << 1) + 1], &iinfo);
		    if (iinfo != 0) {
			goto L670;
		    }
		    i__3 = n - 1;
		    cunm2r_("L", "N", &n, &n, &i__3, &q[1], &n1, &work[1], &t[
			    1], &n1, &work[(n << 1) + 1], &iinfo);
		    if (iinfo != 0) {
			goto L670;
		    }
		    i__3 = n - 1;
		    cunm2r_("R", "C", &n, &n, &i__3, &z__[1], &n1, &work[n + 
			    1], &t[1], &n1, &work[(n << 1) + 1], &iinfo);
		    if (iinfo != 0) {
			goto L670;
		    }
		}
L670:

/*              Copy real and imaginary parts into separate arrays */

		i__3 = n * n;
		for (j = 1; j <= i__3; ++j) {
		    i__4 = j;
		    ar[j] = h__[i__4].r;
		    ai[j] = r_imag(&h__[j]);
		    i__4 = j;
		    br[j] = t[i__4].r;
		    bi[j] = r_imag(&t[j]);
/* L680: */
		}
	    }

/*           Time CQZHES w/ MATZ=.FALSE. for each LDAS(j) */

	    if (timsub[13]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
		    if (lda < n1) {
			times_ref(ipar, itype, in, 14) = 0.f;
			opcnts_ref(ipar, itype, in, 14) = 0.f;
			goto L720;
		    }

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

/*                    Time CQZHES( ...,.FALSE.,..) */

			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L700:
			slacpy_("Full", &n, &n, &ar[1], &n1, &hr[1], &lda);
			slacpy_("Full", &n, &n, &ai[1], &n1, &hi[1], &lda);
			slacpy_("Full", &n, &n, &br[1], &n1, &tr[1], &lda);
			slacpy_("Full", &n, &n, &bi[1], &n1, &ti[1], &lda);
			cqzhes_(&lda, &n, &hr[1], &hi[1], &tr[1], &ti[1], &
				c_false, &qr[1], &qi[1]);
			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L700;
			}

/*                    Subtract the time used in CLACPY. */

			s1 = second_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    slacpy_("Full", &n, &n, &ar[1], &n1, &zr[1], &lda);
			    slacpy_("Full", &n, &n, &ai[1], &n1, &zi[1], &lda);
			    slacpy_("Full", &n, &n, &br[1], &n1, &zr[1], &lda);
			    slacpy_("Full", &n, &n, &bi[1], &n1, &zi[1], &lda);
/* L710: */
			}
			s2 = second_();
			untime = s2 - s1;

/* Computing MAX */
			r__1 = time - untime;
			times_ref(ipar, itype, in, 14) = dmax(r__1,0.f) / (
				real) ic;
			opcnts_ref(ipar, itype, in, 14) = latime_1.ops / (
				real) ic;
		    } else {
			opcnts_ref(ipar, itype, in, 14) = opcnts_ref(lastl, 
				itype, in, 14);
			times_ref(ipar, itype, in, 14) = times_ref(lastl, 
				itype, in, 14);
		    }
		    ldh = lda;
L720:
		    ;
		}
	    } else if (runhes) {
		slacpy_("Full", &n, &n, &ar[1], &n1, &hr[1], &n1);
		slacpy_("Full", &n, &n, &ai[1], &n1, &hi[1], &n1);
		slacpy_("Full", &n, &n, &br[1], &n1, &tr[1], &n1);
		slacpy_("Full", &n, &n, &bi[1], &n1, &ti[1], &n1);
		cqzhes_(&n1, &n, &hr[1], &hi[1], &tr[1], &ti[1], &c_false, &
			qr[1], &qi[1]);
		ldh = n1;
	    }

/*           Time CQZHES w/ MATZ=.TRUE. for each LDAS(j) */

	    if (timsub[14]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
		    if (lda < n1) {
			times_ref(ipar, itype, in, 15) = 0.f;
			opcnts_ref(ipar, itype, in, 15) = 0.f;
			goto L760;
		    }

/*                 If this value of LDA has come up before, just use   
                   the value previously computed. */

		    lastl = 0;
		    i__4 = ipar - 1;
		    for (j = 1; j <= i__4; ++j) {
			if (lda == ldas[j]) {
			    lastl = j;
			}
/* L730: */
		    }

		    if (lastl == 0) {

/*                    Time CQZHES( ...,.TRUE.,..) */

			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L740:
			slacpy_("Full", &n, &n, &ar[1], &n1, &hr[1], &lda);
			slacpy_("Full", &n, &n, &ai[1], &n1, &hi[1], &lda);
			slacpy_("Full", &n, &n, &br[1], &n1, &tr[1], &lda);
			slacpy_("Full", &n, &n, &bi[1], &n1, &ti[1], &lda);
			cqzhes_(&lda, &n, &hr[1], &hi[1], &tr[1], &ti[1], &
				c_true, &qr[1], &qi[1]);
			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L740;
			}

/*                    Subtract the time used in CLACPY. */

			s1 = second_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    slacpy_("Full", &n, &n, &ar[1], &n1, &zr[1], &lda);
			    slacpy_("Full", &n, &n, &ai[1], &n1, &zi[1], &lda);
			    slacpy_("Full", &n, &n, &br[1], &n1, &zr[1], &lda);
			    slacpy_("Full", &n, &n, &bi[1], &n1, &zi[1], &lda);
/* L750: */
			}
			s2 = second_();
			untime = s2 - s1;

/* Computing MAX */
			r__1 = time - untime;
			times_ref(ipar, itype, in, 15) = dmax(r__1,0.f) / (
				real) ic;
			opcnts_ref(ipar, itype, in, 15) = latime_1.ops / (
				real) ic;
		    } else {
			opcnts_ref(ipar, itype, in, 15) = opcnts_ref(lastl, 
				itype, in, 15);
			times_ref(ipar, itype, in, 15) = times_ref(lastl, 
				itype, in, 15);
		    }
		    ldh = lda;
L760:
		    ;
		}
	    }

/*           Time CQZVAL w/ MATZ=.FALSE. for each LDAS(j) */

	    if (timsub[15]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
		    if (lda < n1) {
			times_ref(ipar, itype, in, 16) = 0.f;
			opcnts_ref(ipar, itype, in, 16) = 0.f;
			goto L800;
		    }

/*                 If this value of LDA has come up before, just use   
                   the value previously computed. */

		    lastl = 0;
		    i__4 = ipar - 1;
		    for (j = 1; j <= i__4; ++j) {
			if (lda == ldas[j]) {
			    lastl = j;
			}
/* L770: */
		    }

		    if (lastl == 0) {

/*                    Time CQZVAL with MATZ=.FALSE. */

			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L780:
			slacpy_("Full", &n, &n, &hr[1], &ldh, &ar[1], &lda);
			slacpy_("Full", &n, &n, &hi[1], &ldh, &ai[1], &lda);
			slacpy_("Full", &n, &n, &tr[1], &ldh, &br[1], &lda);
			slacpy_("Full", &n, &n, &ti[1], &ldh, &bi[1], &lda);
			cqzval_(&lda, &n, &ar[1], &ai[1], &br[1], &bi[1], &
				c_b419, &wr[1], &wr[lda + 1], &wr[(lda << 1) 
				+ 1], &c_false, &qr[1], &qi[1], &iinfo);
			if (iinfo != 0) {
			    io___65.ciunit = *nout;
			    s_wsfe(&io___65);
			    do_fio(&c__1, subnam_ref(0, 16), (ftnlen)11);
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
			    goto L780;
			}

/*                    Subtract the time used in CLACPY. */

			s1 = second_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    slacpy_("Full", &n, &n, &hr[1], &ldh, &zr[1], &
				    lda);
			    slacpy_("Full", &n, &n, &hi[1], &ldh, &zi[1], &
				    lda);
			    slacpy_("Full", &n, &n, &tr[1], &ldh, &zr[1], &
				    lda);
			    slacpy_("Full", &n, &n, &ti[1], &ldh, &zi[1], &
				    lda);
/* L790: */
			}
			s2 = second_();
			untime = s2 - s1;

/* Computing MAX */
			r__1 = time - untime;
			times_ref(ipar, itype, in, 16) = dmax(r__1,0.f) / (
				real) ic;
			opcnts_ref(ipar, itype, in, 16) = latime_1.ops / (
				real) ic;
		    } else {
			opcnts_ref(ipar, itype, in, 16) = opcnts_ref(lastl, 
				itype, in, 16);
			times_ref(ipar, itype, in, 16) = times_ref(lastl, 
				itype, in, 16);
		    }
		    lds = 0;
		    ldw = lda;
L800:
		    ;
		}
	    }

/*           Time CQZVAL w/ MATZ=.TRUE. for each LDAS(j) */

	    if (timsub[16]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
		    if (lda < n1) {
			times_ref(ipar, itype, in, 17) = 0.f;
			opcnts_ref(ipar, itype, in, 17) = 0.f;
			goto L840;
		    }

/*                 If this value of LDA has come up before, just use   
                   the value previously computed. */

		    lastl = 0;
		    i__4 = ipar - 1;
		    for (j = 1; j <= i__4; ++j) {
			if (lda == ldas[j]) {
			    lastl = j;
			}
/* L810: */
		    }

		    if (lastl == 0) {

/*                    Time CQZVAL with MATZ=.TRUE. */

			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L820:
			slacpy_("Full", &n, &n, &hr[1], &ldh, &ar[1], &lda);
			slacpy_("Full", &n, &n, &hi[1], &ldh, &ai[1], &lda);
			slacpy_("Full", &n, &n, &tr[1], &ldh, &br[1], &lda);
			slacpy_("Full", &n, &n, &ti[1], &ldh, &bi[1], &lda);
			slaset_("Full", &n, &n, &c_b419, &c_b26, &qr[1], &lda);
			slaset_("Full", &n, &n, &c_b419, &c_b26, &qi[1], &lda);
			cqzval_(&lda, &n, &ar[1], &ai[1], &br[1], &bi[1], &
				c_b419, &wr[1], &wr[lda + 1], &wr[(lda << 1) 
				+ 1], &c_true, &qr[1], &qi[1], &iinfo);
			if (iinfo != 0) {
			    io___67.ciunit = *nout;
			    s_wsfe(&io___67);
			    do_fio(&c__1, subnam_ref(0, 17), (ftnlen)11);
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
			    goto L820;
			}

/*                    Subtract the time used in CLACPY. */

			s1 = second_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    slacpy_("Full", &n, &n, &hr[1], &ldh, &zr[1], &
				    lda);
			    slacpy_("Full", &n, &n, &hi[1], &ldh, &zi[1], &
				    lda);
			    slacpy_("Full", &n, &n, &tr[1], &ldh, &zr[1], &
				    lda);
			    slacpy_("Full", &n, &n, &ti[1], &ldh, &zi[1], &
				    lda);
			    slaset_("Full", &n, &n, &c_b419, &c_b26, &zr[1], &
				    lda);
			    slaset_("Full", &n, &n, &c_b419, &c_b26, &zi[1], &
				    lda);
/* L830: */
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
		    lds = lda;
		    ldw = lda;
L840:
		    ;
		}
	    } else if (runqz) {
		slacpy_("Full", &n, &n, &hr[1], &ldh, &ar[1], &n1);
		slacpy_("Full", &n, &n, &hi[1], &ldh, &ai[1], &n1);
		slacpy_("Full", &n, &n, &tr[1], &ldh, &br[1], &n1);
		slacpy_("Full", &n, &n, &ti[1], &ldh, &bi[1], &n1);
		slaset_("Full", &n, &n, &c_b419, &c_b26, &qr[1], &n1);
		slaset_("Full", &n, &n, &c_b419, &c_b26, &qi[1], &n1);
		cqzval_(&n1, &n, &ar[1], &ai[1], &br[1], &bi[1], &c_b419, &wr[
			1], &wr[n1 + 1], &wr[(n1 << 1) + 1], &c_true, &qr[1], 
			&qi[1], &iinfo);
		if (iinfo != 0) {
		    io___68.ciunit = *nout;
		    s_wsfe(&io___68);
		    do_fio(&c__1, subnam_ref(0, 17), (ftnlen)11);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    goto L930;
		}

		lds = n1;
		ldw = n1;
	    }

/*           Time CQZVEC for each LDAS(j) */

	    if (timsub[17]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
		    if (lda < n1) {
			times_ref(ipar, itype, in, 18) = 0.f;
			opcnts_ref(ipar, itype, in, 18) = 0.f;
			goto L920;
		    }

/*                 If this value of LDA has come up before, just use   
                   the value previously computed. */

		    lastl = 0;
		    i__4 = ipar - 1;
		    for (j = 1; j <= i__4; ++j) {
			if (lda == ldas[j]) {
			    lastl = j;
			}
/* L850: */
		    }

		    if (lastl == 0) {

/*                    Copy W if necessary to get right LDA. */

			if (lda > ldw) {
			    for (jc = 3; jc >= 1; --jc) {
				for (jr = n; jr >= 1; --jr) {
				    wr[jr + (jc - 1) * lda] = wr[jr + (jc - 1)
					     * ldw];
/* L860: */
				}
/* L870: */
			    }
			} else if (lda < ldw) {
			    for (jc = 1; jc <= 3; ++jc) {
				i__4 = n;
				for (jr = 1; jr <= i__4; ++jr) {
				    wr[jr + (jc - 1) * lda] = wr[jr + (jc - 1)
					     * ldw];
/* L880: */
				}
/* L890: */
			    }
			}
			ldw = lda;

/*                    Time CQZVEC */

			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L900:
			slacpy_("Full", &n, &n, &ar[1], &lds, &hr[1], &lda);
			slacpy_("Full", &n, &n, &ai[1], &lds, &hi[1], &lda);
			slacpy_("Full", &n, &n, &br[1], &lds, &tr[1], &lda);
			slacpy_("Full", &n, &n, &bi[1], &lds, &ti[1], &lda);
			slacpy_("Full", &n, &n, &qr[1], &lds, &zr[1], &lda);
			slacpy_("Full", &n, &n, &qi[1], &lds, &zi[1], &lda);
			cqzvec_(&lda, &n, &hr[1], &hi[1], &tr[1], &ti[1], &wr[
				1], &wr[lda + 1], &wr[(lda << 1) + 1], &zr[1],
				 &zi[1]);
			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L900;
			}

/*                    Subtract the time used in CLACPY. */

			s1 = second_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    slacpy_("Full", &n, &n, &ar[1], &lds, &zr[1], &
				    lda);
			    slacpy_("Full", &n, &n, &ai[1], &lds, &zi[1], &
				    lda);
			    slacpy_("Full", &n, &n, &br[1], &lds, &zr[1], &
				    lda);
			    slacpy_("Full", &n, &n, &bi[1], &lds, &zi[1], &
				    lda);
			    slacpy_("Full", &n, &n, &qr[1], &lds, &zr[1], &
				    lda);
			    slacpy_("Full", &n, &n, &qi[1], &lds, &zi[1], &
				    lda);
/* L910: */
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
L920:
		    ;
		}
	    }

L930:
	    ;
	}
/* L940: */
    }

/*     Print a table of results for each timed routine. */

    for (isub = 1; isub <= 18; ++isub) {
	if (timsub[isub - 1]) {
	    sprtbg_(subnam_ref(0, isub), &mtypes, &dotype[1], nsizes, &nn[1], 
		    &inparm[isub - 1], pnames, nparms, &ldas[1], &nnb[1], &
		    nshfts[1], &neisps[1], &minnbs[1], &minbks[1], &
		    opcnts_ref(1, 1, 1, isub), ldo1, ldo2, &times_ref(1, 1, 1,
		     isub), ldt1, ldt2, &rwork[1], &llwork[1], nout, (ftnlen)
		    11, (ftnlen)6);
	}
/* L950: */
    }

    return 0;

/*     End of CTIM51 */


} /* ctim51_ */

#undef opcnts_ref
#undef subnam_ref
#undef times_ref


