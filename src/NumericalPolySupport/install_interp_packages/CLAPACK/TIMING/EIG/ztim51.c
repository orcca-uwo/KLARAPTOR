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

static integer c__18 = 18;
static integer c__1 = 1;
static integer c__3 = 3;
static logical c_true = TRUE_;
static doublereal c_b26 = 1.;
static integer c__2 = 2;
static integer c__8 = 8;
static logical c_false = FALSE_;
static integer c__4 = 4;
static integer c__0 = 0;
static integer c__5 = 5;
static doublereal c_b419 = 0.;

/* Subroutine */ int ztim51_(char *line, integer *nsizes, integer *nn, 
	integer *ntypes, logical *dotype, integer *nparms, integer *nnb, 
	integer *nshfts, integer *neisps, integer *minnbs, integer *minbks, 
	integer *ldas, doublereal *timmin, integer *nout, integer *iseed, 
	doublecomplex *a, doublereal *ar, doublereal *ai, doublecomplex *b, 
	doublereal *br, doublereal *bi, doublecomplex *h__, doublereal *hr, 
	doublereal *hi, doublecomplex *t, doublereal *tr, doublereal *ti, 
	doublecomplex *q, doublereal *qr, doublereal *qi, doublecomplex *z__, 
	doublereal *zr, doublereal *zi, doublecomplex *w, doublereal *wr, 
	doublecomplex *work, integer *lwork, doublereal *rwork, logical *
	llwork, doublereal *times, integer *ldt1, integer *ldt2, integer *
	ldt3, doublereal *opcnts, integer *ldo1, integer *ldo2, integer *ldo3,
	 integer *info, ftnlen line_len)
{
    /* Initialized data */

    static char subnam[11*18] = "ZGGHRD(N)  " "ZGGHRD(Q)  " "ZGGHRD(Z)  " 
	    "ZGGHRD(Q,Z)" "ZHGEQZ(E)  " "ZHGEQZ(S)  " "ZHGEQZ(Q)  " "ZHGEQZ("
	    "Z)  " "ZHGEQZ(Q,Z)" "ZTGEVC(L,A)" "ZTGEVC(L,B)" "ZTGEVC(R,A)" 
	    "ZTGEVC(R,B)" "CQZHES(F)  " "CQZHES(T)  " "CQZVAL(F)  " "CQZVAL("
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
    static char fmt_9997[] = "(\002 ZTIM51: \002,a,\002 returned INFO=\002,i"
	    "6,\002.\002,/9x,\002N=\002,i6,\002, ITYPE=\002,i6,\002, IPAR="
	    "\002,i6,\002, ISEED=(\002,3(i5,\002,\002),i5,\002)\002)";

    /* System generated locals */
    integer opcnts_dim1, opcnts_dim2, opcnts_dim3, opcnts_offset, times_dim1, 
	    times_dim2, times_dim3, times_offset, i__1, i__2, i__3, i__4, 
	    i__5, i__6, i__7, i__8;
    doublereal d__1, d__2;
    doublecomplex z__1, z__2, z__3, z__4;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);
    double d_sign(doublereal *, doublereal *), z_abs(doublecomplex *);
    void d_cnjg(doublecomplex *, doublecomplex *);
    double d_imag(doublecomplex *);

    /* Local variables */
    static integer ipar;
    static doublereal time;
    static integer isub, nmax, j, n;
    extern doublereal dopla_(char *, integer *, integer *, integer *, integer 
	    *, integer *);
    static integer minnb, iinfo;
    static doublecomplex ctemp;
    static integer itemp, lastl, neisp;
    static logical runeq, runes;
    static integer itype, j1, j2, j3, j4, n1;
    static doublereal s1, s2;
    static logical runqz;
    static integer ic, jc;
    extern /* Subroutine */ int zlatm4_(integer *, integer *, integer *, 
	    integer *, logical *, doublereal *, doublereal *, doublereal *, 
	    integer *, integer *, doublecomplex *, integer *);
    static integer nb, in;
    extern doublereal dlamch_(char *);
    static integer jr;
    extern /* Subroutine */ int zunm2r_(char *, char *, integer *, integer *, 
	    integer *, doublecomplex *, integer *, doublecomplex *, 
	    doublecomplex *, integer *, doublecomplex *, integer *);
    extern doublereal dsecnd_(void);
    static integer ldamin, minblk, ioldsd[4];
    extern /* Subroutine */ int dlacpy_(char *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, integer *), 
	    dlaset_(char *, integer *, integer *, doublereal *, doublereal *, 
	    doublereal *, integer *), atimin_(char *, char *, integer 
	    *, char *, logical *, integer *, integer *, ftnlen, ftnlen, 
	    ftnlen), dprtbg_(char *, integer *, logical *, integer *, integer 
	    *, integer *, char *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, doublereal *, integer *, integer 
	    *, doublereal *, integer *, integer *, doublereal *, logical *, 
	    integer *, ftnlen, ftnlen);
    static integer nbsmax;
    extern /* Subroutine */ int zlarfg_(integer *, doublecomplex *, 
	    doublecomplex *, integer *, doublecomplex *);
    extern /* Double Complex */ VOID zlarnd_(doublecomplex *, integer *, 
	    integer *);
    static integer nshift;
    extern /* Subroutine */ int cqzvec_(integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *), cqzhes_(
	    integer *, integer *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, logical *, doublereal *, doublereal *);
    static logical runhrd;
    static doublereal untime;
    static logical runhes, timsub[18];
    extern /* Subroutine */ int cqzval_(integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, logical *, doublereal *,
	     doublereal *, integer *), xlaenv_(integer *, integer *), zhgeqz_(
	    char *, char *, char *, integer *, integer *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublereal *, integer *), zlacpy_(char *, 
	    integer *, integer *, doublecomplex *, integer *, doublecomplex *,
	     integer *), zlaqzh_(logical *, logical *, integer *, 
	    integer *, integer *, doublecomplex *, integer *, doublecomplex *,
	     integer *, doublecomplex *, integer *, doublecomplex *, integer *
	    , doublecomplex *, integer *), ztgevc_(char *, char *, logical *, 
	    integer *, doublecomplex *, integer *, doublecomplex *, integer *,
	     doublecomplex *, integer *, doublecomplex *, integer *, integer *
	    , integer *, doublecomplex *, doublereal *, integer *);
    static integer mtypes, lda, ldh, ldq, lds, ldw;
    static doublereal ulp;

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

    ZTIM51 times the LAPACK routines for the COMPLEX*16 non-symmetric   
    generalized eigenvalue problem   A x = w B x.   

    For each N value in NN(1:NSIZES) and .TRUE. value in   
    DOTYPE(1:NTYPES), a pair of matrices will be generated and used to   
    test the selected routines.  Thus, NSIZES*(number of .TRUE. values   
    in DOTYPE) matrices will be generated.   

    Arguments   
    =========   

    LINE    (input) CHARACTER*80   
            The input line which requested this routine.  This line may   
            contain a subroutine name, such as ZGGHRD, indicating that   
            only routine ZGGHRD will be timed, or it may contain a   
            generic name, such as ZHG.  In this case, the rest of the   
            line is scanned for the first 18 non-blank characters,   
            corresponding to the eighteen combinations of subroutine and   
            options:   
            LAPACK:                                     Table Heading:   
             1: ZGGHRD(no Q, no Z) (+ZGEQRF, etc.)      'CGGHRD(N)'   
             2: ZGGHRD(Q only)     (+ZGEQRF, etc.)      'CGGHRD(Q)'   
             3: ZGGHRD(Z only)     (+ZGEQRF, etc.)      'CGGHRD(Z)'   
             4: ZGGHRD(Q and Z)    (+ZGEQRF, etc.)      'CGGHRD(Q,Z)'   
             5: ZHGEQZ(Eigenvalues only)                'CHGEQZ(E)'   
             6: ZHGEQZ(Schur form only)                 'CHGEQZ(S)'   
             7: ZHGEQZ(Schur form and Q)                'CHGEQZ(Q)'   
             8: ZHGEQZ(Schur form and Z)                'CHGEQZ(Z)'   
             9: ZHGEQZ(Schur form, Q and Z)             'CHGEQZ(Q,Z)'   
            10: ZTGEVC(SIDE='L', HOWMNY='A')            'CTGEVC(L,A)'   
            11: ZTGEVC(SIDE='L', HOWMNY='B')            'CTGEVC(L,B)'   
            12: ZTGEVC(SIDE='R', HOWMNY='A')            'CTGEVC(R,A)'   
            13: ZTGEVC(SIDE='R', HOWMNY='B')            'CTGEVC(R,B)'   
            EISPACK:                       Compare w/:  Table Heading:   
            14: CQZHES w/ matz=.false.            1      'CQZHES(F)'   
            15: CQZHES w/ matz=.true.             3      'CQZHES(T)'   
            16: CQZVAL w/ matz=.false.            5      'CQZVAL(F)'   
            17: CQZVAL w/ matz=.true.             8      'CQZVAL(T)'   
            18: CQZVEC                           13      'CQZVEC'   
            If a character is 'T' or 't', the corresponding routine in   
            this path is timed.  If the entire line is blank, all the   
            routines in the path are timed.   

            Note that since QZHES does more than DGGHRD, the   
            "DGGHRD" timing also includes the time for the calls   
            to DGEQRF, DORMQR, and (if Q is computed) DORGQR   
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
            be at least 1.  Currently, this is only used by ZGEQRF,   
            etc., in the timing of ZGGHRD.   

    NSHFTS  (input) INTEGER array, dimension (NPARMS)   
            The values of the number of shifts ("NSHIFT") to be tested.   
            (Currently not used.)   

    NEISPS  (input) INTEGER array, dimension (NPARMS)   
            The values of "NEISP", the size of largest submatrix to be   
            processed by ZLAEQZ (EISPACK method), to be tested.   
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

    TIMMIN  (input) DOUBLE PRECISION   
            The minimum time a subroutine will be timed.   

    NOUT    (input) INTEGER   
            If NOUT > 0 then NOUT specifies the unit number   
            on which the output will be printed.  If NOUT <= 0, no   
            output is printed.   

    ISEED   (input/output) INTEGER array, dimension (4)   
            The random seed used by the random number generator, used   
            by the test matrix generator.  It is used and updated on   
            each call to ZTIM51.   

    A       (workspace) COMPLEX*16 array, dimension (max(NN)*max(LDAS))   
            (a) During the testing of ZGGHRD, "A", the original   
                left-hand-side matrix to be tested.   
            (b) Later, "S", the Schur form of the original "A" matrix.   

    AR, AI  (workspace) DOUBLE PRECISION arrays, dimension   
                        (max(NN)*max(LDAS))   
            The real and imaginary parts of A, stored separately for   
            the benefit of CQZHES, CQZVAL, and CQZVEC.  These may be   
            equivalenced with A by the calling routine.   

    B       (workspace) COMPLEX*16 array, dimension (max(NN)*max(LDAS))   
            (a) During the testing of ZGGHRD, "B", the original   
                right-hand-side matrix to be tested.   
            (b) Later, "P", the Schur form of the original "B" matrix.   

    BR, BI  (workspace) DOUBLE PRECISION arrays, dimension   
                        (max(NN)*max(LDAS))   
            The real and imaginary parts of B, stored separately for   
            the benefit of CQZHES, CQZVAL, and CQZVEC.  These may be   
            equivalenced with B by the calling routine.   

    H       (workspace) COMPLEX*16 array, dimension (max(NN)*max(LDAS))   
            (a) During the testing of ZGGHRD and ZHGEQZ, "H", the   
                Hessenberg form of the original "A" matrix.   
            (b) During the testing of ZTGEVC, "L", the matrix of left   
                eigenvectors.   

    HR, HI  (workspace) DOUBLE PRECISION arrays, dimension   
                        (max(NN)*max(LDAS))   
            The real and imaginary parts of H, stored separately for   
            the benefit of CQZHES, CQZVAL, and CQZVEC.  These may be   
            equivalenced with H by the calling routine.   

    T       (workspace) COMPLEX*16 array, dimension (max(NN)*max(LDAS))   
            (a) During the testing of ZGGHRD and ZHGEQZ, "T", the   
                triangular form of the original "B" matrix.   
            (b) During the testing of ZTGEVC, "R", the matrix of right   
                eigenvectors.   

    TR, TI  (workspace) DOUBLE PRECISION arrays, dimension   
                        (max(NN)*max(LDAS))   
            The real and imaginary parts of T, stored separately for   
            the benefit of CQZHES, CQZVAL, and CQZVEC.  These may be   
            equivalenced with T by the calling routine.   

    Q       (workspace) COMPLEX*16 array, dimension (max(NN)*max(LDAS))   
            The orthogonal matrix on the left generated by ZGGHRD.  If   
            ZHGEQZ computes only Q or Z, then that matrix is stored here.   
            If both Q and Z are computed, the Q matrix goes here.   

    QR, QI  (workspace) DOUBLE PRECISION arrays, dimension   
                        (max(NN)*max(LDAS))   
            The real and imaginary parts of Q, stored separately for   
            the benefit of CQZVAL.  These may be equivalenced with Q by   
            the calling routine.   

    Z       (workspace) COMPLEX*16 array, dimension (max(NN)*max(LDAS))   
            The orthogonal matrix on the right generated by ZGGHRD.   
            If ZHGEQZ computes both Q and Z, the Z matrix is stored here.   
            Also used as scratch space for timing the ZLACPY calls.   

    ZR, ZI  (workspace) DOUBLE PRECISION arrays, dimension   
                        (max(NN)*max(LDAS))   
            The real and imaginary parts of Z, stored separately for   
            the benefit of CQZHES, CQZVAL, and CQZVEC.  These may be   
            equivalenced with Z by the calling routine.   

    W       (workspace) COMPLEX*16 array, dimension (2*max(LDAS))   
            Treated as an LDA x 2 matrix whose 1st column holds   
            ALPHA, the diagonal entries of "S", and whose 2nd column   
            holds BETA, the diagonal entries of "P".   

    WR      (workspace) DOUBLE PRECISION array, dimension (3*max(LDAS))   
            Treated as an LDA x 3 matrix whose 1st and 2nd columns hold   
            the real and imaginary parts of ALPHA (see the description   
            of W), and whose 3rd column holds BETA (real part only.)   
            This may be equivalenced to W by the calling routine.   

    WORK    (workspace) COMPLEX*16 array, dimension (LWORK)   

    LWORK   (input) INTEGER [the following formulae are certainly wrong]   
            Number of elements in WORK.  LWORK >= 4*max(NN).   

    RWORK   (workspace) DOUBLE PRECISION array, dimension   
                        (max( 2*max(NN), NSIZES*NTYPES*NPARMS ))   

    LLWORK  (workspace) LOGICAL array, dimension (max( max(NN), NPARMS ))   

    TIMES   (output) DOUBLE PRECISION array, dimension   
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

    OPCNTS  (output) DOUBLE PRECISION array, dimension   
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

    atimin_("ZHG", line, &c__18, subnam, timsub, nout, info, (ftnlen)3, (
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

/*     Check to see whether ZGGHRD or ZHGEQZ must be run.   
          RUNHRD -- if ZGGHRD must be run.   
          RUNES  -- if ZHGEQZ must be run to get Schur form.   
          RUNEQ  -- if ZHGEQZ must be run to get Schur form and Q. */

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

    ulp = dlamch_("Epsilon") * dlamch_("Base");

/*     Zero out OPCNTS, TIMES */

    for (j4 = 1; j4 <= 18; ++j4) {
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

		zlatm4_(&katype[itype - 1], &n, &c__3, &c__1, &c_true, &c_b26,
			 &ulp, &c_b26, &c__2, &iseed[1], &a[1], &n1);
		if (3 <= n) {
		    i__3 = (n1 << 1) + 3;
		    a[i__3].r = 1., a[i__3].i = 0.;
		}

/*              Generate B (w/o rotation) */

		zlatm4_(&c__8, &n, &c__3, &c__1, &c_false, &c_b26, &c_b26, &
			c_b26, &c__2, &iseed[1], &b[1], &n1);
		if (2 <= n) {
		    i__3 = n1 + 2;
		    b[i__3].r = 1., b[i__3].i = 0.;
		}

		if (n > 0) {

/*                 Include rotations   

                   Generate U, V as Householder transformations times a   
                   diagonal matrix.  (Note that ZLARFG makes Q(jc+ic)   
                   and Z(jc+ic) real.) */

		    i__3 = n - 1;
		    for (jc = 1; jc <= i__3; ++jc) {
			ic = (jc - 1) * n1;
			i__4 = n;
			for (jr = jc; jr <= i__4; ++jr) {
			    i__5 = jr + ic;
			    zlarnd_(&z__1, &c__3, &iseed[1]);
			    q[i__5].r = z__1.r, q[i__5].i = z__1.i;
			    i__5 = jr + ic;
			    zlarnd_(&z__1, &c__3, &iseed[1]);
			    z__[i__5].r = z__1.r, z__[i__5].i = z__1.i;
/* L80: */
			}
			i__4 = n + 1 - jc;
			zlarfg_(&i__4, &q[jc + ic], &q[jc + 1 + ic], &c__1, &
				work[jc]);
			i__4 = (n << 1) + jc;
			i__5 = jc + ic;
			d__2 = q[i__5].r;
			d__1 = d_sign(&c_b26, &d__2);
			work[i__4].r = d__1, work[i__4].i = 0.;
			i__4 = jc + ic;
			q[i__4].r = 1., q[i__4].i = 0.;
			i__4 = n + 1 - jc;
			zlarfg_(&i__4, &z__[jc + ic], &z__[jc + 1 + ic], &
				c__1, &work[n + jc]);
			i__4 = n * 3 + jc;
			i__5 = jc + ic;
			d__2 = z__[i__5].r;
			d__1 = d_sign(&c_b26, &d__2);
			work[i__4].r = d__1, work[i__4].i = 0.;
			i__4 = jc + ic;
			z__[i__4].r = 1., z__[i__4].i = 0.;
/* L90: */
		    }
		    ic = (n - 1) * n1;
		    zlarnd_(&z__1, &c__3, &iseed[1]);
		    ctemp.r = z__1.r, ctemp.i = z__1.i;
		    i__3 = n + ic;
		    q[i__3].r = 1., q[i__3].i = 0.;
		    i__3 = n;
		    work[i__3].r = 0., work[i__3].i = 0.;
		    i__3 = n * 3;
		    d__1 = z_abs(&ctemp);
		    z__1.r = ctemp.r / d__1, z__1.i = ctemp.i / d__1;
		    work[i__3].r = z__1.r, work[i__3].i = z__1.i;
		    zlarnd_(&z__1, &c__3, &iseed[1]);
		    ctemp.r = z__1.r, ctemp.i = z__1.i;
		    i__3 = n + ic;
		    z__[i__3].r = 1., z__[i__3].i = 0.;
		    i__3 = n << 1;
		    work[i__3].r = 0., work[i__3].i = 0.;
		    i__3 = n << 2;
		    d__1 = z_abs(&ctemp);
		    z__1.r = ctemp.r / d__1, z__1.i = ctemp.i / d__1;
		    work[i__3].r = z__1.r, work[i__3].i = z__1.i;

/*                 Apply the diagonal matrices */

		    i__3 = n;
		    for (jc = 1; jc <= i__3; ++jc) {
			i__4 = n;
			for (jr = 1; jr <= i__4; ++jr) {
			    i__5 = jr + ic;
			    i__6 = (n << 1) + jr;
			    d_cnjg(&z__3, &work[n * 3 + jc]);
			    z__2.r = work[i__6].r * z__3.r - work[i__6].i * 
				    z__3.i, z__2.i = work[i__6].r * z__3.i + 
				    work[i__6].i * z__3.r;
			    i__7 = jr + ic;
			    z__1.r = z__2.r * a[i__7].r - z__2.i * a[i__7].i, 
				    z__1.i = z__2.r * a[i__7].i + z__2.i * a[
				    i__7].r;
			    a[i__5].r = z__1.r, a[i__5].i = z__1.i;
			    i__5 = jr + ic;
			    i__6 = (n << 1) + jr;
			    d_cnjg(&z__3, &work[n * 3 + jc]);
			    z__2.r = work[i__6].r * z__3.r - work[i__6].i * 
				    z__3.i, z__2.i = work[i__6].r * z__3.i + 
				    work[i__6].i * z__3.r;
			    i__7 = jr + ic;
			    z__1.r = z__2.r * b[i__7].r - z__2.i * b[i__7].i, 
				    z__1.i = z__2.r * b[i__7].i + z__2.i * b[
				    i__7].r;
			    b[i__5].r = z__1.r, b[i__5].i = z__1.i;
/* L100: */
			}
/* L110: */
		    }
		    i__3 = n - 1;
		    zunm2r_("L", "N", &n, &n, &i__3, &q[1], &n1, &work[1], &a[
			    1], &n1, &work[(n << 1) + 1], &iinfo);
		    if (iinfo != 0) {
			goto L120;
		    }
		    i__3 = n - 1;
		    zunm2r_("R", "C", &n, &n, &i__3, &z__[1], &n1, &work[n + 
			    1], &a[1], &n1, &work[(n << 1) + 1], &iinfo);
		    if (iinfo != 0) {
			goto L120;
		    }
		    i__3 = n - 1;
		    zunm2r_("L", "N", &n, &n, &i__3, &q[1], &n1, &work[1], &b[
			    1], &n1, &work[(n << 1) + 1], &iinfo);
		    if (iinfo != 0) {
			goto L120;
		    }
		    i__3 = n - 1;
		    zunm2r_("R", "C", &n, &n, &i__3, &z__[1], &n1, &work[n + 
			    1], &b[1], &n1, &work[(n << 1) + 1], &iinfo);
		    if (iinfo != 0) {
			goto L120;
		    }
		}
L120:
		;
	    }

/* . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .   

             Time ZGGHRD   

             Time ZGEQRF+ZGGHRD('N','N',...) for each pair   
             (LDAS(j),NNB(j)) */

	    if (timsub[0]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
		    nb = nnb[ipar];
		    if (lda < n1) {
			times_ref(ipar, itype, in, 1) = 0.;
			opcnts_ref(ipar, itype, in, 1) = 0.;
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

/*                    Time ZGGHRD, computing neither Q nor Z   
                      (Actually, time ZGEQRF + ZUNMQR + ZGGHRD.) */

			xlaenv_(&c__1, &nb);
			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();
L140:
			zlacpy_("Full", &n, &n, &a[1], &n1, &h__[1], &lda);
			zlacpy_("Full", &n, &n, &b[1], &n1, &t[1], &lda);
			zlaqzh_(&c_false, &c_false, &n, &c__1, &n, &h__[1], &
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

			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L140;
			}

/*                    Subtract the time used in ZLACPY. */

			s1 = dsecnd_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    zlacpy_("Full", &n, &n, &a[1], &n1, &z__[1], &lda);
			    zlacpy_("Full", &n, &n, &b[1], &n1, &z__[1], &lda);
/* L150: */
			}
			s2 = dsecnd_();
			untime = s2 - s1;

/* Computing MAX */
			d__1 = time - untime;
			times_ref(ipar, itype, in, 1) = max(d__1,0.) / (
				doublereal) ic;
			opcnts_ref(ipar, itype, in, 1) = latime_1.ops / (
				doublereal) ic + dopla_("ZGEQRF", &n, &n, &
				c__0, &c__0, &nb) + dopla_("ZUNMQR"
				, &n, &n, &c__0, &c__0, &nb);
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
		zlacpy_("Full", &n, &n, &a[1], &n1, &h__[1], &n1);
		zlacpy_("Full", &n, &n, &b[1], &n1, &t[1], &n1);
		zlaqzh_(&c_false, &c_false, &n, &c__1, &n, &h__[1], &n1, &t[1]
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

/*           Time ZGGHRD('I','N',...) for each pair (LDAS(j),NNB(j)) */

	    if (timsub[1]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
		    nb = nnb[ipar];
		    if (lda < n1) {
			times_ref(ipar, itype, in, 2) = 0.;
			opcnts_ref(ipar, itype, in, 2) = 0.;
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

/*                    Time ZGGHRD, computing Q but not Z   
                      (Actually, ZGEQRF + ZUNMQR + CUNGQR + ZGGHRD.) */

			xlaenv_(&c__1, &nb);
			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();
L180:
			zlacpy_("Full", &n, &n, &a[1], &n1, &h__[1], &lda);
			zlacpy_("Full", &n, &n, &b[1], &n1, &t[1], &lda);
			zlaqzh_(&c_true, &c_false, &n, &c__1, &n, &h__[1], &
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

			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L180;
			}

/*                    Subtract the time used in ZLACPY. */

			s1 = dsecnd_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    zlacpy_("Full", &n, &n, &a[1], &n1, &z__[1], &lda);
			    zlacpy_("Full", &n, &n, &b[1], &n1, &z__[1], &lda);
/* L190: */
			}
			s2 = dsecnd_();
			untime = s2 - s1;

/* Computing MAX */
			d__1 = time - untime;
			times_ref(ipar, itype, in, 2) = max(d__1,0.) / (
				doublereal) ic;
			opcnts_ref(ipar, itype, in, 2) = latime_1.ops / (
				doublereal) ic + dopla_("ZGEQRF", &n, &n, &
				c__0, &c__0, &nb) + dopla_("ZUNMQR"
				, &n, &n, &c__0, &c__0, &nb) + 
				dopla_("ZUNGQR", &n, &n, &c__0, &c__0, &nb);
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

/*           Time ZGGHRD('N','I',...) for each pair (LDAS(j),NNB(j)) */

	    if (timsub[2]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
		    nb = nnb[ipar];
		    if (lda < n1) {
			times_ref(ipar, itype, in, 3) = 0.;
			opcnts_ref(ipar, itype, in, 3) = 0.;
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

/*                    Time ZGGHRD, computing Z but not Q   
                      (Actually, ZGEQRF + ZUNMQR + ZGGHRD.) */

			xlaenv_(&c__1, &nb);
			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();
L220:
			zlacpy_("Full", &n, &n, &a[1], &n1, &h__[1], &lda);
			zlacpy_("Full", &n, &n, &b[1], &n1, &t[1], &lda);
			zlaqzh_(&c_false, &c_true, &n, &c__1, &n, &h__[1], &
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

			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L220;
			}

/*                    Subtract the time used in ZLACPY. */

			s1 = dsecnd_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    zlacpy_("Full", &n, &n, &a[1], &n1, &z__[1], &lda);
			    zlacpy_("Full", &n, &n, &b[1], &n1, &z__[1], &lda);
/* L230: */
			}
			s2 = dsecnd_();
			untime = s2 - s1;

/* Computing MAX */
			d__1 = time - untime;
			times_ref(ipar, itype, in, 3) = max(d__1,0.) / (
				doublereal) ic;
			opcnts_ref(ipar, itype, in, 3) = latime_1.ops / (
				doublereal) ic + dopla_("ZGEQRF", &n, &n, &
				c__0, &c__0, &nb) + dopla_("ZUNMQR"
				, &n, &n, &c__0, &c__0, &nb);
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

/*           Time ZGGHRD('I','I',...) for each pair (LDAS(j),NNB(j)) */

	    if (timsub[3]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
		    nb = nnb[ipar];
		    if (lda < n1) {
			times_ref(ipar, itype, in, 4) = 0.;
			opcnts_ref(ipar, itype, in, 4) = 0.;
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

/*                    Time ZGGHRD, computing Q and Z   
                      (Actually, ZGEQRF + ZUNMQR + CUNGQR + ZGGHRD.) */

			xlaenv_(&c__1, &nb);
			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();
L260:
			zlacpy_("Full", &n, &n, &a[1], &n1, &h__[1], &lda);
			zlacpy_("Full", &n, &n, &b[1], &n1, &t[1], &lda);
			zlaqzh_(&c_true, &c_true, &n, &c__1, &n, &h__[1], &
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

			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L260;
			}

/*                    Subtract the time used in ZLACPY. */

			s1 = dsecnd_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    zlacpy_("Full", &n, &n, &a[1], &n1, &z__[1], &lda);
			    zlacpy_("Full", &n, &n, &b[1], &n1, &z__[1], &lda);
/* L270: */
			}
			s2 = dsecnd_();
			untime = s2 - s1;

/* Computing MAX */
			d__1 = time - untime;
			times_ref(ipar, itype, in, 4) = max(d__1,0.) / (
				doublereal) ic;
			opcnts_ref(ipar, itype, in, 4) = latime_1.ops / (
				doublereal) ic + dopla_("ZGEQRF", &n, &n, &
				c__0, &c__0, &nb) + dopla_("ZUNMQR"
				, &n, &n, &c__0, &c__0, &nb) + 
				dopla_("ZUNGQR", &n, &n, &c__0, &c__0, &nb);
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

             Time ZHGEQZ   

             Time ZHGEQZ with JOB='E' for each value of LDAS(j) */

	    if (timsub[4]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
		    if (lda < n1) {
			times_ref(ipar, itype, in, 5) = 0.;
			opcnts_ref(ipar, itype, in, 5) = 0.;
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

/*                    Time ZHGEQZ with JOB='E' */

			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();
L300:
			zlacpy_("Full", &n, &n, &h__[1], &ldh, &a[1], &lda);
			zlacpy_("Full", &n, &n, &t[1], &ldh, &b[1], &lda);
			zhgeqz_("E", "N", "N", &n, &c__1, &n, &a[1], &lda, &b[
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
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L300;
			}

/*                    Subtract the time used in ZLACPY. */

			s1 = dsecnd_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    zlacpy_("Full", &n, &n, &h__[1], &ldh, &z__[1], &
				    lda);
			    zlacpy_("Full", &n, &n, &t[1], &ldh, &z__[1], &
				    lda);
/* L310: */
			}
			s2 = dsecnd_();
			untime = s2 - s1;

/* Computing MAX */
			d__1 = time - untime;
			times_ref(ipar, itype, in, 5) = max(d__1,0.) / (
				doublereal) ic;
			opcnts_ref(ipar, itype, in, 5) = latime_1.ops / (
				doublereal) ic;
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

/*           Time ZHGEQZ with JOB='S', COMPQ=COMPZ='N' for each value   
             of LDAS(j) */

	    if (timsub[5]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
		    if (lda < n1) {
			times_ref(ipar, itype, in, 6) = 0.;
			opcnts_ref(ipar, itype, in, 6) = 0.;
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

/*                 Time ZHGEQZ with JOB='S', COMPQ=COMPZ='N' */

			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();
L340:
			zlacpy_("Full", &n, &n, &h__[1], &ldh, &a[1], &lda);
			zlacpy_("Full", &n, &n, &t[1], &ldh, &b[1], &lda);
			zhgeqz_("S", "N", "N", &n, &c__1, &n, &a[1], &lda, &b[
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
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L340;
			}

/*                 Subtract the time used in ZLACPY. */

			s1 = dsecnd_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    zlacpy_("Full", &n, &n, &h__[1], &ldh, &z__[1], &
				    lda);
			    zlacpy_("Full", &n, &n, &t[1], &ldh, &z__[1], &
				    lda);
/* L350: */
			}
			s2 = dsecnd_();
			untime = s2 - s1;

/* Computing MAX */
			d__1 = time - untime;
			times_ref(ipar, itype, in, 6) = max(d__1,0.) / (
				doublereal) ic;
			opcnts_ref(ipar, itype, in, 6) = latime_1.ops / (
				doublereal) ic;
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
		zlacpy_("Full", &n, &n, &h__[1], &ldh, &a[1], &n1);
		zlacpy_("Full", &n, &n, &t[1], &ldh, &b[1], &n1);
		zhgeqz_("S", "N", "N", &n, &c__1, &n, &a[1], &n1, &b[1], &n1, 
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

/*           Time ZHGEQZ with JOB='S', COMPQ='I', COMPZ='N' for each   
             value of LDAS(j) */

	    if (timsub[6]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
		    if (lda < n1) {
			times_ref(ipar, itype, in, 7) = 0.;
			opcnts_ref(ipar, itype, in, 7) = 0.;
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

/*                 Time ZHGEQZ with JOB='S', COMPQ='I', COMPZ='N' */

			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();
L380:
			zlacpy_("Full", &n, &n, &h__[1], &ldh, &a[1], &lda);
			zlacpy_("Full", &n, &n, &t[1], &ldh, &b[1], &lda);
			zhgeqz_("S", "I", "N", &n, &c__1, &n, &a[1], &lda, &b[
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
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L380;
			}

/*                 Subtract the time used in ZLACPY. */

			s1 = dsecnd_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    zlacpy_("Full", &n, &n, &h__[1], &ldh, &z__[1], &
				    lda);
			    zlacpy_("Full", &n, &n, &t[1], &ldh, &z__[1], &
				    lda);
/* L390: */
			}
			s2 = dsecnd_();
			untime = s2 - s1;

/* Computing MAX */
			d__1 = time - untime;
			times_ref(ipar, itype, in, 7) = max(d__1,0.) / (
				doublereal) ic;
			opcnts_ref(ipar, itype, in, 7) = latime_1.ops / (
				doublereal) ic;
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
		zlacpy_("Full", &n, &n, &h__[1], &ldh, &a[1], &n1);
		zlacpy_("Full", &n, &n, &t[1], &ldh, &b[1], &n1);
		zhgeqz_("S", "I", "N", &n, &c__1, &n, &a[1], &n1, &b[1], &n1, 
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

/*           Time ZHGEQZ with JOB='S', COMPQ='N', COMPZ='I' for each   
             value of LDAS(j) */

	    if (timsub[7]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
		    if (lda < n1) {
			times_ref(ipar, itype, in, 8) = 0.;
			opcnts_ref(ipar, itype, in, 8) = 0.;
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

/*                 Time ZHGEQZ with JOB='S', COMPQ='N', COMPZ='I'   
                   (Note that the "Z" matrix is stored in the array Q) */

			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();
L420:
			zlacpy_("Full", &n, &n, &h__[1], &ldh, &a[1], &lda);
			zlacpy_("Full", &n, &n, &t[1], &ldh, &b[1], &lda);
			zhgeqz_("S", "N", "I", &n, &c__1, &n, &a[1], &lda, &b[
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
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L420;
			}

/*                 Subtract the time used in ZLACPY. */

			s1 = dsecnd_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    zlacpy_("Full", &n, &n, &h__[1], &ldh, &z__[1], &
				    lda);
			    zlacpy_("Full", &n, &n, &t[1], &ldh, &z__[1], &
				    lda);
/* L430: */
			}
			s2 = dsecnd_();
			untime = s2 - s1;

/* Computing MAX */
			d__1 = time - untime;
			times_ref(ipar, itype, in, 8) = max(d__1,0.) / (
				doublereal) ic;
			opcnts_ref(ipar, itype, in, 8) = latime_1.ops / (
				doublereal) ic;
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

/*           Time ZHGEQZ with JOB='S', COMPQ='I', COMPZ='I' for each   
             value of LDAS(j) */

	    if (timsub[8]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
		    if (lda < n1) {
			times_ref(ipar, itype, in, 9) = 0.;
			opcnts_ref(ipar, itype, in, 9) = 0.;
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

/*                 Time ZHGEQZ with JOB='S', COMPQ='I', COMPZ='I' */

			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();
L460:
			zlacpy_("Full", &n, &n, &h__[1], &ldh, &a[1], &lda);
			zlacpy_("Full", &n, &n, &t[1], &ldh, &b[1], &lda);
			zhgeqz_("S", "I", "I", &n, &c__1, &n, &a[1], &lda, &b[
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
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L460;
			}

/*                 Subtract the time used in ZLACPY. */

			s1 = dsecnd_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    zlacpy_("Full", &n, &n, &h__[1], &ldh, &z__[1], &
				    lda);
			    zlacpy_("Full", &n, &n, &t[1], &ldh, &z__[1], &
				    lda);
/* L470: */
			}
			s2 = dsecnd_();
			untime = s2 - s1;

/* Computing MAX */
			d__1 = time - untime;
			times_ref(ipar, itype, in, 9) = max(d__1,0.) / (
				doublereal) ic;
			opcnts_ref(ipar, itype, in, 9) = latime_1.ops / (
				doublereal) ic;
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

             Time ZTGEVC */

	    if (timsub[9] || timsub[10] || timsub[11] || timsub[12]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
		    if (lda < n1) {
			for (j = 10; j <= 13; ++j) {
			    if (timsub[j - 1]) {
				times_ref(ipar, itype, in, j) = 0.;
				opcnts_ref(ipar, itype, in, j) = 0.;
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

/*                 Time ZTGEVC if this is a new value of LDA */

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

/*                    Time ZTGEVC for Left Eigenvectors only,   
                      without back transforming */

			if (timsub[9]) {
			    ic = 0;
			    latime_1.ops = 0.;
			    s1 = dsecnd_();
L550:
			    ztgevc_("L", "A", &llwork[1], &n, &a[1], &lda, &b[
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
			    s2 = dsecnd_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				goto L550;
			    }

			    times_ref(ipar, itype, in, 10) = time / (
				    doublereal) ic;
			    opcnts_ref(ipar, itype, in, 10) = latime_1.ops / (
				    doublereal) ic;
			}

/*                    Time ZTGEVC for Left Eigenvectors only,   
                      with back transforming */

			if (timsub[10]) {
			    ic = 0;
			    latime_1.ops = 0.;
			    s1 = dsecnd_();
L560:
			    zlacpy_("Full", &n, &n, &q[1], &ldq, &h__[1], &
				    lda);
			    ztgevc_("L", "B", &llwork[1], &n, &a[1], &lda, &b[
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
			    s2 = dsecnd_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				goto L560;
			    }

/*                       Subtract the time used in ZLACPY. */

			    s1 = dsecnd_();
			    i__4 = ic;
			    for (j = 1; j <= i__4; ++j) {
				zlacpy_("Full", &n, &n, &q[1], &ldq, &h__[1], 
					&lda);
/* L570: */
			    }
			    s2 = dsecnd_();
			    untime = s2 - s1;

/* Computing MAX */
			    d__1 = time - untime;
			    times_ref(ipar, itype, in, 11) = max(d__1,0.) / (
				    doublereal) ic;
			    opcnts_ref(ipar, itype, in, 11) = latime_1.ops / (
				    doublereal) ic;
			}

/*                    Time ZTGEVC for Right Eigenvectors only,   
                      without back transforming */

			if (timsub[11]) {
			    ic = 0;
			    latime_1.ops = 0.;
			    s1 = dsecnd_();
L580:
			    ztgevc_("R", "A", &llwork[1], &n, &a[1], &lda, &b[
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
			    s2 = dsecnd_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				goto L580;
			    }

			    times_ref(ipar, itype, in, 12) = time / (
				    doublereal) ic;
			    opcnts_ref(ipar, itype, in, 12) = latime_1.ops / (
				    doublereal) ic;
			}

/*                    Time ZTGEVC for Right Eigenvectors only,   
                      with back transforming */

			if (timsub[12]) {
			    ic = 0;
			    latime_1.ops = 0.;
			    s1 = dsecnd_();
L590:
			    zlacpy_("Full", &n, &n, &q[1], &ldq, &t[1], &lda);
			    ztgevc_("R", "B", &llwork[1], &n, &a[1], &lda, &b[
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
			    s2 = dsecnd_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				goto L590;
			    }

/*                       Subtract the time used in ZLACPY. */

			    s1 = dsecnd_();
			    i__4 = ic;
			    for (j = 1; j <= i__4; ++j) {
				zlacpy_("Full", &n, &n, &q[1], &ldq, &t[1], &
					lda);
/* L600: */
			    }
			    s2 = dsecnd_();
			    untime = s2 - s1;

/* Computing MAX */
			    d__1 = time - untime;
			    times_ref(ipar, itype, in, 13) = max(d__1,0.) / (
				    doublereal) ic;
			    opcnts_ref(ipar, itype, in, 13) = latime_1.ops / (
				    doublereal) ic;
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

		zlatm4_(&katype[itype - 1], &n, &c__3, &c__1, &c_true, &c_b26,
			 &ulp, &c_b26, &c__2, &iseed[1], &h__[1], &n1);
		if (n >= 3) {
		    i__3 = (n1 << 1) + 3;
		    h__[i__3].r = 1., h__[i__3].i = 0.;
		}

/*              Generate B (w/o rotation) */

		zlatm4_(&c__8, &n, &c__3, &c__1, &c_false, &c_b26, &c_b26, &
			c_b26, &c__2, &iseed[1], &t[1], &n1);
		if (n >= 2) {
		    i__3 = n1 + 2;
		    t[i__3].r = 1., t[i__3].i = 0.;
		}

		if (n > 0) {

/*                 Include rotations   

                   Generate U, V as Householder transformations times a   
                   diagonal matrix.  (Note that ZLARFG makes Q(jc+ic)   
                   and Z(jc+ic) real.) */

		    i__3 = n - 1;
		    for (jc = 1; jc <= i__3; ++jc) {
			ic = (jc - 1) * n1;
			i__4 = n;
			for (jr = jc; jr <= i__4; ++jr) {
			    i__5 = jr + ic;
			    zlarnd_(&z__1, &c__3, &iseed[1]);
			    q[i__5].r = z__1.r, q[i__5].i = z__1.i;
			    i__5 = jr + ic;
			    zlarnd_(&z__1, &c__3, &iseed[1]);
			    z__[i__5].r = z__1.r, z__[i__5].i = z__1.i;
/* L630: */
			}
			i__4 = n + 1 - jc;
			zlarfg_(&i__4, &q[jc + ic], &q[jc + 1 + ic], &c__1, &
				work[jc]);
			i__4 = (n << 1) + jc;
			i__5 = jc + ic;
			d__2 = q[i__5].r;
			d__1 = d_sign(&c_b26, &d__2);
			work[i__4].r = d__1, work[i__4].i = 0.;
			i__4 = jc + ic;
			q[i__4].r = 1., q[i__4].i = 0.;
			i__4 = n + 1 - jc;
			zlarfg_(&i__4, &z__[jc + ic], &z__[jc + 1 + ic], &
				c__1, &work[n + jc]);
			i__4 = n * 3 + jc;
			i__5 = jc + ic;
			d__2 = z__[i__5].r;
			d__1 = d_sign(&c_b26, &d__2);
			work[i__4].r = d__1, work[i__4].i = 0.;
			i__4 = jc + ic;
			z__[i__4].r = 1., z__[i__4].i = 0.;
/* L640: */
		    }
		    ic = (n - 1) * n1;
		    zlarnd_(&z__1, &c__3, &iseed[1]);
		    ctemp.r = z__1.r, ctemp.i = z__1.i;
		    i__3 = n + ic;
		    q[i__3].r = 1., q[i__3].i = 0.;
		    i__3 = n;
		    work[i__3].r = 0., work[i__3].i = 0.;
		    i__3 = n * 3;
		    d__1 = z_abs(&ctemp);
		    z__1.r = ctemp.r / d__1, z__1.i = ctemp.i / d__1;
		    work[i__3].r = z__1.r, work[i__3].i = z__1.i;
		    zlarnd_(&z__1, &c__3, &iseed[1]);
		    ctemp.r = z__1.r, ctemp.i = z__1.i;
		    i__3 = n + ic;
		    z__[i__3].r = 1., z__[i__3].i = 0.;
		    i__3 = n << 1;
		    work[i__3].r = 0., work[i__3].i = 0.;
		    i__3 = n << 2;
		    d__1 = z_abs(&ctemp);
		    z__1.r = ctemp.r / d__1, z__1.i = ctemp.i / d__1;
		    work[i__3].r = z__1.r, work[i__3].i = z__1.i;

/*                 Apply the diagonal matrices */

		    i__3 = n;
		    for (jc = 1; jc <= i__3; ++jc) {
			i__4 = n;
			for (jr = 1; jr <= i__4; ++jr) {
			    i__5 = jr + ic;
			    i__6 = (n << 1) + jr;
			    d_cnjg(&z__3, &work[n * 3 + jc]);
			    z__2.r = work[i__6].r * z__3.r - work[i__6].i * 
				    z__3.i, z__2.i = work[i__6].r * z__3.i + 
				    work[i__6].i * z__3.r;
			    i__7 = jr + ic;
			    z__1.r = z__2.r * h__[i__7].r - z__2.i * h__[i__7]
				    .i, z__1.i = z__2.r * h__[i__7].i + 
				    z__2.i * h__[i__7].r;
			    h__[i__5].r = z__1.r, h__[i__5].i = z__1.i;
			    i__5 = jr + ic;
			    i__6 = (n << 1) + jr;
			    i__7 = n * 3 + jc;
			    z__3.r = work[i__6].r * work[i__7].r - work[i__6]
				    .i * work[i__7].i, z__3.i = work[i__6].r *
				     work[i__7].i + work[i__6].i * work[i__7]
				    .r;
			    d_cnjg(&z__4, &work[n * 3 + jc]);
			    z__2.r = z__3.r * z__4.r - z__3.i * z__4.i, 
				    z__2.i = z__3.r * z__4.i + z__3.i * 
				    z__4.r;
			    i__8 = jr + ic;
			    z__1.r = z__2.r * t[i__8].r - z__2.i * t[i__8].i, 
				    z__1.i = z__2.r * t[i__8].i + z__2.i * t[
				    i__8].r;
			    t[i__5].r = z__1.r, t[i__5].i = z__1.i;
/* L650: */
			}
/* L660: */
		    }
		    i__3 = n - 1;
		    zunm2r_("L", "N", &n, &n, &i__3, &q[1], &n1, &work[1], &
			    h__[1], &n1, &work[(n << 1) + 1], &iinfo);
		    if (iinfo != 0) {
			goto L670;
		    }
		    i__3 = n - 1;
		    zunm2r_("R", "C", &n, &n, &i__3, &z__[1], &n1, &work[n + 
			    1], &h__[1], &n1, &work[(n << 1) + 1], &iinfo);
		    if (iinfo != 0) {
			goto L670;
		    }
		    i__3 = n - 1;
		    zunm2r_("L", "N", &n, &n, &i__3, &q[1], &n1, &work[1], &t[
			    1], &n1, &work[(n << 1) + 1], &iinfo);
		    if (iinfo != 0) {
			goto L670;
		    }
		    i__3 = n - 1;
		    zunm2r_("R", "C", &n, &n, &i__3, &z__[1], &n1, &work[n + 
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
		    ai[j] = d_imag(&h__[j]);
		    i__4 = j;
		    br[j] = t[i__4].r;
		    bi[j] = d_imag(&t[j]);
/* L680: */
		}
	    }

/*           Time CQZHES w/ MATZ=.FALSE. for each LDAS(j) */

	    if (timsub[13]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
		    if (lda < n1) {
			times_ref(ipar, itype, in, 14) = 0.;
			opcnts_ref(ipar, itype, in, 14) = 0.;
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
			latime_1.ops = 0.;
			s1 = dsecnd_();
L700:
			dlacpy_("Full", &n, &n, &ar[1], &n1, &hr[1], &lda);
			dlacpy_("Full", &n, &n, &ai[1], &n1, &hi[1], &lda);
			dlacpy_("Full", &n, &n, &br[1], &n1, &tr[1], &lda);
			dlacpy_("Full", &n, &n, &bi[1], &n1, &ti[1], &lda);
			cqzhes_(&lda, &n, &hr[1], &hi[1], &tr[1], &ti[1], &
				c_false, &qr[1], &qi[1]);
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L700;
			}

/*                    Subtract the time used in ZLACPY. */

			s1 = dsecnd_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    dlacpy_("Full", &n, &n, &ar[1], &n1, &zr[1], &lda);
			    dlacpy_("Full", &n, &n, &ai[1], &n1, &zi[1], &lda);
			    dlacpy_("Full", &n, &n, &br[1], &n1, &zr[1], &lda);
			    dlacpy_("Full", &n, &n, &bi[1], &n1, &zi[1], &lda);
/* L710: */
			}
			s2 = dsecnd_();
			untime = s2 - s1;

/* Computing MAX */
			d__1 = time - untime;
			times_ref(ipar, itype, in, 14) = max(d__1,0.) / (
				doublereal) ic;
			opcnts_ref(ipar, itype, in, 14) = latime_1.ops / (
				doublereal) ic;
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
		dlacpy_("Full", &n, &n, &ar[1], &n1, &hr[1], &n1);
		dlacpy_("Full", &n, &n, &ai[1], &n1, &hi[1], &n1);
		dlacpy_("Full", &n, &n, &br[1], &n1, &tr[1], &n1);
		dlacpy_("Full", &n, &n, &bi[1], &n1, &ti[1], &n1);
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
			times_ref(ipar, itype, in, 15) = 0.;
			opcnts_ref(ipar, itype, in, 15) = 0.;
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
			latime_1.ops = 0.;
			s1 = dsecnd_();
L740:
			dlacpy_("Full", &n, &n, &ar[1], &n1, &hr[1], &lda);
			dlacpy_("Full", &n, &n, &ai[1], &n1, &hi[1], &lda);
			dlacpy_("Full", &n, &n, &br[1], &n1, &tr[1], &lda);
			dlacpy_("Full", &n, &n, &bi[1], &n1, &ti[1], &lda);
			cqzhes_(&lda, &n, &hr[1], &hi[1], &tr[1], &ti[1], &
				c_true, &qr[1], &qi[1]);
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L740;
			}

/*                    Subtract the time used in ZLACPY. */

			s1 = dsecnd_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    dlacpy_("Full", &n, &n, &ar[1], &n1, &zr[1], &lda);
			    dlacpy_("Full", &n, &n, &ai[1], &n1, &zi[1], &lda);
			    dlacpy_("Full", &n, &n, &br[1], &n1, &zr[1], &lda);
			    dlacpy_("Full", &n, &n, &bi[1], &n1, &zi[1], &lda);
/* L750: */
			}
			s2 = dsecnd_();
			untime = s2 - s1;

/* Computing MAX */
			d__1 = time - untime;
			times_ref(ipar, itype, in, 15) = max(d__1,0.) / (
				doublereal) ic;
			opcnts_ref(ipar, itype, in, 15) = latime_1.ops / (
				doublereal) ic;
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
			times_ref(ipar, itype, in, 16) = 0.;
			opcnts_ref(ipar, itype, in, 16) = 0.;
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
			latime_1.ops = 0.;
			s1 = dsecnd_();
L780:
			dlacpy_("Full", &n, &n, &hr[1], &ldh, &ar[1], &lda);
			dlacpy_("Full", &n, &n, &hi[1], &ldh, &ai[1], &lda);
			dlacpy_("Full", &n, &n, &tr[1], &ldh, &br[1], &lda);
			dlacpy_("Full", &n, &n, &ti[1], &ldh, &bi[1], &lda);
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

			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L780;
			}

/*                    Subtract the time used in ZLACPY. */

			s1 = dsecnd_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    dlacpy_("Full", &n, &n, &hr[1], &ldh, &zr[1], &
				    lda);
			    dlacpy_("Full", &n, &n, &hi[1], &ldh, &zi[1], &
				    lda);
			    dlacpy_("Full", &n, &n, &tr[1], &ldh, &zr[1], &
				    lda);
			    dlacpy_("Full", &n, &n, &ti[1], &ldh, &zi[1], &
				    lda);
/* L790: */
			}
			s2 = dsecnd_();
			untime = s2 - s1;

/* Computing MAX */
			d__1 = time - untime;
			times_ref(ipar, itype, in, 16) = max(d__1,0.) / (
				doublereal) ic;
			opcnts_ref(ipar, itype, in, 16) = latime_1.ops / (
				doublereal) ic;
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
			times_ref(ipar, itype, in, 17) = 0.;
			opcnts_ref(ipar, itype, in, 17) = 0.;
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
			latime_1.ops = 0.;
			s1 = dsecnd_();
L820:
			dlacpy_("Full", &n, &n, &hr[1], &ldh, &ar[1], &lda);
			dlacpy_("Full", &n, &n, &hi[1], &ldh, &ai[1], &lda);
			dlacpy_("Full", &n, &n, &tr[1], &ldh, &br[1], &lda);
			dlacpy_("Full", &n, &n, &ti[1], &ldh, &bi[1], &lda);
			dlaset_("Full", &n, &n, &c_b419, &c_b26, &qr[1], &lda);
			dlaset_("Full", &n, &n, &c_b419, &c_b26, &qi[1], &lda);
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

			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L820;
			}

/*                    Subtract the time used in ZLACPY. */

			s1 = dsecnd_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    dlacpy_("Full", &n, &n, &hr[1], &ldh, &zr[1], &
				    lda);
			    dlacpy_("Full", &n, &n, &hi[1], &ldh, &zi[1], &
				    lda);
			    dlacpy_("Full", &n, &n, &tr[1], &ldh, &zr[1], &
				    lda);
			    dlacpy_("Full", &n, &n, &ti[1], &ldh, &zi[1], &
				    lda);
			    dlaset_("Full", &n, &n, &c_b419, &c_b26, &zr[1], &
				    lda);
			    dlaset_("Full", &n, &n, &c_b419, &c_b26, &zi[1], &
				    lda);
/* L830: */
			}
			s2 = dsecnd_();
			untime = s2 - s1;

/* Computing MAX */
			d__1 = time - untime;
			times_ref(ipar, itype, in, 17) = max(d__1,0.) / (
				doublereal) ic;
			opcnts_ref(ipar, itype, in, 17) = latime_1.ops / (
				doublereal) ic;
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
		dlacpy_("Full", &n, &n, &hr[1], &ldh, &ar[1], &n1);
		dlacpy_("Full", &n, &n, &hi[1], &ldh, &ai[1], &n1);
		dlacpy_("Full", &n, &n, &tr[1], &ldh, &br[1], &n1);
		dlacpy_("Full", &n, &n, &ti[1], &ldh, &bi[1], &n1);
		dlaset_("Full", &n, &n, &c_b419, &c_b26, &qr[1], &n1);
		dlaset_("Full", &n, &n, &c_b419, &c_b26, &qi[1], &n1);
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
			times_ref(ipar, itype, in, 18) = 0.;
			opcnts_ref(ipar, itype, in, 18) = 0.;
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
			latime_1.ops = 0.;
			s1 = dsecnd_();
L900:
			dlacpy_("Full", &n, &n, &ar[1], &lds, &hr[1], &lda);
			dlacpy_("Full", &n, &n, &ai[1], &lds, &hi[1], &lda);
			dlacpy_("Full", &n, &n, &br[1], &lds, &tr[1], &lda);
			dlacpy_("Full", &n, &n, &bi[1], &lds, &ti[1], &lda);
			dlacpy_("Full", &n, &n, &qr[1], &lds, &zr[1], &lda);
			dlacpy_("Full", &n, &n, &qi[1], &lds, &zi[1], &lda);
			cqzvec_(&lda, &n, &hr[1], &hi[1], &tr[1], &ti[1], &wr[
				1], &wr[lda + 1], &wr[(lda << 1) + 1], &zr[1],
				 &zi[1]);
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L900;
			}

/*                    Subtract the time used in ZLACPY. */

			s1 = dsecnd_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    dlacpy_("Full", &n, &n, &ar[1], &lds, &zr[1], &
				    lda);
			    dlacpy_("Full", &n, &n, &ai[1], &lds, &zi[1], &
				    lda);
			    dlacpy_("Full", &n, &n, &br[1], &lds, &zr[1], &
				    lda);
			    dlacpy_("Full", &n, &n, &bi[1], &lds, &zi[1], &
				    lda);
			    dlacpy_("Full", &n, &n, &qr[1], &lds, &zr[1], &
				    lda);
			    dlacpy_("Full", &n, &n, &qi[1], &lds, &zi[1], &
				    lda);
/* L910: */
			}
			s2 = dsecnd_();
			untime = s2 - s1;

/* Computing MAX */
			d__1 = time - untime;
			times_ref(ipar, itype, in, 18) = max(d__1,0.) / (
				doublereal) ic;
			opcnts_ref(ipar, itype, in, 18) = latime_1.ops / (
				doublereal) ic;
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
	    dprtbg_(subnam_ref(0, isub), &mtypes, &dotype[1], nsizes, &nn[1], 
		    &inparm[isub - 1], pnames, nparms, &ldas[1], &nnb[1], &
		    nshfts[1], &neisps[1], &minnbs[1], &minbks[1], &
		    opcnts_ref(1, 1, 1, isub), ldo1, ldo2, &times_ref(1, 1, 1,
		     isub), ldt1, ldt2, &rwork[1], &llwork[1], nout, (ftnlen)
		    11, (ftnlen)6);
	}
/* L950: */
    }

    return 0;

/*     End of ZTIM51 */


} /* ztim51_ */

#undef opcnts_ref
#undef subnam_ref
#undef times_ref


