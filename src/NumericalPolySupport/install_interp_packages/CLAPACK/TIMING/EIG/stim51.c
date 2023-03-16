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
static integer c__2 = 2;
static real c_b24 = 1.f;
static integer c__8 = 8;
static integer c__0 = 0;
static logical c_false = FALSE_;
static integer c__4 = 4;
static logical c_true = TRUE_;
static real c_b403 = 0.f;

/* Subroutine */ int stim51_(char *line, integer *nsizes, integer *nn, 
	integer *ntypes, logical *dotype, integer *nparms, integer *nnb, 
	integer *nshfts, integer *neisps, integer *minnbs, integer *minbks, 
	integer *ldas, real *timmin, integer *nout, integer *iseed, real *a, 
	real *b, real *h__, real *t, real *q, real *z__, real *w, real *work, 
	integer *lwork, logical *llwork, real *times, integer *ldt1, integer *
	ldt2, integer *ldt3, real *opcnts, integer *ldo1, integer *ldo2, 
	integer *ldo3, integer *info, ftnlen line_len)
{
    /* Initialized data */

    static char subnam[11*18] = "SGGHRD(N)  " "SGGHRD(Q)  " "SGGHRD(Z)  " 
	    "SGGHRD(Q,Z)" "SHGEQZ(E)  " "SHGEQZ(S)  " "SHGEQZ(Q)  " "SHGEQZ("
	    "Z)  " "SHGEQZ(Q,Z)" "STGEVC(L,A)" "STGEVC(L,B)" "STGEVC(R,A)" 
	    "STGEVC(R,B)" "QZHES(F)   " "QZHES(T)   " "QZIT(F)    " "QZIT(T)"
	    "    " "QZVEC      ";
    static integer inparm[18] = { 2,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1 };
    static char pnames[6*6] = "   LDA" "    NB" "    NS" " NEISP" " MINNB" 
	    "MINBLK";
    static integer katype[4] = { 5,8,7,9 };

    /* Format strings */
    static char fmt_9999[] = "(1x,a,\002 timing run not attempted -- N > LD"
	    "A\002,/)";
    static char fmt_9998[] = "(1x,a,\002 timing run not attempted -- LWORK t"
	    "oo small.\002,/)";
    static char fmt_9997[] = "(\002 STIM51: \002,a,\002 returned INFO=\002,i"
	    "6,\002.\002,/9x,\002N=\002,i6,\002, ITYPE=\002,i6,\002, IPAR="
	    "\002,i6,\002, ISEED=(\002,3(i5,\002,\002),i5,\002)\002)";

    /* System generated locals */
    integer opcnts_dim1, opcnts_dim2, opcnts_dim3, opcnts_offset, times_dim1, 
	    times_dim2, times_dim3, times_offset, i__1, i__2, i__3, i__4, 
	    i__5;
    real r__1;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);
    double r_sign(real *, real *);

    /* Local variables */
    static integer ipar;
    static real time;
    static integer isub, nmax;
    extern /* Subroutine */ int qzit_(integer *, integer *, real *, real *, 
	    real *, logical *, real *, integer *);
    static integer j, n, minnb, iinfo, itemp, lastl, neisp;
    extern doublereal sopla_(char *, integer *, integer *, integer *, integer 
	    *, integer *);
    extern /* Subroutine */ int qzvec_(integer *, integer *, real *, real *, 
	    real *, real *, real *, real *);
    static logical runeq, runes;
    static integer itype, j1, j2, j3, j4, n1;
    static real s1, s2;
    extern /* Subroutine */ int qzhes_(integer *, integer *, real *, real *, 
	    logical *, real *), qzval_(integer *, integer *, real *, real *, 
	    real *, real *, real *, logical *, real *);
    static logical runqz;
    extern /* Subroutine */ int slatm4_(integer *, integer *, integer *, 
	    integer *, integer *, real *, real *, real *, integer *, integer *
	    , real *, integer *);
    static integer ic, jc, nb;
    extern /* Subroutine */ int sorm2r_(char *, char *, integer *, integer *, 
	    integer *, real *, integer *, real *, real *, integer *, real *, 
	    integer *);
    static integer in, jr, ldamin;
    extern doublereal slamch_(char *), second_(void);
    static integer minblk, ioldsd[4];
    extern /* Subroutine */ int slarfg_(integer *, real *, real *, integer *, 
	    real *), atimin_(char *, char *, integer *, char *, logical *, 
	    integer *, integer *, ftnlen, ftnlen, ftnlen);
    static integer nbsmax;
    extern doublereal slarnd_(integer *, integer *);
    static integer nshift;
    extern /* Subroutine */ int slacpy_(char *, integer *, integer *, real *, 
	    integer *, real *, integer *), slaset_(char *, integer *, 
	    integer *, real *, real *, real *, integer *);
    static logical runhrd;
    static real untime;
    static logical runhes, timsub[18];
    extern /* Subroutine */ int shgeqz_(char *, char *, char *, integer *, 
	    integer *, integer *, real *, integer *, real *, integer *, real *
	    , real *, real *, real *, integer *, real *, integer *, real *, 
	    integer *, integer *), slaqzh_(logical *, 
	    logical *, integer *, integer *, integer *, real *, integer *, 
	    real *, integer *, real *, integer *, real *, integer *, real *, 
	    integer *), sprtbg_(char *, integer *, logical *, integer *, 
	    integer *, integer *, char *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, real *, integer *, 
	    integer *, real *, integer *, integer *, real *, logical *, 
	    integer *, ftnlen, ftnlen), stgevc_(char *, char *, logical *, 
	    integer *, real *, integer *, real *, integer *, real *, integer *
	    , real *, integer *, integer *, integer *, real *, integer *), xlaenv_(integer *, integer *);
    static integer mtypes, lda, ldh, ldq, lds, ldw;
    static real ulp;

    /* Fortran I/O blocks */
    static cilist io___10 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___11 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___37 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___42 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___43 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___44 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___45 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___46 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___49 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___50 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___51 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___52 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___57 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___58 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___60 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___61 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___62 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___63 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___64 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___65 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___67 = { 0, 0, 0, fmt_9997, 0 };



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

    STIM51 times the LAPACK routines for the real non-symmetric   
    generalized eigenvalue problem   A x = w B x.   

    For each N value in NN(1:NSIZES) and .TRUE. value in   
    DOTYPE(1:NTYPES), a pair of matrices will be generated and used to   
    test the selected routines.  Thus, NSIZES*(number of .TRUE. values   
    in DOTYPE) matrices will be generated.   

    Arguments   
    =========   

    LINE    (input) CHARACTER*80   
            The input line which requested this routine.  This line may   
            contain a subroutine name, such as SGGHRD, indicating that   
            only routine SGGHRD will be timed, or it may contain a   
            generic name, such as SHG.  In this case, the rest of the   
            line is scanned for the first 18 non-blank characters,   
            corresponding to the eighteen combinations of subroutine and   
            options:   
            LAPACK:                                     Table Heading:   
             1: SGGHRD(no Q, no Z) (+SGEQRF, etc.)      'SGGHRD(N)'   
             2: SGGHRD(Q only)     (+SGEQRF, etc.)      'SGGHRD(Q)'   
             3: SGGHRD(Z only)     (+SGEQRF, etc.)      'SGGHRD(Z)'   
             4: SGGHRD(Q and Z)    (+SGEQRF, etc.)      'SGGHRD(Q,Z)'   
             5: SHGEQZ(Eigenvalues only)                'SHGEQZ(E)'   
             6: SHGEQZ(Schur form only)                 'SHGEQZ(S)'   
             7: SHGEQZ(Schur form and Q)                'SHGEQZ(Q)'   
             8: SHGEQZ(Schur form and Z)                'SHGEQZ(Z)'   
             9: SHGEQZ(Schur form, Q and Z)             'SHGEQZ(Q,Z)'   
            10: STGEVC(SIDE='L', HOWMNY='A')            'STGEVC(L,A)'   
            11: STGEVC(SIDE='L', HOWMNY='B')            'STGEVC(L,B)'   
            12: STGEVC(SIDE='R', HOWMNY='A')            'STGEVC(R,A)'   
            13: STGEVC(SIDE='R', HOWMNY='B')            'STGEVC(R,B)'   
            EISPACK:                       Compare w/:  Table Heading:   
            14: QZHES w/ matz=.false.            1      'QZHES(F)'   
            15: QZHES w/ matz=.true.             3      'QZHES(T)'   
            16: QZIT and QZVAL w/ matz=.false.   5      'QZIT(F)'   
            17: QZIT and QZVAL w/ matz=.true.    8      'QZIT(T)'   
            18: QZVEC                           13      'QZVEC'   
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
            and  U T2 V , resp., where U and V are orthogonal, T1 is   
            block upper triangular (with 1x1 and 2x2 diagonal blocks),   
            and T2 is upper triangular.  T2 has random O(1) entries in   
            the strict upper triangle and ( 0, 1, 0, 1, 1, ..., 1, 0 )   
            on the diagonal, while T1 has random O(1) entries in the   
            strict (block) upper triangle, its block diagonal will have   
            the singular values:   
            (j=1)   0, 0, 1, 1, ULP,..., ULP, 0.   
            (j=2)   0, 0, 1, 1, 1-d, 1-2*d, ..., 1-(N-5)*d=ULP, 0.   

                                    2        N-5   
            (j=3)   0, 0, 1, 1, a, a , ..., a   =ULP, 0.   
            (j=4)   0, 0, 1, r1, r2, ..., r(N-4), 0, where r1, etc.   
                    are random numbers in (ULP,1).   

    NPARMS  (input) INTEGER   
            The number of values in each of the arrays NNB, NSHFTS,   
            NEISPS, and LDAS.  For each matrix A generated according to   
            NN and DOTYPE, tests will be run with (NB,NSHIFT,NEISP,LDA)=   
            (NNB(1), NSHFTS(1), NEISPS(1), LDAS(1)),...,   
            (NNB(NPARMS), NSHFTS(NPARMS), NEISPS(NPARMS), LDAS(NPARMS))   

    NNB     (input) INTEGER array, dimension (NPARMS)   
            The values of the blocksize ("NB") to be tested.  They must   
            be at least 1.  Currently, this is only used by SGEQRF,   
            etc., in the timing of SGGHRD.   

    NSHFTS  (input) INTEGER array, dimension (NPARMS)   
            The values of the number of shifts ("NSHIFT") to be tested.   
            (Currently not used.)   

    NEISPS  (input) INTEGER array, dimension (NPARMS)   
            The values of "NEISP", the size of largest submatrix to be   
            processed by SLAEQZ (EISPACK method), to be tested.   
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
            each call to STIM51   

    A       (workspace) REAL array, dimension   
                        (max(NN)*max(LDAS))   
            (a) During the testing of SGGHRD, "A", the original   
                left-hand-side matrix to be tested.   
            (b) Later, "S", the Schur form of the original "A" matrix.   

    B       (workspace) REAL array, dimension   
                        (max(NN)*max(LDAS))   
            (a) During the testing of SGGHRD, "B", the original   
                right-hand-side matrix to be tested.   
            (b) Later, "P", the Schur form of the original "B" matrix.   

    H       (workspace) REAL array, dimension   
                        (max(NN)*max(LDAS))   
            (a) During the testing of SGGHRD and SHGEQZ, "H", the   
                Hessenberg form of the original "A" matrix.   
            (b) During the testing of STGEVC, "L", the matrix of left   
                eigenvectors.   

    T       (workspace) REAL array, dimension   
                        (max(NN)*max(LDAS))   
            (a) During the testing of SGGHRD and SHGEQZ, "T", the   
                triangular form of the original "B" matrix.   
            (b) During the testing of STGEVC, "R", the matrix of right   
                eigenvectors.   

    Q       (workspace) REAL array, dimension   
                        (max(NN)*max(LDAS))   
            The orthogonal matrix on the left generated by SGGHRD.  If   
            SHGEQZ computes only Q or Z, then that matrix is stored here.   
            If both Q and Z are computed, the Q matrix goes here.   

    Z       (workspace) REAL array, dimension   
                        (max(NN)*max(LDAS))   
            The orthogonal matrix on the right generated by SGGHRD.   
            If SHGEQZ computes both Q and Z, the Z matrix is stored here.   
            Also used as scratch space for timing the SLACPY calls.   

    W       (workspace) REAL array, dimension (3*max(LDAS))   
            Treated as an LDA x 3 matrix whose 1st and 2nd columns hold   
            ALPHAR and ALPHAI, the real and imaginary parts of the   
            diagonal entries of "S" that would result from reducing "S"   
            and "P" simultaneously to triangular form), and whose 3rd   
            column holds BETA, the diagonal entries of "P" that would so   
            result.   

    WORK    (workspace) REAL array, dimension (LWORK)   

    LWORK   (input) INTEGER   
            Number of elements in WORK.  It must be at least   
            (a)  6*max(NN)   
            (b)  NSIZES*NTYPES*NPARMS   

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
    --b;
    --h__;
    --t;
    --q;
    --z__;
    --w;
    --work;
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

    atimin_("SHG", line, &c__18, subnam, timsub, nout, info, (ftnlen)3, (
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

/*     Check LWORK   

   Computing MAX */
    i__1 = (nbsmax + 1) * ((nbsmax << 1) + nmax + 1), i__2 = nmax * 6, i__1 = 
	    max(i__1,i__2), i__2 = *nsizes * *ntypes * *nparms;
    if (*lwork < max(i__1,i__2)) {
	*info = -24;
	io___11.ciunit = *nout;
	s_wsfe(&io___11);
	do_fio(&c__1, line, (ftnlen)6);
	e_wsfe();
	return 0;
    }

/*     Check to see whether SGGHRD or SHGEQZ must be run.   
          RUNHRD -- if SGGHRD must be run.   
          RUNES  -- if SHGEQZ must be run to get Schur form.   
          RUNEQ  -- if SHGEQZ must be run to get Schur form and Q. */

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

/*     Check to see whether QZHES or QZIT must be run.   

       RUNHES -- if QZHES must be run.   
       RUNQZ  -- if QZIT and QZVAL must be run (w/ MATZ=.TRUE.). */

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
		goto L920;
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

		slatm4_(&katype[itype - 1], &n, &c__3, &c__1, &c__2, &c_b24, &
			ulp, &c_b24, &c__2, &iseed[1], &a[1], &n1);
		if (3 <= n) {
		    a[(n1 << 1) + 3] = 1.f;
		}

/*              Generate B (w/o rotation) */

		slatm4_(&c__8, &n, &c__3, &c__1, &c__0, &c_b24, &c_b24, &
			c_b24, &c__2, &iseed[1], &b[1], &n1);
		if (2 <= n) {
		    b[n1 + 2] = 1.f;
		}

		if (n > 0) {

/*                 Include rotations   

                   Generate U, V as Householder transformations times   
                   a diagonal matrix. */

		    i__3 = n - 1;
		    for (jc = 1; jc <= i__3; ++jc) {
			ic = (jc - 1) * n1;
			i__4 = n;
			for (jr = jc; jr <= i__4; ++jr) {
			    q[jr + ic] = slarnd_(&c__3, &iseed[1]);
			    z__[jr + ic] = slarnd_(&c__3, &iseed[1]);
/* L80: */
			}
			i__4 = n + 1 - jc;
			slarfg_(&i__4, &q[jc + ic], &q[jc + 1 + ic], &c__1, &
				work[jc]);
			work[(n << 1) + jc] = r_sign(&c_b24, &q[jc + ic]);
			q[jc + ic] = 1.f;
			i__4 = n + 1 - jc;
			slarfg_(&i__4, &z__[jc + ic], &z__[jc + 1 + ic], &
				c__1, &work[n + jc]);
			work[n * 3 + jc] = r_sign(&c_b24, &z__[jc + ic]);
			z__[jc + ic] = 1.f;
/* L90: */
		    }
		    ic = (n - 1) * n1;
		    q[n + ic] = 1.f;
		    work[n] = 0.f;
		    r__1 = slarnd_(&c__2, &iseed[1]);
		    work[n * 3] = r_sign(&c_b24, &r__1);
		    z__[n + ic] = 1.f;
		    work[n * 2] = 0.f;
		    r__1 = slarnd_(&c__2, &iseed[1]);
		    work[n * 4] = r_sign(&c_b24, &r__1);

/*                 Apply the diagonal matrices */

		    i__3 = n;
		    for (jc = 1; jc <= i__3; ++jc) {
			i__4 = n;
			for (jr = 1; jr <= i__4; ++jr) {
			    a[jr + ic] = work[(n << 1) + jr] * work[n * 3 + 
				    jc] * a[jr + ic];
			    b[jr + ic] = work[(n << 1) + jr] * work[n * 3 + 
				    jc] * b[jr + ic];
/* L100: */
			}
/* L110: */
		    }
		    i__3 = n - 1;
		    sorm2r_("L", "N", &n, &n, &i__3, &q[1], &n1, &work[1], &a[
			    1], &n1, &work[(n << 1) + 1], &iinfo);
		    if (iinfo != 0) {
			goto L120;
		    }
		    i__3 = n - 1;
		    sorm2r_("R", "T", &n, &n, &i__3, &z__[1], &n1, &work[n + 
			    1], &a[1], &n1, &work[(n << 1) + 1], &iinfo);
		    if (iinfo != 0) {
			goto L120;
		    }
		    i__3 = n - 1;
		    sorm2r_("L", "N", &n, &n, &i__3, &q[1], &n1, &work[1], &b[
			    1], &n1, &work[(n << 1) + 1], &iinfo);
		    if (iinfo != 0) {
			goto L120;
		    }
		    i__3 = n - 1;
		    sorm2r_("R", "T", &n, &n, &i__3, &z__[1], &n1, &work[n + 
			    1], &b[1], &n1, &work[(n << 1) + 1], &iinfo);
		    if (iinfo != 0) {
			goto L120;
		    }
		}
L120:
		;
	    }

/* . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .   

             Time SGGHRD   

             Time SGEQRF+SGGHRD('N','N',...) for each pair   
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

/*                    Time SGGHRD, computing neither Q nor Z   
                      (Actually, time SGEQRF + SORMQR + SGGHRD.) */

			xlaenv_(&c__1, &nb);
			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L140:
			slacpy_("Full", &n, &n, &a[1], &n1, &h__[1], &lda);
			slacpy_("Full", &n, &n, &b[1], &n1, &t[1], &lda);
			slaqzh_(&c_false, &c_false, &n, &c__1, &n, &h__[1], &
				lda, &t[1], &lda, &q[1], &lda, &z__[1], &lda, 
				&work[1], &iinfo);
			if (iinfo != 0) {
			    io___37.ciunit = *nout;
			    s_wsfe(&io___37);
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
			    goto L920;
			}

			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L140;
			}

/*                    Subtract the time used in SLACPY. */

			s1 = second_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    slacpy_("Full", &n, &n, &a[1], &n1, &z__[1], &lda);
			    slacpy_("Full", &n, &n, &b[1], &n1, &z__[1], &lda);
/* L150: */
			}
			s2 = second_();
			untime = s2 - s1;

/* Computing MAX */
			r__1 = time - untime;
			times_ref(ipar, itype, in, 1) = dmax(r__1,0.f) / (
				real) ic;
			opcnts_ref(ipar, itype, in, 1) = latime_1.ops / (real)
				 ic + sopla_("SGEQRF", &n, &n, &c__0, &c__0, &
				nb) + sopla_("SORMQR", &n, &n, &
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
		slacpy_("Full", &n, &n, &a[1], &n1, &h__[1], &n1);
		slacpy_("Full", &n, &n, &b[1], &n1, &t[1], &n1);
		slaqzh_(&c_false, &c_false, &n, &c__1, &n, &h__[1], &n1, &t[1]
			, &n1, &q[1], &n1, &z__[1], &n1, &work[1], &iinfo);
		if (iinfo != 0) {
		    io___42.ciunit = *nout;
		    s_wsfe(&io___42);
		    do_fio(&c__1, subnam_ref(0, 1), (ftnlen)11);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    goto L920;
		}
		ldh = n;
	    }

/*           Time SGGHRD('I','N',...) for each pair (LDAS(j),NNB(j)) */

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

/*                    Time SGGHRD, computing Q but not Z   
                      (Actually, SGEQRF + SORMQR + SORGQR + SGGHRD.) */

			xlaenv_(&c__1, &nb);
			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L180:
			slacpy_("Full", &n, &n, &a[1], &n1, &h__[1], &lda);
			slacpy_("Full", &n, &n, &b[1], &n1, &t[1], &lda);
			slaqzh_(&c_true, &c_false, &n, &c__1, &n, &h__[1], &
				lda, &t[1], &lda, &q[1], &lda, &z__[1], &lda, 
				&work[1], &iinfo);
			if (iinfo != 0) {
			    io___43.ciunit = *nout;
			    s_wsfe(&io___43);
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
			    goto L920;
			}

			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L180;
			}

/*                    Subtract the time used in SLACPY. */

			s1 = second_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    slacpy_("Full", &n, &n, &a[1], &n1, &z__[1], &lda);
			    slacpy_("Full", &n, &n, &b[1], &n1, &z__[1], &lda);
/* L190: */
			}
			s2 = second_();
			untime = s2 - s1;

/* Computing MAX */
			r__1 = time - untime;
			times_ref(ipar, itype, in, 2) = dmax(r__1,0.f) / (
				real) ic;
			opcnts_ref(ipar, itype, in, 2) = latime_1.ops / (real)
				 ic + sopla_("SGEQRF", &n, &n, &c__0, &c__0, &
				nb) + sopla_("SORMQR", &n, &n, &
				c__0, &c__0, &nb) + sopla_("SORGQR"
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

/*           Time SGGHRD('N','I',...) for each pair (LDAS(j),NNB(j)) */

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

/*                    Time SGGHRD, computing Z but not Q   
                      (Actually, SGEQRF + SORMQR + SGGHRD.) */

			xlaenv_(&c__1, &nb);
			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L220:
			slacpy_("Full", &n, &n, &a[1], &n1, &h__[1], &lda);
			slacpy_("Full", &n, &n, &b[1], &n1, &t[1], &lda);
			slaqzh_(&c_false, &c_true, &n, &c__1, &n, &h__[1], &
				lda, &t[1], &lda, &q[1], &lda, &z__[1], &lda, 
				&work[1], &iinfo);
			if (iinfo != 0) {
			    io___44.ciunit = *nout;
			    s_wsfe(&io___44);
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
			    goto L920;
			}

			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L220;
			}

/*                    Subtract the time used in SLACPY. */

			s1 = second_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    slacpy_("Full", &n, &n, &a[1], &n1, &z__[1], &lda);
			    slacpy_("Full", &n, &n, &b[1], &n1, &z__[1], &lda);
/* L230: */
			}
			s2 = second_();
			untime = s2 - s1;

/* Computing MAX */
			r__1 = time - untime;
			times_ref(ipar, itype, in, 3) = dmax(r__1,0.f) / (
				real) ic;
			opcnts_ref(ipar, itype, in, 3) = latime_1.ops / (real)
				 ic + sopla_("SGEQRF", &n, &n, &c__0, &c__0, &
				nb) + sopla_("SORMQR", &n, &n, &
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

/*           Time SGGHRD('I','I',...) for each pair (LDAS(j),NNB(j)) */

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

/*                    Time SGGHRD, computing Q and Z   
                      (Actually, SGEQRF + SORMQR + SORGQR + SGGHRD.) */

			xlaenv_(&c__1, &nb);
			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L260:
			slacpy_("Full", &n, &n, &a[1], &n1, &h__[1], &lda);
			slacpy_("Full", &n, &n, &b[1], &n1, &t[1], &lda);
			slaqzh_(&c_true, &c_true, &n, &c__1, &n, &h__[1], &
				lda, &t[1], &lda, &q[1], &lda, &z__[1], &lda, 
				&work[1], &iinfo);
			if (iinfo != 0) {
			    io___45.ciunit = *nout;
			    s_wsfe(&io___45);
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
			    goto L920;
			}

			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L260;
			}

/*                    Subtract the time used in SLACPY. */

			s1 = second_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    slacpy_("Full", &n, &n, &a[1], &n1, &z__[1], &lda);
			    slacpy_("Full", &n, &n, &b[1], &n1, &z__[1], &lda);
/* L270: */
			}
			s2 = second_();
			untime = s2 - s1;

/* Computing MAX */
			r__1 = time - untime;
			times_ref(ipar, itype, in, 4) = dmax(r__1,0.f) / (
				real) ic;
			opcnts_ref(ipar, itype, in, 4) = latime_1.ops / (real)
				 ic + sopla_("SGEQRF", &n, &n, &c__0, &c__0, &
				nb) + sopla_("SORMQR", &n, &n, &
				c__0, &c__0, &nb) + sopla_("SORGQR"
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

             Time SHGEQZ   

             Time SHGEQZ with JOB='E' for each value of LDAS(j) */

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

/*                    Time SHGEQZ with JOB='E' */

			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L300:
			slacpy_("Full", &n, &n, &h__[1], &ldh, &a[1], &lda);
			slacpy_("Full", &n, &n, &t[1], &ldh, &b[1], &lda);
			shgeqz_("E", "N", "N", &n, &c__1, &n, &a[1], &lda, &b[
				1], &lda, &w[1], &w[lda + 1], &w[(lda << 1) + 
				1], &q[1], &lda, &z__[1], &lda, &work[1], 
				lwork, &iinfo);
			if (iinfo != 0) {
			    io___46.ciunit = *nout;
			    s_wsfe(&io___46);
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
			    goto L920;
			}
			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L300;
			}

/*                    Subtract the time used in SLACPY. */

			s1 = second_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    slacpy_("Full", &n, &n, &h__[1], &ldh, &z__[1], &
				    lda);
			    slacpy_("Full", &n, &n, &t[1], &ldh, &z__[1], &
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

/*           Time SHGEQZ with JOB='S', COMPQ=COMPZ='N' for each value   
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

/*                 Time SHGEQZ with JOB='S', COMPQ=COMPZ='N' */

			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L340:
			slacpy_("Full", &n, &n, &h__[1], &ldh, &a[1], &lda);
			slacpy_("Full", &n, &n, &t[1], &ldh, &b[1], &lda);
			shgeqz_("S", "N", "N", &n, &c__1, &n, &a[1], &lda, &b[
				1], &lda, &w[1], &w[lda + 1], &w[(lda << 1) + 
				1], &q[1], &lda, &z__[1], &lda, &work[1], 
				lwork, &iinfo);
			if (iinfo != 0) {
			    io___49.ciunit = *nout;
			    s_wsfe(&io___49);
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
			    goto L920;
			}
			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L340;
			}

/*                 Subtract the time used in SLACPY. */

			s1 = second_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    slacpy_("Full", &n, &n, &h__[1], &ldh, &z__[1], &
				    lda);
			    slacpy_("Full", &n, &n, &t[1], &ldh, &z__[1], &
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
		slacpy_("Full", &n, &n, &h__[1], &ldh, &a[1], &n1);
		slacpy_("Full", &n, &n, &t[1], &ldh, &b[1], &n1);
		shgeqz_("S", "N", "N", &n, &c__1, &n, &a[1], &n1, &b[1], &n1, 
			&w[1], &w[n1 + 1], &w[(n1 << 1) + 1], &q[1], &n1, &
			z__[1], &n1, &work[1], lwork, &iinfo);
		if (iinfo != 0) {
		    io___50.ciunit = *nout;
		    s_wsfe(&io___50);
		    do_fio(&c__1, subnam_ref(0, 6), (ftnlen)11);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    goto L920;
		}
		lds = n1;
		ldq = 0;
	    }

/*           Time SHGEQZ with JOB='S', COMPQ='I', COMPZ='N' for each   
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

/*                 Time SHGEQZ with JOB='S', COMPQ='I', COMPZ='N' */

			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L380:
			slacpy_("Full", &n, &n, &h__[1], &ldh, &a[1], &lda);
			slacpy_("Full", &n, &n, &t[1], &ldh, &b[1], &lda);
			shgeqz_("S", "I", "N", &n, &c__1, &n, &a[1], &lda, &b[
				1], &lda, &w[1], &w[lda + 1], &w[(lda << 1) + 
				1], &q[1], &lda, &z__[1], &lda, &work[1], 
				lwork, &iinfo);
			if (iinfo != 0) {
			    io___51.ciunit = *nout;
			    s_wsfe(&io___51);
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
			    goto L920;
			}
			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L380;
			}

/*                 Subtract the time used in SLACPY. */

			s1 = second_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    slacpy_("Full", &n, &n, &h__[1], &ldh, &z__[1], &
				    lda);
			    slacpy_("Full", &n, &n, &t[1], &ldh, &z__[1], &
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
		slacpy_("Full", &n, &n, &h__[1], &ldh, &a[1], &n1);
		slacpy_("Full", &n, &n, &t[1], &ldh, &b[1], &n1);
		shgeqz_("S", "I", "N", &n, &c__1, &n, &a[1], &n1, &b[1], &n1, 
			&w[1], &w[n1 + 1], &w[(n1 << 1) + 1], &q[1], &n1, &
			z__[1], &n1, &work[1], lwork, &iinfo);
		if (iinfo != 0) {
		    io___52.ciunit = *nout;
		    s_wsfe(&io___52);
		    do_fio(&c__1, subnam_ref(0, 7), (ftnlen)11);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    goto L920;
		}
		lds = n1;
		ldq = n1;
	    }

/*           Time SHGEQZ with JOB='S', COMPQ='N', COMPZ='I' for each   
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

/*                 Time SHGEQZ with JOB='S', COMPQ='N', COMPZ='I'   
                   (Note that the "Z" matrix is stored in the array Q) */

			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L420:
			slacpy_("Full", &n, &n, &h__[1], &ldh, &a[1], &lda);
			slacpy_("Full", &n, &n, &t[1], &ldh, &b[1], &lda);
			shgeqz_("S", "N", "I", &n, &c__1, &n, &a[1], &lda, &b[
				1], &lda, &w[1], &w[lda + 1], &w[(lda << 1) + 
				1], &z__[1], &lda, &q[1], &lda, &work[1], 
				lwork, &iinfo);
			if (iinfo != 0) {
			    io___57.ciunit = *nout;
			    s_wsfe(&io___57);
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
			    goto L920;
			}
			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L420;
			}

/*                 Subtract the time used in SLACPY. */

			s1 = second_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    slacpy_("Full", &n, &n, &h__[1], &ldh, &z__[1], &
				    lda);
			    slacpy_("Full", &n, &n, &t[1], &ldh, &z__[1], &
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

/*           Time SHGEQZ with JOB='S', COMPQ='I', COMPZ='I' for each   
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

/*                 Time SHGEQZ with JOB='S', COMPQ='I', COMPZ='I' */

			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L460:
			slacpy_("Full", &n, &n, &h__[1], &ldh, &a[1], &lda);
			slacpy_("Full", &n, &n, &t[1], &ldh, &b[1], &lda);
			shgeqz_("S", "I", "I", &n, &c__1, &n, &a[1], &lda, &b[
				1], &lda, &w[1], &w[lda + 1], &w[(lda << 1) + 
				1], &q[1], &lda, &z__[1], &lda, &work[1], 
				lwork, &iinfo);
			if (iinfo != 0) {
			    io___58.ciunit = *nout;
			    s_wsfe(&io___58);
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
			    goto L920;
			}
			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L460;
			}

/*                 Subtract the time used in SLACPY. */

			s1 = second_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    slacpy_("Full", &n, &n, &h__[1], &ldh, &z__[1], &
				    lda);
			    slacpy_("Full", &n, &n, &t[1], &ldh, &z__[1], &
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

             Time STGEVC */

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

/*                 Time STGEVC if this is a new value of LDA */

		    if (lastl == 0) {

/*                    Copy S (which is in A) and P (which is in B)   
                      if necessary to get right LDA. */

			if (lda > lds) {
			    for (jc = n; jc >= 1; --jc) {
				for (jr = n; jr >= 1; --jr) {
				    a[jr + (jc - 1) * lda] = a[jr + (jc - 1) *
					     lds];
				    b[jr + (jc - 1) * lda] = b[jr + (jc - 1) *
					     lds];
/* L510: */
				}
/* L520: */
			    }
			} else if (lda < lds) {
			    i__4 = n;
			    for (jc = 1; jc <= i__4; ++jc) {
				i__5 = n;
				for (jr = 1; jr <= i__5; ++jr) {
				    a[jr + (jc - 1) * lda] = a[jr + (jc - 1) *
					     lds];
				    b[jr + (jc - 1) * lda] = b[jr + (jc - 1) *
					     lds];
/* L530: */
				}
/* L540: */
			    }
			}
			lds = lda;

/*                    Time STGEVC for Left Eigenvectors only,   
                      without back transforming */

			if (timsub[9]) {
			    ic = 0;
			    latime_1.ops = 0.f;
			    s1 = second_();
L550:
			    stgevc_("L", "A", &llwork[1], &n, &a[1], &lda, &b[
				    1], &lda, &h__[1], &lda, &t[1], &lda, &n, 
				    &itemp, &work[1], &iinfo);
			    if (iinfo != 0) {
				io___60.ciunit = *nout;
				s_wsfe(&io___60);
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
				goto L920;
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

/*                    Time STGEVC for Left Eigenvectors only,   
                      with back transforming */

			if (timsub[10]) {
			    ic = 0;
			    latime_1.ops = 0.f;
			    s1 = second_();
L560:
			    slacpy_("Full", &n, &n, &q[1], &ldq, &h__[1], &
				    lda);
			    stgevc_("L", "B", &llwork[1], &n, &a[1], &lda, &b[
				    1], &lda, &h__[1], &lda, &t[1], &lda, &n, 
				    &itemp, &work[1], &iinfo);
			    if (iinfo != 0) {
				io___61.ciunit = *nout;
				s_wsfe(&io___61);
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
				goto L920;
			    }
			    s2 = second_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				goto L560;
			    }

/*                       Subtract the time used in SLACPY. */

			    s1 = second_();
			    i__4 = ic;
			    for (j = 1; j <= i__4; ++j) {
				slacpy_("Full", &n, &n, &q[1], &ldq, &h__[1], 
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

/*                    Time STGEVC for Right Eigenvectors only,   
                      without back transforming */

			if (timsub[11]) {
			    ic = 0;
			    latime_1.ops = 0.f;
			    s1 = second_();
L580:
			    stgevc_("R", "A", &llwork[1], &n, &a[1], &lda, &b[
				    1], &lda, &h__[1], &lda, &t[1], &lda, &n, 
				    &itemp, &work[1], &iinfo);
			    if (iinfo != 0) {
				io___62.ciunit = *nout;
				s_wsfe(&io___62);
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
				goto L920;
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

/*                    Time STGEVC for Right Eigenvectors only,   
                      with back transforming */

			if (timsub[12]) {
			    ic = 0;
			    latime_1.ops = 0.f;
			    s1 = second_();
L590:
			    slacpy_("Full", &n, &n, &q[1], &ldq, &t[1], &lda);
			    stgevc_("R", "B", &llwork[1], &n, &a[1], &lda, &b[
				    1], &lda, &h__[1], &lda, &t[1], &lda, &n, 
				    &itemp, &work[1], &iinfo);
			    if (iinfo != 0) {
				io___63.ciunit = *nout;
				s_wsfe(&io___63);
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
				goto L920;
			    }
			    s2 = second_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				goto L590;
			    }

/*                       Subtract the time used in SLACPY. */

			    s1 = second_();
			    i__4 = ic;
			    for (j = 1; j <= i__4; ++j) {
				slacpy_("Full", &n, &n, &q[1], &ldq, &t[1], &
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

		slatm4_(&katype[itype - 1], &n, &c__3, &c__1, &c__2, &c_b24, &
			ulp, &c_b24, &c__2, &iseed[1], &a[1], &n1);
		if (3 <= n) {
		    a[(n1 << 1) + 3] = 1.f;
		}

/*              Generate B (w/o rotation) */

		slatm4_(&c__8, &n, &c__3, &c__1, &c__0, &c_b24, &c_b24, &
			c_b24, &c__2, &iseed[1], &b[1], &n1);
		if (2 <= n) {
		    b[n1 + 2] = 1.f;
		}

		if (n > 0) {

/*                 Include rotations   

                   Generate U, V as Householder transformations times   
                   a diagonal matrix. */

		    i__3 = n - 1;
		    for (jc = 1; jc <= i__3; ++jc) {
			ic = (jc - 1) * n1;
			i__4 = n;
			for (jr = jc; jr <= i__4; ++jr) {
			    q[jr + ic] = slarnd_(&c__3, &iseed[1]);
			    z__[jr + ic] = slarnd_(&c__3, &iseed[1]);
/* L630: */
			}
			i__4 = n + 1 - jc;
			slarfg_(&i__4, &q[jc + ic], &q[jc + 1 + ic], &c__1, &
				work[jc]);
			work[(n << 1) + jc] = r_sign(&c_b24, &q[jc + ic]);
			q[jc + ic] = 1.f;
			i__4 = n + 1 - jc;
			slarfg_(&i__4, &z__[jc + ic], &z__[jc + 1 + ic], &
				c__1, &work[n + jc]);
			work[n * 3 + jc] = r_sign(&c_b24, &z__[jc + ic]);
			z__[jc + ic] = 1.f;
/* L640: */
		    }
		    ic = (n - 1) * n1;
		    q[n + ic] = 1.f;
		    work[n] = 0.f;
		    r__1 = slarnd_(&c__2, &iseed[1]);
		    work[n * 3] = r_sign(&c_b24, &r__1);
		    z__[n + ic] = 1.f;
		    work[n * 2] = 0.f;
		    r__1 = slarnd_(&c__2, &iseed[1]);
		    work[n * 4] = r_sign(&c_b24, &r__1);

/*                 Apply the diagonal matrices */

		    i__3 = n;
		    for (jc = 1; jc <= i__3; ++jc) {
			i__4 = n;
			for (jr = 1; jr <= i__4; ++jr) {
			    a[jr + ic] = work[(n << 1) + jr] * work[n * 3 + 
				    jc] * a[jr + ic];
			    b[jr + ic] = work[(n << 1) + jr] * work[n * 3 + 
				    jc] * b[jr + ic];
/* L650: */
			}
/* L660: */
		    }
		    i__3 = n - 1;
		    sorm2r_("L", "N", &n, &n, &i__3, &q[1], &n1, &work[1], &a[
			    1], &n1, &work[(n << 1) + 1], &iinfo);
		    if (iinfo != 0) {
			goto L670;
		    }
		    i__3 = n - 1;
		    sorm2r_("R", "T", &n, &n, &i__3, &z__[1], &n1, &work[n + 
			    1], &a[1], &n1, &work[(n << 1) + 1], &iinfo);
		    if (iinfo != 0) {
			goto L670;
		    }
		    i__3 = n - 1;
		    sorm2r_("L", "N", &n, &n, &i__3, &q[1], &n1, &work[1], &b[
			    1], &n1, &work[(n << 1) + 1], &iinfo);
		    if (iinfo != 0) {
			goto L670;
		    }
		    i__3 = n - 1;
		    sorm2r_("R", "T", &n, &n, &i__3, &z__[1], &n1, &work[n + 
			    1], &b[1], &n1, &work[(n << 1) + 1], &iinfo);
		    if (iinfo != 0) {
			goto L670;
		    }
		}
L670:
		;
	    }

/*           Time QZHES w/ MATZ=.FALSE. for each LDAS(j) */

	    if (timsub[13]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
		    if (lda < n1) {
			times_ref(ipar, itype, in, 14) = 0.f;
			opcnts_ref(ipar, itype, in, 14) = 0.f;
			goto L710;
		    }

/*                 If this value of LDA has come up before, just use   
                   the value previously computed. */

		    lastl = 0;
		    i__4 = ipar - 1;
		    for (j = 1; j <= i__4; ++j) {
			if (lda == ldas[j]) {
			    lastl = j;
			}
/* L680: */
		    }

		    if (lastl == 0) {

/*                    Time QZHES( ...,.FALSE.,..) */

			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L690:
			slacpy_("Full", &n, &n, &a[1], &n1, &h__[1], &lda);
			slacpy_("Full", &n, &n, &b[1], &n1, &t[1], &lda);
			qzhes_(&lda, &n, &h__[1], &t[1], &c_false, &q[1]);
			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L690;
			}

/*                    Subtract the time used in SLACPY. */

			s1 = second_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    slacpy_("Full", &n, &n, &a[1], &n1, &z__[1], &lda);
			    slacpy_("Full", &n, &n, &b[1], &n1, &z__[1], &lda);
/* L700: */
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
L710:
		    ;
		}
	    } else if (runhes) {
		slacpy_("Full", &n, &n, &a[1], &n1, &h__[1], &n1);
		slacpy_("Full", &n, &n, &b[1], &n1, &t[1], &n1);
		qzhes_(&n1, &n, &h__[1], &t[1], &c_false, &q[1]);
		ldh = n1;
	    }

/*           Time QZHES w/ MATZ=.TRUE. for each LDAS(j) */

	    if (timsub[14]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
		    if (lda < n1) {
			times_ref(ipar, itype, in, 15) = 0.f;
			opcnts_ref(ipar, itype, in, 15) = 0.f;
			goto L750;
		    }

/*                 If this value of LDA has come up before, just use   
                   the value previously computed. */

		    lastl = 0;
		    i__4 = ipar - 1;
		    for (j = 1; j <= i__4; ++j) {
			if (lda == ldas[j]) {
			    lastl = j;
			}
/* L720: */
		    }

		    if (lastl == 0) {

/*                    Time QZHES( ...,.TRUE.,..) */

			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L730:
			slacpy_("Full", &n, &n, &a[1], &n1, &h__[1], &lda);
			slacpy_("Full", &n, &n, &b[1], &n1, &t[1], &lda);
			qzhes_(&lda, &n, &h__[1], &t[1], &c_true, &q[1]);
			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L730;
			}

/*                    Subtract the time used in SLACPY. */

			s1 = second_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    slacpy_("Full", &n, &n, &a[1], &n1, &z__[1], &lda);
			    slacpy_("Full", &n, &n, &b[1], &n1, &z__[1], &lda);
/* L740: */
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
L750:
		    ;
		}
	    }

/*           Time QZIT and QZVAL w/ MATZ=.FALSE. for each LDAS(j) */

	    if (timsub[15]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
		    if (lda < n1) {
			times_ref(ipar, itype, in, 16) = 0.f;
			opcnts_ref(ipar, itype, in, 16) = 0.f;
			goto L790;
		    }

/*                 If this value of LDA has come up before, just use   
                   the value previously computed. */

		    lastl = 0;
		    i__4 = ipar - 1;
		    for (j = 1; j <= i__4; ++j) {
			if (lda == ldas[j]) {
			    lastl = j;
			}
/* L760: */
		    }

		    if (lastl == 0) {

/*                    Time QZIT and QZVAL with MATZ=.FALSE. */

			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L770:
			slacpy_("Full", &n, &n, &h__[1], &ldh, &a[1], &lda);
			slacpy_("Full", &n, &n, &t[1], &ldh, &b[1], &lda);
			qzit_(&lda, &n, &a[1], &b[1], &c_b403, &c_false, &q[1]
				, &iinfo);
			if (iinfo != 0) {
			    io___64.ciunit = *nout;
			    s_wsfe(&io___64);
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
			    goto L920;
			}

			qzval_(&lda, &n, &a[1], &b[1], &w[1], &w[lda + 1], &w[
				(lda << 1) + 1], &c_false, &q[1]);
			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L770;
			}

/*                    Subtract the time used in SLACPY. */

			s1 = second_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    slacpy_("Full", &n, &n, &h__[1], &ldh, &z__[1], &
				    lda);
			    slacpy_("Full", &n, &n, &t[1], &ldh, &z__[1], &
				    lda);
/* L780: */
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
L790:
		    ;
		}
	    }

/*           Time QZIT and QZVAL w/ MATZ=.TRUE. for each LDAS(j) */

	    if (timsub[16]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
		    if (lda < n1) {
			times_ref(ipar, itype, in, 17) = 0.f;
			opcnts_ref(ipar, itype, in, 17) = 0.f;
			goto L830;
		    }

/*                 If this value of LDA has come up before, just use   
                   the value previously computed. */

		    lastl = 0;
		    i__4 = ipar - 1;
		    for (j = 1; j <= i__4; ++j) {
			if (lda == ldas[j]) {
			    lastl = j;
			}
/* L800: */
		    }

		    if (lastl == 0) {

/*                    Time QZIT and QZVAL with MATZ=.TRUE. */

			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L810:
			slacpy_("Full", &n, &n, &h__[1], &ldh, &a[1], &lda);
			slacpy_("Full", &n, &n, &t[1], &ldh, &b[1], &lda);
			slaset_("Full", &n, &n, &c_b403, &c_b24, &q[1], &lda);
			qzit_(&lda, &n, &a[1], &b[1], &c_b403, &c_true, &q[1],
				 &iinfo);
			if (iinfo != 0) {
			    io___65.ciunit = *nout;
			    s_wsfe(&io___65);
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
			    goto L920;
			}

			qzval_(&lda, &n, &a[1], &b[1], &w[1], &w[lda + 1], &w[
				(lda << 1) + 1], &c_true, &q[1]);
			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L810;
			}

/*                    Subtract the time used in SLACPY. */

			s1 = second_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    slacpy_("Full", &n, &n, &h__[1], &ldh, &z__[1], &
				    lda);
			    slacpy_("Full", &n, &n, &t[1], &ldh, &z__[1], &
				    lda);
			    slaset_("Full", &n, &n, &c_b403, &c_b24, &z__[1], 
				    &lda);
/* L820: */
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
L830:
		    ;
		}
	    } else if (runqz) {
		slacpy_("Full", &n, &n, &h__[1], &ldh, &a[1], &n1);
		slacpy_("Full", &n, &n, &t[1], &ldh, &b[1], &n1);
		slaset_("Full", &n, &n, &c_b403, &c_b24, &q[1], &n1);
		qzit_(&n1, &n, &a[1], &b[1], &c_b403, &c_true, &q[1], &iinfo);
		if (iinfo != 0) {
		    io___67.ciunit = *nout;
		    s_wsfe(&io___67);
		    do_fio(&c__1, subnam_ref(0, 17), (ftnlen)11);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    goto L920;
		}

		qzval_(&n1, &n, &a[1], &b[1], &w[1], &w[n1 + 1], &w[(n1 << 1) 
			+ 1], &c_true, &q[1]);
		lds = n1;
		ldw = n1;
	    }

/*           Time QZVEC for each LDAS(j) */

	    if (timsub[17]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
		    if (lda < n1) {
			times_ref(ipar, itype, in, 18) = 0.f;
			opcnts_ref(ipar, itype, in, 18) = 0.f;
			goto L910;
		    }

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

/*                    Copy W if necessary to get right LDA. */

			if (lda > ldw) {
			    for (jc = 3; jc >= 1; --jc) {
				for (jr = n; jr >= 1; --jr) {
				    w[jr + (jc - 1) * lda] = w[jr + (jc - 1) *
					     ldw];
/* L850: */
				}
/* L860: */
			    }
			} else if (lda < ldw) {
			    for (jc = 1; jc <= 3; ++jc) {
				i__4 = n;
				for (jr = 1; jr <= i__4; ++jr) {
				    w[jr + (jc - 1) * lda] = w[jr + (jc - 1) *
					     ldw];
/* L870: */
				}
/* L880: */
			    }
			}
			ldw = lda;

/*                    Time QZVEC */

			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L890:
			slacpy_("Full", &n, &n, &a[1], &lds, &h__[1], &lda);
			slacpy_("Full", &n, &n, &b[1], &lds, &t[1], &lda);
			slacpy_("Full", &n, &n, &q[1], &lds, &z__[1], &lda);
			qzvec_(&lda, &n, &h__[1], &t[1], &w[1], &w[lda + 1], &
				w[(lda << 1) + 1], &z__[1]);
			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L890;
			}

/*                    Subtract the time used in SLACPY. */

			s1 = second_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    slacpy_("Full", &n, &n, &a[1], &lds, &z__[1], &
				    lda);
			    slacpy_("Full", &n, &n, &b[1], &lds, &z__[1], &
				    lda);
			    slacpy_("Full", &n, &n, &q[1], &lds, &z__[1], &
				    lda);
/* L900: */
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
L910:
		    ;
		}
	    }

L920:
	    ;
	}
/* L930: */
    }

/*     Print a table of results for each timed routine. */

    for (isub = 1; isub <= 18; ++isub) {
	if (timsub[isub - 1]) {
	    sprtbg_(subnam_ref(0, isub), &mtypes, &dotype[1], nsizes, &nn[1], 
		    &inparm[isub - 1], pnames, nparms, &ldas[1], &nnb[1], &
		    nshfts[1], &neisps[1], &minnbs[1], &minbks[1], &
		    opcnts_ref(1, 1, 1, isub), ldo1, ldo2, &times_ref(1, 1, 1,
		     isub), ldt1, ldt2, &work[1], &llwork[1], nout, (ftnlen)
		    11, (ftnlen)6);
	}
/* L940: */
    }

    return 0;

/*     End of STIM51 */


} /* stim51_ */

#undef opcnts_ref
#undef subnam_ref
#undef times_ref


