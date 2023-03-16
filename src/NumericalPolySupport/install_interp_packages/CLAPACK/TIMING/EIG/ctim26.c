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

static complex c_b1 = {1.f,0.f};
static complex c_b2 = {2.f,0.f};
static integer c__17 = 17;
static integer c__1 = 1;
static integer c__9 = 9;
static integer c__25 = 25;
static real c_b27 = 1.f;
static integer c__6 = 6;
static real c_b32 = 0.f;
static integer c__0 = 0;
static integer c__5 = 5;
static integer c__2 = 2;
static integer c__3 = 3;
static integer c__4 = 4;
static integer c__20 = 20;
static integer c__10 = 10;
static integer c__21 = 21;

/* Subroutine */ int ctim26_(char *line, integer *nsizes, integer *nn, 
	integer *mm, integer *ntypes, logical *dotype, integer *nparms, 
	integer *nnb, integer *ldas, real *timmin, integer *nout, integer *
	iseed, complex *a, complex *h__, complex *u, complex *vt, real *d__, 
	complex *dc, real *e, complex *ec, complex *taup, complex *tauq, 
	complex *work, integer *lwork, real *rwork, integer *iwork, logical *
	llwork, real *times, integer *ldt1, integer *ldt2, integer *ldt3, 
	real *opcnts, integer *ldo1, integer *ldo2, integer *ldo3, integer *
	info, ftnlen line_len)
{
    /* Initialized data */

    static char subnam[9*17] = "CGEBRD   " "CBDSQR   " "CBDSQR(L)" "CBDSQR(R)"
	     "CBDSQR(B)" "CBDSQR(V)" "LAPSVD   " "LAPSVD(l)" "LAPSVD(L)" 
	    "LAPSVD(R)" "LAPSVD(B)" "CGESDD(B)" "LINSVD   " "LINSVD(l)" "LIN"
	    "SVD(L)" "LINSVD(R)" "LINSVD(B)";
    static integer inparm[17] = { 2,1,1,1,1,1,2,2,2,2,2,2,1,1,1,1,1 };
    static char pnames[4*2] = "LDA " "NB  ";
    static integer kmode[3] = { 4,3,1 };

    /* Format strings */
    static char fmt_9999[] = "(1x,a,\002 timing run not attempted\002,/)";
    static char fmt_9998[] = "(\002 CTIM26: \002,a,\002 returned INFO=\002,i"
	    "6,\002.\002,/9x,\002M=\002,i6,\002, N=\002,i6,\002, ITYPE=\002,i"
	    "6,\002, IPAR=\002,i6,\002,         \002,\002        ISEED=(\002,"
	    "4(i5,\002,\002),i5,\002)\002)";

    /* System generated locals */
    integer opcnts_dim1, opcnts_dim2, opcnts_dim3, opcnts_offset, times_dim1, 
	    times_dim2, times_dim3, times_offset, i__1, i__2, i__3, i__4, 
	    i__5, i__6, i__7;
    real r__1;
    complex q__1, q__2;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);
    double log(doublereal), exp(doublereal);

    /* Local variables */
    static integer ipar;
    static real time;
    static integer jdum[1], isub;
    static char uplo[1];
    static integer j, m, n, imode;
    extern /* Subroutine */ int csvdc_(complex *, integer *, integer *, 
	    integer *, complex *, complex *, complex *, integer *, complex *, 
	    integer *, complex *, integer *, integer *);
    static integer iinfo;
    static real conds;
    static integer minmn;
    extern doublereal sopla_(char *, integer *, integer *, integer *, integer 
	    *, integer *);
    static integer itype, j1, j2, j3, j4;
    extern /* Subroutine */ int scopy_(integer *, real *, integer *, real *, 
	    integer *);
    static real s1, s2;
    extern doublereal sopla2_(char *, char *, integer *, integer *, integer *,
	     integer *, integer *);
    static integer ic, nb;
    extern /* Subroutine */ int cgebrd_(integer *, integer *, complex *, 
	    integer *, real *, real *, complex *, complex *, complex *, 
	    integer *, integer *);
    static integer in;
    extern /* Subroutine */ int cgesdd_(char *, integer *, integer *, complex 
	    *, integer *, real *, complex *, integer *, complex *, integer *, 
	    complex *, integer *, real *, integer *, integer *);
    static integer ku;
    extern /* Complex */ VOID clarnd_(complex *, integer *, integer *);
    extern doublereal slamch_(char *), second_(void);
    extern /* Subroutine */ int clacpy_(char *, integer *, integer *, complex 
	    *, integer *, complex *, integer *), claset_(char *, 
	    integer *, integer *, complex *, complex *, complex *, integer *);
    static integer ioldsd[4];
    extern /* Subroutine */ int cbdsqr_(char *, integer *, integer *, integer 
	    *, integer *, real *, real *, complex *, integer *, complex *, 
	    integer *, complex *, integer *, real *, integer *), 
	    cungbr_(char *, integer *, integer *, integer *, complex *, 
	    integer *, complex *, complex *, integer *, integer *), 
	    atimin_(char *, char *, integer *, char *, logical *, integer *, 
	    integer *, ftnlen, ftnlen, ftnlen), clatmr_(integer *, integer *, 
	    char *, integer *, char *, complex *, integer *, real *, complex *
	    , char *, char *, complex *, integer *, real *, complex *, 
	    integer *, real *, char *, integer *, integer *, integer *, real *
	    , real *, char *, complex *, integer *, integer *, integer *);
    extern doublereal slarnd_(integer *, integer *);
    extern /* Subroutine */ int clatms_(integer *, integer *, char *, integer 
	    *, char *, real *, integer *, real *, real *, integer *, integer *
	    , char *, complex *, integer *, complex *, integer *);
    static logical trnbrd, runbrd;
    static integer lastnl;
    extern /* Subroutine */ int xlaenv_(integer *, integer *);
    static real untime;
    static logical timsub[17];
    static real ulpinv;
    extern /* Subroutine */ int sprtbv_(char *, integer *, logical *, integer 
	    *, integer *, integer *, integer *, char *, integer *, integer *, 
	    integer *, real *, integer *, integer *, real *, integer *, 
	    integer *, real *, logical *, integer *, ftnlen, ftnlen);
    static integer mtypes, lda;
    static real ulp;
    static integer kvt;

    /* Fortran I/O blocks */
    static cilist io___7 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___9 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___36 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___40 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___42 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___43 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___44 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___45 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___46 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___47 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___48 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___49 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___50 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___51 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___52 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___53 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___54 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___55 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___56 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___57 = { 0, 0, 0, fmt_9998, 0 };



#define times_ref(a_1,a_2,a_3,a_4) times[(((a_4)*times_dim3 + (a_3))*\
times_dim2 + (a_2))*times_dim1 + a_1]
#define subnam_ref(a_0,a_1) &subnam[(a_1)*9 + a_0 - 9]
#define opcnts_ref(a_1,a_2,a_3,a_4) opcnts[(((a_4)*opcnts_dim3 + (a_3))*\
opcnts_dim2 + (a_2))*opcnts_dim1 + a_1]


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   


    Purpose   
    =======   

       CTIM26 times the LAPACK routines for the COMPLEX singular value   
       decomposition.   

       For each N value in NN(1:NSIZES), M value in MM(1:NSIZES),   
       and .TRUE. value in DOTYPE(1:NTYPES), a matrix will be generated   
       and used to test the selected routines.  Thus, NSIZES*(number of   
       .TRUE. values in DOTYPE) matrices will be generated.   

    Arguments   
    =========   

    LINE    (input) CHARACTER*80   
            On entry, LINE contains the input line which requested   
            this routine.  This line may contain a subroutine name,   
            such as CGEBRD, indicating that only routine CGEBRD will   
            be timed, or it may contain a generic name, such as CBD.   
            In this case, the rest of the line is scanned for the   
            first 11 non-blank characters, corresponding to the eleven   
            combinations of subroutine and options:   
            LAPACK:   
             1: CGEBRD   
                (labeled CGEBRD in the output)   
             2: CBDSQR (singular values only)   
                (labeled CBDSQR in the output)   
             3: CBDSQR (singular values and left singular vectors;   
                        assume original matrix M by N)   
                (labeled CBDSQR(L) in the output)   
             4: CBDSQR (singular values and right singular vectors;   
                        assume original matrix M by N)   
                (labeled CBDSQR(R) in the output)   
             5: CBDSQR (singular values and left and right singular   
                        vectors; assume original matrix M by N)   
                (labeled CBDSQR(B) in the output)   
             6: CBDSQR (singular value and multiply square MIN(M,N)   
                        matrix by transpose of left singular vectors)   
                (labeled CBDSQR(V) in the output)   
             7: CGEBRD+CBDSQR (singular values only)   
                (labeled LAPSVD in the output)   
             8: CGEBRD+CUNGBR+CBDSQR(L) (singular values and min(M,N)   
                                         left singular vectors)   
                (labeled LAPSVD(l) in the output)   
             9: CGEBRD+CUNGBR+CBDSQR(L) (singular values and M left   
                                         singular vectors)   
                (labeled LAPSVD(L) in the output)   
            10: CGEBRD+CUNGBR+CBDSQR(R) (singular values and N right   
                                         singular vectors)   
                (labeled LAPSVD(R) in the output)   
            11: CGEBRD+CUNGBR+CBDSQR(B) (singular values and min(M,N)   
                                         left singular vectors and N   
                                         right singular vectors)   
                (labeled LAPSVD(B) in the output)   
            12: CGESDD (singular values and min(M,N) left singular   
                        vectors and N right singular vectors if M>=N,   
                        singular values and M left singular vectors   
                        and min(M,N) right singular vectors otherwise.)   
                (labeled CGESDD(B) in the output)   
            LINPACK:   
            13: CSVDC (singular values only) (comparable to 7 above)   
                (labeled LINSVD in the output)   
            14: CSVDC (singular values and min(M,N) left singular   
                       vectors) (comparable to 8 above)   
                (labeled LINSVD(l) in the output)   
            15: CSVDC (singular values and M left singular vectors)   
                       (comparable to 9 above)   
                (labeled LINSVD(L) in the output)   
            16: CSVDC (singular values and N right singular vectors)   
                       (comparable to 10 above)   
                (labeled LINSVD(R) in the output)   
            17: CSVDC (singular values and min(M,N) left singular   
                       vectors and N right singular vectors)   
                       (comparable to 11 above)   
                (labeled LINSVD(B) in the output)   

            If a character is 'T' or 't', the corresponding routine in   
            this path is timed.  If the entire line is blank, all the   
            routines in the path are timed.   

    NSIZES  (input) INTEGER   
            The number of values of N contained in the vector NN.   

    NN      (input) INTEGER array, dimension( NSIZES )   
            The numbers of columns of the matrices to be tested.  For   
            each N value in the array NN, and each .TRUE. value in   
            DOTYPE, a matrix A will be generated and used to test the   
            routines.   

    MM      (input) INTEGER array, dimension( NSIZES )   
            The numbers of rows of the matrices to be tested.  For   
            each M value in the array MM, and each .TRUE. value in   
            DOTYPE, a matrix A will be generated and used to test the   
            routines.   

    NTYPES  (input) INTEGER   
            The number of types in DOTYPE.  Only the first MAXTYP   
            elements will be examined.  Exception: if NSIZES=1 and   
            NTYPES=MAXTYP+1, and DOTYPE=MAXTYP*f,t, then the input   
            value of A will be used.   

    DOTYPE  (input) LOGICAL   
            If DOTYPE(j) is .TRUE., then a matrix of type j will be   
            generated as follows:   
             j=1: A = U*D*V where U and V are random unitary   
                  matrices and D has evenly spaced entries 1,...,ULP   
                  with random signs on the diagonal   
             j=2: A = U*D*V where U and V are random unitary   
                  matrices and D has geometrically spaced entries   
                  1,...,ULP with random signs on the diagonal   
             j=3: A = U*D*V where U and V are random unitary   
                  matrices and D has "clustered" entries   
                  1,ULP,...,ULP with random signs on the diagonal   
             j=4: A contains uniform complex random numbers with   
                  components from [-1,1]   
             j=5: A is a special nearly bidiagonal matrix, where the   
                  upper bidiagonal entries are exp(-2*r*log(ULP))   
                  where r is a uniform random number from [0,1],   
                  and the nonbidiagonal entries are r*ULP, where   
                  r is a uniform complex random number with components   
                  from [0,1]   

    NPARMS  (input) INTEGER   
            The number of values in each of the arrays NNB and LDAS.   
            For each matrix A generated according to NN, MM and DOTYPE,   
            tests will be run with (NB,,LDA)= (NNB(1), LDAS(1)),...,   
            (NNB(NPARMS), LDAS(NPARMS)).   

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
            each call to CTIM26.   

    A       (workspace) COMPLEX array,   
                        dimension( max(NN)*max(LDAS) )   
            During the testing of CGEBRD, the original dense matrix.   

    H       (workspace) COMPLEX array,   
                        dimension( max(NN)*max(LDAS) )   
            The packed unitary matrices reducing A to bidiagonal   
            form.   

    U       (workspace) COMPLEX array,   
                        dimension( max(NN,MM)*max(LDAS) )   
            The left singular vectors of the original matrix.   

    VT      (workspace) COMPLEX array,   
                        dimension( max(NN,MM)*max(LDAS) )   
            The right singular vectors of the original matrix.   

    D       (workspace) REAL array, dimension( max(NN,MM) )   
            Diagonal entries of bidiagonal matrix to which A   
            is reduced.   

    DC      (workspace) COMPLEX array, dimension( max(NN,MM) )   
            Diagonal entries of bidiagonal matrix to which A   
            is reduced. May be equivalence with D in calling routine.   

    E       (workspace) REAL array, dimension( max(NN,MM) )   
            Offdiagonal entries of bidiagonal matrix to which A   
            is reduced.   

    EC      (workspace) COMPLEX array, dimension( max(NN,MM) )   
            Offdiagonal entries of bidiagonal matrix to which A   
            is reduced. May be equivalence with E in calling routine.   

    TAUP    (workspace) COMPLEX array, dimension( max(NN,MM) )   
            More information used with H.   

    TAUQ    (workspace) COMPLEX array, dimension( max(NN,MM) )   
            More information used with H.   

    WORK    (workspace) COMPLEX array, dimension( LWORK )   

    LWORK   (input) INTEGER   
            Number of elements in WORK and RWORK. Must be at least   
            MAX(6*MIN(M,N),3*MAX(M,N),NSIZES*NPARMS*NTYPES)   

    RWORK   (workspace) REAL array, dimension( LWORK )   
            May be equivalenced to WORK in calling routine.   

    IWORK   (workspace) INTEGER array, dimension at least 8*min(M,N).   

    LLWORK  (workspace) LOGICAL array, dimension( NPARMS ),   

    TIMES   (output) REAL array,   
                     dimension (LDT1,LDT2,LDT3,NSUBS)   
            TIMES(i,j,k,l) will be set to the run time (in seconds) for   
            subroutine/path l, with N=NN(k), M=MM(k), matrix type j,   
            LDA=LDAS(i), and NBLOCK=NNB(i).   

    LDT1    (input) INTEGER   
            The first dimension of TIMES.  LDT1 >= min( 1, NPARMS ).   

    LDT2    (input) INTEGER   
            The second dimension of TIMES.  LDT2 >= min( 1, NTYPES ).   

    LDT3    (input) INTEGER   
            The third dimension of TIMES.  LDT3 >= min( 1, NSIZES ).   

    OPCNTS  (output) REAL array,   
                     dimension (LDO1,LDO2,LDO3,NSUBS)   
            OPCNTS(i,j,k,l) will be set to the number of floating-point   
            operations executed by subroutine/path l, with N=NN(k),   
            M=MM(k), matrix type j, LDA=LDAS(i), and NBLOCK=NNB(i).   

    LDO1    (input) INTEGER   
            The first dimension of OPCNTS.  LDO1 >= min( 1, NPARMS ).   

    LDO2    (input) INTEGER   
            The second dimension of OPCNTS.  LDO2 >= min( 1, NTYPES ).   

    LDO3    (input) INTEGER   
            The third dimension of OPCNTS.   LDO3 >= min( 1, NSIZES ).   

    INFO    (output) INTEGER   
            Error flag.  It will be set to zero if no error occurred.   

    =====================================================================   

       Parameter adjustments */
    --nn;
    --mm;
    --dotype;
    --nnb;
    --ldas;
    --iseed;
    --a;
    --h__;
    --u;
    --vt;
    --d__;
    --dc;
    --e;
    --ec;
    --taup;
    --tauq;
    --work;
    --rwork;
    --iwork;
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


       Extract the timing request from the input line. */

    atimin_("CBD", line, &c__17, subnam, timsub, nout, info, (ftnlen)3, (
	    ftnlen)80, (ftnlen)9);
    if (*info != 0) {
	return 0;
    }

/*     Check LWORK and   
       Check that N <= LDA and M <= LDA for the input values. */

    i__1 = *nsizes;
    for (j2 = 1; j2 <= i__1; ++j2) {
/* Computing MAX   
   Computing MIN */
	i__4 = mm[j2], i__5 = nn[j2];
/* Computing MAX */
	i__6 = mm[j2], i__7 = nn[j2];
	i__2 = min(i__4,i__5) * 6, i__3 = max(i__6,i__7) * 3, i__2 = max(i__2,
		i__3), i__3 = *nsizes * *nparms * *ntypes;
	if (*lwork < max(i__2,i__3)) {
	    *info = -24;
	    io___7.ciunit = *nout;
	    s_wsfe(&io___7);
	    do_fio(&c__1, line, (ftnlen)6);
	    e_wsfe();
	    return 0;
	}
	i__2 = *nparms;
	for (j1 = 1; j1 <= i__2; ++j1) {
/* Computing MAX */
	    i__3 = nn[j2], i__4 = mm[j2];
	    if (max(i__3,i__4) > ldas[j1]) {
		*info = -9;
		io___9.ciunit = *nout;
		s_wsfe(&io___9);
		do_fio(&c__1, line, (ftnlen)6);
		e_wsfe();
		return 0;
	    }
/* L10: */
	}
/* L20: */
    }

/*     Check to see whether CGEBRD must be run.   

       RUNBRD -- if CGEBRD must be run without timing.   
       TRNBRD -- if CGEBRD must be run with timing. */

    runbrd = FALSE_;
    trnbrd = FALSE_;
    if (timsub[1] || timsub[2] || timsub[3] || timsub[4] || timsub[5]) {
	runbrd = TRUE_;
    }
    if (timsub[0]) {
	runbrd = FALSE_;
    }
    if (timsub[6] || timsub[7] || timsub[8] || timsub[9] || timsub[10]) {
	trnbrd = TRUE_;
    }

/*     Various Constants */

    ulp = slamch_("Epsilon") * slamch_("Base");
    ulpinv = 1.f / ulp;
    xlaenv_(&c__9, &c__25);

/*     Zero out OPCNTS, TIMES */

    for (j4 = 1; j4 <= 17; ++j4) {
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
	m = mm[in];
	minmn = min(m,n);
	if (m >= n) {
	    *(unsigned char *)uplo = 'U';
	    ku = minmn;
/* Computing MAX */
	    i__2 = minmn - 1;
	    kvt = max(i__2,0);
	} else {
	    *(unsigned char *)uplo = 'L';
/* Computing MAX */
	    i__2 = minmn - 1;
	    ku = max(i__2,0);
	    kvt = minmn;
	}

/*        Do for each .TRUE. value in DOTYPE: */

	mtypes = min(5,*ntypes);
	if (*ntypes == 6 && *nsizes == 1) {
	    mtypes = *ntypes;
	}
	i__2 = mtypes;
	for (itype = 1; itype <= i__2; ++itype) {
	    if (! dotype[itype]) {
		goto L700;
	    }

/*           Save random number seed for error messages */

	    for (j = 1; j <= 4; ++j) {
		ioldsd[j - 1] = iseed[j];
/* L70: */
	    }

/* -----------------------------------------------------------------------   

             Time the LAPACK Routines   

             Generate A */

	    if (itype <= 5) {
		if (itype >= 1 && itype <= 3) {
		    imode = kmode[itype - 1];
		    clatms_(&m, &n, "U", &iseed[1], "N", &d__[1], &imode, &
			    ulpinv, &c_b27, &m, &n, "N", &a[1], &m, &work[1], 
			    info);
		} else if (itype >= 4 && itype <= 5) {
		    if (itype == 4) {
			conds = -1.f;
		    }
		    if (itype == 5) {
			conds = ulp;
		    }
		    clatmr_(&m, &n, "S", &iseed[1], "N", &dc[1], &c__6, &
			    c_b32, &c_b1, "T", "N", &dc[1], &c__0, &c_b27, &
			    dc[1], &c__0, &c_b27, "N", jdum, &m, &n, &c_b32, &
			    conds, "N", &a[1], &m, jdum, info);
		    if (itype == 5) {
			conds = log(ulp) * -2.f;
			i__3 = (minmn - 1) * m + minmn;
			i__4 = m + 1;
			for (j = 1; i__4 < 0 ? j >= i__3 : j <= i__3; j += 
				i__4) {
			    i__5 = j;
			    clarnd_(&q__2, &c__5, &iseed[1]);
			    r__1 = exp(conds * slarnd_(&c__1, &iseed[1]));
			    q__1.r = r__1 * q__2.r, q__1.i = r__1 * q__2.i;
			    a[i__5].r = q__1.r, a[i__5].i = q__1.i;
/* L80: */
			}
			if (m >= n) {
			    i__4 = (minmn - 1) * m + minmn - 1;
			    i__3 = m + 1;
			    for (j = m + 1; i__3 < 0 ? j >= i__4 : j <= i__4; 
				    j += i__3) {
				i__5 = j;
				clarnd_(&q__2, &c__5, &iseed[1]);
				r__1 = exp(conds * slarnd_(&c__1, &iseed[1]));
				q__1.r = r__1 * q__2.r, q__1.i = r__1 * 
					q__2.i;
				a[i__5].r = q__1.r, a[i__5].i = q__1.i;
/* L90: */
			    }
			} else {
			    i__3 = (minmn - 2) * m + minmn;
			    i__4 = m + 1;
			    for (j = 2; i__4 < 0 ? j >= i__3 : j <= i__3; j +=
				     i__4) {
				i__5 = j;
				clarnd_(&q__2, &c__5, &iseed[1]);
				r__1 = exp(conds * slarnd_(&c__1, &iseed[1]));
				q__1.r = r__1 * q__2.r, q__1.i = r__1 * 
					q__2.i;
				a[i__5].r = q__1.r, a[i__5].i = q__1.i;
/* L100: */
			    }
			}
		    }
		}
	    }

/*           Time CGEBRD for each pair NNB(j), LDAS(j) */

	    if (timsub[0] || trnbrd) {
		i__4 = *nparms;
		for (ipar = 1; ipar <= i__4; ++ipar) {
		    lda = ldas[ipar];
/* Computing MIN */
		    i__3 = n, i__5 = nnb[ipar];
		    nb = min(i__3,i__5);
		    xlaenv_(&c__1, &nb);
		    xlaenv_(&c__2, &c__2);
		    xlaenv_(&c__3, &nb);

/*                 Time CGEBRD */

		    ic = 0;
		    latime_1.ops = 0.f;
		    s1 = second_();
L110:
		    clacpy_("Full", &m, &n, &a[1], &m, &h__[1], &lda);
		    cgebrd_(&m, &n, &h__[1], &lda, &d__[1], &e[1], &tauq[1], &
			    taup[1], &work[1], lwork, &iinfo);
		    if (iinfo != 0) {
			io___36.ciunit = *nout;
			s_wsfe(&io___36);
			do_fio(&c__1, subnam_ref(0, 1), (ftnlen)9);
			do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(integer));
			do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(
				integer));
			e_wsfe();
			*info = abs(iinfo);
			goto L700;
		    }

		    s2 = second_();
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			goto L110;
		    }

/*                 Subtract the time used in CLACPY. */

		    s1 = second_();
		    i__3 = ic;
		    for (j = 1; j <= i__3; ++j) {
			clacpy_("Full", &m, &n, &a[1], &m, &u[1], &lda);
/* L120: */
		    }
		    s2 = second_();
		    untime = s2 - s1;

/* Computing MAX */
		    r__1 = time - untime;
		    times_ref(ipar, itype, in, 1) = dmax(r__1,0.f) / (real) 
			    ic;
		    opcnts_ref(ipar, itype, in, 1) = sopla_("CGEBRD", &m, &n, 
			    &c__0, &c__0, &nb);
/* L130: */
		}
	    } else {
		if (runbrd) {
		    clacpy_("Full", &m, &n, &a[1], &m, &h__[1], &m)
			    ;
		    cgebrd_(&m, &n, &h__[1], &m, &d__[1], &e[1], &tauq[1], &
			    taup[1], &work[1], lwork, &iinfo);
		    if (iinfo != 0) {
			io___40.ciunit = *nout;
			s_wsfe(&io___40);
			do_fio(&c__1, subnam_ref(0, 1), (ftnlen)9);
			do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
			do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(
				integer));
			e_wsfe();
			*info = abs(iinfo);
			goto L700;
		    }
		}
	    }

/*           Time CBDSQR (singular values only) for each pair   
             NNB(j), LDAS(j) */

	    if (timsub[1] || timsub[6]) {
		i__4 = *nparms;
		for (ipar = 1; ipar <= i__4; ++ipar) {
		    lda = ldas[ipar];
/* Computing MIN */
		    i__3 = n, i__5 = nnb[ipar];
		    nb = min(i__3,i__5);

/*                 If this value of LDA has been used before, just   
                   use that value */

		    lastnl = 0;
		    i__3 = ipar - 1;
		    for (j = 1; j <= i__3; ++j) {
			if (lda == ldas[j]) {
			    lastnl = j;
			}
/* L140: */
		    }

		    if (lastnl == 0) {

/*                    Time CBDSQR (singular values only) */

			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L150:
			scopy_(&minmn, &d__[1], &c__1, &rwork[1], &c__1);
			i__3 = minmn - 1;
			scopy_(&i__3, &e[1], &c__1, &rwork[minmn + 1], &c__1);
			cbdsqr_(uplo, &minmn, &c__0, &c__0, &c__0, &rwork[1], 
				&rwork[minmn + 1], &vt[1], &lda, &u[1], &lda, 
				&u[1], &lda, &rwork[(minmn << 1) + 1], &iinfo);
			if (iinfo != 0) {
			    io___42.ciunit = *nout;
			    s_wsfe(&io___42);
			    do_fio(&c__1, subnam_ref(0, 2), (ftnlen)9);
			    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer))
				    ;
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
			    goto L700;
			}
			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L150;
			}

/*                    Subtract the time used in CLACPY. */

			s1 = second_();
			i__3 = ic;
			for (j = 1; j <= i__3; ++j) {
			    scopy_(&minmn, &d__[1], &c__1, &rwork[1], &c__1);
			    i__5 = minmn - 1;
			    scopy_(&i__5, &e[1], &c__1, &rwork[minmn + 1], &
				    c__1);
/* L160: */
			}
			s2 = second_();
			untime = s2 - s1;

/* Computing MAX */
			r__1 = time - untime;
			times_ref(ipar, itype, in, 2) = dmax(r__1,0.f) / (
				real) ic;
			opcnts_ref(ipar, itype, in, 2) = latime_1.ops / (real)
				 ic;

		    } else {

			times_ref(ipar, itype, in, 2) = times_ref(lastnl, 
				itype, in, 2);
			opcnts_ref(ipar, itype, in, 2) = opcnts_ref(lastnl, 
				itype, in, 2);
		    }
/* L170: */
		}
	    }

/*           Time CBDSQR (singular values and left singular vectors,   
             assume original matrix square) for each pair NNB(j), LDAS(j) */

	    if (timsub[2] || timsub[7] || timsub[8]) {
		i__4 = *nparms;
		for (ipar = 1; ipar <= i__4; ++ipar) {
		    lda = ldas[ipar];
/* Computing MIN */
		    i__3 = n, i__5 = nnb[ipar];
		    nb = min(i__3,i__5);

/*                 If this value of LDA has been used before, just   
                   use that value */

		    lastnl = 0;
		    i__3 = ipar - 1;
		    for (j = 1; j <= i__3; ++j) {
			if (lda == ldas[j]) {
			    lastnl = j;
			}
/* L180: */
		    }

		    if (lastnl == 0) {

/*                    Time CBDSQR (singular values and left singular   
                      vectors, assume original matrix square) */

			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L190:
			claset_("Full", &m, &minmn, &c_b1, &c_b2, &u[1], &lda);
			scopy_(&minmn, &d__[1], &c__1, &rwork[1], &c__1);
			i__3 = minmn - 1;
			scopy_(&i__3, &e[1], &c__1, &rwork[minmn + 1], &c__1);
			cbdsqr_(uplo, &minmn, &c__0, &m, &c__0, &rwork[1], &
				rwork[minmn + 1], &vt[1], &lda, &u[1], &lda, &
				u[1], &lda, &rwork[(minmn << 1) + 1], &iinfo);
			if (iinfo != 0) {
			    io___43.ciunit = *nout;
			    s_wsfe(&io___43);
			    do_fio(&c__1, subnam_ref(0, 3), (ftnlen)9);
			    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer))
				    ;
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
			    goto L700;
			}
			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L190;
			}

/*                    Subtract the time used in CLACPY. */

			s1 = second_();
			i__3 = ic;
			for (j = 1; j <= i__3; ++j) {
			    claset_("Full", &m, &minmn, &c_b1, &c_b2, &u[1], &
				    lda);
			    scopy_(&minmn, &d__[1], &c__1, &rwork[1], &c__1);
			    i__5 = minmn - 1;
			    scopy_(&i__5, &e[1], &c__1, &rwork[minmn + 1], &
				    c__1);
/* L200: */
			}
			s2 = second_();
			untime = s2 - s1;

/* Computing MAX */
			r__1 = time - untime;
			times_ref(ipar, itype, in, 3) = dmax(r__1,0.f) / (
				real) ic;
			opcnts_ref(ipar, itype, in, 3) = latime_1.ops / (real)
				 ic;

		    } else {

			times_ref(ipar, itype, in, 3) = times_ref(lastnl, 
				itype, in, 3);
			opcnts_ref(ipar, itype, in, 3) = opcnts_ref(lastnl, 
				itype, in, 3);
		    }
/* L210: */
		}
	    }

/*           Time CBDSQR (singular values and right singular vectors,   
             assume original matrix square) for each pair NNB(j), LDAS(j) */

	    if (timsub[3] || timsub[9]) {
		i__4 = *nparms;
		for (ipar = 1; ipar <= i__4; ++ipar) {
		    lda = ldas[ipar];
/* Computing MIN */
		    i__3 = n, i__5 = nnb[ipar];
		    nb = min(i__3,i__5);

/*                 If this value of LDA has been used before, just   
                   use that value */

		    lastnl = 0;
		    i__3 = ipar - 1;
		    for (j = 1; j <= i__3; ++j) {
			if (lda == ldas[j]) {
			    lastnl = j;
			}
/* L220: */
		    }

		    if (lastnl == 0) {

/*                    Time CBDSQR (singular values and right singular   
                      vectors, assume original matrix square) */

			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L230:
			claset_("Full", &minmn, &n, &c_b1, &c_b2, &vt[1], &
				lda);
			scopy_(&minmn, &d__[1], &c__1, &rwork[1], &c__1);
			i__3 = minmn - 1;
			scopy_(&i__3, &e[1], &c__1, &rwork[minmn + 1], &c__1);
			cbdsqr_(uplo, &minmn, &n, &c__0, &c__0, &rwork[1], &
				rwork[minmn + 1], &vt[1], &lda, &u[1], &lda, &
				u[1], &lda, &rwork[(minmn << 1) + 1], &iinfo);
			if (iinfo != 0) {
			    io___44.ciunit = *nout;
			    s_wsfe(&io___44);
			    do_fio(&c__1, subnam_ref(0, 4), (ftnlen)9);
			    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer))
				    ;
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
			    goto L700;
			}
			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L230;
			}

/*                    Subtract the time used in CLACPY. */

			s1 = second_();
			i__3 = ic;
			for (j = 1; j <= i__3; ++j) {
			    claset_("Full", &minmn, &n, &c_b1, &c_b2, &vt[1], 
				    &lda);
			    scopy_(&minmn, &d__[1], &c__1, &rwork[1], &c__1);
			    i__5 = minmn - 1;
			    scopy_(&i__5, &e[1], &c__1, &rwork[minmn + 1], &
				    c__1);
/* L240: */
			}
			s2 = second_();
			untime = s2 - s1;

/* Computing MAX */
			r__1 = time - untime;
			times_ref(ipar, itype, in, 4) = dmax(r__1,0.f) / (
				real) ic;
			opcnts_ref(ipar, itype, in, 4) = latime_1.ops / (real)
				 ic;

		    } else {

			times_ref(ipar, itype, in, 4) = times_ref(lastnl, 
				itype, in, 4);
			opcnts_ref(ipar, itype, in, 4) = opcnts_ref(lastnl, 
				itype, in, 4);
		    }
/* L250: */
		}
	    }

/*           Time CBDSQR (singular values and left and right singular   
             vectors,assume original matrix square) for each pair   
             NNB(j), LDAS(j) */

	    if (timsub[4] || timsub[10]) {
		i__4 = *nparms;
		for (ipar = 1; ipar <= i__4; ++ipar) {
		    lda = ldas[ipar];
/* Computing MIN */
		    i__3 = n, i__5 = nnb[ipar];
		    nb = min(i__3,i__5);

/*                 If this value of LDA has been used before, just   
                   use that value */

		    lastnl = 0;
		    i__3 = ipar - 1;
		    for (j = 1; j <= i__3; ++j) {
			if (lda == ldas[j]) {
			    lastnl = j;
			}
/* L260: */
		    }

		    if (lastnl == 0) {

/*                    Time CBDSQR (singular values and left and right   
                      singular vectors, assume original matrix square) */

			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L270:
			claset_("Full", &minmn, &n, &c_b1, &c_b2, &vt[1], &
				lda);
			claset_("Full", &m, &minmn, &c_b1, &c_b2, &u[1], &lda);
			scopy_(&minmn, &d__[1], &c__1, &rwork[1], &c__1);
			i__3 = minmn - 1;
			scopy_(&i__3, &e[1], &c__1, &rwork[minmn + 1], &c__1);
			cbdsqr_(uplo, &minmn, &n, &m, &c__0, &rwork[1], &
				rwork[minmn + 1], &vt[1], &lda, &u[1], &lda, &
				u[1], &lda, &rwork[(minmn << 1) + 1], &iinfo);
			if (iinfo != 0) {
			    io___45.ciunit = *nout;
			    s_wsfe(&io___45);
			    do_fio(&c__1, subnam_ref(0, 5), (ftnlen)9);
			    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer))
				    ;
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
			    goto L700;
			}
			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L270;
			}

/*                    Subtract the time used in CLACPY. */

			s1 = second_();
			i__3 = ic;
			for (j = 1; j <= i__3; ++j) {
			    claset_("Full", &minmn, &n, &c_b1, &c_b2, &vt[1], 
				    &lda);
			    claset_("Full", &m, &minmn, &c_b1, &c_b2, &u[1], &
				    lda);
			    scopy_(&minmn, &d__[1], &c__1, &rwork[1], &c__1);
			    i__5 = minmn - 1;
			    scopy_(&i__5, &e[1], &c__1, &rwork[minmn + 1], &
				    c__1);
/* L280: */
			}
			s2 = second_();
			untime = s2 - s1;

/* Computing MAX */
			r__1 = time - untime;
			times_ref(ipar, itype, in, 5) = dmax(r__1,0.f) / (
				real) ic;
			opcnts_ref(ipar, itype, in, 5) = latime_1.ops / (real)
				 ic;

		    } else {

			times_ref(ipar, itype, in, 5) = times_ref(lastnl, 
				itype, in, 5);
			opcnts_ref(ipar, itype, in, 5) = opcnts_ref(lastnl, 
				itype, in, 5);
		    }
/* L290: */
		}
	    }

/*           Time CBDSQR (singular values and multiply square matrix   
             by transpose of left singular vectors) for each pair   
             NNB(j), LDAS(j) */

	    if (timsub[5]) {
		i__4 = *nparms;
		for (ipar = 1; ipar <= i__4; ++ipar) {
		    lda = ldas[ipar];
/* Computing MIN */
		    i__3 = n, i__5 = nnb[ipar];
		    nb = min(i__3,i__5);

/*                 If this value of LDA has been used before, just   
                   use that value */

		    lastnl = 0;
		    i__3 = ipar - 1;
		    for (j = 1; j <= i__3; ++j) {
			if (lda == ldas[j]) {
			    lastnl = j;
			}
/* L300: */
		    }

		    if (lastnl == 0) {

/*                    Time CBDSQR (singular values and multiply square   
                      matrix by transpose of left singular vectors) */

			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L310:
			claset_("Full", &minmn, &minmn, &c_b1, &c_b2, &u[1], &
				lda);
			scopy_(&minmn, &d__[1], &c__1, &rwork[1], &c__1);
			i__3 = minmn - 1;
			scopy_(&i__3, &e[1], &c__1, &rwork[minmn + 1], &c__1);
			cbdsqr_(uplo, &minmn, &c__0, &c__0, &minmn, &rwork[1],
				 &rwork[minmn + 1], &vt[1], &lda, &u[1], &lda,
				 &u[1], &lda, &rwork[(minmn << 1) + 1], &
				iinfo);
			if (iinfo != 0) {
			    io___46.ciunit = *nout;
			    s_wsfe(&io___46);
			    do_fio(&c__1, subnam_ref(0, 6), (ftnlen)9);
			    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer))
				    ;
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
			    goto L700;
			}
			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L310;
			}

/*                    Subtract the time used in CLACPY. */

			s1 = second_();
			i__3 = ic;
			for (j = 1; j <= i__3; ++j) {
			    claset_("Full", &minmn, &minmn, &c_b1, &c_b2, &u[
				    1], &lda);
			    scopy_(&minmn, &d__[1], &c__1, &rwork[1], &c__1);
			    i__5 = minmn - 1;
			    scopy_(&i__5, &e[1], &c__1, &rwork[minmn + 1], &
				    c__1);
/* L320: */
			}
			s2 = second_();
			untime = s2 - s1;

/* Computing MAX */
			r__1 = time - untime;
			times_ref(ipar, itype, in, 6) = dmax(r__1,0.f) / (
				real) ic;
			opcnts_ref(ipar, itype, in, 6) = latime_1.ops / (real)
				 ic;

		    } else {

			times_ref(ipar, itype, in, 6) = times_ref(lastnl, 
				itype, in, 6);
			opcnts_ref(ipar, itype, in, 6) = opcnts_ref(lastnl, 
				itype, in, 6);
		    }
/* L330: */
		}
	    }

/*           Time CGEBRD+CBDSQR (singular values only) for each pair   
             NNB(j), LDAS(j) */

	    if (timsub[6]) {
		i__4 = *nparms;
		for (ipar = 1; ipar <= i__4; ++ipar) {
		    times_ref(ipar, itype, in, 7) = times_ref(ipar, itype, in,
			     1) + times_ref(ipar, itype, in, 2);
		    opcnts_ref(ipar, itype, in, 7) = opcnts_ref(ipar, itype, 
			    in, 1) + opcnts_ref(ipar, itype, in, 2);
/* L340: */
		}
	    }

/*           Time CGEBRD+CUNGBR+CBDSQR (singular values and min(M,N)   
             left singular vectors) for each pair NNB(j), LDAS(j) */

	    if (timsub[7]) {
		i__4 = *nparms;
		for (ipar = 1; ipar <= i__4; ++ipar) {
		    lda = ldas[ipar];
/* Computing MIN */
		    i__3 = n, i__5 = nnb[ipar];
		    nb = min(i__3,i__5);
		    xlaenv_(&c__1, &nb);
		    xlaenv_(&c__2, &c__2);
		    xlaenv_(&c__3, &nb);

/*                 Time CGEBRD+CUNGBR+CBDSQR (singular values and   
                   min(M,N) left singular vectors) */

		    ic = 0;
		    latime_1.ops = 0.f;
		    s1 = second_();
L350:
		    clacpy_("L", &m, &minmn, &h__[1], &lda, &u[1], &lda);
		    cungbr_("Q", &m, &minmn, &ku, &u[1], &lda, &tauq[1], &
			    work[1], lwork, &iinfo);
		    if (iinfo != 0) {
			io___47.ciunit = *nout;
			s_wsfe(&io___47);
			do_fio(&c__1, subnam_ref(0, 8), (ftnlen)9);
			do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(integer));
			do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(
				integer));
			e_wsfe();
			*info = abs(iinfo);
			goto L700;
		    }
		    s2 = second_();
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			goto L350;
		    }

/*                 Subtract the time used in CLACPY. */

		    s1 = second_();
		    i__3 = ic;
		    for (j = 1; j <= i__3; ++j) {
			clacpy_("L", &m, &minmn, &h__[1], &lda, &u[1], &lda);
/* L360: */
		    }
		    s2 = second_();
		    untime = s2 - s1;

/* Computing MAX */
		    r__1 = time - untime;
		    times_ref(ipar, itype, in, 8) = dmax(r__1,0.f) / (real) 
			    ic + times_ref(ipar, itype, in, 1) + times_ref(
			    ipar, itype, in, 3);
		    opcnts_ref(ipar, itype, in, 8) = sopla2_("CUNGBR", "Q", &
			    m, &minmn, &ku, &c__0, &nb) 
			    + opcnts_ref(ipar, itype, in, 1) + opcnts_ref(
			    ipar, itype, in, 3);
/* L370: */
		}
	    }

/*           Time CGEBRD+CUNGBR+CBDSQR (singular values and M   
             left singular vectors) for each pair NNB(j), LDAS(j) */

	    if (timsub[8]) {
		i__4 = *nparms;
		for (ipar = 1; ipar <= i__4; ++ipar) {
		    lda = ldas[ipar];
/* Computing MIN */
		    i__3 = n, i__5 = nnb[ipar];
		    nb = min(i__3,i__5);
		    xlaenv_(&c__1, &nb);
		    xlaenv_(&c__2, &c__2);
		    xlaenv_(&c__3, &nb);

/*                 Time CGEBRD+CUNGBR+CBDSQR (singular values and   
                   M left singular vectors) */

		    ic = 0;
		    latime_1.ops = 0.f;
		    s1 = second_();
L380:
		    clacpy_("L", &m, &minmn, &h__[1], &lda, &u[1], &lda);
		    cungbr_("Q", &m, &m, &ku, &u[1], &lda, &tauq[1], &work[1],
			     lwork, &iinfo);
		    if (iinfo != 0) {
			io___48.ciunit = *nout;
			s_wsfe(&io___48);
			do_fio(&c__1, subnam_ref(0, 9), (ftnlen)9);
			do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(integer));
			do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(
				integer));
			e_wsfe();
			*info = abs(iinfo);
			goto L700;
		    }
		    s2 = second_();
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			goto L380;
		    }

/*                 Subtract the time used in CLACPY. */

		    s1 = second_();
		    i__3 = ic;
		    for (j = 1; j <= i__3; ++j) {
			clacpy_("L", &m, &minmn, &h__[1], &lda, &u[1], &lda);
/* L390: */
		    }
		    s2 = second_();
		    untime = s2 - s1;

/* Computing MAX */
		    r__1 = time - untime;
		    times_ref(ipar, itype, in, 9) = dmax(r__1,0.f) / (real) 
			    ic + times_ref(ipar, itype, in, 1) + times_ref(
			    ipar, itype, in, 3);
		    opcnts_ref(ipar, itype, in, 9) = sopla2_("CUNGBR", "Q", &
			    m, &m, &ku, &c__0, &nb) + 
			    opcnts_ref(ipar, itype, in, 1) + opcnts_ref(ipar, 
			    itype, in, 3);
/* L400: */
		}
	    }

/*           Time CGEBRD+CUNGBR+CBDSQR (singular values and N   
             right singular vectors) for each pair NNB(j), LDAS(j) */

	    if (timsub[9]) {
		i__4 = *nparms;
		for (ipar = 1; ipar <= i__4; ++ipar) {
		    lda = ldas[ipar];
/* Computing MIN */
		    i__3 = n, i__5 = nnb[ipar];
		    nb = min(i__3,i__5);
		    xlaenv_(&c__1, &nb);
		    xlaenv_(&c__2, &c__2);
		    xlaenv_(&c__3, &nb);

/*                 Time CGEBRD+CUNGBR+CBDSQR (singular values and   
                   N right singular vectors) */

		    ic = 0;
		    latime_1.ops = 0.f;
		    s1 = second_();
L410:
		    clacpy_("U", &minmn, &n, &h__[1], &lda, &vt[1], &lda);
		    cungbr_("P", &n, &n, &kvt, &vt[1], &lda, &taup[1], &work[
			    1], lwork, &iinfo);
		    if (iinfo != 0) {
			io___49.ciunit = *nout;
			s_wsfe(&io___49);
			do_fio(&c__1, subnam_ref(0, 10), (ftnlen)9);
			do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(integer));
			do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(
				integer));
			e_wsfe();
			*info = abs(iinfo);
			goto L700;
		    }
		    s2 = second_();
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			goto L410;
		    }

/*                 Subtract the time used in CLACPY. */

		    s1 = second_();
		    i__3 = ic;
		    for (j = 1; j <= i__3; ++j) {
			clacpy_("U", &minmn, &n, &h__[1], &lda, &vt[1], &lda);
/* L420: */
		    }
		    s2 = second_();
		    untime = s2 - s1;

/* Computing MAX */
		    r__1 = time - untime;
		    times_ref(ipar, itype, in, 10) = dmax(r__1,0.f) / (real) 
			    ic + times_ref(ipar, itype, in, 1) + times_ref(
			    ipar, itype, in, 4);
		    opcnts_ref(ipar, itype, in, 10) = sopla2_("CUNGBR", "P", &
			    n, &n, &kvt, &c__0, &nb) + 
			    opcnts_ref(ipar, itype, in, 1) + opcnts_ref(ipar, 
			    itype, in, 4);
/* L430: */
		}
	    }

/*           Time CGEBRD+CUNGBR+CBDSQR (singular values and min(M,N) left   
             singular vectors and N right singular vectors) for each pair   
             NNB(j), LDAS(j) */

	    if (timsub[10]) {
		i__4 = *nparms;
		for (ipar = 1; ipar <= i__4; ++ipar) {
		    lda = ldas[ipar];
/* Computing MIN */
		    i__3 = n, i__5 = nnb[ipar];
		    nb = min(i__3,i__5);
		    xlaenv_(&c__1, &nb);
		    xlaenv_(&c__2, &c__2);
		    xlaenv_(&c__3, &nb);

/*                 Time CGEBRD+CUNGBR+CBDSQR (singular values and   
                   min(M,N) left singular vectors and N right singular   
                   vectors) */

		    ic = 0;
		    latime_1.ops = 0.f;
		    s1 = second_();
L440:
		    clacpy_("L", &m, &minmn, &h__[1], &lda, &u[1], &lda);
		    cungbr_("Q", &m, &minmn, &ku, &u[1], &lda, &tauq[1], &
			    work[1], lwork, &iinfo);
		    if (iinfo != 0) {
			io___50.ciunit = *nout;
			s_wsfe(&io___50);
			do_fio(&c__1, subnam_ref(0, 11), (ftnlen)9);
			do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(integer));
			do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(
				integer));
			e_wsfe();
			*info = abs(iinfo);
			goto L700;
		    }
		    clacpy_("U", &minmn, &n, &h__[1], &lda, &vt[1], &lda);
		    cungbr_("P", &n, &n, &kvt, &vt[1], &lda, &taup[1], &work[
			    1], lwork, &iinfo);
		    if (iinfo != 0) {
			io___51.ciunit = *nout;
			s_wsfe(&io___51);
			do_fio(&c__1, subnam_ref(0, 11), (ftnlen)9);
			do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(integer));
			do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(
				integer));
			e_wsfe();
			*info = abs(iinfo);
			goto L700;
		    }
		    s2 = second_();
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			goto L440;
		    }

/*                 Subtract the time used in CLACPY. */

		    s1 = second_();
		    i__3 = ic;
		    for (j = 1; j <= i__3; ++j) {
			clacpy_("L", &minmn, &minmn, &h__[1], &lda, &vt[1], &
				lda);
/* L450: */
		    }
		    s2 = second_();
		    untime = s2 - s1;

/* Computing MAX */
		    r__1 = time - untime;
		    times_ref(ipar, itype, in, 11) = dmax(r__1,0.f) / (real) 
			    ic + times_ref(ipar, itype, in, 1) + times_ref(
			    ipar, itype, in, 5);
		    opcnts_ref(ipar, itype, in, 11) = sopla2_("CUNGBR", "Q", &
			    m, &minmn, &ku, &c__0, &nb) 
			    + sopla2_("CUNGBR", "P", &n, &n, &kvt, &c__0, &nb) + opcnts_ref(ipar, itype, 
			    in, 1) + opcnts_ref(ipar, itype, in, 5);
/* L460: */
		}
	    }

/*           Time CGESDD( singular values and min(M,N) left singular   
             vectors and N right singular vectors when M>=N,   
             singular values and M left singular vectors and min(M,N)   
             right singular vectors otherwise) for each pair   
             NNB(j), LDAS(j) */

	    if (timsub[11]) {
		i__4 = *nparms;
		for (ipar = 1; ipar <= i__4; ++ipar) {
		    lda = ldas[ipar];
/* Computing MIN */
		    i__3 = n, i__5 = nnb[ipar];
		    nb = min(i__3,i__5);
		    xlaenv_(&c__1, &nb);
		    xlaenv_(&c__2, &c__2);
		    xlaenv_(&c__3, &nb);

/*                 Time CGESDD (singular values and   
                   min(M,N) left singular vectors and N right singular   
                   vectors) */

		    ic = 0;
		    latime_1.ops = 0.f;
		    s1 = second_();
L470:
		    clacpy_("Full", &m, &n, &a[1], &m, &h__[1], &lda);
		    cgesdd_("S", &m, &n, &h__[1], &lda, &rwork[1], &u[1], &
			    lda, &vt[1], &lda, &work[1], lwork, &rwork[(minmn 
			    << 1) + 1], &iwork[1], &iinfo);
		    s2 = second_();
		    if (iinfo != 0) {
			io___52.ciunit = *nout;
			s_wsfe(&io___52);
			do_fio(&c__1, subnam_ref(0, 12), (ftnlen)9);
			do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(integer));
			do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(
				integer));
			e_wsfe();
			*info = abs(iinfo);
			goto L700;
		    }
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			goto L470;
		    }

/*                 Subtract the time used in CLACPY. */

		    s1 = second_();
		    i__3 = ic;
		    for (j = 1; j <= i__3; ++j) {
			clacpy_("Full", &m, &n, &a[1], &m, &h__[1], &lda);
/* L480: */
		    }
		    s2 = second_();
		    untime = s2 - s1;

/* Computing MAX */
		    r__1 = time - untime;
		    times_ref(ipar, itype, in, 12) = dmax(r__1,0.f) / (real) 
			    ic;
		    opcnts_ref(ipar, itype, in, 12) = latime_1.ops / (real) 
			    ic;
/* L490: */
		}
	    }

/*           Time CSVDC (singular values only) for each pair   
             NNB(j), LDAS(j) */

	    if (timsub[12]) {
		i__4 = *nparms;
		for (ipar = 1; ipar <= i__4; ++ipar) {
		    lda = ldas[ipar];

/*                 If this value of LDA has been used before, just   
                   use that value */

		    lastnl = 0;
		    i__3 = ipar - 1;
		    for (j = 1; j <= i__3; ++j) {
			if (lda == ldas[j]) {
			    lastnl = j;
			}
/* L500: */
		    }

		    if (lastnl == 0) {

/*                    Time CSVDC (singular values only) */

			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L510:
			clacpy_("Full", &m, &n, &a[1], &m, &h__[1], &lda);
			csvdc_(&h__[1], &lda, &m, &n, &dc[1], &ec[1], &u[1], &
				lda, &vt[1], &lda, &work[1], &c__0, &iinfo);
			if (iinfo != 0) {
			    io___53.ciunit = *nout;
			    s_wsfe(&io___53);
			    do_fio(&c__1, subnam_ref(0, 13), (ftnlen)9);
			    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer))
				    ;
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
			    goto L700;
			}
			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L510;
			}

/*                    Subtract the time used in CLACPY. */

			s1 = second_();
			i__3 = ic;
			for (j = 1; j <= i__3; ++j) {
			    clacpy_("Full", &m, &n, &a[1], &m, &h__[1], &lda);
/* L520: */
			}
			s2 = second_();
			untime = s2 - s1;

/* Computing MAX */
			r__1 = time - untime;
			times_ref(ipar, itype, in, 13) = dmax(r__1,0.f) / (
				real) ic;
			opcnts_ref(ipar, itype, in, 13) = latime_1.ops / (
				real) ic;

		    } else {

			times_ref(ipar, itype, in, 13) = times_ref(lastnl, 
				itype, in, 13);
			opcnts_ref(ipar, itype, in, 13) = opcnts_ref(lastnl, 
				itype, in, 13);
		    }
/* L530: */
		}
	    }

/*           Time CSVDC (singular values and min(M,N) left singular   
             vectors) for each pair NNB(j), LDAS(j) */

	    if (timsub[13]) {
		i__4 = *nparms;
		for (ipar = 1; ipar <= i__4; ++ipar) {
		    lda = ldas[ipar];

/*                 If this value of LDA has been used before, just   
                   use that value */

		    lastnl = 0;
		    i__3 = ipar - 1;
		    for (j = 1; j <= i__3; ++j) {
			if (lda == ldas[j]) {
			    lastnl = j;
			}
/* L540: */
		    }

		    if (lastnl == 0) {

/*                    Time CSVDC (singular values and min(M,N) left   
                      singular vectors) */

			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L550:
			clacpy_("Full", &m, &n, &a[1], &m, &h__[1], &lda);
			csvdc_(&h__[1], &lda, &m, &n, &dc[1], &ec[1], &u[1], &
				lda, &vt[1], &lda, &work[1], &c__20, &iinfo);
			if (iinfo != 0) {
			    io___54.ciunit = *nout;
			    s_wsfe(&io___54);
			    do_fio(&c__1, subnam_ref(0, 14), (ftnlen)9);
			    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer))
				    ;
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
			    goto L700;
			}
			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L550;
			}

/*                    Subtract the time used in CLACPY. */

			s1 = second_();
			i__3 = ic;
			for (j = 1; j <= i__3; ++j) {
			    clacpy_("Full", &m, &n, &a[1], &m, &h__[1], &lda);
/* L560: */
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

			times_ref(ipar, itype, in, 14) = times_ref(lastnl, 
				itype, in, 14);
			opcnts_ref(ipar, itype, in, 14) = opcnts_ref(lastnl, 
				itype, in, 14);
		    }
/* L570: */
		}
	    }

/*           Time CSVDC (singular values and M left singular   
             vectors) for each pair NNB(j), LDAS(j) */

	    if (timsub[14]) {
		i__4 = *nparms;
		for (ipar = 1; ipar <= i__4; ++ipar) {
		    lda = ldas[ipar];

/*                 If this value of LDA has been used before, just   
                   use that value */

		    lastnl = 0;
		    i__3 = ipar - 1;
		    for (j = 1; j <= i__3; ++j) {
			if (lda == ldas[j]) {
			    lastnl = j;
			}
/* L580: */
		    }

		    if (lastnl == 0) {

/*                    Time CSVDC (singular values and M left singular   
                      vectors) */

			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L590:
			clacpy_("Full", &m, &n, &a[1], &m, &h__[1], &lda);
			csvdc_(&h__[1], &lda, &m, &n, &dc[1], &ec[1], &u[1], &
				lda, &vt[1], &lda, &work[1], &c__10, &iinfo);
			if (iinfo != 0) {
			    io___55.ciunit = *nout;
			    s_wsfe(&io___55);
			    do_fio(&c__1, subnam_ref(0, 14), (ftnlen)9);
			    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer))
				    ;
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
			    goto L700;
			}
			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L590;
			}

/*                    Subtract the time used in CLACPY. */

			s1 = second_();
			i__3 = ic;
			for (j = 1; j <= i__3; ++j) {
			    clacpy_("Full", &m, &n, &a[1], &m, &h__[1], &lda);
/* L600: */
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

			times_ref(ipar, itype, in, 15) = times_ref(lastnl, 
				itype, in, 15);
			opcnts_ref(ipar, itype, in, 15) = opcnts_ref(lastnl, 
				itype, in, 15);
		    }
/* L610: */
		}
	    }

/*           Time CSVDC (singular values and N right singular   
             vectors) for each pair NNB(j), LDAS(j) */

	    if (timsub[15]) {
		i__4 = *nparms;
		for (ipar = 1; ipar <= i__4; ++ipar) {
		    lda = ldas[ipar];

/*                 If this value of LDA has been used before, just   
                   use that value */

		    lastnl = 0;
		    i__3 = ipar - 1;
		    for (j = 1; j <= i__3; ++j) {
			if (lda == ldas[j]) {
			    lastnl = j;
			}
/* L620: */
		    }

		    if (lastnl == 0) {

/*                    Time CSVDC (singular values and N right singular   
                      vectors) */

			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L630:
			clacpy_("Full", &m, &n, &a[1], &m, &h__[1], &lda);
			csvdc_(&h__[1], &lda, &m, &n, &dc[1], &ec[1], &u[1], &
				lda, &vt[1], &lda, &work[1], &c__1, &iinfo);
			if (iinfo != 0) {
			    io___56.ciunit = *nout;
			    s_wsfe(&io___56);
			    do_fio(&c__1, subnam_ref(0, 15), (ftnlen)9);
			    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer))
				    ;
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
			    goto L700;
			}
			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L630;
			}

/*                    Subtract the time used in CLACPY. */

			s1 = second_();
			i__3 = ic;
			for (j = 1; j <= i__3; ++j) {
			    clacpy_("Full", &m, &n, &a[1], &m, &h__[1], &lda);
/* L640: */
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

			times_ref(ipar, itype, in, 16) = times_ref(lastnl, 
				itype, in, 16);
			opcnts_ref(ipar, itype, in, 16) = opcnts_ref(lastnl, 
				itype, in, 16);
		    }
/* L650: */
		}
	    }

/*           Time CSVDC (singular values and min(M,N) left singular   
             vectors and N right singular vectors) for each pair   
             NNB(j), LDAS(j) */

	    if (timsub[16]) {
		i__4 = *nparms;
		for (ipar = 1; ipar <= i__4; ++ipar) {
		    lda = ldas[ipar];

/*                 If this value of LDA has been used before, just   
                   use that value */

		    lastnl = 0;
		    i__3 = ipar - 1;
		    for (j = 1; j <= i__3; ++j) {
			if (lda == ldas[j]) {
			    lastnl = j;
			}
/* L660: */
		    }

		    if (lastnl == 0) {

/*                    Time CSVDC (singular values and min(M,N) left   
                      singular vectors and N right singular vectors) */

			ic = 0;
			latime_1.ops = 0.f;
			s1 = second_();
L670:
			clacpy_("Full", &m, &n, &a[1], &m, &h__[1], &lda);
			csvdc_(&h__[1], &lda, &m, &n, &dc[1], &ec[1], &u[1], &
				lda, &vt[1], &lda, &work[1], &c__21, &iinfo);
			if (iinfo != 0) {
			    io___57.ciunit = *nout;
			    s_wsfe(&io___57);
			    do_fio(&c__1, subnam_ref(0, 16), (ftnlen)9);
			    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer))
				    ;
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
			    goto L700;
			}
			s2 = second_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L670;
			}

/*                    Subtract the time used in CLACPY. */

			s1 = second_();
			i__3 = ic;
			for (j = 1; j <= i__3; ++j) {
			    clacpy_("Full", &m, &n, &a[1], &m, &h__[1], &lda);
/* L680: */
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

			times_ref(ipar, itype, in, 16) = times_ref(lastnl, 
				itype, in, 16);
			opcnts_ref(ipar, itype, in, 16) = opcnts_ref(lastnl, 
				itype, in, 16);
		    }
/* L690: */
		}
	    }

L700:
	    ;
	}
/* L710: */
    }

/* -----------------------------------------------------------------------   

       Print a table of results for each timed routine. */

    for (isub = 1; isub <= 17; ++isub) {
	if (timsub[isub - 1]) {
	    sprtbv_(subnam_ref(0, isub), ntypes, &dotype[1], nsizes, &mm[1], &
		    nn[1], &inparm[isub - 1], pnames, nparms, &ldas[1], &nnb[
		    1], &opcnts_ref(1, 1, 1, isub), ldo1, ldo2, &times_ref(1, 
		    1, 1, isub), ldt1, ldt2, &rwork[1], &llwork[1], nout, (
		    ftnlen)9, (ftnlen)4);
	}
/* L720: */
    }

    return 0;

/*     End of CTIM26 */


} /* ctim26_ */

#undef opcnts_ref
#undef subnam_ref
#undef times_ref


