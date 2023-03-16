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

struct {
    integer iparms[100];
} claenv_;

#define claenv_1 claenv_

/* Table of constant values */

static integer c__1 = 1;
static integer c__3 = 3;
static integer c__12 = 12;
static integer c__0 = 0;
static integer c__400 = 400;
static integer c__420 = 420;
static integer c__5 = 5;
static integer c__6 = 6;
static integer c_b226 = 649241;
static integer c__10 = 10;

/* Main program */ MAIN__(void)
{
    /* Initialized data */

    static integer iseed[4] = { 0,0,0,1 };
    static integer mxtype[4] = { 8,4,5,4 };

    /* Format strings */
    static char fmt_9993[] = "(\002 Timing the Nonsymmetric Eigenvalue Probl"
	    "em routines\002,/\002    DGEHRD, DHSEQR, DTREVC, and DHSEIN\002)";
    static char fmt_9992[] = "(\002 Timing the Symmetric Eigenvalue Problem "
	    "routines\002,/\002    DSYTRD, DSTEQR, and DSTERF\002)";
    static char fmt_9991[] = "(\002 Timing the Singular Value Decomposition "
	    "routines\002,/\002    DGEBRD, DBDSQR, DORGBR, DBDSDC and DGESD"
	    "D\002)";
    static char fmt_9990[] = "(\002 Timing the Generalized Eigenvalue Proble"
	    "m routines\002,/\002    DGGHRD, DHGEQZ, and DTGEVC \002)";
    static char fmt_9996[] = "(1x,a3,\002:  Unrecognized path name\002)";
    static char fmt_9985[] = "(/\002 LAPACK VERSION 3.0, released June 30, 1"
	    "999 \002)";
    static char fmt_9989[] = "(/\002 The following parameter values will be "
	    "used:\002)";
    static char fmt_9995[] = "(\002 *** Invalid input value: \002,a6,\002"
	    "=\002,i6,\002; must be >=\002,i6)";
    static char fmt_9994[] = "(\002 *** Invalid input value: \002,a6,\002"
	    "=\002,i6,\002; must be <=\002,i6)";
    static char fmt_9988[] = "(\002    Values of \002,a5,\002:  \002,10i6,/1"
	    "9x,10i6)";
    static char fmt_9987[] = "(/\002 Minimum time a subroutine will be timed"
	    " = \002,f8.2,\002 seconds\002,/)";
    static char fmt_9999[] = "(/\002 Execution not attempted due to input er"
	    "rors\002)";
    static char fmt_9986[] = "(\002 *** Error code from \002,a6,\002 = \002,"
	    "i4)";
    static char fmt_9998[] = "(//\002 End of timing run\002)";
    static char fmt_9997[] = "(\002 Total time used = \002,f12.2,\002 seco"
	    "nds\002,/)";

    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Builtin functions */
    integer s_rsfe(cilist *), do_fio(integer *, char *, ftnlen), e_rsfe(void),
	     s_wsfe(cilist *), e_wsfe(void);
    /* Subroutine */ int s_stop(char *, ftnlen);
    integer s_rsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_rsle(void);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsle(cilist *), e_wsle(void);

    /* Local variables */
    static char line[80];
    static integer info;
    static char path[3];
    static integer mval[12], nval[12];
    static doublereal work[649241], a[1008000]	/* was [168000][6] */, d__[
	    1600]	/* was [400][4] */;
    static integer i__;
    static logical fatal;
    extern /* Subroutine */ int dtim21_(char *, integer *, integer *, integer 
	    *, logical *, integer *, integer *, integer *, integer *, integer 
	    *, doublereal *, integer *, integer *, doublereal *, doublereal *,
	     doublereal *, doublereal *, doublereal *, integer *, logical *, 
	    integer *, doublereal *, integer *, integer *, integer *, 
	    doublereal *, integer *, integer *, integer *, integer *, ftnlen),
	     dtim22_(char *, integer *, integer *, integer *, logical *, 
	    integer *, integer *, integer *, doublereal *, integer *, integer 
	    *, doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, integer *, logical *, 
	    integer *, doublereal *, integer *, integer *, integer *, 
	    doublereal *, integer *, integer *, integer *, integer *, ftnlen);
    static integer nbval[10];
    extern /* Subroutine */ int dtim51_(char *, integer *, integer *, integer 
	    *, logical *, integer *, integer *, integer *, integer *, integer 
	    *, integer *, integer *, doublereal *, integer *, integer *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, integer *,
	     logical *, doublereal *, integer *, integer *, integer *, 
	    doublereal *, integer *, integer *, integer *, integer *, ftnlen),
	     dtim26_(char *, integer *, integer *, integer *, integer *, 
	    logical *, integer *, integer *, integer *, doublereal *, integer 
	    *, integer *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, integer *, integer *, logical *, 
	    doublereal *, integer *, integer *, integer *, doublereal *, 
	    integer *, integer *, integer *, integer *, ftnlen);
    static char vname[6];
    static integer nsval[10];
    static char c3[3];
    static integer iwork[10];
    static doublereal s1, s2;
    static integer iwork2[20406], nn;
    extern doublereal dsecnd_(void);
    static integer ldaval[10], nbkval[10], nbmval[10];
    extern logical lsamen_(integer *, char *, char *);
    static integer mxbval[10];
    static doublereal timmin;
    static integer nparms;
    static logical dotype[10], logwrk[400];
    static doublereal opcnts[30000]	/* was [10][10][12][25] */, result[
	    30000]	/* was [10][10][12][25] */;
    static integer maxtyp, ntypes;
    static logical gep, nep, sep, svd;

    /* Fortran I/O blocks */
    static cilist io___9 = { 0, 5, 1, "(A3)", 0 };
    static cilist io___11 = { 0, 6, 0, fmt_9993, 0 };
    static cilist io___12 = { 0, 6, 0, fmt_9992, 0 };
    static cilist io___13 = { 0, 6, 0, fmt_9991, 0 };
    static cilist io___14 = { 0, 6, 0, fmt_9990, 0 };
    static cilist io___15 = { 0, 6, 0, fmt_9996, 0 };
    static cilist io___16 = { 0, 6, 0, fmt_9985, 0 };
    static cilist io___17 = { 0, 6, 0, fmt_9989, 0 };
    static cilist io___18 = { 0, 5, 0, 0, 0 };
    static cilist io___20 = { 0, 6, 0, fmt_9995, 0 };
    static cilist io___21 = { 0, 6, 0, fmt_9994, 0 };
    static cilist io___22 = { 0, 5, 0, 0, 0 };
    static cilist io___26 = { 0, 6, 0, fmt_9995, 0 };
    static cilist io___27 = { 0, 6, 0, fmt_9994, 0 };
    static cilist io___28 = { 0, 6, 0, fmt_9988, 0 };
    static cilist io___29 = { 0, 5, 0, 0, 0 };
    static cilist io___31 = { 0, 6, 0, fmt_9995, 0 };
    static cilist io___32 = { 0, 6, 0, fmt_9994, 0 };
    static cilist io___33 = { 0, 6, 0, fmt_9988, 0 };
    static cilist io___34 = { 0, 5, 0, 0, 0 };
    static cilist io___36 = { 0, 6, 0, fmt_9995, 0 };
    static cilist io___37 = { 0, 6, 0, fmt_9994, 0 };
    static cilist io___38 = { 0, 5, 0, 0, 0 };
    static cilist io___40 = { 0, 6, 0, fmt_9995, 0 };
    static cilist io___41 = { 0, 6, 0, fmt_9988, 0 };
    static cilist io___42 = { 0, 5, 0, 0, 0 };
    static cilist io___44 = { 0, 6, 0, fmt_9995, 0 };
    static cilist io___45 = { 0, 6, 0, fmt_9988, 0 };
    static cilist io___46 = { 0, 5, 0, 0, 0 };
    static cilist io___48 = { 0, 6, 0, fmt_9995, 0 };
    static cilist io___49 = { 0, 6, 0, fmt_9988, 0 };
    static cilist io___50 = { 0, 5, 0, 0, 0 };
    static cilist io___52 = { 0, 6, 0, fmt_9995, 0 };
    static cilist io___53 = { 0, 6, 0, fmt_9988, 0 };
    static cilist io___54 = { 0, 5, 0, 0, 0 };
    static cilist io___56 = { 0, 6, 0, fmt_9995, 0 };
    static cilist io___57 = { 0, 6, 0, fmt_9988, 0 };
    static cilist io___58 = { 0, 5, 0, 0, 0 };
    static cilist io___60 = { 0, 6, 0, fmt_9995, 0 };
    static cilist io___61 = { 0, 6, 0, fmt_9994, 0 };
    static cilist io___62 = { 0, 6, 0, fmt_9988, 0 };
    static cilist io___63 = { 0, 5, 0, 0, 0 };
    static cilist io___65 = { 0, 6, 0, fmt_9987, 0 };
    static cilist io___66 = { 0, 5, 0, 0, 0 };
    static cilist io___68 = { 0, 6, 0, fmt_9995, 0 };
    static cilist io___70 = { 0, 5, 0, 0, 0 };
    static cilist io___73 = { 0, 6, 0, fmt_9995, 0 };
    static cilist io___74 = { 0, 6, 0, fmt_9994, 0 };
    static cilist io___75 = { 0, 6, 0, fmt_9999, 0 };
    static cilist io___76 = { 0, 5, 1, "(A80)", 0 };
    static cilist io___87 = { 0, 6, 0, fmt_9986, 0 };
    static cilist io___88 = { 0, 6, 0, fmt_9986, 0 };
    static cilist io___89 = { 0, 6, 0, fmt_9986, 0 };
    static cilist io___90 = { 0, 6, 0, fmt_9986, 0 };
    static cilist io___91 = { 0, 6, 0, 0, 0 };
    static cilist io___92 = { 0, 6, 0, 0, 0 };
    static cilist io___93 = { 0, 6, 0, fmt_9996, 0 };
    static cilist io___94 = { 0, 6, 0, fmt_9998, 0 };
    static cilist io___96 = { 0, 6, 0, fmt_9997, 0 };



#define a_ref(a_1,a_2) a[(a_2)*168000 + a_1 - 168001]
#define d___ref(a_1,a_2) d__[(a_2)*400 + a_1 - 401]


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   

    Purpose   
    =======   

    DTIMEE is the main timing program for the DOUBLE PRECISION matrix   
    eigenvalue routines in LAPACK.   

    There are four sets of routines that can be timed:   

    NEP (Nonsymmetric Eigenvalue Problem):   
        Includes DGEHRD, DHSEQR, DTREVC, and DHSEIN   

    SEP (Symmetric Eigenvalue Problem):   
        Includes DSYTRD, DORGTR, DORMTR, DSTEQR, DSTERF, DPTEQR, DSTEBZ,   
        DSTEIN, and DSTEDC   

    SVD (Singular Value Decomposition):   
        Includes DGEBRD, DBDSQR, DORGBR, DBDSDC and DGESDD   

    GEP (Generalized nonsymmetric Eigenvalue Problem):   
        Includes DGGHRD, DHGEQZ, and DTGEVC   

    Each test path has a different input file.  The first line of the   
    input file should contain the characters NEP, SEP, SVD, or GEP in   
    columns 1-3.  The number of remaining lines depends on what is found   
    on the first line.   

   -----------------------------------------------------------------------   

    NEP input file:   

    line 2:  NN, INTEGER   
             Number of values of N.   

    line 3:  NVAL, INTEGER array, dimension (NN)   
             The values for the matrix dimension N.   

    line 4:  NPARM, INTEGER   
             Number of values of the parameters NB, NS, MAXB, and LDA.   

    line 5:  NBVAL, INTEGER array, dimension (NPARM)   
             The values for the blocksize NB.   

    line 6:  NSVAL, INTEGER array, dimension (NPARM)   
             The values for the number of shifts.   

    line 7:  MXBVAL, INTEGER array, dimension (NPARM)   
             The values for MAXB, used in determining whether multishift   
             will be used.   

    line 8:  LDAVAL, INTEGER array, dimension (NPARM)   
             The values for the leading dimension LDA.   

    line 9:  TIMMIN, DOUBLE PRECISION   
             The minimum time (in seconds) that a subroutine will be   
             timed.  If TIMMIN is zero, each routine should be timed only   
             once.   

    line 10: NTYPES, INTEGER   
             The number of matrix types to be used in the timing run.   
             If NTYPES >= MAXTYP, all the types are used.   

    If 0 < NTYPES < MAXTYP, then line 11 specifies NTYPES integer   
    values, which are the numbers of the matrix types to be used.   

    The remaining lines specify a path name and the specific routines to   
    be timed.  For the nonsymmetric eigenvalue problem, the path name is   
    'DHS'.  A line to request all the routines in this path has the form   
       DHS   T T T T T T T T T T T T   
    where the first 3 characters specify the path name, and up to MAXTYP   
    nonblank characters may appear in columns 4-80.  If the k-th such   
    character is 'T' or 't', the k-th routine will be timed.  If at least   
    one but fewer than 12 nonblank characters are specified, the   
    remaining routines will not be timed.  If columns 4-80 are blank, all   
    the routines will be timed, so the input line   
       DHS   
    is equivalent to the line above.   

   -----------------------------------------------------------------------   

    SEP input file:   

    line 2:  NN, INTEGER   
             Number of values of N.   

    line 3:  NVAL, INTEGER array, dimension (NN)   
             The values for the matrix dimension N.   

    line 4:  NPARM, INTEGER   
             Number of values of the parameters NB and LDA.   

    line 5:  NBVAL, INTEGER array, dimension (NPARM)   
             The values for the blocksize NB.   

    line 6:  LDAVAL, INTEGER array, dimension (NPARM)   
             The values for the leading dimension LDA.   

    line 7:  TIMMIN, DOUBLE PRECISION   
             The minimum time (in seconds) that a subroutine will be   
             timed.  If TIMMIN is zero, each routine should be timed only   
             once.   

    line 8:  NTYPES, INTEGER   
             The number of matrix types to be used in the timing run.   
             If NTYPES >= MAXTYP, all the types are used.   

    If 0 < NTYPES < MAXTYP, then line 9 specifies NTYPES integer   
    values, which are the numbers of the matrix types to be used.   

    The remaining lines specify a path name and the specific routines to   
    be timed as for the NEP input file.  For the symmetric eigenvalue   
    problem, the path name is 'DST' and up to 8 routines may be timed.   

   -----------------------------------------------------------------------   

    SVD input file:   

    line 2:  NN, INTEGER   
             Number of values of M and N.   

    line 3:  MVAL, INTEGER array, dimension (NN)   
             The values for the matrix dimension M.   

    line 4:  NVAL, INTEGER array, dimension (NN)   
             The values for the matrix dimension N.   

    line 5:  NPARM, INTEGER   
             Number of values of the parameters NB and LDA.   

    line 6:  NBVAL, INTEGER array, dimension (NPARM)   
             The values for the blocksize NB.   

    line 7:  LDAVAL, INTEGER array, dimension (NPARM)   
             The values for the leading dimension LDA.   

    line 8:  TIMMIN, DOUBLE PRECISION   
             The minimum time (in seconds) that a subroutine will be   
             timed.  If TIMMIN is zero, each routine should be timed only   
             once.   

    line 9:  NTYPES, INTEGER   
             The number of matrix types to be used in the timing run.   
             If NTYPES >= MAXTYP, all the types are used.   

    If 0 < NTYPES < MAXTYP, then line 10 specifies NTYPES integer   
    values, which are the numbers of the matrix types to be used.   

    The remaining lines specify a path name and the specific routines to   
    be timed as for the NEP input file.  For the singular value   
    decomposition the path name is 'DBD' and up to 16 routines may be   
    timed.   

   -----------------------------------------------------------------------   

    GEP input file:   

    line 2:  NN, INTEGER   
             Number of values of N.   

    line 3:  NVAL, INTEGER array, dimension (NN)   
             The values for the matrix dimension N.   

    line 4:  NPARM, INTEGER   
             Number of values of the parameters NB, NS, MAXB, and LDA.   

    line 5:  NBVAL, INTEGER array, dimension (NPARM)   
             The values for the blocksize NB.   

    line 6:  NSVAL, INTEGER array, dimension (NPARM)   
             The values for the number of shifts.   

    line 7:  NEIVAL, INTEGER array, dimension (NPARM)   
             The values for NEISP, used in determining whether multishift   
             will be used.   

    line 8:  NBMVAL, INTEGER array, dimension (NPARM)   
             The values for MINNB, used in determining minimum blocksize.   

    line 9:  NBKVAL, INTEGER array, dimension (NPARM)   
             The values for MINBLK, also used in determining minimum   
             blocksize.   

    line 10: LDAVAL, INTEGER array, dimension (NPARM)   
             The values for the leading dimension LDA.   

    line 11: TIMMIN, DOUBLE PRECISION   
             The minimum time (in seconds) that a subroutine will be   
             timed.  If TIMMIN is zero, each routine should be timed only   
             once.   

    line 12: NTYPES, INTEGER   
             The number of matrix types to be used in the timing run.   
             If NTYPES >= MAXTYP, all the types are used.   

    If 0 < NTYPES < MAXTYP, then line 13 specifies NTYPES integer   
    values, which are the numbers of the matrix types to be used.   

    The remaining lines specify a path name and the specific routines to   
    be timed.  For the nonsymmetric eigenvalue problem, the path name is   
    'DHG'.  A line to request all the routines in this path has the form   
       DHG   T T T T T T T T T T T T T T T T T T   
    where the first 3 characters specify the path name, and up to MAXTYP   
    nonblank characters may appear in columns 4-80.  If the k-th such   
    character is 'T' or 't', the k-th routine will be timed.  If at least   
    one but fewer than 18 nonblank characters are specified, the   
    remaining routines will not be timed.  If columns 4-80 are blank, all   
    the routines will be timed, so the input line   
       DHG   
    is equivalent to the line above.   

   =======================================================================   

    The workspace requirements in terms of square matrices for the   
    different test paths are as follows:   

    NEP:   3 N**2 + N*(3*NB+2)   
    SEP:   2 N**2 + N*(2*N) + N   
    SVD:   4 N**2 + MAX( 6*N, MAXIN*MAXPRM*MAXT )   
    GEP:   6 N**2 + 3*N   

    MAXN is currently set to 400,   
    LG2MXN = ceiling of log-base-2 of MAXN = 9, and LDAMAX = 420.   
    The real work space needed is LWORK = MAX( MAXN*(4*MAXN+2),   
         2*LDAMAX+1+3*MAXN+2*MAXN*LG2MXN+3*MAXN**2 ),  and the integer   
    workspace needed is  LIWRK2 = 6 + 6*MAXN + 5*MAXN*LG2MXN.   
    For SVD, we assume NRHS may be as big   
    as N.  The parameter NEED is set to 4 to allow for 4 NxN matrices   
    for SVD. */


    s1 = dsecnd_();
    fatal = FALSE_;
    nep = FALSE_;
    sep = FALSE_;
    svd = FALSE_;
    gep = FALSE_;

/*     Read the 3-character test path */

    i__1 = s_rsfe(&io___9);
    if (i__1 != 0) {
	goto L160;
    }
    i__1 = do_fio(&c__1, path, (ftnlen)3);
    if (i__1 != 0) {
	goto L160;
    }
    i__1 = e_rsfe();
    if (i__1 != 0) {
	goto L160;
    }
    nep = lsamen_(&c__3, path, "NEP") || lsamen_(&c__3, 
	    path, "DHS");
    sep = lsamen_(&c__3, path, "SEP") || lsamen_(&c__3, 
	    path, "DST");
    svd = lsamen_(&c__3, path, "SVD") || lsamen_(&c__3, 
	    path, "DBD");
    gep = lsamen_(&c__3, path, "GEP") || lsamen_(&c__3, 
	    path, "DHG");

/*     Report values of parameters as they are read. */

    if (nep) {
	s_wsfe(&io___11);
	e_wsfe();
    } else if (sep) {
	s_wsfe(&io___12);
	e_wsfe();
    } else if (svd) {
	s_wsfe(&io___13);
	e_wsfe();
    } else if (gep) {
	s_wsfe(&io___14);
	e_wsfe();
    } else {
	s_wsfe(&io___15);
	do_fio(&c__1, path, (ftnlen)3);
	e_wsfe();
	s_stop("", (ftnlen)0);
    }
    s_wsfe(&io___16);
    e_wsfe();
    s_wsfe(&io___17);
    e_wsfe();

/*     Read the number of values of M and N. */

    s_rsle(&io___18);
    do_lio(&c__3, &c__1, (char *)&nn, (ftnlen)sizeof(integer));
    e_rsle();
    if (nn < 1) {
	s_wsfe(&io___20);
	do_fio(&c__1, "NN  ", (ftnlen)4);
	do_fio(&c__1, (char *)&nn, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&c__1, (ftnlen)sizeof(integer));
	e_wsfe();
	nn = 0;
	fatal = TRUE_;
    } else if (nn > 12) {
	s_wsfe(&io___21);
	do_fio(&c__1, "NN  ", (ftnlen)4);
	do_fio(&c__1, (char *)&nn, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&c__12, (ftnlen)sizeof(integer));
	e_wsfe();
	nn = 0;
	fatal = TRUE_;
    }

/*     Read the values of M */

    s_rsle(&io___22);
    i__1 = nn;
    for (i__ = 1; i__ <= i__1; ++i__) {
	do_lio(&c__3, &c__1, (char *)&mval[i__ - 1], (ftnlen)sizeof(integer));
    }
    e_rsle();
    if (svd) {
	s_copy(vname, "  M", (ftnlen)6, (ftnlen)3);
    } else {
	s_copy(vname, "  N", (ftnlen)6, (ftnlen)3);
    }
    i__1 = nn;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (mval[i__ - 1] < 0) {
	    s_wsfe(&io___26);
	    do_fio(&c__1, vname, (ftnlen)6);
	    do_fio(&c__1, (char *)&mval[i__ - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
	    e_wsfe();
	    fatal = TRUE_;
	} else if (mval[i__ - 1] > 400) {
	    s_wsfe(&io___27);
	    do_fio(&c__1, vname, (ftnlen)6);
	    do_fio(&c__1, (char *)&mval[i__ - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&c__400, (ftnlen)sizeof(integer));
	    e_wsfe();
	    fatal = TRUE_;
	}
/* L10: */
    }

/*     Read the values of N */

    if (svd) {
	s_wsfe(&io___28);
	do_fio(&c__1, "M   ", (ftnlen)4);
	i__1 = nn;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    do_fio(&c__1, (char *)&mval[i__ - 1], (ftnlen)sizeof(integer));
	}
	e_wsfe();
	s_rsle(&io___29);
	i__1 = nn;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    do_lio(&c__3, &c__1, (char *)&nval[i__ - 1], (ftnlen)sizeof(
		    integer));
	}
	e_rsle();
	i__1 = nn;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (nval[i__ - 1] < 0) {
		s_wsfe(&io___31);
		do_fio(&c__1, "N   ", (ftnlen)4);
		do_fio(&c__1, (char *)&nval[i__ - 1], (ftnlen)sizeof(integer))
			;
		do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
		e_wsfe();
		fatal = TRUE_;
	    } else if (nval[i__ - 1] > 400) {
		s_wsfe(&io___32);
		do_fio(&c__1, "N   ", (ftnlen)4);
		do_fio(&c__1, (char *)&nval[i__ - 1], (ftnlen)sizeof(integer))
			;
		do_fio(&c__1, (char *)&c__400, (ftnlen)sizeof(integer));
		e_wsfe();
		fatal = TRUE_;
	    }
/* L20: */
	}
    } else {
	i__1 = nn;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    nval[i__ - 1] = mval[i__ - 1];
/* L30: */
	}
    }
    s_wsfe(&io___33);
    do_fio(&c__1, "N   ", (ftnlen)4);
    i__1 = nn;
    for (i__ = 1; i__ <= i__1; ++i__) {
	do_fio(&c__1, (char *)&nval[i__ - 1], (ftnlen)sizeof(integer));
    }
    e_wsfe();

/*     Read the number of parameter values. */

    s_rsle(&io___34);
    do_lio(&c__3, &c__1, (char *)&nparms, (ftnlen)sizeof(integer));
    e_rsle();
    if (nparms < 1) {
	s_wsfe(&io___36);
	do_fio(&c__1, "NPARMS", (ftnlen)6);
	do_fio(&c__1, (char *)&nparms, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&c__1, (ftnlen)sizeof(integer));
	e_wsfe();
	nparms = 0;
	fatal = TRUE_;
    } else if (nparms > 12) {
	s_wsfe(&io___37);
	do_fio(&c__1, "NPARMS", (ftnlen)6);
	do_fio(&c__1, (char *)&nparms, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&c__12, (ftnlen)sizeof(integer));
	e_wsfe();
	nparms = 0;
	fatal = TRUE_;
    }

/*     Read the values of NB */

    s_rsle(&io___38);
    i__1 = nparms;
    for (i__ = 1; i__ <= i__1; ++i__) {
	do_lio(&c__3, &c__1, (char *)&nbval[i__ - 1], (ftnlen)sizeof(integer))
		;
    }
    e_rsle();
    i__1 = nparms;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (nbval[i__ - 1] < 0) {
	    s_wsfe(&io___40);
	    do_fio(&c__1, "NB  ", (ftnlen)4);
	    do_fio(&c__1, (char *)&nbval[i__ - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
	    e_wsfe();
	    fatal = TRUE_;
	}
/* L40: */
    }
    s_wsfe(&io___41);
    do_fio(&c__1, "NB  ", (ftnlen)4);
    i__1 = nparms;
    for (i__ = 1; i__ <= i__1; ++i__) {
	do_fio(&c__1, (char *)&nbval[i__ - 1], (ftnlen)sizeof(integer));
    }
    e_wsfe();

    if (nep || gep) {

/*        Read the values of NSHIFT */

	s_rsle(&io___42);
	i__1 = nparms;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    do_lio(&c__3, &c__1, (char *)&nsval[i__ - 1], (ftnlen)sizeof(
		    integer));
	}
	e_rsle();
	i__1 = nparms;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (nsval[i__ - 1] < 0) {
		s_wsfe(&io___44);
		do_fio(&c__1, "NS  ", (ftnlen)4);
		do_fio(&c__1, (char *)&nsval[i__ - 1], (ftnlen)sizeof(integer)
			);
		do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
		e_wsfe();
		fatal = TRUE_;
	    }
/* L50: */
	}
	s_wsfe(&io___45);
	do_fio(&c__1, "NS  ", (ftnlen)4);
	i__1 = nparms;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    do_fio(&c__1, (char *)&nsval[i__ - 1], (ftnlen)sizeof(integer));
	}
	e_wsfe();

/*        Read the values of MAXB */

	s_rsle(&io___46);
	i__1 = nparms;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    do_lio(&c__3, &c__1, (char *)&mxbval[i__ - 1], (ftnlen)sizeof(
		    integer));
	}
	e_rsle();
	i__1 = nparms;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (mxbval[i__ - 1] < 0) {
		s_wsfe(&io___48);
		do_fio(&c__1, "MAXB", (ftnlen)4);
		do_fio(&c__1, (char *)&mxbval[i__ - 1], (ftnlen)sizeof(
			integer));
		do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
		e_wsfe();
		fatal = TRUE_;
	    }
/* L60: */
	}
	s_wsfe(&io___49);
	do_fio(&c__1, "MAXB", (ftnlen)4);
	i__1 = nparms;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    do_fio(&c__1, (char *)&mxbval[i__ - 1], (ftnlen)sizeof(integer));
	}
	e_wsfe();
    } else {
	i__1 = nparms;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    nsval[i__ - 1] = 1;
	    mxbval[i__ - 1] = 1;
/* L70: */
	}
    }

    if (gep) {

/*        Read the values of NBMIN */

	s_rsle(&io___50);
	i__1 = nparms;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    do_lio(&c__3, &c__1, (char *)&nbmval[i__ - 1], (ftnlen)sizeof(
		    integer));
	}
	e_rsle();
	i__1 = nparms;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (nbmval[i__ - 1] < 0) {
		s_wsfe(&io___52);
		do_fio(&c__1, "NBMIN", (ftnlen)5);
		do_fio(&c__1, (char *)&nbmval[i__ - 1], (ftnlen)sizeof(
			integer));
		do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
		e_wsfe();
		fatal = TRUE_;
	    }
/* L80: */
	}
	s_wsfe(&io___53);
	do_fio(&c__1, "NBMIN", (ftnlen)5);
	i__1 = nparms;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    do_fio(&c__1, (char *)&nbmval[i__ - 1], (ftnlen)sizeof(integer));
	}
	e_wsfe();

/*        Read the values of MINBLK */

	s_rsle(&io___54);
	i__1 = nparms;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    do_lio(&c__3, &c__1, (char *)&nbkval[i__ - 1], (ftnlen)sizeof(
		    integer));
	}
	e_rsle();
	i__1 = nparms;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (nbkval[i__ - 1] < 0) {
		s_wsfe(&io___56);
		do_fio(&c__1, "MINBLK", (ftnlen)6);
		do_fio(&c__1, (char *)&nbkval[i__ - 1], (ftnlen)sizeof(
			integer));
		do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
		e_wsfe();
		fatal = TRUE_;
	    }
/* L90: */
	}
	s_wsfe(&io___57);
	do_fio(&c__1, "MINBLK", (ftnlen)6);
	i__1 = nparms;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    do_fio(&c__1, (char *)&nbkval[i__ - 1], (ftnlen)sizeof(integer));
	}
	e_wsfe();
    } else {
	i__1 = nparms;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    nbmval[i__ - 1] = 401;
	    nbkval[i__ - 1] = 401;
/* L100: */
	}
    }

/*     Read the values of LDA */

    s_rsle(&io___58);
    i__1 = nparms;
    for (i__ = 1; i__ <= i__1; ++i__) {
	do_lio(&c__3, &c__1, (char *)&ldaval[i__ - 1], (ftnlen)sizeof(integer)
		);
    }
    e_rsle();
    i__1 = nparms;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (ldaval[i__ - 1] < 0) {
	    s_wsfe(&io___60);
	    do_fio(&c__1, "LDA ", (ftnlen)4);
	    do_fio(&c__1, (char *)&ldaval[i__ - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
	    e_wsfe();
	    fatal = TRUE_;
	} else if (ldaval[i__ - 1] > 420) {
	    s_wsfe(&io___61);
	    do_fio(&c__1, "LDA ", (ftnlen)4);
	    do_fio(&c__1, (char *)&ldaval[i__ - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&c__420, (ftnlen)sizeof(integer));
	    e_wsfe();
	    fatal = TRUE_;
	}
/* L110: */
    }
    s_wsfe(&io___62);
    do_fio(&c__1, "LDA ", (ftnlen)4);
    i__1 = nparms;
    for (i__ = 1; i__ <= i__1; ++i__) {
	do_fio(&c__1, (char *)&ldaval[i__ - 1], (ftnlen)sizeof(integer));
    }
    e_wsfe();

/*     Read the minimum time a subroutine will be timed. */

    s_rsle(&io___63);
    do_lio(&c__5, &c__1, (char *)&timmin, (ftnlen)sizeof(doublereal));
    e_rsle();
    s_wsfe(&io___65);
    do_fio(&c__1, (char *)&timmin, (ftnlen)sizeof(doublereal));
    e_wsfe();

/*     Read the number of matrix types to use in timing. */

    s_rsle(&io___66);
    do_lio(&c__3, &c__1, (char *)&ntypes, (ftnlen)sizeof(integer));
    e_rsle();
    if (ntypes < 0) {
	s_wsfe(&io___68);
	do_fio(&c__1, "NTYPES", (ftnlen)6);
	do_fio(&c__1, (char *)&ntypes, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
	e_wsfe();
	fatal = TRUE_;
	ntypes = 0;
    }

/*     Read the matrix types. */

    if (nep) {
	maxtyp = mxtype[0];
    } else if (sep) {
	maxtyp = mxtype[1];
    } else if (svd) {
	maxtyp = mxtype[2];
    } else {
	maxtyp = mxtype[3];
    }
    if (ntypes < maxtyp) {
	s_rsle(&io___70);
	i__1 = ntypes;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    do_lio(&c__3, &c__1, (char *)&iwork[i__ - 1], (ftnlen)sizeof(
		    integer));
	}
	e_rsle();
	i__1 = maxtyp;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    dotype[i__ - 1] = FALSE_;
/* L120: */
	}
	i__1 = ntypes;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (iwork[i__ - 1] < 0) {
		s_wsfe(&io___73);
		do_fio(&c__1, "TYPE", (ftnlen)4);
		do_fio(&c__1, (char *)&iwork[i__ - 1], (ftnlen)sizeof(integer)
			);
		do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
		e_wsfe();
		fatal = TRUE_;
	    } else if (iwork[i__ - 1] > maxtyp) {
		s_wsfe(&io___74);
		do_fio(&c__1, "TYPE", (ftnlen)4);
		do_fio(&c__1, (char *)&iwork[i__ - 1], (ftnlen)sizeof(integer)
			);
		do_fio(&c__1, (char *)&maxtyp, (ftnlen)sizeof(integer));
		e_wsfe();
		fatal = TRUE_;
	    } else {
		dotype[iwork[i__ - 1] - 1] = TRUE_;
	    }
/* L130: */
	}
    } else {
	ntypes = maxtyp;
	for (i__ = 1; i__ <= 10; ++i__) {
	    dotype[i__ - 1] = TRUE_;
/* L140: */
	}
    }

    if (fatal) {
	s_wsfe(&io___75);
	e_wsfe();
	s_stop("", (ftnlen)0);
    }

/*     Read the input lines indicating the test path and the routines   
       to be timed.  The first three characters indicate the test path. */

L150:
    i__1 = s_rsfe(&io___76);
    if (i__1 != 0) {
	goto L160;
    }
    i__1 = do_fio(&c__1, line, (ftnlen)80);
    if (i__1 != 0) {
	goto L160;
    }
    i__1 = e_rsfe();
    if (i__1 != 0) {
	goto L160;
    }
    s_copy(c3, line, (ftnlen)3, (ftnlen)3);

/*     -------------------------------------   
       NEP:  Nonsymmetric Eigenvalue Problem   
       ------------------------------------- */

    if (lsamen_(&c__3, c3, "DHS") || lsamen_(&c__3, c3, 
	    "NEP")) {
	dtim21_(line, &nn, nval, &maxtyp, dotype, &nparms, nbval, nsval, 
		mxbval, ldaval, &timmin, &c__6, iseed, &a_ref(1, 1), &a_ref(1,
		 2), &a_ref(1, 3), &d___ref(1, 1), work, &c_b226, logwrk, 
		iwork2, result, &c__10, &c__10, &c__12, opcnts, &c__10, &
		c__10, &c__12, &info, (ftnlen)80);
	if (info != 0) {
	    s_wsfe(&io___87);
	    do_fio(&c__1, "DTIM21", (ftnlen)6);
	    do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
	    e_wsfe();
	}

/*     ----------------------------------   
       SEP:  Symmetric Eigenvalue Problem   
       ---------------------------------- */

    } else if (lsamen_(&c__3, c3, "DST") || lsamen_(&
	    c__3, c3, "SEP")) {
	dtim22_(line, &nn, nval, &maxtyp, dotype, &nparms, nbval, ldaval, &
		timmin, &c__6, iseed, &a_ref(1, 1), &d___ref(1, 1), &d___ref(
		1, 2), &d___ref(1, 3), &a_ref(1, 2), &a_ref(1, 3), work, &
		c_b226, logwrk, iwork2, result, &c__10, &c__10, &c__12, 
		opcnts, &c__10, &c__10, &c__12, &info, (ftnlen)80);
	if (info != 0) {
	    s_wsfe(&io___88);
	    do_fio(&c__1, "DTIM22", (ftnlen)6);
	    do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
	    e_wsfe();
	}

/*     ----------------------------------   
       SVD:  Singular Value Decomposition   
       ---------------------------------- */

    } else if (lsamen_(&c__3, c3, "DBD") || lsamen_(&
	    c__3, c3, "SVD")) {
	dtim26_(line, &nn, nval, mval, &maxtyp, dotype, &nparms, nbval, 
		ldaval, &timmin, &c__6, iseed, &a_ref(1, 1), &a_ref(1, 2), &
		a_ref(1, 3), &a_ref(1, 4), &d___ref(1, 1), &d___ref(1, 2), &
		d___ref(1, 3), &d___ref(1, 4), work, &c_b226, iwork2, logwrk, 
		result, &c__10, &c__10, &c__12, opcnts, &c__10, &c__10, &
		c__12, &info, (ftnlen)80);
	if (info != 0) {
	    s_wsfe(&io___89);
	    do_fio(&c__1, "DTIM26", (ftnlen)6);
	    do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
	    e_wsfe();
	}

/*     -------------------------------------------------   
       GEP:  Generalized Nonsymmetric Eigenvalue Problem   
       ------------------------------------------------- */

    } else if (lsamen_(&c__3, c3, "DHG") || lsamen_(&
	    c__3, c3, "GEP")) {
	dtim51_(line, &nn, nval, &maxtyp, dotype, &nparms, nbval, nsval, 
		mxbval, nbmval, nbkval, ldaval, &timmin, &c__6, iseed, &a_ref(
		1, 1), &a_ref(1, 2), &a_ref(1, 3), &a_ref(1, 4), &a_ref(1, 5),
		 &a_ref(1, 6), &d___ref(1, 1), work, &c_b226, logwrk, result, 
		&c__10, &c__10, &c__12, opcnts, &c__10, &c__10, &c__12, &info,
		 (ftnlen)80);
	if (info != 0) {
	    s_wsfe(&io___90);
	    do_fio(&c__1, "DTIM51", (ftnlen)6);
	    do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
	    e_wsfe();
	}
    } else {
	s_wsle(&io___91);
	e_wsle();
	s_wsle(&io___92);
	e_wsle();
	s_wsfe(&io___93);
	do_fio(&c__1, c3, (ftnlen)3);
	e_wsfe();
    }
    goto L150;
L160:
    s_wsfe(&io___94);
    e_wsfe();
    s2 = dsecnd_();
    s_wsfe(&io___96);
    d__1 = s2 - s1;
    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    e_wsfe();


/*     End of DTIMEE */

    return 0;
} /* MAIN__ */

#undef d___ref
#undef a_ref


/* Main program alias */ int dtimee_ () { MAIN__ (); return 0; }
