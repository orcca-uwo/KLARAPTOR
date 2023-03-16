#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    integer nproc, nshift, maxb;
} cenvir_;

#define cenvir_1 cenvir_

struct {
    integer infot, nunit;
    logical ok, lerr;
} infoc_;

#define infoc_1 infoc_

struct {
    char srnamt[6];
} srnamc_;

#define srnamc_1 srnamc_

struct {
    integer selopt, seldim;
    logical selval[20];
    doublereal selwr[20], selwi[20];
} sslct_;

#define sslct_1 sslct_

struct {
    integer iparms[100];
} zlaenv_;

#define zlaenv_1 zlaenv_

/* Table of constant values */

static integer c__1 = 1;
static integer c__3 = 3;
static integer c__5 = 5;
static integer c__6 = 6;
static integer c__20 = 20;
static integer c__0 = 0;
static integer c__132 = 132;
static integer c__2 = 2;
static integer c__4 = 4;
static integer c__8 = 8;
static integer c__89760 = 89760;
static integer c__9 = 9;
static integer c__25 = 25;
static integer c__20064 = 20064;
static integer c__18 = 18;
static integer c__400 = 400;
static integer c__20062 = 20062;
static integer c__264 = 264;

/* Main program */ MAIN__(void)
{
    /* Initialized data */

    static char intstr[10] = "0123456789";
    static integer ioldsd[4] = { 0,0,0,1 };

    /* Format strings */
    static char fmt_9987[] = "(\002 Tests of the Nonsymmetric Eigenvalue Pro"
	    "blem routines\002)";
    static char fmt_9986[] = "(\002 Tests of the Hermitian Eigenvalue Proble"
	    "m routines\002)";
    static char fmt_9985[] = "(\002 Tests of the Singular Value Decompositio"
	    "n routines\002)";
    static char fmt_9979[] = "(/\002 Tests of the Nonsymmetric Eigenvalue Pr"
	    "oblem Driver\002,/\002    ZGEEV (eigenvalues and eigevectors)"
	    "\002)";
    static char fmt_9978[] = "(/\002 Tests of the Nonsymmetric Eigenvalue Pr"
	    "oblem Driver\002,/\002    ZGEES (Schur form)\002)";
    static char fmt_9977[] = "(/\002 Tests of the Nonsymmetric Eigenvalue Pr"
	    "oblem Expert\002,\002 Driver\002,/\002    ZGEEVX (eigenvalues, e"
	    "igenvectors and\002,\002 condition numbers)\002)";
    static char fmt_9976[] = "(/\002 Tests of the Nonsymmetric Eigenvalue Pr"
	    "oblem Expert\002,\002 Driver\002,/\002    ZGEESX (Schur form and"
	    " condition\002,\002 numbers)\002)";
    static char fmt_9975[] = "(/\002 Tests of the Generalized Nonsymmetric E"
	    "igenvalue \002,\002Problem routines\002)";
    static char fmt_9964[] = "(/\002 Tests of the Generalized Nonsymmetric E"
	    "igenvalue \002,\002Problem Driver ZGGES\002)";
    static char fmt_9965[] = "(/\002 Tests of the Generalized Nonsymmetric E"
	    "igenvalue \002,\002Problem Expert Driver ZGGESX\002)";
    static char fmt_9963[] = "(/\002 Tests of the Generalized Nonsymmetric E"
	    "igenvalue \002,\002Problem Driver ZGGEV\002)";
    static char fmt_9962[] = "(/\002 Tests of the Generalized Nonsymmetric E"
	    "igenvalue \002,\002Problem Expert Driver ZGGEVX\002)";
    static char fmt_9974[] = "(\002 Tests of ZHBTRD\002,/\002 (reduction of "
	    "a Hermitian band \002,\002matrix to real tridiagonal form)\002)";
    static char fmt_9967[] = "(\002 Tests of ZGBBRD\002,/\002 (reduction of "
	    "a general band \002,\002matrix to real bidiagonal form)\002)";
    static char fmt_9971[] = "(/\002 Tests of the Generalized Linear Regress"
	    "ion Model \002,\002routines\002)";
    static char fmt_9970[] = "(/\002 Tests of the Generalized QR and RQ rout"
	    "ines\002)";
    static char fmt_9969[] = "(/\002 Tests of the Generalized Singular Valu"
	    "e\002,\002 Decomposition routines\002)";
    static char fmt_9968[] = "(/\002 Tests of the Linear Least Squares routi"
	    "nes\002)";
    static char fmt_9992[] = "(1x,a3,\002:  Unrecognized path name\002)";
    static char fmt_9972[] = "(/\002 LAPACK VERSION 3.0, released June 30, 1"
	    "999 \002)";
    static char fmt_9984[] = "(/\002 The following parameter values will be "
	    "used:\002)";
    static char fmt_9989[] = "(\002 Invalid input value: \002,a6,\002=\002,i"
	    "6,\002; must be >=\002,i6)";
    static char fmt_9988[] = "(\002 Invalid input value: \002,a6,\002=\002,i"
	    "6,\002; must be <=\002,i6)";
    static char fmt_9983[] = "(4x,a6,10i6,/10x,10i6)";
    static char fmt_9981[] = "(\002 Relative machine \002,a,\002 is taken to"
	    " be\002,d16.6)";
    static char fmt_9982[] = "(/\002 Routines pass computational tests if te"
	    "st ratio is \002,\002less than\002,f8.2,/)";
    static char fmt_9999[] = "(/\002 Execution not attempted due to input er"
	    "rors\002)";
    static char fmt_9991[] = "(//\002 *** Invalid integer value in column"
	    " \002,i2,\002 of input\002,\002 line:\002,/a79)";
    static char fmt_9990[] = "(//1x,a3,\002 routines were not tested\002)";
    static char fmt_9998[] = "(//1x,a3,\002:  NB =\002,i4,\002, NBMIN =\002,"
	    "i4,\002, NX =\002,i4,\002, NS =\002,i4,\002, MAXB =\002,i4)";
    static char fmt_9980[] = "(\002 *** Error code from \002,a6,\002 = \002,"
	    "i4)";
    static char fmt_9997[] = "(//1x,a3,\002:  NB =\002,i4,\002, NBMIN =\002,"
	    "i4,\002, NX =\002,i4)";
    static char fmt_9995[] = "(//1x,a3,\002:  NB =\002,i4,\002, NBMIN =\002,"
	    "i4,\002, NX =\002,i4,\002, NRHS =\002,i4)";
    static char fmt_9973[] = "(/1x,71(\002-\002))";
    static char fmt_9996[] = "(//1x,a3,\002:  NB =\002,i4,\002, NBMIN =\002,"
	    "i4,\002, NS =\002,i4,\002, MAXB =\002,i4,\002, NBCOL =\002,i4)";
    static char fmt_9966[] = "(//1x,a3,\002:  NRHS =\002,i4)";
    static char fmt_9994[] = "(//\002 End of tests\002)";
    static char fmt_9993[] = "(\002 Total time used = \002,f12.2,\002 seco"
	    "nds\002,/)";

    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Builtin functions */
    integer s_rsfe(cilist *), do_fio(integer *, char *, ftnlen), e_rsfe(void);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), s_wsfe(cilist *), e_wsfe(
	    void), s_rsle(cilist *), do_lio(integer *, integer *, char *, 
	    ftnlen), e_rsle(void), s_wsle(cilist *), e_wsle(void);
    /* Subroutine */ int s_stop(char *, ftnlen);
    integer i_len(char *, ftnlen);

    /* Local variables */
    static doublereal beta[132];
    static char line[80];
    static doublecomplex taua[132];
    static integer info;
    static char path[3];
    static integer kval[20], lenp, mval[20], nval[20];
    static doublecomplex taub[132];
    static integer pval[20], itmp, nrhs;
    static doublecomplex work[89760], a[243936]	/* was [17424][14] */, b[
	    87120]	/* was [17424][5] */, c__[160000]	/* was [400][
	    400] */;
    static integer i__, k;
    static doublereal s[17424], alpha[132];
    static logical fatal;
    static doublecomplex x[660];
    static integer iseed[4], nbcol[20], nbval[20], nbmin[20];
    static char vname[6];
    static integer newsd, nsval[20];
    static char c1[1], c3[3];
    static integer nxval[20], i1, iwork[20064];
    static doublereal s1, s2, rwork[89760];
    static doublecomplex dc[792]	/* was [132][6] */;
    static integer ic;
    static doublereal dr[1584]	/* was [132][12] */;
    extern doublereal dlamch_(char *);
    static integer nk, nn;
    extern doublereal dsecnd_(void);
    extern /* Subroutine */ int zchkbb_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, logical *, integer *, integer *, 
	    doublereal *, integer *, doublecomplex *, integer *, 
	    doublecomplex *, integer *, doublereal *, doublereal *, 
	    doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, doublecomplex *, 
	    integer *, doublereal *, doublereal *, integer *), alareq_(char *,
	     integer *, logical *, integer *, integer *, integer *), 
	    zchkbd_(integer *, integer *, integer *, integer *, logical *, 
	    integer *, integer *, doublereal *, doublecomplex *, integer *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublecomplex *, integer *, doublecomplex *, doublecomplex *, 
	    doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, integer *, 
	    doublereal *, integer *, integer *), zchkec_(doublereal *, 
	    logical *, integer *, integer *), zchkhb_(integer *, integer *, 
	    integer *, integer *, integer *, logical *, integer *, doublereal 
	    *, integer *, doublecomplex *, integer *, doublereal *, 
	    doublereal *, doublecomplex *, integer *, doublecomplex *, 
	    integer *, doublereal *, doublereal *, integer *), zchkbk_(
	    integer *, integer *), zchkbl_(integer *, integer *);
    extern logical lsamen_(integer *, char *, char *);
    extern /* Subroutine */ int zchkgg_(integer *, integer *, integer *, 
	    logical *, integer *, doublereal *, logical *, doublereal *, 
	    integer *, doublecomplex *, integer *, doublecomplex *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, doublecomplex *
	    , doublecomplex *, doublecomplex *, doublecomplex *, integer *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, doublecomplex *
	    , doublecomplex *, doublecomplex *, doublecomplex *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, integer *, 
	    doublereal *, logical *, doublereal *, integer *), zchkgk_(
	    integer *, integer *), zchkgl_(integer *, integer *), zckglm_(
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    doublereal *, integer *, doublecomplex *, doublecomplex *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, doublecomplex *
	    , doublereal *, integer *, integer *, integer *), zerrbd_(char *, 
	    integer *);
    static integer mxbval[20];
    extern /* Subroutine */ int zchkhs_(integer *, integer *, integer *, 
	    logical *, integer *, doublereal *, integer *, doublecomplex *, 
	    integer *, doublecomplex *, doublecomplex *, doublecomplex *, 
	    doublecomplex *, integer *, doublecomplex *, doublecomplex *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, doublecomplex *
	    , doublecomplex *, doublecomplex *, doublecomplex *, 
	    doublecomplex *, doublecomplex *, integer *, doublereal *, 
	    integer *, logical *, doublereal *, integer *), zcklse_(integer *,
	     integer *, integer *, integer *, integer *, integer *, 
	    doublereal *, integer *, doublecomplex *, doublecomplex *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, doublecomplex *
	    , doublereal *, integer *, integer *, integer *), zdrvbd_(integer 
	    *, integer *, integer *, integer *, logical *, integer *, 
	    doublereal *, doublecomplex *, integer *, doublecomplex *, 
	    integer *, doublecomplex *, integer *, doublecomplex *, 
	    doublecomplex *, doublecomplex *, doublereal *, doublereal *, 
	    doublereal *, doublecomplex *, integer *, doublereal *, integer *,
	     integer *, integer *);
    static logical tstdif;
    static doublereal thresh;
    extern /* Subroutine */ int xlaenv_(integer *, integer *);
    static logical tstchk;
    static integer nparms;
    extern /* Subroutine */ int zckgqr_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, doublereal 
	    *, integer *, doublecomplex *, doublecomplex *, doublecomplex *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, doublecomplex *
	    , doublecomplex *, doublecomplex *, doublecomplex *, 
	    doublecomplex *, doublecomplex *, doublereal *, integer *, 
	    integer *, integer *), zdrges_(integer *, integer *, integer *, 
	    logical *, integer *, doublereal *, integer *, doublecomplex *, 
	    integer *, doublecomplex *, doublecomplex *, doublecomplex *, 
	    doublecomplex *, integer *, doublecomplex *, doublecomplex *, 
	    doublecomplex *, doublecomplex *, integer *, doublereal *, 
	    doublereal *, logical *, integer *);
    static logical dotype[30], logwrk[132];
    static doublereal thrshn;
    extern /* Subroutine */ int zchkst_(integer *, integer *, integer *, 
	    logical *, integer *, doublereal *, integer *, doublecomplex *, 
	    integer *, doublecomplex *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublecomplex *, integer *, doublecomplex *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, doublecomplex *
	    , integer *, doublereal *, integer *, integer *, integer *, 
	    doublereal *, integer *), zckgsv_(integer *, integer *, integer *,
	     integer *, integer *, integer *, doublereal *, integer *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, doublecomplex *
	    , doublecomplex *, doublecomplex *, doublecomplex *, doublereal *,
	     doublereal *, doublecomplex *, integer *, doublecomplex *, 
	    doublereal *, integer *, integer *, integer *), zdrgev_(integer *,
	     integer *, integer *, logical *, integer *, doublereal *, 
	    integer *, doublecomplex *, integer *, doublecomplex *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, integer *, 
	    doublecomplex *, doublecomplex *, integer *, doublecomplex *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, doublecomplex *
	    , integer *, doublereal *, doublereal *, integer *), zdrvgg_(
	    integer *, integer *, integer *, logical *, integer *, doublereal 
	    *, doublereal *, integer *, doublecomplex *, integer *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, doublecomplex *
	    , doublecomplex *, doublecomplex *, integer *, doublecomplex *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, doublecomplex *
	    , doublecomplex *, doublecomplex *, doublecomplex *, integer *, 
	    doublereal *, doublereal *, integer *), zerred_(char *, integer *), zdrves_(integer *, integer *, integer *, logical *, 
	    integer *, doublereal *, integer *, doublecomplex *, integer *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, doublecomplex *
	    , doublecomplex *, integer *, doublereal *, doublecomplex *, 
	    integer *, doublereal *, integer *, logical *, integer *);
    static doublereal result[500];
    extern /* Subroutine */ int zdrvsg_(integer *, integer *, integer *, 
	    logical *, integer *, doublereal *, integer *, doublecomplex *, 
	    integer *, doublecomplex *, integer *, doublereal *, 
	    doublecomplex *, integer *, doublecomplex *, doublecomplex *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, integer *, 
	    doublereal *, integer *, integer *, integer *, doublereal *, 
	    integer *), zdrvev_(integer *, integer *, integer *, logical *, 
	    integer *, doublereal *, integer *, doublecomplex *, integer *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, doublecomplex *
	    , integer *, doublecomplex *, integer *, doublecomplex *, integer 
	    *, doublereal *, doublecomplex *, integer *, doublereal *, 
	    integer *, integer *), zdrgsx_(integer *, integer *, doublereal *,
	     integer *, integer *, doublecomplex *, integer *, doublecomplex *
	    , doublecomplex *, doublecomplex *, doublecomplex *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, doublecomplex *
	    , integer *, doublereal *, doublecomplex *, integer *, doublereal 
	    *, integer *, integer *, logical *, integer *);
    static integer maxtyp;
    static logical tsterr;
    static integer ntypes;
    static logical tstdrv;
    extern /* Subroutine */ int zdrgvx_(integer *, doublereal *, integer *, 
	    integer *, doublecomplex *, integer *, doublecomplex *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, doublecomplex *
	    , doublecomplex *, doublecomplex *, integer *, integer *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublecomplex *, integer *, 
	    doublereal *, integer *, integer *, doublereal *, logical *, 
	    integer *), zerrgg_(char *, integer *), zerrhs_(char *, 
	    integer *), zerrst_(char *, integer *), zdrvst_(
	    integer *, integer *, integer *, logical *, integer *, doublereal 
	    *, integer *, doublecomplex *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublecomplex *, integer *, doublecomplex *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, integer *, 
	    doublereal *, integer *, integer *, integer *, doublereal *, 
	    integer *), zdrvsx_(integer *, integer *, integer *, logical *, 
	    integer *, doublereal *, integer *, integer *, doublecomplex *, 
	    integer *, doublecomplex *, doublecomplex *, doublecomplex *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, integer *, 
	    doublecomplex *, doublereal *, doublecomplex *, integer *, 
	    doublereal *, logical *, integer *), zdrvvx_(integer *, integer *,
	     integer *, logical *, integer *, doublereal *, integer *, 
	    integer *, doublecomplex *, integer *, doublecomplex *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublecomplex *, integer *, doublereal *, integer *)
	    ;
    static logical zbb, glm, nep, lse, zbk, zbl, sep, gqr, zgg, zgk, zgl, svd,
	     zhb, gsv;
    static doublereal eps;
    static logical zes, zgs, zev, zgv, zgx, zsx, zvx, zxv;

    /* Fortran I/O blocks */
    static cilist io___5 = { 0, 5, 1, "(A80)", 0 };
    static cilist io___30 = { 0, 6, 0, fmt_9987, 0 };
    static cilist io___31 = { 0, 6, 0, fmt_9986, 0 };
    static cilist io___32 = { 0, 6, 0, fmt_9985, 0 };
    static cilist io___33 = { 0, 6, 0, fmt_9979, 0 };
    static cilist io___34 = { 0, 6, 0, fmt_9978, 0 };
    static cilist io___35 = { 0, 6, 0, fmt_9977, 0 };
    static cilist io___36 = { 0, 6, 0, fmt_9976, 0 };
    static cilist io___37 = { 0, 6, 0, fmt_9975, 0 };
    static cilist io___38 = { 0, 6, 0, fmt_9964, 0 };
    static cilist io___39 = { 0, 6, 0, fmt_9965, 0 };
    static cilist io___40 = { 0, 6, 0, fmt_9963, 0 };
    static cilist io___41 = { 0, 6, 0, fmt_9962, 0 };
    static cilist io___42 = { 0, 6, 0, fmt_9974, 0 };
    static cilist io___43 = { 0, 6, 0, fmt_9967, 0 };
    static cilist io___44 = { 0, 6, 0, fmt_9971, 0 };
    static cilist io___45 = { 0, 6, 0, fmt_9970, 0 };
    static cilist io___46 = { 0, 6, 0, fmt_9969, 0 };
    static cilist io___47 = { 0, 6, 0, fmt_9968, 0 };
    static cilist io___48 = { 0, 5, 0, 0, 0 };
    static cilist io___51 = { 0, 6, 0, fmt_9992, 0 };
    static cilist io___52 = { 0, 6, 0, fmt_9972, 0 };
    static cilist io___53 = { 0, 6, 0, fmt_9984, 0 };
    static cilist io___54 = { 0, 5, 0, 0, 0 };
    static cilist io___56 = { 0, 6, 0, fmt_9989, 0 };
    static cilist io___57 = { 0, 6, 0, fmt_9988, 0 };
    static cilist io___58 = { 0, 5, 0, 0, 0 };
    static cilist io___62 = { 0, 6, 0, fmt_9989, 0 };
    static cilist io___63 = { 0, 6, 0, fmt_9988, 0 };
    static cilist io___64 = { 0, 6, 0, fmt_9983, 0 };
    static cilist io___65 = { 0, 5, 0, 0, 0 };
    static cilist io___67 = { 0, 6, 0, fmt_9989, 0 };
    static cilist io___68 = { 0, 6, 0, fmt_9988, 0 };
    static cilist io___69 = { 0, 6, 0, fmt_9983, 0 };
    static cilist io___70 = { 0, 5, 0, 0, 0 };
    static cilist io___72 = { 0, 6, 0, fmt_9989, 0 };
    static cilist io___73 = { 0, 6, 0, fmt_9988, 0 };
    static cilist io___74 = { 0, 6, 0, fmt_9983, 0 };
    static cilist io___75 = { 0, 6, 0, fmt_9983, 0 };
    static cilist io___76 = { 0, 5, 0, 0, 0 };
    static cilist io___78 = { 0, 5, 0, 0, 0 };
    static cilist io___80 = { 0, 6, 0, fmt_9989, 0 };
    static cilist io___81 = { 0, 6, 0, fmt_9988, 0 };
    static cilist io___82 = { 0, 6, 0, fmt_9983, 0 };
    static cilist io___83 = { 0, 5, 0, 0, 0 };
    static cilist io___89 = { 0, 6, 0, fmt_9989, 0 };
    static cilist io___90 = { 0, 6, 0, fmt_9989, 0 };
    static cilist io___91 = { 0, 6, 0, fmt_9989, 0 };
    static cilist io___92 = { 0, 6, 0, fmt_9989, 0 };
    static cilist io___93 = { 0, 6, 0, fmt_9989, 0 };
    static cilist io___94 = { 0, 6, 0, fmt_9983, 0 };
    static cilist io___95 = { 0, 6, 0, fmt_9983, 0 };
    static cilist io___96 = { 0, 6, 0, fmt_9983, 0 };
    static cilist io___97 = { 0, 6, 0, fmt_9983, 0 };
    static cilist io___98 = { 0, 6, 0, fmt_9983, 0 };
    static cilist io___99 = { 0, 5, 0, 0, 0 };
    static cilist io___101 = { 0, 6, 0, fmt_9989, 0 };
    static cilist io___102 = { 0, 6, 0, fmt_9988, 0 };
    static cilist io___103 = { 0, 5, 0, 0, 0 };
    static cilist io___104 = { 0, 6, 0, fmt_9989, 0 };
    static cilist io___105 = { 0, 6, 0, fmt_9988, 0 };
    static cilist io___106 = { 0, 6, 0, fmt_9983, 0 };
    static cilist io___107 = { 0, 5, 0, 0, 0 };
    static cilist io___108 = { 0, 6, 0, fmt_9989, 0 };
    static cilist io___109 = { 0, 6, 0, fmt_9988, 0 };
    static cilist io___110 = { 0, 6, 0, fmt_9983, 0 };
    static cilist io___111 = { 0, 5, 0, 0, 0 };
    static cilist io___112 = { 0, 6, 0, fmt_9989, 0 };
    static cilist io___113 = { 0, 6, 0, fmt_9988, 0 };
    static cilist io___114 = { 0, 6, 0, fmt_9983, 0 };
    static cilist io___115 = { 0, 5, 0, 0, 0 };
    static cilist io___116 = { 0, 6, 0, fmt_9989, 0 };
    static cilist io___117 = { 0, 6, 0, fmt_9988, 0 };
    static cilist io___118 = { 0, 6, 0, fmt_9983, 0 };
    static cilist io___119 = { 0, 5, 0, 0, 0 };
    static cilist io___120 = { 0, 6, 0, fmt_9989, 0 };
    static cilist io___121 = { 0, 6, 0, fmt_9988, 0 };
    static cilist io___122 = { 0, 6, 0, fmt_9983, 0 };
    static cilist io___123 = { 0, 5, 0, 0, 0 };
    static cilist io___125 = { 0, 6, 0, fmt_9989, 0 };
    static cilist io___126 = { 0, 6, 0, fmt_9988, 0 };
    static cilist io___127 = { 0, 6, 0, fmt_9983, 0 };
    static cilist io___128 = { 0, 6, 0, 0, 0 };
    static cilist io___130 = { 0, 6, 0, fmt_9981, 0 };
    static cilist io___131 = { 0, 6, 0, fmt_9981, 0 };
    static cilist io___132 = { 0, 6, 0, fmt_9981, 0 };
    static cilist io___133 = { 0, 5, 0, 0, 0 };
    static cilist io___134 = { 0, 6, 0, fmt_9982, 0 };
    static cilist io___135 = { 0, 5, 0, 0, 0 };
    static cilist io___137 = { 0, 5, 0, 0, 0 };
    static cilist io___139 = { 0, 5, 0, 0, 0 };
    static cilist io___140 = { 0, 5, 0, 0, 0 };
    static cilist io___142 = { 0, 5, 0, 0, 0 };
    static cilist io___144 = { 0, 6, 0, fmt_9999, 0 };
    static cilist io___145 = { 0, 5, 1, "(A80)", 0 };
    static cilist io___154 = { 0, 6, 0, fmt_9991, 0 };
    static cilist io___155 = { 0, 6, 0, fmt_9990, 0 };
    static cilist io___158 = { 0, 6, 0, fmt_9998, 0 };
    static cilist io___167 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___168 = { 0, 6, 0, fmt_9997, 0 };
    static cilist io___170 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___171 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___172 = { 0, 6, 0, fmt_9997, 0 };
    static cilist io___173 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___175 = { 0, 6, 0, fmt_9995, 0 };
    static cilist io___176 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___177 = { 0, 6, 0, fmt_9990, 0 };
    static cilist io___178 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___179 = { 0, 6, 0, fmt_9973, 0 };
    static cilist io___180 = { 0, 6, 0, fmt_9990, 0 };
    static cilist io___181 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___182 = { 0, 6, 0, fmt_9973, 0 };
    static cilist io___183 = { 0, 6, 0, fmt_9990, 0 };
    static cilist io___184 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___185 = { 0, 6, 0, fmt_9973, 0 };
    static cilist io___186 = { 0, 6, 0, fmt_9990, 0 };
    static cilist io___187 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___188 = { 0, 6, 0, fmt_9973, 0 };
    static cilist io___189 = { 0, 6, 0, fmt_9996, 0 };
    static cilist io___192 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___193 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___194 = { 0, 6, 0, fmt_9990, 0 };
    static cilist io___195 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___196 = { 0, 6, 0, fmt_9973, 0 };
    static cilist io___197 = { 0, 6, 0, fmt_9990, 0 };
    static cilist io___200 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___201 = { 0, 6, 0, fmt_9973, 0 };
    static cilist io___202 = { 0, 6, 0, fmt_9990, 0 };
    static cilist io___203 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___204 = { 0, 6, 0, fmt_9973, 0 };
    static cilist io___205 = { 0, 6, 0, fmt_9990, 0 };
    static cilist io___206 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___207 = { 0, 6, 0, fmt_9973, 0 };
    static cilist io___208 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___209 = { 0, 6, 0, fmt_9966, 0 };
    static cilist io___210 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___213 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___216 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___219 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___220 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___221 = { 0, 6, 0, 0, 0 };
    static cilist io___222 = { 0, 6, 0, 0, 0 };
    static cilist io___223 = { 0, 6, 0, fmt_9992, 0 };
    static cilist io___224 = { 0, 6, 0, fmt_9994, 0 };
    static cilist io___226 = { 0, 6, 0, fmt_9993, 0 };



#define a_subscr(a_1,a_2) (a_2)*17424 + a_1 - 17425
#define a_ref(a_1,a_2) a[a_subscr(a_1,a_2)]
#define b_subscr(a_1,a_2) (a_2)*17424 + a_1 - 17425
#define b_ref(a_1,a_2) b[b_subscr(a_1,a_2)]
#define dc_subscr(a_1,a_2) (a_2)*132 + a_1 - 133
#define dc_ref(a_1,a_2) dc[dc_subscr(a_1,a_2)]
#define dr_ref(a_1,a_2) dr[(a_2)*132 + a_1 - 133]


/*  -- LAPACK test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   

    Purpose   
    =======   

    ZCHKEE tests the COMPLEX*16 LAPACK subroutines for the matrix   
    eigenvalue problem.  The test paths in this version are   

    NEP (Nonsymmetric Eigenvalue Problem):   
        Test ZGEHRD, ZUNGHR, ZHSEQR, ZTREVC, ZHSEIN, and ZUNMHR   

    SEP (Hermitian Eigenvalue Problem):   
        Test ZHETRD, ZUNGTR, ZSTEQR, ZSTERF, ZSTEIN, ZSTEDC,   
        and drivers ZHEEV(X), ZHBEV(X), ZHPEV(X),   
                    ZHEEVD,   ZHBEVD,   ZHPEVD   

    SVD (Singular Value Decomposition):   
        Test ZGEBRD, ZUNGBR, and ZBDSQR   
        and the drivers ZGESVD, ZGESDD   

    ZEV (Nonsymmetric Eigenvalue/eigenvector Driver):   
        Test ZGEEV   

    ZES (Nonsymmetric Schur form Driver):   
        Test ZGEES   

    ZVX (Nonsymmetric Eigenvalue/eigenvector Expert Driver):   
        Test ZGEEVX   

    ZSX (Nonsymmetric Schur form Expert Driver):   
        Test ZGEESX   

    ZGG (Generalized Nonsymmetric Eigenvalue Problem):   
        Test ZGGHRD, ZGGBAL, ZGGBAK, ZHGEQZ, and ZTGEVC   
        and the driver routines ZGEGS and ZGEGV   

    ZGS (Generalized Nonsymmetric Schur form Driver):   
        Test ZGGES   

    ZGV (Generalized Nonsymmetric Eigenvalue/eigenvector Driver):   
        Test ZGGEV   

    ZGX (Generalized Nonsymmetric Schur form Expert Driver):   
        Test ZGGESX   

    ZXV (Generalized Nonsymmetric Eigenvalue/eigenvector Expert Driver):   
        Test ZGGEVX   

    ZSG (Hermitian Generalized Eigenvalue Problem):   
        Test ZHEGST, ZHEGV, ZHEGVD, ZHEGVX, ZHPGST, ZHPGV, ZHPGVD,   
        ZHPGVX, ZHBGST, ZHBGV, ZHBGVD, and ZHBGVX   

    ZHB (Hermitian Band Eigenvalue Problem):   
        Test ZHBTRD   

    ZBB (Band Singular Value Decomposition):   
        Test ZGBBRD   

    ZEC (Eigencondition estimation):   
        Test ZTRSYL, ZTREXC, ZTRSNA, and ZTRSEN   

    ZBL (Balancing a general matrix)   
        Test ZGEBAL   

    ZBK (Back transformation on a balanced matrix)   
        Test ZGEBAK   

    ZGL (Balancing a matrix pair)   
        Test ZGGBAL   

    ZGK (Back transformation on a matrix pair)   
        Test ZGGBAK   

    GLM (Generalized Linear Regression Model):   
        Tests ZGGGLM   

    GQR (Generalized QR and RQ factorizations):   
        Tests ZGGQRF and ZGGRQF   

    GSV (Generalized Singular Value Decomposition):   
        Tests ZGGSVD, ZGGSVP, ZTGSJA, ZLAGS2, ZLAPLL, and ZLAPMT   

    LSE (Constrained Linear Least Squares):   
        Tests ZGGLSE   

    Each test path has a different set of inputs, but the data sets for   
    the driver routines xEV, xES, xVX, and xSX can be concatenated in a   
    single input file.  The first line of input should contain one of the   
    3-character path names in columns 1-3.  The number of remaining lines   
    depends on what is found on the first line.   

    The number of matrix types used in testing is often controllable from   
    the input file.  The number of matrix types for each path, and the   
    test routine that describes them, is as follows:   

    Path name(s)  Types    Test routine   

    ZHS or NEP      21     ZCHKHS   
    ZST or SEP      21     ZCHKST (routines)   
                    18     ZDRVST (drivers)   
    ZBD or SVD      16     ZCHKBD (routines)   
                     5     ZDRVBD (drivers)   
    ZEV             21     ZDRVEV   
    ZES             21     ZDRVES   
    ZVX             21     ZDRVVX   
    ZSX             21     ZDRVSX   
    ZGG             26     ZCHKGG (routines)   
                    26     ZDRVGG (drivers)   
    ZGS             26     ZDRGES   
    ZGX              5     ZDRGSX   
    ZGV             26     ZDRGEV   
    ZXV              2     ZDRGVX   
    ZSG             21     ZDRVSG   
    ZHB             15     ZCHKHB   
    ZBB             15     ZCHKBB   
    ZEC              -     ZCHKEC   
    ZBL              -     ZCHKBL   
    ZBK              -     ZCHKBK   
    ZGL              -     ZCHKGL   
    ZGK              -     ZCHKGK   
    GLM              8     ZCKGLM   
    GQR              8     ZCKGQR   
    GSV              8     ZCKGSV   
    LSE              8     ZCKLSE   

   -----------------------------------------------------------------------   

    NEP input file:   

    line 2:  NN, INTEGER   
             Number of values of N.   

    line 3:  NVAL, INTEGER array, dimension (NN)   
             The values for the matrix dimension N.   

    line 4:  NPARMS, INTEGER   
             Number of values of the parameters NB, NBMIN, NX, NS, and   
             MAXB.   

    line 5:  NBVAL, INTEGER array, dimension (NPARMS)   
             The values for the blocksize NB.   

    line 6:  NBMIN, INTEGER array, dimension (NPARMS)   
             The values for the minimum blocksize NBMIN.   

    line 7:  NXVAL, INTEGER array, dimension (NPARMS)   
             The values for the crossover point NX.   

    line 8:  NSVAL, INTEGER array, dimension (NPARMS)   
             The values for the number of shifts.   

    line 9:  MXBVAL, INTEGER array, dimension (NPARMS)   
             The values for MAXB, used in determining minimum blocksize.   

    line 10: THRESH   
             Threshold value for the test ratios.  Information will be   
             printed about each test for which the test ratio is greater   
             than or equal to the threshold.  To have all of the test   
             ratios printed, use THRESH = 0.0 .   

    line 11: NEWSD, INTEGER   
             A code indicating how to set the random number seed.   
             = 0:  Set the seed to a default value before each run   
             = 1:  Initialize the seed to a default value only before the   
                   first run   
             = 2:  Like 1, but use the seed values on the next line   

    If line 11 was 2:   

    line 12: INTEGER array, dimension (4)   
             Four integer values for the random number seed.   

    lines 12-EOF:  The remaining lines occur in sets of 1 or 2 and allow   
             the user to specify the matrix types.  Each line contains   
             a 3-character path name in columns 1-3, and the number   
             of matrix types must be the first nonblank item in columns   
             4-80.  If the number of matrix types is at least 1 but is   
             less than the maximum number of possible types, a second   
             line will be read to get the numbers of the matrix types to   
             be used.  For example,   
    NEP 21   
             requests all of the matrix types for the nonsymmetric   
             eigenvalue problem, while   
    NEP  4   
    9 10 11 12   
             requests only matrices of type 9, 10, 11, and 12.   

             The valid 3-character path names are 'NEP' or 'ZHS' for the   
             nonsymmetric eigenvalue routines.   

   -----------------------------------------------------------------------   

    SEP or ZSG input file:   

    line 2:  NN, INTEGER   
             Number of values of N.   

    line 3:  NVAL, INTEGER array, dimension (NN)   
             The values for the matrix dimension N.   

    line 4:  NPARMS, INTEGER   
             Number of values of the parameters NB, NBMIN, and NX.   

    line 5:  NBVAL, INTEGER array, dimension (NPARMS)   
             The values for the blocksize NB.   

    line 6:  NBMIN, INTEGER array, dimension (NPARMS)   
             The values for the minimum blocksize NBMIN.   

    line 7:  NXVAL, INTEGER array, dimension (NPARMS)   
             The values for the crossover point NX.   

    line 8:  THRESH   
             Threshold value for the test ratios.  Information will be   
             printed about each test for which the test ratio is greater   
             than or equal to the threshold.   

    line 9:  TSTCHK, LOGICAL   
             Flag indicating whether or not to test the LAPACK routines.   

    line 10: TSTDRV, LOGICAL   
             Flag indicating whether or not to test the driver routines.   

    line 11: TSTERR, LOGICAL   
             Flag indicating whether or not to test the error exits for   
             the LAPACK routines and driver routines.   

    line 12: NEWSD, INTEGER   
             A code indicating how to set the random number seed.   
             = 0:  Set the seed to a default value before each run   
             = 1:  Initialize the seed to a default value only before the   
                   first run   
             = 2:  Like 1, but use the seed values on the next line   

    If line 12 was 2:   

    line 13: INTEGER array, dimension (4)   
             Four integer values for the random number seed.   

    lines 13-EOF:  Lines specifying matrix types, as for NEP.   
             The valid 3-character path names are 'SEP' or 'ZST' for the   
             Hermitian eigenvalue routines and driver routines, and   
             'ZSG' for the routines for the Hermitian generalized   
             eigenvalue problem.   

   -----------------------------------------------------------------------   

    SVD input file:   

    line 2:  NN, INTEGER   
             Number of values of M and N.   

    line 3:  MVAL, INTEGER array, dimension (NN)   
             The values for the matrix row dimension M.   

    line 4:  NVAL, INTEGER array, dimension (NN)   
             The values for the matrix column dimension N.   

    line 5:  NPARMS, INTEGER   
             Number of values of the parameter NB, NBMIN, NX, and NRHS.   

    line 6:  NBVAL, INTEGER array, dimension (NPARMS)   
             The values for the blocksize NB.   

    line 7:  NBMIN, INTEGER array, dimension (NPARMS)   
             The values for the minimum blocksize NBMIN.   

    line 8:  NXVAL, INTEGER array, dimension (NPARMS)   
             The values for the crossover point NX.   

    line 9:  NSVAL, INTEGER array, dimension (NPARMS)   
             The values for the number of right hand sides NRHS.   

    line 10: THRESH   
             Threshold value for the test ratios.  Information will be   
             printed about each test for which the test ratio is greater   
             than or equal to the threshold.   

    line 11: TSTCHK, LOGICAL   
             Flag indicating whether or not to test the LAPACK routines.   

    line 12: TSTDRV, LOGICAL   
             Flag indicating whether or not to test the driver routines.   

    line 13: TSTERR, LOGICAL   
             Flag indicating whether or not to test the error exits for   
             the LAPACK routines and driver routines.   

    line 14: NEWSD, INTEGER   
             A code indicating how to set the random number seed.   
             = 0:  Set the seed to a default value before each run   
             = 1:  Initialize the seed to a default value only before the   
                   first run   
             = 2:  Like 1, but use the seed values on the next line   

    If line 14 was 2:   

    line 15: INTEGER array, dimension (4)   
             Four integer values for the random number seed.   

    lines 15-EOF:  Lines specifying matrix types, as for NEP.   
             The 3-character path names are 'SVD' or 'ZBD' for both the   
             SVD routines and the SVD driver routines.   

   -----------------------------------------------------------------------   

    ZEV and ZES data files:   

    line 1:  'ZEV' or 'ZES' in columns 1 to 3.   

    line 2:  NSIZES, INTEGER   
             Number of sizes of matrices to use. Should be at least 0   
             and at most 20. If NSIZES = 0, no testing is done   
             (although the remaining  3 lines are still read).   

    line 3:  NN, INTEGER array, dimension(NSIZES)   
             Dimensions of matrices to be tested.   

    line 4:  NB, NBMIN, NX, NS, NBCOL, INTEGERs   
             These integer parameters determine how blocking is done   
             (see ILAENV for details)   
             NB     : block size   
             NBMIN  : minimum block size   
             NX     : minimum dimension for blocking   
             NS     : number of shifts in xHSEQR   
             NBCOL  : minimum column dimension for blocking   

    line 5:  THRESH, REAL   
             The test threshold against which computed residuals are   
             compared. Should generally be in the range from 10. to 20.   
             If it is 0., all test case data will be printed.   

    line 6:  NEWSD, INTEGER   
             A code indicating how to set the random number seed.   
             = 0:  Set the seed to a default value before each run   
             = 1:  Initialize the seed to a default value only before the   
                   first run   
             = 2:  Like 1, but use the seed values on the next line   

    If line 6 was 2:   

    line 7:  INTEGER array, dimension (4)   
             Four integer values for the random number seed.   

    lines 8 and following:  Lines specifying matrix types, as for NEP.   
             The 3-character path name is 'ZEV' to test CGEEV, or   
             'ZES' to test CGEES.   

   -----------------------------------------------------------------------   

    The ZVX data has two parts. The first part is identical to ZEV,   
    and the second part consists of test matrices with precomputed   
    solutions.   

    line 1:  'ZVX' in columns 1-3.   

    line 2:  NSIZES, INTEGER   
             If NSIZES = 0, no testing of randomly generated examples   
             is done, but any precomputed examples are tested.   

    line 3:  NN, INTEGER array, dimension(NSIZES)   

    line 4:  NB, NBMIN, NX, NS, NBCOL, INTEGERs   

    line 5:  THRESH, REAL   

    line 6:  NEWSD, INTEGER   

    If line 6 was 2:   

    line 7:  INTEGER array, dimension (4)   

    lines 8 and following: The first line contains 'ZVX' in columns 1-3   
             followed by the number of matrix types, possibly with   
             a second line to specify certain matrix types.   
             If the number of matrix types = 0, no testing of randomly   
             generated examples is done, but any precomputed examples   
             are tested.   

    remaining lines : Each matrix is stored on 1+N+N**2 lines, where N is   
             its dimension. The first line contains the dimension N and   
             ISRT (two integers). ISRT indicates whether the last N lines   
             are sorted by increasing real part of the eigenvalue   
             (ISRT=0) or by increasing imaginary part (ISRT=1). The next   
             N**2 lines contain the matrix rowwise, one entry per line.   
             The last N lines correspond to each eigenvalue. Each of   
             these last N lines contains 4 real values: the real part of   
             the eigenvalues, the imaginary part of the eigenvalue, the   
             reciprocal condition number of the eigenvalues, and the   
             reciprocal condition number of the vector eigenvector. The   
             end of data is indicated by dimension N=0. Even if no data   
             is to be tested, there must be at least one line containing   
             N=0.   

   -----------------------------------------------------------------------   

    The ZSX data is like ZVX. The first part is identical to ZEV, and the   
    second part consists of test matrices with precomputed solutions.   

    line 1:  'ZSX' in columns 1-3.   

    line 2:  NSIZES, INTEGER   
             If NSIZES = 0, no testing of randomly generated examples   
             is done, but any precomputed examples are tested.   

    line 3:  NN, INTEGER array, dimension(NSIZES)   

    line 4:  NB, NBMIN, NX, NS, NBCOL, INTEGERs   

    line 5:  THRESH, REAL   

    line 6:  NEWSD, INTEGER   

    If line 6 was 2:   

    line 7:  INTEGER array, dimension (4)   

    lines 8 and following: The first line contains 'ZSX' in columns 1-3   
             followed by the number of matrix types, possibly with   
             a second line to specify certain matrix types.   
             If the number of matrix types = 0, no testing of randomly   
             generated examples is done, but any precomputed examples   
             are tested.   

    remaining lines : Each matrix is stored on 3+N**2 lines, where N is   
             its dimension. The first line contains the dimension N, the   
             dimension M of an invariant subspace, and ISRT. The second   
             line contains M integers, identifying the eigenvalues in the   
             invariant subspace (by their position in a list of   
             eigenvalues ordered by increasing real part (if ISRT=0) or   
             by increasing imaginary part (if ISRT=1)). The next N**2   
             lines contain the matrix rowwise. The last line contains the   
             reciprocal condition number for the average of the selected   
             eigenvalues, and the reciprocal condition number for the   
             corresponding right invariant subspace. The end of data in   
             indicated by a line containing N=0, M=0, and ISRT = 0.  Even   
             if no data is to be tested, there must be at least one line   
             containing N=0, M=0 and ISRT=0.   

   -----------------------------------------------------------------------   

    ZGG input file:   

    line 2:  NN, INTEGER   
             Number of values of N.   

    line 3:  NVAL, INTEGER array, dimension (NN)   
             The values for the matrix dimension N.   

    line 4:  NPARMS, INTEGER   
             Number of values of the parameters NB, NBMIN, NBCOL, NS, and   
             MAXB.   

    line 5:  NBVAL, INTEGER array, dimension (NPARMS)   
             The values for the blocksize NB.   

    line 6:  NBMIN, INTEGER array, dimension (NPARMS)   
             The values for NBMIN, the minimum row dimension for blocks.   

    line 7:  NSVAL, INTEGER array, dimension (NPARMS)   
             The values for the number of shifts.   

    line 8:  MXBVAL, INTEGER array, dimension (NPARMS)   
             The values for MAXB, used in determining minimum blocksize.   

    line 9:  NBCOL, INTEGER array, dimension (NPARMS)   
             The values for NBCOL, the minimum column dimension for   
             blocks.   

    line 10: THRESH   
             Threshold value for the test ratios.  Information will be   
             printed about each test for which the test ratio is greater   
             than or equal to the threshold.   

    line 11: TSTCHK, LOGICAL   
             Flag indicating whether or not to test the LAPACK routines.   

    line 12: TSTDRV, LOGICAL   
             Flag indicating whether or not to test the driver routines.   

    line 13: TSTERR, LOGICAL   
             Flag indicating whether or not to test the error exits for   
             the LAPACK routines and driver routines.   

    line 14: NEWSD, INTEGER   
             A code indicating how to set the random number seed.   
             = 0:  Set the seed to a default value before each run   
             = 1:  Initialize the seed to a default value only before the   
                   first run   
             = 2:  Like 1, but use the seed values on the next line   

    If line 14 was 2:   

    line 15: INTEGER array, dimension (4)   
             Four integer values for the random number seed.   

    lines 16-EOF:  Lines specifying matrix types, as for NEP.   
             The 3-character path name is 'ZGG' for the generalized   
             eigenvalue problem routines and driver routines.   

   -----------------------------------------------------------------------   

    ZGS and ZGV input files:   

    line 1:  'ZGS' or 'ZGV' in columns 1 to 3.   

    line 2:  NN, INTEGER   
             Number of values of N.   

    line 3:  NVAL, INTEGER array, dimension(NN)   
             Dimensions of matrices to be tested.   

    line 4:  NB, NBMIN, NX, NS, NBCOL, INTEGERs   
             These integer parameters determine how blocking is done   
             (see ILAENV for details)   
             NB     : block size   
             NBMIN  : minimum block size   
             NX     : minimum dimension for blocking   
             NS     : number of shifts in xHGEQR   
             NBCOL  : minimum column dimension for blocking   

    line 5:  THRESH, REAL   
             The test threshold against which computed residuals are   
             compared. Should generally be in the range from 10. to 20.   
             If it is 0., all test case data will be printed.   

    line 6:  TSTERR, LOGICAL   
             Flag indicating whether or not to test the error exits.   

    line 7:  NEWSD, INTEGER   
             A code indicating how to set the random number seed.   
             = 0:  Set the seed to a default value before each run   
             = 1:  Initialize the seed to a default value only before the   
                   first run   
             = 2:  Like 1, but use the seed values on the next line   

    If line 17 was 2:   

    line 7:  INTEGER array, dimension (4)   
             Four integer values for the random number seed.   

    lines 7-EOF:  Lines specifying matrix types, as for NEP.   
             The 3-character path name is 'ZGS' for the generalized   
             eigenvalue problem routines and driver routines.   

   -----------------------------------------------------------------------   

    ZGX input file:   
    line 1:  'ZGX' in columns 1 to 3.   

    line 2:  N, INTEGER   
             Value of N.   

    line 3:  NB, NBMIN, NX, NS, NBCOL, INTEGERs   
             These integer parameters determine how blocking is done   
             (see ILAENV for details)   
             NB     : block size   
             NBMIN  : minimum block size   
             NX     : minimum dimension for blocking   
             NS     : number of shifts in xHGEQR   
             NBCOL  : minimum column dimension for blocking   

    line 4:  THRESH, REAL   
             The test threshold against which computed residuals are   
             compared. Should generally be in the range from 10. to 20.   
             Information will be printed about each test for which the   
             test ratio is greater than or equal to the threshold.   

    line 5:  TSTERR, LOGICAL   
             Flag indicating whether or not to test the error exits for   
             the LAPACK routines and driver routines.   

    line 6:  NEWSD, INTEGER   
             A code indicating how to set the random number seed.   
             = 0:  Set the seed to a default value before each run   
             = 1:  Initialize the seed to a default value only before the   
                   first run   
             = 2:  Like 1, but use the seed values on the next line   

    If line 6 was 2:   

    line 7: INTEGER array, dimension (4)   
             Four integer values for the random number seed.   

    If line 2 was 0:   

    line 7-EOF: Precomputed examples are tested.   

    remaining lines : Each example is stored on 3+2*N*N lines, where N is   
             its dimension. The first line contains the dimension (a   
             single integer).  The next line contains an integer k such   
             that only the last k eigenvalues will be selected and appear   
             in the leading diagonal blocks of $A$ and $B$. The next N*N   
             lines contain the matrix A, one element per line. The next N*N   
             lines contain the matrix B. The last line contains the   
             reciprocal of the eigenvalue cluster condition number and the   
             reciprocal of the deflating subspace (associated with the   
             selected eigencluster) condition number.  The end of data is   
             indicated by dimension N=0.  Even if no data is to be tested,   
             there must be at least one line containing N=0.   

   -----------------------------------------------------------------------   

    ZXV input files:   
    line 1:  'ZXV' in columns 1 to 3.   

    line 2:  N, INTEGER   
             Value of N.   

    line 3:  NB, NBMIN, NX, NS, NBCOL, INTEGERs   
             These integer parameters determine how blocking is done   
             (see ILAENV for details)   
             NB     : block size   
             NBMIN  : minimum block size   
             NX     : minimum dimension for blocking   
             NS     : number of shifts in xHGEQR   
             NBCOL  : minimum column dimension for blocking   

    line 4:  THRESH, REAL   
             The test threshold against which computed residuals are   
             compared. Should generally be in the range from 10. to 20.   
             Information will be printed about each test for which the   
             test ratio is greater than or equal to the threshold.   

    line 5:  TSTERR, LOGICAL   
             Flag indicating whether or not to test the error exits for   
             the LAPACK routines and driver routines.   

    line 6:  NEWSD, INTEGER   
             A code indicating how to set the random number seed.   
             = 0:  Set the seed to a default value before each run   
             = 1:  Initialize the seed to a default value only before the   
                   first run   
             = 2:  Like 1, but use the seed values on the next line   

    If line 6 was 2:   

    line 7: INTEGER array, dimension (4)   
             Four integer values for the random number seed.   

    If line 2 was 0:   

    line 7-EOF: Precomputed examples are tested.   

    remaining lines : Each example is stored on 3+2*N*N lines, where N is   
             its dimension. The first line contains the dimension (a   
             single integer). The next N*N lines contain the matrix A, one   
             element per line. The next N*N lines contain the matrix B.   
             The next line contains the reciprocals of the eigenvalue   
             condition numbers.  The last line contains the reciprocals of   
             the eigenvector condition numbers.  The end of data is   
             indicated by dimension N=0.  Even if no data is to be tested,   
             there must be at least one line containing N=0.   

   -----------------------------------------------------------------------   

    ZHB input file:   

    line 2:  NN, INTEGER   
             Number of values of N.   

    line 3:  NVAL, INTEGER array, dimension (NN)   
             The values for the matrix dimension N.   

    line 4:  NK, INTEGER   
             Number of values of K.   

    line 5:  KVAL, INTEGER array, dimension (NK)   
             The values for the matrix dimension K.   

    line 6:  THRESH   
             Threshold value for the test ratios.  Information will be   
             printed about each test for which the test ratio is greater   
             than or equal to the threshold.   

    line 7:  NEWSD, INTEGER   
             A code indicating how to set the random number seed.   
             = 0:  Set the seed to a default value before each run   
             = 1:  Initialize the seed to a default value only before the   
                   first run   
             = 2:  Like 1, but use the seed values on the next line   

    If line 7 was 2:   

    line 8:  INTEGER array, dimension (4)   
             Four integer values for the random number seed.   

    lines 8-EOF:  Lines specifying matrix types, as for NEP.   
             The 3-character path name is 'ZHB'.   

   -----------------------------------------------------------------------   

    ZBB input file:   

    line 2:  NN, INTEGER   
             Number of values of M and N.   

    line 3:  MVAL, INTEGER array, dimension (NN)   
             The values for the matrix row dimension M.   

    line 4:  NVAL, INTEGER array, dimension (NN)   
             The values for the matrix column dimension N.   

    line 4:  NK, INTEGER   
             Number of values of K.   

    line 5:  KVAL, INTEGER array, dimension (NK)   
             The values for the matrix bandwidth K.   

    line 6:  NPARMS, INTEGER   
             Number of values of the parameter NRHS   

    line 7:  NSVAL, INTEGER array, dimension (NPARMS)   
             The values for the number of right hand sides NRHS.   

    line 8:  THRESH   
             Threshold value for the test ratios.  Information will be   
             printed about each test for which the test ratio is greater   
             than or equal to the threshold.   

    line 9:  NEWSD, INTEGER   
             A code indicating how to set the random number seed.   
             = 0:  Set the seed to a default value before each run   
             = 1:  Initialize the seed to a default value only before the   
                   first run   
             = 2:  Like 1, but use the seed values on the next line   

    If line 9 was 2:   

    line 10: INTEGER array, dimension (4)   
             Four integer values for the random number seed.   

    lines 10-EOF:  Lines specifying matrix types, as for SVD.   
             The 3-character path name is 'ZBB'.   

   -----------------------------------------------------------------------   

    ZEC input file:   

    line  2: THRESH, REAL   
             Threshold value for the test ratios.  Information will be   
             printed about each test for which the test ratio is greater   
             than or equal to the threshold.   

    lines  3-EOF:   

    Input for testing the eigencondition routines consists of a set of   
    specially constructed test cases and their solutions.  The data   
    format is not intended to be modified by the user.   

   -----------------------------------------------------------------------   

    ZBL and ZBK input files:   

    line 1:  'ZBL' in columns 1-3 to test CGEBAL, or 'ZBK' in   
             columns 1-3 to test CGEBAK.   

    The remaining lines consist of specially constructed test cases.   

   -----------------------------------------------------------------------   

    ZGL and ZGK input files:   

    line 1:  'ZGL' in columns 1-3 to test ZGGBAL, or 'ZGK' in   
             columns 1-3 to test ZGGBAK.   

    The remaining lines consist of specially constructed test cases.   

   -----------------------------------------------------------------------   

    GLM data file:   

    line 1:  'GLM' in columns 1 to 3.   

    line 2:  NN, INTEGER   
             Number of values of M, P, and N.   

    line 3:  MVAL, INTEGER array, dimension(NN)   
             Values of M (row dimension).   

    line 4:  PVAL, INTEGER array, dimension(NN)   
             Values of P (row dimension).   

    line 5:  NVAL, INTEGER array, dimension(NN)   
             Values of N (column dimension), note M <= N <= M+P.   

    line 6:  THRESH, REAL   
             Threshold value for the test ratios.  Information will be   
             printed about each test for which the test ratio is greater   
             than or equal to the threshold.   

    line 7:  TSTERR, LOGICAL   
             Flag indicating whether or not to test the error exits for   
             the LAPACK routines and driver routines.   

    line 8:  NEWSD, INTEGER   
             A code indicating how to set the random number seed.   
             = 0:  Set the seed to a default value before each run   
             = 1:  Initialize the seed to a default value only before the   
                   first run   
             = 2:  Like 1, but use the seed values on the next line   

    If line 8 was 2:   

    line 9:  INTEGER array, dimension (4)   
             Four integer values for the random number seed.   

    lines 9-EOF:  Lines specifying matrix types, as for NEP.   
             The 3-character path name is 'GLM' for the generalized   
             linear regression model routines.   

   -----------------------------------------------------------------------   

    GQR data file:   

    line 1:  'GQR' in columns 1 to 3.   

    line 2:  NN, INTEGER   
             Number of values of M, P, and N.   

    line 3:  MVAL, INTEGER array, dimension(NN)   
             Values of M.   

    line 4:  PVAL, INTEGER array, dimension(NN)   
             Values of P.   

    line 5:  NVAL, INTEGER array, dimension(NN)   
             Values of N.   

    line 6:  THRESH, REAL   
             Threshold value for the test ratios.  Information will be   
             printed about each test for which the test ratio is greater   
             than or equal to the threshold.   

    line 7:  TSTERR, LOGICAL   
             Flag indicating whether or not to test the error exits for   
             the LAPACK routines and driver routines.   

    line 8:  NEWSD, INTEGER   
             A code indicating how to set the random number seed.   
             = 0:  Set the seed to a default value before each run   
             = 1:  Initialize the seed to a default value only before the   
                   first run   
             = 2:  Like 1, but use the seed values on the next line   

    If line 8 was 2:   

    line 9:  INTEGER array, dimension (4)   
             Four integer values for the random number seed.   

    lines 9-EOF:  Lines specifying matrix types, as for NEP.   
             The 3-character path name is 'GQR' for the generalized   
             QR and RQ routines.   

   -----------------------------------------------------------------------   

    GSV data file:   

    line 1:  'GSV' in columns 1 to 3.   

    line 2:  NN, INTEGER   
             Number of values of M, P, and N.   

    line 3:  MVAL, INTEGER array, dimension(NN)   
             Values of M (row dimension).   

    line 4:  PVAL, INTEGER array, dimension(NN)   
             Values of P (row dimension).   

    line 5:  NVAL, INTEGER array, dimension(NN)   
             Values of N (column dimension).   

    line 6:  THRESH, REAL   
             Threshold value for the test ratios.  Information will be   
             printed about each test for which the test ratio is greater   
             than or equal to the threshold.   

    line 7:  TSTERR, LOGICAL   
             Flag indicating whether or not to test the error exits for   
             the LAPACK routines and driver routines.   

    line 8:  NEWSD, INTEGER   
             A code indicating how to set the random number seed.   
             = 0:  Set the seed to a default value before each run   
             = 1:  Initialize the seed to a default value only before the   
                   first run   
             = 2:  Like 1, but use the seed values on the next line   

    If line 8 was 2:   

    line 9:  INTEGER array, dimension (4)   
             Four integer values for the random number seed.   

    lines 9-EOF:  Lines specifying matrix types, as for NEP.   
             The 3-character path name is 'GSV' for the generalized   
             SVD routines.   

   -----------------------------------------------------------------------   

    LSE data file:   

    line 1:  'LSE' in columns 1 to 3.   

    line 2:  NN, INTEGER   
             Number of values of M, P, and N.   

    line 3:  MVAL, INTEGER array, dimension(NN)   
             Values of M.   

    line 4:  PVAL, INTEGER array, dimension(NN)   
             Values of P.   

    line 5:  NVAL, INTEGER array, dimension(NN)   
             Values of N, note P <= N <= P+M.   

    line 6:  THRESH, REAL   
             Threshold value for the test ratios.  Information will be   
             printed about each test for which the test ratio is greater   
             than or equal to the threshold.   

    line 7:  TSTERR, LOGICAL   
             Flag indicating whether or not to test the error exits for   
             the LAPACK routines and driver routines.   

    line 8:  NEWSD, INTEGER   
             A code indicating how to set the random number seed.   
             = 0:  Set the seed to a default value before each run   
             = 1:  Initialize the seed to a default value only before the   
                   first run   
             = 2:  Like 1, but use the seed values on the next line   

    If line 8 was 2:   

    line 9:  INTEGER array, dimension (4)   
             Four integer values for the random number seed.   

    lines 9-EOF:  Lines specifying matrix types, as for NEP.   
             The 3-character path name is 'GSV' for the generalized   
             SVD routines.   

   -----------------------------------------------------------------------   

    NMAX is currently set to 132 and must be at least 12 for some of the   
    precomputed examples, and LWORK = NMAX*(5*NMAX+20) in the parameter   
    statements below.  For SVD, we assume NRHS may be as big as N.  The   
    parameter NEED is set to 14 to allow for 14 N-by-N matrices for ZGG.   

    ===================================================================== */


    s1 = dsecnd_();
    fatal = FALSE_;
    infoc_1.nunit = 6;

/*     Return to here to read multiple sets of data */

L10:

/*     Read the first line and set the 3-character test path */

    i__1 = s_rsfe(&io___5);
    if (i__1 != 0) {
	goto L380;
    }
    i__1 = do_fio(&c__1, line, (ftnlen)80);
    if (i__1 != 0) {
	goto L380;
    }
    i__1 = e_rsfe();
    if (i__1 != 0) {
	goto L380;
    }
    s_copy(path, line, (ftnlen)3, (ftnlen)3);
    nep = lsamen_(&c__3, path, "NEP") || lsamen_(&c__3, 
	    path, "ZHS");
    sep = lsamen_(&c__3, path, "SEP") || lsamen_(&c__3, 
	    path, "ZST") || lsamen_(&c__3, path, "ZSG");
    svd = lsamen_(&c__3, path, "SVD") || lsamen_(&c__3, 
	    path, "ZBD");
    zev = lsamen_(&c__3, path, "ZEV");
    zes = lsamen_(&c__3, path, "ZES");
    zvx = lsamen_(&c__3, path, "ZVX");
    zsx = lsamen_(&c__3, path, "ZSX");
    zgg = lsamen_(&c__3, path, "ZGG");
    zgs = lsamen_(&c__3, path, "ZGS");
    zgx = lsamen_(&c__3, path, "ZGX");
    zgv = lsamen_(&c__3, path, "ZGV");
    zxv = lsamen_(&c__3, path, "ZXV");
    zhb = lsamen_(&c__3, path, "ZHB");
    zbb = lsamen_(&c__3, path, "ZBB");
    glm = lsamen_(&c__3, path, "GLM");
    gqr = lsamen_(&c__3, path, "GQR") || lsamen_(&c__3, 
	    path, "GRQ");
    gsv = lsamen_(&c__3, path, "GSV");
    lse = lsamen_(&c__3, path, "LSE");
    zbl = lsamen_(&c__3, path, "ZBL");
    zbk = lsamen_(&c__3, path, "ZBK");
    zgl = lsamen_(&c__3, path, "ZGL");
    zgk = lsamen_(&c__3, path, "ZGK");

/*     Report values of parameters. */

    if (s_cmp(path, "   ", (ftnlen)3, (ftnlen)3) == 0) {
	goto L10;
    } else if (nep) {
	s_wsfe(&io___30);
	e_wsfe();
    } else if (sep) {
	s_wsfe(&io___31);
	e_wsfe();
    } else if (svd) {
	s_wsfe(&io___32);
	e_wsfe();
    } else if (zev) {
	s_wsfe(&io___33);
	e_wsfe();
    } else if (zes) {
	s_wsfe(&io___34);
	e_wsfe();
    } else if (zvx) {
	s_wsfe(&io___35);
	e_wsfe();
    } else if (zsx) {
	s_wsfe(&io___36);
	e_wsfe();
    } else if (zgg) {
	s_wsfe(&io___37);
	e_wsfe();
    } else if (zgs) {
	s_wsfe(&io___38);
	e_wsfe();
    } else if (zgx) {
	s_wsfe(&io___39);
	e_wsfe();
    } else if (zgv) {
	s_wsfe(&io___40);
	e_wsfe();
    } else if (zxv) {
	s_wsfe(&io___41);
	e_wsfe();
    } else if (zhb) {
	s_wsfe(&io___42);
	e_wsfe();
    } else if (zbb) {
	s_wsfe(&io___43);
	e_wsfe();
    } else if (glm) {
	s_wsfe(&io___44);
	e_wsfe();
    } else if (gqr) {
	s_wsfe(&io___45);
	e_wsfe();
    } else if (gsv) {
	s_wsfe(&io___46);
	e_wsfe();
    } else if (lse) {
	s_wsfe(&io___47);
	e_wsfe();
    } else if (zbl) {

/*        ZGEBAL:  Balancing */

	zchkbl_(&c__5, &c__6);
	goto L380;
    } else if (zbk) {

/*        ZGEBAK:  Back transformation */

	zchkbk_(&c__5, &c__6);
	goto L380;
    } else if (zgl) {

/*        ZGGBAL:  Balancing */

	zchkgl_(&c__5, &c__6);
	goto L380;
    } else if (zgk) {

/*        ZGGBAK:  Back transformation */

	zchkgk_(&c__5, &c__6);
	goto L380;
    } else if (lsamen_(&c__3, path, "ZEC")) {

/*        ZEC:  Eigencondition estimation */

	s_rsle(&io___48);
	do_lio(&c__5, &c__1, (char *)&thresh, (ftnlen)sizeof(doublereal));
	e_rsle();
	tsterr = TRUE_;
	zchkec_(&thresh, &tsterr, &c__5, &c__6);
	goto L380;
    } else {
	s_wsfe(&io___51);
	do_fio(&c__1, path, (ftnlen)3);
	e_wsfe();
	goto L380;
    }
    s_wsfe(&io___52);
    e_wsfe();
    s_wsfe(&io___53);
    e_wsfe();

/*     Read the number of values of M, P, and N. */

    s_rsle(&io___54);
    do_lio(&c__3, &c__1, (char *)&nn, (ftnlen)sizeof(integer));
    e_rsle();
    if (nn < 0) {
	s_wsfe(&io___56);
	do_fio(&c__1, "   NN ", (ftnlen)6);
	do_fio(&c__1, (char *)&nn, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&c__1, (ftnlen)sizeof(integer));
	e_wsfe();
	nn = 0;
	fatal = TRUE_;
    } else if (nn > 20) {
	s_wsfe(&io___57);
	do_fio(&c__1, "   NN ", (ftnlen)6);
	do_fio(&c__1, (char *)&nn, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&c__20, (ftnlen)sizeof(integer));
	e_wsfe();
	nn = 0;
	fatal = TRUE_;
    }

/*     Read the values of M */

    if (! (zgx || zxv)) {
	s_rsle(&io___58);
	i__1 = nn;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    do_lio(&c__3, &c__1, (char *)&mval[i__ - 1], (ftnlen)sizeof(
		    integer));
	}
	e_rsle();
	if (svd) {
	    s_copy(vname, "    M ", (ftnlen)6, (ftnlen)6);
	} else {
	    s_copy(vname, "    N ", (ftnlen)6, (ftnlen)6);
	}
	i__1 = nn;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (mval[i__ - 1] < 0) {
		s_wsfe(&io___62);
		do_fio(&c__1, vname, (ftnlen)6);
		do_fio(&c__1, (char *)&mval[i__ - 1], (ftnlen)sizeof(integer))
			;
		do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
		e_wsfe();
		fatal = TRUE_;
	    } else if (mval[i__ - 1] > 132) {
		s_wsfe(&io___63);
		do_fio(&c__1, vname, (ftnlen)6);
		do_fio(&c__1, (char *)&mval[i__ - 1], (ftnlen)sizeof(integer))
			;
		do_fio(&c__1, (char *)&c__132, (ftnlen)sizeof(integer));
		e_wsfe();
		fatal = TRUE_;
	    }
/* L20: */
	}
	s_wsfe(&io___64);
	do_fio(&c__1, "M:    ", (ftnlen)6);
	i__1 = nn;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    do_fio(&c__1, (char *)&mval[i__ - 1], (ftnlen)sizeof(integer));
	}
	e_wsfe();
    }

/*     Read the values of P */

    if (glm || gqr || gsv || lse) {
	s_rsle(&io___65);
	i__1 = nn;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    do_lio(&c__3, &c__1, (char *)&pval[i__ - 1], (ftnlen)sizeof(
		    integer));
	}
	e_rsle();
	i__1 = nn;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (pval[i__ - 1] < 0) {
		s_wsfe(&io___67);
		do_fio(&c__1, " P  ", (ftnlen)4);
		do_fio(&c__1, (char *)&pval[i__ - 1], (ftnlen)sizeof(integer))
			;
		do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
		e_wsfe();
		fatal = TRUE_;
	    } else if (pval[i__ - 1] > 132) {
		s_wsfe(&io___68);
		do_fio(&c__1, " P  ", (ftnlen)4);
		do_fio(&c__1, (char *)&pval[i__ - 1], (ftnlen)sizeof(integer))
			;
		do_fio(&c__1, (char *)&c__132, (ftnlen)sizeof(integer));
		e_wsfe();
		fatal = TRUE_;
	    }
/* L30: */
	}
	s_wsfe(&io___69);
	do_fio(&c__1, "P:    ", (ftnlen)6);
	i__1 = nn;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    do_fio(&c__1, (char *)&pval[i__ - 1], (ftnlen)sizeof(integer));
	}
	e_wsfe();
    }

/*     Read the values of N */

    if (svd || zbb || glm || gqr || gsv || lse) {
	s_rsle(&io___70);
	i__1 = nn;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    do_lio(&c__3, &c__1, (char *)&nval[i__ - 1], (ftnlen)sizeof(
		    integer));
	}
	e_rsle();
	i__1 = nn;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (nval[i__ - 1] < 0) {
		s_wsfe(&io___72);
		do_fio(&c__1, "    N ", (ftnlen)6);
		do_fio(&c__1, (char *)&nval[i__ - 1], (ftnlen)sizeof(integer))
			;
		do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
		e_wsfe();
		fatal = TRUE_;
	    } else if (nval[i__ - 1] > 132) {
		s_wsfe(&io___73);
		do_fio(&c__1, "    N ", (ftnlen)6);
		do_fio(&c__1, (char *)&nval[i__ - 1], (ftnlen)sizeof(integer))
			;
		do_fio(&c__1, (char *)&c__132, (ftnlen)sizeof(integer));
		e_wsfe();
		fatal = TRUE_;
	    }
/* L40: */
	}
    } else {
	i__1 = nn;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    nval[i__ - 1] = mval[i__ - 1];
/* L50: */
	}
    }
    if (! (zgx || zxv)) {
	s_wsfe(&io___74);
	do_fio(&c__1, "N:    ", (ftnlen)6);
	i__1 = nn;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    do_fio(&c__1, (char *)&nval[i__ - 1], (ftnlen)sizeof(integer));
	}
	e_wsfe();
    } else {
	s_wsfe(&io___75);
	do_fio(&c__1, "N:    ", (ftnlen)6);
	do_fio(&c__1, (char *)&nn, (ftnlen)sizeof(integer));
	e_wsfe();
    }

/*     Read the number of values of K, followed by the values of K */

    if (zhb || zbb) {
	s_rsle(&io___76);
	do_lio(&c__3, &c__1, (char *)&nk, (ftnlen)sizeof(integer));
	e_rsle();
	s_rsle(&io___78);
	i__1 = nk;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    do_lio(&c__3, &c__1, (char *)&kval[i__ - 1], (ftnlen)sizeof(
		    integer));
	}
	e_rsle();
	i__1 = nk;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (kval[i__ - 1] < 0) {
		s_wsfe(&io___80);
		do_fio(&c__1, "    K ", (ftnlen)6);
		do_fio(&c__1, (char *)&kval[i__ - 1], (ftnlen)sizeof(integer))
			;
		do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
		e_wsfe();
		fatal = TRUE_;
	    } else if (kval[i__ - 1] > 132) {
		s_wsfe(&io___81);
		do_fio(&c__1, "    K ", (ftnlen)6);
		do_fio(&c__1, (char *)&kval[i__ - 1], (ftnlen)sizeof(integer))
			;
		do_fio(&c__1, (char *)&c__132, (ftnlen)sizeof(integer));
		e_wsfe();
		fatal = TRUE_;
	    }
/* L60: */
	}
	s_wsfe(&io___82);
	do_fio(&c__1, "K:    ", (ftnlen)6);
	i__1 = nk;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    do_fio(&c__1, (char *)&kval[i__ - 1], (ftnlen)sizeof(integer));
	}
	e_wsfe();
    }

    if (zev || zes || zvx || zsx || zgs || zgx || zgv || zxv) {

/*        For the nonsymmetric driver routines, only one set of   
          parameters is allowed. */

	s_rsle(&io___83);
	do_lio(&c__3, &c__1, (char *)&nbval[0], (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&nbmin[0], (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&nxval[0], (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&nsval[0], (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&mxbval[0], (ftnlen)sizeof(integer));
	e_rsle();
	if (nbval[0] < 1) {
	    s_wsfe(&io___89);
	    do_fio(&c__1, "   NB ", (ftnlen)6);
	    do_fio(&c__1, (char *)&nbval[0], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&c__1, (ftnlen)sizeof(integer));
	    e_wsfe();
	    fatal = TRUE_;
	} else if (nbmin[0] < 1) {
	    s_wsfe(&io___90);
	    do_fio(&c__1, "NBMIN ", (ftnlen)6);
	    do_fio(&c__1, (char *)&nbmin[0], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&c__1, (ftnlen)sizeof(integer));
	    e_wsfe();
	    fatal = TRUE_;
	} else if (nxval[0] < 1) {
	    s_wsfe(&io___91);
	    do_fio(&c__1, "   NX ", (ftnlen)6);
	    do_fio(&c__1, (char *)&nxval[0], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&c__1, (ftnlen)sizeof(integer));
	    e_wsfe();
	    fatal = TRUE_;
	} else if (nsval[0] < 2) {
	    s_wsfe(&io___92);
	    do_fio(&c__1, "   NS ", (ftnlen)6);
	    do_fio(&c__1, (char *)&nsval[0], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&c__2, (ftnlen)sizeof(integer));
	    e_wsfe();
	    fatal = TRUE_;
	} else if (mxbval[0] < 1) {
	    s_wsfe(&io___93);
	    do_fio(&c__1, " MAXB ", (ftnlen)6);
	    do_fio(&c__1, (char *)&mxbval[0], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&c__1, (ftnlen)sizeof(integer));
	    e_wsfe();
	    fatal = TRUE_;
	}
	xlaenv_(&c__1, nbval);
	xlaenv_(&c__2, nbmin);
	xlaenv_(&c__3, nxval);
	xlaenv_(&c__4, nsval);
	xlaenv_(&c__8, mxbval);
	s_wsfe(&io___94);
	do_fio(&c__1, "NB:   ", (ftnlen)6);
	do_fio(&c__1, (char *)&nbval[0], (ftnlen)sizeof(integer));
	e_wsfe();
	s_wsfe(&io___95);
	do_fio(&c__1, "NBMIN:", (ftnlen)6);
	do_fio(&c__1, (char *)&nbmin[0], (ftnlen)sizeof(integer));
	e_wsfe();
	s_wsfe(&io___96);
	do_fio(&c__1, "NX:   ", (ftnlen)6);
	do_fio(&c__1, (char *)&nxval[0], (ftnlen)sizeof(integer));
	e_wsfe();
	s_wsfe(&io___97);
	do_fio(&c__1, "NS:   ", (ftnlen)6);
	do_fio(&c__1, (char *)&nsval[0], (ftnlen)sizeof(integer));
	e_wsfe();
	s_wsfe(&io___98);
	do_fio(&c__1, "MAXB: ", (ftnlen)6);
	do_fio(&c__1, (char *)&mxbval[0], (ftnlen)sizeof(integer));
	e_wsfe();
    } else if (! zhb && ! glm && ! gqr && ! gsv && ! lse) {

/*        For the other paths, the number of parameters can be varied   
          from the input file.  Read the number of parameter values. */

	s_rsle(&io___99);
	do_lio(&c__3, &c__1, (char *)&nparms, (ftnlen)sizeof(integer));
	e_rsle();
	if (nparms < 1) {
	    s_wsfe(&io___101);
	    do_fio(&c__1, "NPARMS", (ftnlen)6);
	    do_fio(&c__1, (char *)&nparms, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&c__1, (ftnlen)sizeof(integer));
	    e_wsfe();
	    nparms = 0;
	    fatal = TRUE_;
	} else if (nparms > 20) {
	    s_wsfe(&io___102);
	    do_fio(&c__1, "NPARMS", (ftnlen)6);
	    do_fio(&c__1, (char *)&nparms, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&c__20, (ftnlen)sizeof(integer));
	    e_wsfe();
	    nparms = 0;
	    fatal = TRUE_;
	}

/*        Read the values of NB */

	if (! zbb) {
	    s_rsle(&io___103);
	    i__1 = nparms;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		do_lio(&c__3, &c__1, (char *)&nbval[i__ - 1], (ftnlen)sizeof(
			integer));
	    }
	    e_rsle();
	    i__1 = nparms;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		if (nbval[i__ - 1] < 0) {
		    s_wsfe(&io___104);
		    do_fio(&c__1, "   NB ", (ftnlen)6);
		    do_fio(&c__1, (char *)&nbval[i__ - 1], (ftnlen)sizeof(
			    integer));
		    do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
		    e_wsfe();
		    fatal = TRUE_;
		} else if (nbval[i__ - 1] > 132) {
		    s_wsfe(&io___105);
		    do_fio(&c__1, "   NB ", (ftnlen)6);
		    do_fio(&c__1, (char *)&nbval[i__ - 1], (ftnlen)sizeof(
			    integer));
		    do_fio(&c__1, (char *)&c__132, (ftnlen)sizeof(integer));
		    e_wsfe();
		    fatal = TRUE_;
		}
/* L70: */
	    }
	    s_wsfe(&io___106);
	    do_fio(&c__1, "NB:   ", (ftnlen)6);
	    i__1 = nparms;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		do_fio(&c__1, (char *)&nbval[i__ - 1], (ftnlen)sizeof(integer)
			);
	    }
	    e_wsfe();
	}

/*        Read the values of NBMIN */

	if (nep || sep || svd || zgg) {
	    s_rsle(&io___107);
	    i__1 = nparms;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		do_lio(&c__3, &c__1, (char *)&nbmin[i__ - 1], (ftnlen)sizeof(
			integer));
	    }
	    e_rsle();
	    i__1 = nparms;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		if (nbmin[i__ - 1] < 0) {
		    s_wsfe(&io___108);
		    do_fio(&c__1, "NBMIN ", (ftnlen)6);
		    do_fio(&c__1, (char *)&nbmin[i__ - 1], (ftnlen)sizeof(
			    integer));
		    do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
		    e_wsfe();
		    fatal = TRUE_;
		} else if (nbmin[i__ - 1] > 132) {
		    s_wsfe(&io___109);
		    do_fio(&c__1, "NBMIN ", (ftnlen)6);
		    do_fio(&c__1, (char *)&nbmin[i__ - 1], (ftnlen)sizeof(
			    integer));
		    do_fio(&c__1, (char *)&c__132, (ftnlen)sizeof(integer));
		    e_wsfe();
		    fatal = TRUE_;
		}
/* L80: */
	    }
	    s_wsfe(&io___110);
	    do_fio(&c__1, "NBMIN:", (ftnlen)6);
	    i__1 = nparms;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		do_fio(&c__1, (char *)&nbmin[i__ - 1], (ftnlen)sizeof(integer)
			);
	    }
	    e_wsfe();
	} else {
	    i__1 = nparms;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		nbmin[i__ - 1] = 1;
/* L90: */
	    }
	}

/*        Read the values of NX */

	if (nep || sep || svd) {
	    s_rsle(&io___111);
	    i__1 = nparms;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		do_lio(&c__3, &c__1, (char *)&nxval[i__ - 1], (ftnlen)sizeof(
			integer));
	    }
	    e_rsle();
	    i__1 = nparms;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		if (nxval[i__ - 1] < 0) {
		    s_wsfe(&io___112);
		    do_fio(&c__1, "   NX ", (ftnlen)6);
		    do_fio(&c__1, (char *)&nxval[i__ - 1], (ftnlen)sizeof(
			    integer));
		    do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
		    e_wsfe();
		    fatal = TRUE_;
		} else if (nxval[i__ - 1] > 132) {
		    s_wsfe(&io___113);
		    do_fio(&c__1, "   NX ", (ftnlen)6);
		    do_fio(&c__1, (char *)&nxval[i__ - 1], (ftnlen)sizeof(
			    integer));
		    do_fio(&c__1, (char *)&c__132, (ftnlen)sizeof(integer));
		    e_wsfe();
		    fatal = TRUE_;
		}
/* L100: */
	    }
	    s_wsfe(&io___114);
	    do_fio(&c__1, "NX:   ", (ftnlen)6);
	    i__1 = nparms;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		do_fio(&c__1, (char *)&nxval[i__ - 1], (ftnlen)sizeof(integer)
			);
	    }
	    e_wsfe();
	} else {
	    i__1 = nparms;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		nxval[i__ - 1] = 1;
/* L110: */
	    }
	}

/*        Read the values of NSHIFT (if NEP or ZGG) or NRHS (if SVD   
          or ZBB). */

	if (nep || svd || zbb || zgg) {
	    s_rsle(&io___115);
	    i__1 = nparms;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		do_lio(&c__3, &c__1, (char *)&nsval[i__ - 1], (ftnlen)sizeof(
			integer));
	    }
	    e_rsle();
	    i__1 = nparms;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		if (nsval[i__ - 1] < 0) {
		    s_wsfe(&io___116);
		    do_fio(&c__1, "   NS ", (ftnlen)6);
		    do_fio(&c__1, (char *)&nsval[i__ - 1], (ftnlen)sizeof(
			    integer));
		    do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
		    e_wsfe();
		    fatal = TRUE_;
		} else if (nsval[i__ - 1] > 132) {
		    s_wsfe(&io___117);
		    do_fio(&c__1, "   NS ", (ftnlen)6);
		    do_fio(&c__1, (char *)&nsval[i__ - 1], (ftnlen)sizeof(
			    integer));
		    do_fio(&c__1, (char *)&c__132, (ftnlen)sizeof(integer));
		    e_wsfe();
		    fatal = TRUE_;
		}
/* L120: */
	    }
	    s_wsfe(&io___118);
	    do_fio(&c__1, "NS:   ", (ftnlen)6);
	    i__1 = nparms;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		do_fio(&c__1, (char *)&nsval[i__ - 1], (ftnlen)sizeof(integer)
			);
	    }
	    e_wsfe();
	} else {
	    i__1 = nparms;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		nsval[i__ - 1] = 1;
/* L130: */
	    }
	}

/*        Read the values for MAXB. */

	if (nep || zgg) {
	    s_rsle(&io___119);
	    i__1 = nparms;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		do_lio(&c__3, &c__1, (char *)&mxbval[i__ - 1], (ftnlen)sizeof(
			integer));
	    }
	    e_rsle();
	    i__1 = nparms;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		if (mxbval[i__ - 1] < 0) {
		    s_wsfe(&io___120);
		    do_fio(&c__1, " MAXB ", (ftnlen)6);
		    do_fio(&c__1, (char *)&mxbval[i__ - 1], (ftnlen)sizeof(
			    integer));
		    do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
		    e_wsfe();
		    fatal = TRUE_;
		} else if (mxbval[i__ - 1] > 132) {
		    s_wsfe(&io___121);
		    do_fio(&c__1, " MAXB ", (ftnlen)6);
		    do_fio(&c__1, (char *)&mxbval[i__ - 1], (ftnlen)sizeof(
			    integer));
		    do_fio(&c__1, (char *)&c__132, (ftnlen)sizeof(integer));
		    e_wsfe();
		    fatal = TRUE_;
		}
/* L140: */
	    }
	    s_wsfe(&io___122);
	    do_fio(&c__1, "MAXB: ", (ftnlen)6);
	    i__1 = nparms;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		do_fio(&c__1, (char *)&mxbval[i__ - 1], (ftnlen)sizeof(
			integer));
	    }
	    e_wsfe();
	} else {
	    i__1 = nparms;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		mxbval[i__ - 1] = 1;
/* L150: */
	    }
	}

/*        Read the values for NBCOL. */

	if (zgg) {
	    s_rsle(&io___123);
	    i__1 = nparms;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		do_lio(&c__3, &c__1, (char *)&nbcol[i__ - 1], (ftnlen)sizeof(
			integer));
	    }
	    e_rsle();
	    i__1 = nparms;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		if (nbcol[i__ - 1] < 0) {
		    s_wsfe(&io___125);
		    do_fio(&c__1, "NBCOL ", (ftnlen)6);
		    do_fio(&c__1, (char *)&nbcol[i__ - 1], (ftnlen)sizeof(
			    integer));
		    do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
		    e_wsfe();
		    fatal = TRUE_;
		} else if (nbcol[i__ - 1] > 132) {
		    s_wsfe(&io___126);
		    do_fio(&c__1, "NBCOL ", (ftnlen)6);
		    do_fio(&c__1, (char *)&nbcol[i__ - 1], (ftnlen)sizeof(
			    integer));
		    do_fio(&c__1, (char *)&c__132, (ftnlen)sizeof(integer));
		    e_wsfe();
		    fatal = TRUE_;
		}
/* L160: */
	    }
	    s_wsfe(&io___127);
	    do_fio(&c__1, "NBCOL:", (ftnlen)6);
	    i__1 = nparms;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		do_fio(&c__1, (char *)&nbcol[i__ - 1], (ftnlen)sizeof(integer)
			);
	    }
	    e_wsfe();
	} else {
	    i__1 = nparms;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		nbcol[i__ - 1] = 1;
/* L170: */
	    }
	}
    }

/*     Calculate and print the machine dependent constants. */

    s_wsle(&io___128);
    e_wsle();
    eps = dlamch_("Underflow threshold");
    s_wsfe(&io___130);
    do_fio(&c__1, "underflow", (ftnlen)9);
    do_fio(&c__1, (char *)&eps, (ftnlen)sizeof(doublereal));
    e_wsfe();
    eps = dlamch_("Overflow threshold");
    s_wsfe(&io___131);
    do_fio(&c__1, "overflow ", (ftnlen)9);
    do_fio(&c__1, (char *)&eps, (ftnlen)sizeof(doublereal));
    e_wsfe();
    eps = dlamch_("Epsilon");
    s_wsfe(&io___132);
    do_fio(&c__1, "precision", (ftnlen)9);
    do_fio(&c__1, (char *)&eps, (ftnlen)sizeof(doublereal));
    e_wsfe();

/*     Read the threshold value for the test ratios. */

    s_rsle(&io___133);
    do_lio(&c__5, &c__1, (char *)&thresh, (ftnlen)sizeof(doublereal));
    e_rsle();
    s_wsfe(&io___134);
    do_fio(&c__1, (char *)&thresh, (ftnlen)sizeof(doublereal));
    e_wsfe();
    if (sep || svd || zgg) {

/*        Read the flag that indicates whether to test LAPACK routines. */

	s_rsle(&io___135);
	do_lio(&c__8, &c__1, (char *)&tstchk, (ftnlen)sizeof(logical));
	e_rsle();

/*        Read the flag that indicates whether to test driver routines. */

	s_rsle(&io___137);
	do_lio(&c__8, &c__1, (char *)&tstdrv, (ftnlen)sizeof(logical));
	e_rsle();
    }

/*     Read the flag that indicates whether to test the error exits. */

    s_rsle(&io___139);
    do_lio(&c__8, &c__1, (char *)&tsterr, (ftnlen)sizeof(logical));
    e_rsle();

/*     Read the code describing how to set the random number seed. */

    s_rsle(&io___140);
    do_lio(&c__3, &c__1, (char *)&newsd, (ftnlen)sizeof(integer));
    e_rsle();

/*     If NEWSD = 2, read another line with 4 integers for the seed. */

    if (newsd == 2) {
	s_rsle(&io___142);
	for (i__ = 1; i__ <= 4; ++i__) {
	    do_lio(&c__3, &c__1, (char *)&ioldsd[i__ - 1], (ftnlen)sizeof(
		    integer));
	}
	e_rsle();
    }

    for (i__ = 1; i__ <= 4; ++i__) {
	iseed[i__ - 1] = ioldsd[i__ - 1];
/* L180: */
    }

    if (fatal) {
	s_wsfe(&io___144);
	e_wsfe();
	s_stop("", (ftnlen)0);
    }

/*     Read the input lines indicating the test path and its parameters.   
       The first three characters indicate the test path, and the number   
       of test matrix types must be the first nonblank item in columns   
       4-80. */

L190:

    if (! (zgx || zxv)) {

L200:
	i__1 = s_rsfe(&io___145);
	if (i__1 != 0) {
	    goto L380;
	}
	i__1 = do_fio(&c__1, line, (ftnlen)80);
	if (i__1 != 0) {
	    goto L380;
	}
	i__1 = e_rsfe();
	if (i__1 != 0) {
	    goto L380;
	}
	s_copy(c3, line, (ftnlen)3, (ftnlen)3);
	lenp = i_len(line, (ftnlen)80);
	i__ = 3;
	itmp = 0;
	i1 = 0;
L210:
	++i__;
	if (i__ > lenp) {
	    if (i1 > 0) {
		goto L240;
	    } else {
		ntypes = 30;
		goto L240;
	    }
	}
	if (*(unsigned char *)&line[i__ - 1] != ' ' && *(unsigned char *)&
		line[i__ - 1] != ',') {
	    i1 = i__;
	    *(unsigned char *)c1 = *(unsigned char *)&line[i1 - 1];

/*        Check that a valid integer was read */

	    for (k = 1; k <= 10; ++k) {
		if (*(unsigned char *)c1 == *(unsigned char *)&intstr[k - 1]) 
			{
		    ic = k - 1;
		    goto L230;
		}
/* L220: */
	    }
	    s_wsfe(&io___154);
	    do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
	    do_fio(&c__1, line, (ftnlen)80);
	    e_wsfe();
	    goto L200;
L230:
	    itmp = itmp * 10 + ic;
	    goto L210;
	} else if (i1 > 0) {
	    goto L240;
	} else {
	    goto L210;
	}
L240:
	ntypes = itmp;

/*     Skip the tests if NTYPES is <= 0. */

	if (! (zev || zes || zvx || zsx || zgv || zgs) && ntypes <= 0) {
	    s_wsfe(&io___155);
	    do_fio(&c__1, c3, (ftnlen)3);
	    e_wsfe();
	    goto L200;
	}

    } else {
	if (zgx) {
	    s_copy(c3, "ZGX", (ftnlen)3, (ftnlen)3);
	}
	if (zxv) {
	    s_copy(c3, "ZXV", (ftnlen)3, (ftnlen)3);
	}
    }

/*     Reset the random number seed. */

    if (newsd == 0) {
	for (k = 1; k <= 4; ++k) {
	    iseed[k - 1] = ioldsd[k - 1];
/* L250: */
	}
    }

    if (lsamen_(&c__3, c3, "ZHS") || lsamen_(&c__3, c3, 
	    "NEP")) {

/*        -------------------------------------   
          NEP:  Nonsymmetric Eigenvalue Problem   
          -------------------------------------   
          Vary the parameters   
             NB    = block size   
             NBMIN = minimum block size   
             NX    = crossover point   
             NS    = number of shifts   
             MAXB  = minimum submatrix size */

	maxtyp = 21;
	ntypes = min(maxtyp,ntypes);
	alareq_(c3, &ntypes, dotype, &maxtyp, &c__5, &c__6);
	if (tsterr) {
	    zerrhs_("ZHSEQR", &c__6);
	}
	i__1 = nparms;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    xlaenv_(&c__1, &nbval[i__ - 1]);
	    xlaenv_(&c__2, &nbmin[i__ - 1]);
	    xlaenv_(&c__3, &nxval[i__ - 1]);
	    xlaenv_(&c__4, &nsval[i__ - 1]);
	    xlaenv_(&c__8, &mxbval[i__ - 1]);

	    if (newsd == 0) {
		for (k = 1; k <= 4; ++k) {
		    iseed[k - 1] = ioldsd[k - 1];
/* L260: */
		}
	    }
	    s_wsfe(&io___158);
	    do_fio(&c__1, c3, (ftnlen)3);
	    do_fio(&c__1, (char *)&nbval[i__ - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&nbmin[i__ - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&nxval[i__ - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&nsval[i__ - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&mxbval[i__ - 1], (ftnlen)sizeof(integer));
	    e_wsfe();
	    zchkhs_(&nn, nval, &maxtyp, dotype, iseed, &thresh, &c__6, &a_ref(
		    1, 1), &c__132, &a_ref(1, 2), &a_ref(1, 3), &a_ref(1, 4), 
		    &a_ref(1, 5), &c__132, &a_ref(1, 6), &a_ref(1, 7), &
		    dc_ref(1, 1), &dc_ref(1, 2), &a_ref(1, 8), &a_ref(1, 9), &
		    a_ref(1, 10), &a_ref(1, 11), &a_ref(1, 12), &dc_ref(1, 3),
		     work, &c__89760, rwork, iwork, logwrk, result, &info);
	    if (info != 0) {
		s_wsfe(&io___167);
		do_fio(&c__1, "ZCHKHS", (ftnlen)6);
		do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
		e_wsfe();
	    }
/* L270: */
	}

    } else if (lsamen_(&c__3, c3, "ZST") || lsamen_(&
	    c__3, c3, "SEP")) {

/*        ----------------------------------   
          SEP:  Symmetric Eigenvalue Problem   
          ----------------------------------   
          Vary the parameters   
             NB    = block size   
             NBMIN = minimum block size   
             NX    = crossover point */

	maxtyp = 21;
	ntypes = min(maxtyp,ntypes);
	alareq_(c3, &ntypes, dotype, &maxtyp, &c__5, &c__6);
	xlaenv_(&c__9, &c__25);
	if (tsterr) {
	    zerrst_("ZST", &c__6);
	}
	i__1 = nparms;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    xlaenv_(&c__1, &nbval[i__ - 1]);
	    xlaenv_(&c__2, &nbmin[i__ - 1]);
	    xlaenv_(&c__3, &nxval[i__ - 1]);

	    if (newsd == 0) {
		for (k = 1; k <= 4; ++k) {
		    iseed[k - 1] = ioldsd[k - 1];
/* L280: */
		}
	    }
	    s_wsfe(&io___168);
	    do_fio(&c__1, c3, (ftnlen)3);
	    do_fio(&c__1, (char *)&nbval[i__ - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&nbmin[i__ - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&nxval[i__ - 1], (ftnlen)sizeof(integer));
	    e_wsfe();
	    if (tstchk) {
		zchkst_(&nn, nval, &maxtyp, dotype, iseed, &thresh, &c__6, &
			a_ref(1, 1), &c__132, &a_ref(1, 2), &dr_ref(1, 1), &
			dr_ref(1, 2), &dr_ref(1, 3), &dr_ref(1, 4), &dr_ref(1,
			 5), &dr_ref(1, 6), &dr_ref(1, 7), &dr_ref(1, 8), &
			dr_ref(1, 9), &dr_ref(1, 10), &dr_ref(1, 11), &a_ref(
			1, 3), &c__132, &a_ref(1, 4), &a_ref(1, 5), &dc_ref(1,
			 1), &a_ref(1, 6), work, &c__89760, rwork, &c__89760, 
			iwork, &c__20064, result, &info);
		if (info != 0) {
		    s_wsfe(&io___170);
		    do_fio(&c__1, "ZCHKST", (ftnlen)6);
		    do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
		    e_wsfe();
		}
	    }
	    if (tstdrv) {
		zdrvst_(&nn, nval, &c__18, dotype, iseed, &thresh, &c__6, &
			a_ref(1, 1), &c__132, &dr_ref(1, 3), &dr_ref(1, 4), &
			dr_ref(1, 5), &dr_ref(1, 8), &dr_ref(1, 9), &dr_ref(1,
			 10), &a_ref(1, 2), &c__132, &a_ref(1, 3), &dc_ref(1, 
			1), &a_ref(1, 4), work, &c__89760, rwork, &c__89760, 
			iwork, &c__20064, result, &info);
		if (info != 0) {
		    s_wsfe(&io___171);
		    do_fio(&c__1, "ZDRVST", (ftnlen)6);
		    do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
		    e_wsfe();
		}
	    }
/* L290: */
	}

    } else if (lsamen_(&c__3, c3, "ZSG")) {

/*        ----------------------------------------------   
          ZSG:  Hermitian Generalized Eigenvalue Problem   
          ----------------------------------------------   
          Vary the parameters   
             NB    = block size   
             NBMIN = minimum block size   
             NX    = crossover point */

	maxtyp = 21;
	ntypes = min(maxtyp,ntypes);
	alareq_(c3, &ntypes, dotype, &maxtyp, &c__5, &c__6);
	xlaenv_(&c__9, &c__25);
	i__1 = nparms;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    xlaenv_(&c__1, &nbval[i__ - 1]);
	    xlaenv_(&c__2, &nbmin[i__ - 1]);
	    xlaenv_(&c__3, &nxval[i__ - 1]);

	    if (newsd == 0) {
		for (k = 1; k <= 4; ++k) {
		    iseed[k - 1] = ioldsd[k - 1];
/* L300: */
		}
	    }
	    s_wsfe(&io___172);
	    do_fio(&c__1, c3, (ftnlen)3);
	    do_fio(&c__1, (char *)&nbval[i__ - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&nbmin[i__ - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&nxval[i__ - 1], (ftnlen)sizeof(integer));
	    e_wsfe();
	    if (tstchk) {
		zdrvsg_(&nn, nval, &maxtyp, dotype, iseed, &thresh, &c__6, &
			a_ref(1, 1), &c__132, &a_ref(1, 2), &c__132, &dr_ref(
			1, 3), &a_ref(1, 3), &c__132, &a_ref(1, 4), &a_ref(1, 
			5), &a_ref(1, 6), &a_ref(1, 7), work, &c__89760, 
			rwork, &c__89760, iwork, &c__20064, result, &info);
		if (info != 0) {
		    s_wsfe(&io___173);
		    do_fio(&c__1, "ZDRVSG", (ftnlen)6);
		    do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
		    e_wsfe();
		}
	    }
/* L310: */
	}

    } else if (lsamen_(&c__3, c3, "ZBD") || lsamen_(&
	    c__3, c3, "SVD")) {

/*        ----------------------------------   
          SVD:  Singular Value Decomposition   
          ----------------------------------   
          Vary the parameters   
             NB    = block size   
             NBMIN = minimum block size   
             NX    = crossover point   
             NRHS  = number of right hand sides */

	maxtyp = 16;
	ntypes = min(maxtyp,ntypes);
	alareq_(c3, &ntypes, dotype, &maxtyp, &c__5, &c__6);
	xlaenv_(&c__9, &c__25);

/*        Test the error exits */

	if (tsterr && tstchk) {
	    zerrbd_("ZBD", &c__6);
	}
	if (tsterr && tstdrv) {
	    zerred_("ZBD", &c__6);
	}

	i__1 = nparms;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    nrhs = nsval[i__ - 1];
	    xlaenv_(&c__1, &nbval[i__ - 1]);
	    xlaenv_(&c__2, &nbmin[i__ - 1]);
	    xlaenv_(&c__3, &nxval[i__ - 1]);
	    if (newsd == 0) {
		for (k = 1; k <= 4; ++k) {
		    iseed[k - 1] = ioldsd[k - 1];
/* L320: */
		}
	    }
	    s_wsfe(&io___175);
	    do_fio(&c__1, c3, (ftnlen)3);
	    do_fio(&c__1, (char *)&nbval[i__ - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&nbmin[i__ - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&nxval[i__ - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&nrhs, (ftnlen)sizeof(integer));
	    e_wsfe();
	    if (tstchk) {
		zchkbd_(&nn, mval, nval, &maxtyp, dotype, &nrhs, iseed, &
			thresh, &a_ref(1, 1), &c__132, &dr_ref(1, 1), &dr_ref(
			1, 2), &dr_ref(1, 3), &dr_ref(1, 4), &a_ref(1, 2), &
			c__132, &a_ref(1, 3), &a_ref(1, 4), &a_ref(1, 5), &
			c__132, &a_ref(1, 6), &c__132, &a_ref(1, 7), &a_ref(1,
			 8), work, &c__89760, rwork, &c__6, &info);
		if (info != 0) {
		    s_wsfe(&io___176);
		    do_fio(&c__1, "ZCHKBD", (ftnlen)6);
		    do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
		    e_wsfe();
		}
	    }
	    if (tstdrv) {
		zdrvbd_(&nn, mval, nval, &maxtyp, dotype, iseed, &thresh, &
			a_ref(1, 1), &c__132, &a_ref(1, 2), &c__132, &a_ref(1,
			 3), &c__132, &a_ref(1, 4), &a_ref(1, 5), &a_ref(1, 6)
			, &dr_ref(1, 1), &dr_ref(1, 2), &dr_ref(1, 3), work, &
			c__89760, rwork, iwork, &c__6, &info);
	    }
/* L330: */
	}

    } else if (lsamen_(&c__3, c3, "ZEV")) {

/*        --------------------------------------------   
          ZEV:  Nonsymmetric Eigenvalue Problem Driver   
                ZGEEV (eigenvalues and eigenvectors)   
          -------------------------------------------- */

	maxtyp = 21;
	ntypes = min(maxtyp,ntypes);
	if (ntypes <= 0) {
	    s_wsfe(&io___177);
	    do_fio(&c__1, c3, (ftnlen)3);
	    e_wsfe();
	} else {
	    if (tsterr) {
		zerred_(c3, &c__6);
	    }
	    alareq_(c3, &ntypes, dotype, &maxtyp, &c__5, &c__6);
	    zdrvev_(&nn, nval, &ntypes, dotype, iseed, &thresh, &c__6, &a_ref(
		    1, 1), &c__132, &a_ref(1, 2), &dc_ref(1, 1), &dc_ref(1, 2)
		    , &a_ref(1, 3), &c__132, &a_ref(1, 4), &c__132, &a_ref(1, 
		    5), &c__132, result, work, &c__89760, rwork, iwork, &info)
		    ;
	    if (info != 0) {
		s_wsfe(&io___178);
		do_fio(&c__1, "ZGEEV", (ftnlen)5);
		do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
		e_wsfe();
	    }
	}
	s_wsfe(&io___179);
	e_wsfe();
	goto L10;

    } else if (lsamen_(&c__3, c3, "ZES")) {

/*        --------------------------------------------   
          ZES:  Nonsymmetric Eigenvalue Problem Driver   
                ZGEES (Schur form)   
          -------------------------------------------- */

	maxtyp = 21;
	ntypes = min(maxtyp,ntypes);
	if (ntypes <= 0) {
	    s_wsfe(&io___180);
	    do_fio(&c__1, c3, (ftnlen)3);
	    e_wsfe();
	} else {
	    if (tsterr) {
		zerred_(c3, &c__6);
	    }
	    alareq_(c3, &ntypes, dotype, &maxtyp, &c__5, &c__6);
	    zdrves_(&nn, nval, &ntypes, dotype, iseed, &thresh, &c__6, &a_ref(
		    1, 1), &c__132, &a_ref(1, 2), &a_ref(1, 3), &dc_ref(1, 1),
		     &dc_ref(1, 2), &a_ref(1, 4), &c__132, result, work, &
		    c__89760, rwork, iwork, logwrk, &info);
	    if (info != 0) {
		s_wsfe(&io___181);
		do_fio(&c__1, "ZGEES", (ftnlen)5);
		do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
		e_wsfe();
	    }
	}
	s_wsfe(&io___182);
	e_wsfe();
	goto L10;

    } else if (lsamen_(&c__3, c3, "ZVX")) {

/*        --------------------------------------------------------------   
          ZVX:  Nonsymmetric Eigenvalue Problem Expert Driver   
                ZGEEVX (eigenvalues, eigenvectors and condition numbers)   
          -------------------------------------------------------------- */

	maxtyp = 21;
	ntypes = min(maxtyp,ntypes);
	if (ntypes < 0) {
	    s_wsfe(&io___183);
	    do_fio(&c__1, c3, (ftnlen)3);
	    e_wsfe();
	} else {
	    if (tsterr) {
		zerred_(c3, &c__6);
	    }
	    alareq_(c3, &ntypes, dotype, &maxtyp, &c__5, &c__6);
	    zdrvvx_(&nn, nval, &ntypes, dotype, iseed, &thresh, &c__5, &c__6, 
		    &a_ref(1, 1), &c__132, &a_ref(1, 2), &dc_ref(1, 1), &
		    dc_ref(1, 2), &a_ref(1, 3), &c__132, &a_ref(1, 4), &
		    c__132, &a_ref(1, 5), &c__132, &dr_ref(1, 1), &dr_ref(1, 
		    2), &dr_ref(1, 3), &dr_ref(1, 4), &dr_ref(1, 5), &dr_ref(
		    1, 6), &dr_ref(1, 7), &dr_ref(1, 8), result, work, &
		    c__89760, rwork, &info);
	    if (info != 0) {
		s_wsfe(&io___184);
		do_fio(&c__1, "ZGEEVX", (ftnlen)6);
		do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
		e_wsfe();
	    }
	}
	s_wsfe(&io___185);
	e_wsfe();
	goto L10;

    } else if (lsamen_(&c__3, c3, "ZSX")) {

/*        ---------------------------------------------------   
          ZSX:  Nonsymmetric Eigenvalue Problem Expert Driver   
                ZGEESX (Schur form and condition numbers)   
          --------------------------------------------------- */

	maxtyp = 21;
	ntypes = min(maxtyp,ntypes);
	if (ntypes < 0) {
	    s_wsfe(&io___186);
	    do_fio(&c__1, c3, (ftnlen)3);
	    e_wsfe();
	} else {
	    if (tsterr) {
		zerred_(c3, &c__6);
	    }
	    alareq_(c3, &ntypes, dotype, &maxtyp, &c__5, &c__6);
	    zdrvsx_(&nn, nval, &ntypes, dotype, iseed, &thresh, &c__5, &c__6, 
		    &a_ref(1, 1), &c__132, &a_ref(1, 2), &a_ref(1, 3), &
		    dc_ref(1, 1), &dc_ref(1, 2), &dc_ref(1, 3), &a_ref(1, 4), 
		    &c__132, &a_ref(1, 5), result, work, &c__89760, rwork, 
		    logwrk, &info);
	    if (info != 0) {
		s_wsfe(&io___187);
		do_fio(&c__1, "ZGEESX", (ftnlen)6);
		do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
		e_wsfe();
	    }
	}
	s_wsfe(&io___188);
	e_wsfe();
	goto L10;

    } else if (lsamen_(&c__3, c3, "ZGG")) {

/*        -------------------------------------------------   
          ZGG:  Generalized Nonsymmetric Eigenvalue Problem   
          -------------------------------------------------   
          Vary the parameters   
             NB    = block size   
             NBMIN = minimum block size   
             NS    = number of shifts   
             MAXB  = minimum submatrix size   
             NBCOL = minimum column dimension for blocks */

	maxtyp = 26;
	ntypes = min(maxtyp,ntypes);
	alareq_(c3, &ntypes, dotype, &maxtyp, &c__5, &c__6);
	if (tstchk && tsterr) {
	    zerrgg_(c3, &c__6);
	}
	i__1 = nparms;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    xlaenv_(&c__1, &nbval[i__ - 1]);
	    xlaenv_(&c__2, &nbmin[i__ - 1]);
	    xlaenv_(&c__4, &nsval[i__ - 1]);
	    xlaenv_(&c__8, &mxbval[i__ - 1]);
	    xlaenv_(&c__5, &nbcol[i__ - 1]);

	    if (newsd == 0) {
		for (k = 1; k <= 4; ++k) {
		    iseed[k - 1] = ioldsd[k - 1];
/* L340: */
		}
	    }
	    s_wsfe(&io___189);
	    do_fio(&c__1, c3, (ftnlen)3);
	    do_fio(&c__1, (char *)&nbval[i__ - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&nbmin[i__ - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&nsval[i__ - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&mxbval[i__ - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&nbcol[i__ - 1], (ftnlen)sizeof(integer));
	    e_wsfe();
	    tstdif = FALSE_;
	    thrshn = 10.;
	    if (tstchk) {
		zchkgg_(&nn, nval, &maxtyp, dotype, iseed, &thresh, &tstdif, &
			thrshn, &c__6, &a_ref(1, 1), &c__132, &a_ref(1, 2), &
			a_ref(1, 3), &a_ref(1, 4), &a_ref(1, 5), &a_ref(1, 6),
			 &a_ref(1, 7), &a_ref(1, 8), &a_ref(1, 9), &c__132, &
			a_ref(1, 10), &a_ref(1, 11), &a_ref(1, 12), &dc_ref(1,
			 1), &dc_ref(1, 2), &dc_ref(1, 3), &dc_ref(1, 4), &
			a_ref(1, 13), &a_ref(1, 14), work, &c__89760, rwork, 
			logwrk, result, &info);
		if (info != 0) {
		    s_wsfe(&io___192);
		    do_fio(&c__1, "ZCHKGG", (ftnlen)6);
		    do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
		    e_wsfe();
		}
	    }
	    if (tstdrv) {
		zdrvgg_(&nn, nval, &maxtyp, dotype, iseed, &thresh, &thrshn, &
			c__6, &a_ref(1, 1), &c__132, &a_ref(1, 2), &a_ref(1, 
			3), &a_ref(1, 4), &a_ref(1, 5), &a_ref(1, 6), &a_ref(
			1, 7), &c__132, &a_ref(1, 8), &dc_ref(1, 1), &dc_ref(
			1, 2), &dc_ref(1, 3), &dc_ref(1, 4), &a_ref(1, 8), &
			a_ref(1, 9), work, &c__89760, rwork, result, &info);
		if (info != 0) {
		    s_wsfe(&io___193);
		    do_fio(&c__1, "ZDRVGG", (ftnlen)6);
		    do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
		    e_wsfe();
		}
	    }
/* L350: */
	}

    } else if (lsamen_(&c__3, c3, "ZGS")) {

/*        -------------------------------------------------   
          ZGS:  Generalized Nonsymmetric Eigenvalue Problem   
                ZGGES (Schur form)   
          ------------------------------------------------- */

	maxtyp = 26;
	ntypes = min(maxtyp,ntypes);
	if (ntypes <= 0) {
	    s_wsfe(&io___194);
	    do_fio(&c__1, c3, (ftnlen)3);
	    e_wsfe();
	} else {
	    if (tsterr) {
		zerrgg_(c3, &c__6);
	    }
	    alareq_(c3, &ntypes, dotype, &maxtyp, &c__5, &c__6);
	    zdrges_(&nn, nval, &maxtyp, dotype, iseed, &thresh, &c__6, &a_ref(
		    1, 1), &c__132, &a_ref(1, 2), &a_ref(1, 3), &a_ref(1, 4), 
		    &a_ref(1, 7), &c__132, &a_ref(1, 8), &dc_ref(1, 1), &
		    dc_ref(1, 2), work, &c__89760, rwork, result, logwrk, &
		    info);

	    if (info != 0) {
		s_wsfe(&io___195);
		do_fio(&c__1, "ZDRGES", (ftnlen)6);
		do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
		e_wsfe();
	    }
	}
	s_wsfe(&io___196);
	e_wsfe();
	goto L10;

    } else if (zgx) {

/*        -------------------------------------------------   
          ZGX  Generalized Nonsymmetric Eigenvalue Problem   
                ZGGESX (Schur form and condition numbers)   
          ------------------------------------------------- */

	maxtyp = 5;
	ntypes = maxtyp;
	if (nn < 0) {
	    s_wsfe(&io___197);
	    do_fio(&c__1, c3, (ftnlen)3);
	    e_wsfe();
	} else {
	    if (tsterr) {
		zerrgg_(c3, &c__6);
	    }
	    alareq_(c3, &ntypes, dotype, &maxtyp, &c__5, &c__6);
	    zdrgsx_(&nn, &c__20, &thresh, &c__5, &c__6, &a_ref(1, 1), &c__132,
		     &a_ref(1, 2), &a_ref(1, 3), &a_ref(1, 4), &a_ref(1, 5), &
		    a_ref(1, 6), &dc_ref(1, 1), &dc_ref(1, 2), c__, &c__400, 
		    s, work, &c__89760, rwork, iwork, &c__20064, logwrk, &
		    info);
	    if (info != 0) {
		s_wsfe(&io___200);
		do_fio(&c__1, "ZDRGSX", (ftnlen)6);
		do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
		e_wsfe();
	    }
	}
	s_wsfe(&io___201);
	e_wsfe();
	goto L10;

    } else if (lsamen_(&c__3, c3, "ZGV")) {

/*        -------------------------------------------------   
          ZGV:  Generalized Nonsymmetric Eigenvalue Problem   
                ZGGEV (Eigenvalue/vector form)   
          ------------------------------------------------- */

	maxtyp = 26;
	ntypes = min(maxtyp,ntypes);
	if (ntypes <= 0) {
	    s_wsfe(&io___202);
	    do_fio(&c__1, c3, (ftnlen)3);
	    e_wsfe();
	} else {
	    if (tsterr) {
		zerrgg_(c3, &c__6);
	    }
	    alareq_(c3, &ntypes, dotype, &maxtyp, &c__5, &c__6);
	    zdrgev_(&nn, nval, &maxtyp, dotype, iseed, &thresh, &c__6, &a_ref(
		    1, 1), &c__132, &a_ref(1, 2), &a_ref(1, 3), &a_ref(1, 4), 
		    &a_ref(1, 7), &c__132, &a_ref(1, 8), &a_ref(1, 9), &
		    c__132, &dc_ref(1, 1), &dc_ref(1, 2), &dc_ref(1, 3), &
		    dc_ref(1, 4), work, &c__89760, rwork, result, &info);
	    if (info != 0) {
		s_wsfe(&io___203);
		do_fio(&c__1, "ZDRGEV", (ftnlen)6);
		do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
		e_wsfe();
	    }
	}
	s_wsfe(&io___204);
	e_wsfe();
	goto L10;

    } else if (zxv) {

/*        -------------------------------------------------   
          ZXV:  Generalized Nonsymmetric Eigenvalue Problem   
                ZGGEVX (eigenvalue/vector with condition numbers)   
          ------------------------------------------------- */

	maxtyp = 2;
	ntypes = maxtyp;
	if (nn < 0) {
	    s_wsfe(&io___205);
	    do_fio(&c__1, c3, (ftnlen)3);
	    e_wsfe();
	} else {
	    if (tsterr) {
		zerrgg_(c3, &c__6);
	    }
	    alareq_(c3, &ntypes, dotype, &maxtyp, &c__5, &c__6);
	    zdrgvx_(&nn, &thresh, &c__5, &c__6, &a_ref(1, 1), &c__132, &a_ref(
		    1, 2), &a_ref(1, 3), &a_ref(1, 4), &dc_ref(1, 1), &dc_ref(
		    1, 2), &a_ref(1, 5), &a_ref(1, 6), iwork, &iwork[1], &
		    dr_ref(1, 1), &dr_ref(1, 2), &dr_ref(1, 3), &dr_ref(1, 4),
		     &dr_ref(1, 5), &dr_ref(1, 6), work, &c__89760, rwork, &
		    iwork[2], &c__20062, result, logwrk, &info);

	    if (info != 0) {
		s_wsfe(&io___206);
		do_fio(&c__1, "ZDRGVX", (ftnlen)6);
		do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
		e_wsfe();
	    }
	}
	s_wsfe(&io___207);
	e_wsfe();
	goto L10;

    } else if (lsamen_(&c__3, c3, "ZHB")) {

/*        ------------------------------   
          ZHB:  Hermitian Band Reduction   
          ------------------------------ */

	maxtyp = 15;
	ntypes = min(maxtyp,ntypes);
	alareq_(c3, &ntypes, dotype, &maxtyp, &c__5, &c__6);
	if (tsterr) {
	    zerrst_("ZHB", &c__6);
	}
	zchkhb_(&nn, nval, &nk, kval, &maxtyp, dotype, iseed, &thresh, &c__6, 
		&a_ref(1, 1), &c__132, &dr_ref(1, 1), &dr_ref(1, 2), &a_ref(1,
		 2), &c__132, work, &c__89760, rwork, result, &info);
	if (info != 0) {
	    s_wsfe(&io___208);
	    do_fio(&c__1, "ZCHKHB", (ftnlen)6);
	    do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
	    e_wsfe();
	}

    } else if (lsamen_(&c__3, c3, "ZBB")) {

/*        ------------------------------   
          ZBB:  General Band Reduction   
          ------------------------------ */

	maxtyp = 15;
	ntypes = min(maxtyp,ntypes);
	alareq_(c3, &ntypes, dotype, &maxtyp, &c__5, &c__6);
	i__1 = nparms;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    nrhs = nsval[i__ - 1];

	    if (newsd == 0) {
		for (k = 1; k <= 4; ++k) {
		    iseed[k - 1] = ioldsd[k - 1];
/* L360: */
		}
	    }
	    s_wsfe(&io___209);
	    do_fio(&c__1, c3, (ftnlen)3);
	    do_fio(&c__1, (char *)&nrhs, (ftnlen)sizeof(integer));
	    e_wsfe();
	    zchkbb_(&nn, mval, nval, &nk, kval, &maxtyp, dotype, &nrhs, iseed,
		     &thresh, &c__6, &a_ref(1, 1), &c__132, &a_ref(1, 2), &
		    c__264, &dr_ref(1, 1), &dr_ref(1, 2), &a_ref(1, 4), &
		    c__132, &a_ref(1, 5), &c__132, &a_ref(1, 6), &c__132, &
		    a_ref(1, 7), work, &c__89760, rwork, result, &info);
	    if (info != 0) {
		s_wsfe(&io___210);
		do_fio(&c__1, "ZCHKBB", (ftnlen)6);
		do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
		e_wsfe();
	    }
/* L370: */
	}

    } else if (lsamen_(&c__3, c3, "GLM")) {

/*        -----------------------------------------   
          GLM:  Generalized Linear Regression Model   
          ----------------------------------------- */

	if (tsterr) {
	    zerrgg_("GLM", &c__6);
	}
	zckglm_(&nn, nval, mval, pval, &ntypes, iseed, &thresh, &c__132, &
		a_ref(1, 1), &a_ref(1, 2), &b_ref(1, 1), &b_ref(1, 2), x, 
		work, &dr_ref(1, 1), &c__5, &c__6, &info);
	if (info != 0) {
	    s_wsfe(&io___213);
	    do_fio(&c__1, "ZCKGLM", (ftnlen)6);
	    do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
	    e_wsfe();
	}

    } else if (lsamen_(&c__3, c3, "GQR")) {

/*        ------------------------------------------   
          GQR:  Generalized QR and RQ factorizations   
          ------------------------------------------ */

	if (tsterr) {
	    zerrgg_("GQR", &c__6);
	}
	zckgqr_(&nn, mval, &nn, pval, &nn, nval, &ntypes, iseed, &thresh, &
		c__132, &a_ref(1, 1), &a_ref(1, 2), &a_ref(1, 3), &a_ref(1, 4)
		, taua, &b_ref(1, 1), &b_ref(1, 2), &b_ref(1, 3), &b_ref(1, 4)
		, &b_ref(1, 5), taub, work, &dr_ref(1, 1), &c__5, &c__6, &
		info);
	if (info != 0) {
	    s_wsfe(&io___216);
	    do_fio(&c__1, "ZCKGQR", (ftnlen)6);
	    do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
	    e_wsfe();
	}

    } else if (lsamen_(&c__3, c3, "GSV")) {

/*        ----------------------------------------------   
          GSV:  Generalized Singular Value Decomposition   
          ---------------------------------------------- */

	if (tsterr) {
	    zerrgg_("GSV", &c__6);
	}
	zckgsv_(&nn, mval, pval, nval, &ntypes, iseed, &thresh, &c__132, &
		a_ref(1, 1), &a_ref(1, 2), &b_ref(1, 1), &b_ref(1, 2), &a_ref(
		1, 3), &b_ref(1, 3), &a_ref(1, 4), alpha, beta, &b_ref(1, 4), 
		iwork, work, &dr_ref(1, 1), &c__5, &c__6, &info);
	if (info != 0) {
	    s_wsfe(&io___219);
	    do_fio(&c__1, "ZCKGSV", (ftnlen)6);
	    do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
	    e_wsfe();
	}

    } else if (lsamen_(&c__3, c3, "LSE")) {

/*        --------------------------------------   
          LSE:  Constrained Linear Least Squares   
          -------------------------------------- */

	if (tsterr) {
	    zerrgg_("LSE", &c__6);
	}
	zcklse_(&nn, mval, pval, nval, &ntypes, iseed, &thresh, &c__132, &
		a_ref(1, 1), &a_ref(1, 2), &b_ref(1, 1), &b_ref(1, 2), x, 
		work, &dr_ref(1, 1), &c__5, &c__6, &info);
	if (info != 0) {
	    s_wsfe(&io___220);
	    do_fio(&c__1, "ZCKLSE", (ftnlen)6);
	    do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
	    e_wsfe();
	}
    } else {
	s_wsle(&io___221);
	e_wsle();
	s_wsle(&io___222);
	e_wsle();
	s_wsfe(&io___223);
	do_fio(&c__1, c3, (ftnlen)3);
	e_wsfe();
    }
    if (! (zgx || zxv)) {
	goto L190;
    }
L380:
    s_wsfe(&io___224);
    e_wsfe();
    s2 = dsecnd_();
    s_wsfe(&io___226);
    d__1 = s2 - s1;
    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    e_wsfe();


/*     End of ZCHKEE */

    return 0;
} /* MAIN__ */

#undef dr_ref
#undef dc_ref
#undef dc_subscr
#undef b_ref
#undef b_subscr
#undef a_ref
#undef a_subscr


/* Main program alias */ int zchkee_ () { MAIN__ (); return 0; }
