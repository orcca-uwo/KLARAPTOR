#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    real opcnt[6], timng[6];
} lstime_;

#define lstime_1 lstime_

struct {
    integer infot, iounit;
    logical ok, lerr;
} infoc_;

#define infoc_1 infoc_

struct {
    char srnamt[6];
} srnamc_;

#define srnamc_1 srnamc_

/* Table of constant values */

static integer c__5 = 5;
static integer c__2 = 2;
static integer c__9 = 9;
static integer c__25 = 25;
static integer c__1 = 1;
static integer c__3 = 3;
static real c_b27 = 1.f;
static real c_b28 = 0.f;
static integer c__6 = 6;

/* Subroutine */ int stimls_(char *line, integer *nm, integer *mval, integer *
	nn, integer *nval, integer *nns, integer *nsval, integer *nnb, 
	integer *nbval, integer *nxval, integer *nlda, integer *ldaval, real *
	timmin, real *a, real *copya, real *b, real *copyb, real *s, real *
	copys, real *opctbl, real *timtbl, real *flptbl, real *work, integer *
	iwork, integer *nout, ftnlen line_len)
{
    /* Initialized data */

    static char subnam[6*5] = "SGELS " "SGELSX" "SGELSY" "SGELSS" "SGELSD";
    static integer iseedy[4] = { 1988,1989,1990,1991 };
    static integer ndata[5] = { 4,6,6,6,5 };

    /* Format strings */
    static char fmt_9999[] = "(///\002 ****** Results for \002,a,\002 *****"
	    "*\002)";
    static char fmt_9998[] = "(/\002 SGELS   : overall performance\002,/\002"
	    " comp. 1 : if M>=N, SGEQRF, QR factorization\002,/\002          "
	    " if M< N, SGELQF, QR factorization\002,/\002 comp. 2 : if M>=N, "
	    "SORMQR, multiplication by\002,\002 reflectors\002,/\002         "
	    "  if M< N, SORMLQ, multiplication by\002,\002 reflectors\002,"
	    "/\002 comp. 3 : STRSM, solution of the triangular\002,\002 system"
	    "\002/,/\002 Types 4 to 6 are the transpose\002,\002 of types 1 t"
	    "o 3\002)";
    static char fmt_9997[] = "(/\002 SGELSX  : overall performance\002,/\002"
	    " comp. 1 : SGEQPF, QR factorization with column\002,\002 pivoting"
	    "\002,/\002 comp. 2 : if RANK<N, STZRQF, reduction to\002,\002 tr"
	    "iangular form\002,/\002 comp. 3 : SORM2R, multiplication by refl"
	    "ectors\002,/\002 comp. 4 : STRSM, solution of the triangular\002,"
	    "\002 system\002,/\002 comp. 5 : if RANK<N, SLATZM, multiplicatio"
	    "n by\002,\002 reflectors\002)";
    static char fmt_9996[] = "(/\002 SGELSY  : overall performance\002,/\002"
	    " comp. 1 : SGEQP3, QR factorization with column\002,\002 pivoting"
	    "\002,/\002 comp. 2 : if RANK<N, STZRZF, reduction to\002,\002 tr"
	    "iangular form\002,/\002 comp. 3 : SORMQR, multiplication by refl"
	    "ectors\002,/\002 comp. 4 : STRSM, solution of the triangular\002,"
	    "\002 system\002,/\002 comp. 5 : if RANK<N, SORMRZ, multiplicatio"
	    "n by\002,\002 reflectors\002)";
    static char fmt_9995[] = "(/\002 SGELSS: overall performance\002,/\002 c"
	    "omp. 1 : if M>>N, SGEQRF, QR factorization\002,/\002            "
	    "        SORMQR, multiplication by\002,\002 reflectors\002,/\002 "
	    "          if N>>M, SGELQF, QL factorization\002,/\002 comp. 2 : "
	    "SGEBRD, reduction to bidiagonal form\002,/\002 comp. 3 : SORMBR,"
	    " multiplication by left\002,\002 bidiagonalizing vectors\002,"
	    "/\002           SORGBR, generation of right\002,\002 bidiagonali"
	    "zing vectors\002,/\002 comp. 4 : SBDSQR, singular value decompos"
	    "ition\002,\002 of the bidiagonal matrix\002,/\002 comp. 5 : mult"
	    "iplication by right bidiagonalizing\002,\002 vectors\002,/\002  "
	    "         (SGEMM or SGEMV, and SORMLQ if N>>M)\002)";
    static char fmt_9994[] = "(/\002 SGELSD: overall performance\002,/\002 c"
	    "omp. 1 : if M>>N, SGEQRF, QR factorization\002,/\002            "
	    "        SORMQR, multiplication by\002,\002 reflectors\002,/\002 "
	    "          if N>>M, SGELQF, QL factorization\002,/\002 comp. 2 : "
	    "SGEBRD, reduction to bidiagonal form\002,/\002 comp. 3 : SORMBR,"
	    " multiplication by left \002,\002 bidiagonalizing vectors\002,"
	    "/\002                   multiplication by right\002,\002 bidiago"
	    "nalizing vectors\002,/\002 comp. 4 : SLALSD, singular value deco"
	    "mposition\002,\002 of the bidiagonal matrix\002)";
    static char fmt_9993[] = "(//\002 *** Time in seconds *** \002)";
    static char fmt_9992[] = "(//\002 *** Number of floating-point operation"
	    "s *** \002)";
    static char fmt_9991[] = "(//\002 *** Speed in megaflops *** \002)";

    /* System generated locals */
    integer flptbl_dim3, flptbl_offset, opctbl_dim3, opctbl_offset, 
	    timtbl_dim3, timtbl_offset, i__1, i__2, i__3, i__4, i__5, i__6, 
	    i__7;
    real r__1, r__2;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    double sqrt(doublereal), log(doublereal);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static integer ilda, itbl, info;
    static char path[3];
    static integer rank;
    static real time;
    static integer ncls, isub, nrhs, nlvl, i__, m, n, ncall, iseed[4], crank, 
	    nclsd, irank;
    static real rcond;
    extern /* Subroutine */ int sscal_(integer *, real *, real *, integer *), 
	    sgemm_(char *, char *, integer *, integer *, integer *, real *, 
	    real *, integer *, real *, integer *, real *, real *, integer *);
    static integer itran, mnmin, ncols;
    static real norma;
    static integer nclss;
    static real normb;
    extern /* Subroutine */ int sgels_(char *, integer *, integer *, integer *
	    , real *, integer *, real *, integer *, real *, integer *, 
	    integer *);
    static char trans[1];
    static integer nclsx, itype, lwork, nclsy;
    extern doublereal sasum_(integer *, real *, integer *);
    extern /* Subroutine */ int sqrt13_(integer *, integer *, integer *, real 
	    *, integer *, real *, integer *), sqrt15_(integer *, integer *, 
	    integer *, integer *, integer *, real *, integer *, real *, 
	    integer *, real *, integer *, real *, real *, integer *, real *, 
	    integer *);
    static real s1, s2;
    extern /* Subroutine */ int scopy_(integer *, real *, integer *, real *, 
	    integer *);
    static integer nrows, lwlsy, nb, im, in, iscale;
    extern doublereal slamch_(char *), second_(void);
    extern /* Subroutine */ int sgelsd_(integer *, integer *, integer *, real 
	    *, integer *, real *, integer *, real *, real *, integer *, real *
	    , integer *, integer *, integer *), atimin_(char *, char *, 
	    integer *, char *, logical *, integer *, integer *, ftnlen, 
	    ftnlen, ftnlen), slacpy_(char *, integer *, integer *, real *, 
	    integer *, real *, integer *), slaset_(char *, integer *, 
	    integer *, real *, real *, real *, integer *), xlaenv_(
	    integer *, integer *);
    extern doublereal smflop_(real *, real *, integer *);
    extern /* Subroutine */ int sgelss_(integer *, integer *, integer *, real 
	    *, integer *, real *, integer *, real *, real *, integer *, real *
	    , integer *, integer *);
    static integer ldwork;
    static logical timsub[5];
    extern /* Subroutine */ int sgelsx_(integer *, integer *, integer *, real 
	    *, integer *, real *, integer *, integer *, real *, integer *, 
	    real *, integer *), sgelsy_(integer *, integer *, integer *, real 
	    *, integer *, real *, integer *, integer *, real *, integer *, 
	    real *, integer *, integer *), slarnv_(integer *, integer *, 
	    integer *, real *), sprtls_(integer *, char *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, integer *, real *, 
	    integer *);
    static integer lda, ldb, inb;
    static real eps;
    static integer ins;

    /* Fortran I/O blocks */
    static cilist io___48 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___49 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___50 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___51 = { 0, 0, 0, fmt_9996, 0 };
    static cilist io___52 = { 0, 0, 0, fmt_9995, 0 };
    static cilist io___53 = { 0, 0, 0, fmt_9994, 0 };
    static cilist io___55 = { 0, 0, 0, fmt_9993, 0 };
    static cilist io___56 = { 0, 0, 0, fmt_9992, 0 };
    static cilist io___57 = { 0, 0, 0, fmt_9991, 0 };



#define opctbl_ref(a_1,a_2,a_3,a_4) opctbl[(((a_4)*opctbl_dim3 + (a_3))*6 \
+ (a_2))*6 + a_1]
#define flptbl_ref(a_1,a_2,a_3,a_4) flptbl[(((a_4)*flptbl_dim3 + (a_3))*6 \
+ (a_2))*6 + a_1]
#define subnam_ref(a_0,a_1) &subnam[(a_1)*6 + a_0 - 6]
#define timtbl_ref(a_1,a_2,a_3,a_4) timtbl[(((a_4)*timtbl_dim3 + (a_3))*6 \
+ (a_2))*6 + a_1]


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       December 22, 1999   


    Purpose   
    =======   

    STIMLS times the least squares driver routines SGELS, SGELSS, SGELSX,   
    SGELSY and SGELSD.   

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
            The values of the matrix row dimension M.   

    NN      (input) INTEGER   
            The number of values of N contained in the vector NVAL.   

    NVAL    (input) INTEGER array, dimension (NN)   
            The values of the matrix column dimension N.   

    NNS     (input) INTEGER   
            The number of values of NRHS contained in the vector NSVAL.   

    NSVAL   (input) INTEGER array, dimension (NNS)   
            The values of the number of right hand sides NRHS.   

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

    A       (workspace) REAL array, dimension (MMAX*NMAX)   
            where MMAX is the maximum value of M in MVAL and NMAX is the   
            maximum value of N in NVAL.   

    COPYA   (workspace) REAL array, dimension (MMAX*NMAX)   

    B       (workspace) REAL array, dimension (MMAX*NSMAX)   
            where MMAX is the maximum value of M in MVAL and NSMAX is the   
            maximum value of NRHS in NSVAL.   

    COPYB   (workspace) REAL array, dimension (MMAX*NSMAX)   

    S       (workspace) REAL array, dimension   
                        (min(MMAX,NMAX))   

    COPYS   (workspace) REAL array, dimension   
                        (min(MMAX,NMAX))   

    OPCTBL  (workspace) REAL array, dimension   
                        (6,6,(NNB+1)*NLDA,NM*NN*NNS,5)   

    TIMTBL  (workspace) REAL array, dimension   
                        (6,6,(NNB+1)*NLDA,NM*NN*NNS,5)   

    FLPTBL  (workspace) REAL array, dimension   
                        (6,6,(NNB+1)*NLDA,NM*NN*NNS,5)   

    WORK    (workspace) REAL array,   
                        dimension (MMAX*NMAX + 4*NMAX + MMAX).   

    IWORK   (workspace) INTEGER array, dimension (NMAX)   

    NOUT    (input) INTEGER   
            The unit number for output.   

    =====================================================================   

       Parameter adjustments */
    --mval;
    --nval;
    --nsval;
    --nbval;
    --nxval;
    flptbl_dim3 = *nm * *nn * *nns * *nlda * (*nnb + 1);
    flptbl_offset = 1 + 6 * (1 + 6 * (1 + flptbl_dim3 * 1));
    flptbl -= flptbl_offset;
    timtbl_dim3 = *nm * *nn * *nns * *nlda * (*nnb + 1);
    timtbl_offset = 1 + 6 * (1 + 6 * (1 + timtbl_dim3 * 1));
    timtbl -= timtbl_offset;
    opctbl_dim3 = *nm * *nn * *nns * *nlda * (*nnb + 1);
    opctbl_offset = 1 + 6 * (1 + 6 * (1 + opctbl_dim3 * 1));
    opctbl -= opctbl_offset;
    --ldaval;
    --a;
    --copya;
    --b;
    --copyb;
    --s;
    --copys;
    --work;
    --iwork;

    /* Function Body   

       Extract the timing request from the input line. */

    s_copy(path, "Single precision", (ftnlen)1, (ftnlen)16);
    s_copy(path + 1, "LS", (ftnlen)2, (ftnlen)2);
    atimin_(path, line, &c__5, subnam, timsub, nout, &info, (ftnlen)3, (
	    ftnlen)80, (ftnlen)6);
    if (info != 0) {
	goto L230;
    }

/*     Initialize constants and the random number seed. */

    ncls = 0;
    nclsd = 0;
    nclss = 0;
    nclsx = 0;
    nclsy = 0;
    for (i__ = 1; i__ <= 4; ++i__) {
	iseed[i__ - 1] = iseedy[i__ - 1];
/* L10: */
    }
    eps = slamch_("Epsilon");

/*     Threshold for rank estimation */

    rcond = sqrt(eps) - (sqrt(eps) - eps) / 2;

    infoc_1.infot = 0;
    xlaenv_(&c__2, &c__2);
    xlaenv_(&c__9, &c__25);

    i__1 = *nm;
    for (im = 1; im <= i__1; ++im) {
	m = mval[im];

	i__2 = *nn;
	for (in = 1; in <= i__2; ++in) {
	    n = nval[in];
	    mnmin = min(m,n);

	    i__3 = *nns;
	    for (ins = 1; ins <= i__3; ++ins) {
		nrhs = nsval[ins];
/* Computing MAX   
   Computing MAX */
		r__1 = 1.f, r__2 = (real) mnmin;
		i__4 = (integer) (log(dmax(r__1,r__2) / 26.f) / log(2.f)) + 1;
		nlvl = max(i__4,0);
/* Computing MAX */
		i__4 = 1, i__5 = (m + nrhs) * (n + 2), i__4 = max(i__4,i__5), 
			i__5 = (n + nrhs) * (m + 2), i__4 = max(i__4,i__5), 
			i__5 = m * n + (mnmin << 2) + max(m,n), i__4 = max(
			i__4,i__5), i__5 = mnmin * 12 + mnmin * 50 + (mnmin <<
			 3) * nlvl + mnmin * nrhs + 676;
		lwork = max(i__4,i__5);

		i__4 = *nlda;
		for (ilda = 1; ilda <= i__4; ++ilda) {
/* Computing MAX */
		    i__5 = 1, i__6 = ldaval[ilda];
		    lda = max(i__5,i__6);
/* Computing MAX */
		    i__5 = 1, i__6 = ldaval[ilda], i__5 = max(i__5,i__6), 
			    i__5 = max(i__5,m);
		    ldb = max(i__5,n);

		    for (irank = 1; irank <= 2; ++irank) {

			for (iscale = 1; iscale <= 3; ++iscale) {

			    if (irank == 1 && timsub[0]) {

/*                          Time SGELS   

                            Generate a matrix of scaling type ISCALE */

				sqrt13_(&iscale, &m, &n, &copya[1], &lda, &
					norma, iseed);
				i__5 = *nnb;
				for (inb = 1; inb <= i__5; ++inb) {
				    nb = nbval[inb];
				    xlaenv_(&c__1, &nb);
				    xlaenv_(&c__3, &nxval[inb]);

				    for (itran = 1; itran <= 2; ++itran) {
					itype = (itran - 1) * 3 + iscale;
					if (itran == 1) {
					    *(unsigned char *)trans = 'N';
					    nrows = m;
					    ncols = n;
					} else {
					    *(unsigned char *)trans = 'T';
					    nrows = n;
					    ncols = m;
					}
					ldwork = max(1,ncols);

/*                                Set up a consistent rhs */

					if (ncols > 0) {
					    i__6 = ncols * nrhs;
					    slarnv_(&c__2, iseed, &i__6, &
						    work[1]);
					    i__6 = ncols * nrhs;
					    r__1 = 1.f / (real) ncols;
					    sscal_(&i__6, &r__1, &work[1], &
						    c__1);
					}
					sgemm_(trans, "No transpose", &nrows, 
						&nrhs, &ncols, &c_b27, &copya[
						1], &lda, &work[1], &ldwork, &
						c_b28, &b[1], &ldb);
					slacpy_("Full", &nrows, &nrhs, &b[1], 
						&ldb, &copyb[1], &ldb);

/*                                Solve LS or overdetermined system */

					ncall = 0;
					time = 0.f;
					slaset_("Full", ndata, &c__1, &c_b28, 
						&c_b28, lstime_1.opcnt, ndata);
					slaset_("Full", ndata, &c__1, &c_b28, 
						&c_b28, lstime_1.timng, ndata);
L20:
					if (m > 0 && n > 0) {
					    slacpy_("Full", &m, &n, &copya[1],
						     &lda, &a[1], &lda);
					    slacpy_("Full", &nrows, &nrhs, &
						    copyb[1], &ldb, &b[1], &
						    ldb);
					}
					s_copy(srnamc_1.srnamt, "SGELS ", (
						ftnlen)6, (ftnlen)6);
					++ncall;
					s1 = second_();
					sgels_(trans, &m, &n, &nrhs, &a[1], &
						lda, &b[1], &ldb, &work[1], &
						lwork, &info);
					s2 = second_();
					time += s2 - s1;
					if (info == 0 && time < *timmin) {
					    goto L20;
					}
					lstime_1.timng[0] = time;
					lstime_1.opcnt[0] = sasum_(ndata, 
						lstime_1.opcnt, &c__1);
					r__1 = 1.f / (real) ncall;
					sscal_(ndata, &r__1, lstime_1.opcnt, &
						c__1);
					r__1 = 1.f / (real) ncall;
					sscal_(ndata, &r__1, lstime_1.timng, &
						c__1);
					scopy_(ndata, lstime_1.opcnt, &c__1, &
						opctbl_ref(1, itype, ncls + 
						inb, 1), &c__1);
					scopy_(ndata, lstime_1.timng, &c__1, &
						timtbl_ref(1, itype, ncls + 
						inb, 1), &c__1);
					i__6 = ndata[0];
					for (i__ = 1; i__ <= i__6; ++i__) {
					    flptbl_ref(i__, itype, ncls + inb,
						     1) = smflop_(&
						    lstime_1.opcnt[i__ - 1], &
						    lstime_1.timng[i__ - 1], &
						    info);
/* L30: */
					}
/* L40: */
				    }
/* L50: */
				}

			    }

/*                       Generate a matrix of scaling type ISCALE and   
                         rank type IRANK. */

			    itype = (irank - 1) * 3 + iscale;
			    sqrt15_(&iscale, &irank, &m, &n, &nrhs, &copya[1],
				     &lda, &copyb[1], &ldb, &copys[1], &rank, 
				    &norma, &normb, iseed, &work[1], &lwork);

			    if (timsub[1]) {

/*                       Time SGELSX   

                         workspace used:   
                         MAX(M+MIN(M,N),NRHS*MIN(M,N),2*N+M) */

				ldwork = max(1,m);

/*                       SGELSX:  Compute the minimum-norm   
                         solution X to min( norm( A * X - B ) )   
                         using a complete orthogonal factorization. */

				ncall = 0;
				time = 0.f;
				slaset_("Full", &ndata[1], &c__1, &c_b28, &
					c_b28, lstime_1.opcnt, &ndata[1]);
				slaset_("Full", &ndata[1], &c__1, &c_b28, &
					c_b28, lstime_1.timng, &ndata[1]);
L60:
				slacpy_("Full", &m, &n, &copya[1], &lda, &a[1]
					, &lda);
				slacpy_("Full", &m, &nrhs, &copyb[1], &ldb, &
					b[1], &ldb);
				s_copy(srnamc_1.srnamt, "SGELSX", (ftnlen)6, (
					ftnlen)6);
				++ncall;
				s1 = second_();
				sgelsx_(&m, &n, &nrhs, &a[1], &lda, &b[1], &
					ldb, &iwork[1], &rcond, &crank, &work[
					1], &info);
				s2 = second_();
				time += s2 - s1;
				if (info == 0 && time < *timmin) {
				    goto L60;
				}
				lstime_1.timng[0] = time;
				lstime_1.opcnt[0] = sasum_(&ndata[1], 
					lstime_1.opcnt, &c__1);
				r__1 = 1.f / (real) ncall;
				sscal_(&ndata[1], &r__1, lstime_1.opcnt, &
					c__1);
				r__1 = 1.f / (real) ncall;
				sscal_(&ndata[1], &r__1, lstime_1.timng, &
					c__1);
				scopy_(&ndata[1], lstime_1.opcnt, &c__1, &
					opctbl_ref(1, itype, nclsx + 1, 2), &
					c__1);
				scopy_(&ndata[1], lstime_1.timng, &c__1, &
					timtbl_ref(1, itype, nclsx + 1, 2), &
					c__1);
				i__5 = ndata[1];
				for (i__ = 1; i__ <= i__5; ++i__) {
				    flptbl_ref(i__, itype, nclsx + 1, 2) = 
					    smflop_(&lstime_1.opcnt[i__ - 1], 
					    &lstime_1.timng[i__ - 1], &info);
/* L70: */
				}

			    }

/*                       Loop for timing different block sizes. */

			    i__5 = *nnb;
			    for (inb = 1; inb <= i__5; ++inb) {
				nb = nbval[inb];
				xlaenv_(&c__1, &nb);
				xlaenv_(&c__3, &nxval[inb]);

				if (timsub[2]) {

/*                          Time SGELSY   

                            SGELSY:  Compute the minimum-norm solution X   
                            to min( norm( A * X - B ) ) using the   
                            rank-revealing orthogonal factorization.   

                            Set LWLSY to the adequate value.   

   Computing MAX */
				    i__6 = 1, i__7 = mnmin + (n << 1) + nb * (
					    n + 1), i__6 = max(i__6,i__7), 
					    i__7 = (mnmin << 1) + nb * nrhs;
				    lwlsy = max(i__6,i__7);

				    ncall = 0;
				    time = 0.f;
				    slaset_("Full", &ndata[2], &c__1, &c_b28, 
					    &c_b28, lstime_1.opcnt, &ndata[2]);
				    slaset_("Full", &ndata[2], &c__1, &c_b28, 
					    &c_b28, lstime_1.timng, &ndata[2]);
L80:
				    slacpy_("Full", &m, &n, &copya[1], &lda, &
					    a[1], &lda);
				    slacpy_("Full", &m, &nrhs, &copyb[1], &
					    ldb, &b[1], &ldb);
				    s_copy(srnamc_1.srnamt, "SGELSY", (ftnlen)
					    6, (ftnlen)6);
				    ++ncall;
				    s1 = second_();
				    sgelsy_(&m, &n, &nrhs, &a[1], &lda, &b[1],
					     &ldb, &iwork[1], &rcond, &crank, 
					    &work[1], &lwlsy, &info);
				    s2 = second_();
				    time += s2 - s1;
				    if (info == 0 && time < *timmin) {
					goto L80;
				    }
				    lstime_1.timng[0] = time;
				    lstime_1.opcnt[0] = sasum_(&ndata[2], 
					    lstime_1.opcnt, &c__1);
				    r__1 = 1.f / (real) ncall;
				    sscal_(&ndata[2], &r__1, lstime_1.opcnt, &
					    c__1);
				    r__1 = 1.f / (real) ncall;
				    sscal_(&ndata[2], &r__1, lstime_1.timng, &
					    c__1);
				    scopy_(&ndata[2], lstime_1.opcnt, &c__1, &
					    opctbl_ref(1, itype, nclsy + inb, 
					    3), &c__1);
				    scopy_(&ndata[2], lstime_1.timng, &c__1, &
					    timtbl_ref(1, itype, nclsy + inb, 
					    3), &c__1);
				    i__6 = ndata[2];
				    for (i__ = 1; i__ <= i__6; ++i__) {
					flptbl_ref(i__, itype, nclsy + inb, 3)
						 = smflop_(&lstime_1.opcnt[
						i__ - 1], &lstime_1.timng[i__ 
						- 1], &info);
/* L90: */
				    }

				}

				if (timsub[3]) {

/*                          Time SGELSS   

                            SGELSS:  Compute the minimum-norm solution X   
                            to min( norm( A * X - B ) ) using the SVD. */

				    ncall = 0;
				    time = 0.f;
				    slaset_("Full", &ndata[3], &c__1, &c_b28, 
					    &c_b28, lstime_1.opcnt, &ndata[3]);
				    slaset_("Full", &ndata[3], &c__1, &c_b28, 
					    &c_b28, lstime_1.timng, &ndata[3]);
L100:
				    slacpy_("Full", &m, &n, &copya[1], &lda, &
					    a[1], &lda);
				    slacpy_("Full", &m, &nrhs, &copyb[1], &
					    ldb, &b[1], &ldb);
				    s_copy(srnamc_1.srnamt, "SGELSS", (ftnlen)
					    6, (ftnlen)6);
				    ++ncall;
				    s1 = second_();
				    sgelss_(&m, &n, &nrhs, &a[1], &lda, &b[1],
					     &ldb, &s[1], &rcond, &crank, &
					    work[1], &lwork, &info);
				    s2 = second_();
				    time += s2 - s1;
				    if (info == 0 && time < *timmin) {
					goto L100;
				    }
				    lstime_1.timng[0] = time;
				    lstime_1.opcnt[0] = sasum_(&ndata[3], 
					    lstime_1.opcnt, &c__1);
				    r__1 = 1.f / (real) ncall;
				    sscal_(&ndata[3], &r__1, lstime_1.opcnt, &
					    c__1);
				    r__1 = 1.f / (real) ncall;
				    sscal_(&ndata[3], &r__1, lstime_1.timng, &
					    c__1);
				    scopy_(&ndata[3], lstime_1.opcnt, &c__1, &
					    opctbl_ref(1, itype, nclss + inb, 
					    4), &c__1);
				    scopy_(&ndata[3], lstime_1.timng, &c__1, &
					    timtbl_ref(1, itype, nclss + inb, 
					    4), &c__1);
				    i__6 = ndata[3];
				    for (i__ = 1; i__ <= i__6; ++i__) {
					flptbl_ref(i__, itype, nclss + inb, 4)
						 = smflop_(&lstime_1.opcnt[
						i__ - 1], &lstime_1.timng[i__ 
						- 1], &info);
/* L110: */
				    }

				}

				if (timsub[4]) {

/*                          Time SGELSD   

                            SGELSD:  Compute the minimum-norm solution X   
                            to min( norm( A * X - B ) ) using a   
                            divide-and-conquer SVD. */

				    ncall = 0;
				    time = 0.f;
				    slaset_("Full", &ndata[4], &c__1, &c_b28, 
					    &c_b28, lstime_1.opcnt, &ndata[4]);
				    slaset_("Full", &ndata[4], &c__1, &c_b28, 
					    &c_b28, lstime_1.timng, &ndata[4]);
L120:
				    slacpy_("Full", &m, &n, &copya[1], &lda, &
					    a[1], &lda);
				    slacpy_("Full", &m, &nrhs, &copyb[1], &
					    ldb, &b[1], &ldb);
				    s_copy(srnamc_1.srnamt, "SGELSD", (ftnlen)
					    6, (ftnlen)6);
				    ++ncall;
				    s1 = second_();
				    sgelsd_(&m, &n, &nrhs, &a[1], &lda, &b[1],
					     &ldb, &s[1], &rcond, &crank, &
					    work[1], &lwork, &iwork[1], &info)
					    ;
				    s2 = second_();
				    time += s2 - s1;
				    if (info == 0 && time < *timmin) {
					goto L120;
				    }
				    lstime_1.timng[0] = time;
				    lstime_1.opcnt[0] = sasum_(&ndata[4], 
					    lstime_1.opcnt, &c__1);
				    r__1 = 1.f / (real) ncall;
				    sscal_(&ndata[4], &r__1, lstime_1.opcnt, &
					    c__1);
				    r__1 = 1.f / (real) ncall;
				    sscal_(&ndata[4], &r__1, lstime_1.timng, &
					    c__1);
				    scopy_(&ndata[4], lstime_1.opcnt, &c__1, &
					    opctbl_ref(1, itype, nclsd + inb, 
					    5), &c__1);
				    scopy_(&ndata[4], lstime_1.timng, &c__1, &
					    timtbl_ref(1, itype, nclsd + inb, 
					    5), &c__1);
				    i__6 = ndata[4];
				    for (i__ = 1; i__ <= i__6; ++i__) {
					flptbl_ref(i__, itype, nclsd + inb, 5)
						 = smflop_(&lstime_1.opcnt[
						i__ - 1], &lstime_1.timng[i__ 
						- 1], &info);
/* L130: */
				    }

				}

/* L140: */
			    }
/* L150: */
			}
/* L160: */
		    }
		    ncls += *nnb;
		    nclsy += *nnb;
		    nclss += *nnb;
		    nclsd += *nnb;
/* L170: */
		}
		++nclsx;
/* L180: */
	    }
/* L190: */
	}
/* L200: */
    }

/*     Print a summary of the results. */

    for (isub = 1; isub <= 5; ++isub) {
	if (timsub[isub - 1]) {
	    io___48.ciunit = *nout;
	    s_wsfe(&io___48);
	    do_fio(&c__1, subnam_ref(0, isub), (ftnlen)6);
	    e_wsfe();
	    if (isub == 1) {
		io___49.ciunit = *nout;
		s_wsfe(&io___49);
		e_wsfe();
	    } else if (isub == 2) {
		io___50.ciunit = *nout;
		s_wsfe(&io___50);
		e_wsfe();
	    } else if (isub == 3) {
		io___51.ciunit = *nout;
		s_wsfe(&io___51);
		e_wsfe();
	    } else if (isub == 4) {
		io___52.ciunit = *nout;
		s_wsfe(&io___52);
		e_wsfe();
	    } else if (isub == 5) {
		io___53.ciunit = *nout;
		s_wsfe(&io___53);
		e_wsfe();
	    }
	    for (itbl = 1; itbl <= 3; ++itbl) {
		if (itbl == 1) {
		    io___55.ciunit = *nout;
		    s_wsfe(&io___55);
		    e_wsfe();
		    sprtls_(&isub, subnam_ref(0, isub), &ndata[isub - 1], nm, 
			    &mval[1], nn, &nval[1], nns, &nsval[1], nnb, &
			    nbval[1], &nxval[1], nlda, &ldaval[1], &c__6, &
			    timtbl_ref(1, 1, 1, isub), nout);
		} else if (itbl == 2) {
		    io___56.ciunit = *nout;
		    s_wsfe(&io___56);
		    e_wsfe();
		    sprtls_(&isub, subnam_ref(0, isub), &ndata[isub - 1], nm, 
			    &mval[1], nn, &nval[1], nns, &nsval[1], nnb, &
			    nbval[1], &nxval[1], nlda, &ldaval[1], &c__6, &
			    opctbl_ref(1, 1, 1, isub), nout);
		} else if (itbl == 3) {
		    io___57.ciunit = *nout;
		    s_wsfe(&io___57);
		    e_wsfe();
		    sprtls_(&isub, subnam_ref(0, isub), &ndata[isub - 1], nm, 
			    &mval[1], nn, &nval[1], nns, &nsval[1], nnb, &
			    nbval[1], &nxval[1], nlda, &ldaval[1], &c__6, &
			    flptbl_ref(1, 1, 1, isub), nout);
		}
/* L210: */
	    }
	}
/* L220: */
    }

L230:
    return 0;

/*     End of STIMLS */

} /* stimls_ */

#undef timtbl_ref
#undef subnam_ref
#undef flptbl_ref
#undef opctbl_ref


