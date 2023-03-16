#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    integer infot, iounit;
    logical ok, lerr;
} infoc_;

#define infoc_1 infoc_

struct {
    doublereal opcnt[6], timng[6];
} lstime_;

#define lstime_1 lstime_

struct {
    char srnamt[6];
} srnamc_;

#define srnamc_1 srnamc_

/* Table of constant values */

static doublecomplex c_b1 = {1.,0.};
static doublecomplex c_b2 = {0.,0.};
static integer c__5 = 5;
static integer c__2 = 2;
static integer c__1 = 1;
static integer c__3 = 3;
static doublereal c_b30 = 0.;
static integer c__9 = 9;
static integer c__25 = 25;
static integer c__6 = 6;

/* Subroutine */ int ztimls_(char *line, integer *nm, integer *mval, integer *
	nn, integer *nval, integer *nns, integer *nsval, integer *nnb, 
	integer *nbval, integer *nxval, integer *nlda, integer *ldaval, 
	doublereal *timmin, doublecomplex *a, doublecomplex *copya, 
	doublecomplex *b, doublecomplex *copyb, doublereal *s, doublereal *
	copys, doublereal *opctbl, doublereal *timtbl, doublereal *flptbl, 
	doublecomplex *work, doublereal *rwork, integer *iwork, integer *nout,
	 ftnlen line_len)
{
    /* Initialized data */

    static char subnam[6*5] = "ZGELS " "ZGELSX" "ZGELSY" "ZGELSS" "ZGELSD";
    static integer iseedy[4] = { 1988,1989,1990,1991 };
    static integer ndata[5] = { 4,6,6,6,5 };

    /* Format strings */
    static char fmt_9999[] = "(///\002 ****** Results for \002,a,\002 *****"
	    "*\002)";
    static char fmt_9998[] = "(/\002 ZGELS   : overall performance\002,/\002"
	    " comp. 1 : if M>=N, ZGEQRF, QR factorization\002,/\002          "
	    " if M< N, ZGELQF, QR factorization\002,/\002 comp. 2 : if M>=N, "
	    "ZUNMQR, multiplication by\002,\002 reflectors\002,/\002         "
	    "  if M< N, ZUNMLQ, multiplication by\002,\002 reflectors\002,"
	    "/\002 comp. 3 : ZTRSM, solution of the triangular\002,\002 system"
	    "\002,//\002 Types 4 to 6 are the conjugate transpose\002,\002 of"
	    " types 1 to 3\002)";
    static char fmt_9997[] = "(/\002 ZGELSX  : overall performance\002,/\002"
	    " comp. 1 : ZGEQPF, QR factorization with column\002,\002 pivoting"
	    "\002,/\002 comp. 2 : if RANK<N, ZTZRQF, reduction to\002,\002 tr"
	    "iangular form\002,/\002 comp. 3 : ZUNM2R, multiplication by refl"
	    "ectors\002,/\002 comp. 4 : ZTRSM, solution of the triangular\002,"
	    "\002 system\002,/\002 comp. 5 : if RANK<N, ZLATZM, multiplicatio"
	    "n by\002,\002 reflectors\002)";
    static char fmt_9996[] = "(/\002 ZGELSY  : overall performance\002,/\002"
	    " comp. 1 : ZGEQP3, QR factorization with column\002,\002 pivoting"
	    "\002,/\002 comp. 2 : if RANK<N, ZTZRZF, reduction to\002,\002 tr"
	    "iangular form\002,/\002 comp. 3 : ZUNMQR, multiplication by refl"
	    "ectors\002,/\002 comp. 4 : ZTRSM, solution of the triangular\002,"
	    "\002 system\002,/\002 comp. 5 : if RANK<N, ZUNMRZ, multiplicatio"
	    "n by\002,\002 reflectors\002)";
    static char fmt_9995[] = "(/\002 ZGELSS: overall performance\002,/\002 c"
	    "omp. 1 : if M>>N, ZGEQRF, QR factorization\002,/\002            "
	    "        ZUNMQR, multiplication by\002,\002 reflectors\002,/\002 "
	    "          if N>>M, ZGELQF, QL factorization\002,/\002 comp. 2 : "
	    "ZGEBRD, reduction to bidiagonal form\002,/\002 comp. 3 : ZUNMBR,"
	    " multiplication by left\002,\002 bidiagonalizing vectors\002,"
	    "/\002           ZUNGBR, generation of right\002,\002 bidiagonali"
	    "zing vectors\002,/\002 comp. 4 : ZBDSQR, singular value decompos"
	    "ition\002,\002 of the bidiagonal matrix\002,/\002 comp. 5 : mult"
	    "iplication by right bidiagonalizing\002,\002 vectors\002,/\002  "
	    "         (ZGEMM or CGEMV, and ZUNMLQ if N>>M)\002)";
    static char fmt_9994[] = "(/\002 ZGELSD: overall performance\002,/\002 c"
	    "omp. 1 : if M>>N, ZGEQRF, QR factorization\002,/\002            "
	    "        ZUNMQR, multiplication by\002,\002 reflectors\002,/\002 "
	    "          if N>>M, ZGELQF, QL factorization\002,/\002 comp. 2 : "
	    "ZGEBRD, reduction to bidiagonal form\002,/\002 comp. 3 : ZUNMBR,"
	    " multiplication by left \002,\002 bidiagonalizing vectors\002,"
	    "/\002                   multiplication by right\002,\002 bidiago"
	    "nalizing vectors\002,/\002 comp. 4 : ZLALSD, singular value deco"
	    "mposition\002,\002 of the bidiagonal matrix\002)";
    static char fmt_9993[] = "(//\002 *** Time in seconds *** \002)";
    static char fmt_9992[] = "(//\002 *** Number of floating-point operation"
	    "s *** \002)";
    static char fmt_9991[] = "(//\002 *** Speed in megaflops *** \002)";

    /* System generated locals */
    integer flptbl_dim3, flptbl_offset, opctbl_dim3, opctbl_offset, 
	    timtbl_dim3, timtbl_offset, i__1, i__2, i__3, i__4, i__5, i__6, 
	    i__7;
    doublereal d__1;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    double sqrt(doublereal);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static integer ilda, itbl, info;
    static char path[3];
    static integer rank;
    static doublereal time;
    static integer ncls, isub, nrhs, i__, m, n;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *, 
	    integer *);
    static integer ncall, iseed[4], crank, nclsd, irank;
    static doublereal rcond;
    extern doublereal dasum_(integer *, doublereal *, integer *);
    static integer itran, mnmin, ncols, nclss;
    static doublereal norma;
    static char trans[1];
    static integer nclsx, itype, lwork, nclsy;
    static doublereal normb;
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *, 
	    doublereal *, integer *), zgels_(char *, integer *, integer *, 
	    integer *, doublecomplex *, integer *, doublecomplex *, integer *,
	     doublecomplex *, integer *, integer *);
    static doublereal s1, s2;
    extern /* Subroutine */ int zgemm_(char *, char *, integer *, integer *, 
	    integer *, doublecomplex *, doublecomplex *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, doublecomplex *, 
	    integer *);
    static integer nrows;
    extern /* Subroutine */ int zqrt13_(integer *, integer *, integer *, 
	    doublecomplex *, integer *, doublereal *, integer *);
    static integer lwlsy;
    extern /* Subroutine */ int zqrt15_(integer *, integer *, integer *, 
	    integer *, integer *, doublecomplex *, integer *, doublecomplex *,
	     integer *, doublereal *, integer *, doublereal *, doublereal *, 
	    integer *, doublecomplex *, integer *);
    static integer nb, im, in;
    extern doublereal dlamch_(char *);
    static integer iscale;
    extern doublereal dsecnd_(void);
    extern /* Subroutine */ int dlaset_(char *, integer *, integer *, 
	    doublereal *, doublereal *, doublereal *, integer *), 
	    zdscal_(integer *, doublereal *, doublecomplex *, integer *);
    extern doublereal dmflop_(doublereal *, doublereal *, integer *);
    extern /* Subroutine */ int atimin_(char *, char *, integer *, char *, 
	    logical *, integer *, integer *, ftnlen, ftnlen, ftnlen), zgelsd_(
	    integer *, integer *, integer *, doublecomplex *, integer *, 
	    doublecomplex *, integer *, doublereal *, doublereal *, integer *,
	     doublecomplex *, integer *, doublereal *, integer *, integer *), 
	    xlaenv_(integer *, integer *);
    static integer ldwork;
    static logical timsub[5];
    extern /* Subroutine */ int zlacpy_(char *, integer *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *), 
	    dprtls_(integer *, char *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, doublereal *, integer 
	    *), zgelss_(integer *, integer *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublereal *, doublereal *, integer *, doublecomplex *, integer *,
	     doublereal *, integer *), zgelsx_(integer *, integer *, integer *
	    , doublecomplex *, integer *, doublecomplex *, integer *, integer 
	    *, doublereal *, integer *, doublecomplex *, doublereal *, 
	    integer *), zgelsy_(integer *, integer *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *, integer *,
	     doublereal *, integer *, doublecomplex *, integer *, doublereal *
	    , integer *), zlarnv_(integer *, integer *, integer *, 
	    doublecomplex *);
    static integer lda, ldb, inb;
    static doublereal eps;
    static integer ins;

    /* Fortran I/O blocks */
    static cilist io___47 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___48 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___49 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___50 = { 0, 0, 0, fmt_9996, 0 };
    static cilist io___51 = { 0, 0, 0, fmt_9995, 0 };
    static cilist io___52 = { 0, 0, 0, fmt_9994, 0 };
    static cilist io___54 = { 0, 0, 0, fmt_9993, 0 };
    static cilist io___55 = { 0, 0, 0, fmt_9992, 0 };
    static cilist io___56 = { 0, 0, 0, fmt_9991, 0 };



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

    ZTIMLS times the least squares driver routines ZGELS, CGELSS, CGELSX,   
    ZGELSY and CGELSD.   

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

    TIMMIN  (input) DOUBLE PRECISION   
            The minimum time a subroutine will be timed.   

    A       (workspace) COMPLEX*16 array, dimension (MMAX*NMAX)   
            where MMAX is the maximum value of M in MVAL and NMAX is the   
            maximum value of N in NVAL.   

    COPYA   (workspace) COMPLEX*16 array, dimension (MMAX*NMAX)   

    B       (workspace) COMPLEX*16 array, dimension (MMAX*NSMAX)   
            where MMAX is the maximum value of M in MVAL and NSMAX is the   
            maximum value of NRHS in NSVAL.   

    COPYB   (workspace) COMPLEX*16 array, dimension (MMAX*NSMAX)   

    S       (workspace) DOUBLE PRECISION array, dimension   
                        (min(MMAX,NMAX))   

    COPYS   (workspace) DOUBLE PRECISION array, dimension   
                        (min(MMAX,NMAX))   

    OPZTBL  (workspace) DOUBLE PRECISION array, dimension   
                        (6,6,(NNB+1)*NLDA,NM*NN*NNS,5)   

    TIMTBL  (workspace) DOUBLE PRECISION array, dimension   
                        (6,6,(NNB+1)*NLDA,NM*NN*NNS,5)   

    FLPTBL  (workspace) DOUBLE PRECISION array, dimension   
                        (6,6,(NNB+1)*NLDA,NM*NN*NNS,5)   

    WORK    (workspace) COMPLEX*16 array,   
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
    --rwork;
    --iwork;

    /* Function Body   

       Extract the timing request from the input line. */

    s_copy(path, "Zomplex precision", (ftnlen)1, (ftnlen)17);
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
    eps = dlamch_("Epsilon");

/*     Threshold for rank estimation */

    rcond = sqrt(eps) - (sqrt(eps) - eps) / 2;

    infoc_1.infot = 0;
    xlaenv_(&c__2, &c__2);

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
/* Computing MAX */
		i__4 = 1, i__5 = (m + nrhs) * (n + 2), i__4 = max(i__4,i__5), 
			i__5 = (n + nrhs) * (m + 2), i__4 = max(i__4,i__5), 
			i__5 = m * n + (mnmin << 2) + max(m,n), i__4 = max(
			i__4,i__5), i__5 = (n << 1) + m;
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

/*                          Time ZGELS   

                            Generate a matrix of scaling type ISCALE */

				zqrt13_(&iscale, &m, &n, &copya[1], &lda, &
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
					    *(unsigned char *)trans = 'C';
					    nrows = n;
					    ncols = m;
					}
					ldwork = max(1,ncols);

/*                                Set up a consistent rhs */

					if (ncols > 0) {
					    i__6 = ncols * nrhs;
					    zlarnv_(&c__2, iseed, &i__6, &
						    work[1]);
					    i__6 = ncols * nrhs;
					    d__1 = 1. / (doublereal) ncols;
					    zdscal_(&i__6, &d__1, &work[1], &
						    c__1);
					}
					zgemm_(trans, "No transpose", &nrows, 
						&nrhs, &ncols, &c_b1, &copya[
						1], &lda, &work[1], &ldwork, &
						c_b2, &b[1], &ldb);
					zlacpy_("Full", &nrows, &nrhs, &b[1], 
						&ldb, &copyb[1], &ldb);

/*                                Solve LS or overdetermined system */

					ncall = 0;
					time = 0.;
					dlaset_("Full", ndata, &c__1, &c_b30, 
						&c_b30, lstime_1.opcnt, ndata);
					dlaset_("Full", ndata, &c__1, &c_b30, 
						&c_b30, lstime_1.timng, ndata);
L20:
					if (m > 0 && n > 0) {
					    zlacpy_("Full", &m, &n, &copya[1],
						     &lda, &a[1], &lda);
					    zlacpy_("Full", &nrows, &nrhs, &
						    copyb[1], &ldb, &b[1], &
						    ldb);
					}
					s_copy(srnamc_1.srnamt, "ZGELS ", (
						ftnlen)6, (ftnlen)6);
					++ncall;
					s1 = dsecnd_();
					zgels_(trans, &m, &n, &nrhs, &a[1], &
						lda, &b[1], &ldb, &work[1], &
						lwork, &info);
					s2 = dsecnd_();
					time += s2 - s1;
					if (info == 0 && time < *timmin) {
					    goto L20;
					}
					lstime_1.timng[0] = time;
					lstime_1.opcnt[0] = dasum_(ndata, 
						lstime_1.opcnt, &c__1);
					d__1 = 1. / (doublereal) ncall;
					dscal_(ndata, &d__1, lstime_1.opcnt, &
						c__1);
					d__1 = 1. / (doublereal) ncall;
					dscal_(ndata, &d__1, lstime_1.timng, &
						c__1);
					dcopy_(ndata, lstime_1.opcnt, &c__1, &
						opctbl_ref(1, itype, ncls + 
						inb, 1), &c__1);
					dcopy_(ndata, lstime_1.timng, &c__1, &
						timtbl_ref(1, itype, ncls + 
						inb, 1), &c__1);
					i__6 = ndata[0];
					for (i__ = 1; i__ <= i__6; ++i__) {
					    flptbl_ref(i__, itype, ncls + inb,
						     1) = dmflop_(&
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
			    zqrt15_(&iscale, &irank, &m, &n, &nrhs, &copya[1],
				     &lda, &copyb[1], &ldb, &copys[1], &rank, 
				    &norma, &normb, iseed, &work[1], &lwork);

			    if (timsub[1]) {

/*                       Time ZGELSX   

                         workspace used:   
                         MAX(M+MIN(M,N),NRHS*MIN(M,N),2*N+M) */

				ldwork = max(1,m);

/*                       ZGELSX:  Compute the minimum-norm   
                         solution X to min( norm( A * X - B ) )   
                         using a complete orthogonal factorization. */

				ncall = 0;
				time = 0.;
				dlaset_("Full", &ndata[1], &c__1, &c_b30, &
					c_b30, lstime_1.opcnt, &ndata[1]);
				dlaset_("Full", &ndata[1], &c__1, &c_b30, &
					c_b30, lstime_1.timng, &ndata[1]);
L60:
				zlacpy_("Full", &m, &n, &copya[1], &lda, &a[1]
					, &lda);
				zlacpy_("Full", &m, &nrhs, &copyb[1], &ldb, &
					b[1], &ldb);
				s_copy(srnamc_1.srnamt, "ZGELSX", (ftnlen)6, (
					ftnlen)6);
				++ncall;
				s1 = dsecnd_();
				zgelsx_(&m, &n, &nrhs, &a[1], &lda, &b[1], &
					ldb, &iwork[1], &rcond, &crank, &work[
					1], &rwork[1], &info);
				s2 = dsecnd_();
				time += s2 - s1;
				if (info == 0 && time < *timmin) {
				    goto L60;
				}
				lstime_1.timng[0] = time;
				lstime_1.opcnt[0] = dasum_(&ndata[1], 
					lstime_1.opcnt, &c__1);
				d__1 = 1. / (doublereal) ncall;
				dscal_(&ndata[1], &d__1, lstime_1.opcnt, &
					c__1);
				d__1 = 1. / (doublereal) ncall;
				dscal_(&ndata[1], &d__1, lstime_1.timng, &
					c__1);
				dcopy_(&ndata[1], lstime_1.opcnt, &c__1, &
					opctbl_ref(1, itype, nclsx + 1, 2), &
					c__1);
				dcopy_(&ndata[1], lstime_1.timng, &c__1, &
					timtbl_ref(1, itype, nclsx + 1, 2), &
					c__1);
				i__5 = ndata[1];
				for (i__ = 1; i__ <= i__5; ++i__) {
				    flptbl_ref(i__, itype, nclsx + 1, 2) = 
					    dmflop_(&lstime_1.opcnt[i__ - 1], 
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

/*                          Time ZGELSY   

                            ZGELSY:  Compute the minimum-norm solution X   
                            to min( norm( A * X - B ) ) using the   
                            rank-revealing orthogonal factorization.   

                            Set LWLSY to the adequate value.   

   Computing MAX */
				    i__6 = 1, i__7 = mnmin + (n << 1) + nb * (
					    n + 1), i__6 = max(i__6,i__7), 
					    i__7 = (mnmin << 1) + nb * nrhs;
				    lwlsy = max(i__6,i__7);

				    ncall = 0;
				    time = 0.;
				    dlaset_("Full", &ndata[2], &c__1, &c_b30, 
					    &c_b30, lstime_1.opcnt, &ndata[2]);
				    dlaset_("Full", &ndata[2], &c__1, &c_b30, 
					    &c_b30, lstime_1.timng, &ndata[2]);
L80:
				    zlacpy_("Full", &m, &n, &copya[1], &lda, &
					    a[1], &lda);
				    zlacpy_("Full", &m, &nrhs, &copyb[1], &
					    ldb, &b[1], &ldb);
				    s_copy(srnamc_1.srnamt, "ZGELSY", (ftnlen)
					    6, (ftnlen)6);
				    ++ncall;
				    s1 = dsecnd_();
				    zgelsy_(&m, &n, &nrhs, &a[1], &lda, &b[1],
					     &ldb, &iwork[1], &rcond, &crank, 
					    &work[1], &lwlsy, &rwork[1], &
					    info);
				    s2 = dsecnd_();
				    time += s2 - s1;
				    if (info == 0 && time < *timmin) {
					goto L80;
				    }
				    lstime_1.timng[0] = time;
				    lstime_1.opcnt[0] = dasum_(&ndata[2], 
					    lstime_1.opcnt, &c__1);
				    d__1 = 1. / (doublereal) ncall;
				    dscal_(&ndata[2], &d__1, lstime_1.opcnt, &
					    c__1);
				    d__1 = 1. / (doublereal) ncall;
				    dscal_(&ndata[2], &d__1, lstime_1.timng, &
					    c__1);
				    dcopy_(&ndata[2], lstime_1.opcnt, &c__1, &
					    opctbl_ref(1, itype, nclsy + inb, 
					    3), &c__1);
				    dcopy_(&ndata[2], lstime_1.timng, &c__1, &
					    timtbl_ref(1, itype, nclsy + inb, 
					    3), &c__1);
				    i__6 = ndata[2];
				    for (i__ = 1; i__ <= i__6; ++i__) {
					flptbl_ref(i__, itype, nclsy + inb, 3)
						 = dmflop_(&lstime_1.opcnt[
						i__ - 1], &lstime_1.timng[i__ 
						- 1], &info);
/* L90: */
				    }

				}

				if (timsub[3]) {

/*                          Time ZGELSS   

                            ZGELSS:  Compute the minimum-norm solution X   
                            to min( norm( A * X - B ) ) using the SVD. */

				    ncall = 0;
				    time = 0.;
				    dlaset_("Full", &ndata[3], &c__1, &c_b30, 
					    &c_b30, lstime_1.opcnt, &ndata[3]);
				    dlaset_("Full", &ndata[3], &c__1, &c_b30, 
					    &c_b30, lstime_1.timng, &ndata[3]);
L100:
				    zlacpy_("Full", &m, &n, &copya[1], &lda, &
					    a[1], &lda);
				    zlacpy_("Full", &m, &nrhs, &copyb[1], &
					    ldb, &b[1], &ldb);
				    s_copy(srnamc_1.srnamt, "ZGELSS", (ftnlen)
					    6, (ftnlen)6);
				    ++ncall;
				    s1 = dsecnd_();
				    zgelss_(&m, &n, &nrhs, &a[1], &lda, &b[1],
					     &ldb, &s[1], &rcond, &crank, &
					    work[1], &lwork, &rwork[1], &info)
					    ;
				    s2 = dsecnd_();
				    time += s2 - s1;
				    if (info == 0 && time < *timmin) {
					goto L100;
				    }
				    lstime_1.timng[0] = time;
				    lstime_1.opcnt[0] = dasum_(&ndata[3], 
					    lstime_1.opcnt, &c__1);
				    d__1 = 1. / (doublereal) ncall;
				    dscal_(&ndata[3], &d__1, lstime_1.opcnt, &
					    c__1);
				    d__1 = 1. / (doublereal) ncall;
				    dscal_(&ndata[3], &d__1, lstime_1.timng, &
					    c__1);
				    dcopy_(&ndata[3], lstime_1.opcnt, &c__1, &
					    opctbl_ref(1, itype, nclss + inb, 
					    4), &c__1);
				    dcopy_(&ndata[3], lstime_1.timng, &c__1, &
					    timtbl_ref(1, itype, nclss + inb, 
					    4), &c__1);
				    i__6 = ndata[3];
				    for (i__ = 1; i__ <= i__6; ++i__) {
					flptbl_ref(i__, itype, nclss + inb, 4)
						 = dmflop_(&lstime_1.opcnt[
						i__ - 1], &lstime_1.timng[i__ 
						- 1], &info);
/* L110: */
				    }

				}

				if (timsub[4]) {

/*                          Time ZGELSD   

                            ZGELSD:  Compute the minimum-norm solution X   
                            to min( norm( A * X - B ) ) using a   
                            divide-and-conquer SVD. */

				    xlaenv_(&c__9, &c__25);
				    ncall = 0;
				    time = 0.;
				    dlaset_("Full", &ndata[4], &c__1, &c_b30, 
					    &c_b30, lstime_1.opcnt, &ndata[4]);
				    dlaset_("Full", &ndata[4], &c__1, &c_b30, 
					    &c_b30, lstime_1.timng, &ndata[4]);
L120:
				    zlacpy_("Full", &m, &n, &copya[1], &lda, &
					    a[1], &lda);
				    zlacpy_("Full", &m, &nrhs, &copyb[1], &
					    ldb, &b[1], &ldb);
				    s_copy(srnamc_1.srnamt, "ZGELSD", (ftnlen)
					    6, (ftnlen)6);
				    ++ncall;
				    s1 = dsecnd_();
				    zgelsd_(&m, &n, &nrhs, &a[1], &lda, &b[1],
					     &ldb, &s[1], &rcond, &crank, &
					    work[1], &lwork, &rwork[1], &
					    iwork[1], &info);
				    s2 = dsecnd_();
				    time += s2 - s1;
				    if (info == 0 && time < *timmin) {
					goto L120;
				    }
				    lstime_1.timng[0] = time;
				    lstime_1.opcnt[0] = dasum_(&ndata[4], 
					    lstime_1.opcnt, &c__1);
				    d__1 = 1. / (doublereal) ncall;
				    dscal_(&ndata[4], &d__1, lstime_1.opcnt, &
					    c__1);
				    d__1 = 1. / (doublereal) ncall;
				    dscal_(&ndata[4], &d__1, lstime_1.timng, &
					    c__1);
				    dcopy_(&ndata[4], lstime_1.opcnt, &c__1, &
					    opctbl_ref(1, itype, nclsd + inb, 
					    5), &c__1);
				    dcopy_(&ndata[4], lstime_1.timng, &c__1, &
					    timtbl_ref(1, itype, nclsd + inb, 
					    5), &c__1);
				    i__6 = ndata[4];
				    for (i__ = 1; i__ <= i__6; ++i__) {
					flptbl_ref(i__, itype, nclsd + inb, 5)
						 = dmflop_(&lstime_1.opcnt[
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
	    io___47.ciunit = *nout;
	    s_wsfe(&io___47);
	    do_fio(&c__1, subnam_ref(0, isub), (ftnlen)6);
	    e_wsfe();
	    if (isub == 1) {
		io___48.ciunit = *nout;
		s_wsfe(&io___48);
		e_wsfe();
	    } else if (isub == 2) {
		io___49.ciunit = *nout;
		s_wsfe(&io___49);
		e_wsfe();
	    } else if (isub == 3) {
		io___50.ciunit = *nout;
		s_wsfe(&io___50);
		e_wsfe();
	    } else if (isub == 4) {
		io___51.ciunit = *nout;
		s_wsfe(&io___51);
		e_wsfe();
	    } else if (isub == 5) {
		io___52.ciunit = *nout;
		s_wsfe(&io___52);
		e_wsfe();
	    }
	    for (itbl = 1; itbl <= 3; ++itbl) {
		if (itbl == 1) {
		    io___54.ciunit = *nout;
		    s_wsfe(&io___54);
		    e_wsfe();
		    dprtls_(&isub, subnam_ref(0, isub), &ndata[isub - 1], nm, 
			    &mval[1], nn, &nval[1], nns, &nsval[1], nnb, &
			    nbval[1], &nxval[1], nlda, &ldaval[1], &c__6, &
			    timtbl_ref(1, 1, 1, isub), nout);
		} else if (itbl == 2) {
		    io___55.ciunit = *nout;
		    s_wsfe(&io___55);
		    e_wsfe();
		    dprtls_(&isub, subnam_ref(0, isub), &ndata[isub - 1], nm, 
			    &mval[1], nn, &nval[1], nns, &nsval[1], nnb, &
			    nbval[1], &nxval[1], nlda, &ldaval[1], &c__6, &
			    opctbl_ref(1, 1, 1, isub), nout);
		} else if (itbl == 3) {
		    io___56.ciunit = *nout;
		    s_wsfe(&io___56);
		    e_wsfe();
		    dprtls_(&isub, subnam_ref(0, isub), &ndata[isub - 1], nm, 
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

/*     End of ZTIMLS */

} /* ztimls_ */

#undef timtbl_ref
#undef subnam_ref
#undef flptbl_ref
#undef opctbl_ref


