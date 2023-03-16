#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__1 = 1;
static integer c__7 = 7;
static integer c__5 = 5;
static integer c__20 = 20;

/* Subroutine */ int zchkgl_(integer *nin, integer *nout)
{
    /* Format strings */
    static char fmt_9999[] = "(\002 .. test output of ZGGBAL .. \002)";
    static char fmt_9998[] = "(\002 ratio of largest test error             "
	    " = \002,d12.3)";
    static char fmt_9997[] = "(\002 example number where info is not zero   "
	    " = \002,i4)";
    static char fmt_9996[] = "(\002 example number where ILO or IHI is wrong"
	    " = \002,i4)";
    static char fmt_9995[] = "(\002 example number having largest error     "
	    " = \002,i4)";
    static char fmt_9994[] = "(\002 number of examples where info is not 0  "
	    " = \002,i4)";
    static char fmt_9993[] = "(\002 total number of examples tested         "
	    " = \002,i4)";

    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    doublereal d__1, d__2, d__3;
    doublecomplex z__1;

    /* Builtin functions */
    integer s_rsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_rsle(void);
    double z_abs(doublecomplex *);
    integer s_wsfe(cilist *), e_wsfe(void), do_fio(integer *, char *, ftnlen);

    /* Local variables */
    static integer info, lmax[3];
    static doublereal rmax, vmax, work[120];
    static doublecomplex a[400]	/* was [20][20] */, b[400]	/* was [20][
	    20] */;
    static integer i__, j, n, ihiin, ninfo, iloin;
    static doublereal anorm, bnorm;
    extern doublereal dlamch_(char *);
    static doublereal lscale[20];
    extern /* Subroutine */ int zggbal_(char *, integer *, doublecomplex *, 
	    integer *, doublecomplex *, integer *, integer *, integer *, 
	    doublereal *, doublereal *, doublereal *, integer *);
    static doublereal rscale[20];
    extern doublereal zlange_(char *, integer *, integer *, doublecomplex *, 
	    integer *, doublereal *);
    static doublereal lsclin[20], rsclin[20];
    static doublecomplex ain[400]	/* was [20][20] */, bin[400]	/* 
	    was [20][20] */;
    static integer ihi, ilo;
    static doublereal eps;
    static integer knt;

    /* Fortran I/O blocks */
    static cilist io___6 = { 0, 0, 0, 0, 0 };
    static cilist io___9 = { 0, 0, 0, 0, 0 };
    static cilist io___12 = { 0, 0, 0, 0, 0 };
    static cilist io___14 = { 0, 0, 0, 0, 0 };
    static cilist io___17 = { 0, 0, 0, 0, 0 };
    static cilist io___19 = { 0, 0, 0, 0, 0 };
    static cilist io___21 = { 0, 0, 0, 0, 0 };
    static cilist io___23 = { 0, 0, 0, 0, 0 };
    static cilist io___34 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___35 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___36 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___37 = { 0, 0, 0, fmt_9996, 0 };
    static cilist io___38 = { 0, 0, 0, fmt_9995, 0 };
    static cilist io___39 = { 0, 0, 0, fmt_9994, 0 };
    static cilist io___40 = { 0, 0, 0, fmt_9993, 0 };



#define a_subscr(a_1,a_2) (a_2)*20 + a_1 - 21
#define a_ref(a_1,a_2) a[a_subscr(a_1,a_2)]
#define b_subscr(a_1,a_2) (a_2)*20 + a_1 - 21
#define b_ref(a_1,a_2) b[b_subscr(a_1,a_2)]
#define ain_subscr(a_1,a_2) (a_2)*20 + a_1 - 21
#define ain_ref(a_1,a_2) ain[ain_subscr(a_1,a_2)]
#define bin_subscr(a_1,a_2) (a_2)*20 + a_1 - 21
#define bin_ref(a_1,a_2) bin[bin_subscr(a_1,a_2)]


/*  -- LAPACK test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    ZCHKGL tests ZGGBAL, a routine for balancing a matrix pair (A, B).   

    Arguments   
    =========   

    NIN     (input) INTEGER   
            The logical unit number for input.  NIN > 0.   

    NOUT    (input) INTEGER   
            The logical unit number for output.  NOUT > 0.   

    ===================================================================== */


    lmax[0] = 0;
    lmax[1] = 0;
    lmax[2] = 0;
    ninfo = 0;
    knt = 0;
    rmax = 0.;

    eps = dlamch_("Precision");

L10:

    io___6.ciunit = *nin;
    s_rsle(&io___6);
    do_lio(&c__3, &c__1, (char *)&n, (ftnlen)sizeof(integer));
    e_rsle();
    if (n == 0) {
	goto L90;
    }
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	io___9.ciunit = *nin;
	s_rsle(&io___9);
	i__2 = n;
	for (j = 1; j <= i__2; ++j) {
	    do_lio(&c__7, &c__1, (char *)&a_ref(i__, j), (ftnlen)sizeof(
		    doublecomplex));
	}
	e_rsle();
/* L20: */
    }

    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	io___12.ciunit = *nin;
	s_rsle(&io___12);
	i__2 = n;
	for (j = 1; j <= i__2; ++j) {
	    do_lio(&c__7, &c__1, (char *)&b_ref(i__, j), (ftnlen)sizeof(
		    doublecomplex));
	}
	e_rsle();
/* L30: */
    }

    io___14.ciunit = *nin;
    s_rsle(&io___14);
    do_lio(&c__3, &c__1, (char *)&iloin, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&ihiin, (ftnlen)sizeof(integer));
    e_rsle();
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	io___17.ciunit = *nin;
	s_rsle(&io___17);
	i__2 = n;
	for (j = 1; j <= i__2; ++j) {
	    do_lio(&c__7, &c__1, (char *)&ain_ref(i__, j), (ftnlen)sizeof(
		    doublecomplex));
	}
	e_rsle();
/* L40: */
    }
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	io___19.ciunit = *nin;
	s_rsle(&io___19);
	i__2 = n;
	for (j = 1; j <= i__2; ++j) {
	    do_lio(&c__7, &c__1, (char *)&bin_ref(i__, j), (ftnlen)sizeof(
		    doublecomplex));
	}
	e_rsle();
/* L50: */
    }

    io___21.ciunit = *nin;
    s_rsle(&io___21);
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	do_lio(&c__5, &c__1, (char *)&lsclin[i__ - 1], (ftnlen)sizeof(
		doublereal));
    }
    e_rsle();
    io___23.ciunit = *nin;
    s_rsle(&io___23);
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	do_lio(&c__5, &c__1, (char *)&rsclin[i__ - 1], (ftnlen)sizeof(
		doublereal));
    }
    e_rsle();

    anorm = zlange_("M", &n, &n, a, &c__20, work);
    bnorm = zlange_("M", &n, &n, b, &c__20, work);

    ++knt;

    zggbal_("B", &n, a, &c__20, b, &c__20, &ilo, &ihi, lscale, rscale, work, &
	    info);

    if (info != 0) {
	++ninfo;
	lmax[0] = knt;
    }

    if (ilo != iloin || ihi != ihiin) {
	++ninfo;
	lmax[1] = knt;
    }

    vmax = 0.;
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = n;
	for (j = 1; j <= i__2; ++j) {
/* Computing MAX */
	    i__3 = a_subscr(i__, j);
	    i__4 = ain_subscr(i__, j);
	    z__1.r = a[i__3].r - ain[i__4].r, z__1.i = a[i__3].i - ain[i__4]
		    .i;
	    d__1 = vmax, d__2 = z_abs(&z__1);
	    vmax = max(d__1,d__2);
/* Computing MAX */
	    i__3 = b_subscr(i__, j);
	    i__4 = bin_subscr(i__, j);
	    z__1.r = b[i__3].r - bin[i__4].r, z__1.i = b[i__3].i - bin[i__4]
		    .i;
	    d__1 = vmax, d__2 = z_abs(&z__1);
	    vmax = max(d__1,d__2);
/* L60: */
	}
/* L70: */
    }

    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* Computing MAX */
	d__2 = vmax, d__3 = (d__1 = lscale[i__ - 1] - lsclin[i__ - 1], abs(
		d__1));
	vmax = max(d__2,d__3);
/* Computing MAX */
	d__2 = vmax, d__3 = (d__1 = rscale[i__ - 1] - rsclin[i__ - 1], abs(
		d__1));
	vmax = max(d__2,d__3);
/* L80: */
    }

    vmax /= eps * max(anorm,bnorm);

    if (vmax > rmax) {
	lmax[2] = knt;
	rmax = vmax;
    }

    goto L10;

L90:

    io___34.ciunit = *nout;
    s_wsfe(&io___34);
    e_wsfe();

    io___35.ciunit = *nout;
    s_wsfe(&io___35);
    do_fio(&c__1, (char *)&rmax, (ftnlen)sizeof(doublereal));
    e_wsfe();
    io___36.ciunit = *nout;
    s_wsfe(&io___36);
    do_fio(&c__1, (char *)&lmax[0], (ftnlen)sizeof(integer));
    e_wsfe();
    io___37.ciunit = *nout;
    s_wsfe(&io___37);
    do_fio(&c__1, (char *)&lmax[1], (ftnlen)sizeof(integer));
    e_wsfe();
    io___38.ciunit = *nout;
    s_wsfe(&io___38);
    do_fio(&c__1, (char *)&lmax[2], (ftnlen)sizeof(integer));
    e_wsfe();
    io___39.ciunit = *nout;
    s_wsfe(&io___39);
    do_fio(&c__1, (char *)&ninfo, (ftnlen)sizeof(integer));
    e_wsfe();
    io___40.ciunit = *nout;
    s_wsfe(&io___40);
    do_fio(&c__1, (char *)&knt, (ftnlen)sizeof(integer));
    e_wsfe();

    return 0;

/*     End of ZCHKGL */

} /* zchkgl_ */

#undef bin_ref
#undef bin_subscr
#undef ain_ref
#undef ain_subscr
#undef b_ref
#undef b_subscr
#undef a_ref
#undef a_subscr


