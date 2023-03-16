#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static doublecomplex c_b8 = {-1.,0.};

/* Subroutine */ int zgefa_(doublecomplex *a, integer *lda, integer *n, 
	integer *ipvt, integer *info)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3, i__4;
    doublereal d__1, d__2;
    doublecomplex z__1;

    /* Builtin functions */
    void z_div(doublecomplex *, doublecomplex *, doublecomplex *);

    /* Local variables */
    static integer j, k, l;
    static doublecomplex t;
    extern /* Subroutine */ int zscal_(integer *, doublecomplex *, 
	    doublecomplex *, integer *), zaxpy_(integer *, doublecomplex *, 
	    doublecomplex *, integer *, doublecomplex *, integer *);
    extern integer izamax_(integer *, doublecomplex *, integer *);
    static integer kp1, nm1;


#define a_subscr(a_1,a_2) (a_2)*a_dim1 + a_1
#define a_ref(a_1,a_2) a[a_subscr(a_1,a_2)]


/*     ZGEFA FACTORS A COMPLEX*16 MATRIX BY GAUSSIAN ELIMINATION.   

       ZGEFA IS USUALLY CALLED BY ZGECO, BUT IT CAN BE CALLED   
       DIRECTLY WITH A SAVING IN TIME IF  RCOND  IS NOT NEEDED.   
       (TIME FOR ZGECO) = (1 + 9/N)*(TIME FOR ZGEFA) .   

       ON ENTRY   

          A       COMPLEX*16(LDA, N)   
                  THE MATRIX TO BE FACTORED.   

          LDA     INTEGER   
                  THE LEADING DIMENSION OF THE ARRAY  A .   

          N       INTEGER   
                  THE ORDER OF THE MATRIX  A .   

       ON RETURN   

          A       AN UPPER TRIANGULAR MATRIX AND THE MULTIPLIERS   
                  WHICH WERE USED TO OBTAIN IT.   
                  THE FACTORIZATION CAN BE WRITTEN  A = L*U  WHERE   
                  L  IS A PRODUCT OF PERMUTATION AND UNIT LOWER   
                  TRIANGULAR MATRICES AND  U  IS UPPER TRIANGULAR.   

          IPVT    INTEGER(N)   
                  AN INTEGER VECTOR OF PIVOT INDICES.   

          INFO    INTEGER   
                  = 0  NORMAL VALUE.   
                  = K  IF  U(K,K) .EQ. 0.0 .  THIS IS NOT AN ERROR   
                       CONDITION FOR THIS SUBROUTINE, BUT IT DOES   
                       INDICATE THAT ZGESL OR ZGEDI WILL DIVIDE BY ZERO   
                       IF CALLED.  USE  RCOND  IN ZGECO FOR A RELIABLE   
                       INDICATION OF SINGULARITY.   

       LINPACK. THIS VERSION DATED 08/14/78 .   
       CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.   

       SUBROUTINES AND FUNCTIONS   

       BLAS ZAXPY,ZSCAL,IZAMAX   
       FORTRAN DABS   

       INTERNAL VARIABLES   



       GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING   

       Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    --ipvt;

    /* Function Body */
    *info = 0;
    nm1 = *n - 1;
    if (nm1 < 1) {
	goto L70;
    }
    i__1 = nm1;
    for (k = 1; k <= i__1; ++k) {
	kp1 = k + 1;

/*        FIND L = PIVOT INDEX */

	i__2 = *n - k + 1;
	l = izamax_(&i__2, &a_ref(k, k), &c__1) + k - 1;
	ipvt[k] = l;

/*        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED */

	i__2 = a_subscr(l, k);
	i__3 = a_subscr(l, k);
	z__1.r = a[i__3].r * 0. - a[i__3].i * -1., z__1.i = a[i__3].i * 0. + 
		a[i__3].r * -1.;
	if ((d__1 = a[i__2].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) == 0.) 
		{
	    goto L40;
	}

/*           INTERCHANGE IF NECESSARY */

	if (l == k) {
	    goto L10;
	}
	i__2 = a_subscr(l, k);
	t.r = a[i__2].r, t.i = a[i__2].i;
	i__2 = a_subscr(l, k);
	i__3 = a_subscr(k, k);
	a[i__2].r = a[i__3].r, a[i__2].i = a[i__3].i;
	i__2 = a_subscr(k, k);
	a[i__2].r = t.r, a[i__2].i = t.i;
L10:

/*           COMPUTE MULTIPLIERS */

	z_div(&z__1, &c_b8, &a_ref(k, k));
	t.r = z__1.r, t.i = z__1.i;
	i__2 = *n - k;
	zscal_(&i__2, &t, &a_ref(k + 1, k), &c__1);

/*           ROW ELIMINATION WITH COLUMN INDEXING */

	i__2 = *n;
	for (j = kp1; j <= i__2; ++j) {
	    i__3 = a_subscr(l, j);
	    t.r = a[i__3].r, t.i = a[i__3].i;
	    if (l == k) {
		goto L20;
	    }
	    i__3 = a_subscr(l, j);
	    i__4 = a_subscr(k, j);
	    a[i__3].r = a[i__4].r, a[i__3].i = a[i__4].i;
	    i__3 = a_subscr(k, j);
	    a[i__3].r = t.r, a[i__3].i = t.i;
L20:
	    i__3 = *n - k;
	    zaxpy_(&i__3, &t, &a_ref(k + 1, k), &c__1, &a_ref(k + 1, j), &
		    c__1);
/* L30: */
	}
	goto L50;
L40:
	*info = k;
L50:
/* L60: */
	;
    }
L70:
    ipvt[*n] = *n;
    i__1 = a_subscr(*n, *n);
    i__2 = a_subscr(*n, *n);
    z__1.r = a[i__2].r * 0. - a[i__2].i * -1., z__1.i = a[i__2].i * 0. + a[
	    i__2].r * -1.;
    if ((d__1 = a[i__1].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) == 0.) {
	*info = *n;
    }
    return 0;
} /* zgefa_ */

#undef a_ref
#undef a_subscr


/* Subroutine */ int zpofa_(doublecomplex *a, integer *lda, integer *n, 
	integer *info)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3, i__4;
    doublereal d__1;
    doublecomplex z__1, z__2, z__3;

    /* Builtin functions */
    void z_div(doublecomplex *, doublecomplex *, doublecomplex *), d_cnjg(
	    doublecomplex *, doublecomplex *);
    double sqrt(doublereal);

    /* Local variables */
    static integer j, k;
    static doublereal s;
    static doublecomplex t;
    extern /* Double Complex */ VOID zdotc_(doublecomplex *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *);
    static integer jm1;


#define a_subscr(a_1,a_2) (a_2)*a_dim1 + a_1
#define a_ref(a_1,a_2) a[a_subscr(a_1,a_2)]


/*     ZPOFA FACTORS A COMPLEX*16 HERMITIAN POSITIVE DEFINITE MATRIX.   

       ZPOFA IS USUALLY CALLED BY ZPOCO, BUT IT CAN BE CALLED   
       DIRECTLY WITH A SAVING IN TIME IF  RCOND  IS NOT NEEDED.   
       (TIME FOR ZPOCO) = (1 + 18/N)*(TIME FOR ZPOFA) .   

       ON ENTRY   

          A       COMPLEX*16(LDA, N)   
                  THE HERMITIAN MATRIX TO BE FACTORED.  ONLY THE   
                  DIAGONAL AND UPPER TRIANGLE ARE USED.   

          LDA     INTEGER   
                  THE LEADING DIMENSION OF THE ARRAY  A .   

          N       INTEGER   
                  THE ORDER OF THE MATRIX  A .   

       ON RETURN   

          A       AN UPPER TRIANGULAR MATRIX  R  SO THAT  A =   
                  CTRANS(R)*R WHERE  CTRANS(R)  IS THE CONJUGATE   
                  TRANSPOSE.  THE STRICT LOWER TRIANGLE IS UNALTERED.   
                  IF  INFO .NE. 0 , THE FACTORIZATION IS NOT COMPLETE.   

          INFO    INTEGER   
                  = 0  FOR NORMAL RETURN.   
                  = K  SIGNALS AN ERROR CONDITION.  THE LEADING MINOR   
                       OF ORDER  K  IS NOT POSITIVE DEFINITE.   

       LINPACK.  THIS VERSION DATED 08/14/78 .   
       CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.   

       SUBROUTINES AND FUNCTIONS   

       BLAS ZDOTC   
       FORTRAN DCMPLX,DCONJG,DSQRT   

       INTERNAL VARIABLES   

       BEGIN BLOCK WITH ...EXITS TO 40   


       Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;

    /* Function Body */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	*info = j;
	s = 0.;
	jm1 = j - 1;
	if (jm1 < 1) {
	    goto L20;
	}
	i__2 = jm1;
	for (k = 1; k <= i__2; ++k) {
	    i__3 = a_subscr(k, j);
	    i__4 = k - 1;
	    zdotc_(&z__2, &i__4, &a_ref(1, k), &c__1, &a_ref(1, j), &c__1);
	    z__1.r = a[i__3].r - z__2.r, z__1.i = a[i__3].i - z__2.i;
	    t.r = z__1.r, t.i = z__1.i;
	    z_div(&z__1, &t, &a_ref(k, k));
	    t.r = z__1.r, t.i = z__1.i;
	    i__3 = a_subscr(k, j);
	    a[i__3].r = t.r, a[i__3].i = t.i;
	    d_cnjg(&z__3, &t);
	    z__2.r = t.r * z__3.r - t.i * z__3.i, z__2.i = t.r * z__3.i + t.i 
		    * z__3.r;
	    z__1.r = z__2.r, z__1.i = z__2.i;
	    s += z__1.r;
/* L10: */
	}
L20:
	i__2 = a_subscr(j, j);
	s = a[i__2].r - s;
/*     ......EXIT */
	i__2 = a_subscr(j, j);
	z__1.r = a[i__2].r * 0. - a[i__2].i * -1., z__1.i = a[i__2].i * 0. + 
		a[i__2].r * -1.;
	if (s <= 0. || z__1.r != 0.) {
	    goto L40;
	}
	i__2 = a_subscr(j, j);
	d__1 = sqrt(s);
	z__1.r = d__1, z__1.i = 0.;
	a[i__2].r = z__1.r, a[i__2].i = z__1.i;
/* L30: */
    }
    *info = 0;
L40:
    return 0;
} /* zpofa_ */

#undef a_ref
#undef a_subscr


/* Subroutine */ int zgtsl_(integer *n, doublecomplex *c__, doublecomplex *
	d__, doublecomplex *e, doublecomplex *b, integer *info)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5, i__6, i__7;
    doublereal d__1, d__2, d__3, d__4;
    doublecomplex z__1, z__2, z__3, z__4, z__5;

    /* Builtin functions */
    void z_div(doublecomplex *, doublecomplex *, doublecomplex *);

    /* Local variables */
    static integer k;
    static doublecomplex t;
    static integer kb, kp1, nm1, nm2;


/*     ZGTSL GIVEN A GENERAL TRIDIAGONAL MATRIX AND A RIGHT HAND   
       SIDE WILL FIND THE SOLUTION.   

       ON ENTRY   

          N       INTEGER   
                  IS THE ORDER OF THE TRIDIAGONAL MATRIX.   

          C       COMPLEX*16(N)   
                  IS THE SUBDIAGONAL OF THE TRIDIAGONAL MATRIX.   
                  C(2) THROUGH C(N) SHOULD CONTAIN THE SUBDIAGONAL.   
                  ON OUTPUT C IS DESTROYED.   

          D       COMPLEX*16(N)   
                  IS THE DIAGONAL OF THE TRIDIAGONAL MATRIX.   
                  ON OUTPUT D IS DESTROYED.   

          E       COMPLEX*16(N)   
                  IS THE SUPERDIAGONAL OF THE TRIDIAGONAL MATRIX.   
                  E(1) THROUGH E(N-1) SHOULD CONTAIN THE SUPERDIAGONAL.   
                  ON OUTPUT E IS DESTROYED.   

          B       COMPLEX*16(N)   
                  IS THE RIGHT HAND SIDE VECTOR.   

       ON RETURN   

          B       IS THE SOLUTION VECTOR.   

          INFO    INTEGER   
                  = 0 NORMAL VALUE.   
                  = K IF THE K-TH ELEMENT OF THE DIAGONAL BECOMES   
                      EXACTLY ZERO.  THE SUBROUTINE RETURNS WHEN   
                      THIS IS DETECTED.   

       LINPACK. THIS VERSION DATED 08/14/78 .   
       JACK DONGARRA, ARGONNE NATIONAL LABORATORY.   

       NO EXTERNALS   
       FORTRAN DABS   

       INTERNAL VARIABLES   

       BEGIN BLOCK PERMITTING ...EXITS TO 100   

       Parameter adjustments */
    --b;
    --e;
    --d__;
    --c__;

    /* Function Body */
    *info = 0;
    c__[1].r = d__[1].r, c__[1].i = d__[1].i;
    nm1 = *n - 1;
    if (nm1 < 1) {
	goto L40;
    }
    d__[1].r = e[1].r, d__[1].i = e[1].i;
    e[1].r = 0., e[1].i = 0.;
    i__1 = *n;
    e[i__1].r = 0., e[i__1].i = 0.;

    i__1 = nm1;
    for (k = 1; k <= i__1; ++k) {
	kp1 = k + 1;

/*              FIND THE LARGEST OF THE TWO ROWS */

	i__2 = kp1;
	i__3 = kp1;
	z__1.r = c__[i__3].r * 0. - c__[i__3].i * -1., z__1.i = c__[i__3].i * 
		0. + c__[i__3].r * -1.;
	i__4 = k;
	i__5 = k;
	z__2.r = c__[i__5].r * 0. - c__[i__5].i * -1., z__2.i = c__[i__5].i * 
		0. + c__[i__5].r * -1.;
	if ((d__1 = c__[i__2].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) < (
		d__3 = c__[i__4].r, abs(d__3)) + (d__4 = z__2.r, abs(d__4))) {
	    goto L10;
	}

/*                 INTERCHANGE ROW */

	i__2 = kp1;
	t.r = c__[i__2].r, t.i = c__[i__2].i;
	i__2 = kp1;
	i__3 = k;
	c__[i__2].r = c__[i__3].r, c__[i__2].i = c__[i__3].i;
	i__2 = k;
	c__[i__2].r = t.r, c__[i__2].i = t.i;
	i__2 = kp1;
	t.r = d__[i__2].r, t.i = d__[i__2].i;
	i__2 = kp1;
	i__3 = k;
	d__[i__2].r = d__[i__3].r, d__[i__2].i = d__[i__3].i;
	i__2 = k;
	d__[i__2].r = t.r, d__[i__2].i = t.i;
	i__2 = kp1;
	t.r = e[i__2].r, t.i = e[i__2].i;
	i__2 = kp1;
	i__3 = k;
	e[i__2].r = e[i__3].r, e[i__2].i = e[i__3].i;
	i__2 = k;
	e[i__2].r = t.r, e[i__2].i = t.i;
	i__2 = kp1;
	t.r = b[i__2].r, t.i = b[i__2].i;
	i__2 = kp1;
	i__3 = k;
	b[i__2].r = b[i__3].r, b[i__2].i = b[i__3].i;
	i__2 = k;
	b[i__2].r = t.r, b[i__2].i = t.i;
L10:

/*              ZERO ELEMENTS */

	i__2 = k;
	i__3 = k;
	z__1.r = c__[i__3].r * 0. - c__[i__3].i * -1., z__1.i = c__[i__3].i * 
		0. + c__[i__3].r * -1.;
	if ((d__1 = c__[i__2].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) != 
		0.) {
	    goto L20;
	}
	*info = k;
/*     ............EXIT */
	goto L100;
L20:
	i__2 = kp1;
	z__2.r = -c__[i__2].r, z__2.i = -c__[i__2].i;
	z_div(&z__1, &z__2, &c__[k]);
	t.r = z__1.r, t.i = z__1.i;
	i__2 = kp1;
	i__3 = kp1;
	i__4 = k;
	z__2.r = t.r * d__[i__4].r - t.i * d__[i__4].i, z__2.i = t.r * d__[
		i__4].i + t.i * d__[i__4].r;
	z__1.r = d__[i__3].r + z__2.r, z__1.i = d__[i__3].i + z__2.i;
	c__[i__2].r = z__1.r, c__[i__2].i = z__1.i;
	i__2 = kp1;
	i__3 = kp1;
	i__4 = k;
	z__2.r = t.r * e[i__4].r - t.i * e[i__4].i, z__2.i = t.r * e[i__4].i 
		+ t.i * e[i__4].r;
	z__1.r = e[i__3].r + z__2.r, z__1.i = e[i__3].i + z__2.i;
	d__[i__2].r = z__1.r, d__[i__2].i = z__1.i;
	i__2 = kp1;
	e[i__2].r = 0., e[i__2].i = 0.;
	i__2 = kp1;
	i__3 = kp1;
	i__4 = k;
	z__2.r = t.r * b[i__4].r - t.i * b[i__4].i, z__2.i = t.r * b[i__4].i 
		+ t.i * b[i__4].r;
	z__1.r = b[i__3].r + z__2.r, z__1.i = b[i__3].i + z__2.i;
	b[i__2].r = z__1.r, b[i__2].i = z__1.i;
/* L30: */
    }
L40:
    i__1 = *n;
    i__2 = *n;
    z__1.r = c__[i__2].r * 0. - c__[i__2].i * -1., z__1.i = c__[i__2].i * 0. 
	    + c__[i__2].r * -1.;
    if ((d__1 = c__[i__1].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) != 0.) {
	goto L50;
    }
    *info = *n;
    goto L90;
L50:

/*           BACK SOLVE */

    nm2 = *n - 2;
    i__1 = *n;
    z_div(&z__1, &b[*n], &c__[*n]);
    b[i__1].r = z__1.r, b[i__1].i = z__1.i;
    if (*n == 1) {
	goto L80;
    }
    i__1 = nm1;
    i__2 = nm1;
    i__3 = nm1;
    i__4 = *n;
    z__3.r = d__[i__3].r * b[i__4].r - d__[i__3].i * b[i__4].i, z__3.i = d__[
	    i__3].r * b[i__4].i + d__[i__3].i * b[i__4].r;
    z__2.r = b[i__2].r - z__3.r, z__2.i = b[i__2].i - z__3.i;
    z_div(&z__1, &z__2, &c__[nm1]);
    b[i__1].r = z__1.r, b[i__1].i = z__1.i;
    if (nm2 < 1) {
	goto L70;
    }
    i__1 = nm2;
    for (kb = 1; kb <= i__1; ++kb) {
	k = nm2 - kb + 1;
	i__2 = k;
	i__3 = k;
	i__4 = k;
	i__5 = k + 1;
	z__4.r = d__[i__4].r * b[i__5].r - d__[i__4].i * b[i__5].i, z__4.i = 
		d__[i__4].r * b[i__5].i + d__[i__4].i * b[i__5].r;
	z__3.r = b[i__3].r - z__4.r, z__3.i = b[i__3].i - z__4.i;
	i__6 = k;
	i__7 = k + 2;
	z__5.r = e[i__6].r * b[i__7].r - e[i__6].i * b[i__7].i, z__5.i = e[
		i__6].r * b[i__7].i + e[i__6].i * b[i__7].r;
	z__2.r = z__3.r - z__5.r, z__2.i = z__3.i - z__5.i;
	z_div(&z__1, &z__2, &c__[k]);
	b[i__2].r = z__1.r, b[i__2].i = z__1.i;
/* L60: */
    }
L70:
L80:
L90:
L100:

    return 0;
} /* zgtsl_   

   Subroutine */ int zptsl_(integer *n, doublecomplex *d__, doublecomplex *e, 
	doublecomplex *b)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5;
    doublecomplex z__1, z__2, z__3, z__4;

    /* Builtin functions */
    void z_div(doublecomplex *, doublecomplex *, doublecomplex *), d_cnjg(
	    doublecomplex *, doublecomplex *);

    /* Local variables */
    static integer nm1d2, k;
    static doublecomplex t1, t2;
    static integer ke, kf, kp1, nm1, kbm1;


/*     ZPTSL GIVEN A POSITIVE DEFINITE TRIDIAGONAL MATRIX AND A RIGHT   
       HAND SIDE WILL FIND THE SOLUTION.   

       ON ENTRY   

          N        INTEGER   
                   IS THE ORDER OF THE TRIDIAGONAL MATRIX.   

          D        COMPLEX*16(N)   
                   IS THE DIAGONAL OF THE TRIDIAGONAL MATRIX.   
                   ON OUTPUT D IS DESTROYED.   

          E        COMPLEX*16(N)   
                   IS THE OFFDIAGONAL OF THE TRIDIAGONAL MATRIX.   
                   E(1) THROUGH E(N-1) SHOULD CONTAIN THE   
                   OFFDIAGONAL.   

          B        COMPLEX*16(N)   
                   IS THE RIGHT HAND SIDE VECTOR.   

       ON RETURN   

          B        CONTAINS THE SOULTION.   

       LINPACK. THIS VERSION DATED 08/14/78 .   
       JACK DONGARRA, ARGONNE NATIONAL LABORATORY.   

       NO EXTERNALS   
       FORTRAN DCONJG,MOD   

       INTERNAL VARIABLES   


       CHECK FOR 1 X 1 CASE   

       Parameter adjustments */
    --b;
    --e;
    --d__;

    /* Function Body */
    if (*n != 1) {
	goto L10;
    }
    z_div(&z__1, &b[1], &d__[1]);
    b[1].r = z__1.r, b[1].i = z__1.i;
    goto L70;
L10:
    nm1 = *n - 1;
    nm1d2 = nm1 / 2;
    if (*n == 2) {
	goto L30;
    }
    kbm1 = *n - 1;

/*           ZERO TOP HALF OF SUBDIAGONAL AND BOTTOM HALF OF   
             SUPERDIAGONAL */

    i__1 = nm1d2;
    for (k = 1; k <= i__1; ++k) {
	d_cnjg(&z__2, &e[k]);
	z_div(&z__1, &z__2, &d__[k]);
	t1.r = z__1.r, t1.i = z__1.i;
	i__2 = k + 1;
	i__3 = k + 1;
	i__4 = k;
	z__2.r = t1.r * e[i__4].r - t1.i * e[i__4].i, z__2.i = t1.r * e[i__4]
		.i + t1.i * e[i__4].r;
	z__1.r = d__[i__3].r - z__2.r, z__1.i = d__[i__3].i - z__2.i;
	d__[i__2].r = z__1.r, d__[i__2].i = z__1.i;
	i__2 = k + 1;
	i__3 = k + 1;
	i__4 = k;
	z__2.r = t1.r * b[i__4].r - t1.i * b[i__4].i, z__2.i = t1.r * b[i__4]
		.i + t1.i * b[i__4].r;
	z__1.r = b[i__3].r - z__2.r, z__1.i = b[i__3].i - z__2.i;
	b[i__2].r = z__1.r, b[i__2].i = z__1.i;
	z_div(&z__1, &e[kbm1], &d__[kbm1 + 1]);
	t2.r = z__1.r, t2.i = z__1.i;
	i__2 = kbm1;
	i__3 = kbm1;
	d_cnjg(&z__3, &e[kbm1]);
	z__2.r = t2.r * z__3.r - t2.i * z__3.i, z__2.i = t2.r * z__3.i + t2.i 
		* z__3.r;
	z__1.r = d__[i__3].r - z__2.r, z__1.i = d__[i__3].i - z__2.i;
	d__[i__2].r = z__1.r, d__[i__2].i = z__1.i;
	i__2 = kbm1;
	i__3 = kbm1;
	i__4 = kbm1 + 1;
	z__2.r = t2.r * b[i__4].r - t2.i * b[i__4].i, z__2.i = t2.r * b[i__4]
		.i + t2.i * b[i__4].r;
	z__1.r = b[i__3].r - z__2.r, z__1.i = b[i__3].i - z__2.i;
	b[i__2].r = z__1.r, b[i__2].i = z__1.i;
	--kbm1;
/* L20: */
    }
L30:
    kp1 = nm1d2 + 1;

/*        CLEAN UP FOR POSSIBLE 2 X 2 BLOCK AT CENTER */

    if (*n % 2 != 0) {
	goto L40;
    }
    d_cnjg(&z__2, &e[kp1]);
    z_div(&z__1, &z__2, &d__[kp1]);
    t1.r = z__1.r, t1.i = z__1.i;
    i__1 = kp1 + 1;
    i__2 = kp1 + 1;
    i__3 = kp1;
    z__2.r = t1.r * e[i__3].r - t1.i * e[i__3].i, z__2.i = t1.r * e[i__3].i + 
	    t1.i * e[i__3].r;
    z__1.r = d__[i__2].r - z__2.r, z__1.i = d__[i__2].i - z__2.i;
    d__[i__1].r = z__1.r, d__[i__1].i = z__1.i;
    i__1 = kp1 + 1;
    i__2 = kp1 + 1;
    i__3 = kp1;
    z__2.r = t1.r * b[i__3].r - t1.i * b[i__3].i, z__2.i = t1.r * b[i__3].i + 
	    t1.i * b[i__3].r;
    z__1.r = b[i__2].r - z__2.r, z__1.i = b[i__2].i - z__2.i;
    b[i__1].r = z__1.r, b[i__1].i = z__1.i;
    ++kp1;
L40:

/*        BACK SOLVE STARTING AT THE CENTER, GOING TOWARDS THE TOP   
          AND BOTTOM */

    i__1 = kp1;
    z_div(&z__1, &b[kp1], &d__[kp1]);
    b[i__1].r = z__1.r, b[i__1].i = z__1.i;
    if (*n == 2) {
	goto L60;
    }
    k = kp1 - 1;
    ke = kp1 + nm1d2 - 1;
    i__1 = ke;
    for (kf = kp1; kf <= i__1; ++kf) {
	i__2 = k;
	i__3 = k;
	i__4 = k;
	i__5 = k + 1;
	z__3.r = e[i__4].r * b[i__5].r - e[i__4].i * b[i__5].i, z__3.i = e[
		i__4].r * b[i__5].i + e[i__4].i * b[i__5].r;
	z__2.r = b[i__3].r - z__3.r, z__2.i = b[i__3].i - z__3.i;
	z_div(&z__1, &z__2, &d__[k]);
	b[i__2].r = z__1.r, b[i__2].i = z__1.i;
	i__2 = kf + 1;
	i__3 = kf + 1;
	d_cnjg(&z__4, &e[kf]);
	i__4 = kf;
	z__3.r = z__4.r * b[i__4].r - z__4.i * b[i__4].i, z__3.i = z__4.r * b[
		i__4].i + z__4.i * b[i__4].r;
	z__2.r = b[i__3].r - z__3.r, z__2.i = b[i__3].i - z__3.i;
	z_div(&z__1, &z__2, &d__[kf + 1]);
	b[i__2].r = z__1.r, b[i__2].i = z__1.i;
	--k;
/* L50: */
    }
L60:
    if (*n % 2 == 0) {
	z__3.r = e[1].r * b[2].r - e[1].i * b[2].i, z__3.i = e[1].r * b[2].i 
		+ e[1].i * b[2].r;
	z__2.r = b[1].r - z__3.r, z__2.i = b[1].i - z__3.i;
	z_div(&z__1, &z__2, &d__[1]);
	b[1].r = z__1.r, b[1].i = z__1.i;
    }
L70:
    return 0;
} /* zptsl_ */

