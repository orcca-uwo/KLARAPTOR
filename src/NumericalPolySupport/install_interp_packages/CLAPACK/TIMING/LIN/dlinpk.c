#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* Subroutine */ int dgefa_(doublereal *a, integer *lda, integer *n, integer *
	ipvt, integer *info)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;

    /* Local variables */
    static integer j, k, l;
    static doublereal t;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *, 
	    integer *), daxpy_(integer *, doublereal *, doublereal *, integer 
	    *, doublereal *, integer *);
    extern integer idamax_(integer *, doublereal *, integer *);
    static integer kp1, nm1;


#define a_ref(a_1,a_2) a[(a_2)*a_dim1 + a_1]


/*     DGEFA FACTORS A DOUBLE PRECISION MATRIX BY GAUSSIAN ELIMINATION.   

       DGEFA IS USUALLY CALLED BY DGECO, BUT IT CAN BE CALLED   
       DIRECTLY WITH A SAVING IN TIME IF  RCOND  IS NOT NEEDED.   
       (TIME FOR DGECO) = (1 + 9/N)*(TIME FOR DGEFA) .   

       ON ENTRY   

          A       DOUBLE PRECISION(LDA, N)   
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
                       INDICATE THAT DGESL OR DGEDI WILL DIVIDE BY ZERO   
                       IF CALLED.  USE  RCOND  IN DGECO FOR A RELIABLE   
                       INDICATION OF SINGULARITY.   

       LINPACK. THIS VERSION DATED 08/14/78 .   
       CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.   

       SUBROUTINES AND FUNCTIONS   

       BLAS DAXPY,DSCAL,IDAMAX   

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
	l = idamax_(&i__2, &a_ref(k, k), &c__1) + k - 1;
	ipvt[k] = l;

/*        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED */

	if (a_ref(l, k) == 0.) {
	    goto L40;
	}

/*           INTERCHANGE IF NECESSARY */

	if (l == k) {
	    goto L10;
	}
	t = a_ref(l, k);
	a_ref(l, k) = a_ref(k, k);
	a_ref(k, k) = t;
L10:

/*           COMPUTE MULTIPLIERS */

	t = -1. / a_ref(k, k);
	i__2 = *n - k;
	dscal_(&i__2, &t, &a_ref(k + 1, k), &c__1);

/*           ROW ELIMINATION WITH COLUMN INDEXING */

	i__2 = *n;
	for (j = kp1; j <= i__2; ++j) {
	    t = a_ref(l, j);
	    if (l == k) {
		goto L20;
	    }
	    a_ref(l, j) = a_ref(k, j);
	    a_ref(k, j) = t;
L20:
	    i__3 = *n - k;
	    daxpy_(&i__3, &t, &a_ref(k + 1, k), &c__1, &a_ref(k + 1, j), &
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
    if (a_ref(*n, *n) == 0.) {
	*info = *n;
    }
    return 0;
} /* dgefa_ */

#undef a_ref


/* Subroutine */ int dpofa_(doublereal *a, integer *lda, integer *n, integer *
	info)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *, 
	    integer *);
    static integer j, k;
    static doublereal s, t;
    static integer jm1;


#define a_ref(a_1,a_2) a[(a_2)*a_dim1 + a_1]


/*     DPOFA FACTORS A DOUBLE PRECISION SYMMETRIC POSITIVE DEFINITE   
       MATRIX.   

       DPOFA IS USUALLY CALLED BY DPOCO, BUT IT CAN BE CALLED   
       DIRECTLY WITH A SAVING IN TIME IF  RCOND  IS NOT NEEDED.   
       (TIME FOR DPOCO) = (1 + 18/N)*(TIME FOR DPOFA) .   

       ON ENTRY   

          A       DOUBLE PRECISION(LDA, N)   
                  THE SYMMETRIC MATRIX TO BE FACTORED.  ONLY THE   
                  DIAGONAL AND UPPER TRIANGLE ARE USED.   

          LDA     INTEGER   
                  THE LEADING DIMENSION OF THE ARRAY  A .   

          N       INTEGER   
                  THE ORDER OF THE MATRIX  A .   

       ON RETURN   

          A       AN UPPER TRIANGULAR MATRIX  R  SO THAT  A = TRANS(R)*R   
                  WHERE  TRANS(R)  IS THE TRANSPOSE.   
                  THE STRICT LOWER TRIANGLE IS UNALTERED.   
                  IF  INFO .NE. 0 , THE FACTORIZATION IS NOT COMPLETE.   

          INFO    INTEGER   
                  = 0  FOR NORMAL RETURN.   
                  = K  SIGNALS AN ERROR CONDITION.  THE LEADING MINOR   
                       OF ORDER  K  IS NOT POSITIVE DEFINITE.   

       LINPACK.  THIS VERSION DATED 08/14/78 .   
       CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.   

       SUBROUTINES AND FUNCTIONS   

       BLAS DDOT   
       FORTRAN DSQRT   

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
	    i__3 = k - 1;
	    t = a_ref(k, j) - ddot_(&i__3, &a_ref(1, k), &c__1, &a_ref(1, j), 
		    &c__1);
	    t /= a_ref(k, k);
	    a_ref(k, j) = t;
	    s += t * t;
/* L10: */
	}
L20:
	s = a_ref(j, j) - s;
/*     ......EXIT */
	if (s <= 0.) {
	    goto L40;
	}
	a_ref(j, j) = sqrt(s);
/* L30: */
    }
    *info = 0;
L40:
    return 0;
} /* dpofa_ */

#undef a_ref


/* Subroutine */ int dqrdc_(doublereal *x, integer *ldx, integer *n, integer *
	p, doublereal *qraux, integer *jpvt, doublereal *work, integer *job)
{
    /* System generated locals */
    integer x_dim1, x_offset, i__1, i__2, i__3;
    doublereal d__1, d__2;

    /* Builtin functions */
    double d_sign(doublereal *, doublereal *), sqrt(doublereal);

    /* Local variables */
    static logical negj;
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *, 
	    integer *);
    static integer maxj;
    extern doublereal dnrm2_(integer *, doublereal *, integer *);
    static integer j, l;
    static doublereal t;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *, 
	    integer *), dswap_(integer *, doublereal *, integer *, doublereal 
	    *, integer *);
    static logical swapj;
    extern /* Subroutine */ int daxpy_(integer *, doublereal *, doublereal *, 
	    integer *, doublereal *, integer *);
    static doublereal nrmxl;
    static integer jj, jp, pl, pu;
    static doublereal tt, maxnrm;
    static integer lp1, lup;


#define x_ref(a_1,a_2) x[(a_2)*x_dim1 + a_1]


/*     DQRDC USES HOUSEHOLDER TRANSFORMATIONS TO COMPUTE THE QR   
       FACTORIZATION OF AN N BY P MATRIX X.  COLUMN PIVOTING   
       BASED ON THE 2-NORMS OF THE REDUCED COLUMNS MAY BE   
       PERFORMED AT THE USERS OPTION.   

       ON ENTRY   

          X       DOUBLE PRECISION(LDX,P), WHERE LDX .GE. N.   
                  X CONTAINS THE MATRIX WHOSE DECOMPOSITION IS TO BE   
                  COMPUTED.   

          LDX     INTEGER.   
                  LDX IS THE LEADING DIMENSION OF THE ARRAY X.   

          N       INTEGER.   
                  N IS THE NUMBER OF ROWS OF THE MATRIX X.   

          P       INTEGER.   
                  P IS THE NUMBER OF COLUMNS OF THE MATRIX X.   

          JPVT    INTEGER(P).   
                  JPVT CONTAINS INTEGERS THAT CONTROL THE SELECTION   
                  OF THE PIVOT COLUMNS.  THE K-TH COLUMN X(K) OF X   
                  IS PLACED IN ONE OF THREE CLASSES ACCORDING TO THE   
                  VALUE OF JPVT(K).   

                     IF JPVT(K) .GT. 0, THEN X(K) IS AN INITIAL   
                                        COLUMN.   

                     IF JPVT(K) .EQ. 0, THEN X(K) IS A FREE COLUMN.   

                     IF JPVT(K) .LT. 0, THEN X(K) IS A FINAL COLUMN.   

                  BEFORE THE DECOMPOSITION IS COMPUTED, INITIAL COLUMNS   
                  ARE MOVED TO THE BEGINNING OF THE ARRAY X AND FINAL   
                  COLUMNS TO THE END.  BOTH INITIAL AND FINAL COLUMNS   
                  ARE FROZEN IN PLACE DURING THE COMPUTATION AND ONLY   
                  FREE COLUMNS ARE MOVED.  AT THE K-TH STAGE OF THE   
                  REDUCTION, IF X(K) IS OCCUPIED BY A FREE COLUMN   
                  IT IS INTERCHANGED WITH THE FREE COLUMN OF LARGEST   
                  REDUCED NORM.  JPVT IS NOT REFERENCED IF   
                  JOB .EQ. 0.   

          WORK    DOUBLE PRECISION(P).   
                  WORK IS A WORK ARRAY.  WORK IS NOT REFERENCED IF   
                  JOB .EQ. 0.   

          JOB     INTEGER.   
                  JOB IS AN INTEGER THAT INITIATES COLUMN PIVOTING.   
                  IF JOB .EQ. 0, NO PIVOTING IS DONE.   
                  IF JOB .NE. 0, PIVOTING IS DONE.   

       ON RETURN   

          X       X CONTAINS IN ITS UPPER TRIANGLE THE UPPER   
                  TRIANGULAR MATRIX R OF THE QR FACTORIZATION.   
                  BELOW ITS DIAGONAL X CONTAINS INFORMATION FROM   
                  WHICH THE ORTHOGONAL PART OF THE DECOMPOSITION   
                  CAN BE RECOVERED.  NOTE THAT IF PIVOTING HAS   
                  BEEN REQUESTED, THE DECOMPOSITION IS NOT THAT   
                  OF THE ORIGINAL MATRIX X BUT THAT OF X   
                  WITH ITS COLUMNS PERMUTED AS DESCRIBED BY JPVT.   

          QRAUX   DOUBLE PRECISION(P).   
                  QRAUX CONTAINS FURTHER INFORMATION REQUIRED TO RECOVER   
                  THE ORTHOGONAL PART OF THE DECOMPOSITION.   

          JPVT    JPVT(K) CONTAINS THE INDEX OF THE COLUMN OF THE   
                  ORIGINAL MATRIX THAT HAS BEEN INTERCHANGED INTO   
                  THE K-TH COLUMN, IF PIVOTING WAS REQUESTED.   

       LINPACK. THIS VERSION DATED 08/14/78 .   
       G.W. STEWART, UNIVERSITY OF MARYLAND, ARGONNE NATIONAL LAB.   

       DQRDC USES THE FOLLOWING FUNCTIONS AND SUBPROGRAMS.   

       BLAS DAXPY,DDOT,DSCAL,DSWAP,DNRM2   
       FORTRAN DABS,DMAX1,MIN0,DSQRT   

       INTERNAL VARIABLES   



       Parameter adjustments */
    x_dim1 = *ldx;
    x_offset = 1 + x_dim1 * 1;
    x -= x_offset;
    --qraux;
    --jpvt;
    --work;

    /* Function Body */
    pl = 1;
    pu = 0;
    if (*job == 0) {
	goto L60;
    }

/*        PIVOTING HAS BEEN REQUESTED.  REARRANGE THE COLUMNS   
          ACCORDING TO JPVT. */

    i__1 = *p;
    for (j = 1; j <= i__1; ++j) {
	swapj = jpvt[j] > 0;
	negj = jpvt[j] < 0;
	jpvt[j] = j;
	if (negj) {
	    jpvt[j] = -j;
	}
	if (! swapj) {
	    goto L10;
	}
	if (j != pl) {
	    dswap_(n, &x_ref(1, pl), &c__1, &x_ref(1, j), &c__1);
	}
	jpvt[j] = jpvt[pl];
	jpvt[pl] = j;
	++pl;
L10:
/* L20: */
	;
    }
    pu = *p;
    i__1 = *p;
    for (jj = 1; jj <= i__1; ++jj) {
	j = *p - jj + 1;
	if (jpvt[j] >= 0) {
	    goto L40;
	}
	jpvt[j] = -jpvt[j];
	if (j == pu) {
	    goto L30;
	}
	dswap_(n, &x_ref(1, pu), &c__1, &x_ref(1, j), &c__1);
	jp = jpvt[pu];
	jpvt[pu] = jpvt[j];
	jpvt[j] = jp;
L30:
	--pu;
L40:
/* L50: */
	;
    }
L60:

/*     COMPUTE THE NORMS OF THE FREE COLUMNS. */

    if (pu < pl) {
	goto L80;
    }
    i__1 = pu;
    for (j = pl; j <= i__1; ++j) {
	qraux[j] = dnrm2_(n, &x_ref(1, j), &c__1);
	work[j] = qraux[j];
/* L70: */
    }
L80:

/*     PERFORM THE HOUSEHOLDER REDUCTION OF X. */

    lup = min(*n,*p);
    i__1 = lup;
    for (l = 1; l <= i__1; ++l) {
	if (l < pl || l >= pu) {
	    goto L120;
	}

/*           LOCATE THE COLUMN OF LARGEST NORM AND BRING IT   
             INTO THE PIVOT POSITION. */

	maxnrm = 0.;
	maxj = l;
	i__2 = pu;
	for (j = l; j <= i__2; ++j) {
	    if (qraux[j] <= maxnrm) {
		goto L90;
	    }
	    maxnrm = qraux[j];
	    maxj = j;
L90:
/* L100: */
	    ;
	}
	if (maxj == l) {
	    goto L110;
	}
	dswap_(n, &x_ref(1, l), &c__1, &x_ref(1, maxj), &c__1);
	qraux[maxj] = qraux[l];
	work[maxj] = work[l];
	jp = jpvt[maxj];
	jpvt[maxj] = jpvt[l];
	jpvt[l] = jp;
L110:
L120:
	qraux[l] = 0.;
	if (l == *n) {
	    goto L190;
	}

/*           COMPUTE THE HOUSEHOLDER TRANSFORMATION FOR COLUMN L. */

	i__2 = *n - l + 1;
	nrmxl = dnrm2_(&i__2, &x_ref(l, l), &c__1);
	if (nrmxl == 0.) {
	    goto L180;
	}
	if (x_ref(l, l) != 0.) {
	    nrmxl = d_sign(&nrmxl, &x_ref(l, l));
	}
	i__2 = *n - l + 1;
	d__1 = 1. / nrmxl;
	dscal_(&i__2, &d__1, &x_ref(l, l), &c__1);
	x_ref(l, l) = x_ref(l, l) + 1.;

/*              APPLY THE TRANSFORMATION TO THE REMAINING COLUMNS,   
                UPDATING THE NORMS. */

	lp1 = l + 1;
	if (*p < lp1) {
	    goto L170;
	}
	i__2 = *p;
	for (j = lp1; j <= i__2; ++j) {
	    i__3 = *n - l + 1;
	    t = -ddot_(&i__3, &x_ref(l, l), &c__1, &x_ref(l, j), &c__1) / 
		    x_ref(l, l);
	    i__3 = *n - l + 1;
	    daxpy_(&i__3, &t, &x_ref(l, l), &c__1, &x_ref(l, j), &c__1);
	    if (j < pl || j > pu) {
		goto L150;
	    }
	    if (qraux[j] == 0.) {
		goto L150;
	    }
/* Computing 2nd power */
	    d__2 = (d__1 = x_ref(l, j), abs(d__1)) / qraux[j];
	    tt = 1. - d__2 * d__2;
	    tt = max(tt,0.);
	    t = tt;
/* Computing 2nd power */
	    d__1 = qraux[j] / work[j];
	    tt = tt * .05 * (d__1 * d__1) + 1.;
	    if (tt == 1.) {
		goto L130;
	    }
	    qraux[j] *= sqrt(t);
	    goto L140;
L130:
	    i__3 = *n - l;
	    qraux[j] = dnrm2_(&i__3, &x_ref(l + 1, j), &c__1);
	    work[j] = qraux[j];
L140:
L150:
/* L160: */
	    ;
	}
L170:

/*              SAVE THE TRANSFORMATION. */

	qraux[l] = x_ref(l, l);
	x_ref(l, l) = -nrmxl;
L180:
L190:
/* L200: */
	;
    }
    return 0;
} /* dqrdc_ */

#undef x_ref


/* Subroutine */ int dgtsl_(integer *n, doublereal *c__, doublereal *d__, 
	doublereal *e, doublereal *b, integer *info)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2;

    /* Local variables */
    static integer k;
    static doublereal t;
    static integer kb, kp1, nm1, nm2;


/*     DGTSL GIVEN A GENERAL TRIDIAGONAL MATRIX AND A RIGHT HAND   
       SIDE WILL FIND THE SOLUTION.   

       ON ENTRY   

          N       INTEGER   
                  IS THE ORDER OF THE TRIDIAGONAL MATRIX.   

          C       DOUBLE PRECISION(N)   
                  IS THE SUBDIAGONAL OF THE TRIDIAGONAL MATRIX.   
                  C(2) THROUGH C(N) SHOULD CONTAIN THE SUBDIAGONAL.   
                  ON OUTPUT C IS DESTROYED.   

          D       DOUBLE PRECISION(N)   
                  IS THE DIAGONAL OF THE TRIDIAGONAL MATRIX.   
                  ON OUTPUT D IS DESTROYED.   

          E       DOUBLE PRECISION(N)   
                  IS THE SUPERDIAGONAL OF THE TRIDIAGONAL MATRIX.   
                  E(1) THROUGH E(N-1) SHOULD CONTAIN THE SUPERDIAGONAL.   
                  ON OUTPUT E IS DESTROYED.   

          B       DOUBLE PRECISION(N)   
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
    c__[1] = d__[1];
    nm1 = *n - 1;
    if (nm1 < 1) {
	goto L40;
    }
    d__[1] = e[1];
    e[1] = 0.;
    e[*n] = 0.;

    i__1 = nm1;
    for (k = 1; k <= i__1; ++k) {
	kp1 = k + 1;

/*              FIND THE LARGEST OF THE TWO ROWS */

	if ((d__1 = c__[kp1], abs(d__1)) < (d__2 = c__[k], abs(d__2))) {
	    goto L10;
	}

/*                 INTERCHANGE ROW */

	t = c__[kp1];
	c__[kp1] = c__[k];
	c__[k] = t;
	t = d__[kp1];
	d__[kp1] = d__[k];
	d__[k] = t;
	t = e[kp1];
	e[kp1] = e[k];
	e[k] = t;
	t = b[kp1];
	b[kp1] = b[k];
	b[k] = t;
L10:

/*              ZERO ELEMENTS */

	if (c__[k] != 0.) {
	    goto L20;
	}
	*info = k;
/*     ............EXIT */
	goto L100;
L20:
	t = -c__[kp1] / c__[k];
	c__[kp1] = d__[kp1] + t * d__[k];
	d__[kp1] = e[kp1] + t * e[k];
	e[kp1] = 0.;
	b[kp1] += t * b[k];
/* L30: */
    }
L40:
    if (c__[*n] != 0.) {
	goto L50;
    }
    *info = *n;
    goto L90;
L50:

/*           BACK SOLVE */

    nm2 = *n - 2;
    b[*n] /= c__[*n];
    if (*n == 1) {
	goto L80;
    }
    b[nm1] = (b[nm1] - d__[nm1] * b[*n]) / c__[nm1];
    if (nm2 < 1) {
	goto L70;
    }
    i__1 = nm2;
    for (kb = 1; kb <= i__1; ++kb) {
	k = nm2 - kb + 1;
	b[k] = (b[k] - d__[k] * b[k + 1] - e[k] * b[k + 2]) / c__[k];
/* L60: */
    }
L70:
L80:
L90:
L100:

    return 0;
} /* dgtsl_   

   Subroutine */ int dptsl_(integer *n, doublereal *d__, doublereal *e, 
	doublereal *b)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer nm1d2, k;
    static doublereal t1, t2;
    static integer ke, kf, kp1, nm1, kbm1;


/*     DPTSL GIVEN A POSITIVE DEFINITE TRIDIAGONAL MATRIX AND A RIGHT   
       HAND SIDE WILL FIND THE SOLUTION.   

       ON ENTRY   

          N        INTEGER   
                   IS THE ORDER OF THE TRIDIAGONAL MATRIX.   

          D        DOUBLE PRECISION(N)   
                   IS THE DIAGONAL OF THE TRIDIAGONAL MATRIX.   
                   ON OUTPUT D IS DESTROYED.   

          E        DOUBLE PRECISION(N)   
                   IS THE OFFDIAGONAL OF THE TRIDIAGONAL MATRIX.   
                   E(1) THROUGH E(N-1) SHOULD CONTAIN THE   
                   OFFDIAGONAL.   

          B        DOUBLE PRECISION(N)   
                   IS THE RIGHT HAND SIDE VECTOR.   

       ON RETURN   

          B        CONTAINS THE SOULTION.   

       LINPACK. THIS VERSION DATED 08/14/78 .   
       JACK DONGARRA, ARGONNE NATIONAL LABORATORY.   

       NO EXTERNALS   
       FORTRAN MOD   

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
    b[1] /= d__[1];
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
	t1 = e[k] / d__[k];
	d__[k + 1] -= t1 * e[k];
	b[k + 1] -= t1 * b[k];
	t2 = e[kbm1] / d__[kbm1 + 1];
	d__[kbm1] -= t2 * e[kbm1];
	b[kbm1] -= t2 * b[kbm1 + 1];
	--kbm1;
/* L20: */
    }
L30:
    kp1 = nm1d2 + 1;

/*        CLEAN UP FOR POSSIBLE 2 X 2 BLOCK AT CENTER */

    if (*n % 2 != 0) {
	goto L40;
    }
    t1 = e[kp1] / d__[kp1];
    d__[kp1 + 1] -= t1 * e[kp1];
    b[kp1 + 1] -= t1 * b[kp1];
    ++kp1;
L40:

/*        BACK SOLVE STARTING AT THE CENTER, GOING TOWARDS THE TOP   
          AND BOTTOM */

    b[kp1] /= d__[kp1];
    if (*n == 2) {
	goto L60;
    }
    k = kp1 - 1;
    ke = kp1 + nm1d2 - 1;
    i__1 = ke;
    for (kf = kp1; kf <= i__1; ++kf) {
	b[k] = (b[k] - e[k] * b[k + 1]) / d__[k];
	b[kf + 1] = (b[kf + 1] - e[kf] * b[kf]) / d__[kf + 1];
	--k;
/* L50: */
    }
L60:
    if (*n % 2 == 0) {
	b[1] = (b[1] - e[1] * b[2]) / d__[1];
    }
L70:
    return 0;
} /* dptsl_ */

