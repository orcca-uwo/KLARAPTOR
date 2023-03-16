#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

union {
    struct {
	real ops, itcnt;
    } _1;
    struct {
	real iops, itcnt;
    } _2;
} latime_;

#define latime_1 (latime_._1)
#define latime_2 (latime_._2)

struct {
    real opst;
} pythop_;

#define pythop_1 pythop_

/* Table of constant values */

static real c_b90 = 0.f;
static real c_b114 = 1.f;
static integer c__1 = 1;
static real c_b405 = -1.f;

/* Subroutine */ int cdiv_(real *ar, real *ai, real *br, real *bi, real *cr, 
	real *ci)
{
    /* System generated locals */
    real r__1, r__2;

    /* Local variables */
    static real s, ais, bis, ars, brs;


/*     COMPLEX DIVISION, (CR,CI) = (AR,AI)/(BR,BI) */

    s = dabs(*br) + dabs(*bi);
    ars = *ar / s;
    ais = *ai / s;
    brs = *br / s;
    bis = *bi / s;
/* Computing 2nd power */
    r__1 = brs;
/* Computing 2nd power */
    r__2 = bis;
    s = r__1 * r__1 + r__2 * r__2;
    *cr = (ars * brs + ais * bis) / s;
    *ci = (ais * brs - ars * bis) / s;
    return 0;
} /* cdiv_ */

doublereal epslon_(real *x)
{
    /* System generated locals */
    real ret_val, r__1;

    /* Local variables */
    static real a, b, c__, eps;


/*     ESTIMATE UNIT ROUNDOFF IN QUANTITIES OF SIZE X.   


       THIS PROGRAM SHOULD FUNCTION PROPERLY ON ALL SYSTEMS   
       SATISFYING THE FOLLOWING TWO ASSUMPTIONS,   
          1.  THE BASE USED IN REPRESENTING FLOATING POINT   
              NUMBERS IS NOT A POWER OF THREE.   
          2.  THE QUANTITY  A  IN STATEMENT 10 IS REPRESENTED TO   
              THE ACCURACY USED IN FLOATING POINT VARIABLES   
              THAT ARE STORED IN MEMORY.   
       THE STATEMENT NUMBER 10 AND THE GO TO 10 ARE INTENDED TO   
       FORCE OPTIMIZING COMPILERS TO GENERATE CODE SATISFYING   
       ASSUMPTION 2.   
       UNDER THESE ASSUMPTIONS, IT SHOULD BE TRUE THAT,   
              A  IS NOT EXACTLY EQUAL TO FOUR-THIRDS,   
              B  HAS A ZERO FOR ITS LAST BIT OR DIGIT,   
              C  IS NOT EXACTLY EQUAL TO ONE,   
              EPS  MEASURES THE SEPARATION OF 1.0 FROM   
                   THE NEXT LARGER FLOATING POINT NUMBER.   
       THE DEVELOPERS OF EISPACK WOULD APPRECIATE BEING INFORMED   
       ABOUT ANY SYSTEMS WHERE THESE ASSUMPTIONS DO NOT HOLD.   

       THIS VERSION DATED 4/6/83. */

    a = 1.3333333333333333f;
L10:
    b = a - 1.f;
    c__ = b + b + b;
    eps = (r__1 = c__ - 1.f, dabs(r__1));
    if (eps == 0.f) {
	goto L10;
    }
    ret_val = eps * dabs(*x);
    return ret_val;
} /* epslon_   

   Subroutine */ int hqr_(integer *nm, integer *n, integer *low, integer *igh,
	 real *h__, real *wr, real *wi, integer *ierr)
{
    /* System generated locals */
    integer h_dim1, h_offset, i__1, i__2, i__3, i__4;
    real r__1, r__2;

    /* Builtin functions */
    double sqrt(doublereal), r_sign(real *, real *);

    /* Local variables */
    static real unfl, ovfl, norm, opst;
    static integer i__, j, k, l, m;
    static real p, q, r__, s, t, w, x, y, small;
    static integer na, en, ll, mm;
    extern doublereal slamch_(char *);
    static real zz;
    static logical notlas;
    static real smlnum;
    static integer mp2, itn, its;
    static real ulp;
    static integer enm2;
    static real tst1, tst2;


#define h___ref(a_1,a_2) h__[(a_2)*h_dim1 + a_1]



/*     COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED   
       OPST IS USED TO ACCUMULATE SMALL CONTRIBUTIONS TO OPS   
       TO AVOID ROUNDOFF ERROR   

       THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE HQR,   
       NUM. MATH. 14, 219-231(1970) BY MARTIN, PETERS, AND WILKINSON.   
       HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 359-371(1971).   

       THIS SUBROUTINE FINDS THE EIGENVALUES OF A REAL   
       UPPER HESSENBERG MATRIX BY THE QR METHOD.   

       ON INPUT   

          NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL   
            ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM   
            DIMENSION STATEMENT.   

          N IS THE ORDER OF THE MATRIX.   

          LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING   
            SUBROUTINE  BALANC.  IF  BALANC  HAS NOT BEEN USED,   
            SET LOW=1, IGH=N.   

          H CONTAINS THE UPPER HESSENBERG MATRIX.  INFORMATION ABOUT   
            THE TRANSFORMATIONS USED IN THE REDUCTION TO HESSENBERG   
            FORM BY  ELMHES  OR  ORTHES, IF PERFORMED, IS STORED   
            IN THE REMAINING TRIANGLE UNDER THE HESSENBERG MATRIX.   

       ON OUTPUT   

          H HAS BEEN DESTROYED.  THEREFORE, IT MUST BE SAVED   
            BEFORE CALLING  HQR  IF SUBSEQUENT CALCULATION AND   
            BACK TRANSFORMATION OF EIGENVECTORS IS TO BE PERFORMED.   

          WR AND WI CONTAIN THE REAL AND IMAGINARY PARTS,   
            RESPECTIVELY, OF THE EIGENVALUES.  THE EIGENVALUES   
            ARE UNORDERED EXCEPT THAT COMPLEX CONJUGATE PAIRS   
            OF VALUES APPEAR CONSECUTIVELY WITH THE EIGENVALUE   
            HAVING THE POSITIVE IMAGINARY PART FIRST.  IF AN   
            ERROR EXIT IS MADE, THE EIGENVALUES SHOULD BE CORRECT   
            FOR INDICES IERR+1,...,N.   

          IERR IS SET TO   
            ZERO       FOR NORMAL RETURN,   
            J          IF THE LIMIT OF 30*N ITERATIONS IS EXHAUSTED   
                       WHILE THE J-TH EIGENVALUE IS BEING SOUGHT.   

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,   
       MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY   

       THIS VERSION DATED AUGUST 1983.   
       MODIFIED ON 11/1/89; ADJUSTING INDICES OF LOOPS   
         200, 210, 230, AND 240 TO INCREASE PERFORMANCE. JACK DONGARRA   

       ------------------------------------------------------------------   


       Parameter adjustments */
    --wi;
    --wr;
    h_dim1 = *nm;
    h_offset = 1 + h_dim1 * 1;
    h__ -= h_offset;

    /* Function Body */
    if (*n <= 0) {
	return 0;
    }


/*     INITIALIZE */
    latime_1.itcnt = 0.f;
    opst = 0.f;
    *ierr = 0;
    k = 1;
/*     .......... STORE ROOTS ISOLATED BY BALANC   
                  AND COMPUTE MATRIX NORM .......... */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (i__ >= *low && i__ <= *igh) {
	    goto L50;
	}
	wr[i__] = h___ref(i__, i__);
	wi[i__] = 0.f;
L50:
	;
    }

/*        INCREMENT OPCOUNT FOR COMPUTING MATRIX NORM */
    latime_1.ops += (*igh - *low + 1) * (*igh - *low + 2) / 2;

/*     COMPUTE THE 1-NORM OF MATRIX H */

    norm = 0.f;
    i__1 = *igh;
    for (j = *low; j <= i__1; ++j) {
	s = 0.f;
/* Computing MIN */
	i__3 = *igh, i__4 = j + 1;
	i__2 = min(i__3,i__4);
	for (i__ = *low; i__ <= i__2; ++i__) {
	    s += (r__1 = h___ref(i__, j), dabs(r__1));
/* L4: */
	}
	norm = dmax(norm,s);
/* L5: */
    }

    unfl = slamch_("SAFE MINIMUM");
    ovfl = slamch_("OVERFLOW");
    ulp = slamch_("EPSILON") * slamch_("BASE");
/* Computing MAX */
    r__1 = unfl * (*n / ulp), r__2 = *n / (ulp * ovfl);
    smlnum = dmax(r__1,r__2);
/* Computing MAX */
    r__1 = smlnum, r__2 = ulp * norm;
    small = dmax(r__1,r__2);

    en = *igh;
    t = 0.f;
    itn = *n * 30;
/*     .......... SEARCH FOR NEXT EIGENVALUES .......... */
L60:
    if (en < *low) {
	goto L1001;
    }
    its = 0;
    na = en - 1;
    enm2 = na - 1;
/*     .......... LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT   
                  FOR L=EN STEP -1 UNTIL LOW DO -- ..........   
       REPLACE SPLITTING CRITERION WITH NEW ONE AS IN LAPACK */

L70:
    i__1 = en;
    for (ll = *low; ll <= i__1; ++ll) {
	l = en + *low - ll;
	if (l == *low) {
	    goto L100;
	}
	s = (r__1 = h___ref(l - 1, l - 1), dabs(r__1)) + (r__2 = h___ref(l, l)
		, dabs(r__2));
	if (s == 0.f) {
	    s = norm;
	}
/* Computing MAX */
	r__2 = ulp * s;
	if ((r__1 = h___ref(l, l - 1), dabs(r__1)) <= dmax(r__2,small)) {
	    goto L100;
	}
/* L80: */
    }
/*     .......... FORM SHIFT .......... */
L100:

/*        INCREMENT OP COUNT FOR CONVERGENCE TEST */
    latime_1.ops += en - l + 1 << 1;
    x = h___ref(en, en);
    if (l == en) {
	goto L270;
    }
    y = h___ref(na, na);
    w = h___ref(en, na) * h___ref(na, en);
    if (l == na) {
	goto L280;
    }
    if (itn == 0) {
	goto L1000;
    }
    if (its != 10 && its != 20) {
	goto L130;
    }
/*     .......... FORM EXCEPTIONAL SHIFT ..........   

          INCREMENT OP COUNT FOR FORMING EXCEPTIONAL SHIFT */
    latime_1.ops += en - *low + 6;
    t += x;

    i__1 = en;
    for (i__ = *low; i__ <= i__1; ++i__) {
/* L120: */
	h___ref(i__, i__) = h___ref(i__, i__) - x;
    }

    s = (r__1 = h___ref(en, na), dabs(r__1)) + (r__2 = h___ref(na, enm2), 
	    dabs(r__2));
    x = s * .75f;
    y = x;
    w = s * -.4375f * s;
L130:
    ++its;
    --itn;

/*       UPDATE ITERATION NUMBER */
    latime_1.itcnt = (real) (*n * 30 - itn);
/*     .......... LOOK FOR TWO CONSECUTIVE SMALL   
                  SUB-DIAGONAL ELEMENTS.   
                  FOR M=EN-2 STEP -1 UNTIL L DO -- ..........   
       REPLACE SPLITTING CRITERION WITH NEW ONE AS IN LAPACK */
    i__1 = enm2;
    for (mm = l; mm <= i__1; ++mm) {
	m = enm2 + l - mm;
	zz = h___ref(m, m);
	r__ = x - zz;
	s = y - zz;
	p = (r__ * s - w) / h___ref(m + 1, m) + h___ref(m, m + 1);
	q = h___ref(m + 1, m + 1) - zz - r__ - s;
	r__ = h___ref(m + 2, m + 1);
	s = dabs(p) + dabs(q) + dabs(r__);
	p /= s;
	q /= s;
	r__ /= s;
	if (m == l) {
	    goto L150;
	}
	tst1 = dabs(p) * ((r__1 = h___ref(m - 1, m - 1), dabs(r__1)) + dabs(
		zz) + (r__2 = h___ref(m + 1, m + 1), dabs(r__2)));
	tst2 = (r__1 = h___ref(m, m - 1), dabs(r__1)) * (dabs(q) + dabs(r__));
/* Computing MAX */
	r__1 = ulp * tst1;
	if (tst2 <= dmax(r__1,small)) {
	    goto L150;
	}
/* L140: */
    }

L150:

/*        INCREMENT OPCOUNT FOR LOOP 140 */
    opst += (enm2 - m + 1) * 20;
    mp2 = m + 2;

    i__1 = en;
    for (i__ = mp2; i__ <= i__1; ++i__) {
	h___ref(i__, i__ - 2) = 0.f;
	if (i__ == mp2) {
	    goto L160;
	}
	h___ref(i__, i__ - 3) = 0.f;
L160:
	;
    }
/*     .......... DOUBLE QR STEP INVOLVING ROWS L TO EN AND   
                  COLUMNS M TO EN ..........   

          INCREMENT OPCOUNT FOR LOOP 260 */
    opst += (na - m + 1) * 18;
    i__1 = na;
    for (k = m; k <= i__1; ++k) {
	notlas = k != na;
	if (k == m) {
	    goto L170;
	}
	p = h___ref(k, k - 1);
	q = h___ref(k + 1, k - 1);
	r__ = 0.f;
	if (notlas) {
	    r__ = h___ref(k + 2, k - 1);
	}
	x = dabs(p) + dabs(q) + dabs(r__);
	if (x == 0.f) {
	    goto L260;
	}
	p /= x;
	q /= x;
	r__ /= x;
L170:
	r__1 = sqrt(p * p + q * q + r__ * r__);
	s = r_sign(&r__1, &p);
	if (k == m) {
	    goto L180;
	}
	h___ref(k, k - 1) = -s * x;
	goto L190;
L180:
	if (l != m) {
	    h___ref(k, k - 1) = -h___ref(k, k - 1);
	}
L190:
	p += s;
	x = p / s;
	y = q / s;
	zz = r__ / s;
	q /= p;
	r__ /= p;
	if (notlas) {
	    goto L225;
	}
/*     .......... ROW MODIFICATION ..........   

          INCREMENT OPCOUNT */
	latime_1.ops += (en - k + 1) * 6;
	i__2 = en;
	for (j = k; j <= i__2; ++j) {
	    p = h___ref(k, j) + q * h___ref(k + 1, j);
	    h___ref(k, j) = h___ref(k, j) - p * x;
	    h___ref(k + 1, j) = h___ref(k + 1, j) - p * y;
/* L200: */
	}

/* Computing MIN */
	i__2 = en, i__3 = k + 3;
	j = min(i__2,i__3);
/*     .......... COLUMN MODIFICATION ..........   

          INCREMENT OPCOUNT */
	latime_1.ops += (j - l + 1) * 6;
	i__2 = j;
	for (i__ = l; i__ <= i__2; ++i__) {
	    p = x * h___ref(i__, k) + y * h___ref(i__, k + 1);
	    h___ref(i__, k) = h___ref(i__, k) - p;
	    h___ref(i__, k + 1) = h___ref(i__, k + 1) - p * q;
/* L210: */
	}
	goto L255;
L225:
/*     .......... ROW MODIFICATION ..........   

          INCREMENT OPCOUNT */
	latime_1.ops += (en - k + 1) * 10;
	i__2 = en;
	for (j = k; j <= i__2; ++j) {
	    p = h___ref(k, j) + q * h___ref(k + 1, j) + r__ * h___ref(k + 2, 
		    j);
	    h___ref(k, j) = h___ref(k, j) - p * x;
	    h___ref(k + 1, j) = h___ref(k + 1, j) - p * y;
	    h___ref(k + 2, j) = h___ref(k + 2, j) - p * zz;
/* L230: */
	}

/* Computing MIN */
	i__2 = en, i__3 = k + 3;
	j = min(i__2,i__3);
/*     .......... COLUMN MODIFICATION ..........   

          INCREMENT OPCOUNT */
	latime_1.ops += (j - l + 1) * 10;
	i__2 = j;
	for (i__ = l; i__ <= i__2; ++i__) {
	    p = x * h___ref(i__, k) + y * h___ref(i__, k + 1) + zz * h___ref(
		    i__, k + 2);
	    h___ref(i__, k) = h___ref(i__, k) - p;
	    h___ref(i__, k + 1) = h___ref(i__, k + 1) - p * q;
	    h___ref(i__, k + 2) = h___ref(i__, k + 2) - p * r__;
/* L240: */
	}
L255:

L260:
	;
    }

    goto L70;
/*     .......... ONE ROOT FOUND .......... */
L270:
    wr[en] = x + t;
    wi[en] = 0.f;
    en = na;
    goto L60;
/*     .......... TWO ROOTS FOUND .......... */
L280:
    p = (y - x) / 2.f;
    q = p * p + w;
    zz = sqrt((dabs(q)));
    x += t;

/*        INCREMENT OP COUNT FOR FINDING TWO ROOTS. */
    opst += 8;
    if (q < 0.f) {
	goto L320;
    }
/*     .......... REAL PAIR .......... */
    zz = p + r_sign(&zz, &p);
    wr[na] = x + zz;
    wr[en] = wr[na];
    if (zz != 0.f) {
	wr[en] = x - w / zz;
    }
    wi[na] = 0.f;
    wi[en] = 0.f;
    goto L330;
/*     .......... COMPLEX PAIR .......... */
L320:
    wr[na] = x + p;
    wr[en] = x + p;
    wi[na] = zz;
    wi[en] = -zz;
L330:
    en = enm2;
    goto L60;
/*     .......... SET ERROR -- ALL EIGENVALUES HAVE NOT   
                  CONVERGED AFTER 30*N ITERATIONS .......... */
L1000:
    *ierr = en;
L1001:

/*     COMPUTE FINAL OP COUNT */
    latime_1.ops += opst;
    return 0;
} /* hqr_ */

#undef h___ref


/* Subroutine */ int hqr2_(integer *nm, integer *n, integer *low, integer *
	igh, real *h__, real *wr, real *wi, real *z__, integer *ierr)
{
    /* System generated locals */
    integer h_dim1, h_offset, z_dim1, z_offset, i__1, i__2, i__3, i__4;
    real r__1, r__2, r__3, r__4;

    /* Builtin functions */
    double sqrt(doublereal), r_sign(real *, real *);

    /* Local variables */
    extern /* Subroutine */ int cdiv_(real *, real *, real *, real *, real *, 
	    real *);
    static real unfl, ovfl, norm, opst;
    static integer i__, j, k, l, m;
    static real p, q, r__, s, t, w, x, y, small;
    static integer na, ii, en, jj;
    static real ra, sa;
    static integer ll, mm, nn;
    static real vi;
    extern doublereal slamch_(char *);
    static real vr, zz;
    static logical notlas;
    static real smlnum;
    static integer mp2, itn, its;
    static real ulp;
    static integer enm2;
    static real tst1, tst2;


#define h___ref(a_1,a_2) h__[(a_2)*h_dim1 + a_1]
#define z___ref(a_1,a_2) z__[(a_2)*z_dim1 + a_1]



/*     COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED   
       OPST IS USED TO ACCUMULATE SMALL CONTRIBUTIONS TO OPS   
       TO AVOID ROUNDOFF ERROR   

       THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE HQR2,   
       NUM. MATH. 16, 181-204(1970) BY PETERS AND WILKINSON.   
       HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 372-395(1971).   

       THIS SUBROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS   
       OF A REAL UPPER HESSENBERG MATRIX BY THE QR METHOD.  THE   
       EIGENVECTORS OF A REAL GENERAL MATRIX CAN ALSO BE FOUND   
       IF  ELMHES  AND  ELTRAN  OR  ORTHES  AND  ORTRAN  HAVE   
       BEEN USED TO REDUCE THIS GENERAL MATRIX TO HESSENBERG FORM   
       AND TO ACCUMULATE THE SIMILARITY TRANSFORMATIONS.   

       ON INPUT   

          NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL   
            ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM   
            DIMENSION STATEMENT.   

          N IS THE ORDER OF THE MATRIX.   

          LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING   
            SUBROUTINE  BALANC.  IF  BALANC  HAS NOT BEEN USED,   
            SET LOW=1, IGH=N.   

          H CONTAINS THE UPPER HESSENBERG MATRIX.   

          Z CONTAINS THE TRANSFORMATION MATRIX PRODUCED BY  ELTRAN   
            AFTER THE REDUCTION BY  ELMHES, OR BY  ORTRAN  AFTER THE   
            REDUCTION BY  ORTHES, IF PERFORMED.  IF THE EIGENVECTORS   
            OF THE HESSENBERG MATRIX ARE DESIRED, Z MUST CONTAIN THE   
            IDENTITY MATRIX.   

       ON OUTPUT   

          H HAS BEEN DESTROYED.   

          WR AND WI CONTAIN THE REAL AND IMAGINARY PARTS,   
            RESPECTIVELY, OF THE EIGENVALUES.  THE EIGENVALUES   
            ARE UNORDERED EXCEPT THAT COMPLEX CONJUGATE PAIRS   
            OF VALUES APPEAR CONSECUTIVELY WITH THE EIGENVALUE   
            HAVING THE POSITIVE IMAGINARY PART FIRST.  IF AN   
            ERROR EXIT IS MADE, THE EIGENVALUES SHOULD BE CORRECT   
            FOR INDICES IERR+1,...,N.   

          Z CONTAINS THE REAL AND IMAGINARY PARTS OF THE EIGENVECTORS.   
            IF THE I-TH EIGENVALUE IS REAL, THE I-TH COLUMN OF Z   
            CONTAINS ITS EIGENVECTOR.  IF THE I-TH EIGENVALUE IS COMPLEX   
            WITH POSITIVE IMAGINARY PART, THE I-TH AND (I+1)-TH   
            COLUMNS OF Z CONTAIN THE REAL AND IMAGINARY PARTS OF ITS   
            EIGENVECTOR.  THE EIGENVECTORS ARE UNNORMALIZED.  IF AN   
            ERROR EXIT IS MADE, NONE OF THE EIGENVECTORS HAS BEEN FOUND.   

          IERR IS SET TO   
            ZERO       FOR NORMAL RETURN,   
            J          IF THE LIMIT OF 30*N ITERATIONS IS EXHAUSTED   
                       WHILE THE J-TH EIGENVALUE IS BEING SOUGHT.   

       CALLS CDIV FOR COMPLEX DIVISION.   

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,   
       MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY   

       THIS VERSION DATED AUGUST 1983.   

       ------------------------------------------------------------------   

       Parameter adjustments */
    z_dim1 = *nm;
    z_offset = 1 + z_dim1 * 1;
    z__ -= z_offset;
    --wi;
    --wr;
    h_dim1 = *nm;
    h_offset = 1 + h_dim1 * 1;
    h__ -= h_offset;

    /* Function Body */
    if (*n <= 0) {
	return 0;
    }

/*     INITIALIZE */

    latime_1.itcnt = 0.f;
    opst = 0.f;

    *ierr = 0;
    k = 1;
/*     .......... STORE ROOTS ISOLATED BY BALANC   
                  AND COMPUTE MATRIX NORM .......... */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (i__ >= *low && i__ <= *igh) {
	    goto L50;
	}
	wr[i__] = h___ref(i__, i__);
	wi[i__] = 0.f;
L50:
	;
    }

/*        INCREMENT OPCOUNT FOR COMPUTING MATRIX NORM */
    latime_1.ops += (*igh - *low + 1) * (*igh - *low + 2) / 2;

/*     COMPUTE THE 1-NORM OF MATRIX H */

    norm = 0.f;
    i__1 = *igh;
    for (j = *low; j <= i__1; ++j) {
	s = 0.f;
/* Computing MIN */
	i__3 = *igh, i__4 = j + 1;
	i__2 = min(i__3,i__4);
	for (i__ = *low; i__ <= i__2; ++i__) {
	    s += (r__1 = h___ref(i__, j), dabs(r__1));
/* L4: */
	}
	norm = dmax(norm,s);
/* L5: */
    }

    unfl = slamch_("SAFE MINIMUM");
    ovfl = slamch_("OVERFLOW");
    ulp = slamch_("EPSILON") * slamch_("BASE");
/* Computing MAX */
    r__1 = unfl * (*n / ulp), r__2 = *n / (ulp * ovfl);
    smlnum = dmax(r__1,r__2);
/* Computing MAX */
    r__1 = smlnum, r__2 = ulp * norm;
    small = dmax(r__1,r__2);

    en = *igh;
    t = 0.f;
    itn = *n * 30;
/*     .......... SEARCH FOR NEXT EIGENVALUES .......... */
L60:
    if (en < *low) {
	goto L340;
    }
    its = 0;
    na = en - 1;
    enm2 = na - 1;
/*     .......... LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT   
                  FOR L=EN STEP -1 UNTIL LOW DO -- ..........   
       REPLACE SPLITTING CRITERION WITH NEW ONE AS IN LAPACK */

L70:
    i__1 = en;
    for (ll = *low; ll <= i__1; ++ll) {
	l = en + *low - ll;
	if (l == *low) {
	    goto L100;
	}
	s = (r__1 = h___ref(l - 1, l - 1), dabs(r__1)) + (r__2 = h___ref(l, l)
		, dabs(r__2));
	if (s == 0.f) {
	    s = norm;
	}
/* Computing MAX */
	r__2 = ulp * s;
	if ((r__1 = h___ref(l, l - 1), dabs(r__1)) <= dmax(r__2,small)) {
	    goto L100;
	}
/* L80: */
    }
/*     .......... FORM SHIFT .......... */
L100:

/*        INCREMENT OP COUNT FOR CONVERGENCE TEST */
    latime_1.ops += en - l + 1 << 1;
    x = h___ref(en, en);
    if (l == en) {
	goto L270;
    }
    y = h___ref(na, na);
    w = h___ref(en, na) * h___ref(na, en);
    if (l == na) {
	goto L280;
    }
    if (itn == 0) {
	goto L1000;
    }
    if (its != 10 && its != 20) {
	goto L130;
    }
/*     .......... FORM EXCEPTIONAL SHIFT ..........   

          INCREMENT OP COUNT */
    latime_1.ops += en - *low + 6;
    t += x;

    i__1 = en;
    for (i__ = *low; i__ <= i__1; ++i__) {
/* L120: */
	h___ref(i__, i__) = h___ref(i__, i__) - x;
    }

    s = (r__1 = h___ref(en, na), dabs(r__1)) + (r__2 = h___ref(na, enm2), 
	    dabs(r__2));
    x = s * .75f;
    y = x;
    w = s * -.4375f * s;
L130:
    ++its;
    --itn;

/*       UPDATE ITERATION NUMBER */
    latime_1.itcnt = (real) (*n * 30 - itn);
/*     .......... LOOK FOR TWO CONSECUTIVE SMALL   
                  SUB-DIAGONAL ELEMENTS.   
                  FOR M=EN-2 STEP -1 UNTIL L DO -- .......... */
    i__1 = enm2;
    for (mm = l; mm <= i__1; ++mm) {
	m = enm2 + l - mm;
	zz = h___ref(m, m);
	r__ = x - zz;
	s = y - zz;
	p = (r__ * s - w) / h___ref(m + 1, m) + h___ref(m, m + 1);
	q = h___ref(m + 1, m + 1) - zz - r__ - s;
	r__ = h___ref(m + 2, m + 1);
	s = dabs(p) + dabs(q) + dabs(r__);
	p /= s;
	q /= s;
	r__ /= s;
	if (m == l) {
	    goto L150;
	}
	tst1 = dabs(p) * ((r__1 = h___ref(m - 1, m - 1), dabs(r__1)) + dabs(
		zz) + (r__2 = h___ref(m + 1, m + 1), dabs(r__2)));
	tst2 = (r__1 = h___ref(m, m - 1), dabs(r__1)) * (dabs(q) + dabs(r__));
/* Computing MAX */
	r__1 = ulp * tst1;
	if (tst2 <= dmax(r__1,small)) {
	    goto L150;
	}
/* L140: */
    }

L150:

/*        INCREMENT OPCOUNT FOR LOOP 140 */
    opst += (enm2 - m + 1) * 20;
    mp2 = m + 2;

    i__1 = en;
    for (i__ = mp2; i__ <= i__1; ++i__) {
	h___ref(i__, i__ - 2) = 0.f;
	if (i__ == mp2) {
	    goto L160;
	}
	h___ref(i__, i__ - 3) = 0.f;
L160:
	;
    }
/*     .......... DOUBLE QR STEP INVOLVING ROWS L TO EN AND   
                  COLUMNS M TO EN ..........   

          INCREMENT OPCOUNT FOR LOOP 260 */
    opst += (na - m + 1) * 18;
    i__1 = na;
    for (k = m; k <= i__1; ++k) {
	notlas = k != na;
	if (k == m) {
	    goto L170;
	}
	p = h___ref(k, k - 1);
	q = h___ref(k + 1, k - 1);
	r__ = 0.f;
	if (notlas) {
	    r__ = h___ref(k + 2, k - 1);
	}
	x = dabs(p) + dabs(q) + dabs(r__);
	if (x == 0.f) {
	    goto L260;
	}
	p /= x;
	q /= x;
	r__ /= x;
L170:
	r__1 = sqrt(p * p + q * q + r__ * r__);
	s = r_sign(&r__1, &p);
	if (k == m) {
	    goto L180;
	}
	h___ref(k, k - 1) = -s * x;
	goto L190;
L180:
	if (l != m) {
	    h___ref(k, k - 1) = -h___ref(k, k - 1);
	}
L190:
	p += s;
	x = p / s;
	y = q / s;
	zz = r__ / s;
	q /= p;
	r__ /= p;
	if (notlas) {
	    goto L225;
	}
/*     .......... ROW MODIFICATION ..........   

          INCREMENT OP COUNT FOR LOOP 200 */
	latime_1.ops += (*n - k + 1) * 6;
	i__2 = *n;
	for (j = k; j <= i__2; ++j) {
	    p = h___ref(k, j) + q * h___ref(k + 1, j);
	    h___ref(k, j) = h___ref(k, j) - p * x;
	    h___ref(k + 1, j) = h___ref(k + 1, j) - p * y;
/* L200: */
	}

/* Computing MIN */
	i__2 = en, i__3 = k + 3;
	j = min(i__2,i__3);
/*     .......... COLUMN MODIFICATION ..........   

          INCREMENT OPCOUNT FOR LOOP 210 */
	latime_1.ops += j * 6;
	i__2 = j;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    p = x * h___ref(i__, k) + y * h___ref(i__, k + 1);
	    h___ref(i__, k) = h___ref(i__, k) - p;
	    h___ref(i__, k + 1) = h___ref(i__, k + 1) - p * q;
/* L210: */
	}
/*     .......... ACCUMULATE TRANSFORMATIONS ..........   

          INCREMENT OPCOUNT FOR LOOP 220 */
	latime_1.ops += (*igh - *low + 1) * 6;
	i__2 = *igh;
	for (i__ = *low; i__ <= i__2; ++i__) {
	    p = x * z___ref(i__, k) + y * z___ref(i__, k + 1);
	    z___ref(i__, k) = z___ref(i__, k) - p;
	    z___ref(i__, k + 1) = z___ref(i__, k + 1) - p * q;
/* L220: */
	}
	goto L255;
L225:
/*     .......... ROW MODIFICATION ..........   

          INCREMENT OPCOUNT FOR LOOP 230 */
	latime_1.ops += (*n - k + 1) * 10;
	i__2 = *n;
	for (j = k; j <= i__2; ++j) {
	    p = h___ref(k, j) + q * h___ref(k + 1, j) + r__ * h___ref(k + 2, 
		    j);
	    h___ref(k, j) = h___ref(k, j) - p * x;
	    h___ref(k + 1, j) = h___ref(k + 1, j) - p * y;
	    h___ref(k + 2, j) = h___ref(k + 2, j) - p * zz;
/* L230: */
	}

/* Computing MIN */
	i__2 = en, i__3 = k + 3;
	j = min(i__2,i__3);
/*     .......... COLUMN MODIFICATION ..........   

          INCREMENT OPCOUNT FOR LOOP 240 */
	latime_1.ops += j * 10;
	i__2 = j;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    p = x * h___ref(i__, k) + y * h___ref(i__, k + 1) + zz * h___ref(
		    i__, k + 2);
	    h___ref(i__, k) = h___ref(i__, k) - p;
	    h___ref(i__, k + 1) = h___ref(i__, k + 1) - p * q;
	    h___ref(i__, k + 2) = h___ref(i__, k + 2) - p * r__;
/* L240: */
	}
/*     .......... ACCUMULATE TRANSFORMATIONS ..........   

          INCREMENT OPCOUNT FOR LOOP 250 */
	latime_1.ops += (*igh - *low + 1) * 10;
	i__2 = *igh;
	for (i__ = *low; i__ <= i__2; ++i__) {
	    p = x * z___ref(i__, k) + y * z___ref(i__, k + 1) + zz * z___ref(
		    i__, k + 2);
	    z___ref(i__, k) = z___ref(i__, k) - p;
	    z___ref(i__, k + 1) = z___ref(i__, k + 1) - p * q;
	    z___ref(i__, k + 2) = z___ref(i__, k + 2) - p * r__;
/* L250: */
	}
L255:

L260:
	;
    }

    goto L70;
/*     .......... ONE ROOT FOUND .......... */
L270:
    h___ref(en, en) = x + t;
    wr[en] = h___ref(en, en);
    wi[en] = 0.f;
    en = na;
    goto L60;
/*     .......... TWO ROOTS FOUND .......... */
L280:
    p = (y - x) / 2.f;
    q = p * p + w;
    zz = sqrt((dabs(q)));
    h___ref(en, en) = x + t;
    x = h___ref(en, en);
    h___ref(na, na) = y + t;
    if (q < 0.f) {
	goto L320;
    }
/*     .......... REAL PAIR .......... */
    zz = p + r_sign(&zz, &p);
    wr[na] = x + zz;
    wr[en] = wr[na];
    if (zz != 0.f) {
	wr[en] = x - w / zz;
    }
    wi[na] = 0.f;
    wi[en] = 0.f;
    x = h___ref(en, na);
    s = dabs(x) + dabs(zz);
    p = x / s;
    q = zz / s;
    r__ = sqrt(p * p + q * q);
    p /= r__;
    q /= r__;

/*        INCREMENT OP COUNT FOR FINDING TWO ROOTS. */
    opst += 18;

/*        INCREMENT OP COUNT FOR MODIFICATION AND ACCUMULATION   
          IN LOOP 290, 300, 310 */
    latime_1.ops = latime_1.ops + (*n - na + 1) * 6 + en * 6 + (*igh - *low + 
	    1) * 6;
/*     .......... ROW MODIFICATION .......... */
    i__1 = *n;
    for (j = na; j <= i__1; ++j) {
	zz = h___ref(na, j);
	h___ref(na, j) = q * zz + p * h___ref(en, j);
	h___ref(en, j) = q * h___ref(en, j) - p * zz;
/* L290: */
    }
/*     .......... COLUMN MODIFICATION .......... */
    i__1 = en;
    for (i__ = 1; i__ <= i__1; ++i__) {
	zz = h___ref(i__, na);
	h___ref(i__, na) = q * zz + p * h___ref(i__, en);
	h___ref(i__, en) = q * h___ref(i__, en) - p * zz;
/* L300: */
    }
/*     .......... ACCUMULATE TRANSFORMATIONS .......... */
    i__1 = *igh;
    for (i__ = *low; i__ <= i__1; ++i__) {
	zz = z___ref(i__, na);
	z___ref(i__, na) = q * zz + p * z___ref(i__, en);
	z___ref(i__, en) = q * z___ref(i__, en) - p * zz;
/* L310: */
    }

    goto L330;
/*     .......... COMPLEX PAIR .......... */
L320:
    wr[na] = x + p;
    wr[en] = x + p;
    wi[na] = zz;
    wi[en] = -zz;

/*        INCREMENT OP COUNT FOR FINDING COMPLEX PAIR. */
    opst += 9;
L330:
    en = enm2;
    goto L60;
/*     .......... ALL ROOTS FOUND.  BACKSUBSTITUTE TO FIND   
                  VECTORS OF UPPER TRIANGULAR FORM .......... */
L340:
    if (norm == 0.f) {
	goto L1001;
    }
/*     .......... FOR EN=N STEP -1 UNTIL 1 DO -- .......... */
    i__1 = *n;
    for (nn = 1; nn <= i__1; ++nn) {
	en = *n + 1 - nn;
	p = wr[en];
	q = wi[en];
	na = en - 1;
	if (q < 0.f) {
	    goto L710;
	} else if (q == 0) {
	    goto L600;
	} else {
	    goto L800;
	}
/*     .......... REAL VECTOR .......... */
L600:
	m = en;
	h___ref(en, en) = 1.f;
	if (na == 0) {
	    goto L800;
	}
/*     .......... FOR I=EN-1 STEP -1 UNTIL 1 DO -- .......... */
	i__2 = na;
	for (ii = 1; ii <= i__2; ++ii) {
	    i__ = en - ii;
	    w = h___ref(i__, i__) - p;
	    r__ = 0.f;


/*        INCREMENT OP COUNT FOR LOOP 610 */
	    opst += en - m + 1 << 1;
	    i__3 = en;
	    for (j = m; j <= i__3; ++j) {
/* L610: */
		r__ += h___ref(i__, j) * h___ref(j, en);
	    }

	    if (wi[i__] >= 0.f) {
		goto L630;
	    }
	    zz = w;
	    s = r__;
	    goto L700;
L630:
	    m = i__;
	    if (wi[i__] != 0.f) {
		goto L640;
	    }
	    t = w;
	    if (t != 0.f) {
		goto L635;
	    }
	    tst1 = norm;
	    t = tst1;
L632:
	    t *= .01f;
	    tst2 = norm + t;
	    if (tst2 > tst1) {
		goto L632;
	    }
L635:
	    h___ref(i__, en) = -r__ / t;
	    goto L680;
/*     .......... SOLVE REAL EQUATIONS .......... */
L640:
	    x = h___ref(i__, i__ + 1);
	    y = h___ref(i__ + 1, i__);
	    q = (wr[i__] - p) * (wr[i__] - p) + wi[i__] * wi[i__];
	    t = (x * s - zz * r__) / q;

/*        INCREMENT OP COUNT FOR SOLVING REAL EQUATION. */
	    opst += 13;
	    h___ref(i__, en) = t;
	    if (dabs(x) <= dabs(zz)) {
		goto L650;
	    }
	    h___ref(i__ + 1, en) = (-r__ - w * t) / x;
	    goto L680;
L650:
	    h___ref(i__ + 1, en) = (-s - y * t) / zz;

/*     .......... OVERFLOW CONTROL .......... */
L680:
	    t = (r__1 = h___ref(i__, en), dabs(r__1));
	    if (t == 0.f) {
		goto L700;
	    }
	    tst1 = t;
	    tst2 = tst1 + 1.f / tst1;
	    if (tst2 > tst1) {
		goto L700;
	    }

/*        INCREMENT OP COUNT. */
	    opst += en - i__ + 1;
	    i__3 = en;
	    for (j = i__; j <= i__3; ++j) {
		h___ref(j, en) = h___ref(j, en) / t;
/* L690: */
	    }

L700:
	    ;
	}
/*     .......... END REAL VECTOR .......... */
	goto L800;
/*     .......... COMPLEX VECTOR .......... */
L710:
	m = na;
/*     .......... LAST VECTOR COMPONENT CHOSEN IMAGINARY SO THAT   
                  EIGENVECTOR MATRIX IS TRIANGULAR .......... */
	if ((r__1 = h___ref(en, na), dabs(r__1)) <= (r__2 = h___ref(na, en), 
		dabs(r__2))) {
	    goto L720;
	}
	h___ref(na, na) = q / h___ref(en, na);
	h___ref(na, en) = -(h___ref(en, en) - p) / h___ref(en, na);

/*        INCREMENT OP COUNT. */
	opst += 3;
	goto L730;
L720:
	r__1 = -h___ref(na, en);
	r__2 = h___ref(na, na) - p;
	cdiv_(&c_b90, &r__1, &r__2, &q, &h___ref(na, na), &h___ref(na, en));

/*        INCREMENT OP COUNT IF (ABS(H(EN,NA)) .LE. ABS(H(NA,EN))) */
	opst += 16;
L730:
	h___ref(en, na) = 0.f;
	h___ref(en, en) = 1.f;
	enm2 = na - 1;
	if (enm2 == 0) {
	    goto L800;
	}
/*     .......... FOR I=EN-2 STEP -1 UNTIL 1 DO -- .......... */
	i__2 = enm2;
	for (ii = 1; ii <= i__2; ++ii) {
	    i__ = na - ii;
	    w = h___ref(i__, i__) - p;
	    ra = 0.f;
	    sa = 0.f;


/*        INCREMENT OP COUNT FOR LOOP 760 */
	    opst += en - m + 1 << 2;
	    i__3 = en;
	    for (j = m; j <= i__3; ++j) {
		ra += h___ref(i__, j) * h___ref(j, na);
		sa += h___ref(i__, j) * h___ref(j, en);
/* L760: */
	    }

	    if (wi[i__] >= 0.f) {
		goto L770;
	    }
	    zz = w;
	    r__ = ra;
	    s = sa;
	    goto L795;
L770:
	    m = i__;
	    if (wi[i__] != 0.f) {
		goto L780;
	    }
	    r__1 = -ra;
	    r__2 = -sa;
	    cdiv_(&r__1, &r__2, &w, &q, &h___ref(i__, na), &h___ref(i__, en));

/*        INCREMENT OP COUNT FOR CDIV */
	    opst += 16;
	    goto L790;
/*     .......... SOLVE COMPLEX EQUATIONS .......... */
L780:
	    x = h___ref(i__, i__ + 1);
	    y = h___ref(i__ + 1, i__);
	    vr = (wr[i__] - p) * (wr[i__] - p) + wi[i__] * wi[i__] - q * q;
	    vi = (wr[i__] - p) * 2.f * q;

/*        INCREMENT OPCOUNT (AVERAGE) FOR SOLVING COMPLEX EQUATIONS */
	    opst += 42;
	    if (vr != 0.f || vi != 0.f) {
		goto L784;
	    }
	    tst1 = norm * (dabs(w) + dabs(q) + dabs(x) + dabs(y) + dabs(zz));
	    vr = tst1;
L783:
	    vr *= .01f;
	    tst2 = tst1 + vr;
	    if (tst2 > tst1) {
		goto L783;
	    }
L784:
	    r__1 = x * r__ - zz * ra + q * sa;
	    r__2 = x * s - zz * sa - q * ra;
	    cdiv_(&r__1, &r__2, &vr, &vi, &h___ref(i__, na), &h___ref(i__, en)
		    );
	    if (dabs(x) <= dabs(zz) + dabs(q)) {
		goto L785;
	    }
	    h___ref(i__ + 1, na) = (-ra - w * h___ref(i__, na) + q * h___ref(
		    i__, en)) / x;
	    h___ref(i__ + 1, en) = (-sa - w * h___ref(i__, en) - q * h___ref(
		    i__, na)) / x;
	    goto L790;
L785:
	    r__1 = -r__ - y * h___ref(i__, na);
	    r__2 = -s - y * h___ref(i__, en);
	    cdiv_(&r__1, &r__2, &zz, &q, &h___ref(i__ + 1, na), &h___ref(i__ 
		    + 1, en));

/*     .......... OVERFLOW CONTROL .......... */
L790:
/* Computing MAX */
	    r__3 = (r__1 = h___ref(i__, na), dabs(r__1)), r__4 = (r__2 = 
		    h___ref(i__, en), dabs(r__2));
	    t = dmax(r__3,r__4);
	    if (t == 0.f) {
		goto L795;
	    }
	    tst1 = t;
	    tst2 = tst1 + 1.f / tst1;
	    if (tst2 > tst1) {
		goto L795;
	    }

/*        INCREMENT OP COUNT. */
	    opst += en - i__ + 1 << 1;
	    i__3 = en;
	    for (j = i__; j <= i__3; ++j) {
		h___ref(j, na) = h___ref(j, na) / t;
		h___ref(j, en) = h___ref(j, en) / t;
/* L792: */
	    }

L795:
	    ;
	}
/*     .......... END COMPLEX VECTOR .......... */
L800:
	;
    }
/*     .......... END BACK SUBSTITUTION.   
                  VECTORS OF ISOLATED ROOTS .......... */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (i__ >= *low && i__ <= *igh) {
	    goto L840;
	}

	i__2 = *n;
	for (j = i__; j <= i__2; ++j) {
/* L820: */
	    z___ref(i__, j) = h___ref(i__, j);
	}

L840:
	;
    }
/*     .......... MULTIPLY BY TRANSFORMATION MATRIX TO GIVE   
                  VECTORS OF ORIGINAL FULL MATRIX.   
                  FOR J=N STEP -1 UNTIL LOW DO -- .......... */
    i__1 = *n;
    for (jj = *low; jj <= i__1; ++jj) {
	j = *n + *low - jj;
	m = min(j,*igh);


/*        INCREMENT OP COUNT. */
	latime_1.ops += (*igh - *low + 1 << 1) * (m - *low + 1);
	i__2 = *igh;
	for (i__ = *low; i__ <= i__2; ++i__) {
	    zz = 0.f;

	    i__3 = m;
	    for (k = *low; k <= i__3; ++k) {
/* L860: */
		zz += z___ref(i__, k) * h___ref(k, j);
	    }

	    z___ref(i__, j) = zz;
/* L880: */
	}
    }

    goto L1001;
/*     .......... SET ERROR -- ALL EIGENVALUES HAVE NOT   
                  CONVERGED AFTER 30*N ITERATIONS .......... */
L1000:
    *ierr = en;
L1001:

/*     COMPUTE FINAL OP COUNT */
    latime_1.ops += opst;
    return 0;
} /* hqr2_ */

#undef z___ref
#undef h___ref


/* Subroutine */ int imtql1_(integer *n, real *d__, real *e, integer *ierr)
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    real r__1, r__2;

    /* Builtin functions */
    double r_sign(real *, real *);

    /* Local variables */
    static real b, c__, f, g;
    static integer i__, j, l, m;
    static real p, r__, s;
    static integer ii;
    extern doublereal slamch_(char *), pythag_(real *, real *);
    static integer mml;
    static real eps, tst;


/*     EISPACK ROUTINE   
       MODIFIED FOR COMPARISON WITH LAPACK ROUTINES.   

       CONVERGENCE TEST WAS MODIFIED TO BE THE SAME AS IN SSTEQR.   



       COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED   
       OPST IS USED TO ACCUMULATE CONTRIBUTIONS TO OPS FROM   
       FUNCTION PYTHAG.  IT IS PASSED TO AND FROM PYTHAG   
       THROUGH COMMON BLOCK PYTHOP.   


       THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE IMTQL1,   
       NUM. MATH. 12, 377-383(1968) BY MARTIN AND WILKINSON,   
       AS MODIFIED IN NUM. MATH. 15, 450(1970) BY DUBRULLE.   
       HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 241-248(1971).   

       THIS SUBROUTINE FINDS THE EIGENVALUES OF A SYMMETRIC   
       TRIDIAGONAL MATRIX BY THE IMPLICIT QL METHOD.   

       ON INPUT   

          N IS THE ORDER OF THE MATRIX.   

          D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX.   

          E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE INPUT MATRIX   
            IN ITS LAST N-1 POSITIONS.  E(1) IS ARBITRARY.   

        ON OUTPUT   

          D CONTAINS THE EIGENVALUES IN ASCENDING ORDER.  IF AN   
            ERROR EXIT IS MADE, THE EIGENVALUES ARE CORRECT AND   
            ORDERED FOR INDICES 1,2,...IERR-1, BUT MAY NOT BE   
            THE SMALLEST EIGENVALUES.   

          E HAS BEEN DESTROYED.   

          IERR IS SET TO   
            ZERO       FOR NORMAL RETURN,   
            J          IF THE J-TH EIGENVALUE HAS NOT BEEN   
                       DETERMINED AFTER 40 ITERATIONS.   

       CALLS PYTHAG FOR  SQRT(A*A + B*B) .   

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,   
       MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY   

       THIS VERSION DATED AUGUST 1983.   

       ------------------------------------------------------------------   

       Parameter adjustments */
    --e;
    --d__;

    /* Function Body */
    *ierr = 0;
    if (*n == 1) {
	goto L1001;
    }

/*        INITIALIZE ITERATION COUNT AND OPST */
    latime_1.itcnt = 0.f;
    pythop_1.opst = 0.f;

/*     DETERMINE THE UNIT ROUNDOFF FOR THIS ENVIRONMENT. */

    eps = slamch_("EPSILON");

    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
/* L100: */
	e[i__ - 1] = e[i__];
    }

    e[*n] = 0.f;

    i__1 = *n;
    for (l = 1; l <= i__1; ++l) {
	j = 0;
/*     .......... LOOK FOR SMALL SUB-DIAGONAL ELEMENT .......... */
L105:
	i__2 = *n;
	for (m = l; m <= i__2; ++m) {
	    if (m == *n) {
		goto L120;
	    }
	    tst = (r__1 = e[m], dabs(r__1));
	    if (tst <= eps * ((r__1 = d__[m], dabs(r__1)) + (r__2 = d__[m + 1]
		    , dabs(r__2)))) {
		goto L120;
	    }
/*            TST1 = ABS(D(M)) + ABS(D(M+1))   
              TST2 = TST1 + ABS(E(M))   
              IF (TST2 .EQ. TST1) GO TO 120   
   L110: */
	}

L120:
	p = d__[l];

/*        INCREMENT OPCOUNT FOR FINDING SMALL SUBDIAGONAL ELEMENT.   
   Computing MIN */
	i__2 = m, i__3 = *n - 1;
	latime_1.ops += min(i__2,i__3) - l + 1 << 1;
	if (m == l) {
	    goto L215;
	}
	if (j == 40) {
	    goto L1000;
	}
	++j;
/*     .......... FORM SHIFT .......... */
	g = (d__[l + 1] - p) / (e[l] * 2.f);
	r__ = pythag_(&g, &c_b114);
	g = d__[m] - p + e[l] / (g + r_sign(&r__, &g));

/*        INCREMENT OPCOUNT FOR FORMING SHIFT. */
	latime_1.ops += 7;
	s = 1.f;
	c__ = 1.f;
	p = 0.f;
	mml = m - l;
/*     .......... FOR I=M-1 STEP -1 UNTIL L DO -- .......... */
	i__2 = mml;
	for (ii = 1; ii <= i__2; ++ii) {
	    i__ = m - ii;
	    f = s * e[i__];
	    b = c__ * e[i__];
	    r__ = pythag_(&f, &g);
	    e[i__ + 1] = r__;
	    if (r__ == 0.f) {
		goto L210;
	    }
	    s = f / r__;
	    c__ = g / r__;
	    g = d__[i__ + 1] - p;
	    r__ = (d__[i__] - g) * s + c__ * 2.f * b;
	    p = s * r__;
	    d__[i__ + 1] = g + p;
	    g = c__ * r__ - b;
/* L200: */
	}

	d__[l] -= p;
	e[l] = g;
	e[m] = 0.f;

/*        INCREMENT OPCOUNT FOR INNER LOOP. */
	latime_1.ops = latime_1.ops + mml * 14 + 1;

/*        INCREMENT ITERATION COUNTER */
	latime_1.itcnt += 1;
	goto L105;
/*     .......... RECOVER FROM UNDERFLOW .......... */
L210:
	d__[i__ + 1] -= p;
	e[m] = 0.f;

/*        INCREMENT OPCOUNT FOR INNER LOOP, WHEN UNDERFLOW OCCURS. */
	latime_1.ops = latime_1.ops + 2 + (ii - 1) * 14 + 1;
	goto L105;
/*     .......... ORDER EIGENVALUES .......... */
L215:
	if (l == 1) {
	    goto L250;
	}
/*     .......... FOR I=L STEP -1 UNTIL 2 DO -- .......... */
	i__2 = l;
	for (ii = 2; ii <= i__2; ++ii) {
	    i__ = l + 2 - ii;
	    if (p >= d__[i__ - 1]) {
		goto L270;
	    }
	    d__[i__] = d__[i__ - 1];
/* L230: */
	}

L250:
	i__ = 1;
L270:
	d__[i__] = p;
/* L290: */
    }

    goto L1001;
/*     .......... SET ERROR -- NO CONVERGENCE TO AN   
                  EIGENVALUE AFTER 40 ITERATIONS .......... */
L1000:
    *ierr = l;
L1001:

/*     COMPUTE FINAL OP COUNT */
    latime_1.ops += pythop_1.opst;
    return 0;
} /* imtql1_   

   Subroutine */ int imtql2_(integer *nm, integer *n, real *d__, real *e, 
	real *z__, integer *ierr)
{
    /* System generated locals */
    integer z_dim1, z_offset, i__1, i__2, i__3;
    real r__1, r__2;

    /* Builtin functions */
    double r_sign(real *, real *);

    /* Local variables */
    static real b, c__, f, g;
    static integer i__, j, k, l, m;
    static real p, r__, s;
    static integer ii;
    extern doublereal slamch_(char *), pythag_(real *, real *);
    static integer mml;
    static real eps, tst;


#define z___ref(a_1,a_2) z__[(a_2)*z_dim1 + a_1]


/*     EISPACK ROUTINE.  MODIFIED FOR COMPARISON WITH LAPACK.   

       CONVERGENCE TEST WAS MODIFIED TO BE THE SAME AS IN SSTEQR.   



       COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED   
       OPST IS USED TO ACCUMULATE CONTRIBUTIONS TO OPS FROM   
       FUNCTION PYTHAG.  IT IS PASSED TO AND FROM PYTHAG   
       THROUGH COMMON BLOCK PYTHOP.   

       THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE IMTQL2,   
       NUM. MATH. 12, 377-383(1968) BY MARTIN AND WILKINSON,   
       AS MODIFIED IN NUM. MATH. 15, 450(1970) BY DUBRULLE.   
       HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 241-248(1971).   

       THIS SUBROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS   
       OF A SYMMETRIC TRIDIAGONAL MATRIX BY THE IMPLICIT QL METHOD.   
       THE EIGENVECTORS OF A FULL SYMMETRIC MATRIX CAN ALSO   
       BE FOUND IF  TRED2  HAS BEEN USED TO REDUCE THIS   
       FULL MATRIX TO TRIDIAGONAL FORM.   

       ON INPUT   

          NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL   
            ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM   
            DIMENSION STATEMENT.   

          N IS THE ORDER OF THE MATRIX.   

          D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX.   

          E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE INPUT MATRIX   
            IN ITS LAST N-1 POSITIONS.  E(1) IS ARBITRARY.   

          Z CONTAINS THE TRANSFORMATION MATRIX PRODUCED IN THE   
            REDUCTION BY  TRED2, IF PERFORMED.  IF THE EIGENVECTORS   
            OF THE TRIDIAGONAL MATRIX ARE DESIRED, Z MUST CONTAIN   
            THE IDENTITY MATRIX.   

        ON OUTPUT   

          D CONTAINS THE EIGENVALUES IN ASCENDING ORDER.  IF AN   
            ERROR EXIT IS MADE, THE EIGENVALUES ARE CORRECT BUT   
            UNORDERED FOR INDICES 1,2,...,IERR-1.   

          E HAS BEEN DESTROYED.   

          Z CONTAINS ORTHONORMAL EIGENVECTORS OF THE SYMMETRIC   
            TRIDIAGONAL (OR FULL) MATRIX.  IF AN ERROR EXIT IS MADE,   
            Z CONTAINS THE EIGENVECTORS ASSOCIATED WITH THE STORED   
            EIGENVALUES.   

          IERR IS SET TO   
            ZERO       FOR NORMAL RETURN,   
            J          IF THE J-TH EIGENVALUE HAS NOT BEEN   
                       DETERMINED AFTER 40 ITERATIONS.   

       CALLS PYTHAG FOR  SQRT(A*A + B*B) .   

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,   
       MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY   

       THIS VERSION DATED AUGUST 1983.   

       ------------------------------------------------------------------   

       Parameter adjustments */
    z_dim1 = *nm;
    z_offset = 1 + z_dim1 * 1;
    z__ -= z_offset;
    --e;
    --d__;

    /* Function Body */
    *ierr = 0;
    if (*n == 1) {
	goto L1001;
    }

/*        INITIALIZE ITERATION COUNT AND OPST */
    latime_1.itcnt = 0.f;
    pythop_1.opst = 0.f;

/*     DETERMINE UNIT ROUNDOFF FOR THIS MACHINE. */
    eps = slamch_("EPSILON");

    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
/* L100: */
	e[i__ - 1] = e[i__];
    }

    e[*n] = 0.f;

    i__1 = *n;
    for (l = 1; l <= i__1; ++l) {
	j = 0;
/*     .......... LOOK FOR SMALL SUB-DIAGONAL ELEMENT .......... */
L105:
	i__2 = *n;
	for (m = l; m <= i__2; ++m) {
	    if (m == *n) {
		goto L120;
	    }
/*            TST1 = ABS(D(M)) + ABS(D(M+1))   
              TST2 = TST1 + ABS(E(M))   
              IF (TST2 .EQ. TST1) GO TO 120 */
	    tst = (r__1 = e[m], dabs(r__1));
	    if (tst <= eps * ((r__1 = d__[m], dabs(r__1)) + (r__2 = d__[m + 1]
		    , dabs(r__2)))) {
		goto L120;
	    }
/* L110: */
	}

L120:
	p = d__[l];

/*        INCREMENT OPCOUNT FOR FINDING SMALL SUBDIAGONAL ELEMENT. */
	latime_1.ops += min(m,*n) - l + 1 << 1;
	if (m == l) {
	    goto L240;
	}
	if (j == 40) {
	    goto L1000;
	}
	++j;
/*     .......... FORM SHIFT .......... */
	g = (d__[l + 1] - p) / (e[l] * 2.f);
	r__ = pythag_(&g, &c_b114);
	g = d__[m] - p + e[l] / (g + r_sign(&r__, &g));

/*        INCREMENT OPCOUNT FOR FORMING SHIFT. */
	latime_1.ops += 7;
	s = 1.f;
	c__ = 1.f;
	p = 0.f;
	mml = m - l;
/*     .......... FOR I=M-1 STEP -1 UNTIL L DO -- .......... */
	i__2 = mml;
	for (ii = 1; ii <= i__2; ++ii) {
	    i__ = m - ii;
	    f = s * e[i__];
	    b = c__ * e[i__];
	    r__ = pythag_(&f, &g);
	    e[i__ + 1] = r__;
	    if (r__ == 0.f) {
		goto L210;
	    }
	    s = f / r__;
	    c__ = g / r__;
	    g = d__[i__ + 1] - p;
	    r__ = (d__[i__] - g) * s + c__ * 2.f * b;
	    p = s * r__;
	    d__[i__ + 1] = g + p;
	    g = c__ * r__ - b;
/*     .......... FORM VECTOR .......... */
	    i__3 = *n;
	    for (k = 1; k <= i__3; ++k) {
		f = z___ref(k, i__ + 1);
		z___ref(k, i__ + 1) = s * z___ref(k, i__) + c__ * f;
		z___ref(k, i__) = c__ * z___ref(k, i__) - s * f;
/* L180: */
	    }

/* L200: */
	}

	d__[l] -= p;
	e[l] = g;
	e[m] = 0.f;

/*        INCREMENT OPCOUNT FOR INNER LOOP. */
	latime_1.ops = latime_1.ops + mml * (*n * 6 + 14) + 1;

/*        INCREMENT ITERATION COUNTER */
	latime_1.itcnt += 1;
	goto L105;
/*     .......... RECOVER FROM UNDERFLOW .......... */
L210:
	d__[i__ + 1] -= p;
	e[m] = 0.f;

/*        INCREMENT OPCOUNT FOR INNER LOOP, WHEN UNDERFLOW OCCURS. */
	latime_1.ops = latime_1.ops + 2 + (ii - 1) * (*n * 6 + 14) + 1;
	goto L105;
L240:
	;
    }
/*     .......... ORDER EIGENVALUES AND EIGENVECTORS .......... */
    i__1 = *n;
    for (ii = 2; ii <= i__1; ++ii) {
	i__ = ii - 1;
	k = i__;
	p = d__[i__];

	i__2 = *n;
	for (j = ii; j <= i__2; ++j) {
	    if (d__[j] >= p) {
		goto L260;
	    }
	    k = j;
	    p = d__[j];
L260:
	    ;
	}

	if (k == i__) {
	    goto L300;
	}
	d__[k] = d__[i__];
	d__[i__] = p;

	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {
	    p = z___ref(j, i__);
	    z___ref(j, i__) = z___ref(j, k);
	    z___ref(j, k) = p;
/* L280: */
	}

L300:
	;
    }

    goto L1001;
/*     .......... SET ERROR -- NO CONVERGENCE TO AN   
                  EIGENVALUE AFTER 40 ITERATIONS .......... */
L1000:
    *ierr = l;
L1001:

/*     COMPUTE FINAL OP COUNT */
    latime_1.ops += pythop_1.opst;
    return 0;
} /* imtql2_ */

#undef z___ref


/* Subroutine */ int invit_(integer *nm, integer *n, real *a, real *wr, real *
	wi, logical *select, integer *mm, integer *m, real *z__, integer *
	ierr, real *rm1, real *rv1, real *rv2)
{
    /* System generated locals */
    integer a_dim1, a_offset, z_dim1, z_offset, rm1_dim1, rm1_offset, i__1, 
	    i__2, i__3;
    real r__1, r__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    extern /* Subroutine */ int cdiv_(real *, real *, real *, real *, real *, 
	    real *);
    static real norm, opst;
    static integer i__, j, k, l, s;
    static real t, w, x, y;
    static integer n1;
    static real normv;
    static integer ii;
    static real ilambd;
    static integer ip, mp, ns, uk;
    static real rlambd;
    extern doublereal slamch_(char *), pythag_(real *, real *);
    static integer km1, ip1;
    static real growto, ukroot;
    static integer its;
    static real ulp, eps3;


#define a_ref(a_1,a_2) a[(a_2)*a_dim1 + a_1]
#define z___ref(a_1,a_2) z__[(a_2)*z_dim1 + a_1]
#define rm1_ref(a_1,a_2) rm1[(a_2)*rm1_dim1 + a_1]



/*     COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED   
       OPST IS USED TO ACCUMULATE SMALL CONTRIBUTIONS TO OPS   
       TO AVOID ROUNDOFF ERROR   

       THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE INVIT   
       BY PETERS AND WILKINSON.   
       HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 418-439(1971).   

       THIS SUBROUTINE FINDS THOSE EIGENVECTORS OF A REAL UPPER   
       HESSENBERG MATRIX CORRESPONDING TO SPECIFIED EIGENVALUES,   
       USING INVERSE ITERATION.   

       ON INPUT   

          NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL   
            ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM   
            DIMENSION STATEMENT.   

          N IS THE ORDER OF THE MATRIX.   

          A CONTAINS THE HESSENBERG MATRIX.   

          WR AND WI CONTAIN THE REAL AND IMAGINARY PARTS, RESPECTIVELY,   
            OF THE EIGENVALUES OF THE MATRIX.  THE EIGENVALUES MUST BE   
            STORED IN A MANNER IDENTICAL TO THAT OF SUBROUTINE  HQR,   
            WHICH RECOGNIZES POSSIBLE SPLITTING OF THE MATRIX.   

          SELECT SPECIFIES THE EIGENVECTORS TO BE FOUND. THE   
            EIGENVECTOR CORRESPONDING TO THE J-TH EIGENVALUE IS   
            SPECIFIED BY SETTING SELECT(J) TO .TRUE..   

          MM SHOULD BE SET TO AN UPPER BOUND FOR THE NUMBER OF   
            COLUMNS REQUIRED TO STORE THE EIGENVECTORS TO BE FOUND.   
            NOTE THAT TWO COLUMNS ARE REQUIRED TO STORE THE   
            EIGENVECTOR CORRESPONDING TO A COMPLEX EIGENVALUE.   

       ON OUTPUT   

          A AND WI ARE UNALTERED.   

          WR MAY HAVE BEEN ALTERED SINCE CLOSE EIGENVALUES ARE PERTURBED   
            SLIGHTLY IN SEARCHING FOR INDEPENDENT EIGENVECTORS.   

          SELECT MAY HAVE BEEN ALTERED.  IF THE ELEMENTS CORRESPONDING   
            TO A PAIR OF CONJUGATE COMPLEX EIGENVALUES WERE EACH   
            INITIALLY SET TO .TRUE., THE PROGRAM RESETS THE SECOND OF   
            THE TWO ELEMENTS TO .FALSE..   

          M IS THE NUMBER OF COLUMNS ACTUALLY USED TO STORE   
            THE EIGENVECTORS.   

          Z CONTAINS THE REAL AND IMAGINARY PARTS OF THE EIGENVECTORS.   
            IF THE NEXT SELECTED EIGENVALUE IS REAL, THE NEXT COLUMN   
            OF Z CONTAINS ITS EIGENVECTOR.  IF THE EIGENVALUE IS   
            COMPLEX, THE NEXT TWO COLUMNS OF Z CONTAIN THE REAL AND   
            IMAGINARY PARTS OF ITS EIGENVECTOR.  THE EIGENVECTORS ARE   
            NORMALIZED SO THAT THE COMPONENT OF LARGEST MAGNITUDE IS 1.   
            ANY VECTOR WHICH FAILS THE ACCEPTANCE TEST IS SET TO ZERO.   

          IERR IS SET TO   
            ZERO       FOR NORMAL RETURN,   
            -(2*N+1)   IF MORE THAN MM COLUMNS OF Z ARE NECESSARY   
                       TO STORE THE EIGENVECTORS CORRESPONDING TO   
                       THE SPECIFIED EIGENVALUES.   
            -K         IF THE ITERATION CORRESPONDING TO THE K-TH   
                       VALUE FAILS,   
            -(N+K)     IF BOTH ERROR SITUATIONS OCCUR.   

          RM1, RV1, AND RV2 ARE TEMPORARY STORAGE ARRAYS.  NOTE THAT RM1   
            IS SQUARE OF DIMENSION N BY N AND, AUGMENTED BY TWO COLUMNS   
            OF Z, IS THE TRANSPOSE OF THE CORRESPONDING ALGOL B ARRAY.   

       THE ALGOL PROCEDURE GUESSVEC APPEARS IN INVIT IN LINE.   

       CALLS CDIV FOR COMPLEX DIVISION.   
       CALLS PYTHAG FOR  SQRT(A*A + B*B) .   

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,   
       MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY   

       THIS VERSION DATED AUGUST 1983.   

       ------------------------------------------------------------------   

       GET ULP FROM SLAMCH FOR NEW SMALL PERTURBATION AS IN LAPACK   
       Parameter adjustments */
    --rv2;
    --rv1;
    rm1_dim1 = *n;
    rm1_offset = 1 + rm1_dim1 * 1;
    rm1 -= rm1_offset;
    --select;
    --wi;
    --wr;
    a_dim1 = *nm;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    z_dim1 = *nm;
    z_offset = 1 + z_dim1 * 1;
    z__ -= z_offset;

    /* Function Body */
    if (*n <= 0) {
	return 0;
    }
    ulp = slamch_("EPSILON");


/*     INITIALIZE */
    opst = 0.f;
    *ierr = 0;
    uk = 0;
    s = 1;
/*     .......... IP = 0, REAL EIGENVALUE   
                       1, FIRST OF CONJUGATE COMPLEX PAIR   
                      -1, SECOND OF CONJUGATE COMPLEX PAIR .......... */
    ip = 0;
    n1 = *n - 1;

    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {
	if (wi[k] == 0.f || ip < 0) {
	    goto L100;
	}
	ip = 1;
	if (select[k] && select[k + 1]) {
	    select[k + 1] = FALSE_;
	}
L100:
	if (! select[k]) {
	    goto L960;
	}
	if (wi[k] != 0.f) {
	    ++s;
	}
	if (s > *mm) {
	    goto L1000;
	}
	if (uk >= k) {
	    goto L200;
	}
/*     .......... CHECK FOR POSSIBLE SPLITTING .......... */
	i__2 = *n;
	for (uk = k; uk <= i__2; ++uk) {
	    if (uk == *n) {
		goto L140;
	    }
	    if (a_ref(uk + 1, uk) == 0.f) {
		goto L140;
	    }
/* L120: */
	}
/*     .......... COMPUTE INFINITY NORM OF LEADING UK BY UK   
                  (HESSENBERG) MATRIX .......... */
L140:
	norm = 0.f;
	mp = 1;


/*        INCREMENT OPCOUNT FOR COMPUTING MATRIX NORM */
	latime_1.ops += uk * (uk - 1) / 2;
	i__2 = uk;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    x = 0.f;

	    i__3 = uk;
	    for (j = mp; j <= i__3; ++j) {
/* L160: */
		x += (r__1 = a_ref(i__, j), dabs(r__1));
	    }

	    if (x > norm) {
		norm = x;
	    }
	    mp = i__;
/* L180: */
	}
/*     .......... EPS3 REPLACES ZERO PIVOT IN DECOMPOSITION   
                  AND CLOSE ROOTS ARE MODIFIED BY EPS3 .......... */
	if (norm == 0.f) {
	    norm = 1.f;
	}
/*        EPS3 = EPSLON(NORM)   

          INCREMENT OPCOUNT */
	opst += 3;
	eps3 = norm * ulp;
/*     .......... GROWTO IS THE CRITERION FOR THE GROWTH .......... */
	ukroot = (real) uk;
	ukroot = sqrt(ukroot);
	growto = .1f / ukroot;
L200:
	rlambd = wr[k];
	ilambd = wi[k];
	if (k == 1) {
	    goto L280;
	}
	km1 = k - 1;
	goto L240;
/*     .......... PERTURB EIGENVALUE IF IT IS CLOSE   
                  TO ANY PREVIOUS EIGENVALUE .......... */
L220:
	rlambd += eps3;
/*     .......... FOR I=K-1 STEP -1 UNTIL 1 DO -- .......... */
L240:
	i__2 = km1;
	for (ii = 1; ii <= i__2; ++ii) {
	    i__ = k - ii;
	    if (select[i__] && (r__1 = wr[i__] - rlambd, dabs(r__1)) < eps3 &&
		     (r__2 = wi[i__] - ilambd, dabs(r__2)) < eps3) {
		goto L220;
	    }
/* L260: */
	}

/*        INCREMENT OPCOUNT FOR LOOP 260 (ASSUME THAT ALL EIGENVALUES   
          ARE DIFFERENT) */
	opst += k - 1 << 1;

	wr[k] = rlambd;
/*     .......... PERTURB CONJUGATE EIGENVALUE TO MATCH .......... */
	ip1 = k + ip;
	wr[ip1] = rlambd;
/*     .......... FORM UPPER HESSENBERG A-RLAMBD*I (TRANSPOSED)   
                  AND INITIAL REAL VECTOR .......... */
L280:
	mp = 1;


/*        INCREMENT OP COUNT FOR LOOP 320 */
	latime_1.ops += uk;
	i__2 = uk;
	for (i__ = 1; i__ <= i__2; ++i__) {

	    i__3 = uk;
	    for (j = mp; j <= i__3; ++j) {
/* L300: */
		rm1_ref(j, i__) = a_ref(i__, j);
	    }

	    rm1_ref(i__, i__) = rm1_ref(i__, i__) - rlambd;
	    mp = i__;
	    rv1[i__] = eps3;
/* L320: */
	}

	its = 0;
	if (ilambd != 0.f) {
	    goto L520;
	}
/*     .......... REAL EIGENVALUE.   
                  TRIANGULAR DECOMPOSITION WITH INTERCHANGES,   
                  REPLACING ZERO PIVOTS BY EPS3 .......... */
	if (uk == 1) {
	    goto L420;
	}


/*        INCREMENT OPCOUNT LU DECOMPOSITION */
	latime_1.ops += (uk - 1) * (uk + 2);
	i__2 = uk;
	for (i__ = 2; i__ <= i__2; ++i__) {
	    mp = i__ - 1;
	    if ((r__1 = rm1_ref(mp, i__), dabs(r__1)) <= (r__2 = rm1_ref(mp, 
		    mp), dabs(r__2))) {
		goto L360;
	    }

	    i__3 = uk;
	    for (j = mp; j <= i__3; ++j) {
		y = rm1_ref(j, i__);
		rm1_ref(j, i__) = rm1_ref(j, mp);
		rm1_ref(j, mp) = y;
/* L340: */
	    }

L360:
	    if (rm1_ref(mp, mp) == 0.f) {
		rm1_ref(mp, mp) = eps3;
	    }
	    x = rm1_ref(mp, i__) / rm1_ref(mp, mp);
	    if (x == 0.f) {
		goto L400;
	    }

	    i__3 = uk;
	    for (j = i__; j <= i__3; ++j) {
/* L380: */
		rm1_ref(j, i__) = rm1_ref(j, i__) - x * rm1_ref(j, mp);
	    }

L400:
	    ;
	}

L420:
	if (rm1_ref(uk, uk) == 0.f) {
	    rm1_ref(uk, uk) = eps3;
	}
/*     .......... BACK SUBSTITUTION FOR REAL VECTOR   
                  FOR I=UK STEP -1 UNTIL 1 DO -- .......... */
L440:
	i__2 = uk;
	for (ii = 1; ii <= i__2; ++ii) {
	    i__ = uk + 1 - ii;
	    y = rv1[i__];
	    if (i__ == uk) {
		goto L480;
	    }
	    ip1 = i__ + 1;

	    i__3 = uk;
	    for (j = ip1; j <= i__3; ++j) {
/* L460: */
		y -= rm1_ref(j, i__) * rv1[j];
	    }

L480:
	    rv1[i__] = y / rm1_ref(i__, i__);
/* L500: */
	}

/*        INCREMENT OP COUNT FOR BACK SUBSTITUTION LOOP 500 */
	latime_1.ops += uk * (uk + 1);

	goto L740;
/*     .......... COMPLEX EIGENVALUE.   
                  TRIANGULAR DECOMPOSITION WITH INTERCHANGES,   
                  REPLACING ZERO PIVOTS BY EPS3.  STORE IMAGINARY   
                  PARTS IN UPPER TRIANGLE STARTING AT (1,3) .......... */
L520:
	ns = *n - s;
	z___ref(1, s - 1) = -ilambd;
	z___ref(1, s) = 0.f;
	if (*n == 2) {
	    goto L550;
	}
	rm1_ref(1, 3) = -ilambd;
	z___ref(1, s - 1) = 0.f;
	if (*n == 3) {
	    goto L550;
	}

	i__2 = *n;
	for (i__ = 4; i__ <= i__2; ++i__) {
/* L540: */
	    rm1_ref(1, i__) = 0.f;
	}

L550:
	i__2 = uk;
	for (i__ = 2; i__ <= i__2; ++i__) {
	    mp = i__ - 1;
	    w = rm1_ref(mp, i__);
	    if (i__ < *n) {
		t = rm1_ref(mp, i__ + 1);
	    }
	    if (i__ == *n) {
		t = z___ref(mp, s - 1);
	    }
	    x = rm1_ref(mp, mp) * rm1_ref(mp, mp) + t * t;
	    if (w * w <= x) {
		goto L580;
	    }
	    x = rm1_ref(mp, mp) / w;
	    y = t / w;
	    rm1_ref(mp, mp) = w;
	    if (i__ < *n) {
		rm1_ref(mp, i__ + 1) = 0.f;
	    }
	    if (i__ == *n) {
		z___ref(mp, s - 1) = 0.f;
	    }


/*        INCREMENT OPCOUNT FOR LOOP 560 */
	    latime_1.ops += uk - i__ + 1 << 2;
	    i__3 = uk;
	    for (j = i__; j <= i__3; ++j) {
		w = rm1_ref(j, i__);
		rm1_ref(j, i__) = rm1_ref(j, mp) - x * w;
		rm1_ref(j, mp) = w;
		if (j < n1) {
		    goto L555;
		}
		l = j - ns;
		z___ref(i__, l) = z___ref(mp, l) - y * w;
		z___ref(mp, l) = 0.f;
		goto L560;
L555:
		rm1_ref(i__, j + 2) = rm1_ref(mp, j + 2) - y * w;
		rm1_ref(mp, j + 2) = 0.f;
L560:
		;
	    }

	    rm1_ref(i__, i__) = rm1_ref(i__, i__) - y * ilambd;
	    if (i__ < n1) {
		goto L570;
	    }
	    l = i__ - ns;
	    z___ref(mp, l) = -ilambd;
	    z___ref(i__, l) = z___ref(i__, l) + x * ilambd;
	    goto L640;
L570:
	    rm1_ref(mp, i__ + 2) = -ilambd;
	    rm1_ref(i__, i__ + 2) = rm1_ref(i__, i__ + 2) + x * ilambd;
	    goto L640;
L580:
	    if (x != 0.f) {
		goto L600;
	    }
	    rm1_ref(mp, mp) = eps3;
	    if (i__ < *n) {
		rm1_ref(mp, i__ + 1) = 0.f;
	    }
	    if (i__ == *n) {
		z___ref(mp, s - 1) = 0.f;
	    }
	    t = 0.f;
	    x = eps3 * eps3;
L600:
	    w /= x;
	    x = rm1_ref(mp, mp) * w;
	    y = -t * w;


/*        INCREMENT OPCOUNT FOR LOOP 620 */
	    latime_1.ops += (uk - i__ + 1) * 6;
	    i__3 = uk;
	    for (j = i__; j <= i__3; ++j) {
		if (j < n1) {
		    goto L610;
		}
		l = j - ns;
		t = z___ref(mp, l);
		z___ref(i__, l) = -x * t - y * rm1_ref(j, mp);
		goto L615;
L610:
		t = rm1_ref(mp, j + 2);
		rm1_ref(i__, j + 2) = -x * t - y * rm1_ref(j, mp);
L615:
		rm1_ref(j, i__) = rm1_ref(j, i__) - x * rm1_ref(j, mp) + y * 
			t;
/* L620: */
	    }

	    if (i__ < n1) {
		goto L630;
	    }
	    l = i__ - ns;
	    z___ref(i__, l) = z___ref(i__, l) - ilambd;
	    goto L640;
L630:
	    rm1_ref(i__, i__ + 2) = rm1_ref(i__, i__ + 2) - ilambd;
L640:
	    ;
	}

/*        INCREMENT OP COUNT (AVERAGE) FOR COMPUTING   
          THE SCALARS IN LOOP 640 */
	latime_1.ops += (uk - 1) * 10;

	if (uk < n1) {
	    goto L650;
	}
	l = uk - ns;
	t = z___ref(uk, l);
	goto L655;
L650:
	t = rm1_ref(uk, uk + 2);
L655:
	if (rm1_ref(uk, uk) == 0.f && t == 0.f) {
	    rm1_ref(uk, uk) = eps3;
	}
/*     .......... BACK SUBSTITUTION FOR COMPLEX VECTOR   
                  FOR I=UK STEP -1 UNTIL 1 DO -- .......... */
L660:
	i__2 = uk;
	for (ii = 1; ii <= i__2; ++ii) {
	    i__ = uk + 1 - ii;
	    x = rv1[i__];
	    y = 0.f;
	    if (i__ == uk) {
		goto L700;
	    }
	    ip1 = i__ + 1;

	    i__3 = uk;
	    for (j = ip1; j <= i__3; ++j) {
		if (j < n1) {
		    goto L670;
		}
		l = j - ns;
		t = z___ref(i__, l);
		goto L675;
L670:
		t = rm1_ref(i__, j + 2);
L675:
		x = x - rm1_ref(j, i__) * rv1[j] + t * rv2[j];
		y = y - rm1_ref(j, i__) * rv2[j] - t * rv1[j];
/* L680: */
	    }

L700:
	    if (i__ < n1) {
		goto L710;
	    }
	    l = i__ - ns;
	    t = z___ref(i__, l);
	    goto L715;
L710:
	    t = rm1_ref(i__, i__ + 2);
L715:
	    cdiv_(&x, &y, &rm1_ref(i__, i__), &t, &rv1[i__], &rv2[i__]);
/* L720: */
	}

/*        INCREMENT OP COUNT FOR LOOP 720. */
	latime_1.ops += (uk << 2) * (uk + 3);
/*     .......... ACCEPTANCE TEST FOR REAL OR COMPLEX   
                  EIGENVECTOR AND NORMALIZATION .......... */
L740:
	++its;
	norm = 0.f;
	normv = 0.f;

	i__2 = uk;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    if (ilambd == 0.f) {
		x = (r__1 = rv1[i__], dabs(r__1));
	    }
	    if (ilambd != 0.f) {
		x = pythag_(&rv1[i__], &rv2[i__]);
	    }
	    if (normv >= x) {
		goto L760;
	    }
	    normv = x;
	    j = i__;
L760:
	    norm += x;
/* L780: */
	}

/*        INCREMENT OP COUNT ACCEPTANCE TEST */
	if (ilambd == 0.f) {
	    latime_1.ops += uk;
	}
	if (ilambd != 0.f) {
	    latime_1.ops += uk << 4;
	}

	if (norm < growto) {
	    goto L840;
	}
/*     .......... ACCEPT VECTOR .......... */
	x = rv1[j];
	if (ilambd == 0.f) {
	    x = 1.f / x;
	}
	if (ilambd != 0.f) {
	    y = rv2[j];
	}


/*        INCREMENT OPCOUNT FOR LOOP 820 */
	if (ilambd == 0.f) {
	    latime_1.ops += uk;
	}
	if (ilambd != 0.f) {
	    latime_1.ops += uk << 4;
	}
	i__2 = uk;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    if (ilambd != 0.f) {
		goto L800;
	    }
	    z___ref(i__, s) = rv1[i__] * x;
	    goto L820;
L800:
	    cdiv_(&rv1[i__], &rv2[i__], &x, &y, &z___ref(i__, s - 1), &
		    z___ref(i__, s));
L820:
	    ;
	}

	if (uk == *n) {
	    goto L940;
	}
	j = uk + 1;
	goto L900;
/*     .......... IN-LINE PROCEDURE FOR CHOOSING   
                  A NEW STARTING VECTOR .......... */
L840:
	if (its >= uk) {
	    goto L880;
	}
	x = ukroot;
	y = eps3 / (x + 1.f);
	rv1[1] = eps3;

	i__2 = uk;
	for (i__ = 2; i__ <= i__2; ++i__) {
/* L860: */
	    rv1[i__] = y;
	}

	j = uk - its + 1;
	rv1[j] -= eps3 * x;
	if (ilambd == 0.f) {
	    goto L440;
	}
	goto L660;
/*     .......... SET ERROR -- UNACCEPTED EIGENVECTOR .......... */
L880:
	j = 1;
	*ierr = -k;
/*     .......... SET REMAINING VECTOR COMPONENTS TO ZERO .......... */
L900:
	i__2 = *n;
	for (i__ = j; i__ <= i__2; ++i__) {
	    z___ref(i__, s) = 0.f;
	    if (ilambd != 0.f) {
		z___ref(i__, s - 1) = 0.f;
	    }
/* L920: */
	}

L940:
	++s;
L960:
	if (ip == -1) {
	    ip = 0;
	}
	if (ip == 1) {
	    ip = -1;
	}
/* L980: */
    }

    goto L1001;
/*     .......... SET ERROR -- UNDERESTIMATE OF EIGENVECTOR   
                  SPACE REQUIRED .......... */
L1000:
    if (*ierr != 0) {
	*ierr -= *n;
    }
    if (*ierr == 0) {
	*ierr = -((*n << 1) + 1);
    }
L1001:
    *m = s - 1 - abs(ip);

/*     COMPUTE FINAL OP COUNT */
    latime_1.ops += opst;
    return 0;
} /* invit_ */

#undef rm1_ref
#undef z___ref
#undef a_ref


/* Subroutine */ int orthes_(integer *nm, integer *n, integer *low, integer *
	igh, real *a, real *ort)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    real r__1;

    /* Builtin functions */
    double sqrt(doublereal), r_sign(real *, real *);

    /* Local variables */
    static real f, g, h__;
    static integer i__, j, m;
    static real scale;
    static integer la, ii, jj, mp, kp1;


#define a_ref(a_1,a_2) a[(a_2)*a_dim1 + a_1]



/*     COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED   
       OPST IS USED TO ACCUMULATE SMALL CONTRIBUTIONS TO OPS   
       TO AVOID ROUNDOFF ERROR   

       THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE ORTHES,   
       NUM. MATH. 12, 349-368(1968) BY MARTIN AND WILKINSON.   
       HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 339-358(1971).   

       GIVEN A REAL GENERAL MATRIX, THIS SUBROUTINE   
       REDUCES A SUBMATRIX SITUATED IN ROWS AND COLUMNS   
       LOW THROUGH IGH TO UPPER HESSENBERG FORM BY   
       ORTHOGONAL SIMILARITY TRANSFORMATIONS.   

       ON INPUT   

          NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL   
            ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM   
            DIMENSION STATEMENT.   

          N IS THE ORDER OF THE MATRIX.   

          LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING   
            SUBROUTINE  BALANC.  IF  BALANC  HAS NOT BEEN USED,   
            SET LOW=1, IGH=N.   

          A CONTAINS THE INPUT MATRIX.   

       ON OUTPUT   

          A CONTAINS THE HESSENBERG MATRIX.  INFORMATION ABOUT   
            THE ORTHOGONAL TRANSFORMATIONS USED IN THE REDUCTION   
            IS STORED IN THE REMAINING TRIANGLE UNDER THE   
            HESSENBERG MATRIX.   

          ORT CONTAINS FURTHER INFORMATION ABOUT THE TRANSFORMATIONS.   
            ONLY ELEMENTS LOW THROUGH IGH ARE USED.   

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,   
       MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY   

       THIS VERSION DATED AUGUST 1983.   

       ------------------------------------------------------------------   

       Parameter adjustments */
    a_dim1 = *nm;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    --ort;

    /* Function Body */
    if (*n <= 0) {
	return 0;
    }
    la = *igh - 1;
    kp1 = *low + 1;
    if (la < kp1) {
	goto L200;
    }


/*     INCREMENT OP COUNR FOR COMPUTING G,H,ORT(M),.. IN LOOP 180 */
    latime_1.ops += (la - kp1 + 1) * 6;
    i__1 = la;
    for (m = kp1; m <= i__1; ++m) {
	h__ = 0.f;
	ort[m] = 0.f;
	scale = 0.f;
/*     .......... SCALE COLUMN (ALGOL TOL THEN NOT NEEDED) ..........   

       INCREMENT OP COUNT FOR LOOP 90 */
	latime_1.ops += *igh - m + 1;
	i__2 = *igh;
	for (i__ = m; i__ <= i__2; ++i__) {
/* L90: */
	    scale += (r__1 = a_ref(i__, m - 1), dabs(r__1));
	}

	if (scale == 0.f) {
	    goto L180;
	}
	mp = m + *igh;
/*     .......... FOR I=IGH STEP -1 UNTIL M DO -- ..........   

       INCREMENT OP COUNT FOR LOOP 100 */
	latime_1.ops += (*igh - m + 1) * 3;
	i__2 = *igh;
	for (ii = m; ii <= i__2; ++ii) {
	    i__ = mp - ii;
	    ort[i__] = a_ref(i__, m - 1) / scale;
	    h__ += ort[i__] * ort[i__];
/* L100: */
	}

	r__1 = sqrt(h__);
	g = -r_sign(&r__1, &ort[m]);
	h__ -= ort[m] * g;
	ort[m] -= g;
/*     .......... FORM (I-(U*UT)/H) * A ..........   

       INCREMENT OP COUNT FOR LOOP 130 AND 160 */
	latime_1.ops += (*n - m + 1 + *igh) * ((*igh - m + 1 << 2) + 1);
	i__2 = *n;
	for (j = m; j <= i__2; ++j) {
	    f = 0.f;
/*     .......... FOR I=IGH STEP -1 UNTIL M DO -- .......... */
	    i__3 = *igh;
	    for (ii = m; ii <= i__3; ++ii) {
		i__ = mp - ii;
		f += ort[i__] * a_ref(i__, j);
/* L110: */
	    }

	    f /= h__;

	    i__3 = *igh;
	    for (i__ = m; i__ <= i__3; ++i__) {
/* L120: */
		a_ref(i__, j) = a_ref(i__, j) - f * ort[i__];
	    }

/* L130: */
	}
/*     .......... FORM (I-(U*UT)/H)*A*(I-(U*UT)/H) .......... */
	i__2 = *igh;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    f = 0.f;
/*     .......... FOR J=IGH STEP -1 UNTIL M DO -- .......... */
	    i__3 = *igh;
	    for (jj = m; jj <= i__3; ++jj) {
		j = mp - jj;
		f += ort[j] * a_ref(i__, j);
/* L140: */
	    }

	    f /= h__;

	    i__3 = *igh;
	    for (j = m; j <= i__3; ++j) {
/* L150: */
		a_ref(i__, j) = a_ref(i__, j) - f * ort[j];
	    }

/* L160: */
	}

	ort[m] = scale * ort[m];
	a_ref(m, m - 1) = scale * g;
L180:
	;
    }

L200:
    return 0;
} /* orthes_ */

#undef a_ref


doublereal pythag_(real *a, real *b)
{
    /* System generated locals */
    real ret_val, r__1, r__2, r__3;

    /* Local variables */
    static real p, r__, s, t, u;


/*     FINDS SQRT(A**2+B**2) WITHOUT OVERFLOW OR DESTRUCTIVE UNDERFLOW   


       COMMON BLOCK TO RETURN OPERATION COUNT   
       OPST IS ONLY INCREMENTED HERE   
   Computing MAX */
    r__1 = dabs(*a), r__2 = dabs(*b);
    p = dmax(r__1,r__2);
    if (p == 0.f) {
	goto L20;
    }
/* Computing MIN */
    r__2 = dabs(*a), r__3 = dabs(*b);
/* Computing 2nd power */
    r__1 = dmin(r__2,r__3) / p;
    r__ = r__1 * r__1;

/*     INCREMENT OPST */
    pythop_1.opst += 2;
L10:
    t = r__ + 4.f;
    if (t == 4.f) {
	goto L20;
    }
    s = r__ / t;
    u = s * 2.f + 1.f;
    p = u * p;
/* Computing 2nd power */
    r__1 = s / u;
    r__ = r__1 * r__1 * r__;

/*        INCREMENT OPST */
    pythop_1.opst += 8;
    goto L10;
L20:
    ret_val = p;
    return ret_val;
} /* pythag_   

   Subroutine */ int tqlrat_(integer *n, real *d__, real *e2, integer *ierr)
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    real r__1, r__2;

    /* Builtin functions */
    double sqrt(doublereal), r_sign(real *, real *);

    /* Local variables */
    static real b, c__, f, g, h__;
    static integer i__, j, l, m;
    static real p, r__, s, t;
    static integer l1, ii;
    extern doublereal slamch_(char *), pythag_(real *, real *), 
	    epslon_(real *);
    static integer mml;
    static real eps, tst;


/*     EISPACK ROUTINE.   
       MODIFIED FOR COMPARISON WITH LAPACK ROUTINES.   

       CONVERGENCE TEST WAS MODIFIED TO BE THE SAME AS IN SSTEQR.   



       COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED   
       OPST IS USED TO ACCUMULATE CONTRIBUTIONS TO OPS FROM   
       FUNCTION PYTHAG.  IT IS PASSED TO AND FROM PYTHAG   
       THROUGH COMMON BLOCK PYTHOP.   

       THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TQLRAT,   
       ALGORITHM 464, COMM. ACM 16, 689(1973) BY REINSCH.   

       THIS SUBROUTINE FINDS THE EIGENVALUES OF A SYMMETRIC   
       TRIDIAGONAL MATRIX BY THE RATIONAL QL METHOD.   

       ON INPUT   

          N IS THE ORDER OF THE MATRIX.   

          D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX.   

          E2 CONTAINS THE SQUARES OF THE SUBDIAGONAL ELEMENTS OF THE   
            INPUT MATRIX IN ITS LAST N-1 POSITIONS.  E2(1) IS ARBITRARY.   

        ON OUTPUT   

          D CONTAINS THE EIGENVALUES IN ASCENDING ORDER.  IF AN   
            ERROR EXIT IS MADE, THE EIGENVALUES ARE CORRECT AND   
            ORDERED FOR INDICES 1,2,...IERR-1, BUT MAY NOT BE   
            THE SMALLEST EIGENVALUES.   

          E2 HAS BEEN DESTROYED.   

          IERR IS SET TO   
            ZERO       FOR NORMAL RETURN,   
            J          IF THE J-TH EIGENVALUE HAS NOT BEEN   
                       DETERMINED AFTER 30 ITERATIONS.   

       CALLS PYTHAG FOR  SQRT(A*A + B*B) .   

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,   
       MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY   

       THIS VERSION DATED AUGUST 1983.   

       ------------------------------------------------------------------   

       Parameter adjustments */
    --e2;
    --d__;

    /* Function Body */
    *ierr = 0;
    if (*n == 1) {
	goto L1001;
    }

/*        INITIALIZE ITERATION COUNT AND OPST */
    latime_1.itcnt = 0.f;
    pythop_1.opst = 0.f;

/*     DETERMINE THE UNIT ROUNDOFF FOR THIS ENVIRONMENT. */

    eps = slamch_("EPSILON");

    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
/* L100: */
	e2[i__ - 1] = e2[i__];
    }

    f = 0.f;
    t = 0.f;
    e2[*n] = 0.f;

    i__1 = *n;
    for (l = 1; l <= i__1; ++l) {
	j = 0;
	h__ = (r__1 = d__[l], dabs(r__1)) + sqrt(e2[l]);
	if (t > h__) {
	    goto L105;
	}
	t = h__;
	b = epslon_(&t);
	c__ = b * b;

/*     INCREMENT OPCOUNT FOR THIS SECTION.   
       (FUNCTION EPSLON IS COUNTED AS 6 FLOPS.  THIS IS THE MINIMUM   
       NUMBER REQUIRED, BUT COUNTING THEM EXACTLY WOULD AFFECT   
       THE TIMING.) */
	latime_1.ops += 9;
/*     .......... LOOK FOR SMALL SQUARED SUB-DIAGONAL ELEMENT .......... */
L105:
	i__2 = *n;
	for (m = l; m <= i__2; ++m) {
	    if (m == *n) {
		goto L120;
	    }
	    tst = sqrt((r__1 = e2[m], dabs(r__1)));
	    if (tst <= eps * ((r__1 = d__[m], dabs(r__1)) + (r__2 = d__[m + 1]
		    , dabs(r__2)))) {
		goto L120;
	    }
/*            IF (E2(M) .LE. C) GO TO 120   
       .......... E2(N) IS ALWAYS ZERO, SO THERE IS NO EXIT   
                  THROUGH THE BOTTOM OF THE LOOP ..........   
   L110: */
	}

L120:

/*        INCREMENT OPCOUNT FOR FINDING SMALL SUBDIAGONAL ELEMENT.   
   Computing MIN */
	i__2 = m, i__3 = *n - 1;
	latime_1.ops += (min(i__2,i__3) - l + 1) * 3;
	if (m == l) {
	    goto L210;
	}
L130:
	if (j == 30) {
	    goto L1000;
	}
	++j;
/*     .......... FORM SHIFT .......... */
	l1 = l + 1;
	s = sqrt(e2[l]);
	g = d__[l];
	p = (d__[l1] - g) / (s * 2.f);
	r__ = pythag_(&p, &c_b114);
	d__[l] = s / (p + r_sign(&r__, &p));
	h__ = g - d__[l];

	i__2 = *n;
	for (i__ = l1; i__ <= i__2; ++i__) {
/* L140: */
	    d__[i__] -= h__;
	}

	f += h__;

/*        INCREMENT OPCOUNT FOR FORMING SHIFT AND SUBTRACTING. */
	latime_1.ops = latime_1.ops + 8 + (i__ - l1 + 1);
/*     .......... RATIONAL QL TRANSFORMATION .......... */
	g = d__[m];
	if (g == 0.f) {
	    g = b;
	}
	h__ = g;
	s = 0.f;
	mml = m - l;
/*     .......... FOR I=M-1 STEP -1 UNTIL L DO -- .......... */
	i__2 = mml;
	for (ii = 1; ii <= i__2; ++ii) {
	    i__ = m - ii;
	    p = g * h__;
	    r__ = p + e2[i__];
	    e2[i__ + 1] = s * r__;
	    s = e2[i__] / r__;
	    d__[i__ + 1] = h__ + s * (h__ + d__[i__]);
	    g = d__[i__] - e2[i__] / g;
	    if (g == 0.f) {
		g = b;
	    }
	    h__ = g * p / r__;
/* L200: */
	}

	e2[l] = s * g;
	d__[l] = h__;

/*        INCREMENT OPCOUNT FOR INNER LOOP. */
	latime_1.ops = latime_1.ops + mml * 11 + 1;

/*        INCREMENT ITERATION COUNTER */
	latime_1.itcnt += 1;
/*     .......... GUARD AGAINST UNDERFLOW IN CONVERGENCE TEST .......... */
	if (h__ == 0.f) {
	    goto L210;
	}
	if ((r__1 = e2[l], dabs(r__1)) <= (r__2 = c__ / h__, dabs(r__2))) {
	    goto L210;
	}
	e2[l] = h__ * e2[l];
	if (e2[l] != 0.f) {
	    goto L130;
	}
L210:
	p = d__[l] + f;
/*     .......... ORDER EIGENVALUES .......... */
	if (l == 1) {
	    goto L250;
	}
/*     .......... FOR I=L STEP -1 UNTIL 2 DO -- .......... */
	i__2 = l;
	for (ii = 2; ii <= i__2; ++ii) {
	    i__ = l + 2 - ii;
	    if (p >= d__[i__ - 1]) {
		goto L270;
	    }
	    d__[i__] = d__[i__ - 1];
/* L230: */
	}

L250:
	i__ = 1;
L270:
	d__[i__] = p;
/* L290: */
    }

    goto L1001;
/*     .......... SET ERROR -- NO CONVERGENCE TO AN   
                  EIGENVALUE AFTER 30 ITERATIONS .......... */
L1000:
    *ierr = l;
L1001:

/*     COMPUTE FINAL OP COUNT */
    latime_1.ops += pythop_1.opst;
    return 0;
} /* tqlrat_   

   Subroutine */ int tred1_(integer *nm, integer *n, real *a, real *d__, real 
	*e, real *e2)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    real r__1, r__2, r__3, r__4;

    /* Builtin functions */
    double sqrt(doublereal), r_sign(real *, real *);

    /* Local variables */
    static real f, g, h__;
    static integer i__, j, k, l;
    static real scale;
    static integer ii, jp1;


#define a_ref(a_1,a_2) a[(a_2)*a_dim1 + a_1]



/*     COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT.   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED.   

       THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TRED1,   
       NUM. MATH. 11, 181-195(1968) BY MARTIN, REINSCH, AND WILKINSON.   
       HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).   

       THIS SUBROUTINE REDUCES A REAL SYMMETRIC MATRIX   
       TO A SYMMETRIC TRIDIAGONAL MATRIX USING   
       ORTHOGONAL SIMILARITY TRANSFORMATIONS.   

       ON INPUT   

          NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL   
            ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM   
            DIMENSION STATEMENT.   

          N IS THE ORDER OF THE MATRIX.   

          A CONTAINS THE REAL SYMMETRIC INPUT MATRIX.  ONLY THE   
            LOWER TRIANGLE OF THE MATRIX NEED BE SUPPLIED.   

       ON OUTPUT   

          A CONTAINS INFORMATION ABOUT THE ORTHOGONAL TRANS-   
            FORMATIONS USED IN THE REDUCTION IN ITS STRICT LOWER   
            TRIANGLE.  THE FULL UPPER TRIANGLE OF A IS UNALTERED.   

          D CONTAINS THE DIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX.   

          E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE TRIDIAGONAL   
            MATRIX IN ITS LAST N-1 POSITIONS.  E(1) IS SET TO ZERO.   

          E2 CONTAINS THE SQUARES OF THE CORRESPONDING ELEMENTS OF E.   
            E2 MAY COINCIDE WITH E IF THE SQUARES ARE NOT NEEDED.   

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,   
       MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY   

       THIS VERSION DATED AUGUST 1983.   

       ------------------------------------------------------------------   


       Parameter adjustments */
    --e2;
    --e;
    --d__;
    a_dim1 = *nm;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;

    /* Function Body   
   Computing MAX   
   Computing 3rd power */
    r__3 = (real) (*n);
/* Computing 2nd power */
    r__4 = (real) (*n);
    r__1 = 0.f, r__2 = r__3 * (r__3 * r__3) * 1.3333333333333333f + r__4 * 
	    r__4 * 12.f + *n * 3.6666666666666665f - 22;
    latime_1.ops += dmax(r__1,r__2);

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	d__[i__] = a_ref(*n, i__);
	a_ref(*n, i__) = a_ref(i__, i__);
/* L100: */
    }
/*     .......... FOR I=N STEP -1 UNTIL 1 DO -- .......... */
    i__1 = *n;
    for (ii = 1; ii <= i__1; ++ii) {
	i__ = *n + 1 - ii;
	l = i__ - 1;
	h__ = 0.f;
	scale = 0.f;
	if (l < 1) {
	    goto L130;
	}
/*     .......... SCALE ROW (ALGOL TOL THEN NOT NEEDED) .......... */
	i__2 = l;
	for (k = 1; k <= i__2; ++k) {
/* L120: */
	    scale += (r__1 = d__[k], dabs(r__1));
	}

	if (scale != 0.f) {
	    goto L140;
	}

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    d__[j] = a_ref(l, j);
	    a_ref(l, j) = a_ref(i__, j);
	    a_ref(i__, j) = 0.f;
/* L125: */
	}

L130:
	e[i__] = 0.f;
	e2[i__] = 0.f;
	goto L300;

L140:
	i__2 = l;
	for (k = 1; k <= i__2; ++k) {
	    d__[k] /= scale;
	    h__ += d__[k] * d__[k];
/* L150: */
	}

	e2[i__] = scale * scale * h__;
	f = d__[l];
	r__1 = sqrt(h__);
	g = -r_sign(&r__1, &f);
	e[i__] = scale * g;
	h__ -= f * g;
	d__[l] = f - g;
	if (l == 1) {
	    goto L285;
	}
/*     .......... FORM A*U .......... */
	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
/* L170: */
	    e[j] = 0.f;
	}

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    f = d__[j];
	    g = e[j] + a_ref(j, j) * f;
	    jp1 = j + 1;
	    if (l < jp1) {
		goto L220;
	    }

	    i__3 = l;
	    for (k = jp1; k <= i__3; ++k) {
		g += a_ref(k, j) * d__[k];
		e[k] += a_ref(k, j) * f;
/* L200: */
	    }

L220:
	    e[j] = g;
/* L240: */
	}
/*     .......... FORM P .......... */
	f = 0.f;

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    e[j] /= h__;
	    f += e[j] * d__[j];
/* L245: */
	}

	h__ = f / (h__ + h__);
/*     .......... FORM Q .......... */
	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
/* L250: */
	    e[j] -= h__ * d__[j];
	}
/*     .......... FORM REDUCED A .......... */
	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    f = d__[j];
	    g = e[j];

	    i__3 = l;
	    for (k = j; k <= i__3; ++k) {
/* L260: */
		a_ref(k, j) = a_ref(k, j) - f * e[k] - g * d__[k];
	    }

/* L280: */
	}

L285:
	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    f = d__[j];
	    d__[j] = a_ref(l, j);
	    a_ref(l, j) = a_ref(i__, j);
	    a_ref(i__, j) = f * scale;
/* L290: */
	}

L300:
	;
    }

    return 0;
} /* tred1_ */

#undef a_ref


/* Subroutine */ int bisect_(integer *n, real *eps1, real *d__, real *e, real 
	*e2, real *lb, real *ub, integer *mm, integer *m, real *w, integer *
	ind, integer *ierr, real *rv4, real *rv5)
{
    /* System generated locals */
    integer i__1, i__2;
    real r__1, r__2, r__3, r__4;

    /* Local variables */
    static integer i__, j, k, l, p, q, r__, s;
    static real u, v, atoli, rtoli;
    static integer m1, m2;
    static real tnorm, t1, t2, x0, x1;
    static integer ii;
    extern doublereal slamch_(char *);
    static real safemn, xu;
    extern doublereal epslon_(real *);
    static real pivmin;
    static integer isturm, tag;
    static real ulp, tmp1, tmp2;


/*     EISPACK ROUTINE.   
       MODIFIED FOR COMPARISON WITH LAPACK ROUTINES.   

       CONVERGENCE TEST WAS MODIFIED TO BE THE SAME AS IN SSTEBZ.   



       COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED   

       THIS SUBROUTINE IS A TRANSLATION OF THE BISECTION TECHNIQUE   
       IN THE ALGOL PROCEDURE TRISTURM BY PETERS AND WILKINSON.   
       HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 418-439(1971).   

       THIS SUBROUTINE FINDS THOSE EIGENVALUES OF A TRIDIAGONAL   
       SYMMETRIC MATRIX WHICH LIE IN A SPECIFIED INTERVAL,   
       USING BISECTION.   

       ON INPUT   

          N IS THE ORDER OF THE MATRIX.   

          EPS1 IS AN ABSOLUTE ERROR TOLERANCE FOR THE COMPUTED   
            EIGENVALUES.  IF THE INPUT EPS1 IS NON-POSITIVE,   
            IT IS RESET FOR EACH SUBMATRIX TO A DEFAULT VALUE,   
            NAMELY, MINUS THE PRODUCT OF THE RELATIVE MACHINE   
            PRECISION AND THE 1-NORM OF THE SUBMATRIX.   

          D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX.   

          E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE INPUT MATRIX   
            IN ITS LAST N-1 POSITIONS.  E(1) IS ARBITRARY.   

          E2 CONTAINS THE SQUARES OF THE CORRESPONDING ELEMENTS OF E.   
            E2(1) IS ARBITRARY.   

          LB AND UB DEFINE THE INTERVAL TO BE SEARCHED FOR EIGENVALUES.   
            IF LB IS NOT LESS THAN UB, NO EIGENVALUES WILL BE FOUND.   

          MM SHOULD BE SET TO AN UPPER BOUND FOR THE NUMBER OF   
            EIGENVALUES IN THE INTERVAL.  WARNING. IF MORE THAN   
            MM EIGENVALUES ARE DETERMINED TO LIE IN THE INTERVAL,   
            AN ERROR RETURN IS MADE WITH NO EIGENVALUES FOUND.   

       ON OUTPUT   

          EPS1 IS UNALTERED UNLESS IT HAS BEEN RESET TO ITS   
            (LAST) DEFAULT VALUE.   

          D AND E ARE UNALTERED.   

          ELEMENTS OF E2, CORRESPONDING TO ELEMENTS OF E REGARDED   
            AS NEGLIGIBLE, HAVE BEEN REPLACED BY ZERO CAUSING THE   
            MATRIX TO SPLIT INTO A DIRECT SUM OF SUBMATRICES.   
            E2(1) IS ALSO SET TO ZERO.   

          M IS THE NUMBER OF EIGENVALUES DETERMINED TO LIE IN (LB,UB).   

          W CONTAINS THE M EIGENVALUES IN ASCENDING ORDER.   

          IND CONTAINS IN ITS FIRST M POSITIONS THE SUBMATRIX INDICES   
            ASSOCIATED WITH THE CORRESPONDING EIGENVALUES IN W --   
            1 FOR EIGENVALUES BELONGING TO THE FIRST SUBMATRIX FROM   
            THE TOP, 2 FOR THOSE BELONGING TO THE SECOND SUBMATRIX, ETC..   

          IERR IS SET TO   
            ZERO       FOR NORMAL RETURN,   
            3*N+1      IF M EXCEEDS MM.   

          RV4 AND RV5 ARE TEMPORARY STORAGE ARRAYS.   

       THE ALGOL PROCEDURE STURMCNT CONTAINED IN TRISTURM   
       APPEARS IN BISECT IN-LINE.   

       NOTE THAT SUBROUTINE TQL1 OR IMTQL1 IS GENERALLY FASTER THAN   
       BISECT, IF MORE THAN N/4 EIGENVALUES ARE TO BE FOUND.   

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,   
       MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY   

       THIS VERSION DATED AUGUST 1983.   

       ------------------------------------------------------------------   

          INITIALIZE ITERATION COUNT.   
       Parameter adjustments */
    --rv5;
    --rv4;
    --e2;
    --e;
    --d__;
    --ind;
    --w;

    /* Function Body */
    latime_1.itcnt = 0.f;
    safemn = slamch_("S");
    ulp = slamch_("E") * slamch_("B");
    rtoli = ulp * 2.f;
    *ierr = 0;
    tag = 0;
    t1 = *lb;
    t2 = *ub;
/*     .......... LOOK FOR SMALL SUB-DIAGONAL ENTRIES .......... */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (i__ == 1) {
	    goto L20;
	}
/* CC         TST1 = ABS(D(I)) + ABS(D(I-1))   
   CC         TST2 = TST1 + ABS(E(I))   
   CC         IF (TST2 .GT. TST1) GO TO 40   
   Computing 2nd power */
	r__1 = e[i__];
	tmp1 = r__1 * r__1;
/* Computing 2nd power */
	r__2 = ulp;
	if ((r__1 = d__[i__] * d__[i__ - 1], dabs(r__1)) * (r__2 * r__2) + 
		safemn <= tmp1) {
	    goto L40;
	}
L20:
	e2[i__] = 0.f;
L40:
	;
    }
/*           INCREMENT OPCOUNT FOR DETERMINING IF MATRIX SPLITS. */
    latime_1.ops += (*n - 1) * 5;

/*                COMPUTE QUANTITIES NEEDED FOR CONVERGENCE TEST. */
    tmp1 = d__[1] - dabs(e[2]);
    tmp2 = d__[1] + dabs(e[2]);
    pivmin = 1.f;
    i__1 = *n - 1;
    for (i__ = 2; i__ <= i__1; ++i__) {
/* Computing MIN */
	r__3 = tmp1, r__4 = d__[i__] - (r__1 = e[i__], dabs(r__1)) - (r__2 = 
		e[i__ + 1], dabs(r__2));
	tmp1 = dmin(r__3,r__4);
/* Computing MAX */
	r__3 = tmp2, r__4 = d__[i__] + (r__1 = e[i__], dabs(r__1)) + (r__2 = 
		e[i__ + 1], dabs(r__2));
	tmp2 = dmax(r__3,r__4);
/* Computing MAX   
   Computing 2nd power */
	r__3 = e[i__];
	r__1 = pivmin, r__2 = r__3 * r__3;
	pivmin = dmax(r__1,r__2);
/* L41: */
    }
/* Computing MIN */
    r__2 = tmp1, r__3 = d__[*n] - (r__1 = e[*n], dabs(r__1));
    tmp1 = dmin(r__2,r__3);
/* Computing MAX */
    r__2 = tmp2, r__3 = d__[*n] + (r__1 = e[*n], dabs(r__1));
    tmp2 = dmax(r__2,r__3);
/* Computing MAX   
   Computing 2nd power */
    r__3 = e[*n];
    r__1 = pivmin, r__2 = r__3 * r__3;
    pivmin = dmax(r__1,r__2);
    pivmin *= safemn;
/* Computing MAX */
    r__1 = dabs(tmp1), r__2 = dabs(tmp2);
    tnorm = dmax(r__1,r__2);
    atoli = ulp * tnorm;
/*        INCREMENT OPCOUNT FOR COMPUTING THESE QUANTITIES. */
    latime_1.ops += *n - 1 << 2;

/*     .......... DETERMINE THE NUMBER OF EIGENVALUES   
                  IN THE INTERVAL .......... */
    p = 1;
    q = *n;
    x1 = *ub;
    isturm = 1;
    goto L320;
L60:
    *m = s;
    x1 = *lb;
    isturm = 2;
    goto L320;
L80:
    *m -= s;
    if (*m > *mm) {
	goto L980;
    }
    q = 0;
    r__ = 0;
/*     .......... ESTABLISH AND PROCESS NEXT SUBMATRIX, REFINING   
                  INTERVAL BY THE GERSCHGORIN BOUNDS .......... */
L100:
    if (r__ == *m) {
	goto L1001;
    }
    ++tag;
    p = q + 1;
    xu = d__[p];
    x0 = d__[p];
    u = 0.f;

    i__1 = *n;
    for (q = p; q <= i__1; ++q) {
	x1 = u;
	u = 0.f;
	v = 0.f;
	if (q == *n) {
	    goto L110;
	}
	u = (r__1 = e[q + 1], dabs(r__1));
	v = e2[q + 1];
L110:
/* Computing MIN */
	r__1 = d__[q] - (x1 + u);
	xu = dmin(r__1,xu);
/* Computing MAX */
	r__1 = d__[q] + (x1 + u);
	x0 = dmax(r__1,x0);
	if (v == 0.f) {
	    goto L140;
	}
/* L120: */
    }
/*        INCREMENT OPCOUNT FOR REFINING INTERVAL. */
    latime_1.ops += *n - p + 1 << 1;

L140:
/* Computing MAX */
    r__2 = dabs(xu), r__3 = dabs(x0);
    r__1 = dmax(r__2,r__3);
    x1 = epslon_(&r__1);
    if (*eps1 <= 0.f) {
	*eps1 = -x1;
    }
    if (p != q) {
	goto L180;
    }
/*     .......... CHECK FOR ISOLATED ROOT WITHIN INTERVAL .......... */
    if (t1 > d__[p] || d__[p] >= t2) {
	goto L940;
    }
    m1 = p;
    m2 = p;
    rv5[p] = d__[p];
    goto L900;
L180:
    x1 *= q - p + 1;
/* Computing MAX */
    r__1 = t1, r__2 = xu - x1;
    *lb = dmax(r__1,r__2);
/* Computing MIN */
    r__1 = t2, r__2 = x0 + x1;
    *ub = dmin(r__1,r__2);
    x1 = *lb;
    isturm = 3;
    goto L320;
L200:
    m1 = s + 1;
    x1 = *ub;
    isturm = 4;
    goto L320;
L220:
    m2 = s;
    if (m1 > m2) {
	goto L940;
    }
/*     .......... FIND ROOTS BY BISECTION .......... */
    x0 = *ub;
    isturm = 5;

    i__1 = m2;
    for (i__ = m1; i__ <= i__1; ++i__) {
	rv5[i__] = *ub;
	rv4[i__] = *lb;
/* L240: */
    }
/*     .......... LOOP FOR K-TH EIGENVALUE   
                  FOR K=M2 STEP -1 UNTIL M1 DO --   
                  (-DO- NOT USED TO LEGALIZE -COMPUTED GO TO-) .......... */
    k = m2;
L250:
    xu = *lb;
/*     .......... FOR I=K STEP -1 UNTIL M1 DO -- .......... */
    i__1 = k;
    for (ii = m1; ii <= i__1; ++ii) {
	i__ = m1 + k - ii;
	if (xu >= rv4[i__]) {
	    goto L260;
	}
	xu = rv4[i__];
	goto L280;
L260:
	;
    }

L280:
    if (x0 > rv5[k]) {
	x0 = rv5[k];
    }
/*     .......... NEXT BISECTION STEP .......... */
L300:
    x1 = (xu + x0) * .5f;
/* CC         IF ((X0 - XU) .LE. ABS(EPS1)) GO TO 420   
   CC         TST1 = 2.0E0 * (ABS(XU) + ABS(X0))   
   CC         TST2 = TST1 + (X0 - XU)   
   CC         IF (TST2 .EQ. TST1) GO TO 420 */
    tmp1 = (r__1 = x0 - xu, dabs(r__1));
/* Computing MAX */
    r__1 = dabs(x0), r__2 = dabs(xu);
    tmp2 = dmax(r__1,r__2);
/* Computing MAX */
    r__1 = max(atoli,pivmin), r__2 = rtoli * tmp2;
    if (tmp1 < dmax(r__1,r__2)) {
	goto L420;
    }
/*     .......... IN-LINE PROCEDURE FOR STURM SEQUENCE .......... */
L320:
    s = p - 1;
    u = 1.f;

    i__1 = q;
    for (i__ = p; i__ <= i__1; ++i__) {
	if (u != 0.f) {
	    goto L325;
	}
	v = (r__1 = e[i__], dabs(r__1)) / epslon_(&c_b114);
	if (e2[i__] == 0.f) {
	    v = 0.f;
	}
	goto L330;
L325:
	v = e2[i__] / u;
L330:
	u = d__[i__] - x1 - v;
	if (u < 0.f) {
	    ++s;
	}
/* L340: */
    }
/*           INCREMENT OPCOUNT FOR STURM SEQUENCE. */
    latime_1.ops += (q - p + 1) * 3;
/*           INCREMENT ITERATION COUNTER. */
    latime_1.itcnt += 1;

    switch (isturm) {
	case 1:  goto L60;
	case 2:  goto L80;
	case 3:  goto L200;
	case 4:  goto L220;
	case 5:  goto L360;
    }
/*     .......... REFINE INTERVALS .......... */
L360:
    if (s >= k) {
	goto L400;
    }
    xu = x1;
    if (s >= m1) {
	goto L380;
    }
    rv4[m1] = x1;
    goto L300;
L380:
    rv4[s + 1] = x1;
    if (rv5[s] > x1) {
	rv5[s] = x1;
    }
    goto L300;
L400:
    x0 = x1;
    goto L300;
/*     .......... K-TH EIGENVALUE FOUND .......... */
L420:
    rv5[k] = x1;
    --k;
    if (k >= m1) {
	goto L250;
    }
/*     .......... ORDER EIGENVALUES TAGGED WITH THEIR   
                  SUBMATRIX ASSOCIATIONS .......... */
L900:
    s = r__;
    r__ = r__ + m2 - m1 + 1;
    j = 1;
    k = m1;

    i__1 = r__;
    for (l = 1; l <= i__1; ++l) {
	if (j > s) {
	    goto L910;
	}
	if (k > m2) {
	    goto L940;
	}
	if (rv5[k] >= w[l]) {
	    goto L915;
	}

	i__2 = s;
	for (ii = j; ii <= i__2; ++ii) {
	    i__ = l + s - ii;
	    w[i__ + 1] = w[i__];
	    ind[i__ + 1] = ind[i__];
/* L905: */
	}

L910:
	w[l] = rv5[k];
	ind[l] = tag;
	++k;
	goto L920;
L915:
	++j;
L920:
	;
    }

L940:
    if (q < *n) {
	goto L100;
    }
    goto L1001;
/*     .......... SET ERROR -- UNDERESTIMATE OF NUMBER OF   
                  EIGENVALUES IN INTERVAL .......... */
L980:
    *ierr = *n * 3 + 1;
L1001:
    *lb = t1;
    *ub = t2;
    return 0;
} /* bisect_   

   Subroutine */ int tinvit_(integer *nm, integer *n, real *d__, real *e, 
	real *e2, integer *m, real *w, integer *ind, real *z__, integer *ierr,
	 real *rv1, real *rv2, real *rv3, real *rv4, real *rv6)
{
    /* System generated locals */
    integer z_dim1, z_offset, i__1, i__2, i__3;
    real r__1, r__2, r__3, r__4;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static real norm;
    static integer i__, j, p, q, r__, s;
    static real u, v, order;
    static integer group;
    static real x0, x1;
    static integer ii, jj, ip;
    static real uk, xu;
    extern doublereal pythag_(real *, real *), epslon_(real *);
    static integer tag, its;
    static real eps2, eps3, eps4;


#define z___ref(a_1,a_2) z__[(a_2)*z_dim1 + a_1]


/*     EISPACK ROUTINE.   

       CONVERGENCE TEST WAS NOT MODIFIED, SINCE IT SHOULD GIVE   
       APPROXIMATELY THE SAME LEVEL OF ACCURACY AS LAPACK ROUTINE,   
       ALTHOUGH THE EIGENVECTORS MAY NOT BE AS CLOSE TO ORTHOGONAL.   



       COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED   

       THIS SUBROUTINE IS A TRANSLATION OF THE INVERSE ITERATION TECH-   
       NIQUE IN THE ALGOL PROCEDURE TRISTURM BY PETERS AND WILKINSON.   
       HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 418-439(1971).   

       THIS SUBROUTINE FINDS THOSE EIGENVECTORS OF A TRIDIAGONAL   
       SYMMETRIC MATRIX CORRESPONDING TO SPECIFIED EIGENVALUES,   
       USING INVERSE ITERATION.   

       ON INPUT   

          NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL   
            ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM   
            DIMENSION STATEMENT.   

          N IS THE ORDER OF THE MATRIX.   

          D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX.   

          E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE INPUT MATRIX   
            IN ITS LAST N-1 POSITIONS.  E(1) IS ARBITRARY.   

          E2 CONTAINS THE SQUARES OF THE CORRESPONDING ELEMENTS OF E,   
            WITH ZEROS CORRESPONDING TO NEGLIGIBLE ELEMENTS OF E.   
            E(I) IS CONSIDERED NEGLIGIBLE IF IT IS NOT LARGER THAN   
            THE PRODUCT OF THE RELATIVE MACHINE PRECISION AND THE SUM   
            OF THE MAGNITUDES OF D(I) AND D(I-1).  E2(1) MUST CONTAIN   
            0.0E0 IF THE EIGENVALUES ARE IN ASCENDING ORDER, OR 2.0E0   
            IF THE EIGENVALUES ARE IN DESCENDING ORDER.  IF  BISECT,   
            TRIDIB, OR  IMTQLV  HAS BEEN USED TO FIND THE EIGENVALUES,   
            THEIR OUTPUT E2 ARRAY IS EXACTLY WHAT IS EXPECTED HERE.   

          M IS THE NUMBER OF SPECIFIED EIGENVALUES.   

          W CONTAINS THE M EIGENVALUES IN ASCENDING OR DESCENDING ORDER.   

          IND CONTAINS IN ITS FIRST M POSITIONS THE SUBMATRIX INDICES   
            ASSOCIATED WITH THE CORRESPONDING EIGENVALUES IN W --   
            1 FOR EIGENVALUES BELONGING TO THE FIRST SUBMATRIX FROM   
            THE TOP, 2 FOR THOSE BELONGING TO THE SECOND SUBMATRIX, ETC.   

       ON OUTPUT   

          ALL INPUT ARRAYS ARE UNALTERED.   

          Z CONTAINS THE ASSOCIATED SET OF ORTHONORMAL EIGENVECTORS.   
            ANY VECTOR WHICH FAILS TO CONVERGE IS SET TO ZERO.   

          IERR IS SET TO   
            ZERO       FOR NORMAL RETURN,   
            -R         IF THE EIGENVECTOR CORRESPONDING TO THE R-TH   
                       EIGENVALUE FAILS TO CONVERGE IN 5 ITERATIONS.   

          RV1, RV2, RV3, RV4, AND RV6 ARE TEMPORARY STORAGE ARRAYS.   

       CALLS PYTHAG FOR  SQRT(A*A + B*B) .   

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,   
       MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY   

       THIS VERSION DATED AUGUST 1983.   

       ------------------------------------------------------------------   

          INITIALIZE ITERATION COUNT.   
       Parameter adjustments */
    --rv6;
    --rv4;
    --rv3;
    --rv2;
    --rv1;
    --e2;
    --e;
    --d__;
    z_dim1 = *nm;
    z_offset = 1 + z_dim1 * 1;
    z__ -= z_offset;
    --ind;
    --w;

    /* Function Body */
    latime_1.itcnt = 0.f;
    *ierr = 0;
    if (*m == 0) {
	goto L1001;
    }
    tag = 0;
    order = 1.f - e2[1];
    q = 0;
/*     .......... ESTABLISH AND PROCESS NEXT SUBMATRIX .......... */
L100:
    p = q + 1;

    i__1 = *n;
    for (q = p; q <= i__1; ++q) {
	if (q == *n) {
	    goto L140;
	}
	if (e2[q + 1] == 0.f) {
	    goto L140;
	}
/* L120: */
    }
/*     .......... FIND VECTORS BY INVERSE ITERATION .......... */
L140:
    ++tag;
    s = 0;

    i__1 = *m;
    for (r__ = 1; r__ <= i__1; ++r__) {
	if (ind[r__] != tag) {
	    goto L920;
	}
	its = 1;
	x1 = w[r__];
	if (s != 0) {
	    goto L510;
	}
/*     .......... CHECK FOR ISOLATED ROOT .......... */
	xu = 1.f;
	if (p != q) {
	    goto L490;
	}
	rv6[p] = 1.f;
	goto L870;
L490:
	norm = (r__1 = d__[p], dabs(r__1));
	ip = p + 1;

	i__2 = q;
	for (i__ = ip; i__ <= i__2; ++i__) {
/* L500:   
   Computing MAX */
	    r__3 = norm, r__4 = (r__1 = d__[i__], dabs(r__1)) + (r__2 = e[i__]
		    , dabs(r__2));
	    norm = dmax(r__3,r__4);
	}
/*     .......... EPS2 IS THE CRITERION FOR GROUPING,   
                  EPS3 REPLACES ZERO PIVOTS AND EQUAL   
                  ROOTS ARE MODIFIED BY EPS3,   
                  EPS4 IS TAKEN VERY SMALL TO AVOID OVERFLOW .......... */
	eps2 = norm * .001f;
	eps3 = epslon_(&norm);
	uk = (real) (q - p + 1);
	eps4 = uk * eps3;
	uk = eps4 / sqrt(uk);
/*           INCREMENT OPCOUNT FOR COMPUTING CRITERIA. */
	latime_1.ops += q - ip + 4;
	s = p;
L505:
	group = 0;
	goto L520;
/*     .......... LOOK FOR CLOSE OR COINCIDENT ROOTS .......... */
L510:
	if ((r__1 = x1 - x0, dabs(r__1)) >= eps2) {
	    goto L505;
	}
	++group;
	if (order * (x1 - x0) <= 0.f) {
	    x1 = x0 + order * eps3;
	}
/*     .......... ELIMINATION WITH INTERCHANGES AND   
                  INITIALIZATION OF VECTOR .......... */
L520:
	v = 0.f;

	i__2 = q;
	for (i__ = p; i__ <= i__2; ++i__) {
	    rv6[i__] = uk;
	    if (i__ == p) {
		goto L560;
	    }
	    if ((r__1 = e[i__], dabs(r__1)) < dabs(u)) {
		goto L540;
	    }
/*     .......... WARNING -- A DIVIDE CHECK MAY OCCUR HERE IF   
                  E2 ARRAY HAS NOT BEEN SPECIFIED CORRECTLY .......... */
	    xu = u / e[i__];
	    rv4[i__] = xu;
	    rv1[i__ - 1] = e[i__];
	    rv2[i__ - 1] = d__[i__] - x1;
	    rv3[i__ - 1] = 0.f;
	    if (i__ != q) {
		rv3[i__ - 1] = e[i__ + 1];
	    }
	    u = v - xu * rv2[i__ - 1];
	    v = -xu * rv3[i__ - 1];
	    goto L580;
L540:
	    xu = e[i__] / u;
	    rv4[i__] = xu;
	    rv1[i__ - 1] = u;
	    rv2[i__ - 1] = v;
	    rv3[i__ - 1] = 0.f;
L560:
	    u = d__[i__] - x1 - xu * v;
	    if (i__ != q) {
		v = e[i__ + 1];
	    }
L580:
	    ;
	}
/*           INCREMENT OPCOUNT FOR ELIMINATION. */
	latime_1.ops += (q - p + 1) * 5;

	if (u == 0.f) {
	    u = eps3;
	}
	rv1[q] = u;
	rv2[q] = 0.f;
	rv3[q] = 0.f;
/*     .......... BACK SUBSTITUTION   
                  FOR I=Q STEP -1 UNTIL P DO -- .......... */
L600:
	i__2 = q;
	for (ii = p; ii <= i__2; ++ii) {
	    i__ = p + q - ii;
	    rv6[i__] = (rv6[i__] - u * rv2[i__] - v * rv3[i__]) / rv1[i__];
	    v = u;
	    u = rv6[i__];
/* L620: */
	}
/*           INCREMENT OPCOUNT FOR BACK SUBSTITUTION. */
	latime_1.ops += (q - p + 1) * 5;
/*     .......... ORTHOGONALIZE WITH RESPECT TO PREVIOUS   
                  MEMBERS OF GROUP .......... */
	if (group == 0) {
	    goto L700;
	}
	j = r__;

	i__2 = group;
	for (jj = 1; jj <= i__2; ++jj) {
L630:
	    --j;
	    if (ind[j] != tag) {
		goto L630;
	    }
	    xu = 0.f;

	    i__3 = q;
	    for (i__ = p; i__ <= i__3; ++i__) {
/* L640: */
		xu += rv6[i__] * z___ref(i__, j);
	    }

	    i__3 = q;
	    for (i__ = p; i__ <= i__3; ++i__) {
/* L660: */
		rv6[i__] -= xu * z___ref(i__, j);
	    }

/*              INCREMENT OPCOUNT FOR ORTHOGONALIZING. */
	    latime_1.ops += q - p + 1 << 2;
/* L680: */
	}

L700:
	norm = 0.f;

	i__2 = q;
	for (i__ = p; i__ <= i__2; ++i__) {
/* L720: */
	    norm += (r__1 = rv6[i__], dabs(r__1));
	}
/*           INCREMENT OPCOUNT FOR COMPUTING NORM. */
	latime_1.ops += q - p + 1;

	if (norm >= 1.f) {
	    goto L840;
	}
/*     .......... FORWARD SUBSTITUTION .......... */
	if (its == 5) {
	    goto L830;
	}
	if (norm != 0.f) {
	    goto L740;
	}
	rv6[s] = eps4;
	++s;
	if (s > q) {
	    s = p;
	}
	goto L780;
L740:
	xu = eps4 / norm;

	i__2 = q;
	for (i__ = p; i__ <= i__2; ++i__) {
/* L760: */
	    rv6[i__] *= xu;
	}
/*     .......... ELIMINATION OPERATIONS ON NEXT VECTOR   
                  ITERATE .......... */
L780:
	i__2 = q;
	for (i__ = ip; i__ <= i__2; ++i__) {
	    u = rv6[i__];
/*     .......... IF RV1(I-1) .EQ. E(I), A ROW INTERCHANGE   
                  WAS PERFORMED EARLIER IN THE   
                  TRIANGULARIZATION PROCESS .......... */
	    if (rv1[i__ - 1] != e[i__]) {
		goto L800;
	    }
	    u = rv6[i__ - 1];
	    rv6[i__ - 1] = rv6[i__];
L800:
	    rv6[i__] = u - rv4[i__] * rv6[i__ - 1];
/* L820: */
	}
/*           INCREMENT OPCOUNT FOR FORWARD SUBSTITUTION. */
	latime_1.ops = latime_1.ops + (q - p + 1) + (q - ip + 1 << 1);

	++its;
/*           INCREMENT ITERATION COUNTER. */
	latime_1.itcnt += 1;
	goto L600;
/*     .......... SET ERROR -- NON-CONVERGED EIGENVECTOR .......... */
L830:
	*ierr = -r__;
	xu = 0.f;
	goto L870;
/*     .......... NORMALIZE SO THAT SUM OF SQUARES IS   
                  1 AND EXPAND TO FULL ORDER .......... */
L840:
	u = 0.f;

	i__2 = q;
	for (i__ = p; i__ <= i__2; ++i__) {
/* L860: */
	    u = pythag_(&u, &rv6[i__]);
	}

	xu = 1.f / u;

L870:
	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
/* L880: */
	    z___ref(i__, r__) = 0.f;
	}

	i__2 = q;
	for (i__ = p; i__ <= i__2; ++i__) {
/* L900: */
	    z___ref(i__, r__) = rv6[i__] * xu;
	}
/*           INCREMENT OPCOUNT FOR NORMALIZING. */
	latime_1.ops += q - p + 1;

	x0 = x1;
L920:
	;
    }

    if (q < *n) {
	goto L100;
    }
/*        INCREMENT OPCOUNT FOR USE OF FUNCTION PYTHAG. */
    latime_1.ops += pythop_1.opst;
L1001:
    return 0;
} /* tinvit_ */

#undef z___ref


/* Subroutine */ int tridib_(integer *n, real *eps1, real *d__, real *e, real 
	*e2, real *lb, real *ub, integer *m11, integer *m, real *w, integer *
	ind, integer *ierr, real *rv4, real *rv5)
{
    /* System generated locals */
    integer i__1, i__2;
    real r__1, r__2, r__3;

    /* Local variables */
    static integer i__, j, k, l, p, q, r__, s;
    static real u, v, atoli, rtoli;
    static integer m1, m2;
    static real tnorm, t1, t2, x0, x1;
    static integer m22, ii;
    extern doublereal slamch_(char *);
    static real safemn, xu;
    extern doublereal epslon_(real *);
    static real pivmin;
    static integer isturm, tag;
    static real ulp, tmp1, tmp2;


/*     EISPACK ROUTINE.   
       MODIFIED FOR COMPARISON WITH LAPACK ROUTINES.   

       CONVERGENCE TEST WAS MODIFIED TO BE THE SAME AS IN SSTEBZ.   



       COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED   

       THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE BISECT,   
       NUM. MATH. 9, 386-393(1967) BY BARTH, MARTIN, AND WILKINSON.   
       HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 249-256(1971).   

       THIS SUBROUTINE FINDS THOSE EIGENVALUES OF A TRIDIAGONAL   
       SYMMETRIC MATRIX BETWEEN SPECIFIED BOUNDARY INDICES,   
       USING BISECTION.   

       ON INPUT   

          N IS THE ORDER OF THE MATRIX.   

          EPS1 IS AN ABSOLUTE ERROR TOLERANCE FOR THE COMPUTED   
            EIGENVALUES.  IF THE INPUT EPS1 IS NON-POSITIVE,   
            IT IS RESET FOR EACH SUBMATRIX TO A DEFAULT VALUE,   
            NAMELY, MINUS THE PRODUCT OF THE RELATIVE MACHINE   
            PRECISION AND THE 1-NORM OF THE SUBMATRIX.   

          D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX.   

          E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE INPUT MATRIX   
            IN ITS LAST N-1 POSITIONS.  E(1) IS ARBITRARY.   

          E2 CONTAINS THE SQUARES OF THE CORRESPONDING ELEMENTS OF E.   
            E2(1) IS ARBITRARY.   

          M11 SPECIFIES THE LOWER BOUNDARY INDEX FOR THE DESIRED   
            EIGENVALUES.   

          M SPECIFIES THE NUMBER OF EIGENVALUES DESIRED.  THE UPPER   
            BOUNDARY INDEX M22 IS THEN OBTAINED AS M22=M11+M-1.   

       ON OUTPUT   

          EPS1 IS UNALTERED UNLESS IT HAS BEEN RESET TO ITS   
            (LAST) DEFAULT VALUE.   

          D AND E ARE UNALTERED.   

          ELEMENTS OF E2, CORRESPONDING TO ELEMENTS OF E REGARDED   
            AS NEGLIGIBLE, HAVE BEEN REPLACED BY ZERO CAUSING THE   
            MATRIX TO SPLIT INTO A DIRECT SUM OF SUBMATRICES.   
            E2(1) IS ALSO SET TO ZERO.   

          LB AND UB DEFINE AN INTERVAL CONTAINING EXACTLY THE DESIRED   
            EIGENVALUES.   

          W CONTAINS, IN ITS FIRST M POSITIONS, THE EIGENVALUES   
            BETWEEN INDICES M11 AND M22 IN ASCENDING ORDER.   

          IND CONTAINS IN ITS FIRST M POSITIONS THE SUBMATRIX INDICES   
            ASSOCIATED WITH THE CORRESPONDING EIGENVALUES IN W --   
            1 FOR EIGENVALUES BELONGING TO THE FIRST SUBMATRIX FROM   
            THE TOP, 2 FOR THOSE BELONGING TO THE SECOND SUBMATRIX, ETC..   

          IERR IS SET TO   
            ZERO       FOR NORMAL RETURN,   
            3*N+1      IF MULTIPLE EIGENVALUES AT INDEX M11 MAKE   
                       UNIQUE SELECTION IMPOSSIBLE,   
            3*N+2      IF MULTIPLE EIGENVALUES AT INDEX M22 MAKE   
                       UNIQUE SELECTION IMPOSSIBLE.   

          RV4 AND RV5 ARE TEMPORARY STORAGE ARRAYS.   

       NOTE THAT SUBROUTINE TQL1, IMTQL1, OR TQLRAT IS GENERALLY FASTER   
       THAN TRIDIB, IF MORE THAN N/4 EIGENVALUES ARE TO BE FOUND.   

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,   
       MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY   

       THIS VERSION DATED AUGUST 1983.   

       ------------------------------------------------------------------   

          INITIALIZE ITERATION COUNT.   
       Parameter adjustments */
    --rv5;
    --rv4;
    --e2;
    --e;
    --d__;
    --ind;
    --w;

    /* Function Body */
    latime_1.itcnt = 0.f;
    safemn = slamch_("S");
    ulp = slamch_("E") * slamch_("B");
    rtoli = ulp * 2.f;
    *ierr = 0;
    tag = 0;
    xu = d__[1];
    x0 = d__[1];
    u = 0.f;
/*     .......... LOOK FOR SMALL SUB-DIAGONAL ENTRIES AND DETERMINE AN   
                  INTERVAL CONTAINING ALL THE EIGENVALUES .......... */
    pivmin = 1.f;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	x1 = u;
	u = 0.f;
	if (i__ != *n) {
	    u = (r__1 = e[i__ + 1], dabs(r__1));
	}
/* Computing MIN */
	r__1 = d__[i__] - (x1 + u);
	xu = dmin(r__1,xu);
/* Computing MAX */
	r__1 = d__[i__] + (x1 + u);
	x0 = dmax(r__1,x0);
	if (i__ == 1) {
	    goto L20;
	}
/* CC         TST1 = ABS(D(I)) + ABS(D(I-1))   
   CC         TST2 = TST1 + ABS(E(I))   
   CC         IF (TST2 .GT. TST1) GO TO 40   
   Computing 2nd power */
	r__1 = e[i__];
	tmp1 = r__1 * r__1;
/* Computing 2nd power */
	r__2 = ulp;
	if ((r__1 = d__[i__] * d__[i__ - 1], dabs(r__1)) * (r__2 * r__2) + 
		safemn <= tmp1) {
	    pivmin = dmax(pivmin,tmp1);
	    goto L40;
	}
L20:
	e2[i__] = 0.f;
L40:
	;
    }
    pivmin *= safemn;
/* Computing MAX */
    r__1 = dabs(xu), r__2 = dabs(x0);
    tnorm = dmax(r__1,r__2);
    atoli = ulp * tnorm;
/*        INCREMENT OPCOUNT FOR DETERMINING IF MATRIX SPLITS. */
    latime_1.ops += (*n - 1) * 9;

    x1 = (real) (*n);
/* Computing MAX */
    r__2 = dabs(xu), r__3 = dabs(x0);
    r__1 = dmax(r__2,r__3);
    x1 *= epslon_(&r__1);
    xu -= x1;
    t1 = xu;
    x0 += x1;
    t2 = x0;
/*     .......... DETERMINE AN INTERVAL CONTAINING EXACTLY   
                  THE DESIRED EIGENVALUES .......... */
    p = 1;
    q = *n;
    m1 = *m11 - 1;
    if (m1 == 0) {
	goto L75;
    }
    isturm = 1;
L50:
    v = x1;
    x1 = xu + (x0 - xu) * .5f;
    if (x1 == v) {
	goto L980;
    }
    goto L320;
L60:
    if ((i__1 = s - m1) < 0) {
	goto L65;
    } else if (i__1 == 0) {
	goto L73;
    } else {
	goto L70;
    }
L65:
    xu = x1;
    goto L50;
L70:
    x0 = x1;
    goto L50;
L73:
    xu = x1;
    t1 = x1;
L75:
    m22 = m1 + *m;
    if (m22 == *n) {
	goto L90;
    }
    x0 = t2;
    isturm = 2;
    goto L50;
L80:
    if ((i__1 = s - m22) < 0) {
	goto L65;
    } else if (i__1 == 0) {
	goto L85;
    } else {
	goto L70;
    }
L85:
    t2 = x1;
L90:
    q = 0;
    r__ = 0;
/*     .......... ESTABLISH AND PROCESS NEXT SUBMATRIX, REFINING   
                  INTERVAL BY THE GERSCHGORIN BOUNDS .......... */
L100:
    if (r__ == *m) {
	goto L1001;
    }
    ++tag;
    p = q + 1;
    xu = d__[p];
    x0 = d__[p];
    u = 0.f;

    i__1 = *n;
    for (q = p; q <= i__1; ++q) {
	x1 = u;
	u = 0.f;
	v = 0.f;
	if (q == *n) {
	    goto L110;
	}
	u = (r__1 = e[q + 1], dabs(r__1));
	v = e2[q + 1];
L110:
/* Computing MIN */
	r__1 = d__[q] - (x1 + u);
	xu = dmin(r__1,xu);
/* Computing MAX */
	r__1 = d__[q] + (x1 + u);
	x0 = dmax(r__1,x0);
	if (v == 0.f) {
	    goto L140;
	}
/* L120: */
    }
/*        INCREMENT OPCOUNT FOR REFINING INTERVAL. */
    latime_1.ops += *n - p + 1 << 1;

L140:
/* Computing MAX */
    r__2 = dabs(xu), r__3 = dabs(x0);
    r__1 = dmax(r__2,r__3);
    x1 = epslon_(&r__1);
    if (*eps1 <= 0.f) {
	*eps1 = -x1;
    }
    if (p != q) {
	goto L180;
    }
/*     .......... CHECK FOR ISOLATED ROOT WITHIN INTERVAL .......... */
    if (t1 > d__[p] || d__[p] >= t2) {
	goto L940;
    }
    m1 = p;
    m2 = p;
    rv5[p] = d__[p];
    goto L900;
L180:
    x1 *= q - p + 1;
/* Computing MAX */
    r__1 = t1, r__2 = xu - x1;
    *lb = dmax(r__1,r__2);
/* Computing MIN */
    r__1 = t2, r__2 = x0 + x1;
    *ub = dmin(r__1,r__2);
    x1 = *lb;
    isturm = 3;
    goto L320;
L200:
    m1 = s + 1;
    x1 = *ub;
    isturm = 4;
    goto L320;
L220:
    m2 = s;
    if (m1 > m2) {
	goto L940;
    }
/*     .......... FIND ROOTS BY BISECTION .......... */
    x0 = *ub;
    isturm = 5;

    i__1 = m2;
    for (i__ = m1; i__ <= i__1; ++i__) {
	rv5[i__] = *ub;
	rv4[i__] = *lb;
/* L240: */
    }
/*     .......... LOOP FOR K-TH EIGENVALUE   
                  FOR K=M2 STEP -1 UNTIL M1 DO --   
                  (-DO- NOT USED TO LEGALIZE -COMPUTED GO TO-) .......... */
    k = m2;
L250:
    xu = *lb;
/*     .......... FOR I=K STEP -1 UNTIL M1 DO -- .......... */
    i__1 = k;
    for (ii = m1; ii <= i__1; ++ii) {
	i__ = m1 + k - ii;
	if (xu >= rv4[i__]) {
	    goto L260;
	}
	xu = rv4[i__];
	goto L280;
L260:
	;
    }

L280:
    if (x0 > rv5[k]) {
	x0 = rv5[k];
    }
/*     .......... NEXT BISECTION STEP .......... */
L300:
    x1 = (xu + x0) * .5f;
/* CC         IF ((X0 - XU) .LE. ABS(EPS1)) GO TO 420   
   CC         TST1 = 2.0E0 * (ABS(XU) + ABS(X0))   
   CC         TST2 = TST1 + (X0 - XU)   
   CC         IF (TST2 .EQ. TST1) GO TO 420 */
    tmp1 = (r__1 = x0 - xu, dabs(r__1));
/* Computing MAX */
    r__1 = dabs(x0), r__2 = dabs(xu);
    tmp2 = dmax(r__1,r__2);
/* Computing MAX */
    r__1 = max(atoli,pivmin), r__2 = rtoli * tmp2;
    if (tmp1 < dmax(r__1,r__2)) {
	goto L420;
    }
/*     .......... IN-LINE PROCEDURE FOR STURM SEQUENCE .......... */
L320:
    s = p - 1;
    u = 1.f;

    i__1 = q;
    for (i__ = p; i__ <= i__1; ++i__) {
	if (u != 0.f) {
	    goto L325;
	}
	v = (r__1 = e[i__], dabs(r__1)) / epslon_(&c_b114);
	if (e2[i__] == 0.f) {
	    v = 0.f;
	}
	goto L330;
L325:
	v = e2[i__] / u;
L330:
	u = d__[i__] - x1 - v;
	if (u < 0.f) {
	    ++s;
	}
/* L340: */
    }
/*           INCREMENT OPCOUNT FOR STURM SEQUENCE. */
    latime_1.ops += (q - p + 1) * 3;
/*           INCREMENT ITERATION COUNTER. */
    latime_1.itcnt += 1;

    switch (isturm) {
	case 1:  goto L60;
	case 2:  goto L80;
	case 3:  goto L200;
	case 4:  goto L220;
	case 5:  goto L360;
    }
/*     .......... REFINE INTERVALS .......... */
L360:
    if (s >= k) {
	goto L400;
    }
    xu = x1;
    if (s >= m1) {
	goto L380;
    }
    rv4[m1] = x1;
    goto L300;
L380:
    rv4[s + 1] = x1;
    if (rv5[s] > x1) {
	rv5[s] = x1;
    }
    goto L300;
L400:
    x0 = x1;
    goto L300;
/*     .......... K-TH EIGENVALUE FOUND .......... */
L420:
    rv5[k] = x1;
    --k;
    if (k >= m1) {
	goto L250;
    }
/*     .......... ORDER EIGENVALUES TAGGED WITH THEIR   
                  SUBMATRIX ASSOCIATIONS .......... */
L900:
    s = r__;
    r__ = r__ + m2 - m1 + 1;
    j = 1;
    k = m1;

    i__1 = r__;
    for (l = 1; l <= i__1; ++l) {
	if (j > s) {
	    goto L910;
	}
	if (k > m2) {
	    goto L940;
	}
	if (rv5[k] >= w[l]) {
	    goto L915;
	}

	i__2 = s;
	for (ii = j; ii <= i__2; ++ii) {
	    i__ = l + s - ii;
	    w[i__ + 1] = w[i__];
	    ind[i__ + 1] = ind[i__];
/* L905: */
	}

L910:
	w[l] = rv5[k];
	ind[l] = tag;
	++k;
	goto L920;
L915:
	++j;
L920:
	;
    }

L940:
    if (q < *n) {
	goto L100;
    }
    goto L1001;
/*     .......... SET ERROR -- INTERVAL CANNOT BE FOUND CONTAINING   
                  EXACTLY THE DESIRED EIGENVALUES .......... */
L980:
    *ierr = *n * 3 + isturm;
L1001:
    *lb = t1;
    *ub = t2;
    return 0;
} /* tridib_   

   Subroutine */ int ssvdc_(real *x, integer *ldx, integer *n, integer *p, 
	real *s, real *e, real *u, integer *ldu, real *v, integer *ldv, real *
	work, integer *job, integer *info)
{
    /* System generated locals */
    integer x_dim1, x_offset, u_dim1, u_offset, v_dim1, v_offset, i__1, i__2, 
	    i__3;
    real r__1, r__2, r__3, r__4, r__5, r__6, r__7;

    /* Builtin functions */
    double r_sign(real *, real *), sqrt(doublereal);

    /* Local variables */
    static integer kase, jobu, iter;
    extern doublereal sdot_(integer *, real *, integer *, real *, integer *);
    static real test;
    extern /* Subroutine */ int srot_(integer *, real *, integer *, real *, 
	    integer *, real *, real *);
    static integer nctp1;
    extern doublereal snrm2_(integer *, real *, integer *);
    static real b, c__;
    static integer nrtp1;
    static real f, g;
    static integer i__, j, k, l, m;
    static real t, scale;
    extern /* Subroutine */ int sscal_(integer *, real *, real *, integer *);
    static real shift;
    static integer maxit;
    extern /* Subroutine */ int sswap_(integer *, real *, integer *, real *, 
	    integer *);
    static real iopst;
    static logical wantu, wantv;
    extern /* Subroutine */ int srotg_(real *, real *, real *, real *), 
	    saxpy_(integer *, real *, real *, integer *, real *, integer *);
    static real t1, el;
    static integer kk;
    static real cs;
    static integer ll, mm, ls;
    static real sl;
    static integer lu;
    static real sm, sn;
    extern doublereal slamch_(char *);
    static integer lm1, mm1, lp1, mp1, nct, ncu;
    static real eps;
    static integer lls, nrt;
    static real emm1, smm1;


#define u_ref(a_1,a_2) u[(a_2)*u_dim1 + a_1]
#define v_ref(a_1,a_2) v[(a_2)*v_dim1 + a_1]
#define x_ref(a_1,a_2) x[(a_2)*x_dim1 + a_1]


/*     COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, IOPS IS ONLY INCREMENTED   
       IOPST IS USED TO ACCUMULATE SMALL CONTRIBUTIONS TO IOPS   
       TO AVOID ROUNDOFF ERROR   


       SSVDC IS A SUBROUTINE TO REDUCE A REAL NXP MATRIX X BY   
       ORTHOGONAL TRANSFORMATIONS U AND V TO DIAGONAL FORM.  THE   
       DIAGONAL ELEMENTS S(I) ARE THE SINGULAR VALUES OF X.  THE   
       COLUMNS OF U ARE THE CORRESPONDING LEFT SINGULAR VECTORS,   
       AND THE COLUMNS OF V THE RIGHT SINGULAR VECTORS.   

       ON ENTRY   

           X         REAL(LDX,P), WHERE LDX.GE.N.   
                     X CONTAINS THE MATRIX WHOSE SINGULAR VALUE   
                     DECOMPOSITION IS TO BE COMPUTED.  X IS   
                     DESTROYED BY SSVDC.   

           LDX       INTEGER.   
                     LDX IS THE LEADING DIMENSION OF THE ARRAY X.   

           N         INTEGER.   
                     N IS THE NUMBER OF ROWS OF THE MATRIX X.   

           P         INTEGER.   
                     P IS THE NUMBER OF COLUMNS OF THE MATRIX X.   

           LDU       INTEGER.   
                     LDU IS THE LEADING DIMENSION OF THE ARRAY U.   
                     (SEE BELOW).   

           LDV       INTEGER.   
                     LDV IS THE LEADING DIMENSION OF THE ARRAY V.   
                     (SEE BELOW).   

           WORK      REAL(N).   
                     WORK IS A SCRATCH ARRAY.   

           JOB       INTEGER.   
                     JOB CONTROLS THE COMPUTATION OF THE SINGULAR   
                     VECTORS.  IT HAS THE DECIMAL EXPANSION AB   
                     WITH THE FOLLOWING MEANING   

                          A.EQ.0    DO NOT COMPUTE THE LEFT SINGULAR   
                                    VECTORS.   
                          A.EQ.1    RETURN THE N LEFT SINGULAR VECTORS   
                                    IN U.   
                          A.GE.2    RETURN THE FIRST MIN(N,P) SINGULAR   
                                    VECTORS IN U.   
                          B.EQ.0    DO NOT COMPUTE THE RIGHT SINGULAR   
                                    VECTORS.   
                          B.EQ.1    RETURN THE RIGHT SINGULAR VECTORS   
                                    IN V.   

       ON RETURN   

           S         REAL(MM), WHERE MM=MIN(N+1,P).   
                     THE FIRST MIN(N,P) ENTRIES OF S CONTAIN THE   
                     SINGULAR VALUES OF X ARRANGED IN DESCENDING   
                     ORDER OF MAGNITUDE.   

           E         REAL(P).   
                     E ORDINARILY CONTAINS ZEROS.  HOWEVER SEE THE   
                     DISCUSSION OF INFO FOR EXCEPTIONS.   

           U         REAL(LDU,K), WHERE LDU.GE.N.  IF JOBA.EQ.1 THEN   
                                     K.EQ.N, IF JOBA.GE.2 THEN   
                                     K.EQ.MIN(N,P).   
                     U CONTAINS THE MATRIX OF LEFT SINGULAR VECTORS.   
                     U IS NOT REFERENCED IF JOBA.EQ.0.  IF N.LE.P   
                     OR IF JOBA.EQ.2, THEN U MAY BE IDENTIFIED WITH X   
                     IN THE SUBROUTINE CALL.   

           V         REAL(LDV,P), WHERE LDV.GE.P.   
                     V CONTAINS THE MATRIX OF RIGHT SINGULAR VECTORS.   
                     V IS NOT REFERENCED IF JOB.EQ.0.  IF P.LE.N,   
                     THEN V MAY BE IDENTIFIED WITH X IN THE   
                     SUBROUTINE CALL.   

           INFO      INTEGER.   
                     THE SINGULAR VALUES (AND THEIR CORRESPONDING   
                     SINGULAR VECTORS) S(INFO+1),S(INFO+2),...,S(M)   
                     ARE CORRECT (HERE M=MIN(N,P)).  THUS IF   
                     INFO.EQ.0, ALL THE SINGULAR VALUES AND THEIR   
                     VECTORS ARE CORRECT.  IN ANY EVENT, THE MATRIX   
                     B = TRANS(U)*X*V IS THE BIDIAGONAL MATRIX   
                     WITH THE ELEMENTS OF S ON ITS DIAGONAL AND THE   
                     ELEMENTS OF E ON ITS SUPER-DIAGONAL (TRANS(U)   
                     IS THE TRANSPOSE OF U).  THUS THE SINGULAR   
                     VALUES OF X AND B ARE THE SAME.   

       LINPACK. THIS VERSION DATED 03/19/79 .   
                CORRECTION TO SHIFT CALCULATION MADE 2/85.   
       G.W. STEWART, UNIVERSITY OF MARYLAND, ARGONNE NATIONAL LAB.   

       ***** USES THE FOLLOWING FUNCTIONS AND SUBPROGRAMS.   

       EXTERNAL SROT   
       BLAS SAXPY,SDOT,SSCAL,SSWAP,SNRM2,SROTG   
       FORTRAN ABS,AMAX1,MAX0,MIN0,MOD,SQRT   

       INTERNAL VARIABLES   

       REAL ZTEST,R   

       GET EPS FROM SLAMCH FOR NEW STOPPING CRITERION   
       Parameter adjustments */
    x_dim1 = *ldx;
    x_offset = 1 + x_dim1 * 1;
    x -= x_offset;
    --s;
    --e;
    u_dim1 = *ldu;
    u_offset = 1 + u_dim1 * 1;
    u -= u_offset;
    v_dim1 = *ldv;
    v_offset = 1 + v_dim1 * 1;
    v -= v_offset;
    --work;

    /* Function Body */
    if (*n <= 0 || *p <= 0) {
	return 0;
    }
    eps = slamch_("EPSILON");



/*     SET THE MAXIMUM NUMBER OF ITERATIONS. */

    maxit = 50;

/*     DETERMINE WHAT IS TO BE COMPUTED. */

    wantu = FALSE_;
    wantv = FALSE_;
    jobu = *job % 100 / 10;
    ncu = *n;
    if (jobu > 1) {
	ncu = min(*n,*p);
    }
    if (jobu != 0) {
	wantu = TRUE_;
    }
    if (*job % 10 != 0) {
	wantv = TRUE_;
    }

/*     REDUCE X TO BIDIAGONAL FORM, STORING THE DIAGONAL ELEMENTS   
       IN S AND THE SUPER-DIAGONAL ELEMENTS IN E.   


       INITIALIZE OP COUNT */
    iopst = 0.f;
    *info = 0;
/* Computing MIN */
    i__1 = *n - 1;
    nct = min(i__1,*p);
/* Computing MAX   
   Computing MIN */
    i__3 = *p - 2;
    i__1 = 0, i__2 = min(i__3,*n);
    nrt = max(i__1,i__2);
    lu = max(nct,nrt);
    if (lu < 1) {
	goto L170;
    }
    i__1 = lu;
    for (l = 1; l <= i__1; ++l) {
	lp1 = l + 1;
	if (l > nct) {
	    goto L20;
	}

/*           COMPUTE THE TRANSFORMATION FOR THE L-TH COLUMN AND   
             PLACE THE L-TH DIAGONAL IN S(L).   


             INCREMENT OP COUNT */
	latime_2.iops += (*n - l + 1 << 1) + 1;
	i__2 = *n - l + 1;
	s[l] = snrm2_(&i__2, &x_ref(l, l), &c__1);
	if (s[l] == 0.f) {
	    goto L10;
	}
	if (x_ref(l, l) != 0.f) {
	    s[l] = r_sign(&s[l], &x_ref(l, l));
	}

/*              INCREMENT OP COUNT */
	latime_2.iops += *n - l + 3;
	i__2 = *n - l + 1;
	r__1 = 1.f / s[l];
	sscal_(&i__2, &r__1, &x_ref(l, l), &c__1);
	x_ref(l, l) = x_ref(l, l) + 1.f;
L10:
	s[l] = -s[l];
L20:
	if (*p < lp1) {
	    goto L50;
	}
	i__2 = *p;
	for (j = lp1; j <= i__2; ++j) {
	    if (l > nct) {
		goto L30;
	    }
	    if (s[l] == 0.f) {
		goto L30;
	    }

/*              APPLY THE TRANSFORMATION.   


                INCREMENT OP COUNT */
	    latime_2.iops += (*n - l << 2) + 5;
	    i__3 = *n - l + 1;
	    t = -sdot_(&i__3, &x_ref(l, l), &c__1, &x_ref(l, j), &c__1) / 
		    x_ref(l, l);
	    i__3 = *n - l + 1;
	    saxpy_(&i__3, &t, &x_ref(l, l), &c__1, &x_ref(l, j), &c__1);
L30:

/*           PLACE THE L-TH ROW OF X INTO  E FOR THE   
             SUBSEQUENT CALCULATION OF THE ROW TRANSFORMATION. */

	    e[j] = x_ref(l, j);
/* L40: */
	}
L50:
	if (! wantu || l > nct) {
	    goto L70;
	}

/*           PLACE THE TRANSFORMATION IN U FOR SUBSEQUENT BACK   
             MULTIPLICATION. */

	i__2 = *n;
	for (i__ = l; i__ <= i__2; ++i__) {
	    u_ref(i__, l) = x_ref(i__, l);
/* L60: */
	}
L70:
	if (l > nrt) {
	    goto L150;
	}

/*           COMPUTE THE L-TH ROW TRANSFORMATION AND PLACE THE   
             L-TH SUPER-DIAGONAL IN E(L).   


             INCREMENT OP COUNT */
	latime_2.iops += (*p - l << 1) + 1;
	i__2 = *p - l;
	e[l] = snrm2_(&i__2, &e[lp1], &c__1);
	if (e[l] == 0.f) {
	    goto L80;
	}
	if (e[lp1] != 0.f) {
	    e[l] = r_sign(&e[l], &e[lp1]);
	}

/*              INCREMENT OP COUNT */
	latime_2.iops += *p - l + 2;
	i__2 = *p - l;
	r__1 = 1.f / e[l];
	sscal_(&i__2, &r__1, &e[lp1], &c__1);
	e[lp1] += 1.f;
L80:
	e[l] = -e[l];
	if (lp1 > *n || e[l] == 0.f) {
	    goto L120;
	}

/*              APPLY THE TRANSFORMATION. */

	i__2 = *n;
	for (i__ = lp1; i__ <= i__2; ++i__) {
	    work[i__] = 0.f;
/* L90: */
	}

/*              INCREMENT OP COUNT */
	latime_2.iops += (real) ((*n - l << 2) + 1) * (*p - l);
	i__2 = *p;
	for (j = lp1; j <= i__2; ++j) {
	    i__3 = *n - l;
	    saxpy_(&i__3, &e[j], &x_ref(lp1, j), &c__1, &work[lp1], &c__1);
/* L100: */
	}
	i__2 = *p;
	for (j = lp1; j <= i__2; ++j) {
	    i__3 = *n - l;
	    r__1 = -e[j] / e[lp1];
	    saxpy_(&i__3, &r__1, &work[lp1], &c__1, &x_ref(lp1, j), &c__1);
/* L110: */
	}
L120:
	if (! wantv) {
	    goto L140;
	}

/*              PLACE THE TRANSFORMATION IN V FOR SUBSEQUENT   
                BACK MULTIPLICATION. */

	i__2 = *p;
	for (i__ = lp1; i__ <= i__2; ++i__) {
	    v_ref(i__, l) = e[i__];
/* L130: */
	}
L140:
L150:
/* L160: */
	;
    }
L170:

/*     SET UP THE FINAL BIDIAGONAL MATRIX OR ORDER M.   

   Computing MIN */
    i__1 = *p, i__2 = *n + 1;
    m = min(i__1,i__2);
    nctp1 = nct + 1;
    nrtp1 = nrt + 1;
    if (nct < *p) {
	s[nctp1] = x_ref(nctp1, nctp1);
    }
    if (*n < m) {
	s[m] = 0.f;
    }
    if (nrtp1 < m) {
	e[nrtp1] = x_ref(nrtp1, m);
    }
    e[m] = 0.f;

/*     IF REQUIRED, GENERATE U. */

    if (! wantu) {
	goto L300;
    }
    if (ncu < nctp1) {
	goto L200;
    }
    i__1 = ncu;
    for (j = nctp1; j <= i__1; ++j) {
	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    u_ref(i__, j) = 0.f;
/* L180: */
	}
	u_ref(j, j) = 1.f;
/* L190: */
    }
L200:
    if (nct < 1) {
	goto L290;
    }
    i__1 = nct;
    for (ll = 1; ll <= i__1; ++ll) {
	l = nct - ll + 1;
	if (s[l] == 0.f) {
	    goto L250;
	}
	lp1 = l + 1;
	if (ncu < lp1) {
	    goto L220;
	}

/*              INCREMENT OP COUNT */
	latime_2.iops += (real) ((*n - l << 2) + 5) * (ncu - l) + (*n - l + 2)
		;
	i__2 = ncu;
	for (j = lp1; j <= i__2; ++j) {
	    i__3 = *n - l + 1;
	    t = -sdot_(&i__3, &u_ref(l, l), &c__1, &u_ref(l, j), &c__1) / 
		    u_ref(l, l);
	    i__3 = *n - l + 1;
	    saxpy_(&i__3, &t, &u_ref(l, l), &c__1, &u_ref(l, j), &c__1);
/* L210: */
	}
L220:
	i__2 = *n - l + 1;
	sscal_(&i__2, &c_b405, &u_ref(l, l), &c__1);
	u_ref(l, l) = u_ref(l, l) + 1.f;
	lm1 = l - 1;
	if (lm1 < 1) {
	    goto L240;
	}
	i__2 = lm1;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    u_ref(i__, l) = 0.f;
/* L230: */
	}
L240:
	goto L270;
L250:
	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    u_ref(i__, l) = 0.f;
/* L260: */
	}
	u_ref(l, l) = 1.f;
L270:
/* L280: */
	;
    }
L290:
L300:

/*     IF IT IS REQUIRED, GENERATE V. */

    if (! wantv) {
	goto L350;
    }
    i__1 = *p;
    for (ll = 1; ll <= i__1; ++ll) {
	l = *p - ll + 1;
	lp1 = l + 1;
	if (l > nrt) {
	    goto L320;
	}
	if (e[l] == 0.f) {
	    goto L320;
	}

/*              INCREMENT OP COUNT */
	latime_2.iops += (real) ((*p - l << 2) + 1) * (*p - l);
	i__2 = *p;
	for (j = lp1; j <= i__2; ++j) {
	    i__3 = *p - l;
	    t = -sdot_(&i__3, &v_ref(lp1, l), &c__1, &v_ref(lp1, j), &c__1) / 
		    v_ref(lp1, l);
	    i__3 = *p - l;
	    saxpy_(&i__3, &t, &v_ref(lp1, l), &c__1, &v_ref(lp1, j), &c__1);
/* L310: */
	}
L320:
	i__2 = *p;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    v_ref(i__, l) = 0.f;
/* L330: */
	}
	v_ref(l, l) = 1.f;
/* L340: */
    }
L350:

/*     MAIN ITERATION LOOP FOR THE SINGULAR VALUES. */

    mm = m;

/*     INITIALIZE ITERATION COUNTER */
    latime_2.itcnt = 0.f;
    iter = 0;
L360:

/*        QUIT IF ALL THE SINGULAR VALUES HAVE BEEN FOUND.   

       ...EXIT */
    if (m == 0) {
	goto L620;
    }

/*        IF TOO MANY ITERATIONS HAVE BEEN PERFORMED, SET   
          FLAG AND RETURN.   


          UPDATE ITERATION COUNTER */
    latime_2.itcnt = (real) iter;
    if (iter < maxit) {
	goto L370;
    }
    *info = m;
/*     ......EXIT */
    goto L620;
L370:

/*        THIS SECTION OF THE PROGRAM INSPECTS FOR   
          NEGLIGIBLE ELEMENTS IN THE S AND E ARRAYS.  ON   
          COMPLETION THE VARIABLES KASE AND L ARE SET AS FOLLOWS.   

             KASE = 1     IF S(M) AND E(L-1) ARE NEGLIGIBLE AND L.LT.M   
             KASE = 2     IF S(L) IS NEGLIGIBLE AND L.LT.M   
             KASE = 3     IF E(L-1) IS NEGLIGIBLE, L.LT.M, AND   
                          S(L), ..., S(M) ARE NOT NEGLIGIBLE (QR STEP).   
             KASE = 4     IF E(M-1) IS NEGLIGIBLE (CONVERGENCE). */

    i__1 = m;
    for (ll = 1; ll <= i__1; ++ll) {
	l = m - ll;
/*        ...EXIT */
	if (l == 0) {
	    goto L400;
	}

/*           INCREMENT OP COUNT */
	iopst += 2;
	test = (r__1 = s[l], dabs(r__1)) + (r__2 = s[l + 1], dabs(r__2));

/*           REPLACE STOPPING CRITERION WITH NEW ONE AS IN LAPACK   

             ZTEST = TEST + ABS(E(L))   
             IF (ZTEST .NE. TEST) GO TO 380 */
	if ((r__1 = e[l], dabs(r__1)) > eps * test) {
	    goto L380;
	}

	e[l] = 0.f;
/*        ......EXIT */
	goto L400;
L380:
/* L390: */
	;
    }
L400:
    if (l != m - 1) {
	goto L410;
    }
    kase = 4;
    goto L480;
L410:
    lp1 = l + 1;
    mp1 = m + 1;
    i__1 = mp1;
    for (lls = lp1; lls <= i__1; ++lls) {
	ls = m - lls + lp1;
/*           ...EXIT */
	if (ls == l) {
	    goto L440;
	}
	test = 0.f;

/*              INCREMENT OP COUNT */
	iopst += 3;
	if (ls != m) {
	    test += (r__1 = e[ls], dabs(r__1));
	}
	if (ls != l + 1) {
	    test += (r__1 = e[ls - 1], dabs(r__1));
	}

/*              REPLACE STOPPING CRITERION WITH NEW ONE AS IN LAPACK   

                ZTEST = TEST + ABS(S(LS))   
                IF (ZTEST .NE. TEST) GO TO 420 */
	if ((r__1 = s[ls], dabs(r__1)) > eps * test) {
	    goto L420;
	}

	s[ls] = 0.f;
/*           ......EXIT */
	goto L440;
L420:
/* L430: */
	;
    }
L440:
    if (ls != l) {
	goto L450;
    }
    kase = 3;
    goto L470;
L450:
    if (ls != m) {
	goto L460;
    }
    kase = 1;
    goto L470;
L460:
    kase = 2;
    l = ls;
L470:
L480:
    ++l;

/*        PERFORM THE TASK INDICATED BY KASE. */

    switch (kase) {
	case 1:  goto L490;
	case 2:  goto L520;
	case 3:  goto L540;
	case 4:  goto L570;
    }

/*        DEFLATE NEGLIGIBLE S(M). */

L490:
    mm1 = m - 1;
    f = e[m - 1];
    e[m - 1] = 0.f;

/*           INCREMENT OP COUNT */
    latime_2.iops += (mm1 - l + 1) * 13 - 2;
    if (wantv) {
	latime_2.iops += (real) (mm1 - l + 1) * 6 * *p;
    }
    i__1 = mm1;
    for (kk = l; kk <= i__1; ++kk) {
	k = mm1 - kk + l;
	t1 = s[k];
	srotg_(&t1, &f, &cs, &sn);
	s[k] = t1;
	if (k == l) {
	    goto L500;
	}
	f = -sn * e[k - 1];
	e[k - 1] = cs * e[k - 1];
L500:
	if (wantv) {
	    srot_(p, &v_ref(1, k), &c__1, &v_ref(1, m), &c__1, &cs, &sn);
	}
/* L510: */
    }
    goto L610;

/*        SPLIT AT NEGLIGIBLE S(L). */

L520:
    f = e[l - 1];
    e[l - 1] = 0.f;

/*           INCREMENT OP COUNT */
    latime_2.iops += (m - l + 1) * 13;
    if (wantu) {
	latime_2.iops += (real) (m - l + 1) * 6 * *n;
    }
    i__1 = m;
    for (k = l; k <= i__1; ++k) {
	t1 = s[k];
	srotg_(&t1, &f, &cs, &sn);
	s[k] = t1;
	f = -sn * e[k];
	e[k] = cs * e[k];
	if (wantu) {
	    srot_(n, &u_ref(1, k), &c__1, &u_ref(1, l - 1), &c__1, &cs, &sn);
	}
/* L530: */
    }
    goto L610;

/*        PERFORM ONE QR STEP. */

L540:

/*           CALCULATE THE SHIFT.   


             INCREMENT OP COUNT */
    iopst += 23;
/* Computing MAX */
    r__6 = (r__1 = s[m], dabs(r__1)), r__7 = (r__2 = s[m - 1], dabs(r__2)), 
	    r__6 = max(r__6,r__7), r__7 = (r__3 = e[m - 1], dabs(r__3)), r__6 
	    = max(r__6,r__7), r__7 = (r__4 = s[l], dabs(r__4)), r__6 = max(
	    r__6,r__7), r__7 = (r__5 = e[l], dabs(r__5));
    scale = dmax(r__6,r__7);
    sm = s[m] / scale;
    smm1 = s[m - 1] / scale;
    emm1 = e[m - 1] / scale;
    sl = s[l] / scale;
    el = e[l] / scale;
/* Computing 2nd power */
    r__1 = emm1;
    b = ((smm1 + sm) * (smm1 - sm) + r__1 * r__1) / 2.f;
/* Computing 2nd power */
    r__1 = sm * emm1;
    c__ = r__1 * r__1;
    shift = 0.f;
    if (b == 0.f && c__ == 0.f) {
	goto L550;
    }
/* Computing 2nd power */
    r__1 = b;
    shift = sqrt(r__1 * r__1 + c__);
    if (b < 0.f) {
	shift = -shift;
    }
    shift = c__ / (b + shift);
L550:
    f = (sl + sm) * (sl - sm) + shift;
    g = sl * el;

/*           CHASE ZEROS. */

    mm1 = m - 1;

/*           INCREMENT OP COUNT */
    latime_2.iops += (mm1 - l + 1) * 38;
    if (wantv) {
	latime_2.iops += (real) (mm1 - l + 1) * 6 * *p;
    }
    if (wantu) {
/* Computing MAX   
   Computing MIN */
	i__2 = mm1, i__3 = *n - 1;
	i__1 = min(i__2,i__3) - l + 1;
	latime_2.iops += (real) max(i__1,0) * 6 * *n;
    }
    i__1 = mm1;
    for (k = l; k <= i__1; ++k) {
	srotg_(&f, &g, &cs, &sn);
	if (k != l) {
	    e[k - 1] = f;
	}
	f = cs * s[k] + sn * e[k];
	e[k] = cs * e[k] - sn * s[k];
	g = sn * s[k + 1];
	s[k + 1] = cs * s[k + 1];
	if (wantv) {
	    srot_(p, &v_ref(1, k), &c__1, &v_ref(1, k + 1), &c__1, &cs, &sn);
	}
	srotg_(&f, &g, &cs, &sn);
	s[k] = f;
	f = cs * e[k] + sn * s[k + 1];
	s[k + 1] = -sn * e[k] + cs * s[k + 1];
	g = sn * e[k + 1];
	e[k + 1] = cs * e[k + 1];
	if (wantu && k < *n) {
	    srot_(n, &u_ref(1, k), &c__1, &u_ref(1, k + 1), &c__1, &cs, &sn);
	}
/* L560: */
    }
    e[m - 1] = f;
    ++iter;
    goto L610;

/*        CONVERGENCE. */

L570:

/*           MAKE THE SINGULAR VALUE  POSITIVE. */

    if (s[l] >= 0.f) {
	goto L580;
    }
    s[l] = -s[l];

/*              INCREMENT OP COUNT */
    if (wantv) {
	latime_2.iops += *p;
    }
    if (wantv) {
	sscal_(p, &c_b405, &v_ref(1, l), &c__1);
    }
L580:

/*           ORDER THE SINGULAR VALUE. */

L590:
    if (l == mm) {
	goto L600;
    }
/*           ...EXIT */
    if (s[l] >= s[l + 1]) {
	goto L600;
    }
    t = s[l];
    s[l] = s[l + 1];
    s[l + 1] = t;
    if (wantv && l < *p) {
	sswap_(p, &v_ref(1, l), &c__1, &v_ref(1, l + 1), &c__1);
    }
    if (wantu && l < *n) {
	sswap_(n, &u_ref(1, l), &c__1, &u_ref(1, l + 1), &c__1);
    }
    ++l;
    goto L590;
L600:
    iter = 0;
    --m;
L610:
    goto L360;
L620:

/*     COMPUTE FINAL OPCOUNT */
    latime_2.iops += iopst;
    return 0;
} /* ssvdc_ */

#undef x_ref
#undef v_ref
#undef u_ref


/* Subroutine */ int qzhes_(integer *nm, integer *n, real *a, real *b, 
	logical *matz, real *z__)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, z_dim1, z_offset, i__1, i__2, 
	    i__3;
    real r__1, r__2;

    /* Builtin functions */
    double sqrt(doublereal), r_sign(real *, real *);

    /* Local variables */
    static integer i__, j, k, l;
    static real r__, s, t;
    static integer l1;
    static real u1, u2, v1, v2;
    static integer lb, nk1, nm1, nm2;
    static real rho;


#define a_ref(a_1,a_2) a[(a_2)*a_dim1 + a_1]
#define b_ref(a_1,a_2) b[(a_2)*b_dim1 + a_1]
#define z___ref(a_1,a_2) z__[(a_2)*z_dim1 + a_1]



/*     ---------------------- BEGIN TIMING CODE -------------------------   
       COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED   
       OPST IS USED TO ACCUMULATE SMALL CONTRIBUTIONS TO OPS   
       TO AVOID ROUNDOFF ERROR   
       ----------------------- END TIMING CODE --------------------------   


       THIS SUBROUTINE IS THE FIRST STEP OF THE QZ ALGORITHM   
       FOR SOLVING GENERALIZED MATRIX EIGENVALUE PROBLEMS,   
       SIAM J. NUMER. ANAL. 10, 241-256(1973) BY MOLER AND STEWART.   

       THIS SUBROUTINE ACCEPTS A PAIR OF REAL GENERAL MATRICES AND   
       REDUCES ONE OF THEM TO UPPER HESSENBERG FORM AND THE OTHER   
       TO UPPER TRIANGULAR FORM USING ORTHOGONAL TRANSFORMATIONS.   
       IT IS USUALLY FOLLOWED BY  QZIT,  QZVAL  AND, POSSIBLY,  QZVEC.   

       ON INPUT   

          NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL   
            ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM   
            DIMENSION STATEMENT.   

          N IS THE ORDER OF THE MATRICES.   

          A CONTAINS A REAL GENERAL MATRIX.   

          B CONTAINS A REAL GENERAL MATRIX.   

          MATZ SHOULD BE SET TO .TRUE. IF THE RIGHT HAND TRANSFORMATIONS   
            ARE TO BE ACCUMULATED FOR LATER USE IN COMPUTING   
            EIGENVECTORS, AND TO .FALSE. OTHERWISE.   

       ON OUTPUT   

          A HAS BEEN REDUCED TO UPPER HESSENBERG FORM.  THE ELEMENTS   
            BELOW THE FIRST SUBDIAGONAL HAVE BEEN SET TO ZERO.   

          B HAS BEEN REDUCED TO UPPER TRIANGULAR FORM.  THE ELEMENTS   
            BELOW THE MAIN DIAGONAL HAVE BEEN SET TO ZERO.   

          Z CONTAINS THE PRODUCT OF THE RIGHT HAND TRANSFORMATIONS IF   
            MATZ HAS BEEN SET TO .TRUE.  OTHERWISE, Z IS NOT REFERENCED.   

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,   
       MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY   

       THIS VERSION DATED AUGUST 1983.   

       ------------------------------------------------------------------   

       .......... INITIALIZE Z ..........   
       Parameter adjustments */
    z_dim1 = *nm;
    z_offset = 1 + z_dim1 * 1;
    z__ -= z_offset;
    b_dim1 = *nm;
    b_offset = 1 + b_dim1 * 1;
    b -= b_offset;
    a_dim1 = *nm;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;

    /* Function Body */
    if (! (*matz)) {
	goto L10;
    }

    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {

	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    z___ref(i__, j) = 0.f;
/* L2: */
	}

	z___ref(j, j) = 1.f;
/* L3: */
    }
/*     .......... REDUCE B TO UPPER TRIANGULAR FORM .......... */
L10:
    if (*n <= 1) {
	goto L170;
    }
    nm1 = *n - 1;

    i__1 = nm1;
    for (l = 1; l <= i__1; ++l) {
	l1 = l + 1;
	s = 0.f;

	i__2 = *n;
	for (i__ = l1; i__ <= i__2; ++i__) {
	    s += (r__1 = b_ref(i__, l), dabs(r__1));
/* L20: */
	}

	if (s == 0.f) {
	    goto L100;
	}
	s += (r__1 = b_ref(l, l), dabs(r__1));
	r__ = 0.f;

	i__2 = *n;
	for (i__ = l; i__ <= i__2; ++i__) {
	    b_ref(i__, l) = b_ref(i__, l) / s;
/* Computing 2nd power */
	    r__1 = b_ref(i__, l);
	    r__ += r__1 * r__1;
/* L25: */
	}

	r__1 = sqrt(r__);
	r__ = r_sign(&r__1, &b_ref(l, l));
	b_ref(l, l) = b_ref(l, l) + r__;
	rho = r__ * b_ref(l, l);

	i__2 = *n;
	for (j = l1; j <= i__2; ++j) {
	    t = 0.f;

	    i__3 = *n;
	    for (i__ = l; i__ <= i__3; ++i__) {
		t += b_ref(i__, l) * b_ref(i__, j);
/* L30: */
	    }

	    t = -t / rho;

	    i__3 = *n;
	    for (i__ = l; i__ <= i__3; ++i__) {
		b_ref(i__, j) = b_ref(i__, j) + t * b_ref(i__, l);
/* L40: */
	    }

/* L50: */
	}

	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {
	    t = 0.f;

	    i__3 = *n;
	    for (i__ = l; i__ <= i__3; ++i__) {
		t += b_ref(i__, l) * a_ref(i__, j);
/* L60: */
	    }

	    t = -t / rho;

	    i__3 = *n;
	    for (i__ = l; i__ <= i__3; ++i__) {
		a_ref(i__, j) = a_ref(i__, j) + t * b_ref(i__, l);
/* L70: */
	    }

/* L80: */
	}

	b_ref(l, l) = -s * r__;

	i__2 = *n;
	for (i__ = l1; i__ <= i__2; ++i__) {
	    b_ref(i__, l) = 0.f;
/* L90: */
	}

L100:
	;
    }

/*     ---------------------- BEGIN TIMING CODE -------------------------   
   Computing 2nd power */
    i__1 = *n;
    latime_1.ops += (real) ((i__1 * i__1 << 3) + *n * 17 + 24) * (real) (*n - 
	    1) / 3.f;
/*     ----------------------- END TIMING CODE --------------------------   

       .......... REDUCE A TO UPPER HESSENBERG FORM, WHILE   
                  KEEPING B TRIANGULAR .......... */
    if (*n == 2) {
	goto L170;
    }
    nm2 = *n - 2;

    i__1 = nm2;
    for (k = 1; k <= i__1; ++k) {
	nk1 = nm1 - k;
/*     .......... FOR L=N-1 STEP -1 UNTIL K+1 DO -- .......... */
	i__2 = nk1;
	for (lb = 1; lb <= i__2; ++lb) {
	    l = *n - lb;
	    l1 = l + 1;
/*     .......... ZERO A(L+1,K) .......... */
	    s = (r__1 = a_ref(l, k), dabs(r__1)) + (r__2 = a_ref(l1, k), dabs(
		    r__2));
	    if (s == 0.f) {
		goto L150;
	    }
	    u1 = a_ref(l, k) / s;
	    u2 = a_ref(l1, k) / s;
	    r__1 = sqrt(u1 * u1 + u2 * u2);
	    r__ = r_sign(&r__1, &u1);
	    v1 = -(u1 + r__) / r__;
	    v2 = -u2 / r__;
	    u2 = v2 / v1;

	    i__3 = *n;
	    for (j = k; j <= i__3; ++j) {
		t = a_ref(l, j) + u2 * a_ref(l1, j);
		a_ref(l, j) = a_ref(l, j) + t * v1;
		a_ref(l1, j) = a_ref(l1, j) + t * v2;
/* L110: */
	    }

	    a_ref(l1, k) = 0.f;

	    i__3 = *n;
	    for (j = l; j <= i__3; ++j) {
		t = b_ref(l, j) + u2 * b_ref(l1, j);
		b_ref(l, j) = b_ref(l, j) + t * v1;
		b_ref(l1, j) = b_ref(l1, j) + t * v2;
/* L120: */
	    }
/*     .......... ZERO B(L+1,L) .......... */
	    s = (r__1 = b_ref(l1, l1), dabs(r__1)) + (r__2 = b_ref(l1, l), 
		    dabs(r__2));
	    if (s == 0.f) {
		goto L150;
	    }
	    u1 = b_ref(l1, l1) / s;
	    u2 = b_ref(l1, l) / s;
	    r__1 = sqrt(u1 * u1 + u2 * u2);
	    r__ = r_sign(&r__1, &u1);
	    v1 = -(u1 + r__) / r__;
	    v2 = -u2 / r__;
	    u2 = v2 / v1;

	    i__3 = l1;
	    for (i__ = 1; i__ <= i__3; ++i__) {
		t = b_ref(i__, l1) + u2 * b_ref(i__, l);
		b_ref(i__, l1) = b_ref(i__, l1) + t * v1;
		b_ref(i__, l) = b_ref(i__, l) + t * v2;
/* L130: */
	    }

	    b_ref(l1, l) = 0.f;

	    i__3 = *n;
	    for (i__ = 1; i__ <= i__3; ++i__) {
		t = a_ref(i__, l1) + u2 * a_ref(i__, l);
		a_ref(i__, l1) = a_ref(i__, l1) + t * v1;
		a_ref(i__, l) = a_ref(i__, l) + t * v2;
/* L140: */
	    }

	    if (! (*matz)) {
		goto L150;
	    }

	    i__3 = *n;
	    for (i__ = 1; i__ <= i__3; ++i__) {
		t = z___ref(i__, l1) + u2 * z___ref(i__, l);
		z___ref(i__, l1) = z___ref(i__, l1) + t * v1;
		z___ref(i__, l) = z___ref(i__, l) + t * v2;
/* L145: */
	    }

L150:
	    ;
	}

/* L160: */
    }


/*     ---------------------- BEGIN TIMING CODE ------------------------- */
    if (*matz) {
	latime_1.ops += (real) (*n * 11 + 20) * (real) (*n - 1) * (real) (*n 
		- 2);
    } else {
	latime_1.ops += (real) ((*n << 3) + 20) * (real) (*n - 1) * (real) (*
		n - 2);
    }
/*     ----------------------- END TIMING CODE -------------------------- */

L170:
    return 0;
} /* qzhes_ */

#undef z___ref
#undef b_ref
#undef a_ref


/* Subroutine */ int qzit_(integer *nm, integer *n, real *a, real *b, real *
	eps1, logical *matz, real *z__, integer *ierr)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, z_dim1, z_offset, i__1, i__2, 
	    i__3;
    real r__1, r__2, r__3;

    /* Builtin functions */
    double sqrt(doublereal), r_sign(real *, real *);

    /* Local variables */
    static real epsa, epsb, opst;
    static integer i__, j, k, l;
    static real r__, s, t, anorm, bnorm;
    static integer enorn;
    static real a1, a2, a3;
    static integer k1, k2, l1;
    static real u1, u2, u3, v1, v2, v3, a11, a12, a21, a22, a33, a34, a43, 
	    a44, b11, b12, b22, b33;
    static integer na, ld;
    static real b34, b44;
    static integer en;
    static real ep;
    static integer ll;
    static real sh;
    extern doublereal epslon_(real *);
    static logical notlas;
    static integer km1, lm1;
    static real ani, bni;
    static integer ish, itn, its, enm2, lor1;


#define a_ref(a_1,a_2) a[(a_2)*a_dim1 + a_1]
#define b_ref(a_1,a_2) b[(a_2)*b_dim1 + a_1]
#define z___ref(a_1,a_2) z__[(a_2)*z_dim1 + a_1]



/*     ---------------------- BEGIN TIMING CODE -------------------------   
       COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED   
       OPST IS USED TO ACCUMULATE SMALL CONTRIBUTIONS TO OPS   
       TO AVOID ROUNDOFF ERROR   
       ----------------------- END TIMING CODE --------------------------   


       THIS SUBROUTINE IS THE SECOND STEP OF THE QZ ALGORITHM   
       FOR SOLVING GENERALIZED MATRIX EIGENVALUE PROBLEMS,   
       SIAM J. NUMER. ANAL. 10, 241-256(1973) BY MOLER AND STEWART,   
       AS MODIFIED IN TECHNICAL NOTE NASA TN D-7305(1973) BY WARD.   

       THIS SUBROUTINE ACCEPTS A PAIR OF REAL MATRICES, ONE OF THEM   
       IN UPPER HESSENBERG FORM AND THE OTHER IN UPPER TRIANGULAR FORM.   
       IT REDUCES THE HESSENBERG MATRIX TO QUASI-TRIANGULAR FORM USING   
       ORTHOGONAL TRANSFORMATIONS WHILE MAINTAINING THE TRIANGULAR FORM   
       OF THE OTHER MATRIX.  IT IS USUALLY PRECEDED BY  QZHES  AND   
       FOLLOWED BY  QZVAL  AND, POSSIBLY,  QZVEC.   

       ON INPUT   

          NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL   
            ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM   
            DIMENSION STATEMENT.   

          N IS THE ORDER OF THE MATRICES.   

          A CONTAINS A REAL UPPER HESSENBERG MATRIX.   

          B CONTAINS A REAL UPPER TRIANGULAR MATRIX.   

          EPS1 IS A TOLERANCE USED TO DETERMINE NEGLIGIBLE ELEMENTS.   
            EPS1 = 0.0 (OR NEGATIVE) MAY BE INPUT, IN WHICH CASE AN   
            ELEMENT WILL BE NEGLECTED ONLY IF IT IS LESS THAN ROUNDOFF   
            ERROR TIMES THE NORM OF ITS MATRIX.  IF THE INPUT EPS1 IS   
            POSITIVE, THEN AN ELEMENT WILL BE CONSIDERED NEGLIGIBLE   
            IF IT IS LESS THAN EPS1 TIMES THE NORM OF ITS MATRIX.  A   
            POSITIVE VALUE OF EPS1 MAY RESULT IN FASTER EXECUTION,   
            BUT LESS ACCURATE RESULTS.   

          MATZ SHOULD BE SET TO .TRUE. IF THE RIGHT HAND TRANSFORMATIONS   
            ARE TO BE ACCUMULATED FOR LATER USE IN COMPUTING   
            EIGENVECTORS, AND TO .FALSE. OTHERWISE.   

          Z CONTAINS, IF MATZ HAS BEEN SET TO .TRUE., THE   
            TRANSFORMATION MATRIX PRODUCED IN THE REDUCTION   
            BY  QZHES, IF PERFORMED, OR ELSE THE IDENTITY MATRIX.   
            IF MATZ HAS BEEN SET TO .FALSE., Z IS NOT REFERENCED.   

       ON OUTPUT   

          A HAS BEEN REDUCED TO QUASI-TRIANGULAR FORM.  THE ELEMENTS   
            BELOW THE FIRST SUBDIAGONAL ARE STILL ZERO AND NO TWO   
            CONSECUTIVE SUBDIAGONAL ELEMENTS ARE NONZERO.   

          B IS STILL IN UPPER TRIANGULAR FORM, ALTHOUGH ITS ELEMENTS   
            HAVE BEEN ALTERED.  THE LOCATION B(N,1) IS USED TO STORE   
            EPS1 TIMES THE NORM OF B FOR LATER USE BY  QZVAL  AND  QZVEC.   

          Z CONTAINS THE PRODUCT OF THE RIGHT HAND TRANSFORMATIONS   
            (FOR BOTH STEPS) IF MATZ HAS BEEN SET TO .TRUE..   

          IERR IS SET TO   
            ZERO       FOR NORMAL RETURN,   
            J          IF THE LIMIT OF 30*N ITERATIONS IS EXHAUSTED   
                       WHILE THE J-TH EIGENVALUE IS BEING SOUGHT.   

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,   
       MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY   

       THIS VERSION DATED AUGUST 1983.   

       ------------------------------------------------------------------   

       Parameter adjustments */
    z_dim1 = *nm;
    z_offset = 1 + z_dim1 * 1;
    z__ -= z_offset;
    b_dim1 = *nm;
    b_offset = 1 + b_dim1 * 1;
    b -= b_offset;
    a_dim1 = *nm;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;

    /* Function Body */
    *ierr = 0;
/*     .......... COMPUTE EPSA,EPSB .......... */
    anorm = 0.f;
    bnorm = 0.f;

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	ani = 0.f;
	if (i__ != 1) {
	    ani = (r__1 = a_ref(i__, i__ - 1), dabs(r__1));
	}
	bni = 0.f;

	i__2 = *n;
	for (j = i__; j <= i__2; ++j) {
	    ani += (r__1 = a_ref(i__, j), dabs(r__1));
	    bni += (r__1 = b_ref(i__, j), dabs(r__1));
/* L20: */
	}

	if (ani > anorm) {
	    anorm = ani;
	}
	if (bni > bnorm) {
	    bnorm = bni;
	}
/* L30: */
    }

/*     ---------------------- BEGIN TIMING CODE ------------------------- */
    latime_1.ops += (real) (*n * (*n + 1));
    opst = 0.f;
    latime_1.itcnt = 0.f;
/*     ----------------------- END TIMING CODE -------------------------- */


    if (anorm == 0.f) {
	anorm = 1.f;
    }
    if (bnorm == 0.f) {
	bnorm = 1.f;
    }
    ep = *eps1;
    if (ep > 0.f) {
	goto L50;
    }
/*     .......... USE ROUNDOFF LEVEL IF EPS1 IS ZERO .......... */
    ep = epslon_(&c_b114);
L50:
    epsa = ep * anorm;
    epsb = ep * bnorm;
/*     .......... REDUCE A TO QUASI-TRIANGULAR FORM, WHILE   
                  KEEPING B TRIANGULAR .......... */
    lor1 = 1;
    enorn = *n;
    en = *n;
    itn = *n * 30;
/*     .......... BEGIN QZ STEP .......... */
L60:
    if (en <= 2) {
	goto L1001;
    }
    if (! (*matz)) {
	enorn = en;
    }
    its = 0;
    na = en - 1;
    enm2 = na - 1;
L70:
    ish = 2;

/*     ---------------------- BEGIN TIMING CODE ------------------------- */
    latime_1.ops += opst;
    opst = 0.f;
    latime_1.itcnt += 1;
/*     ----------------------- END TIMING CODE --------------------------   

       .......... CHECK FOR CONVERGENCE OR REDUCIBILITY.   
                  FOR L=EN STEP -1 UNTIL 1 DO -- .......... */
    i__1 = en;
    for (ll = 1; ll <= i__1; ++ll) {
	lm1 = en - ll;
	l = lm1 + 1;
	if (l == 1) {
	    goto L95;
	}
	if ((r__1 = a_ref(l, lm1), dabs(r__1)) <= epsa) {
	    goto L90;
	}
/* L80: */
    }

L90:
    a_ref(l, lm1) = 0.f;
    if (l < na) {
	goto L95;
    }
/*     .......... 1-BY-1 OR 2-BY-2 BLOCK ISOLATED .......... */
    en = lm1;
    goto L60;
/*     .......... CHECK FOR SMALL TOP OF B .......... */
L95:
    ld = l;
L100:
    l1 = l + 1;
    b11 = b_ref(l, l);
    if (dabs(b11) > epsb) {
	goto L120;
    }
    b_ref(l, l) = 0.f;
    s = (r__1 = a_ref(l, l), dabs(r__1)) + (r__2 = a_ref(l1, l), dabs(r__2));
    u1 = a_ref(l, l) / s;
    u2 = a_ref(l1, l) / s;
    r__1 = sqrt(u1 * u1 + u2 * u2);
    r__ = r_sign(&r__1, &u1);
    v1 = -(u1 + r__) / r__;
    v2 = -u2 / r__;
    u2 = v2 / v1;

    i__1 = enorn;
    for (j = l; j <= i__1; ++j) {
	t = a_ref(l, j) + u2 * a_ref(l1, j);
	a_ref(l, j) = a_ref(l, j) + t * v1;
	a_ref(l1, j) = a_ref(l1, j) + t * v2;
	t = b_ref(l, j) + u2 * b_ref(l1, j);
	b_ref(l, j) = b_ref(l, j) + t * v1;
	b_ref(l1, j) = b_ref(l1, j) + t * v2;
/* L110: */
    }

/*     ---------------------- BEGIN TIMING CODE ------------------------- */
    opst += (real) ((enorn + 1 - l) * 12 + 11);
/*     ----------------------- END TIMING CODE -------------------------- */
    if (l != 1) {
	a_ref(l, lm1) = -a_ref(l, lm1);
    }
    lm1 = l;
    l = l1;
    goto L90;
L120:
    a11 = a_ref(l, l) / b11;
    a21 = a_ref(l1, l) / b11;
    if (ish == 1) {
	goto L140;
    }
/*     .......... ITERATION STRATEGY .......... */
    if (itn == 0) {
	goto L1000;
    }
    if (its == 10) {
	goto L155;
    }
/*     .......... DETERMINE TYPE OF SHIFT .......... */
    b22 = b_ref(l1, l1);
    if (dabs(b22) < epsb) {
	b22 = epsb;
    }
    b33 = b_ref(na, na);
    if (dabs(b33) < epsb) {
	b33 = epsb;
    }
    b44 = b_ref(en, en);
    if (dabs(b44) < epsb) {
	b44 = epsb;
    }
    a33 = a_ref(na, na) / b33;
    a34 = a_ref(na, en) / b44;
    a43 = a_ref(en, na) / b33;
    a44 = a_ref(en, en) / b44;
    b34 = b_ref(na, en) / b44;
    t = (a43 * b34 - a33 - a44) * .5f;
    r__ = t * t + a34 * a43 - a33 * a44;
/*     ---------------------- BEGIN TIMING CODE ------------------------- */
    opst += 16.f;
/*     ----------------------- END TIMING CODE -------------------------- */
    if (r__ < 0.f) {
	goto L150;
    }
/*     .......... DETERMINE SINGLE SHIFT ZEROTH COLUMN OF A .......... */
    ish = 1;
    r__ = sqrt(r__);
    sh = -t + r__;
    s = -t - r__;
    if ((r__1 = s - a44, dabs(r__1)) < (r__2 = sh - a44, dabs(r__2))) {
	sh = s;
    }
/*     .......... LOOK FOR TWO CONSECUTIVE SMALL   
                  SUB-DIAGONAL ELEMENTS OF A.   
                  FOR L=EN-2 STEP -1 UNTIL LD DO -- .......... */
    i__1 = enm2;
    for (ll = ld; ll <= i__1; ++ll) {
	l = enm2 + ld - ll;
	if (l == ld) {
	    goto L140;
	}
	lm1 = l - 1;
	l1 = l + 1;
	t = a_ref(l, l);
	if ((r__1 = b_ref(l, l), dabs(r__1)) > epsb) {
	    t -= sh * b_ref(l, l);
	}
/*        --------------------- BEGIN TIMING CODE ----------------------- */
	if ((r__2 = a_ref(l, lm1), dabs(r__2)) <= (r__1 = t / a_ref(l1, l), 
		dabs(r__1)) * epsa) {
	    opst += (real) ((ll + 1 - ld << 2) + 5);
	    goto L100;
	}
/*        ---------------------- END TIMING CODE ------------------------   
   L130: */
    }
/*     ---------------------- BEGIN TIMING CODE ------------------------- */
    opst += (real) ((enm2 + 1 - ld << 2) + 5);
/*     ----------------------- END TIMING CODE -------------------------- */

L140:
    a1 = a11 - sh;
    a2 = a21;
    if (l != ld) {
	a_ref(l, lm1) = -a_ref(l, lm1);
    }
    goto L160;
/*     .......... DETERMINE DOUBLE SHIFT ZEROTH COLUMN OF A .......... */
L150:
    a12 = a_ref(l, l1) / b22;
    a22 = a_ref(l1, l1) / b22;
    b12 = b_ref(l, l1) / b22;
    a1 = ((a33 - a11) * (a44 - a11) - a34 * a43 + a43 * b34 * a11) / a21 + 
	    a12 - a11 * b12;
    a2 = a22 - a11 - a21 * b12 - (a33 - a11) - (a44 - a11) + a43 * b34;
    a3 = a_ref(l1 + 1, l1) / b22;
/*     ---------------------- BEGIN TIMING CODE ------------------------- */
    opst += 25.f;
/*     ----------------------- END TIMING CODE -------------------------- */
    goto L160;
/*     .......... AD HOC SHIFT .......... */
L155:
    a1 = 0.f;
    a2 = 1.f;
    a3 = 1.1605f;
L160:
    ++its;
    --itn;
    if (! (*matz)) {
	lor1 = ld;
    }
/*     .......... MAIN LOOP .......... */
    i__1 = na;
    for (k = l; k <= i__1; ++k) {
	notlas = k != na && ish == 2;
	k1 = k + 1;
	k2 = k + 2;
/* Computing MAX */
	i__2 = k - 1;
	km1 = max(i__2,l);
/* Computing MIN */
	i__2 = en, i__3 = k1 + ish;
	ll = min(i__2,i__3);
	if (notlas) {
	    goto L190;
	}
/*     .......... ZERO A(K+1,K-1) .......... */
	if (k == l) {
	    goto L170;
	}
	a1 = a_ref(k, km1);
	a2 = a_ref(k1, km1);
L170:
	s = dabs(a1) + dabs(a2);
	if (s == 0.f) {
	    goto L70;
	}
	u1 = a1 / s;
	u2 = a2 / s;
	r__1 = sqrt(u1 * u1 + u2 * u2);
	r__ = r_sign(&r__1, &u1);
	v1 = -(u1 + r__) / r__;
	v2 = -u2 / r__;
	u2 = v2 / v1;

	i__2 = enorn;
	for (j = km1; j <= i__2; ++j) {
	    t = a_ref(k, j) + u2 * a_ref(k1, j);
	    a_ref(k, j) = a_ref(k, j) + t * v1;
	    a_ref(k1, j) = a_ref(k1, j) + t * v2;
	    t = b_ref(k, j) + u2 * b_ref(k1, j);
	    b_ref(k, j) = b_ref(k, j) + t * v1;
	    b_ref(k1, j) = b_ref(k1, j) + t * v2;
/* L180: */
	}

/*        --------------------- BEGIN TIMING CODE ----------------------- */
	opst += (real) ((enorn + 1 - km1) * 12 + 11);
/*        ---------------------- END TIMING CODE ------------------------ */
	if (k != l) {
	    a_ref(k1, km1) = 0.f;
	}
	goto L240;
/*     .......... ZERO A(K+1,K-1) AND A(K+2,K-1) .......... */
L190:
	if (k == l) {
	    goto L200;
	}
	a1 = a_ref(k, km1);
	a2 = a_ref(k1, km1);
	a3 = a_ref(k2, km1);
L200:
	s = dabs(a1) + dabs(a2) + dabs(a3);
	if (s == 0.f) {
	    goto L260;
	}
	u1 = a1 / s;
	u2 = a2 / s;
	u3 = a3 / s;
	r__1 = sqrt(u1 * u1 + u2 * u2 + u3 * u3);
	r__ = r_sign(&r__1, &u1);
	v1 = -(u1 + r__) / r__;
	v2 = -u2 / r__;
	v3 = -u3 / r__;
	u2 = v2 / v1;
	u3 = v3 / v1;

	i__2 = enorn;
	for (j = km1; j <= i__2; ++j) {
	    t = a_ref(k, j) + u2 * a_ref(k1, j) + u3 * a_ref(k2, j);
	    a_ref(k, j) = a_ref(k, j) + t * v1;
	    a_ref(k1, j) = a_ref(k1, j) + t * v2;
	    a_ref(k2, j) = a_ref(k2, j) + t * v3;
	    t = b_ref(k, j) + u2 * b_ref(k1, j) + u3 * b_ref(k2, j);
	    b_ref(k, j) = b_ref(k, j) + t * v1;
	    b_ref(k1, j) = b_ref(k1, j) + t * v2;
	    b_ref(k2, j) = b_ref(k2, j) + t * v3;
/* L210: */
	}
/*        --------------------- BEGIN TIMING CODE ----------------------- */
	opst += (real) ((enorn + 1 - km1) * 20 + 17);
/*        ---------------------- END TIMING CODE ------------------------ */

	if (k == l) {
	    goto L220;
	}
	a_ref(k1, km1) = 0.f;
	a_ref(k2, km1) = 0.f;
/*     .......... ZERO B(K+2,K+1) AND B(K+2,K) .......... */
L220:
	s = (r__1 = b_ref(k2, k2), dabs(r__1)) + (r__2 = b_ref(k2, k1), dabs(
		r__2)) + (r__3 = b_ref(k2, k), dabs(r__3));
	if (s == 0.f) {
	    goto L240;
	}
	u1 = b_ref(k2, k2) / s;
	u2 = b_ref(k2, k1) / s;
	u3 = b_ref(k2, k) / s;
	r__1 = sqrt(u1 * u1 + u2 * u2 + u3 * u3);
	r__ = r_sign(&r__1, &u1);
	v1 = -(u1 + r__) / r__;
	v2 = -u2 / r__;
	v3 = -u3 / r__;
	u2 = v2 / v1;
	u3 = v3 / v1;

	i__2 = ll;
	for (i__ = lor1; i__ <= i__2; ++i__) {
	    t = a_ref(i__, k2) + u2 * a_ref(i__, k1) + u3 * a_ref(i__, k);
	    a_ref(i__, k2) = a_ref(i__, k2) + t * v1;
	    a_ref(i__, k1) = a_ref(i__, k1) + t * v2;
	    a_ref(i__, k) = a_ref(i__, k) + t * v3;
	    t = b_ref(i__, k2) + u2 * b_ref(i__, k1) + u3 * b_ref(i__, k);
	    b_ref(i__, k2) = b_ref(i__, k2) + t * v1;
	    b_ref(i__, k1) = b_ref(i__, k1) + t * v2;
	    b_ref(i__, k) = b_ref(i__, k) + t * v3;
/* L230: */
	}
/*        --------------------- BEGIN TIMING CODE ----------------------- */
	opst += (real) ((ll + 1 - lor1) * 20 + 17);
/*        ---------------------- END TIMING CODE ------------------------ */

	b_ref(k2, k) = 0.f;
	b_ref(k2, k1) = 0.f;
	if (! (*matz)) {
	    goto L240;
	}

	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    t = z___ref(i__, k2) + u2 * z___ref(i__, k1) + u3 * z___ref(i__, 
		    k);
	    z___ref(i__, k2) = z___ref(i__, k2) + t * v1;
	    z___ref(i__, k1) = z___ref(i__, k1) + t * v2;
	    z___ref(i__, k) = z___ref(i__, k) + t * v3;
/* L235: */
	}
/*        --------------------- BEGIN TIMING CODE ----------------------- */
	opst += (real) (*n * 10);
/*        ---------------------- END TIMING CODE ------------------------   
       .......... ZERO B(K+1,K) .......... */
L240:
	s = (r__1 = b_ref(k1, k1), dabs(r__1)) + (r__2 = b_ref(k1, k), dabs(
		r__2));
	if (s == 0.f) {
	    goto L260;
	}
	u1 = b_ref(k1, k1) / s;
	u2 = b_ref(k1, k) / s;
	r__1 = sqrt(u1 * u1 + u2 * u2);
	r__ = r_sign(&r__1, &u1);
	v1 = -(u1 + r__) / r__;
	v2 = -u2 / r__;
	u2 = v2 / v1;

	i__2 = ll;
	for (i__ = lor1; i__ <= i__2; ++i__) {
	    t = a_ref(i__, k1) + u2 * a_ref(i__, k);
	    a_ref(i__, k1) = a_ref(i__, k1) + t * v1;
	    a_ref(i__, k) = a_ref(i__, k) + t * v2;
	    t = b_ref(i__, k1) + u2 * b_ref(i__, k);
	    b_ref(i__, k1) = b_ref(i__, k1) + t * v1;
	    b_ref(i__, k) = b_ref(i__, k) + t * v2;
/* L250: */
	}
/*        --------------------- BEGIN TIMING CODE ----------------------- */
	opst += (real) ((ll + 1 - lor1) * 12 + 11);
/*        ---------------------- END TIMING CODE ------------------------ */

	b_ref(k1, k) = 0.f;
	if (! (*matz)) {
	    goto L260;
	}

	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    t = z___ref(i__, k1) + u2 * z___ref(i__, k);
	    z___ref(i__, k1) = z___ref(i__, k1) + t * v1;
	    z___ref(i__, k) = z___ref(i__, k) + t * v2;
/* L255: */
	}
/*        --------------------- BEGIN TIMING CODE ----------------------- */
	opst += (real) (*n * 6);
/*        ---------------------- END TIMING CODE ------------------------ */

L260:
	;
    }
/*     .......... END QZ STEP .......... */
    goto L70;
/*     .......... SET ERROR -- ALL EIGENVALUES HAVE NOT   
                  CONVERGED AFTER 30*N ITERATIONS .......... */
L1000:
    *ierr = en;
/*     .......... SAVE EPSB FOR USE BY QZVAL AND QZVEC .......... */
L1001:
    if (*n > 1) {
	b_ref(*n, 1) = epsb;
    }

/*     ---------------------- BEGIN TIMING CODE ------------------------- */
    latime_1.ops += opst;
    opst = 0.f;
/*     ----------------------- END TIMING CODE -------------------------- */

    return 0;
} /* qzit_ */

#undef z___ref
#undef b_ref
#undef a_ref


/* Subroutine */ int qzval_(integer *nm, integer *n, real *a, real *b, real *
	alfr, real *alfi, real *beta, logical *matz, real *z__)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, z_dim1, z_offset, i__1, i__2;
    real r__1, r__2, r__3, r__4;

    /* Builtin functions */
    double sqrt(doublereal), r_sign(real *, real *);

    /* Local variables */
    static real epsb, opst, c__, d__, e, opst2;
    static integer i__, j;
    static real r__, s, t, a1, a2, u1, u2, v1, v2, a11, a12, a21, a22, b11, 
	    b12, b22, di, ei;
    static integer na;
    static real an, bn;
    static integer en;
    static real cq, dr;
    static integer nn;
    static real cz, ti, tr, a1i, a2i, a11i, a12i, a22i, a11r, a12r, a22r, sqi,
	     ssi;
    static integer isw;
    static real sqr, szi, ssr, szr;


#define a_ref(a_1,a_2) a[(a_2)*a_dim1 + a_1]
#define b_ref(a_1,a_2) b[(a_2)*b_dim1 + a_1]
#define z___ref(a_1,a_2) z__[(a_2)*z_dim1 + a_1]



/*     ---------------------- BEGIN TIMING CODE -------------------------   
       COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED   
       OPST IS USED TO ACCUMULATE SMALL CONTRIBUTIONS TO OPS   
       TO AVOID ROUNDOFF ERROR   
       ----------------------- END TIMING CODE --------------------------   


       THIS SUBROUTINE IS THE THIRD STEP OF THE QZ ALGORITHM   
       FOR SOLVING GENERALIZED MATRIX EIGENVALUE PROBLEMS,   
       SIAM J. NUMER. ANAL. 10, 241-256(1973) BY MOLER AND STEWART.   

       THIS SUBROUTINE ACCEPTS A PAIR OF REAL MATRICES, ONE OF THEM   
       IN QUASI-TRIANGULAR FORM AND THE OTHER IN UPPER TRIANGULAR FORM.   
       IT REDUCES THE QUASI-TRIANGULAR MATRIX FURTHER, SO THAT ANY   
       REMAINING 2-BY-2 BLOCKS CORRESPOND TO PAIRS OF COMPLEX   
       EIGENVALUES, AND RETURNS QUANTITIES WHOSE RATIOS GIVE THE   
       GENERALIZED EIGENVALUES.  IT IS USUALLY PRECEDED BY  QZHES   
       AND  QZIT  AND MAY BE FOLLOWED BY  QZVEC.   

       ON INPUT   

          NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL   
            ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM   
            DIMENSION STATEMENT.   

          N IS THE ORDER OF THE MATRICES.   

          A CONTAINS A REAL UPPER QUASI-TRIANGULAR MATRIX.   

          B CONTAINS A REAL UPPER TRIANGULAR MATRIX.  IN ADDITION,   
            LOCATION B(N,1) CONTAINS THE TOLERANCE QUANTITY (EPSB)   
            COMPUTED AND SAVED IN  QZIT.   

          MATZ SHOULD BE SET TO .TRUE. IF THE RIGHT HAND TRANSFORMATIONS   
            ARE TO BE ACCUMULATED FOR LATER USE IN COMPUTING   
            EIGENVECTORS, AND TO .FALSE. OTHERWISE.   

          Z CONTAINS, IF MATZ HAS BEEN SET TO .TRUE., THE   
            TRANSFORMATION MATRIX PRODUCED IN THE REDUCTIONS BY QZHES   
            AND QZIT, IF PERFORMED, OR ELSE THE IDENTITY MATRIX.   
            IF MATZ HAS BEEN SET TO .FALSE., Z IS NOT REFERENCED.   

       ON OUTPUT   

          A HAS BEEN REDUCED FURTHER TO A QUASI-TRIANGULAR MATRIX   
            IN WHICH ALL NONZERO SUBDIAGONAL ELEMENTS CORRESPOND TO   
            PAIRS OF COMPLEX EIGENVALUES.   

          B IS STILL IN UPPER TRIANGULAR FORM, ALTHOUGH ITS ELEMENTS   
            HAVE BEEN ALTERED.  B(N,1) IS UNALTERED.   

          ALFR AND ALFI CONTAIN THE REAL AND IMAGINARY PARTS OF THE   
            DIAGONAL ELEMENTS OF THE TRIANGULAR MATRIX THAT WOULD BE   
            OBTAINED IF A WERE REDUCED COMPLETELY TO TRIANGULAR FORM   
            BY UNITARY TRANSFORMATIONS.  NON-ZERO VALUES OF ALFI OCCUR   
            IN PAIRS, THE FIRST MEMBER POSITIVE AND THE SECOND NEGATIVE.   

          BETA CONTAINS THE DIAGONAL ELEMENTS OF THE CORRESPONDING B,   
            NORMALIZED TO BE REAL AND NON-NEGATIVE.  THE GENERALIZED   
            EIGENVALUES ARE THEN THE RATIOS ((ALFR+I*ALFI)/BETA).   

          Z CONTAINS THE PRODUCT OF THE RIGHT HAND TRANSFORMATIONS   
            (FOR ALL THREE STEPS) IF MATZ HAS BEEN SET TO .TRUE.   

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,   
       MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY   

       THIS VERSION DATED AUGUST 1983.   

       ------------------------------------------------------------------   

       Parameter adjustments */
    z_dim1 = *nm;
    z_offset = 1 + z_dim1 * 1;
    z__ -= z_offset;
    --beta;
    --alfi;
    --alfr;
    b_dim1 = *nm;
    b_offset = 1 + b_dim1 * 1;
    b -= b_offset;
    a_dim1 = *nm;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;

    /* Function Body */
    epsb = b_ref(*n, 1);
    isw = 1;
/*     .......... FIND EIGENVALUES OF QUASI-TRIANGULAR MATRICES.   
                  FOR EN=N STEP -1 UNTIL 1 DO -- ..........   

       ---------------------- BEGIN TIMING CODE ------------------------- */
    opst = 0.f;
    opst2 = 0.f;
/*     ----------------------- END TIMING CODE -------------------------- */

    i__1 = *n;
    for (nn = 1; nn <= i__1; ++nn) {

/*        --------------------- BEGIN TIMING CODE ----------------------- */
	opst += opst2;
	opst2 = 0.f;
/*        ---------------------- END TIMING CODE ------------------------ */

	en = *n + 1 - nn;
	na = en - 1;
	if (isw == 2) {
	    goto L505;
	}
	if (en == 1) {
	    goto L410;
	}
	if (a_ref(en, na) != 0.f) {
	    goto L420;
	}
/*     .......... 1-BY-1 BLOCK, ONE REAL ROOT .......... */
L410:
	alfr[en] = a_ref(en, en);
	if (b_ref(en, en) < 0.f) {
	    alfr[en] = -alfr[en];
	}
	beta[en] = (r__1 = b_ref(en, en), dabs(r__1));
	alfi[en] = 0.f;
	goto L510;
/*     .......... 2-BY-2 BLOCK .......... */
L420:
	if ((r__1 = b_ref(na, na), dabs(r__1)) <= epsb) {
	    goto L455;
	}
	if ((r__1 = b_ref(en, en), dabs(r__1)) > epsb) {
	    goto L430;
	}
	a1 = a_ref(en, en);
	a2 = a_ref(en, na);
	bn = 0.f;
	goto L435;
L430:
	an = (r__1 = a_ref(na, na), dabs(r__1)) + (r__2 = a_ref(na, en), dabs(
		r__2)) + (r__3 = a_ref(en, na), dabs(r__3)) + (r__4 = a_ref(
		en, en), dabs(r__4));
	bn = (r__1 = b_ref(na, na), dabs(r__1)) + (r__2 = b_ref(na, en), dabs(
		r__2)) + (r__3 = b_ref(en, en), dabs(r__3));
	a11 = a_ref(na, na) / an;
	a12 = a_ref(na, en) / an;
	a21 = a_ref(en, na) / an;
	a22 = a_ref(en, en) / an;
	b11 = b_ref(na, na) / bn;
	b12 = b_ref(na, en) / bn;
	b22 = b_ref(en, en) / bn;
	e = a11 / b11;
	ei = a22 / b22;
	s = a21 / (b11 * b22);
	t = (a22 - e * b22) / b22;
	if (dabs(e) <= dabs(ei)) {
	    goto L431;
	}
	e = ei;
	t = (a11 - e * b11) / b11;
L431:
	c__ = (t - s * b12) * .5f;
	d__ = c__ * c__ + s * (a12 - e * b12);
/*        --------------------- BEGIN TIMING CODE ----------------------- */
	opst2 += 28.f;
/*        ---------------------- END TIMING CODE ------------------------ */
	if (d__ < 0.f) {
	    goto L480;
	}
/*     .......... TWO REAL ROOTS.   
                  ZERO BOTH A(EN,NA) AND B(EN,NA) .......... */
	r__1 = sqrt(d__);
	e += c__ + r_sign(&r__1, &c__);
	a11 -= e * b11;
	a12 -= e * b12;
	a22 -= e * b22;
/*        --------------------- BEGIN TIMING CODE ----------------------- */
	opst2 += 11.f;
/*        ---------------------- END TIMING CODE ------------------------ */
	if (dabs(a11) + dabs(a12) < dabs(a21) + dabs(a22)) {
	    goto L432;
	}
	a1 = a12;
	a2 = a11;
	goto L435;
L432:
	a1 = a22;
	a2 = a21;
/*     .......... CHOOSE AND APPLY REAL Z .......... */
L435:
	s = dabs(a1) + dabs(a2);
	u1 = a1 / s;
	u2 = a2 / s;
	r__1 = sqrt(u1 * u1 + u2 * u2);
	r__ = r_sign(&r__1, &u1);
	v1 = -(u1 + r__) / r__;
	v2 = -u2 / r__;
	u2 = v2 / v1;

	i__2 = en;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    t = a_ref(i__, en) + u2 * a_ref(i__, na);
	    a_ref(i__, en) = a_ref(i__, en) + t * v1;
	    a_ref(i__, na) = a_ref(i__, na) + t * v2;
	    t = b_ref(i__, en) + u2 * b_ref(i__, na);
	    b_ref(i__, en) = b_ref(i__, en) + t * v1;
	    b_ref(i__, na) = b_ref(i__, na) + t * v2;
/* L440: */
	}
/*        --------------------- BEGIN TIMING CODE ----------------------- */
	opst2 += (real) (en * 12 + 11);
/*        ---------------------- END TIMING CODE ------------------------ */

	if (! (*matz)) {
	    goto L450;
	}

	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    t = z___ref(i__, en) + u2 * z___ref(i__, na);
	    z___ref(i__, en) = z___ref(i__, en) + t * v1;
	    z___ref(i__, na) = z___ref(i__, na) + t * v2;
/* L445: */
	}
/*        --------------------- BEGIN TIMING CODE ----------------------- */
	opst2 += (real) (*n * 6);
/*        ---------------------- END TIMING CODE ------------------------ */

L450:
	if (bn == 0.f) {
	    goto L475;
	}
	if (an < dabs(e) * bn) {
	    goto L455;
	}
	a1 = b_ref(na, na);
	a2 = b_ref(en, na);
	goto L460;
L455:
	a1 = a_ref(na, na);
	a2 = a_ref(en, na);
/*     .......... CHOOSE AND APPLY REAL Q .......... */
L460:
	s = dabs(a1) + dabs(a2);
	if (s == 0.f) {
	    goto L475;
	}
	u1 = a1 / s;
	u2 = a2 / s;
	r__1 = sqrt(u1 * u1 + u2 * u2);
	r__ = r_sign(&r__1, &u1);
	v1 = -(u1 + r__) / r__;
	v2 = -u2 / r__;
	u2 = v2 / v1;

	i__2 = *n;
	for (j = na; j <= i__2; ++j) {
	    t = a_ref(na, j) + u2 * a_ref(en, j);
	    a_ref(na, j) = a_ref(na, j) + t * v1;
	    a_ref(en, j) = a_ref(en, j) + t * v2;
	    t = b_ref(na, j) + u2 * b_ref(en, j);
	    b_ref(na, j) = b_ref(na, j) + t * v1;
	    b_ref(en, j) = b_ref(en, j) + t * v2;
/* L470: */
	}
/*        --------------------- BEGIN TIMING CODE ----------------------- */
	opst2 += (real) ((*n + 1 - na) * 12 + 11);
/*        ---------------------- END TIMING CODE ------------------------ */

L475:
	a_ref(en, na) = 0.f;
	b_ref(en, na) = 0.f;
	alfr[na] = a_ref(na, na);
	alfr[en] = a_ref(en, en);
	if (b_ref(na, na) < 0.f) {
	    alfr[na] = -alfr[na];
	}
	if (b_ref(en, en) < 0.f) {
	    alfr[en] = -alfr[en];
	}
	beta[na] = (r__1 = b_ref(na, na), dabs(r__1));
	beta[en] = (r__1 = b_ref(en, en), dabs(r__1));
	alfi[en] = 0.f;
	alfi[na] = 0.f;
	goto L505;
/*     .......... TWO COMPLEX ROOTS .......... */
L480:
	e += c__;
	ei = sqrt(-d__);
	a11r = a11 - e * b11;
	a11i = ei * b11;
	a12r = a12 - e * b12;
	a12i = ei * b12;
	a22r = a22 - e * b22;
	a22i = ei * b22;
	if (dabs(a11r) + dabs(a11i) + dabs(a12r) + dabs(a12i) < dabs(a21) + 
		dabs(a22r) + dabs(a22i)) {
	    goto L482;
	}
	a1 = a12r;
	a1i = a12i;
	a2 = -a11r;
	a2i = -a11i;
	goto L485;
L482:
	a1 = a22r;
	a1i = a22i;
	a2 = -a21;
	a2i = 0.f;
/*     .......... CHOOSE COMPLEX Z .......... */
L485:
	cz = sqrt(a1 * a1 + a1i * a1i);
	if (cz == 0.f) {
	    goto L487;
	}
	szr = (a1 * a2 + a1i * a2i) / cz;
	szi = (a1 * a2i - a1i * a2) / cz;
	r__ = sqrt(cz * cz + szr * szr + szi * szi);
	cz /= r__;
	szr /= r__;
	szi /= r__;
	goto L490;
L487:
	szr = 1.f;
	szi = 0.f;
L490:
	if (an < (dabs(e) + ei) * bn) {
	    goto L492;
	}
	a1 = cz * b11 + szr * b12;
	a1i = szi * b12;
	a2 = szr * b22;
	a2i = szi * b22;
	goto L495;
L492:
	a1 = cz * a11 + szr * a12;
	a1i = szi * a12;
	a2 = cz * a21 + szr * a22;
	a2i = szi * a22;
/*     .......... CHOOSE COMPLEX Q .......... */
L495:
	cq = sqrt(a1 * a1 + a1i * a1i);
	if (cq == 0.f) {
	    goto L497;
	}
	sqr = (a1 * a2 + a1i * a2i) / cq;
	sqi = (a1 * a2i - a1i * a2) / cq;
	r__ = sqrt(cq * cq + sqr * sqr + sqi * sqi);
	cq /= r__;
	sqr /= r__;
	sqi /= r__;
	goto L500;
L497:
	sqr = 1.f;
	sqi = 0.f;
/*     .......... COMPUTE DIAGONAL ELEMENTS THAT WOULD RESULT   
                  IF TRANSFORMATIONS WERE APPLIED .......... */
L500:
	ssr = sqr * szr + sqi * szi;
	ssi = sqr * szi - sqi * szr;
	i__ = 1;
	tr = cq * cz * a11 + cq * szr * a12 + sqr * cz * a21 + ssr * a22;
	ti = cq * szi * a12 - sqi * cz * a21 + ssi * a22;
	dr = cq * cz * b11 + cq * szr * b12 + ssr * b22;
	di = cq * szi * b12 + ssi * b22;
	goto L503;
L502:
	i__ = 2;
	tr = ssr * a11 - sqr * cz * a12 - cq * szr * a21 + cq * cz * a22;
	ti = -ssi * a11 - sqi * cz * a12 + cq * szi * a21;
	dr = ssr * b11 - sqr * cz * b12 + cq * cz * b22;
	di = -ssi * b11 - sqi * cz * b12;
L503:
	t = ti * dr - tr * di;
	j = na;
	if (t < 0.f) {
	    j = en;
	}
	r__ = sqrt(dr * dr + di * di);
	beta[j] = bn * r__;
	alfr[j] = an * (tr * dr + ti * di) / r__;
	alfi[j] = an * t / r__;
	if (i__ == 1) {
	    goto L502;
	}
/*        --------------------- BEGIN TIMING CODE ----------------------- */
	opst2 += 151.f;
/*        ---------------------- END TIMING CODE ------------------------ */
L505:
	isw = 3 - isw;
L510:
	;
    }

/*     ---------------------- BEGIN TIMING CODE ------------------------- */
    latime_1.ops += opst + opst2;
/*     ----------------------- END TIMING CODE -------------------------- */

    b_ref(*n, 1) = epsb;

    return 0;
} /* qzval_ */

#undef z___ref
#undef b_ref
#undef a_ref


/* Subroutine */ int qzvec_(integer *nm, integer *n, real *a, real *b, real *
	alfr, real *alfi, real *beta, real *z__)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, z_dim1, z_offset, i__1, i__2, 
	    i__3;
    real r__1, r__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static real alfm, almi, betm, epsb, almr, d__;
    static integer i__, j, k, m;
    static real q, r__, s, t, w, x, y;
    static integer in2by2;
    static real t1, t2, w1, x1, z1, di;
    static integer na, ii, en, jj;
    static real ra, dr, sa;
    static integer nn;
    static real ti, rr, tr, zz;
    static integer isw, enm2;


#define a_ref(a_1,a_2) a[(a_2)*a_dim1 + a_1]
#define b_ref(a_1,a_2) b[(a_2)*b_dim1 + a_1]
#define z___ref(a_1,a_2) z__[(a_2)*z_dim1 + a_1]



/*     ---------------------- BEGIN TIMING CODE -------------------------   
       COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED   
       OPST IS USED TO ACCUMULATE SMALL CONTRIBUTIONS TO OPS   
       TO AVOID ROUNDOFF ERROR   
       ----------------------- END TIMING CODE --------------------------   


       THIS SUBROUTINE IS THE OPTIONAL FOURTH STEP OF THE QZ ALGORITHM   
       FOR SOLVING GENERALIZED MATRIX EIGENVALUE PROBLEMS,   
       SIAM J. NUMER. ANAL. 10, 241-256(1973) BY MOLER AND STEWART.   

       THIS SUBROUTINE ACCEPTS A PAIR OF REAL MATRICES, ONE OF THEM IN   
       QUASI-TRIANGULAR FORM (IN WHICH EACH 2-BY-2 BLOCK CORRESPONDS TO   
       A PAIR OF COMPLEX EIGENVALUES) AND THE OTHER IN UPPER TRIANGULAR   
       FORM.  IT COMPUTES THE EIGENVECTORS OF THE TRIANGULAR PROBLEM AND   
       TRANSFORMS THE RESULTS BACK TO THE ORIGINAL COORDINATE SYSTEM.   
       IT IS USUALLY PRECEDED BY  QZHES,  QZIT, AND  QZVAL.   

       ON INPUT   

          NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL   
            ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM   
            DIMENSION STATEMENT.   

          N IS THE ORDER OF THE MATRICES.   

          A CONTAINS A REAL UPPER QUASI-TRIANGULAR MATRIX.   

          B CONTAINS A REAL UPPER TRIANGULAR MATRIX.  IN ADDITION,   
            LOCATION B(N,1) CONTAINS THE TOLERANCE QUANTITY (EPSB)   
            COMPUTED AND SAVED IN  QZIT.   

          ALFR, ALFI, AND BETA  ARE VECTORS WITH COMPONENTS WHOSE   
            RATIOS ((ALFR+I*ALFI)/BETA) ARE THE GENERALIZED   
            EIGENVALUES.  THEY ARE USUALLY OBTAINED FROM  QZVAL.   

          Z CONTAINS THE TRANSFORMATION MATRIX PRODUCED IN THE   
            REDUCTIONS BY  QZHES,  QZIT, AND  QZVAL, IF PERFORMED.   
            IF THE EIGENVECTORS OF THE TRIANGULAR PROBLEM ARE   
            DESIRED, Z MUST CONTAIN THE IDENTITY MATRIX.   

       ON OUTPUT   

          A IS UNALTERED.  ITS SUBDIAGONAL ELEMENTS PROVIDE INFORMATION   
             ABOUT THE STORAGE OF THE COMPLEX EIGENVECTORS.   

          B HAS BEEN DESTROYED.   

          ALFR, ALFI, AND BETA ARE UNALTERED.   

          Z CONTAINS THE REAL AND IMAGINARY PARTS OF THE EIGENVECTORS.   
            IF ALFI(I) .EQ. 0.0, THE I-TH EIGENVALUE IS REAL AND   
              THE I-TH COLUMN OF Z CONTAINS ITS EIGENVECTOR.   
            IF ALFI(I) .NE. 0.0, THE I-TH EIGENVALUE IS COMPLEX.   
              IF ALFI(I) .GT. 0.0, THE EIGENVALUE IS THE FIRST OF   
                A COMPLEX PAIR AND THE I-TH AND (I+1)-TH COLUMNS   
                OF Z CONTAIN ITS EIGENVECTOR.   
              IF ALFI(I) .LT. 0.0, THE EIGENVALUE IS THE SECOND OF   
                A COMPLEX PAIR AND THE (I-1)-TH AND I-TH COLUMNS   
                OF Z CONTAIN THE CONJUGATE OF ITS EIGENVECTOR.   
            EACH EIGENVECTOR IS NORMALIZED SO THAT THE MODULUS   
            OF ITS LARGEST COMPONENT IS 1.0 .   

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,   
       MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY   

       THIS VERSION DATED AUGUST 1983.   

       ------------------------------------------------------------------   

       Parameter adjustments */
    z_dim1 = *nm;
    z_offset = 1 + z_dim1 * 1;
    z__ -= z_offset;
    --beta;
    --alfi;
    --alfr;
    b_dim1 = *nm;
    b_offset = 1 + b_dim1 * 1;
    b -= b_offset;
    a_dim1 = *nm;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;

    /* Function Body */
    epsb = b_ref(*n, 1);
    isw = 1;
/*     .......... FOR EN=N STEP -1 UNTIL 1 DO -- .......... */
    i__1 = *n;
    for (nn = 1; nn <= i__1; ++nn) {
/*        --------------------- BEGIN TIMING CODE ----------------------- */
	in2by2 = 0;
/*        ---------------------- END TIMING CODE ------------------------ */
	en = *n + 1 - nn;
	na = en - 1;
	if (isw == 2) {
	    goto L795;
	}
	if (alfi[en] != 0.f) {
	    goto L710;
	}
/*     .......... REAL VECTOR .......... */
	m = en;
	b_ref(en, en) = 1.f;
	if (na == 0) {
	    goto L800;
	}
	alfm = alfr[m];
	betm = beta[m];
/*     .......... FOR I=EN-1 STEP -1 UNTIL 1 DO -- .......... */
	i__2 = na;
	for (ii = 1; ii <= i__2; ++ii) {
	    i__ = en - ii;
	    w = betm * a_ref(i__, i__) - alfm * b_ref(i__, i__);
	    r__ = 0.f;

	    i__3 = en;
	    for (j = m; j <= i__3; ++j) {
/* L610: */
		r__ += (betm * a_ref(i__, j) - alfm * b_ref(i__, j)) * b_ref(
			j, en);
	    }

	    if (i__ == 1 || isw == 2) {
		goto L630;
	    }
	    if (betm * a_ref(i__, i__ - 1) == 0.f) {
		goto L630;
	    }
	    zz = w;
	    s = r__;
	    goto L690;
L630:
	    m = i__;
	    if (isw == 2) {
		goto L640;
	    }
/*     .......... REAL 1-BY-1 BLOCK .......... */
	    t = w;
	    if (w == 0.f) {
		t = epsb;
	    }
	    b_ref(i__, en) = -r__ / t;
	    goto L700;
/*     .......... REAL 2-BY-2 BLOCK .......... */
L640:
	    x = betm * a_ref(i__, i__ + 1) - alfm * b_ref(i__, i__ + 1);
	    y = betm * a_ref(i__ + 1, i__);
	    q = w * zz - x * y;
	    t = (x * s - zz * r__) / q;
	    b_ref(i__, en) = t;
/*           ------------------- BEGIN TIMING CODE ---------------------- */
	    ++in2by2;
/*           -------------------- END TIMING CODE ----------------------- */
	    if (dabs(x) <= dabs(zz)) {
		goto L650;
	    }
	    b_ref(i__ + 1, en) = (-r__ - w * t) / x;
	    goto L690;
L650:
	    b_ref(i__ + 1, en) = (-s - y * t) / zz;
L690:
	    isw = 3 - isw;
L700:
	    ;
	}
/*     .......... END REAL VECTOR ..........   
          --------------------- BEGIN TIMING CODE ----------------------- */
	latime_1.ops += (real) ((en + 2) * (en - 1) + in2by2) * 2.5f;
/*        ---------------------- END TIMING CODE ------------------------ */
	goto L800;
/*     .......... COMPLEX VECTOR .......... */
L710:
	m = na;
	almr = alfr[m];
	almi = alfi[m];
	betm = beta[m];
/*     .......... LAST VECTOR COMPONENT CHOSEN IMAGINARY SO THAT   
                  EIGENVECTOR MATRIX IS TRIANGULAR .......... */
	y = betm * a_ref(en, na);
	b_ref(na, na) = -almi * b_ref(en, en) / y;
	b_ref(na, en) = (almr * b_ref(en, en) - betm * a_ref(en, en)) / y;
	b_ref(en, na) = 0.f;
	b_ref(en, en) = 1.f;
	enm2 = na - 1;
	if (enm2 == 0) {
	    goto L795;
	}
/*     .......... FOR I=EN-2 STEP -1 UNTIL 1 DO -- .......... */
	i__2 = enm2;
	for (ii = 1; ii <= i__2; ++ii) {
	    i__ = na - ii;
	    w = betm * a_ref(i__, i__) - almr * b_ref(i__, i__);
	    w1 = -almi * b_ref(i__, i__);
	    ra = 0.f;
	    sa = 0.f;

	    i__3 = en;
	    for (j = m; j <= i__3; ++j) {
		x = betm * a_ref(i__, j) - almr * b_ref(i__, j);
		x1 = -almi * b_ref(i__, j);
		ra = ra + x * b_ref(j, na) - x1 * b_ref(j, en);
		sa = sa + x * b_ref(j, en) + x1 * b_ref(j, na);
/* L760: */
	    }

	    if (i__ == 1 || isw == 2) {
		goto L770;
	    }
	    if (betm * a_ref(i__, i__ - 1) == 0.f) {
		goto L770;
	    }
	    zz = w;
	    z1 = w1;
	    r__ = ra;
	    s = sa;
	    isw = 2;
	    goto L790;
L770:
	    m = i__;
	    if (isw == 2) {
		goto L780;
	    }
/*     .......... COMPLEX 1-BY-1 BLOCK .......... */
	    tr = -ra;
	    ti = -sa;
L773:
	    dr = w;
	    di = w1;
/*     .......... COMPLEX DIVIDE (T1,T2) = (TR,TI) / (DR,DI) .......... */
L775:
	    if (dabs(di) > dabs(dr)) {
		goto L777;
	    }
	    rr = di / dr;
	    d__ = dr + di * rr;
	    t1 = (tr + ti * rr) / d__;
	    t2 = (ti - tr * rr) / d__;
	    switch (isw) {
		case 1:  goto L787;
		case 2:  goto L782;
	    }
L777:
	    rr = dr / di;
	    d__ = dr * rr + di;
	    t1 = (tr * rr + ti) / d__;
	    t2 = (ti * rr - tr) / d__;
	    switch (isw) {
		case 1:  goto L787;
		case 2:  goto L782;
	    }
/*     .......... COMPLEX 2-BY-2 BLOCK .......... */
L780:
	    x = betm * a_ref(i__, i__ + 1) - almr * b_ref(i__, i__ + 1);
	    x1 = -almi * b_ref(i__, i__ + 1);
	    y = betm * a_ref(i__ + 1, i__);
	    tr = y * ra - w * r__ + w1 * s;
	    ti = y * sa - w * s - w1 * r__;
	    dr = w * zz - w1 * z1 - x * y;
	    di = w * z1 + w1 * zz - x1 * y;
/*           ------------------- BEGIN TIMING CODE ---------------------- */
	    ++in2by2;
/*           -------------------- END TIMING CODE ----------------------- */
	    if (dr == 0.f && di == 0.f) {
		dr = epsb;
	    }
	    goto L775;
L782:
	    b_ref(i__ + 1, na) = t1;
	    b_ref(i__ + 1, en) = t2;
	    isw = 1;
	    if (dabs(y) > dabs(w) + dabs(w1)) {
		goto L785;
	    }
	    tr = -ra - x * b_ref(i__ + 1, na) + x1 * b_ref(i__ + 1, en);
	    ti = -sa - x * b_ref(i__ + 1, en) - x1 * b_ref(i__ + 1, na);
	    goto L773;
L785:
	    t1 = (-r__ - zz * b_ref(i__ + 1, na) + z1 * b_ref(i__ + 1, en)) / 
		    y;
	    t2 = (-s - zz * b_ref(i__ + 1, en) - z1 * b_ref(i__ + 1, na)) / y;
L787:
	    b_ref(i__, na) = t1;
	    b_ref(i__, en) = t2;
L790:
	    ;
	}
/*        --------------------- BEGIN TIMING CODE ----------------------- */
	latime_1.ops += (real) ((en * 6 - 7) * (en - 2) + in2by2 * 31);
/*        ---------------------- END TIMING CODE ------------------------   
       .......... END COMPLEX VECTOR .......... */
L795:
	isw = 3 - isw;
L800:
	;
    }
/*     .......... END BACK SUBSTITUTION.   
                  TRANSFORM TO ORIGINAL COORDINATE SYSTEM.   
                  FOR J=N STEP -1 UNTIL 1 DO -- .......... */
    i__1 = *n;
    for (jj = 1; jj <= i__1; ++jj) {
	j = *n + 1 - jj;

	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    zz = 0.f;

	    i__3 = j;
	    for (k = 1; k <= i__3; ++k) {
/* L860: */
		zz += z___ref(i__, k) * b_ref(k, j);
	    }

	    z___ref(i__, j) = zz;
/* L880: */
	}
    }
/*     ----------------------- BEGIN TIMING CODE ------------------------   
   Computing 2nd power */
    i__2 = *n;
    latime_1.ops += (real) (i__2 * i__2) * (real) (*n + 1);
/*     ------------------------ END TIMING CODE -------------------------   
       .......... NORMALIZE SO THAT MODULUS OF LARGEST   
                  COMPONENT OF EACH VECTOR IS 1.   
                  (ISW IS 1 INITIALLY FROM BEFORE) ..........   
       ------------------------ BEGIN TIMING CODE ----------------------- */
    in2by2 = 0;
/*     ------------------------- END TIMING CODE ------------------------ */
    i__2 = *n;
    for (j = 1; j <= i__2; ++j) {
	d__ = 0.f;
	if (isw == 2) {
	    goto L920;
	}
	if (alfi[j] != 0.f) {
	    goto L945;
	}

	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if ((r__1 = z___ref(i__, j), dabs(r__1)) > d__) {
		d__ = (r__2 = z___ref(i__, j), dabs(r__2));
	    }
/* L890: */
	}

	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
/* L900: */
	    z___ref(i__, j) = z___ref(i__, j) / d__;
	}

	goto L950;

L920:
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    r__ = (r__1 = z___ref(i__, j - 1), dabs(r__1)) + (r__2 = z___ref(
		    i__, j), dabs(r__2));
	    if (r__ != 0.f) {
/* Computing 2nd power */
		r__1 = z___ref(i__, j - 1) / r__;
/* Computing 2nd power */
		r__2 = z___ref(i__, j) / r__;
		r__ *= sqrt(r__1 * r__1 + r__2 * r__2);
	    }
	    if (r__ > d__) {
		d__ = r__;
	    }
/* L930: */
	}

	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    z___ref(i__, j - 1) = z___ref(i__, j - 1) / d__;
	    z___ref(i__, j) = z___ref(i__, j) / d__;
/* L940: */
	}
/*        ---------------------- BEGIN TIMING CODE ---------------------- */
	++in2by2;
/*        ----------------------- END TIMING CODE ----------------------- */

L945:
	isw = 3 - isw;
L950:
	;
    }
/*     ------------------------ BEGIN TIMING CODE ----------------------- */
    latime_1.ops += (real) (*n * (*n + in2by2 * 5));
/*     ------------------------- END TIMING CODE ------------------------ */

    return 0;
} /* qzvec_ */

#undef z___ref
#undef b_ref
#undef a_ref


