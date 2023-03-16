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

static real c_b169 = 1.f;
static integer c__1 = 1;
static complex c_b319 = {1.f,0.f};
static complex c_b364 = {-1.f,0.f};

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
} /* cdiv_   

   Subroutine */ int cinvit_(integer *nm, integer *n, real *ar, real *ai, 
	real *wr, real *wi, logical *select, integer *mm, integer *m, real *
	zr, real *zi, integer *ierr, real *rm1, real *rm2, real *rv1, real *
	rv2)
{
    /* System generated locals */
    integer ar_dim1, ar_offset, ai_dim1, ai_offset, zr_dim1, zr_offset, 
	    zi_dim1, zi_offset, rm1_dim1, rm1_offset, rm2_dim1, rm2_offset, 
	    i__1, i__2, i__3;
    real r__1, r__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    extern /* Subroutine */ int cdiv_(real *, real *, real *, real *, real *, 
	    real *);
    static real norm, opst;
    static integer i__, j, k, s;
    static real x, y, normv;
    static integer ii;
    static real ilambd;
    static integer mp, uk;
    static real rlambd;
    extern doublereal slamch_(char *), pythag_(real *, real *);
    static integer km1, ip1;
    static real growto, ukroot;
    static integer its;
    static real ulp, eps3;


#define ai_ref(a_1,a_2) ai[(a_2)*ai_dim1 + a_1]
#define ar_ref(a_1,a_2) ar[(a_2)*ar_dim1 + a_1]
#define zi_ref(a_1,a_2) zi[(a_2)*zi_dim1 + a_1]
#define zr_ref(a_1,a_2) zr[(a_2)*zr_dim1 + a_1]
#define rm1_ref(a_1,a_2) rm1[(a_2)*rm1_dim1 + a_1]
#define rm2_ref(a_1,a_2) rm2[(a_2)*rm2_dim1 + a_1]



/*     COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED   
       OPST IS USED TO ACCUMULATE SMALL CONTRIBUTIONS TO OPS   
       TO AVOID ROUNDOFF ERROR   

       THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE CX INVIT   
       BY PETERS AND WILKINSON.   
       HANDBOOK FOR AUTO. COMP. VOL.II-LINEAR ALGEBRA, 418-439(1971).   

       THIS SUBROUTINE FINDS THOSE EIGENVECTORS OF A COMPLEX UPPER   
       HESSENBERG MATRIX CORRESPONDING TO SPECIFIED EIGENVALUES,   
       USING INVERSE ITERATION.   

       ON INPUT   

          NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL   
            ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM   
            DIMENSION STATEMENT.   

          N IS THE ORDER OF THE MATRIX.   

          AR AND AI CONTAIN THE REAL AND IMAGINARY PARTS,   
            RESPECTIVELY, OF THE HESSENBERG MATRIX.   

          WR AND WI CONTAIN THE REAL AND IMAGINARY PARTS, RESPECTIVELY,   
            OF THE EIGENVALUES OF THE MATRIX.  THE EIGENVALUES MUST BE   
            STORED IN A MANNER IDENTICAL TO THAT OF SUBROUTINE  COMLR,   
            WHICH RECOGNIZES POSSIBLE SPLITTING OF THE MATRIX.   

          SELECT SPECIFIES THE EIGENVECTORS TO BE FOUND.  THE   
            EIGENVECTOR CORRESPONDING TO THE J-TH EIGENVALUE IS   
            SPECIFIED BY SETTING SELECT(J) TO .TRUE..   

          MM SHOULD BE SET TO AN UPPER BOUND FOR THE NUMBER OF   
            EIGENVECTORS TO BE FOUND.   

       ON OUTPUT   

          AR, AI, WI, AND SELECT ARE UNALTERED.   

          WR MAY HAVE BEEN ALTERED SINCE CLOSE EIGENVALUES ARE PERTURBED   
            SLIGHTLY IN SEARCHING FOR INDEPENDENT EIGENVECTORS.   

          M IS THE NUMBER OF EIGENVECTORS ACTUALLY FOUND.   

          ZR AND ZI CONTAIN THE REAL AND IMAGINARY PARTS, RESPECTIVELY,   
            OF THE EIGENVECTORS.  THE EIGENVECTORS ARE NORMALIZED   
            SO THAT THE COMPONENT OF LARGEST MAGNITUDE IS 1.   
            ANY VECTOR WHICH FAILS THE ACCEPTANCE TEST IS SET TO ZERO.   

          IERR IS SET TO   
            ZERO       FOR NORMAL RETURN,   
            -(2*N+1)   IF MORE THAN MM EIGENVECTORS HAVE BEEN SPECIFIED,   
            -K         IF THE ITERATION CORRESPONDING TO THE K-TH   
                       VALUE FAILS,   
            -(N+K)     IF BOTH ERROR SITUATIONS OCCUR.   

          RM1, RM2, RV1, AND RV2 ARE TEMPORARY STORAGE ARRAYS.   

       THE ALGOL PROCEDURE GUESSVEC APPEARS IN CINVIT IN LINE.   

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
    rm2_dim1 = *n;
    rm2_offset = 1 + rm2_dim1 * 1;
    rm2 -= rm2_offset;
    rm1_dim1 = *n;
    rm1_offset = 1 + rm1_dim1 * 1;
    rm1 -= rm1_offset;
    --select;
    --wi;
    --wr;
    ai_dim1 = *nm;
    ai_offset = 1 + ai_dim1 * 1;
    ai -= ai_offset;
    ar_dim1 = *nm;
    ar_offset = 1 + ar_dim1 * 1;
    ar -= ar_offset;
    zi_dim1 = *nm;
    zi_offset = 1 + zi_dim1 * 1;
    zi -= zi_offset;
    zr_dim1 = *nm;
    zr_offset = 1 + zr_dim1 * 1;
    zr -= zr_offset;

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

    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {
	if (! select[k]) {
	    goto L980;
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
	    if (ar_ref(uk + 1, uk) == 0.f && ai_ref(uk + 1, uk) == 0.f) {
		goto L140;
	    }
/* L120: */
	}
/*     .......... COMPUTE INFINITY NORM OF LEADING UK BY UK   
                  (HESSENBERG) MATRIX .......... */
L140:
	norm = 0.f;
	mp = 1;


/*        INCREMENT OPCOUNT FOR LOOP 180 */
	latime_1.ops += uk * 6 * (uk - 1);
	i__2 = uk;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    x = 0.f;

	    i__3 = uk;
	    for (j = mp; j <= i__3; ++j) {
/* L160: */
		x += pythag_(&ar_ref(i__, j), &ai_ref(i__, j));
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
/*         EPS3 = EPSLON(NORM)   

          INCREMENT OPCOUNT FOR EPS3, UKROOT */
	opst += 3;
	eps3 = norm * ulp;
/*     .......... GROWTO IS THE CRITERION FOR GROWTH .......... */
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


/*        INCREMENT OPCOUNT FOR LOOP 260. */
	opst += k - 1 << 1;
	wr[k] = rlambd;
/*     .......... FORM UPPER HESSENBERG (AR,AI)-(RLAMBD,ILAMBD)*I   
                  AND INITIAL COMPLEX VECTOR .......... */
L280:
	mp = 1;


/*        INCREMENT OP COUNT FOR LOOP 320 */
	latime_1.ops += uk << 1;
	i__2 = uk;
	for (i__ = 1; i__ <= i__2; ++i__) {

	    i__3 = uk;
	    for (j = mp; j <= i__3; ++j) {
		rm1_ref(i__, j) = ar_ref(i__, j);
		rm2_ref(i__, j) = ai_ref(i__, j);
/* L300: */
	    }

	    rm1_ref(i__, i__) = rm1_ref(i__, i__) - rlambd;
	    rm2_ref(i__, i__) = rm2_ref(i__, i__) - ilambd;
	    mp = i__;
	    rv1[i__] = eps3;
/* L320: */
	}
/*     .......... TRIANGULAR DECOMPOSITION WITH INTERCHANGES,   
                  REPLACING ZERO PIVOTS BY EPS3 .......... */
	if (uk == 1) {
	    goto L420;
	}


/*        INCREMENT OP COUNT FOR LOOP 400 */
	latime_1.ops += ((uk << 2) + 52) * (uk - 1);
	i__2 = uk;
	for (i__ = 2; i__ <= i__2; ++i__) {
	    mp = i__ - 1;
	    if (pythag_(&rm1_ref(i__, mp), &rm2_ref(i__, mp)) <= pythag_(&
		    rm1_ref(mp, mp), &rm2_ref(mp, mp))) {
		goto L360;
	    }

	    i__3 = uk;
	    for (j = mp; j <= i__3; ++j) {
		y = rm1_ref(i__, j);
		rm1_ref(i__, j) = rm1_ref(mp, j);
		rm1_ref(mp, j) = y;
		y = rm2_ref(i__, j);
		rm2_ref(i__, j) = rm2_ref(mp, j);
		rm2_ref(mp, j) = y;
/* L340: */
	    }

L360:
	    if (rm1_ref(mp, mp) == 0.f && rm2_ref(mp, mp) == 0.f) {
		rm1_ref(mp, mp) = eps3;
	    }
	    cdiv_(&rm1_ref(i__, mp), &rm2_ref(i__, mp), &rm1_ref(mp, mp), &
		    rm2_ref(mp, mp), &x, &y);
	    if (x == 0.f && y == 0.f) {
		goto L400;
	    }

	    i__3 = uk;
	    for (j = i__; j <= i__3; ++j) {
		rm1_ref(i__, j) = rm1_ref(i__, j) - x * rm1_ref(mp, j) + y * 
			rm2_ref(mp, j);
		rm2_ref(i__, j) = rm2_ref(i__, j) - x * rm2_ref(mp, j) - y * 
			rm1_ref(mp, j);
/* L380: */
	    }

L400:
	    ;
	}

L420:
	if (rm1_ref(uk, uk) == 0.f && rm2_ref(uk, uk) == 0.f) {
	    rm1_ref(uk, uk) = eps3;
	}
	its = 0;
/*     .......... BACK SUBSTITUTION   
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
		x = x - rm1_ref(i__, j) * rv1[j] + rm2_ref(i__, j) * rv2[j];
		y = y - rm1_ref(i__, j) * rv2[j] - rm2_ref(i__, j) * rv1[j];
/* L680: */
	    }

L700:
	    cdiv_(&x, &y, &rm1_ref(i__, i__), &rm2_ref(i__, i__), &rv1[i__], &
		    rv2[i__]);
/* L720: */
	}

/*        INCREMENT OP COUNT FOR BACK SUBSTITUTION LOOP 720 */
	latime_1.ops += (uk << 2) * (uk + 3);
/*     .......... ACCEPTANCE TEST FOR EIGENVECTOR   
                  AND NORMALIZATION .......... */
	++its;
	norm = 0.f;
	normv = 0.f;


/*        INCREMENT OP COUNT ACCEPTANCE TEST */
	latime_1.ops += uk * 19;
	i__2 = uk;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    x = pythag_(&rv1[i__], &rv2[i__]);
	    if (normv >= x) {
		goto L760;
	    }
	    normv = x;
	    j = i__;
L760:
	    norm += x;
/* L780: */
	}

	if (norm < growto) {
	    goto L840;
	}
/*     .......... ACCEPT VECTOR .......... */
	x = rv1[j];
	y = rv2[j];


/*        INCREMENT OP COUNT ACCEPT VECTOR LOOP 820 */
	latime_1.ops += uk << 4;
	i__2 = uk;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    cdiv_(&rv1[i__], &rv2[i__], &x, &y, &zr_ref(i__, s), &zi_ref(i__, 
		    s));
/* L820: */
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
	goto L660;
/*     .......... SET ERROR -- UNACCEPTED EIGENVECTOR .......... */
L880:
	j = 1;
	*ierr = -k;
/*     .......... SET REMAINING VECTOR COMPONENTS TO ZERO .......... */
L900:
	i__2 = *n;
	for (i__ = j; i__ <= i__2; ++i__) {
	    zr_ref(i__, s) = 0.f;
	    zi_ref(i__, s) = 0.f;
/* L920: */
	}

L940:
	++s;
L980:
	;
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
    *m = s - 1;

/*     COMPUTE FINAL OP COUNT */
    latime_1.ops += opst;
    return 0;
} /* cinvit_ */

#undef rm2_ref
#undef rm1_ref
#undef zr_ref
#undef zi_ref
#undef ar_ref
#undef ai_ref


/* Subroutine */ int comqr_(integer *nm, integer *n, integer *low, integer *
	igh, real *hr, real *hi, real *wr, real *wi, integer *ierr)
{
    /* System generated locals */
    integer hr_dim1, hr_offset, hi_dim1, hi_offset, i__1, i__2, i__3, i__4;
    real r__1, r__2, r__3, r__4;

    /* Local variables */
    extern /* Subroutine */ int cdiv_(real *, real *, real *, real *, real *, 
	    real *);
    static real unfl, ovfl, norm, opst;
    static integer i__, j, l;
    static real small;
    static integer en, ll;
    static real si, ti, xi, yi, sr, tr;
    extern doublereal slamch_(char *);
    static real xr, yr;
    extern doublereal pythag_(real *, real *);
    extern /* Subroutine */ int csroot_(real *, real *, real *, real *);
    static real smlnum;
    static integer lp1, itn, its;
    static real ulp, zzi, zzr;
    static integer enm1;
    static real tst1, tst2;


#define hi_ref(a_1,a_2) hi[(a_2)*hi_dim1 + a_1]
#define hr_ref(a_1,a_2) hr[(a_2)*hr_dim1 + a_1]



/*     COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED   
       OPST IS USED TO ACCUMULATE SMALL CONTRIBUTIONS TO OPS   
       TO AVOID ROUNDOFF ERROR   

       THIS SUBROUTINE IS A TRANSLATION OF A UNITARY ANALOGUE OF THE   
       ALGOL PROCEDURE  COMLR, NUM. MATH. 12, 369-376(1968) BY MARTIN   
       AND WILKINSON.   
       HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 396-403(1971).   
       THE UNITARY ANALOGUE SUBSTITUTES THE QR ALGORITHM OF FRANCIS   
       (COMP. JOUR. 4, 332-345(1962)) FOR THE LR ALGORITHM.   

       THIS SUBROUTINE FINDS THE EIGENVALUES OF A COMPLEX   
       UPPER HESSENBERG MATRIX BY THE QR METHOD.   

       ON INPUT   

          NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL   
            ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM   
            DIMENSION STATEMENT.   

          N IS THE ORDER OF THE MATRIX.   

          LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING   
            SUBROUTINE  CBAL.  IF  CBAL  HAS NOT BEEN USED,   
            SET LOW=1, IGH=N.   

          HR AND HI CONTAIN THE REAL AND IMAGINARY PARTS,   
            RESPECTIVELY, OF THE COMPLEX UPPER HESSENBERG MATRIX.   
            THEIR LOWER TRIANGLES BELOW THE SUBDIAGONAL CONTAIN   
            INFORMATION ABOUT THE UNITARY TRANSFORMATIONS USED IN   
            THE REDUCTION BY  CORTH, IF PERFORMED.   

       ON OUTPUT   

          THE UPPER HESSENBERG PORTIONS OF HR AND HI HAVE BEEN   
            DESTROYED.  THEREFORE, THEY MUST BE SAVED BEFORE   
            CALLING  COMQR  IF SUBSEQUENT CALCULATION OF   
            EIGENVECTORS IS TO BE PERFORMED.   

          WR AND WI CONTAIN THE REAL AND IMAGINARY PARTS,   
            RESPECTIVELY, OF THE EIGENVALUES.  IF AN ERROR   
            EXIT IS MADE, THE EIGENVALUES SHOULD BE CORRECT   
            FOR INDICES IERR+1,...,N.   

          IERR IS SET TO   
            ZERO       FOR NORMAL RETURN,   
            J          IF THE LIMIT OF 30*N ITERATIONS IS EXHAUSTED   
                       WHILE THE J-TH EIGENVALUE IS BEING SOUGHT.   

       CALLS CDIV FOR COMPLEX DIVISION.   
       CALLS CSROOT FOR COMPLEX SQUARE ROOT.   
       CALLS PYTHAG FOR  SQRT(A*A + B*B) .   

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,   
       MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY   

       THIS VERSION DATED AUGUST 1983.   

       ------------------------------------------------------------------   



       Parameter adjustments */
    --wi;
    --wr;
    hi_dim1 = *nm;
    hi_offset = 1 + hi_dim1 * 1;
    hi -= hi_offset;
    hr_dim1 = *nm;
    hr_offset = 1 + hr_dim1 * 1;
    hr -= hr_offset;

    /* Function Body */
    if (*n <= 0) {
	return 0;
    }

/*     COMPUTE THE 1-NORM OF MATRIX H */

    norm = 0.f;
    i__1 = *igh;
    for (j = *low; j <= i__1; ++j) {
	sr = 0.f;
/* Computing MIN */
	i__3 = *igh, i__4 = j + 1;
	i__2 = min(i__3,i__4);
	for (i__ = *low; i__ <= i__2; ++i__) {
	    sr += pythag_(&hr_ref(i__, j), &hi_ref(i__, j));
/* L4: */
	}
	norm = dmax(norm,sr);
/* L5: */
    }

/*     GET SMALL FOR NEW CONVERGENCE CRITERION AS IN LAPACK */

    unfl = slamch_("SAFE MINIMUM");
    ovfl = slamch_("OVERFLOW");
    ulp = slamch_("EPSILON") * slamch_("BASE");
/* Computing MAX */
    r__1 = unfl * (*n / ulp), r__2 = *n / (ulp * ovfl);
    smlnum = dmax(r__1,r__2);
/* Computing MAX */
    r__1 = smlnum, r__2 = ulp * norm;
    small = dmax(r__1,r__2);


/*     INITIALIZE */
    latime_1.itcnt = 0.f;
    opst = 0.f;
    *ierr = 0;
    if (*low == *igh) {
	goto L180;
    }
/*     .......... CREATE REAL SUBDIAGONAL ELEMENTS .......... */
    l = *low + 1;


/*        INCREMENT OP COUNT FOR LOOP 170 */
    latime_1.ops += ((*igh - *low + 1) * 6 + 32) * (*igh - l + 1);
    i__1 = *igh;
    for (i__ = l; i__ <= i__1; ++i__) {
/* Computing MIN */
	i__2 = i__ + 1;
	ll = min(i__2,*igh);
	if (hi_ref(i__, i__ - 1) == 0.f) {
	    goto L170;
	}
	norm = pythag_(&hr_ref(i__, i__ - 1), &hi_ref(i__, i__ - 1));
	yr = hr_ref(i__, i__ - 1) / norm;
	yi = hi_ref(i__, i__ - 1) / norm;
	hr_ref(i__, i__ - 1) = norm;
	hi_ref(i__, i__ - 1) = 0.f;

	i__2 = *igh;
	for (j = i__; j <= i__2; ++j) {
	    si = yr * hi_ref(i__, j) - yi * hr_ref(i__, j);
	    hr_ref(i__, j) = yr * hr_ref(i__, j) + yi * hi_ref(i__, j);
	    hi_ref(i__, j) = si;
/* L155: */
	}

	i__2 = ll;
	for (j = *low; j <= i__2; ++j) {
	    si = yr * hi_ref(j, i__) + yi * hr_ref(j, i__);
	    hr_ref(j, i__) = yr * hr_ref(j, i__) - yi * hi_ref(j, i__);
	    hi_ref(j, i__) = si;
/* L160: */
	}

L170:
	;
    }
/*     .......... STORE ROOTS ISOLATED BY CBAL .......... */
L180:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (i__ >= *low && i__ <= *igh) {
	    goto L200;
	}
	wr[i__] = hr_ref(i__, i__);
	wi[i__] = hi_ref(i__, i__);
L200:
	;
    }

    en = *igh;
    tr = 0.f;
    ti = 0.f;
    itn = *n * 30;
/*     .......... SEARCH FOR NEXT EIGENVALUE .......... */
L220:
    if (en < *low) {
	goto L1001;
    }
    its = 0;
    enm1 = en - 1;
/*     .......... LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT   
                  FOR L=EN STEP -1 UNTIL LOW E0 -- .......... */
L240:
    i__1 = en;
    for (ll = *low; ll <= i__1; ++ll) {
	l = en + *low - ll;
	if (l == *low) {
	    goto L300;
	}
	tst1 = (r__1 = hr_ref(l - 1, l - 1), dabs(r__1)) + (r__2 = hi_ref(l - 
		1, l - 1), dabs(r__2)) + (r__3 = hr_ref(l, l), dabs(r__3)) + (
		r__4 = hi_ref(l, l), dabs(r__4));
/*         TST2 = TST1 + ABS(HR(L,L-1))   
           IF (TST2 .EQ. TST1) GO TO 300 */
	tst2 = (r__1 = hr_ref(l, l - 1), dabs(r__1));
/* Computing MIN */
	r__1 = ulp * tst1;
	if (tst2 <= dmin(r__1,small)) {
	    goto L300;
	}
/* L260: */
    }
/*     .......... FORM SHIFT .......... */
L300:

/*        INCREMENT OP COUNT FOR CONVERGENCE TEST */
    latime_1.ops += en - l + 1 << 2;
    if (l == en) {
	goto L660;
    }
    if (itn == 0) {
	goto L1000;
    }
    if (its == 10 || its == 20) {
	goto L320;
    }

/*        INCREMENT OPCOUNT FOR FOMING SHIFT */
    opst += 58;
    sr = hr_ref(en, en);
    si = hi_ref(en, en);
    xr = hr_ref(enm1, en) * hr_ref(en, enm1);
    xi = hi_ref(enm1, en) * hr_ref(en, enm1);
    if (xr == 0.f && xi == 0.f) {
	goto L340;
    }
    yr = (hr_ref(enm1, enm1) - sr) / 2.f;
    yi = (hi_ref(enm1, enm1) - si) / 2.f;
/* Computing 2nd power */
    r__2 = yr;
/* Computing 2nd power */
    r__3 = yi;
    r__1 = r__2 * r__2 - r__3 * r__3 + xr;
    r__4 = yr * 2.f * yi + xi;
    csroot_(&r__1, &r__4, &zzr, &zzi);
    if (yr * zzr + yi * zzi >= 0.f) {
	goto L310;
    }
    zzr = -zzr;
    zzi = -zzi;
L310:
    r__1 = yr + zzr;
    r__2 = yi + zzi;
    cdiv_(&xr, &xi, &r__1, &r__2, &xr, &xi);
    sr -= xr;
    si -= xi;
    goto L340;
/*     .......... FORM EXCEPTIONAL SHIFT .......... */
L320:
    sr = (r__1 = hr_ref(en, enm1), dabs(r__1)) + (r__2 = hr_ref(enm1, en - 2),
	     dabs(r__2));
    si = 0.f;

L340:
    i__1 = en;
    for (i__ = *low; i__ <= i__1; ++i__) {
	hr_ref(i__, i__) = hr_ref(i__, i__) - sr;
	hi_ref(i__, i__) = hi_ref(i__, i__) - si;
/* L360: */
    }

/*        INCREMENT OPCOUNT FOR LOOP 360 */
    latime_1.ops += en << 1;

    tr += sr;
    ti += si;
    ++its;
    --itn;

/*       UPDATE ITERATION NUMBER */
    latime_1.itcnt = (real) (*n * 30 - itn);
/*     .......... REDUCE TO TRIANGLE (ROWS) .......... */
    lp1 = l + 1;


/*        INCREMENT OPCOUNT FOR REDUCING TO TRIANGULAR, LOOP 500 */
    latime_1.ops += (en - lp1 + 1) * ((en - lp1) * 10 + 61);
    i__1 = en;
    for (i__ = lp1; i__ <= i__1; ++i__) {
	sr = hr_ref(i__, i__ - 1);
	hr_ref(i__, i__ - 1) = 0.f;
	r__1 = pythag_(&hr_ref(i__ - 1, i__ - 1), &hi_ref(i__ - 1, i__ - 1));
	norm = pythag_(&r__1, &sr);
	xr = hr_ref(i__ - 1, i__ - 1) / norm;
	wr[i__ - 1] = xr;
	xi = hi_ref(i__ - 1, i__ - 1) / norm;
	wi[i__ - 1] = xi;
	hr_ref(i__ - 1, i__ - 1) = norm;
	hi_ref(i__ - 1, i__ - 1) = 0.f;
	hi_ref(i__, i__ - 1) = sr / norm;

	i__2 = en;
	for (j = i__; j <= i__2; ++j) {
	    yr = hr_ref(i__ - 1, j);
	    yi = hi_ref(i__ - 1, j);
	    zzr = hr_ref(i__, j);
	    zzi = hi_ref(i__, j);
	    hr_ref(i__ - 1, j) = xr * yr + xi * yi + hi_ref(i__, i__ - 1) * 
		    zzr;
	    hi_ref(i__ - 1, j) = xr * yi - xi * yr + hi_ref(i__, i__ - 1) * 
		    zzi;
	    hr_ref(i__, j) = xr * zzr - xi * zzi - hi_ref(i__, i__ - 1) * yr;
	    hi_ref(i__, j) = xr * zzi + xi * zzr - hi_ref(i__, i__ - 1) * yi;
/* L490: */
	}

/* L500: */
    }

    si = hi_ref(en, en);
    if (si == 0.f) {
	goto L540;
    }
    norm = pythag_(&hr_ref(en, en), &si);
    sr = hr_ref(en, en) / norm;
    si /= norm;
    hr_ref(en, en) = norm;
    hi_ref(en, en) = 0.f;

/*        INCREMENT OPCOUNT */
    opst += 20;
/*     .......... INVERSE OPERATION (COLUMNS) .......... */
L540:
    i__1 = en;
    for (j = lp1; j <= i__1; ++j) {
	xr = wr[j - 1];
	xi = wi[j - 1];

	i__2 = j;
	for (i__ = l; i__ <= i__2; ++i__) {
	    yr = hr_ref(i__, j - 1);
	    yi = 0.f;
	    zzr = hr_ref(i__, j);
	    zzi = hi_ref(i__, j);
	    if (i__ == j) {
		goto L560;
	    }
	    yi = hi_ref(i__, j - 1);
	    hi_ref(i__, j - 1) = xr * yi + xi * yr + hi_ref(j, j - 1) * zzi;
L560:
	    hr_ref(i__, j - 1) = xr * yr - xi * yi + hi_ref(j, j - 1) * zzr;
	    hr_ref(i__, j) = xr * zzr + xi * zzi - hi_ref(j, j - 1) * yr;
	    hi_ref(i__, j) = xr * zzi - xi * zzr - hi_ref(j, j - 1) * yi;
/* L580: */
	}

/* L600: */
    }

/*        INCREMENT OPCOUNT FOR INVERSE OPERATION LOOP 600 */
    latime_1.ops += (en - lp1 + 1) * 10 * (en + lp1);

    if (si == 0.f) {
	goto L240;
    }


/*        INCREMENT OP COUNT FOR LOOP 630 */
    latime_1.ops += (en - l + 1) * 6;
    i__1 = en;
    for (i__ = l; i__ <= i__1; ++i__) {
	yr = hr_ref(i__, en);
	yi = hi_ref(i__, en);
	hr_ref(i__, en) = sr * yr - si * yi;
	hi_ref(i__, en) = sr * yi + si * yr;
/* L630: */
    }

    goto L240;
/*     .......... A ROOT FOUND .......... */
L660:
    wr[en] = hr_ref(en, en) + tr;
    wi[en] = hi_ref(en, en) + ti;
    en = enm1;
    goto L220;
/*     .......... SET ERROR -- ALL EIGENVALUES HAVE NOT   
                  CONVERGED AFTER 30*N ITERATIONS .......... */
L1000:
    *ierr = en;
L1001:

/*     COMPUTE FINAL OP COUNT */
    latime_1.ops += opst;
    return 0;
} /* comqr_ */

#undef hr_ref
#undef hi_ref


/* Subroutine */ int comqr2_(integer *nm, integer *n, integer *low, integer *
	igh, real *ortr, real *orti, real *hr, real *hi, real *wr, real *wi, 
	real *zr, real *zi, integer *ierr)
{
    /* System generated locals */
    integer hr_dim1, hr_offset, hi_dim1, hi_offset, zr_dim1, zr_offset, 
	    zi_dim1, zi_offset, i__1, i__2, i__3, i__4;
    real r__1, r__2, r__3, r__4;

    /* Local variables */
    static integer iend;
    extern /* Subroutine */ int cdiv_(real *, real *, real *, real *, real *, 
	    real *);
    static real unfl, ovfl, norm, opst;
    static integer i__, j, k, l, m;
    static real small;
    static integer ii, en, jj, ll, nn;
    static real si, ti, xi, yi, sr, tr;
    extern doublereal slamch_(char *);
    static real xr, yr;
    extern doublereal pythag_(real *, real *);
    extern /* Subroutine */ int csroot_(real *, real *, real *, real *);
    static integer ip1;
    static real smlnum;
    static integer lp1, itn, its;
    static real ulp, zzi, zzr;
    static integer enm1;
    static real tst1, tst2;


#define hi_ref(a_1,a_2) hi[(a_2)*hi_dim1 + a_1]
#define hr_ref(a_1,a_2) hr[(a_2)*hr_dim1 + a_1]
#define zi_ref(a_1,a_2) zi[(a_2)*zi_dim1 + a_1]
#define zr_ref(a_1,a_2) zr[(a_2)*zr_dim1 + a_1]



/*     COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED   
       OPST IS USED TO ACCUMULATE SMALL CONTRIBUTIONS TO OPS   
       TO AVOID ROUNDOFF ERROR   

       THIS SUBROUTINE IS A TRANSLATION OF A UNITARY ANALOGUE OF THE   
       ALGOL PROCEDURE  COMLR2, NUM. MATH. 16, 181-204(1970) BY PETERS   
       AND WILKINSON.   
       HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 372-395(1971).   
       THE UNITARY ANALOGUE SUBSTITUTES THE QR ALGORITHM OF FRANCIS   
       (COMP. JOUR. 4, 332-345(1962)) FOR THE LR ALGORITHM.   

       THIS SUBROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS   
       OF A COMPLEX UPPER HESSENBERG MATRIX BY THE QR   
       METHOD.  THE EIGENVECTORS OF A COMPLEX GENERAL MATRIX   
       CAN ALSO BE FOUND IF  CORTH  HAS BEEN USED TO REDUCE   
       THIS GENERAL MATRIX TO HESSENBERG FORM.   

       ON INPUT   

          NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL   
            ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM   
            DIMENSION STATEMENT.   

          N IS THE ORDER OF THE MATRIX.   

          LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING   
            SUBROUTINE  CBAL.  IF  CBAL  HAS NOT BEEN USED,   
            SET LOW=1, IGH=N.   

          ORTR AND ORTI CONTAIN INFORMATION ABOUT THE UNITARY TRANS-   
            FORMATIONS USED IN THE REDUCTION BY  CORTH, IF PERFORMED.   
            ONLY ELEMENTS LOW THROUGH IGH ARE USED.  IF THE EIGENVECTORS   
            OF THE HESSENBERG MATRIX ARE DESIRED, SET ORTR(J) AND   
            ORTI(J) TO 0.0E0 FOR THESE ELEMENTS.   

          HR AND HI CONTAIN THE REAL AND IMAGINARY PARTS,   
            RESPECTIVELY, OF THE COMPLEX UPPER HESSENBERG MATRIX.   
            THEIR LOWER TRIANGLES BELOW THE SUBDIAGONAL CONTAIN FURTHER   
            INFORMATION ABOUT THE TRANSFORMATIONS WHICH WERE USED IN THE   
            REDUCTION BY  CORTH, IF PERFORMED.  IF THE EIGENVECTORS OF   
            THE HESSENBERG MATRIX ARE DESIRED, THESE ELEMENTS MAY BE   
            ARBITRARY.   

       ON OUTPUT   

          ORTR, ORTI, AND THE UPPER HESSENBERG PORTIONS OF HR AND HI   
            HAVE BEEN DESTROYED.   

          WR AND WI CONTAIN THE REAL AND IMAGINARY PARTS,   
            RESPECTIVELY, OF THE EIGENVALUES.  IF AN ERROR   
            EXIT IS MADE, THE EIGENVALUES SHOULD BE CORRECT   
            FOR INDICES IERR+1,...,N.   

          ZR AND ZI CONTAIN THE REAL AND IMAGINARY PARTS,   
            RESPECTIVELY, OF THE EIGENVECTORS.  THE EIGENVECTORS   
            ARE UNNORMALIZED.  IF AN ERROR EXIT IS MADE, NONE OF   
            THE EIGENVECTORS HAS BEEN FOUND.   

          IERR IS SET TO   
            ZERO       FOR NORMAL RETURN,   
            J          IF THE LIMIT OF 30*N ITERATIONS IS EXHAUSTED   
                       WHILE THE J-TH EIGENVALUE IS BEING SOUGHT.   

       CALLS CDIV FOR COMPLEX DIVISION.   
       CALLS CSROOT FOR COMPLEX SQUARE ROOT.   
       CALLS PYTHAG FOR  SQRT(A*A + B*B) .   

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,   
       MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY   

       THIS VERSION DATED AUGUST 1983.   

       THE ORIGINAL DO STATEMENTS   

           DO 840 I = 1, ENM1   
           DO 820 J = IP1, N   
           DO 880 JJ = LOW, ENM1   

       HAVE BEEN CHANGED TO   

           DO 840 I = 1, N   
           DO 820 J = I, N   
           DO 880 JJ = LOW, N   

       ACCORDING TO BURT GARBOW'S SUGGESTION ON NA-NET.   
       ZHAOJUN BAI, NOV.28, 1989   
       ------------------------------------------------------------------   



       Parameter adjustments */
    zi_dim1 = *nm;
    zi_offset = 1 + zi_dim1 * 1;
    zi -= zi_offset;
    zr_dim1 = *nm;
    zr_offset = 1 + zr_dim1 * 1;
    zr -= zr_offset;
    --wi;
    --wr;
    hi_dim1 = *nm;
    hi_offset = 1 + hi_dim1 * 1;
    hi -= hi_offset;
    hr_dim1 = *nm;
    hr_offset = 1 + hr_dim1 * 1;
    hr -= hr_offset;
    --orti;
    --ortr;

    /* Function Body */
    if (*n <= 0) {
	return 0;
    }

/*     COMPUTE THE 1-NORM OF MATRIX H */

    norm = 0.f;
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	sr = 0.f;
/* Computing MIN */
	i__3 = *n, i__4 = j + 1;
	i__2 = min(i__3,i__4);
	for (i__ = 1; i__ <= i__2; ++i__) {
	    sr += pythag_(&hr_ref(i__, j), &hi_ref(i__, j));
/* L4: */
	}
	norm = dmax(norm,sr);
/* L5: */
    }

/*     GET SMALL FOR NEW CONVERGENCE CRITERION AS IN LAPACK */

    unfl = slamch_("SAFE MINIMUM");
    ovfl = slamch_("OVERFLOW");
    ulp = slamch_("EPSILON") * slamch_("BASE");
/* Computing MAX */
    r__1 = unfl * (*n / ulp), r__2 = *n / (ulp * ovfl);
    smlnum = dmax(r__1,r__2);
/* Computing MAX */
    r__1 = smlnum, r__2 = ulp * norm;
    small = dmax(r__1,r__2);


/*     INITIALIZE */
    latime_1.itcnt = 0.f;
    opst = 0.f;
    *ierr = 0;
/*     .......... INITIALIZE EIGENVECTOR MATRIX .......... */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {

	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    zr_ref(i__, j) = 0.f;
	    zi_ref(i__, j) = 0.f;
/* L100: */
	}
	zr_ref(j, j) = 1.f;
/* L101: */
    }
/*     .......... FORM THE MATRIX OF ACCUMULATED TRANSFORMATIONS   
                  FROM THE INFORMATION LEFT BY CORTH .......... */
    iend = *igh - *low - 1;
    if (iend < 0) {
	goto L180;
    } else if (iend == 0) {
	goto L150;
    } else {
	goto L105;
    }
/*     .......... FOR I=IGH-1 STEP -1 UNTIL LOW+1 DO -- .......... */
L105:
    i__1 = iend;
    for (ii = 1; ii <= i__1; ++ii) {
	i__ = *igh - ii;
	if (ortr[i__] == 0.f && orti[i__] == 0.f) {
	    goto L140;
	}
	if (hr_ref(i__, i__ - 1) == 0.f && hi_ref(i__, i__ - 1) == 0.f) {
	    goto L140;
	}
/*     .......... NORM BELOW IS NEGATIVE OF H FORMED IN CORTH .......... */
	norm = hr_ref(i__, i__ - 1) * ortr[i__] + hi_ref(i__, i__ - 1) * orti[
		i__];
	ip1 = i__ + 1;

	i__2 = *igh;
	for (k = ip1; k <= i__2; ++k) {
	    ortr[k] = hr_ref(k, i__ - 1);
	    orti[k] = hi_ref(k, i__ - 1);
/* L110: */
	}


/*        INCREMENT OP COUNT FOR LOOP 130 */
	latime_1.ops += ((*igh - i__ + 1 << 4) + 2) * (*igh - i__ + 1);
	i__2 = *igh;
	for (j = i__; j <= i__2; ++j) {
	    sr = 0.f;
	    si = 0.f;

	    i__3 = *igh;
	    for (k = i__; k <= i__3; ++k) {
		sr = sr + ortr[k] * zr_ref(k, j) + orti[k] * zi_ref(k, j);
		si = si + ortr[k] * zi_ref(k, j) - orti[k] * zr_ref(k, j);
/* L115: */
	    }

	    sr /= norm;
	    si /= norm;

	    i__3 = *igh;
	    for (k = i__; k <= i__3; ++k) {
		zr_ref(k, j) = zr_ref(k, j) + sr * ortr[k] - si * orti[k];
		zi_ref(k, j) = zi_ref(k, j) + sr * orti[k] + si * ortr[k];
/* L120: */
	    }

/* L130: */
	}

L140:
	;
    }

/*        INCREMENT OP COUNT FOR COMPUTING NORM IN LOOP 140 */
    latime_1.ops += iend * 3;
/*     .......... CREATE REAL SUBDIAGONAL ELEMENTS .......... */
L150:
    l = *low + 1;


/*        INCREMENT OP COUNT FOR LOOP 170 */
    latime_1.ops += ((*igh - *low + 1) * 12 + 42) * (*igh - l + 1);
    i__1 = *igh;
    for (i__ = l; i__ <= i__1; ++i__) {
/* Computing MIN */
	i__2 = i__ + 1;
	ll = min(i__2,*igh);
	if (hi_ref(i__, i__ - 1) == 0.f) {
	    goto L170;
	}
	norm = pythag_(&hr_ref(i__, i__ - 1), &hi_ref(i__, i__ - 1));
	yr = hr_ref(i__, i__ - 1) / norm;
	yi = hi_ref(i__, i__ - 1) / norm;
	hr_ref(i__, i__ - 1) = norm;
	hi_ref(i__, i__ - 1) = 0.f;

	i__2 = *n;
	for (j = i__; j <= i__2; ++j) {
	    si = yr * hi_ref(i__, j) - yi * hr_ref(i__, j);
	    hr_ref(i__, j) = yr * hr_ref(i__, j) + yi * hi_ref(i__, j);
	    hi_ref(i__, j) = si;
/* L155: */
	}

	i__2 = ll;
	for (j = 1; j <= i__2; ++j) {
	    si = yr * hi_ref(j, i__) + yi * hr_ref(j, i__);
	    hr_ref(j, i__) = yr * hr_ref(j, i__) - yi * hi_ref(j, i__);
	    hi_ref(j, i__) = si;
/* L160: */
	}

	i__2 = *igh;
	for (j = *low; j <= i__2; ++j) {
	    si = yr * zi_ref(j, i__) + yi * zr_ref(j, i__);
	    zr_ref(j, i__) = yr * zr_ref(j, i__) - yi * zi_ref(j, i__);
	    zi_ref(j, i__) = si;
/* L165: */
	}

L170:
	;
    }
/*     .......... STORE ROOTS ISOLATED BY CBAL .......... */
L180:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (i__ >= *low && i__ <= *igh) {
	    goto L200;
	}
	wr[i__] = hr_ref(i__, i__);
	wi[i__] = hi_ref(i__, i__);
L200:
	;
    }

    en = *igh;
    tr = 0.f;
    ti = 0.f;
    itn = *n * 30;
/*     .......... SEARCH FOR NEXT EIGENVALUE .......... */
L220:
    if (en < *low) {
	goto L680;
    }
    its = 0;
    enm1 = en - 1;
/*     .......... LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT   
                  FOR L=EN STEP -1 UNTIL LOW DO -- .......... */
L240:
    i__1 = en;
    for (ll = *low; ll <= i__1; ++ll) {
	l = en + *low - ll;
	if (l == *low) {
	    goto L300;
	}
	tst1 = (r__1 = hr_ref(l - 1, l - 1), dabs(r__1)) + (r__2 = hi_ref(l - 
		1, l - 1), dabs(r__2)) + (r__3 = hr_ref(l, l), dabs(r__3)) + (
		r__4 = hi_ref(l, l), dabs(r__4));
/*         TST2 = TST1 + ABS(HR(L,L-1))   
           IF (TST2 .EQ. TST1) GO TO 300 */
	tst2 = (r__1 = hr_ref(l, l - 1), dabs(r__1));
/* Computing MIN */
	r__1 = ulp * tst1;
	if (tst2 <= dmin(r__1,small)) {
	    goto L300;
	}
/* L260: */
    }
/*     .......... FORM SHIFT .......... */
L300:

/*        INCREMENT OP COUNT FOR CONVERGENCE TEST */
    latime_1.ops += en - l + 1 << 2;
    if (l == en) {
	goto L660;
    }
    if (itn == 0) {
	goto L1000;
    }
    if (its == 10 || its == 20) {
	goto L320;
    }

/*        INCREMENT OPCOUNT FOR FOMING SHIFT */
    opst += 58;
    sr = hr_ref(en, en);
    si = hi_ref(en, en);
    xr = hr_ref(enm1, en) * hr_ref(en, enm1);
    xi = hi_ref(enm1, en) * hr_ref(en, enm1);
    if (xr == 0.f && xi == 0.f) {
	goto L340;
    }
    yr = (hr_ref(enm1, enm1) - sr) / 2.f;
    yi = (hi_ref(enm1, enm1) - si) / 2.f;
/* Computing 2nd power */
    r__2 = yr;
/* Computing 2nd power */
    r__3 = yi;
    r__1 = r__2 * r__2 - r__3 * r__3 + xr;
    r__4 = yr * 2.f * yi + xi;
    csroot_(&r__1, &r__4, &zzr, &zzi);
    if (yr * zzr + yi * zzi >= 0.f) {
	goto L310;
    }
    zzr = -zzr;
    zzi = -zzi;
L310:
    r__1 = yr + zzr;
    r__2 = yi + zzi;
    cdiv_(&xr, &xi, &r__1, &r__2, &xr, &xi);
    sr -= xr;
    si -= xi;
    goto L340;
/*     .......... FORM EXCEPTIONAL SHIFT .......... */
L320:
    sr = (r__1 = hr_ref(en, enm1), dabs(r__1)) + (r__2 = hr_ref(enm1, en - 2),
	     dabs(r__2));
    si = 0.f;

L340:
    i__1 = en;
    for (i__ = *low; i__ <= i__1; ++i__) {
	hr_ref(i__, i__) = hr_ref(i__, i__) - sr;
	hi_ref(i__, i__) = hi_ref(i__, i__) - si;
/* L360: */
    }

/*        INCREMENT OPCOUNT FOR LOOP 360 */
    latime_1.ops += en - *low + 1 << 1;

    tr += sr;
    ti += si;
    ++its;
    --itn;

/*       UPDATE ITERATION NUMBER */
    latime_1.itcnt = (real) (*n * 30 - itn);
/*     .......... REDUCE TO TRIANGLE (ROWS) .......... */
    lp1 = l + 1;


/*        INCREMENT OPCOUNT FOR REDUCING TO TRIANGULAR, LOOP 500 */
    latime_1.ops += (en - lp1 + 1) * ((en - lp1) * 10 + 61);
    i__1 = en;
    for (i__ = lp1; i__ <= i__1; ++i__) {
	sr = hr_ref(i__, i__ - 1);
	hr_ref(i__, i__ - 1) = 0.f;
	r__1 = pythag_(&hr_ref(i__ - 1, i__ - 1), &hi_ref(i__ - 1, i__ - 1));
	norm = pythag_(&r__1, &sr);
	xr = hr_ref(i__ - 1, i__ - 1) / norm;
	wr[i__ - 1] = xr;
	xi = hi_ref(i__ - 1, i__ - 1) / norm;
	wi[i__ - 1] = xi;
	hr_ref(i__ - 1, i__ - 1) = norm;
	hi_ref(i__ - 1, i__ - 1) = 0.f;
	hi_ref(i__, i__ - 1) = sr / norm;

	i__2 = *n;
	for (j = i__; j <= i__2; ++j) {
	    yr = hr_ref(i__ - 1, j);
	    yi = hi_ref(i__ - 1, j);
	    zzr = hr_ref(i__, j);
	    zzi = hi_ref(i__, j);
	    hr_ref(i__ - 1, j) = xr * yr + xi * yi + hi_ref(i__, i__ - 1) * 
		    zzr;
	    hi_ref(i__ - 1, j) = xr * yi - xi * yr + hi_ref(i__, i__ - 1) * 
		    zzi;
	    hr_ref(i__, j) = xr * zzr - xi * zzi - hi_ref(i__, i__ - 1) * yr;
	    hi_ref(i__, j) = xr * zzi + xi * zzr - hi_ref(i__, i__ - 1) * yi;
/* L490: */
	}

/* L500: */
    }

    si = hi_ref(en, en);
    if (si == 0.f) {
	goto L540;
    }
    norm = pythag_(&hr_ref(en, en), &si);
    sr = hr_ref(en, en) / norm;
    si /= norm;
    hr_ref(en, en) = norm;
    hi_ref(en, en) = 0.f;

/*        INCREMENT OP COUNT */
    opst += 20;
    if (en == *n) {
	goto L540;
    }
    ip1 = en + 1;


/*        INCREMENT OP COUNT FOR LOOP 520 */
    opst += (*n - ip1 + 1) * 6;
    i__1 = *n;
    for (j = ip1; j <= i__1; ++j) {
	yr = hr_ref(en, j);
	yi = hi_ref(en, j);
	hr_ref(en, j) = sr * yr + si * yi;
	hi_ref(en, j) = sr * yi - si * yr;
/* L520: */
    }
/*     .......... INVERSE OPERATION (COLUMNS) .......... */
L540:
    i__1 = en;
    for (j = lp1; j <= i__1; ++j) {
	xr = wr[j - 1];
	xi = wi[j - 1];

	i__2 = j;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    yr = hr_ref(i__, j - 1);
	    yi = 0.f;
	    zzr = hr_ref(i__, j);
	    zzi = hi_ref(i__, j);
	    if (i__ == j) {
		goto L560;
	    }
	    yi = hi_ref(i__, j - 1);
	    hi_ref(i__, j - 1) = xr * yi + xi * yr + hi_ref(j, j - 1) * zzi;
L560:
	    hr_ref(i__, j - 1) = xr * yr - xi * yi + hi_ref(j, j - 1) * zzr;
	    hr_ref(i__, j) = xr * zzr + xi * zzi - hi_ref(j, j - 1) * yr;
	    hi_ref(i__, j) = xr * zzi - xi * zzr - hi_ref(j, j - 1) * yi;
/* L580: */
	}

	i__2 = *igh;
	for (i__ = *low; i__ <= i__2; ++i__) {
	    yr = zr_ref(i__, j - 1);
	    yi = zi_ref(i__, j - 1);
	    zzr = zr_ref(i__, j);
	    zzi = zi_ref(i__, j);
	    zr_ref(i__, j - 1) = xr * yr - xi * yi + hi_ref(j, j - 1) * zzr;
	    zi_ref(i__, j - 1) = xr * yi + xi * yr + hi_ref(j, j - 1) * zzi;
	    zr_ref(i__, j) = xr * zzr + xi * zzi - hi_ref(j, j - 1) * yr;
	    zi_ref(i__, j) = xr * zzi - xi * zzr - hi_ref(j, j - 1) * yi;
/* L590: */
	}

/* L600: */
    }

/*        INCREMENT OPCOUNT FOR INVERSE OPERATION LOOP 600 */
    latime_1.ops += ((en + lp1) * 10 + (*igh - *low + 1) * 20) * (en - lp1 + 
	    1);

    if (si == 0.f) {
	goto L240;
    }


/*        INCREMENT OPCOUNT FOR LOOP 630 AND 640 */
    latime_1.ops = latime_1.ops + en * 6 + (*igh - *low + 1) * 6;
    i__1 = en;
    for (i__ = 1; i__ <= i__1; ++i__) {
	yr = hr_ref(i__, en);
	yi = hi_ref(i__, en);
	hr_ref(i__, en) = sr * yr - si * yi;
	hi_ref(i__, en) = sr * yi + si * yr;
/* L630: */
    }

    i__1 = *igh;
    for (i__ = *low; i__ <= i__1; ++i__) {
	yr = zr_ref(i__, en);
	yi = zi_ref(i__, en);
	zr_ref(i__, en) = sr * yr - si * yi;
	zi_ref(i__, en) = sr * yi + si * yr;
/* L640: */
    }

    goto L240;
/*     .......... A ROOT FOUND .......... */
L660:
    hr_ref(en, en) = hr_ref(en, en) + tr;
    wr[en] = hr_ref(en, en);
    hi_ref(en, en) = hi_ref(en, en) + ti;
    wi[en] = hi_ref(en, en);
    en = enm1;
    goto L220;
/*     .......... ALL ROOTS FOUND.  BACKSUBSTITUTE TO FIND   
                  VECTORS OF UPPER TRIANGULAR FORM .......... */
L680:
    norm = 0.f;


/*        INCREMENT OP COUNT FOR LOOP 720 */
    latime_1.ops += *n * (*n + 1) / 2;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {

	i__2 = *n;
	for (j = i__; j <= i__2; ++j) {
	    tr = (r__1 = hr_ref(i__, j), dabs(r__1)) + (r__2 = hi_ref(i__, j),
		     dabs(r__2));
	    if (tr > norm) {
		norm = tr;
	    }
/* L720: */
	}
    }

    if (*n == 1 || norm == 0.f) {
	goto L1001;
    }
/*     .......... FOR EN=N STEP -1 UNTIL 2 DO -- .......... */
    i__2 = *n;
    for (nn = 2; nn <= i__2; ++nn) {
	en = *n + 2 - nn;
	xr = wr[en];
	xi = wi[en];
	hr_ref(en, en) = 1.f;
	hi_ref(en, en) = 0.f;
	enm1 = en - 1;
/*     .......... FOR I=EN-1 STEP -1 UNTIL 1 DO -- ..........   

          INCREMENT OP COUNT FOR COMPUT YR, .. IN LOOP 780 */
	latime_1.ops += enm1 * 22;
	i__1 = enm1;
	for (ii = 1; ii <= i__1; ++ii) {
	    i__ = en - ii;
	    zzr = 0.f;
	    zzi = 0.f;
	    ip1 = i__ + 1;


/*        INCREMENT OP COUNT FOR LOOP 740 */
	    latime_1.ops += (en - ip1 + 1) * 7;
	    i__3 = en;
	    for (j = ip1; j <= i__3; ++j) {
		zzr = zzr + hr_ref(i__, j) * hr_ref(j, en) - hi_ref(i__, j) * 
			hi_ref(j, en);
		zzi = zzi + hr_ref(i__, j) * hi_ref(j, en) + hi_ref(i__, j) * 
			hr_ref(j, en);
/* L740: */
	    }

	    yr = xr - wr[i__];
	    yi = xi - wi[i__];
	    if (yr != 0.f || yi != 0.f) {
		goto L765;
	    }
	    tst1 = norm;
	    yr = tst1;
L760:
	    yr *= .01f;
	    tst2 = norm + yr;
	    if (tst2 > tst1) {
		goto L760;
	    }
L765:
	    cdiv_(&zzr, &zzi, &yr, &yi, &hr_ref(i__, en), &hi_ref(i__, en));

/*        INCREMENT OP COUNT FOR CDIV */
	    opst += 16;
/*     .......... OVERFLOW CONTROL .......... */
	    tr = (r__1 = hr_ref(i__, en), dabs(r__1)) + (r__2 = hi_ref(i__, 
		    en), dabs(r__2));
	    if (tr == 0.f) {
		goto L780;
	    }
	    tst1 = tr;
	    tst2 = tst1 + 1.f / tst1;
	    if (tst2 > tst1) {
		goto L780;
	    }

/*        INCREMENT OP COUNT FOR LOOP 770 */
	    latime_1.ops += en - i__ + 1 << 1;
	    i__3 = en;
	    for (j = i__; j <= i__3; ++j) {
		hr_ref(j, en) = hr_ref(j, en) / tr;
		hi_ref(j, en) = hi_ref(j, en) / tr;
/* L770: */
	    }

L780:
	    ;
	}

/* L800: */
    }
/*     .......... END BACKSUBSTITUTION .......... */
    enm1 = *n - 1;
/*     .......... VECTORS OF ISOLATED ROOTS .......... */
    i__2 = *n;
    for (i__ = 1; i__ <= i__2; ++i__) {
	if (i__ >= *low && i__ <= *igh) {
	    goto L840;
	}
	ip1 = i__ + 1;

	i__1 = *n;
	for (j = i__; j <= i__1; ++j) {
	    zr_ref(i__, j) = hr_ref(i__, j);
	    zi_ref(i__, j) = hi_ref(i__, j);
/* L820: */
	}

L840:
	;
    }
/*     .......... MULTIPLY BY TRANSFORMATION MATRIX TO GIVE   
                  VECTORS OF ORIGINAL FULL MATRIX.   
                  FOR J=N STEP -1 UNTIL LOW+1 DO -- .......... */
    i__2 = *n;
    for (jj = *low; jj <= i__2; ++jj) {
	j = *n + *low - jj;
	m = min(j,*igh);


/*        INCREMENT OP COUNT FOR LOOP 880 */
	latime_1.ops += (m - *low + 1 << 3) * (*igh - *low + 1);
	i__1 = *igh;
	for (i__ = *low; i__ <= i__1; ++i__) {
	    zzr = 0.f;
	    zzi = 0.f;

	    i__3 = m;
	    for (k = *low; k <= i__3; ++k) {
		zzr = zzr + zr_ref(i__, k) * hr_ref(k, j) - zi_ref(i__, k) * 
			hi_ref(k, j);
		zzi = zzi + zr_ref(i__, k) * hi_ref(k, j) + zi_ref(i__, k) * 
			hr_ref(k, j);
/* L860: */
	    }

	    zr_ref(i__, j) = zzr;
	    zi_ref(i__, j) = zzi;
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
} /* comqr2_ */

#undef zr_ref
#undef zi_ref
#undef hr_ref
#undef hi_ref


/* Subroutine */ int corth_(integer *nm, integer *n, integer *low, integer *
	igh, real *ar, real *ai, real *ortr, real *orti)
{
    /* System generated locals */
    integer ar_dim1, ar_offset, ai_dim1, ai_offset, i__1, i__2, i__3;
    real r__1, r__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static real f, g, h__;
    static integer i__, j, m;
    static real scale;
    static integer la;
    static real fi;
    static integer ii, jj;
    static real fr;
    static integer mp;
    extern doublereal pythag_(real *, real *);
    static integer kp1;


#define ai_ref(a_1,a_2) ai[(a_2)*ai_dim1 + a_1]
#define ar_ref(a_1,a_2) ar[(a_2)*ar_dim1 + a_1]



/*     COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED   
       OPST IS USED TO ACCUMULATE SMALL CONTRIBUTIONS TO OPS   
       TO AVOID ROUNDOFF ERROR   

       THIS SUBROUTINE IS A TRANSLATION OF A COMPLEX ANALOGUE OF   
       THE ALGOL PROCEDURE ORTHES, NUM. MATH. 12, 349-368(1968)   
       BY MARTIN AND WILKINSON.   
       HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 339-358(1971).   

       GIVEN A COMPLEX GENERAL MATRIX, THIS SUBROUTINE   
       REDUCES A SUBMATRIX SITUATED IN ROWS AND COLUMNS   
       LOW THROUGH IGH TO UPPER HESSENBERG FORM BY   
       UNITARY SIMILARITY TRANSFORMATIONS.   

       ON INPUT   

          NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL   
            ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM   
            DIMENSION STATEMENT.   

          N IS THE ORDER OF THE MATRIX.   

          LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING   
            SUBROUTINE  CBAL.  IF  CBAL  HAS NOT BEEN USED,   
            SET LOW=1, IGH=N.   

          AR AND AI CONTAIN THE REAL AND IMAGINARY PARTS,   
            RESPECTIVELY, OF THE COMPLEX INPUT MATRIX.   

       ON OUTPUT   

          AR AND AI CONTAIN THE REAL AND IMAGINARY PARTS,   
            RESPECTIVELY, OF THE HESSENBERG MATRIX.  INFORMATION   
            ABOUT THE UNITARY TRANSFORMATIONS USED IN THE REDUCTION   
            IS STORED IN THE REMAINING TRIANGLES UNDER THE   
            HESSENBERG MATRIX.   

          ORTR AND ORTI CONTAIN FURTHER INFORMATION ABOUT THE   
            TRANSFORMATIONS.  ONLY ELEMENTS LOW THROUGH IGH ARE USED.   

       CALLS PYTHAG FOR  SQRT(A*A + B*B) .   

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,   
       MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY   

       THIS VERSION DATED AUGUST 1983.   

       ------------------------------------------------------------------   

       Parameter adjustments */
    ai_dim1 = *nm;
    ai_offset = 1 + ai_dim1 * 1;
    ai -= ai_offset;
    ar_dim1 = *nm;
    ar_offset = 1 + ar_dim1 * 1;
    ar -= ar_offset;
    --orti;
    --ortr;

    /* Function Body */
    if (*n <= 0) {
	return 0;
    }
/* **   
       INITIALIZE */
    pythop_1.opst = 0.f;
/* ** */
    la = *igh - 1;
    kp1 = *low + 1;
    if (la < kp1) {
	goto L200;
    }

    i__1 = la;
    for (m = kp1; m <= i__1; ++m) {
	h__ = 0.f;
	ortr[m] = 0.f;
	orti[m] = 0.f;
	scale = 0.f;
/*     .......... SCALE COLUMN (ALGOL TOL THEN NOT NEEDED) .......... */
	i__2 = *igh;
	for (i__ = m; i__ <= i__2; ++i__) {
/* L90: */
	    scale = scale + (r__1 = ar_ref(i__, m - 1), dabs(r__1)) + (r__2 = 
		    ai_ref(i__, m - 1), dabs(r__2));
	}
/* **   
          INCREMENT OPCOUNT FOR LOOP 90 */
	latime_1.ops += *igh - m + 1 << 1;
/* ** */

	if (scale == 0.f) {
	    goto L180;
	}
	mp = m + *igh;
/*     .......... FOR I=IGH STEP -1 UNTIL M DO -- .......... */
	i__2 = *igh;
	for (ii = m; ii <= i__2; ++ii) {
	    i__ = mp - ii;
	    ortr[i__] = ar_ref(i__, m - 1) / scale;
	    orti[i__] = ai_ref(i__, m - 1) / scale;
	    h__ = h__ + ortr[i__] * ortr[i__] + orti[i__] * orti[i__];
/* L100: */
	}
/* **   
          INCREMENT OP COUNT FOR LOOP 100 AND SQRT */
	latime_1.ops = latime_1.ops + (*igh - m + 1) * 6 + 1;
/* ** */

	g = sqrt(h__);
	f = pythag_(&ortr[m], &orti[m]);
	if (f == 0.f) {
	    goto L103;
	}
	h__ += f * g;
	g /= f;
	ortr[m] = (g + 1.f) * ortr[m];
	orti[m] = (g + 1.f) * orti[m];
	pythop_1.opst += 7;
	goto L105;

L103:
	ortr[m] = g;
	ar_ref(m, m - 1) = scale;
/*     .......... FORM (I-(U*UT)/H) * A .......... */
L105:
	i__2 = *n;
	for (j = m; j <= i__2; ++j) {
	    fr = 0.f;
	    fi = 0.f;
/*     .......... FOR I=IGH STEP -1 UNTIL M DO -- .......... */
	    i__3 = *igh;
	    for (ii = m; ii <= i__3; ++ii) {
		i__ = mp - ii;
		fr = fr + ortr[i__] * ar_ref(i__, j) + orti[i__] * ai_ref(i__,
			 j);
		fi = fi + ortr[i__] * ai_ref(i__, j) - orti[i__] * ar_ref(i__,
			 j);
/* L110: */
	    }

	    fr /= h__;
	    fi /= h__;

	    i__3 = *igh;
	    for (i__ = m; i__ <= i__3; ++i__) {
		ar_ref(i__, j) = ar_ref(i__, j) - fr * ortr[i__] + fi * orti[
			i__];
		ai_ref(i__, j) = ai_ref(i__, j) - fr * orti[i__] - fi * ortr[
			i__];
/* L120: */
	    }

/* L130: */
	}
/*     .......... FORM (I-(U*UT)/H)*A*(I-(U*UT)/H) .......... */
	i__2 = *igh;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    fr = 0.f;
	    fi = 0.f;
/*     .......... FOR J=IGH STEP -1 UNTIL M DO -- .......... */
	    i__3 = *igh;
	    for (jj = m; jj <= i__3; ++jj) {
		j = mp - jj;
		fr = fr + ortr[j] * ar_ref(i__, j) - orti[j] * ai_ref(i__, j);
		fi = fi + ortr[j] * ai_ref(i__, j) + orti[j] * ar_ref(i__, j);
/* L140: */
	    }

	    fr /= h__;
	    fi /= h__;

	    i__3 = *igh;
	    for (j = m; j <= i__3; ++j) {
		ar_ref(i__, j) = ar_ref(i__, j) - fr * ortr[j] - fi * orti[j];
		ai_ref(i__, j) = ai_ref(i__, j) + fr * orti[j] - fi * ortr[j];
/* L150: */
	    }

/* L160: */
	}
/* **   
          INCREMENT OP COUNT FOR LOOPS 130 AND 160 */
	latime_1.ops += (*igh + *n - m + 1) * ((*igh - m + 1 << 4) + 2);
	pythop_1.opst += 4;
/* ** */

	ortr[m] = scale * ortr[m];
	orti[m] = scale * orti[m];
	ar_ref(m, m - 1) = -g * ar_ref(m, m - 1);
	ai_ref(m, m - 1) = -g * ai_ref(m, m - 1);
L180:
	;
    }
    latime_1.ops += pythop_1.opst;

L200:
    return 0;
} /* corth_ */

#undef ar_ref
#undef ai_ref


/* Subroutine */ int csroot_(real *xr, real *xi, real *yr, real *yi)
{
    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static real s, ti, tr;
    extern doublereal pythag_(real *, real *);


/*     (YR,YI) = COMPLEX SQRT(XR,XI)   
       BRANCH CHOSEN SO THAT YR .GE. 0.0 AND SIGN(YI) .EQ. SIGN(XI) */

    tr = *xr;
    ti = *xi;
    s = sqrt((pythag_(&tr, &ti) + dabs(tr)) * .5f);
    if (tr >= 0.f) {
	*yr = s;
    }
    if (ti < 0.f) {
	s = -s;
    }
    if (tr <= 0.f) {
	*yi = s;
    }
    if (tr < 0.f) {
	*yr = ti / *yi * .5f;
    }
    if (tr > 0.f) {
	*yi = ti / *yr * .5f;
    }
    return 0;
} /* csroot_   

   Subroutine */ int htribk_(integer *nm, integer *n, real *ar, real *ai, 
	real *tau, integer *m, real *zr, real *zi)
{
    /* System generated locals */
    integer ar_dim1, ar_offset, ai_dim1, ai_offset, zr_dim1, zr_offset, 
	    zi_dim1, zi_offset, i__1, i__2, i__3;
    real r__1, r__2, r__3;

    /* Local variables */
    static real h__;
    static integer i__, j, k, l;
    static real s, si;


#define ai_ref(a_1,a_2) ai[(a_2)*ai_dim1 + a_1]
#define ar_ref(a_1,a_2) ar[(a_2)*ar_dim1 + a_1]
#define zi_ref(a_1,a_2) zi[(a_2)*zi_dim1 + a_1]
#define zr_ref(a_1,a_2) zr[(a_2)*zr_dim1 + a_1]
#define tau_ref(a_1,a_2) tau[(a_2)*2 + a_1]



/*     COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT.   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED.   

       THIS SUBROUTINE IS A TRANSLATION OF A COMPLEX ANALOGUE OF   
       THE ALGOL PROCEDURE TRBAK1, NUM. MATH. 11, 181-195(1968)   
       BY MARTIN, REINSCH, AND WILKINSON.   
       HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).   

       THIS SUBROUTINE FORMS THE EIGENVECTORS OF A COMPLEX HERMITIAN   
       MATRIX BY BACK TRANSFORMING THOSE OF THE CORRESPONDING   
       REAL SYMMETRIC TRIDIAGONAL MATRIX DETERMINED BY  HTRIDI.   

       ON INPUT   

          NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL   
            ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM   
            DIMENSION STATEMENT.   

          N IS THE ORDER OF THE MATRIX.   

          AR AND AI CONTAIN INFORMATION ABOUT THE UNITARY TRANS-   
            FORMATIONS USED IN THE REDUCTION BY  HTRIDI  IN THEIR   
            FULL LOWER TRIANGLES EXCEPT FOR THE DIAGONAL OF AR.   

          TAU CONTAINS FURTHER INFORMATION ABOUT THE TRANSFORMATIONS.   

          M IS THE NUMBER OF EIGENVECTORS TO BE BACK TRANSFORMED.   

          ZR CONTAINS THE EIGENVECTORS TO BE BACK TRANSFORMED   
            IN ITS FIRST M COLUMNS.   

       ON OUTPUT   

          ZR AND ZI CONTAIN THE REAL AND IMAGINARY PARTS,   
            RESPECTIVELY, OF THE TRANSFORMED EIGENVECTORS   
            IN THEIR FIRST M COLUMNS.   

       NOTE THAT THE LAST COMPONENT OF EACH RETURNED VECTOR   
       IS REAL AND THAT VECTOR EUCLIDEAN NORMS ARE PRESERVED.   

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,   
       MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY   

       THIS VERSION DATED AUGUST 1983.   

       ------------------------------------------------------------------   

       Parameter adjustments */
    tau -= 3;
    ai_dim1 = *nm;
    ai_offset = 1 + ai_dim1 * 1;
    ai -= ai_offset;
    ar_dim1 = *nm;
    ar_offset = 1 + ar_dim1 * 1;
    ar -= ar_offset;
    zi_dim1 = *nm;
    zi_offset = 1 + zi_dim1 * 1;
    zi -= zi_offset;
    zr_dim1 = *nm;
    zr_offset = 1 + zr_dim1 * 1;
    zr -= zr_offset;

    /* Function Body */
    if (*m == 0) {
	goto L200;
    }

/* Computing MAX   
   Computing 2nd power */
    r__3 = (real) (*n);
    r__1 = 0.f, r__2 = (*m << 3) * (r__3 * r__3) - (*m << 1) * (real) (*n) - (
	    *m << 2);
    latime_1.ops += dmax(r__1,r__2);

/*     .......... TRANSFORM THE EIGENVECTORS OF THE REAL SYMMETRIC   
                  TRIDIAGONAL MATRIX TO THOSE OF THE HERMITIAN   
                  TRIDIAGONAL MATRIX. .......... */
    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {

	i__2 = *m;
	for (j = 1; j <= i__2; ++j) {
	    zi_ref(k, j) = -zr_ref(k, j) * tau_ref(2, k);
	    zr_ref(k, j) = zr_ref(k, j) * tau_ref(1, k);
/* L50: */
	}
    }

    if (*n == 1) {
	goto L200;
    }
/*     .......... RECOVER AND APPLY THE HOUSEHOLDER MATRICES .......... */
    i__2 = *n;
    for (i__ = 2; i__ <= i__2; ++i__) {
	l = i__ - 1;
	h__ = ai_ref(i__, i__);
	if (h__ == 0.f) {
	    goto L140;
	}

	i__1 = *m;
	for (j = 1; j <= i__1; ++j) {
	    s = 0.f;
	    si = 0.f;

	    i__3 = l;
	    for (k = 1; k <= i__3; ++k) {
		s = s + ar_ref(i__, k) * zr_ref(k, j) - ai_ref(i__, k) * 
			zi_ref(k, j);
		si = si + ar_ref(i__, k) * zi_ref(k, j) + ai_ref(i__, k) * 
			zr_ref(k, j);
/* L110: */
	    }
/*     .......... DOUBLE DIVISIONS AVOID POSSIBLE UNDERFLOW .......... */
	    s = s / h__ / h__;
	    si = si / h__ / h__;

	    i__3 = l;
	    for (k = 1; k <= i__3; ++k) {
		zr_ref(k, j) = zr_ref(k, j) - s * ar_ref(i__, k) - si * 
			ai_ref(i__, k);
		zi_ref(k, j) = zi_ref(k, j) - si * ar_ref(i__, k) + s * 
			ai_ref(i__, k);
/* L120: */
	    }

/* L130: */
	}

L140:
	;
    }

L200:
    return 0;
} /* htribk_ */

#undef tau_ref
#undef zr_ref
#undef zi_ref
#undef ar_ref
#undef ai_ref


/* Subroutine */ int htridi_(integer *nm, integer *n, real *ar, real *ai, 
	real *d__, real *e, real *e2, real *tau)
{
    /* System generated locals */
    integer ar_dim1, ar_offset, ai_dim1, ai_offset, i__1, i__2, i__3;
    real r__1, r__2, r__3, r__4;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static real f, g, h__;
    static integer i__, j, k, l;
    static real scale, fi, gi, hh;
    static integer ii;
    static real si;
    extern doublereal pythag_(real *, real *);
    static integer jp1;


#define ai_ref(a_1,a_2) ai[(a_2)*ai_dim1 + a_1]
#define ar_ref(a_1,a_2) ar[(a_2)*ar_dim1 + a_1]
#define tau_ref(a_1,a_2) tau[(a_2)*2 + a_1]



/*     COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT.   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED.   

       THIS SUBROUTINE IS A TRANSLATION OF A COMPLEX ANALOGUE OF   
       THE ALGOL PROCEDURE TRED1, NUM. MATH. 11, 181-195(1968)   
       BY MARTIN, REINSCH, AND WILKINSON.   
       HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).   

       THIS SUBROUTINE REDUCES A COMPLEX HERMITIAN MATRIX   
       TO A REAL SYMMETRIC TRIDIAGONAL MATRIX USING   
       UNITARY SIMILARITY TRANSFORMATIONS.   

       ON INPUT   

          NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL   
            ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM   
            DIMENSION STATEMENT.   

          N IS THE ORDER OF THE MATRIX.   

          AR AND AI CONTAIN THE REAL AND IMAGINARY PARTS,   
            RESPECTIVELY, OF THE COMPLEX HERMITIAN INPUT MATRIX.   
            ONLY THE LOWER TRIANGLE OF THE MATRIX NEED BE SUPPLIED.   

       ON OUTPUT   

          AR AND AI CONTAIN INFORMATION ABOUT THE UNITARY TRANS-   
            FORMATIONS USED IN THE REDUCTION IN THEIR FULL LOWER   
            TRIANGLES.  THEIR STRICT UPPER TRIANGLES AND THE   
            DIAGONAL OF AR ARE UNALTERED.   

          D CONTAINS THE DIAGONAL ELEMENTS OF THE THE TRIDIAGONAL MATRIX.   

          E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE TRIDIAGONAL   
            MATRIX IN ITS LAST N-1 POSITIONS.  E(1) IS SET TO ZERO.   

          E2 CONTAINS THE SQUARES OF THE CORRESPONDING ELEMENTS OF E.   
            E2 MAY COINCIDE WITH E IF THE SQUARES ARE NOT NEEDED.   

          TAU CONTAINS FURTHER INFORMATION ABOUT THE TRANSFORMATIONS.   

       CALLS PYTHAG FOR  SQRT(A*A + B*B) .   

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,   
       MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY   

       THIS VERSION DATED AUGUST 1983.   

       ------------------------------------------------------------------   


       Parameter adjustments */
    tau -= 3;
    --e2;
    --e;
    --d__;
    ai_dim1 = *nm;
    ai_offset = 1 + ai_dim1 * 1;
    ai -= ai_offset;
    ar_dim1 = *nm;
    ar_offset = 1 + ar_dim1 * 1;
    ar -= ar_offset;

    /* Function Body   
   Computing MAX   
   Computing 3rd power */
    r__3 = (real) (*n);
/* Computing 2nd power */
    r__4 = (real) (*n);
    r__1 = 0.f, r__2 = r__3 * (r__3 * r__3) * 5.333333333333333f + r__4 * 
	    r__4 * 3 + *n * 18.666666666666668f - 61;
    latime_1.ops += dmax(r__1,r__2);

    tau_ref(1, *n) = 1.f;
    tau_ref(2, *n) = 0.f;

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L100: */
	d__[i__] = ar_ref(i__, i__);
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
	    scale = scale + (r__1 = ar_ref(i__, k), dabs(r__1)) + (r__2 = 
		    ai_ref(i__, k), dabs(r__2));
	}

	if (scale != 0.f) {
	    goto L140;
	}
	tau_ref(1, l) = 1.f;
	tau_ref(2, l) = 0.f;
L130:
	e[i__] = 0.f;
	e2[i__] = 0.f;
	goto L290;

L140:
	i__2 = l;
	for (k = 1; k <= i__2; ++k) {
	    ar_ref(i__, k) = ar_ref(i__, k) / scale;
	    ai_ref(i__, k) = ai_ref(i__, k) / scale;
	    h__ = h__ + ar_ref(i__, k) * ar_ref(i__, k) + ai_ref(i__, k) * 
		    ai_ref(i__, k);
/* L150: */
	}

	e2[i__] = scale * scale * h__;
	g = sqrt(h__);
	e[i__] = scale * g;
	f = pythag_(&ar_ref(i__, l), &ai_ref(i__, l));
/*     .......... FORM NEXT DIAGONAL ELEMENT OF MATRIX T .......... */
	if (f == 0.f) {
	    goto L160;
	}
	tau_ref(1, l) = (ai_ref(i__, l) * tau_ref(2, i__) - ar_ref(i__, l) * 
		tau_ref(1, i__)) / f;
	si = (ar_ref(i__, l) * tau_ref(2, i__) + ai_ref(i__, l) * tau_ref(1, 
		i__)) / f;
	h__ += f * g;
	g = g / f + 1.f;
	ar_ref(i__, l) = g * ar_ref(i__, l);
	ai_ref(i__, l) = g * ai_ref(i__, l);
	if (l == 1) {
	    goto L270;
	}
	goto L170;
L160:
	tau_ref(1, l) = -tau_ref(1, i__);
	si = tau_ref(2, i__);
	ar_ref(i__, l) = g;
L170:
	f = 0.f;

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    g = 0.f;
	    gi = 0.f;
/*     .......... FORM ELEMENT OF A*U .......... */
	    i__3 = j;
	    for (k = 1; k <= i__3; ++k) {
		g = g + ar_ref(j, k) * ar_ref(i__, k) + ai_ref(j, k) * ai_ref(
			i__, k);
		gi = gi - ar_ref(j, k) * ai_ref(i__, k) + ai_ref(j, k) * 
			ar_ref(i__, k);
/* L180: */
	    }

	    jp1 = j + 1;
	    if (l < jp1) {
		goto L220;
	    }

	    i__3 = l;
	    for (k = jp1; k <= i__3; ++k) {
		g = g + ar_ref(k, j) * ar_ref(i__, k) - ai_ref(k, j) * ai_ref(
			i__, k);
		gi = gi - ar_ref(k, j) * ai_ref(i__, k) - ai_ref(k, j) * 
			ar_ref(i__, k);
/* L200: */
	    }
/*     .......... FORM ELEMENT OF P .......... */
L220:
	    e[j] = g / h__;
	    tau_ref(2, j) = gi / h__;
	    f = f + e[j] * ar_ref(i__, j) - tau_ref(2, j) * ai_ref(i__, j);
/* L240: */
	}

	hh = f / (h__ + h__);
/*     .......... FORM REDUCED A .......... */
	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    f = ar_ref(i__, j);
	    g = e[j] - hh * f;
	    e[j] = g;
	    fi = -ai_ref(i__, j);
	    gi = tau_ref(2, j) - hh * fi;
	    tau_ref(2, j) = -gi;

	    i__3 = j;
	    for (k = 1; k <= i__3; ++k) {
		ar_ref(j, k) = ar_ref(j, k) - f * e[k] - g * ar_ref(i__, k) + 
			fi * tau_ref(2, k) + gi * ai_ref(i__, k);
		ai_ref(j, k) = ai_ref(j, k) - f * tau_ref(2, k) - g * ai_ref(
			i__, k) - fi * e[k] - gi * ar_ref(i__, k);
/* L260: */
	    }
	}

L270:
	i__3 = l;
	for (k = 1; k <= i__3; ++k) {
	    ar_ref(i__, k) = scale * ar_ref(i__, k);
	    ai_ref(i__, k) = scale * ai_ref(i__, k);
/* L280: */
	}

	tau_ref(2, l) = -si;
L290:
	hh = d__[i__];
	d__[i__] = ar_ref(i__, i__);
	ar_ref(i__, i__) = hh;
	ai_ref(i__, i__) = scale * sqrt(h__);
/* L300: */
    }

    return 0;
} /* htridi_ */

#undef tau_ref
#undef ar_ref
#undef ai_ref


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
	r__ = pythag_(&g, &c_b169);
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
	r__ = pythag_(&g, &c_b169);
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
} /* pythag_ */

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

   Subroutine */ int bisect_(integer *n, real *eps1, real *d__, real *e, real 
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
	v = (r__1 = e[i__], dabs(r__1)) / epslon_(&c_b169);
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
	v = (r__1 = e[i__], dabs(r__1)) / epslon_(&c_b169);
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

   Subroutine */ int csvdc_(complex *x, integer *ldx, integer *n, integer *p, 
	complex *s, complex *e, complex *u, integer *ldu, complex *v, integer 
	*ldv, complex *work, integer *job, integer *info)
{
    /* System generated locals */
    integer x_dim1, x_offset, u_dim1, u_offset, v_dim1, v_offset, i__1, i__2, 
	    i__3, i__4;
    real r__1, r__2, r__3, r__4;
    complex q__1, q__2, q__3;

    /* Builtin functions */
    double r_imag(complex *), c_abs(complex *);
    void c_div(complex *, complex *, complex *), r_cnjg(complex *, complex *);
    double sqrt(doublereal);

    /* Local variables */
    static integer kase, jobu, iter;
    static real test;
    static integer nctp1;
    static real b, c__;
    static integer nrtp1;
    static real f, g;
    static integer i__, j, k, l, m;
    static complex r__, t;
    static real scale;
    extern /* Subroutine */ int cscal_(integer *, complex *, complex *, 
	    integer *);
    extern /* Complex */ VOID cdotc_(complex *, integer *, complex *, integer 
	    *, complex *, integer *);
    static real shift;
    extern /* Subroutine */ int cswap_(integer *, complex *, integer *, 
	    complex *, integer *);
    static integer maxit;
    extern /* Subroutine */ int caxpy_(integer *, complex *, complex *, 
	    integer *, complex *, integer *), csrot_(integer *, complex *, 
	    integer *, complex *, integer *, real *, real *);
    static real iopst;
    static logical wantu, wantv;
    extern /* Subroutine */ int srotg_(real *, real *, real *, real *);
    static real t1;
    extern doublereal scnrm2_(integer *, complex *, integer *);
    static real el;
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


#define u_subscr(a_1,a_2) (a_2)*u_dim1 + a_1
#define u_ref(a_1,a_2) u[u_subscr(a_1,a_2)]
#define v_subscr(a_1,a_2) (a_2)*v_dim1 + a_1
#define v_ref(a_1,a_2) v[v_subscr(a_1,a_2)]
#define x_subscr(a_1,a_2) (a_2)*x_dim1 + a_1
#define x_ref(a_1,a_2) x[x_subscr(a_1,a_2)]


/*     COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, IOPS IS ONLY INCREMENTED   
       IOPST IS USED TO ACCUMULATE SMALL CONTRIBUTIONS TO IOPS   
       TO AVOID ROUNDOFF ERROR   


       CSVDC IS A SUBROUTINE TO REDUCE A COMPLEX NXP MATRIX X BY   
       UNITARY TRANSFORMATIONS U AND V TO DIAGONAL FORM.  THE   
       DIAGONAL ELEMENTS S(I) ARE THE SINGULAR VALUES OF X.  THE   
       COLUMNS OF U ARE THE CORRESPONDING LEFT SINGULAR VECTORS,   
       AND THE COLUMNS OF V THE RIGHT SINGULAR VECTORS.   

       ON ENTRY   

           X         COMPLEX(LDX,P), WHERE LDX.GE.N.   
                     X CONTAINS THE MATRIX WHOSE SINGULAR VALUE   
                     DECOMPOSITION IS TO BE COMPUTED.  X IS   
                     DESTROYED BY CSVDC.   

           LDX       INTEGER.   
                     LDX IS THE LEADING DIMENSION OF THE ARRAY X.   

           N         INTEGER.   
                     N IS THE NUMBER OF ROWS OF THE MATRIX X.   

           P         INTEGER.   
                     P IS THE NUMBER OF COLUMNS OF THE MATRIX X.   

           LDU       INTEGER.   
                     LDU IS THE LEADING DIMENSION OF THE ARRAY U   
                     (SEE BELOW).   

           LDV       INTEGER.   
                     LDV IS THE LEADING DIMENSION OF THE ARRAY V   
                     (SEE BELOW).   

           WORK      COMPLEX(N).   
                     WORK IS A SCRATCH ARRAY.   

           JOB       INTEGER.   
                     JOB CONTROLS THE COMPUTATION OF THE SINGULAR   
                     VECTORS.  IT HAS THE DECIMAL EXPANSION AB   
                     WITH THE FOLLOWING MEANING   

                          A.EQ.0    DO NOT COMPUTE THE LEFT SINGULAR   
                                    VECTORS.   
                          A.EQ.1    RETURN THE N LEFT SINGULAR VECTORS   
                                    IN U.   
                          A.GE.2    RETURNS THE FIRST MIN(N,P)   
                                    LEFT SINGULAR VECTORS IN U.   
                          B.EQ.0    DO NOT COMPUTE THE RIGHT SINGULAR   
                                    VECTORS.   
                          B.EQ.1    RETURN THE RIGHT SINGULAR VECTORS   
                                    IN V.   

       ON RETURN   

           S         COMPLEX(MM), WHERE MM=MIN(N+1,P).   
                     THE FIRST MIN(N,P) ENTRIES OF S CONTAIN THE   
                     SINGULAR VALUES OF X ARRANGED IN DESCENDING   
                     ORDER OF MAGNITUDE.   

           E         COMPLEX(P).   
                     E ORDINARILY CONTAINS ZEROS.  HOWEVER SEE THE   
                     DISCUSSION OF INFO FOR EXCEPTIONS.   

           U         COMPLEX(LDU,K), WHERE LDU.GE.N.  IF JOBA.EQ.1 THEN   
                                     K.EQ.N, IF JOBA.GE.2 THEN   
                                     K.EQ.MIN(N,P).   
                     U CONTAINS THE MATRIX OF LEFT SINGULAR VECTORS.   
                     U IS NOT REFERENCED IF JOBA.EQ.0.  IF N.LE.P   
                     OR IF JOBA.GT.2, THEN U MAY BE IDENTIFIED WITH X   
                     IN THE SUBROUTINE CALL.   

           V         COMPLEX(LDV,P), WHERE LDV.GE.P.   
                     V CONTAINS THE MATRIX OF RIGHT SINGULAR VECTORS.   
                     V IS NOT REFERENCED IF JOBB.EQ.0.  IF P.LE.N,   
                     THEN V MAY BE IDENTIFIED WHTH X IN THE   
                     SUBROUTINE CALL.   

           INFO      INTEGER.   
                     THE SINGULAR VALUES (AND THEIR CORRESPONDING   
                     SINGULAR VECTORS) S(INFO+1),S(INFO+2),...,S(M)   
                     ARE CORRECT (HERE M=MIN(N,P)).  THUS IF   
                     INFO.EQ.0, ALL THE SINGULAR VALUES AND THEIR   
                     VECTORS ARE CORRECT.  IN ANY EVENT, THE MATRIX   
                     B = CTRANS(U)*X*V IS THE BIDIAGONAL MATRIX   
                     WITH THE ELEMENTS OF S ON ITS DIAGONAL AND THE   
                     ELEMENTS OF E ON ITS SUPER-DIAGONAL (CTRANS(U)   
                     IS THE CONJUGATE-TRANSPOSE OF U).  THUS THE   
                     SINGULAR VALUES OF X AND B ARE THE SAME.   

       LINPACK. THIS VERSION DATED 03/19/79 .   
                CORRECTION TO SHIFT CALCULATION MADE 2/85.   
       G.W. STEWART, UNIVERSITY OF MARYLAND, ARGONNE NATIONAL LAB.   

       CSVDC USES THE FOLLOWING FUNCTIONS AND SUBPROGRAMS.   

       EXTERNAL CSROT   
       BLAS CAXPY,CDOTC,CSCAL,CSWAP,SCNRM2,SROTG   
       FORTRAN ABS,AIMAG,AMAX1,CABS,CMPLX   
       FORTRAN CONJG,MAX0,MIN0,MOD,REAL,SQRT   

       INTERNAL VARIABLES   

       REAL ZTEST   


       DECLARE EPS AND SLAMCH FOR NEW STOPPING CRITERION   


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
	latime_2.iops += (*n - l + 1 << 2) + 2;
	i__2 = l;
	i__3 = *n - l + 1;
	r__1 = scnrm2_(&i__3, &x_ref(l, l), &c__1);
	q__1.r = r__1, q__1.i = 0.f;
	s[i__2].r = q__1.r, s[i__2].i = q__1.i;
	i__2 = l;
	if ((r__1 = s[i__2].r, dabs(r__1)) + (r__2 = r_imag(&s[l]), dabs(r__2)
		) == 0.f) {
	    goto L10;
	}
	i__2 = x_subscr(l, l);
	if ((r__1 = x[i__2].r, dabs(r__1)) + (r__2 = r_imag(&x_ref(l, l)), 
		dabs(r__2)) != 0.f) {
	    i__3 = l;
	    r__3 = c_abs(&s[l]);
	    i__4 = x_subscr(l, l);
	    r__4 = c_abs(&x_ref(l, l));
	    q__2.r = x[i__4].r / r__4, q__2.i = x[i__4].i / r__4;
	    q__1.r = r__3 * q__2.r, q__1.i = r__3 * q__2.i;
	    s[i__3].r = q__1.r, s[i__3].i = q__1.i;
	}

/*              INCREMENT OP COUNT */
	latime_2.iops += (*n - l + 1) * 6 + 23;
	i__2 = *n - l + 1;
	c_div(&q__1, &c_b319, &s[l]);
	cscal_(&i__2, &q__1, &x_ref(l, l), &c__1);
	i__2 = x_subscr(l, l);
	i__3 = x_subscr(l, l);
	q__1.r = x[i__3].r + 1.f, q__1.i = x[i__3].i + 0.f;
	x[i__2].r = q__1.r, x[i__2].i = q__1.i;
L10:
	i__2 = l;
	i__3 = l;
	q__1.r = -s[i__3].r, q__1.i = -s[i__3].i;
	s[i__2].r = q__1.r, s[i__2].i = q__1.i;
L20:
	if (*p < lp1) {
	    goto L50;
	}
	i__2 = *p;
	for (j = lp1; j <= i__2; ++j) {
	    if (l > nct) {
		goto L30;
	    }
	    i__3 = l;
	    if ((r__1 = s[i__3].r, dabs(r__1)) + (r__2 = r_imag(&s[l]), dabs(
		    r__2)) == 0.f) {
		goto L30;
	    }

/*              APPLY THE TRANSFORMATION.   


                INCREMENT OP COUNT */
	    latime_2.iops += (*n - l << 4) + 26;
	    i__3 = *n - l + 1;
	    cdotc_(&q__3, &i__3, &x_ref(l, l), &c__1, &x_ref(l, j), &c__1);
	    q__2.r = -q__3.r, q__2.i = -q__3.i;
	    c_div(&q__1, &q__2, &x_ref(l, l));
	    t.r = q__1.r, t.i = q__1.i;
	    i__3 = *n - l + 1;
	    caxpy_(&i__3, &t, &x_ref(l, l), &c__1, &x_ref(l, j), &c__1);
L30:

/*           PLACE THE L-TH ROW OF X INTO  E FOR THE   
             SUBSEQUENT CALCULATION OF THE ROW TRANSFORMATION. */

	    i__3 = j;
	    r_cnjg(&q__1, &x_ref(l, j));
	    e[i__3].r = q__1.r, e[i__3].i = q__1.i;
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
	    i__3 = u_subscr(i__, l);
	    i__4 = x_subscr(i__, l);
	    u[i__3].r = x[i__4].r, u[i__3].i = x[i__4].i;
/* L60: */
	}
L70:
	if (l > nrt) {
	    goto L150;
	}

/*           COMPUTE THE L-TH ROW TRANSFORMATION AND PLACE THE   
             L-TH SUPER-DIAGONAL IN E(L).   


             INCREMENT OP COUNT */
	latime_2.iops += (*p - l << 2) + 3;
	i__2 = l;
	i__3 = *p - l;
	r__1 = scnrm2_(&i__3, &e[lp1], &c__1);
	q__1.r = r__1, q__1.i = 0.f;
	e[i__2].r = q__1.r, e[i__2].i = q__1.i;
	i__2 = l;
	if ((r__1 = e[i__2].r, dabs(r__1)) + (r__2 = r_imag(&e[l]), dabs(r__2)
		) == 0.f) {
	    goto L80;
	}
	i__2 = lp1;
	if ((r__1 = e[i__2].r, dabs(r__1)) + (r__2 = r_imag(&e[lp1]), dabs(
		r__2)) != 0.f) {
	    i__3 = l;
	    r__3 = c_abs(&e[l]);
	    i__4 = lp1;
	    r__4 = c_abs(&e[lp1]);
	    q__2.r = e[i__4].r / r__4, q__2.i = e[i__4].i / r__4;
	    q__1.r = r__3 * q__2.r, q__1.i = r__3 * q__2.i;
	    e[i__3].r = q__1.r, e[i__3].i = q__1.i;
	}

/*              INCREMENT OP COUNT */
	latime_2.iops += (*p - l) * 6 + 23;
	i__2 = *p - l;
	c_div(&q__1, &c_b319, &e[l]);
	cscal_(&i__2, &q__1, &e[lp1], &c__1);
	i__2 = lp1;
	i__3 = lp1;
	q__1.r = e[i__3].r + 1.f, q__1.i = e[i__3].i + 0.f;
	e[i__2].r = q__1.r, e[i__2].i = q__1.i;
L80:
	i__2 = l;
	r_cnjg(&q__2, &e[l]);
	q__1.r = -q__2.r, q__1.i = -q__2.i;
	e[i__2].r = q__1.r, e[i__2].i = q__1.i;
	i__2 = l;
	if (lp1 > *n || (r__1 = e[i__2].r, dabs(r__1)) + (r__2 = r_imag(&e[l])
		, dabs(r__2)) == 0.f) {
	    goto L120;
	}

/*              APPLY THE TRANSFORMATION. */

	i__2 = *n;
	for (i__ = lp1; i__ <= i__2; ++i__) {
	    i__3 = i__;
	    work[i__3].r = 0.f, work[i__3].i = 0.f;
/* L90: */
	}

/*              INCREMENT OP COUNT */
	latime_2.iops += (real) ((*n - l << 4) + 9) * (*p - l);
	i__2 = *p;
	for (j = lp1; j <= i__2; ++j) {
	    i__3 = *n - l;
	    caxpy_(&i__3, &e[j], &x_ref(lp1, j), &c__1, &work[lp1], &c__1);
/* L100: */
	}
	i__2 = *p;
	for (j = lp1; j <= i__2; ++j) {
	    i__3 = *n - l;
	    i__4 = j;
	    q__3.r = -e[i__4].r, q__3.i = -e[i__4].i;
	    c_div(&q__2, &q__3, &e[lp1]);
	    r_cnjg(&q__1, &q__2);
	    caxpy_(&i__3, &q__1, &work[lp1], &c__1, &x_ref(lp1, j), &c__1);
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
	    i__3 = v_subscr(i__, l);
	    i__4 = i__;
	    v[i__3].r = e[i__4].r, v[i__3].i = e[i__4].i;
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
	i__1 = nctp1;
	i__2 = x_subscr(nctp1, nctp1);
	s[i__1].r = x[i__2].r, s[i__1].i = x[i__2].i;
    }
    if (*n < m) {
	i__1 = m;
	s[i__1].r = 0.f, s[i__1].i = 0.f;
    }
    if (nrtp1 < m) {
	i__1 = nrtp1;
	i__2 = x_subscr(nrtp1, m);
	e[i__1].r = x[i__2].r, e[i__1].i = x[i__2].i;
    }
    i__1 = m;
    e[i__1].r = 0.f, e[i__1].i = 0.f;

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
	    i__3 = u_subscr(i__, j);
	    u[i__3].r = 0.f, u[i__3].i = 0.f;
/* L180: */
	}
	i__2 = u_subscr(j, j);
	u[i__2].r = 1.f, u[i__2].i = 0.f;
/* L190: */
    }
L200:
    if (nct < 1) {
	goto L290;
    }
    i__1 = nct;
    for (ll = 1; ll <= i__1; ++ll) {
	l = nct - ll + 1;
	i__2 = l;
	if ((r__1 = s[i__2].r, dabs(r__1)) + (r__2 = r_imag(&s[l]), dabs(r__2)
		) == 0.f) {
	    goto L250;
	}
	lp1 = l + 1;
	if (ncu < lp1) {
	    goto L220;
	}

/*              INCREMENT OP COUNT */
	latime_2.iops += (real) ((*n - l << 4) + 25) * (ncu - l) + (*n - l) * 
		6 + 9;
	i__2 = ncu;
	for (j = lp1; j <= i__2; ++j) {
	    i__3 = *n - l + 1;
	    cdotc_(&q__3, &i__3, &u_ref(l, l), &c__1, &u_ref(l, j), &c__1);
	    q__2.r = -q__3.r, q__2.i = -q__3.i;
	    c_div(&q__1, &q__2, &u_ref(l, l));
	    t.r = q__1.r, t.i = q__1.i;
	    i__3 = *n - l + 1;
	    caxpy_(&i__3, &t, &u_ref(l, l), &c__1, &u_ref(l, j), &c__1);
/* L210: */
	}
L220:
	i__2 = *n - l + 1;
	cscal_(&i__2, &c_b364, &u_ref(l, l), &c__1);
	i__2 = u_subscr(l, l);
	i__3 = u_subscr(l, l);
	q__1.r = u[i__3].r + 1.f, q__1.i = u[i__3].i + 0.f;
	u[i__2].r = q__1.r, u[i__2].i = q__1.i;
	lm1 = l - 1;
	if (lm1 < 1) {
	    goto L240;
	}
	i__2 = lm1;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    i__3 = u_subscr(i__, l);
	    u[i__3].r = 0.f, u[i__3].i = 0.f;
/* L230: */
	}
L240:
	goto L270;
L250:
	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    i__3 = u_subscr(i__, l);
	    u[i__3].r = 0.f, u[i__3].i = 0.f;
/* L260: */
	}
	i__2 = u_subscr(l, l);
	u[i__2].r = 1.f, u[i__2].i = 0.f;
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
	i__2 = l;
	if ((r__1 = e[i__2].r, dabs(r__1)) + (r__2 = r_imag(&e[l]), dabs(r__2)
		) == 0.f) {
	    goto L320;
	}

/*              INCREMENT OP COUNT */
	latime_2.iops += (real) ((*p - l << 4) + 9) * (*p - l) + 1;
	i__2 = *p;
	for (j = lp1; j <= i__2; ++j) {
	    i__3 = *p - l;
	    cdotc_(&q__3, &i__3, &v_ref(lp1, l), &c__1, &v_ref(lp1, j), &c__1)
		    ;
	    q__2.r = -q__3.r, q__2.i = -q__3.i;
	    c_div(&q__1, &q__2, &v_ref(lp1, l));
	    t.r = q__1.r, t.i = q__1.i;
	    i__3 = *p - l;
	    caxpy_(&i__3, &t, &v_ref(lp1, l), &c__1, &v_ref(lp1, j), &c__1);
/* L310: */
	}
L320:
	i__2 = *p;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    i__3 = v_subscr(i__, l);
	    v[i__3].r = 0.f, v[i__3].i = 0.f;
/* L330: */
	}
	i__2 = v_subscr(l, l);
	v[i__2].r = 1.f, v[i__2].i = 0.f;
/* L340: */
    }
L350:

/*     TRANSFORM S AND E SO THAT THEY ARE REAL.   


       INCREMENT OP COUNT */
    latime_2.iops += (m << 1) - 1;
    i__1 = m;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = i__;
	if ((r__1 = s[i__2].r, dabs(r__1)) + (r__2 = r_imag(&s[i__]), dabs(
		r__2)) == 0.f) {
	    goto L360;
	}

/*           INCREMENT OP COUNT */
	latime_2.iops += 23;
	if (wantu) {
	    latime_2.iops += *n * 6;
	}
	r__1 = c_abs(&s[i__]);
	q__1.r = r__1, q__1.i = 0.f;
	t.r = q__1.r, t.i = q__1.i;
	c_div(&q__1, &s[i__], &t);
	r__.r = q__1.r, r__.i = q__1.i;
	i__2 = i__;
	s[i__2].r = t.r, s[i__2].i = t.i;
	if (i__ < m) {
	    i__2 = i__;
	    c_div(&q__1, &e[i__], &r__);
	    e[i__2].r = q__1.r, e[i__2].i = q__1.i;
	}
	if (wantu) {
	    cscal_(n, &r__, &u_ref(1, i__), &c__1);
	}
L360:
/*     ...EXIT */
	if (i__ == m) {
	    goto L390;
	}
	i__2 = i__;
	if ((r__1 = e[i__2].r, dabs(r__1)) + (r__2 = r_imag(&e[i__]), dabs(
		r__2)) == 0.f) {
	    goto L370;
	}

/*           INCREMENT OP COUNT */
	latime_2.iops += 20;
	if (wantv) {
	    latime_2.iops += *p * 6;
	}
	r__1 = c_abs(&e[i__]);
	q__1.r = r__1, q__1.i = 0.f;
	t.r = q__1.r, t.i = q__1.i;
	c_div(&q__1, &t, &e[i__]);
	r__.r = q__1.r, r__.i = q__1.i;
	i__2 = i__;
	e[i__2].r = t.r, e[i__2].i = t.i;
	i__2 = i__ + 1;
	i__3 = i__ + 1;
	q__1.r = s[i__3].r * r__.r - s[i__3].i * r__.i, q__1.i = s[i__3].r * 
		r__.i + s[i__3].i * r__.r;
	s[i__2].r = q__1.r, s[i__2].i = q__1.i;
	if (wantv) {
	    cscal_(p, &r__, &v_ref(1, i__ + 1), &c__1);
	}
L370:
/* L380: */
	;
    }
L390:

/*     MAIN ITERATION LOOP FOR THE SINGULAR VALUES. */

    mm = m;

/*     INITIALIZE ITERATION COUNTER */
    latime_2.itcnt = 0.f;
    iter = 0;
L400:

/*        QUIT IF ALL THE SINGULAR VALUES HAVE BEEN FOUND.   

       ...EXIT */
    if (m == 0) {
	goto L660;
    }

/*        IF TOO MANY ITERATIONS HAVE BEEN PERFORMED, SET   
          FLAG AND RETURN.   


          UPDATE ITERATION COUNTER */
    latime_2.itcnt = (real) iter;
    if (iter < maxit) {
	goto L410;
    }
    *info = m;
/*     ......EXIT */
    goto L660;
L410:

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
	    goto L440;
	}

/*           INCREMENT OP COUNT */
	iopst += 17;
	test = c_abs(&s[l]) + c_abs(&s[l + 1]);

/*           REPLACE STOPPING CRITERION WITH NEW ONE   

             ZTEST = TEST + CABS(E(L))   
             IF (ZTEST .NE. TEST) GO TO 420 */
	if (c_abs(&e[l]) > eps * test) {
	    goto L420;
	}

	i__2 = l;
	e[i__2].r = 0.f, e[i__2].i = 0.f;
/*        ......EXIT */
	goto L440;
L420:
/* L430: */
	;
    }
L440:
    if (l != m - 1) {
	goto L450;
    }
    kase = 4;
    goto L520;
L450:
    lp1 = l + 1;
    mp1 = m + 1;
    i__1 = mp1;
    for (lls = lp1; lls <= i__1; ++lls) {
	ls = m - lls + lp1;
/*           ...EXIT */
	if (ls == l) {
	    goto L480;
	}
	test = 0.f;

/*              INCREMENT OP COUNT */
	iopst += 18;
	if (ls != m) {
	    test += c_abs(&e[ls]);
	}
	if (ls != l + 1) {
	    test += c_abs(&e[ls - 1]);
	}

/*              REPLACE STOPPING CRITERION WITH NEW ONE AS IN LAPACK   

                ZTEST = TEST + CABS(S(LS))   
                IF (ZTEST .NE. TEST) GO TO 460 */
	if (c_abs(&s[ls]) > eps * test) {
	    goto L460;
	}

	i__2 = ls;
	s[i__2].r = 0.f, s[i__2].i = 0.f;
/*           ......EXIT */
	goto L480;
L460:
/* L470: */
	;
    }
L480:
    if (ls != l) {
	goto L490;
    }
    kase = 3;
    goto L510;
L490:
    if (ls != m) {
	goto L500;
    }
    kase = 1;
    goto L510;
L500:
    kase = 2;
    l = ls;
L510:
L520:
    ++l;

/*        PERFORM THE TASK INDICATED BY KASE. */

    switch (kase) {
	case 1:  goto L530;
	case 2:  goto L560;
	case 3:  goto L580;
	case 4:  goto L610;
    }

/*        DEFLATE NEGLIGIBLE S(M). */

L530:
    mm1 = m - 1;
    i__1 = m - 1;
    f = e[i__1].r;
    i__1 = m - 1;
    e[i__1].r = 0.f, e[i__1].i = 0.f;

/*           INCREMENT OP COUNT */
    latime_2.iops += (mm1 - l + 1) * 14 - 3;
    if (wantv) {
	latime_2.iops += (real) (mm1 - l + 1) * 12 * *p;
    }
    i__1 = mm1;
    for (kk = l; kk <= i__1; ++kk) {
	k = mm1 - kk + l;
	i__2 = k;
	t1 = s[i__2].r;
	srotg_(&t1, &f, &cs, &sn);
	i__2 = k;
	q__1.r = t1, q__1.i = 0.f;
	s[i__2].r = q__1.r, s[i__2].i = q__1.i;
	if (k == l) {
	    goto L540;
	}
	i__2 = k - 1;
	f = -sn * e[i__2].r;
	i__2 = k - 1;
	i__3 = k - 1;
	q__1.r = cs * e[i__3].r, q__1.i = cs * e[i__3].i;
	e[i__2].r = q__1.r, e[i__2].i = q__1.i;
L540:
	if (wantv) {
	    csrot_(p, &v_ref(1, k), &c__1, &v_ref(1, m), &c__1, &cs, &sn);
	}
/* L550: */
    }
    goto L650;

/*        SPLIT AT NEGLIGIBLE S(L). */

L560:
    i__1 = l - 1;
    f = e[i__1].r;
    i__1 = l - 1;
    e[i__1].r = 0.f, e[i__1].i = 0.f;

/*           INCREMENT OP COUNT */
    latime_2.iops += (m - l + 1) * 14;
    if (wantu) {
	latime_2.iops += (real) (m - l + 1) * 12 * *n;
    }
    i__1 = m;
    for (k = l; k <= i__1; ++k) {
	i__2 = k;
	t1 = s[i__2].r;
	srotg_(&t1, &f, &cs, &sn);
	i__2 = k;
	q__1.r = t1, q__1.i = 0.f;
	s[i__2].r = q__1.r, s[i__2].i = q__1.i;
	i__2 = k;
	f = -sn * e[i__2].r;
	i__2 = k;
	i__3 = k;
	q__1.r = cs * e[i__3].r, q__1.i = cs * e[i__3].i;
	e[i__2].r = q__1.r, e[i__2].i = q__1.i;
	if (wantu) {
	    csrot_(n, &u_ref(1, k), &c__1, &u_ref(1, l - 1), &c__1, &cs, &sn);
	}
/* L570: */
    }
    goto L650;

/*        PERFORM ONE QR STEP. */

L580:

/*           CALCULATE THE SHIFT.   


             INCREMENT OP COUNT */
    iopst += 48;
/* Computing MAX */
    r__1 = c_abs(&s[m]), r__2 = c_abs(&s[m - 1]), r__1 = max(r__1,r__2), r__2 
	    = c_abs(&e[m - 1]), r__1 = max(r__1,r__2), r__2 = c_abs(&s[l]), 
	    r__1 = max(r__1,r__2), r__2 = c_abs(&e[l]);
    scale = dmax(r__1,r__2);
    i__1 = m;
    sm = s[i__1].r / scale;
    i__1 = m - 1;
    smm1 = s[i__1].r / scale;
    i__1 = m - 1;
    emm1 = e[i__1].r / scale;
    i__1 = l;
    sl = s[i__1].r / scale;
    i__1 = l;
    el = e[i__1].r / scale;
/* Computing 2nd power */
    r__1 = emm1;
    b = ((smm1 + sm) * (smm1 - sm) + r__1 * r__1) / 2.f;
/* Computing 2nd power */
    r__1 = sm * emm1;
    c__ = r__1 * r__1;
    shift = 0.f;
    if (b == 0.f && c__ == 0.f) {
	goto L590;
    }
/* Computing 2nd power */
    r__1 = b;
    shift = sqrt(r__1 * r__1 + c__);
    if (b < 0.f) {
	shift = -shift;
    }
    shift = c__ / (b + shift);
L590:
    f = (sl + sm) * (sl - sm) + shift;
    g = sl * el;

/*           CHASE ZEROS. */

    mm1 = m - 1;

/*           INCREMENT OP COUNT */
    latime_2.iops += (mm1 - l + 1) * 46;
    if (wantv) {
	latime_2.iops += (real) (mm1 - l + 1) * 12 * *p;
    }
    if (wantu) {
/* Computing MAX   
   Computing MIN */
	i__2 = mm1, i__3 = *n - 1;
	i__1 = min(i__2,i__3) - l + 1;
	latime_2.iops += (real) max(i__1,0) * 12 * *n;
    }
    i__1 = mm1;
    for (k = l; k <= i__1; ++k) {
	srotg_(&f, &g, &cs, &sn);
	if (k != l) {
	    i__2 = k - 1;
	    q__1.r = f, q__1.i = 0.f;
	    e[i__2].r = q__1.r, e[i__2].i = q__1.i;
	}
	i__2 = k;
	i__3 = k;
	f = cs * s[i__2].r + sn * e[i__3].r;
	i__2 = k;
	i__3 = k;
	q__2.r = cs * e[i__3].r, q__2.i = cs * e[i__3].i;
	i__4 = k;
	q__3.r = sn * s[i__4].r, q__3.i = sn * s[i__4].i;
	q__1.r = q__2.r - q__3.r, q__1.i = q__2.i - q__3.i;
	e[i__2].r = q__1.r, e[i__2].i = q__1.i;
	i__2 = k + 1;
	g = sn * s[i__2].r;
	i__2 = k + 1;
	i__3 = k + 1;
	q__1.r = cs * s[i__3].r, q__1.i = cs * s[i__3].i;
	s[i__2].r = q__1.r, s[i__2].i = q__1.i;
	if (wantv) {
	    csrot_(p, &v_ref(1, k), &c__1, &v_ref(1, k + 1), &c__1, &cs, &sn);
	}
	srotg_(&f, &g, &cs, &sn);
	i__2 = k;
	q__1.r = f, q__1.i = 0.f;
	s[i__2].r = q__1.r, s[i__2].i = q__1.i;
	i__2 = k;
	i__3 = k + 1;
	f = cs * e[i__2].r + sn * s[i__3].r;
	i__2 = k + 1;
	r__1 = -sn;
	i__3 = k;
	q__2.r = r__1 * e[i__3].r, q__2.i = r__1 * e[i__3].i;
	i__4 = k + 1;
	q__3.r = cs * s[i__4].r, q__3.i = cs * s[i__4].i;
	q__1.r = q__2.r + q__3.r, q__1.i = q__2.i + q__3.i;
	s[i__2].r = q__1.r, s[i__2].i = q__1.i;
	i__2 = k + 1;
	g = sn * e[i__2].r;
	i__2 = k + 1;
	i__3 = k + 1;
	q__1.r = cs * e[i__3].r, q__1.i = cs * e[i__3].i;
	e[i__2].r = q__1.r, e[i__2].i = q__1.i;
	if (wantu && k < *n) {
	    csrot_(n, &u_ref(1, k), &c__1, &u_ref(1, k + 1), &c__1, &cs, &sn);
	}
/* L600: */
    }
    i__1 = m - 1;
    q__1.r = f, q__1.i = 0.f;
    e[i__1].r = q__1.r, e[i__1].i = q__1.i;
    ++iter;
    goto L650;

/*        CONVERGENCE. */

L610:

/*           MAKE THE SINGULAR VALUE  POSITIVE */

    i__1 = l;
    if (s[i__1].r >= 0.f) {
	goto L620;
    }
    i__1 = l;
    i__2 = l;
    q__1.r = -s[i__2].r, q__1.i = -s[i__2].i;
    s[i__1].r = q__1.r, s[i__1].i = q__1.i;

/*              INCREMENT OP COUNT */
    if (wantv) {
	latime_2.iops += *p * 6;
    }
    if (wantv) {
	cscal_(p, &c_b364, &v_ref(1, l), &c__1);
    }
L620:

/*           ORDER THE SINGULAR VALUE. */

L630:
    if (l == mm) {
	goto L640;
    }
/*           ...EXIT */
    i__1 = l;
    i__2 = l + 1;
    if (s[i__1].r >= s[i__2].r) {
	goto L640;
    }
    i__1 = l;
    t.r = s[i__1].r, t.i = s[i__1].i;
    i__1 = l;
    i__2 = l + 1;
    s[i__1].r = s[i__2].r, s[i__1].i = s[i__2].i;
    i__1 = l + 1;
    s[i__1].r = t.r, s[i__1].i = t.i;
    if (wantv && l < *p) {
	cswap_(p, &v_ref(1, l), &c__1, &v_ref(1, l + 1), &c__1);
    }
    if (wantu && l < *n) {
	cswap_(n, &u_ref(1, l), &c__1, &u_ref(1, l + 1), &c__1);
    }
    ++l;
    goto L630;
L640:
    iter = 0;
    --m;
L650:
    goto L400;
L660:

/*     COMPUTE FINAL OPCOUNT */
    latime_2.iops += iopst;
    return 0;
} /* csvdc_ */

#undef x_ref
#undef x_subscr
#undef v_ref
#undef v_subscr
#undef u_ref
#undef u_subscr



/*     ------------------------------------------------------------------   

   Subroutine */ int cqzhes_(integer *nm, integer *n, real *ar, real *ai, 
	real *br, real *bi, logical *matz, real *zr, real *zi)
{
    /* System generated locals */
    integer ar_dim1, ar_offset, ai_dim1, ai_offset, br_dim1, br_offset, 
	    bi_dim1, bi_offset, zr_dim1, zr_offset, zi_dim1, zi_offset, i__1, 
	    i__2, i__3;
    real r__1, r__2, r__3;
    complex q__1;

    /* Builtin functions */
    double sqrt(doublereal), c_abs(complex *);

    /* Local variables */
    static real opst;
    static integer i__, j, k, l;
    static real r__, s, t;
    static integer k1, l1, iopst;
    static real u1, u2;
    static integer lb;
    static real ti, xi, yi, xr, yr;
    static integer nk1, nm1;
    static real u1i, rho;


#define ai_subscr(a_1,a_2) (a_2)*ai_dim1 + a_1
#define ai_ref(a_1,a_2) ai[(a_2)*ai_dim1 + a_1]
#define bi_subscr(a_1,a_2) (a_2)*bi_dim1 + a_1
#define bi_ref(a_1,a_2) bi[(a_2)*bi_dim1 + a_1]
#define ar_subscr(a_1,a_2) (a_2)*ar_dim1 + a_1
#define ar_ref(a_1,a_2) ar[(a_2)*ar_dim1 + a_1]
#define br_subscr(a_1,a_2) (a_2)*br_dim1 + a_1
#define br_ref(a_1,a_2) br[(a_2)*br_dim1 + a_1]
#define zi_ref(a_1,a_2) zi[(a_2)*zi_dim1 + a_1]
#define zr_ref(a_1,a_2) zr[(a_2)*zr_dim1 + a_1]



/*     ----------------------- BEGIN TIMING CODE ------------------------   
       COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED   
       OPST IS USED TO ACCUMULATE SMALL CONTRIBUTIONS TO OPS   
       TO AVOID ROUNDOFF ERROR   
       ------------------------ END TIMING CODE -------------------------   


       THIS SUBROUTINE IS A COMPLEX ANALOGUE OF THE FIRST STEP OF THE   
       QZ ALGORITHM FOR SOLVING GENERALIZED MATRIX EIGENVALUE PROBLEMS,   
       SIAM J. NUMER. ANAL. 10, 241-256(1973) BY MOLER AND STEWART.   

       THIS SUBROUTINE ACCEPTS A PAIR OF COMPLEX GENERAL MATRICES AND   
       REDUCES ONE OF THEM TO UPPER HESSENBERG FORM WITH REAL (AND NON-   
       NEGATIVE) SUBDIAGONAL ELEMENTS AND THE OTHER TO UPPER TRIANGULAR   
       FORM USING UNITARY TRANSFORMATIONS.  IT IS USUALLY FOLLOWED BY   
       CQZVAL  AND POSSIBLY  CQZVEC.   

       ON INPUT-   

          NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL   
            ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM   
            DIMENSION STATEMENT,   

          N IS THE ORDER OF THE MATRICES,   

          A=(AR,AI) CONTAINS A COMPLEX GENERAL MATRIX,   

          B=(BR,BI) CONTAINS A COMPLEX GENERAL MATRIX,   

          MATZ SHOULD BE SET TO .TRUE. IF THE RIGHT HAND TRANSFORMATIONS   
            ARE TO BE ACCUMULATED FOR LATER USE IN COMPUTING   
            EIGENVECTORS, AND TO .FALSE. OTHERWISE.   

       ON OUTPUT-   

          A HAS BEEN REDUCED TO UPPER HESSENBERG FORM.  THE ELEMENTS   
            BELOW THE FIRST SUBDIAGONAL HAVE BEEN SET TO ZERO, AND THE   
            SUBDIAGONAL ELEMENTS HAVE BEEN MADE REAL (AND NON-NEGATIVE),   

          B HAS BEEN REDUCED TO UPPER TRIANGULAR FORM.  THE ELEMENTS   
            BELOW THE MAIN DIAGONAL HAVE BEEN SET TO ZERO,   

          Z=(ZR,ZI) CONTAINS THE PRODUCT OF THE RIGHT HAND   
            TRANSFORMATIONS IF MATZ HAS BEEN SET TO .TRUE.   
            OTHERWISE, Z IS NOT REFERENCED.   

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,   
       APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY   

       ------------------------------------------------------------------   

       ********** INITIALIZE Z **********   
       Parameter adjustments */
    zi_dim1 = *nm;
    zi_offset = 1 + zi_dim1 * 1;
    zi -= zi_offset;
    zr_dim1 = *nm;
    zr_offset = 1 + zr_dim1 * 1;
    zr -= zr_offset;
    bi_dim1 = *nm;
    bi_offset = 1 + bi_dim1 * 1;
    bi -= bi_offset;
    br_dim1 = *nm;
    br_offset = 1 + br_dim1 * 1;
    br -= br_offset;
    ai_dim1 = *nm;
    ai_offset = 1 + ai_dim1 * 1;
    ai -= ai_offset;
    ar_dim1 = *nm;
    ar_offset = 1 + ar_dim1 * 1;
    ar -= ar_offset;

    /* Function Body */
    if (! (*matz)) {
	goto L10;
    }

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {

	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {
	    zr_ref(i__, j) = 0.f;
	    zi_ref(i__, j) = 0.f;
/* L2: */
	}

	zr_ref(i__, i__) = 1.f;
/* L3: */
    }
/*     ********** REDUCE B TO UPPER TRIANGULAR FORM WITH   
                  TEMPORARILY REAL DIAGONAL ELEMENTS ********** */
L10:
    if (*n <= 1) {
	goto L170;
    }
    nm1 = *n - 1;

    i__1 = nm1;
    for (l = 1; l <= i__1; ++l) {
/*        ---------------------- BEGIN TIMING CODE ---------------------- */
	iopst = 0;
/*        ----------------------- END TIMING CODE ----------------------- */
	l1 = l + 1;
	s = 0.f;

	i__2 = *n;
	for (i__ = l; i__ <= i__2; ++i__) {
	    s = s + (r__1 = br_ref(i__, l), dabs(r__1)) + (r__2 = bi_ref(i__, 
		    l), dabs(r__2));
/* L20: */
	}
/*        ---------------------- BEGIN TIMING CODE ---------------------- */
	iopst += *n + 1 - l << 1;
/*        ----------------------- END TIMING CODE ----------------------- */

	if (s == 0.f) {
	    goto L100;
	}
	rho = 0.f;

	i__2 = *n;
	for (i__ = l; i__ <= i__2; ++i__) {
	    br_ref(i__, l) = br_ref(i__, l) / s;
	    bi_ref(i__, l) = bi_ref(i__, l) / s;
/* Computing 2nd power */
	    r__1 = br_ref(i__, l);
/* Computing 2nd power */
	    r__2 = bi_ref(i__, l);
	    rho = rho + r__1 * r__1 + r__2 * r__2;
/* L25: */
	}

	r__ = sqrt(rho);
	i__2 = br_subscr(l, l);
	i__3 = bi_subscr(l, l);
	q__1.r = br[i__2], q__1.i = bi[i__3];
	xr = c_abs(&q__1);
	if (xr == 0.f) {
	    goto L27;
	}
/*        ---------------------- BEGIN TIMING CODE ---------------------- */
	iopst += 8;
/*        ----------------------- END TIMING CODE ----------------------- */
	rho += xr * r__;
	u1 = -br_ref(l, l) / xr;
	u1i = -bi_ref(l, l) / xr;
	yr = r__ / xr + 1.f;
	br_ref(l, l) = yr * br_ref(l, l);
	bi_ref(l, l) = yr * bi_ref(l, l);
	goto L28;

L27:
	br_ref(l, l) = r__;
	u1 = -1.f;
	u1i = 0.f;

L28:
	i__2 = *n;
	for (j = l1; j <= i__2; ++j) {
	    t = 0.f;
	    ti = 0.f;

	    i__3 = *n;
	    for (i__ = l; i__ <= i__3; ++i__) {
		t = t + br_ref(i__, l) * br_ref(i__, j) + bi_ref(i__, l) * 
			bi_ref(i__, j);
		ti = ti + br_ref(i__, l) * bi_ref(i__, j) - bi_ref(i__, l) * 
			br_ref(i__, j);
/* L30: */
	    }

	    t /= rho;
	    ti /= rho;

	    i__3 = *n;
	    for (i__ = l; i__ <= i__3; ++i__) {
		br_ref(i__, j) = br_ref(i__, j) - t * br_ref(i__, l) + ti * 
			bi_ref(i__, l);
		bi_ref(i__, j) = bi_ref(i__, j) - t * bi_ref(i__, l) - ti * 
			br_ref(i__, l);
/* L40: */
	    }

	    xi = u1 * bi_ref(l, j) - u1i * br_ref(l, j);
	    br_ref(l, j) = u1 * br_ref(l, j) + u1i * bi_ref(l, j);
	    bi_ref(l, j) = xi;
/* L50: */
	}

	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {
	    t = 0.f;
	    ti = 0.f;

	    i__3 = *n;
	    for (i__ = l; i__ <= i__3; ++i__) {
		t = t + br_ref(i__, l) * ar_ref(i__, j) + bi_ref(i__, l) * 
			ai_ref(i__, j);
		ti = ti + br_ref(i__, l) * ai_ref(i__, j) - bi_ref(i__, l) * 
			ar_ref(i__, j);
/* L60: */
	    }

	    t /= rho;
	    ti /= rho;

	    i__3 = *n;
	    for (i__ = l; i__ <= i__3; ++i__) {
		ar_ref(i__, j) = ar_ref(i__, j) - t * br_ref(i__, l) + ti * 
			bi_ref(i__, l);
		ai_ref(i__, j) = ai_ref(i__, j) - t * bi_ref(i__, l) - ti * 
			br_ref(i__, l);
/* L70: */
	    }

	    xi = u1 * ai_ref(l, j) - u1i * ar_ref(l, j);
	    ar_ref(l, j) = u1 * ar_ref(l, j) + u1i * ai_ref(l, j);
	    ai_ref(l, j) = xi;
/* L80: */
	}

	br_ref(l, l) = r__ * s;
	bi_ref(l, l) = 0.f;

	i__2 = *n;
	for (i__ = l1; i__ <= i__2; ++i__) {
	    br_ref(i__, l) = 0.f;
	    bi_ref(i__, l) = 0.f;
/* L90: */
	}
/*        ---------------------- BEGIN TIMING CODE ---------------------- */
	latime_1.ops += (real) ((*n - l << 4) + (*n << 4) + 30) * (real) (*n 
		- l) + (real) (*n * 24 + 13 + iopst);
/*        ----------------------- END TIMING CODE ----------------------- */

L100:
	;
    }
/*     ********** REDUCE A TO UPPER HESSENBERG FORM WITH REAL SUBDIAGONAL   
                  ELEMENTS, WHILE KEEPING B TRIANGULAR ********** */
    i__1 = nm1;
    for (k = 1; k <= i__1; ++k) {
/*        ---------------------- BEGIN TIMING CODE ---------------------- */
	opst = 0.f;
/*        ----------------------- END TIMING CODE ----------------------- */
	k1 = k + 1;
/*     ********** SET BOTTOM ELEMENT IN K-TH COLUMN OF A REAL ********** */
	if (ai_ref(*n, k) == 0.f) {
	    goto L105;
	}
	i__2 = ar_subscr(*n, k);
	i__3 = ai_subscr(*n, k);
	q__1.r = ar[i__2], q__1.i = ai[i__3];
	r__ = c_abs(&q__1);
	u1 = ar_ref(*n, k) / r__;
	u1i = ai_ref(*n, k) / r__;
	ar_ref(*n, k) = r__;
	ai_ref(*n, k) = 0.f;

	i__2 = *n;
	for (j = k1; j <= i__2; ++j) {
	    xi = u1 * ai_ref(*n, j) - u1i * ar_ref(*n, j);
	    ar_ref(*n, j) = u1 * ar_ref(*n, j) + u1i * ai_ref(*n, j);
	    ai_ref(*n, j) = xi;
/* L103: */
	}

	xi = u1 * bi_ref(*n, *n) - u1i * br_ref(*n, *n);
	br_ref(*n, *n) = u1 * br_ref(*n, *n) + u1i * bi_ref(*n, *n);
	bi_ref(*n, *n) = xi;
/*        ---------------------- BEGIN TIMING CODE ---------------------- */
	opst += (real) ((*n - k) * 6 + 18);
/*        ----------------------- END TIMING CODE ----------------------- */
L105:
	if (k == nm1) {
	    goto L170;
	}
	nk1 = nm1 - k;
/*     ********** FOR L=N-1 STEP -1 UNTIL K+1 DO -- ********** */
	i__2 = nk1;
	for (lb = 1; lb <= i__2; ++lb) {
	    l = *n - lb;
	    l1 = l + 1;
/*     ********** ZERO A(L+1,K) ********** */
	    s = (r__1 = ar_ref(l, k), dabs(r__1)) + (r__2 = ai_ref(l, k), 
		    dabs(r__2)) + ar_ref(l1, k);
	    if (s == 0.f) {
		goto L150;
	    }
/*           -------------------- BEGIN TIMING CODE --------------------- */
	    opst += (real) (((*n << 1) - k - l) * 20 + 18);
/*           --------------------- END TIMING CODE ---------------------- */
	    u1 = ar_ref(l, k) / s;
	    u1i = ai_ref(l, k) / s;
	    u2 = ar_ref(l1, k) / s;
	    r__ = sqrt(u1 * u1 + u1i * u1i + u2 * u2);
	    u1 /= r__;
	    u1i /= r__;
	    u2 /= r__;
	    ar_ref(l, k) = r__ * s;
	    ai_ref(l, k) = 0.f;
	    ar_ref(l1, k) = 0.f;

	    i__3 = *n;
	    for (j = k1; j <= i__3; ++j) {
		xr = ar_ref(l, j);
		xi = ai_ref(l, j);
		yr = ar_ref(l1, j);
		yi = ai_ref(l1, j);
		ar_ref(l, j) = u1 * xr + u1i * xi + u2 * yr;
		ai_ref(l, j) = u1 * xi - u1i * xr + u2 * yi;
		ar_ref(l1, j) = u1 * yr - u1i * yi - u2 * xr;
		ai_ref(l1, j) = u1 * yi + u1i * yr - u2 * xi;
/* L110: */
	    }

	    xr = br_ref(l, l);
	    br_ref(l, l) = u1 * xr;
	    bi_ref(l, l) = -u1i * xr;
	    br_ref(l1, l) = -u2 * xr;

	    i__3 = *n;
	    for (j = l1; j <= i__3; ++j) {
		xr = br_ref(l, j);
		xi = bi_ref(l, j);
		yr = br_ref(l1, j);
		yi = bi_ref(l1, j);
		br_ref(l, j) = u1 * xr + u1i * xi + u2 * yr;
		bi_ref(l, j) = u1 * xi - u1i * xr + u2 * yi;
		br_ref(l1, j) = u1 * yr - u1i * yi - u2 * xr;
		bi_ref(l1, j) = u1 * yi + u1i * yr - u2 * xi;
/* L120: */
	    }
/*     ********** ZERO B(L+1,L) ********** */
	    s = (r__1 = br_ref(l1, l1), dabs(r__1)) + (r__2 = bi_ref(l1, l1), 
		    dabs(r__2)) + (r__3 = br_ref(l1, l), dabs(r__3));
	    if (s == 0.f) {
		goto L150;
	    }
/*           -------------------- BEGIN TIMING CODE --------------------- */
	    opst += (real) ((*n + l) * 20 + 13);
/*           --------------------- END TIMING CODE ---------------------- */
	    u1 = br_ref(l1, l1) / s;
	    u1i = bi_ref(l1, l1) / s;
	    u2 = br_ref(l1, l) / s;
	    r__ = sqrt(u1 * u1 + u1i * u1i + u2 * u2);
	    u1 /= r__;
	    u1i /= r__;
	    u2 /= r__;
	    br_ref(l1, l1) = r__ * s;
	    bi_ref(l1, l1) = 0.f;
	    br_ref(l1, l) = 0.f;

	    i__3 = l;
	    for (i__ = 1; i__ <= i__3; ++i__) {
		xr = br_ref(i__, l1);
		xi = bi_ref(i__, l1);
		yr = br_ref(i__, l);
		yi = bi_ref(i__, l);
		br_ref(i__, l1) = u1 * xr + u1i * xi + u2 * yr;
		bi_ref(i__, l1) = u1 * xi - u1i * xr + u2 * yi;
		br_ref(i__, l) = u1 * yr - u1i * yi - u2 * xr;
		bi_ref(i__, l) = u1 * yi + u1i * yr - u2 * xi;
/* L130: */
	    }

	    i__3 = *n;
	    for (i__ = 1; i__ <= i__3; ++i__) {
		xr = ar_ref(i__, l1);
		xi = ai_ref(i__, l1);
		yr = ar_ref(i__, l);
		yi = ai_ref(i__, l);
		ar_ref(i__, l1) = u1 * xr + u1i * xi + u2 * yr;
		ai_ref(i__, l1) = u1 * xi - u1i * xr + u2 * yi;
		ar_ref(i__, l) = u1 * yr - u1i * yi - u2 * xr;
		ai_ref(i__, l) = u1 * yi + u1i * yr - u2 * xi;
/* L140: */
	    }

	    if (! (*matz)) {
		goto L150;
	    }
/*           -------------------- BEGIN TIMING CODE --------------------- */
	    opst += *n * 20;
/*           --------------------- END TIMING CODE ---------------------- */

	    i__3 = *n;
	    for (i__ = 1; i__ <= i__3; ++i__) {
		xr = zr_ref(i__, l1);
		xi = zi_ref(i__, l1);
		yr = zr_ref(i__, l);
		yi = zi_ref(i__, l);
		zr_ref(i__, l1) = u1 * xr + u1i * xi + u2 * yr;
		zi_ref(i__, l1) = u1 * xi - u1i * xr + u2 * yi;
		zr_ref(i__, l) = u1 * yr - u1i * yi - u2 * xr;
		zi_ref(i__, l) = u1 * yi + u1i * yr - u2 * xi;
/* L145: */
	    }

L150:
	    ;
	}
/*        ---------------------- BEGIN TIMING CODE ---------------------- */
	latime_1.ops += opst + (real) (*n - 1 - k << 1);
/*        ----------------------- END TIMING CODE -----------------------   

   L160: */
    }

L170:
    return 0;
/*     ********** LAST CARD OF CQZHES ********** */
} /* cqzhes_ */

#undef zr_ref
#undef zi_ref
#undef br_ref
#undef br_subscr
#undef ar_ref
#undef ar_subscr
#undef bi_ref
#undef bi_subscr
#undef ai_ref
#undef ai_subscr


/* Subroutine */ int cqzval_(integer *nm, integer *n, real *ar, real *ai, 
	real *br, real *bi, real *eps1, real *alfr, real *alfi, real *beta, 
	logical *matz, real *zr, real *zi, integer *ierr)
{
    /* System generated locals */
    integer ar_dim1, ar_offset, ai_dim1, ai_offset, br_dim1, br_offset, 
	    bi_dim1, bi_offset, zr_dim1, zr_offset, zi_dim1, zi_offset, i__1, 
	    i__2;
    real r__1, r__2, r__3, r__4;
    complex q__1, q__2, q__3, q__4, q__5, q__6, q__7;

    /* Builtin functions */
    double c_abs(complex *), sqrt(doublereal);
    void c_sqrt(complex *, complex *);
    double r_imag(complex *);
    void c_div(complex *, complex *, complex *);

    /* Local variables */
    static real b3344i, epsa, epsb, opst;
    static integer i__, j, k, l;
    static real r__, s, anorm, bnorm;
    static integer enorn;
    static real a1, a2;
    static integer k1, k2, l1, iopst;
    static real u1, u2;
    static complex z3;
    static real b11, a33, a34, a43, a44, b33, b44;
    static integer na, en;
    static real ep;
    static integer ll;
    static real sh, xi, yi, xr, yr, a1i;
    static integer km1, lm1;
    static real u1i, a33i, a34i, a43i, a44i, b33i, b44i, b3344, ani, bni, shi;
    static integer its, enm2, lor1;


#define ai_subscr(a_1,a_2) (a_2)*ai_dim1 + a_1
#define ai_ref(a_1,a_2) ai[(a_2)*ai_dim1 + a_1]
#define bi_subscr(a_1,a_2) (a_2)*bi_dim1 + a_1
#define bi_ref(a_1,a_2) bi[(a_2)*bi_dim1 + a_1]
#define ar_subscr(a_1,a_2) (a_2)*ar_dim1 + a_1
#define ar_ref(a_1,a_2) ar[(a_2)*ar_dim1 + a_1]
#define br_subscr(a_1,a_2) (a_2)*br_dim1 + a_1
#define br_ref(a_1,a_2) br[(a_2)*br_dim1 + a_1]
#define zi_ref(a_1,a_2) zi[(a_2)*zi_dim1 + a_1]
#define zr_ref(a_1,a_2) zr[(a_2)*zr_dim1 + a_1]



/*     ----------------------- BEGIN TIMING CODE ------------------------   
       COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED   
       OPST IS USED TO ACCUMULATE SMALL CONTRIBUTIONS TO OPS   
       TO AVOID ROUNDOFF ERROR   
       ------------------------ END TIMING CODE -------------------------   






       THIS SUBROUTINE IS A COMPLEX ANALOGUE OF STEPS 2 AND 3 OF THE   
       QZ ALGORITHM FOR SOLVING GENERALIZED MATRIX EIGENVALUE PROBLEMS,   
       SIAM J. NUMER. ANAL. 10, 241-256(1973) BY MOLER AND STEWART,   
       AS MODIFIED IN TECHNICAL NOTE NASA TN E-7305(1973) BY WARD.   

       THIS SUBROUTINE ACCEPTS A PAIR OF COMPLEX MATRICES, ONE OF THEM   
       IN UPPER HESSENBERG FORM AND THE OTHER IN UPPER TRIANGULAR FORM,   
       THE HESSENBERG MATRIX MUST FURTHER HAVE REAL SUBDIAGONAL ELEMENTS.   
       IT REDUCES THE HESSENBERG MATRIX TO TRIANGULAR FORM USING   
       UNITARY TRANSFORMATIONS WHILE MAINTAINING THE TRIANGULAR FORM   
       OF THE OTHER MATRIX AND FURTHER MAKING ITS DIAGONAL ELEMENTS   
       REAL AND NON-NEGATIVE.  IT THEN RETURNS QUANTITIES WHOSE RATIOS   
       GIVE THE GENERALIZED EIGENVALUES.  IT IS USUALLY PRECEDED BY   
       CQZHES  AND POSSIBLY FOLLOWED BY  CQZVEC.   

       ON INPUT-   

          NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL   
            ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM   
            DIMENSION STATEMENT,   

          N IS THE ORDER OF THE MATRICES,   

          A=(AR,AI) CONTAINS A COMPLEX UPPER HESSENBERG MATRIX   
            WITH REAL SUBDIAGONAL ELEMENTS,   

          B=(BR,BI) CONTAINS A COMPLEX UPPER TRIANGULAR MATRIX,   

          EPS1 IS A TOLERANCE USED TO DETERMINE NEGLIGIBLE ELEMENTS.   
            EPS1 = 0.0 (OR NEGATIVE) MAY BE INPUT, IN WHICH CASE AN   
            ELEMENT WILL BE NEGLECTED ONLY IF IT IS LESS THAN ROUNDOFF   
            ERROR TIMES THE NORM OF ITS MATRIX.  IF THE INPUT EPS1 IS   
            POSITIVE, THEN AN ELEMENT WILL BE CONSIDERED NEGLIGIBLE   
            IF IT IS LESS THAN EPS1 TIMES THE NORM OF ITS MATRIX.  A   
            POSITIVE VALUE OF EPS1 MAY RESULT IN FASTER EXECUTION,   
            BUT LESS ACCURATE RESULTS,   

          MATZ SHOULD BE SET TO .TRUE. IF THE RIGHT HAND TRANSFORMATIONS   
            ARE TO BE ACCUMULATED FOR LATER USE IN COMPUTING   
            EIGENVECTORS, AND TO .FALSE. OTHERWISE,   

          Z=(ZR,ZI) CONTAINS, IF MATZ HAS BEEN SET TO .TRUE., THE   
            TRANSFORMATION MATRIX PRODUCED IN THE REDUCTION   
            BY  CQZHES, IF PERFORMED, OR ELSE THE IDENTITY MATRIX.   
            IF MATZ HAS BEEN SET TO .FALSE., Z IS NOT REFERENCED.   

       ON OUTPUT-   

          A HAS BEEN REDUCED TO UPPER TRIANGULAR FORM.  THE ELEMENTS   
            BELOW THE MAIN DIAGONAL HAVE BEEN SET TO ZERO,   

          B IS STILL IN UPPER TRIANGULAR FORM, ALTHOUGH ITS ELEMENTS   
            HAVE BEEN ALTERED.  IN PARTICULAR, ITS DIAGONAL HAS BEEN SET   
            REAL AND NON-NEGATIVE.  THE LOCATION BR(N,1) IS USED TO   
            STORE EPS1 TIMES THE NORM OF B FOR LATER USE BY  CQZVEC,   

          ALFR AND ALFI CONTAIN THE REAL AND IMAGINARY PARTS OF THE   
            DIAGONAL ELEMENTS OF THE TRIANGULARIZED A MATRIX,   

          BETA CONTAINS THE REAL NON-NEGATIVE DIAGONAL ELEMENTS OF THE   
            CORRESPONDING B.  THE GENERALIZED EIGENVALUES ARE THEN   
            THE RATIOS ((ALFR+I*ALFI)/BETA),   

          Z CONTAINS THE PRODUCT OF THE RIGHT HAND TRANSFORMATIONS   
            (FOR BOTH STEPS) IF MATZ HAS BEEN SET TO .TRUE.,   

          IERR IS SET TO   
            ZERO       FOR NORMAL RETURN,   
            J          IF AR(J,J-1) HAS NOT BECOME   
                       ZERO AFTER 50 ITERATIONS.   

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,   
       APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY   

       ------------------------------------------------------------------   

       Parameter adjustments */
    zi_dim1 = *nm;
    zi_offset = 1 + zi_dim1 * 1;
    zi -= zi_offset;
    zr_dim1 = *nm;
    zr_offset = 1 + zr_dim1 * 1;
    zr -= zr_offset;
    --beta;
    --alfi;
    --alfr;
    bi_dim1 = *nm;
    bi_offset = 1 + bi_dim1 * 1;
    bi -= bi_offset;
    br_dim1 = *nm;
    br_offset = 1 + br_dim1 * 1;
    br -= br_offset;
    ai_dim1 = *nm;
    ai_offset = 1 + ai_dim1 * 1;
    ai -= ai_offset;
    ar_dim1 = *nm;
    ar_offset = 1 + ar_dim1 * 1;
    ar -= ar_offset;

    /* Function Body */
    *ierr = 0;
/*     ********** COMPUTE EPSA,EPSB ********** */
    anorm = 0.f;
    bnorm = 0.f;

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	ani = 0.f;
	if (i__ != 1) {
	    ani = (r__1 = ar_ref(i__, i__ - 1), dabs(r__1));
	}
	bni = 0.f;

	i__2 = *n;
	for (j = i__; j <= i__2; ++j) {
	    ani = ani + (r__1 = ar_ref(i__, j), dabs(r__1)) + (r__2 = ai_ref(
		    i__, j), dabs(r__2));
	    bni = bni + (r__1 = br_ref(i__, j), dabs(r__1)) + (r__2 = bi_ref(
		    i__, j), dabs(r__2));
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
/*     ********** COMPUTE ROUNDOFF LEVEL IF EPS1 IS ZERO ********** */
    ep = 1.f;
L40:
    ep /= 2.f;
    if (ep + 1.f > 1.f) {
	goto L40;
    }
L50:
    epsa = ep * anorm;
    epsb = ep * bnorm;
/*     ----------------------- BEGIN TIMING CODE ------------------------   
       COUNT OPS FOR NORMS, BUT NOT FOR CALCULATION OF "EP" */
    latime_1.ops += (real) ((*n << 1) * (*n + 1) + 2);
    opst = 0.f;
    latime_1.itcnt = 0.f;
/*     ------------------------ END TIMING CODE -------------------------   
       ********** REDUCE A TO TRIANGULAR FORM, WHILE   
                  KEEPING B TRIANGULAR ********** */
    lor1 = 1;
    enorn = *n;
    en = *n;
/*     ********** BEGIN QZ STEP ********** */
L60:
    if (en == 0) {
	goto L1001;
    }
    if (! (*matz)) {
	enorn = en;
    }
    its = 0;
    na = en - 1;
    enm2 = na - 1;
/*     ********** CHECK FOR CONVERGENCE OR REDUCIBILITY.   
                  FOR L=EN STEP -1 UNTIL 1 DO -- ********** */
L70:
/*     ----------------------- BEGIN TIMING CODE ------------------------ */
    latime_1.ops += opst;
    opst = 0.f;
/*     ------------------------ END TIMING CODE ------------------------- */
    i__1 = en;
    for (ll = 1; ll <= i__1; ++ll) {
	lm1 = en - ll;
	l = lm1 + 1;
	if (l == 1) {
	    goto L95;
	}
	if ((r__1 = ar_ref(l, lm1), dabs(r__1)) <= epsa) {
	    goto L90;
	}
/* L80: */
    }

L90:
    ar_ref(l, lm1) = 0.f;
/*     ********** SET DIAGONAL ELEMENT AT TOP OF B REAL ********** */
L95:
    i__1 = br_subscr(l, l);
    i__2 = bi_subscr(l, l);
    q__1.r = br[i__1], q__1.i = bi[i__2];
    b11 = c_abs(&q__1);
    if (b11 == 0.f) {
	goto L98;
    }
    u1 = br_ref(l, l) / b11;
    u1i = bi_ref(l, l) / b11;

    i__1 = enorn;
    for (j = l; j <= i__1; ++j) {
	xi = u1 * ai_ref(l, j) - u1i * ar_ref(l, j);
	ar_ref(l, j) = u1 * ar_ref(l, j) + u1i * ai_ref(l, j);
	ai_ref(l, j) = xi;
	xi = u1 * bi_ref(l, j) - u1i * br_ref(l, j);
	br_ref(l, j) = u1 * br_ref(l, j) + u1i * bi_ref(l, j);
	bi_ref(l, j) = xi;
/* L97: */
    }
/*     ----------------------- BEGIN TIMING CODE ------------------------ */
    opst += (real) ((enorn + 1 - l) * 12 + 7);
/*     ------------------------ END TIMING CODE ------------------------- */

    bi_ref(l, l) = 0.f;
L98:
    if (l != en) {
	goto L100;
    }
/*     ********** 1-BY-1 BLOCK ISOLATED ********** */
    alfr[en] = ar_ref(en, en);
    alfi[en] = ai_ref(en, en);
    beta[en] = b11;
    en = na;
    goto L60;
/*     ********** CHECK FOR SMALL TOP OF B ********** */
L100:
    l1 = l + 1;
    if (b11 > epsb) {
	goto L120;
    }
    br_ref(l, l) = 0.f;
    s = (r__1 = ar_ref(l, l), dabs(r__1)) + (r__2 = ai_ref(l, l), dabs(r__2)) 
	    + (r__3 = ar_ref(l1, l), dabs(r__3));
    u1 = ar_ref(l, l) / s;
    u1i = ai_ref(l, l) / s;
    u2 = ar_ref(l1, l) / s;
    r__ = sqrt(u1 * u1 + u1i * u1i + u2 * u2);
    u1 /= r__;
    u1i /= r__;
    u2 /= r__;
    ar_ref(l, l) = r__ * s;
    ai_ref(l, l) = 0.f;

    i__1 = enorn;
    for (j = l1; j <= i__1; ++j) {
	xr = ar_ref(l, j);
	xi = ai_ref(l, j);
	yr = ar_ref(l1, j);
	yi = ai_ref(l1, j);
	ar_ref(l, j) = u1 * xr + u1i * xi + u2 * yr;
	ai_ref(l, j) = u1 * xi - u1i * xr + u2 * yi;
	ar_ref(l1, j) = u1 * yr - u1i * yi - u2 * xr;
	ai_ref(l1, j) = u1 * yi + u1i * yr - u2 * xi;
	xr = br_ref(l, j);
	xi = bi_ref(l, j);
	yr = br_ref(l1, j);
	yi = bi_ref(l1, j);
	br_ref(l1, j) = u1 * yr - u1i * yi - u2 * xr;
	br_ref(l, j) = u1 * xr + u1i * xi + u2 * yr;
	bi_ref(l, j) = u1 * xi - u1i * xr + u2 * yi;
	bi_ref(l1, j) = u1 * yi + u1i * yr - u2 * xi;
/* L110: */
    }
/*     ----------------------- BEGIN TIMING CODE ------------------------ */
    opst += (real) ((enorn - l) * 40 + 15);
/*     ------------------------ END TIMING CODE ------------------------- */

    lm1 = l;
    l = l1;
    goto L90;
/*     ********** ITERATION STRATEGY ********** */
L120:
    if (its == 50) {
	goto L1000;
    }
    if (its == 10) {
	goto L135;
    }
/*     ********** DETERMINE SHIFT ********** */
    b33 = br_ref(na, na);
    b33i = bi_ref(na, na);
    q__1.r = b33, q__1.i = b33i;
    if (c_abs(&q__1) >= epsb) {
	goto L122;
    }
    b33 = epsb;
    b33i = 0.f;
L122:
    b44 = br_ref(en, en);
    b44i = bi_ref(en, en);
    q__1.r = b44, q__1.i = b44i;
    if (c_abs(&q__1) >= epsb) {
	goto L124;
    }
    b44 = epsb;
    b44i = 0.f;
L124:
    b3344 = b33 * b44 - b33i * b44i;
    b3344i = b33 * b44i + b33i * b44;
    a33 = ar_ref(na, na) * b44 - ai_ref(na, na) * b44i;
    a33i = ar_ref(na, na) * b44i + ai_ref(na, na) * b44;
    a34 = ar_ref(na, en) * b33 - ai_ref(na, en) * b33i - ar_ref(na, na) * 
	    br_ref(na, en) + ai_ref(na, na) * bi_ref(na, en);
    a34i = ar_ref(na, en) * b33i + ai_ref(na, en) * b33 - ar_ref(na, na) * 
	    bi_ref(na, en) - ai_ref(na, na) * br_ref(na, en);
    a43 = ar_ref(en, na) * b44;
    a43i = ar_ref(en, na) * b44i;
    a44 = ar_ref(en, en) * b33 - ai_ref(en, en) * b33i - ar_ref(en, na) * 
	    br_ref(na, en);
    a44i = ar_ref(en, en) * b33i + ai_ref(en, en) * b33 - ar_ref(en, na) * 
	    bi_ref(na, en);
    sh = a44;
    shi = a44i;
    xr = a34 * a43 - a34i * a43i;
    xi = a34 * a43i + a34i * a43;
/*     ----------------------- BEGIN TIMING CODE ------------------------ */
    opst += 54.f;
/*     ------------------------ END TIMING CODE ------------------------- */
    if (xr == 0.f && xi == 0.f) {
	goto L140;
    }
    yr = (a33 - sh) / 2.f;
    yi = (a33i - shi) / 2.f;
/* Computing 2nd power */
    r__2 = yr;
/* Computing 2nd power */
    r__3 = yi;
    r__1 = r__2 * r__2 - r__3 * r__3 + xr;
    r__4 = yr * 2.f * yi + xi;
    q__2.r = r__1, q__2.i = r__4;
    c_sqrt(&q__1, &q__2);
    z3.r = q__1.r, z3.i = q__1.i;
    u1 = z3.r;
    u1i = r_imag(&z3);
    if (yr * u1 + yi * u1i >= 0.f) {
	goto L125;
    }
    u1 = -u1;
    u1i = -u1i;
L125:
    q__3.r = sh, q__3.i = shi;
    q__5.r = xr, q__5.i = xi;
    r__1 = yr + u1;
    r__2 = yi + u1i;
    q__6.r = r__1, q__6.i = r__2;
    c_div(&q__4, &q__5, &q__6);
    q__2.r = q__3.r - q__4.r, q__2.i = q__3.i - q__4.i;
    q__7.r = b3344, q__7.i = b3344i;
    c_div(&q__1, &q__2, &q__7);
    z3.r = q__1.r, z3.i = q__1.i;
    sh = z3.r;
    shi = r_imag(&z3);
/*     ----------------------- BEGIN TIMING CODE ------------------------ */
    opst += 66.f;
/*     ------------------------ END TIMING CODE ------------------------- */
    goto L140;
/*     ********** AD HOC SHIFT ********** */
L135:
    sh = ar_ref(en, na) + ar_ref(na, enm2);
    shi = 0.f;
/*     ********** DETERMINE ZEROTH COLUMN OF A ********** */
L140:
    a1 = ar_ref(l, l) / b11 - sh;
    a1i = ai_ref(l, l) / b11 - shi;
    a2 = ar_ref(l1, l) / b11;
    ++its;
/*     ----------------------- BEGIN TIMING CODE ------------------------ */
    latime_1.itcnt += 1.f;
/*     ------------------------ END TIMING CODE ------------------------- */
    if (! (*matz)) {
	lor1 = l;
    }
/*     ********** MAIN LOOP ********** */
    i__1 = na;
    for (k = l; k <= i__1; ++k) {
	k1 = k + 1;
	k2 = k + 2;
/* Computing MAX */
	i__2 = k - 1;
	km1 = max(i__2,l);
/*     ********** ZERO A(K+1,K-1) ********** */
	if (k == l) {
	    goto L170;
	}
	a1 = ar_ref(k, km1);
	a1i = ai_ref(k, km1);
	a2 = ar_ref(k1, km1);
L170:
	s = dabs(a1) + dabs(a1i) + dabs(a2);
	u1 = a1 / s;
	u1i = a1i / s;
	u2 = a2 / s;
	r__ = sqrt(u1 * u1 + u1i * u1i + u2 * u2);
	u1 /= r__;
	u1i /= r__;
	u2 /= r__;

	i__2 = enorn;
	for (j = km1; j <= i__2; ++j) {
	    xr = ar_ref(k, j);
	    xi = ai_ref(k, j);
	    yr = ar_ref(k1, j);
	    yi = ai_ref(k1, j);
	    ar_ref(k, j) = u1 * xr + u1i * xi + u2 * yr;
	    ai_ref(k, j) = u1 * xi - u1i * xr + u2 * yi;
	    ar_ref(k1, j) = u1 * yr - u1i * yi - u2 * xr;
	    ai_ref(k1, j) = u1 * yi + u1i * yr - u2 * xi;
	    xr = br_ref(k, j);
	    xi = bi_ref(k, j);
	    yr = br_ref(k1, j);
	    yi = bi_ref(k1, j);
	    br_ref(k, j) = u1 * xr + u1i * xi + u2 * yr;
	    bi_ref(k, j) = u1 * xi - u1i * xr + u2 * yi;
	    br_ref(k1, j) = u1 * yr - u1i * yi - u2 * xr;
	    bi_ref(k1, j) = u1 * yi + u1i * yr - u2 * xi;
/* L180: */
	}

	if (k == l) {
	    goto L240;
	}
	ai_ref(k, km1) = 0.f;
	ar_ref(k1, km1) = 0.f;
	ai_ref(k1, km1) = 0.f;
/*     ********** ZERO B(K+1,K) ********** */
L240:
	s = (r__1 = br_ref(k1, k1), dabs(r__1)) + (r__2 = bi_ref(k1, k1), 
		dabs(r__2)) + (r__3 = br_ref(k1, k), dabs(r__3));
	u1 = br_ref(k1, k1) / s;
	u1i = bi_ref(k1, k1) / s;
	u2 = br_ref(k1, k) / s;
	r__ = sqrt(u1 * u1 + u1i * u1i + u2 * u2);
	u1 /= r__;
	u1i /= r__;
	u2 /= r__;
	if (k == na) {
	    goto L245;
	}
	xr = ar_ref(k2, k1);
	ar_ref(k2, k1) = u1 * xr;
	ai_ref(k2, k1) = -u1i * xr;
	ar_ref(k2, k) = -u2 * xr;

L245:
	i__2 = k1;
	for (i__ = lor1; i__ <= i__2; ++i__) {
	    xr = ar_ref(i__, k1);
	    xi = ai_ref(i__, k1);
	    yr = ar_ref(i__, k);
	    yi = ai_ref(i__, k);
	    ar_ref(i__, k1) = u1 * xr + u1i * xi + u2 * yr;
	    ai_ref(i__, k1) = u1 * xi - u1i * xr + u2 * yi;
	    ar_ref(i__, k) = u1 * yr - u1i * yi - u2 * xr;
	    ai_ref(i__, k) = u1 * yi + u1i * yr - u2 * xi;
	    xr = br_ref(i__, k1);
	    xi = bi_ref(i__, k1);
	    yr = br_ref(i__, k);
	    yi = bi_ref(i__, k);
	    br_ref(i__, k1) = u1 * xr + u1i * xi + u2 * yr;
	    bi_ref(i__, k1) = u1 * xi - u1i * xr + u2 * yi;
	    br_ref(i__, k) = u1 * yr - u1i * yi - u2 * xr;
	    bi_ref(i__, k) = u1 * yi + u1i * yr - u2 * xi;
/* L250: */
	}

	bi_ref(k1, k1) = 0.f;
	br_ref(k1, k) = 0.f;
	bi_ref(k1, k) = 0.f;
	if (! (*matz)) {
	    goto L260;
	}

	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    xr = zr_ref(i__, k1);
	    xi = zi_ref(i__, k1);
	    yr = zr_ref(i__, k);
	    yi = zi_ref(i__, k);
	    zr_ref(i__, k1) = u1 * xr + u1i * xi + u2 * yr;
	    zi_ref(i__, k1) = u1 * xi - u1i * xr + u2 * yi;
	    zr_ref(i__, k) = u1 * yr - u1i * yi - u2 * xr;
	    zi_ref(i__, k) = u1 * yi + u1i * yr - u2 * xi;
/* L255: */
	}

L260:
	;
    }

/*     ----------------------- BEGIN TIMING CODE ------------------------   
       COUNT OPS FOR STATEMENTS 140 -- 260 */
    iopst = (enorn - lor1 + 4) * 40 + 29;
    if (*matz) {
	iopst += *n * 20;
    }
    opst += (real) (*n - l) * (real) iopst + 2;
    if (l <= 1) {
	opst += -40;
    }
/*     ------------------------ END TIMING CODE -------------------------   

       ********** SET LAST A SUBDIAGONAL REAL AND END QZ STEP ********** */
    if (ai_ref(en, na) == 0.f) {
	goto L70;
    }
    i__1 = ar_subscr(en, na);
    i__2 = ai_subscr(en, na);
    q__1.r = ar[i__1], q__1.i = ai[i__2];
    r__ = c_abs(&q__1);
    u1 = ar_ref(en, na) / r__;
    u1i = ai_ref(en, na) / r__;
    ar_ref(en, na) = r__;
    ai_ref(en, na) = 0.f;

    i__1 = enorn;
    for (j = en; j <= i__1; ++j) {
	xi = u1 * ai_ref(en, j) - u1i * ar_ref(en, j);
	ar_ref(en, j) = u1 * ar_ref(en, j) + u1i * ai_ref(en, j);
	ai_ref(en, j) = xi;
	xi = u1 * bi_ref(en, j) - u1i * br_ref(en, j);
	br_ref(en, j) = u1 * br_ref(en, j) + u1i * bi_ref(en, j);
	bi_ref(en, j) = xi;
/* L270: */
    }
/*     ----------------------- BEGIN TIMING CODE ------------------------ */
    opst += (real) ((en + 1 - enorn) * 12 + 7);
/*     ------------------------ END TIMING CODE ------------------------- */

    goto L70;
/*     ********** SET ERROR -- BOTTOM SUBDIAGONAL ELEMENT HAS NOT   
                  BECOME NEGLIGIBLE AFTER 50 ITERATIONS ********** */
L1000:
    *ierr = en;
/*     ********** SAVE EPSB FOR USE BY CQZVEC ********** */
L1001:
    if (*n > 1) {
	br_ref(*n, 1) = epsb;
    }
/*     ----------------------- BEGIN TIMING CODE ------------------------ */
    latime_1.ops += opst;
    opst = 0.f;
/*     ------------------------ END TIMING CODE ------------------------- */
    return 0;
/*     ********** LAST CARD OF CQZVAL ********** */
} /* cqzval_ */

#undef zr_ref
#undef zi_ref
#undef br_ref
#undef br_subscr
#undef ar_ref
#undef ar_subscr
#undef bi_ref
#undef bi_subscr
#undef ai_ref
#undef ai_subscr


/* Subroutine */ int cqzvec_(integer *nm, integer *n, real *ar, real *ai, 
	real *br, real *bi, real *alfr, real *alfi, real *beta, real *zr, 
	real *zi)
{
    /* System generated locals */
    integer ar_dim1, ar_offset, ai_dim1, ai_offset, br_dim1, br_offset, 
	    bi_dim1, bi_offset, zr_dim1, zr_offset, zi_dim1, zi_offset, i__1, 
	    i__2, i__3, i__4;
    complex q__1, q__2, q__3;

    /* Builtin functions */
    void c_div(complex *, complex *, complex *);
    double r_imag(complex *), c_abs(complex *);

    /* Local variables */
    static real almi, betm, epsb, almr;
    static integer i__, j, k, m;
    static real r__, t;
    static complex z3;
    static integer na, ii, en, jj, nn;
    static real ri, ti, xi;


#define ai_ref(a_1,a_2) ai[(a_2)*ai_dim1 + a_1]
#define bi_ref(a_1,a_2) bi[(a_2)*bi_dim1 + a_1]
#define ar_ref(a_1,a_2) ar[(a_2)*ar_dim1 + a_1]
#define br_ref(a_1,a_2) br[(a_2)*br_dim1 + a_1]
#define zi_subscr(a_1,a_2) (a_2)*zi_dim1 + a_1
#define zi_ref(a_1,a_2) zi[(a_2)*zi_dim1 + a_1]
#define zr_subscr(a_1,a_2) (a_2)*zr_dim1 + a_1
#define zr_ref(a_1,a_2) zr[(a_2)*zr_dim1 + a_1]





/*     ----------------------- BEGIN TIMING CODE ------------------------   
       COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED   
       OPST IS USED TO ACCUMULATE SMALL CONTRIBUTIONS TO OPS   
       TO AVOID ROUNDOFF ERROR   
       ------------------------ END TIMING CODE -------------------------   




       THIS SUBROUTINE IS A COMPLEX ANALOGUE OF THE FOURTH STEP OF THE   
       QZ ALGORITHM FOR SOLVING GENERALIZED MATRIX EIGENVALUE PROBLEMS,   
       SIAM J. NUMER. ANAL. 10, 241-256(1973) BY MOLER AND STEWART.   

       THIS SUBROUTINE ACCEPTS A PAIR OF COMPLEX MATRICES IN UPPER   
       TRIANGULAR FORM, WHERE ONE OF THEM FURTHER MUST HAVE REAL DIAGONAL   
       ELEMENTS.  IT COMPUTES THE EIGENVECTORS OF THE TRIANGULAR PROBLEM   
       AND TRANSFORMS THE RESULTS BACK TO THE ORIGINAL COORDINATE SYSTEM.   
       IT IS USUALLY PRECEDED BY  CQZHES  AND  CQZVAL.   

       ON INPUT-   

          NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL   
            ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM   
            DIMENSION STATEMENT,   

          N IS THE ORDER OF THE MATRICES,   

          A=(AR,AI) CONTAINS A COMPLEX UPPER TRIANGULAR MATRIX,   

          B=(BR,BI) CONTAINS A COMPLEX UPPER TRIANGULAR MATRIX WITH REAL   
            DIAGONAL ELEMENTS.  IN ADDITION, LOCATION BR(N,1) CONTAINS   
            THE TOLERANCE QUANTITY (EPSB) COMPUTED AND SAVED IN  CQZVAL,   

          ALFR, ALFI, AND BETA ARE VECTORS WITH COMPONENTS WHOSE   
            RATIOS ((ALFR+I*ALFI)/BETA) ARE THE GENERALIZED   
            EIGENVALUES.  THEY ARE USUALLY OBTAINED FROM  CQZVAL,   

          Z=(ZR,ZI) CONTAINS THE TRANSFORMATION MATRIX PRODUCED IN THE   
            REDUCTIONS BY  CQZHES  AND  CQZVAL, IF PERFORMED.   
            IF THE EIGENVECTORS OF THE TRIANGULAR PROBLEM ARE   
            DESIRED, Z MUST CONTAIN THE IDENTITY MATRIX.   

       ON OUTPUT-   

          A IS UNALTERED,   

          B HAS BEEN DESTROYED,   

          ALFR, ALFI, AND BETA ARE UNALTERED,   

          Z CONTAINS THE EIGENVECTORS.  EACH EIGENVECTOR IS NORMALIZED   
            SO THAT THE MODULUS OF ITS LARGEST COMPONENT IS 1.0 .   

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,   
       APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY   

       ------------------------------------------------------------------   

       Parameter adjustments */
    zi_dim1 = *nm;
    zi_offset = 1 + zi_dim1 * 1;
    zi -= zi_offset;
    zr_dim1 = *nm;
    zr_offset = 1 + zr_dim1 * 1;
    zr -= zr_offset;
    --beta;
    --alfi;
    --alfr;
    bi_dim1 = *nm;
    bi_offset = 1 + bi_dim1 * 1;
    bi -= bi_offset;
    br_dim1 = *nm;
    br_offset = 1 + br_dim1 * 1;
    br -= br_offset;
    ai_dim1 = *nm;
    ai_offset = 1 + ai_dim1 * 1;
    ai -= ai_offset;
    ar_dim1 = *nm;
    ar_offset = 1 + ar_dim1 * 1;
    ar -= ar_offset;

    /* Function Body */
    if (*n <= 1) {
	goto L1001;
    }
    epsb = br_ref(*n, 1);
/*     ********** FOR EN=N STEP -1 UNTIL 2 DO -- ********** */
    i__1 = *n;
    for (nn = 2; nn <= i__1; ++nn) {
	en = *n + 2 - nn;
	na = en - 1;
	almr = alfr[en];
	almi = alfi[en];
	betm = beta[en];
/*     ********** FOR I=EN-1 STEP -1 UNTIL 1 DO -- ********** */
	i__2 = na;
	for (ii = 1; ii <= i__2; ++ii) {
	    i__ = en - ii;
	    r__ = 0.f;
	    ri = 0.f;
	    m = i__ + 1;

	    i__3 = en;
	    for (j = m; j <= i__3; ++j) {
		t = betm * ar_ref(i__, j) - almr * br_ref(i__, j) + almi * 
			bi_ref(i__, j);
		ti = betm * ai_ref(i__, j) - almr * bi_ref(i__, j) - almi * 
			br_ref(i__, j);
		if (j == en) {
		    goto L605;
		}
		xi = t * bi_ref(j, en) + ti * br_ref(j, en);
		t = t * br_ref(j, en) - ti * bi_ref(j, en);
		ti = xi;
L605:
		r__ += t;
		ri += ti;
/* L610: */
	    }

	    t = almr * beta[i__] - betm * alfr[i__];
	    ti = almi * beta[i__] - betm * alfi[i__];
	    if (t == 0.f && ti == 0.f) {
		t = epsb;
	    }
	    q__2.r = r__, q__2.i = ri;
	    q__3.r = t, q__3.i = ti;
	    c_div(&q__1, &q__2, &q__3);
	    z3.r = q__1.r, z3.i = q__1.i;
	    br_ref(i__, en) = z3.r;
	    bi_ref(i__, en) = r_imag(&z3);
/* L700: */
	}

/* L800: */
    }
/*     ********** END BACK SUBSTITUTION.   
                  TRANSFORM TO ORIGINAL COORDINATE SYSTEM.   
                  FOR J=N STEP -1 UNTIL 2 DO -- ********** */
    i__1 = *n;
    for (jj = 2; jj <= i__1; ++jj) {
	j = *n + 2 - jj;
	m = j - 1;

	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {

	    i__3 = m;
	    for (k = 1; k <= i__3; ++k) {
		zr_ref(i__, j) = zr_ref(i__, j) + zr_ref(i__, k) * br_ref(k, 
			j) - zi_ref(i__, k) * bi_ref(k, j);
		zi_ref(i__, j) = zi_ref(i__, j) + zr_ref(i__, k) * bi_ref(k, 
			j) + zi_ref(i__, k) * br_ref(k, j);
/* L860: */
	    }

/* L880: */
	}
    }
/*     ********** NORMALIZE SO THAT MODULUS OF LARGEST   
                  COMPONENT OF EACH VECTOR IS 1 ********** */
    i__2 = *n;
    for (j = 1; j <= i__2; ++j) {
	t = 0.f;

	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    i__3 = zr_subscr(i__, j);
	    i__4 = zi_subscr(i__, j);
	    q__1.r = zr[i__3], q__1.i = zi[i__4];
	    r__ = c_abs(&q__1);
	    if (r__ > t) {
		t = r__;
	    }
/* L930: */
	}

	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    zr_ref(i__, j) = zr_ref(i__, j) / t;
	    zi_ref(i__, j) = zi_ref(i__, j) / t;
/* L940: */
	}

/* L950: */
    }

L1001:

/*     ----------------------- BEGIN TIMING CODE ------------------------   
   Computing 2nd power */
    i__2 = *n;
    latime_1.ops += (real) (*n) * (real) (i__2 * i__2 * 14 + *n * 15 - 15) / 
	    2.f;
/*     ------------------------ END TIMING CODE ------------------------- */

    return 0;
/*     ********** LAST CARD OF CQZVEC ********** */
} /* cqzvec_ */

#undef zr_ref
#undef zr_subscr
#undef zi_ref
#undef zi_subscr
#undef br_ref
#undef ar_ref
#undef bi_ref
#undef ai_ref


