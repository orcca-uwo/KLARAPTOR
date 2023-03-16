#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* Subroutine */ int cpbt05_(char *uplo, integer *n, integer *kd, integer *
	nrhs, complex *ab, integer *ldab, complex *b, integer *ldb, complex *
	x, integer *ldx, complex *xact, integer *ldxact, real *ferr, real *
	berr, real *reslts)
{
    /* System generated locals */
    integer ab_dim1, ab_offset, b_dim1, b_offset, x_dim1, x_offset, xact_dim1,
	     xact_offset, i__1, i__2, i__3, i__4, i__5;
    real r__1, r__2, r__3, r__4;
    complex q__1, q__2;

    /* Builtin functions */
    double r_imag(complex *);

    /* Local variables */
    static real diff, axbi;
    static integer imax;
    static real unfl, ovfl;
    static integer i__, j, k;
    extern logical lsame_(char *, char *);
    static logical upper;
    static real xnorm;
    extern integer icamax_(integer *, complex *, integer *);
    extern doublereal slamch_(char *);
    static integer nz;
    static real errbnd, eps, tmp;


#define xact_subscr(a_1,a_2) (a_2)*xact_dim1 + a_1
#define xact_ref(a_1,a_2) xact[xact_subscr(a_1,a_2)]
#define b_subscr(a_1,a_2) (a_2)*b_dim1 + a_1
#define b_ref(a_1,a_2) b[b_subscr(a_1,a_2)]
#define x_subscr(a_1,a_2) (a_2)*x_dim1 + a_1
#define x_ref(a_1,a_2) x[x_subscr(a_1,a_2)]
#define ab_subscr(a_1,a_2) (a_2)*ab_dim1 + a_1
#define ab_ref(a_1,a_2) ab[ab_subscr(a_1,a_2)]


/*  -- LAPACK test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    CPBT05 tests the error bounds from iterative refinement for the   
    computed solution to a system of equations A*X = B, where A is a   
    Hermitian band matrix.   

    RESLTS(1) = test of the error bound   
              = norm(X - XACT) / ( norm(X) * FERR )   

    A large value is returned if this ratio is not less than one.   

    RESLTS(2) = residual from the iterative refinement routine   
              = the maximum of BERR / ( NZ*EPS + (*) ), where   
                (*) = NZ*UNFL / (min_i (abs(A)*abs(X) +abs(b))_i )   
                and NZ = max. number of nonzeros in any row of A, plus 1   

    Arguments   
    =========   

    UPLO    (input) CHARACTER*1   
            Specifies whether the upper or lower triangular part of the   
            Hermitian matrix A is stored.   
            = 'U':  Upper triangular   
            = 'L':  Lower triangular   

    N       (input) INTEGER   
            The number of rows of the matrices X, B, and XACT, and the   
            order of the matrix A.  N >= 0.   

    KD      (input) INTEGER   
            The number of super-diagonals of the matrix A if UPLO = 'U',   
            or the number of sub-diagonals if UPLO = 'L'.  KD >= 0.   

    NRHS    (input) INTEGER   
            The number of columns of the matrices X, B, and XACT.   
            NRHS >= 0.   

    AB      (input) COMPLEX array, dimension (LDAB,N)   
            The upper or lower triangle of the Hermitian band matrix A,   
            stored in the first KD+1 rows of the array.  The j-th column   
            of A is stored in the j-th column of the array AB as follows:   
            if UPLO = 'U', AB(kd+1+i-j,j) = A(i,j) for max(1,j-kd)<=i<=j;   
            if UPLO = 'L', AB(1+i-j,j)    = A(i,j) for j<=i<=min(n,j+kd).   

    LDAB    (input) INTEGER   
            The leading dimension of the array AB.  LDAB >= KD+1.   

    B       (input) COMPLEX array, dimension (LDB,NRHS)   
            The right hand side vectors for the system of linear   
            equations.   

    LDB     (input) INTEGER   
            The leading dimension of the array B.  LDB >= max(1,N).   

    X       (input) COMPLEX array, dimension (LDX,NRHS)   
            The computed solution vectors.  Each vector is stored as a   
            column of the matrix X.   

    LDX     (input) INTEGER   
            The leading dimension of the array X.  LDX >= max(1,N).   

    XACT    (input) COMPLEX array, dimension (LDX,NRHS)   
            The exact solution vectors.  Each vector is stored as a   
            column of the matrix XACT.   

    LDXACT  (input) INTEGER   
            The leading dimension of the array XACT.  LDXACT >= max(1,N).   

    FERR    (input) REAL array, dimension (NRHS)   
            The estimated forward error bounds for each solution vector   
            X.  If XTRUE is the true solution, FERR bounds the magnitude   
            of the largest entry in (X - XTRUE) divided by the magnitude   
            of the largest entry in X.   

    BERR    (input) REAL array, dimension (NRHS)   
            The componentwise relative backward error of each solution   
            vector (i.e., the smallest relative change in any entry of A   
            or B that makes X an exact solution).   

    RESLTS  (output) REAL array, dimension (2)   
            The maximum over the NRHS solution vectors of the ratios:   
            RESLTS(1) = norm(X - XACT) / ( norm(X) * FERR )   
            RESLTS(2) = BERR / ( NZ*EPS + (*) )   

    =====================================================================   


       Quick exit if N = 0 or NRHS = 0.   

       Parameter adjustments */
    ab_dim1 = *ldab;
    ab_offset = 1 + ab_dim1 * 1;
    ab -= ab_offset;
    b_dim1 = *ldb;
    b_offset = 1 + b_dim1 * 1;
    b -= b_offset;
    x_dim1 = *ldx;
    x_offset = 1 + x_dim1 * 1;
    x -= x_offset;
    xact_dim1 = *ldxact;
    xact_offset = 1 + xact_dim1 * 1;
    xact -= xact_offset;
    --ferr;
    --berr;
    --reslts;

    /* Function Body */
    if (*n <= 0 || *nrhs <= 0) {
	reslts[1] = 0.f;
	reslts[2] = 0.f;
	return 0;
    }

    eps = slamch_("Epsilon");
    unfl = slamch_("Safe minimum");
    ovfl = 1.f / unfl;
    upper = lsame_(uplo, "U");
/* Computing MAX */
    i__1 = *kd, i__2 = *n - 1;
    nz = (max(i__1,i__2) << 1) + 1;

/*     Test 1:  Compute the maximum of   
          norm(X - XACT) / ( norm(X) * FERR )   
       over all the vectors X and XACT using the infinity-norm. */

    errbnd = 0.f;
    i__1 = *nrhs;
    for (j = 1; j <= i__1; ++j) {
	imax = icamax_(n, &x_ref(1, j), &c__1);
/* Computing MAX */
	i__2 = x_subscr(imax, j);
	r__3 = (r__1 = x[i__2].r, dabs(r__1)) + (r__2 = r_imag(&x_ref(imax, j)
		), dabs(r__2));
	xnorm = dmax(r__3,unfl);
	diff = 0.f;
	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    i__3 = x_subscr(i__, j);
	    i__4 = xact_subscr(i__, j);
	    q__2.r = x[i__3].r - xact[i__4].r, q__2.i = x[i__3].i - xact[i__4]
		    .i;
	    q__1.r = q__2.r, q__1.i = q__2.i;
/* Computing MAX */
	    r__3 = diff, r__4 = (r__1 = q__1.r, dabs(r__1)) + (r__2 = r_imag(&
		    q__1), dabs(r__2));
	    diff = dmax(r__3,r__4);
/* L10: */
	}

	if (xnorm > 1.f) {
	    goto L20;
	} else if (diff <= ovfl * xnorm) {
	    goto L20;
	} else {
	    errbnd = 1.f / eps;
	    goto L30;
	}

L20:
	if (diff / xnorm <= ferr[j]) {
/* Computing MAX */
	    r__1 = errbnd, r__2 = diff / xnorm / ferr[j];
	    errbnd = dmax(r__1,r__2);
	} else {
	    errbnd = 1.f / eps;
	}
L30:
	;
    }
    reslts[1] = errbnd;

/*     Test 2:  Compute the maximum of BERR / ( NZ*EPS + (*) ), where   
       (*) = NZ*UNFL / (min_i (abs(A)*abs(X) +abs(b))_i ) */

    i__1 = *nrhs;
    for (k = 1; k <= i__1; ++k) {
	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    i__3 = b_subscr(i__, k);
	    tmp = (r__1 = b[i__3].r, dabs(r__1)) + (r__2 = r_imag(&b_ref(i__, 
		    k)), dabs(r__2));
	    if (upper) {
/* Computing MAX */
		i__3 = i__ - *kd;
		i__4 = i__ - 1;
		for (j = max(i__3,1); j <= i__4; ++j) {
		    i__3 = ab_subscr(*kd + 1 - i__ + j, i__);
		    i__5 = x_subscr(j, k);
		    tmp += ((r__1 = ab[i__3].r, dabs(r__1)) + (r__2 = r_imag(&
			    ab_ref(*kd + 1 - i__ + j, i__)), dabs(r__2))) * ((
			    r__3 = x[i__5].r, dabs(r__3)) + (r__4 = r_imag(&
			    x_ref(j, k)), dabs(r__4)));
/* L40: */
		}
		i__4 = ab_subscr(*kd + 1, i__);
		i__3 = x_subscr(i__, k);
		tmp += (r__1 = ab[i__4].r, dabs(r__1)) * ((r__2 = x[i__3].r, 
			dabs(r__2)) + (r__3 = r_imag(&x_ref(i__, k)), dabs(
			r__3)));
/* Computing MIN */
		i__3 = i__ + *kd;
		i__4 = min(i__3,*n);
		for (j = i__ + 1; j <= i__4; ++j) {
		    i__3 = ab_subscr(*kd + 1 + i__ - j, j);
		    i__5 = x_subscr(j, k);
		    tmp += ((r__1 = ab[i__3].r, dabs(r__1)) + (r__2 = r_imag(&
			    ab_ref(*kd + 1 + i__ - j, j)), dabs(r__2))) * ((
			    r__3 = x[i__5].r, dabs(r__3)) + (r__4 = r_imag(&
			    x_ref(j, k)), dabs(r__4)));
/* L50: */
		}
	    } else {
/* Computing MAX */
		i__4 = i__ - *kd;
		i__3 = i__ - 1;
		for (j = max(i__4,1); j <= i__3; ++j) {
		    i__4 = ab_subscr(i__ + 1 - j, j);
		    i__5 = x_subscr(j, k);
		    tmp += ((r__1 = ab[i__4].r, dabs(r__1)) + (r__2 = r_imag(&
			    ab_ref(i__ + 1 - j, j)), dabs(r__2))) * ((r__3 = 
			    x[i__5].r, dabs(r__3)) + (r__4 = r_imag(&x_ref(j, 
			    k)), dabs(r__4)));
/* L60: */
		}
		i__3 = ab_subscr(1, i__);
		i__4 = x_subscr(i__, k);
		tmp += (r__1 = ab[i__3].r, dabs(r__1)) * ((r__2 = x[i__4].r, 
			dabs(r__2)) + (r__3 = r_imag(&x_ref(i__, k)), dabs(
			r__3)));
/* Computing MIN */
		i__4 = i__ + *kd;
		i__3 = min(i__4,*n);
		for (j = i__ + 1; j <= i__3; ++j) {
		    i__4 = ab_subscr(j + 1 - i__, i__);
		    i__5 = x_subscr(j, k);
		    tmp += ((r__1 = ab[i__4].r, dabs(r__1)) + (r__2 = r_imag(&
			    ab_ref(j + 1 - i__, i__)), dabs(r__2))) * ((r__3 =
			     x[i__5].r, dabs(r__3)) + (r__4 = r_imag(&x_ref(j,
			     k)), dabs(r__4)));
/* L70: */
		}
	    }
	    if (i__ == 1) {
		axbi = tmp;
	    } else {
		axbi = dmin(axbi,tmp);
	    }
/* L80: */
	}
/* Computing MAX */
	r__1 = axbi, r__2 = nz * unfl;
	tmp = berr[k] / (nz * eps + nz * unfl / dmax(r__1,r__2));
	if (k == 1) {
	    reslts[2] = tmp;
	} else {
	    reslts[2] = dmax(reslts[2],tmp);
	}
/* L90: */
    }

    return 0;

/*     End of CPBT05 */

} /* cpbt05_ */

#undef ab_ref
#undef ab_subscr
#undef x_ref
#undef x_subscr
#undef b_ref
#undef b_subscr
#undef xact_ref
#undef xact_subscr


