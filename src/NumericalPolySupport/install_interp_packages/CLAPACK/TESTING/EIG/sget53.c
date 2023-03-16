#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Subroutine */ int sget53_(real *a, integer *lda, real *b, integer *ldb, 
	real *scale, real *wr, real *wi, real *result, integer *info)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset;
    real r__1, r__2, r__3, r__4, r__5, r__6;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static real deti, absw, detr, temp, anorm, bnorm, cnorm, s1, cscale;
    extern doublereal slamch_(char *);
    static real scales, safmin, sigmin, ci11, ci12, ci22, cr11, cr12, cr21, 
	    cr22, ulp, wis, wrs;


#define a_ref(a_1,a_2) a[(a_2)*a_dim1 + a_1]
#define b_ref(a_1,a_2) b[(a_2)*b_dim1 + a_1]


/*  -- LAPACK test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       March 31, 1993   


    Purpose   
    =======   

    SGET53  checks the generalized eigenvalues computed by SLAG2.   

    The basic test for an eigenvalue is:   

                                 | det( s A - w B ) |   
        RESULT =  ---------------------------------------------------   
                  ulp max( s norm(A), |w| norm(B) )*norm( s A - w B )   

    Two "safety checks" are performed:   

    (1)  ulp*max( s*norm(A), |w|*norm(B) )  must be at least   
         safe_minimum.  This insures that the test performed is   
         not essentially  det(0*A + 0*B)=0.   

    (2)  s*norm(A) + |w|*norm(B) must be less than 1/safe_minimum.   
         This insures that  s*A - w*B  will not overflow.   

    If these tests are not passed, then  s  and  w  are scaled and   
    tested anyway, if this is possible.   

    Arguments   
    =========   

    A       (input) REAL array, dimension (LDA, 2)   
            The 2x2 matrix A.   

    LDA     (input) INTEGER   
            The leading dimension of A.  It must be at least 2.   

    B       (input) REAL array, dimension (LDB, N)   
            The 2x2 upper-triangular matrix B.   

    LDB     (input) INTEGER   
            The leading dimension of B.  It must be at least 2.   

    SCALE   (input) REAL   
            The "scale factor" s in the formula  s A - w B .  It is   
            assumed to be non-negative.   

    WR      (input) REAL   
            The real part of the eigenvalue  w  in the formula   
            s A - w B .   

    WI      (input) REAL   
            The imaginary part of the eigenvalue  w  in the formula   
            s A - w B .   

    RESULT  (output) REAL   
            If INFO is 2 or less, the value computed by the test   
               described above.   
            If INFO=3, this will just be 1/ulp.   

    INFO    (output) INTEGER   
            =0:  The input data pass the "safety checks".   
            =1:  s*norm(A) + |w|*norm(B) > 1/safe_minimum.   
            =2:  ulp*max( s*norm(A), |w|*norm(B) ) < safe_minimum   
            =3:  same as INFO=2, but  s  and  w  could not be scaled so   
                 as to compute the test.   

    =====================================================================   


       Initialize   

       Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    b_dim1 = *ldb;
    b_offset = 1 + b_dim1 * 1;
    b -= b_offset;

    /* Function Body */
    *info = 0;
    *result = 0.f;
    scales = *scale;
    wrs = *wr;
    wis = *wi;

/*     Machine constants and norms */

    safmin = slamch_("Safe minimum");
    ulp = slamch_("Epsilon") * slamch_("Base");
    absw = dabs(wrs) + dabs(wis);
/* Computing MAX */
    r__5 = (r__1 = a_ref(1, 1), dabs(r__1)) + (r__2 = a_ref(2, 1), dabs(r__2))
	    , r__6 = (r__3 = a_ref(1, 2), dabs(r__3)) + (r__4 = a_ref(2, 2), 
	    dabs(r__4)), r__5 = max(r__5,r__6);
    anorm = dmax(r__5,safmin);
/* Computing MAX */
    r__4 = (r__3 = b_ref(1, 1), dabs(r__3)), r__5 = (r__1 = b_ref(1, 2), dabs(
	    r__1)) + (r__2 = b_ref(2, 2), dabs(r__2)), r__4 = max(r__4,r__5);
    bnorm = dmax(r__4,safmin);

/*     Check for possible overflow. */

    temp = safmin * bnorm * absw + safmin * anorm * scales;
    if (temp >= 1.f) {

/*        Scale down to avoid overflow */

	*info = 1;
	temp = 1.f / temp;
	scales *= temp;
	wrs *= temp;
	wis *= temp;
	absw = dabs(wrs) + dabs(wis);
    }
/* Computing MAX   
   Computing MAX */
    r__3 = scales * anorm, r__4 = absw * bnorm;
    r__1 = ulp * dmax(r__3,r__4), r__2 = safmin * dmax(scales,absw);
    s1 = dmax(r__1,r__2);

/*     Check for W and SCALE essentially zero. */

    if (s1 < safmin) {
	*info = 2;
	if (scales < safmin && absw < safmin) {
	    *info = 3;
	    *result = 1.f / ulp;
	    return 0;
	}

/*        Scale up to avoid underflow   

   Computing MAX */
	r__1 = scales * anorm + absw * bnorm;
	temp = 1.f / dmax(r__1,safmin);
	scales *= temp;
	wrs *= temp;
	wis *= temp;
	absw = dabs(wrs) + dabs(wis);
/* Computing MAX   
   Computing MAX */
	r__3 = scales * anorm, r__4 = absw * bnorm;
	r__1 = ulp * dmax(r__3,r__4), r__2 = safmin * dmax(scales,absw);
	s1 = dmax(r__1,r__2);
	if (s1 < safmin) {
	    *info = 3;
	    *result = 1.f / ulp;
	    return 0;
	}
    }

/*     Compute C = s A - w B */

    cr11 = scales * a_ref(1, 1) - wrs * b_ref(1, 1);
    ci11 = -wis * b_ref(1, 1);
    cr21 = scales * a_ref(2, 1);
    cr12 = scales * a_ref(1, 2) - wrs * b_ref(1, 2);
    ci12 = -wis * b_ref(1, 2);
    cr22 = scales * a_ref(2, 2) - wrs * b_ref(2, 2);
    ci22 = -wis * b_ref(2, 2);

/*     Compute the smallest singular value of s A - w B:   

                   |det( s A - w B )|   
       sigma_min = ------------------   
                   norm( s A - w B )   

   Computing MAX */
    r__1 = dabs(cr11) + dabs(ci11) + dabs(cr21), r__2 = dabs(cr12) + dabs(
	    ci12) + dabs(cr22) + dabs(ci22), r__1 = max(r__1,r__2);
    cnorm = dmax(r__1,safmin);
    cscale = 1.f / sqrt(cnorm);
    detr = cscale * cr11 * (cscale * cr22) - cscale * ci11 * (cscale * ci22) 
	    - cscale * cr12 * (cscale * cr21);
    deti = cscale * cr11 * (cscale * ci22) + cscale * ci11 * (cscale * cr22) 
	    - cscale * ci12 * (cscale * cr21);
    sigmin = dabs(detr) + dabs(deti);
    *result = sigmin / s1;
    return 0;

/*     End of SGET53 */

} /* sget53_ */

#undef b_ref
#undef a_ref


