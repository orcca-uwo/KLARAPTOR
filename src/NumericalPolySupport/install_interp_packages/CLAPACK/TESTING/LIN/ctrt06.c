#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Subroutine */ int ctrt06_(real *rcond, real *rcondc, char *uplo, char *
	diag, integer *n, complex *a, integer *lda, real *rwork, real *rat)
{
    /* System generated locals */
    integer a_dim1, a_offset;
    real r__1, r__2;

    /* Local variables */
    static real rmin, rmax, anorm;
    extern doublereal slamch_(char *);
    static real bignum;
    extern doublereal clantr_(char *, char *, char *, integer *, integer *, 
	    complex *, integer *, real *);
    static real eps;


/*  -- LAPACK test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 29, 1992   


    Purpose   
    =======   

    CTRT06 computes a test ratio comparing RCOND (the reciprocal   
    condition number of a triangular matrix A) and RCONDC, the estimate   
    computed by CTRCON.  Information about the triangular matrix A is   
    used if one estimate is zero and the other is non-zero to decide if   
    underflow in the estimate is justified.   

    Arguments   
    =========   

    RCOND   (input) REAL   
            The estimate of the reciprocal condition number obtained by   
            forming the explicit inverse of the matrix A and computing   
            RCOND = 1/( norm(A) * norm(inv(A)) ).   

    RCONDC  (input) REAL   
            The estimate of the reciprocal condition number computed by   
            CTRCON.   

    UPLO    (input) CHARACTER   
            Specifies whether the matrix A is upper or lower triangular.   
            = 'U':  Upper triangular   
            = 'L':  Lower triangular   

    DIAG    (input) CHARACTER   
            Specifies whether or not the matrix A is unit triangular.   
            = 'N':  Non-unit triangular   
            = 'U':  Unit triangular   

    N       (input) INTEGER   
            The order of the matrix A.  N >= 0.   

    A       (input) COMPLEX array, dimension (LDA,N)   
            The triangular matrix A.  If UPLO = 'U', the leading n by n   
            upper triangular part of the array A contains the upper   
            triangular matrix, and the strictly lower triangular part of   
            A is not referenced.  If UPLO = 'L', the leading n by n lower   
            triangular part of the array A contains the lower triangular   
            matrix, and the strictly upper triangular part of A is not   
            referenced.  If DIAG = 'U', the diagonal elements of A are   
            also not referenced and are assumed to be 1.   

    LDA     (input) INTEGER   
            The leading dimension of the array A.  LDA >= max(1,N).   

    RWORK   (workspace) REAL array, dimension (N)   

    RAT     (output) REAL   
            The test ratio.  If both RCOND and RCONDC are nonzero,   
               RAT = MAX( RCOND, RCONDC )/MIN( RCOND, RCONDC ) - 1.   
            If RAT = 0, the two estimates are exactly the same.   

    =====================================================================   


       Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    --rwork;

    /* Function Body */
    eps = slamch_("Epsilon");
    rmax = dmax(*rcond,*rcondc);
    rmin = dmin(*rcond,*rcondc);

/*     Do the easy cases first. */

    if (rmin < 0.f) {

/*        Invalid value for RCOND or RCONDC, return 1/EPS. */

	*rat = 1.f / eps;

    } else if (rmin > 0.f) {

/*        Both estimates are positive, return RMAX/RMIN - 1. */

	*rat = rmax / rmin - 1.f;

    } else if (rmax == 0.f) {

/*        Both estimates zero. */

	*rat = 0.f;

    } else {

/*        One estimate is zero, the other is non-zero.  If the matrix is   
          ill-conditioned, return the nonzero estimate multiplied by   
          1/EPS; if the matrix is badly scaled, return the nonzero   
          estimate multiplied by BIGNUM/TMAX, where TMAX is the maximum   
          element in absolute value in A. */

	bignum = 1.f / slamch_("Safe minimum");
	anorm = clantr_("M", uplo, diag, n, n, &a[a_offset], lda, &rwork[1]);

/* Computing MIN */
	r__1 = bignum / dmax(1.f,anorm), r__2 = 1.f / eps;
	*rat = rmax * dmin(r__1,r__2);
    }

    return 0;

/*     End of CTRT06 */

} /* ctrt06_ */

