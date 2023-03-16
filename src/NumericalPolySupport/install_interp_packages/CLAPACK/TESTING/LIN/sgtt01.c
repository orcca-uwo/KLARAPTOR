#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Subroutine */ int sgtt01_(integer *n, real *dl, real *d__, real *du, real *
	dlf, real *df, real *duf, real *du2, integer *ipiv, real *work, 
	integer *ldwork, real *rwork, real *resid)
{
    /* System generated locals */
    integer work_dim1, work_offset, i__1, i__2;

    /* Local variables */
    static integer i__, j;
    static real anorm;
    static integer lastj;
    extern /* Subroutine */ int sswap_(integer *, real *, integer *, real *, 
	    integer *), saxpy_(integer *, real *, real *, integer *, real *, 
	    integer *);
    static real li;
    static integer ip;
    extern doublereal slamch_(char *), slangt_(char *, integer *, 
	    real *, real *, real *), slanhs_(char *, integer *, real *
	    , integer *, real *);
    static real eps;


#define work_ref(a_1,a_2) work[(a_2)*work_dim1 + a_1]


/*  -- LAPACK test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 29, 1992   


    Purpose   
    =======   

    SGTT01 reconstructs a tridiagonal matrix A from its LU factorization   
    and computes the residual   
       norm(L*U - A) / ( norm(A) * EPS ),   
    where EPS is the machine epsilon.   

    Arguments   
    =========   

    N       (input) INTEGTER   
            The order of the matrix A.  N >= 0.   

    DL      (input) REAL array, dimension (N-1)   
            The (n-1) sub-diagonal elements of A.   

    D       (input) REAL array, dimension (N)   
            The diagonal elements of A.   

    DU      (input) REAL array, dimension (N-1)   
            The (n-1) super-diagonal elements of A.   

    DLF     (input) REAL array, dimension (N-1)   
            The (n-1) multipliers that define the matrix L from the   
            LU factorization of A.   

    DF      (input) REAL array, dimension (N)   
            The n diagonal elements of the upper triangular matrix U from   
            the LU factorization of A.   

    DUF     (input) REAL array, dimension (N-1)   
            The (n-1) elements of the first super-diagonal of U.   

    DU2F    (input) REAL array, dimension (N-2)   
            The (n-2) elements of the second super-diagonal of U.   

    IPIV    (input) INTEGER array, dimension (N)   
            The pivot indices; for 1 <= i <= n, row i of the matrix was   
            interchanged with row IPIV(i).  IPIV(i) will always be either   
            i or i+1; IPIV(i) = i indicates a row interchange was not   
            required.   

    WORK    (workspace) REAL array, dimension (LDWORK,N)   

    LDWORK  (input) INTEGER   
            The leading dimension of the array WORK.  LDWORK >= max(1,N).   

    RWORK   (workspace) REAL array, dimension (N)   

    RESID   (output) REAL   
            The scaled residual:  norm(L*U - A) / (norm(A) * EPS)   

    =====================================================================   


       Quick return if possible   

       Parameter adjustments */
    --dl;
    --d__;
    --du;
    --dlf;
    --df;
    --duf;
    --du2;
    --ipiv;
    work_dim1 = *ldwork;
    work_offset = 1 + work_dim1 * 1;
    work -= work_offset;
    --rwork;

    /* Function Body */
    if (*n <= 0) {
	*resid = 0.f;
	return 0;
    }

    eps = slamch_("Epsilon");

/*     Copy the matrix U to WORK. */

    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    work_ref(i__, j) = 0.f;
/* L10: */
	}
/* L20: */
    }
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (i__ == 1) {
	    work_ref(i__, i__) = df[i__];
	    if (*n >= 2) {
		work_ref(i__, i__ + 1) = duf[i__];
	    }
	    if (*n >= 3) {
		work_ref(i__, i__ + 2) = du2[i__];
	    }
	} else if (i__ == *n) {
	    work_ref(i__, i__) = df[i__];
	} else {
	    work_ref(i__, i__) = df[i__];
	    work_ref(i__, i__ + 1) = duf[i__];
	    if (i__ < *n - 1) {
		work_ref(i__, i__ + 2) = du2[i__];
	    }
	}
/* L30: */
    }

/*     Multiply on the left by L. */

    lastj = *n;
    for (i__ = *n - 1; i__ >= 1; --i__) {
	li = dlf[i__];
	i__1 = lastj - i__ + 1;
	saxpy_(&i__1, &li, &work_ref(i__, i__), ldwork, &work_ref(i__ + 1, 
		i__), ldwork);
	ip = ipiv[i__];
	if (ip == i__) {
/* Computing MIN */
	    i__1 = i__ + 2;
	    lastj = min(i__1,*n);
	} else {
	    i__1 = lastj - i__ + 1;
	    sswap_(&i__1, &work_ref(i__, i__), ldwork, &work_ref(i__ + 1, i__)
		    , ldwork);
	}
/* L40: */
    }

/*     Subtract the matrix A. */

    work_ref(1, 1) = work_ref(1, 1) - d__[1];
    if (*n > 1) {
	work_ref(1, 2) = work_ref(1, 2) - du[1];
	work_ref(*n, *n - 1) = work_ref(*n, *n - 1) - dl[*n - 1];
	work_ref(*n, *n) = work_ref(*n, *n) - d__[*n];
	i__1 = *n - 1;
	for (i__ = 2; i__ <= i__1; ++i__) {
	    work_ref(i__, i__ - 1) = work_ref(i__, i__ - 1) - dl[i__ - 1];
	    work_ref(i__, i__) = work_ref(i__, i__) - d__[i__];
	    work_ref(i__, i__ + 1) = work_ref(i__, i__ + 1) - du[i__];
/* L50: */
	}
    }

/*     Compute the 1-norm of the tridiagonal matrix A. */

    anorm = slangt_("1", n, &dl[1], &d__[1], &du[1]);

/*     Compute the 1-norm of WORK, which is only guaranteed to be   
       upper Hessenberg. */

    *resid = slanhs_("1", n, &work[work_offset], ldwork, &rwork[1])
	    ;

/*     Compute norm(L*U - A) / (norm(A) * EPS) */

    if (anorm <= 0.f) {
	if (*resid != 0.f) {
	    *resid = 1.f / eps;
	}
    } else {
	*resid = *resid / anorm / eps;
    }

    return 0;

/*     End of SGTT01 */

} /* sgtt01_ */

#undef work_ref


