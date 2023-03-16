#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static doublereal c_b12 = -1.;

/* Subroutine */ int dgbt01_(integer *m, integer *n, integer *kl, integer *ku,
	 doublereal *a, integer *lda, doublereal *afac, integer *ldafac, 
	integer *ipiv, doublereal *work, doublereal *resid)
{
    /* System generated locals */
    integer a_dim1, a_offset, afac_dim1, afac_offset, i__1, i__2, i__3, i__4;
    doublereal d__1, d__2;

    /* Local variables */
    static integer lenj, i__, j;
    static doublereal t;
    extern doublereal dasum_(integer *, doublereal *, integer *);
    static doublereal anorm;
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *, 
	    doublereal *, integer *), daxpy_(integer *, doublereal *, 
	    doublereal *, integer *, doublereal *, integer *);
    static integer i1, i2, kd, il, jl;
    extern doublereal dlamch_(char *);
    static integer ip, ju, iw, jua;
    static doublereal eps;


#define a_ref(a_1,a_2) a[(a_2)*a_dim1 + a_1]
#define afac_ref(a_1,a_2) afac[(a_2)*afac_dim1 + a_1]


/*  -- LAPACK test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 29, 1992   


    Purpose   
    =======   

    DGBT01 reconstructs a band matrix  A  from its L*U factorization and   
    computes the residual:   
       norm(L*U - A) / ( N * norm(A) * EPS ),   
    where EPS is the machine epsilon.   

    The expression L*U - A is computed one column at a time, so A and   
    AFAC are not modified.   

    Arguments   
    =========   

    M       (input) INTEGER   
            The number of rows of the matrix A.  M >= 0.   

    N       (input) INTEGER   
            The number of columns of the matrix A.  N >= 0.   

    KL      (input) INTEGER   
            The number of subdiagonals within the band of A.  KL >= 0.   

    KU      (input) INTEGER   
            The number of superdiagonals within the band of A.  KU >= 0.   

    A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)   
            The original matrix A in band storage, stored in rows 1 to   
            KL+KU+1.   

    LDA     (input) INTEGER.   
            The leading dimension of the array A.  LDA >= max(1,KL+KU+1).   

    AFAC    (input) DOUBLE PRECISION array, dimension (LDAFAC,N)   
            The factored form of the matrix A.  AFAC contains the banded   
            factors L and U from the L*U factorization, as computed by   
            DGBTRF.  U is stored as an upper triangular band matrix with   
            KL+KU superdiagonals in rows 1 to KL+KU+1, and the   
            multipliers used during the factorization are stored in rows   
            KL+KU+2 to 2*KL+KU+1.  See DGBTRF for further details.   

    LDAFAC  (input) INTEGER   
            The leading dimension of the array AFAC.   
            LDAFAC >= max(1,2*KL*KU+1).   

    IPIV    (input) INTEGER array, dimension (min(M,N))   
            The pivot indices from DGBTRF.   

    WORK    (workspace) DOUBLE PRECISION array, dimension (2*KL+KU+1)   

    RESID   (output) DOUBLE PRECISION   
            norm(L*U - A) / ( N * norm(A) * EPS )   

    =====================================================================   


       Quick exit if M = 0 or N = 0.   

       Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    afac_dim1 = *ldafac;
    afac_offset = 1 + afac_dim1 * 1;
    afac -= afac_offset;
    --ipiv;
    --work;

    /* Function Body */
    *resid = 0.;
    if (*m <= 0 || *n <= 0) {
	return 0;
    }

/*     Determine EPS and the norm of A. */

    eps = dlamch_("Epsilon");
    kd = *ku + 1;
    anorm = 0.;
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/* Computing MAX */
	i__2 = kd + 1 - j;
	i1 = max(i__2,1);
/* Computing MIN */
	i__2 = kd + *m - j, i__3 = *kl + kd;
	i2 = min(i__2,i__3);
	if (i2 >= i1) {
/* Computing MAX */
	    i__2 = i2 - i1 + 1;
	    d__1 = anorm, d__2 = dasum_(&i__2, &a_ref(i1, j), &c__1);
	    anorm = max(d__1,d__2);
	}
/* L10: */
    }

/*     Compute one column at a time of L*U - A. */

    kd = *kl + *ku + 1;
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {

/*        Copy the J-th column of U to WORK.   

   Computing MIN */
	i__2 = *kl + *ku, i__3 = j - 1;
	ju = min(i__2,i__3);
/* Computing MIN */
	i__2 = *kl, i__3 = *m - j;
	jl = min(i__2,i__3);
	lenj = min(*m,j) - j + ju + 1;
	if (lenj > 0) {
	    dcopy_(&lenj, &afac_ref(kd - ju, j), &c__1, &work[1], &c__1);
	    i__2 = ju + jl + 1;
	    for (i__ = lenj + 1; i__ <= i__2; ++i__) {
		work[i__] = 0.;
/* L20: */
	    }

/*           Multiply by the unit lower triangular matrix L.  Note that L   
             is stored as a product of transformations and permutations.   

   Computing MIN */
	    i__2 = *m - 1;
	    i__3 = j - ju;
	    for (i__ = min(i__2,j); i__ >= i__3; --i__) {
/* Computing MIN */
		i__2 = *kl, i__4 = *m - i__;
		il = min(i__2,i__4);
		if (il > 0) {
		    iw = i__ - j + ju + 1;
		    t = work[iw];
		    daxpy_(&il, &t, &afac_ref(kd + 1, i__), &c__1, &work[iw + 
			    1], &c__1);
		    ip = ipiv[i__];
		    if (i__ != ip) {
			ip = ip - j + ju + 1;
			work[iw] = work[ip];
			work[ip] = t;
		    }
		}
/* L30: */
	    }

/*           Subtract the corresponding column of A. */

	    jua = min(ju,*ku);
	    if (jua + jl + 1 > 0) {
		i__3 = jua + jl + 1;
		daxpy_(&i__3, &c_b12, &a_ref(*ku + 1 - jua, j), &c__1, &work[
			ju + 1 - jua], &c__1);
	    }

/*           Compute the 1-norm of the column.   

   Computing MAX */
	    i__3 = ju + jl + 1;
	    d__1 = *resid, d__2 = dasum_(&i__3, &work[1], &c__1);
	    *resid = max(d__1,d__2);
	}
/* L40: */
    }

/*     Compute norm( L*U - A ) / ( N * norm(A) * EPS ) */

    if (anorm <= 0.) {
	if (*resid != 0.) {
	    *resid = 1. / eps;
	}
    } else {
	*resid = *resid / (doublereal) (*n) / anorm / eps;
    }

    return 0;

/*     End of DGBT01 */

} /* dgbt01_ */

#undef afac_ref
#undef a_ref


