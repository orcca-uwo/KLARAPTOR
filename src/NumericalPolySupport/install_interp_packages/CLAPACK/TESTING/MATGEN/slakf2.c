#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static real c_b3 = 0.f;

/* Subroutine */ int slakf2_(integer *m, integer *n, real *a, integer *lda, 
	real *b, real *d__, real *e, real *z__, integer *ldz)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, d_dim1, d_offset, e_dim1, 
	    e_offset, z_dim1, z_offset, i__1, i__2, i__3;

    /* Local variables */
    static integer i__, j, l, ik, jk, mn;
    extern /* Subroutine */ int slaset_(char *, integer *, integer *, real *, 
	    real *, real *, integer *);
    static integer mn2;


#define a_ref(a_1,a_2) a[(a_2)*a_dim1 + a_1]
#define b_ref(a_1,a_2) b[(a_2)*b_dim1 + a_1]
#define d___ref(a_1,a_2) d__[(a_2)*d_dim1 + a_1]
#define e_ref(a_1,a_2) e[(a_2)*e_dim1 + a_1]
#define z___ref(a_1,a_2) z__[(a_2)*z_dim1 + a_1]


/*  -- LAPACK test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   


    Purpose   
    =======   

    Form the 2*M*N by 2*M*N matrix   

           Z = [ kron(In, A)  -kron(B', Im) ]   
               [ kron(In, D)  -kron(E', Im) ],   

    where In is the identity matrix of size n and X' is the transpose   
    of X. kron(X, Y) is the Kronecker product between the matrices X   
    and Y.   

    Arguments   
    =========   

    M       (input) INTEGER   
            Size of matrix, must be >= 1.   

    N       (input) INTEGER   
            Size of matrix, must be >= 1.   

    A       (input) REAL, dimension ( LDA, M )   
            The matrix A in the output matrix Z.   

    LDA     (input) INTEGER   
            The leading dimension of A, B, D, and E. ( LDA >= M+N )   

    B       (input) REAL, dimension ( LDA, N )   
    D       (input) REAL, dimension ( LDA, M )   
    E       (input) REAL, dimension ( LDA, N )   
            The matrices used in forming the output matrix Z.   

    Z       (output) REAL, dimension ( LDZ, 2*M*N )   
            The resultant Kronecker M*N*2 by M*N*2 matrix (see above.)   

    LDZ     (input) INTEGER   
            The leading dimension of Z. ( LDZ >= 2*M*N )   

    ====================================================================   


       Initialize Z   

       Parameter adjustments */
    e_dim1 = *lda;
    e_offset = 1 + e_dim1 * 1;
    e -= e_offset;
    d_dim1 = *lda;
    d_offset = 1 + d_dim1 * 1;
    d__ -= d_offset;
    b_dim1 = *lda;
    b_offset = 1 + b_dim1 * 1;
    b -= b_offset;
    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    z_dim1 = *ldz;
    z_offset = 1 + z_dim1 * 1;
    z__ -= z_offset;

    /* Function Body */
    mn = *m * *n;
    mn2 = mn << 1;
    slaset_("Full", &mn2, &mn2, &c_b3, &c_b3, &z__[z_offset], ldz);

    ik = 1;
    i__1 = *n;
    for (l = 1; l <= i__1; ++l) {

/*        form kron(In, A) */

	i__2 = *m;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    i__3 = *m;
	    for (j = 1; j <= i__3; ++j) {
		z___ref(ik + i__ - 1, ik + j - 1) = a_ref(i__, j);
/* L10: */
	    }
/* L20: */
	}

/*        form kron(In, D) */

	i__2 = *m;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    i__3 = *m;
	    for (j = 1; j <= i__3; ++j) {
		z___ref(ik + mn + i__ - 1, ik + j - 1) = d___ref(i__, j);
/* L30: */
	    }
/* L40: */
	}

	ik += *m;
/* L50: */
    }

    ik = 1;
    i__1 = *n;
    for (l = 1; l <= i__1; ++l) {
	jk = mn + 1;

	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {

/*           form -kron(B', Im) */

	    i__3 = *m;
	    for (i__ = 1; i__ <= i__3; ++i__) {
		z___ref(ik + i__ - 1, jk + i__ - 1) = -b_ref(j, l);
/* L60: */
	    }

/*           form -kron(E', Im) */

	    i__3 = *m;
	    for (i__ = 1; i__ <= i__3; ++i__) {
		z___ref(ik + mn + i__ - 1, jk + i__ - 1) = -e_ref(j, l);
/* L70: */
	    }

	    jk += *m;
/* L80: */
	}

	ik += *m;
/* L90: */
    }

    return 0;

/*     End of SLAKF2 */

} /* slakf2_ */

#undef z___ref
#undef e_ref
#undef d___ref
#undef b_ref
#undef a_ref


