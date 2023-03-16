#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublecomplex c_b1 = {0.,0.};

/* Subroutine */ int zlakf2_(integer *m, integer *n, doublecomplex *a, 
	integer *lda, doublecomplex *b, doublecomplex *d__, doublecomplex *e, 
	doublecomplex *z__, integer *ldz)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, d_dim1, d_offset, e_dim1, 
	    e_offset, z_dim1, z_offset, i__1, i__2, i__3, i__4, i__5;
    doublecomplex z__1;

    /* Local variables */
    static integer i__, j, l, ik, jk, mn;
    extern /* Subroutine */ int zlaset_(char *, integer *, integer *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, integer *);
    static integer mn2;


#define a_subscr(a_1,a_2) (a_2)*a_dim1 + a_1
#define a_ref(a_1,a_2) a[a_subscr(a_1,a_2)]
#define b_subscr(a_1,a_2) (a_2)*b_dim1 + a_1
#define b_ref(a_1,a_2) b[b_subscr(a_1,a_2)]
#define d___subscr(a_1,a_2) (a_2)*d_dim1 + a_1
#define d___ref(a_1,a_2) d__[d___subscr(a_1,a_2)]
#define e_subscr(a_1,a_2) (a_2)*e_dim1 + a_1
#define e_ref(a_1,a_2) e[e_subscr(a_1,a_2)]
#define z___subscr(a_1,a_2) (a_2)*z_dim1 + a_1
#define z___ref(a_1,a_2) z__[z___subscr(a_1,a_2)]


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

    A       (input) COMPLEX*16, dimension ( LDA, M )   
            The matrix A in the output matrix Z.   

    LDA     (input) INTEGER   
            The leading dimension of A, B, D, and E. ( LDA >= M+N )   

    B       (input) COMPLEX*16, dimension ( LDA, N )   
    D       (input) COMPLEX*16, dimension ( LDA, M )   
    E       (input) COMPLEX*16, dimension ( LDA, N )   
            The matrices used in forming the output matrix Z.   

    Z       (output) COMPLEX*16, dimension ( LDZ, 2*M*N )   
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
    zlaset_("Full", &mn2, &mn2, &c_b1, &c_b1, &z__[z_offset], ldz);

    ik = 1;
    i__1 = *n;
    for (l = 1; l <= i__1; ++l) {

/*        form kron(In, A) */

	i__2 = *m;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    i__3 = *m;
	    for (j = 1; j <= i__3; ++j) {
		i__4 = z___subscr(ik + i__ - 1, ik + j - 1);
		i__5 = a_subscr(i__, j);
		z__[i__4].r = a[i__5].r, z__[i__4].i = a[i__5].i;
/* L10: */
	    }
/* L20: */
	}

/*        form kron(In, D) */

	i__2 = *m;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    i__3 = *m;
	    for (j = 1; j <= i__3; ++j) {
		i__4 = z___subscr(ik + mn + i__ - 1, ik + j - 1);
		i__5 = d___subscr(i__, j);
		z__[i__4].r = d__[i__5].r, z__[i__4].i = d__[i__5].i;
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
		i__4 = z___subscr(ik + i__ - 1, jk + i__ - 1);
		i__5 = b_subscr(j, l);
		z__1.r = -b[i__5].r, z__1.i = -b[i__5].i;
		z__[i__4].r = z__1.r, z__[i__4].i = z__1.i;
/* L60: */
	    }

/*           form -kron(E', Im) */

	    i__3 = *m;
	    for (i__ = 1; i__ <= i__3; ++i__) {
		i__4 = z___subscr(ik + mn + i__ - 1, jk + i__ - 1);
		i__5 = e_subscr(j, l);
		z__1.r = -e[i__5].r, z__1.i = -e[i__5].i;
		z__[i__4].r = z__1.r, z__[i__4].i = z__1.i;
/* L70: */
	    }

	    jk += *m;
/* L80: */
	}

	ik += *m;
/* L90: */
    }

    return 0;

/*     End of ZLAKF2 */

} /* zlakf2_ */

#undef z___ref
#undef z___subscr
#undef e_ref
#undef e_subscr
#undef d___ref
#undef d___subscr
#undef b_ref
#undef b_subscr
#undef a_ref
#undef a_subscr


