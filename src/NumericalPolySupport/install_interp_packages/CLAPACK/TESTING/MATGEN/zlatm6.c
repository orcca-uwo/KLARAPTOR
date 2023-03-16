#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__4 = 4;
static integer c__8 = 8;
static integer c__24 = 24;

/* Subroutine */ int zlatm6_(integer *type__, integer *n, doublecomplex *a, 
	integer *lda, doublecomplex *b, doublecomplex *x, integer *ldx, 
	doublecomplex *y, integer *ldy, doublecomplex *alpha, doublecomplex *
	beta, doublecomplex *wx, doublecomplex *wy, doublereal *s, doublereal 
	*dif)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, x_dim1, x_offset, y_dim1, 
	    y_offset, i__1, i__2, i__3;
    doublereal d__1, d__2;
    doublecomplex z__1, z__2, z__3, z__4;

    /* Builtin functions */
    void d_cnjg(doublecomplex *, doublecomplex *);
    double z_abs(doublecomplex *), sqrt(doublereal);

    /* Local variables */
    static integer info;
    static doublecomplex work[26];
    static integer i__, j;
    static doublecomplex z__[64]	/* was [8][8] */;
    static doublereal rwork[50];
    extern /* Subroutine */ int zlakf2_(integer *, integer *, doublecomplex *,
	     integer *, doublecomplex *, doublecomplex *, doublecomplex *, 
	    doublecomplex *, integer *), zgesvd_(char *, char *, integer *, 
	    integer *, doublecomplex *, integer *, doublereal *, 
	    doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublecomplex *, integer *, doublereal *, integer *), zlacpy_(char *, integer *, integer *, doublecomplex *, 
	    integer *, doublecomplex *, integer *);


#define a_subscr(a_1,a_2) (a_2)*a_dim1 + a_1
#define a_ref(a_1,a_2) a[a_subscr(a_1,a_2)]
#define b_subscr(a_1,a_2) (a_2)*b_dim1 + a_1
#define b_ref(a_1,a_2) b[b_subscr(a_1,a_2)]
#define x_subscr(a_1,a_2) (a_2)*x_dim1 + a_1
#define x_ref(a_1,a_2) x[x_subscr(a_1,a_2)]
#define y_subscr(a_1,a_2) (a_2)*y_dim1 + a_1
#define y_ref(a_1,a_2) y[y_subscr(a_1,a_2)]


/*  -- LAPACK test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   


    Purpose   
    =======   

    ZLATM6 generates test matrices for the generalized eigenvalue   
    problem, their corresponding right and left eigenvector matrices,   
    and also reciprocal condition numbers for all eigenvalues and   
    the reciprocal condition numbers of eigenvectors corresponding to   
    the 1th and 5th eigenvalues.   

    Test Matrices   
    =============   

    Two kinds of test matrix pairs   
             (A, B) = inverse(YH) * (Da, Db) * inverse(X)   
    are used in the tests:   

    Type 1:   
       Da = 1+a   0    0    0    0    Db = 1   0   0   0   0   
             0   2+a   0    0    0         0   1   0   0   0   
             0    0   3+a   0    0         0   0   1   0   0   
             0    0    0   4+a   0         0   0   0   1   0   
             0    0    0    0   5+a ,      0   0   0   0   1   
    and Type 2:   
       Da = 1+i   0    0       0       0    Db = 1   0   0   0   0   
             0   1-i   0       0       0         0   1   0   0   0   
             0    0    1       0       0         0   0   1   0   0   
             0    0    0 (1+a)+(1+b)i  0         0   0   0   1   0   
             0    0    0       0 (1+a)-(1+b)i,   0   0   0   0   1 .   

    In both cases the same inverse(YH) and inverse(X) are used to compute   
    (A, B), giving the exact eigenvectors to (A,B) as (YH, X):   

    YH:  =  1    0   -y    y   -y    X =  1   0  -x  -x   x   
            0    1   -y    y   -y         0   1   x  -x  -x   
            0    0    1    0    0         0   0   1   0   0   
            0    0    0    1    0         0   0   0   1   0   
            0    0    0    0    1,        0   0   0   0   1 , where   

    a, b, x and y will have all values independently of each other.   

    Arguments   
    =========   

    TYPE    (input) INTEGER   
            Specifies the problem type (see futher details).   

    N       (input) INTEGER   
            Size of the matrices A and B.   

    A       (output) COMPLEX*16 array, dimension (LDA, N).   
            On exit A N-by-N is initialized according to TYPE.   

    LDA     (input) INTEGER   
            The leading dimension of A and of B.   

    B       (output) COMPLEX*16 array, dimension (LDA, N).   
            On exit B N-by-N is initialized according to TYPE.   

    X       (output) COMPLEX*16 array, dimension (LDX, N).   
            On exit X is the N-by-N matrix of right eigenvectors.   

    LDX     (input) INTEGER   
            The leading dimension of X.   

    Y       (output) COMPLEX*16 array, dimension (LDY, N).   
            On exit Y is the N-by-N matrix of left eigenvectors.   

    LDY     (input) INTEGER   
            The leading dimension of Y.   

    ALPHA   (input) COMPLEX*16   
    BETA    (input) COMPLEX*16   
            Weighting constants for matrix A.   

    WX      (input) COMPLEX*16   
            Constant for right eigenvector matrix.   

    WY      (input) COMPLEX*16   
            Constant for left eigenvector matrix.   

    S       (output) DOUBLE PRECISION array, dimension (N)   
            S(i) is the reciprocal condition number for eigenvalue i.   

    DIF     (output) DOUBLE PRECISION array, dimension (N)   
            DIF(i) is the reciprocal condition number for eigenvector i.   

    =====================================================================   


       Generate test problem ...   
       (Da, Db) ...   

       Parameter adjustments */
    b_dim1 = *lda;
    b_offset = 1 + b_dim1 * 1;
    b -= b_offset;
    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    x_dim1 = *ldx;
    x_offset = 1 + x_dim1 * 1;
    x -= x_offset;
    y_dim1 = *ldy;
    y_offset = 1 + y_dim1 * 1;
    y -= y_offset;
    --s;
    --dif;

    /* Function Body */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {

	    if (i__ == j) {
		i__3 = a_subscr(i__, i__);
		z__2.r = (doublereal) i__, z__2.i = 0.;
		z__1.r = z__2.r + alpha->r, z__1.i = z__2.i + alpha->i;
		a[i__3].r = z__1.r, a[i__3].i = z__1.i;
		i__3 = b_subscr(i__, i__);
		b[i__3].r = 1., b[i__3].i = 0.;
	    } else {
		i__3 = a_subscr(i__, j);
		a[i__3].r = 0., a[i__3].i = 0.;
		i__3 = b_subscr(i__, j);
		b[i__3].r = 0., b[i__3].i = 0.;
	    }

/* L10: */
	}
/* L20: */
    }
    if (*type__ == 2) {
	i__1 = a_subscr(1, 1);
	a[i__1].r = 1., a[i__1].i = 1.;
	i__1 = a_subscr(2, 2);
	d_cnjg(&z__1, &a_ref(1, 1));
	a[i__1].r = z__1.r, a[i__1].i = z__1.i;
	i__1 = a_subscr(3, 3);
	a[i__1].r = 1., a[i__1].i = 0.;
	i__1 = a_subscr(4, 4);
	z__2.r = alpha->r + 1., z__2.i = alpha->i + 0.;
	d__1 = z__2.r;
	z__3.r = beta->r + 1., z__3.i = beta->i + 0.;
	d__2 = z__3.r;
	z__1.r = d__1, z__1.i = d__2;
	a[i__1].r = z__1.r, a[i__1].i = z__1.i;
	i__1 = a_subscr(5, 5);
	d_cnjg(&z__1, &a_ref(4, 4));
	a[i__1].r = z__1.r, a[i__1].i = z__1.i;
    }

/*     Form X and Y */

    zlacpy_("F", n, n, &b[b_offset], lda, &y[y_offset], ldy);
    i__1 = y_subscr(3, 1);
    d_cnjg(&z__2, wy);
    z__1.r = -z__2.r, z__1.i = -z__2.i;
    y[i__1].r = z__1.r, y[i__1].i = z__1.i;
    i__1 = y_subscr(4, 1);
    d_cnjg(&z__1, wy);
    y[i__1].r = z__1.r, y[i__1].i = z__1.i;
    i__1 = y_subscr(5, 1);
    d_cnjg(&z__2, wy);
    z__1.r = -z__2.r, z__1.i = -z__2.i;
    y[i__1].r = z__1.r, y[i__1].i = z__1.i;
    i__1 = y_subscr(3, 2);
    d_cnjg(&z__2, wy);
    z__1.r = -z__2.r, z__1.i = -z__2.i;
    y[i__1].r = z__1.r, y[i__1].i = z__1.i;
    i__1 = y_subscr(4, 2);
    d_cnjg(&z__1, wy);
    y[i__1].r = z__1.r, y[i__1].i = z__1.i;
    i__1 = y_subscr(5, 2);
    d_cnjg(&z__2, wy);
    z__1.r = -z__2.r, z__1.i = -z__2.i;
    y[i__1].r = z__1.r, y[i__1].i = z__1.i;

    zlacpy_("F", n, n, &b[b_offset], lda, &x[x_offset], ldx);
    i__1 = x_subscr(1, 3);
    z__1.r = -wx->r, z__1.i = -wx->i;
    x[i__1].r = z__1.r, x[i__1].i = z__1.i;
    i__1 = x_subscr(1, 4);
    z__1.r = -wx->r, z__1.i = -wx->i;
    x[i__1].r = z__1.r, x[i__1].i = z__1.i;
    i__1 = x_subscr(1, 5);
    x[i__1].r = wx->r, x[i__1].i = wx->i;
    i__1 = x_subscr(2, 3);
    x[i__1].r = wx->r, x[i__1].i = wx->i;
    i__1 = x_subscr(2, 4);
    z__1.r = -wx->r, z__1.i = -wx->i;
    x[i__1].r = z__1.r, x[i__1].i = z__1.i;
    i__1 = x_subscr(2, 5);
    z__1.r = -wx->r, z__1.i = -wx->i;
    x[i__1].r = z__1.r, x[i__1].i = z__1.i;

/*     Form (A, B) */

    i__1 = b_subscr(1, 3);
    z__1.r = wx->r + wy->r, z__1.i = wx->i + wy->i;
    b[i__1].r = z__1.r, b[i__1].i = z__1.i;
    i__1 = b_subscr(2, 3);
    z__2.r = -wx->r, z__2.i = -wx->i;
    z__1.r = z__2.r + wy->r, z__1.i = z__2.i + wy->i;
    b[i__1].r = z__1.r, b[i__1].i = z__1.i;
    i__1 = b_subscr(1, 4);
    z__1.r = wx->r - wy->r, z__1.i = wx->i - wy->i;
    b[i__1].r = z__1.r, b[i__1].i = z__1.i;
    i__1 = b_subscr(2, 4);
    z__1.r = wx->r - wy->r, z__1.i = wx->i - wy->i;
    b[i__1].r = z__1.r, b[i__1].i = z__1.i;
    i__1 = b_subscr(1, 5);
    z__2.r = -wx->r, z__2.i = -wx->i;
    z__1.r = z__2.r + wy->r, z__1.i = z__2.i + wy->i;
    b[i__1].r = z__1.r, b[i__1].i = z__1.i;
    i__1 = b_subscr(2, 5);
    z__1.r = wx->r + wy->r, z__1.i = wx->i + wy->i;
    b[i__1].r = z__1.r, b[i__1].i = z__1.i;
    i__1 = a_subscr(1, 3);
    i__2 = a_subscr(1, 1);
    z__2.r = wx->r * a[i__2].r - wx->i * a[i__2].i, z__2.i = wx->r * a[i__2]
	    .i + wx->i * a[i__2].r;
    i__3 = a_subscr(3, 3);
    z__3.r = wy->r * a[i__3].r - wy->i * a[i__3].i, z__3.i = wy->r * a[i__3]
	    .i + wy->i * a[i__3].r;
    z__1.r = z__2.r + z__3.r, z__1.i = z__2.i + z__3.i;
    a[i__1].r = z__1.r, a[i__1].i = z__1.i;
    i__1 = a_subscr(2, 3);
    z__3.r = -wx->r, z__3.i = -wx->i;
    i__2 = a_subscr(2, 2);
    z__2.r = z__3.r * a[i__2].r - z__3.i * a[i__2].i, z__2.i = z__3.r * a[
	    i__2].i + z__3.i * a[i__2].r;
    i__3 = a_subscr(3, 3);
    z__4.r = wy->r * a[i__3].r - wy->i * a[i__3].i, z__4.i = wy->r * a[i__3]
	    .i + wy->i * a[i__3].r;
    z__1.r = z__2.r + z__4.r, z__1.i = z__2.i + z__4.i;
    a[i__1].r = z__1.r, a[i__1].i = z__1.i;
    i__1 = a_subscr(1, 4);
    i__2 = a_subscr(1, 1);
    z__2.r = wx->r * a[i__2].r - wx->i * a[i__2].i, z__2.i = wx->r * a[i__2]
	    .i + wx->i * a[i__2].r;
    i__3 = a_subscr(4, 4);
    z__3.r = wy->r * a[i__3].r - wy->i * a[i__3].i, z__3.i = wy->r * a[i__3]
	    .i + wy->i * a[i__3].r;
    z__1.r = z__2.r - z__3.r, z__1.i = z__2.i - z__3.i;
    a[i__1].r = z__1.r, a[i__1].i = z__1.i;
    i__1 = a_subscr(2, 4);
    i__2 = a_subscr(2, 2);
    z__2.r = wx->r * a[i__2].r - wx->i * a[i__2].i, z__2.i = wx->r * a[i__2]
	    .i + wx->i * a[i__2].r;
    i__3 = a_subscr(4, 4);
    z__3.r = wy->r * a[i__3].r - wy->i * a[i__3].i, z__3.i = wy->r * a[i__3]
	    .i + wy->i * a[i__3].r;
    z__1.r = z__2.r - z__3.r, z__1.i = z__2.i - z__3.i;
    a[i__1].r = z__1.r, a[i__1].i = z__1.i;
    i__1 = a_subscr(1, 5);
    z__3.r = -wx->r, z__3.i = -wx->i;
    i__2 = a_subscr(1, 1);
    z__2.r = z__3.r * a[i__2].r - z__3.i * a[i__2].i, z__2.i = z__3.r * a[
	    i__2].i + z__3.i * a[i__2].r;
    i__3 = a_subscr(5, 5);
    z__4.r = wy->r * a[i__3].r - wy->i * a[i__3].i, z__4.i = wy->r * a[i__3]
	    .i + wy->i * a[i__3].r;
    z__1.r = z__2.r + z__4.r, z__1.i = z__2.i + z__4.i;
    a[i__1].r = z__1.r, a[i__1].i = z__1.i;
    i__1 = a_subscr(2, 5);
    i__2 = a_subscr(2, 2);
    z__2.r = wx->r * a[i__2].r - wx->i * a[i__2].i, z__2.i = wx->r * a[i__2]
	    .i + wx->i * a[i__2].r;
    i__3 = a_subscr(5, 5);
    z__3.r = wy->r * a[i__3].r - wy->i * a[i__3].i, z__3.i = wy->r * a[i__3]
	    .i + wy->i * a[i__3].r;
    z__1.r = z__2.r + z__3.r, z__1.i = z__2.i + z__3.i;
    a[i__1].r = z__1.r, a[i__1].i = z__1.i;

/*     Compute condition numbers */

    s[1] = 1. / sqrt((z_abs(wy) * 3. * z_abs(wy) + 1.) / (z_abs(&a_ref(1, 1)) 
	    * z_abs(&a_ref(1, 1)) + 1.));
    s[2] = 1. / sqrt((z_abs(wy) * 3. * z_abs(wy) + 1.) / (z_abs(&a_ref(2, 2)) 
	    * z_abs(&a_ref(2, 2)) + 1.));
    s[3] = 1. / sqrt((z_abs(wx) * 2. * z_abs(wx) + 1.) / (z_abs(&a_ref(3, 3)) 
	    * z_abs(&a_ref(3, 3)) + 1.));
    s[4] = 1. / sqrt((z_abs(wx) * 2. * z_abs(wx) + 1.) / (z_abs(&a_ref(4, 4)) 
	    * z_abs(&a_ref(4, 4)) + 1.));
    s[5] = 1. / sqrt((z_abs(wx) * 2. * z_abs(wx) + 1.) / (z_abs(&a_ref(5, 5)) 
	    * z_abs(&a_ref(5, 5)) + 1.));

    zlakf2_(&c__1, &c__4, &a[a_offset], lda, &a_ref(2, 2), &b[b_offset], &
	    b_ref(2, 2), z__, &c__8);
    zgesvd_("N", "N", &c__8, &c__8, z__, &c__8, rwork, work, &c__1, &work[1], 
	    &c__1, &work[2], &c__24, &rwork[8], &info);
    dif[1] = rwork[7];

    zlakf2_(&c__4, &c__1, &a[a_offset], lda, &a_ref(5, 5), &b[b_offset], &
	    b_ref(5, 5), z__, &c__8);
    zgesvd_("N", "N", &c__8, &c__8, z__, &c__8, rwork, work, &c__1, &work[1], 
	    &c__1, &work[2], &c__24, &rwork[8], &info);
    dif[5] = rwork[7];

    return 0;

/*     End of ZLATM6 */

} /* zlatm6_ */

#undef y_ref
#undef y_subscr
#undef x_ref
#undef x_subscr
#undef b_ref
#undef b_subscr
#undef a_ref
#undef a_subscr


