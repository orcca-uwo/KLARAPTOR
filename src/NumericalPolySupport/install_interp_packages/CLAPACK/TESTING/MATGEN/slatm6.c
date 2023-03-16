#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__4 = 4;
static integer c__12 = 12;
static integer c__8 = 8;
static integer c__40 = 40;
static integer c__2 = 2;
static integer c__3 = 3;
static integer c__60 = 60;

/* Subroutine */ int slatm6_(integer *type__, integer *n, real *a, integer *
	lda, real *b, real *x, integer *ldx, real *y, integer *ldy, real *
	alpha, real *beta, real *wx, real *wy, real *s, real *dif)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, x_dim1, x_offset, y_dim1, 
	    y_offset, i__1, i__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static integer info;
    static real work[100];
    static integer i__, j;
    static real z__[144]	/* was [12][12] */;
    extern /* Subroutine */ int slakf2_(integer *, integer *, real *, integer 
	    *, real *, real *, real *, real *, integer *), sgesvd_(char *, 
	    char *, integer *, integer *, real *, integer *, real *, real *, 
	    integer *, real *, integer *, real *, integer *, integer *), slacpy_(char *, integer *, integer *, real *, 
	    integer *, real *, integer *);


#define a_ref(a_1,a_2) a[(a_2)*a_dim1 + a_1]
#define b_ref(a_1,a_2) b[(a_2)*b_dim1 + a_1]
#define x_ref(a_1,a_2) x[(a_2)*x_dim1 + a_1]
#define y_ref(a_1,a_2) y[(a_2)*y_dim1 + a_1]


/*  -- LAPACK test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       October 31, 1999   


    Purpose   
    =======   

    SLATM6 generates test matrices for the generalized eigenvalue   
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
             0    0    0    0   5+a ,      0   0   0   0   1 , and   

    Type 2:   
       Da =  1   -1    0    0    0    Db = 1   0   0   0   0   
             1    1    0    0    0         0   1   0   0   0   
             0    0    1    0    0         0   0   1   0   0   
             0    0    0   1+a  1+b        0   0   0   1   0   
             0    0    0  -1-b  1+a ,      0   0   0   0   1 .   

    In both cases the same inverse(YH) and inverse(X) are used to compute   
    (A, B), giving the exact eigenvectors to (A,B) as (YH, X):   

    YH:  =  1    0   -y    y   -y    X =  1   0  -x  -x   x   
            0    1   -y    y   -y         0   1   x  -x  -x   
            0    0    1    0    0         0   0   1   0   0   
            0    0    0    1    0         0   0   0   1   0   
            0    0    0    0    1,        0   0   0   0   1 ,   

   where a, b, x and y will have all values independently of each other.   

    Arguments   
    =========   

    TYPE    (input) INTEGER   
            Specifies the problem type (see futher details).   

    N       (input) INTEGER   
            Size of the matrices A and B.   

    A       (output) REAL array, dimension (LDA, N).   
            On exit A N-by-N is initialized according to TYPE.   

    LDA     (input) INTEGER   
            The leading dimension of A and of B.   

    B       (output) REAL array, dimension (LDA, N).   
            On exit B N-by-N is initialized according to TYPE.   

    X       (output) REAL array, dimension (LDX, N).   
            On exit X is the N-by-N matrix of right eigenvectors.   

    LDX     (input) INTEGER   
            The leading dimension of X.   

    Y       (output) REAL array, dimension (LDY, N).   
            On exit Y is the N-by-N matrix of left eigenvectors.   

    LDY     (input) INTEGER   
            The leading dimension of Y.   

    ALPHA   (input) REAL   
    BETA    (input) REAL   
            Weighting constants for matrix A.   

    WX      (input) REAL   
            Constant for right eigenvector matrix.   

    WY      (input) REAL   
            Constant for left eigenvector matrix.   

    S       (output) REAL array, dimension (N)   
            S(i) is the reciprocal condition number for eigenvalue i.   

    DIF     (output) REAL array, dimension (N)   
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
		a_ref(i__, i__) = (real) i__ + *alpha;
		b_ref(i__, i__) = 1.f;
	    } else {
		a_ref(i__, j) = 0.f;
		b_ref(i__, j) = 0.f;
	    }

/* L10: */
	}
/* L20: */
    }

/*     Form X and Y */

    slacpy_("F", n, n, &b[b_offset], lda, &y[y_offset], ldy);
    y_ref(3, 1) = -(*wy);
    y_ref(4, 1) = *wy;
    y_ref(5, 1) = -(*wy);
    y_ref(3, 2) = -(*wy);
    y_ref(4, 2) = *wy;
    y_ref(5, 2) = -(*wy);

    slacpy_("F", n, n, &b[b_offset], lda, &x[x_offset], ldx);
    x_ref(1, 3) = -(*wx);
    x_ref(1, 4) = -(*wx);
    x_ref(1, 5) = *wx;
    x_ref(2, 3) = *wx;
    x_ref(2, 4) = -(*wx);
    x_ref(2, 5) = -(*wx);

/*     Form (A, B) */

    b_ref(1, 3) = *wx + *wy;
    b_ref(2, 3) = -(*wx) + *wy;
    b_ref(1, 4) = *wx - *wy;
    b_ref(2, 4) = *wx - *wy;
    b_ref(1, 5) = -(*wx) + *wy;
    b_ref(2, 5) = *wx + *wy;
    if (*type__ == 1) {
	a_ref(1, 3) = *wx * a_ref(1, 1) + *wy * a_ref(3, 3);
	a_ref(2, 3) = -(*wx) * a_ref(2, 2) + *wy * a_ref(3, 3);
	a_ref(1, 4) = *wx * a_ref(1, 1) - *wy * a_ref(4, 4);
	a_ref(2, 4) = *wx * a_ref(2, 2) - *wy * a_ref(4, 4);
	a_ref(1, 5) = -(*wx) * a_ref(1, 1) + *wy * a_ref(5, 5);
	a_ref(2, 5) = *wx * a_ref(2, 2) + *wy * a_ref(5, 5);
    } else if (*type__ == 2) {
	a_ref(1, 3) = *wx * 2.f + *wy;
	a_ref(2, 3) = *wy;
	a_ref(1, 4) = -(*wy) * (*alpha + 2.f + *beta);
	a_ref(2, 4) = *wx * 2.f - *wy * (*alpha + 2.f + *beta);
	a_ref(1, 5) = *wx * -2.f + *wy * (*alpha - *beta);
	a_ref(2, 5) = *wy * (*alpha - *beta);
	a_ref(1, 1) = 1.f;
	a_ref(1, 2) = -1.f;
	a_ref(2, 1) = 1.f;
	a_ref(2, 2) = a_ref(1, 1);
	a_ref(3, 3) = 1.f;
	a_ref(4, 4) = *alpha + 1.f;
	a_ref(4, 5) = *beta + 1.f;
	a_ref(5, 4) = -a_ref(4, 5);
	a_ref(5, 5) = a_ref(4, 4);
    }

/*     Compute condition numbers */

    if (*type__ == 1) {

	s[1] = 1.f / sqrt((*wy * 3.f * *wy + 1.f) / (a_ref(1, 1) * a_ref(1, 1)
		 + 1.f));
	s[2] = 1.f / sqrt((*wy * 3.f * *wy + 1.f) / (a_ref(2, 2) * a_ref(2, 2)
		 + 1.f));
	s[3] = 1.f / sqrt((*wx * 2.f * *wx + 1.f) / (a_ref(3, 3) * a_ref(3, 3)
		 + 1.f));
	s[4] = 1.f / sqrt((*wx * 2.f * *wx + 1.f) / (a_ref(4, 4) * a_ref(4, 4)
		 + 1.f));
	s[5] = 1.f / sqrt((*wx * 2.f * *wx + 1.f) / (a_ref(5, 5) * a_ref(5, 5)
		 + 1.f));

	slakf2_(&c__1, &c__4, &a[a_offset], lda, &a_ref(2, 2), &b[b_offset], &
		b_ref(2, 2), z__, &c__12);
	sgesvd_("N", "N", &c__8, &c__8, z__, &c__12, work, &work[8], &c__1, &
		work[9], &c__1, &work[10], &c__40, &info);
	dif[1] = work[7];

	slakf2_(&c__4, &c__1, &a[a_offset], lda, &a_ref(5, 5), &b[b_offset], &
		b_ref(5, 5), z__, &c__12);
	sgesvd_("N", "N", &c__8, &c__8, z__, &c__12, work, &work[8], &c__1, &
		work[9], &c__1, &work[10], &c__40, &info);
	dif[5] = work[7];

    } else if (*type__ == 2) {

	s[1] = 1.f / sqrt(*wy * *wy + .33333333333333331f);
	s[2] = s[1];
	s[3] = 1.f / sqrt(*wx * *wx + .5f);
	s[4] = 1.f / sqrt((*wx * 2.f * *wx + 1.f) / ((*alpha + 1.f) * (*alpha 
		+ 1.f) + 1.f + (*beta + 1.f) * (*beta + 1.f)));
	s[5] = s[4];

	slakf2_(&c__2, &c__3, &a[a_offset], lda, &a_ref(3, 3), &b[b_offset], &
		b_ref(3, 3), z__, &c__12);
	sgesvd_("N", "N", &c__12, &c__12, z__, &c__12, work, &work[12], &c__1,
		 &work[13], &c__1, &work[14], &c__60, &info);
	dif[1] = work[11];

	slakf2_(&c__3, &c__2, &a[a_offset], lda, &a_ref(4, 4), &b[b_offset], &
		b_ref(4, 4), z__, &c__12);
	sgesvd_("N", "N", &c__12, &c__12, z__, &c__12, work, &work[12], &c__1,
		 &work[13], &c__1, &work[14], &c__60, &info);
	dif[5] = work[11];

    }

    return 0;

/*     End of SLATM6 */

} /* slatm6_ */

#undef y_ref
#undef x_ref
#undef b_ref
#undef a_ref


