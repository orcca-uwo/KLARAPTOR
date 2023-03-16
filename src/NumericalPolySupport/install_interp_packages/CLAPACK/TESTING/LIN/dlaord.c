#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Subroutine */ int dlaord_(char *job, integer *n, doublereal *x, integer *
	incx)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static doublereal temp;
    static integer i__;
    extern logical lsame_(char *, char *);
    static integer ix, ixnext, inc;


/*  -- LAPACK auxiliary routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 29, 1992   


    Purpose   
    =======   

    DLAORD sorts the elements of a vector x in increasing or decreasing   
    order.   

    Arguments   
    =========   

    JOB     (input) CHARACTER   
            = 'I':  Sort in increasing order   
            = 'D':  Sort in decreasing order   

    N       (input) INTEGER   
            The length of the vector X.   

    X       (input/output) DOUBLE PRECISION array, dimension   
                           (1+(N-1)*INCX)   
            On entry, the vector of length n to be sorted.   
            On exit, the vector x is sorted in the prescribed order.   

    INCX    (input) INTEGER   
            The spacing between successive elements of X.  INCX >= 0.   

    =====================================================================   


       Parameter adjustments */
    --x;

    /* Function Body */
    inc = abs(*incx);
    if (lsame_(job, "I")) {

/*        Sort in increasing order */

	i__1 = *n;
	for (i__ = 2; i__ <= i__1; ++i__) {
	    ix = (i__ - 1) * inc + 1;
L10:
	    if (ix == 1) {
		goto L20;
	    }
	    ixnext = ix - inc;
	    if (x[ix] > x[ixnext]) {
		goto L20;
	    } else {
		temp = x[ix];
		x[ix] = x[ixnext];
		x[ixnext] = temp;
	    }
	    ix = ixnext;
	    goto L10;
L20:
	    ;
	}

    } else if (lsame_(job, "D")) {

/*        Sort in decreasing order */

	i__1 = *n;
	for (i__ = 2; i__ <= i__1; ++i__) {
	    ix = (i__ - 1) * inc + 1;
L30:
	    if (ix == 1) {
		goto L40;
	    }
	    ixnext = ix - inc;
	    if (x[ix] < x[ixnext]) {
		goto L40;
	    } else {
		temp = x[ix];
		x[ix] = x[ixnext];
		x[ixnext] = temp;
	    }
	    ix = ixnext;
	    goto L30;
L40:
	    ;
	}
    }
    return 0;

/*     End of DLAORD */

} /* dlaord_ */

