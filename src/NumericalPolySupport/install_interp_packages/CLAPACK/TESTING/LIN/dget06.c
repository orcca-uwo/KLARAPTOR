#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

doublereal dget06_(doublereal *rcond, doublereal *rcondc)
{
    /* System generated locals */
    doublereal ret_val;

    /* Local variables */
    extern doublereal dlamch_(char *);
    static doublereal rat, eps;


/*  -- LAPACK test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    DGET06 computes a test ratio to compare two values for RCOND.   

    Arguments   
    ==========   

    RCOND   (input) DOUBLE PRECISION   
            The estimate of the reciprocal of the condition number of A,   
            as computed by DGECON.   

    RCONDC  (input) DOUBLE PRECISION   
            The reciprocal of the condition number of A, computed as   
            ( 1/norm(A) ) / norm(inv(A)).   

    ===================================================================== */


    eps = dlamch_("Epsilon");
    if (*rcond > 0.) {
	if (*rcondc > 0.) {
	    rat = max(*rcond,*rcondc) / min(*rcond,*rcondc) - (1. - eps);
	} else {
	    rat = *rcond / eps;
	}
    } else {
	if (*rcondc > 0.) {
	    rat = *rcondc / eps;
	} else {
	    rat = 0.;
	}
    }
    ret_val = rat;
    return ret_val;

/*     End of DGET06 */

} /* dget06_ */

