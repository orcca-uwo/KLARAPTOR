#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    integer m, n, mplusn, i__;
    logical fs;
} mn_;

#define mn_1 mn_

logical slctsx_(real *ar, real *ai, real *beta)
{
    /* System generated locals */
    logical ret_val;


/*  -- LAPACK test routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   


    Purpose   
    =======   

    This function is used to determine what eigenvalues will be   
    selected.  If this is part of the test driver SDRGSX, do not   
    change the code UNLESS you are testing input examples and not   
    using the built-in examples.   

    Arguments   
    =========   

    AR      (input) REAL   
            The numerator of the real part of a complex eigenvalue   
            (AR/BETA) + i*(AI/BETA).   

    AI      (input) REAL   
            The numerator of the imaginary part of a complex eigenvalue   
            (AR/BETA) + i*(AI).   

    BETA    (input) REAL   
            The denominator part of a complex eigenvalue   
            (AR/BETA) + i*(AI/BETA).   

    ===================================================================== */


    if (mn_1.fs) {
	++mn_1.i__;
	if (mn_1.i__ <= mn_1.m) {
	    ret_val = FALSE_;
	} else {
	    ret_val = TRUE_;
	}
	if (mn_1.i__ == mn_1.mplusn) {
	    mn_1.fs = FALSE_;
	    mn_1.i__ = 0;
	}
    } else {
	++mn_1.i__;
	if (mn_1.i__ <= mn_1.n) {
	    ret_val = TRUE_;
	} else {
	    ret_val = FALSE_;
	}
	if (mn_1.i__ == mn_1.mplusn) {
	    mn_1.fs = TRUE_;
	    mn_1.i__ = 0;
	}
    }

/*       IF( AR/BETA.GT.0.0 )THEN   
            SLCTSX = .TRUE.   
         ELSE   
            SLCTSX = .FALSE.   
         END IF */

    return ret_val;

/*     End of SLCTSX */

} /* slctsx_ */

