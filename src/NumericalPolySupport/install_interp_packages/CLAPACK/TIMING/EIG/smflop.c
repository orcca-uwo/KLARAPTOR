#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

doublereal smflop_(real *ops, real *time, integer *info)
{
    /* System generated locals */
    real ret_val, r__1;


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 29, 1992   


    Purpose   
    =======   

       SMFLOP computes the megaflop rate given the number of operations   
       and time in seconds.  This is basically just a divide operation,   
       but care is taken not to divide by zero.   

    Arguments   
    =========   

    OPS    - REAL   
             On entry, OPS is the number of floating point operations   
             performed by the timed routine.   

    TIME   - REAL   
             On entry, TIME is the total time in seconds used by the   
             timed routine.   

    INFO   - INTEGER   
             On entry, INFO specifies the return code from the timed   
             routine.  If INFO is not 0, then SMFLOP returns a negative   
             value, indicating an error.   

    ===================================================================== */


    if (*time <= 0.f) {
	ret_val = 0.f;
    } else {
	ret_val = *ops / (*time * 1e6f);
    }
    if (*info != 0) {
	ret_val = -(r__1 = (real) (*info), dabs(r__1));
    }
    return ret_val;

/*     End of SMFLOP */

} /* smflop_ */

