#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* Subroutine */ int dprtls_(integer *isub, char *subnam, integer *ndata, 
	integer *nm, integer *mval, integer *nn, integer *nval, integer *nns, 
	integer *nsval, integer *nnb, integer *nbval, integer *nxval, integer 
	*nlda, integer *ldaval, integer *mtype, doublereal *rslts, integer *
	nout)
{
    /* Format strings */
    static char fmt_9999[] = "(/\002 M = \002,i5,\002, N = \002,i5,\002, NRH"
	    "S = \002,i5,\002, LDA = \002,i5)";
    static char fmt_9998[] = "(/\002 TYPE \002,4x,a6,1x,8(4x,\002comp.\002,i"
	    "2,:))";
    static char fmt_9997[] = "(i5,2x,1p,6g11.2)";
    static char fmt_9996[] = "(/\002 M = \002,i5,\002, N = \002,i5,\002, NRH"
	    "S = \002,i5,\002, LDA = \002,i5,\002, NB = \002,i3,\002, NX ="
	    " \002,i3)";

    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5, i__6, i__7;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static integer ilda, nrhs, m, n, idata, icase, itype, nb, im, in, nx, lda,
	     inb, ins;

    /* Fortran I/O blocks */
    static cilist io___10 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___11 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___14 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___18 = { 0, 0, 0, fmt_9996, 0 };
    static cilist io___19 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___20 = { 0, 0, 0, fmt_9997, 0 };



#define rslts_ref(a_1,a_2,a_3) rslts[((a_3)*6 + (a_2))*6 + a_1]


/*  -- LAPACK timing routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   


    Purpose   
    =======   

    DPRTLS prints a table of timing data for the least squares routines.   

    Arguments   
    =========   

    ISUB    (input) INTEGER   
            Subroutine index.   

    SUBNAM  (input) CHARACTER*6   
            Subroutine name.   

    NDATA   (input) INTEGER   
            Number of components for subroutine SUBNAM.   

    NM      (input) INTEGER   
            The number of values of M contained in the vector MVAL.   

    MVAL    (input) INTEGER array, dimension (NM)   
            The values of the matrix row dimension M.   

    NN      (input) INTEGER   
            The number of values of N contained in the vector NVAL.   

    NVAL    (input) INTEGER array, dimension (NN)   
            The values of the matrix column dimension N.   

    NNS     (input) INTEGER   
            The number of values of NRHS contained in the vector NSVAL.   

    NSVAL   (input) INTEGER array, dimension (NNS)   
            The values of the number of right hand sides NRHS.   

    NNB     (input) INTEGER   
            The number of values of NB and NX contained in the   
            vectors NBVAL and NXVAL.  The blocking parameters are used   
            in pairs (NB,NX).   

    NBVAL   (input) INTEGER array, dimension (NNB)   
            The values of the blocksize NB.   

    NXVAL   (input) INTEGER array, dimension (NNB)   
            The values of the crossover point NX.   

    NLDA    (input) INTEGER   
            The number of values of LDA contained in the vector LDAVAL.   

    LDAVAL  (input) INTEGER array, dimension (NLDA)   
            The values of the leading dimension of the array A.   

    MTYPE   (input) INTEGER   
            Number of matrix types.   

    RSLTS   (workspace) DOUBLE PRECISION array   
            dimension( 6, 6, number of runs )   

    NOUT    (input) INTEGER   
            The unit number for output.   

    =====================================================================   


       Parameter adjustments */
    rslts -= 43;
    --ldaval;
    --nxval;
    --nbval;
    --nsval;
    --nval;
    --mval;

    /* Function Body */
    icase = 1;

    i__1 = *nm;
    for (im = 1; im <= i__1; ++im) {
	m = mval[im];
	i__2 = *nn;
	for (in = 1; in <= i__2; ++in) {
	    n = nval[in];
	    i__3 = *nns;
	    for (ins = 1; ins <= i__3; ++ins) {
		nrhs = nsval[ins];
		i__4 = *nlda;
		for (ilda = 1; ilda <= i__4; ++ilda) {
/* Computing MAX */
		    i__5 = 1, i__6 = ldaval[ilda];
		    lda = max(i__5,i__6);
		    if (*isub == 2) {
			io___10.ciunit = *nout;
			s_wsfe(&io___10);
			do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&nrhs, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&lda, (ftnlen)sizeof(integer));
			e_wsfe();
			io___11.ciunit = *nout;
			s_wsfe(&io___11);
			do_fio(&c__1, subnam, (ftnlen)6);
			i__5 = *ndata - 1;
			for (idata = 1; idata <= i__5; ++idata) {
			    do_fio(&c__1, (char *)&idata, (ftnlen)sizeof(
				    integer));
			}
			e_wsfe();
			i__5 = *mtype;
			for (itype = 1; itype <= i__5; ++itype) {
			    io___14.ciunit = *nout;
			    s_wsfe(&io___14);
			    do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
				    integer));
			    i__6 = *ndata;
			    for (idata = 1; idata <= i__6; ++idata) {
				do_fio(&c__1, (char *)&rslts_ref(idata, itype,
					 icase), (ftnlen)sizeof(doublereal));
			    }
			    e_wsfe();
/* L10: */
			}
			++icase;
		    } else {
			i__5 = *nnb;
			for (inb = 1; inb <= i__5; ++inb) {
			    nb = nbval[inb];
			    nx = nxval[inb];
			    io___18.ciunit = *nout;
			    s_wsfe(&io___18);
			    do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer))
				    ;
			    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer))
				    ;
			    do_fio(&c__1, (char *)&nrhs, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&lda, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&nb, (ftnlen)sizeof(integer)
				    );
			    do_fio(&c__1, (char *)&nx, (ftnlen)sizeof(integer)
				    );
			    e_wsfe();
			    io___19.ciunit = *nout;
			    s_wsfe(&io___19);
			    do_fio(&c__1, subnam, (ftnlen)6);
			    i__6 = *ndata - 1;
			    for (idata = 1; idata <= i__6; ++idata) {
				do_fio(&c__1, (char *)&idata, (ftnlen)sizeof(
					integer));
			    }
			    e_wsfe();
			    i__6 = *mtype;
			    for (itype = 1; itype <= i__6; ++itype) {
				io___20.ciunit = *nout;
				s_wsfe(&io___20);
				do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
					integer));
				i__7 = *ndata;
				for (idata = 1; idata <= i__7; ++idata) {
				    do_fio(&c__1, (char *)&rslts_ref(idata, 
					    itype, icase), (ftnlen)sizeof(
					    doublereal));
				}
				e_wsfe();
/* L20: */
			    }
			    ++icase;
/* L30: */
			}
		    }
/* L40: */
		}
/* L50: */
	    }
/* L60: */
	}
/* L70: */
    }

    return 0;

/*     End of DPRTLS */

} /* dprtls_ */

#undef rslts_ref


