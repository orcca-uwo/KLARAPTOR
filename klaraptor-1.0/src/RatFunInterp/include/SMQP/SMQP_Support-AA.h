
/*****
 * Supporting methods for sparse multivariate ratioanl polynomials written in 
 * pure C. 
 *
 * Throughout this it is assumed that polynomials are compatiable. They must have
 * the same number of variables and the same variable ordering. 
 *
 * Lexicographical ordering is used for term ordering throughout. 
 *
 * Functions assume that the number of variables is at least 1. But, in the most
 * basic case, a NULL exponent vector encodes nvar = 0. Similar to how Node* = NULL
 * encodes the zero polynomial.
 *****/

#ifndef _SMQP_SUPPORT_AA_H_
#define _SMQP_SUPPORT_AA_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <gmp.h>

///**
// * Defines whether to use chained heaps over basic heaps.
// */
// #define SMQP_SUPPORT_CHAINED_HEAP 1

/**
 * Compile methods for debugging SMQP Support.
 */
#define SMQP_SUPPORT_DEBUG 0

/** 
 * Define if SMQP should count number of comparisons in a global variable
 */
#define SMQP_COUNT_COMPARISONS 0

/**
 * Define if SMQP should count heap chains and inserts.
 */
#define SMQP_COUNT_CHAINS 0

/**
 * Define if the product heap should use indices or pointers.
 */
#define SMQP_INT_PRODHEAP 1


/*****************
 * Data types used throughout 
 *****************/

typedef unsigned long long int polysize_t;

typedef mpq_t ratNum_t;


/*****************
 * Exponent Vector functions.
 *****************/

#include "ExponentPackingDefines.h"

#if SMQP_COUNT_COMPARISONS
	static unsigned long long smqpCompareCount = 0;
#endif



/*****************
 * Node defintion and helper functions.
 *****************/

/**
 * A Node struct containing a single term of a polynomial, that is, 
 * the coefficient and the monomial's exponent vector. It also contains
 * a pointer to the next Node in the polynomial. In this way, a Node represents
 * both a single term and a polynomial. 
 *
 * Note that a NULL Node is considered to be 0. 
 */

typedef struct Node {
	ratNum_t coef;
	degrees_t degs;
	// a pointer connecting to the next term
	struct Node* next;
} Node;

/**
 * Free a node and its exponent vector
 */
static inline void freeNode(Node* node) {
	if (node != NULL) {
		mpq_clear(node->coef);
		// free(node->degs);
		free(node);
	}
}

/**
 * Create a copy of supplied Node, except for the next pointer.
 * returns this new copy.
 */
static inline Node* deepCopyNode(Node* node, int nvar) {
	Node* newNode = (Node*) calloc(1, sizeof(Node));
	newNode->degs = node->degs;
	mpq_init(newNode->coef);
	mpq_set(newNode->coef, node->coef);
	newNode->next = NULL;

	return newNode;
}

/**
 * Free a node, and all it's sucessors. 
 */
void freePolynomial(Node* node);

/**
 * Deep copy the polynomail whose head in the input Node*.
 */
Node* deepCopyPolynomial(Node* node, int nvar);

/**
 * Determine the number of terms in a polynomial given the head node 
 * of the linked-list representation of the polynomial.
 * returns the number of terms including and following the supplied node 
 */
polysize_t numberOfTermsNode(Node* node);

/**
 * Given a linked-list of nodes, whose head is poly, 
 * sort the list based on the ordering of compareExponentVectors.
 * Updates the polynomial in place by modifying *poly to be the new head node.
 * Also returns a pointer which is the new head node. 
 */
Node* sortPolynomial(Node** poly, int nvar);

/**
 * Given a polynomial in sorted order but with possible duplicates, 
 * condense the list such that like-terms are combined into a single node.
 */
void condensePolyomial(Node* poly, int nvar);

/**
 * Add a term to the end of the polynomial linked list given the tail node,
 * trailingTerm, the exponent vector for the new term, d, and the coefficient, 
 * ratNum_t. A copy of the input coef is made and stored in the node. But the
 * degrees_t is stored directly. 
 * 
 * returns a pointer to the new tail of the list (the node created)
 */
Node* addTerm(Node* trailingTerm, degrees_t d, const ratNum_t coef);

/**
 * Add a term to the end of linked list with tail trailingTerm. The new term
 * will have degs d and a coef of 0.
 * 
 * returns a pointer to the new tail of the list (the node created)
 */
Node* addZeroTerm(Node* trailingTerm, degrees_t d);

/** 
 * Given two terms, as nodes a and b, multiply these two and return the
 * single result as a node.
 */
Node* multiplyTerms(Node* a, Node* b, int nvar);

/**
 * Negate a polynomial. 
 * Given the head of a polynomial, negate all coefficients in place.
 */
void negatePolynomial(Node* a);

/**
 * Evaluate a polynomial whose head is given by a. 
 * This method returns another polynomial as not all variables need
 * to have values supplied. But, if they do, a constant term will be returned. 
 * both active and vals are arrays of size nvar. active[i] determines if 
 * the variable at degs[i] is to be evaluated using vals[i] as value.
 */
Node* evaluatePoly(const Node* a, const int* active, const mpq_t* vals, int nvar);

/**
 * Given polynomial a, return the leading term
 */
Node* leadingTerm (Node* a);


/*****************
 * Alternating Array definition and helpers
 *****************/

typedef struct AAElem {
	ratNum_t coef;
	degrees_t degs;
} AAElem_t;

typedef struct AltArr {
	int size;
	int alloc;
	int nvar;
	AAElem_t* elems;
} AltArr_t;

#define AA_SIZE(A) ((A)->size)
#define AA_ALLOC(A) ((A)->alloc)

static inline void freePolynomial_AA(AltArr_t* aa) {
	if (aa != NULL) {
		int size = aa->size;
		AAElem_t* elems = aa->elems;
		for (int i = 0; i < size; ++i) {
			mpq_clear(elems[i].coef);
		}
		free(aa->elems);
		free(aa);
	}
}

typedef struct AAElem_DegList {
	ratNum_t coef;
	degree_t* degs;
} AAElem_DegList_t;

typedef struct AltArrDegList {
	int size;
	int alloc;
	int nvar;
	AAElem_DegList_t* elems;
} AltArrDegList_t;

static inline void freePolynomial_AADL(AltArrDegList_t* aa) {
	if (aa != NULL) {
		int size = aa->size;
		AAElem_DegList_t* elems = aa->elems;
		for (int i = 0; i < size; ++i) {
			mpq_clear(elems[i].coef);
			free(elems[i].degs);
		}
		free(aa->elems);
		free(aa);
	}
}

void printPolyDouble_AA(FILE* fp, AltArr_t* aa, char** syms, int nvar);

void printPoly_AA(FILE* fp, AltArr_t* aa, char** syms, int nvar);

void printLocalAA(AltArr_t* aa, char** syms, int nvar);

/**
 * Create a new polynomial alternating array with a specified allocation size.
 * The array is not initialized.
 */
static inline AltArr_t* makePolynomial_AA(int allocSize, int nvar) {
	if (allocSize < 1) {
		return NULL;
	}
	AltArr_t* newAA = (AltArr_t*) malloc(sizeof(AltArr_t));
	newAA->size = 0; 
	newAA->alloc = allocSize;
	newAA->nvar = nvar;
	newAA->elems = (AAElem_t*) malloc(sizeof(AAElem_t)*allocSize);
	return newAA;
}

static inline AltArrDegList_t* makePolynomial_AADL(int allocSize, int nvar) {
	if (allocSize < 1) {
		return NULL;
	}
	AltArrDegList_t* newAA = (AltArrDegList_t*) malloc(sizeof(AltArrDegList_t));
	newAA->size = 0; 
	newAA->alloc = allocSize;
	newAA->nvar = nvar;
	newAA->elems = (AAElem_DegList_t*) malloc(sizeof(AAElem_DegList_t)*allocSize);
	return newAA;	
}

static inline AltArr_t* makeConstPolynomial_AA(int allocSize, int nvar, const mpq_t coef) {
	if (allocSize < 1) {
		return NULL;
	}

	AltArr_t* newAA = (AltArr_t*) malloc(sizeof(AltArr_t));
	newAA->size = 1;
	newAA->alloc = allocSize;
	newAA->nvar = nvar;
	newAA->elems = (AAElem_t*) malloc(sizeof(AAElem_t)*allocSize);
	mpq_init(newAA->elems->coef);
	mpq_set(newAA->elems->coef, coef);
	newAA->elems->degs = 0;
	return newAA;
}

/**
 * Returns true iff a is exactly eqaul to b. Does NOT check if a and b represent
 * the same polynomial but encoded over different polynomial rings.
 */
int isExactlyEqual(AltArr_t* a, AltArr_t* b);

/**
 * Re-pack the exponent vectors of aa so that they have newNvar number of variables.
 * Expansion is done to the right. That is, exponents are packed into indices 
 * in the new exponent vector starting from 0.
 */
void expandNumVars_AA(AltArr_t* aa, int newNvar);

/**
 * Re-pack the exponent vectors of aa so that they have newNvar number of variables.
 * Expansion is done to the left. That is, exponents are packed so that the the 
 * leading exponents in the new exponent vector are 0.
 */
void expandNumVarsLeft_AA(AltArr_t* aa, int newNvar);

/**
 * Re-pack the exponent vectors of aa so they have one less variable. 
 * This re-packing is done such that the exponent at index idx is discarded completely. 
 * NOTE sorting may be needed after a call to this function, depending on the circumstances.
 * No combination of like-terms or re-ordering is done here.
 */
void shrinkNumVarsAtIdx_AA(AltArr_t* aa, int idx);

/**
 * Like reorderVars except that the varMap can map to indices less than 0. That is, 
 * if varMap[i] < 0 then variable at index i is removed from the exponent vector.
 *
 * It is assumed that the polynomial aa has zero exponents for all such i. 
 */
void shrinkAndReorderVars_AA(AltArr_t* aa, int* varMap, int varmapSize);

/**
 * Given a mapping between current exponent indices and new ones (such that
 * exponent at index i is moved to index varMap[i]), reorder the exponent
 * vectors of aa.
 *
 * For variables of index >= varMapSize, it is assumed they are all zero and so
 * their mapped position is not important.
 */
void reorderVars_AA(AltArr_t* aa, int* varMap, int varMapSize);

degrees_t calculateMaxDegs_AA(const AltArr_t* aa);

/**
 * Alloc more (or less) space for the elements of an alternating array. 
 * If the requested alloc size is less than the number of elements currently
 * in the array, then those excess elements are freed.
 */
static inline void resizePolynomial_AA(AltArr_t* aa, int allocSize) {
	if (allocSize < aa->size) {
		for (int i = aa->size; i >= allocSize; --i) {
			mpq_clear(aa->elems[i].coef);
		}
		aa->size = allocSize;
	}
	aa->elems = (AAElem_t*) realloc(aa->elems, sizeof(AAElem_t)*allocSize);
	aa->alloc = allocSize;
}

/**
 * Construct an alternating array polynomial representation from a Node rep.
 *
 * returns the new alternating array pointer. 
 */
AltArr_t* deepCopyPolynomial_AAFromNode(Node* a, int nvar);

/**
 * Construct an linked list polynomial representation from an alternating array rep.
 *
 * returns the head node pointer of the linked list. 
 */
Node* deepCopyPolynomial_NodeFromAA(AltArr_t* aa);

/**
 * Construct an alternating array with un-packed exponents from an alternating
 * array with packed exponents. 
 */
AltArrDegList_t* deepCopyPolynomial_AADegListFromAA(AltArr_t* aa);

/** 
 * Create a deep copy of the polynomial represented by the alternating array aa.
 * 
 * returns a pointer to the new alternating array.
 */
AltArr_t* deepCopyPolynomial_AA(AltArr_t* aa);

/**
 * Sort a polynomial in place represented by an alternating array.
 *
 * returns the same pointer given.
 */
AltArr_t* sortPolynomial_AA(AltArr_t* aa);

/**
 * Sort a polynomial in place using merge sort.
 */
void mergeSortPolynomial_AA(AltArr_t* aa);

/**
 * Given a polynomial in sorted order but with possible monomial duplicates, 
 * combine like terms and condense the polynomial.
 */
void condensePolyomial_AA(AltArr_t* aa);

/**
 * Negate a polynomial in place.
 */
void negatePolynomial_AA(AltArr_t* a);

/**
 * Multiply through a polynomial by a single rational number.
 */
void multiplyByRational_AA_inp(AltArr_t* aa, const mpq_t z);

/**
 * Evaluate a polynomial represented as an alternating array.
 * This method returns another polynomial as not all variables need
 * to have values supplied. But, if they do, a constant term will be returned. 
 * both active and vals are arrays of size nvar. active[i] determines if 
 * the variable at degs[i] is to be evaluated using vals[i] as value.
 */
AltArr_t* evaluatePoly_AA(AltArr_t* aa, int* active, ratNum_t* vals, int nvar);

/**
 * Evaluate a polynomial represented as an alternating array.
 * This method returns the rational number vlaue in res.
 * vals[i] corresponds to the value for variable i in aa.
 */
void evalPolyToVal_AA(const AltArr_t* aa, ratNum_t* vals, int nvar, ratNum_t res);

AltArr_t* convertFromAAElemToAA (AAElem_t* coef, int coefSize, int nvar);


AltArr_t* swappingExponents_AA (AltArr_t* aa, int idx1, int idx2);


/**
 * Given polynomial a, return the leading term
 */
AltArr_t* leadingTerm_AA (AltArr_t* aa, int nvar);

/**
 * Get the leading variable, that is, the highest-order variable with positive degree
 * of this polynomial.
 * returns the postion of leading variable or -1 if this polynomial has zero variables.
 */
int leadingVariable_AA (AltArr_t* aa);

/**
 * given two polynomials, return polynomial with bigger leading term
 * Note: return polynomial is a deep copy of the polynomial
 */
AltArr_t* maxPolynomials_AA (AltArr_t* a, AltArr_t* b);

/**
 * given two polynomials, return polynomial with bigger leading term
 * Note: Is is done INPLACE w.r.t. the first input, a.
 */
AltArr_t* maxPolynomials_AA_inp (AltArr_t* a, AltArr_t* b);

/*****************
 * SMQP Addition
 *****************/

/**
 * Add two polynomials given their head nodes, a and b.
 * nvar: number of variables in the polynomials.
 * returns a pointer to the head Node of the sum.
 */
Node* addPolynomials(Node *a, Node *b, int nvar);

/**
 * Add two polynomials given their alternating arrays, a and b.
 * nvar: number of variables in the polynomials.
 * returns a pointer to the sum alternating array.
 */
AltArr_t* addPolynomials_AA(AltArr_t* a, AltArr_t* b, int nvar);

AltArr_t* subPolynomials_AA(AltArr_t* a, AltArr_t* b, int nvar);

/**
 * Add polynomial a and b, putting the sum back into a.
 * 
 * nvar: size of exponent vectors in a and b.
 *
 * returns a pointer to the new head of a.
 */
Node* addPolynomials_inp(Node* a, Node* b, int nvar);
AltArr_t* addPolynomials_AA_inp(AltArr_t* a, AltArr_t* b, int nvar);


/**
 * Subtract b from a, putting the difference back into a.
 * 
 * nvar: size of exponent vectors in a and b.
 *
 * returns a pointer to the new head of a.
 */
Node* subPolynomials_inp(Node* a, Node* b, int nvar);
AltArr_t* subPolynomials_AA_inp(AltArr_t* a, AltArr_t* b, int nvar);


/*****************
 * SMQP Multiplication & Helpers
 *****************/

/**
 * Data element for the ProductHeap data structure
 */
typedef struct productHeapElem {
	Node* a_i;
	Node* b;
	Node* product;
	struct productHeapElem* next;
} productHeapElem;

/**
 * Data structure 'heap' holding the intermediate terms produced during multiplication.
 */
typedef struct {
	productHeapElem** elements;
	polysize_t heapSize;
	polysize_t maxHeapSize;
	int nvar;
} ProductHeap;

typedef struct ProductHeapChain_AA {
#if SMQP_INT_PRODHEAP
	int a_i;
	int b;
#else
	AAElem_t* a_i;
	AAElem_t* b;
#endif
	struct ProductHeapChain_AA* next;
} ProductHeapChain_AA;

typedef struct ProductHeapElem_AA {
	degrees_t degs;
	ProductHeapChain_AA* chain;
} ProductHeapElem_AA;

typedef struct {
	ProductHeapElem_AA* elements;
	polysize_t heapSize;
	polysize_t maxHeapSize;
	int nvar;
#if SMQP_INT_PRODHEAP
	int lastB;
#else
	AAElem_t* lastB;
#endif
} ProductHeap_AA;

#if SMQP_SUPPORT_DEBUG
/**
 * Print the product degrees_t currently in the heap.
 */
void prodheapPrint(ProductHeap* h, int nvar);
#endif

/**
 * Make an element for the product heap, combining nodes a and b as the 
 * element's product.
 */
productHeapElem* prodheapMakeElement(ProductHeap* h, Node* a, Node* b);

#if SMQP_INT_PRODHEAP
static inline ProductHeapChain_AA* prodheapMakeChain_AA(int a, int b, ProductHeapChain_AA* next) {
#else
static inline ProductHeapChain_AA* prodheapMakeChain_AA(AAElem_t* a, AAElem_t* b, ProductHeapChain_AA* next) {
#endif
	ProductHeapChain_AA* chain = (ProductHeapChain_AA*) malloc(sizeof(ProductHeapChain_AA));
	chain->a_i = a;
	chain->b = b;
	chain->next = next;
	return chain;
}

/**
 * Free a product heap element and its product node. 
 */
static inline void prodheapFreeElement(productHeapElem* elem) {
	freeNode(elem->product);
	free(elem);
}

/**
 * Cleanup memory initialized for the heap
 */
void prodheapFree(ProductHeap* h);

static inline void prodheapFreeChain_AA(ProductHeapChain_AA* chain) {
	ProductHeapChain_AA* next;
	while(chain != NULL) {
		next = chain->next;
		free(chain);
		chain = next;
	}
}

/**
 * Cleanup memory initialized for the heap
 */
static inline void prodheapFree_AA(ProductHeap_AA* h) {
	ProductHeapElem_AA* elems = h->elements;
	polysize_t s = h->heapSize;
	for (polysize_t i = 0; i < s; ++i) {
		prodheapFreeChain_AA(elems[i].chain);
	}
	free(h->elements);
	free(h); 
}

/**
 * Create an empty product heap. 
 */
ProductHeap* prodheapCreate(int nvar);

/**
 * Create an empty product heap. 
 */
static inline ProductHeap_AA* prodheapCreate_AA(int nvar) {
	ProductHeap_AA* h = (ProductHeap_AA*) malloc(sizeof(ProductHeap_AA));
	h->elements = NULL;
	h->heapSize = 0;
	h->maxHeapSize = 0;
	h->nvar = nvar;
	return h;
}

/**
 * Initialize the product heap with the two polynomials to multiply, a and b.
 *
 * We know the maximum heap size is numTerms(a) as at most one of a_i*b
 * is in the heap at once. 
 */
ProductHeap* prodheapInit(Node* a, Node* b, int nvar);

/**
 * Initialize the product heap with the two polynomials to multiply, a and b.
 *
 * We know the maximum heap size is numTerms(a) as at most one of a_i*b
 * is in the heap at once. 
 */
ProductHeap_AA* prodheapInit_AA(AltArr_t* a, AltArr_t* b, int nvar);

/**
 * Increase the capacity of the heap to newAllocSize. 
 */
static inline void prodheapResize_AA(ProductHeap_AA* h, int newAllocSize) {
	h->elements = (ProductHeapElem_AA*) realloc(h->elements, sizeof(ProductHeapElem_AA)*newAllocSize);
	h->maxHeapSize  = newAllocSize;
}

/**
 * Given an index, i, into the heap, swim that node up so that the heap 
 * is properly ordered. 
 */
void prodheapSwim(ProductHeap* h, int index);

/**
 * Given an index, i, into the heap, sink that node down so that the heap 
 * is properly ordered. 
 * Note: This is not used in the chained heap case
 */
void prodheapSink(ProductHeap* h, int i);

#if SMQP_COUNT_CHAINS
	static unsigned long long int SMQP_CHAINS = 0;
	static unsigned long long int SMQP_INSERTS = 0;
#endif 

/**
 * Insert a new element, elem, into the product heap, h, chaining as possible.
 */
void prodheapInsert(ProductHeap* h, productHeapElem* elem);

/**
 * Insert a new element, elem, into the product heap, h, chaining as possible.
 */
#if SMQP_INT_PRODHEAP
void prodheapInsert_AA(ProductHeap_AA* h, ProductHeapChain_AA* chain, register degrees_t degs);
#else
void prodheapInsert_AA(ProductHeap_AA* h, ProductHeapChain_AA* elem);
#endif

/**
 * Peek into the heap, h, to get the exponent vector of the product
 * of the max element.
 */
static inline degrees_t* prodheapPeek(ProductHeap* h) {
	if (h->heapSize > 0) {
		return &(h->elements[0]->product->degs);
	}
	return NULL;
}

/**
 * Peak into the heap and get the exponent vector of the product of the max elem.
 * NOTE that a non-NULL pointer is invalidated after calling prodheapRemoveMax on h.
 */
static inline degrees_t* prodheapPeek_AA(ProductHeap_AA* h) {
	if (h->heapSize > 0) {
		return &(h->elements->degs);
	}
	return NULL;
}

/**
 * Extract the maximal heap element (chain) from the heap and return it.
 * Automatic insertion of the next element, a_i * b_j+1, is not done.
 * This allows the multiplication to limit the number of entries in the heap.
 * returns NULL if no such element in the heap.
 */
productHeapElem* prodheapRemoveMax(ProductHeap* h);

/**
 * Extract the maximal heap element (chain) from the heap and return it.
 * Automatic insertion of the next element, a_i * b_j+1, is not done.
 * This allows the multiplication to limit the number of entries in the heap.
 * NOTE it is only valid ot call RemoveMax if prodheapPeek_AA returns non-NULL.
 */
ProductHeapChain_AA* prodheapRemoveMax_AA(ProductHeap_AA* h);

/**
 * Extract the maximum product from the heap, returned as the Node*. 
 * This automatically adds the next term in the stream of the chosen maximum
 * if such a term exists.
 */
Node* prodheapExtract(ProductHeap* h);

/**
 * Multiply two polynomials given their head Nodes, a and b.
 * This algorithm makes use of heaps as an efficient search data structure. 
 * It is assumed that both a and b have compatible exponent vectors.
 * 
 * nvar: number of elements in the exponent vectors.
 *
 * returns a pointer to the head Node of the product polynomial.
 */
Node* multiplyPolynomials(Node* a, Node* b, int nvar);

/**
 * Multiply two polynomials given their head Nodes, a and b.
 * This algorithm makes use of heaps as an efficient search data structure. 
 * It is assumed that both a and b have compatible exponent vectors.
 * 
 * nvar: number of elements in the exponent vectors.
 *
 * returns a pointer to the head Node of the product polynomial.
 */
AltArr_t* multiplyPolynomials_AA(AltArr_t* a, AltArr_t* b, int nvar);

/**
 * Multiply two polynomials in-place wrt the first polynomial.
 * That is, the multiplication occurs, reusing elements of a as much as possible
 * and the previous content of a is destroyed in the process.
 * It is assumed that a and b have the same sized exponent vectors. 
 *
 *
 * nvar: number of elements in the exponent vectors.
 *
 * returns a pointer to the new head node of a.
 *
 */
Node* multiplyPolynomials_inp(Node* a, Node* b, int nvar);

/**
 * Multiply two polynomials in-place wrt the first polynomial.
 * That is, the multiplication occurs, reusing elements of a as much as possible
 * and the previous content of a is destroyed in the process.
 * It is assumed that a and b have the same sized exponent vectors. 
 *
 *
 * nvar: number of elements in the exponent vectors.
 *
 * returns the product as a new AltArr_t.
 *
 */
AltArr_t* multiplyPolynomials_AA_inp(AltArr_t* a, AltArr_t* b, int nvar);



/*****************
 * Polynomial exponentiation 
 *****************/

/**
 * Given a polynomial, a, compute a^n.
 * 
 * n: a positive integer
 * nvar: the number of variables of a
 * 
 */
Node* exponentiatePoly(Node* a, unsigned int n, int nvar);


/**
 * Given a polynomial, a, compute a^n.
 *
 * n: a positive integer
 * nvar: the number of variables of a.
 *
 */
AltArr_t* exponentiatePoly_AA(AltArr_t* a, unsigned int n, int nvar);



/*****************
 * Polynomial division
 *****************/

/**
 * Extract a product term from the heap.
 * This product term is a_i*b_j for some i and j.
 * If the term b_(j+1) exists then the heap is updated by inserting a_i*b_(j+1).
 * This process continues as long as the next element in the heap has the same
 * product degree_t
 */
Node* divisionGetNextTerm(ProductHeap* h);

#if SMQP_INT_PRODHEAP 
void divisionGetNextTerm_AA(ProductHeap_AA* h, const AAElem_t* __restrict__ aElems, const AAElem_t* __restrict__ bElems, mpq_t* retCoef);
#else 
void divisionGetNextTerm_AA(ProductHeap_AA* h, mpq_t* retCoef);
#endif

/** 
 * Given a polynomial, c, and a term, b, determine polynomials a and r
 * such that c = b*a + r.
 * a and r are returned in res_a and res_r, respectively. 
 */
void divideBySingleTerm(Node* c, Node* b, Node** res_a, Node** res_r, int nvar);

/** 
 * Given a polynomial, c, and a term, b, determine polynomials a and r
 * such that c = b*a + r.
 * a and r are returned in res_a and res_r, respectively. 
 */
void divideBySingleTerm_AA(AltArr_t* c, AltArr_t* b, AltArr_t** res_a, AltArr_t** res_r, int nvar);

/** 
 * Given two polynomials, c and b, find their quotient and remainder such that
 * c = b*a + r. The quotient a is returned in res_a, and the remainder r in res_r 
 * Based on Stephen Johnson's "Sparse Polynomial Arithmetic".
 */
void dividePolynomials(Node* c, Node* b, Node** res_a, Node** res_r, int nvar);

/** 
 * Given two polynomials, c and b, find their quotient and remainder such that
 * c = b*a + r. The quotient a is returned in res_a, and the remainder r in res_r 
 * Based on Stephen Johnson's "Sparse Polynomial Arithmetic".
 */
void dividePolynomials_AA(AltArr_t* c, AltArr_t* b, AltArr_t** res_a, AltArr_t** res_r, int nvar);

/**
 * Perform pseudodivision of c by b, both univariate. The pseudoquotient is returned in 
 * res_a and the pseudoremainder in res_r. 
 *
 * The actual number of division steps performed is returned in e such that lc(b)^e*c = qb + r;
 * If lazy is 0 then e = deg(c) - deg(b) + 1;
 *
 */
void univariatePseudoDividePolynomials_AA(AltArr_t* c, AltArr_t* b, AltArr_t** res_a, AltArr_t** res_r, int* e, int lazy);

/**
 * Determine if a polynomial, c, is exactly divisble by another, b.
 * If the division is exact, returns the quoteint in res_a. 
 * Otherwise, the value of res_a is undefined.
 * returns 1 iff division is exact. 0 otherwise.
 */
int divideTest_AA(AltArr_t* c, AltArr_t* b, AltArr_t** res_a, int nvar);



/*****************
 * Derivative / Integral
 *****************/

/**
 * Get the k-th partial derivative of aa for the variable at index idx.
 *
 * returns the partial derivative
 */
AltArr_t* derivative_AA(AltArr_t* aa, int idx, int k);


/**
 * Get the k-th partial integral of aa for the variable at index idx;
 *
 * Note: constants of integration are not included.
 *
 * returns the partial integral.
 */
AltArr_t* integral_AA(AltArr_t* aa, int idx, int k);



/*****************
 * Content, PrimitivePart, etc.
 *****************/

/**
 * Get the "integral" content of a polynomial.
 * That is, the rational number content whose corresponding
 * primitive part is a primitive integer polynomial.
 *
 * returns the content through ret parameter.
 */
void integralContent_AA(AltArr_t* aa, mpq_t ret);

/**
 * Get the primitive part, a primitive integer polynomial, of aa.
 *
 * returns the primitive part.
 */
AltArr_t* primitivePart_AA(AltArr_t* aa);

/**
 * Get the primitive part, a primitive integer polynomial, of aa.
 * Returns the integral content in cont.
 *
 * returns the primitive part.
 */
AltArr_t* primitivePartAndContent_AA(AltArr_t* aa, mpq_t cont);

/**
 * Convert the input aa to its primitive part in place.
 */
void primitivePart_AA_inp(AltArr_t* aa);

/**
 * Given to polynomials, both with 1 variable, get their GCD.
 *
 * returns the GCD.
 */
AltArr_t* univariateGCD_AA(AltArr_t* a, AltArr_t* b);

/**
 * Test for if the polynomial aa is actually an integer polynomial.
 * If so, returns the integral content in mpzG. Otherwise, mpzG is set to 0.
 */
void integerPolynomialTest_AA(AltArr_t* aa, mpz_t mpzG);

/**
 * Given a polynomial, find a monic factor common to all terms of the polynomial.
 * If factored is not NULL then the input polynomial with common factor removed
 * is returned in factored.
 *
 * returns the common factor. 
 */
AltArr_t* commonFactor_AA(AltArr_t* a, AltArr_t** factored);



/*****************
 * Interpolation
 *****************/

/**
 * Interpolate a univariate polynomial of degree nPoints-1 for the point-value
 * pairs specified by points and vals arrays. There are treated as parallel arrays.
 *
 * returns the interpolating polynomial. 
 */
AltArr_t* univarInterpolate_AA(mpq_t* points, mpq_t* vals, int nPoints);

/**
 * Interpolate a univariate polynomial of degree nPoints-1 for the point-value
 * pairs specified by points and vals arrays. There are treated as parallel arrays.
 * The points and values are doubles, to facilitate numerical sources. These
 * doubles are converted to rational number coefficients.
 *
 * returns the interpolating polynomial with multi-precision coefficients. 
 */
AltArr_t* univarInterpolateDoubles_AA(double* points, double* vals, int nPoints);

/**
 * Evaluate a univariate polynomial at the point, point. The result is 
 * returned as a multi-precision rational number, res.
 */
void univarEvaluate_AA(AltArr_t* aa, const mpq_t point, mpq_t res);

/**
 * Convert an alternating array into a C program that can be
 * compiled to produce the same alternating array.
 * polyVar is the name of the alternating array variable in the generated code.
 *
 * returns the generated code as a string.
 */
char* polyToProgram_AA(AltArr_t* aa, char* polyVar);

#ifdef __cplusplus
}
#endif

#endif

