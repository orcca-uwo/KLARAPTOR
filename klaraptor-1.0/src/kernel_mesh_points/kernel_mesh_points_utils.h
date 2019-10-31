/*!
 \file kernel_mesh_points_utils.h
 \author Davood Mohajerani <dmohajer@uwo.ca>
 \author Linxiao Wang <lwang739@uwo.ca>
 \brief
 */

///////////////////////////////////////
#ifndef KERNEL_MESH_POINTS_UTILS_H_
#define KERNEL_MESH_POINTS_UTILS_H_

///////////////////////////////////////
typedef enum point_type_enum {
	POINT_TYPE_1D, POINT_TYPE_2D, POINT_TYPE_3D, POINT_TYPE_4D, POINT_TYPE_5D
} point_type_t;

///////////////////////////////////////
typedef struct single_point_s {
	int N[5];
} single_point_t;

///////////////////////////////////////
typedef struct point_set_s {
	int size;
	single_point_t * values;
} point_set_t;

///////////////////////////////////////
int point_set_copy(point_set_t*dest, const point_set_t *src) {
	size_t values_size = (src->size) * sizeof(single_point_t);
	dest->size = src->size;
	dest->values = (single_point_t*) malloc(values_size);
	memcpy(dest->values, src->values, values_size);
	return EXIT_SUCCESS;
}

///////////////////////////////////////
int point_set_clear(point_set_t*dest) {
	if (dest->values)
		free(dest->values);
}

///////////////////////////////////////

#endif
