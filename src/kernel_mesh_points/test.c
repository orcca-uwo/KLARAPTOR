#include <stdio.h>
#include "kernel_mesh_points.h"

///////////////////////////////////////

// void print_kernel_mesh_points_couple(couple * point_list, int n)
// {
// 	couple* p = point_list;
// 	//p = (&point_list);
// 	for (int i = 0; i < n; i++)
// 	{
// 		//p = (&point_list[i]);

// 		printf("[%d, %d] \n", p->N, p->b);
// 		p++;
// 	}
// }

///////////////////////////////////////
//
//triple * get_next_kernel_mesh_point(triple * current_point_list)
//{
//	triple* p;
//	p=current_point_list;
//	p++;
//	if (p->N[0] && p->N[1] && p->N[2])
//	{
//		return p;
//	}
//	else
//	{
//		printf("WARNING: END OF THE LIST!\n");
//		return NULL;
//	}
//}

///////////////////////////////////////

void print_kernel_mesh_points_triple(triple * point_list)
{
	triple* p;
	p = (&point_list[0]);
//	for (int i = 0; i < n; i++)
	while (p->N[0] && p->N[1] && p->N[2])
	{
		printf("[%d ,%d ,%d] \n", p->N[0], p->N[1], p->N[2]);
		p++;
	}
	printf("[END OF LIST]"
			"\n"
			"====================="
			"\n");
}

////////////////////////////////////////
void print_kernel_mesh_points_pentuple(pentuple * point_list)
{
	pentuple* p;
	p = (&point_list[0]);
//	for (int i = 0; i < n; i++)
	while (p->N[0] && p->N[1] && p->N[2] && p-> N[3] && p-> N[4])
	{
		printf("[N=%-9d ,pencil=%3d ,mx=%3d, my=%3d, mz=%3d] \n", p->N[0], p->N[1], p->N[2], p->N[3], p->N[4]);
		p++;
	}
	printf("[END OF LIST]"
			"\n"
			"====================="
			"\n");
}

///////////////////////////////////////

int test_kernel_mesh_points(int argc, char ** argv)
{
	printf("===================\n");
	printf("points for 1D problem-1D kernel:\n");
	printf("===================\n");
	print_kernel_mesh_points_triple(&global_p1k1_list);

	printf("===================\n");
	printf("points for 2D problem-1D kernel:\n");
	printf("===================\n");
	print_kernel_mesh_points_triple(&global_p2k1_list);

	printf("====================\n");
	printf("points for 2D problem-2D same params kernel:\n");
	printf("===================\n");
	print_kernel_mesh_points_triple(&global_p2k2_same_param_list);

	printf("====================\n");
	printf("points for 2D problem-2D kernel:\n");
	printf("===================\n");
	print_kernel_mesh_points_triple(&global_p2k2_list);


	printf("====================\n");
	printf("points for 3D problem-2D kernel:\n");
	printf("===================\n");
	print_kernel_mesh_points_pentuple(&global_p3k3_list);

}

///////////////////////////////////////

int main(int argc, char **argv)
{
	test_kernel_mesh_points(argc, argv);
	return 0;
}
