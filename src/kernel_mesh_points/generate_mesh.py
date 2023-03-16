from gen_3d_points import *

MIN_N_1D_PROBLEM_LOG2=10
MAX_N_1D_PROBLEM_LOG2=20
##########################
MIN_N_2D_PROBLEM_LOG2=5
MAX_N_2D_PROBLEM_LOG2=14
##########################
MIN_BLOCK_SIZE_LOG2=5
MAX_BLOCK_SIZE_LOG2=10

#######################################
def generate_mesh_1d_prob_1d_kernel():
	point_list=[]
	##		n= 2^nb s.t. n >=1024
#	print("1D problem - 1D kernel");
	for nb in range(MIN_N_1D_PROBLEM_LOG2,MAX_N_1D_PROBLEM_LOG2+1):
		for i in range(MIN_BLOCK_SIZE_LOG2,MAX_BLOCK_SIZE_LOG2+1):
			n=1<<nb;
			b0=1<<i;
			point_list.append("{"+str(n)+","+str(b0)+","+str(1)+"}")

	return point_list;

#######################################
def generate_mesh_2d_prob_1d_kernel():
	point_list=[]
#	print("2D problem - 1D kernel");
	##	n= 2^nb s.t. n^2>=1024
	for nb in range(MIN_N_2D_PROBLEM_LOG2,MAX_N_2D_PROBLEM_LOG2+1):
		n=1<<nb;
		
		for i in range(MIN_BLOCK_SIZE_LOG2,MAX_BLOCK_SIZE_LOG2+1):			
			b0=1<<i;
			if b0 > n:
				break
			point_list.append("{"+str(n)+","+str(b0)+ ","+str(1)+"}")
		
		n=int(n*1.5);
		for i in range(MIN_BLOCK_SIZE_LOG2,MAX_BLOCK_SIZE_LOG2+1):			
			b0=1<<i;
			if b0 > n:
				break
			point_list.append("{"+str(n)+","+str(b0)+ ","+str(1)+"}")

	return point_list;

#######################################
def generate_mesh_2d_prob_2d_kernel():
	
	point_list=[]
#	print("2D problem - 2D kernel");
	##	n= 2^nb s.t. n^2>=1024
	for nb in range(MIN_N_2D_PROBLEM_LOG2,MAX_N_2D_PROBLEM_LOG2+1):
		n=1<<nb;
		for i in range(MIN_BLOCK_SIZE_LOG2,MAX_BLOCK_SIZE_LOG2+1):
			for t in range(i+1):
				j=i-t;
				b0=1<<t;
				b1=1<<j
				if b0 > n or b1 > n:
					continue
				point_list.append("{"+str(n)+","+str(b0)+","+str(b1)+"}")
	return point_list;


#######################################
def generate_mesh_2d_prob_2d_kernel_same_param():
	
	point_list=[]
#	print("2D problem - 2D kernel");
	##	n= 2^nb s.t. n^2>=1024
	for nb in range(MIN_N_2D_PROBLEM_LOG2,MAX_N_2D_PROBLEM_LOG2+1):
		n=1<<nb;
		for i in range(MIN_BLOCK_SIZE_LOG2,MAX_BLOCK_SIZE_LOG2+1):
			b=1<<i;
			b_sqrt=(1<<(i/2))
			if (b_sqrt)**2==b:
				point_list.append("{"+str(n)+","+str(b_sqrt)+","+str(b_sqrt)+"}")
		
		n=int(n*1.5)
		for i in range(MIN_BLOCK_SIZE_LOG2,MAX_BLOCK_SIZE_LOG2+1):
			b=1<<i;
			b_sqrt=(1<<(i/2))
			if (b_sqrt)**2==b:
				point_list.append("{"+str(n)+","+str(b_sqrt)+","+str(b_sqrt)+"}")
	return point_list;

#######################################
def print_point_list(pl):
	for p in pl[:-1]:
		print(str(p)+",");
	print(pl[-1])

#######################################
def generate_mesh():
	
	guards_begin='''
#ifndef KERNEL_MESH_POINTS_H_
#define KERNEL_MESH_POINTS_H_


#ifndef VERBOSE
#define VERBOSE 0
#endif

'''
	guards_end='''
#endif'''

	structs='''
typedef struct {
int N[3];
} triple;

typedef struct {
int N[5];

//N,pencil,mx, my,mz
} pentuple;
	'''
	
	line='''
///////////////////////////////////////
'''

	get_next_function='''
triple * get_next_kernel_mesh_point(triple * current_point_list)
{
	triple* p;
	p=current_point_list;
	p++;
	if (p->N[0] && p->N[1] && p->N[2])
	{
		return p;
	}
	else
	{
	#if VERBOSE
		printf("WARNING: END OF THE LIST!\\n");
	#endif
		return NULL;
	}
}


int get_n_kernel_mesh_points(triple* p)
{
	int n=1;
	while(1)
	{

		p=get_next_kernel_mesh_point(p);
		if (p==NULL)
			break;
		else
			n++;
	}
	return n;
}

pentuple * get_next_kernel_mesh_point_pentuple(pentuple * current_point_list)
{
	pentuple* p;
	p=current_point_list;
	p++;
	if (p->N[0] && p->N[1] && p->N[2] && p->N[3]&& p->N[4])
	{
		return p;
	}
	else
	{
	#if VERBOSE
		printf("WARNING: END OF THE LIST!\\n");
	#endif
		return NULL;
	}
}
'''

	p1k1_begin='''const triple global_p1k1_list[]={'''
	p1k1_end=''',{0,0,0}};'''

	p2k1_begin='''const triple global_p2k1_list[]={'''
	p2k1_end=''',{0,0,0}};'''


	p2k2_begin='''const triple global_p2k2_list[]={'''
	p2k2_end=''',{0,0,0}};'''


	p2k2_same_param_begin='''const triple global_p2k2_same_param_list[]={'''
	p2k2_same_param_end=''',{0,0,0}};'''

	p1k1=generate_mesh_1d_prob_1d_kernel();
	p2k1=generate_mesh_2d_prob_1d_kernel();
	p2k2=generate_mesh_2d_prob_2d_kernel();
	p2k2_same_param=generate_mesh_2d_prob_2d_kernel_same_param();
	
	print(line)	
	print(guards_begin)
	print(structs);
	
	print(line)	
	print(get_next_function);

	
	print(line)
	print(p1k1_begin)
	print_point_list(p1k1);
	print(p1k1_end);


	print(line)
	print(p2k1_begin)
	print_point_list(p2k1);
	print(p2k1_end);
	

	print(line)
	print(p2k2_same_param_begin)
	print_point_list(p2k2_same_param);
	print(p2k2_same_param_end);

	print(line)
	print(p2k2_begin)
	print_point_list(p2k2);
	print(p2k2_end);

	
	
	p3k3_begin='''const pentuple global_p3k3_list[]={'''
	p3k3_end=''',{0,0,0,0,0}};'''	
	print(line)
	print(p3k3_begin)
	print(get_3d_points())
	print(p3k3_end)

	print(line)
	print(guards_end)
#######################################

def main():
	generate_mesh();
#######################################
if __name__=="__main__":
	main();

