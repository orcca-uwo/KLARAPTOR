
MIN_3D_DIM_LOG2=2
MAX_3D_DIM_LOG2=10

MIN_3D_PEN_LOG2=2;
MAX_3D_PEN_LOG2=10;
######################################
def gen_3d_points():
	points=dict();
	for mx2 in range(MIN_3D_DIM_LOG2,MAX_3D_DIM_LOG2):
		mx=(2**mx2)
		for my2 in range(MIN_3D_DIM_LOG2, MAX_3D_DIM_LOG2):
			my=(2**my2)
			for mz2 in range(MIN_3D_DIM_LOG2, MAX_3D_DIM_LOG2):
				mz=(2**mz2)
				for pencil2 in range(MIN_3D_PEN_LOG2, MAX_3D_PEN_LOG2):
					pencil=2**pencil2
					if ((mx % pencil==0) and (my %pencil ==0)and (mz %pencil==0)):
						N=mx*my*mz
						block_size=mx*pencil
						if block_size>1024:
							continue;
#						print("[pencil, mx, my, mz, N]",[pencil, mx, my, mz, N])
						if N not in points.keys():
							points[N]=[]
						point_str=str(pencil)+","+str(mx)+","+str(my)+","+str(mz)
						if point_str not in points[N]:
							points[N].append(point_str);
	return points;
#######################################
def process_points(points):	
	output_str=""
	for k in sorted(points.keys()):
#		print("N",k)
		for p in points[k]:
#			print(str(k)+p);
			output_str+="{"+str(k)+","+p+"},\n";

	output_str+="__END";
	output_str=output_str.replace(",\n__END","");
#	print(output_str)
#	print("====================")
	return output_str;	
#######################################
def get_3d_points():
	points=gen_3d_points()
	points_str=process_points(points)
	return points_str;
#######################################
def main():
	print(get_3d_points());
#######################################
if __name__=="main":
	main()

