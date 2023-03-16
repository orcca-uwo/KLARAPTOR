import sys


extra_points=[64, 128, 256, 512]
#######################################
def read_content_from_file(path):
	f=open(path,"r");
	content=f.read();
	f.close();
	return content;

#######################################
def write_list_to_file(content, path):
	f=open(path,"w");
	for x in content:	
		f.write(x+"\n");
	
	f.close();
#######################################
def get_n(line):
	return int(line.split(" ")[0])

#######################################
def add_extra_points(example):
	log_name="tracer_params.log"
	path="./"+example+"/"+log_name
	content=read_content_from_file(path).split("\n")[:-1];
	n=len(content);
	max_n=get_n(content[-1])
	max_n_points=[]
	###########################
	print(n)
	for i in range(n-2,-1,-1):
		current_n=get_n(content[i]);
		if current_n!=max_n:
			break;
		else:
			max_n_points.append(content[i]);

	###########################
	max_n_points.reverse();
	print(max_n_points);
	###########################
	for p in extra_points:
		for x in max_n_points:
			x=x.split(" ")
			n=int(x[0]);
#			print(x,n)
			if n in extra_points:
				continue;
			x[0]=str(p);
			x=" ".join(x)
			content.append(x)
	###########################
	write_list_to_file(content, path);
	print("[result written to "+path+"]");


#######################################
def main():

	argc=len(sys.argv);
	if argc!=2:
		print("args: [1]:example_name");
		exit(-1);
		
	example=sys.argv[1];
	add_extra_points(example);

#######################################
if __name__=="__main__":
	main()
