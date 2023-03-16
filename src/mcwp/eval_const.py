import sys

#######################################
def eval_constraints(kernel_name, n, b0, b1, verbose):
	const_path="kernel_"+kernel_name+"_constraints.txt"
	const_file=open(const_path,"r");
	consts=const_file.read().split("\n")[:-1];
	const_file.close()
	
	[n,b0,b1]=[int(n),int(b0),int(b1)]
	for x in consts:
		x=x.lower()
		val=int(eval(x))
		if val>=0:
			if verbose:
				print ( "["+x+"] ... PASS")
		else:
			if verbose:
				print ( "["+x+"] ... FAIL")	
			return (-1)
	return(0)

#######################################
def main():
	args=sys.argv;
	argc=len(args)

	if argc<5:
		print("args: [1]:kernel_name [2]:n [3]:b0 [4]:b1")
		exit(-1);
	
	verbose=0;
	[kernel_name,n,b0,b1]=args[1:5]
	if argc>5:
		verbose=int(args[5])

	print(eval_constraints(kernel_name, n, b0,b1, verbose))

#######################################
if __name__=="__main__":
	main()
