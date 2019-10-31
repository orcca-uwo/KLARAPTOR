
import sys

n_sym="n"
bx_sym="bx"
by_sym="by"
bz_sym="bz"
#######################################
def eval_constraints(kernel_name, n, bx, by, bz, verbose):
	const_path="kernel_"+kernel_name+"_constraints.txt"
	const_file=open(const_path,"r");
	consts=const_file.read().split("\n")[:-1];
	const_file.close()
	
	[n,bx,by,bz]=[int(n),int(bx),int(by),int(bz)]
	for x in consts:
		x=x.lower()
		x=x.replace(bx_sym, "bx")
		x=x.replace(by_sym, "by")
		x=x.replace(bz_sym, "bz")
#		print(x)
#		continue;
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
	
	verbose=0;
	b2=1;
	print(eval_constraints(kernel_name, n, b0, b1, b2, verbose))

#######################################
if __name__=="__main__":
	main()
