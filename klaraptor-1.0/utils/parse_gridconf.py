import os
import sys
from math import *


#######################################
def print_args():
	print("args: [1]:gridconf_path [2]:n [3]:bx [4]:by [5]:bz");


#######################################
def exit_failure():
	exit(-1);

#######################################
def exit_success():
	exit(0)


#######################################
def read_content_from_file(path):
	f=open(path,"r");
	content=f.read();
	f.close();
	return content;


#######################################
## ToDo: Turn the parsing to a full expression evaluator.

def parse_g_str(g_str):
	if g_str=="1" or g_str=="":
		return "1";
	g_str=g_str.replace("\n","")
	g_str=g_str.replace("[","");
	g_str=g_str.replace("]","");
	g_str=g_str.split("=")[1]
	
	return g_str
#######################################
def compute_griddim(conf_path, n, bx, by, bz):
	print("computing griddim for [%s,%s,%s] from [%s]"%(bx, by, bz, conf_path))
	content=read_content_from_file(conf_path);
	content=content.split	(";")
	
	gx_str="1";
	gy_str="1";
	gz_str="1";

	for c in content:
		if "gx" in c:
			gx_str=c;
			break;

	for c in content:
		if "gy" in c:
			gy_str=c;	
			break;

	for c in content:
		if "gz" in c:
			gz_str=c;
			break;
	
	g_str_list=[gx_str, gy_str, gz_str];
	g_eval_list=[1,1,1]
	for i in range(len(g_str_list)):
		current=g_str_list[i];
		current=parse_g_str(current);
		current=current.replace("n","(float(%s))"%(n));
		current=current.replace("bx","(float(%s))"%(bx));
		current=current.replace("by","(float(%s))"%(by));
		current=current.replace("bz","(float(%s))"%(bz));
		g_str_list[i]=current;
		g_eval_list[i]=int(eval(current));	
	
#	print("[gx=%s ... gy=%s ... gz=%s]\n"%(gx_str, gy_str, gz_str));
	print("[eval][gx, gy, gz]");
	print("%d, %d, %d\n"%(g_eval_list[0], g_eval_list[1], g_eval_list[2]));

#######################################
def main():
#	print("MAIN")
	argc=len(sys.argv);
	argv=sys.argv
	if argc<6:
		print_args();
		exit_failure();
	
	conf_path=argv[1];
	n=str(int(argv[2]))
	bx=str(int(argv[3]));
	by=str(int(argv[4]))
	bz=str(int(argv[5]));
	compute_griddim(conf_path, n, bx, by, bz);


#######################################
if __name__=="__main__":
	main();
