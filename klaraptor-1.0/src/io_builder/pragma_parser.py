
import os
import sys
import re

#######################################
#def read_content_from_file(path):
#	f=open(path, "r");
#	content=f.read();
#	f.close();
#	return content;
#
########################################
#def write_content_to_file(content, path):
#	f=open(path, "w");
#	f.write(content);	
#	f.close();	
#######################################
def parse_pragmas(content):
	#content=read_content_from_file(path)
#	print(content)
	prefix="kernel_info"
	param_list=["size_param_idx", "dim"]
	unused_attr=" __attribute__((used)) "

	for p in param_list:
		prefix_pattern=re.compile("^[ ]*\#pragma[ ]*"+prefix+"_"+p+"_");
		pattern=re.compile("^[ ]*\#pragma[ ]*"+prefix+"_"+p+"_.*");
		res=re.findall(pattern, content);
		for x in res:
			s=re.sub(prefix_pattern, "", x)
			[kernel_name, val]=s.split(" ")	
			val=val.replace(";","");
#			print(kernel_name, val)
			s="const int kernel_info_";
			s+=p+"_"+kernel_name+unused_attr
			s+=" = "+str(val)
			content=re.sub(x, s, content);	
	#write_content_to_file(content, dest);

#######################################
#def main():
#	argc=len(sys.argv);
#	if argc!=2:
#		print("args: [0]: src_path.cu");
#		exit(-1);
#	
#	path=sys.argv[1]
#	dest=path.replace(".cu","")+"_pragma_used.cu"
#	parse_pragmas(path, dest);
	
#######################################
#if __name__=="__main__":
#	main()
