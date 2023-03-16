import os
import sys
import re
import math

#######################################
def get_default_mem_ld(device):
	default_mem_ld=400;
	device_dir="../src/device_profiles"
	device_path=device_dir+"/"+device+".specs"
	
	f=open(device_path,"r");
	content=f.read()
	f.close();

	mem_ld=re.findall(r'\[Mem_LD: [0-9]*\]',content)[0]
	mem_ld=re.sub(r'\[Mem_LD: ','',mem_ld);
	mem_ld=re.sub(r'[ ]*\]','',mem_ld);	
	
	default_mem_ld=mem_ld;
	return default_mem_ld

#######################################
def get_mem_ld(n,b, device, cached=0):
	return get_default_mem_ld(device)
	cached=0
	n=int(math.log(n,2))
	device_dir="../device_profiles"
	device_path=device_dir+"/"+device+".mem"
	f=open(device_path,"r");
	content=f.read()
	f.close();

	p=re.compile("\["+str(n)+"[ ]*,[ ]*"+str(b)+"[^\]]*\]")
	m=re.findall(p, content);
	if len(m)==0:
		return get_default_mem_ld(device)
	else:
		v=m[0].replace("[","").replace("]","").replace(" ","").split(",")[2:]
		[cached_val,non_cached_val]=[int(v[0]),int(v[1])]
#	print(cached, v, cached_val, non_cached_val)
	cached=int(cached)

	if cached==1:
		return cached_val
	else:
		return non_cached_val;

#######################################
def main():
	argc=len(sys.argv)
	if argc<4:
		print("args: [1]:input_vector_size [2]:block_size [3]:device_name [4]:cached")
	else:
		n=int(sys.argv[1])
		b=int(sys.argv[2])
		device=(sys.argv[3])
		cached=1;
		if argc>4:
			cached=int(sys.argv[4])
#			print(cached)
		mem_ld=get_mem_ld(n,b, device, cached)
		print(mem_ld)

#######################################
if __name__=="__main__":
	main()
