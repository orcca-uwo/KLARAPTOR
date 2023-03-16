import os
import re


#######################################
def get_compatible_sm_list():
    depricated_list={'compute_30', 'compute_32', 'compute_35', 'compute_37', 'compute_50'}
    nvcc_msg=os.popen("nvcc -h").read()
    sm_list=re.findall(r'compute_[1-9][0-9]',nvcc_msg)
    sm_list = [ x for x in sm_list if x not in depricated_list]
    sm_list=sorted(sm_list)
    non_dup_sm_list=[]

    sm_str=""
    for x in sm_list:
        x=re.sub(r'compute_','',x)
        if x not in non_dup_sm_list:
            non_dup_sm_list.append(x);
            sm_str+=x+" "

    print(sm_str)


#######################################
def main():
	get_compatible_sm_list()

#######################################
if __name__=="__main__":
	main();
