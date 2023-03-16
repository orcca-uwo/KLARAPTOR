import os

def get_active_device_id():
	env_vars=(os.environ);
	dev_id=0;
	if "CUDA_VISIBLE_DEVICES" not in env_vars.keys():
#		print("Device visibility is not specified... going with id=0")
		dev_id=0;
	else:
		dev_list=env_vars['CUDA_VISIBLE_DEVICES'].replace(" ","");
		dev_id=0;
		if dev_list:
			dev_list=dev_list.split(",");
			dev_id=(dev_list[0]);
	
	return dev_id

#######################################
def get_gpu_name():
	id=get_active_device_id();
	cmd='nvidia-smi -q -i '+str(id)+' | grep "Product Name"'
	dev_name=os.popen(cmd,"r").read().split(":")[1].replace("\n","");
	dev_name=dev_name.split(" ")[2:];
	dev_name="".join(dev_name)
	print(dev_name.lower())


#######################################
def main():
	get_gpu_name();

#######################################
if __name__=="__main__":
	main()

