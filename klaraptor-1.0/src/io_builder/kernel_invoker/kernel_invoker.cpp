/*!
 \file kernel_invoker.cpp
 \author Davood Mohajerani <dmohajer@uwo.ca>
 \brief
 */

#include "kernel_invoker.h"

///////////////////////////////////////////////////////////

//definition
void kernel_invoker_init(void) {
	printf("%s\n", __func__);
	ptx_lookup_manager_init();
}

///////////////////////////////////////////////////////////

//definition
void kernel_invoker_clear(void) {
//  printf ("in [%s]\n", __func__);
	ptx_lookup_manager_clear();

	kernel_invoker_data_t * ptr = &global_kernel_invoker_data;
	if (ptr->cu_init_done == 1) {
//      checkCudaErrors(cuCtxGetCurrent (&ptr->context));
//      if (ptr->context != NULL)
//	{
//	  checkCudaErrors(cuCtxDestroy (ptr->context));
//	}
		ptr->cu_init_done = 0;
	}
}

///////////////////////////////////////////////////////////

void set_kernel_launch_params(int* launch_params, dim3 gridDim, dim3 blockDim)
//, int n_dynamic_shared_mem_bytes)
		{
	launch_params[0] = gridDim.x;
	launch_params[1] = gridDim.y;
	launch_params[2] = gridDim.z;

	launch_params[3] = blockDim.x;
	launch_params[4] = blockDim.y;
	launch_params[5] = blockDim.z;

//	launch_params[6]=n_dynamic_shared_mem_bytes;
}

///////////////////////////////////////////////////////////

//this function will be called at the beginning of kernel_invoker.
int kernel_invoker_check_cu_init() {
//  printf ("in ............................................................\n");
	kernel_invoker_data_t * ptr = &global_kernel_invoker_data;
//  printf ("[%s][ptr->cu_init_done=%d]\n", __func__, ptr->cu_init_done);
	//at this point, device is not set yet.
	if (ptr->cu_init_done == 0) {
		//  // CUDA initialization
		//  checkCudaErrors(cuInit (0));
		//	checkCudaErrors(cuDeviceGetCount(&devCount));

		//check if a ctx is already allocated.
		checkCudaErrors(cuCtxGetCurrent(&ptr->context));

		//if there is not context, the returned value will be null.
		if (ptr->context == NULL) {
			printf("context is NULL! ... creating context\n");
			//// CUDA initialization
			checkCudaErrors(cuInit(0));

			////get the device.
			checkCudaErrors(cuDeviceGet(&ptr->device, 0));

			////Create driver context
			checkCudaErrors(cuCtxCreate(&ptr->context, 0, ptr->device));
		} else {
			////do nothing, just get the device.
			printf("[a context is already initialized!] ... skipping \n");
			checkCudaErrors(cuDeviceGet(&ptr->device, 0));
		}

		//// set the flag.
		ptr->cu_init_done = 1;
//      printf ("[cu_init_done]->[%d]\n", ptr->cu_init_done);
	}
	return EXIT_SUCCESS;
}

///////////////////////////////////////////////////////////

int kernel_invoker(char * kernel_name, int* launch_params,
		void **KernelParams) {
	dim3 block_dim, grid_dim;
	int n_dynamic_shared_mem_bytes;
	int stream_idx;

	grid_dim.x = launch_params[0];
	grid_dim.y = launch_params[1];
	grid_dim.z = launch_params[2];
	block_dim.x = launch_params[3];
	block_dim.y = launch_params[4];
	block_dim.z = launch_params[5];
	n_dynamic_shared_mem_bytes = launch_params[6];
	stream_idx = launch_params[7];
#if VERBOSE
	{
//		std::cout << short_ln(60);
		std::cout << "[kernel_name : " << kernel_name << "]\n";
	}
#endif
//  CUdevice device;
	CUmodule cudaModule;
//  CUcontext context;
	CUfunction function;
//	CUlinkState linker;
//	int devCount;

//  // CUDA initialization
//  checkCudaErrors(cuInit (0));
//	checkCudaErrors(cuDeviceGetCount(&devCount));
//  checkCudaErrors(cuDeviceGet (&device, 0));

//	char name[128];
//	checkCudaErrors(cuDeviceGetName(name, 128, device));
//	if (verbose)
//		std::cout << "Using CUDA Device [0]: " << name << "\n";

//	int devMajor, devMinor;
//	checkCudaErrors(cuDeviceComputeCapability(&devMajor, &devMinor, device));
//	if (verbose)
//		std::cout << "Device Compute Capability: " << devMajor << "."
//				<< devMinor << "\n";
//
//	if (devMajor < 2)
//	{
//		std::cerr << "ERROR: Device 0 is not SM 2.0 or greater\n";
//		return 1;
//	}

//#if VERBOSE
//	std::cout << short_ln(60);
//#endif

//  printf ("here before !\n");

	checkErrors(kernel_invoker_check_cu_init());

//  printf ("here after!\n");
	int kernel_idx;
//  exit(EXIT_FAILURE);
	char kernel_name_ptx[MAX_KERNEL_NAME_LEN];
//  char kernel_name_ptxsass[MAX_KERNEL_NAME_LEN];
	char entry_name[MAX_KERNEL_NAME_LEN];

	sprintf(kernel_name_ptx, "%s.ptx", kernel_name);
//  sprintf (kernel_name_ptxsass, "%s.ptxsass", kernel_name);

//  printf ("getting kernel idx...\n");
	ptx_lookup_manager_get_kernel_idx(kernel_name_ptx, &kernel_idx);
//  ptx_lookup_manager_get_ptx_str (kernel_name_ptx, &ptxstr);
	ptx_lookup_manager_get_ptx_entry_name(kernel_idx, entry_name);

	unsigned char *ptxstr;
	ptx_lookup_manager_get_ptx_str(kernel_idx, &ptxstr);

//  printf ("entry-name=[%s]\n", entry_name);
//	std::string decorated_kernel_name = decorate_kernel_name(kernel_name);
//	std::string ptxstr = get_ptx_str(std::string(kernel_name_ptx));
//	std::string ptxsass_str = get_ptxsass_str(decorated_kernel_name+"sass");
//#if VERBOSE
//	std::cout << short_ln(60);
//#endif

	kernel_invoker_data_t * ptr = &global_kernel_invoker_data;

//////Create driver context
//  checkCudaErrors(cuCtxCreate_v2 (&context, 0, device));
//  checkCudaErrors(cuCtxCreate_v2 (&context, 0, ptr->device));

//	std::cout<<"decorated kernel name = "<<decorated_kernel_name+"sass"<<"\n";
//	std::cout<<ptxsass_str.length()<<"\n";

//  printf ("kernel_name_ptx=[%s]\n", kernel_name_ptx);
	// Create module for object
//	checkCudaErrors(cuModuleLoadDataEx(&cudaModule, str.c_str(), 0, 0, 0));

//  printf ("ptxstr=%s\n", ptxstr);
	checkCudaErrors(cuModuleLoadData(&cudaModule, (ptxstr)));

//  checkCudaErrors(cuModuleLoad (&cudaModule, kernel_name_ptxsass));
//  checkCudaErrors(cuModuleLoad (&cudaModule, kernel_name_ptx));
//	checkCudaErrors(cuModuleLoad(&cudaModule, kernel_ptxobj.c_str()));

	// Get kernel function
	checkCudaErrors(cuModuleGetFunction(&function, cudaModule, entry_name));
//  printf ("ptx-entry-name=[%s]\n", entry_name);

	// Device data
	//	CUdeviceptr devBufferA;
	//	CUdeviceptr devBufferB;
	//	CUdeviceptr devBufferC;

//	unsigned int blockSizeX = block_dim.x;
//	unsigned int blockSizeY = block_dim.y;
//	unsigned int blockSizeZ = block_dim.z;
//	unsigned int gridSizeX = grid_dim.x;
//	unsigned int gridSizeY = grid_dim.y;
//	unsigned int gridSizeZ = grid_dim.z;

//#if VERBOSE
//    {
//      std::cout<<"[@kernel_invoker][launch_params]";
//      std::cout<<"[gx="<<grid_dim.x<<"]";
//      std::cout<<"[gy="<<grid_dim.y<<"]";
//      std::cout<<"[gz="<<grid_dim.z<<"]";
//      std::cout<<"[bx="<<block_dim.x<<"]";
//      std::cout<<"[by="<<block_dim.y<<"]";
//      std::cout<<"[bz="<<block_dim.z<<"]";
//      std::cout<<"\n";

//    }
//#endif

	cuda_timer t_kernel_launch;
	cuda_timer_init_record_start(t_kernel_launch);

	CUresult cu_error = cuLaunchKernel(function, grid_dim.x, grid_dim.y,
			grid_dim.z, block_dim.x, block_dim.y, block_dim.z, 0, NULL,
			KernelParams, NULL);

	cuda_timer_record_stop(t_kernel_launch);
	cuda_timer_record_get_elapsed_time(t_kernel_launch);

	checkCudaErrors(cu_error);
	checkCudaErrors(cuModuleUnload(cudaModule));
//  checkCudaErrors(cuCtxDestroy (context));
//  free (ptxstr);

//  char msg[MAX_KERNEL_NAME_LEN];
//  sprintf (msg, , kernel_name);
	const char *msg_format = "========================================\n"
			"[@invoker][%s]"
			"[(gx, gy, gz)=(%d, %d, %d)]"
			"[(bx, by, bz)=(%d, %d, %d)]\n"
			"[[@driver][%s]=[%.3f (ms)]\n"
			"========================================\n";
	char msg[4 * MAX_KERNEL_NAME_LEN];
	sprintf(msg, msg_format, kernel_name, grid_dim.x, grid_dim.y, grid_dim.z,
			block_dim.x, block_dim.y, block_dim.z, kernel_name,
			t_kernel_launch.elapsed_time);
	fwrite(msg, 1, strlen(msg), stdout);
	cuda_timer_destroy(t_kernel_launch);
	return EXIT_SUCCESS;
}

///////////////////////////////////////////////////////////
