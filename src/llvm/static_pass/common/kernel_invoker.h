#ifndef KERNEL_INVOKER_H_
#define KERNEL_INVOKER_H_

//#include <iostream>
//#include <fstream>
#include <cassert>
#include "cuda.h"
#include "ptx_lookup_table_manager.h"
#include "cuda_timer.h"

//static int module_list_created = 0;

#ifndef VERBOSE
#define VERBOSE 3
#endif

#define MAX_KERNEL_NAME_LEN 1024
///////////////////////////////////////////////////////////

//void checkCudaErrors(CUresult err)
//{
//	assert(err == CUDA_SUCCESS);
//}

#define checkCudaErrors(err)do{\
		assert(err == CUDA_SUCCESS);\
}while(0);

///////////////////////////////////////////////////////////
//std::string short_ln(int s)
//{
//	std::string line = "";
//
//	for (int i = 0; i < s; i++)
//		line += "-";
//	line += "\n";
//	return line;
//}
///////////////////////////////////////////////////////////

//std::string decorate_kernel_name(std::string kernel_name)
//{
//	std::string result = kernel_name + ".ptx";
//	return result;
//}

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
///////////////////////////////////////////////////////////

int kernel_invoker(char * kernel_name, int* launch_params, void **KernelParams)
{
	dim3 block_dim, grid_dim;
	int n_dynamic_shared_mem_bytes;

	grid_dim.x = launch_params[0];
	grid_dim.y = launch_params[1];
	grid_dim.z = launch_params[2];
	block_dim.x = launch_params[3];
	block_dim.y = launch_params[4];
	block_dim.z = launch_params[5];
	n_dynamic_shared_mem_bytes = 0; //launch_params[6];

#if VERBOSE
	{
//		std::cout << short_ln(60);
		std::cout << "[kernel_name : " << kernel_name << "]\n";
	}
#endif
	CUdevice device;
	CUmodule cudaModule;
	CUcontext context;
	CUfunction function;
//	CUlinkState linker;
//	int devCount;

	// CUDA initialization
	checkCudaErrors(cuInit(0));
//	checkCudaErrors(cuDeviceGetCount(&devCount));
	checkCudaErrors(cuDeviceGet(&device, 0));

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

	char kernel_name_ptx[MAX_KERNEL_NAME_LEN];
	char kernel_name_ptxsass[MAX_KERNEL_NAME_LEN];

	sprintf(kernel_name_ptx, "%s.ptx", kernel_name);
	sprintf(kernel_name_ptxsass, "%s.ptxsass", kernel_name);

//	std::string decorated_kernel_name = decorate_kernel_name(kernel_name);
//	std::string ptxstr = get_ptx_str(std::string(kernel_name_ptx));

	char *ptxstr;
	get_ptx_str(kernel_name_ptx, &ptxstr);

//	std::string ptxsass_str = get_ptxsass_str(decorated_kernel_name+"sass");
//#if VERBOSE
//	std::cout << short_ln(60);
//#endif
////Create driver context 
	 	checkCudaErrors(cuCtxCreate(&context, 0, device));

//	std::cout<<"decorated kernel name = "<<decorated_kernel_name+"sass"<<"\n";
//	std::cout<<ptxsass_str.length()<<"\n";

	// Create module for object
//	checkCudaErrors(cuModuleLoadDataEx(&cudaModule, str.c_str(), 0, 0, 0));
//	checkCudaErrors(cuModuleLoadData(&cudaModule, ptxstr.c_str()));
//	checkCudaErrors
//	(cuModuleLoad(&cudaModule, kernel_name_ptxsass));
	checkCudaErrors
		(cuModuleLoad(&cudaModule, kernel_name_ptx));
//	checkCudaErrors(cuModuleLoad(&cudaModule, kernel_ptxobj.c_str()));

	char entry_name[MAX_KERNEL_NAME_LEN];
	get_ptx_entry_name(ptxstr, entry_name);
	// Get kernel function
	checkCudaErrors
	(cuModuleGetFunction(&function, cudaModule, entry_name));

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

#if VERBOSE
	{
		std::cout<<"[kernel_invoker][launch_params]";
		std::cout<<"[gx="<<grid_dim.x<<"]";
		std::cout<<"[gy="<<grid_dim.y<<"]";
		std::cout<<"[gz="<<grid_dim.z<<"]";
		std::cout<<"[bx="<<block_dim.x<<"]";
		std::cout<<"[by="<<block_dim.y<<"]";
		std::cout<<"[bz="<<block_dim.z<<"]";
		std::cout<<"\n";
	}
#endif
	
	cuda_timer t_kernel_launch;
	cuda_timer_init_record_start(t_kernel_launch);

	checkCudaErrors
	(cuLaunchKernel(function, grid_dim.x, grid_dim.y, grid_dim.z,
			block_dim.x, block_dim.y, block_dim.z, 0, NULL,
			KernelParams, NULL));

	cuda_timer_record_stop(t_kernel_launch);
		cuda_timer_record_get_elapsed_time(t_kernel_launch,
				"elapsed-time-through-driver");
	cuda_timer_destroy(t_kernel_launch);
	(cuModuleUnload(cudaModule));
	(cuCtxDestroy(context));
	free(ptxstr);

	return EXIT_SUCCESS;
}

///////////////////////////////////////////////////////////

#endif 
