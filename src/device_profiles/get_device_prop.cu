/* This sample queries the properties of the CUDA devices
 * present in the system via CUDA Runtime API.
 * modified from CUDA 7 sdk*/

#include <stdio.h>
#include <ctype.h>
#include <cuda_runtime.h> 

typedef unsigned int u32;
typedef unsigned long long int u64;

#define gpuErrchk(ans) { gpuAssert((ans), __FILE__, __LINE__); }
inline void
gpuAssert (cudaError_t code, const char *file, int line, bool abort = true)
{
  if (code != cudaSuccess)
    {
      fprintf (stderr, "GPUassert: %s %s %d\n", cudaGetErrorString (code), file,
	       line);
      if (abort)
	exit (code);

    }
}

#if CUDART_VERSION < 5000

// CUDA-C includes
#include <cuda.h>

// This function wraps the CUDA Driver API into a template function
template<class T>
  inline void
  getCudaAttribute (T *attribute, CUdevice_attribute device_attribute,
		    int device)
  {
    CUresult error = cuDeviceGetAttribute (attribute, device_attribute, device);

    if (CUDA_SUCCESS != error)
      {
	fprintf (
	    stderr,
	    "cuSafeCallNoSync() Driver API error = %04d from file <%s>, line %i.\n",
	    error, __FILE__, __LINE__);

	// cudaDeviceReset causes the driver to clean up all state. While
	// not mandatory in normal operation, it is good practice.  It is also
	// needed to ensure correct operation when the application is being
	// profiled. Calling cudaDeviceReset causes all profile data to be
	// flushed before the application exits
	cudaDeviceReset ();
	exit (EXIT_FAILURE);
      }
  }

#endif /* CUDART_VERSION < 5000 */

///////////////////////////////////////

/* The folloing function is copied and modified from
 * CUDA-7.0 SDK samples/common/inc/helper_cuda.h
 * //#include <helper_cuda.h>
 */

// Beginning of GPU Architecture definitions
inline int
_ConvertSMVer2Cores (int major, int minor)
{
  // Defines for GPU Architecture types (using the SM version to determine the # of cores per SM
  typedef struct
  {
    int SM; // 0xMm (hexidecimal notation), M = SM Major version, and m = SM minor version
    int Cores;
  } sSMtoCores;

  sSMtoCores nGpuArchCoresPerSM[] = {
        {0x30, 192},
        {0x32, 192},
        {0x35, 192},
        {0x37, 192},
        {0x50, 128},
        {0x52, 128},
        {0x53, 128},
        {0x60,  64},
        {0x61, 128},
        {0x62, 128},
        {0x70,  64},
        {0x72,  64},
        {0x75,  64},
        {0x80,  64},
        {0x86, 128},
        {0x87, 128},
        {-1, -1}};

  int index = 0;

  while (nGpuArchCoresPerSM[index].SM != -1)
    {
      if (nGpuArchCoresPerSM[index].SM == ((major << 4) + minor))
	{
	  return nGpuArchCoresPerSM[index].Cores;
	}

      index++;
    }

  // If we don't find the values, we default use the previous one to run properly
//    printf("MapSMtoCores for SM %d.%d is undefined.  Default to use %d Cores/SM\n", major, minor, nGpuArchCoresPerSM[index-1].Cores);
  return nGpuArchCoresPerSM[index - 1].Cores;
}

///////////////////////////////////////

inline int
get_nblocks_per_SM (int major, int minor)
{
  // Defines for GPU Architecture types (using the SM version to determine the # of cores per SM
  typedef struct
  {
    int SM; // 0xMm (hexidecimal notation), M = SM Major version, and m = SM minor version
    int Cores;
  } sSMtoCores;

  sSMtoCores nGpuArchCoresPerSM[] =
    {
	  { 0x30, 16 }, // Kepler Generation (SM 3.0) GK10x class
	  { 0x32, 16 }, // Kepler Generation (SM 3.2) GK10x class
	  { 0x35, 16 }, // Kepler Generation (SM 3.5) GK11x class
	  { 0x37, 16 }, // Kepler Generation (SM 3.7) GK21x class
	  { 0x50, 32 }, // Maxwell Generation (SM 5.0) GM10x class
	  { 0x52, 32 }, // Maxwell Generation (SM 5.2) GM20x class
      { 0x53, 32 },
	  { 0x60, 32 }, // Pascal Generation (SM 6.0)  GP10X class
	  { 0x61, 32 }, // Pascal Generation (SM 6.1) GP10X class
	  { 0x62, 32 }, // Pascal Generation (SM 6.2) GP10X class
	  { 0x70, 32 }, // Volta Generation (SM 7.0) GV10x class
      { 0x72, 32 },
      { 0x75, 32 },
      { 0x80, 32 },
      { 0x86, 16 },
      { 0x87, 16 },
	  { -1, -1 } };
  int index = 0;

  while (nGpuArchCoresPerSM[index].SM != -1)
    {
      if (nGpuArchCoresPerSM[index].SM == ((major << 4) + minor))
	{
	  return nGpuArchCoresPerSM[index].Cores;
	}

      index++;
    }

  // If we don't find the values, we default use the previous one to run properly
//    printf("MapSMtoCores for SM %d.%d is undefined.  Default to use %d Cores/SM\n", major, minor, nGpuArchCoresPerSM[index-1].Cores);
  return nGpuArchCoresPerSM[index - 1].Cores;
}

///////////////////////////////////////

unsigned long long int
compute_peak_theoretical_bw_kflops (int memory_bus_width,
				    unsigned long long int memory_clock_khz)
{

//	unsigned long long int peak_bw_kflops = 2 * (memory_bus_width / 8) * memory_clock_khz;
  unsigned long long int peak_bw_kflops = (memory_bus_width >> 2)
      * memory_clock_khz;
  return peak_bw_kflops;
}

///////////////////////////////////////

char *
to_lower_str (const char * input_src_str)
{

  int n = strlen (input_src_str);

  char * src_str = (char*) malloc (n * sizeof(char));
  char * dest_str = (char*) malloc (n * sizeof(char));
  memcpy (src_str, input_src_str, n);
  int len = 0;
  int n_tokens = 0;

  for (int i = 0; i < n; i++)
    {
      if ('a' <= src_str[i] && src_str[i] <= 'z')
	continue;
      if ('A' <= src_str[i] && src_str[i] <= 'Z')
	continue;
      if ('0' <= src_str[i] && src_str[i] <= '9')
	continue;
      src_str[i] = '_';
    }

  for (int i = 0; i < n; i++)
    {
      if (src_str[i] == '_')
	{
	  if (n_tokens != 1)
	    n_tokens++;
	  continue;
	}

      if (n_tokens == 1)
	len += sprintf (dest_str + len, "%c", tolower (src_str[i]));
    }

  printf ("\ndest_str=%s\n", dest_str);
  return dest_str;

}

///////////////////////////////////////

int
host_get_device_specs_full (int argc, char ** argv)
{
//	printf("CUDA Device Query (Runtime API) version (CUDART static linking)\n");

  int n_devices = 0;
  cudaError_t error_id = cudaGetDeviceCount (&n_devices);

  if (error_id != cudaSuccess)
    {
      printf ("cudaGetDeviceCount returned %d\n-> %s\n", (int) error_id,
	      cudaGetErrorString (error_id));
//		printf("Result = FAIL\n");
      exit (EXIT_FAILURE);
    }

// This function call returns 0 if there are no CUDA capable devices.
  if (n_devices == 0)
    {
      printf ("-There are no available device(s) that support CUDA\n");
      exit (EXIT_FAILURE);
    }
  else
    {
      printf ("[Found %d CUDA Capable device(s) ... ]\n", n_devices);
    }

  int dev_id, driver_version = 0, runtime_version = 0;

  for (dev_id = 0; dev_id < n_devices; ++dev_id)
    {
      char file_name[256];
      char device_name[256];

      cudaSetDevice (dev_id);
      cudaDeviceProp deviceProp;

      cudaGetDeviceProperties (&deviceProp, dev_id);

      sprintf (device_name, "%s", deviceProp.name);
      printf ("checking device : %s\n", device_name);
      sprintf (file_name, "%s.full.specs", to_lower_str (device_name));
      printf ("file_name=%s\n", file_name);
      FILE* file = fopen (file_name, "w");

      fprintf (file, "[device_id: %d]\n", dev_id);
      fprintf (file, "[device_name: %s]\n", deviceProp.name);

      cudaDriverGetVersion (&driver_version);
      cudaRuntimeGetVersion (&runtime_version);
      fprintf (file, "[driver_version: %d.%d]\n", driver_version / 1000,
	       (driver_version % 100) / 10);
      fprintf (file, "[runtim_version: %d.%d]\n", runtime_version / 1000,
	       (runtime_version % 100) / 10);
      fprintf (file, "[compute_capability: %d.%d]\n", deviceProp.major,
	       deviceProp.minor);

      fprintf (file, "[global_memory_bytes: %llu]\n",
	       (unsigned long long) deviceProp.totalGlobalMem);
      fprintf (file, "[n_sm: %2d]\n", deviceProp.multiProcessorCount);

      fprintf (file, "[n_cores_per_sm: %3d]\n",
	       _ConvertSMVer2Cores (deviceProp.major, deviceProp.minor));

      fprintf (file, "[n_blocks_per_sm: %d]\n",
	       get_nblocks_per_SM (deviceProp.major, deviceProp.minor));

      fprintf (
	  file,
	  "[n_cores: %d]\n",
	  _ConvertSMVer2Cores (deviceProp.major, deviceProp.minor)
	      * deviceProp.multiProcessorCount);
//		fprintf(file,
//				"(%2d) Multiprocessors, (%3d) CUDA Cores/MP:     %d CUDA Cores\n",
//				deviceProp.multiProcessorCount,
//				_ConvertSMVer2Cores(deviceProp.major, deviceProp.minor),
//				_ConvertSMVer2Cores(deviceProp.major, deviceProp.minor)
//						* deviceProp.multiProcessorCount);
      fprintf (file, "[max_clock_rate_khz: %d]\n", deviceProp.clockRate);
      fprintf (file, "[max_clock_rate_mhz: %.0f]\n",
	       deviceProp.clockRate * 1e-3f);
      fprintf (file, "[max_clock_rate_ghz: %.2f]\n",
	       deviceProp.clockRate * 1e-6f);

      int memory_clock = 0;
      int memory_bus_width = 0;
      int l2_cache_size = 0;

#if CUDART_VERSION >= 5000
// This is supported in CUDA 5.0 (runtime API device properties)
//		printf("  Memory Clock rate:                             %.0f Mhz\n", deviceProp.memoryClockRate * 1e-3f);
//		printf("  Memory Bus Width:                              %d-bit\n", deviceProp.memoryBusWidth);

      memory_clock=deviceProp.memoryClockRate;
      memory_bus_width=deviceProp.memoryBusWidth;
      if (deviceProp.l2CacheSize)
	{
//			printf("  L2 Cache Size:                                 %d bytes\n", deviceProp.l2CacheSize);
	  l2_cache_size=deviceProp.l2CacheSize;
	}

#else
// This only available in CUDA 4.0-4.2 (but these were only exposed in the CUDA Driver API)
//		int memoryClock;
      getCudaAttribute<int> (&memory_clock,
			     CU_DEVICE_ATTRIBUTE_MEMORY_CLOCK_RATE, dev_id);

//		int memBusWidth;
      getCudaAttribute<int> (&memory_bus_width,
			     CU_DEVICE_ATTRIBUTE_GLOBAL_MEMORY_BUS_WIDTH,
			     dev_id);

//		int L2CacheSize;
      getCudaAttribute<int> (&l2_cache_size, CU_DEVICE_ATTRIBUTE_L2_CACHE_SIZE,
			     dev_id);

#endif

      fprintf (file, "[memory_clock_rate_khz: %d]\n", memory_clock);
      fprintf (file, "[memory_clock_rate_mhz: %.0f]\n", memory_clock * 1e-3f);
      fprintf (file, "[memory_clock_rate_ghz: %.2f]\n", memory_clock * 1e-6f);

      fprintf (file, "[memory_bus_width_bits: %d]\n", memory_bus_width);
      if (l2_cache_size != 0)
	{
	  fprintf (file, "[l2_cache_size_bytes: %d]\n", l2_cache_size);
	}

      fprintf (
	  file,
	  "[peak_bw_gflops: %.2f]\n",
	  compute_peak_theoretical_bw_kflops (memory_bus_width, memory_clock)
	      * 1e-6f);

//		printf(
//				"  Maximum Texture Dimension Size (x,y,z)         1D=(%d), 2D=(%d, %d), 3D=(%d, %d, %d)\n",
//				deviceProp.maxTexture1D, deviceProp.maxTexture2D[0],
//				deviceProp.maxTexture2D[1], deviceProp.maxTexture3D[0],
//				deviceProp.maxTexture3D[1], deviceProp.maxTexture3D[2]);
//		printf(
//				"  Maximum Layered 1D Texture Size, (num) layers  1D=(%d), %d layers\n",
//				deviceProp.maxTexture1DLayered[0],
//				deviceProp.maxTexture1DLayered[1]);
//		printf(
//				"  Maximum Layered 2D Texture Size, (num) layers  2D=(%d, %d), %d layers\n",
//				deviceProp.maxTexture2DLayered[0],
//				deviceProp.maxTexture2DLayered[1],
//				deviceProp.maxTexture2DLayered[2]);

      fprintf (file, "[total_constant memory_bytes: %lu]\n",
	       deviceProp.totalConstMem);
      fprintf (file, "[total_shared_memory_per_block_bytes: %lu]\n",
	       deviceProp.sharedMemPerBlock);
      fprintf (file, "[total_registers_available_per_block: %d]\n",
	       deviceProp.regsPerBlock);
      fprintf (file, "[warp_size: %d]\n", deviceProp.warpSize);
      fprintf (file, "[maximum_number_of_threads_per_sm: %d]\n",
	       deviceProp.maxThreadsPerMultiProcessor);
      fprintf (file, "[maximum_number_of_threads_per_block: %d]\n",
	       deviceProp.maxThreadsPerBlock);
      fprintf (file, "[max_dimensions_of_block (x,y,z): (%d, %d, %d)]\n",
	       deviceProp.maxThreadsDim[0], deviceProp.maxThreadsDim[1],
	       deviceProp.maxThreadsDim[2]);
      fprintf (file, "[max_dimensions_of_grid (x,y,z): (%d, %d, %d)]\n",
	       deviceProp.maxGridSize[0], deviceProp.maxGridSize[1],
	       deviceProp.maxGridSize[2]);
//		fprintf(file,"  Maximum memory pitch:                          %lu bytes\n",
//				deviceProp.memPitch);
//		printf("  Texture alignment:                             %lu bytes\n",
//				deviceProp.textureAlignment);
      fprintf (file, "[concurrent_copy_and_kernel_execution: %d\n",
	       deviceProp.deviceOverlap);
      fprintf (file, "[n_copy_engines: %d]\n", deviceProp.asyncEngineCount);
//		fprintf(file,"  Run time limit on kernels:                     %s\n",
//				deviceProp.kernelExecTimeoutEnabled ? "Yes" : "No");
      fprintf (file, "[integrated_gpu_sharing_host_memory: %d\n",
	       deviceProp.integrated);
      fprintf (file, "[support_host_page_locked_memory_mapping: %d]\n",
	       deviceProp.canMapHostMemory);
//		fprintf(file,"[alignment_requirement_for_surfaces: %d]\n",
//				deviceProp.surfaceAlignment);
      fprintf (file, "[ecc_support: %d]\n", deviceProp.ECCEnabled);
      fprintf (file, "[support_unified_addressing (UVA): %d]\n",
	       deviceProp.unifiedAddressing);
//		printf(
//				"  Device PCI Domain ID / Bus ID / location ID:   %d / %d / %d\n",
//				deviceProp.pciDomainID, deviceProp.pciBusID,
//				deviceProp.pciDeviceID);

//		const char *sComputeMode[] =
//				{
//						"Default (multiple host threads can use ::cudaSetDevice() with device simultaneously)",
//						"Exclusive (only one host thread in one process is able to use ::cudaSetDevice() with this device)",
//						"Prohibited (no host thread can use ::cudaSetDevice() with this device)",
//						"Exclusive Process (many threads in one process is able to use ::cudaSetDevice() with this device)",
//						"Unknown",
//						NULL };
//		printf("  Compute Mode:\n");
//		printf("     < %s >\n", sComputeMode[deviceProp.computeMode]);
      fclose (file);

    }

// finish
// cudaDeviceReset causes the driver to clean up all state. While
// not mandatory in normal operation, it is good practice.  It is also
// needed to ensure correct operation when the application is being
// profiled. Calling cudaDeviceReset causes all profile data to be
// flushed before the application exits
  cudaDeviceReset ();
  exit (EXIT_SUCCESS);
}

///////////////////////////////////////

__global__ void
kernel_gmem_latency_v0 (u32* a, u32*b, u32*clock_count, int n)
{

  int tid = blockIdx.x * blockDim.x + threadIdx.x;
//	printf("tid=%d\n", tid);
//	a[tid] = 12;//b[tid]; //tid;//stop - start;
  int t0, t1;
  clock_t start, stop;
  start = clock ();
  t0 = a[tid];
  stop = clock ();
  t1 = b[tid];
  a[tid] = t0 - t1;
  clock_count[tid] = (u32 (stop - start));
//	if (tid==0)
//		printf("%llu , %llu \n", start, stop-start);

}

///////////////////////////////////////
__global__ void
kernel_gmem_non_cached_latency (u32* a, u32*b, u32*clock_count, int n)
{

  int tid = blockIdx.x * blockDim.x + threadIdx.x;
  int t;
  clock_t start, elapsed = 0;

  int rep = 1;
  for (int i = 0; i < rep; i++)
    {
      start = clock ();
      t = b[tid];
      elapsed += clock () - start;
      a[tid] += t * (i + rep);
      b[tid] = a[tid];
    }

  clock_count[tid] = u32 (elapsed) / (rep);
}

///////////////////////////////////////
__global__ void
kernel_gmem_cached_latency (u32* a, u32*b, u32*clock_count, int n)
{

  int tid = blockIdx.x * blockDim.x + threadIdx.x;
  int t;
  clock_t start, elapsed = 0;

  int rep = 1;
  for (int i = 0; i < rep; i++)
    {
      start = clock ();
      t = b[tid];
      elapsed += clock () - start;
      a[tid] += t * (i + rep);
    }

  clock_count[tid] = u32 (elapsed) / (rep);
}

///////////////////////////////////////
__global__ void
kernel_gmem_departure_delay (u32* a, u32*b, u32*clock_count, int n, int d)
{

  int tid = blockIdx.x * blockDim.x + threadIdx.x;
  int t;
  clock_t start, elapsed = 0;

  int rep = 10;
//  for (int i = 0; i < rep; i++)
    {
//      for (int j = 0; j < d; j++)
	{
//	  int idx=tid+j*n;

	  int idx = n + tid;
	  t = a[tid];
//	  elapsed += clock () - start;
	  b[tid] += t * (tid + rep);
	  start = clock ();
	  t = a[idx];
	  elapsed = clock () - start;
	  b[tid] += t * (tid + rep);
	}
    }

  clock_count[tid] = u32 (elapsed); // / (d*rep);
}

///////////////////////////////////////

int
compute_device_gmem_latency (u32 &LD_cached, u32 &LD_non_cached,
			     int n_log2 = 20, int block_size = 32)
{

  LD_cached = 0, LD_non_cached = 0;
  int n = (1 << n_log2);
  u32* host_a, *host_b, *host_clock;

  u32* dev_a, *dev_b, *dev_clock;
  host_a = (u32*) malloc (n * sizeof(u32));
  host_b = (u32*) malloc (n * sizeof(u32));
  host_clock = (u32*) malloc (n * sizeof(u32));

  for (int i = 0; i < n; i++)
    host_a[i] = 0xFFFFFFFF - i;
  for (int i = 0; i < n; i++)
    host_b[i] = 0xFFFFFFFF / 2 - (i / 2);
  memset (host_clock, 0x00, n * sizeof(u32));

  cudaMalloc ((void**) &dev_a, n * sizeof(u32));
  cudaMalloc ((void**) &dev_b, n * sizeof(u32));
  cudaMalloc ((void**) &dev_clock, n * sizeof(u32));

  cudaMemcpy (dev_a, host_a, n * sizeof(u32), cudaMemcpyHostToDevice);
  cudaMemcpy (dev_b, host_b, n * sizeof(u32), cudaMemcpyHostToDevice);
  cudaMemset (dev_clock, 0x00, n * sizeof(u32));
//  int block_size = 256;
//	printf("n=%d, n_blocks=%d, block_size=%d\n", n, n / block_size, block_size);
  kernel_gmem_cached_latency <<<n / block_size, block_size>>> (dev_a, dev_b,
							       dev_clock, n);
  cudaMemcpy (host_clock, dev_clock, n * sizeof(u32), cudaMemcpyDeviceToHost);

  u32 max_ld = 0;
  u32 min_ld = 0xFFFFFFFF;
  for (int i = 0; i < n; i++)
    {
      if (host_clock[i] > max_ld)
	max_ld = host_clock[i];
      if (host_clock[i] < min_ld)
	min_ld = host_clock[i];
    }
  LD_cached = max_ld;
//      printf("MAX_LD_CACHED =%d\n", max_ld);

  kernel_gmem_non_cached_latency <<<n / block_size, block_size>>> (dev_a, dev_b,
								   dev_clock,
								   n);
  cudaMemcpy (host_clock, dev_clock, n * sizeof(u32), cudaMemcpyDeviceToHost);
  max_ld = 0;
  min_ld = 0xFFFFFFFF;
  for (int i = 0; i < n; i++)
    {
      if (host_clock[i] > max_ld)
	max_ld = host_clock[i];
      if (host_clock[i] < min_ld)
	min_ld = host_clock[i];
    }
//    printf("MAX_LD_NON_CACHED =%d\n", max_ld);
  LD_non_cached = max_ld;

  cudaFree (dev_a);
  cudaFree (dev_b);
  cudaFree (dev_clock);

  return 0;
}

///////////////////////////////////////

int
compute_device_departure_delay (u32 &departure_delay, int n_log2 = 20,
				int block_size = 32)
{

  departure_delay = 0;
  int n = (1 << n_log2);

  int d = 32;
  u32* host_a, *host_b, *host_clock;

  u32* dev_a, *dev_b, *dev_clock;

  host_a = (u32*) malloc (n * d * sizeof(u32));
  host_b = (u32*) malloc (n * sizeof(u32));
  host_clock = (u32*) malloc (n * sizeof(u32));

  for (int i = 0; i < n * d; i++)
    host_a[i] = 0xFFFFFFFF - i;
  for (int i = 0; i < n; i++)
    host_b[i] = 0xFFFFFFFF / 2 - (i / 2);
  memset (host_clock, 0x00, n * sizeof(u32));

  cudaMalloc ((void**) &dev_a, n * d * sizeof(u32));
  cudaMalloc ((void**) &dev_b, n * sizeof(u32));
  cudaMalloc ((void**) &dev_clock, n * sizeof(u32));

  cudaMemcpy (dev_a, host_a, n * d * sizeof(u32), cudaMemcpyHostToDevice);
  cudaMemcpy (dev_b, host_b, n * sizeof(u32), cudaMemcpyHostToDevice);
  cudaMemset (dev_clock, 0x00, n * sizeof(u32));
//  int block_size = 256;
//	printf("n=%d, n_blocks=%d, block_size=%d\n", n, n / block_size, block_size);
  kernel_gmem_departure_delay <<<n / block_size, block_size>>> (dev_a, dev_b,
								dev_clock, n,
								d);
  cudaMemcpy (host_clock, dev_clock, n * sizeof(u32), cudaMemcpyDeviceToHost);

  u32 max_ld = 0;
  u32 min_ld = 0xFFFFFFFF;
  for (int i = 0; i < n; i++)
    {
//      printf("[tid=%d, warp_id=%d, cc = %d] \n", i, (i/32), host_clock[i]);
//      printf("[tid=%-10d, warp_id=%-10d, cc = %lu] \n", i, (i/32), host_clock[i]);
      if (host_clock[i] > max_ld)
	max_ld = host_clock[i];
      if (host_clock[i] < min_ld)
	min_ld = host_clock[i];
    }

  for (int i = 0; i < n; i+=32)
    {
//      host_clock[i] -= min_ld;
      //      printf("[tid=%d, warp_id=%d, cc = %d] \n", i, (i/32), host_clock[i]);
      if (i % 32 == 0)
	printf ("[tid=%-10d, warp_id=%-10d, cc = %lu] \n", i, (i / 32),
		host_clock[i]);
    }

  departure_delay = max_ld;
//  LD_cached = max_ld;
//  printf ("DEPARTURE_DELAY = %d\n", max_ld);
//
//  kernel_gmem_non_cached_latency<<<n / block_size, block_size>>> (dev_a, dev_b,
//      dev_clock,
//      n);
//  cudaMemcpy (host_clock, dev_clock, n * sizeof(u32), cudaMemcpyDeviceToHost);
//  max_ld = 0;
//  min_ld = 0xFFFFFFFF;
//  for (int i = 0; i < n; i++)
//    {
//      if (host_clock[i] > max_ld)
//	max_ld = host_clock[i];
//      if (host_clock[i] < min_ld)
//	min_ld = host_clock[i];
//    }
//    printf("MAX_LD_NON_CACHED =%d\n", max_ld);
//  LD_non_cached = max_ld;
  return 0;
}

///////////////////////////////////////

//[Issue_cycles: 4]
//[Mem_bandwidth: 120]
//[Mem_LD: 420]
//[Departure_del_uncoal: 10]
//[Departure_del_coal: 4]
//[Active_SMs: 16]
//[Freq: 1]
//[Load_bytes_per_warp: 128]
int
host_get_device_specs_for_mwp_cwp (int argc, char ** argv)
{
//	printf("CUDA Device Query (Runtime API) version (CUDART static linking)\n");

  int n_devices = 0;
  cudaError_t error_id = cudaGetDeviceCount (&n_devices);

  if (error_id != cudaSuccess)
    {
      printf ("cudaGetDeviceCount returned %d\n-> %s\n", (int) error_id,
	      cudaGetErrorString (error_id));
//		printf("Result = FAIL\n");
      exit (EXIT_FAILURE);
    }

// This function call returns 0 if there are no CUDA capable devices.
  if (n_devices == 0)
    {
      printf ("-There are no available device(s) that support CUDA\n");
      exit (EXIT_FAILURE);
    }
  else
    {
      printf ("[Found %d CUDA Capable device(s) ... ]\n", n_devices);
    }

  int dev_id, driver_version = 0, runtime_version = 0;

  for (dev_id = 0; dev_id < n_devices; ++dev_id)
    {
      char file_name[256];
      char device_name[256];

      cudaSetDevice (dev_id);
      cudaDeviceProp deviceProp;

      cudaGetDeviceProperties (&deviceProp, dev_id);

      sprintf (device_name, "%s", deviceProp.name);
//		printf("checking device : %s\n", device_name);
      sprintf (file_name, "%s.specs", to_lower_str (device_name));
      printf ("writing specs to [%s] ... \n", file_name);
      FILE* file = fopen (file_name, "w");

      cudaDriverGetVersion (&driver_version);
      cudaRuntimeGetVersion (&runtime_version);
//				fprintf(file,
//				"(%2d) Multiprocessors, (%3d) CUDA Cores/MP:     %d CUDA Cores\n",
//				deviceProp.multiProcessorCount,
//				_ConvertSMVer2Cores(deviceProp.major, deviceProp.minor),
//				_ConvertSMVer2Cores(deviceProp.major, deviceProp.minor)
//						* deviceProp.multiProcessorCount);
//		fprintf(file, "[max_clock_rate_khz: %d]\n", deviceProp.clockRate);
//		fprintf(file, "[max_clock_rate_mhz: %.0f]\n",
//				deviceProp.clockRate * 1e-3f);

      int memory_clock = 0;
      int memory_bus_width = 0;
      int l2_cache_size = 0;

#if CUDART_VERSION >= 5000
// This is supported in CUDA 5.0 (runtime API device properties)
//		printf("  Memory Clock rate:                             %.0f Mhz\n", deviceProp.memoryClockRate * 1e-3f);
//		printf("  Memory Bus Width:                              %d-bit\n", deviceProp.memoryBusWidth);

      memory_clock=deviceProp.memoryClockRate;
      memory_bus_width=deviceProp.memoryBusWidth;
      if (deviceProp.l2CacheSize)
	{
//			printf("  L2 Cache Size:                                 %d bytes\n", deviceProp.l2CacheSize);
	  l2_cache_size=deviceProp.l2CacheSize;
	}

#else
// This only available in CUDA 4.0-4.2 (but these were only exposed in the CUDA Driver API)
//		int memoryClock;
      getCudaAttribute<int> (&memory_clock,
			     CU_DEVICE_ATTRIBUTE_MEMORY_CLOCK_RATE, dev_id);

//		int memBusWidth;
      getCudaAttribute<int> (&memory_bus_width,
			     CU_DEVICE_ATTRIBUTE_GLOBAL_MEMORY_BUS_WIDTH,
			     dev_id);

//		int L2CacheSize;
      getCudaAttribute<int> (&l2_cache_size, CU_DEVICE_ATTRIBUTE_L2_CACHE_SIZE,
			     dev_id);

#endif

      u32 mem_ld = 0;
      u32 mem_ld_cached = 0;
      u64 max_mem_ld = 0;
      for (int b = 5; b <= 10; b++)
	{
	  int block_size = (1 << b);
	  for (int i = 10; i < 25; i++)
	    {
	      compute_device_gmem_latency (mem_ld_cached, mem_ld, i,
					   block_size);
	      if (mem_ld > max_mem_ld)
		max_mem_ld = mem_ld;
	    }
	}
//		mem_ld/=(20-10);
//		printf("mem_ld=%d\n",mem_ld);

      //??
      fprintf (file, "[Issue_cycles: %d]\n", 1);

      //checked.
      fprintf (
	  file,
	  "[Mem_bandwidth: %.2f]\n",
	  compute_peak_theoretical_bw_kflops (memory_bus_width, memory_clock)
	      * 1e-6f);

      //checked.
      fprintf (file, "[Mem_LD: %d]\n", max_mem_ld);
      //??
      fprintf (file, "[Departure_del_uncoal: %d]\n", 4);
      //??
      fprintf (file, "[Departure_del_coal: %d]\n", 4);

      //checked.
      fprintf (file, "[Active_SMs: %d]\n", deviceProp.multiProcessorCount);
      //checked.
      fprintf (file, "[Freq: %d]\n", int (deviceProp.clockRate * 1e-3f));
      //checked.
      fprintf (file, "[Load_bytes_per_warp: %d]\n", 128);

      fprintf (file, "\n");

//		fprintf(file, "[device_id: %d]\n", dev_id);
      fprintf (file, "[device_name: %s]\n", (device_name));
      fprintf (file, "[driver_version: %d.%d]\n", driver_version / 1000,
	       (driver_version % 100) / 10);
      fprintf (file, "[runtim_version: %d.%d]\n", runtime_version / 1000,
	       (runtime_version % 100) / 10);
      fprintf (file, "[compute_capability: %d.%d]\n", deviceProp.major,
	       deviceProp.minor);
      fprintf (file, "[global_memory_bytes: %llu]\n",
	       (unsigned long long) deviceProp.totalGlobalMem);
      fprintf (file, "[n_cores_per_sm: %3d]\n",
	       _ConvertSMVer2Cores (deviceProp.major, deviceProp.minor));

      fprintf (file, "[n_blocks_per_sm: %d]\n",
	       get_nblocks_per_SM (deviceProp.major, deviceProp.minor));

      fprintf (
	  file,
	  "[n_cores: %d]\n",
	  _ConvertSMVer2Cores (deviceProp.major, deviceProp.minor)
	      * deviceProp.multiProcessorCount);
      //		fprintf(file, "[memory_clock_rate_khz: %d]\n", memory_clock);
      //		fprintf(file, "[memory_clock_rate_mhz: %.0f]\n", memory_clock * 1e-3f);
      fprintf (file, "[memory_clock_rate_ghz: %.2f]\n", memory_clock * 1e-6f);

      fprintf (file, "[memory_bus_width_bits: %d]\n", memory_bus_width);
      if (l2_cache_size != 0)
	{
	  fprintf (file, "[l2_cache_size_bytes: %d]\n", l2_cache_size);
	}

//		printf(
//				"  Maximum Texture Dimension Size (x,y,z)         1D=(%d), 2D=(%d, %d), 3D=(%d, %d, %d)\n",
//				deviceProp.maxTexture1D, deviceProp.maxTexture2D[0],
//				deviceProp.maxTexture2D[1], deviceProp.maxTexture3D[0],
//				deviceProp.maxTexture3D[1], deviceProp.maxTexture3D[2]);
//		printf(
//				"  Maximum Layered 1D Texture Size, (num) layers  1D=(%d), %d layers\n",
//				deviceProp.maxTexture1DLayered[0],
//				deviceProp.maxTexture1DLayered[1]);
//		printf(
//				"  Maximum Layered 2D Texture Size, (num) layers  2D=(%d, %d), %d layers\n",
//				deviceProp.maxTexture2DLayered[0],
//				deviceProp.maxTexture2DLayered[1],
//				deviceProp.maxTexture2DLayered[2]);

      fprintf (file, "[total_constant memory_bytes: %lu]\n",
	       deviceProp.totalConstMem);
      fprintf (file, "[total_shared_memory_per_block_bytes: %lu]\n",
	       deviceProp.sharedMemPerBlock);
      fprintf (file, "[total_registers_available_per_block: %d]\n",
	       deviceProp.regsPerBlock);
      fprintf (file, "[warp_size: %d]\n", deviceProp.warpSize);
      fprintf (file, "[maximum_number_of_threads_per_sm: %d]\n",
	       deviceProp.maxThreadsPerMultiProcessor);
      fprintf (file, "[maximum_number_of_threads_per_block: %d]\n",
	       deviceProp.maxThreadsPerBlock);
      fprintf (file, "[max_dimensions_of_block (x,y,z): (%d, %d, %d)]\n",
	       deviceProp.maxThreadsDim[0], deviceProp.maxThreadsDim[1],
	       deviceProp.maxThreadsDim[2]);
      fprintf (file, "[max_dimensions_of_grid (x,y,z): (%d, %d, %d)]\n",
	       deviceProp.maxGridSize[0], deviceProp.maxGridSize[1],
	       deviceProp.maxGridSize[2]);
//		fprintf(file,"  Maximum memory pitch:                          %lu bytes\n",
//				deviceProp.memPitch);
//		printf("  Texture alignment:                             %lu bytes\n",
//				deviceProp.textureAlignment);
      fprintf (file, "[concurrent_copy_and_kernel_execution: %d\n",
	       deviceProp.deviceOverlap);
      fprintf (file, "[n_copy_engines: %d]\n", deviceProp.asyncEngineCount);
//		fprintf(file,"  Run time limit on kernels:                     %s\n",
//				deviceProp.kernelExecTimeoutEnabled ? "Yes" : "No");
      fprintf (file, "[integrated_gpu_sharing_host_memory: %d\n",
	       deviceProp.integrated);
      fprintf (file, "[support_host_page_locked_memory_mapping: %d]\n",
	       deviceProp.canMapHostMemory);
//		fprintf(file,"[alignment_requirement_for_surfaces: %d]\n",
//				deviceProp.surfaceAlignment);
      fprintf (file, "[ecc_support: %d]\n", deviceProp.ECCEnabled);
      fprintf (file, "[support_unified_addressing (UVA): %d]\n",
	       deviceProp.unifiedAddressing);
//		printf(
//				"  Device PCI Domain ID / Bus ID / location ID:   %d / %d / %d\n",
//				deviceProp.pciDomainID, deviceProp.pciBusID,
//				deviceProp.pciDeviceID);

//		const char *sComputeMode[] =
//				{
//						"Default (multiple host threads can use ::cudaSetDevice() with device simultaneously)",
//						"Exclusive (only one host thread in one process is able to use ::cudaSetDevice() with this device)",
//						"Prohibited (no host thread can use ::cudaSetDevice() with this device)",
//						"Exclusive Process (many threads in one process is able to use ::cudaSetDevice() with this device)",
//						"Unknown",
//						NULL };
//		printf("  Compute Mode:\n");
//		printf("     < %s >\n", sComputeMode[deviceProp.computeMode]);
      fclose (file);

      sprintf (file_name, "%s.mem", to_lower_str (device_name));
      printf ("writing mem load specs to [%s] ... \n", file_name);
      file = fopen (file_name, "w");

      u32 ld_cached, ld_noncached;
      for (int n = 10; n <= 20; n++)
	for (int block_size = 32; block_size <= 1024; block_size <<= 1)
	  {
	    compute_device_gmem_latency (ld_cached, ld_noncached, n,
					 block_size);

	    // log(n), block_size, cached_ld, non_cached_ld
	    fprintf (file, "[%d, %d, %d, %d]\n", n, block_size, ld_cached,
		     ld_noncached);
	  }
      fclose (file);
    }

// finish
// cudaDeviceReset causes the driver to clean up all state. While
// not mandatory in normal operation, it is good practice.  It is also
// needed to ensure correct operation when the application is being
// profiled. Calling cudaDeviceReset causes all profile data to be
// flushed before the application exits
  cudaDeviceReset ();
  exit (EXIT_SUCCESS);
}

///////////////////////////////////////
int
test_device_gmem_latency (int argc, char ** argv)
{
  int n = 10;
  int block_size = 32;
  if (argc > 1)
    n = atoi (argv[1]);

  if (argc > 2)
    block_size = atoi (argv[2]);

  u32 ld_cached, ld_noncached;

  for (n = 10; n < 20; n++)
    for (block_size = 32; block_size <= 1024; block_size <<= 1)
      {
	compute_device_gmem_latency (ld_cached, ld_noncached, n, block_size);
	printf ("[n=2^%d, b=%d, cached=%d, non-cached=%d]\n", n, block_size,
		ld_cached, ld_noncached);
	printf ("============================\n");
      }
  return 0;
}

///////////////////////////////////////
int
test_device_gmem_departure_delay (int argc, char ** argv)
{
  int n = 10;
  int block_size = 32;
  if (argc > 1)
    n = atoi (argv[1]);

  if (argc > 2)
    block_size = atoi (argv[2]);
  u32 departure_delay = 0;
//  for (block_size = 32; block_size <= 1024; block_size <<= 1)
//    for (n = 10; n < 20; n++)
    {
      compute_device_departure_delay (departure_delay, n, block_size);
//	printf ("[n=2^%d, b=%d, departure_delay=%d]\n", n, block_size,
//		departure_delay);
//	printf ("============================\n");
    }
  return 0;
}

///////////////////////////////////////
// Program main
///////////////////////////////////////
int
main (int argc, char **argv)
{
//	return host_get_device_specs_full(argc, argv);
  return host_get_device_specs_for_mwp_cwp (argc, argv);
//	return compute_device_gmem_latency(argc, argv);

//  return test_device_gmem_latency (argc, argv);
//  return test_device_gmem_departure_delay(argc, argv);
}

///////////////////////////////////////

