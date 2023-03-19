# Tutorial for KLARAPTOR

* KLARAPTOR (Kernel LAunch parameters RAtional Program estimaTOR), a freely available tool to dynamically determine the optimal values of kernel launch parameters of a CUDA kernel. We describe a technique for building a helper program, at the compile-time of a CUDA program, that is used at run-time to determine near-optimal kernel launch parameters for the kernels of that CUDA program. This technique leverages the MWP-CWP performance prediction model, runtime data parameters, and runtime hardware parameters to dynamically determine the launch parameters for each kernel invocation. This technique is implemented within the KLARAPTOR tool, utilizing the LLVM Pass Framework and NVIDIA Nsight Compute (ncu) profiler. We demonstrate the effectiveness of our approach through experimentation on the PolyBench benchmark suite of CUDA kernels.


* The example below is to demonstrate how to run KLARAPTOR on the available PolyBench examples in the `tests` directory. However KLARAPTOR can be used for all CUDA kernels, given that the correct annotations are used. The annotations made will be shown in the example below.

* For the tutorial below you will need to access your device's .specs which will be located in `src/device_profiles/` directory
  * This file contains basic hardware information about the GPU such as Compute Capability which will be required for running KLARAPTOR.


## `polybench_2DConv` example

### Annotations

* Below is an example of the pragma's used for the 2DConv example ():

```c++
#pragma kernel_info_size_param_idx_Convolution2D_kernel = 2;
#pragma kernel_info_dim_Convolution2D_kernel = 2;
```

* Refer to `2DConvolution_KLARAPTOR_edited.cu` & `2DConvolution.cu` to see all the differences being made to code.


### Running KLARAPTOR

* `cd $KLARAPTOR_PATH/tests/polybench_2DConv_llvm/`   ## move to example directory.

* `klaraptor --conf CONFIG_FILE_PATH.conf`   ## create a config file.	

* Edit the config file that you just created, modify the following	
	* Change `SM_ARCH` to Compute Capability (CC) of your target GPU. If you do 
	not know the CC value, refer to the following file:

		`$KLARAPTOR_PATH/src/device_specs/DEVICE_NAME.specs` (find the line containing `Compute_Capability`).
	* Change `DEVICE_NAME` to the name of your target GPU (similar to above).
* `klaraptor --opt CONFIG_FILE_PATH.conf`   ## optimize the runtime parameters for
	`2DConvolution.cu`

* After the optimization is done (a few minutes), you will find two binaries:
	- `2DConvolution_instrumented.bin` (optimized by KLARAPTOR, helper program that determines the best kernel launch parameters for the given Kernel)
	- `2DConvolution.bin` (non-optimized CUDA program)

For further instructions on how to run KLARAPTOR, please contact:
  * Taabish Jeshani <tjeshan@uwo.ca>