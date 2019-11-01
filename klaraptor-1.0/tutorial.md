# Tutorial for `polybench_2DConv`
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
	- `2DConvolution_instrumented.bin` (optimized by KLARAPTOR)
	- `2DConvolution.bin` (non-optimized program)
