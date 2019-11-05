* KLARAPTOR is a compile-time tool for CUDA programs which constructs and
  evaluates a rational program encoding the MWP-CWP performance model to 
  optimize the program’s performance. The tool dynamically chooses a kernel’s
  launch parameters (thread block configuration) which optimize its performance
  for the data and hardware param- eters of the current kernel invocation.

* KLARAPTOR is built in the C programming language making use of (1) NVIDIA
  CUPTI API to measure low-level metrics, (2) LAPACK for linear algebra, (3)
  BPAS (Basic Polynomial Algebra Subprograms) for efficient rational functions,
  and (4) LLVM Pass Framework to connect the constructed rational program and
  the CUDA program.

* Install KLARAPTOR by following the instructions in INSTALL.

* NVIDIA cuda 9.2 or later must be already intalled, also, variable CUDA_PATH
  should be part of your ~/.bashrc.  

* After installation, you might need to reload nvidia modules. The easiest way
  is to reboot.

* Authors
  * Davood Mohajerani <dmohajer@uwo.ca>
  * Linxiao Wang <lwang739@uwo.ca>
  * Alexander Brandt <abrandt5@uwo.ca> 
  * Marc Moreno Maza <moreno@csd.uwo.ca>
