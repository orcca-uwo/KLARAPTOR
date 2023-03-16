#include "rp.h"
#include <string.h>
///////////////////////////////////////
int
get_value_from_process (char *cmd, int * dim3_blockdim)
{
  FILE *fp;
  char buffer[1024];

  /* Open the command for reading. */
  fp = popen (cmd, "r");
  if (fp == NULL)
    {
      printf ("[@rp][ERROR: Failed to run command]...\n");
      exit (EXIT_FAILURE);
    }

	
//  while (fgets(buffer, sizeof(buffer)-1, fp) != NULL)
	
	
	const char start_pattern[64]="[best_ec][idx, B0, B1]";
	int pattern_found=0;
	while(pattern_found==0)
	{
		if(fgets(buffer, sizeof(buffer)-1, fp)!=NULL)
			if (strstr(buffer, start_pattern)!=NULL)
			{
				pattern_found=1;
				break;
			}
	}
	
	if (pattern_found==1)
  for(int i=0;i<1;i++)
    {
      if (fgets(buffer, sizeof(buffer)-1, fp)!=NULL)
				{
//					printf("%s", buffer);	
					int idx;
					sscanf(buffer, "%d, %d, %d, %d", &idx, &dim3_blockdim[0],
							&dim3_blockdim[1], &dim3_blockdim[2]);
				}
      else
	break;
    }
  pclose(fp);

	//normalizing problematic values.
  for(int i=0;i<3;i++)
  	if(dim3_blockdim[i]<=0)
  		dim3_blockdim[i]=1;
//	printf("BEST_BX_BY_BZ=%d,%d,%d\n", dim3_blockdim[0], dim3_blockdim[1],
//			dim3_blockdim[2]);

  return EXIT_SUCCESS;
}
///////////////////////////////////////

int
rp_estimator (char * kernel_name, int n, int * launch_params)
{
//	return EXIT_SUCCESS;
  int * gx = &launch_params[0];
  int * gy = &launch_params[1];
  int * gz = &launch_params[2];

  int * bx = &launch_params[3];
  int * by = &launch_params[4];
  int * bz = &launch_params[5];

//	printf("    [@RP][RP init]: simulating the elapsed time of computing best config ...\n");
//	sleep(1);
//	printf("    [@RP][RP done]; best results computed ...\n");

  char rp_name[1024];
  char rp_result_path[1024];
  sprintf (rp_name, "rational_program_%s.bin", kernel_name);
//  sprintf (rp_result_path, "best_occupancy_kernel_%s_%d.tmp", kernel_name, n);
  sprintf (rp_result_path, "best_ec_kernel_%s_%d.tmp", kernel_name, n);
  char rp_cmd[1024];
//  sprintf (rp_cmd,
//	   "./%s %d sm_60 ../../device_profiles/gtx1080ti.specs >/dev/null",
//	   rp_name, n);
  sprintf (rp_cmd, "./%s %d sm_75 ../../src/device_profiles/geforcertx2070super.specs 1",
	   rp_name, n); ///
  system (rp_cmd);
  int blockdim[3];
  get_value_from_process(rp_cmd, blockdim);

//	printf("rp_cmd= [%s]\n", rp_cmd);

  int best_x = blockdim[0], best_y = blockdim[1], best_z=blockdim[2];

//  FILE* rp_result_file = fopen (rp_result_path, "r");
//  fscanf (rp_result_file, "%d,%d", &best_x, &best_y);
//  fclose (rp_result_file);

  printf ("[@RP][best][B0 = %d, B1 = %d, B2 = %d]\n", best_x, best_y, best_z);

////	computing griddim3 based on blockdim3 and n must be writtern as a function.
  *gx = n / best_x;
  *gy = n / best_y;
  *gz = 1;

  *bx = best_x; //2*n;
  *by = best_y; //3*n;
  *bz = best_z; //4*n;

//	*gx=1;
//	*gy=1;
//	*gz=1;
//	*bx=n/4;
//	*by=4;
//	*bz=1;

  return EXIT_SUCCESS;
}

///////////////////////////////////////
