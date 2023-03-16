#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <math.h>
#include <gmp.h>
#include "../../src/mcwp/mcwp.h"

///////////////////////////////////////
"INCLUDE_FILES_REPLACEMENT"

///////////////////////////////////////
// check the constraint file for a kernel
// return 0 on success.
//int
//eval_constraints (char * kernel_name, int n, int b0, int b1)
//  {
//    ////////////////////////////////////
//    const char * eval_const_script = "../../mcwp/eval_const.py";
////  copy_file (eval_const_script, ".");
//    ////////////////////////////////////
//    char cmd[1024];
//    sprintf (cmd, "python %s %s %d %d %d", eval_const_script, kernel_name, n, b0,
//	b1);
//
//    FILE* pipe;
//    pipe = popen (cmd, "r");
//    if (pipe == NULL)
//      {
//	printf ("ERROR: FAILED TO RUN [%s]\n", cmd);
//	return (EXIT_FAILURE);
//      }
//    char buffer[32];
//    int value = 0;
//    if (fgets (buffer, sizeof(buffer) - 1, pipe) != NULL)
//      {
//	value = atoi (buffer);
//      }
//    else
//      {
//	//		printf("ERROR: NOTHING READ!\n");
//	value = -1;
//      }
//
//    pclose (pipe);
//    return value;
//  }

/////////////////////////////////////////
//int
//ceiling_div (int x, int y)
//{
//  return ((x + y - 1) / y);
//}
//
///////////////////////////////////////

#define MAX_BLOCK_SIZE 1024
#define MIN_BLOCK_SIZE 32

///////////////////////////////////////

#ifndef VERBOSE_RP
#define VERBOSE_RP 1
#endif

///////////////////////////////////////

int
flooring_div (int x, int y)
{
  return (x / y);
}

/////////////////////////////////////////

int
min (int x, int y)
{
  if (x <= y)
    return x;
  else
    return y;
}

///////////////////////////////////////

int
eval_functions (int n, int b0, int b1, int n_var, AltArr_t* Numer,
		AltArr_t* Denom)
{
  int nvar = 3;
  mpq_t numVal;
  mpq_t denVal;
  mpq_t point[3];
  mpq_init (numVal);
  mpq_init (denVal);
  for (int i = 0; i < 3; ++i)
    {
      mpq_init (point[i]);
    }

  mpq_set_si (point[0], n, 1);
  mpq_set_si (point[1], b0, 1);
  mpq_set_si (point[2], b1, 1);

  evalPolyToVal_AA (Numer, point, nvar, numVal);
  evalPolyToVal_AA (Denom, point, nvar, denVal);

//  gmp_printf ("%f\n", mpq_get_d (numVal));
//  gmp_printf ("%f\n", mpq_get_d (denVal));
  if (mpq_sgn (denVal) == 0)
    {
      printf ("Zero denominator error for actual block number!\n");
      exit (EXIT_FAILURE);
    }
  mpq_div (numVal, numVal, denVal);
  int dval = ceil (mpq_get_d (numVal));

//    printf("evalued_val=%d\n", dval);

  mpq_clear (numVal);
  mpq_clear (denVal);
  for (int i = 0; i < 3; ++i)
    {
      mpq_clear (point[i]);
    }

  return dval;
}

/////////////////////////////////////////
//int get_shared_mem(int b0, int b1, int n_var, AltArr_t* Numer,
//		AltArr_t* Denom) {
//	int nvar = n_var;
//	mpq_t numVal;
//	mpq_t denVal;
//	mpq_t point[3];
//	mpq_init(numVal);
//	mpq_init(denVal);
//	for (int i = 0; i < 3; i++) {
//		mpq_init(point[i]);
//	}
//	mpq_set_si(point[0], b0, 1);
//	if (nvar == 2)
//		mpq_set_si(point[1], b1, 1);
//
//	evalPolyToVal_AA(Numer, point, nvar, numVal);
//	evalPolyToVal_AA(Denom, point, nvar, denVal);
//	if (mpq_sgn(denVal) == 0) {
//		printf("Zero denominator error for shared memory!\n");
//		exit (EXIT_FAILURE);
//	}
//
//	mpq_div(numVal, numVal, denVal);
//	int dval = mpq_get_d(numVal);
//	return dval;
//}

///////////////////////////////////////

float
get_occupancy (int b0, int b1, int* arch_params, int B_active)
{

  int W_max = arch_params[1];

  int T = b0 * b1;

  float occupancy = ceiling_div(((float) 1000000 * B_active * T), ((float) 32 * W_max));
  occupancy = (1.0f*occupancy)/1000000.0f;
  return occupancy;
}

///////////////////////////////////////

int
get_b_actual (int n, int b0, int b1, int n_register, int* arch_params,
	      int n_var, int B_actual, int z)
{

  int R = n_register;
  int B_max = arch_params[0];
  int W_max = arch_params[1];
  int T_max = arch_params[2];
  int R_max = arch_params[3];
  int Z_max = arch_params[4];
//  printf("%d %d %d %d %d\n",arch_params[0],arch_params[1],arch_params[2],arch_params[3],arch_params[4]);

  int B_active = 0;
//  printf("%d\n",B_actual);

  int Z = z;
  //printf("%d\n",Z);
  int T = b0 * b1;

  if ((T * B_max <= 32 * W_max) && (R * T * B_max <= R_max)
      && (Z * B_max <= Z_max))
    {
//      printf("Case 1\n");
      B_active = B_max;
    }
  else if ((32 * W_max <= T * B_max) && (32 * W_max * R <= R_max)
      && (32 * W_max * Z <= Z_max * T))
    {
//      	printf("Case 2\n");
//      	printf("T=%d\n", T);

      B_active = flooring_div (32 * W_max, T);
    }
  else if ((R_max <= R * T * B_max) && (R_max <= 32 * R * W_max)
      && (R_max * Z <= R * T * Z_max))
    {
//      	printf("Case 3\n");
      B_active = flooring_div (R_max, R * T);
    }
  else if ((Z_max <= B_max * Z) && (Z_max * T <= 32 * W_max * Z)
      && (Z_max * R * T <= Z * R_max))
    {
//      	printf("Case 4\n");
      B_active = flooring_div (Z_max, Z);
    }
  else
    {
//      	printf("Active block is 0, fail to lunch the kernel!\n");
      return 0;
    }

  if (B_actual != 0)
    B_active = min (B_active, B_actual);
//  printf ("B_active=%d\n", B_active);

  return B_active;
}

///////////////////////////////////////

void
get_mwp_cwp (int* mcwp_case, mpq_t mwp, mpq_t cwp, mpq_t clockcycles,
	     int* program_params, device_params_t* device_params,
	     int thread_per_block, int n_block)
{

  // mpq_t* res = (mpq_t*) malloc(sizeof(mpq_t) * 3);
  // mpq_inits(res);

  //	{ "13.Total_insts", 0 },
  //	{ "14.Comp_insts", 1 },
  //	{ "15.Mem_insts", 2 },
  //	{ "16.Uncoal_Mem_insts", 3 },
  //	{ "17.Coal_Mem_insts", 4 },
  //	{ "18.Synch_insts", 5 },
  //	{ "19.Coal_per_mw", 6 },
  //	{ "20.Uncoal_per_mw", 7 },
  //	{ "n_active_blocks_per_sm", 8 },
  //	{ "n_blocks", 9 },
  //	{ "n_warps", 10 } };

  mpq_t Comp_insts;
  mpq_t Uncoal_Mem_insts;
  mpq_t Coal_Mem_insts;
  mpq_t Synch_insts;
  mpq_t Coal_per_mw;
  mpq_t Uncoal_per_mw;
  mpq_t Active_blocks_per_SM;

  mpq_init (Comp_insts);
  mpq_init (Uncoal_Mem_insts);
  mpq_init (Coal_Mem_insts);
  mpq_init (Synch_insts);
  mpq_init (Coal_per_mw);
  mpq_init (Uncoal_per_mw);
  mpq_init (Active_blocks_per_SM);

  mpq_set_si (Comp_insts, program_params[0], 1);
  mpq_set_si (Uncoal_Mem_insts, program_params[1], 1);
  mpq_set_si (Coal_Mem_insts, program_params[2], 1);
  mpq_set_si (Synch_insts, program_params[3], 1);
  mpq_set_si (Coal_per_mw, program_params[4], 1);
  mpq_set_si (Uncoal_per_mw, program_params[5], 1);
  mpq_set_si (Active_blocks_per_SM, program_params[6], 1);

#if VERBOSE >=3
  gmp_printf("Comp_insts = %Qd\n", Comp_insts);
  gmp_printf("Uncoal_Mem_insts = %Qd\n", Uncoal_Mem_insts);
  gmp_printf("Coal_Mem_insts = %Qd\n", Coal_Mem_insts);
  gmp_printf("Synch_insts = %Qd\n", Synch_insts);
  gmp_printf("Coal_per_mw = %Qd\n", Coal_per_mw);
  gmp_printf("Uncoal_per_mw = %Qd\n", Uncoal_per_mw);
  gmp_printf("Active_blocks_per_SM = %Qd\n", Active_blocks_per_SM);
#endif

//  mpq_set (active_blocks_per_sm, Active_blocks_per_SM); # FLAGGED

//	double Issue_cycles = device_params[0];
//	double Mem_bandwidth = device_params[1];
//	double Mem_LD = device_params[2];
//	double Departure_del_uncoal = device_params[3];
//	double Departure_del_coal = device_params[4];
//	double Active_blocks_per_SM = device_params[5];
//	double Active_SMs = device_params[6];
//	double Freq = device_params[7];
//	double Load_bytes_per_warp = device_params[8];

//  mpq_t mem_to_core_freq_ratio;
//  mpq_init (mem_to_core_freq_ratio);
//  mpq_set_ui (mem_to_core_freq_ratio, 2, 3);
//
//  gmp_printf ("mem_to_core_freq_ratio: %Qd\n", mem_to_core_freq_ratio);
//  mpq_mul (Coal_Mem_insts, Coal_Mem_insts, mem_to_core_freq_ratio);
//  mpq_mul (Uncoal_Mem_insts, Uncoal_Mem_insts, mem_to_core_freq_ratio);

//    gmp_printf ("Comp_cycles: %Qd\n", Comp_cycles);

  mpq_t Issue_cycles; //0
  mpq_t Mem_bandwidth; //1
  mpq_t Mem_LD; //2
  mpq_t Departure_del_uncoal;
  mpq_t Departure_del_coal;
  mpq_t Active_SMs;
  mpq_t Freq;
  mpq_t Load_bytes_per_warp;

  mpq_init (Issue_cycles);
  mpq_init (Mem_bandwidth);
  mpq_init (Mem_LD);
  mpq_init (Departure_del_uncoal);
  mpq_init (Departure_del_coal);
  mpq_init (Active_SMs);
  mpq_init (Freq);
  mpq_init (Load_bytes_per_warp);

//	{ "Issue_cycles", 0 },
//	{ "Mem_bandwidth", 1 },
//	{ "Mem_LD", 2 },
//	{ "Departure_del_uncoal", 3 },
//	{ "Departure_del_coal", 4 },
//	{ "Active_SMs", 5 },
//	{ "Freq", 6 },
//	{ "Load_bytes_per_warp", 7 } };

  mpq_set_si (Issue_cycles, device_params->Issue_cycles, 1);
  mpq_set_si (Mem_bandwidth, device_params->Mem_bandwidth, 1);
  mpq_set_si (Mem_LD, device_params->Mem_LD, 1);
  mpq_set_si (Departure_del_uncoal, device_params->Departure_del_uncoal, 1);
  mpq_set_si (Departure_del_coal, device_params->Departure_del_coal, 1);
  mpq_set_si (Active_SMs, device_params->Active_SMs, 1);
  mpq_set_si (Freq, device_params->Freq, 1000);
  mpq_set_si (Load_bytes_per_warp, device_params->Load_bytes_per_warp, 1);

  if (mpq_cmp_ui (Active_blocks_per_SM, 0, 1) == 0)
    {
      printf ("ERROR: active_blocks_per_sm==0!\n");
      exit (EXIT_FAILURE);
    }
#if VERBOSE >=3
  gmp_printf("issue_cycles = %Qd\n", Issue_cycles);
  gmp_printf("Mem_bandwidth = %Qd\n", Mem_bandwidth);
  gmp_printf("Mem_LD %Qd\n", Mem_LD);
  gmp_printf("Departure_del_uncoal %Qd\n", Departure_del_uncoal);
  gmp_printf("Departure_del_coal %Qd\n", Departure_del_coal);
  gmp_printf("Active_SMs %Qd\n", Active_SMs);
  gmp_printf("Freq %Qd\n", Freq);
  gmp_printf("Load_bytes_per_warp %Qd\n", Load_bytes_per_warp);
  printf("===========================\n");
#endif
  //printf("Error check done\n");

  // double Threads_per_block = thread_per_block;
  // double Blocks = n_block;
  // double Active_warps_per_block = Threads_per_block / 32;
  // double Active_warps_per_SM = Active_warps_per_block * Active_blocks_per_SM;

  mpq_t Threads_per_block;
  mpq_t Blocks;
  mpq_t Active_warps_per_block;
  mpq_t Active_warps_per_SM;

  mpq_init (Threads_per_block);
  mpq_init (Blocks);
  mpq_init (Active_warps_per_block);
  mpq_init (Active_warps_per_SM);

  mpq_set_si (Threads_per_block, thread_per_block, 1);
  //gmp_printf("Threads_per_block is : %Qd\n",Threads_per_block);
  mpq_set_si (Blocks, n_block, 1);
  //gmp_printf("Blocks is : %Qd\n",Blocks);
  mpq_t const_temp;
  mpq_init (const_temp);
  mpq_set_si (const_temp, 32, 1);

  // this is not correct, the number of warps is computed as ceiling div
  // ceil(div(n_threads_per_block/32));

  int n_warps_per_block = (thread_per_block + 31) / 32;

  //mpq_div(Active_warps_per_block, Threads_per_block, const_temp);
  mpq_set_si (Active_warps_per_block, n_warps_per_block, 1);
  mpq_mul (Active_warps_per_SM, Active_warps_per_block, Active_blocks_per_SM);

  mpq_set_si (const_temp, 1, 1);
  if (mpq_cmp (Active_warps_per_block, const_temp) < 0)
    {
      printf ("Active warps per block is less than 1!\n");
      exit (EXIT_FAILURE);
    }

#if VERBOSE >=3
  gmp_printf("Threads_per_block is : %Qd\n",Threads_per_block);
  gmp_printf("Blocks is : %Qd\n",Blocks);
  gmp_printf("Active_warps_per_block is : %Qd\n",Active_warps_per_block);
  gmp_printf("Active_warps_per_SM is : %Qd\n",Active_warps_per_SM);
#endif

  //double N = Active_warps_per_SM;
  //double Mem_L_Uncoal = Mem_LD + (Uncoal_per_mw - 1) * Departure_del_uncoal;
  mpq_t Mem_L_Uncoal;
  mpq_init (Mem_L_Uncoal);

  mpq_sub (Uncoal_per_mw, Uncoal_per_mw, const_temp);
  mpq_mul (Mem_L_Uncoal, Departure_del_uncoal, Uncoal_per_mw);
  mpq_add (Mem_L_Uncoal, Mem_LD, Mem_L_Uncoal);
  mpq_add (Uncoal_per_mw, Uncoal_per_mw, const_temp);

  // if Uncoal_per_mw is 0, then set to 1
    if (mpq_cmp_ui (Uncoal_per_mw, 0, 1) == 0) {
        mpq_set_si (Uncoal_per_mw, 1, 1);
    }

  //double Mem_L_Coal = Mem_LD;

  //double Weight_uncoal = Uncoal_Mem_insts
  //		/ (Uncoal_Mem_insts + Coal_Mem_insts);

  mpq_t Weight_uncoal;
  mpq_init (Weight_uncoal);
  mpq_add (Weight_uncoal, Uncoal_Mem_insts, Coal_Mem_insts);
  mpq_div (Weight_uncoal, Uncoal_Mem_insts, Weight_uncoal);

  //double Weight_coal = Coal_Mem_insts / (Uncoal_Mem_insts + Coal_Mem_insts);

  mpq_t Weight_coal;
  mpq_init (Weight_coal);
  mpq_add (Weight_coal, Uncoal_Mem_insts, Coal_Mem_insts);
  mpq_div (Weight_coal, Coal_Mem_insts, Weight_coal);

#if VERBOSE >=3
  gmp_printf("Weight_uncoal is : %Qd\n",Weight_uncoal);
#endif

  //double Mem_L = Mem_L_Coal * Weight_coal + Mem_L_Uncoal * Weight_uncoal;
  mpq_t Mem_L;
  mpq_init (Mem_L);
  mpq_t temp;
  mpq_init (temp);
  mpq_mul (Mem_L, Mem_LD, Weight_coal);
  mpq_mul (temp, Mem_L_Uncoal, Weight_uncoal);
  mpq_add (Mem_L, Mem_L, temp);


#if VERBOSE >=3
  gmp_printf("Mem_L is : %Qd\n",Mem_L);
#endif

  //double Departure_delay = Uncoal_per_mw * Weight_uncoal
  //		* Departure_del_uncoal + Weight_coal * Departure_del_coal;
  mpq_t Departure_delay;
  mpq_init (Departure_delay);
  mpq_mul (temp, Uncoal_per_mw, Weight_uncoal);
  mpq_mul (temp, temp, Departure_del_uncoal);
  mpq_mul (Departure_delay, Weight_coal, Departure_del_coal);
  mpq_add (Departure_delay, temp, Departure_delay);

#if VERBOSE >=3
  gmp_printf("Departure_delay is : %Qd\n",Departure_delay);
#endif

  //double MWP_Without_BW_full = Mem_L / Departure_delay;
  mpq_t MWP_Without_BW_full;
  mpq_init (MWP_Without_BW_full);
  mpq_div (MWP_Without_BW_full, Mem_L, Departure_delay);
  mpq_ceil (MWP_Without_BW_full);

  //double MWP_Without_BW = min2(MWP_Without_BW_full, Active_warps_per_SM);
  mpq_t MWP_Without_BW;
  mpq_init (MWP_Without_BW);
  mpq_ceil (MWP_Without_BW);
  min2 (MWP_Without_BW, MWP_Without_BW_full, Active_warps_per_SM);

  //double BW_per_warp = Freq * Load_bytes_per_warp / Mem_L;
  mpq_t BW_per_warp;
  mpq_init (BW_per_warp);
  mpq_div (BW_per_warp, Load_bytes_per_warp, Mem_L);
  mpq_mul (BW_per_warp, BW_per_warp, Freq);

#if VERBOSE >=3
  gmp_printf("BW_per_warp is : %Qd\n",BW_per_warp);
#endif

  //double MWP_peak_BW = Mem_bandwidth / (BW_per_warp * Active_SMs);
  mpq_t MWP_peak_BW;
  mpq_init (MWP_peak_BW);
  mpq_mul (MWP_peak_BW, BW_per_warp, Active_SMs);
  mpq_div (MWP_peak_BW, Mem_bandwidth, MWP_peak_BW);

#if VERBOSE >=3
  gmp_printf("MWP_peak_BW is : %Qd\n",MWP_peak_BW);
#endif

  //double MWP = min3(MWP_Without_BW, MWP_peak_BW, Active_warps_per_SM);
  mpq_t MWP;
  mpq_init (MWP);
  min3 (MWP, MWP_Without_BW, MWP_peak_BW, Active_warps_per_SM);

#if VERBOSE >=3
  gmp_printf("MWP is : %Qd\n",MWP);
#endif

  //double Mem_cycles = Coal_Mem_insts * Mem_L_Coal
  //		+ Mem_L_Uncoal * Uncoal_Mem_insts;
  mpq_t Mem_cycles;
  mpq_init (Mem_cycles);
  mpq_mul (temp, Coal_Mem_insts, Mem_LD);
  mpq_mul (Mem_cycles, Mem_L_Uncoal, Uncoal_Mem_insts);
  mpq_add (Mem_cycles, Mem_cycles, temp);

#if VERBOSE >=3
  gmp_printf("Mem_cycles is : %Qd\n",Mem_cycles);
#endif

//	double total_insts = Comp_insts + Uncoal_Mem_insts + Coal_Mem_insts;
  mpq_t total_insts;
  mpq_init (total_insts);
  mpq_add (total_insts, Comp_insts, Uncoal_Mem_insts);
  mpq_add (total_insts, total_insts, Coal_Mem_insts);
  

//	double Comp_cycles = Issue_cycles * total_insts;
  mpq_t Comp_cycles;
  mpq_init (Comp_cycles);
  mpq_mul (Comp_cycles, Issue_cycles, total_insts);

#if VERBOSE >=3
  gmp_printf("Comp_cycles is : %Qd\n",Comp_cycles);
#endif

//	double CWP_full = (Mem_cycles + Comp_cycles) / Comp_cycles;
  mpq_t CWP_full;
  mpq_init (CWP_full);
  mpq_add (CWP_full, Mem_cycles, Comp_cycles);
  mpq_div (CWP_full, CWP_full, Comp_cycles);
  mpq_ceil (CWP_full);

  //double CWP = min2(CWP_full, Active_warps_per_SM);
  mpq_t CWP;
  mpq_init (CWP);
  min2 (CWP, CWP_full, Active_warps_per_SM);
#if VERBOSE >=3
  gmp_printf("CWP is : %Qd\n",CWP);
#endif
//	double Mem_insts = Uncoal_Mem_insts + Coal_Mem_insts;
  mpq_t Mem_insts;
  mpq_init (Mem_insts);
  mpq_add (Mem_insts, Uncoal_Mem_insts, Coal_Mem_insts);

  //double Total_insts = Mem_insts + Comp_insts;
  mpq_t Total_insts;
  mpq_init (Total_insts);
  mpq_add (Total_insts, Mem_insts, Comp_insts);

//	double Rep = Blocks / (Active_blocks_per_SM * Active_SMs);
  mpq_t Rep;

  mpq_init (Rep);
  mpq_mul (Rep, Active_blocks_per_SM, Active_SMs);
  mpq_div (Rep, Blocks, Rep);
  mpq_ceil (Rep);

#if VERBOSE >=2
  gmp_printf("Rep is : %Qd\n",Rep);
#endif
//  mpz_t numer;
//  mpz_t denom;
//  mpz_init (numer);
//  mpz_init (denom);
//  mpz_set (numer, mpq_numref(Rep));
//  mpz_set (denom, mpq_denref(Rep));
//  mpz_cdiv_q (numer, numer, denom);
//  mpq_set_z (Rep, numer);
//  mpq_set(rep, Rep);
//  mpz_clear (numer);
//  mpz_clear (denom);

  mpq_set (mwp, MWP);
  mpq_set (cwp, CWP);
  //res[0] = MWP;
  //res[1] = CWP;
  //    printf("MWP: %f\n",MWP);
  //    printf("CWP: %f\n",CWP);

//	double Exec_cycles_app;
  mpq_t Exec_cycles_app;
  mpq_init (Exec_cycles_app);

  if (mpq_equal (MWP, Active_warps_per_SM) != 0
      && mpq_equal (CWP, Active_warps_per_SM) != 0)
    {
      //        printf("Case:(MWP==N) && (CWP== N)\n");
      //	Exec_cycles_app = (Mem_cycles + Comp_cycles
      //			+ Comp_cycles * (MWP - 1) / Mem_insts) * Rep;
      *mcwp_case = 1;
      //	printf("CASE 1!\n");
      mpq_sub (temp, MWP, const_temp);
      mpq_mul (Exec_cycles_app, Comp_cycles, temp);
      mpq_div (Exec_cycles_app, Exec_cycles_app, Mem_insts);
      mpq_add (Exec_cycles_app, Exec_cycles_app, Mem_cycles);
      mpq_add (Exec_cycles_app, Exec_cycles_app, Comp_cycles);
      mpq_mul (Exec_cycles_app, Exec_cycles_app, Rep);
    }
  else if (mpq_cmp (CWP, MWP) >= 0 || mpq_cmp (Comp_cycles, Mem_cycles) > 0)
    {
      //        printf("Case:(CWP > MWP) || (Comp_cycles > Mem_cycles)\n");
      //	Exec_cycles_app = (Mem_cycles * N / MWP
      //			+ Comp_cycles * (MWP - 1) / Mem_insts) * Rep;
      *mcwp_case = 2;
      //printf("CASE 2!\n");
      mpq_sub (temp, MWP, const_temp);
      mpq_mul (Exec_cycles_app, Comp_cycles, temp);
      mpq_div (Exec_cycles_app, Exec_cycles_app, Mem_insts);

      mpq_mul (temp, Mem_cycles, Active_warps_per_SM);
      mpq_div (temp, temp, MWP);
      mpq_add (Exec_cycles_app, Exec_cycles_app, temp);
      mpq_mul (Exec_cycles_app, Exec_cycles_app, Rep);
    }
  else
    {
      //        printf("Case:else\n");
//		Exec_cycles_app = (Active_warps_per_SM * Comp_cycles + Mem_L) * Rep;
      *mcwp_case = 3;
      //printf("CASE 3!\n");
      mpq_mul (Exec_cycles_app, Active_warps_per_SM, Comp_cycles);
      mpq_add (Exec_cycles_app, Exec_cycles_app, Mem_L);
      mpq_mul (Exec_cycles_app, Exec_cycles_app, Rep);
    }

#if VERBOSE >=3
  gmp_printf("Exec_cycles_app is : %Qd\n",Exec_cycles_app);
#endif
  //    printf("Exec_cycles_app: %f\n",Exec_cycles_app);
  //double NpWB = min2(MWP, Active_warps_per_block);
  mpq_t NpWB;
  mpq_init (NpWB);
  min2 (NpWB, MWP, Active_warps_per_block);

#if VERBOSE >=3
  gmp_printf("NpWB %Qd\n",NpWB);
#endif

  //double Synch_cost = Departure_delay * (NpWB - 1) * Synch_insts
  //		* Active_blocks_per_SM * Rep;
  mpq_t Synch_cost;
  mpq_init (Synch_cost);
  mpq_sub (Synch_cost, NpWB, const_temp);

  mpq_mul (Synch_cost, Synch_cost, Departure_delay);

  mpq_mul (Synch_cost, Synch_cost, Synch_insts);
  mpq_mul (Synch_cost, Synch_cost, Active_blocks_per_SM);

  mpq_mul (Synch_cost, Synch_cost, Rep);

#if VERBOSE >=3
  gmp_printf("Synch_cost %Qd\n",Synch_cost);
#endif
  //double Exec_cycles_with_synch = Exec_cycles_app + Synch_cost;
  //    printf("Exec_cycles_with_synch: %f\n",Exec_cycles_with_synch);
  mpq_t Exec_cycles_with_synch;
  mpq_init (Exec_cycles_with_synch);

#if VERBOSE >=3
  gmp_printf("Exec_cycles_with_synch %Qd\n",Exec_cycles_with_synch);
#endif

  mpq_add (Exec_cycles_with_synch, Exec_cycles_app, Synch_cost);
  mpq_set (clockcycles, Exec_cycles_with_synch);
#if VERBOSE >=3
  gmp_printf("mcwp done\n");
#endif
  /////////////////////////////////////
  mpq_clear (Comp_insts);
  mpq_clear (Uncoal_Mem_insts);
  mpq_clear (Coal_Mem_insts);
  mpq_clear (Synch_insts);
  mpq_clear (Coal_per_mw);
  mpq_clear (Uncoal_per_mw);
  mpq_clear (Active_blocks_per_SM);
  mpq_clear (Issue_cycles);
  mpq_clear (Mem_bandwidth);
  mpq_clear (Mem_LD);
  mpq_clear (Departure_del_uncoal);
  mpq_clear (Departure_del_coal);
  mpq_clear (Active_SMs);
  mpq_clear (Freq);
  mpq_clear (Load_bytes_per_warp);

  mpq_clear (Threads_per_block);
  mpq_clear (Blocks);
  mpq_clear (Active_warps_per_block);
  mpq_clear (Active_warps_per_SM);

  mpq_clear (const_temp);

  mpq_clear (Mem_L_Uncoal);

  mpq_clear (Weight_uncoal);
  mpq_clear (Weight_coal);
  mpq_clear (Mem_L);
  mpq_clear (temp);
  mpq_clear (Departure_delay);
  mpq_clear (MWP_Without_BW_full);
  mpq_clear (MWP_Without_BW);

  mpq_clear (BW_per_warp);
  mpq_clear (MWP_peak_BW);
  mpq_clear (MWP);
  mpq_clear (Mem_cycles);
  mpq_clear (total_insts);
  mpq_clear (Comp_cycles);
  mpq_clear (CWP_full);
  mpq_clear (CWP);
  mpq_clear (Mem_insts);
  mpq_clear (Total_insts);
  mpq_clear (Rep);
  mpq_clear (Exec_cycles_app);
  mpq_clear (NpWB);
  mpq_clear (Synch_cost);
  mpq_clear (Exec_cycles_with_synch);

  /////////////////////////////////////

}

///////////////////////////////////////

int
get_sm_arch_from_str (char * sm_arch)
{
  int len = strlen (sm_arch);
  char tmp[3];
  for (int i = 0; i < 2; i++)
    tmp[i] = sm_arch[i + 3];
  tmp[2] = '\0';

  int sm_arch_val = atoi (tmp);
  return sm_arch_val;
}

///////////////////////////////////////

bool in(triple arr[], int size, int b0, int b1) {
  for (int i = 0; i < size; i++) {
    if (arr[i].N[1] == b0 && arr[i].N[2] == b1) {
      return true;
    }
  }
  return false;
}

///////////////////////////////////////

bool read_outliers(char * kernel_name, int n, int b0, int b1) {

    FILE *fp;
    char filename[100];
    sprintf(filename, "kernel_%s_outliers_%s.txt", kernel_name, "geforcertx2070super");
    fp = fopen(filename, "r");

    fseek(fp, 0, SEEK_END);
    int size = ftell(fp);

    int outliers_n, outliers_b0, outliers_b1; 
    double exec_time;
    triple outliers[size];

    rewind(fp);

    int i = 0;

    while (fscanf(fp, "%d %d %d %lf", &outliers_n, &outliers_b0, &outliers_b1, &exec_time) != EOF) {
        outliers[i].N[0] = outliers_n;
        outliers[i].N[1] = outliers_b0;
        outliers[i].N[2] = outliers_b1;
        i++;
    }

    fclose(fp);
    
    return in(outliers, size, b0, b1);

}

void
get_best_config (int n, char * kernel_name, int n_var, char* sm_arch,
		 int n_register, device_params_t* device_params, int verbose)
{

  int arch_cc = get_sm_arch_from_str (sm_arch);
  //  printf ("[looking for arch=sm_%d]\n", arch);
  int arch_index = -1;
  int sm_list[13] =
    { 20, 21, 30, 32, 35, 37, 50, 52, 53, 60, 61, 62, 75 };
  for (int i = 0; i <= 13; i++)
    {
      if (sm_list[i] == arch_cc)
	{
	  arch_index = i;
	  break;
	}
    }
  if (arch_index == -1)
    {
      printf ("Invalid SM: %s\n", sm_arch);
      exit (EXIT_FAILURE);
    }

  int max_registers_per_block_list[13] =
    { 32768, 32768, 65536, 65536, 65536, 65536, 65536, 65536, 32768, 65536,
	65536, 65536, 65536 };
  int max_blocks_per_sm_list[13] =
    { 8, 8, 16, 16, 16, 16, 32, 32, 32, 32, 32, 32, 32 };
  int max_register_per_thread_list[13] =
    { 63, 63, 63, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 };
  int max_warps_per_sm_list[13] =
    { 48, 48, 64, 64, 64, 64, 64, 64, 64, 64, 64, 128, 64 };
  int max_shared_mem_per_block = 49152;
  int max_block_size = 1024;

  int arch_params[5] =
    { max_blocks_per_sm_list[arch_index], max_warps_per_sm_list[arch_index],
	max_block_size, max_registers_per_block_list[arch_index],
	max_shared_mem_per_block };

  AltArr_t* Comp_insts_numer = get_numer_Comp_insts ();
  AltArr_t* Comp_insts_denom = get_denom_Comp_insts ();

  AltArr_t* Uncoal_Mem_insts_numer = get_numer_Uncoal_Mem_insts ();
  AltArr_t* Uncoal_Mem_insts_denom = get_denom_Uncoal_Mem_insts ();

  AltArr_t* Coal_Mem_insts_numer = get_numer_Coal_Mem_insts ();
  AltArr_t* Coal_Mem_insts_denom = get_denom_Coal_Mem_insts ();

  AltArr_t* Synch_insts_numer = get_numer_Synch_insts ();
  AltArr_t* Synch_insts_denom = get_denom_Synch_insts ();

  AltArr_t* Coal_per_mw_numer = get_numer_Coal_per_mw ();
  AltArr_t* Coal_per_mw_denom = get_denom_Coal_per_mw ();

  AltArr_t* Uncoal_per_mw_numer = get_numer_Uncoal_per_mw ();
  AltArr_t* Uncoal_per_mw_denom = get_denom_Uncoal_per_mw ();

  AltArr_t* Mem_ld_numer = get_numer_Mem_LD ();
  AltArr_t* Mem_ld_denom = get_denom_Mem_LD ();

  AltArr_t* b_numer = get_numer_Blocks ();
  AltArr_t* b_denom = get_denom_Blocks ();

  AltArr_t* s_numer = get_numer_shared_mem_bytes_total ();
  AltArr_t* s_denom = get_denom_shared_mem_bytes_total ();

//  AltArr_t* Active_blocks_per_SM_numer = get_numer_Active_blocks_per_SM ();
//    AltArr_t* Active_blocks_per_SM_denom  = get_denom_Active_blocks_per_SM ();

  float best_occ = 0;
  int best_b0_occ = 0, best_b1_occ = 0;
  int best_b0_ec = 0, best_b1_ec = 0;
  int b0 = 1, b1 = 1;

  int b0_list[51] =
    { 0 };
  int b1_list[51] =
    { 0 };
  double best_ec[51];

  mpq_t mwp, cwp, clockcycles, best_ec_tmp;
  mpq_init (mwp);
  mpq_init (cwp);
  mpq_init (clockcycles);
  mpq_init (best_ec_tmp);

  int active_sm = device_params->Active_SMs;
//  eval_constraints ()
  int k = 0;

  for (unsigned int i = log2(MIN_BLOCK_SIZE); i < log2(MAX_BLOCK_SIZE)+1; i++)
    for (unsigned int t = 0; t < i+1; t++)
      {
        int j = i-t;
        b0 = 1 << t;
        b1 = 1 << j;
    if ((b0 > n) || (b1 > n))
      continue;    
	if ((b0 * b1 > MAX_BLOCK_SIZE) || (b0 * b1 < MIN_BLOCK_SIZE))
	  continue;
	if (eval_constraints (kernel_name, n, b0, b1) != 0)
	  continue;
    if (read_outliers(kernel_name, n, b0, b1) != 0)
      continue;
	if (verbose)
	  {
	    printf ("[checking b0=%d, b1=%d]\n", b0, b1);
	  }
	int b_actual = eval_functions (n, b0, b1, n_var, b_numer, b_denom);

//	printf ("b_actual=%d\n", b_actual);
//	b_actual = (b_actual + active_sm - 1)/ active_sm;
//
//	continue;
//	int b_actual = 32;

	int z = eval_functions (n, b0, b1, n_var, s_numer, s_denom);
	if (z == 1)
	  z = 0;
	int program_params[7] =
	  { 0, 0, 0, 0, 0, 0, 0 };

	program_params[0] = eval_functions (n, b0, b1, n_var, Comp_insts_numer,
					    Comp_insts_denom);

	program_params[1] = eval_functions (n, b0, b1, n_var,
					    Uncoal_Mem_insts_numer,
					    Uncoal_Mem_insts_denom);

	/*program_params[2] = eval_functions (n, b0, b1, n_var,
					    Coal_Mem_insts_numer,
					    Coal_Mem_insts_denom);*/

	/*program_params[3] = eval_functions (n, b0, b1, n_var, Synch_insts_numer,
					    Synch_insts_denom);*/

	/*program_params[4] = eval_functions (n, b0, b1, n_var, Coal_per_mw_numer,
					    Coal_per_mw_denom);*/

	program_params[5] = eval_functions (n, b0, b1, n_var,
					    Uncoal_per_mw_numer,
					    Uncoal_per_mw_denom);
	program_params[6] = get_b_actual (
	    n, b0, b1, n_register, arch_params, n_var,
	    (b_actual + active_sm - 1) / active_sm, z);
//	program_params[6] = eval_functions (n, b0, b1, n_var,
	//                                    Active_blocks_per_SM_numer,
	//                                  Active_blocks_per_SM_denom);

	/*device_params->Mem_LD = eval_functions (n, b0, b1, n_var, Mem_ld_numer,
						Mem_ld_denom);*/
//	printf("Mem_ld: %d\n",device_params->Mem_LD);

	int skip_this_iteration = 0;
	for (int i = 0; i < 7; i++)
	  {
	    if (program_params[i] < 0)
	      {
		skip_this_iteration = 1;
		break;
	      }
	  }
	if (skip_this_iteration)
	  {
	    if (verbose)
	      {
		printf ("SKIP ...\n");
		printf ("------------------------------------\n");
	      }
	    continue;
	  }

//	if (program_params[0] < 0)
//	  continue;
//	if (program_params[1] < 0)
//	  continue;
//	if (program_params[2] < 0)
//	  continue;
//	if (program_params[3] < 0)
//	  continue;
//	if (program_params[4] < 0)
//	  continue;
//	if (program_params[5] < 0)
//	  continue;
//	if (program_params[6] < 0)
//	  continue;

//	if (program_params[1]==0 && program_params[2]==0)
//		program_params[2]=1;
//
//	if (program_params[4]==0 && program_params[5]==0)
//		program_params[5]=1;

//	for(int i=0;i<7;i++)
//	{
//		printf("param[%d]=%d\n", i, program_params[i]);
//	}
	float occ = get_occupancy (b0, b1, arch_params, program_params[6]);

	int mcwp_case = 0;
	get_mwp_cwp (&mcwp_case, mwp, cwp, clockcycles, program_params,
		     device_params, b0 * b1, b_actual);
//	get_mwp_cwp (int* mcwp_case, mpq_t mwp, mpq_t cwp, mpq_t clockcycles,
//		     int* program_params, device_params_t* device_params,
//		     int thread_per_block, int n_block)

	if (verbose)
	  {
	    printf ("[N=%d, b0=%d, b1=%d, n_blocks=%d, z=%d, Occupancy=%.2f,"
		    " Ec=%.2f, MWP=%0.0f, CWP=%.0f, mcwp_case=%d]"
		    "\n------------------------------------\n",
		    n, b0, b1, b_actual, z, occ, mpq_get_d (clockcycles),
		    mpq_get_d (mwp),
		    mpq_get_d (cwp), 
        mcwp_case);
	  }

	if (mpq_cmp_ui (clockcycles, 0, 1) == 0)
	  {
	    if (verbose)
	      {
		printf ("SKIP ...\n");
		printf ("------------------------------------\n");
	      }
	    continue;
	  }
//	if ((mpq_cmp (clockcycles, best_ec) < 0)
//	    || (mpq_cmp_ui (best_ec, 0, 1) == 0))
//	  {
//	    mpq_set (best_ec, clockcycles);
//	    best_b0_ec = b0;
//	    best_b1_ec = b1;
//	  }
	b0_list[k] = b0;
	b1_list[k] = b1;
	best_ec[k] = mpq_get_d (clockcycles);
	k++;
	if (occ > best_occ)
	  {
	    best_occ = occ;
	    best_b0_occ = b0;
	    best_b1_occ = b1;
	  }
      }

  ///////////////////////////////////////////
  // writing best occupancy result to file //
  ///////////////////////////////////////////

//  char best_occ_dump_path[1024];
//  sprintf (best_occ_dump_path, "best_occupancy_kernel_%s_%d.tmp", kernel_name,
//	   n);
//  FILE * best_occ_dump_file = fopen (best_occ_dump_path, "w");
//
//#if VERBOSE_RP
//  printf ("[Best occupancy for N=%d is found at (B0=%4d, B1=%4d) = %.2f]"
//	  "\n------------------------------------\n",
//	  n, best_b0_occ, best_b1_occ, best_occ);
//#endif
//
//  fprintf (best_occ_dump_file, "%d,%d,%.2f", best_b0_occ, best_b1_occ,
//	   best_occ);
//  fclose (best_occ_dump_file);

  ///////////////////////////////////////////

  for (int i = 0; i < k - 1; i++)
    {
      // Last i elements are already in place
      for (int j = 0; j < k - i - 1; j++)
	{
	  if (best_ec[j] > best_ec[j + 1])
	    {
	      double tmp = best_ec[j];
	      best_ec[j] = best_ec[j + 1];
	      best_ec[j + 1] = tmp;

	      int tmp0 = b0_list[j];
	      b0_list[j] = b0_list[j + 1];
	      b0_list[j + 1] = tmp0;

	      int tmp1 = b1_list[j];
	      b1_list[j] = b1_list[j + 1];
	      b1_list[j + 1] = tmp1;
	    }
	}
    }

  if (verbose)
    {
      for (int i = 0; i < 5; i++)
	{
	  //  printf("[NO. %d Ec for N = %d is at (B0 = %4d, B1 = %4d) = %.2f]\n",
	  //		i + 1,n,b0_list[i],b1_list[i],best_ec[i]);

	  printf ("[BestEC NO.=%d , N = %d, B0 = %d, B1 = %d, ec = %.2f]\n\n",
		  i + 1, n, b0_list[i], b1_list[i], best_ec[i]);
	}
      printf ("\n------------------------------------\n");
    }

//  printf ("[Best Ec for N=%d is found at (B0=%d, B1=%d) = %.2f]"
//	  "\n------------------------------------\n",
//	  n, best_b0_ec, best_b1_ec, mpq_get_d (best_ec));

  ///////////////////////////////////////////
  // writing best occupancy result to file //
  ///////////////////////////////////////////
//  char best_ec_dump_path[1024];
//  sprintf (best_ec_dump_path, "best_ec_kernel_%s_%d.tmp", kernel_name, n);
//  FILE * best_ec_dump_file = fopen (best_ec_dump_path, "w");

  FILE * best_ec_dump_file = stdout;
	
	fprintf (best_ec_dump_file, "[best_ec][idx, B0, B1]\n");
  for (int i = 0; i < 5; i++)
    fprintf (best_ec_dump_file, "%d, %d, %d\n", i, b0_list[i], b1_list[i]);

// fclose (best_ec_dump_file);
  ///////////////////////////////////////////
  // writing best occupancy result to file //
  ///////////////////////////////////////////
  mpq_clear (mwp);
  mpq_clear (cwp);
  mpq_clear (clockcycles);
  mpq_clear (best_ec_tmp);
}

///////////////////////////////////////
///////////////////////////////////////

int
main (int argc, char **argv)
{
  if (argc < 4)
    {
      printf (
	  "Usage: ./occupancy.bin [1]:N [2]:SM_ARCH (sm_CC) [3]:DEVICE_PARAMS_PATH [4]:VERBOSE (default=0)\n");
      exit (EXIT_FAILURE);
    }
  char sm_arch[8];
  char kernel_name[64];

  char klaraptor_path[1024];
  sprintf (klaraptor_path, "%s", getenv ("KLARAPTOR_PATH"));
  char device_params_path[1024];
  sprintf (device_params_path, "%s/device_profiles/geforcertx2070super.specs",
	   klaraptor_path);
  sprintf (device_params_path, "%s", argv[3]);

  device_params_t device_params;
  read_params_from_file (device_params_path, &device_params, N_DEVICE_PARAMS,
			 global_device_params_list);

  // print Mem_LD
  // printf("Mem_LD = %d\n", device_params.Mem_LD);
      
//	 device_params.Mem_LD = read_mem_ld (n, kernel_type, b0 * b1,
//							  device_name);

  

  int n = atoi (argv[1]);
  sprintf (kernel_name, "KERNEL_NAME");
//  int n_var = atoi ("KERNEL_N_VAR");
  int n_var = 3;
  sprintf (sm_arch, "%s", argv[2]);

  int n_registers = 0;
  char register_file_path[1024];
//  printf("%s, %s",kernel_name, sm_arch);
  sprintf (register_file_path, "kernel_%s_%s.registers", kernel_name, sm_arch);
//  printf("reg_path=[%s]\n",register_file_path);
  FILE * register_file = fopen (register_file_path, "r");
  fscanf (register_file, "%d", &n_registers);
  fclose (register_file);
//  printf("n_reg=%d", n_registers);

  int verbose = 0;
  if (argc > 4)
    verbose = atoi (argv[4]);
  get_best_config (n, kernel_name, n_var, sm_arch, n_registers, &device_params,
		   verbose);
//  get_best_clock_cycles (n, kernel_name, n_var, sm_arch, n_registers);
}

///////////////////////////////////////
