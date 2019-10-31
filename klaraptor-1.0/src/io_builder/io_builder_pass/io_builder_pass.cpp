/*!
 \file io_builder_pass.cpp
 \author Davood Mohajerani <dmohajer@uwo.ca>
 \author Linxiao Wang <lwang739@uwo.ca>
 \brief
 */

#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/IR/Constants.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/IR/CallSite.h"
#include <vector>
#include <regex>
//c++ ABI: used for demangling function name.
#include <cxxabi.h>

using namespace llvm;

#ifndef VERBOSE_PASS
#define VERBOSE_PASS 0
#endif

///////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////

namespace {
///////////////////////////////////////////////////////////
typedef std::pair<bool, std::string> demangled_name_t;
///////////////////////////////////////////////////////////
const std::string kernel_invoker_signature =
		"kernel_invoker(char*, int*, void**)";
///////////////////////////////////////////////////////////
const std::string init_kernel_info_dict_signature =
		"init_kernel_info_dict(char*, int)";
///////////////////////////////////////////////////////////
const std::string add_to_kernel_info_dict_signature =
		"add_to_kernel_info_dict(char*, char*, int)";
///////////////////////////////////////////////////////////
const std::string get_best_config_signature =
		"get_best_config(char const*, int*, void const**)";
///////////////////////////////////////////////////////////
const std::string init_cu_driver_call_signature = "init_cu_driver_call()";
///////////////////////////////////////////////////////////
struct func_insert: public ModulePass {
	static char ID;

	func_insert() :
			ModulePass(ID) {
	}
	virtual bool runOnModule(Module &M);

	/////////////////////////////////////////////
	//// add all the utility functions here.
	/////////////////////////////////////////////
	demangled_name_t demangle_name(StringRef mangled_name);
	demangled_name_t demangle_name(std::string mangled_name);

	ConstantData *get_value_of_global_string(std::string global_string_name,
			Function &f);
	Value * store_string_in_global_variable(std::string s,
			std::string global_var_name, Instruction *I);

	std::string get_mangled_function_name(std::string plain_func_name,
			Module &M);

	/////////////////////////////////////////////
	std::map<std::string, int> kernel_size_idx_map;
	std::map<std::string, int> kernel_dim_map;
	void print_maps_in_connector();

	bool extract_kernel_info_constants_done = false;
	bool extract_kernel_info_constants(Module &M);

	bool create_maps_in_connector_done = false;
	bool create_maps_in_connector(Module & I);

	/////////////////////////////////////////////
	bool insert_RP_connector(Instruction & I);

	std::string sp(int n) {

		std::string t = "";
		for (int i = 0; i < n; i++)
			t += " ";
		return t;
	}
};
}

///////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////

demangled_name_t func_insert::demangle_name(std::string mangled_name) {
	int status = -1;
	char *res;
	res = abi::__cxa_demangle(mangled_name.c_str(), NULL, NULL, &status);
	bool return_stat = false;
	std::string value = "";
	if (status == 0) {
		return_stat = true;
		value = std::string(res);
	}
	free(res);
//	if (return_stat)
//		errs() << "result=[" << value << "]\n";

	return demangled_name_t(return_stat, value);

}

///////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////

demangled_name_t func_insert::demangle_name(StringRef name) {
	return demangle_name(name.str());
}

///////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////

bool func_insert::runOnModule(Module& M) {
	int verbose = VERBOSE_PASS;
	if (verbose)
		errs() << "[A module visited] ... \n";

	//lookup global variables; extract the index
	//add function call to create the idx values
	create_maps_in_connector(M);

	for (auto &F : M) {
		demangled_name_t fd = demangle_name(F.getName());
		if (fd.first && (fd.second == kernel_invoker_signature)) {
			//insert connector before all calls to kernel_invoker.
			for (User*u : F.users()) {
				if (CallInst *I = dyn_cast < CallInst > (u)) {
					if (verbose)
						errs() << "[a callinst to kernel_invoker]..."
								"[inserting connector to RP]\n";
					Instruction &op(*I);
					insert_RP_connector(op);
				}
			}
		}
	}
	return false;
}

///////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////

//ToDo: fix the return value in the case of failure.
ConstantData* func_insert::get_value_of_global_string(
		std::string global_string_name, Function &F) {
	int verbose = VERBOSE_PASS;
	for (auto & g : F.getParent()->globals()) {
		if (g.getName() == global_string_name) {
			if (verbose)
				errs() << "[kernel name string] = "
						"[" << g.getName() << "]\n";
			return dyn_cast < ConstantData > (g.getInitializer());
		}
	}
	return NULL;
}

///////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////

//TODO: proper messages upon error or success.
Value * func_insert::store_string_in_global_variable(std::string s,
		std::string global_var_name, Instruction *I) {
//	errs()<<"storing in global variable "<<s<<"\n";
	BasicBlock &BB = (*I->getParent());
	Function &F = *(BB.getParent());
	auto * op = (I);

	LLVMContext &ctx = F.getContext();
	Type *u8_type = Type::getInt8Ty(ctx);
	Type *u8ptr_type = Type::getInt8PtrTy(ctx);
	Type *u32_type = Type::getInt32Ty(ctx);
	Type *u32ptr_type = Type::getInt32PtrTy(ctx);

	IRBuilder<> builder(op);
	SmallVector<Value *, 4> assertArgs;

	Value *kn_value = builder.CreateGlobalString(StringRef(s), global_var_name);
	ConstantData*x0 = get_value_of_global_string(global_var_name, F);

	//		ConstantInt* idx = ConstantInt::get(IntegerType::get(ctx, 32), 0);
	Type* x0_type = x0->getType();
	Type* x0_ptr_type = PointerType::get(x0->getType(), 0);
	AllocaInst * kn_ptr = builder.CreateAlloca(x0_type);
	builder.CreateStore(x0, kn_ptr, false);
	return kn_ptr;
}

///////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////

std::string func_insert::get_mangled_function_name(std::string func_name,
		Module & M) {
	int verbose = VERBOSE_PASS;
	;
	std::string mangled_name;

	for (auto &F : M) {
//		errs() << F.getName() << "\n";
		demangled_name_t dn = demangle_name(F.getName());
		if (dn.first) {
//			errs()<<"FOUND MANGLED NAME FOR "<<dn.second<<"]\n";
			if (dn.second == func_name) {
				if (verbose)
					errs() << "FOUND MANGLED NAME FOR [" << func_name
							<< "] ==[ " << F.getName() << "]\n";
				mangled_name = F.getName().str();
				return mangled_name;
			}
		}
	}

	errs() << "[FATAL ERROR: @get_mangled_function_name: "
			"cannot find mangled name!]...[EXIT IMMEDIATELY]\n";
	exit (EXIT_FAILURE);
}

///////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////

bool func_insert::create_maps_in_connector(Module & M) {
	int verbose = VERBOSE_PASS;
	;
	this->extract_kernel_info_constants(M);
	if (verbose)
		this->print_maps_in_connector();

	Instruction *I;
	//find the first instruction in main().
	int first_inst_found = false;
	for (auto &F : M) {
		if (F.getName() == "main") {
			for (auto &BB : F) {
				for (auto &Inst : BB) {
					I = &Inst;
					first_inst_found = true;
					break;
				}
				break;
			}
		}
	}

	if (first_inst_found == false) {
		errs() << "[FATAL ERROR: could not found the first instruction!]\n";
		exit (EXIT_FAILURE);
	}

	//check.
	if (this->create_maps_in_connector_done == true) {
		errs()
				<< "[create_maps_in_connector is already done! (declared as true)]\n...";
		return false;
	}

	BasicBlock &BB = (*I->getParent());
	Function &F = *(BB.getParent());
	auto * op = (I);

	LLVMContext &ctx = F.getContext();
	Type *u8_type = Type::getInt8Ty(ctx);
	Type *u8ptr_type = Type::getInt8PtrTy(ctx);
	Type *u32_type = Type::getInt32Ty(ctx);
	Type *u32ptr_type = Type::getInt32PtrTy(ctx);

	IRBuilder<> builder(op);
	SmallVector<Value *, 4> assertArgs;

	if (verbose)
		errs() << "[creating kernel_idx_map in connector] ... \n";

	//the function for initializing the dictionary.
	std::string mangled_func_name;
	mangled_func_name = get_mangled_function_name(
			init_kernel_info_dict_signature, M);

	Constant* init_kernel_info_dict_func = F.getParent()->getOrInsertFunction(
			mangled_func_name, Type::getVoidTy(ctx), u8ptr_type, u32_type);

	//the function for adding a new entry to dictionary.
	mangled_func_name = get_mangled_function_name(
			add_to_kernel_info_dict_signature, M);

	Constant* add_to_kernel_info_dict_func = F.getParent()->getOrInsertFunction(
			mangled_func_name, Type::getVoidTy(ctx), u8ptr_type, u8ptr_type,
			u32_type);

	//setting the insert point.
	BasicBlock::iterator current_point = builder.GetInsertPoint();
	builder.SetInsertPoint(&BB, current_point);

	//size of maps that store constants in the LLVM pass.
	ConstantInt * sizeof_kernel_size_idx_map = ConstantInt::get(ctx,
			APInt(32, kernel_size_idx_map.size()));
	ConstantInt * sizeof_kernel_dim_map = ConstantInt::get(ctx,
			APInt(32, kernel_dim_map.size()));

	///////////////////////////////////
	///////////////////////////////////
	{
		//form the name of the dictionary for size_param_idx_map.
		Value * dict_name1 = store_string_in_global_variable(
				("global_kernel_size_param_idx_map"),
				"global_kernel_size_param_idx_dict_name", I);
		Value * dict_name1_i8ptr = builder.CreateBitCast(dict_name1,
				u8ptr_type);

		assertArgs.clear();
		assertArgs.push_back(dict_name1_i8ptr);
		assertArgs.push_back(sizeof_kernel_size_idx_map);
		builder.CreateCall(init_kernel_info_dict_func, assertArgs);
		current_point++;

		int current_val_idx = 0;
		for (auto m : kernel_size_idx_map) {
			std::string current_kernel_global_str_name = (std::string(
					"global_kernel_info_param_size_idx_str_"))
					+ std::to_string(current_val_idx);
			Value * kn_ptr = store_string_in_global_variable(m.first,
					current_kernel_global_str_name, I);
			assertArgs.clear();
			assertArgs.push_back(dict_name1_i8ptr); // dict_name
			assertArgs.push_back(builder.CreateBitCast(kn_ptr, u8ptr_type));
			Constant * tmp_val = ConstantInt::get(ctx, APInt(32, m.second));
			assertArgs.push_back(tmp_val);
			builder.CreateCall(add_to_kernel_info_dict_func, assertArgs);
			current_val_idx++;
			current_point++;
		}
	}
	///////////////////////////////////
	///////////////////////////////////
	{
		//form the name of the dictionary for size_param_idx_map.
		Value * dict_name2 = store_string_in_global_variable(
				("global_kernel_dim_map"), "global_kernel_dim_map_dict_name",
				I);
		Value * dict_name2_i8ptr = builder.CreateBitCast(dict_name2,
				u8ptr_type);

		assertArgs.clear();
		assertArgs.push_back(dict_name2_i8ptr);
		assertArgs.push_back(sizeof_kernel_dim_map);
		builder.CreateCall(init_kernel_info_dict_func, assertArgs);
		current_point++;

		int current_val_idx = 0;
		for (auto m : kernel_dim_map) {
			std::string current_kernel_global_str_name = (std::string(
					"global_kernel_info_dim_str_"))
					+ std::to_string(current_val_idx);
			Value * kn_ptr = store_string_in_global_variable(m.first,
					current_kernel_global_str_name, I);
			assertArgs.clear();
			assertArgs.push_back(dict_name2_i8ptr); // dict_name
			Constant * tmp_val = ConstantInt::get(ctx, APInt(32, m.second));
			assertArgs.push_back(builder.CreateBitCast(kn_ptr, u8ptr_type));
			assertArgs.push_back(tmp_val);
			builder.CreateCall(add_to_kernel_info_dict_func, assertArgs);
			current_val_idx++;
			current_point++;
		}
	}
	///////////////////////////////////
	///////////////////////////////////
	//the function for initializing modules, context, and device.
	mangled_func_name = get_mangled_function_name(init_cu_driver_call_signature,
			M);

	Constant* init_cu_driver_call_func = F.getParent()->getOrInsertFunction(
			mangled_func_name, u32_type);

	builder.CreateCall(init_cu_driver_call_func);
	current_point++;
	///////////////////////////////////
	///////////////////////////////////

	this->create_maps_in_connector_done = true;
	return true;
}

///////////////////////////////////////////////////////////

bool func_insert::insert_RP_connector(Instruction & I) {
	int verbose = VERBOSE_PASS;
	;
	if (verbose)
		errs() << "[simply inserting  RP_connector for the callinst]\n";

	BasicBlock &BB = (*I.getParent());
	Function &F = *(BB.getParent());
	Module &M = *(F.getParent());
	auto * op = dyn_cast < CallInst > (&I);

	LLVMContext &ctx = F.getContext();
	Type *void_type = Type::getVoidTy(ctx);
	Type *u8_type = Type::getInt8Ty(ctx);
	Type *u8ptr_type = Type::getInt8PtrTy(ctx);
	Type *u32_type = Type::getInt32Ty(ctx);
	Type *u32ptr_type = Type::getInt32PtrTy(ctx);

//	void get_best_config_ptr_all_info(char *kernel_name, void* kernel_params, int *launch_params)
	std::string mangled_func_name = get_mangled_function_name(
			get_best_config_signature, M);

	Constant * get_best_config_func = F.getParent()->getOrInsertFunction(
			mangled_func_name, void_type, u8ptr_type, u32ptr_type,
			PointerType::get(u8ptr_type, 0));

	IRBuilder<> builder(op);
	//setting the instruction insert point.
	BasicBlock::iterator current_point = builder.GetInsertPoint();
	SmallVector<Value *, 4> assertArgs;

	assertArgs.clear();

	int n_arg_operands = op->getNumArgOperands();
	if (n_arg_operands != 3) {
		errs() << "[number of arg operands does not match "
				"[get_best_config_ptr_all_info_func]"
				"]\n";
		return false;
	}

	for (int i = 0; i < n_arg_operands; i++) {
		assertArgs.push_back(op->getOperand(i));
	}

	//call to get_best_config_ptr which will set the values
	builder.CreateCall(get_best_config_func, assertArgs);

	return true;

//	AllocaInst *v0 = builder.CreateAlloca(Type::getInt32Ty(ctx));
//	AllocaInst *v1 = builder.CreateAlloca(Type::getInt32Ty(ctx));
//
//	ConstantInt * const_zero = ConstantInt::get(ctx, APInt(32, 0));
//
//	return true;
//	Value * N_value; //call_get_kernel_size_idx_from_map_func;
//
//	StoreInst * s0 = builder.CreateStore(N_value, v0, false);
//	StoreInst * s1 = builder.CreateStore(const_zero, v1, false);
//	return true;
////	setting the insert point right before the current instruction.
//
////								current_point++;
////	builder.SetInsertPoint(&BB, current_point);
//
//	assertArgs.clear();
//	assertArgs.push_back(v0);
//	assertArgs.push_back(v1);
//	return true;
//
//	CallInst * fcall = builder.CreateCall(get_best_config_ptr_all_info_func,
//			assertArgs);
//	if (fcall->getNumArgOperands() == 2)
//	{
////		 errs() << "changing I operands\n";
//		/////////////////////////////////////////
//		/////////////////////////////////////////
//		for (int i = 0; i < 3; i++)
//		{
//			ConstantInt* idx = ConstantInt::get(IntegerType::get(ctx, 32), i);
//			Value *gxptr = builder.CreateGEP(u32_type, fcall->getOperand(1),
//					idx);
//			Value * new_value = builder.CreateLoad(u32_type, gxptr);
//			I.setOperand(i + 1, new_value);
//		}
//		/////////////////////////////////////////
//		/////////////////////////////////////////
//	}
//	return true;
}

///////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////

//ToDo: add regex checking for pattern that matches kernel name.
bool func_insert::extract_kernel_info_constants(Module&M) {
	int verbose = VERBOSE_PASS;
	;

	if (extract_kernel_info_constants_done == true) {
		errs() << "[extract_kernel_info_constants_done =True]...[ignore]\n";
		return false;
	}
	if (verbose) {
		errs() << "--------------------------------------\n";
		errs() << "[Extracting global kernel info]... \n";
	}
	LLVMContext & ctx = M.getContext();
	//reference to the map which stores <kernel_name, idx> doubles.
	std::map<std::string, int> & kernel_size_idx_map = this->kernel_size_idx_map;
	std::map<std::string, int> & kernel_dim_map = this->kernel_dim_map;

	Type* u32_ptr = Type::getInt32PtrTy(ctx);
	Type* u64_ptr = Type::getInt64PtrTy(ctx);

	std::string pattern1 = "kernel_info_size_param_idx_.*";
	std::regex re1(pattern1, std::regex::ECMAScript);

	std::string pattern2 = "kernel_info_dim_.*";
	std::regex re2(pattern2, std::regex::ECMAScript);

	for (auto &global_var : M.globals()) {

//				if (in.hasName())
		bool valid_constant = false;
		//get type of the global variable.

//		GlobalVariable &global_var;
		Type* var_type = global_var.getType();

//		errs()<<"Type= ";
//		var_type->
//		errs()<<"\n";
//		continue;

		//proceed only if the type is int*32 or int*64
		int idx;
		std::string name;

		if ((var_type == u32_ptr) || (var_type == u64_ptr)) {
//			errs()<<"a new one++;\n";
			//ToDo: regex matching needed here to get the kernel_size_param_idx --> Fixed.
			//currently, assume that all constant global variables are kernel_param_idx;
			if (auto x0 = dyn_cast < GlobalVariable > (&global_var)) {
				if (auto x1 = dyn_cast < ConstantData
						> (x0->getInitializer())) {
					auto x2 = dyn_cast < ConstantInt > (x1);
					idx = x2->getSExtValue();
//					if (x0->hasName())
					{
						demangled_name_t dn = demangle_name(x0->getName());
						if (dn.first) {
//							errs() << "NAME in extract -> [" << dn.second
//									<< "]\n";
							name = (dn.second);
//							errs()<<"name = "<<name<<"\n";
							valid_constant = true;
						}
					}
				}
			}
		}
//		continue;
		if (valid_constant == false)
			continue;

		std::string search_str = name;
		std::smatch match_value;

		//adding parameter to kernel_size_idx_map.
		if (std::regex_search(search_str, match_value, re1)) {
			kernel_size_idx_map.insert(std::pair<std::string, int>(name, idx));
			if (verbose) {
				std::string result = match_value[0];
				errs() << "[regex_matched]"
						"[adding [" << match_value[0] << "] "
						"to kernel_size_idx_map "
						"[size=" << kernel_size_idx_map.size() << "]]\n";

				errs() << "-----------------------------------\n";
			}
		}

		//adding parameter to kernel_dim_map.
		if (std::regex_search(search_str, match_value, re2)) {
			kernel_dim_map.insert(std::pair<std::string, int>(name, idx));
			//search_str = m1.suffix().str();
			if (verbose) {
				std::string result = match_value[0];
				errs() << "[regex_matched]"
						"[adding [" << match_value[0] << "] "
						"to kernel_dim_map "
						"[size=" << kernel_dim_map.size() << "]]\n";

				errs() << "-----------------------------------\n";
			}
		}
	}
	errs() << "--------------------------------------\n";
	extract_kernel_info_constants_done = true;
	return true;
}

///////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////

void func_insert::print_maps_in_connector() {
	errs() << "--------------------------------------\n";
	errs() << "[print_kernel_size_idx_map]\n";
	for (auto it : this->kernel_size_idx_map) {
		errs() << "-----------------------\n";
		errs() << sp(4) << "[" << it.first << "] : [" << it.second << "]\n";
	}
	errs() << "--------------------------------------\n";
	errs() << "[print_kernel_dim_map]\n";
	for (auto it : this->kernel_dim_map) {
		errs() << "-----------------------\n";
		errs() << sp(4) << "[" << it.first << "] : [" << it.second << "]\n";
	}
	errs() << "--------------------------------------\n";
}

///////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////

char func_insert::ID = 0;
static RegisterPass<func_insert> X("io_builder_pass", "test function exist",
		false, false);

///////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////
