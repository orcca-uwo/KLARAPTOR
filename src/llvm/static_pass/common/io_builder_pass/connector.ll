; ModuleID = 'connector.cpp'
source_filename = "connector.cpp"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.kernel_info_dictionary_s = type { i32, [256 x i8], [64 x i8], i32*, i8**, i32, i32 }

@kernel_info_dictionary_default = dso_local global %struct.kernel_info_dictionary_s zeroinitializer, align 8
@.str = private unnamed_addr constant [33 x i8] c"global_kernel_size_param_idx_map\00", align 1
@.str.1 = private unnamed_addr constant [22 x i8] c"global_kernel_dim_map\00", align 1
@.str.2 = private unnamed_addr constant [95 x i8] c"[@%s][FATAL ERROR: could not get pointer to kernel info dictionary[%s]]...[EXIT IMMEDIATELY!]\0A\00", align 1
@__func__._Z27get_ptr_to_kernel_info_dictPc = private unnamed_addr constant [28 x i8] c"get_ptr_to_kernel_info_dict\00", align 1
@.str.3 = private unnamed_addr constant [62 x i8] c"[@%s][initializing kernel info dictionary [%s] of size=[%d]]\0A\00", align 1
@__func__._Z21init_kernel_info_dictPci = private unnamed_addr constant [22 x i8] c"init_kernel_info_dict\00", align 1
@.str.4 = private unnamed_addr constant [53 x i8] c"[kernel_idx_map is already initialized!]...ignoring\0A\00", align 1
@.str.5 = private unnamed_addr constant [38 x i8] c"[dict_name=%s][dict_param_prefix=%s]\0A\00", align 1
@.str.6 = private unnamed_addr constant [1 x i8] zeroinitializer, align 1
@.str.7 = private unnamed_addr constant [66 x i8] c"[accessing [%s] failed as it is not initialized yet!]...ignoring\0A\00", align 1
@.str.8 = private unnamed_addr constant [61 x i8] c"[accessing [%s] failed as it has no more space!]...ignoring\0A\00", align 1
@.str.9 = private unnamed_addr constant [64 x i8] c"[add_to_kernel_idx_map][idx=%d][kernel_param_str=%s][value=%d]\0A\00", align 1
@.str.10 = private unnamed_addr constant [52 x i8] c"[INIT CUDA DRIVER CALL CONTEXTS, MODULES, etc!]...\0A\00", align 1
@.str.11 = private unnamed_addr constant [6 x i8] c"%s_%s\00", align 1
@.str.12 = private unnamed_addr constant [24 x i8] c"[@%s][getting [%s=%d]]\0A\00", align 1
@__func__._Z31get_value_from_kernel_info_dictPcS_Pi = private unnamed_addr constant [32 x i8] c"get_value_from_kernel_info_dict\00", align 1
@.str.13 = private unnamed_addr constant [3 x i8] c"%c\00", align 1
@.str.14 = private unnamed_addr constant [3 x i8] c"%s\00", align 1
@.str.15 = private unnamed_addr constant [32 x i8] c"[kernel_name] = [%s] --> [%s] \0A\00", align 1
@.str.16 = private unnamed_addr constant [89 x i8] c"[@get_best_config][ERROR: Failed to get the global_kernel_param !]...[EXIT IMMEDIATELY]\0A\00", align 1
@.str.17 = private unnamed_addr constant [87 x i8] c"[@get_best_config][ERROR: Failed to get the global_kernel_dim !]...[EXIT IMMEDIATELY]\0A\00", align 1
@.str.18 = private unnamed_addr constant [48 x i8] c"----------------------------------------------\0A\00", align 1
@.str.19 = private unnamed_addr constant [38 x i8] c"    [@connector][kernel_size_idx=%d]\0A\00", align 1
@.str.20 = private unnamed_addr constant [33 x i8] c"    [@connector][kernel_dim=%d]\0A\00", align 1
@.str.21 = private unnamed_addr constant [72 x i8] c"    [@connector][passing input value N=[%d] for kernel [%s] to RP]... \0A\00", align 1
@__const._Z15get_best_configPKcPiPPKv.result_str_format = private unnamed_addr constant <{ [47 x i8], [17 x i8] }> <{ [47 x i8] c"    [@connector][RP result]: best_config: %s%s\0A", [17 x i8] zeroinitializer }>, align 16
@__const._Z15get_best_configPKcPiPPKv.result_griddim_format = private unnamed_addr constant <{ [21 x i8], [43 x i8] }> <{ [21 x i8] c"[gx=%d, gy=%d, gz=%d]", [43 x i8] zeroinitializer }>, align 16
@__const._Z15get_best_configPKcPiPPKv.result_blockdim_format = private unnamed_addr constant <{ [21 x i8], [43 x i8] }> <{ [21 x i8] c"[bx=%d, by=%d, bz=%d]", [43 x i8] zeroinitializer }>, align 16
@_ZL32global_kernel_size_param_idx_map = internal global { i32, <{ [32 x i8], [224 x i8] }>, <{ [26 x i8], [38 x i8] }>, i32*, i8**, i32, i32 } { i32 0, <{ [32 x i8], [224 x i8] }> <{ [32 x i8] c"global_kernel_size_param_idx_map", [224 x i8] zeroinitializer }>, <{ [26 x i8], [38 x i8] }> <{ [26 x i8] c"kernel_info_size_param_idx", [38 x i8] zeroinitializer }>, i32* null, i8** null, i32 0, i32 0 }, align 8
@_ZL21global_kernel_dim_map = internal global { i32, <{ [21 x i8], [235 x i8] }>, <{ [15 x i8], [49 x i8] }>, i32*, i8**, i32, i32 } { i32 0, <{ [21 x i8], [235 x i8] }> <{ [21 x i8] c"global_kernel_dim_map", [235 x i8] zeroinitializer }>, <{ [15 x i8], [49 x i8] }> <{ [15 x i8] c"kernel_info_dim", [49 x i8] zeroinitializer }>, i32* null, i8** null, i32 0, i32 0 }, align 8

; Function Attrs: noinline optnone uwtable mustprogress
define dso_local %struct.kernel_info_dictionary_s* @_Z27get_ptr_to_kernel_info_dictPc(i8* %0) #0 {
  %2 = alloca %struct.kernel_info_dictionary_s*, align 8
  %3 = alloca i8*, align 8
  store i8* %0, i8** %3, align 8
  %4 = load i8*, i8** %3, align 8
  %5 = call i32 @strcmp(i8* %4, i8* getelementptr inbounds ([33 x i8], [33 x i8]* @.str, i64 0, i64 0)) #7
  %6 = icmp eq i32 %5, 0
  br i1 %6, label %7, label %8

7:                                                ; preds = %1
  store %struct.kernel_info_dictionary_s* bitcast ({ i32, <{ [32 x i8], [224 x i8] }>, <{ [26 x i8], [38 x i8] }>, i32*, i8**, i32, i32 }* @_ZL32global_kernel_size_param_idx_map to %struct.kernel_info_dictionary_s*), %struct.kernel_info_dictionary_s** %2, align 8
  br label %16

8:                                                ; preds = %1
  %9 = load i8*, i8** %3, align 8
  %10 = call i32 @strcmp(i8* %9, i8* getelementptr inbounds ([22 x i8], [22 x i8]* @.str.1, i64 0, i64 0)) #7
  %11 = icmp eq i32 %10, 0
  br i1 %11, label %12, label %13

12:                                               ; preds = %8
  store %struct.kernel_info_dictionary_s* bitcast ({ i32, <{ [21 x i8], [235 x i8] }>, <{ [15 x i8], [49 x i8] }>, i32*, i8**, i32, i32 }* @_ZL21global_kernel_dim_map to %struct.kernel_info_dictionary_s*), %struct.kernel_info_dictionary_s** %2, align 8
  br label %16

13:                                               ; preds = %8
  %14 = load i8*, i8** %3, align 8
  %15 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([95 x i8], [95 x i8]* @.str.2, i64 0, i64 0), i8* getelementptr inbounds ([28 x i8], [28 x i8]* @__func__._Z27get_ptr_to_kernel_info_dictPc, i64 0, i64 0), i8* %14)
  call void @exit(i32 1) #8
  unreachable

16:                                               ; preds = %12, %7
  %17 = load %struct.kernel_info_dictionary_s*, %struct.kernel_info_dictionary_s** %2, align 8
  ret %struct.kernel_info_dictionary_s* %17
}

; Function Attrs: nounwind readonly willreturn
declare dso_local i32 @strcmp(i8*, i8*) #1

declare dso_local i32 @printf(i8*, ...) #2

; Function Attrs: noreturn nounwind
declare dso_local void @exit(i32) #3

; Function Attrs: noinline optnone uwtable mustprogress
define dso_local void @_Z21init_kernel_info_dictPci(i8* %0, i32 %1) #0 {
  %3 = alloca i8*, align 8
  %4 = alloca i32, align 4
  %5 = alloca %struct.kernel_info_dictionary_s*, align 8
  %6 = alloca i32, align 4
  store i8* %0, i8** %3, align 8
  store i32 %1, i32* %4, align 4
  %7 = load i8*, i8** %3, align 8
  %8 = load i32, i32* %4, align 4
  %9 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([62 x i8], [62 x i8]* @.str.3, i64 0, i64 0), i8* getelementptr inbounds ([22 x i8], [22 x i8]* @__func__._Z21init_kernel_info_dictPci, i64 0, i64 0), i8* %7, i32 %8)
  %10 = load i8*, i8** %3, align 8
  %11 = call %struct.kernel_info_dictionary_s* @_Z27get_ptr_to_kernel_info_dictPc(i8* %10)
  store %struct.kernel_info_dictionary_s* %11, %struct.kernel_info_dictionary_s** %5, align 8
  %12 = load %struct.kernel_info_dictionary_s*, %struct.kernel_info_dictionary_s** %5, align 8
  %13 = getelementptr inbounds %struct.kernel_info_dictionary_s, %struct.kernel_info_dictionary_s* %12, i32 0, i32 0
  %14 = load i32, i32* %13, align 8
  %15 = icmp eq i32 %14, 1
  br i1 %15, label %16, label %18

16:                                               ; preds = %2
  %17 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([53 x i8], [53 x i8]* @.str.4, i64 0, i64 0))
  br label %76

18:                                               ; preds = %2
  %19 = load %struct.kernel_info_dictionary_s*, %struct.kernel_info_dictionary_s** %5, align 8
  %20 = getelementptr inbounds %struct.kernel_info_dictionary_s, %struct.kernel_info_dictionary_s* %19, i32 0, i32 1
  %21 = getelementptr inbounds [256 x i8], [256 x i8]* %20, i64 0, i64 0
  %22 = load %struct.kernel_info_dictionary_s*, %struct.kernel_info_dictionary_s** %5, align 8
  %23 = getelementptr inbounds %struct.kernel_info_dictionary_s, %struct.kernel_info_dictionary_s* %22, i32 0, i32 2
  %24 = getelementptr inbounds [64 x i8], [64 x i8]* %23, i64 0, i64 0
  %25 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([38 x i8], [38 x i8]* @.str.5, i64 0, i64 0), i8* %21, i8* %24)
  %26 = load i32, i32* %4, align 4
  %27 = load %struct.kernel_info_dictionary_s*, %struct.kernel_info_dictionary_s** %5, align 8
  %28 = getelementptr inbounds %struct.kernel_info_dictionary_s, %struct.kernel_info_dictionary_s* %27, i32 0, i32 5
  store i32 %26, i32* %28, align 8
  %29 = load i32, i32* %4, align 4
  %30 = sext i32 %29 to i64
  %31 = mul i64 %30, 8
  %32 = call noalias i8* @malloc(i64 %31) #9
  %33 = bitcast i8* %32 to i8**
  %34 = load %struct.kernel_info_dictionary_s*, %struct.kernel_info_dictionary_s** %5, align 8
  %35 = getelementptr inbounds %struct.kernel_info_dictionary_s, %struct.kernel_info_dictionary_s* %34, i32 0, i32 4
  store i8** %33, i8*** %35, align 8
  %36 = load i32, i32* %4, align 4
  %37 = sext i32 %36 to i64
  %38 = mul i64 %37, 4
  %39 = call noalias i8* @malloc(i64 %38) #9
  %40 = bitcast i8* %39 to i32*
  %41 = load %struct.kernel_info_dictionary_s*, %struct.kernel_info_dictionary_s** %5, align 8
  %42 = getelementptr inbounds %struct.kernel_info_dictionary_s, %struct.kernel_info_dictionary_s* %41, i32 0, i32 3
  store i32* %40, i32** %42, align 8
  %43 = load %struct.kernel_info_dictionary_s*, %struct.kernel_info_dictionary_s** %5, align 8
  %44 = getelementptr inbounds %struct.kernel_info_dictionary_s, %struct.kernel_info_dictionary_s* %43, i32 0, i32 0
  store i32 1, i32* %44, align 8
  %45 = load %struct.kernel_info_dictionary_s*, %struct.kernel_info_dictionary_s** %5, align 8
  %46 = getelementptr inbounds %struct.kernel_info_dictionary_s, %struct.kernel_info_dictionary_s* %45, i32 0, i32 6
  store i32 0, i32* %46, align 4
  store i32 0, i32* %6, align 4
  br label %47

47:                                               ; preds = %73, %18
  %48 = load i32, i32* %6, align 4
  %49 = load i32, i32* %4, align 4
  %50 = icmp slt i32 %48, %49
  br i1 %50, label %51, label %76

51:                                               ; preds = %47
  %52 = load %struct.kernel_info_dictionary_s*, %struct.kernel_info_dictionary_s** %5, align 8
  %53 = getelementptr inbounds %struct.kernel_info_dictionary_s, %struct.kernel_info_dictionary_s* %52, i32 0, i32 3
  %54 = load i32*, i32** %53, align 8
  %55 = load i32, i32* %6, align 4
  %56 = sext i32 %55 to i64
  %57 = getelementptr inbounds i32, i32* %54, i64 %56
  store i32 -1, i32* %57, align 4
  %58 = call noalias i8* @malloc(i64 1024) #9
  %59 = load %struct.kernel_info_dictionary_s*, %struct.kernel_info_dictionary_s** %5, align 8
  %60 = getelementptr inbounds %struct.kernel_info_dictionary_s, %struct.kernel_info_dictionary_s* %59, i32 0, i32 4
  %61 = load i8**, i8*** %60, align 8
  %62 = load i32, i32* %6, align 4
  %63 = sext i32 %62 to i64
  %64 = getelementptr inbounds i8*, i8** %61, i64 %63
  store i8* %58, i8** %64, align 8
  %65 = load %struct.kernel_info_dictionary_s*, %struct.kernel_info_dictionary_s** %5, align 8
  %66 = getelementptr inbounds %struct.kernel_info_dictionary_s, %struct.kernel_info_dictionary_s* %65, i32 0, i32 4
  %67 = load i8**, i8*** %66, align 8
  %68 = load i32, i32* %6, align 4
  %69 = sext i32 %68 to i64
  %70 = getelementptr inbounds i8*, i8** %67, i64 %69
  %71 = load i8*, i8** %70, align 8
  %72 = call i8* @strcpy(i8* %71, i8* getelementptr inbounds ([1 x i8], [1 x i8]* @.str.6, i64 0, i64 0)) #9
  br label %73

73:                                               ; preds = %51
  %74 = load i32, i32* %6, align 4
  %75 = add nsw i32 %74, 1
  store i32 %75, i32* %6, align 4
  br label %47, !llvm.loop !2

76:                                               ; preds = %16, %47
  ret void
}

; Function Attrs: nounwind
declare dso_local noalias i8* @malloc(i64) #4

; Function Attrs: nounwind
declare dso_local i8* @strcpy(i8*, i8*) #4

; Function Attrs: noinline optnone uwtable mustprogress
define dso_local void @_Z23add_to_kernel_info_dictPcS_i(i8* %0, i8* %1, i32 %2) #0 {
  %4 = alloca i8*, align 8
  %5 = alloca i8*, align 8
  %6 = alloca i32, align 4
  %7 = alloca i32, align 4
  %8 = alloca %struct.kernel_info_dictionary_s*, align 8
  %9 = alloca i32, align 4
  store i8* %0, i8** %4, align 8
  store i8* %1, i8** %5, align 8
  store i32 %2, i32* %6, align 4
  store i32 0, i32* %7, align 4
  %10 = load i8*, i8** %4, align 8
  %11 = call %struct.kernel_info_dictionary_s* @_Z27get_ptr_to_kernel_info_dictPc(i8* %10)
  store %struct.kernel_info_dictionary_s* %11, %struct.kernel_info_dictionary_s** %8, align 8
  %12 = load %struct.kernel_info_dictionary_s*, %struct.kernel_info_dictionary_s** %8, align 8
  %13 = getelementptr inbounds %struct.kernel_info_dictionary_s, %struct.kernel_info_dictionary_s* %12, i32 0, i32 6
  %14 = load i32, i32* %13, align 4
  store i32 %14, i32* %9, align 4
  %15 = load %struct.kernel_info_dictionary_s*, %struct.kernel_info_dictionary_s** %8, align 8
  %16 = getelementptr inbounds %struct.kernel_info_dictionary_s, %struct.kernel_info_dictionary_s* %15, i32 0, i32 6
  %17 = load i32, i32* %16, align 4
  %18 = add nsw i32 %17, 1
  store i32 %18, i32* %16, align 4
  %19 = load %struct.kernel_info_dictionary_s*, %struct.kernel_info_dictionary_s** %8, align 8
  %20 = getelementptr inbounds %struct.kernel_info_dictionary_s, %struct.kernel_info_dictionary_s* %19, i32 0, i32 0
  %21 = load i32, i32* %20, align 8
  %22 = icmp ne i32 %21, 1
  br i1 %22, label %23, label %26

23:                                               ; preds = %3
  %24 = load i8*, i8** %4, align 8
  %25 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([66 x i8], [66 x i8]* @.str.7, i64 0, i64 0), i8* %24)
  br label %56

26:                                               ; preds = %3
  %27 = load i32, i32* %9, align 4
  %28 = load %struct.kernel_info_dictionary_s*, %struct.kernel_info_dictionary_s** %8, align 8
  %29 = getelementptr inbounds %struct.kernel_info_dictionary_s, %struct.kernel_info_dictionary_s* %28, i32 0, i32 5
  %30 = load i32, i32* %29, align 8
  %31 = icmp sge i32 %27, %30
  br i1 %31, label %32, label %35

32:                                               ; preds = %26
  %33 = load i8*, i8** %4, align 8
  %34 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([61 x i8], [61 x i8]* @.str.8, i64 0, i64 0), i8* %33)
  br label %56

35:                                               ; preds = %26
  %36 = load i32, i32* %6, align 4
  %37 = load %struct.kernel_info_dictionary_s*, %struct.kernel_info_dictionary_s** %8, align 8
  %38 = getelementptr inbounds %struct.kernel_info_dictionary_s, %struct.kernel_info_dictionary_s* %37, i32 0, i32 3
  %39 = load i32*, i32** %38, align 8
  %40 = load i32, i32* %9, align 4
  %41 = sext i32 %40 to i64
  %42 = getelementptr inbounds i32, i32* %39, i64 %41
  store i32 %36, i32* %42, align 4
  %43 = load %struct.kernel_info_dictionary_s*, %struct.kernel_info_dictionary_s** %8, align 8
  %44 = getelementptr inbounds %struct.kernel_info_dictionary_s, %struct.kernel_info_dictionary_s* %43, i32 0, i32 4
  %45 = load i8**, i8*** %44, align 8
  %46 = load i32, i32* %9, align 4
  %47 = sext i32 %46 to i64
  %48 = getelementptr inbounds i8*, i8** %45, i64 %47
  %49 = load i8*, i8** %48, align 8
  %50 = load i8*, i8** %5, align 8
  %51 = call i8* @strcpy(i8* %49, i8* %50) #9
  %52 = load i32, i32* %9, align 4
  %53 = load i8*, i8** %5, align 8
  %54 = load i32, i32* %6, align 4
  %55 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([64 x i8], [64 x i8]* @.str.9, i64 0, i64 0), i32 %52, i8* %53, i32 %54)
  br label %56

56:                                               ; preds = %35, %32, %23
  ret void
}

; Function Attrs: noinline optnone uwtable mustprogress
define dso_local i32 @_Z19init_cu_driver_callv() #0 {
  %1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([52 x i8], [52 x i8]* @.str.10, i64 0, i64 0))
  ret i32 0
}

; Function Attrs: noinline optnone uwtable mustprogress
define dso_local i32 @_Z31get_value_from_kernel_info_dictPcS_Pi(i8* %0, i8* %1, i32* %2) #0 {
  %4 = alloca i32, align 4
  %5 = alloca i8*, align 8
  %6 = alloca i8*, align 8
  %7 = alloca i32*, align 8
  %8 = alloca [1024 x i8], align 16
  %9 = alloca %struct.kernel_info_dictionary_s*, align 8
  %10 = alloca i32, align 4
  store i8* %0, i8** %5, align 8
  store i8* %1, i8** %6, align 8
  store i32* %2, i32** %7, align 8
  %11 = load i8*, i8** %5, align 8
  %12 = call %struct.kernel_info_dictionary_s* @_Z27get_ptr_to_kernel_info_dictPc(i8* %11)
  store %struct.kernel_info_dictionary_s* %12, %struct.kernel_info_dictionary_s** %9, align 8
  %13 = getelementptr inbounds [1024 x i8], [1024 x i8]* %8, i64 0, i64 0
  %14 = load %struct.kernel_info_dictionary_s*, %struct.kernel_info_dictionary_s** %9, align 8
  %15 = getelementptr inbounds %struct.kernel_info_dictionary_s, %struct.kernel_info_dictionary_s* %14, i32 0, i32 2
  %16 = getelementptr inbounds [64 x i8], [64 x i8]* %15, i64 0, i64 0
  %17 = load i8*, i8** %6, align 8
  %18 = call i32 (i8*, i8*, ...) @sprintf(i8* %13, i8* getelementptr inbounds ([6 x i8], [6 x i8]* @.str.11, i64 0, i64 0), i8* %16, i8* %17) #9
  store i32 0, i32* %10, align 4
  br label %19

19:                                               ; preds = %50, %3
  %20 = load i32, i32* %10, align 4
  %21 = load %struct.kernel_info_dictionary_s*, %struct.kernel_info_dictionary_s** %9, align 8
  %22 = getelementptr inbounds %struct.kernel_info_dictionary_s, %struct.kernel_info_dictionary_s* %21, i32 0, i32 6
  %23 = load i32, i32* %22, align 4
  %24 = icmp slt i32 %20, %23
  br i1 %24, label %25, label %53

25:                                               ; preds = %19
  %26 = load %struct.kernel_info_dictionary_s*, %struct.kernel_info_dictionary_s** %9, align 8
  %27 = getelementptr inbounds %struct.kernel_info_dictionary_s, %struct.kernel_info_dictionary_s* %26, i32 0, i32 4
  %28 = load i8**, i8*** %27, align 8
  %29 = load i32, i32* %10, align 4
  %30 = sext i32 %29 to i64
  %31 = getelementptr inbounds i8*, i8** %28, i64 %30
  %32 = load i8*, i8** %31, align 8
  %33 = getelementptr inbounds [1024 x i8], [1024 x i8]* %8, i64 0, i64 0
  %34 = call i32 @strcmp(i8* %32, i8* %33) #7
  %35 = icmp eq i32 %34, 0
  br i1 %35, label %36, label %49

36:                                               ; preds = %25
  %37 = load %struct.kernel_info_dictionary_s*, %struct.kernel_info_dictionary_s** %9, align 8
  %38 = getelementptr inbounds %struct.kernel_info_dictionary_s, %struct.kernel_info_dictionary_s* %37, i32 0, i32 3
  %39 = load i32*, i32** %38, align 8
  %40 = load i32, i32* %10, align 4
  %41 = sext i32 %40 to i64
  %42 = getelementptr inbounds i32, i32* %39, i64 %41
  %43 = load i32, i32* %42, align 4
  %44 = load i32*, i32** %7, align 8
  store i32 %43, i32* %44, align 4
  %45 = getelementptr inbounds [1024 x i8], [1024 x i8]* %8, i64 0, i64 0
  %46 = load i32*, i32** %7, align 8
  %47 = load i32, i32* %46, align 4
  %48 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([24 x i8], [24 x i8]* @.str.12, i64 0, i64 0), i8* getelementptr inbounds ([32 x i8], [32 x i8]* @__func__._Z31get_value_from_kernel_info_dictPcS_Pi, i64 0, i64 0), i8* %45, i32 %47)
  store i32 0, i32* %4, align 4
  br label %54

49:                                               ; preds = %25
  br label %50

50:                                               ; preds = %49
  %51 = load i32, i32* %10, align 4
  %52 = add nsw i32 %51, 1
  store i32 %52, i32* %10, align 4
  br label %19, !llvm.loop !4

53:                                               ; preds = %19
  store i32 1, i32* %4, align 4
  br label %54

54:                                               ; preds = %53, %36
  %55 = load i32, i32* %4, align 4
  ret i32 %55
}

; Function Attrs: nounwind
declare dso_local i32 @sprintf(i8*, i8*, ...) #4

; Function Attrs: noinline nounwind optnone uwtable mustprogress
define dso_local i32 @_Z17strip_kernel_namePc(i8* %0) #5 {
  %2 = alloca i8*, align 8
  %3 = alloca [1024 x i8], align 16
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  store i8* %0, i8** %2, align 8
  %7 = load i8*, i8** %2, align 8
  %8 = call i64 @strlen(i8* %7) #7
  %9 = trunc i64 %8 to i32
  store i32 %9, i32* %4, align 4
  store i32 0, i32* %5, align 4
  store i32 7, i32* %6, align 4
  br label %10

10:                                               ; preds = %29, %1
  %11 = load i32, i32* %6, align 4
  %12 = load i32, i32* %4, align 4
  %13 = sub nsw i32 %12, 6
  %14 = icmp slt i32 %11, %13
  br i1 %14, label %15, label %32

15:                                               ; preds = %10
  %16 = load i8*, i8** %2, align 8
  %17 = load i32, i32* %5, align 4
  %18 = sext i32 %17 to i64
  %19 = getelementptr inbounds i8, i8* %16, i64 %18
  %20 = load i8*, i8** %2, align 8
  %21 = load i32, i32* %6, align 4
  %22 = sext i32 %21 to i64
  %23 = getelementptr inbounds i8, i8* %20, i64 %22
  %24 = load i8, i8* %23, align 1
  %25 = sext i8 %24 to i32
  %26 = call i32 (i8*, i8*, ...) @sprintf(i8* %19, i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.13, i64 0, i64 0), i32 %25) #9
  %27 = load i32, i32* %5, align 4
  %28 = add nsw i32 %27, %26
  store i32 %28, i32* %5, align 4
  br label %29

29:                                               ; preds = %15
  %30 = load i32, i32* %6, align 4
  %31 = add nsw i32 %30, 1
  store i32 %31, i32* %6, align 4
  br label %10, !llvm.loop !5

32:                                               ; preds = %10
  ret i32 0
}

; Function Attrs: nounwind readonly willreturn
declare dso_local i64 @strlen(i8*) #1

; Function Attrs: noinline optnone uwtable mustprogress
define dso_local void @_Z15get_best_configPKcPiPPKv(i8* %0, i32* %1, i8** %2) #0 {
  %4 = alloca i8*, align 8
  %5 = alloca i32*, align 8
  %6 = alloca i8**, align 8
  %7 = alloca [1024 x i8], align 16
  %8 = alloca i32, align 4
  %9 = alloca i32, align 4
  %10 = alloca i32, align 4
  %11 = alloca i32, align 4
  %12 = alloca [64 x i8], align 16
  %13 = alloca [64 x i8], align 16
  %14 = alloca [64 x i8], align 16
  %15 = alloca [256 x i8], align 16
  %16 = alloca [256 x i8], align 16
  %17 = alloca [1024 x i8], align 16
  store i8* %0, i8** %4, align 8
  store i32* %1, i32** %5, align 8
  store i8** %2, i8*** %6, align 8
  %18 = getelementptr inbounds [1024 x i8], [1024 x i8]* %7, i64 0, i64 0
  %19 = load i8*, i8** %4, align 8
  %20 = call i32 (i8*, i8*, ...) @sprintf(i8* %18, i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.14, i64 0, i64 0), i8* %19) #9
  %21 = getelementptr inbounds [1024 x i8], [1024 x i8]* %7, i64 0, i64 0
  %22 = call i32 @_Z17strip_kernel_namePc(i8* %21)
  %23 = load i8*, i8** %4, align 8
  %24 = getelementptr inbounds [1024 x i8], [1024 x i8]* %7, i64 0, i64 0
  %25 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([32 x i8], [32 x i8]* @.str.15, i64 0, i64 0), i8* %23, i8* %24)
  %26 = getelementptr inbounds [1024 x i8], [1024 x i8]* %7, i64 0, i64 0
  %27 = call i32 @_Z31get_value_from_kernel_info_dictPcS_Pi(i8* getelementptr inbounds ([33 x i8], [33 x i8]* @.str, i64 0, i64 0), i8* %26, i32* %8)
  %28 = icmp ne i32 %27, 0
  br i1 %28, label %29, label %31

29:                                               ; preds = %3
  %30 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([89 x i8], [89 x i8]* @.str.16, i64 0, i64 0))
  call void @exit(i32 1) #8
  unreachable

31:                                               ; preds = %3
  %32 = getelementptr inbounds [1024 x i8], [1024 x i8]* %7, i64 0, i64 0
  %33 = call i32 @_Z31get_value_from_kernel_info_dictPcS_Pi(i8* getelementptr inbounds ([22 x i8], [22 x i8]* @.str.1, i64 0, i64 0), i8* %32, i32* %9)
  %34 = icmp ne i32 %33, 0
  br i1 %34, label %35, label %37

35:                                               ; preds = %31
  %36 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([87 x i8], [87 x i8]* @.str.17, i64 0, i64 0))
  call void @exit(i32 1) #8
  unreachable

37:                                               ; preds = %31
  store i32 -1, i32* %10, align 4
  %38 = load i8**, i8*** %6, align 8
  %39 = load i32, i32* %8, align 4
  %40 = sext i32 %39 to i64
  %41 = getelementptr inbounds i8*, i8** %38, i64 %40
  %42 = bitcast i8** %41 to i32**
  %43 = load i32*, i32** %42, align 8
  %44 = load i32, i32* %43, align 4
  store i32 %44, i32* %10, align 4
  %45 = load i32, i32* %10, align 4
  store i32 %45, i32* %11, align 4
  %46 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([48 x i8], [48 x i8]* @.str.18, i64 0, i64 0))
  %47 = load i32, i32* %8, align 4
  %48 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([38 x i8], [38 x i8]* @.str.19, i64 0, i64 0), i32 %47)
  %49 = load i32, i32* %9, align 4
  %50 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([33 x i8], [33 x i8]* @.str.20, i64 0, i64 0), i32 %49)
  %51 = load i32, i32* %11, align 4
  %52 = getelementptr inbounds [1024 x i8], [1024 x i8]* %7, i64 0, i64 0
  %53 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([72 x i8], [72 x i8]* @.str.21, i64 0, i64 0), i32 %51, i8* %52)
  %54 = getelementptr inbounds [1024 x i8], [1024 x i8]* %7, i64 0, i64 0
  %55 = load i32, i32* %11, align 4
  %56 = load i32*, i32** %5, align 8
  %57 = call i32 @_Z12rp_estimatorPciPi(i8* %54, i32 %55, i32* %56)
  %58 = bitcast [64 x i8]* %12 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 16 %58, i8* align 16 getelementptr inbounds (<{ [47 x i8], [17 x i8] }>, <{ [47 x i8], [17 x i8] }>* @__const._Z15get_best_configPKcPiPPKv.result_str_format, i32 0, i32 0, i32 0), i64 64, i1 false)
  %59 = bitcast [64 x i8]* %13 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 16 %59, i8* align 16 getelementptr inbounds (<{ [21 x i8], [43 x i8] }>, <{ [21 x i8], [43 x i8] }>* @__const._Z15get_best_configPKcPiPPKv.result_griddim_format, i32 0, i32 0, i32 0), i64 64, i1 false)
  %60 = bitcast [64 x i8]* %14 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 16 %60, i8* align 16 getelementptr inbounds (<{ [21 x i8], [43 x i8] }>, <{ [21 x i8], [43 x i8] }>* @__const._Z15get_best_configPKcPiPPKv.result_blockdim_format, i32 0, i32 0, i32 0), i64 64, i1 false)
  %61 = getelementptr inbounds [256 x i8], [256 x i8]* %15, i64 0, i64 0
  %62 = getelementptr inbounds [64 x i8], [64 x i8]* %13, i64 0, i64 0
  %63 = load i32*, i32** %5, align 8
  %64 = getelementptr inbounds i32, i32* %63, i64 0
  %65 = load i32, i32* %64, align 4
  %66 = load i32*, i32** %5, align 8
  %67 = getelementptr inbounds i32, i32* %66, i64 1
  %68 = load i32, i32* %67, align 4
  %69 = load i32*, i32** %5, align 8
  %70 = getelementptr inbounds i32, i32* %69, i64 2
  %71 = load i32, i32* %70, align 4
  %72 = call i32 (i8*, i8*, ...) @sprintf(i8* %61, i8* %62, i32 %65, i32 %68, i32 %71) #9
  %73 = getelementptr inbounds [256 x i8], [256 x i8]* %16, i64 0, i64 0
  %74 = getelementptr inbounds [64 x i8], [64 x i8]* %14, i64 0, i64 0
  %75 = load i32*, i32** %5, align 8
  %76 = getelementptr inbounds i32, i32* %75, i64 3
  %77 = load i32, i32* %76, align 4
  %78 = load i32*, i32** %5, align 8
  %79 = getelementptr inbounds i32, i32* %78, i64 4
  %80 = load i32, i32* %79, align 4
  %81 = load i32*, i32** %5, align 8
  %82 = getelementptr inbounds i32, i32* %81, i64 5
  %83 = load i32, i32* %82, align 4
  %84 = call i32 (i8*, i8*, ...) @sprintf(i8* %73, i8* %74, i32 %77, i32 %80, i32 %83) #9
  %85 = getelementptr inbounds [1024 x i8], [1024 x i8]* %17, i64 0, i64 0
  %86 = getelementptr inbounds [64 x i8], [64 x i8]* %12, i64 0, i64 0
  %87 = getelementptr inbounds [256 x i8], [256 x i8]* %15, i64 0, i64 0
  %88 = getelementptr inbounds [256 x i8], [256 x i8]* %16, i64 0, i64 0
  %89 = call i32 (i8*, i8*, ...) @sprintf(i8* %85, i8* %86, i8* %87, i8* %88) #9
  %90 = getelementptr inbounds [1024 x i8], [1024 x i8]* %17, i64 0, i64 0
  %91 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.14, i64 0, i64 0), i8* %90)
  %92 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([48 x i8], [48 x i8]* @.str.18, i64 0, i64 0))
  ret void
}

declare dso_local i32 @_Z12rp_estimatorPciPi(i8*, i32, i32*) #2

; Function Attrs: argmemonly nofree nosync nounwind willreturn
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* noalias nocapture writeonly, i8* noalias nocapture readonly, i64, i1 immarg) #6

attributes #0 = { noinline optnone uwtable mustprogress "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readonly willreturn "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { noreturn nounwind "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = { nounwind "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #5 = { noinline nounwind optnone uwtable mustprogress "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #6 = { argmemonly nofree nosync nounwind willreturn }
attributes #7 = { nounwind readonly willreturn }
attributes #8 = { noreturn nounwind }
attributes #9 = { nounwind }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"Ubuntu clang version 12.0.1-++20211029101322+fed41342a82f-1~exp1~20211029221816.4"}
!2 = distinct !{!2, !3}
!3 = !{!"llvm.loop.mustprogress"}
!4 = distinct !{!4, !3}
!5 = distinct !{!5, !3}
