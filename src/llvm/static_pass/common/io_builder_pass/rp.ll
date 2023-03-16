; ModuleID = 'rp.cpp'
source_filename = "rp.cpp"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct._IO_FILE = type { i32, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, %struct._IO_marker*, %struct._IO_FILE*, i32, i32, i64, i16, i8, [1 x i8], i8*, i64, %struct._IO_codecvt*, %struct._IO_wide_data*, %struct._IO_FILE*, i8*, i64, i32, [20 x i8] }
%struct._IO_marker = type opaque
%struct._IO_codecvt = type opaque
%struct._IO_wide_data = type opaque

@.str = private unnamed_addr constant [2 x i8] c"r\00", align 1
@.str.1 = private unnamed_addr constant [40 x i8] c"[@rp][ERROR: Failed to run command]...\0A\00", align 1
@__const._Z22get_value_from_processPcPi.start_pattern = private unnamed_addr constant <{ [22 x i8], [42 x i8] }> <{ [22 x i8] c"[best_ec][idx, B0, B1]", [42 x i8] zeroinitializer }>, align 16
@.str.2 = private unnamed_addr constant [15 x i8] c"%d, %d, %d, %d\00", align 1
@.str.3 = private unnamed_addr constant [24 x i8] c"rational_program_%s.bin\00", align 1
@.str.4 = private unnamed_addr constant [25 x i8] c"best_ec_kernel_%s_%d.tmp\00", align 1
@.str.5 = private unnamed_addr constant [68 x i8] c"./%s %d sm_75 ../../src/device_profiles/geforcertx2070super.specs 1\00", align 1
@.str.6 = private unnamed_addr constant [40 x i8] c"[@RP][best][B0 = %d, B1 = %d, B2 = %d]\0A\00", align 1

; Function Attrs: noinline optnone uwtable mustprogress
define dso_local i32 @_Z22get_value_from_processPcPi(i8* %0, i32* %1) #0 {
  %3 = alloca i8*, align 8
  %4 = alloca i32*, align 8
  %5 = alloca %struct._IO_FILE*, align 8
  %6 = alloca [1024 x i8], align 16
  %7 = alloca [64 x i8], align 16
  %8 = alloca i32, align 4
  %9 = alloca i32, align 4
  %10 = alloca i32, align 4
  %11 = alloca i32, align 4
  store i8* %0, i8** %3, align 8
  store i32* %1, i32** %4, align 8
  %12 = load i8*, i8** %3, align 8
  %13 = call %struct._IO_FILE* @popen(i8* %12, i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str, i64 0, i64 0))
  store %struct._IO_FILE* %13, %struct._IO_FILE** %5, align 8
  %14 = load %struct._IO_FILE*, %struct._IO_FILE** %5, align 8
  %15 = icmp eq %struct._IO_FILE* %14, null
  br i1 %15, label %16, label %18

16:                                               ; preds = %2
  %17 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([40 x i8], [40 x i8]* @.str.1, i64 0, i64 0))
  call void @exit(i32 1) #6
  unreachable

18:                                               ; preds = %2
  %19 = bitcast [64 x i8]* %7 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 16 %19, i8* align 16 getelementptr inbounds (<{ [22 x i8], [42 x i8] }>, <{ [22 x i8], [42 x i8] }>* @__const._Z22get_value_from_processPcPi.start_pattern, i32 0, i32 0, i32 0), i64 64, i1 false)
  store i32 0, i32* %8, align 4
  br label %20

20:                                               ; preds = %35, %18
  %21 = load i32, i32* %8, align 4
  %22 = icmp eq i32 %21, 0
  br i1 %22, label %23, label %36

23:                                               ; preds = %20
  %24 = getelementptr inbounds [1024 x i8], [1024 x i8]* %6, i64 0, i64 0
  %25 = load %struct._IO_FILE*, %struct._IO_FILE** %5, align 8
  %26 = call i8* @fgets(i8* %24, i32 1023, %struct._IO_FILE* %25)
  %27 = icmp ne i8* %26, null
  br i1 %27, label %28, label %35

28:                                               ; preds = %23
  %29 = getelementptr inbounds [1024 x i8], [1024 x i8]* %6, i64 0, i64 0
  %30 = getelementptr inbounds [64 x i8], [64 x i8]* %7, i64 0, i64 0
  %31 = call i8* @strstr(i8* %29, i8* %30) #7
  %32 = icmp ne i8* %31, null
  br i1 %32, label %33, label %34

33:                                               ; preds = %28
  store i32 1, i32* %8, align 4
  br label %36

34:                                               ; preds = %28
  br label %35

35:                                               ; preds = %34, %23
  br label %20, !llvm.loop !2

36:                                               ; preds = %33, %20
  %37 = load i32, i32* %8, align 4
  %38 = icmp eq i32 %37, 1
  br i1 %38, label %39, label %63

39:                                               ; preds = %36
  store i32 0, i32* %9, align 4
  br label %40

40:                                               ; preds = %59, %39
  %41 = load i32, i32* %9, align 4
  %42 = icmp slt i32 %41, 1
  br i1 %42, label %43, label %62

43:                                               ; preds = %40
  %44 = getelementptr inbounds [1024 x i8], [1024 x i8]* %6, i64 0, i64 0
  %45 = load %struct._IO_FILE*, %struct._IO_FILE** %5, align 8
  %46 = call i8* @fgets(i8* %44, i32 1023, %struct._IO_FILE* %45)
  %47 = icmp ne i8* %46, null
  br i1 %47, label %48, label %57

48:                                               ; preds = %43
  %49 = getelementptr inbounds [1024 x i8], [1024 x i8]* %6, i64 0, i64 0
  %50 = load i32*, i32** %4, align 8
  %51 = getelementptr inbounds i32, i32* %50, i64 0
  %52 = load i32*, i32** %4, align 8
  %53 = getelementptr inbounds i32, i32* %52, i64 1
  %54 = load i32*, i32** %4, align 8
  %55 = getelementptr inbounds i32, i32* %54, i64 2
  %56 = call i32 (i8*, i8*, ...) @__isoc99_sscanf(i8* %49, i8* getelementptr inbounds ([15 x i8], [15 x i8]* @.str.2, i64 0, i64 0), i32* %10, i32* %51, i32* %53, i32* %55) #8
  br label %58

57:                                               ; preds = %43
  br label %62

58:                                               ; preds = %48
  br label %59

59:                                               ; preds = %58
  %60 = load i32, i32* %9, align 4
  %61 = add nsw i32 %60, 1
  store i32 %61, i32* %9, align 4
  br label %40, !llvm.loop !4

62:                                               ; preds = %57, %40
  br label %63

63:                                               ; preds = %62, %36
  %64 = load %struct._IO_FILE*, %struct._IO_FILE** %5, align 8
  %65 = call i32 @pclose(%struct._IO_FILE* %64)
  store i32 0, i32* %11, align 4
  br label %66

66:                                               ; preds = %82, %63
  %67 = load i32, i32* %11, align 4
  %68 = icmp slt i32 %67, 3
  br i1 %68, label %69, label %85

69:                                               ; preds = %66
  %70 = load i32*, i32** %4, align 8
  %71 = load i32, i32* %11, align 4
  %72 = sext i32 %71 to i64
  %73 = getelementptr inbounds i32, i32* %70, i64 %72
  %74 = load i32, i32* %73, align 4
  %75 = icmp sle i32 %74, 0
  br i1 %75, label %76, label %81

76:                                               ; preds = %69
  %77 = load i32*, i32** %4, align 8
  %78 = load i32, i32* %11, align 4
  %79 = sext i32 %78 to i64
  %80 = getelementptr inbounds i32, i32* %77, i64 %79
  store i32 1, i32* %80, align 4
  br label %81

81:                                               ; preds = %76, %69
  br label %82

82:                                               ; preds = %81
  %83 = load i32, i32* %11, align 4
  %84 = add nsw i32 %83, 1
  store i32 %84, i32* %11, align 4
  br label %66, !llvm.loop !5

85:                                               ; preds = %66
  ret i32 0
}

declare dso_local %struct._IO_FILE* @popen(i8*, i8*) #1

declare dso_local i32 @printf(i8*, ...) #1

; Function Attrs: noreturn nounwind
declare dso_local void @exit(i32) #2

; Function Attrs: argmemonly nofree nosync nounwind willreturn
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* noalias nocapture writeonly, i8* noalias nocapture readonly, i64, i1 immarg) #3

declare dso_local i8* @fgets(i8*, i32, %struct._IO_FILE*) #1

; Function Attrs: nounwind readonly willreturn
declare dso_local i8* @strstr(i8*, i8*) #4

; Function Attrs: nounwind
declare dso_local i32 @__isoc99_sscanf(i8*, i8*, ...) #5

declare dso_local i32 @pclose(%struct._IO_FILE*) #1

; Function Attrs: noinline optnone uwtable mustprogress
define dso_local i32 @_Z12rp_estimatorPciPi(i8* %0, i32 %1, i32* %2) #0 {
  %4 = alloca i8*, align 8
  %5 = alloca i32, align 4
  %6 = alloca i32*, align 8
  %7 = alloca i32*, align 8
  %8 = alloca i32*, align 8
  %9 = alloca i32*, align 8
  %10 = alloca i32*, align 8
  %11 = alloca i32*, align 8
  %12 = alloca i32*, align 8
  %13 = alloca [1024 x i8], align 16
  %14 = alloca [1024 x i8], align 16
  %15 = alloca [1024 x i8], align 16
  %16 = alloca [3 x i32], align 4
  %17 = alloca i32, align 4
  %18 = alloca i32, align 4
  %19 = alloca i32, align 4
  store i8* %0, i8** %4, align 8
  store i32 %1, i32* %5, align 4
  store i32* %2, i32** %6, align 8
  %20 = load i32*, i32** %6, align 8
  %21 = getelementptr inbounds i32, i32* %20, i64 0
  store i32* %21, i32** %7, align 8
  %22 = load i32*, i32** %6, align 8
  %23 = getelementptr inbounds i32, i32* %22, i64 1
  store i32* %23, i32** %8, align 8
  %24 = load i32*, i32** %6, align 8
  %25 = getelementptr inbounds i32, i32* %24, i64 2
  store i32* %25, i32** %9, align 8
  %26 = load i32*, i32** %6, align 8
  %27 = getelementptr inbounds i32, i32* %26, i64 3
  store i32* %27, i32** %10, align 8
  %28 = load i32*, i32** %6, align 8
  %29 = getelementptr inbounds i32, i32* %28, i64 4
  store i32* %29, i32** %11, align 8
  %30 = load i32*, i32** %6, align 8
  %31 = getelementptr inbounds i32, i32* %30, i64 5
  store i32* %31, i32** %12, align 8
  %32 = getelementptr inbounds [1024 x i8], [1024 x i8]* %13, i64 0, i64 0
  %33 = load i8*, i8** %4, align 8
  %34 = call i32 (i8*, i8*, ...) @sprintf(i8* %32, i8* getelementptr inbounds ([24 x i8], [24 x i8]* @.str.3, i64 0, i64 0), i8* %33) #8
  %35 = getelementptr inbounds [1024 x i8], [1024 x i8]* %14, i64 0, i64 0
  %36 = load i8*, i8** %4, align 8
  %37 = load i32, i32* %5, align 4
  %38 = call i32 (i8*, i8*, ...) @sprintf(i8* %35, i8* getelementptr inbounds ([25 x i8], [25 x i8]* @.str.4, i64 0, i64 0), i8* %36, i32 %37) #8
  %39 = getelementptr inbounds [1024 x i8], [1024 x i8]* %15, i64 0, i64 0
  %40 = getelementptr inbounds [1024 x i8], [1024 x i8]* %13, i64 0, i64 0
  %41 = load i32, i32* %5, align 4
  %42 = call i32 (i8*, i8*, ...) @sprintf(i8* %39, i8* getelementptr inbounds ([68 x i8], [68 x i8]* @.str.5, i64 0, i64 0), i8* %40, i32 %41) #8
  %43 = getelementptr inbounds [1024 x i8], [1024 x i8]* %15, i64 0, i64 0
  %44 = call i32 @system(i8* %43)
  %45 = getelementptr inbounds [1024 x i8], [1024 x i8]* %15, i64 0, i64 0
  %46 = getelementptr inbounds [3 x i32], [3 x i32]* %16, i64 0, i64 0
  %47 = call i32 @_Z22get_value_from_processPcPi(i8* %45, i32* %46)
  %48 = getelementptr inbounds [3 x i32], [3 x i32]* %16, i64 0, i64 0
  %49 = load i32, i32* %48, align 4
  store i32 %49, i32* %17, align 4
  %50 = getelementptr inbounds [3 x i32], [3 x i32]* %16, i64 0, i64 1
  %51 = load i32, i32* %50, align 4
  store i32 %51, i32* %18, align 4
  %52 = getelementptr inbounds [3 x i32], [3 x i32]* %16, i64 0, i64 2
  %53 = load i32, i32* %52, align 4
  store i32 %53, i32* %19, align 4
  %54 = load i32, i32* %17, align 4
  %55 = load i32, i32* %18, align 4
  %56 = load i32, i32* %19, align 4
  %57 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([40 x i8], [40 x i8]* @.str.6, i64 0, i64 0), i32 %54, i32 %55, i32 %56)
  %58 = load i32, i32* %5, align 4
  %59 = load i32, i32* %17, align 4
  %60 = sdiv i32 %58, %59
  %61 = load i32*, i32** %7, align 8
  store i32 %60, i32* %61, align 4
  %62 = load i32, i32* %5, align 4
  %63 = load i32, i32* %18, align 4
  %64 = sdiv i32 %62, %63
  %65 = load i32*, i32** %8, align 8
  store i32 %64, i32* %65, align 4
  %66 = load i32*, i32** %9, align 8
  store i32 1, i32* %66, align 4
  %67 = load i32, i32* %17, align 4
  %68 = load i32*, i32** %10, align 8
  store i32 %67, i32* %68, align 4
  %69 = load i32, i32* %18, align 4
  %70 = load i32*, i32** %11, align 8
  store i32 %69, i32* %70, align 4
  %71 = load i32, i32* %19, align 4
  %72 = load i32*, i32** %12, align 8
  store i32 %71, i32* %72, align 4
  ret i32 0
}

; Function Attrs: nounwind
declare dso_local i32 @sprintf(i8*, i8*, ...) #5

declare dso_local i32 @system(i8*) #1

attributes #0 = { noinline optnone uwtable mustprogress "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { noreturn nounwind "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { argmemonly nofree nosync nounwind willreturn }
attributes #4 = { nounwind readonly willreturn "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #5 = { nounwind "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #6 = { noreturn nounwind }
attributes #7 = { nounwind readonly willreturn }
attributes #8 = { nounwind }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"Ubuntu clang version 12.0.1-++20211029101322+fed41342a82f-1~exp1~20211029221816.4"}
!2 = distinct !{!2, !3}
!3 = !{!"llvm.loop.mustprogress"}
!4 = distinct !{!4, !3}
!5 = distinct !{!5, !3}
