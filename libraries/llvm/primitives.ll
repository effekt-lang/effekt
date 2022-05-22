declare void @printf(i8*, ...)


@printIntString = private unnamed_addr constant [5 x i8] c"%ld\0A\00"

define void @printInt_impl(%Int %n) {
    %printIntStringPtr = getelementptr inbounds [5 x i8], [5 x i8]* @printIntString , i32 0, i32 0
    call void (i8*, ...) @printf(i8* %printIntStringPtr, %Int %n)
    ret void
}


@printBooleanString = private unnamed_addr constant [4 x i8] c"%s\0A\00"

@booleanTrueString = private unnamed_addr constant [5 x i8] c"true\00"
@booleanFalseString = private unnamed_addr constant [6 x i8] c"false\00"

define void @printBoolean_impl(%Boolean %b) {
    %booleanTrueString = getelementptr inbounds [5 x i8], [5 x i8]* @booleanTrueString , i32 0, i32 0
    %booleanFalseString = getelementptr inbounds [6 x i8], [6 x i8]* @booleanFalseString , i32 0, i32 0
    %booleanString = select i1 %b, i8* %booleanTrueString, i8* %booleanFalseString
    %printBooleanString = getelementptr inbounds [4 x i8], [4 x i8]* @printBooleanString , i32 0, i32 0
    call void (i8*, ...) @printf(i8* %printBooleanString, i8* %booleanString)
    ret void
}
