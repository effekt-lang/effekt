declare void @printf(i8*, ...)


@printIntString = private unnamed_addr constant [5 x i8] c"%ld\0A\00"

define void @printInt_impl(%Int %n) {
    %printIntStringPtr = getelementptr inbounds [5 x i8], [5 x i8]* @printIntString , i32 0, i32 0
    call void (i8*, ...) @printf(i8* %printIntStringPtr, %Int %n)
    ret void
}


; @printBoolString = private unnamed_addr constant [4 x i8] c"%s\0A\00"

; @boolTrueString = private unnamed_addr constant [5 x i8] c"true\00"
; @boolFalseString = private unnamed_addr constant [6 x i8] c"false\00"

; define void @printBool_impl(%Bool %b) {
;     %boolTrueString = getelementptr inbounds [5 x i8], [5 x i8]* @boolTrueString , i32 0, i32 0
;     %boolFalseString = getelementptr inbounds [6 x i8], [6 x i8]* @boolFalseString , i32 0, i32 0
;     %boolString = select i1 %b, i8* %boolTrueString, i8* %boolFalseString
;     %printBoolString = getelementptr inbounds [4 x i8], [4 x i8]* @printBoolString , i32 0, i32 0
;     call void (i8*, ...) @printf(i8* %printBoolString, i8* %boolString)
;     ret void
; }
