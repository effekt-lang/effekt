; Run-Time System


; Basic types

%Rc = type i64

%Sp = type i8*
%Base = type %Sp
%Limit = type %Sp

%StkVal = type { %Rc, %Sp, %Base, %Limit, %StkVal* }

%Stk = type %StkVal*

%Env = type i8*


; Global locations

@base = private global %Base null
@limit = private global %Limit null
@rest = private global %Stk undef


; Foreign imports

declare i8* @malloc(i64)
declare void @free(i8*)
declare i8* @realloc(i8*, i64)
declare void @memcpy(i8*, i8*, i64)
declare i64 @llvm.ctlz.i64 (i64 , i1)
declare i64 @llvm.fshr.i64(i64, i64, i64)
declare void @print(i64)
declare void @exit(i64)


; Meta-stack management

define %StkVal* @newStack() alwaysinline {

    ; TODO find actual size of stack
    %stkm = call i8* @malloc(i64 40)
    %stkp = bitcast i8* %stkm to %StkVal*

    ; TODO initialize to zero and grow later
    %sp = call %Sp @malloc(i64 1024)

    %stk.0 = insertvalue %StkVal undef, %Rc 0, 0
    %stk.1 = insertvalue %StkVal %stk.0, %Sp %sp, 1
    %stk.2 = insertvalue %StkVal %stk.1, %Base %sp, 2
    %stk.3 = insertvalue %StkVal %stk.2, %Limit null, 3
    %stk.4 = insertvalue %StkVal %stk.3, %Stk null, 4

    store %StkVal %stk.4, %StkVal* %stkp

    ret %StkVal* %stkp
}

define %Sp @pushStack(%StkVal* %stkp, %Sp %oldsp) alwaysinline {

    ; TODO check reference count and do a deep copy

    %stkrc = getelementptr %StkVal, %StkVal* %stkp, i64 0, i32 0
    %stksp = getelementptr %StkVal, %StkVal* %stkp, i64 0, i32 1
    %stkbase = getelementptr %StkVal, %StkVal* %stkp, i64 0, i32 2
    %stklimit = getelementptr %StkVal, %StkVal* %stkp, i64 0, i32 3
    %stkrest = getelementptr %StkVal, %StkVal* %stkp, i64 0, i32 4

    %newsp = load %Sp, %Sp* %stksp
    %newbase = load %Base, %Base* %stkbase
    %newlimit = load %Limit, %Limit* %stklimit
    ; %newrest = load %Stk, %Stk* %stkrest
    ; assert %newrest == null

    %oldbase = load %Base, %Base* @base
    %oldlimit = load %Limit, %Limit* @limit
    %oldrest = load %Stk, %Stk* @rest

    store %Base %newbase, %Base* @base
    store %Limit %newlimit, %Limit* @limit
    store %Stk %stkp, %Stk* @rest

    store %Rc 0, %Rc* %stkrc
    store %Sp %oldsp, %Sp* %stksp
    store %Base %oldbase, %Base* %stkbase
    store %Limit %oldlimit, %Limit* %stklimit
    store %Stk %oldrest, %Stk* %stkrest

    ret %Sp %newsp
}

define {%StkVal*, %Sp} @popStack(%Sp %oldsp) alwaysinline {

    %stkp = load %StkVal*, %StkVal** @rest

    %stkrc = getelementptr %StkVal, %StkVal* %stkp, i64 0, i32 0
    %stksp = getelementptr %StkVal, %StkVal* %stkp, i64 0, i32 1
    %stkbase = getelementptr %StkVal, %StkVal* %stkp, i64 0, i32 2
    %stklimit = getelementptr %StkVal, %StkVal* %stkp, i64 0, i32 3
    %stkrest = getelementptr %StkVal, %StkVal* %stkp, i64 0, i32 4

    ; %newrc = load %Rc, %Rc* %stkrc
    %newsp = load %Sp, %Sp* %stksp
    %newbase = load %Base, %Base* %stkbase
    %newlimit = load %Limit, %Limit* %stklimit
    %newrest = load %Stk, %Stk* %stkrest

    %oldbase = load %Base, %Base* @base
    %oldlimit = load %Limit, %Limit* @limit
    ; %oldrest = load %Stk, %Stk* @rest

    store %Base %newbase, %Base* @base
    store %Limit %newlimit, %Limit* @limit
    store %Stk %newrest, %Stk* @rest

    store %Rc 0, %Rc* %stkrc
    store %Sp %oldsp, %Sp* %stksp
    store %Base %oldbase, %Base* %stkbase
    store %Limit %oldlimit, %Limit* %stklimit
    store %Stk null, %Stk* %stkrest

    %ret.0 = insertvalue {%StkVal*, %Sp} undef, %StkVal* %stkp, 0
    %ret.1 = insertvalue {%StkVal*, %Sp} %ret.0, %Sp %newsp, 1

    ret {%StkVal*, %Sp} %ret.1
}

define %Sp @underflowStack(%Sp %sp) alwaysinline {
    %stkp = load %StkVal*, %StkVal** @rest

    %stkrc = getelementptr %StkVal, %StkVal* %stkp, i64 0, i32 0
    %stksp = getelementptr %StkVal, %StkVal* %stkp, i64 0, i32 1
    %stkbase = getelementptr %StkVal, %StkVal* %stkp, i64 0, i32 2
    %stklimit = getelementptr %StkVal, %StkVal* %stkp, i64 0, i32 3
    %stkrest = getelementptr %StkVal, %StkVal* %stkp, i64 0, i32 4

    ; %newrc = load %Rc, %Rc* %stkrc
    %newsp = load %Sp, %Sp* %stksp
    %newbase = load %Base, %Base* %stkbase
    %newlimit = load %Limit, %Limit* %stklimit
    %newrest = load %Stk, %Stk* %stkrest

    store %Base %newbase, %Base* @base
    store %Limit %newlimit, %Limit* @limit
    store %Stk %newrest, %Stk* @rest

    %stkpuntyped = bitcast %StkVal* %stkp to i8*
    call void @free(%Sp %sp)
    call void @free(i8* %stkpuntyped)
    ret %Sp %newsp
}

; RTS initialization

define fastcc void @topLevel(%Env %env, %Sp noalias %sp) {
    %base = load %Base, %Base* @base
    call void @free(i8* %base)
    call void @free(i8* %env)
    ret void
}


; Primitive Types

%Int = type i64

%Evi = type i64
