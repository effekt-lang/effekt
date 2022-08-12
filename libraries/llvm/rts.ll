; Run-Time System


; Basic types

%Env = type i8*

%Rc = type i64
%Eraser = type void (%Env)*
%Header = type {%Rc, %Eraser}

%Obj = type %Header*


%Sp = type i8*
%Base = type %Sp
%Limit = type %Sp

%StkVal = type { %Rc, %Sp, %Base, %Limit, %StkVal* }

%Stk = type %StkVal*


%Pos = type {i64, %Obj}
%Neg = type {void (%Obj, %Env, %Sp)*, %Obj}


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

; Garbage collection

define %Obj @newObject(%Eraser %eraser, i64 %envsize) alwaysinline {
    ; This magical 16 is the size of the object header
    %size = add i64 %envsize, 16
    %mem = call i8* @malloc(i64 %size)
    %obj = bitcast i8* %mem to %Obj
    %objrc = getelementptr %Header, %Header* %obj, i64 0, i32 0
    %objeraser = getelementptr %Header, %Header* %obj, i64 0, i32 1
    store %Rc 0, %Rc* %objrc
    store %Eraser %eraser, %Eraser* %objeraser
    ret %Obj %obj
}

define %Env @objectEnvironment(%Obj %obj) alwaysinline {
    ; Environment is stored right after header
    %obj.1 = getelementptr %Header, %Header* %obj, i64 1
    %env = bitcast %Obj %obj.1 to %Env
    ret %Env %env
}

define void @shareObject(%Obj %obj) alwaysinline {
    %isnull = icmp eq %Obj %obj, null
    br i1 %isnull, label %done, label %next
    next:
    %objrc = getelementptr %Header, %Header* %obj, i64 0, i32 0
    %rc = load %Rc, %Rc* %objrc
    %rc.1 = add %Rc %rc, 1
    store %Rc %rc.1, %Rc* %objrc
    br label %done
    done:
    ret void
}

define void @sharePositive(%Pos %val) alwaysinline {
    %obj = extractvalue %Pos %val, 1
    tail call void @shareObject(%Obj %obj)
    ret void
}

define void @shareNegative(%Neg %val) alwaysinline {
    %obj = extractvalue %Neg %val, 1
    tail call void @shareObject(%Obj %obj)
    ret void
}

define void @eraseObject(%Obj %obj) alwaysinline {
    %isnull = icmp eq %Obj %obj, null
    br i1 %isnull, label %done, label %next
    next:
    %objrc = getelementptr %Header, %Header* %obj, i64 0, i32 0
    %rc = load %Rc, %Rc* %objrc
    switch %Rc %rc, label %decr [%Rc 0, label %free]
    decr:
    %rc.1 = sub %Rc %rc, 1
    store %Rc %rc.1, %Rc* %objrc
    ret void
    free:
    %objeraser = getelementptr %Header, %Header* %obj, i64 0, i32 1
    %eraser = load %Eraser, %Eraser* %objeraser
    %env = call %Env @objectEnvironment(%Obj %obj)
    call fastcc void %eraser(%Env %env)
    %objuntyped = bitcast %Obj %obj to i8*
    call void @free(i8* %objuntyped)
    br label %done
    done:
    ret void
}

define void @erasePositive(%Pos %val) alwaysinline {
    %obj = extractvalue %Pos %val, 1
    tail call void @eraseObject(%Obj %obj)
    ret void
}

define void @eraseNegative(%Neg %val) alwaysinline {
    %obj = extractvalue %Neg %val, 1
    tail call void @eraseObject(%Obj %obj)
    ret void
}


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

define void @shareStack(%Stk %stk) alwaysinline {
    %stkrc = getelementptr %StkVal, %Stk %stk, i64 0, i32 0
    %rc = load %Rc, %Rc* %stkrc
    %rc.1 = add %Rc %rc, 1
    store %Rc %rc.1, %Rc* %stkrc
    ret void
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
