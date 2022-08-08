; Run-Time System


; Basic types

%Sp = type i8*
%Base = type %Sp
%Limit = type %Sp

%StkVal = type { %Sp, %Base, %Limit, %StkVal* }

%Stk = type %StkVal*

%Env = type i8*

; pos
%ADT = type {i64, i8*}


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
    %stkm = call i8* @malloc(i64 32)
    %stkp = bitcast i8* %stkm to %StkVal*

    ; TODO initialize to zero and grow later
    %sp = call %Sp @malloc(i64 1024)

    %stk.0 = insertvalue %StkVal undef, %Sp %sp, 0
    %stk.1 = insertvalue %StkVal %stk.0, %Base %sp, 1
    %stk.2 = insertvalue %StkVal %stk.1, %Limit null, 2
    %stk.3 = insertvalue %StkVal %stk.2, %Stk null, 3

    store %StkVal %stk.3, %StkVal* %stkp

    ret %StkVal* %stkp
}

define %Sp @pushStack(%StkVal* %stkp, %Sp %oldsp) alwaysinline {

    %stksp = getelementptr %StkVal, %StkVal* %stkp, i64 0, i32 0
    %stkbase = getelementptr %StkVal, %StkVal* %stkp, i64 0, i32 1
    %stklimit = getelementptr %StkVal, %StkVal* %stkp, i64 0, i32 2
    %stkrest = getelementptr %StkVal, %StkVal* %stkp, i64 0, i32 3

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

    store %Sp %oldsp, %Sp* %stksp
    store %Base %oldbase, %Base* %stkbase
    store %Limit %oldlimit, %Limit* %stklimit
    store %Stk %oldrest, %Stk* %stkrest

    ret %Sp %newsp
}

define {%StkVal*, %Sp} @popStack(%Sp %oldsp) alwaysinline {

    %stkp = load %StkVal*, %StkVal** @rest

    %stksp = getelementptr %StkVal, %StkVal* %stkp, i64 0, i32 0
    %stkbase = getelementptr %StkVal, %StkVal* %stkp, i64 0, i32 1
    %stklimit = getelementptr %StkVal, %StkVal* %stkp, i64 0, i32 2
    %stkrest = getelementptr %StkVal, %StkVal* %stkp, i64 0, i32 3

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

    store %Sp %oldsp, %Sp* %stksp
    store %Base %oldbase, %Base* %stkbase
    store %Limit %oldlimit, %Limit* %stklimit
    store %Stk null, %Stk* %stkrest

    %ret.0 = insertvalue {%StkVal*, %Sp} undef, %StkVal* %stkp, 0
    %ret.1 = insertvalue {%StkVal*, %Sp} %ret.0, %Sp %newsp, 1

    ret {%StkVal*, %Sp} %ret.1
}

define %StkVal* @copyStack(%StkVal* %stkp) alwaysinline {
entry:
    %stk = load %StkVal, %StkVal* %stkp

    %sp = extractvalue %StkVal %stk, 0
    %base = extractvalue %StkVal %stk, 1
    %limit = extractvalue %StkVal %stk, 2
    ; %rest = extractvalue %StkVal %stk, 3
    ; assert %rest == null

    %intsp = ptrtoint %Sp %sp to i64
    %intbase = ptrtoint %Base %base to i64
    %intlimit = ptrtoint %Limit %limit to i64
    %used = sub i64 %intsp, %intbase
    %size = sub i64 %intlimit, %intbase

    %newbase = call i8* @malloc(i64 %size)
    call void @memcpy(i8* %newbase, i8* %base, i64 %used)

    %intnewbase = ptrtoint %Base %newbase to i64
    %intnewsp = add i64 %intnewbase, %used
    %intnewlimit = add i64 %intnewbase, %size
    %newsp = inttoptr i64 %intnewsp to %Sp
    %newlimit = inttoptr i64 %intnewlimit to %Limit

;     %intboxesbase = ptrtoint %BoxesBase %boxesbase to i64
;     %intboxeslimit = ptrtoint %BoxesLimit %boxeslimit to i64
;     %boxessize = sub i64 %intboxeslimit, %intboxesbase

;     %newboxesbase = call i8* @malloc(i64 %boxessize)

;     %intnewboxesbase = ptrtoint %BoxesBase %newboxesbase to i64
;     %intnewboxeslimit = add i64 %intnewboxesbase, %boxessize
;     %newboxeslimit = inttoptr i64 %intnewboxeslimit to %BoxesLimit

;     ; TODO walk in the other direction
;     %boxessptyped = bitcast %BoxesSp %boxessp to %StkVal**
;     %boxesbasetyped = bitcast %BoxesSp %boxesbase to %StkVal**
;     %newboxesbasetyped = bitcast %BoxesSp %newboxesbase to %StkVal**
;     br label %comp
; comp:
;     %currentboxessp = phi %StkVal** [%boxesbasetyped, %entry], [%nextboxessp, %loop]
;     %currentnewboxessp = phi %StkVal** [%newboxesbasetyped, %entry], [%nextnewboxessp, %loop]
;     %atend = icmp eq %StkVal** %currentboxessp, %boxessptyped
;     br i1 %atend, label %done, label %loop
; loop:
;     %currentstk = load %StkVal*, %StkVal** %currentboxessp
;     %currentnewstk = call %StkVal* @copyStack(%StkVal* %currentstk)
;     store %StkVal* %currentnewstk, %StkVal** %currentnewboxessp
;     %nextboxessp = getelementptr %StkVal*, %StkVal** %currentboxessp, i64 1
;     %nextnewboxessp = getelementptr %StkVal*, %StkVal** %currentnewboxessp, i64 1
;     br label %comp
; done:
;     %newboxessp = bitcast %StkVal** %currentnewboxessp to %BoxesSp

    %newstk.0 = insertvalue %StkVal undef, %Sp %newsp, 0
    %newstk.1 = insertvalue %StkVal %newstk.0, %Base %newbase, 1
    %newstk.2 = insertvalue %StkVal %newstk.1, %Limit %newlimit, 2
    %newstk.3 = insertvalue %StkVal %newstk.2, %Stk null, 3

    %newstkp = call %StkVal* @newStack()
    store %StkVal %newstk.3, %StkVal* %newstkp
    ret %StkVal* %newstkp
}

define void @eraseStack(%StkVal* %stkp) alwaysinline {
entry:
    %stk = load %StkVal, %StkVal* %stkp

    %baseuntyped = extractvalue %StkVal %stk, 1

;     %boxesspuntyped = extractvalue %StkVal %stk, 3
;     %boxesbaseuntyped = extractvalue %StkVal %stk, 4

;     %boxessp = bitcast %BoxesSp %boxesspuntyped to %StkVal**
;     %boxesbase = bitcast %BoxesBase %boxesbaseuntyped to %StkVal**

;     br label %comp
; comp:
;     %currentboxessp = phi %StkVal** [%boxessp, %entry], [%newboxessp, %loop]
;     %atbase = icmp eq %StkVal** %currentboxessp, %boxesbase
;     br i1 %atbase, label %done, label %loop
; loop:
;     %newboxessp = getelementptr %StkVal*, %StkVal** %currentboxessp, i64 -1
;     %nextstk = load %StkVal*, %StkVal** %newboxessp
;     call fastcc void @eraseStack(%StkVal* %nextstk)
;     br label %comp
; done:
    %stkuntyped = bitcast %StkVal* %stkp to i8*
    call void @free(i8* %stkuntyped)
    call void @free(i8* %baseuntyped)
;     call void @free(i8* %boxesbaseuntyped)
    ret void
}

define %Sp @checkOverflow(%Sp %incedsp, %Sp* %spp) alwaysinline {
    %sp = load %Sp, %Sp* %spp
    %limit = load %Limit, %Limit* @limit

    %intlimit = ptrtoint %Limit %limit to i64
    %intincedsp = ptrtoint %Sp %incedsp to i64

    %overflow = icmp ugt i64 %intincedsp, %intlimit
    br i1 %overflow, label %grow, label %good
good:
    store %Sp %incedsp, %Sp* %spp
    ret %Sp %sp
grow:
    %results = call {%Sp, %Sp} @growStack(%Sp %sp, %Sp %incedsp)
    %newsp = extractvalue {%Sp, %Sp} %results, 0
    %newincedsp = extractvalue {%Sp, %Sp} %results, 1
    store %Sp %newincedsp, %Sp* %spp
    ret %Sp %newsp
}

define %Sp @underflowStack(%Sp %sp) alwaysinline {
    %stkp = load %StkVal*, %StkVal** @rest

    %stksp = getelementptr %StkVal, %StkVal* %stkp, i64 0, i32 0
    %stkbase = getelementptr %StkVal, %StkVal* %stkp, i64 0, i32 1
    %stklimit = getelementptr %StkVal, %StkVal* %stkp, i64 0, i32 2
    %stkrest = getelementptr %StkVal, %StkVal* %stkp, i64 0, i32 3

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

define {%Sp, %Sp} @growStack(%Sp %sp, %Sp %incedsp) noinline {

    %intincedsp = ptrtoint %Sp %incedsp to i64

    %base = load %Base, %Base* @base

    %intsp = ptrtoint %Sp %sp to i64
    %intbase = ptrtoint %Base %base to i64

    %needed = sub i64 %intincedsp, %intbase
    %used = sub i64 %intsp, %intbase

    %needed.1 = sub i64 %needed, 1
    %leadingzeroes = call i64 @llvm.ctlz.i64(i64 %needed.1, i1 false)
    %newsize = call i64 @llvm.fshr.i64(i64 1, i64 0, i64 %leadingzeroes)
    %newbase = call i8* @realloc(i8* %base, i64 %newsize)

    %intnewbase = ptrtoint %Base %newbase to i64
    %intnewsp = add i64 %intnewbase, %used
    %intnewlimit = add i64 %intnewbase, %newsize
    %intnewincedsp = add i64 %intnewbase, %needed
    %newsp = inttoptr i64 %intnewsp to %Sp
    %newlimit = inttoptr i64 %intnewlimit to %Limit
    %newincedsp = inttoptr i64 %intnewincedsp to %Sp
    store %Base %newbase, %Base* @base
    store %Limit %newlimit, %Limit* @limit

    %results.0 = insertvalue {%Sp, %Sp} undef ,%Sp %newsp, 0
    %results.1 = insertvalue {%Sp, %Sp} %results.0 ,%Sp %newincedsp, 1

    ret {%Sp,%Sp} %results.1
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
