; Basic types

%Sp = type i8*
%Base = type %Sp
%Limit = type %Sp
%BoxesSp = type i8*
%BoxesBase = type %BoxesSp
%BoxesLimit = type %BoxesSp

%Stk = type { %Sp, %Base, %Limit, %BoxesSp, %BoxesBase, %BoxesLimit, %Stk* }

%MStk = type %Stk*

; Global locations

@base = private global %Base null
@limit = private global %Limit null
@boxessp = private global %BoxesSp null
@boxesbase = private global %BoxesBase null
@boxeslimit = private global %BoxesLimit null
@rest = private global %MStk undef

; Foreign imports

declare i8* @malloc(i64)
declare void @free(i8*)
declare i8* @realloc(i8*, i64)
declare void @memcpy(i8*, i8*, i64)
declare i64 @llvm.ctlz.i64 (i64 , i1)
declare i64 @llvm.fshr.i64(i64, i64, i64)
declare void @print(i64)

; Meta-stack management

define fastcc %Stk* @newStack() alwaysinline {

    ; TODO find actual size of stack
    %stkm = call i8* @malloc(i64 56)
    %stkp = bitcast i8* %stkm to %Stk*

    %stk.0 = insertvalue %Stk undef, %Sp null, 0
    %stk.1 = insertvalue %Stk %stk.0, %Base null, 1
    %stk.2 = insertvalue %Stk %stk.1, %Limit null, 2
    %stk.3 = insertvalue %Stk %stk.2, %BoxesSp null, 3
    %stk.4 = insertvalue %Stk %stk.3, %BoxesBase null, 4
    %stk.5 = insertvalue %Stk %stk.4, %BoxesLimit null, 5
    %stk.6 = insertvalue %Stk %stk.5, %MStk null, 6

    store %Stk %stk.6, %Stk* %stkp

    ret %Stk* %stkp
}

define fastcc void @pushStack(%Sp* %spp, %Stk* %stkp) alwaysinline {

    %stksp = getelementptr %Stk, %Stk* %stkp, i64 0, i32 0
    %stkbase = getelementptr %Stk, %Stk* %stkp, i64 0, i32 1
    %stklimit = getelementptr %Stk, %Stk* %stkp, i64 0, i32 2
    %stkboxessp = getelementptr %Stk, %Stk* %stkp, i64 0, i32 3
    %stkboxesbase = getelementptr %Stk, %Stk* %stkp, i64 0, i32 4
    %stkboxeslimit = getelementptr %Stk, %Stk* %stkp, i64 0, i32 5
    %stkrest = getelementptr %Stk, %Stk* %stkp, i64 0, i32 6

    %newsp = load %Sp, %Sp* %stksp
    %newbase = load %Base, %Base* %stkbase
    %newlimit = load %Limit, %Limit* %stklimit
    %newboxessp = load %BoxesSp, %BoxesSp* %stkboxessp
    %newboxesbase = load %BoxesBase, %BoxesBase* %stkboxesbase
    %newboxeslimit = load %BoxesLimit, %BoxesLimit* %stkboxeslimit
    ; %newrest = load %MStk, %MStk* %stkrest
    ; assert %newrest == null

    %oldsp = load %Sp, %Sp* %spp
    %oldbase = load %Base, %Base* @base
    %oldlimit = load %Limit, %Limit* @limit
    %oldboxessp = load %BoxesSp, %BoxesSp* @boxessp
    %oldboxesbase = load %BoxesBase, %BoxesBase* @boxesbase
    %oldboxeslimit = load %BoxesLimit, %BoxesLimit* @boxeslimit
    %oldrest = load %MStk, %MStk* @rest

    store %Sp %newsp, %Sp* %spp
    store %Base %newbase, %Base* @base
    store %Limit %newlimit, %Limit* @limit
    store %BoxesSp %newboxessp, %BoxesSp* @boxessp
    store %BoxesBase %newboxesbase, %BoxesBase* @boxesbase
    store %BoxesLimit %newboxeslimit, %BoxesLimit* @boxeslimit
    store %MStk %stkp, %MStk* @rest

    store %Sp %oldsp, %Sp* %stksp
    store %Base %oldbase, %Base* %stkbase
    store %Limit %oldlimit, %Limit* %stklimit
    store %BoxesSp %oldboxessp, %BoxesSp* %stkboxessp
    store %BoxesBase %oldboxesbase, %BoxesBase* %stkboxesbase
    store %BoxesLimit %oldboxeslimit, %BoxesLimit* %stkboxeslimit
    store %MStk %oldrest, %MStk* %stkrest

    ret void
}

define fastcc %Stk* @popStack(%Sp* %spp) alwaysinline {

    %stkp = load %Stk*, %Stk** @rest

    %stksp = getelementptr %Stk, %Stk* %stkp, i64 0, i32 0
    %stkbase = getelementptr %Stk, %Stk* %stkp, i64 0, i32 1
    %stklimit = getelementptr %Stk, %Stk* %stkp, i64 0, i32 2
    %stkboxessp = getelementptr %Stk, %Stk* %stkp, i64 0, i32 3
    %stkboxesbase = getelementptr %Stk, %Stk* %stkp, i64 0, i32 4
    %stkboxeslimit = getelementptr %Stk, %Stk* %stkp, i64 0, i32 5
    %stkrest = getelementptr %Stk, %Stk* %stkp, i64 0, i32 6

    %newsp = load %Sp, %Sp* %stksp
    %newbase = load %Base, %Base* %stkbase
    %newlimit = load %Limit, %Limit* %stklimit
    %newboxessp = load %BoxesSp, %BoxesSp* %stkboxessp
    %newboxesbase = load %BoxesBase, %BoxesBase* %stkboxesbase
    %newboxeslimit = load %BoxesLimit, %BoxesLimit* %stkboxeslimit
    %newrest = load %MStk, %MStk* %stkrest

    %oldsp = load %Sp, %Sp* %spp
    %oldbase = load %Base, %Base* @base
    %oldlimit = load %Limit, %Limit* @limit
    %oldboxessp = load %BoxesSp, %BoxesSp* @boxessp
    %oldboxesbase = load %BoxesBase, %BoxesBase* @boxesbase
    %oldboxeslimit = load %BoxesLimit, %BoxesLimit* @boxeslimit
    ; %oldrest = load %MStk, %MStk* @rest

    store %Sp %newsp, %Sp* %spp
    store %Base %newbase, %Base* @base
    store %Limit %newlimit, %Limit* @limit
    store %BoxesSp %newboxessp, %BoxesSp* @boxessp
    store %BoxesBase %newboxesbase, %BoxesBase* @boxesbase
    store %BoxesLimit %newboxeslimit, %BoxesLimit* @boxeslimit
    store %MStk %newrest, %MStk* @rest

    store %Sp %oldsp, %Sp* %stksp
    store %Base %oldbase, %Base* %stkbase
    store %Limit %oldlimit, %Limit* %stklimit
    store %BoxesSp %oldboxessp, %BoxesSp* %stkboxessp
    store %BoxesBase %oldboxesbase, %BoxesBase* %stkboxesbase
    store %BoxesLimit %oldboxeslimit, %BoxesLimit* %stkboxeslimit
    store %MStk null, %MStk* %stkrest

    ret %Stk* %stkp
}

define fastcc %Stk* @copyStack(%Stk* %stkp) alwaysinline {
entry:
    %stk = load %Stk, %Stk* %stkp

    %sp = extractvalue %Stk %stk, 0
    %base = extractvalue %Stk %stk, 1
    %limit = extractvalue %Stk %stk, 2
    %boxessp = extractvalue %Stk %stk, 3
    %boxesbase = extractvalue %Stk %stk, 4
    %boxeslimit = extractvalue %Stk %stk, 5
    ; %rest = extractvalue %Stk %stk, 6
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

    %intboxesbase = ptrtoint %BoxesBase %boxesbase to i64
    %intboxeslimit = ptrtoint %BoxesLimit %boxeslimit to i64
    %boxessize = sub i64 %intboxeslimit, %intboxesbase

    %newboxesbase = call i8* @malloc(i64 %boxessize)

    %intnewboxesbase = ptrtoint %BoxesBase %newboxesbase to i64
    %intnewboxeslimit = add i64 %intnewboxesbase, %boxessize
    %newboxeslimit = inttoptr i64 %intnewboxeslimit to %BoxesLimit

    ; TODO walk in the other direction
    %boxessptyped = bitcast %BoxesSp %boxessp to %Stk**
    %boxesbasetyped = bitcast %BoxesSp %boxesbase to %Stk**
    %newboxesbasetyped = bitcast %BoxesSp %newboxesbase to %Stk**
    br label %comp
comp:
    %currentboxessp = phi %Stk** [%boxesbasetyped, %entry], [%nextboxessp, %loop]
    %currentnewboxessp = phi %Stk** [%newboxesbasetyped, %entry], [%nextnewboxessp, %loop]
    %atend = icmp eq %Stk** %currentboxessp, %boxessptyped
    br i1 %atend, label %done, label %loop
loop:
    %currentstk = load %Stk*, %Stk** %currentboxessp
    %currentnewstk = call fastcc %Stk* @copyStack(%Stk* %currentstk)
    store %Stk* %currentnewstk, %Stk** %currentnewboxessp
    %nextboxessp = getelementptr %Stk*, %Stk** %currentboxessp, i64 1
    %nextnewboxessp = getelementptr %Stk*, %Stk** %currentnewboxessp, i64 1
    br label %comp
done:
    %newboxessp = bitcast %Stk** %currentnewboxessp to %BoxesSp

    %newstk.0 = insertvalue %Stk undef, %Sp %newsp, 0
    %newstk.1 = insertvalue %Stk %newstk.0, %Base %newbase, 1
    %newstk.2 = insertvalue %Stk %newstk.1, %Limit %newlimit, 2
    %newstk.3 = insertvalue %Stk %newstk.2, %BoxesSp %newboxessp, 3
    %newstk.4 = insertvalue %Stk %newstk.3, %BoxesBase %newboxesbase, 4
    %newstk.5 = insertvalue %Stk %newstk.4, %BoxesLimit %newboxeslimit, 5
    %newstk.6 = insertvalue %Stk %newstk.5, %MStk null, 6

    %newstkp = call fastcc %Stk* @newStack()
    store %Stk %newstk.6, %Stk* %newstkp
    ret %Stk* %newstkp
}

define fastcc void @eraseStack(%Stk* %stkp) alwaysinline {
entry:
    %stk = load %Stk, %Stk* %stkp

    %baseuntyped = extractvalue %Stk %stk, 1
    %boxesspuntyped = extractvalue %Stk %stk, 3
    %boxesbaseuntyped = extractvalue %Stk %stk, 4

    %boxessp = bitcast %BoxesSp %boxesspuntyped to %Stk**
    %boxesbase = bitcast %BoxesBase %boxesbaseuntyped to %Stk**

    br label %comp
comp:
    %currentboxessp = phi %Stk** [%boxessp, %entry], [%newboxessp, %loop]
    %atbase = icmp eq %Stk** %currentboxessp, %boxesbase
    br i1 %atbase, label %done, label %loop
loop:
    %newboxessp = getelementptr %Stk*, %Stk** %currentboxessp, i64 -1
    %nextstk = load %Stk*, %Stk** %newboxessp
    call fastcc void @eraseStack(%Stk* %nextstk)
    br label %comp
done:
    %stkuntyped = bitcast %Stk* %stkp to i8*
    call void @free(i8* %stkuntyped)
    call void @free(i8* %baseuntyped)
    call void @free(i8* %boxesbaseuntyped)
    ret void
}

define fastcc %Sp @growStack(%Sp %incedsp, %Sp* %spp, %Base* %basep, %Limit* %limitp) alwaysinline {
entry:
    %sp = load %Sp, %Sp* %spp
    %limit = load %Limit, %Limit* %limitp

    %intlimit = ptrtoint %Limit %limit to i64
    %intincedsp = ptrtoint %Sp %incedsp to i64

    %overflow = icmp ugt i64 %intincedsp, %intlimit
    br i1 %overflow, label %grow, label %done
grow:
    %base = load %Base, %Base* %basep

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
    store %Base %newbase, %Base* %basep
    store %Limit %newlimit, %Limit* %limitp

    br label %done
done:
    %incedsp.1 = phi %Sp [%incedsp, %entry], [%newincedsp, %grow]
    %sp.1 = phi %Sp [%sp, %entry], [%newsp, %grow]
    store %Sp %incedsp.1, %Sp* %spp
    ret %Sp %sp.1
}

; RTS initialization

define fastcc void @topLevel(%Sp noalias %sp, i64 %res) {
    %base = load %Base, %Base* @base
    %boxesbase = load %BoxesBase, %BoxesBase* @boxesbase
    call void @free(i8* %base)
    call void @free(i8* %boxesbase)
    ret void
}

; Primitive Types

%Int = type i64

%Boolean = type i1

%Unit = type i64

%Evi = type i64

