; Run-Time System

; Basic types

%Env = type i8*

; Reference counts
%Rc = type i64

; Code to share (bump rc) an environment
%Sharer = type void (%Env)*

; Code to drop an environment
%Eraser = type void (%Env)*

; Every heap object starts with a header
%Header = type {%Rc, %Eraser}

; A heap object is a pointer to a header followed by payload.
;
;   +--[ Header ]--+-------------+
;   | Rc  | Eraser | Payload ... |
;   +--------------+-------------+
%Obj = type %Header*


; A Frame has the following layout
;
;   +-------[ FrameHeader ]-----+--------------+
;   | RetAdr  | Sharer | Eraser | Payload ...  |
;   +---------------------------+--------------+

; A stack pointer points to the top-most frame followed by all other frames.
;
; For example
;
;     +--------------------+   <- Limit
;     :                    :
;     :                    :   <- Sp
;     +--------------------+
;     | FrameHeader        |
;     |    z               |
;     |    y               |
;     |    x               |
;     +--------------------+
;     | ... next frame ... |
;     +--------------------+
;     :        ...         :
;     +--------------------+ <- Base
%Sp = type i8*
%Base = type %Sp
%Limit = type %Sp
%RetAdr = type void (%Env, %Sp)*
%FrameHeader = type { %RetAdr, %Sharer, %Eraser }

; Pointers for a heap allocated stack
%Mem = type { %Sp, %Base, %Limit }

; This is used for two purposes:
;   - a refied first-class stack (then the last field is null)
;   - as part of an intrusive linked-list of stacks (meta stack)
; This contains pointers for the stack and the arena for the current region.
%StkVal = type { %Rc, %Mem, %Mem, %StkVal* }

; The "meta" stack (a stack of stacks)
%Stk = type %StkVal*

; Positive data types consist of a (type-local) tag and a heap object
%Pos = type {i64, %Obj}
%Boolean = type %Pos
%Unit = type %Pos

; Negative types (codata) consist of a vtable and a heap object
%Neg = type {void (%Obj, %Env, %Sp)*, %Obj}

; Pointer types consist of the offset from the arena base
%Ptr = type i64

; Global locations

@base = private global %Base null
@limit = private global %Limit null
@rest = private global %Stk undef
@arena = private global %Mem undef


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


; Arena management
define %Ptr @alloc(i64 %size) alwaysinline {
    %spp = getelementptr %Mem, %Mem* @arena, i64 0, i32 0
    %sp = load %Sp, %Sp* %spp
    %basep = getelementptr %Mem, %Mem* @arena, i64 0, i32 1
    %base = load %Base, %Base* %basep

    %newsp = getelementptr i8, i8* %sp, i64 %size
    store %Sp %newsp, %Sp* %spp

    %spval = ptrtoint %Sp %sp to i64
    %baseval = ptrtoint %Base %base to i64
    %offset = sub i64 %spval, %baseval
    ret %Ptr %offset
}

define i8* @getPtr(%Ptr %offset) alwaysinline {
    %basep = getelementptr %Mem, %Mem* @arena, i64 0, i32 1
    %base = load %Base, %Base* %basep
    %ptr = getelementptr i8, %Base %base, %Ptr %offset
    ret i8* %ptr
}


; Meta-stack management

define %Mem @newMem() alwaysinline {
    %sp = call %Sp @malloc(i64 1024)
    %limit = getelementptr i8, i8* %sp, i64 1024

    %mem.0 = insertvalue %Mem undef, %Sp %sp, 0
    %mem.1 = insertvalue %Mem %mem.0, %Base %sp, 1
    %mem.2 = insertvalue %Mem %mem.1, %Limit %limit, 2

    ret %Mem %mem.2
}

define %Stk @newStack() alwaysinline {

    ; TODO find actual size of stack
    %stkmem = call i8* @malloc(i64 64)
    %stk = bitcast i8* %stkmem to %Stk

    ; TODO initialize to zero and grow later
    %sp = call %Sp @malloc(i64 1024)
    %stackmem = call %Mem @newMem()
    %arenamem = call %Mem @newMem()

    %stk.0 = insertvalue %StkVal undef, %Rc 0, 0
    %stk.1 = insertvalue %StkVal %stk.0, %Mem %stackmem, 1
    %stk.2 = insertvalue %StkVal %stk.1, %Mem %arenamem, 2
    %stk.3 = insertvalue %StkVal %stk.2, %Stk null, 3

    store %StkVal %stk.3, %Stk %stk

    ret %Stk %stk
}

define %Sp @pushStack(%Stk %stk, %Sp %oldsp) alwaysinline {

    %stkrc = getelementptr %StkVal, %Stk %stk, i64 0, i32 0
    ; assert %stkrc == null
    %stksp = getelementptr %StkVal, %Stk %stk, i64 0, i32 1, i32 0
    %stkbase = getelementptr %StkVal, %Stk %stk, i64 0, i32 1, i32 1
    %stklimit = getelementptr %StkVal, %Stk %stk, i64 0, i32 1, i32 2
    %stkarena = getelementptr %StkVal, %Stk %stk, i64 0, i32 2
    %stkrest = getelementptr %StkVal, %Stk %stk, i64 0, i32 3

    %newsp = load %Sp, %Sp* %stksp
    %newbase = load %Base, %Base* %stkbase
    %newlimit = load %Limit, %Limit* %stklimit
    %newarena = load %Mem, %Mem* %stkarena
    ; %newrest = load %Stk, %Stk* %stkrest
    ; assert %newrest == null

    %oldbase = load %Base, %Base* @base
    %oldlimit = load %Limit, %Limit* @limit
    %oldrest = load %Stk, %Stk* @rest
    %oldarena = load %Mem, %Mem* @arena

    store %Base %newbase, %Base* @base
    store %Limit %newlimit, %Limit* @limit
    store %Mem %newarena, %Mem* @arena
    store %Stk %stk, %Stk* @rest

    store %Rc 0, %Rc* %stkrc
    store %Sp %oldsp, %Sp* %stksp
    store %Base %oldbase, %Base* %stkbase
    store %Limit %oldlimit, %Limit* %stklimit
    store %Mem %oldarena, %Mem* %stkarena
    store %Stk %oldrest, %Stk* %stkrest

    ret %Sp %newsp
}

define {%StkVal*, %Sp} @popStack(%Sp %oldsp) alwaysinline {

    %stk = load %StkVal*, %StkVal** @rest

    %stkrc = getelementptr %StkVal, %Stk %stk, i64 0, i32 0
    %stksp = getelementptr %StkVal, %Stk %stk, i64 0, i32 1, i32 0
    %stkbase = getelementptr %StkVal, %Stk %stk, i64 0, i32 1, i32 1
    %stklimit = getelementptr %StkVal, %Stk %stk, i64 0, i32 1, i32 2
    %stkarena = getelementptr %StkVal, %Stk %stk, i64 0, i32 2
    %stkrest = getelementptr %StkVal, %Stk %stk, i64 0, i32 3

    ; %newrc = load %Rc, %Rc* %stkrc
    %newsp = load %Sp, %Sp* %stksp
    %newbase = load %Base, %Base* %stkbase
    %newlimit = load %Limit, %Limit* %stklimit
    %newarena = load %Mem, %Mem* %stkarena
    %newrest = load %Stk, %Stk* %stkrest

    %oldbase = load %Base, %Base* @base
    %oldlimit = load %Limit, %Limit* @limit
    %oldarena = load %Mem, %Mem* @arena
    ; %oldrest = load %Stk, %Stk* @rest

    store %Base %newbase, %Base* @base
    store %Limit %newlimit, %Limit* @limit
    store %Mem %newarena, %Mem* @arena
    store %Stk %newrest, %Stk* @rest

    store %Rc 0, %Rc* %stkrc
    store %Sp %oldsp, %Sp* %stksp
    store %Base %oldbase, %Base* %stkbase
    store %Limit %oldlimit, %Limit* %stklimit
    store %Mem %oldarena, %Mem* %stkarena
    store %Stk null, %Stk* %stkrest

    %ret.0 = insertvalue {%StkVal*, %Sp} undef, %Stk %stk, 0
    %ret.1 = insertvalue {%StkVal*, %Sp} %ret.0, %Sp %newsp, 1

    ret {%StkVal*, %Sp} %ret.1
}

define %Sp @underflowStack(%Sp %sp) alwaysinline {
    %stk = load %Stk, %Stk* @rest
    %arenabase = getelementptr %Mem, %Mem* @arena, i64 0, i32 1
    %arenaptr = load %Base, %Base* %arenabase

    %stkrc = getelementptr %StkVal, %Stk %stk, i64 0, i32 0
    %stksp = getelementptr %StkVal, %Stk %stk, i64 0, i32 1, i32 0
    %stkbase = getelementptr %StkVal, %Stk %stk, i64 0, i32 1, i32 1
    %stklimit = getelementptr %StkVal, %Stk %stk, i64 0, i32 1, i32 2
    %stkarena = getelementptr %StkVal, %Stk %stk, i64 0, i32 2
    %stkrest = getelementptr %StkVal, %Stk %stk, i64 0, i32 3

    ; %newrc = load %Rc, %Rc* %stkrc
    %newsp = load %Sp, %Sp* %stksp
    %newbase = load %Base, %Base* %stkbase
    %newlimit = load %Limit, %Limit* %stklimit
    %newarena = load %Mem, %Mem* %stkarena
    %newrest = load %Stk, %Stk* %stkrest

    store %Base %newbase, %Base* @base
    store %Limit %newlimit, %Limit* @limit
    store %Mem %newarena, %Mem* @arena
    store %Stk %newrest, %Stk* @rest

    %stkpuntyped = bitcast %Stk %stk to i8*
    call void @free(%Sp %sp)
    call void @free(%Base %arenaptr)
    call void @free(i8* %stkpuntyped)
    ret %Sp %newsp
}

define %Stk @uniqueStack(%Stk %stk) alwaysinline {

    %stkrc = getelementptr %StkVal, %Stk %stk, i64 0, i32 0
    %rc = load %Rc, %Rc* %stkrc
    switch %Rc %rc, label %next [%Rc 0, label %done]

    next:
    %stksp = getelementptr %StkVal, %Stk %stk, i64 0, i32 1, i32 0
    %stkbase = getelementptr %StkVal, %Stk %stk, i64 0, i32 1, i32 1
    %stklimit = getelementptr %StkVal, %Stk %stk, i64 0, i32 1, i32 2
    %stkarenasp = getelementptr %StkVal, %Stk %stk, i64 0, i32 2, i32 0
    %stkarenabase = getelementptr %StkVal, %Stk %stk, i64 0, i32 2, i32 1
    %stkarenalimit = getelementptr %StkVal, %Stk %stk, i64 0, i32 2, i32 2
    %stkrest = getelementptr %StkVal, %Stk %stk, i64 0, i32 3

    %sp = load %Sp, %Sp* %stksp
    %base = load %Base, %Base* %stkbase
    %limit = load %Limit, %Limit* %stklimit
    %arenasp = load %Sp, %Sp* %stkarenasp
    %arenabase = load %Base, %Base* %stkarenabase
    %arenalimit = load %Limit, %Limit* %stkarenalimit
    %rest = load %Stk, %Stk* %stkrest

    %intsp = ptrtoint %Sp %sp to i64
    %intbase = ptrtoint %Base %base to i64
    %intlimit = ptrtoint %Limit %limit to i64
    %used = sub i64 %intsp, %intbase
    %size = sub i64 %intlimit, %intbase

    %intarenasp = ptrtoint %Sp %arenasp to i64
    %intarenabase = ptrtoint %Base %arenabase to i64
    %intarenalimit = ptrtoint %Limit %arenalimit to i64
    %arenaused = sub i64 %intsp, %intarenabase
    %arenasize = sub i64 %intlimit, %intarenabase

    %newstk = call %Stk @newStack()
    %newstkrc = getelementptr %StkVal, %Stk %newstk, i64 0, i32 0
    %newstksp = getelementptr %StkVal, %Stk %newstk, i64 0, i32 1, i32 0
    %newstkbase = getelementptr %StkVal, %Stk %newstk, i64 0, i32 1, i32 1
    %newstklimit = getelementptr %StkVal, %Stk %newstk, i64 0, i32 1, i32 2
    %newstkarenasp = getelementptr %StkVal, %Stk %newstk, i64 0, i32 2, i32 0
    %newstkarenabase = getelementptr %StkVal, %Stk %newstk, i64 0, i32 2, i32 1
    %newstkarenalimit = getelementptr %StkVal, %Stk %newstk, i64 0, i32 2, i32 2
    %newstkrest = getelementptr %StkVal, %Stk %newstk, i64 0, i32 3

    %newbase = load %Base, %Base* %newstkbase
    %intnewbase = ptrtoint %Base %newbase to i64
    %intnewsp = add i64 %intnewbase, %used
    %intnewlimit = add i64 %intnewbase, %size
    %newsp = inttoptr i64 %intnewsp to %Sp
    %newlimit = inttoptr i64 %intnewlimit to %Limit

    %newarenabase = load %Base, %Base* %newstkarenabase
    %intnewarenabase = ptrtoint %Base %newarenabase to i64
    %intnewarenasp = add i64 %intnewarenabase, %arenaused
    %intnewarenalimit = add i64 %intnewarenabase, %arenasize
    %newarenasp = inttoptr i64 %intnewarenasp to %Sp
    %newarenalimit = inttoptr i64 %intnewarenalimit to %Limit

    call void @memcpy(i8* %newbase, i8* %base, i64 %used)
    call void @memcpy(i8* %newarenabase, i8* %arenabase, i64 %arenaused)
    call fastcc void @shareFrames(%Sp %newsp)

    store %Rc 0, %Rc* %stkrc
    store %Sp %newsp, %Sp* %newstksp
    store %Base %newbase, %Base* %newstkbase
    store %Limit %newlimit, %Limit* %newstklimit
    store %Sp %newarenasp, %Sp* %newstkarenasp
    store %Base %newarenabase, %Base* %newstkarenabase
    store %Limit %newarenalimit, %Limit* %newstkarenalimit
    store %Stk null, %Stk* %newstkrest

    %newoldrc = sub %Rc %rc, 1
    store %Rc %newoldrc, %Rc* %stkrc

    ret %Stk %newstk

    done:
    ret %Stk %stk
}

define void @shareStack(%Stk %stk) alwaysinline {
    %stkrc = getelementptr %StkVal, %Stk %stk, i64 0, i32 0
    %rc = load %Rc, %Rc* %stkrc
    %rc.1 = add %Rc %rc, 1
    store %Rc %rc.1, %Rc* %stkrc
    ret void
}

define void @eraseStack(%Stk %stk) alwaysinline {
    %stkrc = getelementptr %StkVal, %Stk %stk, i64 0, i32 0
    %rc = load %Rc, %Rc* %stkrc
    switch %Rc %rc, label %decr [%Rc 0, label %free]

    decr:
    %rc.1 = sub %Rc %rc, 1
    store %Rc %rc.1, %Rc* %stkrc
    ret void

    free:
    %stksp = getelementptr %StkVal, %Stk %stk, i64 0, i32 1, i32 0
    %sp = load %Sp, %Sp* %stksp
    call fastcc void @eraseFrames(%Sp %sp)
    %stkuntyped = bitcast %Stk %stk to i8*
    call void @free(i8* %stkuntyped)
    ret void
}

define fastcc void @shareFrames(%Sp %sp) alwaysinline {
    %sptyped = bitcast %Sp %sp to %FrameHeader*
    %newsptyped = getelementptr %FrameHeader, %FrameHeader* %sptyped, i64 -1
    %stksharer = getelementptr %FrameHeader, %FrameHeader* %newsptyped, i64 0, i32 1
    %sharer = load %Sharer, %Sharer* %stksharer
    %newsp = bitcast %FrameHeader* %newsptyped to %Sp
    tail call fastcc void %sharer(%Sp %newsp)
    ret void
}

define fastcc void @eraseFrames(%Sp %sp) alwaysinline {
    %sptyped = bitcast %Sp %sp to %FrameHeader*
    %newsptyped = getelementptr %FrameHeader, %FrameHeader* %sptyped, i64 -1
    %stkeraser = getelementptr %FrameHeader, %FrameHeader* %newsptyped, i64 0, i32 2
    %eraser = load %Eraser, %Eraser* %stkeraser
    %newsp = bitcast %FrameHeader* %newsptyped to %Sp
    tail call fastcc void %eraser(%Sp %newsp)
    ret void
}

; RTS initialization

define fastcc void @topLevel(%Env %env, %Sp noalias %sp) {
    %base = load %Base, %Base* @base
    call void @free(i8* %base)
    call void @free(i8* %env)
    ret void
}

define fastcc void @topLevelSharer(%Env %env) {
    ; TODO this should never be called
    ret void
}

define fastcc void @topLevelEraser(%Env %env) {
    ; TODO this should never be called
    ret void
}

; Primitive Types

%Int = type i64
%Double = type double

%Evi = type i64
