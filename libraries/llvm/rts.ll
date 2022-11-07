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

; Manages memory for mutable variables
%RegionVal = type { %Mem }

; Fixed pointer used by references
%Region = type %RegionVal*

; This is used for two purposes:
;   - a refied first-class stack (then the last field is null)
;   - as part of an intrusive linked-list of stacks (meta stack)
; This contains pointers for the stack and the arena for the current region.
%StkVal = type { %Rc, %Mem, %Region, %StkVal* }

; The "meta" stack (a stack of stacks)
%Stk = type %StkVal*

; Stack outside of the meta stack.
; All copies of a stack must point to the same region,
; so we store updated pointers separately.
%DetachedStk = type { %Stk, %RegionVal }

; Positive data types consist of a (type-local) tag and a heap object
%Pos = type {i64, %Obj}
%Boolean = type %Pos
%Unit = type %Pos

; Negative types (codata) consist of a vtable and a heap object
%Neg = type {void (%Obj, %Env, %Sp)*, %Obj}

; Pointer types consist of a region and the offset from the arena base
%Ref = type { %Region, i64 }

; Global locations

@base = private global %Base null
@limit = private global %Limit null
@rest = private global %Stk undef
@region = private global %Region undef


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
define %Ref @alloc(i64 %size, %Region %region) alwaysinline {
    %spp = getelementptr %RegionVal, %Region %region, i64 0, i32 0, i32 0
    %sp = load %Sp, %Sp* %spp
    %basep = getelementptr %RegionVal, %Region %region, i64 0, i32 0, i32 1
    %base = load %Base, %Base* %basep

    %newsp = getelementptr i8, i8* %sp, i64 %size
    store %Sp %newsp, %Sp* %spp

    %spval = ptrtoint %Sp %sp to i64
    %baseval = ptrtoint %Base %base to i64
    %offset = sub i64 %spval, %baseval

    %ref.0 = insertvalue %Ref undef, %Region %region, 0
    %ref.1 = insertvalue %Ref %ref.0, i64 %offset, 1
    ret %Ref %ref.1
}

define i8* @getPtr(%Ref %ref) alwaysinline {
    %region = extractvalue %Ref %ref, 0
    %offset = extractvalue %Ref %ref, 1
    %basep = getelementptr %RegionVal, %Region %region, i64 0, i32 0, i32 1
    %base = load %Base, %Base* %basep
    %ptr = getelementptr i8, %Base %base, i64 %offset
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

define %DetachedStk @newStack() alwaysinline {

    ; TODO find actual size of stack
    %stkmem = call i8* @malloc(i64 64)
    %stk = bitcast i8* %stkmem to %Stk

    %regionmem = call i8* @malloc(i64 24)
    %region = bitcast i8* %regionmem to %Region

    ; TODO initialize to zero and grow later
    %stackmem = call %Mem @newMem()

    %arena = call %Mem @newMem()
    %regionval = insertvalue %RegionVal undef, %Mem %arena, 0
    ; store %RegionVal %regionval, %Region %region

    %stk.0 = insertvalue %StkVal undef, %Rc 0, 0
    %stk.1 = insertvalue %StkVal %stk.0, %Mem %stackmem, 1
    %stk.2 = insertvalue %StkVal %stk.1, %Region %region, 2
    %stk.3 = insertvalue %StkVal %stk.2, %Stk null, 3

    store %StkVal %stk.3, %Stk %stk

    %dstk.0 = insertvalue %DetachedStk undef, %Stk %stk, 0
    %dstk.1 = insertvalue %DetachedStk %dstk.0, %RegionVal %regionval, 1

    ret %DetachedStk %dstk.1
}

define %Sp @pushStack(%DetachedStk %dstk, %Sp %oldsp) alwaysinline {

    %stk = extractvalue %DetachedStk %dstk, 0
    %regionval = extractvalue %DetachedStk %dstk, 1

    %stkrc = getelementptr %StkVal, %Stk %stk, i64 0, i32 0
    ; assert %stkrc == null
    %stksp = getelementptr %StkVal, %Stk %stk, i64 0, i32 1, i32 0
    %stkbase = getelementptr %StkVal, %Stk %stk, i64 0, i32 1, i32 1
    %stklimit = getelementptr %StkVal, %Stk %stk, i64 0, i32 1, i32 2
    %stkregion = getelementptr %StkVal, %Stk %stk, i64 0, i32 2
    %stkrest = getelementptr %StkVal, %Stk %stk, i64 0, i32 3

    %newsp = load %Sp, %Sp* %stksp
    %newbase = load %Base, %Base* %stkbase
    %newlimit = load %Limit, %Limit* %stklimit
    %newregion = load %Region, %Region* %stkregion
    ; %newrest = load %Stk, %Stk* %stkrest
    ; assert %newrest == null

    %oldbase = load %Base, %Base* @base
    %oldlimit = load %Limit, %Limit* @limit
    %oldrest = load %Stk, %Stk* @rest
    %oldregion = load %Region, %Region* @region

    store %Base %newbase, %Base* @base
    store %Limit %newlimit, %Limit* @limit
    store %Region %newregion, %Region* @region
    store %Stk %stk, %Stk* @rest

    store %Rc 0, %Rc* %stkrc
    store %Sp %oldsp, %Sp* %stksp
    store %Base %oldbase, %Base* %stkbase
    store %Limit %oldlimit, %Limit* %stklimit
    store %Region %oldregion, %Region* %stkregion
    store %Stk %oldrest, %Stk* %stkrest

    store %RegionVal %regionval, %Region %newregion

    ret %Sp %newsp
}

define {%DetachedStk, %Sp} @popStack(%Sp %oldsp) alwaysinline {

    %stk = load %StkVal*, %StkVal** @rest

    %stkrc = getelementptr %StkVal, %Stk %stk, i64 0, i32 0
    %stksp = getelementptr %StkVal, %Stk %stk, i64 0, i32 1, i32 0
    %stkbase = getelementptr %StkVal, %Stk %stk, i64 0, i32 1, i32 1
    %stklimit = getelementptr %StkVal, %Stk %stk, i64 0, i32 1, i32 2
    %stkregion = getelementptr %StkVal, %Stk %stk, i64 0, i32 2
    %stkrest = getelementptr %StkVal, %Stk %stk, i64 0, i32 3

    ; %newrc = load %Rc, %Rc* %stkrc
    %newsp = load %Sp, %Sp* %stksp
    %newbase = load %Base, %Base* %stkbase
    %newlimit = load %Limit, %Limit* %stklimit
    %newregion = load %Region, %Region* %stkregion
    %newrest = load %Stk, %Stk* %stkrest

    %oldbase = load %Base, %Base* @base
    %oldlimit = load %Limit, %Limit* @limit
    %oldregion = load %Region, %Region* @region
    ; %oldrest = load %Stk, %Stk* @rest

    store %Base %newbase, %Base* @base
    store %Limit %newlimit, %Limit* @limit
    store %Region %newregion, %Region* @region
    store %Stk %newrest, %Stk* @rest

    store %Rc 0, %Rc* %stkrc
    store %Sp %oldsp, %Sp* %stksp
    store %Base %oldbase, %Base* %stkbase
    store %Limit %oldlimit, %Limit* %stklimit
    store %Region %oldregion, %Region* %stkregion
    store %Stk null, %Stk* %stkrest

    %oldregionval = load %RegionVal, %Region %oldregion
    %ret.0 = insertvalue {%DetachedStk, %Sp} undef, %Stk %stk, 0, 0
    %ret.1 = insertvalue {%DetachedStk, %Sp} %ret.0, %RegionVal %oldregionval, 0, 1
    %ret.2 = insertvalue {%DetachedStk, %Sp} %ret.1, %Sp %newsp, 1

    ret {%DetachedStk, %Sp} %ret.2
}

define %Sp @underflowStack(%Sp %sp) alwaysinline {
    %stk = load %Stk, %Stk* @rest
    %region = load %Region, %Region* @region
    %arenabase = getelementptr %RegionVal, %Region %region, i64 0, i32 0, i32 1
    %arenaptr = load %Base, %Base* %arenabase

    %stkrc = getelementptr %StkVal, %Stk %stk, i64 0, i32 0
    %stksp = getelementptr %StkVal, %Stk %stk, i64 0, i32 1, i32 0
    %stkbase = getelementptr %StkVal, %Stk %stk, i64 0, i32 1, i32 1
    %stklimit = getelementptr %StkVal, %Stk %stk, i64 0, i32 1, i32 2
    %stkregion = getelementptr %StkVal, %Stk %stk, i64 0, i32 2
    %stkrest = getelementptr %StkVal, %Stk %stk, i64 0, i32 3

    ; %newrc = load %Rc, %Rc* %stkrc
    %newsp = load %Sp, %Sp* %stksp
    %newbase = load %Base, %Base* %stkbase
    %newlimit = load %Limit, %Limit* %stklimit
    %newregion = load %Region, %Region* %stkregion
    %newrest = load %Stk, %Stk* %stkrest

    store %Base %newbase, %Base* @base
    store %Limit %newlimit, %Limit* @limit
    store %Region %newregion, %Region* @region
    store %Stk %newrest, %Stk* @rest

    %stkpuntyped = bitcast %Stk %stk to i8*
    call void @free(%Sp %sp)
    call void @free(%Base %arenaptr)
    call void @free(i8* %stkpuntyped)
    ret %Sp %newsp
}

define %DetachedStk @uniqueStack(%DetachedStk %dstk) alwaysinline {

    %stk = extractvalue %DetachedStk %dstk, 0

    %stkrc = getelementptr %StkVal, %Stk %stk, i64 0, i32 0
    %rc = load %Rc, %Rc* %stkrc
    switch %Rc %rc, label %next [%Rc 0, label %done]

    next:
    %stksp = getelementptr %StkVal, %Stk %stk, i64 0, i32 1, i32 0
    %stkbase = getelementptr %StkVal, %Stk %stk, i64 0, i32 1, i32 1
    %stklimit = getelementptr %StkVal, %Stk %stk, i64 0, i32 1, i32 2
    %stkregion = getelementptr %StkVal, %Stk %stk, i64 0, i32 2
    %stkrest = getelementptr %StkVal, %Stk %stk, i64 0, i32 3

    %sp = load %Sp, %Sp* %stksp
    %base = load %Base, %Base* %stkbase
    %limit = load %Limit, %Limit* %stklimit
    %region = load %Region, %Region* %stkregion
    %rest = load %Stk, %Stk* %stkrest

    %intsp = ptrtoint %Sp %sp to i64
    %intbase = ptrtoint %Base %base to i64
    %intlimit = ptrtoint %Limit %limit to i64
    %used = sub i64 %intsp, %intbase
    %size = sub i64 %intlimit, %intbase

    %arenasp = extractvalue %DetachedStk %dstk, 1, 0, 0
    %arenabase = extractvalue %DetachedStk %dstk, 1, 0, 1
    %arenalimit = extractvalue %DetachedStk %dstk, 1, 0, 2

    %intarenasp = ptrtoint %Sp %arenasp to i64
    %intarenabase = ptrtoint %Base %arenabase to i64
    %intarenalimit = ptrtoint %Limit %arenalimit to i64
    %arenaused = sub i64 %intsp, %intarenabase
    %arenasize = sub i64 %intlimit, %intarenabase

    %newstkmem = call i8* @malloc(i64 64)
    %newstk = bitcast i8* %newstkmem to %Stk
    %newstkrc = getelementptr %StkVal, %Stk %newstk, i64 0, i32 0
    %newstksp = getelementptr %StkVal, %Stk %newstk, i64 0, i32 1, i32 0
    %newstkbase = getelementptr %StkVal, %Stk %newstk, i64 0, i32 1, i32 1
    %newstklimit = getelementptr %StkVal, %Stk %newstk, i64 0, i32 1, i32 2
    %newstkregion = getelementptr %StkVal, %Stk %newstk, i64 0, i32 2
    %newstkrest = getelementptr %StkVal, %Stk %newstk, i64 0, i32 3

    %newbase = call i8* @malloc(i64 %size)
    %intnewbase = ptrtoint %Base %newbase to i64
    %intnewsp = add i64 %intnewbase, %used
    %intnewlimit = add i64 %intnewbase, %size
    %newsp = inttoptr i64 %intnewsp to %Sp
    %newlimit = inttoptr i64 %intnewlimit to %Limit

    %newarenabase = call i8* @malloc(i64 %arenasize)
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
    store %Region %region, %Region* %newstkregion
    store %Stk null, %Stk* %newstkrest

    %newoldrc = sub %Rc %rc, 1
    store %Rc %newoldrc, %Rc* %stkrc

    %dstk.0 = insertvalue %DetachedStk undef, %Stk %newstk, 0
    %dstk.1 = insertvalue %DetachedStk %dstk.0, %Sp %newarenasp, 1, 0, 0
    %dstk.2 = insertvalue %DetachedStk %dstk.1, %Base %newarenabase, 1, 0, 1
    %dstk.3 = insertvalue %DetachedStk %dstk.2, %Limit %newarenalimit, 1, 0, 2
    ret %DetachedStk %dstk.3

    done:
    ret %DetachedStk %dstk
}

define void @shareStack(%DetachedStk %dstk) alwaysinline {
    %stk = extractvalue %DetachedStk %dstk, 0
    %stkrc = getelementptr %StkVal, %Stk %stk, i64 0, i32 0
    %rc = load %Rc, %Rc* %stkrc
    %rc.1 = add %Rc %rc, 1
    store %Rc %rc.1, %Rc* %stkrc
    ret void
}

define void @eraseStack(%DetachedStk %dstk) alwaysinline {
    %stk = extractvalue %DetachedStk %dstk, 0
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

    %region = load %Region, %Region* @region
    %arenabaseptr = getelementptr %RegionVal, %Region %region, i64 0, i32 0, i32 1
    %arenabase = load %Base, %Base* %arenabaseptr
    call void @free(i8* %arenabase)

    %regionp = bitcast %Region %region to i8*
    call void @free(i8* %regionp)

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

define void @initRegion() alwaysinline {
    %arenabase = call i8* @malloc(i64 1024)
    %arenalimit = getelementptr i8, i8* %arenabase, i64 1024

    %mem.0 = insertvalue %RegionVal undef, %Sp %arenabase, 0, 0
    %mem.1 = insertvalue %RegionVal %mem.0, %Base %arenabase, 0, 1
    %mem.2 = insertvalue %RegionVal %mem.1, %Base %arenalimit, 0, 2

    %regionmem = call i8* @malloc(i64 24)
    %region = bitcast i8* %regionmem to %Region
    store %RegionVal %mem.2, %Region %region

    store %Region %region, %Region* @region

    ret void
}

; Primitive Types

%Int = type i64
%Double = type double

%Evi = type i64
