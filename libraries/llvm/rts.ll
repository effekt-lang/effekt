; Run-Time System

; Basic types

%Env = type ptr

; Reference counts
%Rc = type i64

; Code to share (bump rc) an environment
%Sharer = type ptr

; Code to drop an environment
%Eraser = type ptr

; Every heap object starts with a header
%Header = type {%Rc, %Eraser}

; A heap object is a pointer to a header followed by payload.
;
;   +--[ Header ]--+-------------+
;   | Rc  | Eraser | Payload ... |
;   +--------------+-------------+
%Obj = type ptr


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
%Sp = type ptr
%Base = type %Sp
%Limit = type %Sp
%RetAdr = type ptr
%FrameHeader = type { %RetAdr, %Sharer, %Eraser }

; Pointers for a heap allocated stack
%Mem = type { %Sp, %Base, %Limit }

%Bounds = type { %Base, %Limit }

; The garbage collector differentiates three groups of types:
; - Values (Integer, Double)
; - Objects (Positive, Negative)
; - Strings
; For each group we have a linked list of arenas where mutable state is allocated.
; We also store pointers to the first non-full arena for faster allocation.
;
; +---------+          +---------+           +---------+
; | Bounds  | -------> | Bounds  | --------> | Bounds  | ---------> ...
; +---------+          +---------+           +---------+
; | data... |          | data... |           | no data |
; +---------+          +---------+           +---------+
;     ^                     ^
;     |                     |
;   Bounds                 Mem
;
; In addition, the total size of used memory is tracked for backup allocation
; (excluding the pointers to the following arena)
%RegionVal = type { [ 3 x { %Mem, %Bounds } ], i64 }

; Fixed pointer used to allocate states
%Region = type ptr

; A region backup stores the size followed by data for each type group.
;
;   +------+-------------+------+-------------+------+-------------+
;   | Size | Payload ... | Size | Payload ... | Size | Payload ... |
;   +------+-------------+------+-------------+------+-------------+
%RegionBackup = type ptr

; This is used for two purposes:
;   - a refied first-class stack (then the last field is null)
;   - as part of an intrusive linked-list of stacks (meta stack)
%StkVal = type { %Rc, %Mem, %Region, %RegionBackup, ptr }

; The "meta" stack (a stack of stacks)
%Stk = type ptr

; Positive data types consist of a (type-local) tag and a heap object
%Pos = type {i64, %Obj}
%Boolean = type %Pos
%Unit = type %Pos

; Negative types (codata) consist of a vtable and a heap object
%Neg = type {ptr, %Obj}

%Ref = type ptr

; Global locations

@base = private global %Base null
@limit = private global %Limit null
@region = private global %Region undef
@rest = private global %Stk undef


define %StkVal @getStk(%Sp %sp) alwaysinline {
    %base = load %Base, ptr @base
    %limit = load %Limit, ptr @limit
    %region = load %Region, ptr @region
    %rest = load %Stk, ptr @rest

    %stk.0 = insertvalue %StkVal undef, %Rc 0, 0
    %stk.1 = insertvalue %StkVal %stk.0, %Sp %sp, 1, 0
    %stk.2 = insertvalue %StkVal %stk.1, %Base %base, 1, 1
    %stk.3 = insertvalue %StkVal %stk.2, %Limit %limit, 1, 2
    %stk.4 = insertvalue %StkVal %stk.3, %Region %region, 2
    %stk.5 = insertvalue %StkVal %stk.4, %RegionBackup null, 3
    %stk.6 = insertvalue %StkVal %stk.5, %Stk %rest, 4

    ret %StkVal %stk.6
}

define void @setStk(%StkVal %stk) alwaysinline {
    %base = extractvalue %StkVal %stk, 1, 1
    %limit = extractvalue %StkVal %stk, 1, 2
    %region = extractvalue %StkVal %stk, 2
    %rest = extractvalue %StkVal %stk, 4

    store %Base %base, ptr @base
    store %Limit %limit, ptr @limit
    store %Region %region, ptr @region
    store %Stk %rest, ptr @rest
    ret void
}

; Foreign imports

declare ptr @malloc(i64)
declare void @free(ptr)
declare ptr @realloc(ptr, i64)
declare void @memcpy(ptr, ptr, i64)
declare i64 @llvm.ctlz.i64 (i64 , i1)
declare i64 @llvm.fshr.i64(i64, i64, i64)
declare void @print(i64)
declare void @exit(i64)

; Garbage collection

define %Obj @newObject(%Eraser %eraser, i64 %envsize) alwaysinline {
    ; This magical 16 is the size of the object header
    %size = add i64 %envsize, 16
    %obj = call ptr @malloc(i64 %size)
    %objrc = getelementptr %Header, ptr %obj, i64 0, i32 0
    %objeraser = getelementptr %Header, ptr %obj, i64 0, i32 1
    store %Rc 0, ptr %objrc
    store %Eraser %eraser, ptr %objeraser
    ret %Obj %obj
}

define %Env @objectEnvironment(%Obj %obj) alwaysinline {
    ; Environment is stored right after header
    %env = getelementptr %Header, ptr %obj, i64 1
    ret %Env %env
}

define void @shareObject(%Obj %obj) alwaysinline {
    %isnull = icmp eq %Obj %obj, null
    br i1 %isnull, label %done, label %next

    next:
    %objrc = getelementptr %Header, ptr %obj, i64 0, i32 0
    %rc = load %Rc, ptr %objrc
    %rc.1 = add %Rc %rc, 1
    store %Rc %rc.1, ptr %objrc
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
    %objrc = getelementptr %Header, ptr %obj, i64 0, i32 0
    %rc = load %Rc, ptr %objrc
    switch %Rc %rc, label %decr [%Rc 0, label %free]

    decr:
    %rc.1 = sub %Rc %rc, 1
    store %Rc %rc.1, ptr %objrc
    ret void

    free:
    %objeraser = getelementptr %Header, ptr %obj, i64 0, i32 1
    %eraser = load %Eraser, ptr %objeraser
    %env = call %Env @objectEnvironment(%Obj %obj)
    call fastcc void %eraser(%Env %env)
    call void @free(%Obj %obj)
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
define %Ref @alloc(i8 %idx, %Region %region) alwaysinline {
    %spp = getelementptr %RegionVal, %Region %region, i64 0, i32 0, i8 %idx, i32 0, i32 0
    %basep = getelementptr %RegionVal, %Region %region, i64 0, i32 0, i8 %idx, i32 0, i32 1
    %limitp = getelementptr %RegionVal, %Region %region, i64 0, i32 0, i8 %idx, i32 0, i32 2
    %sizep = getelementptr %RegionVal, %Region %region, i64 0, i32 1

    %sp = load %Sp, ptr %spp
    %base = load %Base, ptr %basep
    %limit = load %Limit, ptr %limitp
    %regionsize = load i64, ptr %sizep

    %obj = icmp ne i8 %idx, 0
    %size = select i1 %obj, i64 16, i64 8

    %newregionsize = add i64 %regionsize, %size
    store i64 %newregionsize, ptr %sizep

    %noarena = icmp eq %Base %base, null
    br i1 %noarena, label %makearena, label %checkspace

checkspace:
    %nextsp = getelementptr i8, %Sp %sp, i64 %size

    %cmp = icmp ule %Sp %nextsp, %limit
    br i1 %cmp, label %continue, label %checknext

continue:
    store %Sp %nextsp, ptr %spp
    ret %Ref %sp

makearena:
    %firstbasep = getelementptr %RegionVal, %Region %region, i64 0, i32 0, i8 %idx, i32 1, i32 0
    %firstlimitp = getelementptr %RegionVal, %Region %region, i64 0, i32 0, i8 %idx, i32 1, i32 1
    br label %newarena

checknext:
    %nextbasep = getelementptr %Bounds, %Base %base, i64 0, i32 0
    %nextlimitp = getelementptr %Bounds, %Base %base, i64 0, i32 1
    %checkbase = load %Base, ptr %nextbasep
    %checklimit = load %Limit, ptr %nextlimitp
    %isnull = icmp eq %Base %checkbase, null
    br i1 %isnull, label %newarena, label %nextarena

newarena:
    %newbasep = phi ptr [%firstbasep, %makearena], [%nextbasep, %checknext]
    %newlimitp = phi ptr [%firstlimitp, %makearena], [%nextlimitp, %checknext]
    %newbase = call ptr @malloc(i64 1024)
    %newlimit = getelementptr i8, %Base %newbase, i64 1024
    store %Bounds zeroinitializer, %Base %newbase
    store %Base %newbase, ptr %newbasep
    store %Limit %newlimit, ptr %newlimitp
    br label %nextarena

nextarena:
    %nextbase = phi %Base [%checkbase, %checknext], [%newbase, %newarena]
    %nextlimit = phi %Limit [%checklimit, %checknext], [%newlimit, %newarena]
    %ref = getelementptr %Bounds, %Base %nextbase, i64 1
    %newsp = getelementptr i8, %Sp %ref, i64 %size

    store %Sp %newsp, ptr %spp
    store %Base %nextbase, ptr %basep
    store %Limit %nextlimit, ptr %limitp

    ret %Ref %ref
}


; Meta-stack management

define %Mem @newMem() alwaysinline {
    %sp = call %Sp @malloc(i64 1024)
    %limit = getelementptr i8, ptr %sp, i64 1024

    %mem.0 = insertvalue %Mem undef, %Sp %sp, 0
    %mem.1 = insertvalue %Mem %mem.0, %Base %sp, 1
    %mem.2 = insertvalue %Mem %mem.1, %Limit %limit, 2

    ret %Mem %mem.2
}

define %Stk @newStack() alwaysinline {

    ; TODO find actual size of stack
    %stk = call ptr @malloc(i64 56)

    %region = call ptr @malloc(i64 128)
    store %RegionVal zeroinitializer, %Region %region

    ; TODO initialize to zero and grow later
    %stackmem = call %Mem @newMem()

    %stk.0 = insertvalue %StkVal undef, %Rc 0, 0
    %stk.1 = insertvalue %StkVal %stk.0, %Mem %stackmem, 1
    %stk.2 = insertvalue %StkVal %stk.1, %Region %region, 2
    %stk.3 = insertvalue %StkVal %stk.2, %RegionBackup null, 3
    %stk.4 = insertvalue %StkVal %stk.3, %Stk null, 4

    store %StkVal %stk.4, %Stk %stk

    ret %Stk %stk
}

define %Mem @restoreBackup(%Bounds %bounds, ptr %backup, i64 %size) {
    %base = extractvalue %Bounds %bounds, 0
    %limit = extractvalue %Bounds %bounds, 1
    %data = getelementptr %Bounds, %Base %base, i64 1
    %intdata = ptrtoint ptr %data to i64
    %intlimit = ptrtoint %Limit %limit to i64
    %arenasize = sub i64 %intlimit, %intdata
    %final = icmp ule i64 %size, %arenasize
    br i1 %final, label %lastcopy, label %continue

lastcopy:
    call void @memcpy(ptr %data, ptr %backup, i64 %size)
    %sp = getelementptr i8, ptr %data, i64 %size
    %mem.0 = insertvalue %Mem undef, %Sp %sp, 0
    %mem.1 = insertvalue %Mem %mem.0, %Base %base, 1
    %mem.2 = insertvalue %Mem %mem.1, %Limit %limit, 2
    ret %Mem %mem.2

continue:
    call void @memcpy(ptr %data, ptr %backup, i64 %arenasize)
    %nextbounds = load %Bounds, %Base %base
    %nextbackup = getelementptr i8, ptr %backup, i64 %arenasize
    %restsize = sub i64 %size, %arenasize
    %mem = tail call %Mem @restoreBackup(%Bounds %nextbounds, ptr %nextbackup, i64 %restsize)
    ret %Mem %mem
}

define %Sp @pushStack(%Stk %stk, %Sp %oldsp) alwaysinline {

    %backupp = getelementptr %StkVal, %Stk %stk, i64 0, i32 3
    %backup = load %RegionBackup, ptr %backupp
    %isnull = icmp eq %RegionBackup %backup, null
    br i1 %isnull, label %push, label %restore

restore:
    %regionp = getelementptr %StkVal, %Stk %stk, i64 0, i32 2
    %region = load %Region, ptr %regionp

    br label %loop

loop:
    %i = phi i64 [0, %restore], [1, %loop]
    %ptr = phi ptr [%backup, %restore], [%nextptr, %loop]
    %fullsize.0 = phi i64 [0, %restore], [%fullsize, %loop]

    %size = load i64, ptr %ptr
    %fullsize = add i64 %fullsize.0, %size

    %backupdata = getelementptr i64, ptr %ptr, i64 1
    %nextptr = getelementptr i8, ptr %backupdata, i64 %size

    %boundsp = getelementptr %RegionVal, %Region %region, i64 0, i32 0, i64 %i, i32 1
    %bounds = load %Bounds, ptr %boundsp
    %mem = call %Mem @restoreBackup(%Bounds %bounds, ptr %backupdata, i64 %size)

    %memp = getelementptr %RegionVal, %Region %region, i64 0, i32 0, i64 %i, i32 0
    store %Mem %mem, ptr %memp

    %nexti = add i64 %i, 1
    %cmp = icmp ult i64 %nexti, 2
    br i1 %cmp, label %loop, label %storesize

storesize:
    %sizep = getelementptr %RegionVal, %Region %region, i64 0, i32 1
    store i64 %fullsize, ptr %sizep
    br label %push

push:
    %newstk.0 = load %StkVal, %Stk %stk
    %newstk.1 = insertvalue %StkVal %newstk.0, %Stk %stk, 4
    %newstk = insertvalue %StkVal %newstk.1, %RegionBackup null, 3

    %oldstk = call %StkVal @getStk(%Sp %oldsp)

    call void @setStk(%StkVal %newstk)

    store %StkVal %oldstk, %Stk %stk

    %newsp = extractvalue %StkVal %newstk, 1, 0
    ret %Sp %newsp
}

define {%Stk, %Sp} @popStack(%Sp %oldsp) alwaysinline {

    %oldstk = call %StkVal @getStk(%Sp %oldsp)

    %rest = extractvalue %StkVal %oldstk, 4
    %newstk = load %StkVal, %Stk %rest

    call void @setStk(%StkVal %newstk)

    store %StkVal %oldstk, %Stk %rest

    %newsp = extractvalue %StkVal %newstk, 1, 0
    %ret.0 = insertvalue {%Stk, %Sp} undef, %Stk %rest, 0
    %ret.1 = insertvalue {%Stk, %Sp} %ret.0, %Sp %newsp, 1

    ret {%Stk, %Sp} %ret.1
}

define %Sp @underflowStack(%Sp %sp) alwaysinline {
    %stk = load %Stk, ptr @rest
    %newstk = load %StkVal, %Stk %stk

    %region = load %Region, ptr @region

    call void @setStk(%StkVal %newstk)

    call void @free(%Sp %sp)
    call void @free(%Stk %stk)

    %newsp = extractvalue %StkVal %newstk, 1, 0
    ret %Sp %newsp
}

define %Stk @uniqueStack(%Stk %stk) alwaysinline {

entry:
    %stkrc = getelementptr %StkVal, %Stk %stk, i64 0, i32 0
    %rc = load %Rc, ptr %stkrc
    switch %Rc %rc, label %copy [%Rc 0, label %done]

copy:
    %stksp = getelementptr %StkVal, %Stk %stk, i64 0, i32 1, i32 0
    %stkbase = getelementptr %StkVal, %Stk %stk, i64 0, i32 1, i32 1
    %stklimit = getelementptr %StkVal, %Stk %stk, i64 0, i32 1, i32 2
    %stkregion = getelementptr %StkVal, %Stk %stk, i64 0, i32 2
    %stkbackup = getelementptr %StkVal, %Stk %stk, i64 0, i32 3
    %stkrest = getelementptr %StkVal, %Stk %stk, i64 0, i32 4

    %sp = load %Sp, ptr %stksp
    %base = load %Base, ptr %stkbase
    %limit = load %Limit, ptr %stklimit
    %region = load %Region, ptr %stkregion
    %backup = load %RegionBackup, ptr %stkbackup
    %rest = load %Stk, ptr %stkrest

    %intsp = ptrtoint %Sp %sp to i64
    %intbase = ptrtoint %Base %base to i64
    %intlimit = ptrtoint %Limit %limit to i64
    %used = sub i64 %intsp, %intbase
    %size = sub i64 %intlimit, %intbase

    %newstk = call ptr @malloc(i64 56)
    %newstkrc = getelementptr %StkVal, %Stk %newstk, i64 0, i32 0
    %newstksp = getelementptr %StkVal, %Stk %newstk, i64 0, i32 1, i32 0
    %newstkbase = getelementptr %StkVal, %Stk %newstk, i64 0, i32 1, i32 1
    %newstklimit = getelementptr %StkVal, %Stk %newstk, i64 0, i32 1, i32 2
    %newstkregion = getelementptr %StkVal, %Stk %newstk, i64 0, i32 2
    %newstkbackup = getelementptr %StkVal, %Stk %newstk, i64 0, i32 3
    %newstkrest = getelementptr %StkVal, %Stk %newstk, i64 0, i32 4

    %newbase = call ptr @malloc(i64 %size)
    %intnewbase = ptrtoint %Base %newbase to i64
    %intnewsp = add i64 %intnewbase, %used
    %intnewlimit = add i64 %intnewbase, %size
    %newsp = inttoptr i64 %intnewsp to %Sp
    %newlimit = inttoptr i64 %intnewlimit to %Limit

    call void @memcpy(ptr %newbase, ptr %base, i64 %used)
    call fastcc void @shareFrames(%Sp %newsp)

    store %Rc 0, ptr %newstkrc
    store %Sp %newsp, ptr %newstksp
    store %Base %newbase, ptr %newstkbase
    store %Limit %newlimit, ptr %newstklimit
    store %Region %region, ptr %newstkregion
    store %RegionBackup %backup, ptr %newstkbackup
    store %Stk null, ptr %newstkrest

    %newoldrc = sub %Rc %rc, 1
    store %Rc %newoldrc, ptr %stkrc

    %cmp = icmp eq %RegionBackup %backup, null
    br i1 %cmp, label %backupRegion, label %done

backupRegion:
    %newbackup = call %RegionBackup @backupRegion(%Region %region)
    store %RegionBackup %newbackup, ptr %stkbackup
    ret %Stk %newstk

done:
    %ret = phi %Stk [%stk, %entry], [%newstk, %copy]
    ret %Stk %ret

}

define %RegionBackup @backupRegion(%Region %region) {
entry:
    %regionval = load %RegionVal, %Region %region
    %size.0 = extractvalue %RegionVal %regionval, 1
    %size.1 = add i64 %size.0, 24 ; add 3 x i64
    %backup = call ptr @malloc(i64 %size.1)
    br label %loop

loop:
    %i = phi i64 [0, %entry], [%nexti, %loop]
    %ptr = phi ptr [%backup, %entry], [%backupnext, %loop]

    %backupdata = getelementptr i64, ptr %ptr, i64 1

    %memp = getelementptr %RegionVal, %Region %region, i64 0, i32 0, i64 %i, i32 0
    %boundsp = getelementptr %RegionVal, %Region %region, i64 0, i32 0, i64 %i, i32 1

    %mem = load %Mem, ptr %memp
    %bounds = load %Bounds, ptr %boundsp

    %backupnext = call ptr @backupArenas(%Bounds %bounds, %Mem %mem, ptr %backupdata)

    %intnext = ptrtoint ptr %backupnext to i64
    %intdata = ptrtoint ptr %backupdata to i64
    %size = sub i64 %intnext, %intdata
    store i64 %size, ptr %ptr

    %nexti = add i64 %i, 1
    %cmp = icmp ult i64 %nexti, 2
    br i1 %cmp, label %loop, label %done

done:
    ret %RegionBackup %backup
}

define ptr @backupArenas(%Bounds %bounds, %Mem %mem, ptr %backup) {
    %base = extractvalue %Bounds %bounds, 0
    %isnull = icmp eq %Base %base, null
    br i1 %isnull, label %done, label %continue

done:
    ret ptr %backup

continue:
    %membase = extractvalue %Mem %mem, 1

    %data = getelementptr %Bounds, %Base %base, i64 1
    %intdata = ptrtoint ptr %data to i64

    %cmp = icmp eq %Base %base, %membase
    br i1 %cmp, label %copymem, label %copybounds

copymem:
    %sp = extractvalue %Mem %mem, 0
    %intsp = ptrtoint %Sp %sp to i64
    %memsize = sub i64 %intsp, %intdata
    call void @memcpy(ptr %backup, ptr %data, i64 %memsize)
    %end = getelementptr i8, ptr %backup, i64 %memsize
    ret ptr %end


copybounds:
    %limit = extractvalue %Bounds %bounds, 1
    %intlimit = ptrtoint %Limit %limit to i64
    %boundssize = sub i64 %intlimit, %intdata
    call void @memcpy(ptr %backup, ptr %data, i64 %boundssize)

    %backuprest = getelementptr i8, ptr %backup, i64 %boundssize
    %next = load %Bounds, %Base %base
    %end.0 = tail call ptr @backupArenas(%Bounds %next, %Mem %mem, ptr %backuprest)
    ret ptr %end.0
}

define void @shareStack(%Stk %stk) alwaysinline {
    %stkrc = getelementptr %StkVal, %Stk %stk, i64 0, i32 0
    %rc = load %Rc, ptr %stkrc
    %rc.1 = add %Rc %rc, 1
    store %Rc %rc.1, ptr %stkrc
    ret void
}

define void @eraseStack(%Stk %stk) alwaysinline {
    %stkrc = getelementptr %StkVal, %Stk %stk, i64 0, i32 0
    %rc = load %Rc, ptr %stkrc
    switch %Rc %rc, label %decr [%Rc 0, label %free]

    decr:
    %rc.1 = sub %Rc %rc, 1
    store %Rc %rc.1, ptr %stkrc
    ret void

    free:
    %stksp = getelementptr %StkVal, %Stk %stk, i64 0, i32 1, i32 0
    %sp = load %Sp, ptr %stksp
    call fastcc void @eraseFrames(%Sp %sp)
    call void @free(%Stk %stk)
    ret void
}

define fastcc void @shareFrames(%Sp %sp) alwaysinline {
    %newsp = getelementptr %FrameHeader, %Sp %sp, i64 -1
    %stksharer = getelementptr %FrameHeader, %Sp %newsp, i64 0, i32 1
    %sharer = load %Sharer, ptr %stksharer
    tail call fastcc void %sharer(%Sp %newsp)
    ret void
}

define fastcc void @eraseFrames(%Sp %sp) alwaysinline {
    %newsp = getelementptr %FrameHeader, %Sp %sp, i64 -1
    %stkeraser = getelementptr %FrameHeader, %Sp %newsp, i64 0, i32 2
    %eraser = load %Eraser, ptr %stkeraser
    tail call fastcc void %eraser(%Sp %newsp)
    ret void
}

; RTS initialization

define fastcc void @topLevel(%Env %env, %Sp noalias %sp) {
    %base = load %Base, ptr @base
    call void @free(%Base %base)

    %region = load %Region, ptr @region

    call void @free(%Region %region)

    call void @free(%Env %env)
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
    %region = call ptr @malloc(i64 128)
    store %RegionVal zeroinitializer, %Region %region

    store %Region %region, ptr @region

    ret void
}

; Primitive Types

%Int = type i64
%Double = type double

%Evi = type i64
