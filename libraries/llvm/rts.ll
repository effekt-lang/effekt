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

; This is part of a linked list containing all states in a region.
;
;   +[ StateHeader ]+--------------+
;   | Next  | Size  | Payload ...  |
;   +---------------+--------------+
%State = type ptr

%StateHeader = type { %State, i64 }

; Linked list of states and the total size of allocated space (without state headers)
%RegionVal = type { %State, i64 }

; Fixed pointer used to allocate states
%Region = type ptr

; A region backup stores the region value followed by the content of its states.
;
;   +[ RegionVal ]-+--------------+
;   | State | Size | Payload ...  |
;   +--------------+--------------+
;
; Restoring a backup deletes all states allocated after its creation.
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
    %mem = call ptr @malloc(i64 %size)
    %obj = bitcast ptr %mem to %Obj
    %objrc = getelementptr %Header, ptr %obj, i64 0, i32 0
    %objeraser = getelementptr %Header, ptr %obj, i64 0, i32 1
    store %Rc 0, ptr %objrc
    store %Eraser %eraser, ptr %objeraser
    ret %Obj %obj
}

define %Env @objectEnvironment(%Obj %obj) alwaysinline {
    ; Environment is stored right after header
    %obj.1 = getelementptr %Header, ptr %obj, i64 1
    %env = bitcast %Obj %obj.1 to %Env
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
    %objuntyped = bitcast %Obj %obj to ptr
    call void @free(ptr %objuntyped)
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
    %fullsize = add i64 %size, 16 ; add the size of the state header
    %statemem = call ptr @malloc(i64 %fullsize)

    %state = bitcast ptr %statemem to %State
    %statenext = getelementptr %StateHeader, %State %state, i64 0, i32 0
    %statesize = getelementptr %StateHeader, %State %state, i64 0, i32 1

    %regionval = load %RegionVal, %Region %region
    %oldstate = extractvalue %RegionVal %regionval, 0
    %oldsize = extractvalue %RegionVal %regionval, 1

    %newsize = add i64 %oldsize, %size
    %newregionval.0 = insertvalue %RegionVal undef, %State %state, 0
    %newregionval = insertvalue %RegionVal %newregionval.0, i64 %newsize, 1
    store %RegionVal %newregionval, %Region %region

    store %State %oldstate, ptr %statenext
    store i64 %size, ptr %statesize

    %ptr = getelementptr i8, ptr %statemem, i64 16
    ret %Ref %ptr
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
    %stkmem = call ptr @malloc(i64 56)
    %stk = bitcast ptr %stkmem to %Stk

    %regionmem = call ptr @malloc(i64 16)
    %region = bitcast ptr %regionmem to %Region
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

define void @deleteUntil(%State %state, %State %end) {
    %cmp = icmp eq %State %state, %end
    br i1 %cmp, label %done, label %delete

delete:
    %nextp = getelementptr %StateHeader, %State %state, i64 0, i32 0
    %next = load %State, ptr %nextp
    %statep = bitcast %State %state to ptr
    call void @free(ptr %statep)
    tail call void @deleteUntil(%State %next, %State %end)
    ret void

done:
    ret void
}

define void @restoreBackup(%State %state, ptr %backup) {
    %cmp = icmp eq %State %state, null
    br i1 %cmp, label %done, label %restore

done:
    ret void

restore:
    %stateheader = load %StateHeader, %State %state
    %nextstate = extractvalue %StateHeader %stateheader, 0
    %size = extractvalue %StateHeader %stateheader, 1
    %statetp = getelementptr %StateHeader, %State %state, i64 1
    %statep = bitcast %State %statetp to ptr

    call void @memcpy(ptr %statep, ptr %backup, i64 %size)
    %restbackup = getelementptr i8, ptr %backup, i64 %size
    tail call void @restoreBackup(%State %nextstate, ptr %restbackup)
    ret void
}

define %Sp @pushStack(%Stk %stk, %Sp %oldsp) alwaysinline {

    %backupp = getelementptr %StkVal, %Stk %stk, i64 0, i32 3
    %backup = load %RegionBackup, ptr %backupp
    %cmp = icmp eq %RegionBackup %backup, null
    br i1 %cmp, label %push, label %restore

restore:
    %backupheader = load %RegionVal, %RegionBackup %backup
    %backupstate = extractvalue %RegionVal %backupheader, 0
    %backupdatap = getelementptr %RegionVal, %RegionBackup %backup, i64 1
    %backupdata = bitcast %RegionBackup %backupdatap to ptr

    %regionp = getelementptr %StkVal, %Stk %stk, i64 0, i32 2
    %region = load %Region, ptr %regionp
    %oldstatep = getelementptr %RegionVal, %Region %region, i64 0, i32 0
    %oldstate = load %State, ptr %oldstatep
    call void @deleteUntil(%State %oldstate, %State %backupstate)
    call void @restoreBackup(%State %backupstate, ptr %backupdata)
    store %RegionVal %backupheader, %Region %region
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

    %stkpuntyped = bitcast %Stk %stk to ptr
    call void @free(%Sp %sp)
    call void @free(ptr %stkpuntyped)

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

    %newstkmem = call ptr @malloc(i64 56)
    %newstk = bitcast ptr %newstkmem to %Stk
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
    %regionval = load %RegionVal, %Region %region
    %regionstate = extractvalue %RegionVal %regionval, 0
    %regionsize = extractvalue %RegionVal %regionval, 1
    %backupsize = add i64 %regionsize, 16
    %backupmem = call ptr @malloc(i64 %backupsize)
    %backupheader = bitcast ptr %backupmem to %RegionBackup
    store %RegionVal %regionval, %RegionBackup %backupheader
    %backupdata = getelementptr i8, ptr %backupmem, i64 16
    call void @backupRegion(%State %regionstate, ptr %backupdata)
    ret %Stk %newstk

done:
    %ret = phi %Stk [%stk, %entry], [%newstk, %copy]
    ret %Stk %ret

}

define void @backupRegion(%State %state, ptr %backup) {
    %cmp = icmp eq %State %state, null
    br i1 %cmp, label %done, label %backupState

backupState:
    %header = load %StateHeader, %State %state
    %next = extractvalue %StateHeader %header, 0
    %size = extractvalue %StateHeader %header, 1
    %statedata.0 = getelementptr %StateHeader, %State %state, i64 1
    %statedata = bitcast %State %statedata.0 to ptr
    call void @memcpy(ptr %backup, ptr %statedata, i64 %size)
    %backuprest = getelementptr i8, ptr %backup, i64 %size
    tail call void @backupRegion(%State %next, ptr %backuprest)
    ret void

done:
    ret void
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
    %stkuntyped = bitcast %Stk %stk to ptr
    call void @free(ptr %stkuntyped)
    ret void
}

define fastcc void @shareFrames(%Sp %sp) alwaysinline {
    %sptyped = bitcast %Sp %sp to ptr
    %newsptyped = getelementptr %FrameHeader, ptr %sptyped, i64 -1
    %stksharer = getelementptr %FrameHeader, ptr %newsptyped, i64 0, i32 1
    %sharer = load %Sharer, ptr %stksharer
    %newsp = bitcast ptr %newsptyped to %Sp
    tail call fastcc void %sharer(%Sp %newsp)
    ret void
}

define fastcc void @eraseFrames(%Sp %sp) alwaysinline {
    %sptyped = bitcast %Sp %sp to ptr
    %newsptyped = getelementptr %FrameHeader, ptr %sptyped, i64 -1
    %stkeraser = getelementptr %FrameHeader, ptr %newsptyped, i64 0, i32 2
    %eraser = load %Eraser, ptr %stkeraser
    %newsp = bitcast ptr %newsptyped to %Sp
    tail call fastcc void %eraser(%Sp %newsp)
    ret void
}

; RTS initialization

define fastcc void @topLevel(%Env %env, %Sp noalias %sp) {
    %base = load %Base, ptr @base
    call void @free(ptr %base)

    %region = load %Region, ptr @region
    %statep = getelementptr %RegionVal, %Region %region, i64 0, i32 0
    %state = load %State, ptr %statep
    call void @deleteUntil(%State %state, %State null)

    %regionp = bitcast %Region %region to ptr
    call void @free(ptr %regionp)

    call void @free(ptr %env)
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
    %regionmem = call ptr @malloc(i64 16)
    %region = bitcast ptr %regionmem to %Region
    store %RegionVal zeroinitializer, %Region %region

    store %Region %region, ptr @region

    ret void
}

; Primitive Types

%Int = type i64
%Double = type double

%Evi = type i64
