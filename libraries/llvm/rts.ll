; Run-Time System

; TBAA Top type
!0 = !{!"TBAA Top"}

%Evidence = type i64

; Basic types

%Environment = type ptr

; Reference counts
%ReferenceCount = type i64
!3 = !{!"ReferenceCount", !0, i64 0}

; Code to share (bump rc) an environment
%Sharer = type ptr
!5 = !{!"Sharer", !0, i64 0}

; Code to drop an environment
%Eraser = type ptr
!4 = !{!"Eraser", !0, i64 0}


; Every heap object starts with a header
; size = 16
%Header = type {%ReferenceCount, %Eraser}
!17 = !{!"Header", !3, i64 0, !4, i64 8}

; A heap object is a pointer to a header followed by payload.
;
;   +--[ Header ]--------------+-------------+
;   | ReferenceCount  | Eraser | Payload ... |
;   +-----------------+--------+-------------+
%Object = type ptr

!7 = !{!"Object", !0, i64 0}

!38 = !{!"ObjectContents", !17, i64 0, !0, i64 16}
; Object->ReferenceCount
!39 = !{!38, !3, i64 0}
; Object->Eraser
!40 = !{!38, !4, i64 8}

; object contents: !{!"ObjectContents", !17, i64 0, !0, i64 16}

; Attribute set for user defined functions
attributes #0 = { nounwind norecurse nosync }

; Attribute set for extern functions
attributes #1 = { nounwind norecurse nosync willreturn }



; Runtime types



!6 = !{!"FunctionPointer", !0, i64 0}



; Base types
!10 = !{!"Byte", !0, i64 0}
!12 = !{!"Double", !0, i64 0}
!13 = !{!"Int", !0, i64 0}
!14 = !{!"String", !0, i64 0}
!24 = !{!"Reference", !0, i64 0}

!28 = !{!"IntBox", !13, i64 0}
!29 = !{!28, !13, i64 0}
!30 = !{!"DoubleBox", !12, i64 0}
!31 = !{!30, !12, i64 0}
!32 = !{!"StringBox", !14, i64 0}
!33 = !{!31, !14, i64 0}
!34 = !{!"PosBox", !15, i64 0}
!35 = !{!34, !15, i64 0}
!36 = !{!"NegBox", !16, i64 0}
!37 = !{!36, !16, i64 0}



; A Frame has the following layout
;
;   +-------[ FrameHeader ]------------+--------------+
;   | ReturnAddress  | Sharer | Eraser | Payload ...  |
;   +----------------------------------+--------------+

; A stack pointer points to the top-most frame followed by all other frames.
;
; For example
;
;     +--------------------+   <- Limit
;     :                    :
;     :                    :   <- StackPointer
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
%StackPointer = type ptr
!2 = !{!"StackPointer", !0, i64 0}

%Base = type %StackPointer
%Limit = type %StackPointer
%ReturnAddress = type ptr
!18 = !{!"ReturnAddress", !0, i64 0}

%FrameHeader = type { %ReturnAddress, %Sharer, %Eraser }
!19 = !{!"FrameHeader", !18, i64 0, !5, i64 8, !4, i64 16}
; return address in frame header
!25 = !{ !19, !18, i64 0 }
; sharer in frame header
!26 = !{ !19, !5, i64 8 }
; eraser in frame header
!27 = !{ !19, !4, i64 16 }


; Pointers for a heap allocated stack
; size = 24
%Memory = type { %StackPointer, %Base, %Limit }
!8 = !{!"Memory", !0, i64 0 } ; !2, i64 0, !2, i64 8, !2, i64 16}


; The garbage collector differentiates three groups of types:
; - Values (Integer, Double)
; - Objects (Positive, Negative)
; - Strings
; For each group we have an arena where mutable state is allocated.
;
; size = 3 * 24 = 72
%Region = type [ 3 x %Memory ]
!9 = !{!"Region", !0, i64 0} ; !8, i64 0, !8, i64 24, !8, i64 48}
; I currently treat this as scalar since I do not know how to do it otherwise
; "Access type node must be a valid scalar type"

; The "meta" stack (a stack of stacks) -- a pointer to a %StackValue
%Stack = type ptr
!1 = !{!"Stack", !0, i64 0}


; This is used for two purposes:
;   - a refied first-class list of stacks (cyclic linked-list)
;   - as part of an intrusive linked-list of stacks (meta stack)
%StackValue = type { %ReferenceCount, %Memory, %Region, %Stack }
!20 = !{!"StackValue", !3, i64 0, !8, i64 8, !9, i64 32, !1, i64 104}

; StackValue->Stack
!41 = !{!20, !1, i64 104}
; StackValue->Memory
!42 = !{!20, !8, i64 8}
; StackValue->Region
!43 = !{!20, !9, i64 32}
; StackValue->Refcount
!44 = !{!20, !3, i64 0}


; access type
; Stackvalue->Stackpointer
!23 = !{!20, !2, i64 8}

; %stackStackPointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 1, i32 0
; %stackPointer = load %StackPointer, ptr %stackStackPointer

; %stackPointer_2 = getelementptr i8, %StackPointer %stackPointer, i64 %n
; store %StackPointer %stackPointer_2, ptr %stackStackPointer

; ret %StackPointer %stackPointer

!21 = !{!"Tag", !0, i64 0}

; Positive data types consist of a (type-local) tag and a heap object
%Pos = type {i64, %Object}
!15 = !{!"Pos", !0, i64 0 }  ; !21, i64 0, !7, i64 8}
; !pos_access = !{!pos_type, !pos_type, i64 0}

; Negative types (codata) consist of a vtable and a heap object
%Neg = type {ptr, %Object}
!16 = !{!"Neg", !0, i64 0 } ;, !0, i64 0, !7, i64 8} ;, !3, i64 0, !7, i64 8}

; Offset within the arena
%Reference = type i64

; Builtin Types

%Int = type i64
%Double = type double
%Byte = type i8
%Char = type i64
%Bool = type %Pos
%Unit = type %Pos
%String = type %Pos

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

define %Object @newObject(%Eraser %eraser, i64 %environmentSize) alwaysinline {
    ; This magical 16 is the size of the object header
    %size = add i64 %environmentSize, 16
    %object = call ptr @malloc(i64 %size)
    %objectReferenceCount = getelementptr %Header, ptr %object, i64 0, i32 0
    %objectEraser = getelementptr %Header, ptr %object, i64 0, i32 1
    store %ReferenceCount 0, ptr %objectReferenceCount
    store %Eraser %eraser, ptr %objectEraser, !invariant.group !0
    ret %Object %object
}

define %Environment @objectEnvironment(%Object %object) alwaysinline {
    ; Environment is stored right after header
    %environment = getelementptr %Header, ptr %object, i64 1
    ret %Environment %environment
}

define void @shareObject(%Object %object) alwaysinline {
    %isNull = icmp eq %Object %object, null
    br i1 %isNull, label %done, label %next

    next:
    %objectReferenceCount = getelementptr %Header, ptr %object, i64 0, i32 0
    %referenceCount = load %ReferenceCount, ptr %objectReferenceCount
    %referenceCount.1 = add %ReferenceCount %referenceCount, 1
    store %ReferenceCount %referenceCount.1, ptr %objectReferenceCount
    br label %done

    done:
    ret void
}

define void @sharePositive(%Pos %val) alwaysinline {
    %object = extractvalue %Pos %val, 1
    tail call void @shareObject(%Object %object)
    ret void
}

define void @shareNegative(%Neg %val) alwaysinline {
    %object = extractvalue %Neg %val, 1
    tail call void @shareObject(%Object %object)
    ret void
}

define void @eraseObject(%Object %object) alwaysinline {
    %isNull = icmp eq %Object %object, null
    br i1 %isNull, label %done, label %next

    next:
    %objectReferenceCount = getelementptr %Header, ptr %object, i64 0, i32 0
    %referenceCount = load %ReferenceCount, ptr %objectReferenceCount, !tbaa !39
    switch %ReferenceCount %referenceCount, label %decr [%ReferenceCount 0, label %free]

    decr:
    %referenceCount.1 = sub %ReferenceCount %referenceCount, 1
    store %ReferenceCount %referenceCount.1, ptr %objectReferenceCount, !tbaa !39
    ret void

    free:
    %objectEraser = getelementptr %Header, ptr %object, i64 0, i32 1
    %eraser = load %Eraser, ptr %objectEraser, !tbaa !40, !invariant.group !0
    %environment = call %Environment @objectEnvironment(%Object %object)
    call void %eraser(%Environment %environment)
    call void @free(%Object %object)
    br label %done

    done:
    ret void
}

define void @erasePositive(%Pos %val) alwaysinline {
    %object = extractvalue %Pos %val, 1
    tail call void @eraseObject(%Object %object)
    ret void
}

define void @eraseNegative(%Neg %val) alwaysinline {
    %object = extractvalue %Neg %val, 1
    tail call void @eraseObject(%Object %object)
    ret void
}


; Arena management
define ptr @getRegionPointer(%Evidence %evidence, %Stack noalias nonnull %stack) {
entry:
    switch %Evidence %evidence, label %else [i64 0, label %here]

here:
    %stackRegion = getelementptr %StackValue, %Stack %stack, i64 0, i32 2
    ret ptr %stackRegion

else:
    %nextEvidence = sub %Evidence %evidence, 1
    %stackRest = getelementptr %StackValue, %Stack %stack, i64 0, i32 3
    %rest = load %Stack, ptr %stackRest, !tbaa !41
    %region = tail call ptr @getRegionPointer(%Evidence %nextEvidence, %Stack %rest)
    ret ptr %region
}

define { ptr, %Reference } @alloc(i64 %index, %Evidence %evidence, %Stack %stack) alwaysinline {
    %regionPointer = call ptr @getRegionPointer(%Evidence %evidence, %Stack %stack)
    %stackPointerPointer = getelementptr %Region, ptr %regionPointer, i64 0, i64 %index, i32 0
    %basePointer = getelementptr %Region, ptr %regionPointer, i64 0, i64 %index, i32 1
    %limitPointer = getelementptr %Region, ptr %regionPointer, i64 0, i64 %index, i32 2

    %stackPointer = load %StackPointer, ptr %stackPointerPointer
    %base = load %Base, ptr %basePointer
    %limit = load %Limit, ptr %limitPointer

    %object = icmp ne i64 %index, 0
    %size = select i1 %object, i64 16, i64 8

    %nextStackPointer = getelementptr i8, %StackPointer %stackPointer, i64 %size

    %cmp = icmp ule %StackPointer %nextStackPointer, %limit
    br i1 %cmp, label %continue, label %realloc

continue:
    store %StackPointer %nextStackPointer, ptr %stackPointerPointer
    %intBase = ptrtoint %Base %base to i64
    %intStackPointer = ptrtoint %StackPointer %stackPointer to i64
    %offset = sub i64 %intStackPointer, %intBase

    %ret.0 = insertvalue { ptr, %Reference } undef, %StackPointer %stackPointer, 0
    %ret.1 = insertvalue { ptr, %Reference } %ret.0, %Reference %offset, 1
    ret { ptr, %Reference } %ret.1

realloc:
    %intBase_2 = ptrtoint %Base %base to i64
    %intLimit = ptrtoint %Limit %limit to i64
    %arenaSize = sub i64 %intLimit, %intBase_2
    %empty = icmp eq i64 %arenaSize, 0
    %double = mul i64 %arenaSize, 2
    %newArenaSize = select i1 %empty, i64 1024, i64 %double

    %newBase = call ptr @realloc(ptr %base, i64 %newArenaSize)
    %newlimit = getelementptr i8, %Base %newBase, i64 %newArenaSize
    %newStackPointer = getelementptr i8, %Base %newBase, i64 %arenaSize
    %newNextStackPointer = getelementptr i8, %StackPointer %newStackPointer, i64 %size

    store %Base %newBase, ptr %basePointer
    store %Limit %newlimit, ptr %limitPointer
    store %StackPointer %newNextStackPointer, ptr %stackPointerPointer

    %ret..0 = insertvalue { ptr, %Reference } undef, %StackPointer %newStackPointer, 0
    %ret..1 = insertvalue { ptr, %Reference } %ret..0, %Reference %arenaSize, 1
    ret { ptr, %Reference } %ret..1

}

define ptr @getPointer(%Reference %reference, i64 %index, %Evidence %evidence, %Stack %stack) alwaysinline {
    %regionPointer = call ptr @getRegionPointer(%Evidence %evidence, %Stack %stack)
    %basePointer = getelementptr %Region, ptr %regionPointer, i64 0, i64 %index, i32 1
    %base = load %Base, ptr %basePointer
    %pointer = getelementptr i8, ptr %base, %Reference %reference
    ret ptr %pointer
}

; Stack management

define nonnull noalias %StackPointer @stackAllocate(%Stack noalias nonnull %stack, i64 %n) {
    %stackStackPointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 1, i32 0
    %stackPointer = load %StackPointer, ptr %stackStackPointer

    %stackPointer_2 = getelementptr i8, %StackPointer %stackPointer, i64 %n
    store %StackPointer %stackPointer_2, ptr %stackStackPointer

    ret %StackPointer %stackPointer
}

define nonnull noalias %StackPointer @stackDeallocate(%Stack noalias nonnull %stack, i64 %n) {
    %stackStackPointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 1, i32 0
    %stackPointer = load %StackPointer, ptr %stackStackPointer

    %o = sub i64 0, %n
    %stackPointer_2 = getelementptr i8, %StackPointer %stackPointer, i64 %o
    store %StackPointer %stackPointer_2, ptr %stackStackPointer

    ret %StackPointer %stackPointer_2
}

; Meta-stack management

define %Memory @newMemory() {
    %stackPointer = call %StackPointer @malloc(i64 268435456)
    %limit = getelementptr i8, ptr %stackPointer, i64 268435456

    %memory.0 = insertvalue %Memory undef, %StackPointer %stackPointer, 0
    %memory.1 = insertvalue %Memory %memory.0, %Base %stackPointer, 1
    %memory.2 = insertvalue %Memory %memory.1, %Limit %limit, 2

    ret %Memory %memory.2
}

define nonnull noalias %Stack @newStack() {

    ; TODO find actual size of stack
    %stack = call ptr @malloc(i64 112)

    ; TODO initialize to zero and grow later
    %stackMemory = call %Memory @newMemory()

    %stack.0 = insertvalue %StackValue undef, %ReferenceCount 0, 0
    %stack.1 = insertvalue %StackValue %stack.0, %Memory %stackMemory, 1
    %stack.2 = insertvalue %StackValue %stack.1, %Region zeroinitializer, 2
    %stack.3 = insertvalue %StackValue %stack.2, %Stack zeroinitializer, 3

    store %StackValue %stack.3, %Stack %stack

    ret %Stack %stack
}

define void @pushStack(%Stack nonnull noalias %stack, %Stack nonnull noalias %oldStack) {
    %stackRest = getelementptr %StackValue, %Stack %stack, i64 0, i32 3
    %rest = load %Stack, ptr %stackRest, !tbaa !41
    %isNull = icmp eq %Stack %rest, null
    br i1 %isNull, label %done, label %next

done:
    store %Stack %oldStack, ptr %stackRest, !tbaa !41 ; TODO is this correct?
    ret void

next:
    tail call void @pushStack(%Stack %rest, %Stack %oldStack)
    ret void
}

define nonnull noalias %Stack @popStacks(%Stack noalias nonnull %stack, i64 %n) {
    %stackRest = getelementptr %StackValue, %Stack %stack, i64 0, i32 3
    %rest = load %Stack, ptr %stackRest, !tbaa !41
    %isZero = icmp eq i64 %n, 0
    br i1 %isZero, label %done, label %next
done:
    store %Stack null, ptr %stackRest, !tbaa !41
    ret %Stack %rest
next:
    %o = sub i64 %n, 1
    %result = tail call %Stack @popStacks(%Stack %rest, i64 %o)
    ret %Stack %result
}

define void @eraseMemory(%Memory %memory) {
    %stackPointer = extractvalue %Memory %memory, 0
    call void @free(%StackPointer %stackPointer)
    ret void
}

define void @forEachObject(ptr %elementPointer, ptr %end, ptr %f) alwaysinline {
    %done = icmp uge ptr %elementPointer, %end
    br i1 %done, label %return, label %erase

erase:
    %element = load %Pos, ptr %elementPointer, !invariant.group !0
    call void %f(%Pos %element)

    %nextElementPointer = getelementptr %Pos, ptr %elementPointer, i64 1
    tail call void @forEachObject(ptr %nextElementPointer, ptr %end, ptr %f)
    ret void

return:
    ret void
}

define void @eraseRegion(%Region %region) alwaysinline {
    %valuesBase = extractvalue %Region %region, 0, 1
    call void @free(%Base %valuesBase)

    %objectsBase = extractvalue %Region %region, 1, 1
    %objectsStackPointer = extractvalue %Region %region, 1, 0
    call void @forEachObject(%Base %objectsBase, %StackPointer %objectsStackPointer, %Eraser @erasePositive)
    call void @free(%Base %objectsBase)

    %stringsBase = extractvalue %Region %region, 2, 1
    %stringsStackPointer = extractvalue %Region %region, 2, 0
    call void @forEachObject(%Base %stringsBase, %StackPointer %stringsStackPointer, %Eraser @erasePositive)
    call void @free(%Base %stringsBase)

    ret void
}

define nonnull noalias %Stack @underflowStack(%Stack noalias nonnull %stack) {
    %stackMemory = getelementptr %StackValue, %Stack %stack, i64 0, i32 1
    %stackRegion = getelementptr %StackValue, %Stack %stack, i64 0, i32 2
    %stackRest = getelementptr %StackValue, %Stack %stack, i64 0, i32 3

    %memory = load %Memory, ptr %stackMemory, !tbaa !42
    %region = load %Region, ptr %stackRegion, !tbaa !43
    %rest = load %Stack, ptr %stackRest, !tbaa !41

    call void @eraseMemory(%Memory %memory)
    call void @eraseRegion(%Region %region)
    call void @free(%Stack %stack)

    ret %Stack %rest
}

define %Memory @copyMemory(%Memory %memory) alwaysinline {
    %stackPointer = extractvalue %Memory %memory, 0
    %base = extractvalue %Memory %memory, 1
    %limit = extractvalue %Memory %memory, 2

    %intStackPointer = ptrtoint %StackPointer %stackPointer to i64
    %intBase = ptrtoint %Base %base to i64
    %intLimit = ptrtoint %Limit %limit to i64
    %used = sub i64 %intStackPointer, %intBase
    %size = sub i64 %intLimit, %intBase

    %newBase = call ptr @malloc(i64 %size)
    %intNewBase = ptrtoint %Base %newBase to i64
    %intNewStackPointer = add i64 %intNewBase, %used
    %intNewLimit = add i64 %intNewBase, %size
    %newStackPointer = inttoptr i64 %intNewStackPointer to %StackPointer
    %newLimit = inttoptr i64 %intNewLimit to %Limit

    call void @memcpy(ptr %newBase, ptr %base, i64 %used)

    %memory.0 = insertvalue %Memory undef, %StackPointer %newStackPointer, 0
    %memory.1 = insertvalue %Memory %memory.0, %Base %newBase, 1
    %memory.2 = insertvalue %Memory %memory.1, %Limit %newLimit, 2

    ret %Memory %memory.2
}

define %Region @copyRegion(%Region %region) alwaysinline {
    %memory.0 = extractvalue %Region %region, 0
    %memory.1 = extractvalue %Region %region, 1
    %memory.2 = extractvalue %Region %region, 2

    %objectsBase = extractvalue %Region %region, 1, 1
    %objectsStackPointer = extractvalue %Region %region, 1, 0
    call void @forEachObject(%Base %objectsBase, %StackPointer %objectsStackPointer, %Sharer @sharePositive)

    %stringsBase = extractvalue %Region %region, 2, 1
    %stringsStackPointer = extractvalue %Region %region, 2, 0
    call void @forEachObject(%Base %stringsBase, %StackPointer %stringsStackPointer, %Sharer @sharePositive)

    %newMemory.0 = call %Memory @copyMemory(%Memory %memory.0)
    %newMemory.1 = call %Memory @copyMemory(%Memory %memory.1)
    %newMemory.2 = call %Memory @copyMemory(%Memory %memory.2)

    %region.0 = insertvalue %Region undef, %Memory %newMemory.0, 0
    %region.1 = insertvalue %Region %region.0, %Memory %newMemory.1, 1
    %region.2 = insertvalue %Region %region.1, %Memory %newMemory.2, 2

    ret %Region %region.2
}

define %Stack @uniqueStack(%Stack %stack) alwaysinline {

entry:
    %stackReferenceCount = getelementptr %StackValue, %Stack %stack, i64 0, i32 0
    %referenceCount = load %ReferenceCount, ptr %stackReferenceCount, !tbaa !44
    switch %ReferenceCount %referenceCount, label %copy [%ReferenceCount 0, label %done]

done:
    ret %Stack %stack

copy:
    %newOldReferenceCount = sub %ReferenceCount %referenceCount, 1
    store %ReferenceCount %newOldReferenceCount, ptr %stackReferenceCount, !tbaa !44

    %newHead = call ptr @malloc(i64 112)
    br label %loop

loop:
    %old = phi %Stack [%stack, %copy], [%rest, %next]
    %newStack = phi %Stack [%newHead, %copy], [%nextNew, %next]

    %stackMemory = getelementptr %StackValue, %Stack %old, i64 0, i32 1
    %stackRegion = getelementptr %StackValue, %Stack %old, i64 0, i32 2
    %stackRest = getelementptr %StackValue, %Stack %old, i64 0, i32 3

    %memory = load %Memory, ptr %stackMemory, !tbaa !42
    %region = load %Region, ptr %stackRegion, !tbaa !43
    %rest = load %Stack, ptr %stackRest, !tbaa !41

    %newStackReferenceCount = getelementptr %StackValue, %Stack %newStack, i64 0, i32 0
    %newStackMemory = getelementptr %StackValue, %Stack %newStack, i64 0, i32 1
    %newStackRegion = getelementptr %StackValue, %Stack %newStack, i64 0, i32 2
    %newStackRest = getelementptr %StackValue, %Stack %newStack, i64 0, i32 3

    %newMemory = call %Memory @copyMemory(%Memory %memory)

    %newStackPointer = extractvalue %Memory %newMemory, 0
    call void @shareFrames(%StackPointer %newStackPointer)

    %newRegion = call %Region @copyRegion(%Region %region)

    store %ReferenceCount 0, ptr %newStackReferenceCount, !tbaa !44
    store %Memory %newMemory, ptr %newStackMemory, !tbaa !42
    store %Region %newRegion, ptr %newStackRegion, !tbaa !43

    %isNull = icmp eq %Stack %rest, null
    br i1 %isNull, label %stop, label %next

next:
    %nextNew = call ptr @malloc(i64 112)
    store %Stack %nextNew, ptr %newStackRest, !tbaa !41
    br label %loop

stop:
    store %Stack null, ptr %newStackRest, !tbaa !41
    ret %Stack %newHead
}

define void @shareStack(%Stack %stack) alwaysinline {
    %stackReferenceCount = getelementptr %StackValue, %Stack %stack, i64 0, i32 0
    %referenceCount = load %ReferenceCount, ptr %stackReferenceCount, !tbaa !44
    %referenceCount.1 = add %ReferenceCount %referenceCount, 1
    store %ReferenceCount %referenceCount.1, ptr %stackReferenceCount, !tbaa !44
    ret void
}

define void @eraseStack(%Stack %stack) alwaysinline {
    %stackReferenceCount = getelementptr %StackValue, %Stack %stack, i64 0, i32 0
    %referenceCount = load %ReferenceCount, ptr %stackReferenceCount, !tbaa !44
    switch %ReferenceCount %referenceCount, label %decr [%ReferenceCount 0, label %free]

    decr:
    %referenceCount.1 = sub %ReferenceCount %referenceCount, 1
    store %ReferenceCount %referenceCount.1, ptr %stackReferenceCount, !tbaa !44
    ret void

    free:
    %stackStackPointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 1, i32 0
    %stackRegion = getelementptr %StackValue, %Stack %stack, i64 0, i32 2
    %stackRest = getelementptr %StackValue, %Stack %stack, i64 0, i32 3

    %stackPointer = load %StackPointer, ptr %stackStackPointer
    %region = load %Region, ptr %stackRegion, !tbaa !43
    %rest = load %Stack, ptr %stackRest, !tbaa !41

    call void @free(%Stack %stack)
    call void @eraseFrames(%StackPointer %stackPointer)
    call void @eraseRegion(%Region %region)

    %isNull = icmp eq %Stack %rest, null
    br i1 %isNull, label %done, label %next

next:
    call void @eraseStack(%Stack %rest)
    ret void

done:
    ret void
}

define void @shareFrames(%StackPointer %stackPointer) alwaysinline {
    %newStackPointer = getelementptr %FrameHeader, %StackPointer %stackPointer, i64 -1
    %stackSharer = getelementptr %FrameHeader, %StackPointer %newStackPointer, i64 0, i32 1
    %sharer = load %Sharer, ptr %stackSharer, !tbaa !5, !invariant.group !0
    tail call void %sharer(%StackPointer %newStackPointer)
    ret void
}

define void @eraseFrames(%StackPointer %stackPointer) alwaysinline {
    %newStackPointer = getelementptr %FrameHeader, %StackPointer %stackPointer, i64 -1
    %stackEraser = getelementptr %FrameHeader, %StackPointer %newStackPointer, i64 0, i32 2
    %eraser = load %Eraser, ptr %stackEraser, !tbaa !4, !invariant.group !0
    tail call void %eraser(%StackPointer %newStackPointer)
    ret void
}

; RTS initialization

define tailcc void @topLevel(%Pos %val, %Stack noalias nonnull %stack) {
    %rest = call %Stack @underflowStack(%Stack %stack)
    ; assert %rest == null
    ret void
}

define void @topLevelSharer(%Environment %environment) {
    ; TODO this should never be called
    ret void
}

define void @topLevelEraser(%Environment %environment) {
    ; TODO this should never be called
    ret void
}

define noalias nonnull %Stack @withEmptyStack() {
    %stack = call %Stack @newStack()

    %stackStackPointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 1, i32 0
    %stackPointer = load %StackPointer, ptr %stackStackPointer

    %returnAddressPointer = getelementptr %FrameHeader, %StackPointer %stackPointer, i64 0, i32 0
    %sharerPointer = getelementptr %FrameHeader, %StackPointer %stackPointer, i64 0, i32 1
    %eraserPointer = getelementptr %FrameHeader, %StackPointer %stackPointer, i64 0, i32 2

    store %ReturnAddress @topLevel, ptr %returnAddressPointer, !tbaa !25
    store %Sharer @topLevelSharer, ptr %sharerPointer, !tbaa !26, !invariant.group !0
    store %Eraser @topLevelEraser, ptr %eraserPointer, !tbaa !27, !invariant.group !0

    %stackPointer_2 = getelementptr %FrameHeader, %StackPointer %stackPointer, i64 1
    store %StackPointer %stackPointer_2, ptr %stackStackPointer

    ret %Stack %stack
}

define void @run_i64(%Neg %f, i64 %arg) {
    ; fresh stack
    %stack = call %Stack @withEmptyStack()

    ; prepare call
    %arrayPointer = extractvalue %Neg %f, 0
    %object = extractvalue %Neg %f, 1
    %functionPointerPointer = getelementptr ptr, ptr %arrayPointer, i64 0
    %functionPointer = load ptr, ptr %functionPointerPointer, !tbaa !6

    ; call
    tail call tailcc %Pos %functionPointer(%Object %object, %Evidence 0, i64 %arg, %Stack %stack)
    ret void
}


define void @run_Pos(%Neg %f, %Pos %arg) {
    ; fresh stack
    %stack = call %Stack @withEmptyStack()

    ; prepare call
    %arrayPointer = extractvalue %Neg %f, 0
    %object = extractvalue %Neg %f, 1
    %functionPointerPointer = getelementptr ptr, ptr %arrayPointer, i64 0
    %functionPointer = load ptr, ptr %functionPointerPointer, !tbaa !6

    ; call
    tail call tailcc %Pos %functionPointer(%Object %object, %Evidence 0, %Pos %arg, %Stack %stack)
    ret void
}

define void @run(%Neg %f) {
    ; fresh stack
    %stack = call %Stack @withEmptyStack()

    ; prepare call
    %arrayPointer = extractvalue %Neg %f, 0
    %object = extractvalue %Neg %f, 1
    %functionPointerPointer = getelementptr ptr, ptr %arrayPointer, i64 0
    %functionPointer = load ptr, ptr %functionPointerPointer, !tbaa !6

    ; call
    tail call tailcc %Pos %functionPointer(%Object %object, %Evidence 0, %Stack %stack)
    ret void
}
