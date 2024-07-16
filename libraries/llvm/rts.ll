; Run-Time System

attributes #0 = { "ccc" }

%Evidence = type i64

; Basic types

%Environment = type ptr

; Reference counts
%ReferenceCount = type i64

; Code to share (bump rc) an environment
%Sharer = type ptr

; Code to drop an environment
%Eraser = type ptr

; Every heap object starts with a header
%Header = type {%ReferenceCount, %Eraser}

; A heap object is a pointer to a header followed by payload.
;
;   +--[ Header ]--------------+-------------+
;   | ReferenceCount  | Eraser | Payload ... |
;   +-----------------+--------+-------------+
%Object = type ptr


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
%Base = type %StackPointer
%Limit = type %StackPointer
%ReturnAddress = type ptr
%FrameHeader = type { %ReturnAddress, %Sharer, %Eraser }

; Pointers for a heap allocated stack
%Memory = type { %StackPointer, %Base, %Limit }

; The garbage collector differentiates three groups of types:
; - Values (Integer, Double)
; - Objects (Positive, Negative)
; - Strings
; For each group we have an arena where mutable state is allocated.
;
%Region = type [ 3 x %Memory ]

; The "meta" stack (a stack of stacks) -- a pointer to a %StackValue
%Stack = type ptr

; This is used for two purposes:
;   - a refied first-class list of stacks (cyclic linked-list)
;   - as part of an intrusive linked-list of stacks (meta stack)
%StackValue = type { %ReferenceCount, %Memory, %Region, %Stack }

; Positive data types consist of a (type-local) tag and a heap object
%Pos = type {i64, %Object}

; Negative types (codata) consist of a vtable and a heap object
%Neg = type {ptr, %Object}

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

; Global locations

@base = private global %Base null
@limit = private global %Limit null
@region = private global %Region zeroinitializer
@rest = private global %Stack undef


define %StackValue @getStack(%StackPointer %stackPointer) alwaysinline {
    %base = load %Base, ptr @base
    %limit = load %Limit, ptr @limit
    %region = load %Region, ptr @region
    %rest = load %Stack, ptr @rest

    %stack.0 = insertvalue %StackValue undef, %ReferenceCount 0, 0
    %stack.1 = insertvalue %StackValue %stack.0, %StackPointer %stackPointer, 1, 0
    %stack.2 = insertvalue %StackValue %stack.1, %Base %base, 1, 1
    %stack.3 = insertvalue %StackValue %stack.2, %Limit %limit, 1, 2
    %stack.4 = insertvalue %StackValue %stack.3, %Region %region, 2
    %stack.5 = insertvalue %StackValue %stack.4, %Stack %rest, 3

    ret %StackValue %stack.5
}

define void @setStack(%StackValue %stack) alwaysinline {
    %base = extractvalue %StackValue %stack, 1, 1
    %limit = extractvalue %StackValue %stack, 1, 2
    %region = extractvalue %StackValue %stack, 2
    %rest = extractvalue %StackValue %stack, 3

    store %Base %base, ptr @base
    store %Limit %limit, ptr @limit
    store %Region %region, ptr @region
    store %Stack %rest, ptr @rest
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

define %Object @newObject(%Eraser %eraser, i64 %environmentSize) alwaysinline {
    ; This magical 16 is the size of the object header
    %size = add i64 %environmentSize, 16
    %object = call ptr @malloc(i64 %size)
    %objectReferenceCount = getelementptr %Header, ptr %object, i64 0, i32 0
    %objectEraser = getelementptr %Header, ptr %object, i64 0, i32 1
    store %ReferenceCount 0, ptr %objectReferenceCount
    store %Eraser %eraser, ptr %objectEraser
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
    %referenceCount = load %ReferenceCount, ptr %objectReferenceCount
    switch %ReferenceCount %referenceCount, label %decr [%ReferenceCount 0, label %free]

    decr:
    %referenceCount.1 = sub %ReferenceCount %referenceCount, 1
    store %ReferenceCount %referenceCount.1, ptr %objectReferenceCount
    ret void

    free:
    %objectEraser = getelementptr %Header, ptr %object, i64 0, i32 1
    %eraser = load %Eraser, ptr %objectEraser
    %environment = call %Environment @objectEnvironment(%Object %object)
    call fastcc void %eraser(%Environment %environment)
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
define ptr @getRegionPointer(i64 %evidence) alwaysinline {
entry:
    switch i64 %evidence, label %loop [i64 0, label %here]

here:
    ret ptr @region

loop:
    %stackPointer = phi ptr [@rest, %entry], [%nextPointer, %loop]
    %index = phi i64 [%evidence, %entry], [%nextIndex, %loop]
    %stack = load %Stack, ptr %stackPointer
    %regionPointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 2
    %nextPointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 3
    %nextIndex = sub i64 %index, 1
    %cmp = icmp eq i64 %nextIndex, 0

    br i1 %cmp, label %done, label %loop

done:
    ret ptr %regionPointer

}



define { ptr, %Reference } @alloc(i64 %index, i64 %evidence) alwaysinline {
    %regionPointer = call ptr @getRegionPointer(i64 %evidence)
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

define ptr @getPointer(%Reference %reference, i64 %index, i64 %evidence) alwaysinline {
    %regionPointer = call ptr @getRegionPointer(i64 %evidence)
    %basePointer = getelementptr %Region, ptr %regionPointer, i64 0, i64 %index, i32 1
    %base = load %Base, ptr %basePointer
    %pointer = getelementptr i8, ptr %base, %Reference %reference
    ret ptr %pointer
}

; Meta-stack management

define %Memory @newMemory() alwaysinline {
    %stackPointer = call %StackPointer @malloc(i64 268435456)
    %limit = getelementptr i8, ptr %stackPointer, i64 268435456

    %memory.0 = insertvalue %Memory undef, %StackPointer %stackPointer, 0
    %memory.1 = insertvalue %Memory %memory.0, %Base %stackPointer, 1
    %memory.2 = insertvalue %Memory %memory.1, %Limit %limit, 2

    ret %Memory %memory.2
}

define %Stack @newStack() alwaysinline {

    ; TODO find actual size of stack
    %stack = call ptr @malloc(i64 112)

    ; TODO initialize to zero and grow later
    %stackMemory = call %Memory @newMemory()

    %stack.0 = insertvalue %StackValue undef, %ReferenceCount 0, 0
    %stack.1 = insertvalue %StackValue %stack.0, %Memory %stackMemory, 1
    %stack.2 = insertvalue %StackValue %stack.1, %Region zeroinitializer, 2
    %stack.3 = insertvalue %StackValue %stack.2, %Stack %stack, 3

    store %StackValue %stack.3, %Stack %stack

    ret %Stack %stack
}

define %StackPointer @pushStack(%Stack %stack, %StackPointer %oldStackPointer) alwaysinline {
    %newStack = load %StackValue, %Stack %stack

    %oldStack = call %StackValue @getStack(%StackPointer %oldStackPointer)

    call void @setStack(%StackValue %newStack)

    store %StackValue %oldStack, %Stack %stack

    %newStackPointer = extractvalue %StackValue %newStack, 1, 0
    ret %StackPointer %newStackPointer
}

; pop n+1 stacks
define {%Stack, %StackPointer} @popStacks(%StackPointer %oldStackPointer, i64 %n) alwaysinline {
entry:
    %oldStack = call %StackValue @getStack(%StackPointer %oldStackPointer)
    br label %loop

loop:
    %stackValue = phi %StackValue [%oldStack, %entry], [%newStack, %loop]
    %index = phi i64 [%n, %entry], [%nextIndex, %loop]

    %newStackPointer = extractvalue %StackValue %stackValue, 3
    %newStack = load %StackValue, %Stack %newStackPointer

    %nextIndex = sub i64 %index, 1

    %cmp = icmp eq i64 %index, 0
    br i1 %cmp, label %done, label %loop

done:
    call void @setStack(%StackValue %newStack)

    store %StackValue %oldStack, %Stack %newStackPointer

    %newStackPointer_2 = extractvalue %StackValue %newStack, 1, 0
    %ret.0 = insertvalue {%Stack, %StackPointer} undef, %Stack %newStackPointer, 0
    %ret.1 = insertvalue {%Stack, %StackPointer} %ret.0, %StackPointer %newStackPointer_2, 1

    ret {%Stack, %StackPointer} %ret.1
}

define %StackPointer @underflowStack(%StackPointer %stackPointer) alwaysinline {
    %stack = load %Stack, ptr @rest
    %newStack = load %StackValue, %Stack %stack

    %region = load %Region, ptr @region
    call void @eraseRegion(%Region %region)

    call void @setStack(%StackValue %newStack)

    call void @free(%StackPointer %stackPointer)
    call void @free(%Stack %stack)

    %newStackPointer = extractvalue %StackValue %newStack, 1, 0
    ret %StackPointer %newStackPointer
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
    call void @forEachObject(%Base %objectsBase, %StackPointer %objectsStackPointer, %Eraser @sharePositive)

    %stringsBase = extractvalue %Region %region, 2, 1
    %stringsStackPointer = extractvalue %Region %region, 2, 0
    call void @forEachObject(%Base %stringsBase, %StackPointer %stringsStackPointer, %Eraser @sharePositive)

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
    %referenceCount = load %ReferenceCount, ptr %stackReferenceCount
    switch %ReferenceCount %referenceCount, label %copy [%ReferenceCount 0, label %done]

done:
    ret %Stack %stack

copy:
    %newOldReferenceCount = sub %ReferenceCount %referenceCount, 1
    store %ReferenceCount %newOldReferenceCount, ptr %stackReferenceCount

    %newHead = call ptr @malloc(i64 112)
    br label %loop

loop:
    %old = phi %Stack [%stack, %copy], [%rest, %next]
    %newStack = phi %Stack [%newHead, %copy], [%nextNew, %next]

    %stackMemory = getelementptr %StackValue, %Stack %old, i64 0, i32 1
    %stackRegion = getelementptr %StackValue, %Stack %old, i64 0, i32 2
    %stackRest = getelementptr %StackValue, %Stack %old, i64 0, i32 3

    %memory = load %Memory, ptr %stackMemory
    %region = load %Region, ptr %stackRegion
    %rest = load %Stack, ptr %stackRest

    %newStackReferenceCount = getelementptr %StackValue, %Stack %newStack, i64 0, i32 0
    %newStackMemory = getelementptr %StackValue, %Stack %newStack, i64 0, i32 1
    %newStackRegion = getelementptr %StackValue, %Stack %newStack, i64 0, i32 2
    %newStackRest = getelementptr %StackValue, %Stack %newStack, i64 0, i32 3

    %newMemory = call %Memory @copyMemory(%Memory %memory)

    %newStackPointer = extractvalue %Memory %newMemory, 0
    call fastcc void @shareFrames(%StackPointer %newStackPointer)

    %newRegion = call %Region @copyRegion(%Region %region)

    store %ReferenceCount 0, ptr %newStackReferenceCount
    store %Memory %newMemory, ptr %newStackMemory
    store %Region %newRegion, ptr %newStackRegion

    %last = icmp eq %Stack %rest, %stack
    br i1 %last, label %closeCycle, label %next

next:
    %nextNew = call ptr @malloc(i64 112)
    store %Stack %nextNew, ptr %newStackRest
    br label %loop

closeCycle:
    store %Stack %newHead, ptr %newStackRest
    ret %Stack %newHead

}

define void @shareStack(%Stack %stack) alwaysinline {
    %stackReferenceCount = getelementptr %StackValue, %Stack %stack, i64 0, i32 0
    %referenceCount = load %ReferenceCount, ptr %stackReferenceCount
    %referenceCount.1 = add %ReferenceCount %referenceCount, 1
    store %ReferenceCount %referenceCount.1, ptr %stackReferenceCount
    ret void
}

define void @eraseStack(%Stack %stack) alwaysinline {
    %stackReferenceCount = getelementptr %StackValue, %Stack %stack, i64 0, i32 0
    %referenceCount = load %ReferenceCount, ptr %stackReferenceCount
    switch %ReferenceCount %referenceCount, label %decr [%ReferenceCount 0, label %free]

    decr:
    %referenceCount.1 = sub %ReferenceCount %referenceCount, 1
    store %ReferenceCount %referenceCount.1, ptr %stackReferenceCount
    ret void

    free:
    %stackStackPointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 1, i32 0
    %stackPointer = load %StackPointer, ptr %stackStackPointer
    call fastcc void @eraseFrames(%StackPointer %stackPointer)

    %regionPointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 2
    %region = load %Region, ptr %regionPointer

    call void @eraseRegion(%Region %region)

    call void @free(%Stack %stack)
    ret void
}

define fastcc void @shareFrames(%StackPointer %stackPointer) alwaysinline {
    %newStackPointer = getelementptr %FrameHeader, %StackPointer %stackPointer, i64 -1
    %stackSharer = getelementptr %FrameHeader, %StackPointer %newStackPointer, i64 0, i32 1
    %sharer = load %Sharer, ptr %stackSharer
    tail call fastcc void %sharer(%StackPointer %newStackPointer)
    ret void
}

define fastcc void @eraseFrames(%StackPointer %stackPointer) alwaysinline {
    %newStackPointer = getelementptr %FrameHeader, %StackPointer %stackPointer, i64 -1
    %stackEraser = getelementptr %FrameHeader, %StackPointer %newStackPointer, i64 0, i32 2
    %eraser = load %Eraser, ptr %stackEraser
    tail call fastcc void %eraser(%StackPointer %newStackPointer)
    ret void
}

define void @forEachObject(ptr %elementPointer, ptr %end, ptr %f) alwaysinline {
    %done = icmp uge ptr %elementPointer, %end
    br i1 %done, label %return, label %erase

erase:
    %element = load %Pos, ptr %elementPointer
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

; RTS initialization

define fastcc void @topLevel(%Environment %environment, %StackPointer noalias %stackPointer) {
    %base = load %Base, ptr @base
    call void @free(%Base %base)

    %region = load %Region, ptr @region
    %base.0 = extractvalue %Region %region, 0, 1
    %base.1 = extractvalue %Region %region, 1, 1
    %base.2 = extractvalue %Region %region, 2, 1

    call void @free(%Base %base.0)
    call void @free(%Base %base.1)
    call void @free(%Base %base.2)

    call void @free(%Environment %environment)
    ret void
}

define fastcc void @topLevelSharer(%Environment %environment) {
    ; TODO this should never be called
    ret void
}

define fastcc void @topLevelEraser(%Environment %environment) {
    ; TODO this should never be called
    ret void
}

define %StackPointer @withEmptyStack() {
    %stackPointer = call %StackPointer @malloc(i64 268435456)
    store %StackPointer %stackPointer, ptr @base
    %returnAddressPointer_1 = getelementptr %FrameHeader, %StackPointer %stackPointer, i64 0, i32 0
    %sharerPointer_2 = getelementptr %FrameHeader, %StackPointer %stackPointer, i64 0, i32 1
    %eraserPointer_3 = getelementptr %FrameHeader, %StackPointer %stackPointer, i64 0, i32 2
    store %ReturnAddress @topLevel, ptr %returnAddressPointer_1
    store %Sharer @topLevelSharer, ptr %sharerPointer_2
    store %Eraser @topLevelEraser, ptr %eraserPointer_3
    %stackPointer_2 = getelementptr %FrameHeader, %StackPointer %stackPointer, i64 1
    ret %StackPointer %stackPointer_2
}

define fastcc void @run_i64(%Neg %f, i64 %arg) {
    ; backup globals
    %base = load %Base, ptr @base
    %region = load %Region, ptr @region
    %limit = load %Limit, ptr @limit
    %rest = load %Stack, ptr @rest

    ; fresh stack
    %stackPointer = call %StackPointer @withEmptyStack()

    ; prepare call
    %arrayPointer = extractvalue %Neg %f, 0
    %object = extractvalue %Neg %f, 1
    %functionPointerPointer = getelementptr ptr, ptr %arrayPointer, i64 0
    %functionPointer = load ptr, ptr %functionPointerPointer

    ; Store the argument (0th index is evidence)
    %environment = call %Environment @malloc(i64 1048576)
    %evidence2 = getelementptr {%Int, %Int}, %Environment %environment, i64 0, i32 1
    store i64 %arg, ptr %evidence2

    ; call
    %result = call fastcc %Pos %functionPointer(%Object %object, %Environment %environment, %StackPointer %stackPointer)

    ; restore stack (TODO this shouldn't be necessary, the moment we pass stacks...; then this is a tail-call again)
    store %StackPointer %base, ptr @base
    store %Region %region, ptr @region
    store %Limit %limit, ptr @limit
    store %Stack %rest, ptr @rest

    ret void
}


define fastcc void @run_Pos(%Neg %f, %Pos %arg) {
    ; backup globals
    %base = load %Base, ptr @base
    %region = load %Region, ptr @region
    %limit = load %Limit, ptr @limit
    %rest = load %Stack, ptr @rest

    ; fresh stack
    %stackPointer = call %StackPointer @withEmptyStack()

    ; prepare call
    %arrayPointer = extractvalue %Neg %f, 0
    %object = extractvalue %Neg %f, 1
    %functionPointerPointer = getelementptr ptr, ptr %arrayPointer, i64 0
    %functionPointer = load ptr, ptr %functionPointerPointer

    ; Store the argument (0th index is evidence)
    %environment = call %Environment @malloc(i64 1048576)
    %arg_pos = getelementptr {%Int, %Pos}, %Environment %environment, i64 0, i32 1
    store %Pos %arg, ptr %arg_pos

    ; call
    %result = call fastcc %Pos %functionPointer(%Object %object, %Environment %environment, %StackPointer %stackPointer)

    ; restore stack (TODO this shouldn't be necessary, the moment we pass stacks...; then this is a tail-call again)
    store %StackPointer %base, ptr @base
    store %Region %region, ptr @region
    store %Limit %limit, ptr @limit
    store %Stack %rest, ptr @rest

    ret void
}

define void @run(%Neg %f) {
    ; backup globals
    %base = load %Base, ptr @base
    %region = load %Region, ptr @region
    %limit = load %Limit, ptr @limit
    %rest = load %Stack, ptr @rest

    ; fresh stack
    %stackPointer = call %StackPointer @withEmptyStack()

    ; prepare call
    %arrayPointer = extractvalue %Neg %f, 0
    %object = extractvalue %Neg %f, 1
    %functionPointerPointer = getelementptr ptr, ptr %arrayPointer, i64 0
    %functionPointer = load ptr, ptr %functionPointerPointer

    ; call
    %environment = call %Environment @malloc(i64 1048576)
    %result = call fastcc %Pos %functionPointer(%Object %object, %Environment %environment, %StackPointer %stackPointer)

    ; restore stack (TODO this shouldn't be necessary, the moment we pass stacks...; then this is a tail-call again)
    store %StackPointer %base, ptr @base
    store %Region %region, ptr @region
    store %Limit %limit, ptr @limit
    store %Stack %rest, ptr @rest

    ret void
}
