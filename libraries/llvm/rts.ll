; Run-Time System

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

; The "meta" stack (a stack of stacks) -- a pointer to a %MetaStack
%MetaStackPointer = type ptr

; This is used as part of an intrusive linked-list of stacks (meta stack)
%MetaStack = type { %ReferenceCount, %Memory, %Region, %MetaStackPointer }

%ResumptionPointer = type ptr

; TODO use array instead of linked list
%Resumption = type { %ReferenceCount, %MetaStackPointer, %Memory, %Region, %ResumptionPointer }

; Positive data types consist of a (type-local) tag and a heap object
%Pos = type {i64, %Object}

; Negative types (codata) consist of a vtable and a heap object
%Neg = type {ptr, %Object}

; Reference into an arena (prompt, offset)
%Reference = type {%MetaStackPointer, i64}

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
declare double @llvm.sqrt.f64(double)
declare double @llvm.round.f64(double)
declare double @llvm.ceil.f64(double)
declare double @llvm.floor.f64(double)
declare void @print(i64)
declare void @exit(i64)


; Prompts

define private %MetaStackPointer @currentPrompt(%MetaStackPointer %stack) alwaysinline {
    ret %MetaStackPointer %stack
}

define private %MetaStackPointer @freshPrompt() alwaysinline {
    %prompt = call ptr @malloc(i64 112)
    %memory = call %Memory @newMemory()
    %metaStack = insertvalue %MetaStack zeroinitializer, %Memory %memory, 1
    store %MetaStack %metaStack, %MetaStackPointer %prompt
    ret %MetaStackPointer %prompt
}

; Garbage collection

define private %Object @newObject(%Eraser %eraser, i64 %environmentSize) alwaysinline {
    ; This magical 16 is the size of the object header
    %size = add i64 %environmentSize, 16
    %object = call ptr @malloc(i64 %size)
    %objectReferenceCount = getelementptr %Header, ptr %object, i64 0, i32 0
    %objectEraser = getelementptr %Header, ptr %object, i64 0, i32 1
    store %ReferenceCount 0, ptr %objectReferenceCount
    store %Eraser %eraser, ptr %objectEraser
    ret %Object %object
}

define private %Environment @objectEnvironment(%Object %object) alwaysinline {
    ; Environment is stored right after header
    %environment = getelementptr %Header, ptr %object, i64 1
    ret %Environment %environment
}

define private void @shareObject(%Object %object) alwaysinline {
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

define private void @eraseObject(%Object %object) alwaysinline {
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

define private { ptr, %Reference } @alloc(i64 %index, %MetaStackPointer %stack) alwaysinline {
    %region_pointer = getelementptr %MetaStack, %MetaStackPointer %stack, i64 0, i32 2

    %stackPointer_pointer = getelementptr %Region, ptr %region_pointer, i64 0, i64 %index, i32 0
    %base_pointer = getelementptr %Region, ptr %region_pointer, i64 0, i64 %index, i32 1
    %limit_pointer = getelementptr %Region, ptr %region_pointer, i64 0, i64 %index, i32 2

    %stackPointer = load %StackPointer, ptr %stackPointer_pointer
    %base = load %Base, ptr %base_pointer
    %limit = load %Limit, ptr %limit_pointer

    %object = icmp ne i64 %index, 0
    %size = select i1 %object, i64 16, i64 8

    %nextStackPointer = getelementptr i8, %StackPointer %stackPointer, i64 %size

    %cmp = icmp ule %StackPointer %nextStackPointer, %limit
    br i1 %cmp, label %continue, label %realloc

continue:
    store %StackPointer %nextStackPointer, ptr %stackPointer_pointer
    %intBase = ptrtoint %Base %base to i64
    %intStackPointer = ptrtoint %StackPointer %stackPointer to i64
    %offset = sub i64 %intStackPointer, %intBase

    %ret.0 = insertvalue { ptr, %Reference } undef, %StackPointer %stackPointer, 0
    %ref.1 = insertvalue %Reference undef, %MetaStackPointer %stack, 0
    %ref.2 = insertvalue %Reference %ref.1, i64 %offset, 1
    %ret.1 = insertvalue { ptr, %Reference } %ret.0, %Reference %ref.2, 1
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

    store %Base %newBase, ptr %base_pointer
    store %Limit %newlimit, ptr %limit_pointer
    store %StackPointer %newNextStackPointer, ptr %stackPointer_pointer

    %ret..0 = insertvalue { ptr, %Reference } undef, %StackPointer %newStackPointer, 0
    %ref..1 = insertvalue %Reference undef, %MetaStackPointer %stack, 0
    %ref..2 = insertvalue %Reference %ref..1, i64 %arenaSize, 1
    %ret..1 = insertvalue { ptr, %Reference } %ret..0, %Reference %ref..2, 1
    ret { ptr, %Reference } %ret..1
}


define private ptr @getPointer(%Reference %reference, i64 %index) {
    %stack = extractvalue %Reference %reference, 0
    %offset = extractvalue %Reference %reference, 1

    %base_pointer = getelementptr %MetaStack, %MetaStackPointer %stack, i64 0, i32 2, i64 %index, i32 1
    %base = load %Base, ptr %base_pointer
    %pointer = getelementptr i8, ptr %base, i64 %offset
    ret ptr %pointer
}

define private ptr @getVarPointer(%Reference %reference) {
    %stack = extractvalue %Reference %reference, 0
    %offset = extractvalue %Reference %reference, 1

    %basePointer = getelementptr %MetaStack, %MetaStackPointer %stack, i64 0, i32 1, i32 1
    %base = load %Base, ptr %basePointer
    %varPointer = getelementptr i8, %Base %base, i64 %offset
    ret ptr %varPointer
}

define private %Reference @newReference(%MetaStackPointer %stack) alwaysinline {
    %stackPointerPointer = getelementptr %MetaStack, %MetaStackPointer %stack, i64 0, i32 1, i32 0
    %basePointer = getelementptr %MetaStack, %MetaStackPointer %stack, i64 0, i32 1, i32 1

    %stackPointer = load %StackPointer, ptr %stackPointerPointer
    %base = load %StackPointer, ptr %basePointer

    %intStack = ptrtoint %StackPointer %stackPointer to i64
    %intBase = ptrtoint %StackPointer %base to i64

    %offset = sub i64 %intStack, %intBase

    %reference..1 = insertvalue %Reference undef, %MetaStackPointer %stack, 0
    %reference = insertvalue %Reference %reference..1, i64 %offset, 1

    ret %Reference %reference
}

; Stack management

define private %StackPointer @stackAllocate(%MetaStackPointer %stack, i64 %n) {
    %stackStackPointer = getelementptr %MetaStack, %MetaStackPointer %stack, i64 0, i32 1, i32 0
    %stackPointer = load %StackPointer, ptr %stackStackPointer

    %stackPointer_2 = getelementptr i8, %StackPointer %stackPointer, i64 %n
    store %StackPointer %stackPointer_2, ptr %stackStackPointer
    ret %StackPointer %stackPointer
}

define private %StackPointer @stackDeallocate(%MetaStackPointer %stack, i64 %n) {
    %stackStackPointer = getelementptr %MetaStack, %MetaStackPointer %stack, i64 0, i32 1, i32 0
    %stackPointer = load %StackPointer, ptr %stackStackPointer

    %o = sub i64 0, %n
    %stackPointer_2 = getelementptr i8, %StackPointer %stackPointer, i64 %o
    store %StackPointer %stackPointer_2, ptr %stackStackPointer

    ret %StackPointer %stackPointer_2
}

; Meta-stack management

define private %Memory @newMemory() {
    %stackPointer = call %StackPointer @malloc(i64 268435456)
    %limit = getelementptr i8, ptr %stackPointer, i64 268435456

    %memory.0 = insertvalue %Memory undef, %StackPointer %stackPointer, 0
    %memory.1 = insertvalue %Memory %memory.0, %Base %stackPointer, 1
    %memory.2 = insertvalue %Memory %memory.1, %Limit %limit, 2

    ret %Memory %memory.2
}

define private %ResumptionPointer @newStack(%MetaStackPointer %metaStack) alwaysinline {

    ; TODO find actual size of stack
    %resumption = call ptr @malloc(i64 120)

    %memory_pointer = getelementptr %MetaStack, %MetaStackPointer %metaStack, i64 0, i32 1
    %region_pointer = getelementptr %MetaStack, %MetaStackPointer %metaStack, i64 0, i32 2

    %memory = load %Memory, ptr %memory_pointer
    %region = load %Region, ptr %region_pointer

    %resumption.0 = insertvalue %Resumption undef, %ReferenceCount 0, 0
    %resumption.1 = insertvalue %Resumption %resumption.0, %MetaStackPointer %metaStack, 1
    %resumption.2 = insertvalue %Resumption %resumption.1, %Memory %memory, 2
    %resumption.3 = insertvalue %Resumption %resumption.2, %Region %region, 3
    %resumption.4 = insertvalue %Resumption %resumption.3, %ResumptionPointer zeroinitializer, 4

    store %Resumption %resumption.4, %ResumptionPointer %resumption

    ret %ResumptionPointer %resumption
}

define private %MetaStackPointer @pushStack(%ResumptionPointer %resumption, %MetaStackPointer %oldStack) alwaysinline {
    %isNull = icmp eq %ResumptionPointer %resumption, null
    br i1 %isNull, label %return, label %push

return:
    ret %MetaStackPointer %oldStack

push:
    %metaStack_pointer = getelementptr %Resumption, %ResumptionPointer %resumption, i64 0, i32 1
    %metaStack = load %MetaStackPointer, ptr %metaStack_pointer

    %memory_pointer = getelementptr %Resumption, %ResumptionPointer %resumption, i64 0, i32 2
    %region_pointer = getelementptr %Resumption, %ResumptionPointer %resumption, i64 0, i32 3

    %newMemory_pointer = getelementptr %MetaStack, %MetaStackPointer %metaStack, i64 0, i32 1
    %newRegion_pointer = getelementptr %MetaStack, %MetaStackPointer %metaStack, i64 0, i32 2

    %memory = load %Memory, ptr %memory_pointer
    %region = load %Region, ptr %region_pointer

    store %Memory %memory, ptr %newMemory_pointer
    store %Region %region, ptr %newRegion_pointer

    %rest_pointer = getelementptr %Resumption, %ResumptionPointer %resumption, i64 0, i32 4
    %rest = load %ResumptionPointer, ptr %rest_pointer

    %nextMetaStack = call %MetaStackPointer @pushStack(%ResumptionPointer %rest, %MetaStackPointer %oldStack)

    %newRest_pointer = getelementptr %MetaStack, %MetaStackPointer %metaStack, i64 0, i32 3
    store %MetaStackPointer %nextMetaStack, ptr %newRest_pointer

    call void @free(%ResumptionPointer %resumption)
    ret %MetaStackPointer %metaStack
}

define private %ResumptionPointer @popStacks(%MetaStackPointer %stack, %MetaStackPointer %prompt) alwaysinline {
    %memory_pointer = getelementptr %MetaStack, %MetaStackPointer %stack, i64 0, i32 1
    %region_pointer = getelementptr %MetaStack, %MetaStackPointer %stack, i64 0, i32 2
    %rest_pointer = getelementptr %MetaStack, %MetaStackPointer %stack, i64 0, i32 3

    %resumption = call ptr @malloc(i64 120)
    %newReferenceCount_pointer = getelementptr %Resumption, %ResumptionPointer %resumption, i64 0, i32 0
    %newMetaStack_pointer = getelementptr %Resumption, %ResumptionPointer %resumption, i64 0, i32 1
    %newMemory_pointer = getelementptr %Resumption, %ResumptionPointer %resumption, i64 0, i32 2
    %newRegion_pointer = getelementptr %Resumption, %ResumptionPointer %resumption, i64 0, i32 3
    %newRest_pointer = getelementptr %Resumption, %ResumptionPointer %resumption, i64 0, i32 4

    %memory = load %Memory, ptr %memory_pointer
    %region = load %Region, ptr %region_pointer

    store %ReferenceCount 0, ptr %newReferenceCount_pointer
    store %MetaStackPointer %stack, ptr %newMetaStack_pointer
    store %Memory %memory, ptr %newMemory_pointer
    store %Region %region, ptr %newRegion_pointer

    %isPrompt = icmp eq %MetaStackPointer %stack, %prompt
    br i1 %isPrompt, label %end, label %recurse

end:
    store %ResumptionPointer null, ptr %newRest_pointer
    ret %ResumptionPointer %resumption

recurse:
    %rest = load %MetaStackPointer, ptr %rest_pointer
    %restResumption = call %ResumptionPointer @popStacks(%MetaStackPointer %rest, %MetaStackPointer %prompt)
    store %ResumptionPointer %restResumption, ptr %newRest_pointer
    ret %ResumptionPointer %resumption
}

define private void @eraseMemory(%Memory %memory) {
    %base_pointer = extractvalue %Memory %memory, 1
    call void @free(%Base %base_pointer)
    ret void
}

define private void @forEachObject(ptr %elementPointer, ptr %end, ptr %f) alwaysinline {
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

define private void @eraseRegion(%Region %region) alwaysinline {
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

define void @eraseMetaStack(%MetaStackPointer %metaStack) alwaysinline {
    %referenceCount_pointer = getelementptr %MetaStack, %MetaStackPointer %metaStack, i64 0, i32 0
    %referenceCount = load %ReferenceCount, ptr %referenceCount_pointer
    switch %ReferenceCount %referenceCount, label %decrement [%ReferenceCount 0, label %free]

decrement:
    %newReferenceCount = sub %ReferenceCount %referenceCount, 1
    store %ReferenceCount %newReferenceCount, ptr %referenceCount_pointer
    ret void

free:
    call void @free(%MetaStackPointer %metaStack)
    ret void
}

define void @shareMetaStack(%MetaStackPointer %metaStack) alwaysinline {
    %referenceCount_pointer = getelementptr %MetaStack, %MetaStackPointer %metaStack, i64 0, i32 0
    %referenceCount = load %ReferenceCount, ptr %referenceCount_pointer
    %newReferenceCount = add %ReferenceCount %referenceCount, 1
    store %ReferenceCount %newReferenceCount, ptr %referenceCount_pointer
    ret void
}

define private %MetaStackPointer @underflowStack(%MetaStackPointer %metaStack) {
    %memory_pointer = getelementptr %MetaStack, %MetaStackPointer %metaStack, i64 0, i32 1
    %region_pointer = getelementptr %MetaStack, %MetaStackPointer %metaStack, i64 0, i32 2
    %rest_pointer = getelementptr %MetaStack, %MetaStackPointer %metaStack, i64 0, i32 3

    %memory = load %Memory, ptr %memory_pointer
    %region = load %Region, ptr %region_pointer
    %rest = load %MetaStackPointer, ptr %rest_pointer

    call void @eraseMemory(%Memory %memory)
    call void @eraseRegion(%Region %region)
    call void @eraseMetaStack(%MetaStackPointer %metaStack)

    ret %MetaStackPointer %rest
}

define private %Memory @copyMemory(%Memory %memory) alwaysinline {
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

define private %Region @copyRegion(%Region %region) alwaysinline {
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

define private %ResumptionPointer @uniqueStack(%ResumptionPointer %resumption) alwaysinline {
    %referenceCount_pointer = getelementptr %Resumption, %ResumptionPointer %resumption, i64 0, i32 0
    %referenceCount = load %ReferenceCount, ptr %referenceCount_pointer
    switch %ReferenceCount %referenceCount, label %copy [%ReferenceCount 0, label %done]

done:
    ret %ResumptionPointer %resumption

copy:
    %newOldReferenceCount = sub %ReferenceCount %referenceCount, 1
    store %ReferenceCount %newOldReferenceCount, ptr %referenceCount_pointer

    %newResumption = call %ResumptionPointer @copyResumption(%ResumptionPointer %resumption)
    ret %ResumptionPointer %newResumption
}

define private %ResumptionPointer @copyResumption(%ResumptionPointer %resumption) alwaysinline {
    %newResumption = call ptr @malloc(i64 120)

    %metaStack_pointer = getelementptr %Resumption, %ResumptionPointer %resumption, i64 0, i32 1
    %memory_pointer = getelementptr %Resumption, %ResumptionPointer %resumption, i64 0, i32 2
    %region_pointer = getelementptr %Resumption, %ResumptionPointer %resumption, i64 0, i32 3
    %rest_pointer = getelementptr %Resumption, %ResumptionPointer %resumption, i64 0, i32 4

    %metaStack = load %MetaStackPointer, ptr %metaStack_pointer
    %memory = load %Memory, ptr %memory_pointer
    %region = load %Region, ptr %region_pointer
    %rest = load %ResumptionPointer, ptr %rest_pointer

    %newReferenceCount_pointer = getelementptr %Resumption, %ResumptionPointer %newResumption, i64 0, i32 0
    %newMetaStack_pointer = getelementptr %Resumption, %ResumptionPointer %newResumption, i64 0, i32 1
    %newMemory_pointer = getelementptr %Resumption, %ResumptionPointer %newResumption, i64 0, i32 2
    %newRegion_pointer = getelementptr %Resumption, %ResumptionPointer %newResumption, i64 0, i32 3
    %newRest_pointer = getelementptr %Resumption, %ResumptionPointer %newResumption, i64 0, i32 4

    %newMemory = call %Memory @copyMemory(%Memory %memory)

    %newStackPointer = extractvalue %Memory %newMemory, 0
    call void @shareFrames(%StackPointer %newStackPointer)

    %newRegion = call %Region @copyRegion(%Region %region)

    store %ReferenceCount 0, ptr %newReferenceCount_pointer
    store %MetaStackPointer %metaStack, ptr %newMetaStack_pointer
    store %Memory %newMemory, ptr %newMemory_pointer
    store %Region %newRegion, ptr %newRegion_pointer

    call void @shareMetaStack(%MetaStackPointer %metaStack)

    %isNull = icmp eq %ResumptionPointer %rest, null
    br i1 %isNull, label %stop, label %next

next:
    %newRest = call %ResumptionPointer @copyResumption(%ResumptionPointer %rest)
    store %ResumptionPointer %newRest, ptr %newRest_pointer
    ret %ResumptionPointer %newResumption

stop:
    store %MetaStackPointer null, ptr %newRest_pointer
    ret %ResumptionPointer %newResumption
}

define void @shareStack(%ResumptionPointer %resumption) alwaysinline {
    %referenceCount_pointer = getelementptr %Resumption, %ResumptionPointer %resumption, i64 0, i32 0
    %referenceCount = load %ReferenceCount, ptr %referenceCount_pointer
    %referenceCount.1 = add %ReferenceCount %referenceCount, 1
    store %ReferenceCount %referenceCount.1, ptr %referenceCount_pointer
    ret void
}

define void @eraseStack(%ResumptionPointer %resumption) alwaysinline {
    %referenceCount_pointer = getelementptr %Resumption, %ResumptionPointer %resumption, i64 0, i32 0
    %referenceCount = load %ReferenceCount, ptr %referenceCount_pointer
    switch %ReferenceCount %referenceCount, label %decr [%ReferenceCount 0, label %free]

decr:
    %referenceCount.1 = sub %ReferenceCount %referenceCount, 1
    store %ReferenceCount %referenceCount.1, ptr %referenceCount_pointer
    ret void

free:
    %metaStack_pointer = getelementptr %Resumption, %ResumptionPointer %resumption, i64 0, i32 1
    %stackPointer_pointer = getelementptr %Resumption, %ResumptionPointer %resumption, i64 0, i32 2, i32 0
    %region_pointer = getelementptr %Resumption, %ResumptionPointer %resumption, i64 0, i32 3
    %rest_pointer = getelementptr %Resumption, %ResumptionPointer %resumption, i64 0, i32 4

    %metaStack = load %MetaStackPointer, ptr %metaStack_pointer
    %stackPointer = load %StackPointer, ptr %stackPointer_pointer
    %region = load %Region, ptr %region_pointer
    %rest = load %ResumptionPointer, ptr %rest_pointer

    call void @free(%ResumptionPointer %resumption)
    call void @eraseMetaStack(%MetaStackPointer %metaStack)
    call void @eraseFrames(%StackPointer %stackPointer)
    call void @eraseRegion(%Region %region)

    %isNull = icmp eq %ResumptionPointer %rest, null
    br i1 %isNull, label %done, label %next

next:
    call void @eraseStack(%MetaStackPointer %rest)
    ret void

done:
    ret void
}

define private void @shareFrames(%StackPointer %stackPointer) alwaysinline {
    %newStackPointer = getelementptr %FrameHeader, %StackPointer %stackPointer, i64 -1
    %stackSharer = getelementptr %FrameHeader, %StackPointer %newStackPointer, i64 0, i32 1
    %sharer = load %Sharer, ptr %stackSharer
    tail call void %sharer(%StackPointer %newStackPointer)
    ret void
}

define private void @eraseFrames(%StackPointer %stackPointer) alwaysinline {
    %newStackPointer = getelementptr %FrameHeader, %StackPointer %stackPointer, i64 -1
    %stackEraser = getelementptr %FrameHeader, %StackPointer %newStackPointer, i64 0, i32 2
    %eraser = load %Eraser, ptr %stackEraser
    tail call void %eraser(%StackPointer %newStackPointer)
    ret void
}

; RTS initialization

define private tailcc void @topLevel(%Pos %val, %MetaStackPointer %stack) {
    %rest = call %MetaStackPointer @underflowStack(%MetaStackPointer %stack)
    ; assert %rest == null
    ret void
}

define private void @topLevelSharer(%Environment %environment) {
    ; TODO this should never be called
    ret void
}

define private void @topLevelEraser(%Environment %environment) {
    ; TODO this should never be called
    ret void
}

define private %MetaStackPointer @withEmptyStack() {
    ; TODO all stacks share the same source of fresh prompts
    %metaStack = call %MetaStackPointer @freshPrompt()

    %stackStackPointer = getelementptr %MetaStack, %MetaStackPointer %metaStack, i64 0, i32 1, i32 0
    %stackPointer = load %StackPointer, ptr %stackStackPointer

    %returnAddressPointer = getelementptr %FrameHeader, %StackPointer %stackPointer, i64 0, i32 0
    %sharerPointer = getelementptr %FrameHeader, %StackPointer %stackPointer, i64 0, i32 1
    %eraserPointer = getelementptr %FrameHeader, %StackPointer %stackPointer, i64 0, i32 2

    store %ReturnAddress @topLevel, ptr %returnAddressPointer
    store %Sharer @topLevelSharer, ptr %sharerPointer
    store %Eraser @topLevelEraser, ptr %eraserPointer

    %stackPointer_2 = getelementptr %FrameHeader, %StackPointer %stackPointer, i64 1
    store %StackPointer %stackPointer_2, ptr %stackStackPointer

    ret %MetaStackPointer %metaStack
}

define void @resume_Int(%MetaStackPointer %stack, %Int %argument) {
    %stackPointer = call ccc %StackPointer @stackDeallocate(%MetaStackPointer %stack, i64 24)
    %returnAddressPointer = getelementptr %FrameHeader, %StackPointer %stackPointer, i64 0, i32 0
    %returnAddress = load %ReturnAddress, ptr %returnAddressPointer
    tail call tailcc void %returnAddress(%Int %argument, %MetaStackPointer %stack)
    ret void
}

define void @resume_Pos(%MetaStackPointer %stack, %Pos %argument) {
    %stackPointer = call ccc %StackPointer @stackDeallocate(%MetaStackPointer %stack, i64 24)
    %returnAddressPointer = getelementptr %FrameHeader, %StackPointer %stackPointer, i64 0, i32 0
    %returnAddress = load %ReturnAddress, ptr %returnAddressPointer
    tail call tailcc void %returnAddress(%Pos %argument, %MetaStackPointer %stack)
    ret void
}

define void @run(%Neg %f) {
    ; fresh stack
    %stack = call %MetaStackPointer @withEmptyStack()

    ; prepare call
    %arrayPointer = extractvalue %Neg %f, 0
    %object = extractvalue %Neg %f, 1
    %functionPointerPointer = getelementptr ptr, ptr %arrayPointer, i64 0
    %functionPointer = load ptr, ptr %functionPointerPointer

    ; call
    tail call tailcc %Pos %functionPointer(%Object %object, %MetaStackPointer %stack)
    ret void
}

define void @run_Int(%Neg %f, i64 %argument) {
    ; fresh stack
    %stack = call %MetaStackPointer @withEmptyStack()

    ; prepare call
    %arrayPointer = extractvalue %Neg %f, 0
    %object = extractvalue %Neg %f, 1
    %functionPointerPointer = getelementptr ptr, ptr %arrayPointer, i64 0
    %functionPointer = load ptr, ptr %functionPointerPointer

    ; call
    tail call tailcc %Pos %functionPointer(%Object %object, %Evidence 0, i64 %argument, %MetaStackPointer %stack)
    ret void
}

define void @run_Pos(%Neg %f, %Pos %argument) {
    ; fresh stack
    %stack = call %MetaStackPointer @withEmptyStack()

    ; prepare call
    %arrayPointer = extractvalue %Neg %f, 0
    %object = extractvalue %Neg %f, 1
    %functionPointerPointer = getelementptr ptr, ptr %arrayPointer, i64 0
    %functionPointer = load ptr, ptr %functionPointerPointer

    ; call
    tail call tailcc %Pos %functionPointer(%Object %object, %Evidence 0, %Pos %argument, %MetaStackPointer %stack)
    ret void
}
