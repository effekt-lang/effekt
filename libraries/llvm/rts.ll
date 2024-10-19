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

; Unique address for each handler.
%Prompt = type ptr

; A Continuation capturing a list of stacks
%Resumption = type ptr

; The "meta" stack (a stack of stacks) -- a pointer to a %StackValue
%Stack = type ptr

; Lives in a stable address
%PromptValue = type { %ReferenceCount, %Stack }

; This points to a linked-list of stacks,
; and also holds a direct pointer to the last stack
%ResumptionValue = type { %ReferenceCount, %Stack, %Stack }

; This is used as part of an intrusive linked-list of stacks (meta stack)
%StackValue = type { %Memory, %Region, %Prompt, %Stack }



; Positive data types consist of a (type-local) tag and a heap object
%Pos = type {i64, %Object}

; Negative types (codata) consist of a vtable and a heap object
%Neg = type {ptr, %Object}

; Reference into an arena (prompt -- cast to 32bit, offset 32bit)
%Reference = type { %Prompt, i64 }

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

define private %Prompt @currentPrompt(%Stack %stack) {
    %prompt_pointer = getelementptr %StackValue, ptr %stack, i64 0, i32 2
    %prompt = load %Prompt, ptr %prompt_pointer
    ret %Prompt %prompt
}

define private %Prompt @freshPrompt() {
    %prompt = call %Prompt @malloc(i64 16)
    store %PromptValue zeroinitializer, %Prompt %prompt
    ret %Prompt %prompt
}

; Garbage collection

define private %Object @newObject(%Eraser %eraser, i64 %environmentSize) alwaysinline {
    %headerSize = ptrtoint ptr getelementptr (%Header, ptr null, i64 1) to i64
    %size = add i64 %environmentSize, %headerSize
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
define private ptr @getRegionPointer(%Prompt %prompt) {
    %stack_pointer = getelementptr %PromptValue, %Prompt %prompt, i64 0, i32 1
    %stack = load %Stack, ptr %stack_pointer
    %region_pointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 1
    ret ptr %region_pointer
}

define private { ptr, %Reference } @alloc(i64 %index, %Prompt %prompt) alwaysinline {
    %region_pointer = call ptr @getRegionPointer(%Prompt %prompt)

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
    %ret.1 = insertvalue { ptr, %Reference } %ret.0, %Prompt %prompt, 1, 0
    %ret.2 = insertvalue { ptr, %Reference } %ret.1, i64 %offset, 1, 1
    ret { ptr, %Reference } %ret.2

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
    %ret..1 = insertvalue { ptr, %Reference } %ret..0, %Prompt %prompt, 1, 0
    %ret..2 = insertvalue { ptr, %Reference } %ret..1, i64 %arenaSize, 1, 1
    ret { ptr, %Reference } %ret..2
}


define private ptr @getPointer(%Reference %reference, i64 %index, %Stack %stack) {
    %prompt = extractvalue %Reference %reference, 0
    %offset = extractvalue %Reference %reference, 1

    %region_pointer = call ptr @getRegionPointer(%Prompt %prompt)
    %base_pointer = getelementptr %Region, ptr %region_pointer, i64 0, i64 %index, i32 1
    %base = load %Base, ptr %base_pointer
    %pointer = getelementptr i8, ptr %base, i64 %offset
    ret ptr %pointer
}

define private %Stack @getStack(%Prompt %prompt) {
    %stack_pointer = getelementptr %PromptValue, %Prompt %prompt, i64 0, i32 1
    %stack = load %Stack, ptr %stack_pointer
    ret %Stack %stack
}

define private ptr @getVarPointer(%Reference %reference, %Stack %stack) {
    %prompt = extractvalue %Reference %reference, 0
    %offset = extractvalue %Reference %reference, 1

    %targetStack = call %Stack @getStack(%Prompt %prompt)
    %base_pointer = getelementptr %StackValue, %Stack %targetStack, i64 0, i32 0, i32 1
    %base = load %Base, ptr %base_pointer
    %varPointer = getelementptr i8, %Base %base, i64 %offset
    ret ptr %varPointer
}

define private %Reference @newReference(%Stack %stack) alwaysinline {
    %stackPointer_pointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 0, i32 0
    %base_pointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 0, i32 1

    %stackPointer = load %StackPointer, ptr %stackPointer_pointer
    %base = load %StackPointer, ptr %base_pointer

    %intStack = ptrtoint %StackPointer %stackPointer to i64
    %intBase = ptrtoint %StackPointer %base to i64

    %offset = sub i64 %intStack, %intBase

    %prompt = call %Prompt @currentPrompt(%Stack %stack)

    %reference..1 = insertvalue %Reference undef, %Prompt %prompt, 0
    %reference = insertvalue %Reference %reference..1, i64 %offset, 1

    ret %Reference %reference
}

; Stack management

define private %StackPointer @stackAllocate(%Stack %stack, i64 %n) {
    %stackPointer_pointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 0, i32 0
    %limit_pointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 0, i32 2

    %currentStackPointer = load %StackPointer, ptr %stackPointer_pointer
    %limit = load %Limit, ptr %limit_pointer

    %nextStackPointer = getelementptr i8, %StackPointer %currentStackPointer, i64 %n
    %isInside = icmp ule %StackPointer %nextStackPointer, %limit
    br i1 %isInside, label %continue, label %realloc

continue:
    store %StackPointer %nextStackPointer, ptr %stackPointer_pointer
    ret %StackPointer %currentStackPointer

realloc:
    %base_pointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 0, i32 1
    %base = load %Base, ptr %base_pointer

    %intStackPointer = ptrtoint %StackPointer %currentStackPointer to i64
    %intBase = ptrtoint %Base %base to i64

    %size = sub i64 %intStackPointer, %intBase
    %nextSize = add i64 %size, %n
    %newSize = call i64 @nextPowerOfTwo(i64 %nextSize)

    %newBase = call ptr @realloc(ptr %base, i64 %newSize)
    %newLimit = getelementptr i8, %Base %newBase, i64 %newSize
    %newStackPointer = getelementptr i8, %Base %newBase, i64 %size
    %newNextStackPointer = getelementptr i8, %StackPointer %newStackPointer, i64 %n

    store %StackPointer %newNextStackPointer, ptr %stackPointer_pointer
    store %Base %newBase, ptr %base_pointer
    store %Limit %newLimit, ptr %limit_pointer

    ret %StackPointer %newStackPointer
}

define private %StackPointer @stackDeallocate(%Stack %stack, i64 %n) {
    %stackPointer_pointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 0, i32 0
    %stackPointer = load %StackPointer, ptr %stackPointer_pointer

    %o = sub i64 0, %n
    %newStackPointer = getelementptr i8, %StackPointer %stackPointer, i64 %o
    store %StackPointer %newStackPointer, ptr %stackPointer_pointer

    ret %StackPointer %newStackPointer
}

define i64 @nextPowerOfTwo(i64 %x) {
    %leadingZeros = call i64 @llvm.ctlz.i64(i64 %x, i1 false)
    %numBits = sub i64 64, %leadingZeros
    %result = shl i64 1, %numBits
    ret i64 %result
}

; Meta-stack management

define private %Memory @newMemory() {
    %size = shl i64 1, 6
    %stackPointer = call %StackPointer @malloc(i64 %size)
    %limit = getelementptr i8, ptr %stackPointer, i64 %size

    %memory.0 = insertvalue %Memory undef, %StackPointer %stackPointer, 0
    %memory.1 = insertvalue %Memory %memory.0, %Base %stackPointer, 1
    %memory.2 = insertvalue %Memory %memory.1, %Limit %limit, 2

    ret %Memory %memory.2
}

define private %Resumption @reset(%Stack %oldStack) {

    %prompt = call %Prompt @freshPrompt()

    %size = ptrtoint ptr getelementptr (%StackValue, ptr null, i64 1) to i64
    %stack = call ptr @malloc(i64 %size)


    %stackMemory = call %Memory @newMemory()

    %stack.0 = insertvalue %StackValue zeroinitializer, %Memory %stackMemory, 0
    %stack.1 = insertvalue %StackValue %stack.0, %Prompt %prompt, 2
    %stack.2 = insertvalue %StackValue %stack.1, %Stack %oldStack, 3

    store %StackValue %stack.2, %Stack %stack

    ret %Stack %stack
}

define private void @updatePrompts(%Stack %stack) alwaysinline {
    %isNull = icmp eq %Stack %stack, null
    br i1 %isNull, label %done, label %continue

done:
    ret void

continue:
    %prompt_pointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 2
    %prompt = load %Prompt, ptr %prompt_pointer
    %stack_pointer = getelementptr %PromptValue, %Prompt %prompt, i64 0, i32 1
    store %Stack %stack, ptr %stack_pointer

    %next_pointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 3
    %next = load %Stack, ptr %next_pointer
    call void @updatePrompts(%Stack %next)
    ret void
}

define %Stack @resume(%Resumption %resumption, %Stack %oldStack) {
    %uniqueResumption = call %Resumption @uniqueStack(%Resumption %resumption)
    %start_pointer = getelementptr %ResumptionValue, %Resumption %uniqueResumption, i64 0, i32 1
    %start = load %Stack, ptr %start_pointer
    call void @updatePrompts(%Stack %start); TODO find out if prompts are still valid

    %end_pointer = getelementptr %ResumptionValue, %Resumption %uniqueResumption, i64 0, i32 2
    %end = load %Stack, ptr %end_pointer
    %rest_pointer = getelementptr %StackValue, %Stack %end, i64 0, i32 3
    store %Stack %oldStack, ptr %rest_pointer
    ret %Stack %start
}

define private {%Resumption, %Stack} @shift(%Stack %stack, %Prompt %prompt) {
    %resumptionSize = ptrtoint ptr getelementptr (%ResumptionValue, ptr null, i64 1) to i64
    %resumption = call %Resumption @malloc(i64 %resumptionSize)
    %promptStack_pointer = getelementptr %PromptValue, %Prompt %prompt, i64 0, i32 1
    %promptStack = load %Stack, ptr %promptStack_pointer
    %next_pointer = getelementptr %StackValue, %Stack %promptStack, i64 0, i32 3
    %next = load %Stack, ptr %next_pointer

    %resumption.0 = insertvalue %ResumptionValue zeroinitializer, %Stack %stack, 1
    %resumption.1 = insertvalue %ResumptionValue %resumption.0, %Stack %promptStack, 2
    store %ResumptionValue %resumption.1, %Resumption %resumption

    store %Stack null, ptr %next_pointer

    %result.0 = insertvalue {%Resumption, %Stack} undef, %Resumption %resumption, 0
    %result = insertvalue {%Resumption, %Stack} %result.0, %Stack %next, 1
    ret {%Resumption, %Stack} %result
}

define private void @eraseMemory(%Memory %memory) {
    %stackPointer = extractvalue %Memory %memory, 0
    call void @free(%StackPointer %stackPointer)
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

define void @erasePrompt(%Prompt %prompt) alwaysinline {
    %referenceCount_pointer = getelementptr %PromptValue, %Prompt %prompt, i64 0, i32 0
    %referenceCount = load %ReferenceCount, ptr %referenceCount_pointer
    switch %ReferenceCount %referenceCount, label %decrement [%ReferenceCount 0, label %free]

decrement:
    %newReferenceCount = sub %ReferenceCount %referenceCount, 1
    store %ReferenceCount %referenceCount, ptr %referenceCount_pointer
    ret void

free:
    call void @free(%Prompt %prompt)
    ret void
}

define void @sharePrompt(%Prompt %prompt) alwaysinline {
    %referenceCount_pointer = getelementptr %PromptValue, %Prompt %prompt, i64 0, i32 0
    %referenceCount = load %ReferenceCount, ptr %referenceCount_pointer
    %newReferenceCount = add %ReferenceCount %referenceCount, 1
    store %ReferenceCount %newReferenceCount, ptr %referenceCount_pointer
    ret void
}

define private %Stack @underflowStack(%Stack %stack) {
    %stackMemory = getelementptr %StackValue, %Stack %stack, i64 0, i32 0
    %stackRegion = getelementptr %StackValue, %Stack %stack, i64 0, i32 1
    %stackPrompt = getelementptr %StackValue, %Stack %stack, i64 0, i32 2
    %stackRest = getelementptr %StackValue, %Stack %stack, i64 0, i32 3

    %memory = load %Memory, ptr %stackMemory
    %region = load %Region, ptr %stackRegion
    %prompt = load %Prompt, ptr %stackPrompt
    %rest = load %Stack, ptr %stackRest

    call void @eraseMemory(%Memory %memory)
    call void @eraseRegion(%Region %region)
    call void @erasePrompt(%Prompt %prompt)
    call void @free(%Stack %stack)

    ret %Stack %rest
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

define private %Resumption @uniqueStack(%Resumption %resumption) alwaysinline {

entry:
    %referenceCount_pointer = getelementptr %ResumptionValue, %Resumption %resumption, i64 0, i32 0
    %referenceCount = load %ReferenceCount, ptr %referenceCount_pointer
    switch %ReferenceCount %referenceCount, label %copy [%ReferenceCount 0, label %done]

done:
    ret %Resumption %resumption

copy:
    %newOldReferenceCount = sub %ReferenceCount %referenceCount, 1
    store %ReferenceCount %newOldReferenceCount, ptr %referenceCount_pointer
    %stack_pointer = getelementptr %ResumptionValue, %Resumption %resumption, i64 0, i32 1
    %stack = load %Stack, ptr %stack_pointer

    %newResumption = call ptr @malloc(i64 24)
    %newReferenceCount_pointer = getelementptr %ResumptionValue, %Resumption %newResumption, i64 0, i32 0
    %newStart_pointer = getelementptr %ResumptionValue, %Resumption %newResumption, i64 0, i32 1
    %newEnd_pointer = getelementptr %ResumptionValue, %Resumption %newResumption, i64 0, i32 2

    store %ReferenceCount 0, ptr %newReferenceCount_pointer

    %size = ptrtoint ptr getelementptr (%StackValue, ptr null, i64 1) to i64
    %newHead = call ptr @malloc(i64 %size)
    store %Stack %newHead, ptr %newStart_pointer

    br label %loop

loop:
    %old = phi %Stack [%stack, %copy], [%rest, %next]
    %newStack = phi %Stack [%newHead, %copy], [%nextNew, %next]

    %stackMemory = getelementptr %StackValue, %Stack %old, i64 0, i32 0
    %stackRegion = getelementptr %StackValue, %Stack %old, i64 0, i32 1
    %stackPrompt = getelementptr %StackValue, %Stack %old, i64 0, i32 2
    %stackRest = getelementptr %StackValue, %Stack %old, i64 0, i32 3

    %memory = load %Memory, ptr %stackMemory
    %region = load %Region, ptr %stackRegion
    %prompt = load %Prompt, ptr %stackPrompt
    %rest = load %Stack, ptr %stackRest

    %newStackMemory = getelementptr %StackValue, %Stack %newStack, i64 0, i32 0
    %newStackRegion = getelementptr %StackValue, %Stack %newStack, i64 0, i32 1
    %newStackPrompt = getelementptr %StackValue, %Stack %newStack, i64 0, i32 2
    %newStackRest = getelementptr %StackValue, %Stack %newStack, i64 0, i32 3

    %newMemory = call %Memory @copyMemory(%Memory %memory)

    %newStackPointer = extractvalue %Memory %newMemory, 0
    call void @shareFrames(%StackPointer %newStackPointer)

    %newRegion = call %Region @copyRegion(%Region %region)

    call void @sharePrompt(%Prompt %prompt)

    store %Memory %newMemory, ptr %newStackMemory
    store %Region %newRegion, ptr %newStackRegion
    store %Prompt %prompt, ptr %newStackPrompt

    %isNull = icmp eq %Stack %rest, null
    br i1 %isNull, label %stop, label %next

next:
    %nextNew = call ptr @malloc(i64 %size)
    store %Stack %nextNew, ptr %newStackRest
    br label %loop

stop:
    store %Stack null, ptr %newStackRest
    store %Stack %newStack, ptr %newEnd_pointer
    ret %Stack %newResumption
}

define void @shareResumption(%Resumption %resumption) alwaysinline {
    %referenceCount_pointer = getelementptr %ResumptionValue, %Resumption %resumption, i64 0, i32 0
    %referenceCount = load %ReferenceCount, ptr %referenceCount_pointer
    %referenceCount.1 = add %ReferenceCount %referenceCount, 1
    store %ReferenceCount %referenceCount.1, ptr %referenceCount_pointer
    ret void
}

define void @eraseResumption(%Resumption %resumption) alwaysinline {
    %referenceCount_pointer = getelementptr %ResumptionValue, %Resumption %resumption, i64 0, i32 0
    %referenceCount = load %ReferenceCount, ptr %referenceCount_pointer
    switch %ReferenceCount %referenceCount, label %decr [%ReferenceCount 0, label %free]

    decr:
    %referenceCount.1 = sub %ReferenceCount %referenceCount, 1
    store %ReferenceCount %referenceCount.1, ptr %referenceCount_pointer
    ret void

    free:
    %stack_pointer = getelementptr %ResumptionValue, %Resumption %resumption, i64 0, i32 1
    %stack = load %Stack, ptr %stack_pointer
    call void @eraseStack(%Stack %stack)
    ret void
}

define void @eraseStack(%Stack %stack) alwaysinline {
    %stackPointer_pointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 0, i32 0
    %region_pointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 1
    %prompt_pointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 2
    %rest_pointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 3

    %stackPointer = load %StackPointer, ptr %stackPointer_pointer
    %region = load %Region, ptr %region_pointer
    %prompt = load %Stack, ptr %prompt_pointer
    %rest = load %Stack, ptr %rest_pointer

    call void @free(%Stack %stack)
    call void @eraseFrames(%StackPointer %stackPointer)
    call void @eraseRegion(%Region %region)
    call void @erasePrompt(%Prompt %prompt)

    %isNull = icmp eq %Stack %rest, null
    br i1 %isNull, label %done, label %next

next:
    call void @eraseStack(%Stack %rest)
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

define private tailcc void @topLevel(%Pos %val, %Stack %stack) {
    %rest = call %Stack @underflowStack(%Stack %stack)
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

define private %Stack @withEmptyStack() {
    ; TODO all stacks share the same source of fresh prompts
    %stack = call %Stack @reset(%Stack null)

    %stackStackPointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 0, i32 0
    %stackPointer = load %StackPointer, ptr %stackStackPointer

    %returnAddressPointer = getelementptr %FrameHeader, %StackPointer %stackPointer, i64 0, i32 0
    %sharerPointer = getelementptr %FrameHeader, %StackPointer %stackPointer, i64 0, i32 1
    %eraserPointer = getelementptr %FrameHeader, %StackPointer %stackPointer, i64 0, i32 2

    store %ReturnAddress @topLevel, ptr %returnAddressPointer
    store %Sharer @topLevelSharer, ptr %sharerPointer
    store %Eraser @topLevelEraser, ptr %eraserPointer

    %stackPointer_2 = getelementptr %FrameHeader, %StackPointer %stackPointer, i64 1
    store %StackPointer %stackPointer_2, ptr %stackStackPointer

    ret %Stack %stack
}

define void @resume_Int(%Stack %stack, %Int %argument) {
    %stackPointer = call ccc %StackPointer @stackDeallocate(%Stack %stack, i64 24)
    %returnAddressPointer = getelementptr %FrameHeader, %StackPointer %stackPointer, i64 0, i32 0
    %returnAddress = load %ReturnAddress, ptr %returnAddressPointer
    tail call tailcc void %returnAddress(%Int %argument, %Stack %stack)
    ret void
}

define void @resume_Pos(%Stack %stack, %Pos %argument) {
    %stackPointer = call ccc %StackPointer @stackDeallocate(%Stack %stack, i64 24)
    %returnAddressPointer = getelementptr %FrameHeader, %StackPointer %stackPointer, i64 0, i32 0
    %returnAddress = load %ReturnAddress, ptr %returnAddressPointer
    tail call tailcc void %returnAddress(%Pos %argument, %Stack %stack)
    ret void
}

define void @run(%Neg %f) {
    ; fresh stack
    %stack = call %Stack @withEmptyStack()

    ; prepare call
    %arrayPointer = extractvalue %Neg %f, 0
    %object = extractvalue %Neg %f, 1
    %functionPointerPointer = getelementptr ptr, ptr %arrayPointer, i64 0
    %functionPointer = load ptr, ptr %functionPointerPointer

    ; call
    tail call tailcc %Pos %functionPointer(%Object %object, %Stack %stack)
    ret void
}

define void @run_Int(%Neg %f, i64 %argument) {
    ; fresh stack
    %stack = call %Stack @withEmptyStack()

    ; prepare call
    %arrayPointer = extractvalue %Neg %f, 0
    %object = extractvalue %Neg %f, 1
    %functionPointerPointer = getelementptr ptr, ptr %arrayPointer, i64 0
    %functionPointer = load ptr, ptr %functionPointerPointer

    ; call
    tail call tailcc %Pos %functionPointer(%Object %object, %Evidence 0, i64 %argument, %Stack %stack)
    ret void
}

define void @run_Pos(%Neg %f, %Pos %argument) {
    ; fresh stack
    %stack = call %Stack @withEmptyStack()

    ; prepare call
    %arrayPointer = extractvalue %Neg %f, 0
    %object = extractvalue %Neg %f, 1
    %functionPointerPointer = getelementptr ptr, ptr %arrayPointer, i64 0
    %functionPointer = load ptr, ptr %functionPointerPointer

    ; call
    tail call tailcc %Pos %functionPointer(%Object %object, %Evidence 0, %Pos %argument, %Stack %stack)
    ret void
}
