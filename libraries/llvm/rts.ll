; Run-Time System

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

; Unique address for each handler.
%Prompt = type ptr

; A Continuation capturing a list of stacks.
; This points to the last element in a cyclic linked list of StackValues
%Resumption = type ptr

; The "meta" stack (a stack of stacks) -- a pointer to a %StackValue
%Stack = type ptr

; Lives in a stable address
%PromptValue = type { %ReferenceCount, %Stack }

; This is used for two purposes:
;   - a refied first-class list of stacks (cyclic linked-list)
;   - as part of an intrusive linked-list of stacks (meta stack)
%StackValue = type { %ReferenceCount, %StackPointer, %Limit, %Prompt, %Stack }



; Positive data types consist of a (type-local) tag and a heap object
%Pos = type {i64, %Object}

; Negative types (codata) consist of a vtable and a heap object
%Neg = type {ptr, %Object}

; Reference to a mutable variable (prompt, offset)
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
declare double @llvm.cos.f64(double)
declare double @llvm.sin.f64(double)
declare double @llvm.log.f64(double)
declare double @llvm.exp.f64(double)
declare double @llvm.pow.f64(double, double)
declare double @log1p(double)
; Intrinsic versions of the following two only added in LLVM 19
declare double @atan(double)
declare double @tan(double)
declare void @print(i64)
declare void @exit(i64)
declare void @llvm.assume(i1)


; Prompts

define private %Prompt @currentPrompt(%Stack %stack) {
    %prompt_pointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 3
    %prompt = load %Prompt, ptr %prompt_pointer, !alias.scope !11, !noalias !21
    ret %Prompt %prompt
}

define private %Prompt @freshPrompt() {
    %promptSize = ptrtoint ptr getelementptr (%PromptValue, ptr null, i64 1) to i64
    %prompt = call %Prompt @malloc(i64 %promptSize)
    store %PromptValue zeroinitializer, %Prompt %prompt, !alias.scope !13, !noalias !23
    ret %Prompt %prompt
}

; Garbage collection

define private %Object @newObject(%Eraser %eraser, i64 %environmentSize) alwaysinline {
    %headerSize = ptrtoint ptr getelementptr (%Header, ptr null, i64 1) to i64
    %size = add i64 %environmentSize, %headerSize
    %object = call ptr @malloc(i64 %size)
    %objectReferenceCount = getelementptr %Header, ptr %object, i64 0, i32 0
    %objectEraser = getelementptr %Header, ptr %object, i64 0, i32 1
    store %ReferenceCount 0, ptr %objectReferenceCount, !alias.scope !14, !noalias !24
    store %Eraser %eraser, ptr %objectEraser, !alias.scope !14, !noalias !24
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
    %referenceCount = load %ReferenceCount, ptr %objectReferenceCount, !alias.scope !14, !noalias !24
    %referenceCount.1 = add %ReferenceCount %referenceCount, 1
    store %ReferenceCount %referenceCount.1, ptr %objectReferenceCount, !alias.scope !14, !noalias !24
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
    %referenceCount = load %ReferenceCount, ptr %objectReferenceCount, !alias.scope !14, !noalias !24
    switch %ReferenceCount %referenceCount, label %decr [%ReferenceCount 0, label %free]

    decr:
    %referenceCount.1 = sub %ReferenceCount %referenceCount, 1
    store %ReferenceCount %referenceCount.1, ptr %objectReferenceCount, !alias.scope !14, !noalias !24
    ret void

    free:
    %objectEraser = getelementptr %Header, ptr %object, i64 0, i32 1
    %eraser = load %Eraser, ptr %objectEraser, !alias.scope !14, !noalias !24
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

define private %Stack @getStack(%Prompt %prompt) {
    %stack_pointer = getelementptr %PromptValue, %Prompt %prompt, i64 0, i32 1
    %stack = load %Stack, ptr %stack_pointer, !alias.scope !13, !noalias !23
    ret %Stack %stack
}

define private ptr @getVarPointer(%Reference %reference, %Stack %stack) {
    %prompt = extractvalue %Reference %reference, 0
    %offset = extractvalue %Reference %reference, 1

    %targetStack = call %Stack @getStack(%Prompt %prompt)
    %varPointer = getelementptr i8, %Base %targetStack, i64 %offset
    ret ptr %varPointer
}

define private %Reference @newReference(%Stack %stack) alwaysinline {
    %stackPointer_pointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 1
    %stackPointer = load %StackPointer, ptr %stackPointer_pointer, !alias.scope !11, !noalias !21

    %intStack = ptrtoint %StackPointer %stackPointer to i64
    %intBase = ptrtoint %Stack %stack to i64

    %offset = sub i64 %intStack, %intBase

    %prompt = call %Prompt @currentPrompt(%Stack %stack)

    %reference..1 = insertvalue %Reference undef, %Prompt %prompt, 0
    %reference = insertvalue %Reference %reference..1, i64 %offset, 1

    ret %Reference %reference
}

; Stack management

define private %Stack @checkLimit(%Stack %stack, i64 %n) alwaysinline {
    %stackPointer_pointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 1
    %limit_pointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 2

    %currentStackPointer = load %StackPointer, ptr %stackPointer_pointer, !alias.scope !11, !noalias !21
    %limit = load %Limit, ptr %limit_pointer, !alias.scope !11, !noalias !21
    %nextStackPointer = getelementptr i8, %StackPointer %currentStackPointer, i64 %n
    %isInside = icmp ule %StackPointer %nextStackPointer, %limit
    br i1 %isInside, label %done, label %realloc

done:
    ret %Stack %stack

realloc:
    %intStackPointer = ptrtoint %StackPointer %currentStackPointer to i64
    %intBase = ptrtoint %Stack %stack to i64

    %size = sub i64 %intStackPointer, %intBase
    %nextSize = add i64 %size, %n
    %newSize = call i64 @nextPowerOfTwo(i64 %nextSize)

    %newStack = call ptr @realloc(ptr %stack, i64 %newSize)
    %newLimit = getelementptr i8, %Base %newStack, i64 %newSize
    %newStackPointer = getelementptr i8, %Base %newStack, i64 %size

    %newStackPointer_pointer = getelementptr %StackValue, %Stack %newStack, i64 0, i32 1
    %newLimit_pointer = getelementptr %StackValue, %Stack %newStack, i64 0, i32 2

    store %StackPointer %newStackPointer, ptr %newStackPointer_pointer, !alias.scope !11, !noalias !21
    store %Limit %newLimit, ptr %newLimit_pointer, !alias.scope !11, !noalias !21

    %prompt_pointer = getelementptr %StackValue, %Stack %newStack, i64 0, i32 3
    %prompt = load %Prompt, ptr %prompt_pointer, !alias.scope !11, !noalias !21
    %promptStack = getelementptr %PromptValue, %Prompt %prompt, i64 0, i32 1
    store %Stack %newStack, ptr %promptStack, !alias.scope !13, !noalias !23

    ret %Stack %newStack
}

define private %StackPointer @stackAllocate(%Stack %stack, i64 %n) alwaysinline {
    %stackPointer_pointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 1
    %limit_pointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 2

    %currentStackPointer = load %StackPointer, ptr %stackPointer_pointer, !alias.scope !11, !noalias !21
    %limit = load %Limit, ptr %limit_pointer, !alias.scope !11, !noalias !21
    %nextStackPointer = getelementptr i8, %StackPointer %currentStackPointer, i64 %n

    store %StackPointer %nextStackPointer, ptr %stackPointer_pointer, !alias.scope !11, !noalias !21
    ret %StackPointer %currentStackPointer
}

define private %StackPointer @stackDeallocate(%Stack %stack, i64 %n) {
    %stackPointer_pointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 1
    %stackPointer = load %StackPointer, ptr %stackPointer_pointer, !alias.scope !11, !noalias !21

    %limit_pointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 2
    %limit = load %Limit, ptr %limit_pointer, !alias.scope !11, !noalias !21
    %isInside = icmp ule %StackPointer %stackPointer, %limit
    call void @llvm.assume(i1 %isInside)

    %o = sub i64 0, %n
    %newStackPointer = getelementptr i8, %StackPointer %stackPointer, i64 %o
    store %StackPointer %newStackPointer, ptr %stackPointer_pointer, !alias.scope !11, !noalias !21

    ret %StackPointer %newStackPointer
}

define private i64 @nextPowerOfTwo(i64 %x) {
    %leadingZeros = call i64 @llvm.ctlz.i64(i64 %x, i1 false)
    %numBits = sub i64 64, %leadingZeros
    %result = shl i64 1, %numBits
    ret i64 %result
}

define private void @assumeFrameHeaderWasPopped(%Stack %stack) alwaysinline {
    %stackPointer_pointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 1
    %stackPointer = load %StackPointer, ptr %stackPointer_pointer, !alias.scope !11, !noalias !21
    %oldStackPointer = getelementptr %FrameHeader, %StackPointer %stackPointer, i64 1

    %limit_pointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 2
    %limit = load %Limit, ptr %limit_pointer, !alias.scope !11, !noalias !21
    %isInside = icmp ule %StackPointer %oldStackPointer, %limit
    call void @llvm.assume(i1 %isInside)
    ret void
}

; Meta-stack management

define private %Stack @reset(%Stack %oldStack) {

    %prompt = call %Prompt @freshPrompt()

    %size = shl i64 1, 7
    %stack = call ptr @malloc(i64 %size)
    %stackPointer = getelementptr %StackValue, %Stack %stack, i64 1
    %limit = getelementptr i8, ptr %stack, i64 %size

    %stack.0 = insertvalue %StackValue zeroinitializer, %StackPointer %stackPointer, 1
    %stack.1 = insertvalue %StackValue %stack.0, %Limit %limit, 2
    %stack.2 = insertvalue %StackValue %stack.1, %Prompt %prompt, 3
    %stack.3 = insertvalue %StackValue %stack.2, %Stack %oldStack, 4

    store %StackValue %stack.3, %Stack %stack, !alias.scope !11, !noalias !21

    %promptStack = getelementptr %PromptValue, %Prompt %prompt, i64 0, i32 1
    store %Stack %stack, ptr %promptStack, !alias.scope !13, !noalias !23

    ret %Stack %stack
}

define private void @revalidate(%Stack %stack) {
    %prompt_pointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 3
    %prompt = load %Prompt, ptr %prompt_pointer, !alias.scope !11, !noalias !21
    %stack_pointer = getelementptr %PromptValue, %Prompt %prompt, i64 0, i32 1
    %promptStack = load %Stack, ptr %stack_pointer, !alias.scope !13, !noalias !23
    %isThis = icmp eq %Stack %promptStack, %stack
    br i1 %isThis, label %done, label %continue

done:
    ret void

continue:
    %isOccupied = icmp ne %Stack %promptStack, null
    br i1 %isOccupied, label %displace, label %update

displace:
    call void @invalidate(%Stack %promptStack, %Stack %promptStack)
    br label %update

update:
    store %Stack %stack, ptr %stack_pointer, !alias.scope !13, !noalias !23

    %next_pointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 4
    %next = load %Stack, ptr %next_pointer, !alias.scope !11, !noalias !21
    tail call void @revalidate(%Stack %next)
    ret void
}

; This panics if we invalidate the meta stack
define private void @invalidate(%Stack %stack, %Stack %end) {
    %prompt_pointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 3
    %next_pointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 4
    %prompt = load %Prompt, ptr %prompt_pointer, !alias.scope !11, !noalias !21
    %stack_pointer = getelementptr %PromptValue, %Prompt %prompt, i64 0, i32 1
    store %Stack null, ptr %stack_pointer, !alias.scope !13, !noalias !23

    %next = load %Stack, ptr %next_pointer, !alias.scope !11, !noalias !21

    %isNull = icmp eq %Stack %next, null
    br i1 %isNull, label %error, label %check

error:
    call void @duplicated_prompt()
    unreachable

check:
    %isEnd = icmp eq %Stack %next, %end
    br i1 %isEnd, label %done, label %continue

done:
    ret void

continue:
    tail call void @invalidate(%Stack %next, %Stack %end)
    ret void
}

define private %Stack @resume(%Resumption %resumption, %Stack %oldStack) {
    %uniqueResumption = call %Resumption @uniqueStack(%Resumption %resumption)
    %rest_pointer = getelementptr %StackValue, %Resumption %uniqueResumption, i64 0, i32 4
    %start = load %Stack, ptr %rest_pointer, !alias.scope !11, !noalias !21
    call void @revalidate(%Stack %start)

    store %Stack %oldStack, ptr %rest_pointer, !alias.scope !11, !noalias !21

    ret %Stack %start
}

define private {%Resumption, %Stack} @shift(%Stack %stack, %Prompt %prompt) {
    %resumpion_pointer = getelementptr %PromptValue, %Prompt %prompt, i64 0, i32 1
    %resumption = load %Stack, ptr %resumpion_pointer, !alias.scope !13, !noalias !23
    %next_pointer = getelementptr %StackValue, %Stack %resumption, i64 0, i32 4
    %next = load %Stack, ptr %next_pointer, !alias.scope !11, !noalias !21

    store %Stack %stack, ptr %next_pointer, !alias.scope !11, !noalias !21

    %result.0 = insertvalue {%Resumption, %Stack} undef, %Resumption %resumption, 0
    %result = insertvalue {%Resumption, %Stack} %result.0, %Stack %next, 1
    ret {%Resumption, %Stack} %result
}

define private void @erasePrompt(%Prompt %prompt) alwaysinline {
    %referenceCount_pointer = getelementptr %PromptValue, %Prompt %prompt, i64 0, i32 0
    %referenceCount = load %ReferenceCount, ptr %referenceCount_pointer, !alias.scope !13, !noalias !23
    switch %ReferenceCount %referenceCount, label %decrement [%ReferenceCount 0, label %free]

decrement:
    %newReferenceCount = sub %ReferenceCount %referenceCount, 1
    store %ReferenceCount %newReferenceCount, ptr %referenceCount_pointer, !alias.scope !13, !noalias !23
    ret void

free:
    call void @free(%Prompt %prompt)
    ret void
}

define private void @sharePrompt(%Prompt %prompt) alwaysinline {
    %referenceCount_pointer = getelementptr %PromptValue, %Prompt %prompt, i64 0, i32 0
    %referenceCount = load %ReferenceCount, ptr %referenceCount_pointer, !alias.scope !13, !noalias !23
    %newReferenceCount = add %ReferenceCount %referenceCount, 1
    store %ReferenceCount %newReferenceCount, ptr %referenceCount_pointer, !alias.scope !13, !noalias !23
    ret void
}

define private %Stack @underflowStack(%Stack %stack) {
    %stackPrompt = getelementptr %StackValue, %Stack %stack, i64 0, i32 3
    %stackRest = getelementptr %StackValue, %Stack %stack, i64 0, i32 4

    %prompt = load %Prompt, ptr %stackPrompt, !alias.scope !11, !noalias !21
    %rest = load %Stack, ptr %stackRest, !alias.scope !11, !noalias !21

    %promptStack_pointer = getelementptr %PromptValue, %Prompt %prompt, i64 0, i32 1
    store %Stack null, ptr %promptStack_pointer, !alias.scope !13, !noalias !23

    call void @erasePrompt(%Prompt %prompt)
    call void @free(%Stack %stack)

    ret %Stack %rest
}

define private void @nop(%Stack %stack) {
    ret void
}

define private %Stack @copyStack(%Stack %stack) alwaysinline {
    %stackPointer_pointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 1
    %limit_pointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 2

    %stackPointer = load %StackPointer, ptr %stackPointer_pointer, !alias.scope !11, !noalias !21
    %limit = load %Limit, ptr %limit_pointer, !alias.scope !11, !noalias !21

    %intStackPointer = ptrtoint %StackPointer %stackPointer to i64
    %intBase = ptrtoint %Stack %stack to i64
    %intLimit = ptrtoint %Limit %limit to i64
    %used = sub i64 %intStackPointer, %intBase
    %size = sub i64 %intLimit, %intBase

    %newStack = call ptr @malloc(i64 %size)
    %newStackPointer = getelementptr i8, %Stack %newStack, i64 %used
    %newLimit = getelementptr i8, %Stack %newStack, i64 %size

    call void @memcpy(ptr %newStack, ptr %stack, i64 %used)

    %newStackPointer_pointer = getelementptr %StackValue, %Stack %newStack, i64 0, i32 1
    %newLimit_pointer = getelementptr %StackValue, %Stack %newStack, i64 0, i32 2

    store %StackPointer %newStackPointer, ptr %newStackPointer_pointer, !alias.scope !11, !noalias !21
    store %Limit %newLimit, ptr %newLimit_pointer, !alias.scope !11, !noalias !21

    call void @shareFrames(%StackPointer %newStackPointer)

    %prompt_pointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 3
    %prompt = load %Prompt, ptr %prompt_pointer, !alias.scope !11, !noalias !21
    call void @sharePrompt(%Prompt %prompt)

    %referenceCount_pointer = getelementptr %StackValue, %Stack %newStack, i64 0, i32 0
    store i64 0, ptr %referenceCount_pointer, !alias.scope !11, !noalias !21

    ret %Stack %newStack
}


define private %Resumption @uniqueStack(%Resumption %resumption) alwaysinline {

entry:
    %referenceCount_pointer = getelementptr %StackValue, %Resumption %resumption, i64 0, i32 0
    %referenceCount = load %ReferenceCount, ptr %referenceCount_pointer, !alias.scope !11, !noalias !21
    switch %ReferenceCount %referenceCount, label %copy [%ReferenceCount 0, label %done]

done:
    ret %Resumption %resumption

copy:
    %newOldReferenceCount = sub %ReferenceCount %referenceCount, 1
    store %ReferenceCount %newOldReferenceCount, ptr %referenceCount_pointer, !alias.scope !11, !noalias !21

    %firstCopy = call %Stack @copyStack(%Stack %resumption)

    %stack_pointer = getelementptr %StackValue, %Resumption %resumption, i64 0, i32 4
    %stack = load %Stack, ptr %stack_pointer, !alias.scope !11, !noalias !21

    br label %check

check:
    %current = phi %Stack [%stack, %copy], [%next, %loop]
    %previousCopy = phi %Stack [%firstCopy, %copy], [%newCopy, %loop]

    %rest_pointer = getelementptr %StackValue, %Stack %previousCopy, i64 0, i32 4

    %isEnd = icmp eq %Stack %current, %resumption
    br i1 %isEnd, label %stop, label %loop

loop:

    %newCopy = call %Stack @copyStack(%Stack %current)

    store %Stack %newCopy, ptr %rest_pointer, !alias.scope !11, !noalias !21

    %next_pointer = getelementptr %StackValue, %Stack %current, i64 0, i32 4
    %next = load %Stack, ptr %next_pointer, !alias.scope !11, !noalias !21

    br label %check

stop:
    store %Stack %firstCopy, ptr %rest_pointer, !alias.scope !11, !noalias !21
    ret %Stack %firstCopy
}

define void @shareResumption(%Resumption %resumption) alwaysinline {
    %referenceCount_pointer = getelementptr %StackValue, %Resumption %resumption, i64 0, i32 0
    %referenceCount = load %ReferenceCount, ptr %referenceCount_pointer, !alias.scope !11, !noalias !21
    %referenceCount.1 = add %ReferenceCount %referenceCount, 1
    store %ReferenceCount %referenceCount.1, ptr %referenceCount_pointer, !alias.scope !11, !noalias !21
    ret void
}

define void @eraseResumption(%Resumption %resumption) alwaysinline {
    %referenceCount_pointer = getelementptr %StackValue, %Resumption %resumption, i64 0, i32 0
    %referenceCount = load %ReferenceCount, ptr %referenceCount_pointer, !alias.scope !11, !noalias !21
    switch %ReferenceCount %referenceCount, label %decr [%ReferenceCount 0, label %free]

    decr:
    %referenceCount.1 = sub %ReferenceCount %referenceCount, 1
    store %ReferenceCount %referenceCount.1, ptr %referenceCount_pointer, !alias.scope !11, !noalias !21
    ret void

    free:
    %stack_pointer = getelementptr %StackValue, %Resumption %resumption, i64 0, i32 4
    %stack = load %Stack, ptr %stack_pointer, !alias.scope !11, !noalias !21
    store %Stack null, ptr %stack_pointer, !alias.scope !11, !noalias !21
    call void @eraseStack(%Stack %stack)
    ret void
}

define void @eraseStack(%Stack %stack) alwaysinline {
    %stackPointer_pointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 1
    %prompt_pointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 3
    %rest_pointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 4

    %stackPointer = load %StackPointer, ptr %stackPointer_pointer, !alias.scope !11, !noalias !21
    %prompt = load %Stack, ptr %prompt_pointer, !alias.scope !11, !noalias !21
    %rest = load %Stack, ptr %rest_pointer, !alias.scope !11, !noalias !21

    %promptStack_pointer = getelementptr %PromptValue, %Prompt %prompt, i64 0, i32 1
    %promptStack = load %Stack, ptr %promptStack_pointer, !alias.scope !13, !noalias !23
    %isThisStack = icmp eq %Stack %promptStack, %stack
    br i1 %isThisStack, label %clearPrompt, label %free

clearPrompt:
    store %Stack null, ptr %promptStack_pointer, !alias.scope !13, !noalias !23
    br label %free

free:
    call void @eraseFrames(%StackPointer %stackPointer)
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
    %sharer = load %Sharer, ptr %stackSharer, !alias.scope !12, !noalias !22
    tail call void %sharer(%StackPointer %newStackPointer)
    ret void
}

define private void @eraseFrames(%StackPointer %stackPointer) alwaysinline {
    %newStackPointer = getelementptr %FrameHeader, %StackPointer %stackPointer, i64 -1
    %stackEraser = getelementptr %FrameHeader, %StackPointer %newStackPointer, i64 0, i32 2
    %eraser = load %Eraser, ptr %stackEraser, !alias.scope !12, !noalias !22
    tail call void %eraser(%StackPointer %newStackPointer)
    ret void
}

define private void @freeStack(%StackPointer %stackPointer) alwaysinline {
    %base = getelementptr %StackValue, %StackPointer %stackPointer, i64 -1
    call void @free(%Base %base)
    ret void
}

; RTS initialization

define private tailcc void @topLevel(%Pos %val, %Stack %stack) {
    call %Stack @underflowStack(%Stack %stack)
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
    %stack = call %Stack @reset(%Stack null)

    %stackStackPointer = getelementptr %StackValue, %Stack %stack, i64 0, i32 1
    %stackPointer = load %StackPointer, ptr %stackStackPointer, !alias.scope !11, !noalias !21

    %returnAddressPointer = getelementptr %FrameHeader, %StackPointer %stackPointer, i64 0, i32 0
    %sharerPointer = getelementptr %FrameHeader, %StackPointer %stackPointer, i64 0, i32 1
    %eraserPointer = getelementptr %FrameHeader, %StackPointer %stackPointer, i64 0, i32 2

    store %ReturnAddress @topLevel, ptr %returnAddressPointer, !alias.scope !12, !noalias !22
    store %Sharer @topLevelSharer, ptr %sharerPointer, !alias.scope !12, !noalias !22
    store %Eraser @topLevelEraser, ptr %eraserPointer, !alias.scope !12, !noalias !22

    %stackPointer_2 = getelementptr %FrameHeader, %StackPointer %stackPointer, i64 1
    store %StackPointer %stackPointer_2, ptr %stackStackPointer, !alias.scope !11, !noalias !21

    ret %Stack %stack
}

define void @resume_Int(%Stack %stack, %Int %argument) {
    %stackPointer = call ccc %StackPointer @stackDeallocate(%Stack %stack, i64 24)
    %returnAddressPointer = getelementptr %FrameHeader, %StackPointer %stackPointer, i64 0, i32 0
    %returnAddress = load %ReturnAddress, ptr %returnAddressPointer, !alias.scope !12, !noalias !22
    tail call tailcc void %returnAddress(%Int %argument, %Stack %stack)
    ret void
}

define void @resume_Pos(%Stack %stack, %Pos %argument) {
    %stackPointer = call ccc %StackPointer @stackDeallocate(%Stack %stack, i64 24)
    %returnAddressPointer = getelementptr %FrameHeader, %StackPointer %stackPointer, i64 0, i32 0
    %returnAddress = load %ReturnAddress, ptr %returnAddressPointer, !alias.scope !12, !noalias !22
    tail call tailcc void %returnAddress(%Pos %argument, %Stack %stack)
    ret void
}

define void @run(%Pos %boxed) {
    ; unbox
    %f = call %Neg @coercePosNeg(%Pos %boxed)

    ; fresh stack
    %stack = call %Stack @withEmptyStack()

    ; prepare call
    %arrayPointer = extractvalue %Neg %f, 0
    %object = extractvalue %Neg %f, 1
    %functionPointerPointer = getelementptr ptr, ptr %arrayPointer, i64 0
    %functionPointer = load ptr, ptr %functionPointerPointer, !alias.scope !15, !noalias !25

    ; call
    tail call tailcc %Pos %functionPointer(%Object %object, %Stack %stack)
    ret void
}

define void @run_Int(%Pos %boxed, i64 %argument) {
    ; unbox
    %f = call %Neg @coercePosNeg(%Pos %boxed)

    ; fresh stack
    %stack = call %Stack @withEmptyStack()

    ; prepare call
    %arrayPointer = extractvalue %Neg %f, 0
    %object = extractvalue %Neg %f, 1
    %functionPointerPointer = getelementptr ptr, ptr %arrayPointer, i64 0
    %functionPointer = load ptr, ptr %functionPointerPointer, !alias.scope !15, !noalias !25

    ; call
    tail call tailcc %Pos %functionPointer(%Object %object, i64 %argument, %Stack %stack)
    ret void
}

define void @run_Pos(%Pos %boxed, %Pos %argument) {
    ; unbox
    %f = call %Neg @coercePosNeg(%Pos %boxed)

    ; fresh stack
    %stack = call %Stack @withEmptyStack()

    ; prepare call
    %arrayPointer = extractvalue %Neg %f, 0
    %object = extractvalue %Neg %f, 1
    %functionPointerPointer = getelementptr ptr, ptr %arrayPointer, i64 0
    %functionPointer = load ptr, ptr %functionPointerPointer, !alias.scope !15, !noalias !25

    ; call
    tail call tailcc %Pos %functionPointer(%Object %object, %Pos %argument, %Stack %stack)
    ret void
}


define ccc %Pos @coerceNegPos(%Neg %neg) {
    %arrayPointer = extractvalue %Neg %neg, 0
    %object = extractvalue %Neg %neg, 1
    %tagValue = ptrtoint ptr %arrayPointer to i64
    %boxed1 = insertvalue %Pos undef, i64 %tagValue, 0
    %boxed2 = insertvalue %Pos %boxed1, ptr %object, 1
    ret %Pos %boxed2
}

define ccc %Neg @coercePosNeg(%Pos %pos) {
    %tagValue = extractvalue %Pos %pos, 0
    %object = extractvalue %Pos %pos, 1
    %arrayPointer = inttoptr i64 %tagValue to ptr
    %unboxed1 = insertvalue %Neg undef, ptr %arrayPointer, 0
    %unboxed2 = insertvalue %Neg %unboxed1, ptr %object, 1
    ret %Neg %unboxed2
}

define ccc %Pos @coerceIntPos(%Int %input) {
    %boxed1 = insertvalue %Pos zeroinitializer, i64 %input, 0
    %boxed2 = insertvalue %Pos %boxed1, %Object null, 1
    ret %Pos %boxed2
}

define ccc %Int @coercePosInt(%Pos %input) {
    %unboxed = extractvalue %Pos %input, 0
    ret %Int %unboxed
}

define ccc %Pos @coerceBytePos(%Byte %input) {
    %extended = sext i8 %input to i64
    %boxed1 = insertvalue %Pos zeroinitializer, i64 %extended, 0
    %boxed2 = insertvalue %Pos %boxed1, %Object null, 1
    ret %Pos %boxed2
}

define ccc %Byte @coercePosByte(%Pos %input) {
    %unboxed = extractvalue %Pos %input, 0
    %truncated = trunc i64 %unboxed to i8
    ret i8 %truncated
}

define ccc %Pos @coerceDoublePos(%Double %input) {
    %number = bitcast double %input to i64
    %boxed1 = insertvalue %Pos zeroinitializer, i64 %number, 0
    %boxed2 = insertvalue %Pos %boxed1, %Object null, 1
    ret %Pos %boxed2
}

define ccc %Double @coercePosDouble(%Pos %input) {
    %unboxed = extractvalue %Pos %input, 0
    %d = bitcast i64 %unboxed to double
    ret %Double %d
}


; Scope domains
!0 = !{!"types"}

; Scopes
!1 = !{!"stackBase", !0}
!2 = !{!"stackPointer", !0}
!3 = !{!"prompt", !0}
!4 = !{!"object", !0}
!5 = !{!"vtable", !0}

; Scope lists
!11 = !{!1} ; stackBase
!12 = !{!2} ; stackPointer
!13 = !{!3} ; prompt
!14 = !{!4} ; object
!15 = !{!5} ; vtable

!21 = !{    !2, !3, !4, !5} ; not stackBase
!22 = !{!1,     !3, !4, !5} ; not stackPointer
!23 = !{!1, !2,     !4, !5} ; not prompt
!24 = !{!1, !2, !3,     !5} ; not object
!25 = !{!1, !2, !3, !4    } ; not vtable