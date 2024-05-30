; Ideas behind the LLVM / libuv implementation.
;
; Share memory between Effekt-buffers and libuv buffers
; -----------------------------------------------------
; A libuv buffer is a {ptr, i64} where i64 is the capacity
; while our buffer (from buffer.c, which is ref-counted) is a %Pos with a
; pointer and a length. The data ptr is shared between the two such that
; libuv (and underlying mechanisms) can directly write into our buffer without
; copying. Since our buffer is ref-counted, this has the advantage that we
; do not need to manually memory manage the buffer.
;
;
; Callbacks in Data-fields
; ------------------------
; In order to call Effekt-functions as a callback, we store a pointer
; to their closure into the user-definable data-field (at address 0)
; in each request object.
;
;
; TODO
; - Error reporting
; - pooling of request objects (benchmark first!)



declare i64 @uv_error_to_errno(i64)
declare ptr @uv_strerror(i32)

declare ptr @uv_default_loop()
declare i32 @uv_run(ptr, i32)
declare void @uv_stop(ptr)
declare void @uv_loop_close(ptr)

declare i32 @uv_timer_init(ptr, ptr)
declare i32 @uv_timer_start(ptr, ptr, i64, i64)
declare i32 @uv_timer_stop(ptr) #1

declare i32 @uv_fs_close(ptr, ptr, i32, ptr) #1
declare void @uv_fs_req_cleanup(ptr) #1
declare i32 @uv_fs_read(ptr, ptr, i32, ptr, i32, i64, ptr) #1
declare i32 @uv_fs_open(ptr, ptr, ptr, i32, i32, ptr) #1


; Lib UV Types
; ... copy and pasted from C compiled to LLVM IR, then simplified. Where do we get this from, other than writing the bindings in C???


%struct.uv_loop_s = type opaque

%struct.uv__queue = type { ptr, ptr }
%union.anon.4 = type { [4 x ptr] }
%union.anon.5 = type { [3 x ptr] }

; Timer Types
%struct.uv_timer_s = type { ptr, ptr, i32, ptr, %struct.uv__queue, %union.anon.4, ptr, i32, {}*, %union.anon.5, i64, i64, i64 }

; Buffers
%struct.uv_buf_t = type { ptr, i64 } ; TODO apparently the order of ptr and capacity is not fixed!

declare [2 x i64] @uv_buf_init(ptr, i32) #1

; Filesystem Types

; Result type (defined for async IO in Effekt) of read/write operations
; - the buffer that has been read from / written to
; - the fs result (bytes read/written)
; - the callback as a closure
%fs_rw_result = type { %struct.uv_buf_t, i64, %Neg }

%struct.uv_fs_s = type { ptr, i32, [6 x ptr], i32, ptr, {}*, i64, ptr, ptr, %struct.uv_stat_t, ptr, i32, i32, i16, i32, ptr, i64, i32, i32, double, double, %struct.uv__work, [4 x %struct.uv_buf_t] }

%struct.uv_stat_t = type { i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, %struct.uv_timespec_t, %struct.uv_timespec_t, %struct.uv_timespec_t, %struct.uv_timespec_t }
%struct.uv_timespec_t = type { i64, i64 }
%struct.uv__work = type { ptr, ptr, ptr, %struct.uv__queue }



; Opening a File
; --------------

; Result type (defined for async IO in Effekt) of the file-open operation
; - a pointer to the 0-terminated string (only used for memory management at the moment)
; - the callback as a closure
%fs_open_payload = type { ptr, %Neg }

define i64 @openFile(%Pos %path, %Neg %callback) {
    %req = call ptr @malloc(i64 440) ; 440 happens to be sizeof(uv_fs_t)

    ; Convert the Effekt String to a 0-terminated string
    %path_str = call ptr @c_buffer_as_null_terminated_string(%Pos %path)
    call void @c_buffer_refcount_decrement(%Pos %path)

    ; Allocate memory for %fs_open_payload and store the path and callback into it
    %payload = call ptr @malloc(i64 24) ; three pointers, so 24 byte

    %payload_path = getelementptr inbounds %fs_open_payload, ptr %payload, i32 0, i32 0
    store ptr %path_str, ptr %payload_path, align 8

    %payload_callback = getelementptr inbounds %fs_open_payload, ptr %payload, i32 0, i32 1
    store %Neg %callback, ptr %payload_callback, align 8

    ; Store the payload in the req's data field
    %req_data = getelementptr inbounds %struct.uv_fs_s, ptr %req, i32 0, i32 0
    store ptr %payload, ptr %req_data, align 8

    ; Get the default loop and call fs_open
    %loop = call ptr @uv_default_loop()

    %result_i32 = call i32 @uv_fs_open(ptr %loop, ptr %req, ptr %path_str, i32 0, i32 0, ptr @on_open)
    %result_i64 = sext i32 %result_i32 to i64

    ret i64 %result_i64
}

define void @on_open(ptr %req) {
entry:
    ; Extract the file descriptor from the uv_fs_t structure
    %result_ptr = getelementptr inbounds %struct.uv_fs_s, ptr %req, i32 0, i32 6
    %fd = load i64, i64* %result_ptr, align 8

    ; Load the payload
    %req_data = getelementptr inbounds %struct.uv_fs_s, ptr %req, i32 0, i32 0
    %payload = load ptr, ptr %req_data, align 8

    ; Extract the callback from the payload
    %payload_callback = getelementptr inbounds %fs_open_payload, ptr %payload, i32 0, i32 1
    %f = load %Neg, ptr %payload_callback, align 8

    ; Extract the path string from the payload
    %payload_path = getelementptr inbounds %fs_open_payload, ptr %payload, i32 0, i32 0
    %path = load ptr, ptr %payload_path, align 8

    ; Free request structure
    call void @uv_fs_req_cleanup(ptr %req)
    call void @free(ptr %req)

    ; Free the payload memory
    call void @free(ptr %payload)

    ; Free the path string
    call void @free(ptr %path)

    ; Call callback
    %res = tail call fastcc %Pos @run_i64(%Neg %f, i64 %fd)

    ret void
}


; Reading a File
; --------------

define %Pos @readFile(i64 %fd, %Pos %buffer, i64 %offset, %Neg %callback) {
    ; Get the default loop, stop and close it.
    %loop = call ptr @uv_default_loop()

    %buffer_data = call ptr @c_buffer_bytes(%Pos %buffer)
    %len64 = call i64 @c_buffer_length(%Pos %buffer)

    ; Convert len from 64bit to 32bit
    %len32 = trunc i64 %len64 to i32

    %buf = alloca %struct.uv_buf_t
    %buf_init = call %struct.uv_buf_t @uv_buf_init(ptr %buffer_data, i32 %len32)
    store %struct.uv_buf_t %buf_init, ptr %buf

    ; Convert fd from 64bit to 32bit
    %fd32 = trunc i64 %fd to i32

    %req = call ptr @malloc(i64 440) ; 440 happens to be sizeof(uv_fs_t)

    ; Allocate memory for the callback (of type %Neg)
    %neg = call ptr @malloc(i64 16) ; two pointers, so 16 byte
    store %Neg %callback, ptr %neg

    ; Store the Neg pointer in the req's data field
    %data_ptr = getelementptr inbounds %struct.uv_fs_s, ptr %req, i32 0, i32 0
    store ptr %neg, ptr %data_ptr, align 8

    ; Argument `1` here means: we pass exactly one buffer
    %result = call i32 @uv_fs_read(ptr %loop, ptr %req, i32 %fd32, ptr %buf, i32 1, i64 %offset, ptr @on_read)

    ; return Unit
    ret %Pos zeroinitializer
}

define void @on_read(ptr %req) {
entry:
    ; Extract the number of read bytes from the uv_fs_t structure
    %result_ptr = getelementptr inbounds %struct.uv_fs_s, ptr %req, i32 0, i32 6
    %result = load i64, ptr %result_ptr, align 8

    ; Get the callback
    %data = getelementptr inbounds %struct.uv_fs_s, ptr %req, i32 0, i32 0
    %f_ptr = load ptr, ptr %data, align 8
    %f_struct = load %Neg, ptr %f_ptr, align 8

    ; Free request structure
    call void @uv_fs_req_cleanup(ptr %req)
    call void @free(ptr %req)

    ; Call callback
    %res = tail call fastcc %Pos @run_i64(%Neg %f_struct, i64 %result)

    ret void
}


; Timer
; -----

define %Pos @timer(i64 %n, %Neg %callback) {
    ; Get the default loop
    %loop = call %struct.uv_loop_s* @uv_default_loop()

    ; Allocate memory for the timer handle
    %timer = call ptr @malloc(i64 152) #3 ; Here, 152 is a magic number generated by Clang from SIZE_OF struct.uv_timer_s

    ; Initialize the timer handle
    %init_result = call i32 @uv_timer_init(%struct.uv_loop_s* %loop, ptr %timer)

    ; Allocate memory for the callback (of type %Neg)
    %neg = call ptr @malloc(i64 16) ; two pointers, so 16 byte
    store %Neg %callback, ptr %neg

    ; Cast the Neg pointer to ptr and store it in the timer's data field
    %data_ptr = getelementptr inbounds %struct.uv_timer_s, ptr %timer, i32 0, i32 0
    store ptr %neg, ptr %data_ptr, align 8

    ; Start the timer to call the callback after n ms
    %start_result = call i32 @uv_timer_start(ptr %timer, ptr @on_timer, i64 %n, i64 0)

    ; Return Unit
    ret %Pos zeroinitializer
}

define void @on_timer(ptr %handle) {
entry:
    ; Load the callback
    %data = getelementptr inbounds %struct.uv_timer_s, ptr %handle, i32 0, i32 0
    %f_ptr = load ptr, ptr %data, align 8
    %f_struct = load %Neg, ptr %f_ptr, align 8

    ; Free handle and stop this timer instance
    %stop_result = call i32 @uv_timer_stop(ptr %handle)
    call void @free(ptr %handle)

    ; Call the function with the Neg struct
    %res = tail call fastcc %Pos @run(%Neg %f_struct)

    ret void
}