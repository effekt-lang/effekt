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


declare %struct.uv_loop_s* @uv_default_loop()
declare i32 @uv_run(%struct.uv_loop_s*, i32)
declare void @uv_stop(%struct.uv_loop_s*)
declare void @uv_loop_close(%struct.uv_loop_s*)

declare i32 @uv_timer_init(%struct.uv_loop_s*, %struct.uv_timer_s*)
declare i32 @uv_timer_start(%struct.uv_timer_s*, void (i8*)*, i64, i64)
declare i32 @uv_timer_stop(%struct.uv_timer_s*) #1

declare i32 @uv_fs_close(%struct.uv_loop_s*, %struct.uv_fs_s*, i32, void (%struct.uv_fs_s*)*) #1
declare void @uv_fs_req_cleanup(%struct.uv_fs_s*) #1
declare i32 @uv_fs_read(%struct.uv_loop_s*, %struct.uv_fs_s*, i32, %struct.uv_buf_t*, i32, i64, void (%struct.uv_fs_s*)*) #1
declare i32 @uv_fs_open(%struct.uv_loop_s*, %struct.uv_fs_s*, i8*, i32, i32, void (%struct.uv_fs_s*)*) #1


; The following is copy and pasted from C compiled to LLVM IR, where do we get this from???

; Lib UV Types

%struct.uv_loop_s = type { i8*, i32, %struct.uv__queue, %union.anon, i8*, i32, i64, i32, %struct.uv__queue, %struct.uv__queue, %struct.uv__io_s**, i32, i32, %struct.uv__queue, %struct._opaque_pthread_mutex_t, %struct.uv_async_s, %struct._opaque_pthread_rwlock_t, %struct.uv_handle_s*, %struct.uv__queue, %struct.uv__queue, %struct.uv__queue, %struct.uv__queue, %struct.uv__queue, void ()*, %struct.uv__io_s, i32, %struct.anon, i64, i64, [2 x i32], %struct.uv__io_s, %struct.uv_signal_s, i32, %struct._opaque_pthread_t*, i8*, i8*, %struct._opaque_pthread_mutex_t, i32, %struct.uv__queue }

%union.anon = type { i8* }
%struct.uv_async_s = type { i8*, %struct.uv_loop_s*, i32, void (%struct.uv_handle_s*)*, %struct.uv__queue, %union.anon.1, %struct.uv_handle_s*, i32, void (%struct.uv_async_s*)*, %struct.uv__queue, i32 }
%union.anon.1 = type { [4 x i8*] }
%struct._opaque_pthread_rwlock_t = type { i64, [192 x i8] }
%struct.anon = type { i8*, i32 }
%struct.uv__io_s = type { void (%struct.uv_loop_s*, %struct.uv__io_s*, i32)*, %struct.uv__queue, %struct.uv__queue, i32, i32, i32, i32, i32 }
%struct.uv_signal_s = type { i8*, %struct.uv_loop_s*, i32, void (%struct.uv_handle_s*)*, %struct.uv__queue, %union.anon.2, %struct.uv_handle_s*, i32, void (%struct.uv_signal_s*, i32)*, i32, %struct.anon.3, i32, i32 }
%union.anon.2 = type { [4 x i8*] }
%struct.anon.3 = type { %struct.uv_signal_s*, %struct.uv_signal_s*, %struct.uv_signal_s*, i32 }
%struct._opaque_pthread_t = type { i64, %struct.__darwin_pthread_handler_rec*, [8176 x i8] }
%struct.__darwin_pthread_handler_rec = type { void (i8*)*, i8*, %struct.__darwin_pthread_handler_rec* }
%struct._opaque_pthread_mutex_t = type { i64, [56 x i8] }
%struct.uv__queue = type { %struct.uv__queue*, %struct.uv__queue* }
%union.anon.4 = type { [4 x i8*] }
%struct.uv_handle_s = type { i8*, %struct.uv_loop_s*, i32, {}*, %struct.uv__queue, %union.anon.0, %struct.uv_handle_s*, i32 }
%union.anon.0 = type { [4 x i8*] }
%union.anon.5 = type { [3 x i8*] }


; Timer Types
%struct.uv_timer_s = type { i8*, %struct.uv_loop_s*, i32, void (%struct.uv_handle_s*)*, %struct.uv__queue, %union.anon.4, %struct.uv_handle_s*, i32, {}*, %union.anon.5, i64, i64, i64 }

; Buffers
%struct.__sbuf = type { i8*, i32 }
%struct.uv_buf_t = type { i8*, i64 }

declare [2 x i64] @uv_buf_init(i8*, i32) #1

; Filesystem Types

; Result type (defined for async IO in Effekt) of read/write operations
; - the buffer that has been read from / written to
; - the fs result (bytes read/written)
; - the callback as a closure
%fs_rw_result = type { %struct.uv_buf_t, i64, %Neg }

; Result type (defined for async IO in Effekt) of the file-open operation
; - the file descriptor (extended to 64bit)
; - the callback as a closure
%fs_open_result = type { i64, %Neg }

%struct.__sFILE = type { i8*, i32, i32, i16, i16, %struct.__sbuf, i32, i8*, i32 (i8*)*, i32 (i8*, i8*, i32)*, i64 (i8*, i64, i32)*, i32 (i8*, i8*, i32)*, %struct.__sbuf, %struct.__sFILEX*, i32, [3 x i8], [1 x i8], %struct.__sbuf, i32, i64 }
%struct.__sFILEX = type opaque

%struct.uv_fs_s = type { i8*, i32, [6 x i8*], i32, %struct.uv_loop_s*, {}*, i64, i8*, i8*, %struct.uv_stat_t, i8*, i32, i32, i16, i32, %struct.uv_buf_t*, i64, i32, i32, double, double, %struct.uv__work, [4 x %struct.uv_buf_t] }

%struct.uv_stat_t = type { i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, %struct.uv_timespec_t, %struct.uv_timespec_t, %struct.uv_timespec_t, %struct.uv_timespec_t }
%struct.uv_timespec_t = type { i64, i64 }
%struct.uv__work = type { void (%struct.uv__work*)*, void (%struct.uv__work*, i32)*, %struct.uv_loop_s*, %struct.uv__queue }



; External function declarations
declare i32 @printf(ptr, ...)
declare i32 @puts(ptr)

@output = private unnamed_addr constant [15 x i8] c"out/output.txt\00", align 1

@timer_msg = private constant [24 x i8] c"Timer callback called!\0A\00"
@stop_msg = private constant [25 x i8] c"Stopping the event loop\0A\00"

; Define the timer callback function
define void @on_timer(%struct.uv_timer_s* %handle) {
entry:
    ; Load the callback
    %data = getelementptr inbounds %struct.uv_timer_s, %struct.uv_timer_s* %handle, i32 0, i32 0
    %f_ptr_untyped = load ptr, ptr %data, align 8
    %f_ptr = bitcast ptr %f_ptr_untyped to %Neg*
    %f_struct = load %Neg, %Neg* %f_ptr, align 8

    ; Free handle and stop this timer instance
    %stop_result = call i32 @uv_timer_stop(%struct.uv_timer_s* %handle)
    call void @free(ptr %handle)

    ; Call the function with the Neg struct
    %res = tail call fastcc %Pos @run(%Neg %f_struct)

    ret void
}

define void @on_read(%struct.uv_fs_s* %req) {
entry:
    ; Extract the number of read bytes from the uv_fs_t structure
    %result_ptr = getelementptr inbounds %struct.uv_fs_s, %struct.uv_fs_s* %req, i32 0, i32 6
    %result = load i64, i64* %result_ptr, align 8

    ; Get the callback
    %data = getelementptr inbounds %struct.uv_fs_s, %struct.uv_fs_s* %req, i32 0, i32 0
    %f_ptr = load ptr, ptr %data, align 8
    %f_neg = bitcast ptr %f_ptr to %Neg*
    %f_struct = load %Neg, %Neg* %f_neg, align 8

    ; Free request structure
    call void @uv_fs_req_cleanup(%struct.uv_fs_s* %req)
    %req_ptr = bitcast %struct.uv_fs_s* %req to ptr
    call void @free(ptr %req_ptr)

    ; Call callback
    %res = tail call fastcc %Pos @run_i64(%Neg %f_struct, i64 %result)

    ret void
}


define void @on_open(%struct.uv_fs_s* %req) {
entry:
    ; Extract the file descriptor from the uv_fs_t structure
    %result_ptr = getelementptr inbounds %struct.uv_fs_s, %struct.uv_fs_s* %req, i32 0, i32 6
    %fd = load i64, i64* %result_ptr, align 8

    ; Get the callback
    %data = getelementptr inbounds %struct.uv_fs_s, %struct.uv_fs_s* %req, i32 0, i32 0
    %f_ptr = load ptr, ptr %data, align 8
    %f_neg = bitcast ptr %f_ptr to %Neg*
    %f_struct = load %Neg, %Neg* %f_neg, align 8

    ; Free request structure
    call void @uv_fs_req_cleanup(%struct.uv_fs_s* %req)
    %req_ptr = bitcast %struct.uv_fs_s* %req to ptr
    call void @free(ptr %req_ptr)

    ; Call callback
    %res = tail call fastcc %Pos @run_i64(%Neg %f_struct, i64 %fd)

    ret void
}


define %Pos @readFile(i64 %fd, %Pos %buffer, i64 %offset, %Neg %callback) {
    ; Get the default loop, stop and close it.
    %loop = call %struct.uv_loop_s* @uv_default_loop()

    %buffer_data = call ptr @c_buffer_bytes(%Pos %buffer)
    %len64 = call i64 @c_buffer_length(%Pos %buffer)

    ; Convert len from 64bit to 32bit
    %len32 = trunc i64 %len64 to i32

    %buf = alloca %struct.uv_buf_t
    %buf_init = call %struct.uv_buf_t @uv_buf_init(i8* %buffer_data, i32 %len32)
    store %struct.uv_buf_t %buf_init, %struct.uv_buf_t* %buf

    ; Convert fd from 64bit to 32bit
    %fd32 = trunc i64 %fd to i32

    %req_mem = call ptr @malloc(i64 440) ; 440 happens to be sizeof(uv_fs_t)
    %req = bitcast ptr %req_mem to %struct.uv_fs_s* ; Cast the allocated memory to a uv_fs_s pointer

    ; Allocate memory for the callback (of type %Neg)
    %neg_mem = call ptr @malloc(i64 16) ; two pointers, so 16 byte
    %neg_ptr = bitcast ptr %neg_mem to %Neg*
    store %Neg %callback, %Neg* %neg_ptr

    ; Cast the Neg pointer to ptr and store it in the req's data field
    %data_ptr = getelementptr inbounds %struct.uv_fs_s, %struct.uv_fs_s* %req, i32 0, i32 0
    store ptr %neg_mem, ptr %data_ptr, align 8

    %result = call i32 @uv_fs_read(
      %struct.uv_loop_s* %loop,
      %struct.uv_fs_s* %req,
      i32 %fd32,
      %struct.uv_buf_t* %buf,
      i32 1,
      i64 %offset,
      ptr @on_read)

    ; return Unit
    ret %Pos zeroinitializer
}

define %Pos @openFile(%Pos %path, %Neg %callback) {
    %req_mem = call ptr @malloc(i64 440) ; 440 happens to be sizeof(uv_fs_t)
    %req = bitcast ptr %req_mem to %struct.uv_fs_s* ; Cast the allocated memory to a uv_fs_s pointer

    ; TODO this leaks memory: store the string (together with the callback) in the data-field
    %path_str = call ptr @c_buffer_as_null_terminated_string(%Pos %path)
    call void @c_buffer_refcount_decrement(%Pos %path)

    ; Allocate memory for the callback (of type %Neg)
    %neg_mem = call ptr @malloc(i64 16) ; two pointers, so 16 byte
    %neg_ptr = bitcast ptr %neg_mem to %Neg*
    store %Neg %callback, %Neg* %neg_ptr

    ; Cast the Neg pointer to ptr and store it in the eq's data field
    %data_ptr = getelementptr inbounds %struct.uv_fs_s, %struct.uv_fs_s* %req, i32 0, i32 0
    store ptr %neg_mem, ptr %data_ptr, align 8

    ; Get the default loop and call fs_open
    %loop = call %struct.uv_loop_s* @uv_default_loop()

    %result = call i32 @uv_fs_open(
      %struct.uv_loop_s* %loop,
      %struct.uv_fs_s* %req,
      ptr %path_str,
      i32 0,
      i32 0,
      void (%struct.uv_fs_s*)* @on_open)
    ; return Unit
    ret %Pos zeroinitializer
}

define %Pos @timer(i64 %n, %Neg %callback) {
    ; Get the default loop
    %loop = call %struct.uv_loop_s* @uv_default_loop()

    ; Allocate memory for the timer handle
    %timer_mem = call ptr @malloc(i64 152) #3 ; Here, 152 is a magic number generated by Clang from SIZE_OF struct.uv_timer_s
    %timer = bitcast ptr %timer_mem to %struct.uv_timer_s*

    ; Initialize the timer handle
    %init_result = call i32 @uv_timer_init(%struct.uv_loop_s* %loop, %struct.uv_timer_s* %timer)

    ; Allocate memory for the callback (of type %Neg)
    %neg_mem = call ptr @malloc(i64 16) ; two pointers, so 16 byte
    %neg_ptr = bitcast ptr %neg_mem to %Neg*
    store %Neg %callback, %Neg* %neg_ptr

    ; Cast the Neg pointer to ptr and store it in the timer's data field
    %data_ptr = getelementptr inbounds %struct.uv_timer_s, %struct.uv_timer_s* %timer, i32 0, i32 0
    store ptr %neg_mem, ptr %data_ptr, align 8

    ; Start the timer to call the callback after n ms
    %start_result = call i32 @uv_timer_start(%struct.uv_timer_s* %timer, void (%struct.uv_timer_s*)* @on_timer, i64 %n, i64 0)

    ; Return Unit
    ret %Pos zeroinitializer
}