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

; Helpers defined in C
declare void @timer(i64, %Neg)
declare i64 @openFile(%Pos, %Neg)
declare i64 @readFile(i64, %Pos, i64, %Neg)
