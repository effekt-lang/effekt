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
