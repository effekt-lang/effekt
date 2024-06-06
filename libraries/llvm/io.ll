declare i64 @uv_error_to_errno(i64)
declare ptr @uv_strerror(i32)

declare ptr @uv_default_loop()
declare i32 @uv_run(ptr, i32)
declare void @uv_stop(ptr)
declare void @uv_loop_close(ptr)

; Helpers defined in C
declare void @c_timer_wait(i64, %Neg)
declare i64 @c_file_open(%Pos, %Pos, %Neg, %Neg)
declare void @c_file_close(%Pos)
declare void @c_file_read([2 x i64], [2 x i64], i64, [2 x i64], [2 x i64])
declare void @c_file_write(%Pos, %Pos, i64, %Neg, %Neg)
declare %Pos @c_filedescriptor_show(%Pos)

declare %Pos @c_promise_make()
declare void @c_promise_resolve(%Pos, %Pos)
declare void @c_promise_await(%Pos, %Neg)