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
declare void @c_file_read(i32, %Pos, i64, ptr, ptr)
declare void @c_file_write(i32, %Pos, i64, ptr, ptr)
declare %Pos @c_filedescriptor_show(%Pos)

declare void @c_open_file(%Pos, %Pos, %Stack)
declare void @c_read_file(%Int, %Pos, %Int, %Stack)
declare void @c_write_file(%Int, %Pos, %Int, %Stack)
declare void @c_close_file(%Int, %Stack)
declare void @c_wait_timer(%Int, %Stack)

declare %Pos @c_promise_make()
declare void @c_promise_resolve(%Pos, %Pos)
declare void @c_promise_await(%Pos, %Neg)