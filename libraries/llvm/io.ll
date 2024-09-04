declare i64 @uv_error_to_errno(i64)
declare ptr @uv_strerror(i32)

declare ptr @uv_default_loop()
declare i32 @uv_run(ptr, i32)
declare void @uv_stop(ptr)
declare void @uv_loop_close(ptr)

declare void @c_fs_open(%Pos, %Pos, %Stack)
declare void @c_fs_read(%Int, %Pos, %Int, %Stack)
declare void @c_fs_write(%Int, %Pos, %Int, %Stack)
declare void @c_fs_close(%Int, %Stack)
declare void @c_timer_start(%Int, %Stack)
declare %Int @c_error_number(%Int)

declare %Pos @c_promise_make()
declare void @c_promise_resolve(%Pos, %Pos)
declare void @c_promise_await(%Pos, %Neg)