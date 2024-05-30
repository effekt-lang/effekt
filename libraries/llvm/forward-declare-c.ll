; forward-declared from primitives.c

declare i64 @c_get_argc()
declare %Pos @c_get_arg(i64)

declare void @c_io_println_Boolean(%Pos)
declare void @c_io_println_Int(%Int)
declare void @c_io_println_Double(%Double)
declare void @c_io_println_String(%Pos)

declare %Pos @c_buffer_construct(i64, ptr)
declare void @c_buffer_refcount_increment(%Pos)
declare void @c_buffer_refcount_decrement(%Pos)
declare %Pos @c_buffer_concatenate(%Pos, %Pos)
declare ptr @c_buffer_as_null_terminated_string(%Pos)
declare %Pos @c_buffer_construct_from_null_terminated_string(ptr)
declare ptr @c_buffer_construct_uninitialized(i64)
declare ptr @c_buffer_bytes(ptr)

declare void @hole()

declare %Pos @c_buffer_eq(%Pos, %Pos)
declare %Pos @c_buffer_substring(%Pos, i64, i64)
declare i64 @c_buffer_length(%Pos)
declare i64 @c_buffer_toInt(%Pos)
declare %Pos @c_buffer_show_Int(i64)
declare %Pos @c_buffer_show_Char(i32)
declare %Pos @c_buffer_show_Byte(i8)
declare %Pos @c_buffer_show_Double(double)
declare %Int @c_buffer_index(%Pos, i64)
declare void @c_buffer_set(%Pos, i64, i8)
declare %Int @c_buffer_character_at(%Pos, i64)
