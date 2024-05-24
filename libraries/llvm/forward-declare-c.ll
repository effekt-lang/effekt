; forward-declared from primitives.c

declare i64 @c_get_argc()
declare %Pos @c_get_arg(i64)

declare void @c_io_println_Boolean(%Pos)
declare void @c_io_println_Int(%Int)
declare void @c_io_println_Double(%Double)
declare void @c_io_println_String(%Pos)

declare %Pos @c_buffer_construct(i64, i8*)
declare void @c_buffer_refcount_increment(%Pos)
declare void @c_buffer_refcount_decrement(%Pos)
declare %Pos @c_buffer_concatenate(%Pos, %Pos)

declare void @hole()

declare %Pos @c_buffer_eq(%Pos, %Pos)
declare %Pos @c_buffer_substring(%Pos, i64, i64)
declare i64 @c_buffer_length(%Pos)
declare i64 @c_buffer_toInt(%Pos)
declare %Pos @c_buffer_show_Int(%Pos)
declare %Pos @c_buffer_show_Char(%Pos)
declare %Pos @c_buffer_show_Double(%Pos)
declare %Int @c_buffer_index(%Pos, i64)
declare %Int @c_buffer_character_at(%Pos, i64)
