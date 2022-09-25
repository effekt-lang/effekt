; forward-declared from primitives.c

declare void @c_io_println_Boolean(%Pos)
declare void @c_io_println_Int(%Int)
declare void @c_io_println_Double(%Double)
declare void @c_io_println_String(%Pos)

declare %Pos @c_buffer_construct(i64, i8*)
declare void @c_buffer_refcount_increment(%Pos)
declare void @c_buffer_refcount_decrement(%Pos)
declare %Pos @c_buffer_concatenate(%Pos, %Pos)

declare %Pos @c_buffer_base64decode(%Pos)
