; forward-declared from primitives.c

declare void @c_println_TODO()

declare %Pos @c_println_Boolean(%Pos)
declare %Pos @c_println_Int(%Int)
declare %Pos @c_println_Double(%Double)
declare void @c_println_String(%Pos)

declare %Pos @c_buffer_construct(i64, i8*)
