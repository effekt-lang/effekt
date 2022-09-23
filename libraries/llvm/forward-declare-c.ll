; forward-declared from primitives.c

declare void @c_io_println_Boolean(%Pos)
declare void @c_io_println_Int(%Int)
declare void @c_io_println_Double(%Double)
declare void @c_io_println_String(%Pos)

declare %Pos @c_buffer_construct(i64, i8*)
declare void @c_buffer_refcount_increment(%Pos)
declare void @c_buffer_refcount_decrement(%Pos)

declare %Int @c_io_number_of_command_line_arguments()
declare %Pos @c_io_command_line_argument(%Int)
declare %Pos @c_io_getenv(%Pos)

declare %Pos @c_io_read(%Int)
declare void @c_io_write(%Int, %Pos)
