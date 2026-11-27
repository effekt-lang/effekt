; forward-declared from primitives.c

declare i64 @c_get_argc()
declare %Pos @c_get_arg(i64 zeroext)

declare void @c_io_print(%Pos)
declare void @c_io_println(%Pos)
declare %Pos @c_io_readln() #25
declare %Double @c_io_random() #25

declare void @hole(i8*) cold noreturn
declare void @duplicated_prompt() cold noreturn
declare void @unmatched_tag() cold noreturn

declare %Pos @c_bytearray_new(%Int signext)
declare %Int @c_bytearray_size(%Pos)
declare %Byte @c_bytearray_get(%Pos, %Int signext)
declare %Pos @c_bytearray_set(%Pos, %Int signext, %Byte zeroext)

declare %Pos @c_bytearray_copy(%Pos, %Int signext, %Pos, %Int signext, %Int signext)
declare ptr @c_bytearray_data(%Pos)
declare %Pos @c_bytearray_construct(i64 zeroext, ptr)
declare void @c_bytearray_erase_noop(ptr)
declare %Pos @c_bytearray_copy_shared(%Pos)

declare %Pos @c_bytearray_from_nullterminated_string(ptr)
declare ptr @c_bytearray_into_nullterminated_string(%Pos)

declare %Pos @c_bytearray_show_Int(i64 signext) #25
declare %Pos @c_bytearray_show_Char(i64 zeroext) #25
declare %Pos @c_bytearray_show_Byte(i8 zeroext) #25
declare %Pos @c_bytearray_show_Double(double) #25

declare %Pos @c_bytearray_concatenate(%Pos, %Pos)
declare %Pos @c_bytearray_equal(%Pos, %Pos)
declare %Int @c_bytearray_compare(%Pos, %Pos)

declare %Pos @c_bytearray_substring(%Pos, i64 zeroext, i64 zeroext)
declare i32 @c_bytearray_character_at(%Pos, i64 zeroext)

attributes #25 = { nounwind memory(inaccessiblemem: readwrite) }
