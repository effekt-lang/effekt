; forward-declared from primitives.c

declare i64 @c_get_argc() mustprogress nofree norecurse nosync nounwind sspstrong willreturn memory(read, argmem: none, inaccessiblemem: none) uwtable
declare %Pos @c_get_arg(i64) nofree nounwind sspstrong uwtable

declare void @c_io_print(%Pos)
declare void @c_io_println(%Pos)
declare %Pos @c_io_readln()
declare %Double @c_io_random()

declare void @hole(i8*) cold
declare void @duplicated_prompt() cold

declare %Pos @c_ref_fresh(%Pos) mustprogress nofree nounwind sspstrong willreturn memory(write, argmem: none, inaccessiblemem: readwrite) uwtable
declare %Pos @c_ref_get(%Pos) nounwind sspstrong uwtable
declare %Pos @c_ref_set(%Pos, %Pos) nounwind sspstrong uwtable

declare %Pos @c_array_new(%Int) mustprogress nofree nounwind sspstrong willreturn memory(write, argmem: none, inaccessiblemem: readwrite) uwtable
declare %Int @c_array_size(%Pos) nounwind sspstrong uwtable
declare %Pos @c_array_get(%Pos, %Int) nounwind sspstrong uwtable
declare %Pos @c_array_set(%Pos, %Int, %Pos) nounwind sspstrong uwtable

declare %Pos @c_bytearray_new(%Int) mustprogress nofree nounwind sspstrong willreturn memory(write, argmem: none, inaccessiblemem: readwrite) uwtable
declare %Int @c_bytearray_size(%Pos) nounwind sspstrong uwtable
declare %Byte @c_bytearray_get(%Pos, %Int) nounwind sspstrong uwtable
declare %Pos @c_bytearray_set(%Pos, %Int, %Byte) nounwind sspstrong uwtable

declare ptr @c_bytearray_data(%Pos) mustprogress nofree norecurse nosync nounwind sspstrong willreturn memory(none) uwtable
declare %Pos @c_bytearray_construct(i64, ptr) mustprogress nofree nounwind sspstrong willreturn uwtable

declare %Pos @c_bytearray_from_nullterminated_string(ptr) nofree nounwind sspstrong uwtable
declare ptr @c_bytearray_into_nullterminated_string(%Pos) mustprogress nofree nounwind sspstrong willreturn uwtable

declare %Pos @c_bytearray_show_Int(i64) nofree nounwind sspstrong uwtable
declare %Pos @c_bytearray_show_Char(i32) nofree nounwind sspstrong memory(readwrite, argmem: none) uwtable
declare %Pos @c_bytearray_show_Byte(i8) nofree nounwind sspstrong uwtable
declare %Pos @c_bytearray_show_Double(double) nofree nounwind sspstrong uwtable

declare %Pos @c_bytearray_concatenate(%Pos, %Pos) nounwind sspstrong uwtable
declare %Pos @c_bytearray_equal(%Pos, %Pos) nounwind sspstrong uwtable
declare %Int @c_bytearray_compare(%Pos, %Pos) nounwind sspstrong uwtable

declare %Pos @c_bytearray_substring(%Pos, i64, i64) nounwind sspstrong uwtable
declare %Int @c_bytearray_character_at(%Pos, i64) nounwind sspstrong uwtable