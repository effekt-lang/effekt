#ifndef EFFEKT_IO_C
#define EFFEKT_IO_C


// println

void c_io_println_Int(const Int n) {
    printf("%" PRId64 "\n", n);
}

void c_io_println_Boolean(const struct Pos p) {
    printf("%s\n", p.tag ? "true" : "false");
}

void c_io_println_Double(const Double x) {
    printf("%g\n", x);
}

void c_io_println_String(String text) {
    for (uint64_t j = 0; j < c_buffer_length(text); ++j)
        putchar(c_buffer_bytes(text)[j]);
    putchar('\n');
}


#endif
