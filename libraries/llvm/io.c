#ifndef EFFEKT_ARGS_C
#define EFFEKT_ARGS_C


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

void c_io_println_String(String buffer) {
    for (uint64_t j = 0; j < c_buffer_length(buffer); ++j)
        putchar(c_buffer_bytes(buffer)[j]);
    putchar('\n');
}


// command-line arguments

static int c_io_argc;
static char **c_io_argv;
void c_io_prepare_command_line_arguments(int argc, char *argv[]) {
    c_io_argc = argc;
    c_io_argv = argv;
}

Int c_io_number_of_command_line_arguments() {
    return (Int) c_io_argc;
}

String c_io_command_line_argument(const Int k) {
    if (k < 0 || k >= c_io_argc)
        return c_buffer_construct(0, NULL);
    return c_buffer_construct_from_null_terminated_string(c_io_argv[k]);
}


// environment variables

String c_io_getenv(const struct Pos key) {
    char *key_nt = c_buffer_as_null_terminated_string(key);
    // NOTE: value_nt must not be freed (its memory is managed by the kernel)
    const char *value_nt = getenv(key_nt);
    ASSERT_NON_NULL(value_nt);
    free(key_nt);

    return c_buffer_construct_from_null_terminated_string(value_nt);
}


#endif
