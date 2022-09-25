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


#define IO_CHUNK_SIZE (4096)

struct Pos c_io_read(const Int fd) {
    const struct Pos buffer = c_buffer_construct_zeroed(IO_CHUNK_SIZE);
    const int64_t n = read((int) fd, c_buffer_bytes(buffer), IO_CHUNK_SIZE);
    if (n == -1) {
        fprintf(stderr, "*** IO ERROR ENCOUNTERED: read: %s\n", strerror(errno));
        fflush(stderr);
        c_buffer_truncate(buffer, 0);
    }
    c_buffer_truncate(buffer, n);
    return buffer;
}

void c_io_write(const Int fd, const struct Pos buffer) {
    const int64_t n = write((int) fd, c_buffer_bytes(buffer), c_buffer_length(buffer));
    if (n == -1) {
        fprintf(stderr, "*** IO ERROR ENCOUNTERED: write: %s\n", strerror(errno));
        fflush(stderr);
    }
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
        return c_buffer_construct_zeroed(0);
    return c_buffer_construct_from_null_terminated_string(c_io_argv[k]);
}


// environment variables

String c_io_getenv(const String key) {
    char *key_nt = c_buffer_as_null_terminated_string(key);
    // NOTE: value_nt must not be freed (its memory is managed by the kernel)
    const char *value_nt = getenv(key_nt);
    ASSERT_NON_NULL(value_nt);
    free(key_nt);

    return c_buffer_construct_from_null_terminated_string(value_nt);
}


#endif
