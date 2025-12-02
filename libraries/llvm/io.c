#ifndef EFFEKT_IO_C
#define EFFEKT_IO_C

#include <uv.h>
#include <string.h> // to compare flag names

// Println and Readln and Random
// -----------------------------

void c_io_println(String text) {
    for (uint64_t j = 0; j < text.tag; ++j) {
        putchar(c_bytearray_data(text)[j]);
    };
    erasePositive(text);
    putchar('\n');
}

String c_io_readln() {
    uint64_t capacity = 64;
    uint64_t length = 0;
    char* buffer = malloc(capacity * sizeof(char));

    for (int c = getchar(); c != '\n' && c != EOF; c = getchar()) {
        if (length >= capacity) {
            capacity *= 2;
            buffer = realloc(buffer, capacity * sizeof(char));
        }
        buffer[length] = (char)c;
        length++;
    }

    String result = c_bytearray_construct(length, (const uint8_t*)buffer);
    free(buffer);
    return result;
}

double c_io_random(void) {
    return (double)rand() / (double)RAND_MAX;
}

// Lib UV Bindings
// ---------------
// Ideas behind the LLVM / libuv implementation.
//
// Share memory between Effekt-buffers and libuv buffers
// -----------------------------------------------------
// A libuv buffer is a {ptr, i64} where i64 is the capacity
// while our buffer (from bytearray.c, which is ref-counted) is a %Pos with a
// pointer and a length. The data ptr is shared between the two such that
// libuv (and underlying mechanisms) can directly write into our buffer without
// copying. Since our buffer is ref-counted, this has the advantage that we
// do not need to manually memory manage the buffer.
//
//
// Stacks in Data-fields
// ------------------------
// In order to return back to Effekt in the callback, we store a pointer
// to the Effekt stack into the user-definable data-field (at address 0)
// in each request object.
//
//
// TODO
// - pooling of request objects (benchmark first!)

typedef struct {
    Stack stack;
    struct Pos buffer;
    size_t offset;
} io_closure_t;

void c_resume_int_fs(uv_fs_t* request) {
    int64_t result = (int64_t)request->result;
    Stack stack = (Stack)request->data;

    uv_fs_req_cleanup(request);
    free(request);

    resume_Int(stack, result);
}

void c_fs_open(String path, int flags, Stack stack) {

    // Convert the Effekt String to a 0-terminated string
    char* path_str = c_bytearray_into_nullterminated_string(path);
    erasePositive((struct Pos) path);

    uv_fs_t* request = malloc(sizeof(uv_fs_t));
    request->data = stack;

    int result = uv_fs_open(uv_default_loop(), request, path_str, flags, 0777, c_resume_int_fs);

    if (result < 0) {
        // TODO free path_str?
        uv_fs_req_cleanup(request);
        free(request);
        resume_Int(stack, result);
    }

    // Free the string since libuv copies it into the request
    free(path_str);

    return;
}

void c_fs_open_reading(String path, Stack stack) {
    c_fs_open(path, UV_FS_O_RDONLY, stack);
}

void c_fs_open_writing(String path, Stack stack) {
    c_fs_open(path, UV_FS_O_WRONLY | UV_FS_O_CREAT | UV_FS_O_TRUNC, stack);
}

void c_fs_open_appending(String path, Stack stack) {
    c_fs_open(path, UV_FS_O_WRONLY | UV_FS_O_CREAT | UV_FS_O_APPEND, stack);
}

void c_fs_cb(uv_fs_t* request) {
    io_closure_t* closure = (io_closure_t*)request->data;
    Stack stack = closure->stack;
    int64_t result = (int64_t)request->result;

    uv_fs_req_cleanup(request);
    free(request);
    erasePositive(closure->buffer);
    free(closure);
    resume_Int(stack, result);
}

void c_fs_read(Int file, struct Pos buffer, Int offset, Int size, Int position, Stack stack) {

    uv_buf_t buf = uv_buf_init((char*)(c_bytearray_data(buffer) + offset), size);

    io_closure_t* read_closure = malloc(sizeof(io_closure_t));
    read_closure->stack = stack;
    read_closure->buffer = buffer;
    read_closure->offset = offset;

    uv_fs_t* request = malloc(sizeof(uv_fs_t));
    request->data = read_closure;

    int result = uv_fs_read(uv_default_loop(), request, file, &buf, 1, position, c_fs_cb);

    if (result < 0) {
        uv_fs_req_cleanup(request);
        free(request);
        resume_Int(stack, result);
	free(read_closure);
    }
}

void c_fs_write(Int file, struct Pos buffer, Int offset, Int size, Int position, Stack stack) {
    (void)size;

    uv_buf_t buf = uv_buf_init((char*)(c_bytearray_data(buffer) + offset), size);

    io_closure_t* write_closure = malloc(sizeof(io_closure_t));
    write_closure->stack = stack;
    write_closure->buffer = buffer;
    write_closure->offset = offset;

    uv_fs_t* request = malloc(sizeof(uv_fs_t));
    request->data = write_closure;

    int result = uv_fs_write(uv_default_loop(), request, file, &buf, 1, position, c_fs_cb);

    if (result < 0) {
        uv_fs_req_cleanup(request);
        free(request);
        resume_Int(stack, result);
	free(write_closure);
    }
}

void c_fs_close(Int file, Stack stack) {

    uv_fs_t* request = malloc(sizeof(uv_fs_t));
    request->data = stack;

    int result = uv_fs_close(uv_default_loop(), request, file, c_resume_int_fs);

    if (result < 0) {
        uv_fs_req_cleanup(request);
        free(request);
        resume_Int(stack, result);
    }
}

void c_fs_mkdir(String path, Stack stack) {

    // Convert the Effekt String to a null-terminated string
    char* path_str = c_bytearray_into_nullterminated_string(path);
    erasePositive((struct Pos) path);

    uv_fs_t* request = malloc(sizeof(uv_fs_t));
    request->data = stack;

    // Perform the mkdir operation
    int result = uv_fs_mkdir(uv_default_loop(), request, path_str, 0777, c_resume_int_fs);

    if (result < 0) {
        uv_fs_req_cleanup(request);
        free(request);
        resume_Int(stack, result);
    }

    // Free the string since libuv copies it into the request
    free(path_str);

    return;
}

// Network
// -------

void c_tcp_connect_cb(uv_connect_t* request, int status) {
    Stack stack = (Stack)request->data;

    if (status < 0) {
        uv_close((uv_handle_t*)request->handle, (uv_close_cb)free);
        free(request);
        resume_Int(stack, status);
    } else {
        int64_t handle = (int64_t)request->handle;
        free(request);
        resume_Int(stack, handle);
    }
}

void c_tcp_connect(String host, Int port, Stack stack) {
    char* host_str = c_bytearray_into_nullterminated_string(host);
    erasePositive(host);

    uv_tcp_t* tcp_handle = malloc(sizeof(uv_tcp_t));
    int result = uv_tcp_init(uv_default_loop(), tcp_handle);

    if (result < 0) {
        free(tcp_handle);
        free(host_str);
        resume_Int(stack, result);
        return;
    }

    uv_connect_t* connect_req = malloc(sizeof(uv_connect_t));
    connect_req->data = stack;

    struct sockaddr_in addr;
    result = uv_ip4_addr(host_str, port, &addr);
    free(host_str);

    if (result < 0) {
        free(tcp_handle);
        free(connect_req);
        resume_Int(stack, result);
        return;
    }

    result = uv_tcp_connect(connect_req, tcp_handle, (const struct sockaddr*)&addr, c_tcp_connect_cb);

    if (result < 0) {
        free(tcp_handle);
        free(connect_req);
        resume_Int(stack, result);
        return;
    }
}

void c_tcp_read_cb(uv_stream_t* stream, ssize_t bytes_read, const uv_buf_t* buf) {
    (void)(buf);
    io_closure_t* read_closure = (io_closure_t*)stream->data;
    Stack stack = read_closure->stack;

    uv_read_stop(stream);
    erasePositive(read_closure->buffer);
    free(read_closure);

    resume_Int(stack, (int64_t)bytes_read);
}

void c_tcp_read_alloc_cb(uv_handle_t* handle, size_t suggested_size, uv_buf_t* buf) {
    (void)(suggested_size);
    io_closure_t* read_closure = (io_closure_t*)handle->data;
    buf->base = (char*)c_bytearray_data(read_closure->buffer) + read_closure->offset;
    // TODO c_bytearray_size also erases
    buf->len = read_closure->buffer.tag;
}

void c_tcp_read(Int handle, struct Pos buffer, Int offset, Int size, Stack stack) {
    (void)size;
    uv_stream_t* stream = (uv_stream_t*)handle;

    io_closure_t* read_closure = malloc(sizeof(io_closure_t));
    read_closure->stack = stack;
    read_closure->buffer = buffer;
    read_closure->offset = offset;
    stream->data = read_closure;

    int result = uv_read_start(stream, c_tcp_read_alloc_cb, c_tcp_read_cb);

    if (result < 0) {
        free(read_closure);
        stream->data = NULL;
        resume_Int(stack, result);
    }
}

void c_tcp_write_cb(uv_write_t* request, int status) {
    io_closure_t* write_closure = (io_closure_t*)request->data;
    free(request);
    erasePositive(write_closure->buffer);
    Stack stack = write_closure->stack;
    free(write_closure);
    resume_Int(stack, (int64_t)status);
}

void c_tcp_write(Int handle, struct Pos buffer, Int offset, Int size, Stack stack) {
    uv_stream_t* stream = (uv_stream_t*)handle;

    uv_buf_t buf = uv_buf_init((char*)(c_bytearray_data(buffer) + offset), size);

    io_closure_t* write_closure = malloc(sizeof(io_closure_t));
    write_closure->stack = stack;
    write_closure->buffer = buffer;
    write_closure->offset = offset;

    uv_write_t* request = malloc(sizeof(uv_write_t));
    request->data = write_closure;

    int result = uv_write(request, stream, &buf, 1, c_tcp_write_cb);

    if (result < 0) {
        free(request);
        resume_Int(stack, result);
	free(write_closure);
    }
}

void c_tcp_close_cb(uv_handle_t* handle) {
    Stack stack = (Stack)handle->data;
    free(handle);
    resume_Pos(stack, Unit);
}

void c_tcp_close(Int handle, Stack stack) {
    uv_handle_t* uv_handle = (uv_handle_t*)handle;
    uv_handle->data = stack;
    uv_close(uv_handle, c_tcp_close_cb);
}

Int c_tcp_bind(String host, Int port) {
    char* host_str = c_bytearray_into_nullterminated_string(host);
    erasePositive(host);

    uv_tcp_t* tcp_handle = malloc(sizeof(uv_tcp_t));
    int result = uv_tcp_init(uv_default_loop(), tcp_handle);

    if (result < 0) {
        free(tcp_handle);
        free(host_str);
        return result;
    }

    struct sockaddr_in addr;
    result = uv_ip4_addr(host_str, port, &addr);
    free(host_str);

    if (result < 0) {
        uv_close((uv_handle_t*)tcp_handle, (uv_close_cb)free);
        return result;
    }

    result = uv_tcp_bind(tcp_handle, (const struct sockaddr*)&addr, 0);
    if (result < 0) {
        uv_close((uv_handle_t*)tcp_handle, (uv_close_cb)free);
        return result;
    }

    return (int64_t)tcp_handle;
}

typedef struct {
    Stack stack;
    struct Pos handler;
} tcp_listen_closure_t;

void c_tcp_listen_cb(uv_stream_t* server, int status) {
    tcp_listen_closure_t* listen_closure = (tcp_listen_closure_t*)server->data;
    Stack closure_stack = listen_closure->stack;
    struct Pos closure_handler = listen_closure->handler;

    if (status < 0) {
        server->data = NULL;
        free(listen_closure);
        erasePositive(closure_handler);
        resume_Int(closure_stack, status);
        return;
    }

    uv_tcp_t* client = malloc(sizeof(uv_tcp_t));
    int result = uv_tcp_init(uv_default_loop(), client);

    if (result < 0) {
        free(client);
        server->data = NULL;
        free(listen_closure);
        erasePositive(closure_handler);
        resume_Int(closure_stack, result);
        return;
    }

    result = uv_accept(server, (uv_stream_t*)client);
    if (result < 0) {
        uv_close((uv_handle_t*)client, (uv_close_cb)free);
        server->data = NULL;
        free(listen_closure);
        erasePositive(closure_handler);
        resume_Int(closure_stack, result);
        return;
    }

    sharePositive(closure_handler);
    run_Int(closure_handler, (int64_t)client);
}

void c_tcp_listen(Int listener, struct Pos handler, Stack stack) {
    uv_stream_t* server = (uv_stream_t*)listener;

    tcp_listen_closure_t* listen_closure = malloc(sizeof(tcp_listen_closure_t));
    listen_closure->stack = stack;
    listen_closure->handler = handler;
    server->data = listen_closure;

    int result = uv_listen(server, SOMAXCONN, c_tcp_listen_cb);
    if (result < 0) {
        free(listen_closure);
        erasePositive(handler);
        resume_Int(stack, result);
        return;
    }
}

void c_tcp_shutdown(Int handle, Stack stack) {
    uv_handle_t* uv_handle = (uv_handle_t*)handle;
    tcp_listen_closure_t* listen_closure = (tcp_listen_closure_t*)uv_handle->data;

    uv_handle->data = stack;
    uv_close(uv_handle, c_tcp_close_cb);

    if (listen_closure) {
        Stack closure_stack = listen_closure->stack;
        struct Pos closure_handler = listen_closure->handler;
        free(listen_closure);
        erasePositive(closure_handler);
        resume_Int(closure_stack, 0);
    }
}


/**
 * Maps the libuv error code to a stable (platform independent) numeric value.
 *
 * Tries to use most common error number integer values, but introduces fresh values (> 200)
 * for those without common error number values.
 */
Int c_error_number(Int error) {
    switch (error) {
        case UV_EPERM:            return 1;    // EPERM
        case UV_ENOENT:           return 2;    // ENOENT
        case UV_ESRCH:            return 3;    // ESRCH
        case UV_EINTR:            return 4;    // EINTR
        case UV_EIO:              return 5;    // EIO
        case UV_ENXIO:            return 6;    // ENXIO
        case UV_E2BIG:            return 7;    // E2BIG
        case UV_EBADF:            return 9;    // EBADF
        case UV_EAGAIN:           return 11;   // EAGAIN
        case UV_ENOMEM:           return 12;   // ENOMEM
        case UV_EACCES:           return 13;   // EACCES
        case UV_EFAULT:           return 14;   // EFAULT
        case UV_EBUSY:            return 16;   // EBUSY
        case UV_EEXIST:           return 17;   // EEXIST
        case UV_EXDEV:            return 18;   // EXDEV
        case UV_ENODEV:           return 19;   // ENODEV
        case UV_ENOTDIR:          return 20;   // ENOTDIR
        case UV_EISDIR:           return 21;   // EISDIR
        case UV_EINVAL:           return 22;   // EINVAL
        case UV_ENFILE:           return 23;   // ENFILE
        case UV_EMFILE:           return 24;   // EMFILE
        case UV_ENOTTY:           return 25;   // ENOTTY
        case UV_ETXTBSY:          return 26;   // ETXTBSY
        case UV_EFBIG:            return 27;   // EFBIG
        case UV_ENOSPC:           return 28;   // ENOSPC
        case UV_ESPIPE:           return 29;   // ESPIPE
        case UV_EROFS:            return 30;   // EROFS
        case UV_EMLINK:           return 31;   // EMLINK
        case UV_EPIPE:            return 32;   // EPIPE
        case UV_ERANGE:           return 34;   // ERANGE
        case UV_ENAMETOOLONG:     return 36;   // ENAMETOOLONG
        case UV_ELOOP:            return 40;   // ELOOP
        case UV_EOVERFLOW:        return 75;   // EOVERFLOW
        case UV_EFTYPE:           return 79;   // EFTYPE
        case UV_EILSEQ:           return 84;   // EILSEQ
        case UV_ENOTSOCK:         return 88;   // ENOTSOCK
        case UV_EDESTADDRREQ:     return 89;   // EDESTADDRREQ
        case UV_EMSGSIZE:         return 90;   // EMSGSIZE
        case UV_EPROTOTYPE:       return 91;   // EPROTOTYPE
        case UV_ENOPROTOOPT:      return 92;   // ENOPROTOOPT
        case UV_EPROTONOSUPPORT:  return 93;   // EPROTONOSUPPORT
        case UV_ESOCKTNOSUPPORT:  return 94;   // ESOCKTNOSUPPORT
        case UV_ENOTSUP:          return 95;   // EOPNOTSUPP
        case UV_EAFNOSUPPORT:     return 97;   // EAFNOSUPPORT
        case UV_EADDRINUSE:       return 98;   // EADDRINUSE
        case UV_EADDRNOTAVAIL:    return 99;   // EADDRNOTAVAIL
        case UV_ENETDOWN:         return 100;  // ENETDOWN
        case UV_ENETUNREACH:      return 101;  // ENETUNREACH
        case UV_ECONNABORTED:     return 103;  // ECONNABORTED
        case UV_ECONNRESET:       return 104;  // ECONNRESET
        case UV_ENOBUFS:          return 105;  // ENOBUFS
        case UV_EISCONN:          return 106;  // EISCONN
        case UV_ENOTCONN:         return 107;  // ENOTCONN
        case UV_ETIMEDOUT:        return 110;  // ETIMEDOUT
        case UV_ECONNREFUSED:     return 111;  // ECONNREFUSED
        case UV_EHOSTUNREACH:     return 113;  // EHOSTUNREACH
        case UV_EALREADY:         return 114;  // EALREADY
        case UV_ECANCELED:        return 125;  // ECANCELED

        case UV_EAI_ADDRFAMILY:   return 200;  // Fresh unique value
        case UV_EAI_AGAIN:        return 201;  // Fresh unique value
        case UV_EAI_BADFLAGS:     return 202;  // Fresh unique value
        case UV_EAI_BADHINTS:     return 203;  // Fresh unique value
        case UV_EAI_CANCELED:     return 204;  // Fresh unique value
        case UV_EAI_FAIL:         return 205;  // Fresh unique value
        case UV_EAI_FAMILY:       return 206;  // Fresh unique value
        case UV_EAI_MEMORY:       return 207;  // Fresh unique value
        case UV_EAI_NODATA:       return 208;  // Fresh unique value
        case UV_EAI_NONAME:       return 209;  // Fresh unique value
        case UV_EAI_OVERFLOW:     return 210;  // Fresh unique value
        case UV_EAI_PROTOCOL:     return 211;  // Fresh unique value
        case UV_EAI_SERVICE:      return 212;  // Fresh unique value
        case UV_EAI_SOCKTYPE:     return 213;  // Fresh unique value
        case UV_ECHARSET:         return 215;  // Fresh unique value
        case UV_ENONET:           return 216;  // Fresh unique value
        case UV_UNKNOWN:          return 217;  // Fresh unique value
        case UV_EOF:              return 218;  // Fresh unique value

        case UV_ESHUTDOWN:        return 220;  // Fresh unique value

        // Not available in libuv1-dev on ubuntu
        //case UV_EUNATCH:          return 219;  // Fresh unique value

        default:                  return -1;   // Unknown error
    }
}

void c_resume_unit_timer(uv_timer_t* handle) {
    Stack stack = handle->data;

    uv_timer_stop(handle);
    uv_close((uv_handle_t*)handle, (uv_close_cb)free);

    resume_Pos(stack, Unit);
}

void c_timer_start(Int millis, Stack stack) {
    uv_timer_t* timer = malloc(sizeof(uv_timer_t));
    timer->data = stack;

    uv_timer_init(uv_default_loop(), timer);

    uv_timer_start(timer, c_resume_unit_timer, millis, 0);
}

void c_yield(Stack stack) {
    c_timer_start(0, stack);
}

// Signals
// --------

typedef enum { INITIAL, NOTIFIED } signal_state_t;

typedef struct {
    uint64_t rc;
    void* eraser;
    signal_state_t state;
    struct Pos value;
    Stack stack;
} Signal;

void c_signal_erase(void *envPtr) {
    // envPtr points to a Signal _after_ the eraser, so let's adjust it to point to the beginning.
    Signal *signal = (Signal*) (envPtr - offsetof(Signal, state));
    erasePositive(signal->value);
    if (signal->stack != NULL) { eraseStack(signal->stack); }
}

struct Pos c_signal_make() {
    Signal* f = (Signal*)malloc(sizeof(Signal));

    f->rc = 0;
    f->eraser = c_signal_erase;
    f->state = INITIAL;
    f->value = (struct Pos) { .tag = 0, .obj = NULL, };
    f->stack = NULL;

    return (struct Pos) { .tag = 0, .obj = f, };
}

void c_signal_notify(struct Pos signal, struct Pos value, Stack stack) {
    Signal* f = (Signal*)signal.obj;
    switch (f->state) {
        case INITIAL: {
            f->state = NOTIFIED;
            f->value = value;
            f->stack = stack;
            erasePositive(signal);
            break;
        }
        case NOTIFIED: {
            struct Pos other_value = f->value;
            f->value = (struct Pos) { .tag = 0, .obj = NULL, };
            Stack other_stack = f->stack;
            f->stack = NULL;
            erasePositive(signal);
            resume_Pos(other_stack, value);
            resume_Pos(stack, other_value);
            break;
        }
    }
}

// Subprocesses
// ------------

struct Pos c_spawn_options_default() {
    uv_process_options_t* options = (uv_process_options_t*)malloc(sizeof(uv_process_options_t));
    memset(options, 0, sizeof(uv_process_options_t));
    uv_stdio_container_t* stdio = (uv_stdio_container_t*)malloc(3*sizeof(uv_stdio_container_t));
    stdio[0].flags = UV_INHERIT_FD;
    stdio[0].data.fd = 0;
    stdio[1].flags = UV_INHERIT_FD;
    stdio[1].data.fd = 1;
    stdio[2].flags = UV_INHERIT_FD;
    stdio[2].data.fd = 2;
    options->stdio = stdio;
    options->stdio_count = 3;
    return (struct Pos) { .tag = 0, .obj = options, };
}

static void subproc_alloc_cb(uv_handle_t* handle, size_t suggested_size, uv_buf_t* buf) {
    (void)handle;
    buf->base = malloc(suggested_size);
    buf->len = suggested_size;
}

typedef struct {
  struct Pos handler;
} subproc_stream_cb_closure_t;
void subproc_stream_cb(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buf) {
  subproc_stream_cb_closure_t* clos = (subproc_stream_cb_closure_t*)(stream->data);
  if(nread >= 0) {
    struct Pos chunk = c_bytearray_construct(nread, (uint8_t*)(buf->base));
    if (buf->base) free(buf->base);
    run_Pos(clos->handler, chunk);
  } else {
    uv_read_stop(stream);
    erasePositive(clos->handler);
    free(clos);
    if (buf->base) free(buf->base);
  }
}
struct Pos subproc_options_on_stream(struct Pos opts, size_t id, uv_stdio_flags flags, struct Pos callback) {
    uv_process_options_t* options = (uv_process_options_t*)opts.obj;

    options->stdio[id].flags = UV_CREATE_PIPE | flags;
    uv_pipe_t* pipe = (uv_pipe_t*)malloc(sizeof(uv_pipe_t));

    uv_pipe_init(uv_default_loop(), pipe, 1);
    options->stdio[id].data.stream = (uv_stream_t*)pipe;

    subproc_stream_cb_closure_t* clos = (subproc_stream_cb_closure_t*)malloc(sizeof(subproc_stream_cb_closure_t));
    clos->handler = callback;
    pipe->data = clos;

    return opts;
}
struct Pos c_spawn_options_on_stdout(struct Pos opts, struct Pos callback) {
  return subproc_options_on_stream(opts, 1, UV_WRITABLE_PIPE, callback);
}
struct Pos c_spawn_options_on_stderr(struct Pos opts, struct Pos callback) {
  return subproc_options_on_stream(opts, 2, UV_WRITABLE_PIPE, callback);
}
struct Pos c_spawn_options_pipe_stdin(struct Pos opts, struct Pos callback) {
  return subproc_options_on_stream(opts, 0, UV_READABLE_PIPE, callback);
}


typedef struct {
  char** args;
  Stack k;
  uv_process_options_t* opts;
} subproc_proc_data_t;
void c_close_stream_callback_exit(uv_handle_t* req) {
    free(req);
}
void subproc_on_close(uv_handle_t* handle) {
    subproc_proc_data_t* pd = (subproc_proc_data_t*)((uv_process_t*)handle)->data;
    uv_process_options_t* opts = (uv_process_options_t*)(pd->opts);
    if(opts->stdio[0].flags & UV_CREATE_PIPE){
        uv_close((uv_handle_t*)(opts->stdio[0].data.stream), c_close_stream_callback_exit);
    }
    if(opts->stdio[1].flags & UV_CREATE_PIPE){
        uv_close((uv_handle_t*)(opts->stdio[1].data.stream), c_close_stream_callback_exit);
    }
    if(opts->stdio[2].flags & UV_CREATE_PIPE){
        uv_close((uv_handle_t*)(opts->stdio[2].data.stream), c_close_stream_callback_exit);
    }
    // TODO shutdown pipes for stdio
    free(opts->stdio);
    free(opts);
    free(pd);
    free(handle);
}
void subproc_on_exit(uv_process_t* proc, int64_t exit_status, int term_signal) {
    (void)term_signal; (void)exit_status;
    subproc_proc_data_t* pd = (subproc_proc_data_t*)proc->data;
    if (pd->args) {
        for(int i = 0; pd->args[i] != NULL; i++) {
            free(pd->args[i]);
        }
        free(pd->args);
    }
    Stack k = pd->k;
    uv_close((uv_handle_t*)proc, subproc_on_close);
    resume_Int(k, exit_status);
}

void c_close_stream_callback(uv_shutdown_t* req, int status) {
    if(status != 0){
        // TODO error handling
    }
    free(req);
}
void c_close_stream(struct Pos stream) {
    uv_stream_t* str = (uv_stream_t*)stream.obj;
    uv_shutdown_t* req = (uv_shutdown_t*)malloc(sizeof(uv_shutdown_t));
    uv_shutdown(req, str, c_close_stream_callback);
}

typedef struct {
    struct Pos buffer;
    uv_buf_t buf;
} subproc_write_req_data_t;
void c_write_stream_callback(uv_write_t* req, int status) {
    if(status != 0){
        // TODO error handling
    }
    subproc_write_req_data_t* data = (subproc_write_req_data_t*)(req->data);
    erasePositive(data->buffer);
    free(data);
    free(req);
}
void c_write_stream(struct Pos stream, struct Pos buffer) {
    uv_stream_t* str = (uv_stream_t*)stream.obj;
    uv_write_t* req = (uv_write_t*)malloc(sizeof(uv_write_t));
    memset(req, 0, sizeof(uv_write_t));
    subproc_write_req_data_t* data = (subproc_write_req_data_t*)malloc(sizeof(subproc_write_req_data_t));
    data->buffer = buffer;
    uv_buf_t buf = uv_buf_init((char*)(c_bytearray_data(buffer)), buffer.tag);
    req->data = data;
    data->buf = buf;
    uv_write(req, str, &(data->buf), 1, c_write_stream_callback);
}

void c_spawn(struct Pos cmd, struct Pos args, struct Pos options, Stack stack) {
    uv_process_options_t* opts = (uv_process_options_t*)options.obj;
    uv_process_t* proc = (uv_process_t*)malloc(sizeof(uv_process_t));

    // command
    char* cmd_s = c_bytearray_into_nullterminated_string(cmd);
    erasePositive(cmd);
    opts->file = cmd_s;

    // args
    int _argc = (int)((uint64_t*)args.obj)[2];
    char** _args = (char**)malloc((2 + _argc) * sizeof(char*));
    struct Pos* _argv = (struct Pos*)(((uint64_t*)args.obj) + 3);
    _args[0] = cmd_s;
    for(int i = 0; i < _argc; i++) {
        _args[i + 1] = c_bytearray_into_nullterminated_string(_argv[i]);
    }
    erasePositive(args);
    _args[_argc + 1] = NULL;
    opts->args = _args;
    // callback to free _args
    opts->exit_cb = subproc_on_exit;
    subproc_proc_data_t* pd = (subproc_proc_data_t*)malloc(sizeof(subproc_proc_data_t));
    pd->args = _args;
    pd->k = stack;
    proc->data = pd;

    // spawn process
    int err = uv_spawn(uv_default_loop(), proc, opts);

    if(opts->stdio[0].flags & UV_CREATE_PIPE) {
        uv_stream_t* stream = (uv_stream_t*)(opts->stdio[0].data.stream);
        subproc_stream_cb_closure_t* clos = (subproc_stream_cb_closure_t*)(stream->data);
        struct Pos str = (struct Pos) { .tag = 0, .obj = stream, };
        run_Pos(clos->handler, str);
        erasePositive(clos->handler);
        free(clos);
    }
    if(opts->stdio[1].flags & UV_CREATE_PIPE)
      uv_read_start((uv_stream_t*)(opts->stdio[1].data.stream), subproc_alloc_cb, subproc_stream_cb);
    if(opts->stdio[2].flags & UV_CREATE_PIPE)
      uv_read_start((uv_stream_t*)(opts->stdio[2].data.stream), subproc_alloc_cb, subproc_stream_cb);
    pd->opts = opts;

    erasePositive(options);
    if(err) {
        printf("%s\n", uv_strerror(err));
        free(_args);
        free(proc);
    }
}

#endif
