#ifndef EFFEKT_IO_C
#define EFFEKT_IO_C

#include <uv.h>
#include <string.h> // to compare flag names

// Println and Readln and Random
// -----------------------------

void c_io_print(String text) {
    for (uint64_t j = 0; j < text.tag; ++j) {
        putchar(c_bytearray_data(text)[j]);
    };
    erasePositive(text);
}

void c_io_println(String text) {
    c_io_print(text);
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

// `c_fs_cb`'s data: the stack to resume in and the buffer to release.
typedef struct {
    Stack stack;
    struct Pos buffer;
} io_closure_t;

void c_resume_int_fs(uv_fs_t* request) {
    int64_t result = (int64_t)request->result;
    Stack stack = (Stack)request->data;

    uv_fs_req_cleanup(request);
    free(request);

    resume_Int(stack, result);
}

// Files, connections and listeners are generational handles into one table of open I/O resources.
// A handle is a slot of the table and the generation which the slot had when the resource was opened,
// so it is a plain integer, as cheap to copy as an `Int`.
// Closing a resource starts the next generation of its slot,
// so its handles no longer match, and using them raises `EBADF`, even once the slot is reused.
typedef struct {
    intptr_t value;        // the resource: a file descriptor, a `uv_tcp_t*`, or a `struct addrinfo*` of `c_tcp_resolve`
    uint32_t generation;   // 31 bits, so that a handle is never negative (negative results are errors)
    uint32_t next_free;    // the next free slot, while this one is free
} io_slot_t;

#define IO_NO_SLOT UINT32_MAX

static io_slot_t* io_slots = NULL;
static uint32_t io_slots_used = 0;
static uint32_t io_slots_capacity = 0;
static uint32_t io_slots_free = IO_NO_SLOT;

Int io_handle_new(intptr_t value) {
    uint32_t slot;
    if (io_slots_free != IO_NO_SLOT) {
        slot = io_slots_free;
        io_slots_free = io_slots[slot].next_free;
    } else {
        if (io_slots_used == io_slots_capacity) {
            io_slots_capacity = io_slots_capacity == 0 ? 16 : 2 * io_slots_capacity;
            io_slots = (io_slot_t*)realloc(io_slots, io_slots_capacity * sizeof(io_slot_t));
        }
        slot = io_slots_used++;
        io_slots[slot].generation = 0;
    }

    io_slots[slot].value = value;
    io_slots[slot].next_free = IO_NO_SLOT;
    return ((Int)io_slots[slot].generation << 32) | slot;
}

// The slot of a handle which is still open, or `NULL`.
io_slot_t* io_handle_slot(Int handle) {
    if (handle < 0) return NULL;

    uint32_t slot = (uint32_t)handle;
    uint32_t generation = (uint32_t)(handle >> 32);
    if (slot >= io_slots_used || io_slots[slot].next_free != IO_NO_SLOT || io_slots[slot].generation != generation) return NULL;
    return &io_slots[slot];
}

// Closes a handle which is still open, and returns its resource in `value`, or returns 0 if it was not open.
int io_handle_close(Int handle, intptr_t* value) {
    io_slot_t* open = io_handle_slot(handle);
    if (open == NULL) return 0;

    *value = open->value;
    open->generation = (open->generation + 1) & 0x7fffffff;
    open->next_free = io_slots_free;
    io_slots_free = (uint32_t)(open - io_slots);
    return 1;
}

// Resumes with the handle of the opened file, or with the (negative) error.
void c_fs_open_cb(uv_fs_t* request) {
    Stack stack = (Stack)request->data;
    int64_t result = (int64_t)request->result;
    uv_fs_req_cleanup(request);
    free(request);
    resume_Int(stack, result < 0 ? result : io_handle_new(result));
}

void c_fs_open(String path, int flags, Stack stack) {

    // Convert the Effekt String to a 0-terminated string
    char* path_str = c_bytearray_into_nullterminated_string(path);
    erasePositive((struct Pos) path);

    uv_fs_t* request = malloc(sizeof(uv_fs_t));
    request->data = stack;

    int result = uv_fs_open(uv_default_loop(), request, path_str, flags, 0777, c_fs_open_cb);

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
    io_slot_t* open = io_handle_slot(file);
    if (open == NULL) {
        erasePositive(buffer);
        resume_Int(stack, UV_EBADF);
        return;
    }


    uv_buf_t buf = uv_buf_init((char*)(c_bytearray_data(buffer) + offset), size);

    io_closure_t* read_closure = malloc(sizeof(io_closure_t));
    read_closure->stack = stack;
    read_closure->buffer = buffer;

    uv_fs_t* request = malloc(sizeof(uv_fs_t));
    request->data = read_closure;

    int result = uv_fs_read(uv_default_loop(), request, open->value, &buf, 1, position, c_fs_cb);

    if (result < 0) {
        uv_fs_req_cleanup(request);
        free(request);
        free(read_closure);
        resume_Int(stack, result);
    }
}

void c_fs_write(Int file, struct Pos buffer, Int offset, Int size, Int position, Stack stack) {
    io_slot_t* open = io_handle_slot(file);
    if (open == NULL) {
        erasePositive(buffer);
        resume_Int(stack, UV_EBADF);
        return;
    }

    (void)size;

    uv_buf_t buf = uv_buf_init((char*)(c_bytearray_data(buffer) + offset), size);

    io_closure_t* write_closure = malloc(sizeof(io_closure_t));
    write_closure->stack = stack;
    write_closure->buffer = buffer;

    uv_fs_t* request = malloc(sizeof(uv_fs_t));
    request->data = write_closure;

    int result = uv_fs_write(uv_default_loop(), request, open->value, &buf, 1, position, c_fs_cb);

    if (result < 0) {
        uv_fs_req_cleanup(request);
        free(request);
        free(write_closure);
        resume_Int(stack, result);
    }
}

void c_fs_close(Int file, Stack stack) {
    intptr_t fd;
    if (!io_handle_close(file, &fd)) {
        resume_Int(stack, UV_EBADF);
        return;
    }


    uv_fs_t* request = malloc(sizeof(uv_fs_t));
    request->data = stack;

    int result = uv_fs_close(uv_default_loop(), request, fd, c_resume_int_fs);

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
        uv_tcp_t* handle = (uv_tcp_t*)request->handle;
        free(request);
        resume_Int(stack, io_handle_new((intptr_t)handle));
    }
}

// Hosts are resolved in Effekt (see `io/network`), which then connects to or binds one address at a time,
// so the resolution only collects the addresses, as a handle to the list which `uv_getaddrinfo` returns.

void c_tcp_resolve_cb(uv_getaddrinfo_t* request, int status, struct addrinfo* addresses) {
    Stack stack = (Stack)request->data;
    free(request);
    resume_Int(stack, status < 0 ? status : io_handle_new((intptr_t)addresses));
}

void c_tcp_resolve(String host, Stack stack) {
    char* host_str = c_bytearray_into_nullterminated_string(host);
    erasePositive(host);

    struct addrinfo hints;
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_protocol = IPPROTO_TCP;

    uv_getaddrinfo_t* request = malloc(sizeof(uv_getaddrinfo_t));
    request->data = stack;
    int result = uv_getaddrinfo(uv_default_loop(), request, c_tcp_resolve_cb, host_str, NULL, &hints);

    // Free the string since libuv copies it into the request
    free(host_str);

    if (result < 0) {
        free(request);
        resume_Int(stack, result);
    }
}

// The addresses of a resolution which is still open, or `NULL` otherwise.
static struct addrinfo* c_tcp_resolution(Int resolution) {
    io_slot_t* open = io_handle_slot(resolution);
    return open == NULL ? NULL : (struct addrinfo*)open->value;
}

Int c_tcp_address_count(Int resolution) {
    Int count = 0;
    for (struct addrinfo* address = c_tcp_resolution(resolution); address != NULL; address = address->ai_next) {
        count++;
    }
    return count;
}

String c_tcp_address(Int resolution, Int index) {
    struct addrinfo* address = c_tcp_resolution(resolution);
    for (Int i = 0; address != NULL && i < index; i++) {
        address = address->ai_next;
    }

    // An IPv6 address keeps its zone, such as `fe80::1%lo0` (or `fe80::1%12` on Windows), much like on Node
    char text[INET6_ADDRSTRLEN + 1 + UV_IF_NAMESIZE] = "";
    if (address != NULL) {
        uv_ip_name(address->ai_addr, text, sizeof(text));

        struct sockaddr_in6* v6 = (struct sockaddr_in6*)address->ai_addr;
        size_t zone_size = UV_IF_NAMESIZE;
        char zone[UV_IF_NAMESIZE];
        if (address->ai_family == AF_INET6 && v6->sin6_scope_id != 0
            && uv_if_indextoiid(v6->sin6_scope_id, zone, &zone_size) == 0) {
            strcat(text, "%");
            strcat(text, zone);
        }
    }
    return c_bytearray_from_nullterminated_string(text);
}

void c_tcp_resolution_release(Int resolution) {
    intptr_t addresses;
    if (io_handle_close(resolution, &addresses)) {
        uv_freeaddrinfo((struct addrinfo*)addresses);
    }
}

// Parses an `address` as `c_tcp_address` writes it, IPv4 or IPv6, together with the given `port`.
static int c_tcp_parse(String address, Int port, struct sockaddr_storage* addr) {
    char* text = c_bytearray_into_nullterminated_string(address);
    erasePositive(address);

    int result = uv_ip4_addr(text, port, (struct sockaddr_in*)addr);
    if (result < 0) {
        result = uv_ip6_addr(text, port, (struct sockaddr_in6*)addr);
    }

    free(text);
    return result;
}

void c_tcp_connect(String address, Int port, Stack stack) {
    struct sockaddr_storage addr;
    int result = c_tcp_parse(address, port, &addr);

    if (result < 0) {
        resume_Int(stack, result);
        return;
    }

    uv_tcp_t* tcp_handle = malloc(sizeof(uv_tcp_t));
    result = uv_tcp_init(uv_default_loop(), tcp_handle);

    if (result < 0) {
        free(tcp_handle);
        resume_Int(stack, result);
        return;
    }
    tcp_handle->data = NULL;  // no pending read yet (explicit `NULL` since libuv leaves it uninitialised)

    uv_connect_t* connect_req = malloc(sizeof(uv_connect_t));
    connect_req->data = stack;

    result = uv_tcp_connect(connect_req, tcp_handle, (const struct sockaddr*)&addr, c_tcp_connect_cb);

    if (result < 0) {
        uv_close((uv_handle_t*)tcp_handle, (uv_close_cb)free);
        free(connect_req);
        resume_Int(stack, result);
        return;
    }
}

typedef struct {
    Stack stack;
    struct Pos buffer;
    size_t offset;
    size_t size;
} tcp_read_closure_t;

void c_tcp_read_cb(uv_stream_t* stream, ssize_t bytes_read, const uv_buf_t* buf) {
    (void)(buf);
    // 0 means nothing was read this time (EAGAIN), not end of file, so keep waiting.
    if (bytes_read == 0) return;

    tcp_read_closure_t* read_closure = (tcp_read_closure_t*)stream->data;
    Stack stack = read_closure->stack;

    uv_read_stop(stream);
    stream->data = NULL;
    erasePositive(read_closure->buffer);
    free(read_closure);

    // End of file is 0, as the contract in io/network says, but other errors stay negative.
    resume_Int(stack, bytes_read == UV_EOF ? 0 : (int64_t)bytes_read);
}

void c_tcp_read_alloc_cb(uv_handle_t* handle, size_t suggested_size, uv_buf_t* buf) {
    (void)(suggested_size);
    tcp_read_closure_t* read_closure = (tcp_read_closure_t*)handle->data;
    // Exactly the requested window [offset, offset + size), io/network checks it is in bounds.
    buf->base = (char*)c_bytearray_data(read_closure->buffer) + read_closure->offset;
    buf->len = read_closure->size;
}

void c_tcp_read(Int handle, struct Pos buffer, Int offset, Int size, Stack stack) {
    io_slot_t* open = io_handle_slot(handle);
    if (open == NULL) {
        erasePositive(buffer);
        resume_Int(stack, UV_EBADF);
        return;
    }
    uv_stream_t* stream = (uv_stream_t*)open->value;

    // At most one read is pending on a connection, its closure is in stream->data.
    if (stream->data != NULL) {
        erasePositive(buffer);
        resume_Int(stack, UV_EALREADY);
        return;
    }

    tcp_read_closure_t* read_closure = malloc(sizeof(tcp_read_closure_t));
    read_closure->stack = stack;
    read_closure->buffer = buffer;
    read_closure->offset = offset;
    read_closure->size = size;
    stream->data = read_closure;

    int result = uv_read_start(stream, c_tcp_read_alloc_cb, c_tcp_read_cb);

    if (result < 0) {
        stream->data = NULL;
        erasePositive(buffer);
        free(read_closure);
        resume_Int(stack, result);
    }
}

typedef struct {
    Stack stack;
    struct Pos buffer;
    size_t size;
} tcp_write_closure_t;

void c_tcp_write_cb(uv_write_t* request, int status) {
    tcp_write_closure_t* write_closure = (tcp_write_closure_t*)request->data;
    free(request);
    erasePositive(write_closure->buffer);
    Stack stack = write_closure->stack;
    // `uv_write` writes everything or fails, but our contract returns the number of bytes written.
    int64_t written = (int64_t)write_closure->size;
    free(write_closure);
    resume_Int(stack, status < 0 ? (int64_t)status : written);
}

void c_tcp_write(Int handle, struct Pos buffer, Int offset, Int size, Stack stack) {
    io_slot_t* open = io_handle_slot(handle);
    if (open == NULL) {
        erasePositive(buffer);
        resume_Int(stack, UV_EBADF);
        return;
    }
    uv_stream_t* stream = (uv_stream_t*)open->value;

    uv_buf_t buf = uv_buf_init((char*)(c_bytearray_data(buffer) + offset), size);

    tcp_write_closure_t* write_closure = malloc(sizeof(tcp_write_closure_t));
    write_closure->stack = stack;
    write_closure->buffer = buffer;
    write_closure->size = size;

    uv_write_t* request = malloc(sizeof(uv_write_t));
    request->data = write_closure;

    int result = uv_write(request, stream, &buf, 1, c_tcp_write_cb);

    if (result < 0) {
        free(request);
        erasePositive(buffer);
        free(write_closure);
        resume_Int(stack, result);
    }
}

void c_tcp_close_cb(uv_handle_t* handle) {
    Stack stack = (Stack)handle->data;
    free(handle);
    resume_Pos(stack, Unit);
}

void c_tcp_close(Int handle, Stack stack) {
    intptr_t value;
    if (!io_handle_close(handle, &value)) {
        resume_Pos(stack, Unit);
        return;
    }
    uv_handle_t* uv_handle = (uv_handle_t*)value;

    // A read still pending ends as at end of file, much like on the JS backend.
    tcp_read_closure_t* pending = (tcp_read_closure_t*)uv_handle->data;
    uv_handle->data = stack;
    if (pending != NULL) {
        uv_read_stop((uv_stream_t*)uv_handle);
        Stack reader = pending->stack;
        erasePositive(pending->buffer);
        free(pending);
        resume_Int(reader, 0);
    }
    uv_close(uv_handle, c_tcp_close_cb);
}

void c_tcp_listen_cb(uv_stream_t* server, int status);

void c_tcp_bind(String address, Int port, Stack stack) {
    struct sockaddr_storage addr;
    int result = c_tcp_parse(address, port, &addr);

    if (result < 0) {
        resume_Int(stack, result);
        return;
    }

    uv_tcp_t* tcp_handle = malloc(sizeof(uv_tcp_t));
    result = uv_tcp_init(uv_default_loop(), tcp_handle);

    if (result < 0) {
        free(tcp_handle);
        resume_Int(stack, result);
        return;
    }

    tcp_handle->data = NULL;  // no listen yet

    result = uv_tcp_bind(tcp_handle, (const struct sockaddr*)&addr, 0);
    if (result < 0) {
        uv_close((uv_handle_t*)tcp_handle, (uv_close_cb)free);
        resume_Int(stack, result);
        return;
    }

    // Listening already, so that the address is reserved (errors such as `EADDRINUSE` arise here),
    // but connections are only accepted once `listen` starts (see `c_tcp_listen_cb`).
    result = uv_listen((uv_stream_t*)tcp_handle, SOMAXCONN, c_tcp_listen_cb);
    if (result < 0) {
        uv_close((uv_handle_t*)tcp_handle, (uv_close_cb)free);
        resume_Int(stack, result);
        return;
    }

    resume_Int(stack, io_handle_new((intptr_t)tcp_handle));
}

Int c_tcp_port(Int listener) {
    io_slot_t* open = io_handle_slot(listener);
    if (open == NULL) return UV_EBADF;

    struct sockaddr_storage addr;
    int length = sizeof(addr);
    int result = uv_tcp_getsockname((uv_tcp_t*)open->value, (struct sockaddr*)&addr, &length);
    if (result < 0) return result;

    if (addr.ss_family == AF_INET6) return ntohs(((struct sockaddr_in6*)&addr)->sin6_port);
    return ntohs(((struct sockaddr_in*)&addr)->sin_port);
}

typedef struct {
    Stack stack;
    struct Neg handler;
} tcp_listen_closure_t;

// Accepts a connection and runs the handler of `listen` on it.
void c_tcp_accept(uv_stream_t* server) {
    tcp_listen_closure_t* listen_closure = (tcp_listen_closure_t*)server->data;

    uv_tcp_t* client = malloc(sizeof(uv_tcp_t));
    if (uv_tcp_init(uv_default_loop(), client) < 0) {
        free(client);
        return;
    }
    client->data = NULL;  // no pending read yet
    if (uv_accept(server, (uv_stream_t*)client) < 0) {
        uv_close((uv_handle_t*)client, (uv_close_cb)free);
        return;
    }

    struct Neg handler = listen_closure->handler;
    shareNegative(handler);
    run_Int(handler, io_handle_new((intptr_t)client));
}

void c_tcp_listen_cb(uv_stream_t* server, int status) {
    // An error while accepting concerns that one client, as opposed to the listener:
    // listen returns only once shutdown is called (as on the JS backend), so skip the client and keep listening.
    if (status < 0) return;

    // Before `listen` starts, libuv keeps the connection alive, and stops accepting until `uv_accept` takes it.
    if (server->data == NULL) return;

    c_tcp_accept(server);
}

void c_tcp_listen(Int listener, struct Neg handler, Stack stack) {
    io_slot_t* open = io_handle_slot(listener);
    if (open == NULL) {
        eraseNegative(handler);
        resume_Int(stack, UV_EBADF);
        return;
    }
    uv_stream_t* server = (uv_stream_t*)open->value;

    tcp_listen_closure_t* listen_closure = malloc(sizeof(tcp_listen_closure_t));
    listen_closure->stack = stack;
    listen_closure->handler = handler;
    server->data = listen_closure;

    c_tcp_accept(server);
}

void c_tcp_shutdown(Int handle, Stack stack) {
    intptr_t value;
    if (!io_handle_close(handle, &value)) {
        resume_Pos(stack, Unit);
        return;
    }
    uv_handle_t* uv_handle = (uv_handle_t*)value;
    tcp_listen_closure_t* listen_closure = (tcp_listen_closure_t*)uv_handle->data;

    uv_handle->data = stack;
    uv_close(uv_handle, c_tcp_close_cb);

    if (listen_closure) {
        Stack closure_stack = listen_closure->stack;
        struct Neg closure_handler = listen_closure->handler;
        free(listen_closure);
        eraseNegative(closure_handler);
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

// Subprocesses
// ------------
//
// Spawn options are plain Effekt data (see the `process` module):
// `c_spawn` receives, per each standard stream, whether to pipe it (1) or inherit it (0), and the callback to run for it,
// and uses that to build libuv's options only for the duration of `uv_spawn`.
// The pipes belong to the process in question and are closed when it closes.

// The standard input of a spawned process, as an Effekt object holding its pipe.
// The pipe belongs to the process, so the eraser only frees the object.
typedef struct {
    uv_stream_t* stream;
} writable_stream_t;

void c_writable_stream_erase(void* object) { free(object); }

struct Pos c_writable_stream_new(uv_stream_t* stream) {
    writable_stream_t* writable = (writable_stream_t*)effekt_alloc(c_writable_stream_erase, sizeof(writable_stream_t));
    writable->stream = stream;
    return (struct Pos) { .tag = 0, .obj = (char*)writable - sizeof(struct Header) };
}

uv_stream_t* c_writable_stream(struct Pos writable) {
    return ((writable_stream_t*)((char*)writable.obj + sizeof(struct Header)))->stream;
}

static void subproc_alloc_cb(uv_handle_t* handle, size_t suggested_size, uv_buf_t* buf) {
    (void)handle;
    buf->base = malloc(suggested_size);
    buf->len = suggested_size;
}

typedef struct subproc_proc_data subproc_proc_data_t;

typedef struct {
  struct Neg handler;
  subproc_proc_data_t* process;
} subproc_stream_cb_closure_t;

void subproc_finish(subproc_proc_data_t* pd);

// Calling a handler consumes a reference to it,
// so it is shared before each chunk and released once the stream ends (as in `c_tcp_listen_cb`).
void subproc_stream_cb(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buf) {
  subproc_stream_cb_closure_t* clos = (subproc_stream_cb_closure_t*)(stream->data);
  if (nread > 0) {
    struct Pos chunk = c_bytearray_construct(nread, (uint8_t*)(buf->base));
    if (buf->base) free(buf->base);
    shareNegative(clos->handler);
    run_Pos(clos->handler, chunk);
  } else if (nread < 0) {
    subproc_proc_data_t* pd = clos->process;
    uv_read_stop(stream);
    eraseNegative(clos->handler);
    free(clos);
    stream->data = NULL;
    if (buf->base) free(buf->base);
    subproc_finish(pd);
  } else {
    if (buf->base) free(buf->base);  // 0: nothing read this time
  }
}

// `spawn` resumes once the process has exited and each of its piped outputs has ended,
// so `pending` counts these, and `status` keeps the exit status until then.
struct subproc_proc_data {
  char** args;
  Stack k;
  uv_process_t* process;
  uv_pipe_t* pipes[3];  // NULL iff the stream is inherited
  int pending;
  Int status;
};

static void subproc_free_args(char** args) {
    for (int i = 0; args[i] != NULL; i++) {
        free(args[i]);
    }
    free(args);
}

void c_close_stream_callback_exit(uv_handle_t* req) {
    free(req);
}

void subproc_on_close(uv_handle_t* handle) {
    subproc_proc_data_t* pd = (subproc_proc_data_t*)((uv_process_t*)handle)->data;
    for (int i = 0; i < 3; i++) {
        if (pd->pipes[i] != NULL) {
            uv_close((uv_handle_t*)pd->pipes[i], c_close_stream_callback_exit);
        }
    }
    free(pd);
    free(handle);
}

void subproc_finish(subproc_proc_data_t* pd) {
    pd->pending--;
    if (pd->pending > 0) return;

    Stack k = pd->k;
    Int status = pd->status;
    uv_close((uv_handle_t*)pd->process, subproc_on_close);
    resume_Int(k, status);
}

// The status is the exit code, or the signal times 2^32 (see `process::internal::exitStatus`).
void subproc_on_exit(uv_process_t* proc, int64_t exit_status, int term_signal) {
    subproc_proc_data_t* pd = (subproc_proc_data_t*)proc->data;
    subproc_free_args(pd->args);
    pd->args = NULL;
    pd->status = term_signal != 0 ? (Int)term_signal * 4294967296 : exit_status;
    subproc_finish(pd);
}

void c_close_stream_callback(uv_shutdown_t* req, int status) {
    if(status != 0){
        // TODO error handling
    }
    free(req);
}

void c_close_stream(struct Pos writable) {
    uv_stream_t* str = c_writable_stream(writable);
    erasePositive(writable);
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

void c_write_stream(struct Pos writable, struct Pos buffer) {
    uv_stream_t* str = c_writable_stream(writable);
    erasePositive(writable);
    uv_write_t* req = (uv_write_t*)malloc(sizeof(uv_write_t));
    memset(req, 0, sizeof(uv_write_t));
    subproc_write_req_data_t* data = (subproc_write_req_data_t*)malloc(sizeof(subproc_write_req_data_t));
    data->buffer = buffer;
    uv_buf_t buf = uv_buf_init((char*)(c_bytearray_data(buffer)), buffer.tag);
    req->data = data;
    data->buf = buf;
    uv_write(req, str, &(data->buf), 1, c_write_stream_callback);
}

// Connects standard stream `id` of the process `pd` to a new pipe running `handler` if `pipe` is 1, or inherits it.
static uv_pipe_t* subproc_stdio(subproc_proc_data_t* pd, uv_stdio_container_t* stdio, int id, Int pipe, int flags, struct Neg handler) {
    if (!pipe) {
        eraseNegative(handler);
        stdio[id].flags = UV_INHERIT_FD;
        stdio[id].data.fd = id;
        return NULL;
    }

    uv_pipe_t* p = (uv_pipe_t*)malloc(sizeof(uv_pipe_t));
    uv_pipe_init(uv_default_loop(), p, 1);
    subproc_stream_cb_closure_t* clos = (subproc_stream_cb_closure_t*)malloc(sizeof(subproc_stream_cb_closure_t));
    clos->handler = handler;
    clos->process = pd;
    p->data = clos;
    stdio[id].flags = UV_CREATE_PIPE | flags;
    stdio[id].data.stream = (uv_stream_t*)p;
    return p;
}

// Note: The struct arguments come first (on x64, they may get split by LLVM)
void c_spawn(struct Pos cmd, struct Pos args, struct Neg on_stdin, struct Neg on_stdout, struct Neg on_stderr,
             Int pipeStdin, Int pipeStdout, Int pipeStderr, Stack stack) {
    subproc_proc_data_t* pd = (subproc_proc_data_t*)malloc(sizeof(subproc_proc_data_t));

    uv_stdio_container_t stdio[3];
    pd->pipes[0] = subproc_stdio(pd, stdio, 0, pipeStdin, UV_READABLE_PIPE, on_stdin);
    pd->pipes[1] = subproc_stdio(pd, stdio, 1, pipeStdout, UV_WRITABLE_PIPE, on_stdout);
    pd->pipes[2] = subproc_stdio(pd, stdio, 2, pipeStderr, UV_WRITABLE_PIPE, on_stderr);

    // command
    char* cmd_s = c_bytearray_into_nullterminated_string(cmd);
    erasePositive(cmd);

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

    // libuv only reads the options during `uv_spawn`.
    uv_process_options_t opts;
    memset(&opts, 0, sizeof(uv_process_options_t));
    opts.file = cmd_s;
    opts.args = _args;
    opts.exit_cb = subproc_on_exit;
    opts.stdio = stdio;
    opts.stdio_count = 3;

    pd->args = _args;
    pd->k = stack;
    uv_process_t* proc = (uv_process_t*)malloc(sizeof(uv_process_t));
    proc->data = pd;
    pd->process = proc;
    pd->pending = 1 + (pd->pipes[1] != NULL) + (pd->pipes[2] != NULL);

    // spawn process
    int err = uv_spawn(uv_default_loop(), proc, &opts);

    // The loop keeps the process even if `uv_spawn` fails
    if (err < 0) {
        for (int i = 0; i < 3; i++) {
            if (pd->pipes[i] != NULL) {
                subproc_stream_cb_closure_t* clos = (subproc_stream_cb_closure_t*)(pd->pipes[i]->data);
                eraseNegative(clos->handler);
                free(clos);
                uv_close((uv_handle_t*)pd->pipes[i], c_close_stream_callback_exit);
            }
        }
        subproc_free_args(_args);
        free(pd);
        uv_close((uv_handle_t*)proc, (uv_close_cb)free);
        resume_Int(stack, err);
        return;
    }

    if (pd->pipes[0] != NULL) {
        uv_stream_t* stream = (uv_stream_t*)pd->pipes[0];
        subproc_stream_cb_closure_t* clos = (subproc_stream_cb_closure_t*)(stream->data);
        stream->data = NULL;
        struct Neg handler = clos->handler;
        free(clos);
        run_Pos(handler, c_writable_stream_new(stream));  // consumes the handler's reference
    }
    if (pd->pipes[1] != NULL)
      uv_read_start((uv_stream_t*)pd->pipes[1], subproc_alloc_cb, subproc_stream_cb);
    if (pd->pipes[2] != NULL)
      uv_read_start((uv_stream_t*)pd->pipes[2], subproc_alloc_cb, subproc_stream_cb);
}

// Returns the required buffer size if the variable exists, or -1 if it does not.
// This avoids allocating when the variable is not set.
Int c_getenv_size(struct Pos name) {
    char* name_str = c_bytearray_into_nullterminated_string(name);
    erasePositive(name);

    char dummy;
    size_t size = 1; // Note: size = 1 is required, otherwise libuv always returns EINVAL.
    int result = uv_os_getenv(name_str, &dummy, &size);
    free(name_str);

    if (result == UV_ENOENT) return -1;
    if (result == 0 || result == UV_ENOBUFS) return (Int)size;
    return -1;
}

// Returns the value, given the caller already knows it exists with
// the given buffer size. Caller passes the size from c_getenv_size.
struct Pos c_getenv_value(struct Pos name, Int size) {
    char* name_str = c_bytearray_into_nullterminated_string(name);
    erasePositive(name);

    char* buffer = malloc((size_t)size);
    size_t buf_size = (size_t)size;
    int result = uv_os_getenv(name_str, buffer, &buf_size);
    free(name_str);

    if (result != 0) {
        // This should not happen, since we already checked with `c_getenv_size`
        // ...but in case it does, we return an empty string instead.
        free(buffer);
        return c_bytearray_new(0);
    }

    struct Pos value = c_bytearray_from_nullterminated_string(buffer);
    free(buffer);
    return value;
}

#endif
