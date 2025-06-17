#ifndef EFFEKT_IO_C
#define EFFEKT_IO_C

#include <uv.h>
#include <string.h> // to compare flag names

// Println and Readln
// ------------------

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

void c_fs_read(Int file, struct Pos buffer, Int offset, Int size, Int position, Stack stack) {

    uv_buf_t buf = uv_buf_init((char*)(c_bytearray_data(buffer) + offset), size);
    erasePositive(buffer);
    // TODO panic if this was the last reference

    uv_fs_t* request = malloc(sizeof(uv_fs_t));
    request->data = stack;

    int result = uv_fs_read(uv_default_loop(), request, file, &buf, 1, position, c_resume_int_fs);

    if (result < 0) {
        uv_fs_req_cleanup(request);
        free(request);
        resume_Int(stack, result);
    }
}

void c_fs_write(Int file, struct Pos buffer, Int offset, Int size, Int position, Stack stack) {

    uv_buf_t buf = uv_buf_init((char*)(c_bytearray_data(buffer) + offset), size);
    erasePositive(buffer);
    // TODO panic if this was the last reference

    uv_fs_t* request = malloc(sizeof(uv_fs_t));
    request->data = stack;

    int result = uv_fs_write(uv_default_loop(), request, file, &buf, 1, position, c_resume_int_fs);

    if (result < 0) {
        uv_fs_req_cleanup(request);
        free(request);
        resume_Int(stack, result);
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
        uv_close((uv_handle_t*)request->handle, NULL);
        free(request->handle);
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

    if (result < 0) {
        free(tcp_handle);
        free(connect_req);
        free(host_str);
        resume_Int(stack, result);
        return;
    }

    result = uv_tcp_connect(connect_req, tcp_handle, (const struct sockaddr*)&addr, c_tcp_connect_cb);

    if (result < 0) {
        free(tcp_handle);
        free(connect_req);
        free(host_str);
        resume_Int(stack, result);
        return;
    }
}

typedef struct {
    Stack stack;
    size_t size;
    char* data;
} tcp_read_closure_t;

void c_tcp_read_cb(uv_stream_t* stream, ssize_t bytes_read, const uv_buf_t* buf) {
    tcp_read_closure_t* read_closure = (tcp_read_closure_t*)stream->data;
    Stack stack = read_closure->stack;

    uv_read_stop(stream);
    free(read_closure);

    resume_Int(stack, (int64_t)bytes_read);
}

void c_tcp_read_alloc_cb(uv_handle_t* handle, size_t suggested_size, uv_buf_t* buf) {
    tcp_read_closure_t* read_closure = (tcp_read_closure_t*)handle->data;
    buf->base = read_closure->data;
    buf->len = read_closure->size;
}

void c_tcp_read(Int handle, struct Pos buffer, Int offset, Int size, Stack stack) {
    uv_stream_t* stream = (uv_stream_t*)handle;

    char* buffer_ptr = (char*)(c_bytearray_data(buffer) + offset);
    erasePositive(buffer);

    tcp_read_closure_t* read_closure = malloc(sizeof(tcp_read_closure_t));
    read_closure->stack = stack;
    read_closure->size = size;
    read_closure->data = buffer_ptr;
    stream->data = read_closure;

    int result = uv_read_start(stream, c_tcp_read_alloc_cb, c_tcp_read_cb);

    if (result < 0) {
        free(read_closure);
        stream->data = NULL;
        resume_Int(stack, result);
    }
}

void c_tcp_write_cb(uv_write_t* request, int status) {
    Stack stack = (Stack)request->data;
    free(request);
    resume_Int(stack, (int64_t)status);
}

void c_tcp_write(Int handle, struct Pos buffer, Int offset, Int size, Stack stack) {
    uv_stream_t* stream = (uv_stream_t*)handle;

    uv_buf_t buf = uv_buf_init((char*)(c_bytearray_data(buffer) + offset), size);
    erasePositive(buffer);

    uv_write_t* request = malloc(sizeof(uv_write_t));
    request->data = stack;

    int result = uv_write(request, stream, &buf, 1, c_tcp_write_cb);

    if (result < 0) {
        free(request);
        resume_Int(stack, result);
    }
}

void c_tcp_close_cb(uv_handle_t* handle) {
    Stack stack = (Stack)handle->data;
    free(handle);
    // TODO resume_Pos Unit
    resume_Int(stack, 0);
}

void c_tcp_close(Int handle, Stack stack) {
    uv_handle_t* uv_handle = (uv_handle_t*)handle;
    uv_handle->data = stack;
    uv_close(uv_handle, c_tcp_close_cb);
}


typedef struct {
    Stack stack;
    struct Pos handler;
} tcp_accept_closure_t;

void c_tcp_listen(String host, Int port, Int backlog, Stack stack) {
    // TODO make non-async
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

    struct sockaddr_in addr;
    result = uv_ip4_addr(host_str, port, &addr);
    free(host_str);

    if (result < 0) {
        free(tcp_handle);
        resume_Int(stack, result);
        return;
    }

    result = uv_tcp_bind(tcp_handle, (const struct sockaddr*)&addr, 0);
    if (result < 0) {
        free(tcp_handle);
        resume_Int(stack, result);
        return;
    }

    result = uv_listen((uv_stream_t*)tcp_handle, backlog, NULL);
    if (result < 0) {
        free(tcp_handle);
        resume_Int(stack, result);
        return;
    }

    resume_Int(stack, (int64_t)tcp_handle);
}

void c_tcp_accept_cb(uv_stream_t* server, int status) {
    tcp_accept_closure_t* accept_closure = (tcp_accept_closure_t*)server->data;

    if (status < 0) {
        // TODO resume last
        erasePositive(accept_closure->handler);
        resume_Int(accept_closure->stack, status);
        free(accept_closure);
        server->data = NULL;
        return;
    }

    uv_tcp_t* client = malloc(sizeof(uv_tcp_t));
    int result = uv_tcp_init(uv_default_loop(), client);

    if (result < 0) {
        // TODO resume last
        free(client);
        erasePositive(accept_closure->handler);
        resume_Int(accept_closure->stack, result);
        free(accept_closure);
        server->data = NULL;
        return;
    }

    result = uv_accept(server, (uv_stream_t*)client);
    if (result < 0) {
        // TODO resume last
        uv_close((uv_handle_t*)client, NULL);
        free(client);
        erasePositive(accept_closure->handler);
        resume_Int(accept_closure->stack, result);
        free(accept_closure);
        server->data = NULL;
        return;
    }

    sharePositive(accept_closure->handler);
    run_Int(accept_closure->handler, (int64_t)client);
}

void c_tcp_accept(Int listener, struct Pos handler, Stack stack) {
    uv_stream_t* server = (uv_stream_t*)listener;

    tcp_accept_closure_t* accept_closure = malloc(sizeof(tcp_accept_closure_t));
    accept_closure->stack = stack;
    accept_closure->handler = handler;
    server->data = accept_closure;

    int result = uv_listen(server, 0, c_tcp_accept_cb);
    if (result < 0) {
        free(accept_closure);
        erasePositive(handler);
        resume_Int(stack, result);
        return;
    }
}

void c_tcp_shutdown_cb(uv_handle_t* handle) {
    Stack stack = (Stack)handle->data;
    free(handle);
    // TODO resume_Pos Unit
    resume_Int(stack, 0);
}

void c_tcp_shutdown(Int handle, Stack stack) {
    uv_handle_t* uv_handle = (uv_handle_t*)handle;

    tcp_accept_closure_t* accept_closure = (tcp_accept_closure_t*)uv_handle->data;
    if (accept_closure) {
        eraseStack(accept_closure->stack);
        erasePositive(accept_closure->handler);
        free(accept_closure);
    }

    uv_handle->data = stack;
    uv_close(uv_handle, c_tcp_shutdown_cb);
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


// Promises
// --------

typedef enum { UNRESOLVED, RESOLVED } promise_state_t;

typedef struct Listeners {
    Stack head;
    struct Listeners* tail;
} Listeners;

typedef struct {
    uint64_t rc;
    void* eraser;
    promise_state_t state;
    // state of {
    //   case UNRESOLVED => Possibly empty (head is NULL) list of listeners
    //   case RESOLVED   => Pos (the result)
    // }
    union {
        struct Pos value;
        Listeners listeners;
    } payload;
} Promise;

void c_promise_erase_listeners(void *envPtr) {
    // envPtr points to a Promise _after_ the eraser, so let's adjust it to point to the promise.
    Promise *promise = (Promise*) (envPtr - offsetof(Promise, state));
    promise_state_t state = promise->state;

    Stack head;
    Listeners* tail;
    Listeners* current;

    switch (state) {
        case UNRESOLVED:
            head = promise->payload.listeners.head;
            tail = promise->payload.listeners.tail;
            if (head != NULL) {
                // Erase head
                eraseStack(head);
                // Erase tail
                current = tail;
                while (current != NULL) {
                    head = current->head;
                    tail = current->tail;
                    free(current);
                    eraseStack(head);
                    current = tail;
                };
            };
            break;
        case RESOLVED:
            erasePositive(promise->payload.value);
            break;
    }
}

void c_promise_resume_listeners(Listeners* listeners, struct Pos value) {
    if (listeners != NULL) {
        Stack head = listeners->head;
        Listeners* tail = listeners->tail;
        free(listeners);
        c_promise_resume_listeners(tail, value);
        sharePositive(value);
        resume_Pos(head, value);
    }
}

void c_promise_resolve(struct Pos promise, struct Pos value, Stack stack) {
    Promise* p = (Promise*)promise.obj;

    Stack head;
    Listeners* tail;

    switch (p->state) {
        case UNRESOLVED:
            head = p->payload.listeners.head;
            tail = p->payload.listeners.tail;

            p->state = RESOLVED;
            p->payload.value = value;
            resume_Pos(stack, Unit);

            if (head != NULL) {
                // Execute tail
                c_promise_resume_listeners(tail, value);
                // Execute head
                sharePositive(value);
                resume_Pos(head, value);
            };
            break;
        case RESOLVED:
            erasePositive(promise);
            erasePositive(value);
            eraseStack(stack);
            fprintf(stderr, "ERROR: Promise already resolved\n");
            exit(1);
            break;
    }
    // TODO stack overflow?
    // We need to erase the promise now, since we consume it.
    erasePositive(promise);
}

void c_promise_await(struct Pos promise, Stack stack) {
    Promise* p = (Promise*)promise.obj;

    Stack head;
    Listeners* tail;
    Listeners* node;
    struct Pos value;

    switch (p->state) {
        case UNRESOLVED:
            head = p->payload.listeners.head;
            tail = p->payload.listeners.tail;
            if (head != NULL) {
                node = (Listeners*)malloc(sizeof(Listeners));
                node->head = head;
                node->tail = tail;
                p->payload.listeners.head = stack;
                p->payload.listeners.tail = node;
            } else {
                p->payload.listeners.head = stack;
            };
            break;
        case RESOLVED:
            value = p->payload.value;
            sharePositive(value);
            resume_Pos(stack, value);
            break;
    };
    // TODO hmm, stack overflow?
    erasePositive(promise);
}

struct Pos c_promise_make() {
    Promise* promise = (Promise*)malloc(sizeof(Promise));

    promise->rc = 0;
    promise->eraser = c_promise_erase_listeners;
    promise->state = UNRESOLVED;
    promise->payload.listeners.head = NULL;
    promise->payload.listeners.tail = NULL;

    return (struct Pos) { .tag = 0, .obj = promise, };
}


#endif
