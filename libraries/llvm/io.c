#ifndef EFFEKT_IO_C
#define EFFEKT_IO_C

#include <uv.h>
#include <string.h> // to compare flag names

// Println
// -------

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
    erasePositive(text);
    putchar('\n');
}


// Lib UV Bindings
// ---------------
// Ideas behind the LLVM / libuv implementation.
//
// Share memory between Effekt-buffers and libuv buffers
// -----------------------------------------------------
// A libuv buffer is a {ptr, i64} where i64 is the capacity
// while our buffer (from buffer.c, which is ref-counted) is a %Pos with a
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

int modeFlags(struct Pos mode) {
    switch (mode.tag) {
        case 0: // ReadOnly()
            return UV_FS_O_RDONLY;
        case 1: // WriteOnly()
            return UV_FS_O_WRONLY | UV_FS_O_CREAT | UV_FS_O_TRUNC;
        case 2: // AppendOnly()
            return UV_FS_O_WRONLY | UV_FS_O_CREAT | UV_FS_O_APPEND;
        case 3: // ReadWrite()
            return UV_FS_O_RDWR | UV_FS_O_CREAT | UV_FS_O_TRUNC;
        case 4: // ReadAppend()
            return UV_FS_O_RDWR | UV_FS_O_CREAT | UV_FS_O_APPEND;
        case 5: // AppendExclusive()
            return UV_FS_O_WRONLY | UV_FS_O_CREAT | UV_FS_O_APPEND | UV_FS_O_EXCL;
        case 6: // ReadAppendExclusive()
            return UV_FS_O_RDWR | UV_FS_O_CREAT | UV_FS_O_APPEND | UV_FS_O_EXCL;
        case 7: // AppendSync()
            return UV_FS_O_WRONLY | UV_FS_O_CREAT | UV_FS_O_APPEND | UV_FS_O_SYNC;
        case 8: // ReadAppendSync()
            return UV_FS_O_RDWR | UV_FS_O_CREAT | UV_FS_O_APPEND | UV_FS_O_SYNC;
        case 9: // ReadSync()
            return UV_FS_O_RDONLY | UV_FS_O_SYNC;
        case 10: // ReadWriteSync()
            return UV_FS_O_RDWR | UV_FS_O_SYNC;
        case 11: // WriteExclusive()
            return UV_FS_O_WRONLY | UV_FS_O_CREAT | UV_FS_O_TRUNC | UV_FS_O_EXCL;
        case 12: // ReadWriteExclusive()
            return UV_FS_O_RDWR | UV_FS_O_CREAT | UV_FS_O_TRUNC | UV_FS_O_EXCL;
        default:
            // Invalid tag value
            return -1;
    }
}

void c_fs_open(struct Pos path, struct Pos mode, Stack stack) {

    // Convert the Effekt String to a 0-terminated string
    char* path_str = c_buffer_as_null_terminated_string(path);
    erasePositive((struct Pos) path);

    // Convert the Effekt String representing the opening mode to libuv flags
    int flags = modeFlags(mode);
    erasePositive((struct Pos) mode);

    uv_fs_t* request = malloc(sizeof(uv_fs_t));
    request->data = stack;

    int result = uv_fs_open(uv_default_loop(), request, path_str, flags, 0666, c_resume_int_fs);

    if (result < 0) {
        uv_fs_req_cleanup(request);
        free(request);
        resume_Int(stack, result);
    }

    // We must free the string either way, since libuv copies it into the request
    free(path_str);

    return;
}

void c_fs_read(Int fd, struct Pos buffer, Int offset, Stack stack) {

    uv_fs_t* request = malloc(sizeof(uv_fs_t));
    request->data = stack;

    char* bytes = (char*) c_buffer_bytes(buffer); // libuv expects signed integers
    uv_buf_t buf = uv_buf_init(bytes, c_buffer_length(buffer));

    int result = uv_fs_read(uv_default_loop(), request, fd, &buf, 1, offset, c_resume_int_fs);

    if (result < 0) {
        uv_fs_req_cleanup(request);
        free(request);
        resume_Int(stack, result);
    }
}

void c_fs_write(Int fd, struct Pos buffer, Int offset, Stack stack) {

    uv_fs_t* request = malloc(sizeof(uv_fs_t));
    request->data = stack;

    char* bytes = (char*) c_buffer_bytes(buffer); // libuv expects signed integers
    uv_buf_t buf = uv_buf_init(bytes, c_buffer_length(buffer));

    int result = uv_fs_write(uv_default_loop(), request, fd, &buf, 1, offset, c_resume_int_fs);

    if (result < 0) {
        uv_fs_req_cleanup(request);
        free(request);
        resume_Int(stack, result);
    }
}

void c_fs_close(Int fd, Stack stack) {

    uv_fs_t* request = malloc(sizeof(uv_fs_t));
    request->data = stack;

    int result = uv_fs_close(uv_default_loop(), request, fd, c_resume_int_fs);

    if (result < 0) {
        uv_fs_req_cleanup(request);
        free(request);
        resume_Int(stack, result);
    }
}


/**
 * Maps the libuv error code to a stable (platform independent) numeric value.
 *
 * Tries to use most common errno integer values, but introduces fresh values (> 200)
 * for those without common errno values.
 */
Int c_error_number(Int errno) {
    switch (errno) {
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
