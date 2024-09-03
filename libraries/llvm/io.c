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


// Promises
// --------

typedef enum { UNRESOLVED, RESOLVED, AWAITED } promise_state_t;

typedef struct Listeners {
    struct Neg listener;
    struct Listeners* next;
} Listeners;

typedef struct {
    uint64_t rc;
    void* eraser;
    promise_state_t state;
    // state of {
    //   case UNRESOLVED => NULL
    //   case RESOLVED   => Pos (the result)
    //   case AWAITED    => Nonempty list of listeners
    // }
    union {
        struct Pos pos;
        Listeners* listeners;
    } payload;
} Promise;

void c_promise_erase_listeners(struct Pos promise) {
    Promise* p = (Promise*)promise.obj;

    switch (p->state) {
        case UNRESOLVED:
            return;
        case AWAITED:
            {
                Listeners* current = p->payload.listeners;
                // Free all listeners
                while (current != NULL) {
                    Listeners* k = current;
                    current = current->next;
                    eraseNegative(k->listener);
                    free(k);
                }
                p->payload.listeners = NULL;
            }
            return;
        case RESOLVED:
            erasePositive(p->payload.pos);
            return;
    }
}

void c_promise_resolve(struct Pos promise, struct Pos value) {
    Promise* p = (Promise*)promise.obj;

    switch (p->state) {
        case UNRESOLVED:
            p->state = RESOLVED;
            p->payload.pos = value; // Store value in payload
            break;
        case RESOLVED:
            fprintf(stderr, "ERROR: Promise already resolved\n");
            exit(1);
        case AWAITED: {
            Listeners* current = p->payload.listeners;
            p->state = RESOLVED;
            p->payload.pos = value;

             // Call each listeners
            while (current != NULL) {
                sharePositive(value);
                run_Pos(current->listener, value);
                Listeners* temp = current;
                current = current->next;
                free(temp);
            }
            break;
        }
    }
    // do we need to erase promise now? Is it shared before?
    erasePositive(promise);
}

void c_promise_await(struct Pos promise, struct Neg listener) {
    Promise* p = (Promise*)promise.obj;

    switch (p->state) {
        case UNRESOLVED:
            p->state = AWAITED;
            p->payload.listeners = (Listeners*)malloc(sizeof(Listeners));
            p->payload.listeners->listener = listener;
            p->payload.listeners->next = NULL;
            break;
        case RESOLVED:
            run_Pos(listener, p->payload.pos);
            break;
        case AWAITED: {
            Listeners* new_node = (Listeners*)malloc(sizeof(Listeners));
            new_node->listener = listener;
            new_node->next = NULL;

            // We traverse the listeners to attach this last .
            // This has O(n) for EACH await -- reverse on resolve would be O(n) ONCE.
            // But how many listeners will there be?
            // If really necessary, we can store a second pointer that points to the last one...
            Listeners* current = p->payload.listeners;
            while (current->next != NULL) {
                current = current->next;
            }
            current->next = new_node;
            break;
        }
    }
    erasePositive(promise);
}


struct Pos c_promise_make() {
    Promise* promise = (Promise*)malloc(sizeof(Promise));

    promise->rc = 0;
    promise->eraser = (void*)c_promise_erase_listeners;
    promise->state = UNRESOLVED;
    promise->payload.pos = Unit;

    return (struct Pos) { .tag = 0, .obj = promise, };
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
// Callbacks in Data-fields
// ------------------------
// In order to call Effekt-functions as a callback, we store a pointer
// to their closure into the user-definable data-field (at address 0)
// in each request object.
//
//
// TODO
// - Error reporting
// - pooling of request objects (benchmark first!)
// - always pass by-reference, not by-value? (to work around C ABI issues)


/**
 * Maps the libuv error code to a stable (platform independent) numeric value.
 *
 * Tries to use most common errno integer values, but introduces fresh values (> 200)
 * for those without common errno values.
 */
int uv_error_to_errno(int uv_err) {
    switch (uv_err) {
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


// File Descriptors
// ----------------
// Extern type defs at the moment are always treated as %Pos.
// For this reason, we need to translate the 32bit integer into
// a %Pos in the following way:
//
// +----------------+-------+
// | filedescriptor | Obj   |
// | 32bit -> 64bit | NULL  |
// +----------------+-------+
struct Pos filedescriptor_to_pos(int32_t fd) {
    return (struct Pos) { .tag = (int64_t) fd, .obj = NULL, };
}

int32_t pos_to_filedescriptor(struct Pos fd) {
    return (int32_t) fd.tag;
}

struct Pos c_filedescriptor_show(struct Pos fd) {
  return c_buffer_show_Int(pos_to_filedescriptor(fd));
}

// Timers
// ------

void c_timer_handler(uv_timer_t* handle) {
    // Load callback
    struct Neg callback = *(struct Neg*)handle->data;

    // Clean up
    uv_timer_stop(handle);
    free(handle->data);
    free(handle);

    // Finally call the callback
    run(callback);
}

void c_timer_wait(int64_t n, struct Neg callback) {

    // Get the default loop
    uv_loop_t* loop = uv_default_loop();

    // Allocate memory for the timer handle
    uv_timer_t* timer = (uv_timer_t*)malloc(sizeof(uv_timer_t));

    // // Initialize the timer handle
    uv_timer_init(loop, timer);

    // Allocate memory for the callback (of type Neg)
    struct Neg* payload = (struct Neg*)malloc(sizeof(struct Neg));
    payload->vtable = callback.vtable;
    payload->obj = callback.obj;

    // Store the Neg pointer in the timer's data field
    timer->data = (void*) payload;

    // Start the timer to call the callback after n ms
    uv_timer_start(timer, c_timer_handler, n, 0);
}


// Opening a File
// --------------

int modeToFlags(const char* flags) {
    if (strcmp(flags, "r") == 0) {
        return UV_FS_O_RDONLY;
    } else if (strcmp(flags, "r+") == 0) {
        return UV_FS_O_RDWR;
    } else if (strcmp(flags, "rs") == 0) {
        return UV_FS_O_RDONLY | UV_FS_O_SYNC;
    } else if (strcmp(flags, "rs+") == 0) {
        return UV_FS_O_RDWR | UV_FS_O_SYNC;
    } else if (strcmp(flags, "w") == 0) {
        return UV_FS_O_WRONLY | UV_FS_O_CREAT | UV_FS_O_TRUNC;
    } else if (strcmp(flags, "wx") == 0) {
        return UV_FS_O_WRONLY | UV_FS_O_CREAT | UV_FS_O_TRUNC | UV_FS_O_EXCL;
    } else if (strcmp(flags, "w+") == 0) {
        return UV_FS_O_RDWR | UV_FS_O_CREAT | UV_FS_O_TRUNC;
    } else if (strcmp(flags, "wx+") == 0) {
        return UV_FS_O_RDWR | UV_FS_O_CREAT | UV_FS_O_TRUNC | UV_FS_O_EXCL;
    } else if (strcmp(flags, "a") == 0) {
        return UV_FS_O_WRONLY | UV_FS_O_CREAT | UV_FS_O_APPEND;
    } else if (strcmp(flags, "ax") == 0) {
        return UV_FS_O_WRONLY | UV_FS_O_CREAT | UV_FS_O_APPEND | UV_FS_O_EXCL;
    } else if (strcmp(flags, "a+") == 0) {
        return UV_FS_O_RDWR | UV_FS_O_CREAT | UV_FS_O_APPEND;
    } else if (strcmp(flags, "ax+") == 0) {
        return UV_FS_O_RDWR | UV_FS_O_CREAT | UV_FS_O_APPEND | UV_FS_O_EXCL;
    } else if (strcmp(flags, "as") == 0) {
        return UV_FS_O_WRONLY | UV_FS_O_CREAT | UV_FS_O_APPEND | UV_FS_O_SYNC;
    } else if (strcmp(flags, "as+") == 0) {
        return UV_FS_O_RDWR | UV_FS_O_CREAT | UV_FS_O_APPEND | UV_FS_O_SYNC;
    } else {
        // Invalid flags string
        return -1;
    }
}

typedef struct Callbacks {
    struct Neg on_success;
    struct Neg on_failure;
} Callbacks;

void c_file_open_handler(uv_fs_t* req) {
    // Extract the file descriptor from the uv_fs_t structure
    int64_t fd = req->result;

    // Load the callbacks
    Callbacks* callbacks = (Callbacks*)req->data;
    struct Neg success = callbacks->on_success;
    struct Neg failure = callbacks->on_failure;

    // Free request structure
    uv_fs_req_cleanup(req);
    free(req);
    free(callbacks);

    // Check if file descriptor is valid
    if (fd >= 0) {
        eraseNegative(failure);
        run_Pos(success, filedescriptor_to_pos(fd));
    } else {
        eraseNegative(success);
        run_Int(failure, uv_error_to_errno(fd));
    }
}


void c_file_open(struct Pos path, struct Pos modeString, struct Neg* success, struct Neg* failure) {
    int permissions = 0666;  // rw-rw-rw- permissions

    uv_fs_t* req = (uv_fs_t*)malloc(sizeof(uv_fs_t));

    // Convert the Effekt String to a 0-terminated string
    char* path_str = c_buffer_as_null_terminated_string(path);
    erasePositive((struct Pos) path);

    // Convert the Effekt String representing the opening mode to libuv flags
    int32_t mode = modeToFlags(c_buffer_as_null_terminated_string(modeString));
    erasePositive((struct Pos) modeString);

    // Allocate Callbacks on the heap
    Callbacks* callbacks = (Callbacks*)malloc(sizeof(Callbacks));
    callbacks->on_success = *success;
    callbacks->on_failure = *failure;

    // Store the callbacks in the req's data field
    req->data = callbacks;

    // Get the default loop and call fs_open
    uv_loop_t* loop = uv_default_loop();

    int32_t result_i32 = uv_fs_open(loop, req, path_str, mode, (int32_t)permissions, c_file_open_handler);
    int64_t result_i64 = (int64_t)result_i32;

    // We can free the string, since libuv copies it into req
    free(path_str);

    return; // result_i64;
}


// Reading a File
// --------------

void c_file_read_handler(uv_fs_t* req) {
    // Extract the file descriptor from the uv_fs_t structure
    int64_t result = req->result;

    // Load the callbacks
    Callbacks* callbacks = (Callbacks*)req->data;
    struct Neg success = callbacks->on_success;
    struct Neg failure = callbacks->on_failure;

    // Free request structure
    uv_fs_req_cleanup(req);
    free(req);
    free(callbacks);

    if (result >= 0) {
        eraseNegative(failure);
        run_Int(success, result);
    } else {
        eraseNegative(success);
        run_Int(failure, uv_error_to_errno(result));
    }
}

/**
 * Here we require success and failure to be passed by reference (can be
 * stack-allocated). This is to work around an issue with the C ABI where
 * late arguments are scrambled.
 */
void c_file_read(int32_t fd, struct Pos buffer, int64_t offset, struct Neg* success, struct Neg* failure) {

    // Get the default loop
    uv_loop_t* loop = uv_default_loop();

    uint8_t* buffer_data = c_buffer_bytes(buffer);
    int32_t len = (int32_t)c_buffer_length(buffer);

    uv_buf_t buf = uv_buf_init((char*)buffer_data, len);

    uv_fs_t* req = (uv_fs_t*)malloc(sizeof(uv_fs_t));

    // Allocate Callbacks on the heap
    Callbacks* callbacks = (Callbacks*)malloc(sizeof(Callbacks));
    callbacks->on_success = *success;
    callbacks->on_failure = *failure;

    // // Store the callbacks in the req's data field
    req->data = callbacks;

    // // Argument `1` here means: we pass exactly one buffer
    uv_fs_read(loop, req, fd, &buf, 1, offset, c_file_read_handler);
}


// Writing to a File
// -----------------

void c_file_write_handler(uv_fs_t* req) {
    // Extract the result from the uv_fs_t structure
    int64_t result = req->result;

    // Load the callbacks
    Callbacks* callbacks = (Callbacks*)req->data;
    struct Neg success = callbacks->on_success;
    struct Neg failure = callbacks->on_failure;

    // Free request structure
    uv_fs_req_cleanup(req);
    free(req);
    free(callbacks);

    if (result >= 0) {
        eraseNegative(failure);
        run_Int(success, result);
    } else {
        eraseNegative(success);
        run_Int(failure, uv_error_to_errno(result));
    }
}

/**
 * Here we require success and failure to be passed by reference (can be
 * stack-allocated). This is to work around an issue with the C ABI where
 * late arguments are scrambled.
 */
void c_file_write(int32_t fd, struct Pos buffer, int64_t offset, struct Neg* success, struct Neg* failure) {
    // Get the default loop
    uv_loop_t* loop = uv_default_loop();

    uint8_t* buffer_data = c_buffer_bytes(buffer);
    int32_t len = (int32_t)c_buffer_length(buffer);

    uv_buf_t buf = uv_buf_init((char*)buffer_data, len);

    uv_fs_t* req = (uv_fs_t*)malloc(sizeof(uv_fs_t));

    // Allocate Callbacks on the heap
    Callbacks* callbacks = (Callbacks*)malloc(sizeof(Callbacks));
    callbacks->on_success = *success;
    callbacks->on_failure = *failure;

    // Store the callbacks in the req's data field
    req->data = callbacks;

    // Argument `1` here means: we pass exactly one buffer
    uv_fs_write(loop, req, fd, &buf, 1, offset, c_file_write_handler);
}

// ; Closing a File
// ; --------------


void c_file_close(int32_t fd) {
    uv_fs_t req;
    uv_loop_t* loop = uv_default_loop();
    uv_fs_close(loop, &req, fd, NULL);
}


// DIRECT STYLE

void c_resume_int_fs(uv_fs_t* req) {
    int64_t result = (int64_t)req->result;
    Stack stack = (Stack)req->data;

    // Free request structure
    uv_fs_req_cleanup(req);
    free(req);

    resume_Int(stack, result);
}

void c_open_file(struct Pos path, struct Pos modeString, Stack stack) {
    int permissions = 0666;  // rw-rw-rw- permissions

    uv_fs_t* req = (uv_fs_t*)malloc(sizeof(uv_fs_t));

    // Convert the Effekt String to a 0-terminated string
    char* path_str = c_buffer_as_null_terminated_string(path);
    erasePositive((struct Pos) path);

    // Convert the Effekt String representing the opening mode to libuv flags
    int32_t mode = modeToFlags(c_buffer_as_null_terminated_string(modeString));
    erasePositive((struct Pos) modeString);

    // Store the stack in the req's data field
    req->data = stack;

    // TODO fix mode and permissions and stuff

    // Get the default loop and call fs_open
    uv_loop_t* loop = uv_default_loop();
    int32_t result_i32 = uv_fs_open(loop, req, path_str, mode, (int32_t)permissions, c_resume_int_fs);
    int64_t result_i64 = (int64_t)result_i32;

    // TODO report error result (UV_EINVAL)

    // We can free the string, since libuv copies it into req
    free(path_str);

    return; // result_i64;
}

void c_read_file(Int fd, struct Pos buffer, Int offset, Stack stack) {

    // Get the default loop
    uv_loop_t* loop = uv_default_loop();

    // TODO fix types, cast only on assignment!
    // TODO fix names
    // TODO remove comments!!!

    uint8_t* buffer_data = c_buffer_bytes(buffer);
    int32_t len = (int32_t)c_buffer_length(buffer);

    uv_buf_t buf = uv_buf_init((char*)buffer_data, len);

    uv_fs_t* req = (uv_fs_t*)malloc(sizeof(uv_fs_t));

    req->data = stack;

    // // Argument `1` here means: we pass exactly one buffer
    uv_fs_read(loop, req, fd, &buf, 1, offset, c_resume_int_fs);
}

void c_write_file(Int fd, struct Pos buffer, Int offset, Stack stack) {
    // Get the default loop
    uv_loop_t* loop = uv_default_loop();

    // TODO fix types, cast only on assignment!
    // TODO fix names
    // TODO remove comments!!!

    uint8_t* buffer_data = c_buffer_bytes(buffer);
    int32_t len = (int32_t)c_buffer_length(buffer);

    uv_buf_t buf = uv_buf_init((char*)buffer_data, len);

    uv_fs_t* req = (uv_fs_t*)malloc(sizeof(uv_fs_t));

    req->data = stack;

    // Argument `1` here means: we pass exactly one buffer
    uv_fs_write(loop, req, fd, &buf, 1, offset, c_resume_int_fs);
}

void c_close_file(Int fd, Stack stack) {
    // Get the default loop
    uv_loop_t* loop = uv_default_loop();

    // TODO fix types, cast only on assignment!
    // TODO fix names
    // TODO remove comments!!!

    uv_fs_t* req = (uv_fs_t*)malloc(sizeof(uv_fs_t));

    req->data = stack;

    uv_fs_close(loop, req, fd, c_resume_int_fs);
}

void c_resume_unit_timer(uv_timer_t* handle) {
  Stack stack = handle->data;
  free(handle);
  resume_Pos(stack, Unit);
}

void c_wait_timer(Int millis, Stack stack) {

  // Get the default loop
  uv_loop_t* loop = uv_default_loop();

  // Allocate memory for the timer handle
  uv_timer_t* timer = (uv_timer_t*)malloc(sizeof(uv_timer_t));

  // // Initialize the timer handle
  uv_timer_init(loop, timer);

  timer->data = stack;

  // Start the timer to call the callback after n ms
  uv_timer_start(timer, c_resume_unit_timer, millis, 0);
}


#endif
