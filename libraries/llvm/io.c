#ifndef EFFEKT_IO_C
#define EFFEKT_IO_C

#include <uv.h>

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


// libuv

extern struct Pos run(struct Neg);

/**
 * Allocates memory for a uv_fs_t
 */
uv_fs_t* fresh_fs_req() {
    uv_fs_t* req = (uv_fs_t*)malloc(sizeof(uv_fs_t));
    return req;
}

void on_timer(uv_timer_t* handle) {
    struct Neg* neg = (struct Neg*)handle->data;
    uv_timer_stop(handle);
    free(handle);
    // finally call the callback
    run(*neg);
}

void timer(int64_t n, struct Neg callback) {

    // Get the default loop
    uv_loop_t* loop = uv_default_loop();

    // Allocate memory for the timer handle
    uv_timer_t* timer = (uv_timer_t*)malloc(sizeof(uv_timer_t));

    // // Initialize the timer handle
    uv_timer_init(loop, timer);

    // Allocate memory for the callback (of type Neg)
    struct Neg* neg = (struct Neg*) malloc(sizeof(struct Neg));
    neg->vtable = callback.vtable;
    neg->obj    = callback.obj;

    // Store the Neg pointer in the timer's data field
    timer->data = (void*) neg;

    // Start the timer to call the callback after n ms
    uv_timer_start(timer, on_timer, n, 0);
}



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
        case UV_EUNATCH:          return 219;  // Fresh unique value
        case UV_ESHUTDOWN:        return 220;  // Fresh unique value

        default:                  return -1;   // Unknown error
    }
}

/**
 * Maps the stable (platform independent) numeric value back to the libuv error code.
 */
int errno_to_uv_error(int errno_value) {
    switch (errno_value) {
        case 1:    return UV_EPERM;
        case 2:    return UV_ENOENT;
        case 3:    return UV_ESRCH;
        case 4:    return UV_EINTR;
        case 5:    return UV_EIO;
        case 6:    return UV_ENXIO;
        case 7:    return UV_E2BIG;
        case 9:    return UV_EBADF;
        case 11:   return UV_EAGAIN;
        case 12:   return UV_ENOMEM;
        case 13:   return UV_EACCES;
        case 14:   return UV_EFAULT;
        case 16:   return UV_EBUSY;
        case 17:   return UV_EEXIST;
        case 18:   return UV_EXDEV;
        case 19:   return UV_ENODEV;
        case 20:   return UV_ENOTDIR;
        case 21:   return UV_EISDIR;
        case 22:   return UV_EINVAL;
        case 23:   return UV_ENFILE;
        case 24:   return UV_EMFILE;
        case 25:   return UV_ENOTTY;
        case 26:   return UV_ETXTBSY;
        case 27:   return UV_EFBIG;
        case 28:   return UV_ENOSPC;
        case 29:   return UV_ESPIPE;
        case 30:   return UV_EROFS;
        case 31:   return UV_EMLINK;
        case 32:   return UV_EPIPE;
        case 34:   return UV_ERANGE;
        case 36:   return UV_ENAMETOOLONG;
        case 40:   return UV_ELOOP;
        case 75:   return UV_EOVERFLOW;
        case 79:   return UV_EFTYPE;
        case 84:   return UV_EILSEQ;
        case 88:   return UV_ENOTSOCK;
        case 89:   return UV_EDESTADDRREQ;
        case 90:   return UV_EMSGSIZE;
        case 91:   return UV_EPROTOTYPE;
        case 92:   return UV_ENOPROTOOPT;
        case 93:   return UV_EPROTONOSUPPORT;
        case 94:   return UV_ESOCKTNOSUPPORT;
        case 95:   return UV_ENOTSUP;
        case 97:   return UV_EAFNOSUPPORT;
        case 98:   return UV_EADDRINUSE;
        case 99:   return UV_EADDRNOTAVAIL;
        case 100:  return UV_ENETDOWN;
        case 101:  return UV_ENETUNREACH;
        case 103:  return UV_ECONNABORTED;
        case 104:  return UV_ECONNRESET;
        case 105:  return UV_ENOBUFS;
        case 106:  return UV_EISCONN;
        case 107:  return UV_ENOTCONN;
        case 110:  return UV_ETIMEDOUT;
        case 111:  return UV_ECONNREFUSED;
        case 113:  return UV_EHOSTUNREACH;
        case 114:  return UV_EALREADY;
        case 125:  return UV_ECANCELED;

        case 200:  return UV_EAI_ADDRFAMILY;
        case 201:  return UV_EAI_AGAIN;
        case 202:  return UV_EAI_BADFLAGS;
        case 203:  return UV_EAI_BADHINTS;
        case 204:  return UV_EAI_CANCELED;
        case 205:  return UV_EAI_FAIL;
        case 206:  return UV_EAI_FAMILY;
        case 207:  return UV_EAI_MEMORY;
        case 208:  return UV_EAI_NODATA;
        case 209:  return UV_EAI_NONAME;
        case 210:  return UV_EAI_OVERFLOW;
        case 211:  return UV_EAI_PROTOCOL;
        case 212:  return UV_EAI_SERVICE;
        case 213:  return UV_EAI_SOCKTYPE;
        case 215:  return UV_ECHARSET;
        case 216:  return UV_ENONET;
        case 217:  return UV_UNKNOWN;
        case 218:  return UV_EOF;
        case 219:  return UV_EUNATCH;
        case 220:  return UV_ESHUTDOWN;

        default:   return UV_UNKNOWN;  // Unknown error
    }
}

#endif
