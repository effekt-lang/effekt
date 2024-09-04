
/**
 * Maps the error code to a Effekt-stable (platform independent) numeric value.
 *
 * Tries to use most common errno integer values, but introduces fresh values (> 200)
 * for those without common errno values.
 */
function errnoToStableId(errorName) {
    const errorMap = {
        EPERM: 1,
        ENOENT: 2,
        ESRCH: 3,
        EINTR: 4,
        EIO: 5,
        ENXIO: 6,
        E2BIG: 7,
        EBADF: 9,
        EAGAIN: 11,
        ENOMEM: 12,
        EACCES: 13,
        EFAULT: 14,
        EBUSY: 16,
        EEXIST: 17,
        EXDEV: 18,
        ENODEV: 19,
        ENOTDIR: 20,
        EISDIR: 21,
        EINVAL: 22,
        ENFILE: 23,
        EMFILE: 24,
        ENOTTY: 25,
        ETXTBSY: 26,
        EFBIG: 27,
        ENOSPC: 28,
        ESPIPE: 29,
        EROFS: 30,
        EMLINK: 31,
        EPIPE: 32,
        ERANGE: 34,
        ENAMETOOLONG: 36,
        ELOOP: 40,
        EOVERFLOW: 75,
        EFTYPE: 79,
        EILSEQ: 84,
        ENOTSOCK: 88,
        EDESTADDRREQ: 89,
        EMSGSIZE: 90,
        EPROTOTYPE: 91,
        ENOPROTOOPT: 92,
        EPROTONOSUPPORT: 93,
        ESOCKTNOSUPPORT: 94,
        ENOTSUP: 95,
        EAFNOSUPPORT: 97,
        EADDRINUSE: 98,
        EADDRNOTAVAIL: 99,
        ENETDOWN: 100,
        ENETUNREACH: 101,
        ECONNABORTED: 103,
        ECONNRESET: 104,
        ENOBUFS: 105,
        EISCONN: 106,
        ENOTCONN: 107,
        ETIMEDOUT: 110,
        ECONNREFUSED: 111,
        EHOSTUNREACH: 113,
        EALREADY: 114,
        ECANCELED: 125,
        EAI_ADDRFAMILY: 200,
        EAI_AGAIN: 201,
        EAI_BADFLAGS: 202,
        EAI_BADHINTS: 203,
        EAI_CANCELED: 204,
        EAI_FAIL: 205,
        EAI_FAMILY: 206,
        EAI_MEMORY: 207,
        EAI_NODATA: 208,
        EAI_NONAME: 209,
        EAI_OVERFLOW: 210,
        EAI_PROTOCOL: 211,
        EAI_SERVICE: 212,
        EAI_SOCKTYPE: 213,
        ECHARSET: 215,
        ENONET: 216,
        UNKNOWN: 217,
        EOF: 218,
        EUNATCH: 219,
        ESHUTDOWN: 220
    };

    return errorMap[errorName] || -1; // Default to -1 for unknown error names
}

function open(path, mode, callback) {
  fs.open(path, mode, (err, fd) => {
    if (err) { callback(err.errno) } else { callback(fd) }
  })
}

function read(fd, buffer, offset, callback) {
  let position = offset === -1 ? null : offset;
  fs.read(fd, toBuffer(buffer), 0, buffer.length, position, (err, bytesRead) => {
    if (err) { callback(err.errno) } else { callback(bytesRead) }
  })
}

function write(fd, buffer, offset, callback) {
  let position = offset === -1 ? null : offset;
  fs.write(fd, toBuffer(buffer), 0, buffer.length, position, (err, bytesWritten) => {
    if (err) { callback(err.errno) } else { callback(bytesWritten) }
  })
}

function close(fd, callback) {
  fs.close(fd, (err) => {
    if (err) { callback(err.errno) } else { callback(0) }
  })
}


/**
 * Nodejs file operations expect buffers, but we represent buffers as typed arrays.
 * This function converts between the two without copying.
 */
function toBuffer(buffer) {
  return Buffer.from(buffer.buffer, buffer.byteOffset, buffer.byteLength)
}
