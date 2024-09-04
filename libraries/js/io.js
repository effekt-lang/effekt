
const os = require('os');

/**
 * Maps the error code to a Effekt-stable (platform independent) numeric value.
 *
 * Tries to use most common errno integer values, but introduces fresh values (> 200)
 * for those without common errno values.
 */
function errorNumber(errno) {
    const errnoMap = {
        [os.constants.errno.EPERM]: 1,
        [os.constants.errno.ENOENT]: 2,
        [os.constants.errno.ESRCH]: 3,
        [os.constants.errno.EINTR]: 4,
        [os.constants.errno.EIO]: 5,
        [os.constants.errno.ENXIO]: 6,
        [os.constants.errno.E2BIG]: 7,
        [os.constants.errno.EBADF]: 9,
        [os.constants.errno.EAGAIN]: 11,
        [os.constants.errno.ENOMEM]: 12,
        [os.constants.errno.EACCES]: 13,
        [os.constants.errno.EFAULT]: 14,
        [os.constants.errno.EBUSY]: 16,
        [os.constants.errno.EEXIST]: 17,
        [os.constants.errno.EXDEV]: 18,
        [os.constants.errno.ENODEV]: 19,
        [os.constants.errno.ENOTDIR]: 20,
        [os.constants.errno.EISDIR]: 21,
        [os.constants.errno.EINVAL]: 22,
        [os.constants.errno.ENFILE]: 23,
        [os.constants.errno.EMFILE]: 24,
        [os.constants.errno.ENOTTY]: 25,
        [os.constants.errno.ETXTBSY]: 26,
        [os.constants.errno.EFBIG]: 27,
        [os.constants.errno.ENOSPC]: 28,
        [os.constants.errno.ESPIPE]: 29,
        [os.constants.errno.EROFS]: 30,
        [os.constants.errno.EMLINK]: 31,
        [os.constants.errno.EPIPE]: 32,
        [os.constants.errno.ERANGE]: 34,
        [os.constants.errno.ENAMETOOLONG]: 36,
        [os.constants.errno.ELOOP]: 40,
        [os.constants.errno.EOVERFLOW]: 75,
        [os.constants.errno.EFTYPE]: 79,
        [os.constants.errno.EILSEQ]: 84,
        [os.constants.errno.ENOTSOCK]: 88,
        [os.constants.errno.EDESTADDRREQ]: 89,
        [os.constants.errno.EMSGSIZE]: 90,
        [os.constants.errno.EPROTOTYPE]: 91,
        [os.constants.errno.ENOPROTOOPT]: 92,
        [os.constants.errno.EPROTONOSUPPORT]: 93,
        [os.constants.errno.ESOCKTNOSUPPORT]: 94,
        [os.constants.errno.ENOTSUP]: 95,
        [os.constants.errno.EAFNOSUPPORT]: 97,
        [os.constants.errno.EADDRINUSE]: 98,
        [os.constants.errno.EADDRNOTAVAIL]: 99,
        [os.constants.errno.ENETDOWN]: 100,
        [os.constants.errno.ENETUNREACH]: 101,
        [os.constants.errno.ECONNABORTED]: 103,
        [os.constants.errno.ECONNRESET]: 104,
        [os.constants.errno.ENOBUFS]: 105,
        [os.constants.errno.EISCONN]: 106,
        [os.constants.errno.ENOTCONN]: 107,
        [os.constants.errno.ETIMEDOUT]: 110,
        [os.constants.errno.ECONNREFUSED]: 111,
        [os.constants.errno.EHOSTUNREACH]: 113,
        [os.constants.errno.EALREADY]: 114,
        [os.constants.errno.ECANCELED]: 125,
        [os.constants.errno.EAI_ADDRFAMILY]: 200,
        [os.constants.errno.EAI_AGAIN]: 201,
        [os.constants.errno.EAI_BADFLAGS]: 202,
        [os.constants.errno.EAI_BADHINTS]: 203,
        [os.constants.errno.EAI_CANCELED]: 204,
        [os.constants.errno.EAI_FAIL]: 205,
        [os.constants.errno.EAI_FAMILY]: 206,
        [os.constants.errno.EAI_MEMORY]: 207,
        [os.constants.errno.EAI_NODATA]: 208,
        [os.constants.errno.EAI_NONAME]: 209,
        [os.constants.errno.EAI_OVERFLOW]: 210,
        [os.constants.errno.EAI_PROTOCOL]: 211,
        [os.constants.errno.EAI_SERVICE]: 212,
        [os.constants.errno.EAI_SOCKTYPE]: 213,
        [os.constants.errno.ECHARSET]: 215,
        [os.constants.errno.ENONET]: 216,
        [os.constants.errno.UNKNOWN]: 217,
        [os.constants.errno.EOF]: 218,
        [os.constants.errno.EUNATCH]: 219,
        [os.constants.errno.ESHUTDOWN]: 220
    };
    return errnoMap[-errno] || -1; // Default to -1 for unknown error names
}

function modeName(mode) {
  switch (mode.__tag) {
  case 0: // ReadOnly()
    return 'r';
  case 1: // WriteOnly()
    return 'w';
  case 2: // AppendOnly()
    return 'a';
  case 3: // ReadWrite()
    return 'w+';
  case 4: // ReadAppend()
    return 'a+';
  case 5: // AppendExclusive()
    return 'ax';
  case 6: // ReadAppendExclusive()
    return 'ax+';
  case 7: // AppendSync()
    return 'as';
  case 8: // ReadAppendSync()
    return 'as+';
  case 9: // ReadSync()
    return 'rs';
  case 10: // ReadWriteSync()
    return 'rs+';
  case 11: // WriteExclusive()
    return 'wx';
  case 12: // ReadWriteExclusive()
    return 'wx+';
  default:
    // Invalid tag value
    return null;
  }
}

function open(path, mode, callback) {
  fs.open(path, modeName(mode), (err, fd) => {
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
