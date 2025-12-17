package effekt.util

object UByte:
  // Opaque type: erased to Byte at runtime (zero-cost wrapper).
  opaque type UByte = Byte

  inline def fromInt(i: Int): Option[UByte] =
    if 0 <= i && i <= 0xFF then Some(i.toByte) else None

  /** Unsafe: construct from an Int without checking */
  inline def unsafeFromInt(i: Int): UByte = i.toByte

  /** Unsafe: wraps a Byte (every Byte is a valid UByte because it encodes 0..255). */
  inline def unsafeFromByte(b: Byte): UByte = b

  /** Compile-time checked literal constructor:
   *  UByte.lit(255)  // ok
   *  UByte.lit(300)  // compile error
   *
   *  Uses inline + compiletime error so literal (constant) calls are checked at compile-time.
   */
  inline def lit(inline i: Int): UByte =
    inline if 0 <= i && i <= 0xFF then i.toByte
    else scala.compiletime.error(s"UByte literal out of range: $i (expected 0..255)")

  extension (b: UByte)
    /** Raw underlying byte (signed as stored). */
    inline def toByte: Byte = b

    inline def toInt: Int = java.lang.Byte.toUnsignedInt(b)

    inline def toHexString: String = f"0x${java.lang.Byte.toUnsignedInt(b)}%02X"

export UByte.*

given Ordering[UByte] with
  def compare(x: UByte, y: UByte): Int = (x.toInt) - (y.toInt)
