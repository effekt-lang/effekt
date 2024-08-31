package effekt

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MLTests extends AnyFlatSpec with Matchers {

  "MLton backend" should "correctly escape characters like '✓' and '✕' to byte-by-byte format" in {
    val transformer = new effekt.generator.ml.Transformer()
    val inputString = "✓✕"
    val escapedString = transformer.escape(inputString)
    escapedString should be("\\342\\234\\223\\342\\234\\225")
  }

  it should "correctly escape characters outside the ASCII range to byte-by-byte format" in {
    val transformer = new effekt.generator.ml.Transformer()
    val inputString = "Hello, 世界"
    val escapedString = transformer.escape(inputString)
    escapedString should be("Hello, \\344\\270\\255\\345\\245\\275")
  }

  it should "correctly escape control characters to byte-by-byte format" in {
    val transformer = new effekt.generator.ml.Transformer()
    val inputString = "Hello,\nWorld"
    val escapedString = transformer.escape(inputString)
    escapedString should be("Hello,\\012World")
  }

  it should "not escape characters within the ASCII range" in {
    val transformer = new effekt.generator.ml.Transformer()
    val inputString = "Hello, World!"
    val escapedString = transformer.escape(inputString)
    escapedString should be("Hello, World!")
  }
}
