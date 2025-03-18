package kiama
package util


class RopeTests extends munit.FunSuite {

  test("empty rope") {
    val rope = Rope("")
    assert(rope.length == 0)
    assert(rope.toString == "")
    assert(rope.iterator.toList.isEmpty)
  }

  test("single character operations") {
    val rope = Rope("hello")
    assert(rope.length == 5)
    assert(rope.charAt(0) == 'h')
    assert(rope.charAt(4) == 'o')

    intercept[IllegalArgumentException] {
      rope.charAt(-1)
    }
    intercept[IllegalArgumentException] {
      rope.charAt(5)
    }
  }

  test("concatenation") {
    val rope1 = Rope("Hello")
    val rope2 = Rope(" World")
    val combined = rope1.concat(rope2)
    assert(combined.toString == "Hello World")

    // test empty concatenations
    assert(rope1.concat(Rope("")).toString == "Hello")
    assert(Rope("").concat(rope2).toString == " World")
    assert(Rope("").concat(Rope("")).toString == "")
  }

  test("substring") {
    val rope = Rope("Hello World")

    // full string
    assert(rope.substring(0, 11).toString == "Hello World")

    // partial string
    assert(rope.substring(0, 5).toString == "Hello")
    assert(rope.substring(6, 11).toString == "World")

    // empty substring
    assert(rope.substring(0, 0).toString == "")
    assert(rope.substring(5, 5).toString == "")

    // single character
    assert(rope.substring(0, 1).toString == "H")

    intercept[IllegalArgumentException] {
      rope.substring(-1, 5)
    }
    intercept[IllegalArgumentException] {
      rope.substring(5, 12)
    }
    intercept[IllegalArgumentException] {
      rope.substring(6, 5)
    }
  }

  test("delete") {
    val rope = Rope("Hello World")

    // delete from middle
    assert(rope.delete(5, 6).toString == "HelloWorld")

    // delete multiple characters
    assert(rope.delete(5, 11).toString == "Hello")

    // delete from start
    assert(rope.delete(0, 6).toString == "World")

    // delete nothing
    assert(rope.delete(5, 5).toString == "Hello World")

    // delete everything
    assert(rope.delete(0, 11).toString == "")

    intercept[IllegalArgumentException] {
      rope.delete(-1, 5)
    }
    intercept[IllegalArgumentException] {
      rope.delete(5, 12)
    }
    intercept[IllegalArgumentException] {
      rope.delete(6, 5)
    }
  }

  test("insert") {
    val rope = Rope("Hello World")

    // insert at start
    assert(rope.insert(0, "Start: ").toString == "Start: Hello World")

    // insert in middle
    assert(rope.insert(5, " there").toString == "Hello there World")

    // insert at end
    assert(rope.insert(11, "!").toString == "Hello World!")

    // insert empty string
    assert(rope.insert(5, "").toString == "Hello World")

    // insert with multiple characters
    assert(rope.insert(6, "beautiful ").toString == "Hello beautiful World")

    intercept[IllegalArgumentException] {
      rope.insert(-1, "test")
    }
    intercept[IllegalArgumentException] {
      rope.insert(12, "test")
    }
  }

  test("compound operations") {
    var rope = Rope("Hello World")

    // insert then delete
    rope = rope.insert(5, " beautiful")
    assert(rope.toString == "Hello beautiful World")
    rope = rope.delete(5, 15)
    assert(rope.toString == "Hello World")

    // substring then insert
    rope = rope.substring(0, 5)
    assert(rope.toString == "Hello")
    rope = rope.insert(5, " there")
    assert(rope.toString == "Hello there")

    // multiple operations
    rope = Rope("base")
        .insert(0, "pre")
        .insert(7, "post")
        .delete(3, 7)
        .insert(3, "middle")
    assert(rope.toString == "premiddlepost")
  }

  test("long string handling") {
    val longStr = "a" * 1000
    val rope = Rope(longStr)
    assert(rope.length == 1000)
    assert(rope.toString == longStr)

    // test splitting into chunks
    assert(rope.charAt(500) == 'a')

    // substring on long string
    val sub = rope.substring(100, 200)
    assert(sub.length == 100)
    assert(sub.toString == "a" * 100)
  }

  test("iterator") {
    val rope = Rope("Hello")
    val chars = rope.iterator.toList
    assert(chars == List('H', 'e', 'l', 'l', 'o'))

    // empty rope iterator
    assert(Rope("").iterator.toList.isEmpty)

    // iterator after operations
    val modified = rope.insert(5, " World")
    assert(modified.iterator.toList == "Hello World".toList)
  }

  test("edge cases") {
    // very short strings
    assert(Rope("a").toString == "a")
    assert(Rope("a").length == 1)

    // strings around chunk threshold
    val threshold = Rope.LeafThreshold
    val nearThreshold = "a" * (threshold - 1)
    val atThreshold = "a" * threshold
    val overThreshold = "a" * (threshold + 1)

    assert(Rope(nearThreshold).toString == nearThreshold)
    assert(Rope(atThreshold).toString == atThreshold)
    assert(Rope(overThreshold).toString == overThreshold)
  }
}
