//package effekt.util
//
//// https://github.com/Mesabloo/diagnose
//// https://github.com/quchen/prettyprinter
//
///**
// * The datatype representation of a document to render
// *
// * [[A]] is the type of annotations that can be used to provide additional markup
// */
//sealed trait Document[+A]
//case object Empty extends Document[Nothing]
//case object Line extends Document[Nothing]
//case class Text(content: String) extends Document[Nothing]
//case class Column[A](body: Int => Document[A]) extends Document[A]
//case class Nest[A](indent: Int, body: Document[A]) extends Document[A]
//case class Nesting[A](body: Int => Document[A]) extends Document[A]
//case class Group[A](body: Document[A]) extends Document[A]
//case class Concat[A](first: Document[A], second: Document[A]) extends Document[A]
//case class Annotated[A](annotation: A, body: Document[A]) extends Document[A]
//
//// https://github.com/quchen/prettyprinter/blob/master/prettyprinter/src/Prettyprinter/Internal.hs
//// also has WithPageWidth, Union, and FlatAlt
//// it does not have Group
//
//trait Style[From, To] {
//  def apply(
//    from: PrettyPrinter { type Annotation = From },
//    to: PrettyPrinter { type Annotation = To }
//  ): from.Doc => to.Doc
//}
//
//trait PrettyPrinter {
//
//  val defaultIndent = 2
//
//  type Doc
//  type Annotation
//
//  // we try to be a drop in replacement for kiama (for now)
//  def emptyDoc: Doc
//  def line: Doc
//  implicit def text(content: String): Doc
//  def column(f: Int => Doc): Doc
//  def nest(d: Doc, j: Int = defaultIndent): Doc
//  def nesting(f: Int => Doc): Doc
//  def group(doc: Doc): Doc
//  def concat(first: Doc, second: Doc): Doc
//
//  def annotated(ann: Annotation, doc: Doc): Doc
//
//  def hcat(ds: Seq[Doc]): Doc = folddoc(ds, _ <> _)
//
//  /**
//   * Return a document that is the result of folding `f` over the sequence
//   * `ds`. Returns the empty document is `ds` is empty.
//   */
//  def folddoc(ds: Seq[Doc], f: (Doc, Doc) => Doc) =
//    if (ds.isEmpty) emptyDoc else ds.tail.foldLeft(ds.head)(f)
//
//  implicit def char(c: Char): Doc =
//    if (c == '\n') line else text(c.toString)
//
//  /**
//   * Return a document that behaves like `space` if the resulting output
//   * fits the page, otherwise it behaves like `line`.
//   */
//  def softline: Doc =
//    group(line)
//
//  /**
//   * A hard line break that will always be rendered as a newline, regardless of the
//   * available space.
//   */
//  def hardline: Doc
//
//  /**
//   * Return a document representing `n` spaces if `n` s greater than zero,
//   * otherwise return an empty document.
//   */
//  def spaces(n: Int): Doc =
//    if (n <= 0)
//      emptyDoc
//    else
//      text(" " * n)
//
//  extension(d: Doc) {
//    def <>(other: Doc): Doc = concat(d, other)
//  }
//
//}
//
