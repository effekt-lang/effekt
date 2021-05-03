package effekt.modules

import effekt.context.Context
import effekt.source.Id
import effekt.source.IdDef
import effekt.source.IdRef

/**
 * Regulare Name
 *  note: [[toString()]] returns qualified user name.
 */
sealed trait Name {
  /* Predicates *
   * ---------- */

  /** check if name is `_`. */
  def isEmpty: Boolean = this == Name.Blk

  /** check if name is `*`. */
  def isEvery: Boolean = this == Name.All

  /* Properties *
   * ---------- */

  /** left-most name component. */
  def frst: Name = this

  /** right-most name component. */
  def last: Name = this

  /** convert to qualified path [[String]]. */
  def unix: String = qual(Name.pthSep)

  /** convert to qualified user [[String]]. */
  def full: String = qual(Name.usrSep)

  /** (short) local name. */
  def local: String = last.toString()

  /* Functions *
   * --------- */

  /** flattend name components. */
  def cmps(): List[String] = List(this.toString())

  /** append name. */
  def nest(nm: Name): Name = Name(this, nm)

  /** compute qualified [[String]] using separator. */
  def qual(sep: String): String = this.toString()

  /** removes left-most name component. */
  def dropFirst(): Name = Name.Blk

  /** removes right-most name component. */
  def dropLast(): Name = Name.Blk

  /* Higher-Order *
   * ------------ */

  /** map name components. */
  def rename(f: String => String): Name = this

  /* Utility *
   * ------- */

  /** converts name to [[IdDef]] */
  def toDef() = IdDef(this.toString())

  /** converts name to [[IdRef]] */
  def toRef() = IdRef(this.toString())
}

/** Pseudo-Class */
object Name {
  val main = Word("main")

  /** user name separator */
  val usrSep = "."

  /** path name separator*/
  val pthSep = "/"

  /** empty */
  def apply(): Name = Name.Blk

  /** word */
  def apply(str: String): Name = {
    if (str.isEmpty()) {
      return Blk
    } else if (str == All.toString()) {
      return All
    } else {
      return Word(str)
    }
  }

  /** link */
  def apply(ln: Name, rn: Name): Name = {
    if (ln == Blk || rn == All) {
      return rn
    } else if (ln == All || rn == Blk) {
      return ln
    } else {
      return Link(ln, rn)
    }
  }

  /** components */
  def apply(as: List[String]): Name = as.foldLeft(Name()) { (nme, str) =>
    Name(nme, Name(str))
  }

  /** components */
  def join(as: List[Word]): Name = as.foldLeft(Name()) { (l, r) =>
    Name(l, r)
  }

  /** components */
  def apply(as: Array[String]): Name = as.foldLeft(Name()) { (nme, str) =>
    Name(nme, Name(str))
  }

  def apply(id: Id)(implicit C: Context): Name = C.module.name.nest(Name(id.name))

  /** parse [[Name]] from qualified [[String]].*/
  def qual(qal: String, sep: String) = Name(qal.split(sep))

  /** parse [[Name]] from path [[String]].*/
  def path(str: String) = Name.qual(str, Name.pthSep)

  /** parse [[Name]] from user [[String]].*/
  def user(str: String) = Name.qual(str, Name.usrSep)

  /** Empty */
  object Blk extends Name {
    override def toString() = "_"
  }

  /** Single Word */
  case class Word(str: String) extends Name {
    override def toString() = str
    override def rename(f: String => String): Name = Word(f(str))
  }

  /** Nested Name */
  case class Link(lft: Name, rgt: Name) extends Name {
    override def frst: Name = lft.frst
    override def last: Name = rgt.last

    override def dropFirst(): Name = lft match {
      case l: Link => l.dropFirst()
      case _       => rgt
    }

    override def dropLast(): Name = rgt match {
      case r: Link => r.dropLast()
      case _       => lft
    }

    override def cmps(): List[String] = lft.cmps() ++ rgt.cmps()
    override def qual(sep: String): String = lft.qual(sep) + sep + rgt.qual(sep)

    override def toString() = lft.toString() + Name.usrSep + rgt.toString()
    override def rename(f: String => String): Name = Link(lft.rename(f), rgt.rename(f))
  }

  /** Everyone */
  object All extends Name {
    override def toString() = "*"
  }
}
