package effekt
package util

import scala.collection.mutable

/**
 * This is literally a copy of Scala-Effekt, a bit more simplified.
 *  Currently not used! Mostly here in case we want to implement an interpreter again.
 */
package object control {

  class Capability {
    type Res
  }

  type Cap[R] = Capability { type Res = R }

  private[control] type Frame[-A, +B] = A => Control[B]

  trait Control[+A] { outer =>

    // alias for flatMap since it is used the most
    //    def apply[B](f: A => Control[B]): Control[B] = flatMap(f)

    def run(): A = Result.trampoline(Impure(this, ReturnCont(identity[A])))
    def map[B](f: A => B): Control[B] = flatMap { a => pure(f(a)) }
    def andThen[B](f: Control[B]): Control[B] = this flatMap { _ => f }
    def flatMap[B](f: A => Control[B]): Control[B] =
      Computation { k => Impure(outer, k flatMap f) }

    def apply[R](k: MetaCont[A, R]): Result[R]
  }

  def pure[A](v: A): Control[A] = new Trivial(v)

  def sequence[R](ar: List[Control[R]]): Control[List[R]] = ar match {
    case Nil => pure { Nil }
    case (r :: rs) => for {
      rv <- r
      rsv <- sequence(rs)
    } yield rv :: rsv
  }

  private[control] final class Trivial[+A](a: A) extends Control[A] {
    def apply[R](k: MetaCont[A, R]): Result[R] = k(a)

    override def run(): A = a
    override def toString = s"Trivial($a)"
  }

  private[control] sealed trait ω

  private[control] final case class Computation[+A](body: MetaCont[A, ω] => Result[ω]) extends Control[A] {
    def apply[R](k: MetaCont[A, R]): Result[R] =
      body(k.asInstanceOf[MetaCont[A, ω]]).asInstanceOf[Result[R]]
  }

  def use[A](c: Capability)(f: (A => Control[c.Res]) => Control[c.Res]): Control[A] = Computation { k =>
    val (init, tail) = k splitAt c
    val localCont: A => Control[c.Res] = a => Computation[c.Res] { k =>
      val repushedPrompt = init append HandlerCont[c.Res, ω](c, k)
      Impure(pure(a), repushedPrompt)
    }
    Impure(f(localCont), tail)
  }

  final def handle(h: Capability)(c: Control[h.Res]): Control[h.Res] = Computation { k =>
    Impure(c, HandlerCont[h.Res, ω](h, k))
  }

  final def handle[R](f: Cap[R] => Control[R]): Control[R] = {
    val c = new Capability { type Res = R }
    handle(c)(f(c))
  }

  final def lookup[V](key: AnyRef): Control[V] = Computation { k =>
    Impure(pure((k lookup key).asInstanceOf[V]), k)
  }

  // introduces a new scope
  final def bind[R](key: AnyRef, value: Any)(body: Control[R]): Control[R] = Computation { k =>
    Impure(body, k.bind(key, value))
  }

  final def update[R](key: AnyRef, value: Any): Control[Unit] = Computation { k =>
    Impure(pure(k.update(key, value)), k)
  }

  sealed trait Result[+A] { def isPure: Boolean }

  private[control] case class Pure[A](value: A) extends Result[A] { val isPure = true }

  private[control] case class Impure[A, R](c: Control[R], k: MetaCont[R, A]) extends Result[A] {
    val isPure = false
  }

  object Result {
    def trampoline[A](y: Result[A]): A = {
      var res: Result[A] = y

      while (!res.isPure) {
        val imp = res.asInstanceOf[Impure[A, Any]]
        res = imp.c.apply(imp.k)
      }
      res.asInstanceOf[Pure[A]].value
    }
  }

  private[control] sealed trait MetaCont[-A, +B] extends Serializable {
    def apply(a: A): Result[B]

    def append[C](s: MetaCont[B, C]): MetaCont[A, C]

    def splitAt(c: Capability): (MetaCont[A, c.Res], MetaCont[c.Res, B])

    def lookup(key: AnyRef): Any

    def update(key: AnyRef, value: Any): Unit

    def map[C](f: C => A): MetaCont[C, B] = flatMap(f andThen pure)

    def flatMap[C](f: Frame[C, A]): MetaCont[C, B] = FramesCont(List(f.asInstanceOf[Frame[Any, Any]]), this)

    def bind(key: AnyRef, value: Any): MetaCont[A, B] = StateCont(mutable.Map(key -> value), this)
  }

  private[control] case class ReturnCont[-A, +B](f: A => B) extends MetaCont[A, B] {
    final def apply(a: A): Result[B] = Pure(f(a))

    final def append[C](s: MetaCont[B, C]): MetaCont[A, C] = s map f

    final def splitAt(c: Capability) = sys error s"Prompt $c not found on the stack."

    final def lookup(key: AnyRef): Any = sys error s"Key $key not found on the stack."

    final def update(key: AnyRef, value: Any) = sys error s"Key $key not found on the stack."

    override def map[C](g: C => A): MetaCont[C, B] = ReturnCont(g andThen f)

    override def toString = "[]"
  }

  private[control] case class CastCont[-A, +B]() extends MetaCont[A, B] {

    final def apply(a: A): Result[B] = Pure(a.asInstanceOf[B])

    final def append[C](s: MetaCont[B, C]): MetaCont[A, C] = s.asInstanceOf[MetaCont[A, C]]

    final def splitAt(c: Capability) = sys error s"Prompt $c not found on the stack."

    final def lookup(key: AnyRef): Any = sys error s"Key $key not found on the stack."
    final def update(key: AnyRef, value: Any) = sys error s"Key $key not found on the stack."

    override def map[C](g: C => A): MetaCont[C, B] = ReturnCont(x => g(x).asInstanceOf[B])

    override def toString = "{}"
  }

  private[control] case class FramesCont[-A, B, +C](frames: List[Frame[Any, Any]], tail: MetaCont[B, C]) extends MetaCont[A, C] {

    final def apply(a: A): Result[C] = {
      val first :: rest = frames : @unchecked
      val result = first.asInstanceOf[Frame[A, B]](a)
      if (rest.isEmpty) {
        Impure(result, tail)
      } else {
        Impure(result, FramesCont(rest, tail))
      }
    }

    final def append[D](s: MetaCont[C, D]): MetaCont[A, D] = FramesCont(frames, tail append s)
    final def splitAt(c: Capability) = tail.splitAt(c) match {
      case (head, tail) => (FramesCont(frames, head), tail)
    }
    final def lookup(key: AnyRef): Any = tail lookup key
    final def update(key: AnyRef, value: Any) = tail.update(key, value)
    override def flatMap[D](f: Frame[D, A]): MetaCont[D, C] = FramesCont(f.asInstanceOf[Frame[Any, Any]] :: frames, tail)
    override def toString = s"fs(${frames.size}) :: ${tail}"
  }

  // dynamically allocated state
  // TODO change to mutable state, so that setting does not require the continuation
  //   otherwise the object lang. programs have to be written in CPS
  private[control] case class StateCont[-A, +B](bindings: mutable.Map[AnyRef, Any], tail: MetaCont[A, B]) extends MetaCont[A, B] {
    final def apply(a: A): Result[B] = tail(a)
    final def append[C](s: MetaCont[B, C]): MetaCont[A, C] = StateCont(bindings.clone(), tail append s)
    final def splitAt(c: Capability) = tail.splitAt(c) match {
      case (head, tail) => (StateCont(bindings, head), tail)
    }
    final def lookup(searched: AnyRef): Any =
      bindings.getOrElse(searched, tail lookup searched)

    final def update(key: AnyRef, value: Any) =
      if (bindings.isDefinedAt(key)) {
        bindings.update(key, value)
      } else {
        tail.update(key, value)
      }

    // Also: Stateconts can be commuted if they don't shadow each other
    final override def bind(key: AnyRef, value: Any): MetaCont[A, B] =
      StateCont(bindings concat Map(key -> value), tail)
  }

  private[effekt] case class HandlerCont[R, A](h: Capability { type Res = R }, tail: MetaCont[R, A]) extends MetaCont[R, A] {
    final def apply(r: R): Result[A] = tail(r)

    final def append[C](s: MetaCont[A, C]): MetaCont[R, C] = HandlerCont(h, tail append s)

    final def splitAt(c: Capability) =

      if (h eq c) {
        val head = CastCont[R, c.Res]()
        val rest = tail.asInstanceOf[MetaCont[c.Res, A]]
        (head, rest)

      } else tail.splitAt(c) match {
        case (head, tail) => (HandlerCont(h, head), tail)
      }

    final def lookup(key: AnyRef): Any = tail lookup key
    final def update(key: AnyRef, value: Any) = tail.update(key, value)

    override def toString = s"${h} :: ${tail}"
  }
}
