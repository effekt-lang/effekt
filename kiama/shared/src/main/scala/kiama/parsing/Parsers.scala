/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2015-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package kiama
package parsing

import kiama.util.Positions

/**
 * Simple packrat parsing combinator suite. These combinators are
 * largely source compatible with the Scala parser combinator library
 * but the implementation is simpler and less general. Broadly speaking
 * this library provides behaviour similar to the Scala library
 * `RegexParsers` trait with sensible defaults and better integration
 * with the rest of Kiama.
 *
 * The parameter `positions` provides the position store that this
 * suite should use to track AST node positions. Usually the value
 * passed in is shared with the rest of a program that reports
 * errors etc.
 *
 * This library should not be used if efficiency is a concern. These
 * combinators are essentially interpreting the grammar so the performance
 * can't compare to a parser generator which compiles the grammar.
 * Nevertheless, the library is perfectly fine for prototyping and for
 * processing small to medium-sized inputs.
 *
 * Some of the implementation details in this module are based on the
 * implementation of the Scala parser combinator library but in a much
 * simpler form.
 *
 * The algorithms used here to handle left recursion are from "Packrat
 * parsers can support left recursion" by Warth, Douglass and Millstein,
 * ACM SIGPLAN Symposium on Partial Evaluation and Semantics-based Program
 * Manipulation, 2008.
 */
class ParsersBase(positions: Positions) {

  import kiama.util.{ Source, StringSource }
  import scala.annotation.tailrec
  import scala.collection.immutable.Set
  import scala.collection.mutable.{ Builder, HashMap }
  import scala.language.implicitConversions
  import scala.util.DynamicVariable
  import scala.util.matching.Regex

  /**
   * Record lack of success so that we can nicely handle the case where a phrase
   * doesn't parse when looking for the end of input but there was a later lack
   * of success for some other reason.
   */
  lazy val latestNoSuccess = new DynamicVariable[Option[NoSuccess]](None)

  /**
   * Convenience method for making a parser out of its body function,
   * including adding support for whitespace prefix skipping and position
   * recording. All parsers should be created using this method so that they
   * share the book-keeping.
   */
  def Parser[T](f: Input => ParseResult[T]): Parser[T] =
    new Parser[T] {
      def apply(in: Input): ParseResult[T] =
        parseWhitespace(in) match {
          case Success(_, start) =>
            f(start) match {
              case res @ Success(t, finish) =>
                positions.setStart(t, start.position)
                positions.setFinish(t, finish.position)
                res
              // Can't merge these into a NoSuccess case since type is ParseResult[T]
              case res @ Error(message, next) =>
                updateLatestNoSuccess(res)
              case res @ Failure(message, next) =>
                updateLatestNoSuccess(res)
            }
          case result: NoSuccess =>
            result
        }
    }

  def updateLatestNoSuccess[T](res: NoSuccess): ParseResult[T] = {
    if ((res.message != "") &&
      (latestNoSuccess.value.forall(_.next.offset <= res.next.offset)))
      latestNoSuccess.value = Some(res)
    res
  }

  // Memoising parsers, support left recursion.

  /**
   * Information about an active instance of left recursion.
   */
  case class Head(rule: Rule, var involvedSet: Set[Rule], var evalSet: Set[Rule])

  /**
   * Map between left input positions and active left recursion instances.
   */
  var heads = new HashMap[Input, Head]

  /**
   * Parsing answers.
   */
  sealed abstract class Answer[T]

  /**
   * An answer that is a resolved parser result.
   */
  case class Resolution[T](result: ParseResult[T]) extends Answer[T]

  /**
   * An answer that is a left recursion record.
   */
  case class LR[T](var seed: ParseResult[T], rule: Rule, var head: Head, next: LR[T]) extends Answer[T]

  /**
   * Common supertype for all rules (ie regardless of result type).
   */
  trait Rule

  /**
   * A parser that is a memoising, left recursion-detecting encapsulation
   * of a normal parser that returns a value of a particular type. This
   * type of parser must be used if any of the alternatives are left
   * recursive. Otherwise a plain `Parser` can be used if memoisation is
   * not desired.
   *
   * Note that it is non-trivial (impossible?) to combine this class with
   * `Parser`. We need the latter to be covariant but this class can't be
   * because `T` occurs in an invariant position in `MemoEntry`.
   */
  class PackratParser[T](body: => Parser[T]) extends Parser[T] with Rule {

    p =>

    /**
     * Memo table entries.
     */
    case class MemoEntry(var ans: Answer[T], var in: Input)

    /**
     * The section of the memo table relating to this rule.
     */
    val memo = new HashMap[Input, MemoEntry]

    /**
     * Left recursion stack.
     */
    var LRStack: LR[T] = null

    /**
     * Apply this rule to the given input, memoising the result.
     */
    def apply(in: Input): ParseResult[T] = {
      recall(in) match {
        case None =>
          val lr = LR[T](Failure("", in), this, null, LRStack)
          LRStack = lr
          val m = MemoEntry(lr, in)
          memo += (in -> m)
          val ans = body(in)
          LRStack = LRStack.next
          m.in = ans.next
          if (lr.head == null) {
            m.ans = Resolution(ans)
            ans
          } else {
            lr.seed = ans
            lranswer(in, m)
          }
        case Some(MemoEntry(Resolution(r), _)) =>
          r
        case Some(MemoEntry(lr @ LR(_, _, _, _), _)) =>
          setuplr(lr)
          lr.seed
      }

    }

    /**
     * Initialise the left recursion data for a new application of this rule.
     */
    def setuplr(l: LR[T]): Unit = {
      if (l.head == null)
        l.head = Head(p, Set(), Set())
      var s = LRStack
      while (s.head != l.head) {
        s.head = l.head
        l.head.involvedSet = l.head.involvedSet + s.rule
        s = s.next
      }
    }

    /**
     * Process a given left recursion instance.
     */
    def lranswer(in: Input, m: MemoEntry): ParseResult[T] = {
      m.ans match {
        case lr @ LR(_, _, _, _) =>
          val h = lr.head
          if (h.rule == p) {
            m.ans = Resolution(lr.seed)
            m.ans match {
              case Resolution(f @ Failure(_, _)) =>
                f
              case _ =>
                growlr(in, m, h)
            }
          } else
            lr.seed
        case _ =>
          sys.error("lranswer: unexpected non-LR answer")
      }
    }

    /**
     * Look up the memoised result for this rule, taking into account that
     * it might be participating in an active left recursion.
     */
    def recall(in: Input): Option[MemoEntry] = {
      val om = memo.get(in)
      heads.get(in) match {
        case None => om
        case Some(h) =>
          if ((om == None) && !((h.involvedSet + h.rule) contains p))
            Some(MemoEntry(Resolution(Failure("", in)), in))
          else if (h.evalSet contains this) {
            h.evalSet = h.evalSet - p
            val ans = body(in)
            memo += (in -> MemoEntry(Resolution(ans), ans.next))
            memo.get(in)
          } else
            om
      }
    }

    /**
     * Grow the current parse result according to a left recursion.
     */
    def growlr(in: Input, m: MemoEntry, h: Head): ParseResult[T] = {
      heads += (in -> h)
      while (true) {
        h.evalSet = h.involvedSet.toSet
        val ans = body(in)
        if (ans.isInstanceOf[Failure] || ans.next.offset <= m.in.offset) {
          heads -= in
          m.ans match {
            case Resolution(r) =>
              return r
            case _ =>
              sys.error("growlr: unexpected non-result answer")
          }
        }
        m.ans = Resolution(ans)
        m.in = ans.next
      }
      sys.error("growlr: went where I shouldn't go")
    }

  }

  /**
   * (Implicit) conversion of non-memoising parser into a memoising one.
   */
  implicit def memo[T](parser: => Parser[T]): PackratParser[T] =
    new PackratParser[T](parser)

  // Non-memoising parsers, don't support left recursion.

  /**
   * Special tuple class to match sequence combinator.
   */
  case class ~[+T, +U](_1: T, _2: U) {
    override def toString = s"(${_1}~${_2})"
  }

  /**
   * A parser is a function from a string to a parser result. This kind
   * of parser cannot handle left recursive alternatives and does not
   * memoise its results so it may repeat work. If those properties are
   * desired use the `PackratParser` type instead.
   */
  abstract class Parser[+T] extends (Input => ParseResult[T]) {

    p =>

    /**
     * Alternative entry point to directly parse a string.
     */
    def apply(str: String): ParseResult[T] =
      apply(Input(StringSource(str), 0))

    // Functional operators

    def append[U >: T](q: => Parser[U]): Parser[U] =
      Parser { in => p(in).append(q(in)) }

    def flatMap[U](f: T => Parser[U]): Parser[U] =
      Parser { in => p(in).flatMapWithNext(f) }

    def map[U](f: T => U): Parser[U] =
      Parser { in => p(in).map(f) }

    // Combinators

    /**
     * Sequential composition.
     */
    def ~[U](q: => Parser[U]): Parser[~[T, U]] = {
      lazy val qq = q
      for (t <- p; u <- qq)
        yield new ~(t, u)
    }

    /**
     * Sequential composition with no back-tracking back past this point.
     * Modelled on same operator in FastParse and `~!` in Scala parser
     * combinators.
     */
    def ~/[U](q: => Parser[U]): Parser[~[T, U]] = {
      lazy val qq = q
      for (t <- p; u <- commit(qq))
        yield new ~(t, u)
    }

    /**
     * Sequential composition ignoring left side.
     */
    def ~>[U](q: => Parser[U]): Parser[U] = {
      lazy val qq = q
      for (t <- p; u <- qq)
        yield u
    }

    /**
     * Sequential composition with no back-tracking back past this point
     * and ignoring left side.
     */
    def ~/>[U](q: => Parser[U]): Parser[U] =
      p ~> commit(q)

    /**
     * Sequential composition ignoring right side.
     */
    def <~[U](q: => Parser[U]): Parser[T] = {
      lazy val qq = q
      for (t <- p; u <- qq)
        yield t
    }

    /**
     * Sequential composition with no back-tracking back past this point
     * and ignoring right side.
     */
    def <~/[U](q: => Parser[U]): Parser[T] =
      p <~ commit(q)

    /**
     * Alternation.
     */
    def |[U >: T](q: => Parser[U]): Parser[U] =
      append(q)

    /**
     * Apply function to successful result.
     */
    def ^^[U](f: T => U): Parser[U] =
      map(f)

    /**
     * Turn a successful result into a specific value which is evaluated each
     * each time this parser is used.
     */
    def ^^^[U](u: => U): Parser[U] =
      Parser {
        in =>
          this(in).map(_ => u)
      }

    // Aliases

    // /**
    //  * Repetition zero or more times.
    //  */
    // def * : Parser[CC[T]] =
    //     rep (p)

    // /**
    //  * Repetition one or more times.
    //  */
    // def + : Parser[CC[T]] =
    //     rep1 (p)

    /**
     * Optional parsing.
     */
    def ? : Parser[Option[T]] =
      opt(p)

    /**
     * Parameterise next parse step by result from previous one.
     */
    def into[U](fq: T => Parser[U]): Parser[U] =
      flatMap(fq)

  }

  // Running parsers

  /**
   * Run a parser on a string to obtain its result.
   */
  def parse[T](p: Parser[T], source: Source): ParseResult[T] =
    p(Input(source, 0))

  /**
   * Run a parser on all of a string to obtain its result.
   */
  def parseAll[T](p: Parser[T], source: Source): ParseResult[T] =
    parse(phrase(p), source)

  // Constructors

  /**
   * A parser that matches any character, failing if the end of input
   * is reached.
   */
  def any: Parser[Char] =
    Parser {
      in =>
        if (in.atEnd)
          Failure("any character expected but end of source found", in)
        else
          elem("any character", _ => true)(in)
    }

  /**
   * A parser that accepts just the given character.
   */
  def elem(ch: Char): Parser[Char] =
    elem(ch.toString, _ == ch)

  /**
   * A parser that accepts just those characters that pass the given predicate.
   * The message is used to describe what was expected if an error occurs.
   */
  def elem(message: String, p: Char => Boolean): Parser[Char] =
    Parser {
      in =>
        in.first match {
          case None =>
            Failure("end of input", in)
          case Some(c) if p(c) =>
            Success(c, in.rest)
          case _ =>
            Failure(message, in)
        }
    }

  /**
   * A parser that always errors with the given message.
   */
  def error(message: String): Parser[Nothing] =
    Parser {
      in =>
        Error(message, in)
    }

  /**
   * A parser that always fails with the given message.
   */
  def failure(message: String): Parser[Nothing] =
    Parser {
      in =>
        Failure(message, in)
    }

  /**
   * Interface for operations needed for repetition collections.
   */
  trait CCOps[CC[_] <: Seq[_]] {
    def empty[T]: CC[T]
    def newBuilder[T]: Builder[T, CC[T]]
    def prepend[T](t: T, cc: CC[T]): CC[T]
  }

  /**
   * Generic repetition zero or more times.
   */
  def grep[T, CC[_] <: Seq[_]](p: => Parser[T])(ops: CCOps[CC]): Parser[CC[T]] =
    grep1(p)(ops) | success(ops.empty)

  /**
   * Generic repetition one or more times.
   */
  def grep1[T, CC[_] <: Seq[_]](p: => Parser[T])(ops: CCOps[CC]): Parser[CC[T]] = {
    lazy val pp = p
    Parser {
      in =>
        val buf = ops.newBuilder[T]

        def rest(last: T, in: Input): ParseResult[CC[T]] = {
          val ppp = pp

          @tailrec
          def loop(last: T, in: Input): ParseResult[CC[T]] =
            ppp(in) match {
              case Success(t, next) =>
                buf += t
                loop(t, next)
              case _ =>
                Success(buf.result(), in)
            }

          loop(last, in)
        }

        pp(in) match {
          case Success(t, next) =>
            buf += t
            rest(t, next)
          case result: NoSuccess =>
            result
        }
    }

  }

  /**
   * Generic repetition zero or more times with separators.
   */
  def grepsep[T, CC[_] <: Seq[_]](p: => Parser[T], q: => Parser[Any])(ops: CCOps[CC]): Parser[CC[T]] =
    grep1sep(p, q)(ops) | success(ops.empty)

  /**
   * Generic repetition one or more times with separators.
   */
  def grep1sep[T, CC[_] <: Seq[_]](p: => Parser[T], q: => Parser[Any])(ops: CCOps[CC]): Parser[CC[T]] = {
    lazy val pp = p
    pp ~ grep(q ~> pp)(ops) ^^ {
      case t ~ ts =>
        ops.prepend(t, ts)
    }
  }

  /**
   * A parser that succeeds iff its argument parser succeeds, but consumes
   * no input in any circumstances.
   */
  def guard[T](p: => Parser[T]): Parser[T] =
    Parser {
      in =>
        p(in) match {
          case Success(t, _) =>
            Success(t, in)
          case failure =>
            failure
        }
    }

  /**
   * A marker of a position. Instaneces of this are used as placeholders when
   * there are no other suitable values associated with a parse position.
   */
  class Marker

  /**
   * Mark a string parser so that its value is discarded but a marker is
   * returned instead. That return value can then be used to set the
   * position of another value. We can't use the string value itself
   * since we are not guaranteed to have reference equality on strings.
   */
  def mark[T](p: => Parser[String]): Parser[Marker] =
    p ^^ (_ => new Marker)

  /**
   * Invert the result of a parser without consuming any input.
   */
  def not[T](p: => Parser[T]): Parser[Unit] =
    Parser {
      in =>
        p(in) match {
          case _: Success[_] =>
            Failure("failure of not", in)
          case _ =>
            Success((), in)
        }
    }

  /**
   * Optional parsing.
   */
  def opt[T](p: => Parser[T]): Parser[Option[T]] =
    p ^^ (t => Some(t)) | success(None)

  /**
   * Wrap `p` so that its failures become errors. See also `nocut`.
   */
  def commit[U](p: => Parser[U]) =
    Parser {
      in =>
        p(in) match {
          case Failure(msg, next) =>
            Error(msg, next)
          case result =>
            result
        }
    }

  /**
   * Suppress cuts in the parser `p`. I.e., errors produced by `p` are
   * propagated as failures instead. See also `commit`.
   */
  def nocut[T](p: => Parser[T]): Parser[T] =
    Parser {
      in =>
        p(in) match {
          case Error(msg, next) =>
            Failure(msg, next)
          case result =>
            result
        }
    }

  /**
   * Phrases, i.e., parse and succeed with no input remaining, except possibly
   * for whitespace at the end. If there is another later failure, prefer it
   * over the failure due to end of input being expected since the later one is
   * usually more informative.
   */
  def phrase[T](p: => Parser[T]): Parser[T] =
    Parser {
      in =>
        latestNoSuccess.withValue(None) {
          (p <~ "")(in) match {
            case s @ Success(t, next) =>
              if (next.atEnd)
                s
              else
                latestNoSuccess.value.filterNot {
                  _.next.offset < next.offset
                }.getOrElse(Failure("end of input expected", next))
            case result: NoSuccess =>
              latestNoSuccess.value.getOrElse(result)
          }
        }
    }

  /**
   * Succeed with a given value consuming no non-whitespace input. The value is
   * evaluated each time this parser is used.
   */
  def success[T](v: => T): Parser[T] =
    Parser {
      in =>
        Success(v, in)
    }

  // Whitespace handling and lexical level parsing

  /**
   * A parser that skips whitespace (default: sequences of zero or more
   * whitespace characters). This definition can be overridden as long
   * as the new definition succeeds at the end of the input.
   */
  def whitespace: Parser[Any] =
    regex("""\s*""".r)

  /**
   * Are we currently parsing whitespace?
   */
  var parsingWhitespace = false

  /**
   * If we are parsing whitespace already, succeed with no progress
   * so that we don't recurse. If we are not already parsing whitespace,
   * then apply the whitespace parser, swallowing any errors from it
   * unless they occur at the end of the input. In other words, an
   * error not at the end is treated as the absence of whitespace.
   */
  def parseWhitespace(in: Input): ParseResult[Any] =
    if (parsingWhitespace) {
      Success("", in)
    } else {
      parsingWhitespace = true
      val result =
        whitespace(in) match {
          case failure @ Failure(_, next) =>
            if (next.atEnd)
              failure
            else
              Success("", in)
          case success =>
            success
        }
      parsingWhitespace = false
      result
    }

  /**
   * A parser that matches a literal string after skipping any whitespace.
   * The form of the latter is defined by the `whitespace` parser.
   */
  implicit def literal(s: String): Parser[String] =
    Parser {
      in =>
        if (in.source.content.regionMatches(in.offset, s, 0, s.length)) {
          Success(s, Input(in.source, in.offset + s.length))
        } else {
          Failure(s"'$s' expected but ${in.found} found", in)
        }
    }

  /**
   * A parser that matches a regex string after skipping any whitespace.
   * The form of the latter is defined by the `whitespace` parser.
   */
  implicit def regex(r: Regex): Parser[String] =
    Parser {
      in =>
        val s = in.source.content.substring(in.offset)
        r.findPrefixMatchOf(s) match {
          case Some(m) =>
            Success(s.substring(0, m.end), Input(in.source, in.offset + m.end))
          case None =>
            Failure(s"string matching regex '$r' expected but ${in.found} found", in)
        }
    }

  /**
   * Convenience conversion to lift parsers that return 2-tilde-tuples to parsers
   * that return regular 2-tuples.
   */
  implicit def parseResultToTuple2[A, B](p: Parser[A ~ B]): Parser[(A, B)] =
    p ^^ { case a ~ b => (a, b) }

  /**
   * Convenience conversion to lift parsers that return 3-tilde-tuples to parsers
   * that return regular 3-tuples.
   */
  implicit def parseResultToTuple3[A, B, C](p: Parser[A ~ B ~ C]): Parser[(A, B, C)] =
    p ^^ { case a ~ b ~ c => (a, b, c) }

  /**
   * Convenience conversion to lift parsers that return 4-tilde-tuples to parsers
   * that return regular 4-tuples.
   */
  implicit def parseResultToTuple4[A, B, C, D](p: Parser[A ~ B ~ C ~ D]): Parser[(A, B, C, D)] =
    p ^^ { case a ~ b ~ c ~ d => (a, b, c, d) }

  /**
   * Convenience conversion to lift parsers that return 5-tilde-tuples to parsers
   * that return regular 5-tuples.
   */
  implicit def parseResultToTuple5[A, B, C, D, E](p: Parser[A ~ B ~ C ~ D ~ E]): Parser[(A, B, C, D, E)] =
    p ^^ { case a ~ b ~ c ~ d ~ e => (a, b, c, d, e) }

  /**
   * Convenience conversion to lift parsers that return 6-tilde-tuples to parsers
   * that return regular 6-tuples.
   */
  implicit def parseResultToTuple6[A, B, C, D, E, F](p: Parser[A ~ B ~ C ~ D ~ E ~ F]): Parser[(A, B, C, D, E, F)] =
    p ^^ { case a ~ b ~ c ~ d ~ e ~ f => (a, b, c, d, e, f) }

  /**
   * Convenience conversion to allow arity two functions to be used directly in
   * tree construction actions.
   */
  implicit def constToTupleFunction2[A, B, X](x: (A, B) => X): (A ~ B) => X = {
    case a ~ b =>
      x(a, b)
  }

  /**
   * Convenience conversion to allow arity three functions to be used directly in
   * tree construction actions.
   */
  implicit def constToTupleFunction3[A, B, C, X](x: (A, B, C) => X): (A ~ B ~ C) => X = {
    case a ~ b ~ c =>
      x(a, b, c)
  }

  /**
   * Convenience conversion to allow arity four functions to be used directly in
   * tree construction actions.
   */
  implicit def constToTupleFunction4[A, B, C, D, X](x: (A, B, C, D) => X): (A ~ B ~ C ~ D) => X = {
    case a ~ b ~ c ~ d =>
      x(a, b, c, d)
  }

  /**
   * Convenience conversion to allow arity five functions to be used directly in
   * tree construction actions.
   */
  implicit def constToTupleFunction5[A, B, C, D, E, X](x: (A, B, C, D, E) => X): (A ~ B ~ C ~ D ~ E) => X = {
    case a ~ b ~ c ~ d ~ e =>
      x(a, b, c, d, e)
  }

  /**
   * Convenience conversion to allow arity six functions to be used directly in
   * tree construction actions.
   */
  implicit def constToTupleFunction6[A, B, C, D, E, F, X](x: (A, B, C, D, E, F) => X): (A ~ B ~ C ~ D ~ E ~ F) => X = {
    case a ~ b ~ c ~ d ~ e ~ f =>
      x(a, b, c, d, e, f)
  }

  // Utilities

  /**
   * Parse digit strings that are constrained to fit into an `Int` value.
   * If the digit string is too big, a parse error results.
   */
  lazy val constrainedInt: Parser[Int] =
    wrap(regex("[0-9]+".r), stringToInt)

  /**
   * Parser for keywords. The list of string arguments gives the text
   * of the keywords in a language. The regular expression gives the
   * possible extension of the keyword to stop the keyword being seen as
   * an identifier instead. For example, the keyword list might contain
   * `"begin"` and `"end"` and the extension regular expression might
   * be `[^a-zA-Z0-9]`. Thus, `begin` followed by something other than
   * a letter or digit is a keyword, but `beginfoo8` is an identifier.
   * This parser succeeds if any of the keywords is present, provided
   * that it's not immediately followed by something that extends it.
   */
  def keywords(ext: Regex, kws: List[String]): Parser[String] =
    regex("(%s)(%s|\\z)".format(kws.mkString("|"), ext).r)

  /**
   * Convert the digit string `s` to an `Int` if it's in range, but return an
   * error message if it's too big.
   */
  def stringToInt(s: String): Either[Int, String] = {
    val value =
      s.foldLeft(0) {
        case (i, d) =>
          val dv = d.toInt - '0'.toInt
          if ((i >= 0) && (i <= (Int.MaxValue - dv) / 10))
            i * 10 + dv
          else
            -1
      }
    if (value == -1)
      Right("integer will not fit into 32 bits")
    else
      Left(value)
  }

  /**
   * Wrap a parser `p` that produces a value of type `T` to produce a
   * parser returning values of type `U`.
   *
   * The function `f` is responsible for converting the `T` value into
   * either a `U` value or a string that indicates what went wrong.
   * In the latter case, the resulting parser will error at the
   * original position with the message, ignoring any other errors
   * at that position.  Failures or errors of `p` will be lifted to
   * the returned type.
   */
  def wrap[T, U](p: => Parser[T], f: T => Either[U, String]): Parser[U] =
    Parser {
      in =>
        p(in) match {
          case Success(t, out) =>
            f(t) match {
              case Left(u) =>
                Success(u, out)
              case Right(msg) =>
                Failure(msg, in)
            }
          case result: NoSuccess =>
            result
        }
    }

}

/**
 * Repetitive parser combinators that use vectors to represent repetitive
 * constructs.
 */
trait VectorRepetitionParsers {

  self: ParsersBase =>

  import scala.collection.mutable.Builder

  object VectorOps extends CCOps[Vector] {
    def empty[T]: Vector[T] = Vector()
    def newBuilder[T]: Builder[T, Vector[T]] = Vector.newBuilder
    def prepend[T](t: T, cc: Vector[T]): Vector[T] = t +: cc
  }

  /**
   * Repetition zero or more times (vector version).
   */
  def rep[T](p: => Parser[T]): Parser[Vector[T]] =
    grep(p)(VectorOps)

  /**
   * Repetition one or more times (vector version).
   */
  def rep1[T](p: => Parser[T]): Parser[Vector[T]] =
    grep1(p)(VectorOps)

  /**
   * Repetition zero or more times with separators (vector version)
   */
  def repsep[T](p: => Parser[T], q: => Parser[Any]): Parser[Vector[T]] =
    grepsep(p, q)(VectorOps)

  /**
   * Repetition one or more times with separators (vector version)
   */
  def rep1sep[T](p: => Parser[T], q: => Parser[Any]): Parser[Vector[T]] =
    grep1sep(p, q)(VectorOps)

  /**
   * Adds postfix `*` and `+` to `Parser` class (vector version).
   */
  implicit class PostfixParserCombinators[T](p: Parser[T]) {
    def * = rep(p)
    def + = rep1(p)
  }

}

/**
 * Repetitive parser combinators that use lists to represent repetitive
 * constructs,
 */
trait ListRepetitionParsers {

  self: ParsersBase =>

  import scala.collection.mutable.Builder

  object ListOps extends CCOps[List] {
    def empty[T]: List[T] = List()
    def newBuilder[T]: Builder[T, List[T]] = List.newBuilder
    def prepend[T](t: T, cc: List[T]): List[T] = t :: cc
  }

  /**
   * Repetition zero or more times (list version).
   */
  def rep[T](p: => Parser[T]): Parser[List[T]] =
    grep(p)(ListOps)

  /**
   * Repetition one or more times (list version).
   */
  def rep1[T](p: => Parser[T]): Parser[List[T]] =
    grep1(p)(ListOps)

  /**
   * Repetition zero or more times with separators (list version)
   */
  def repsep[T](p: => Parser[T], q: => Parser[Any]): Parser[List[T]] =
    grepsep(p, q)(ListOps)

  /**
   * Repetition one or more times with separators (list version)
   */
  def rep1sep[T](p: => Parser[T], q: => Parser[Any]): Parser[List[T]] =
    grep1sep(p, q)(ListOps)

  /**
   * Adds postfix `*` and `+` to `Parser` class (list version).
   */
  implicit class PostfixParserCombinators[T](p: Parser[T]) {
    def * = rep(p)
    def + = rep1(p)
  }

}

/**
 * Parser combinators that use vectors to represent repetitive constructs.
 */
class Parsers(positions: Positions) extends ParsersBase(positions)
  with VectorRepetitionParsers

/**
 * Parser combinators that use lists to represent repetitive constructs.
 */
class ListParsers(positions: Positions) extends ParsersBase(positions)
  with ListRepetitionParsers
