package effekt
package generator
package chez

import effekt.context.Context
import effekt.core.*
import effekt.symbols.{ Module, Symbol, Wildcard }

import scala.language.implicitConversions
import effekt.util.paths.*

import scala.util.matching.Regex


def uniqueName(id: Symbol): String = id.name.toString.replace("?", "").replace("!", "") + "_" + id.id

def nameRef(id: Symbol): ChezName = nameDef(id)

// TODO sanitize
def nameDef(id: Symbol): ChezName = ChezName(uniqueName(id))

/** An Effekt Int is a fixnum (i61) on Chez, so a literal that does not fit wraps into one. */
def intLiteral(n: Long): chez.Expr = {
  val spare = 64 - 61 // |long| - |fixnum|
  chez.RawValue(((n << spare) >> spare).toString) // wrap correctly
}

def intersperse[T](l: List[T], el: T): List[T] = l match {
  case Nil => Nil
  case head :: Nil => head :: Nil
  case head :: rest => head :: el :: intersperse(rest, el)
}

case class RecordNames(sym: Symbol) {
  val name = uniqueName(sym)
  val basename = sym.name.name
  val id = sym.id.toString

  val uid = ChezName(name)
  val typeName = ChezName(basename + "$Type" + id)
  val predicate = ChezName(name + "?")
  val matcher = ChezName("match-" + name)
  val constructor = ChezName(name)
  val instance = ChezName(name + "$instance")
}

// https://www.scheme.com/csug8/objects.html
// https://scheme.com/tspl4/records.html
//
// There is also a paper by Andrey and Kent on records in Chez Scheme:
//    https://andykeep.com/pubs/scheme-12b.pdf
def generateConstructor(id: Symbol, fields: List[Symbol]): List[chez.Def] = {

  val did = id match {
    case c: symbols.Constructor => c
    case r: symbols.Record => r.constructor
    // right now, we also use core.Records to represent capabilities
    case i: symbols.Interface => i
    case other => other
      // sys error s"Compiler error: cannot generate a scheme record for internal symbol ${other}"
  }

  val names = RecordNames(did)

  // Record
  val record = chez.Record(names.typeName, names.constructor, names.predicate, names.uid, fields map nameDef)

  // The matcher only applies the given block with the extracted fields
  // (define (<MATCHER-NAME> sc block) (block (field-1 sc) (field-2 sc) ...)))
  def matcher = {
    val sc = ChezName("sc")
    val block = ChezName("block")

    val selectors = fields map { field =>
      chez.Call(nameRef(field), chez.Variable(sc))
    }
    chez.Function(names.matcher, List(sc, block),
      chez.Call(chez.Variable(block), selectors))
  }

  List(record, matcher)
}

/** Generate the singleton instance of a constructor without fields. */
def generateInstance(id: Symbol): chez.Def = {
  val names = RecordNames(id)
  chez.Constant(names.instance, chez.Call(chez.Variable(names.constructor), Nil))
}

/** Tests whether `sc` was built by constructor `id`. If the constructor has no fields, we can use `eq?` instead of the full predicate. */
def generateTest(id: Symbol, nullary: Boolean, sc: chez.Expr): chez.Expr =
  if (nullary) {
    chez.Call(chez.Variable(ChezName("eq?")), List(sc, chez.Variable(RecordNames(id).instance)))
  } else {
    chez.Call(chez.Variable(RecordNames(id).predicate), List(sc))
  }

/**
 * Generates a `match` from its clauses, each a `(predicate test, matcher call)` pair, plus its default:
 *   (cond [... (<PREDICATE-i> sc) (<MATCHER-i> sc block-i)] ... [else <DEFAULT>])
 * Without a default, the match is exhaustive, so its last clause becomes the default.
 */
def generateMatch(clauses: List[(chez.Expr, chez.Expr)], default: Option[chez.Expr]): chez.Expr = (clauses, default) match {
  case (Nil, None) => chez.Builtin("unreachable") // no constructor can reach this match
  case (tested :+ ((_, last)), None) => generateMatch(tested, Some(last)) // move last clause into the default
  case (Nil, Some(otherwise)) => otherwise
  case (tested, otherwise) => chez.Cond(tested, otherwise)
}
