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


private val unicodeRx = Regex("""[\\]u([0-9a-fA-F]{1,4})""")

// unicode escape sequences \u001b need to be translated to \033
// where 033 is an octal value left-padded with 0 to exactly three digits.
def hexToOctal(str: String) =
  val n = Integer.parseInt(str, 16)
  val octal = Integer.toString(n, 8)
  octal.reverse.padTo(3, '0').reverse

def adaptEscapes(str: String) =
  unicodeRx.replaceAllIn(str, m => "\\\\" + s"${hexToOctal(m.group(1))}")
