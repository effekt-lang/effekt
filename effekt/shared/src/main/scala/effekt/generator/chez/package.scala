package effekt
package generator
package chez

import effekt.context.Context
import effekt.core.*
import effekt.symbols.{ Module, Symbol, Wildcard }

import scala.language.implicitConversions
import effekt.util.paths.*


def uniqueName(id: Symbol): String = id.name.toString + "_" + id.id

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
  val constructor = sym match {
    case _: effekt.symbols.Interface => ChezName(s"make-${name}")
    case _ => uid
  }
}

def generateConstructor(ctor: effekt.symbols.Record): List[chez.Def] =
  generateConstructor(ctor, ctor.fields)

// https://www.scheme.com/csug8/objects.html
// https://scheme.com/tspl4/records.html
def generateConstructor(did: Symbol, fields: List[Symbol]): List[chez.Def] = {

  val names = RecordNames(did)

  // Record
  val record = chez.Record(names.typeName, names.constructor, names.predicate, names.uid, fields map nameDef)

  // Matcher
  def matcher = {

    var fresh = 0;
    val fieldNames = fields map { _ => fresh += 1; ChezName("p" + fresh) }
    val matcherName = ChezName("match-" + names.name)
    val sc = ChezName("sc")
    val matched = ChezName("matched")
    val failed = ChezName("failed")
    val k = ChezName("k")

    val matcherBody = fields match {

      // (define (<MATCHER-NAME>)
      //   (lambda (sc matched failed k)
      //     (if (pred sc) (k matched) (failed))))
      case Nil => chez.Call(k, Variable(matched))

      // (define (name p1 p2 ...)
      //   (lambda (sc matched failed k)
      //     ;; has correct tag?
      //     (if (pred sc)
      //       (match-fields sc matched failed k ([p1 sel1] [p2 sel2] ...))
      //       (failed))))]))
      case _ =>
        def matchFields(fields: List[(ChezName, Symbol)]): chez.Expr = fields match {
          case Nil => chez.Call(k, Variable(matched))
          case (p, field) :: fields =>
            val sel = nameRef(field)
            chez.Call(p, chez.Call(sel, Variable(sc)), Variable(matched), Variable(failed),
              chez.Lambda(List(matched), // the continuation shadows `matched`
                matchFields(fields)))
        }
        matchFields(fieldNames zip fields)
    }
    chez.Function(matcherName, fieldNames,
          chez.Lambda(List(sc, matched, failed, k),
            chez.If(
              chez.Call(names.predicate, Variable(sc)),
              matcherBody,
              chez.Call(failed))))
  }

  List(record, matcher)
}


// STATE
// -----

// (define (getter ref)
//  (lambda () (unbox ref)))
//
// (define (setter ref)
//  (lambda (v) (set-box! ref v)))
def generateStateAccessors: List[chez.Function] = {
  val ref = ChezName("ref")
  val value = ChezName("value")

  val getter = chez.Function(nameDef(symbols.builtins.TState.get), List(ref),
    chez.Lambda(Nil, chez.Builtin("unbox", Variable(ref))))

  val setter = chez.Function(nameDef(symbols.builtins.TState.put), List(ref),
    chez.Lambda(List(value), chez.Builtin("set-box!", Variable(ref), Variable(value))))

  List(getter, setter)
}
