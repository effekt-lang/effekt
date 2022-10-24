package effekt
package generator
package ml

import effekt.context.Context
import effekt.core.*
import effekt.symbols.{ Module, Symbol, Wildcard }

import scala.language.implicitConversions
import effekt.util.paths.*


def uniqueName(id: Symbol): String = id.name.toString + "_" + id.id

def nameRef(id: Symbol): MLName = nameDef(id)

// TODO sanitize
def nameDef(id: Symbol): MLName = MLName(uniqueName(id))

//def intersperse[T](l: List[T], el: T): List[T] = l match {
//  case Nil => Nil
//  case head :: Nil => head :: Nil
//  case head :: rest => head :: el :: intersperse(rest, el)
//}
//
//case class RecordNames(sym: Symbol) {
//  val name = uniqueName(sym)
//  val basename = sym.name.name
//  val id = sym.id.toString
//
//  val uid = MLName(name)
//  val typeName = MLName(basename + "$Type" + id)
//  val predicate = MLName(name + "?")
//  val constructor = sym match {
//    case _: effekt.symbols.InterfaceType => MLName(s"make-${name}")
//    case _ => uid
//  }
//}
//
//def generateConstructor(ctor: effekt.symbols.Record): List[ml.Def] =
//  generateConstructor(ctor, ctor.fields)
//
//// https://www.scheme.com/csug8/objects.html
//// https://scheme.com/tspl4/records.html
//def generateConstructor(did: Symbol, fields: List[Symbol]): List[ml.Def] = {
//
//  val names = RecordNames(did)
//
//  // Record
//  val record = ml.Record(names.typeName, names.constructor, names.predicate, names.uid, fields map nameDef)
//
//  // Matcher
//  def matcher = {
//
//    var fresh = 0;
//    val fieldNames = fields map { _ => fresh += 1; MLName("p" + fresh) }
//    val matcherName = MLName("match-" + names.name)
//    val sc = MLName("sc")
//    val matched = MLName("matched")
//    val failed = MLName("failed")
//    val k = MLName("k")
//
//    val matcherBody = fields match {
//
//      // (define (<MATCHER-NAME>)
//      //   (lambda (sc matched failed k)
//      //     (if (pred sc) (k matched) (failed))))
//      case Nil => ml.Call(k, Variable(matched))
//
//      // (define (name p1 p2 ...)
//      //   (lambda (sc matched failed k)
//      //     ;; has correct tag?
//      //     (if (pred sc)
//      //       (match-fields sc matched failed k ([p1 sel1] [p2 sel2] ...))
//      //       (failed))))]))
//      case _ =>
//        def matchFields(fields: List[(MLName, Symbol)]): ml.Expr = fields match {
//          case Nil => ml.Call(k, Variable(matched))
//          case (p, field) :: fields =>
//            val sel = nameRef(field)
//            ml.Call(p, ml.Call(sel, Variable(sc)), Variable(matched), Variable(failed),
//              ml.Lambda(List(matched), // the continuation shadows `matched`
//                matchFields(fields)))
//        }
//        matchFields(fieldNames zip fields)
//    }
//    ml.Function(matcherName, fieldNames,
//          ml.Lambda(List(sc, matched, failed, k),
//            ml.If(
//              ml.Call(names.predicate, Variable(sc)),
//              matcherBody,
//              ml.Call(failed))))
//  }
//
//  List(record, matcher)
//}
//
//
//// STATE
//// -----
//
//// (define (getter ref)
////  (lambda () (unbox ref)))
////
//// (define (setter ref)
////  (lambda (v) (set-box! ref v)))
//def generateStateAccessors: List[ml.Function] = {
//  val ref = MLName("ref")
//  val value = MLName("value")
//
//  val getter = ml.Function(nameDef(symbols.builtins.TState.get), List(ref),
//    ml.Lambda(Nil, ml.Builtin("unbox", Variable(ref))))
//
//  val setter = ml.Function(nameDef(symbols.builtins.TState.put), List(ref),
//    ml.Lambda(List(value), ml.Builtin("set-box!", Variable(ref), Variable(value))))
//
//  List(getter, setter)
//}
