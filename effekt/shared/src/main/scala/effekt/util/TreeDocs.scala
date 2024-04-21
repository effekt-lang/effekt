//package effekt
//package util
//
//import scala.deriving.*
//import scala.compiletime.*
//
//
//// TODO we need to get rid of spurious pipes:
////
////    |- Type
////    | |- ValueType
////    | | |- ValueTypeTree
////    | | |- BoxedType
////    | | |- ValueTypeRef
////    | |
////    | |- BlockType
////    | | |- BlockTypeTree
////    | | |- FunctionType
////    | | |- BlockTypeRef
////    | |
////      |
//trait Docs[A] { def show(indentation: String, depth: Int): String }
//
//object Docs {
//
//  inline def summonAll[T <: Tuple]: List[Docs[_]] =
//    inline erasedValue[T] match
//      case _: EmptyTuple => Nil
//      case _: (t *: ts) => summonInline[Docs[t]] :: summonAll[ts]
//
//  inline given derived[T](using m: Mirror.Of[T]): Docs[T] =
//    val name = constValue[m.MirroredLabel]
//
//    def showNode(indentation: String) = s"${indentation}─ [[ ${name} ]]"
//
//    inline m match
//      case s: Mirror.SumOf[T] => new Docs[T] {
//        def show(indentation: String, depth: Int) = {
//          val elemsShowInstances = summonAll[m.MirroredElemTypes]
//          val children = elemsShowInstances.map(c => c.show(indentation + "  │", depth - 1))
//          // should not be rendered for the last element in a subtree
//          val endGroup = if children.isEmpty then "" else s"\n${indentation}"
//          if (depth <= 0) {
//            showNode(indentation)
//          } else {
//            showNode(indentation) + s"\n${children.mkString("\n")}${endGroup}"
//          }
//        }
//      }
//      case p: Mirror.ProductOf[T] => new Docs[T] {
//        def show(indentation: String, depth: Int) = showNode(indentation)
//      }
//      case _ => new Docs[T] {
//        def show(indentation: String, depth: Int) = ""
//      }
//}
//
