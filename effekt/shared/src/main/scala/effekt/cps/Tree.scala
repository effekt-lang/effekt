package effekt
package cps

import effekt.core.CoreParsers.definition

export effekt.core.Id

case class ModuleDecl(
  path: String,
  includes: List[String],
  decls: List[Declaration],
  externs: List[Extern],
  definitions: List[Definition],
  exports: List[Symbol]
)

/**
 * Toplevel data and interface declarations
 */
enum Declaration {
  def id: Id

  case Data(id: Id, tparams: List[Id], constructors: List[Constructor])
  case Interface(id: Id, tparams: List[Id], properties: List[Property])
}
export Declaration.*

case class Constructor(id: Id, fields: List[Field])
case class Field(id: Id, tpe: ValueType)
case class Property(id: Id, tpe: BlockType)

/**
 * FFI external definitions
 */
enum Extern {
  // WARNING: builtins do not take evidence. If they are passed as function argument, they need to be eta-expanded.
  //   (however, if they _would_ take evidence, we could model mutable state with this)
  // TODO revisit
  case Def(id: Id, tparams: List[Id], params: List[Param], ret: ValueType, body: Template[Expr])
  case Include(contents: String)
}

case class Name(name: String) 

enum Definition {
        case Function(name: Name, params: List[Param], cont: Name, body: Term)
    }

enum Term {
        case LetCont(cont: Name, param: Param, body: Term, rest: Term)
        case Let(name: Name, expr: Expr, rest: Term)
        case AppCont(cont: Name, arg: Expr)
        case App(func: Name, arg: Expr, cont: Name)
}

enum Expr {
        case Var(name: Name)
        case Lit(n: Int)
}

enum Param {
        def id: Id

        case ValueParam(id: Id, tpe: ValueType)
        case BlockParam(id: Id, tpe: BlockType)
        case EvidenceParam(id: Id)
    }
    sealed trait Type

enum ValueType extends Type {
        case Var(name: Id)
        case Data(name: Id, targs: List[ValueType])
        case Boxed(tpe: BlockType) // WARNING not supported
    }

    case class EvidenceType() extends Type

enum BlockType extends Type {

        //   [A, B, C] (EV, EV, X, Y, Z, (EV, T) => T)    =>    T
        //    ^^^^^^^   ^^^^^^  ^^^^^^^  ^^^^^^^^^^^^^         ^^^
        //    tparams   evid.   vparams    bparams            result
        case Function(tparams: List[Id], eparams: List[EvidenceType], vparams: List[ValueType], bparams: List[BlockType], result: ValueType)
        case Interface(name: Id, targs: List[ValueType])
    }


