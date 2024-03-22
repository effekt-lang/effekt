package effekt

package generator

package hvm

import effekt.core.CoreParsers.definition

export effekt.core.Id

object cps {
    case class Name(name: String) 

    enum Definition {
        case Function(name: Name, params: List[Param], cont: Name, body: Term)
    }

    enum Term {
        case LetCont(cont: Name, body: Term, rest: Term)
        case Let(name: Name, expr: Expr, rest: Term)
        case Ret(cont: Name, arg: Expr)
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
}

