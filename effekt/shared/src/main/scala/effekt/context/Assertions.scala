package effekt
package context

import effekt.symbols._
import effekt.util.messages.ErrorReporter

object assertions {

  // format: -alignSingleLineCaseStatements

  /**
   * The Assertions trait is designed to keep all error messages
   * in one place
   */
  extension(s: Symbol)(using reporter: ErrorReporter) {
    def asTypeVar: TypeVar = s match {
      case t: TypeVar => t
      case _ => reporter.abort("Expected a type variable")
    }
    def asValueParam: ValueParam = s match {
      case t: ValueParam => t
      case _ => reporter.abort("Expected a value parameter")
    }
    def asBlockParam: BlockParam = s match {
      case t: BlockParam => t
      case _ => reporter.abort("Expected a block parameter")
    }
    def asControlEffect: Interface = s match {
      case t: Interface => t
      case _ => reporter.abort("Expected a user defined control effect")
    }
    def asEffectOp: Operation = s match {
      case t: Operation => t
      case _ => reporter.abort("Expected an effect operation, but got " + s)
    }
    def asUserFunction: UserFunction = s match {
      case t: UserFunction => t
      case _ => reporter.abort("Expected a user defined function")
    }
    def asBuiltinFunction: BuiltinFunction = s match {
      case t: BuiltinFunction => t
      case _ => reporter.abort("Expected a builtin function")
    }
    def asConstructor: Record = s match {
      case t: Record => t
      case _ => reporter.abort("Expected a constructor")
    }
    def asDataType: DataType = s match {
      case t: DataType => t
      case _ => reporter.abort("Expected a data type")
    }
    def asValueType: ValueType = s match {
      case t: ValueType => t
      case _ => reporter.abort("Expected a value type")
    }
    def asFunctionType: FunctionType = s match {
      case t: FunctionType => t
      case _ => reporter.abort("Expected a block type")
    }
    def asValBinder: ValBinder = s match {
      case t: ValBinder => t
      case _ => reporter.abort("Expected a value binder")
    }
    def asVarBinder: VarBinder = s match {
      case t: VarBinder => t
      case _ => reporter.abort("Expected a mutable variable")
    }
    def asBinder: Binder = s match {
      case t: Binder => t
      case _ => reporter.abort("Expected a binder")
    }
    def asType: Type = s match {
      case t: Type => t
      case _ => reporter.abort("Expected a type")
    }
    def asEffect: InterfaceType = s match {
      case t: InterfaceType => t
      case t => reporter.abort("Expected an effect")
    }
    def asInterface: Interface = s match {
      case t: Interface => t
      case t => reporter.abort("Expected an interface")
    }
    def asFun: Fun = s match {
      case t: Fun => t
      case _ => reporter.abort("Expected a function")
    }
    def asCallTarget: CallTarget = s match {
      case t: CallTarget => t
      case _ => reporter.abort("Expected a call target")
    }
    def asTermSymbol: TermSymbol = s match {
      case t: TermSymbol => t
      case _ => reporter.panic("Expected a term symbol")
    }
    def asBlockSymbol: BlockSymbol = s match {
      case t: BlockSymbol => t
      case _ => reporter.panic("Expected a block symbol")
    }
  }

  extension(t: symbols.Type)(using reporter: ErrorReporter) {
    def asEffect: InterfaceType = t match {
      case t: InterfaceType => t
      case _ => reporter.abort("Expected a capability type")
    }
  }

  extension(t: source.Type)(using reporter: ErrorReporter) {
    def asTypeVar: source.TypeVar = t match {
      case t: source.TypeVar => t
      case _ => reporter.abort("Expected a value type")
    }
  }
}
