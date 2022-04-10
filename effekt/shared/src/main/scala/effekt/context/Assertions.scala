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
  implicit class SymbolAssertions(s: Symbol)(implicit reporter: ErrorReporter) {

    def asValueParam: ValueParam = s match {
      case t: ValueParam => t
      case _ => reporter.abort("Expected a value parameter")
    }
    def asBlockParam: BlockParam = s match {
      case t: BlockParam => t
      case _ => reporter.abort("Expected a block parameter")
    }
    def asUserEffect: UserEffect = s match {
      case t: UserEffect => t
      case _ => reporter.abort("Expected a user defined effect")
    }
    def asEffectOp: EffectOp = s match {
      case t: EffectOp => t
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
    def asBlockType: BlockType = s match {
      case t: BlockType => t
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
    def asEffect: Effect = s match {
      case t: Effect => t
      case _ => reporter.abort("Expected an effect")
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
      case _ => reporter.panic("Expected a term symbol")
    }
  }

  implicit class TypeAssertions(t: source.Type)(implicit reporter: ErrorReporter) {
    def asTypeVar: source.TypeVar = t match {
      case t: source.TypeVar => t
      case _ => reporter.abort("Expected a value type")
    }
  }
}
