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
    def asTypeParam: TypeParam = s match {
      case t: TypeParam => t
      case _ => reporter.abort("Expected a type parameter")
    }
    def asValueParam: ValueParam = s match {
      case t: ValueParam => t
      case _ => reporter.abort("Expected a value parameter")
    }
    def asBlockParam: BlockParam = s match {
      case t: BlockParam => t
      case _ => reporter.abort("Expected a block parameter")
    }
    def asEffectOp: Operation = s match {
      case t: Operation => t
      case _ => reporter.abort("Expected an effect operation, but got " + s)
    }
    def asUserFunction: UserFunction = s match {
      case t: UserFunction => t
      case _ => reporter.abort("Expected a user defined function")
    }
    def asBuiltinFunction: ExternFunction = s match {
      case t: ExternFunction => t
      case _ => reporter.abort("Expected a builtin function")
    }
    def asConstructor: Constructor = s match {
      case t: Constructor => t
      case _ => reporter.abort("Expected a constructor")
    }
    def asDataType: DataType = s match {
      case t: DataType => t
      case _ => reporter.abort("Expected a data type")
    }
    def asRecord: Record = s match {
      case t: Record => t
      case _ => reporter.abort("Expected a record")
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
    def asInterface: Interface = s match {
      case t: Interface => t
      case t => reporter.abort("Expected an interface")
    }
    def asOperation: Operation = s match {
      case t: Operation => t
      case t => reporter.abort("Expected an operation")
    }
    def asFun: Callable = s match {
      case t: Callable => t
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
    def asInterfaceType: InterfaceType = t match {
      case t: InterfaceType => t
      case _ => reporter.abort("Expected a capability type")
    }
  }
}
