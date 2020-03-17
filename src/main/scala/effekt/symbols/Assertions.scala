package effekt
package symbols

import effekt.util.messages.ErrorReporter

/**
 * The Assertions trait is designed to keep all error messages
 * in one place
 */
trait Assertions { self: ErrorReporter =>

  def (s: Symbol) asValueParam: ValueParam = s match {
    case t: ValueParam => t
    case _ => abort("Expected a value parameter")
  }
  def (s: Symbol) asBlockParam: BlockParam = s match {
    case t: BlockParam => t
    case _ => abort("Expected a block parameter")
  }
  def (s: Symbol) asUserEffect: UserEffect = s match {
    case t: UserEffect => t
    case _ => abort("Expected a user defined effect")
  }
  def (s: Symbol) asEffectOp: EffectOp = s match {
    case t: EffectOp => t
    case _ => abort("Expected an effect operation, but got " + s)
  }
  def (s: Symbol) asUserFunction: UserFunction = s match {
    case t: UserFunction => t
    case _ => abort("Expected a user defined function")
  }
  def (s: Symbol) asBuiltinFunction: BuiltinFunction = s match {
    case t: BuiltinFunction => t
    case _ => abort("Expected a builtin function")
  }
  def (s: Symbol) asConstructor: Constructor = s match {
    case t: Constructor => t
    case _ => abort("Expected a constructor")
  }
  def (s: Symbol) asDataType: DataType = s match {
    case t: DataType => t
    case _ => abort("Expected a data type")
  }
  def (s: Symbol) asValueType: ValueType = s match {
    case t: ValueType => t
    case _ => abort("Expected a value type")
  }
  def (t: Type) asBlockType: BlockType = t match {
    case t: BlockType => t
    case _ => abort("Expected a block type")
  }
  def (s: Symbol) asValBinder: ValBinder = s match {
    case t: ValBinder => t
    case _ => abort("Expected a value binder")
  }
  def (s: Symbol) asVarBinder: VarBinder = s match {
    case t: VarBinder => t
    case _ => abort("Expected a mutable variable")
  }
  def (s: Symbol) asType: Type = s match {
    case t: Type => t
    case _ => abort("Expected a type")
  }
  def (s: Symbol) asEffect: Effect = s match {
    case t: Effect => t
    case _ => abort("Expected an effect")
  }
  def (t: source.Type) asTypeVar: source.TypeVar = t match {
    case t: source.TypeVar => t
    case _ => abort("Expected a value type")
  }
  def (s: Symbol) asFun: Fun = s match {
    case t: Fun => t
    case _ => abort("Expected a function")
  }
}