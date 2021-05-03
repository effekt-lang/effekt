package effekt.modules

import effekt.symbols.{ TermSymbol, TypeSymbol, Symbol }
import effekt.modules.Name

/** Module Interface */
trait ModuleFace {
  /** Set of Term Symbols*/
  type TrmSet = Set[TermSymbol]

  /** Optional Type Symbol */
  type TypOpt = Option[TypeSymbol]

  /** Optional Symbol */
  type SymOpt = Option[Symbol]

  /** Optional Module Symbole */
  type ModOpt = Option[Mod.Usr]

  /** resolve [[TermSymbol]] with [[Name]]. */
  def trm(nm: Name): TrmSet

  /** resolve [[TypeSymbol]] with [[Name]]. */
  def typ(nm: Name): TypOpt

  /** resolve [[Module.Mod]] with [[Name]]. */
  def mod(nm: Name): ModOpt

  /** resolve [[Symbol]] with [[Name]]. */
  def sym(nm: Name): SymOpt
}

/** Super-Module */
trait ParentModule[C <: ModuleFace] extends ModuleFace {
  def childn: List[C]

  /** delegates resolution to children. */
  def subTrm(nm: Name): TrmSet

  /** delegates resolution to children. */
  def subTyp(nm: Name): TypOpt

  /** delegates resolution to children. */
  def subMod(nm: Name): ModOpt

  /** delegates resolution to children. */
  def subSym(nm: Name): SymOpt
}

/** Sub-Module */
trait ChildModule[P <: ModuleFace] extends ModuleFace {
  def parent: Option[P]

  /** delegates resolution to parent. */
  def supTrm(nm: Name): TrmSet

  /** delegates resolution to parent. */
  def supTyp(nm: Name): TypOpt

  /** delegates resolution to parent. */
  def supMod(nm: Name): ModOpt

  /** delegates resolution to parent. */
  def supSym(nm: Name): SymOpt
}
