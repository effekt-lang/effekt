package effekt.modules

import collection.Map

import effekt.symbols.{ TermSymbol, TypeSymbol, Symbol, BlockSymbol }
import effekt.symbols.scopes.Scope
import effekt.symbols.scopes.BlockScope
import effekt.source.Modl.Decl
import effekt.modules.Name.Blk
import effekt.modules.Name.Word
import effekt.modules.Name.Link
import effekt.modules.Name.All

/** Pseudo-Class */
object Mod {
  /** Set of Term Symbols*/
  type TrmSet = Set[TermSymbol]

  /** Optional Type Symbol */
  type TypOpt = Option[TypeSymbol]

  /** Optional Symbol */
  type SymOpt = Option[Symbol]

  /** Optional Module Symbole */
  type ModOpt = Option[Mod.Usr]

  /** Module Symbol */
  sealed class Usr(var user: Name)
      extends BlockSymbol with ModuleFace {

    /** Type Buffer */
    type Types = Map[Name.Word, TypeSymbol]

    /** Term Buffer */
    type Terms = Map[Name.Word, TrmSet]

    def name: Name = user

    // backend
    var _typ: Types = Map.empty
    var _trm: Terms = Map.empty

    /** Copies terms ands types from [[Usr]]. */
    def copy(mod: Usr) = {
      _typ ++= mod._typ
      _trm ++= mod._trm
    }

    /** Creates new [[Usr]] with same contents. */
    def clone(nm: Name = this.name): Usr = {
      val m = new Usr(user)
      m.copy(this)
      return m
    }

    /** export definitions from scope. */
    def save(sc: Scope) = {
      _typ ++= sc.types.map { kv => (Name.Word(kv._1), kv._2) }
      _trm ++= sc.terms.map { kv => (Name.Word(kv._1), kv._2) }
    }

    /** import definitions into scope. */
    def load(sc: Scope) = {
      _typ.foreach { (kv) =>
        sc.types.put(kv._1.str, kv._2)
      }

      _trm.foreach { (kv) =>
        val ts = sc.currentTermsFor(kv._1.str)
        sc.terms.put(kv._1.str, ts ++ kv._2)
      }
    }

    /** Hide module behind a new [[Msk]]. */
    def mask(face: Name = Name.Blk): Msk = new Msk(Some(this), this.name, face)

    /** Wrap module in virutal [[Src]] */
    def virt(path: Name = Name.Blk): Src = new Src(path, this.name, List(this))

    /* Interface *
     * --------- */

    def trm(nm: Name): TrmSet = nm match {
      case Blk        => Set.empty
      case w: Word    => _trm.get(w).getOrElse { Set.empty }
      case Link(l, r) => mod(l).map { m => m.trm(r) }.getOrElse { Set.empty }
      case All        => _trm.values.flatten.toSet
    }

    def typ(nm: Name): TypOpt = nm match {
      case Blk        => None
      case w: Word    => _typ.get(w)
      case Link(l, r) => mod(l).flatMap { m => m.typ(r) }
      case All        => None
    }

    def mod(nm: Name): ModOpt = {
      if (nm == Name.All || nm == user) {
        return Some(this)
      } else {
        return None
      }
    }

    def sym(nm: Name): SymOpt = {
      mod(nm).orElse {
        typ(nm).orElse {
          trm(nm).headOption
        }
      }
    }
  }

  /** Source Module */
  class Src(var path: Name, user: Name, var childn: List[Usr])
      extends Usr(user) with ParentModule[Usr] {

    override def name: Name = Name(path, user)

    def this(name: Name, childn: List[Usr] = List.empty) =
      this(name.dropLast(), name.last, childn)

    /** imported source modules.*/
    def imps(): List[Src] = childn
      .filter({ ch => ch.isInstanceOf[Src] })
      .map { ch => ch.asInstanceOf[Src] }

    /** exported user modules. */
    def exps(): List[Usr] = childn
      .filter { ch => !ch.isInstanceOf[Src] }
      .appended(this.clone(Name(path, Name.Blk)))

    /** calculate dependencies. */
    def deps(): List[Src] = imps().flatMap { i => i.deps() :+ i }.distinct

    /** inserts child if needed. */
    def adopt(mod: Usr): Unit = {
      if (childn.exists({ ch => ch.name == mod.name })) {
        return
      }

      childn = childn.appended(mod)
    }

    /** create & adopt child module. */
    def child(nm: Name): Usr = {
      val m = new Mod.Usr(nm)
      adopt(m)
      return m
    }

    /* Interface *
     * --------- */

    def subTrm(nm: Name): TrmSet = childn.collectFirst { ch =>
      return ch.trm(nm)
    }.getOrElse { Set.empty }

    def subTyp(nm: Name): TypOpt = childn.collectFirst { ch => return ch.typ(nm) }
    def subMod(nm: Name): ModOpt = childn.collectFirst { ch => return ch.mod(nm) }
    def subSym(nm: Name): SymOpt = childn.collectFirst { ch => return ch.sym(nm) }

    /* Overrides *
     * --------- */

    override def trm(nm: Name): TrmSet = super.trm(nm) ++ subTrm(nm)
    override def typ(nm: Name): TypOpt = super.typ(nm).orElse { subTyp(nm) }
    override def mod(nm: Name): ModOpt = super.mod(nm).orElse { subMod(nm) }
    override def sym(nm: Name): SymOpt = super.sym(nm).orElse { subSym(nm) }
  }

  /** Module Mask */
  class Msk(var parent: Option[Usr], user: Name, var face: Name)
      extends Usr(user) with ChildModule[Usr] {

    override val name: Name = Name(user, face)

    /** switche parent. */
    def attach(prt: Usr) = {
      this.parent = Some(prt)
    }

    /** First parent which is not a mask. */
    def root(): Usr = parent match {
      case Some(p) => p match {
        case m: Msk => m.root()
        case _      => p
      }
      case None => this
    }

    /** parent stack. */
    def stack(): List[Usr] = parent match {
      case Some(p) => p match {
        case m: Msk => m.stack().appended(p)
        case _      => List(p)
      }
      case None => List.empty
    }

    /* Interface *
     * --------- */

    def supTrm(nm: Name): TrmSet = parent.map { p =>
      p.trm(nm)
    }.getOrElse { Set.empty }

    def supTyp(nm: Name): TypOpt = parent.flatMap { p => p.typ(nm) }
    def supSym(nm: Name): SymOpt = parent.flatMap { p => p.sym(nm) }
    def supMod(nm: Name): ModOpt = parent.flatMap { p => p.mod(nm) }

    /* Overrides *
     * --------- */

    override def trm(nm: Name): TrmSet = {
      var ts = super.trm(nm)
      if (ts.isEmpty) {
        ts = supTrm(nm)
      }
      return ts
    }

    override def typ(nm: Name): TypOpt = super.typ(nm).orElse { supTyp(nm) }
    override def sym(nm: Name): SymOpt = super.sym(nm).orElse { supSym(nm) }
    override def mod(nm: Name): ModOpt = super.mod(nm).orElse { supMod(nm) }
  }

  /** Composed Module */
  class Cmp(var _top: Src, var _btm: Msk)
      extends Usr(Name(_top.user, _btm.user)) with ChildModule[Src] with ParentModule[Msk] {

    def parent: Option[Src] = Some(_top)
    def childn: List[Msk] = _btm.stack()
      .filter { m => m.isInstanceOf[Msk] }
      .map { m => m.asInstanceOf[Msk] }

    def supTrm(nm: Name): TrmSet = _top.subTrm(nm)
    def supTyp(nm: Name): TypOpt = _top.subTyp(nm)
    def supMod(nm: Name): ModOpt = _top.subMod(nm)
    def supSym(nm: Name): SymOpt = _top.subSym(nm)

    def subTrm(nm: Name): TrmSet = _btm.supTrm(nm)
    def subTyp(nm: Name): TypOpt = _btm.supTyp(nm)
    def subMod(nm: Name): ModOpt = _btm.supMod(nm)
    def subSym(nm: Name): SymOpt = _btm.supSym(nm)

    override def trm(nm: Name): TrmSet = subTrm(nm) ++ super.trm(nm) ++ supTrm(nm)
    override def typ(nm: Name): TypOpt = subTyp(nm).orElse { super.typ(nm) }.orElse { supTyp(nm) }
    override def mod(nm: Name): ModOpt = subMod(nm).orElse { super.mod(nm) }.orElse { supMod(nm) }
    override def sym(nm: Name): SymOpt = subSym(nm).orElse { super.sym(nm) }.orElse { supSym(nm) }
  }

  /**
   * Meta Module
   * note: not a [[Symbol]]
   */
  class Meta extends ModuleFace {

    /** source map */
    var _src: List[Src] = List.empty

    /** lookup source module. */
    def src(path: Name): Option[Src] = _src.find { s => s.path == path }

    /** lookup user module. */
    def usr(path: Name, user: Name): Option[Usr] = src(path).flatMap { s => s.mod(user) }

    /** lookup mask module. */
    def msk(path: Name, user: Name, face: Name): Option[Msk] = None

    /** lookup composed module. */
    def cmp(path: Name, user: Name, face: List[Name]): Option[Cmp] = None

    /** generate empty source module */
    def gen(path: Name): Src = {
      val s = new Src(path, Name.All, List.empty)
      _src = _src.appended(s)
      return s
    }

    /** generate empty user module */
    def gen(path: Name, user: Name): Usr = {
      val s = bind(path)
      return s.child(user)
    }

    /** lookup or generate source module. */
    def bind(path: Name): Src = src(path).getOrElse { gen(path) }

    /** lookup or generate user module. */
    def bind(path: Name, user: Name): Usr = usr(path, user).getOrElse { gen(path, user) }

    /* Interface *
     * --------- */

    def trm(nm: Name): TrmSet = _src.map { s => s.trm(nm) }.reduce { (l, r) => l.concat(r) }
    def typ(nm: Name): TypOpt = _src.collectFirst { s => return s.typ(nm) }
    def mod(nm: Name): ModOpt = _src.collectFirst { s => return s.mod(nm) }
    def sym(nm: Name): SymOpt = _src.collectFirst { s => return s.sym(nm) }
  }

  /*
  type Lines = List[String]

  def gap(ls: Lines): Lines = ls.map { l => "  " + l }

  /** pretty print map. */
  def ppt[S](mp: Map[Name.Word, S]): Lines = {
    mp.map { kv => s"${kv._1} := ${kv._2.getClass().getSimpleName()}" }
  }

  /** pretty print named map. */
  def ppt[S](nm: String, mp: Map[Name.Word, S]): Lines = {
    List(s"${nm}:").appendedAll(gap(ppt(mp)))
  }

  def ppt(mod: Mod.Usr): Lines = {

    List(s"${mod.user.full} {").appendedAll(gap(ppt("types", mod._typ)))
  }*/
}

