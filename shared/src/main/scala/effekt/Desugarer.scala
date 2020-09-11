package effekt
package desugarer

/**
 *
 */
import effekt.context.Context
import effekt.context.assertions.{ SymbolAssertions, TypeAssertions }
import effekt.source.{ Def, Id, Tree, ModuleDecl }
import effekt.symbols._
import effekt.util.Task
import scopes._
import org.bitbucket.inkytonik.kiama.util.Source

/**
 * The output of this phase:
 *
 */
case class DesugarerState()

class Desugarer extends Phase[Module, Module] { namer =>

  def run(mod: Module)(implicit C: Context): Option[Module] = {
    Some(desugar(mod))
  }

  def desugar(mod: Module)(implicit C: Context): Module = {
    val module = mod.decl

    val newdefs = Context in {
      module.defs.map { d =>
        Context.focusing(d) {
          case d @ source.EffDef(id, ops) => {
            val newops = ops.map { op =>
              if (op.ret.eff.effs.length >= 1) {
                println(ops)
                op
              } else {
                op
              }
            }
            source.EffDef(id, newops)
          }
          case d => d
        }
      }
    }

    Module(ModuleDecl(module.path, module.imports, newdefs), mod.source)
  }

  // def precheckDef(d: Def)(implicit C: Context): Unit = Context.focusing(d) {
  //     case d @ source.FunDef(id, tparams, params, ret, body) =>
  //       d.symbol.ret.foreach { annot => Context.assignType(d.symbol, d.symbol.toType) }
  //
  //     case d @ source.ExternFun(pure, id, tparams, params, tpe, body) =>
  //       Context.assignType(d.symbol, d.symbol.toType)
  //
  //     case d @ source.EffDef(id, ops) =>
  //       d.symbol.ops.foreach { op => Context.assignType(op, op.toType) }
  //
  //     case source.DataDef(id, tparams, ctors) =>
  //       ctors.foreach { ctor =>
  //         val sym = ctor.symbol
  //         Context.assignType(sym, sym.toType)
  //
  //         sym.fields.foreach { field =>
  //           Context.assignType(field, field.toType)
  //         }
  //       }
  //
  //     case d @ source.RecordDef(id, tparams, fields) =>
  //       val rec = d.symbol
  //       Context.assignType(rec, rec.toType)
  //       rec.fields.foreach { field =>
  //         Context.assignType(field, field.toType)
  //       }
  //
  //     case d => ()
  //   }
  //
}
