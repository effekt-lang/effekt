package effekt
package generator
package chez

import effekt.context.Context

object TransformerCPS {
    def compile(input: cps.ModuleDecl, coreModule: core.ModuleDecl, mainSymbol: symbols.TermSymbol)(using Context): chez.Block = ???
}