package effekt
package generator
package chez

import kiama.output.ParenPrettyPrinter
import kiama.output.PrettyPrinterTypes.Document

object PrettyPrinterCPS extends ParenPrettyPrinter {
    def format(block: chez.Block): Document = ???
}