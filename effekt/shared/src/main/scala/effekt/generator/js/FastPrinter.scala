package effekt
package generator
package js

import effekt.util.intercalate
import kiama.output.ParenPrettyPrinter
import kiama.output.PrettyPrinterTypes.Document

import scala.annotation.tailrec
import scala.language.implicitConversions

class FastPrinter {

  def pretty(stmt: List[Stmt]): Document =
    buffer = new StringBuffer()
    stmt foreach { s => toDoc(s); line() }
    Document(buffer.toString, Nil)

  // The `emit` effect
  private var buffer = new StringBuffer()

  inline def emit(str: String): Unit = buffer.append(str)

  // the `line` / `indent` effect
  private var level = 0

  private inline def indented[T](p: => T): T =
    val before = level
    level = level + 1
    val res = p
    level = before
    res

  private inline def indent(): Unit =
    var i = 0
    while (i < level) { emit("  "); i += 1 }

  private inline def line(): Unit = { emit("\n"); indent() }

  private def toDoc(name: JSName): Unit = emit(name.name)

  private def toDoc(expr: Expr): Unit = expr match {
    case Call(callee, args)           => toDocParens(callee) <> sep(args, toDoc, "(", ", ", ")")
    case New(callee, args)            => "new" <+> toDocParens(callee) <> sep(args, toDoc, "(", ", ", ")")
    case RawExpr(strings, args)       => intercalate(strings, args, toDocAsAtom)
    case RawLiteral(content)          => emit(content)
    case Member(callee, selection)    => toDocParens(callee) <> "." <> toDoc(selection)
    case IfExpr(cond, thn, els)       => parens { toDoc(cond) } <+> "?" <+> toDoc(thn) <+> ":" <+> toDoc(els)
    case Lambda(params, Return(obj: js.Object)) => sep(params, toDoc, "(", ", ", ")") <+> "=>" <+> parens { toDoc(expr) }
    case Lambda(params, Return(expr)) => sep(params, toDoc, "(", ", ", ")") <+> "=>" <+> toDoc(expr)
    case Lambda(params, Block(stmts)) => sep(params, toDoc, "(", ", ", ")") <+> "=>" <+> jsBlock(stmts, toDoc)
    case Lambda(params, body)         => sep(params, toDoc, "(", ", ", ")") <+> "=>" <+> jsBlock { toDoc(body) }
    case Object(properties)           => jsBlock {
      properties.foreach { case (n, d) => toDoc(n) <> emit(":") <+> toDoc(d) <> ", " }
    }
    case ArrayLiteral(elements)       => sep(elements, toDoc, "[", ", ", "]")
    case Variable(name)               => toDoc(name)
  }

  // to be used in low precedence positions
  private def toDocParens(e: Expr): Unit = e match {
    case e: IfExpr => parens(toDoc(e))
    case e: Lambda => parens(toDoc(e))
    case o: js.Object => parens(toDoc(e))
    case e => toDoc(e)
  }

  // to be used in really low precedence positions
  private def toDocAsAtom(e: Expr): Unit = e match {
    case e: Variable => toDoc(e)
    case e: Object => toDoc(e)
    case e: ArrayLiteral => toDoc(e)
    case e: RawLiteral => toDoc(e)
    case e => parens(toDoc(e))
  }

  private def toDoc(stmt: Stmt): Unit = stmt match {
    case RawStmt(strings, args)        => intercalate(strings, args, toDocAsAtom)
    case Block(stmts)                  => jsBlock(stmts, toDoc)
    case Return(expr)                  => "return" <+> toDoc(expr) <> ";"
    case ExprStmt(expr)                => toDoc(expr) <> ";"
    case Const(id, expr)               => "const" <+> toDoc(id) <+> "=" <+> toDoc(expr) <> ";"
    case Let(id, expr)                 => "let" <+> toDoc(id) <+> "=" <+> toDoc(expr) <> ";"
    case Destruct(ids, expr)           => "const" <+> braces { ids.foreach { id => toDoc(id) <> ", " } } <+> "=" <+> toDoc(expr) <> ";"
    case Assign(target, expr)          => toDoc(target) <+> "=" <+> toDoc(expr) <> ";"
    case Function(name, params, stmts) => "function" <+> toDoc(name) <> sep(params, toDoc, "(", ", ", ")") <+> jsBlock(stmts, toDoc)
    case Class(name, methods)          => "class" <+> toDoc(name) <+> jsBlock(methods, jsMethod)
    case If(cond, thn, Block(Nil))     => "if" <+> parens(toDoc(cond)) <+> toDocBlock(thn)
    case If(cond, thn, els)            => "if" <+> parens(toDoc(cond)) <+> toDocBlock(thn) <+> "else" <+> toDocBlock(els)
    case Try(prog, id, handler, Nil)   => "try" <+> jsBlock(prog, toDoc) <+> "catch" <+> parens(toDoc(id)) <+> jsBlock(handler, toDoc)
    case Try(prog, id, handler, fin)    => "try" <+> jsBlock(prog, toDoc) <+> "catch" <+> parens(toDoc(id)) <+> jsBlock(handler, toDoc) <+> "finally" <+> jsBlock(fin, toDoc)
    case Throw(expr)                   => "throw" <+> toDoc(expr) <> ";"
    case Break()                       => emit("break;")
    case Continue(label)               => "continue" <> label.foreach(l => " " <> toDoc(l)) <> ";"
    case While(cond, stmts, label)     =>
      label.foreach(l => toDoc(l) <> ": ") <>
        "while" <+> parens(toDoc(cond)) <+> jsBlock(stmts, toDoc)

    case Switch(sc, branches, default) => "switch" <+> parens(toDoc(sc)) <+> jsBlock(branches.foreach {
      case (tag, stmts) => "case" <+> toDoc(tag) <> ":" <+> (stmts foreach toDoc)
    } <+> default.foreach { stmts => "default:" <+> (stmts foreach toDoc) })
  }

  private def toDocBlock(stmt: Stmt): Unit = stmt match {
    case Block(stmts) => toDoc(stmt)
    case If(cond, thn, els) => toDoc(stmt)
    case _ => jsBlock(toDoc(stmt))
  }

  private def jsMethod(c: js.Function): Unit = c match {
    case js.Function(name, params, stmts) =>
      toDoc(name) <> sep(params, toDoc, "(", ", ", ")") <+> jsBlock(stmts, toDoc)
  }

  private def toDoc(pattern: Pattern): Unit = pattern match {
    case Pattern.Variable(name) => toDoc(name)
    case Pattern.Array(ps) => sep(ps, toDoc, "[", ",", "]")
  }

  // some helpers
  extension (d: Unit) {
    inline def <>(other: => Unit): Unit = other
    inline def <>(other: String): Unit = emit(other)
    inline def <+>(other: => Unit): Unit = { emit(" "); other }
    inline def <+>(other: String): Unit = { emit(" "); emit(other) }
  }
  extension (self: String) {
    inline def <>(other: => Unit): Unit = { emit(self); other }
    inline def <+>(other: => Unit): Unit = { emit(self); emit(" "); other }
  }


  @tailrec
  private def intercalate[A](strings: List[String], els: List[A], f: A => Unit): Unit =
    (strings, els) match {
      case (Nil, Nil) => ()
      case (str :: strs, el :: rest) =>
        emit(str)
        f(el)
        intercalate(strs, rest, f)
      case (strs, Nil) =>
        strs.foreach(s => emit(s))
      case (Nil, el :: rest) =>
        f(el)
        intercalate(Nil, rest, f)
    }

  private def sep[A](l: List[A], f: A => Unit, before: String, separator: String, after: String): Unit = {
    emit(before)
    sep(l, f, separator)
    emit(after)
  }

  private def sep[A](l: List[A], f: A => Unit, separator: String): Unit = sep(l, f, emit(separator))

  @tailrec
  private def sep[A](l: List[A], f: A => Unit, separator: => Unit): Unit = {
    l match {
      case Nil => ()
      case head :: Nil => f(head)
      case head :: tail =>
        f(head)
        separator
        sep(tail, f, separator)
    }
  }

  private inline def parens(p: => Unit): Unit = { emit("("); p; emit(")") }

  private inline def braces(p: => Unit): Unit = { emit("{"); p; emit("}") }

  private inline def jsBlock(content: => Unit): Unit = braces { indented { line() <> content } <> line() }

  private inline def jsBlock[A](els: List[A], show: A => Unit): Unit =
    braces { indented { line() <> sep(els, show, line()) } <> line() }
}
