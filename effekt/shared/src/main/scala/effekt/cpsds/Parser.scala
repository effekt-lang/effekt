package effekt
package cpsds

import effekt.core.{Id, Type, ValueType, BlockType}
import effekt.source.{FeatureFlag, Span}
import effekt.util.UByte
import effekt.util.messages.{ErrorReporter, ParseError}
import kiama.parsing.{NoSuccess, ParseResult, Parsers, Success}
import kiama.util.{Position, Range, Severities, Source, StringSource}

class Parser extends Parsers {

  type P[T] = PackratParser[T]

  // === Lexing ===

  lazy val nameFirst = """[a-zA-Z_]""".r
  lazy val nameRest = """[a-zA-Z0-9_!?$]""".r
  lazy val nameBoundary = """(?!%s)""".format(nameRest).r
  lazy val name = "%s(%s)*%s".format(nameFirst, nameRest, nameBoundary).r

  lazy val ident = not(anyKeyword) ~> name | failure("Expected an identifier")

  lazy val `=` = literal("=")
  lazy val `@` = literal("@")
  lazy val `{` = literal("{")
  lazy val `}` = literal("}")
  lazy val `(` = literal("(")
  lazy val `)` = literal(")")
  lazy val `,` = literal(",")
  lazy val `.` = literal(".")
  lazy val `=>` = literal("=>")
  lazy val `!` = literal("!")
  lazy val `|` = literal("|")
  lazy val `;` = literal(";")
  lazy val `:` = literal(":")
  lazy val `<>` = literal("<>")

  lazy val `def` = keyword("def")
  lazy val `let` = keyword("let")
  lazy val `new` = keyword("new")
  lazy val `run` = keyword("run")
  lazy val `if` = keyword("if")
  lazy val `else` = keyword("else")
  lazy val `match` = keyword("match")
  lazy val `case` = keyword("case")
  lazy val `var` = keyword("var")
  lazy val `dealloc` = keyword("dealloc")
  lazy val `region` = keyword("region")
  lazy val `alloc` = keyword("alloc")
  lazy val `in` = keyword("in")
  lazy val `reset` = keyword("reset")
  lazy val `shift` = keyword("shift")
  lazy val `resume` = keyword("resume")
  lazy val `module` = keyword("module")
  lazy val `make` = keyword("make")
  lazy val `get` = keyword("get")
  lazy val `put` = keyword("put")
  lazy val `true` = keyword("true")
  lazy val `false` = keyword("false")
  lazy val `abort` = keyword("abort")
  lazy val `return` = keyword("return")
  lazy val `toplevel` = keyword("toplevel")

  lazy val `run!` : P[String] = literal("run!") <~ nameBoundary
  lazy val `run~` : P[String] = literal("run~") <~ nameBoundary

  def keywordStrings: List[String] = List(
    "def", "let", "new", "run", "if", "else", "match", "case",
    "var", "dealloc", "region", "alloc", "in", "reset", "shift",
    "resume", "module", "make", "get", "put", "true", "false",
    "abort", "return", "toplevel"
  )

  def keyword(kw: String): Parser[String] =
    regex((s"$kw(?!$nameRest)").r, kw)

  lazy val anyKeyword =
    keywords("[^a-zA-Z0-9_!?$]".r, keywordStrings)

  lazy val singleline = """//[^\n]*(\n|\z)""".r
  lazy val multiline = """/\*[^*]*\*+(?:[^/*][^*]*\*+)*/""".r
  lazy val simplespace = """\s+""".r

  override val whitespace = rep(simplespace | singleline | multiline)

  // === Literals ===

  lazy val integerLiteral = regex("([-+])?(0|[1-9][0-9]*)".r, "Integer literal")
  lazy val doubleLiteral =
    regex("([-+])?(0|[1-9][0-9]*)[.]([0-9]+)([eE][+-]?[0-9]+)?".r, "Double literal")
  lazy val stringLiteral =
    regex("""\"(\\.|[^\"])*\"""".r, "String literal") ^^ { s =>
      s.substring(1, s.length - 1)
    }
  lazy val byteLiteral = regex("0x([0-9A-F]{2})".r, "Byte literal") ^^ { s =>
    UByte.unsafeFromInt(Integer.parseInt(s.substring(2), 16))
  }

  // === Names ===

  private val ids = scala.collection.mutable.Map.empty[String, Id]

  def idFor(name: String): Id =
    ids.getOrElseUpdate(name, Id(name))

  lazy val id: P[Id] = ident ^^ idFor

  lazy val moduleName = """[a-zA-Z_][a-zA-Z0-9_]*(/[a-zA-Z_][a-zA-Z0-9_]*)*""".r

  // === Helpers ===

  def parens[T](p: => Parser[T]): Parser[T] = `(` ~> p <~ `)`
  def braces[T](p: => Parser[T]): Parser[T] = `{` ~> p <~ `}`
  def commaList[T](p: => Parser[T]): Parser[List[T]] = repsep(p, `,`) ^^ { _.toList }
  def many[T](p: => Parser[T]): Parser[List[T]] = rep(p) ^^ { _.toList }

  // === Module ===

  lazy val program: P[ModuleDecl] =
    `module` ~> moduleName ~ many(toplevelDef) ^^ {
      case path ~ defs => ModuleDecl(path, Nil, Nil, Nil, defs, Nil)
    }

  // === Toplevel ===

  lazy val toplevelDef: P[ToplevelDefinition] =
    ( `def` ~> id ~ parens(commaList(id)) ~ (`=` ~> braces(stmt)) <~ `;`.? ^^ {
        case name ~ params ~ body => ToplevelDefinition.Def(name, params, body)
      }
    | `let` ~> id ~ (`|` ~> id <~ `,`) ~ id ~ (`=` ~> stmt) <~ `;`.? ^^ {
        case name ~ ks ~ k ~ binding => ToplevelDefinition.Val(name, ks, k, binding)
      }
    | `let` ~> id ~ (`=` ~> expr) <~ `;`.? ^^ {
        case name ~ binding => ToplevelDefinition.Let(name, binding)
      }
    )

  // === Expressions ===

  lazy val expr: P[Expr] =
    ( `abort` ^^^ Expr.Abort
    | `return` ^^^ Expr.Return
    | `toplevel` ^^^ Expr.Toplevel
    | literal("()") ^^^ Expr.Literal((), Type.TUnit)
    | `true` ^^^ Expr.Literal(true, Type.TBoolean)
    | `false` ^^^ Expr.Literal(false, Type.TBoolean)
    | byteLiteral ^^ { b => Expr.Literal(b, Type.TByte) }
    | doubleLiteral ^^ { n => Expr.Literal(n.toDouble, Type.TDouble) }
    | integerLiteral ^^ { n => Expr.Literal(n.toLong, Type.TInt) }
    | stringLiteral ^^ { s => Expr.Literal(s, Type.TString) }
    | `make` ~> id ~ parens(commaList(expr)) ^^ {
        case tag ~ args => Expr.Make(null.asInstanceOf[ValueType.Data], tag, args)
      }
    | id ^^ Expr.Variable.apply
    )

  // === Statements ===

  lazy val stmt: P[Stmt] =
    ( defStmt
    | newStmt
    | getStmt
    | putStmt
    | letStmt
    | runStmt
    | ifStmt
    | matchStmt
    | varStmt
    | deallocStmt
    | regionStmt
    | allocStmt
    | resetStmt
    | shiftStmt
    | resumeStmt
    | holeStmt
    | invokeOrApp
    )

  // def id(params) = { body } rest
  lazy val defStmt: P[Stmt] =
    `def` ~> id ~ parens(commaList(id)) ~ (`=` ~> braces(stmt)) ~ stmt ^^ {
      case name ~ params ~ body ~ rest => Stmt.Def(name, params, body, rest)
    }

  // new id : Interface { ops } rest
  lazy val newStmt: P[Stmt] =
    `new` ~> id ~ (`:` ~> id) ~ braces(many(operation)) ~ stmt ^^ {
      case name ~ iface ~ ops ~ rest => Stmt.New(name, iface, ops, rest)
    }

  // def name(params) = { body }
  lazy val operation: P[Operation] =
    `def` ~> id ~ parens(commaList(id)) ~ (`=` ~> braces(stmt)) ^^ {
      case name ~ params ~ body => Operation(name, params, body)
    }

  // get id = ref;
  lazy val getStmt: P[Stmt] =
    `get` ~> id ~ (`=` ~> id <~ `;`) ~ stmt ^^ {
      case name ~ ref ~ rest => Stmt.Get(ref, name, rest)
    }

  // put ref = value;
  lazy val putStmt: P[Stmt] =
    `put` ~> id ~ (`=` ~> expr <~ `;`) ~ stmt ^^ {
      case ref ~ value ~ rest => Stmt.Put(ref, value, rest)
    }

  // let id = expr;
  lazy val letStmt: P[Stmt] =
    `let` ~> id ~ (`=` ~> expr <~ `;`) ~ stmt ^^ {
      case name ~ binding ~ rest => Stmt.Let(name, binding, rest)
    }

  // run id = callee(args);
  // run! id = callee(args);
  // run~ id = callee(args);
  lazy val runStmt: P[Stmt] =
    ( `run~` ~> id ~ (`=` ~> id) ~ parens(commaList(expr)) ~ (`;` ~> stmt) ^^ {
        case name ~ callee ~ args ~ rest => Stmt.Run(name, callee, args, Purity.Async, rest)
      }
    | `run!` ~> id ~ (`=` ~> id) ~ parens(commaList(expr)) ~ (`;` ~> stmt) ^^ {
        case name ~ callee ~ args ~ rest => Stmt.Run(name, callee, args, Purity.Impure, rest)
      }
    | `run` ~> id ~ (`=` ~> id) ~ parens(commaList(expr)) ~ (`;` ~> stmt) ^^ {
        case name ~ callee ~ args ~ rest => Stmt.Run(name, callee, args, Purity.Pure, rest)
      }
    )

  // if (cond) { thn } else { els }
  lazy val ifStmt: P[Stmt] =
    `if` ~> parens(expr) ~ braces(stmt) ~ (`else` ~> braces(stmt)) ^^ {
      case cond ~ thn ~ els => Stmt.If(cond, thn, els)
    }

  // scrutinee match { case Tag (params) => { body } ... } else { default }
  lazy val matchStmt: P[Stmt] =
    expr ~ (`match` ~> `{` ~> many(matchClause) <~ `}`) ~ (`else` ~> braces(stmt)).? ^^ {
      case scrutinee ~ clauses ~ default => Stmt.Match(scrutinee, clauses, default)
    }

  lazy val matchClause: P[(Id, Clause)] =
    `case` ~> id ~ clause ^^ { case tag ~ cl => (tag, cl) }

  lazy val clause: P[Clause] =
    parens(commaList(id)) ~ (`=>` ~> braces(stmt)) ^^ {
      case params ~ body => Clause(params, body)
    }

  // var id = init @ ks;
  lazy val varStmt: P[Stmt] =
    `var` ~> id ~ (`=` ~> expr) ~ (`@` ~> expr <~ `;`) ~ stmt ^^ {
      case name ~ init ~ ks ~ rest => Stmt.Var(name, init, ks, rest)
    }

  // dealloc(ref);
  lazy val deallocStmt: P[Stmt] =
    `dealloc` ~> parens(id) ~ (`;` ~> stmt) ^^ {
      case ref ~ rest => Stmt.Dealloc(ref, rest)
    }

  // region id @ ks { rest }
  lazy val regionStmt: P[Stmt] =
    `region` ~> id ~ (`@` ~> expr) ~ braces(stmt) ^^ {
      case name ~ ks ~ rest => Stmt.Region(name, ks, rest)
    }

  // alloc id in region = init;
  lazy val allocStmt: P[Stmt] =
    `alloc` ~> id ~ (`in` ~> id) ~ (`=` ~> expr <~ `;`) ~ stmt ^^ {
      case name ~ region ~ init ~ rest => Stmt.Alloc(name, init, region, rest)
    }

  // reset(p, ks, k) { body } @ ks1, k1
  lazy val resetStmt: P[Stmt] =
    `reset` ~> parens(id ~ (`,` ~> id) ~ (`,` ~> id)) ~
      braces(stmt) ~ (`@` ~> expr <~ `,`) ~ expr ^^ {
      case (p ~ ks ~ k) ~ body ~ ks1 ~ k1 =>
        Stmt.Reset(p, ks, k, body, ks1, k1)
    }

  // shift(prompt) { resume, ks, k => body } @ ks1, k1
  lazy val shiftStmt: P[Stmt] =
    `shift` ~> parens(id) ~
      (`{` ~> id <~ `,`) ~ (id <~ `,`) ~ (id <~ `=>`) ~ (stmt <~ `}`) ~
      (`@` ~> expr <~ `,`) ~ expr ^^ {
      case prompt ~ resume ~ ks ~ k ~ body ~ ks1 ~ k1 =>
        Stmt.Shift(prompt, resume, ks, k, body, ks1, k1)
    }

  // resume(r) { ks, k => body } @ ks1, k1
  lazy val resumeStmt: P[Stmt] =
    `resume` ~> parens(id) ~
      (`{` ~> id <~ `,`) ~ (id <~ `=>`) ~ (stmt <~ `}`) ~
      (`@` ~> expr <~ `,`) ~ expr ^^ {
      case r ~ ks ~ k ~ body ~ ks1 ~ k1 =>
        Stmt.Resume(r, ks, k, body, ks1, k1)
    }

  // <>
  lazy val holeStmt: P[Stmt] =
    `<>` ^^^ Stmt.Hole(Span.missing)

  // id.method(args) or id(args) or id!(args)
  lazy val invokeOrApp: P[Stmt] =
    id ~ (`.` ~> id).? ~ (`!`.? <~ `(`) ~ (commaList(expr) <~ `)`) ^^ {
      case obj ~ Some(method) ~ _ ~ args => Stmt.Invoke(obj, method, args)
      case func ~ None ~ Some(_) ~ args => Stmt.App(func, args, true)
      case func ~ None ~ None ~ args => Stmt.App(func, args, false)
    }

  // === Entry points ===

  def parse(source: Source)(using C: ErrorReporter): Option[ModuleDecl] =
    parseAll(program, source) match {
      case Success(ast, _) => Some(ast)
      case res: NoSuccess =>
        val input = res.next
        val range = Range(input.position, input.nextPosition)
        C.report(ParseError(res.message, Some(range), Severities.Error))
        None
    }

  def parseStmt(input: String): ParseResult[Stmt] =
    parseAll(stmt, StringSource(input, "input-string"))
}

object Parser {
  def apply(): Parser = new Parser()

  def module(input: String): ParseResult[ModuleDecl] = {
    val parsers = Parser()
    parsers.parseAll(parsers.program, StringSource(input, "input-string"))
  }

  def statement(input: String): ParseResult[Stmt] = {
    val parsers = Parser()
    parsers.parseAll(parsers.stmt, StringSource(input, "input-string"))
  }
}
